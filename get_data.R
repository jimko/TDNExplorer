# load packages
library(RCurl)
library(XML)
library(lubridate)
library(dplyr)
library(reshape)
projectDir <- "/home/jim/R/Class9/project"
dataDir <- paste(projectDir,"/data",sep="")

# Loop Through the priviously downloaded search results.
setwd(projectDir)

if ( ! exists("noticeList")) {
    noticeList <- data.frame(noticeID=character(), noticeURL=character())
    file.names <- dir(dataDir, pattern ="selma[0-9]*.html")
    for(i in 1:length(file.names)) {
        dat <- paste(dataDir,file.names[i],sep="/")
        resultPg <- htmlParse(dat, asText=FALSE)
        xID  <- xpathSApply(resultPg, '//*[@class="results-list"]/li[@class="notice result"]', xmlAttrs)
        xURL <- xpathSApply(resultPg, '//*[@class="results-list"]/li[@class="notice result"]/h3/a', xmlAttrs)
        noticeList <- rbind(noticeList, data.frame(noticeID=xID[2,], noticeURL=xURL,stringsAsFactors=FALSE))
    }
} else {
    print("Reused existing noticeList") 
}

#For each notice, go to the url and download the details. This variable could get very large.
#Each element in notices is a full page of html detailing the takedown notice content.
cacheDataFrame1 <- paste(dataDir,"notices.Rda",sep="/")
cacheDataFrame2 <- paste(dataDir,"notice_details.Rda",sep="/")
if ( ! exists("notices")) {
    if ( file.exists(cacheDataFrame1)) {
        print("Load notices data from from saved file") 
        load(file=cacheDataFrame1)
    } else {
        print("No cache. Re-downloading from the web, this could take a while....") 
        notices <-  sapply(noticeList$noticeURL, function(url)  {getURL(url, followlocation = TRUE)} )
        save(notices,file=cacheDataFrame1)
    }
} else { print("Resued existing notices data frame") 
}

# To really reset, rm(dfWorkList,dfURL,dfNotices); system(paste("rm ", dataDir, "/notice_details.Rda", sep=""))
if ( ! exists("dfWorkList")) {
    if ( file.exists(cacheDataFrame2)) {
        print("Load dfWorkList and dfURL data from from saved file2") 
        load(file=cacheDataFrame2)
    } else {
        print("No cache2. Build from scratch") 
        dfWorkList <- data.frame(noticeID=character(), workID=character(), workDesc=character(),kindOfWork=character(),takeDowns=integer())
        dfURL <- data.frame(noticeID=character(), workID=character(), domain=character())        

        # There is a 1 to 1 correspondence between noticeList and notices
        for (i in 1:nrow(noticeList)) {
        #for (i in 1:10) {    #small set for testing
            print(paste("Processing Notice: ",i,noticeList$noticeID[i]))
            doc = htmlParse(notices[i], asText=TRUE)   #notices has the full-text of each page
            ID <-  as.integer(unlist(strsplit(noticeList$noticeID[i], split='_', fixed=TRUE))[2])
                        
            hdr        <- xpathSApply(doc, '//*[@class="body"]/header/h1/text()', xmlValue)
            nType      <- xpathSApply(doc, '//*[@class="body"]/div[@class="main"]/dl[@class="notice-type"]/dd[@class="field"]/text()', xmlValue)
            actn       <- xpathSApply(doc, '//*[@class="body"]/div[@class="main"]/dl[@class="action-taken"]/dd[@class="field"]/text()', xmlValue)
            
            sndr       <- xpathSApply(doc, '//*[@class="body"]/header/div[@class="entities-wrapper"]/section[@class="sender "]/h6/a/text()', xmlValue)            
            dateSent   <- xpathSApply(doc, '//*[@class="body"]/header/div[@class="entities-wrapper"]/section[@class="sender "]/span[@class="date sent"]/time/text()', xmlValue)
            
            rcpnt      <- xpathSApply(doc, '//*[@class="body"]/header/div[@class="entities-wrapper"]/section[@class="recipient "]/h6/a/text()', xmlValue)
            dateRecv   <- xpathSApply(doc, '//*[@class="body"]/header/div[@class="entities-wrapper"]/section[@class="recipient "]/span[@class="date received"]/time/text()', xmlValue)
            
            sndr <- paste(sndr, collapse = '/')
            if (length(actn) == 0) {
                actn <- NA
            }
            dfTmp1 <-  data.frame(noticeID=ID, 
                                  headline=hdr,
                                  noticeType=nType,
                                  sender=sndr,
                                  dateSent=mdy(dateSent),
                                  recipient=rcpnt,
                                  dateRecvd=mdy(dateRecv),
                                  actionTaken=actn,
                                  stringsAsFactors = FALSE)
            
            if (exists("dfNotices")) {                
                dfNotices <- rbind(dfNotices,dfTmp1)
            } else {
                dfNotices <- dfTmp1
            }
            
            #Now go through each cited work on the notice
            worksBase <- '//*[@class="works"]/ol[@class="copyright-claims"]'
            worksPath <- paste(worksBase,'/li[@class="work"]', sep="")

            
            worksDoc <- getNodeSet(doc,worksBase)[[1]]  # Mini-html with just the works and URLs
            x <- xpathSApply(worksDoc, worksPath, xmlAttrs)
            workIDs <- x[2,]  #just the ID
            
            for (wid in workIDs) {    
                WID <-  as.integer(unlist(strsplit(wid, split='_', fixed=TRUE))[2])
                descField <- '/div[@class="row"]/div[@class="description"]/span[@class="field"]'
                kindField <- '/div[@class="row"]/div[@class="kind"]/span[@class="field"]'
                descFieldPath <- paste(worksBase,'/li[@id="',wid,'"]', descField, '/text()', sep="")
                kindFieldPath <- paste(worksBase,'/li[@id="',wid,'"]', kindField, '/text()', sep="")
                desc <- xpathSApply(worksDoc, descFieldPath, xmlValue)
                kind <- xpathSApply(worksDoc, kindFieldPath, xmlValue)
                workName <- gsub("\r\n?|\n","",desc) #remove New Lines
                workName <- gsub(" \\(Original Movie\\)","",workName) #consolidates different versions of same title
                
                if (nchar(workName) > 200) {
                    workName <- paste(substr(workName,1,97)," [TRUNCATED]")  #Sometimes people write a whole leagal brief here
                }
                if (length(desc) == 0) {
                    desc <- NA
                }
                if (length(kind) == 0) {
                    kind <- NA
                }

                if (workName == "Selma") {
                #Now go through each alledged infringing URL on related to the work
                urlBase <- paste(worksBase,'/li[@id="',wid,'"]/div[@class="row"]/ol[@class="list infringing-urls"]/li[@class="infringing_url"]',sep="")
                urlsDoc <- getNodeSet(worksDoc,urlBase)  # Mini-html with just the urls for this work
                
                #If the list isn't empty (no URLs), then go get them
                if ( length(urlsDoc) > 0 ) {
                    urlsDoc <- urlsDoc[[1]]
                    x2 <- xpathSApply(urlsDoc,urlBase, xmlAttrs)
                    urlList <- x2[2,]  #just the ID
                    
                    urlCnt <- 0
                    for (uid in urlList) {
                        #print(paste("    Processing URL:",noticeList$noticeID[i], wid, uid) )
                        urlPath <- paste(worksBase,'/li[@id="',wid,'"]/div[@class="row"]/ol[@class="list infringing-urls"]/li[@id="',uid, '"]/text()',sep="")
                        URL <- xpathSApply(urlsDoc, urlPath, xmlValue)
                        dom <- unlist(strsplit(URL, split='/', fixed=TRUE))[3]
                        dfURL <- rbind(dfURL, data.frame(noticeID=ID, workID=WID, domain=dom, stringsAsFactors = FALSE))
                        urlCnt <- urlCnt + 1
                    } #for3 urlList
                } #if
                print(paste("  ",urlCnt," urls found for ",workName))
                if (exists("urlsDoc")) {
                    rm(urlsDoc)
                }   #end of URLs
                }  #WorkName=Selma
                dfWorkList <- rbind(dfWorkList, data.frame(noticeID=ID, workID=WID, workDesc=workName, kindOfWork=kind, takeDowns=urlCnt, stringsAsFactors = FALSE))
            } #for2 wid
            rm(worksDoc)            
        } #for1
        
        save(dfNotices,dfWorkList,dfURL,file=cacheDataFrame2)
        rm(notices)
    }
} else { 
    print("Resued existing dfWorkList and dfURL data frames") 
    load(file=cacheDataFrame2)
}
#main dataframes to be uploaded with app, and summarys for charting.
TD <- inner_join(dfNotices,dfWorkList,by=c("noticeID"))
K <- melt(TD,id.vars=c("sender","kindOfWork"),measure.vars=c("takeDowns"))
TD_Sender<-cast(K,sender~variable,sum)
TD_Kind<-cast(K,kindOfWork~variable,sum)
d <- select (dfURL,noticeID,domain) %>% group_by(domain) %>% summarise(count=n()) %>% arrange(desc(count))
TD_Top10 <- d[1:10,]
save(TD,TD_Sender,TD_Kind,TD_Top10,file="/home/jim/R/Class9/project/TDN/project_data.Rda")