library(shiny)

load(file="project_data.Rda")

# Define server logic required to draw a plot
shinyServer(function(input, output) {
    
    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should re-execute automatically
    #     when inputs change
    #  2) Its output type is a plot
        
    output$distPlot <- renderPlot({
        title <- paste("Total number of takedown requests by",input$plotType)
        if (input$plotType == "Work Category") {
            x <- as.factor(TD_Kind$kindOfWork)
            y <- TD_Kind$takeDown / 1000
            end_point = 0.5 + nrow(TD_Kind) + nrow(TD_Kind)-1
        } else {
            x <- TD_Sender$sender
            y <- TD_Sender$takeDown / 1000
            end_point = 0.5 + nrow(TD_Sender) + nrow(TD_Sender)-1
        }
        
        
        par(mar=c(15, 4, 2, 0.5)) 
        barplot(y,xlab="",
                ylab="Thousands of URLs",
                space=1,
                main=title)
        
        #Rotate the x labels 90 degrees
        text(seq(1.5,end_point,by=2), par("usr")[3]-0.25, 
             srt = 90, adj= 1, xpd = TRUE,
             labels = paste(x), cex=1)
    })
})