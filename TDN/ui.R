library(shiny)
#shinyapps::deployApp('TDN', appName="DMCA-Notice-Explorer")


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("DMCA Takedown Notice Explorer"),
    
    # Sidebar with a slider input for the number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("Choose the variable you wish to graph. 'Work Category' is the kind of copyrighted work. 'Sender' is the party submitting the take done request."),
            selectInput("plotType", label="Choose a variable to plot", choices=c("Work Category","Sender")),
            helpText("Plot show the number of URLs (x 1000) flagged as copyright infringing between April 13 to May 13 2015 using keyword search for 'Selma'.")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot", height = "600px")
        )
    )
))