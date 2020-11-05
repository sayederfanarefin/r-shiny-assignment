#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

update.packages(ask = FALSE, checkBuilt = TRUE)

source("eigen.R")
# Define UI for data upload app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("R assignment"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
          
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            textOutput("accuracy"),
            tableOutput("contents"),
            
            plotOutput("plot3"),
            textOutput("trainingImage"),
            
            plotOutput("plot4"),
            textOutput("avgImage"),
            
            plotOutput("plot1"),
            textOutput("reconstructedImage"),
            plotOutput("plot2")
            
            
        )
        
    )
)

options(shiny.maxRequestSize=30*1024^3)

# Define server logic to read selected file ----
server <- function(input, output, session) {
    
    output$accuracy <- renderText({
        
        req(input$file1)
        
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
                mainMethod(df)
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    
        
    })
    
    output$contents <- renderTable({
        invalidateLater(1000)
       
        tryCatch(
            {
                df <- read.csv("temp\\results.csv",
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
               
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    
        
    }, caption = "Results", caption.placement = getOption("xtable.caption.placement", "top"))
    
    
    
    
    
    
    output$plot3 <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\plot.png", width = 400, height = 400,
             alt = "  Loading...")
    }, deleteFile = FALSE)
    
    output$trainingImage <- renderText("Training Image: ")
    output$plot1 <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ Train .png", width = 400, height = 400,
             alt = "  Loading...")
        
    }, deleteFile = FALSE)

    output$avgImage <- renderText("Average Face: ")
    output$plot4 <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ AverageFace .png", width = 400, height = 400,
             alt = "  Loading...")
        
    }, deleteFile = FALSE)
    
    
    output$reconstructedImage <- renderText("Reconstructing faces with coefficient and eigenvector: ")
    output$plot2 <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ LastOne .png", width = 400, height = 400,
             alt = "  Loading...")
    }, deleteFile = FALSE)
    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)