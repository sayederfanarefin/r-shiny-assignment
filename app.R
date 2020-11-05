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
            checkboxInput("header", "Header", FALSE),
            
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
            
            #Horizontal line ----
                tags$hr(),
            
            sliderInput("split", "Split: Train-Test", 5, 95,
                        value = 80, step = 1),
            #Horizontal line ----
            tags$hr(),
            
            sliderInput("ratio", "Variance ratio threshold to select principal components", 0.10, 0.99,
                        value = 0.95, step = 0.01)
            
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            textOutput("accuracy"),
            tableOutput("contents"),
            
            plotOutput("graph"),
            
            
            textOutput("trainingImageTitle"),
            plotOutput("trainingImage"),
            
            
            textOutput("avgImageTitle"),
            plotOutput("avgImage"),
            
            
            textOutput("igenImageTitle"),
            plotOutput("igenImage"),
            
            
            textOutput("reconstructedImageTitle"),
            plotOutput("reconstructedImage")
           
            
        )
        
    )
)

unlink("temp", recursive = T)
dir.create("temp")

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
                
                unlink("temp", recursive = T)
                dir.create("temp")
                
                mainMethod(df, input$split, input$ratio)
                
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
                df <- read.csv("temp\\results.csv")
                # ,
                # header = input$header,
                # sep = input$sep,
                # quote = input$quote
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                # stop(safeError(e))
            }
        )
    
        
    }, caption = "Results", caption.placement = getOption("xtable.caption.placement", "top"))
    
    
    output$graph <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\plot.png", width = 400, height = 400,
             alt = "  Loading...")
    }, deleteFile = FALSE)
    
    
    output$trainingImageTitle <- renderText("Training Image: ")
    output$trainingImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ Train .png", width = 400, height = 400,
             alt = "  Loading...")
        
    }, deleteFile = FALSE)

    output$avgImageTitle <- renderText("Average Face: ")
    output$avgImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ AverageFace .png", width = 400, height = 400,
             alt = "  Loading...")
        
    }, deleteFile = FALSE)
    
    
    
    output$igenImageTitle <- renderText("Igen Face: ")
    output$igenImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ sel_vec- 1 .png", width = 400, height = 400,
             alt = "  Loading...")
    }, deleteFile = FALSE)
    
    
    output$reconstructedImageTitle <- renderText("Reconstructing faces with coefficient and eigenvector: ")
    output$reconstructedImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ LastOne .png", width = 400, height = 400,
             alt = "  Loading...")
    }, deleteFile = FALSE)
    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)