library(shiny)

update.packages(ask = FALSE, checkBuilt = TRUE)

source("support.R")
ui <- fluidPage(
    
    titlePanel("Eigen Faces Assignment"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            tags$hr(),
            
            checkboxInput("header", "Header", FALSE),
            
            
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
                tags$hr(),
            
            sliderInput("split", "Split: Train-Test", 5, 95,
                        value = 80, step = 1),
            tags$hr(),
            
            sliderInput("ratio", "Variance ratio threshold to select principal components", 0.10, 0.99,
                        value = 0.95, step = 0.01)
            
            
        ),
        
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

server <- function(input, output, session) {
    output$accuracy <- renderText({
        req(input$file1)
        tryCatch(
            {
                df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
                unlink("temp", recursive = T)
                dir.create("temp")
                mainMethod(df, input$split, input$ratio)
                
            },
            error = function(e) {
                
                stop(safeError(e))
            }
        )
    })
    
    output$contents <- renderTable({
        invalidateLater(1000)
        tryCatch(
            {
                df <- read.csv("temp\\results.csv")
            },
            error = function(e) {}
        )
        
    }, caption = "Results", caption.placement = getOption("xtable.caption.placement", "top"))
    
    
    output$graph <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\plot.png", width = 400, height = 400, alt = "  Loading...") 
        }, deleteFile = FALSE)
    
    
    output$trainingImageTitle <- renderText("Training Image: ")
    output$trainingImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ Train .png", width = 400, height = 400, alt = "  Loading...") 
    }, deleteFile = FALSE)

    output$avgImageTitle <- renderText("Average Face: ")
    output$avgImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ AverageFace .png", width = 400, height = 400, alt = "  Loading...") 
    }, deleteFile = FALSE)
    
    output$igenImageTitle <- renderText("Eigen Face: ")
    output$igenImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ sel_vec- 1 .png", width = 400, height = 400, alt = "  Loading...")
    }, deleteFile = FALSE)
    
    
    output$reconstructedImageTitle <- renderText("Reconstructing faces with coefficient and eigenvector: ")
    output$reconstructedImage <- renderImage({
        invalidateLater(1000)
        list(src = "temp\\ LastOne .png", width = 400, height = 400, alt = "  Loading...")
    }, deleteFile = FALSE)
    
    
}

shinyApp(ui, server)