#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(SummarizedExperiment)
library(org.Hs.eg.db)
library(pheatmap)
library(shinyjs)

#load the example data
       load("expression.Rdata")
       load("experiment.Rdata")
       load("expression_fifty_one_to_one_hundred.Rdata")
       load("experiment_fifty_one_to_one_hundred.Rdata")      

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("myHeatmap"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("font_row",
                        "Font size row:",
                        min = 6,
                        max = 14,
                        value = 10),
            sliderInput("font_col",
                        "Font size col:",
                        min = 6,
                        max = 14,
                        value = 10),
            selectInput("select", "Select annotation", choices=c("none",colnames(experiment)),selected = "none", multiple = T,
                        selectize = TRUE),
            checkboxInput("srownames", "Show Row Names", FALSE),
            checkboxInput("logtansform", "Log transform values", FALSE),
            radioButtons("norm", "Scale by", choices=c("none","row","column")),
            radioButtons("datasource", "Choose rank group", choices=c("Top 1-50 genes","Top 51-100 genes"))
            
        ),
        
        

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("combined_text_display")
           ,plotOutput("distPlot")
           
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

    combined_text <- reactive({
      paste0("Variables:", 
             "Data Source=", input$datasource, 
             "Logtransform=", input$logtansform, 
             "Selected Annotations=", input$select, 
             sep = "\n")
    })
  
    output$combined_text_display <- renderText({
      combined_text()
    })
    
    output$distPlot <- renderPlot({

      if(input$datasource == "Top 51-100 genes"){
        expression <- expression_fifty_one_to_one_hundred
        experiment <- experiment_fifty_one_to_one_hundred
      }         

        
        if(input$logtansform){
            expression <- log2(expression + 1)
        }
       
       if(is.null(input$select)){
           mysel<-NULL
       }else if(input$select[1]=="none"){
           mysel<-NULL
       }else if(length(input$select)==1){
           #if the data frame has one column it converts to a factor
           #force the type to be a data frame and restore row and column names
           mysel <-as.data.frame(experiment[,input$select[1]])
           rownames(mysel) <-rownames(experiment)
           colnames(mysel) <-input$select[1]
       }else{
           mysel<-experiment[,input$select]
       }
        
       pheatmap(expression,
                fontsize_row = input$font_row,
                fontsize_col=input$font_col,
                show_rownames=input$srownames,
                scale=input$norm,
                annotation_col=mysel)
    }, execOnResize = F)
    
    observeEvent(input$refresh, {
        session$invalidate
    })
        

}

# Run the application 
shinyApp(ui = ui, server = server)
