# library(shiny)
# 
# # attempt 3
# count_module_ui <- function(id){
#     ns <- NS(id)
#     
#     tagList(
#         h4("Modulized Count"),
#         textOutput(ns("count_inside")),
#         h4("Is Modulized Count Odd or Even?"),
#         textOutput(ns("odd_even"))
#     )
# }
# 
# count_module <- function(input, output, session, action){
#     
#     output$count_inside <- renderText(as.character(action()))
#     
#     temp_text <- eventReactive(action(), {
#         if (action() %% 2 == 0) {
#             return("even")
#         } else{
#             return("odd")
#         }
#     })
#     
#     output$odd_even <- renderText(temp_text())
# }
# 
# ui <- fluidPage(
#     actionButton(
#         "submit", 
#         "Press me"
#     ),
#     h4("Count Regular"),
#     textOutput("count"),
#     count_module_ui("count_module")
# )
# 
# 
# server <- function(input, output, session) {
#     session$onSessionEnded(stopApp)
#     output$count <- renderText(as.character(input$submit))
#     count_value <- reactive(input$submit)
#     callModule(count_module, "count_module", action = count_value)
# }
# 
# shinyApp(ui = ui, server = server)

library(shiny)
library(rCharts)

rating <- c(2, 3, 5, 4, 1, 5, 3, 1, 4)
date_time <- c("2015-05-14", "2015-05-07", "2015-05-06", "2015-04-11", "2015-01-07", "2014-12-06", "2014-04-11", "2014-01-07", "2013-12-06")
repliesOnly <- c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
data <- data.frame(rating, date_time, repliesOnly)
data$rating <- as.character(data$rating)


function(input, output, session) {
    
    load("data.Rdata")
    
    datadata <- data
    makeReactiveBinding("datadata")
    
    newData <- reactive({
        
        input$Button
        isolate({
            
            datadata <- data
            
            datadata <- subset(datadata, rating %in% input$checkGroups)
            
            
        })
        
    })
    
    output$plot <- renderChart({
        
        datadata <- newData()
        
        plot <- nPlot(rating ~ date_time, data = datadata, 
                      type = "multiBarHorizontalChart", dom = 'plot')
        return(plot)
        
    })
    
    output$pieplot <- renderChart({
        
        datadata <- newData()
        
        pieplot <- nPlot(rating ~ date_time, data = datadata, 
                         type = "pieChart", dom = 'pieplot')
        return(pieplot)
        
    })
    
}