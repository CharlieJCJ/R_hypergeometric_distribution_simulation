
library(shiny)
library(rsconnect)
options(rsconnect.http.trace = FALSE)
options(rsconnect.http.verbose = FALSE)


N <- 10; M <- 4; n <- 3; num <- 1000


ui <- tagList(
    navbarPage(title = "Hypergeometric Distribution Simulation",
               tabPanel("Simulation",
                        sidebarPanel(
                            # sliderInput("M", "M", 0, 100, value = M,
                            #              step = 1),
                            # sliderInput("N", "N", 0, 100, value = N,
                                         # step = 1),
                            # sliderInput("n", "n", 0, 100, value = n,
                                        # step = 1),
                            numericInput(inputId = "M",
                                         label = "Number of yellow balls (M)",
                                         value = 4),
                            numericInput(inputId = "N",
                                         label = "Number of all balls (N)",
                                         value = 10),
                            numericInput(inputId = "n",
                                         label = "Randomly pick how many balls (n)",
                                         value = 3),
                            numericInput(inputId = "num",
                                         label = "Trials",
                                         value = 100),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            verbatimTextOutput("text_trial"),
                            tableOutput("view"),
                            tableOutput("summary"),
                            plotOutput("probplot"),
                            verbatimTextOutput("text")
                        )
               
               )
               
    )
)

# session in server function argument is used by updateSelectInput
server <- function(session, input, output) {
    observe({
        n <- input$n
        M <- input$M
        num <- input$num
        N <- input$N
        trials <- data.frame()
        for (i in 1:num){
            temp <- sample(0:(N-1), n, replace=F)
            trials <- rbind(trials, c(temp, sum(temp<=(M-1)), sum(temp>(M-1))))
        }
        names(trials) <- c(1:n, "Yellow", "White")
        t <- table(trials$Yellow)
        prop.table(t)
        
        E <- sum(as.integer(names(t)) * prop.table(t))
        
        
        output$text_trial <- renderText({
            paste("Define random variable (x):\n\n0 <= x <= ", (M-1), "is Yellow\n", (M-1), "< x <=", (N-1),
                  "is White" )
        })
        output$probplot <- renderPlot({
            
            h <- barplot(prop.table(table(trials$Yellow )),  
                      main = "Hypergeometric Probability Distribution",
                      ylab = "Relative Frequency",
                      xlab = "Number of Yellow Balls Picked", ylim = c(0, 1.1 * max(prop.table(table(trials$Yellow)))))            
            text(h, prop.table(table(trials$Yellow)) + 0.02, prop.table(table(trials$Yellow)), xpd=TRUE)
            
            # y <- x[x$state == input$thisstate, ]
            # y = y[order(y$date, decreasing = FALSE),]
            # plot(y$positiveIncrease[1:LastDay], type = "l",
            #      xlab = "Days since the start of March 2, 2020",
            #      ylab = "Cases",
            #      main = paste("State/Region:", input$thisstate)
            # )
        })
        output$view <- renderTable({
            trials
        })
        output$summary <- renderTable({
            table(trials$Yellow, dnn = c("Yellow Balls"))
            
        })
        output$text <- renderText({
            paste("Expected value (from simulation) is: ", E, "\n\nM (Yellow balls) =", 
                  M, "\nN (All balls) =", N, "\nTrials =", num, 
                  "\nn (Number of picked balls) =",  n, 
                  "\np = M/N =", M/N, 
                  "\n\nTheoretical expected value (np) =", n, "* ",
                  M/N, "=", n*M/N)
        })
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
