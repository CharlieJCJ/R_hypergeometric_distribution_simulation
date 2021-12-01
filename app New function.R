# 2021/3/27 Updated, New Tab, New Functions
# Charlie Ji
library(shiny)
library(rsconnect)
options(rsconnect.http.trace = FALSE)
options(rsconnect.http.verbose = FALSE)


N <- 10; M <- 4; n <- 3; num <- 1000
N_2 <- 100; M_2 <- 4; n_2 <- 10
i <- 20; f <- 100
d_hyper <- function(M, N, n, x){
    return (choose(M, x) * choose(N - M, n - x) / choose(N, n))
}
ui <- tagList(
    navbarPage(title = "Hypergeometric Distribution Simulation",
               tabPanel("Simulation",
                        sidebarLayout(
                            sidebarPanel(
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
                                verbatimTextOutput("text"),
                                # tags$hr(),
                                br(),
                                br(),
                                br(),
                                br(),
                                br(),
                                tableOutput("specs")
                            )
                        )
               ),
               tabPanel("Comparison: Hypergeom & Binom",
                        sidebarLayout(
                            sidebarPanel(
                                numericInput(inputId = "M_2",
                                             label = "Number of yellow balls (M)",
                                             value = 4),
                                numericInput(inputId = "N_2",
                                             label = "Number of all balls (N)",
                                             value = 100),
                                numericInput(inputId = "n_2",
                                             label = "Randomly pick how many balls (n)",
                                             value = 10),
                                submitButton("Submit")
                            ),
                            mainPanel(
                                plotOutput("compplot"),
                                verbatimTextOutput("sum")
                            )
                        )

               ),
               tabPanel("Limit behavior of Hypergeom",
                        sidebarLayout(
                            sidebarPanel(
                                numericInput(inputId = "i",
                                             label = "Minimum N",
                                             value = 20),
                                numericInput(inputId = "f",
                                             label = "Maximum N",
                                             value = 1000),
                                submitButton("Submit")
                            ),
                            
                            mainPanel(
                                plotOutput("limplot"),
                                verbatimTextOutput("sumtext")
                            )
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
            
            
        })
        output$view <- renderTable({
            trials
        })
        output$summary <- renderTable({
            table(trials$Yellow, dnn = c("Yellow Balls"))
            
        })
        output$specs <- renderTable({
            x <- data.frame(N, M, n, n*M/N)
            colnames(x) <- (c("All balls (N)", "Yellow Balls (M)", "# of picked (n)", "n * M/N"))
            x
        })
        output$text <- renderText({
            paste("Sample mean of number of yellow balls (from simulation) is: ", E)
            
            # paste("Sample mean of number of yellow ball (from simulation) is: ", E, "\n\nM (Yellow balls) =", 
            #       M, "\nN (All balls) =", N, "\nTrials =", num, 
            #       "\nn (Number of picked balls) =",  n, 
            #       "\np = M/N =", M/N, 
            #       "\n\nTheoretical expected value (np) =", n, "* ",
            #       M/N, "=", n*M/N)
    
        })
        
        ###############
        # Second tab
        pop <- input$N_2 # N 
        items <- input$M_2 # M (x = 0 ~ min(items, sample))
        sample <- input$n_2 # n = Binomial size (n)
        
        k <- min(sample, items)
        print(k)
        # Modified dhyper function
        # d_hyper <- function(n, m, k, x){
        #   return (choose(n, x) * choose(m - n, k - x) / choose(pop, k))
        # }
        
        
        Binom_prob = dbinom(x = c(0:as.numeric(k)), size = as.numeric(sample), prob =as.numeric(items)/as.numeric(pop))
        # Hyper_prob = d_hyper(x = c(0:items), m = pop, n = sample, k = items)
        Hyper_prob = d_hyper(x = c(0:as.numeric(k)), N = as.numeric(pop), n = as.numeric(sample), M = as.numeric(items))
        
        SE <- sum((Binom_prob - Hyper_prob)^2)
        MSE <- SE / (k + 1)
        
        
        # k = number of items in the group (white balls)
        # n = choose number of items from whole population (choose how many balls)
        # m = population size (all balls)
        
        output$compplot <- renderPlot({
            plot(0:as.numeric(k), as.numeric(Hyper_prob), ylab = "Probability", xlab = "x (number of success)",
                 main = "Comparison between binomial and hypergeometric\nprobability distributions",
                 ylim = c(0, 1.1 * max(Hyper_prob)))
            points(0:k, Binom_prob, cex = 1, col = "blue", pch = 4)
            segments(0:k, Binom_prob, 0:k, Hyper_prob, col = "red", lwd = 1)
            legend("topright", legend=c("Binomial", "Hypergeometric", "Difference"),
                   col=c("black","blue", "red"), pch = c(1, 4, 26), lty = c(0, 0, 1))
            # Two scatterplots
        })

        output$sum <- renderText({
            # MSE
            paste("MSE (Mean Squared Error): ", MSE)
        })
        
        
        ###########
        # Tab 3
        res <- c()
        ini <- input$i
        f <- input$f
        for (i in ini:f){
            # () = notation in the function below
            pop_2 <- i # N 
            items_2 <- 4 # M (x = 0 ~ min(pop, sample))
            sample_2 <- 10 # n = Binomial size (n)
            
            k_2 <- min(pop_2, items_2)
            # Modified dhyper function
            # d_hyper <- function(n, m, k, x){
            #   return (choose(n, x) * choose(m - n, k - x) / choose(pop, k))
            # }
            
            Binom_prob_2 = dbinom(x = c(0:as.numeric(k_2)), size = sample_2, prob =items_2/pop_2)
            # Hyper_prob = d_hyper(x = c(0:items), m = pop, n = sample, k = items)
            Hyper_prob_2 = d_hyper(x = c(0:as.numeric(k_2)), N = as.numeric(pop_2), n = as.numeric(sample_2), M = as.numeric(items_2))
            
            
            # k = number of items in the group (white balls)
            # n = choose number of items from whole population (choose how many balls)
            # m = population size (all balls)
            
            # Two scatterplots
            # plot(0:k, Hyper_prob, ylab = "Probability", xlab = "x (number of success)", 
            #      main = "Comparison between binomial and hypergeometric\nprobability distributions")
            # points(0:k, Binom_prob, cex = 1, col = "blue", pch = 4)
            # segments(0:k, Binom_prob, 0:k, Hyper_prob, col = "red", lwd = 1)
            # legend("topright", legend=c("Binomial", "Hypergeometric", "Difference"),
            #        col=c("black","blue", "red"), pch = c(1, 4, 26), lty = c(0, 0, 1))
            
            
            SE_2 <- sum((Binom_prob_2 - Hyper_prob_2)^2)
            MSE_2 <- SE_2 / (k_2 + 1)
            res <- rbind(res, MSE_2)
        }
        output$limplot <- renderPlot({
            plot(ini:f, res, xlab = "N", ylab = "MSE", main = "MSE with different N")
        })
        output$sumtext <- renderText({
            paste("M =", items_2, "\nN from", ini, "to", f, "\nn =", sample_2)
        })
        
        
        
        
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
