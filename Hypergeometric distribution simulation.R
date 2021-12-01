library(gridExtra)
library(grid)
a <- c(177, 270, -37, 326, -72, -249, 80, 55, -101, 10)
sd(a)
mean(a)

175.663/sqrt(10)
d <- head(iris[,1:3])
d <- iris[,1:3]
grid.table(d)

n <- 3
trials <- data.frame()
for (i in 1:1000){
  temp <- sample(0:9, n, replace=F)
  trials <- rbind(trials, c(temp, sum(temp<=3), sum(temp>3)))
}
names(trials) <- c(1:n, "Yellow", "White")

hist(trials$Yellow + 1, breaks <- 0:(n+1))
t <- table(trials$Yellow)
prop.table(t)

E <- sum(as.integer(names(t)) * prop.table(t))

E





############################
# 3.27 New function from LiuQing
# Comparing Binomial & Hypergeometric
# Fixed the base function  


# () = notation in the function below
pop <- 100 # N 
items <- 4 # M (x = 0 ~ min(pop, sample))
sample <- 10 # n = Binomial size (n)

k <- min(pop, items)
# Modified dhyper function
# d_hyper <- function(n, m, k, x){
#   return (choose(n, x) * choose(m - n, k - x) / choose(pop, k))
# }
d_hyper <- function(M, N, n, x){
  return (choose(M, x) * choose(N - M, n - x) / choose(N, n))
}

Binom_prob = dbinom(x = c(0:k), size = sample, prob =items/pop)
# Hyper_prob = d_hyper(x = c(0:items), m = pop, n = sample, k = items)
Hyper_prob = d_hyper(x = c(0:k), N = pop, n = sample, M = items)


# k = number of items in the group (white balls)
# n = choose number of items from whole population (choose how many balls)
# m = population size (all balls)

# Two scatterplots
plot(0:k, Hyper_prob, ylab = "Probability", xlab = "x (number of success)", 
     main = "Comparison between binomial and hypergeometric\nprobability distributions")
points(0:k, Binom_prob, cex = 1, col = "blue", pch = 4)
segments(0:k, Binom_prob, 0:k, Hyper_prob, col = "red", lwd = 1)
legend("topright", legend=c("Binomial", "Hypergeometric", "Difference"),
       col=c("black","blue", "red"), pch = c(1, 4, 26), lty = c(0, 0, 1))


SE <- sum((Binom_prob - Hyper_prob)^2)
MSE <- SE / (k + 1)
MSE





################
# N from 20 to 1000
# M = 4 
# n = 10

res <- c()
for (i in 20:1000){
  # () = notation in the function below
  pop <- i # N 
  items <- 4 # M (x = 0 ~ min(pop, sample))
  sample <- 10 # n = Binomial size (n)
  
  k <- min(pop, items)
  # Modified dhyper function
  # d_hyper <- function(n, m, k, x){
  #   return (choose(n, x) * choose(m - n, k - x) / choose(pop, k))
  # }
  d_hyper <- function(M, N, n, x){
    return (choose(M, x) * choose(N - M, n - x) / choose(N, n))
  }
  
  Binom_prob = dbinom(x = c(0:k), size = sample, prob =items/pop)
  # Hyper_prob = d_hyper(x = c(0:items), m = pop, n = sample, k = items)
  Hyper_prob = d_hyper(x = c(0:k), N = pop, n = sample, M = items)
  
  
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
  
  
  SE <- sum((Binom_prob - Hyper_prob)^2)
  MSE <- SE / (k + 1)
  res <- rbind(res, MSE)
}
plot(20:1000, res, xlab = "N", ylab = "MSE", main = "MSE with different N")


res <- data.frame(a=2:3, b=1:2, c=3:4)
new <- data.frame(a=3, b=4, c=2)

nrow(merge(temp, new))>0
res <- rbind(res, new)
