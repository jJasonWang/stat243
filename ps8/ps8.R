#1
sim_data <- function(n, outlier_prop=0.01, a=5, b=1, xrange=c(-5, 5), sigma=2){
  #Construct pair of x,y
  x <- runif(n, xrange[1], xrange[2])
  y <- seq_along(x)
  
  #Number of outlier
  nout <- n*outlier_prop
  
  idx <- sample(n, n - nout)
  y[idx] <- a*x[idx] + b + rnorm(n - nout, 0, sqrt(sigma))
  
  #Outlier
  y[-idx] <- a*x[-idx] + b + rt(nout, 1)
  
  data <- cbind(x, y)
  as.data.frame(data)
}

data <- sim_data(100, outlier_prop=0.05)
with(data, plot(x, y))
fit <- lm(y ~ x, data)
abline(fit)

#bootstrap
par(mfrow=c(2, 5))
m <- 10
for(i in 1:m){
  n <- nrow(data)
  boot_index <- sample(n, replace=TRUE)
  
  bootsample <- data[boot_index, ]
  
  with(bootsample, plot(x, y))
  fit <- lm(y ~ x, data)
  abline(fit)
}

#3
set.seed(0)

#beta to generate data
beta <- c(1, -20, 3, 0)

#Setup
n <- 100
p <- length(beta) - 1
X <- matrix(rnorm(n*p), ncol=p)
z <- rnorm(n, X*beta)
y <- (z > 1)*1

#Use glm to fit probit model
fit <- glm(y ~ X, family=binomial(link="probit"))
beta_1_hat <- summary(fit)$coefficients[2, 1]
se_1_hat <- summary(fit)$coefficients[2, 2]

#Should be close to 2
beta_1_hat/se_1_hat

#Expectation of zi when yi equal to 1 and 0
fun <- function(X, y, beta){
  mu <- cbind(1, X) %*% beta
  ifelse(y == 1, 
         mu + dnorm(-mu)/(1 - pnorm(-mu)),
         mu - dnorm(-mu)/pnorm(-mu))
}

#EM
my_em <- function(X, y, beta, tol=1e-8){
  #Initial setup
  beta_old <- beta
  expect_z <- fun(X, y, beta_old)
  fit <- lm(expect_z ~ X)
  beta_new <- coef(fit)
  iter <- 1
  
  #Update step
  while(sqrt(sum((beta_old - beta_new)^2)) > tol){
    beta_old <- beta_new
    expect_z <- fun(X, y, beta_old)
    fit <- lm(expect_z ~ X)
    beta_new <- coef(fit)
    iter <- iter + 1
  }
  list(beta_new, iter)
}
beta_init <- c(0, 0, 0, 0)
out <- my_em(X, y, beta_init)
out

#Log-likelihood
loglike <- function(beta){
  sum((fun(X, y, beta) - cbind(1, X) %*% beta)^2)
  
}
optim(beta_init, loglike, method="BFGS")


dpareto <- function(x, alpha, beta){
  (beta*alpha^beta)/(x^(beta + 1))*(x > 2)
}

rpareto <- function(n, alpha, beta){
  u <- runif(n)
  alpha/((1 - u)^(1/beta))
}


#Setting
alpha <- 2; beta <- 3; rate <- 1; m <- 10000
#Exponential 
set.seed(0)
#E(X)
x1 <- rpareto(m, alpha, beta)
f1 <- dexp(x1 - 2, rate)
g1 <- dpareto(x1, alpha, beta)
#Weight
w1 <- f1/g1
mu_hat <- mean(x1*w1) 

par(mfrow=c(1, 2))
hist(w1, prob=TRUE, main="Histogram of Weights")
hist(x1*w1, prob=TRUE, main="Histogram of weighted x")

#E(X^2)
x2 <- rpareto(m, alpha, beta)
f2 <- dexp(x2 - 2, rate)
g2 <- dpareto(x2, alpha, beta)
#Weight
w2 <- f2/g2
xsquare_hat <- mean((x2^2)*w2) 

par(mfrow=c(2, 2))
hist


set.seed(0)
#Pareto
#E(X)
x1 <- rexp(m, rate) + 2
f1 <- dpareto(x1, alpha, beta)
g1 <- dexp(x1 - 2, rate)
#Weight
w1 <- f1/g1
mu_hat <- mean(x1*w1) 

#E(X^2)
x2 <- rexp(m, rate) + 2
f2 <- dpareto(x2, alpha, beta)
g2 <- dexp(x2 - 2, rate)
#Weight
w2 <- f2/g2
xsquare_hat <- mean((x2^2)*w2) 

#4
theta <- function(x1, x2){
  atan2(x2, x1)/(2*pi)
}

f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1], x[2]))
  f2 <- 10*(sqrt(x[1]^2 + x[2]^2) - 1)
  f3 <- x[3]
  return(f1^2 + f2^2 + f3^2)
}

#functions that generate data
data_generate <- function(fix_value, fix_position, limit){
  n <- length(limit)^2
  
  #Empty matrix for filling value
  m <- matrix(0, nrow=n, ncol=4)
  colnames(m) <- c("x1", "x2", "x3", "value")
  
  #Fill in the x1, x2, x3
  m[, fix_position] <- rep(fix_value, n)
  m[, -c(fix_position, 4)] <- as.matrix(expand.grid(limit, limit))
  
  #Compute the value
  m[, 4] <- apply(m[, -4], 1, f)
  
  as.data.frame(m)
}

limit <- -10:10
#Constant x1 for 0, 5, -5
dt <- data_generate(0, 1, limit)
dt2 <- data_generate(5, 1, limit)
dt3 <- data_generate(-5, 1, limit)

#Constant x2 for 0, 5, -5
dt4 <- data_generate(0, 2, limit)
dt5 <- data_generate(5, 2, limit)
dt6 <- data_generate(-5, 2, limit)

#Constant x3 for 0, 5, -5
dt7 <- data_generate(0, 3, limit)
dt8 <- data_generate(5, 3, limit)
dt9 <- data_generate(-5, 3, limit)

library(ggplot2)
library(gridExtra)

#Constant x1
g1 <- ggplot(dt, aes(x=x2, y=x3, fill=value)) + 
  geom_tile() + 
  scale_fill_continuous(low="white", high="black", limit=c(0, 30000)) + 
  labs(title="x1 equal to 0") +
  theme(legend.margin=unit(1, "cm"))

g2 <- g1 %+% dt2 + labs(title="x1 equal to 5")
g3 <- g1 %+% dt3 + labs(title="x1 equal to -5")
g4 <- g1 %+% dt4 + aes(x=x1) + labs(title="x2 equal to 0")
g5 <- g1 %+% dt5 + aes(x=x1) + labs(title="x2 equal to 5")
g6 <- g1 %+% dt6 + aes(x=x1) + labs(title="x2 equal to -5")
g7 <- g1 %+% dt7 + aes(x=x1, y=x2) + labs(title="x3 equal to 0")
g8 <- g1 %+% dt8 + aes(x=x1, y=x2) + labs(title="x3 equal to 5")
g9 <- g1 %+% dt9 + aes(x=x1, y=x2) + labs(title="x3 equal to -5")

#Reference: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]])$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lwidth <- sum(legend$widths)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 2,
    widths = unit.c(unit(1, "npc") - lwidth, lwidth))
}

grid_arrange_shared_legend(g1, g2, g3, g4, g5, g6, g7, g8, g9, ncol=3)

library(optimx)
usemethod <- c("Nelder-Mead", "BFGS", "nlm", "nlminb", "L-BFGS-B")
try1 <- optimx(c(0, 0, 0), f, method=usemethod)
try2 <- optimx(c(5, 0, 0), f, method=usemethod)
try3 <- optimx(c(0, 5, 0), f, method=usemethod)
try4 <- optimx(c(0, 0, 5), f, method=usemethod)

all <- unique(rbind(try1[, 1:4], try2[1:4], try3[1:4], try4[1:4]))
all[order(all$value), ]
