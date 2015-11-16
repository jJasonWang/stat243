#2.(c)
n <- 1000
gc()
X <- crossprod(matrix(rnorm(n^2), n))
gc()

library(pryr)
mem_used()
gc()
invisible(chol(X))
gc()
mem_used()

#reset
gc(reset=TRUE)
gc()

n <- seq(1000, 5000, by=1000) 
record <- function(n){
  X <- crossprod(matrix(rnorm(n^2), n))
  time <- rep(0, 2)
  time[1] <- system.time(U <- chol(X))[3]
  time[2] <- gc()[2, 5]
  time
}

result <- t(sapply(n, record))
result <- as.data.frame(cbind(n, result))
#Change the variable name
names(result) <- c("n", "time", "memory")

library(ggplot2); library(gridExtra)
#Plot
grid.arrange(
  ggplot(result, aes(x=n, y=time)) + geom_line(),
  ggplot(result, aes(x=n, y=memory)) + geom_line(),
  ncol=2
)

#3.
n <- 5000
X <- crossprod(matrix(rnorm(n^2), n))
y <- as.matrix(rnorm(n))

#First approach
system.time(b1 <- solve(X) %*% y)

#Second approach
system.time(b2 <- solve(X, y))

#Third approach
approach3 <- function(X, y){
  U <- chol(X)
  b <- backsolve(U, backsolve(U, y, transpose=TRUE))
  b
}
system.time(b3 <- approach3(X, y))


#4. 
gls <- function(X, Sigma, y){
  #Cholesky decomposition of Sigma
  U <- chol(Sigma)
  
  #Compute the new X matrix and y vector
  UT_inv <- t(solve(U))
  newX <- UT_inv %*% X
  newy <- UT_inv %*% y
  
  #Do the QR decomposition
  qrX <- qr(newX)
  
  #Q and R matrix
  Q <- qr.Q(qrX); R <- qr.R(qrX)
  
  #Solve the equation
  b <- backsolve(R, t(Q) %*% newy)
  b
}


n <- 3000
p <- 100

X <- matrix(rnorm(n*p), ncol=p)
Sigma <- crossprod(matrix(runif(n^2), n))
Y <- rnorm(n)

system.time(beta <- gls(X, Sigma, Y))

Yhat <- X %*% beta
plot(Y, Yhat)
