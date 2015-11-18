source("common.r")

logisticCost <- function(theta,X,y)
{
  z <- X %*% theta
  h <- sigmod(z)
  logh <- log(h)
  param1 <- t(logh) %*% as.matrix((-1 * y))
  param2 <- t(log(1-h)) %*% as.matrix(1-y)
  diff <- param1 - param2
  m <- length(y)
  j <- 1/m * (sum(diff))
  j
}

logisticGradient <- function(theta,X,y)
{
  m <- length(y)
  z <- X %*% theta
  h <- sigmod(z)
  S <- t(h) - y
  Z <- as.matrix(S) %*% X
  grad <- 1/m * Z
  as.numeric(grad)
}

predict <- function(theta, X)
{
  z = X %*% theta
  g = sigmod(z)
  g[g > 0.5] = 1
  g[g <= 0.5] = 0
  as.numeric(g)
}

exec_logistic <- function()
{
  csvData <- read.csv("ex2data1.csv",header = F)
  names(csvData) <- c("Test1","Test2","Result")
  #plotData("Test1","Test2","Result",csvData)
  #Start - Plot the data in scatter plot
  library(lattice)
  plot <- xyplot(Test2~Test1,group=Result,data=csvData)
  print(plot)
  #End - Plot the data in scatter plot
  
  csvData <- as.matrix(csvData)
  colLentgth <- dim(csvData)[2]
  X <- csvData[,c(1:(colLentgth-1))]
  X <- cbind(1,X)
  Y <- csvData[,colLentgth]
  initial_theta <- c(rep(0,dim(X)[2]))
  optimal <- optim(initial_theta,logisticCost,logisticGradient,X=X,y=Y)
  par <- optimal$par
  #Calculate the probability with Score 45 in Exam1 and 85 in Exam 2
  prob <- sigmod(c(1,45,85) %*% par)
  print(prob)
  #Let's predict the training accuracy
  predictedResult <- predict(par,X)
  mean(predictedResult == as.numeric(Y)) * 100
}