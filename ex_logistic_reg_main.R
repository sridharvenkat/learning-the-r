source("ex_logistic_main.r")

featureExpansion <- function(X,deg)
{
  X <- polym(X[,1],X[,2],raw = T,degree = deg)
  X <- cbind(1,X)
  X
}

logisticRegCost <- function(theta,X,y, lambda)
{
  cost <- logisticCost(theta,X,y)
  theta <- as.matrix(theta)[,-1]
  m <- length(y)
  theta <-  theta ^ 2;
  costReg <- cost + (lambda/(2*m)) * sum(theta)
  costReg
}

logisticRegGradient <- function(theta,X,y, lambda)
{
  m <- length(y)
  grad <- logisticGradient(theta,X,y);
  gradReg <- grad + (lambda/m)*theta;
  gradReg[1] <- grad[1]
  gradReg 
}

exec_logistic_reg <- function()
{
  csvData <- read.csv("ex2data2.csv",header = F)
  names(csvData) <- c("Test1","Test2","Result")
  
  csvData <- as.matrix(csvData)
  colLentgth <- dim(csvData)[2]
  X <- csvData[,c(1:(colLentgth-1))]
  X <- featureExpansion(X,6)
  write.csv(X,file="expanded.csv");
  Y <- csvData[,colLentgth]
  initial_theta <- c(rep(0,dim(X)[2]))
  lambda <- 1
#   costReg <- logisticRegCost(initial_theta,X,Y,lambda)
#   gradReg <- logisticRegGradient(initial_theta,X,Y,lambda)
#   print(gradReg)
#   print(costReg)
  optimal <- optim(initial_theta,logisticRegCost,logisticRegGradient,X=X,y=Y, lambda = lambda, method = "BFGS")
  par <- optimal$par
  #Calculate the probability with Score 45 in Exam1 and 85 in Exam 2
#   prob <- sigmod(c(1,45,85) %*% par)
#   print(prob)
  #Let's predict the training accuracy
  predictedResult <- predict(par,X)
  mean(predictedResult == as.numeric(Y)) * 100
}