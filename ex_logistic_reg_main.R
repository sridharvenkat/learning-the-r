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
  costReg <- logisticRegCost(initial_theta,X,Y,lambda)
  
  costReg
}