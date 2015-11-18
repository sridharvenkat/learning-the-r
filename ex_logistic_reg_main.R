exec_logistic_reg <- function()
{
  csvData <- read.csv("ex2data2.csv",header = F)
  names(csvData) <- c("Test1","Test2","Result")
  
  csvData <- as.matrix(csvData)
  colLentgth <- dim(csvData)[2]
  X <- csvData[,c(1:(colLentgth-1))]
  X <- cbind(1,X)
  Y <- csvData[,colLentgth]
  initial_theta <- c(rep(0,dim(X)[2]))
}