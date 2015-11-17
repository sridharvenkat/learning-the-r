source("ex_main.r")

normalEquation <- function(X,Y)
{
  A <- t(X) %*% X
  theta <- solve(A) %*% t(X) %*% Y
  theta
}

exec_norm <- function()
{
  csvData <- loadData("ex1data2.txt","\t")
  csvData <- as.matrix(csvData)
  colLentgth <- dim(csvData)[2]
  X <- csvData[,c(1:(colLentgth-1))]
  X <- cbind(1,X)
  Y <- csvData[,colLentgth]
  theta <- normalEquation(X,Y)
  theta
  values <- c(1,1650,3)
  price <- t(as.matrix(values)) %*% theta
  print("Predicted of 1650sqft 3 bedroom house is : ")
  as.character(price)
}