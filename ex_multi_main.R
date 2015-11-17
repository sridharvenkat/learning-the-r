source("ex_main.r")

normalize <- function(X)
{
  doNormalize <- function(x)
  {
    m <- mean(x)
    s <- sd(x)
    x <- (x - m)/s
    x
  }
  getMean <- function()
  {
    apply(X,2,mean)
  }
  getSd <- function()
  {
    apply(X,2,sd)
  }
  getNormalized <- function()
  {
    normX <- apply(X,2,doNormalize)
    normX 
  }
  get <- function()
  {
    X
  }
  transform <- function(x)
  {
    (x - getMean()) / getSd()
  }
  list(get = get,getMean = getMean,getNormalized = getNormalized,getSd = getSd, transform = transform)
#  normX <- apply(X,2,doNormalize)
#  normX
}

exec_multi <- function()
{
  csvData <- loadData("ex1data2.txt","\t")
  csvData <- as.matrix(csvData)
  colLentgth <- dim(csvData)[2]
  X <- csvData[,c(1:(colLentgth-1))]
  normX <- normalize(X)
  X <- cbind(1,normX$getNormalized())
  #print(X)
  Y <- csvData[,colLentgth]
  theta <- c(rep(0,3))
  theta <- gradientDescent(X,Y,theta,alpha = 1.00,iterations = 150)
  print(theta)
  normValues <- normX$transform(c(1650,3))
  values <- c(1,normValues)
  price <- t(as.matrix(values)) %*% theta
  print("Predicted of 1650sqft 3 bedroom house is : ")
  as.character(price)
}