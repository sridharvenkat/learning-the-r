source("common.r")

computeCost <- function(input = matrix(),parameters = c(),output = c())
{
  hypothesis <- input %*% parameters
  error <- hypothesis - output
  error <- sum(error ^ 2)
  error <- error / (2*length(output))
  error
}

gradientDescent = function(input,output,parameters,alpha,iterations)
{
  theta = parameters;
  costHistory = c(1:iterations)
  for(cntr in 1:iterations)
  {
    hypothesis <- input %*% theta
    error <- hypothesis - output
    Z = t(input) %*% error
    theta = theta - (alpha * (Z/length(output)))
    costHistory[cntr] = computeCost(input,theta,output)
  }
  plotIterations <- c(1:iterations)
  plotWithLines(plotIterations,costHistory)
  #plot(costHistory~plotIterations)
  #frame <- data.frame(plotIterations,costHistory)
  #loess_fit <- loess(costHistory~plotIterations,frame)
  #lines(frame$plotIterations, predict(loess_fit), col="blue")
#print(costHistory)
  theta 
}
  

exec <- function()
{
  csvData <- loadData("ex1data1.txt")
  X <- csvData$x
  Y <- csvData$y
  plotWithLines(X,Y)
  theta <- c(rep(0,2))
  X <- cbind(1,X)
  cost <- computeCost(X,theta,Y)
  print(cost)
  theta <- gradientDescent(X,Y,theta,alpha = 0.01,iterations = 1500)
  theta
}



