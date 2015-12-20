source("ex_logistic_reg_main.r")

exec_logistic_nn <- function()
{
  X <- as.matrix(read.csv(".\\data\\ex_logistic_nn_x.csv", header = F))
  Y <- read.csv(".\\data\\ex_logistic_nn_y.csv", header = F)
  Y <- as.numeric(Y$V1)
  X <- cbind(1,X)
  yLevels <- as.numeric(levels(factor(Y)))
  optimSet <- matrix(nrow=length(yLevels),ncol = dim(X)[2])
  for(yLevel in yLevels)
  {
    logicalY <- as.numeric(Y == yLevel)
    initial_theta <- c(rep(0,dim(X)[2]))
    lambda <- 0.1
    optimal <- optim(initial_theta,logisticRegCost,logisticRegGradient,X=X,y=logicalY, lambda = lambda, method = "BFGS")  
    optimSet[yLevel,] = optimal$par
  }
  prediction <- X %*% t(optimSet)
  predictedResult <- apply(prediction,1,which.max)
  mean(predictedResult == Y) * 100
}

predict_feedForwardPropogation <- function(X, Y, Theta1, Theta2)
{
  z1 <- X %*% t(Theta1)
  a1 <- sigmod(z1)
  a1 <- cbind(1,a1)
  z2 <- a1 %*% t(Theta2)
  h <- sigmod(z2)
  h
}

exec_nn_feedForwardPropogation <- function()
{
  X <- as.matrix(read.csv(".\\data\\ex_logistic_nn_x.csv", header = F))
  Y <- read.csv(".\\data\\ex_logistic_nn_y.csv", header = F)
  theta1 <- as.matrix(read.csv(".\\data\\theta1_data.csv", header = F))
  theta2 <- as.matrix(read.csv(".\\data\\theta2_data.csv", header = F))
  X <- cbind(1,X)
  
  prediction <- predict_feedForwardPropogation(X,Y,theta1,theta2) 
  predictedResult <- apply(prediction,1,which.max)
  mean(predictedResult == Y) * 100
}

