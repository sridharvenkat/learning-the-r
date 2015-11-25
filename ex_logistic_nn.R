source("ex_logistic_reg_main.r")

exec_logistic_nn <- function()
{
  X <- read.csv(".\\data\\ex_logistic_nn_x.csv", header = F)
  Y <- read.csv(".\\data\\ex_logistic_nn_y.csv", header = F)
  
  yLevels <- as.numeric(levels(factor(Y)))
  
  for(yLevel in yLevels)
  {
    logicalY <- as.numeric(Y == yLevel)
    theta <- c(rep(0,length(Y)))
    lambda <- 0.1
    optimal <- optim(theta,logisticRegCost,logisticRegGradient,X=X,y=Y, lambda = lambda, method = "BFGS")  
  }
}