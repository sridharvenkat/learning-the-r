loadData <- function(fileName, delim=",")
{
  csvData <- read.csv(fileName, sep=delim);
  csvData;
}

plotWithLines <- function(x,y,xlab="Iterations",ylab="Cost")
{
  plot(y~x,xlab=xlab,ylab=ylab)
  data_frame <- data.frame(x,y)
  loess_fit <- loess(y~x,data_frame)
  lines(data_frame$x,predict(loess_fit), col = "blue")
}

sigmod <- function(x)
{
  minusX <- -1 * x
  g <- ((1 + exp(minusX)) ^ -1)
  g
}