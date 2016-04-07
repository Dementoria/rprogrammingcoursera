columnmean <- function(x) {
  cols <- ncol(x)
  means <- numeric(cols)
  for(i in 1:cols) {
    means[i] <- mean(x[,i])
  }
  means
}