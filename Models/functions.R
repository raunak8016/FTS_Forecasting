mytsplot<-function(z, ind = 1, lag = 40, d = 12)
{
  if(ind == 1) {
    par(mfcol = c(3, 2))
    ts.plot(z)
    abline(mean(z), 0)
    acf(z, lag.max = lag)
    pacf(z, lag.max = lag)
    x <- diff(z)
    ts.plot(x)
    acf(x, lag.max = lag)
    pacf(x, lag.max = lag)
  }
  else {
    par(mfcol = c(3, 2))
    ts.plot(z)
    abline(mean(z), 0)
    acf(z, lag.max = lag)
    pacf(z, lag.max = lag)
    x <- diff(z, lag = d)
    ts.plot(x)
    acf(x, lag.max = lag)
    pacf(x, lag.max = lag)
  }
  par(mfcol=c(1,1))
}

boxtr <-function(z, d = 1.5)
{
  z <- z + 0.1
  lam <- seq(-d, d, 0.05)
  m <- length(lam)
  n <- length(z)
  y <- rep(0, m)
  for(i in 1:m) {
    if(lam[i] != 0) {
      a <- (z^lam[i] - 1)/lam[i]
      b <- sum((a - mean(a))^2)/(n - 1)
      y[i] <- (n/2) * log(b) - (lam[i] - 1)*sum(log(z))
    }
    else {
      a <- log(z)
      b <- sum((a - mean(a))^2)/(n - 1)
      y[i] <- (n/2) * log(b) - (lam[i] - 1)*sum(log(z))
    }
  }
  par(mfrow = c(1, 1))
  plot(lam, y, xlab="lambda", ylab="Minus Log-Likelihood")
  #cbind(lam, y)
  cat(sprintf("Estimated Lambda = %s\n", lam[which.min(y)]))
}

trendtest<-function(w)
{# w is the differenced series.
  a <- mean(w)/sqrt(var(w)/length(w))
  b <- 2 * (1 - pnorm(abs(a)))
  print(c("Statistic", "P-value"))
  c(a, b)
}

