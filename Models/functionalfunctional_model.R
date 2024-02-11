library(fda)
library(tidyverse)
library(lubridate)

setwd("C:/Users/rauna/OneDrive/Desktop/UNI/4th YR/Winter/MATH 518/RCode/Project/")


# data processing
basis = create.bspline.basis(c())

prices = read.csv("2020_AAPL_november.csv", stringsAsFactors = FALSE)
prices$AAPL <- as.numeric(prices$AAPL)


summary(prices)

any(is.na(prices))

matplot(prices$AAPL, type="l")

prices$TimeStamp <- ymd_hms(prices$TimeStamp)
prices$Date <- as.Date(prices$TimeStamp)
prices$Time <- format(prices$TimeStamp, "%H:%M:%S")

prices_matrix = matrix(0, length(unique(prices$Time)), length(unique(prices$Date)))

# iterate over each day
for (i in 1:ncol(prices_matrix)) {
  # iterate over each 10-minute interval
  for (j in 1:nrow(prices_matrix)) {
    prices_matrix[j, i] <- prices$AAPL[(i-1)*nrow(prices_matrix) + j]
  }
}

matplot(prices_matrix, type="l")

dim(prices_matrix)

AAPLNovPricesMat = as.matrix(prices_matrix)

length(unique(prices$Date))

dimnames(AAPLNovPricesMat)[[2]] <- paste('b', unique(prices$Date), sep='')

# plot specific functions




# smooth the intra-day pricing observations

nbasis = 40
norder = 3

PriceTime = 0:39;
PricesRng = c(0,39);

PriceBasis = create.bspline.basis(PricesRng, nbasis, norder)

D2fdPar = fdPar(PriceBasis, lambda=1e-7)

AAPLNovPricesMatfd = smooth.basis(PriceTime, AAPLNovPricesMat, D2fdPar)$fd


# view indivudal fit

plotfit.fd(AAPLNovPricesMat, PriceTime, AAPLNovPricesMatfd)

# Set up regression coefficients

nbasis     = 23
SwedeRng   = c(0,39)
PricesBetaBasis = create.bspline.basis(SwedeRng,nbasis, norder=4)

PricesBeta0Par = fdPar(PricesBetaBasis, 2, 1e-5)

PricesBeta1fd  = bifd(matrix(0,23,23), PricesBetaBasis, PricesBetaBasis)

PricesBeta1Par = bifdPar(PricesBeta1fd, 2, 2, 1e3, 1e3)

PricesBetaList = list(PricesBeta0Par, PricesBeta1Par)

#  Define the dependent and independent variable objects

NextYear = AAPLNovPricesMatfd[2:20] # Y(t)
LastYear = AAPLNovPricesMatfd[1:19] # X(t)

#  Do the regression analysis

Prices.linmod = linmod(NextYear, LastYear, PricesBetaList)

Prices.times = seq(0, 39, 2)
Prices.beta1mat = eval.bifd(Prices.times, Prices.times, Prices.linmod$beta1estbifd)

persp(Prices.times, Prices.times, Prices.beta1mat,
      xlab="time", ylab="time",zlab="beta(s,t)",
      cex.lab=1.5,cex.axis=1.5)


predict(Prices.linmod, newdata=AAPLNovPricesMatfd[19:20])
