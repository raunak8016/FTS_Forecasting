library(fda)

equity_returns_matrix_name = "2020_SPY_returns_2020-08-03_2020-09-30_matrix"
wheat_prices_matrix_name = "WHEAT_prices_2020-08-03_2020-09-30_matrix"
ethereum_prices_matrix_name = "Ether_prices_2020-08-03_2020-09-30_matrix"

# data processing
mat =load(paste("ProcessedRData/", equity_returns_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)
EquityReturnsMat = as.matrix(returns_matrix)

mat =load(paste("ProcessedRData/", wheat_prices_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)
WheatPricesMat = as.matrix(returns_matrix)

mat =load(paste("ProcessedRData/", ethereum_prices_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)
EtherPricesMat = as.matrix(returns_matrix)

# create smooth curves for each explanatory and response variable

# equity returns
ReturnsDayTime = 0:39;
ReturnsDayRng = c(0,39);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis=40, norder=4)

D2fdPar = fdPar(ReturnsBasis, lambda=0.0001)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, EquityReturnsMat, D2fdPar)$fd

plotfit.fd(EquityReturnsMat, ReturnsDayTime, EquityReturnsMatfd)

# ether prices
EtherDayTime = 0:26;
EtherDayRng = c(0,26);

EtherBasis = create.bspline.basis(EtherDayRng, nbasis=27, norder=4)

D2fdPar = fdPar(EtherBasis, lambda=0.001)

EtherPricesMatfd = smooth.basis(EtherDayTime, EtherPricesMat, D2fdPar)$fd

plotfit.fd(EtherPricesMat, EtherDayTime, EtherPricesMatfd)


# Set up a  functional linear regression

xfdlist   = list(const=rep(1,43), eth=EtherPricesMatfd)
betafdPar = fdPar(EtherPricesMatfd)
betalist  = list(const=betafdPar, eth=EtherPricesMatfd)

fRegressList= fRegress(EtherPricesMatfd, xfdlist, betalist)

# Intercept

betaestlist = fRegressList$betaestlist
returnIntercept = predict(betaestlist$const$fd, seq(0,26,len=50))

matplot(returnIntercept, type="l")

kneefdMean <- EtherPricesMat[,43]

kneeMean = predict(kneefdMean, seq(0,26,len=50))
plot(kneeMean, type="l")

