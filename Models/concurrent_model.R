library(fda)

equity_returns_matrix_name = "2020_SPY_cidr_2020-06-15_2020-08-11_matrix"
wheat_prices_matrix_name = "WHEAT_prices_2020-06-15_2020-08-11_matrix"
sugar_prices_matrix_name = "SUGAR_prices_2020-06-15_2020-08-11_matrix"
corn_prices_matrix_name = "CORN_prices_2020-06-15_2020-08-11_matrix"
ethereum_prices_matrix_name = "Ether_prices_2020-06-15_2020-08-11_matrix"

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

mat =load(paste("ProcessedRData/", sugar_prices_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)
SugarPricesMat = as.matrix(returns_matrix)

mat =load(paste("ProcessedRData/", corn_prices_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)
CornPricesMat = as.matrix(returns_matrix)


# split into train test sets
EquityReturnsMatTest = EquityReturnsMat[,36:40]
EquityReturnsMat = EquityReturnsMat[,1:35]

EtherPricesMatTest = EtherPricesMat[,36:40]
EtherPricesMat = EtherPricesMat[,1:35]

WheatPricesMatTest = t(matrix(WheatPricesMat[1,36:40]))
WheatPricesMat = t(matrix(WheatPricesMat[,1:35]))

SugarPricesMatTest = t(matrix(SugarPricesMat[1,36:40]))
SugarPricesMat = t(matrix(SugarPricesMat[,1:35]))

CornPricesMatTest = t(matrix(CornPricesMat[1,36:40]))
CornPricesMat = t(matrix(CornPricesMat[,1:35]))

# create smooth curves for each explanatory and response variable

# equity returns (10 min intervals)
ReturnsDayTime = 0:39;
ReturnsDayRng = c(0,39);

ReturnsBasis = create.bspline.basis(ReturnsDayRng, nbasis=40, norder=4)

D2fdPar = fdPar(ReturnsBasis, lambda=0.8)

EquityReturnsMatfd = smooth.basis(ReturnsDayTime, EquityReturnsMat, D2fdPar)$fd

# plotfit.fd(EquityReturnsMat, ReturnsDayTime, EquityReturnsMatfd)

# ether prices (15 min intervals)
EtherDayTime = seq(0,39, 40/27);
EtherDayRng = c(0,39);

EtherBasis = create.bspline.basis(EtherDayRng, nbasis=40, norder=3)

D2fdPar = fdPar(EtherBasis, lambda=1)

EtherPricesMatfd = smooth.basis(EtherDayTime, EtherPricesMat, D2fdPar)$fd

# plotfit.fd(EtherPricesMat, EtherDayTime, EtherPricesMatfd)

plot(EtherPricesMatfd)

# wheat prices
CommodityConstantBasis = create.constant.basis(c(0,39))

WheatPricesMatfd = fd(WheatPricesMat, CommodityConstantBasis)

plot(WheatPricesMatfd)

# corn prices
CornPricesMatfd = fd(CornPricesMat, CommodityConstantBasis)

plot(CornPricesMatfd)

# sugar prices
SugarPricesMatfd = fd(SugarPricesMat, CommodityConstantBasis)

plot(SugarPricesMatfd)

# Set up a  functional linear regression

xfdlist   = list(const=rep(1,35), eth=EtherPricesMatfd, wheat=WheatPricesMatfd, corn=CornPricesMatfd, sugar=SugarPricesMatfd)
betafdPar = fdPar(ReturnsBasis)
betalist  = list(const=betafdPar, eth=betafdPar, wheat=betafdPar, corn=betafdPar, sugar=betafdPar)

fRegressList= fRegress(EquityReturnsMatfd, xfdlist, betalist)

# Intercept

betaestlist = fRegressList$betaestlist
returnIntercept = predict(betaestlist$const$fd, seq(0,39,1))

matplot(returnIntercept, type="l")

returnBeta1 = predict(fRegressList$betaestlist$eth$fd, seq(0,39,1))

matplot(returnBeta1, type="l")

returnBeta2 = predict(fRegressList$betaestlist$wheat$fd, seq(0,39,1))

matplot(returnBeta2, type="l")

# viewing fit
mult_test <- (EtherPricesMatfd * fRegressList$betaestlist$eth$fd) + (WheatPricesMatfd * fRegressList$betaestlist$wheat$fd) + (CornPricesMatfd * fRegressList$betaestlist$corn$fd) + (SugarPricesMatfd * fRegressList$betaestlist$sugar$fd) 

curve <- betaestlist$const$fd + mult_test

c = eval.fd(seq(0, 39, 1), curve)
matplot(EquityReturnsMat[,9:12], type="l")
matplot(c, type="l", add=TRUE)

# prediction

# create fd objects for test data
EtherPricesMatTestfd = smooth.basis(EtherDayTime, EtherPricesMatTest, D2fdPar)$fd
WheatPricesMatTestfd = fd(WheatPricesMatTest, CommodityConstantBasis)
CornPricesMatTestfd = fd(CornPricesMatTest, CommodityConstantBasis)
SugarPricesMatTestfd = fd(SugarPricesMatTest, CommodityConstantBasis)

# mult_test <- (EtherPricesMatfd[,n] * fRegressList$betaestlist$eth$fd) + (WheatPricesMatfd[,n] * fRegressList$betaestlist$wheat$fd)
# mult_test <- (EtherPricesMatTestfd * fRegressList$betaestlist$eth$fd) + (WheatPricesMatTestfd * fRegressList$betaestlist$wheat$fd)
mult_test <- (EtherPricesMatTestfd * fRegressList$betaestlist$eth$fd) + (WheatPricesMatTestfd * fRegressList$betaestlist$wheat$fd) + (CornPricesMatTestfd * fRegressList$betaestlist$corn$fd) + (SugarPricesMatTestfd * fRegressList$betaestlist$sugar$fd) 

# mult_test <- (EtherPricesMatfd * fRegressList$betaestlist$eth$fd)
curve <- betaestlist$const$fd + mult_test

c = eval.fd(seq(0, 39, 1), curve)
matplot(EquityReturnsMatTest, type="l")
matplot(c, type="l", add=TRUE)
