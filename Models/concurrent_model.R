library(fda)

equity_returns_matrix_name = "2020_SPY_returns_2020-08-01_2020-10-01_matrix"

# data processing
mat =load(paste("ProcessedRData/", equity_returns_matrix_name, ".RData", sep=""))
mat=as.matrix(returns_matrix)

EquityReturnsMat = as.matrix(returns_matrix)



