library(fda)

mat =load("ProcessedRData/prices_matrix.Rdata")
mat=as.matrix(prices_matrix)

matplot(prices_matrix, type="l")

