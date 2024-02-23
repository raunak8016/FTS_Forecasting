library("ftsa")

mat =load("ProcessedRData/2020_SPY_cidr_2020-06-15_2020-08-11_matrix.RData")
mat =as.matrix(returns_matrix)
returns_matrix_test = returns_matrix[,36:40]
returns_matrix = returns_matrix[,1:35]
colnames(returns_matrix) <- c(1:35)

fts_prices <- fts(c(1:40), returns_matrix, xname="Time", yname="Return")
matplot(returns_matrix, type="l")
f <- ftsm(fts_prices, order=4)
T_stationary(returns_matrix)
plot(forecast(ftsm(fts_prices, order=4), h=4), "components")
plot(fts_prices, col = gray(0.8), xlab = "Time",
     ylab = "Percentage Return",
     main = "Forecasted return")
plot(forecast(ftsm(fts_prices, order=4), h = 4), add=TRUE)
par(new=TRUE)
plot(returns_matrix_test[,5], type="l")
legend("topright", c("20", "23"), col = c("red", "blue"), lty = 1)

plot(fts_prices, col = gray(0.8), xlab = "Time",
     ylab = "Percentage Return",
     main = "Forecasted return")
# Plot the forecasts in rainbow color for Fig. 4(b)
plot(ftsmiterativeforecasts(fts_prices, components = 4, iteration = 20))
legend("topright", c("20", "23"), col = c("red", "blue"), lty = 1)

