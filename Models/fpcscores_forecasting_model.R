library("ftsa")

mat =load("ProcessedRData/prices_matrix.Rdata")
mat =as.matrix("prices_matrix")
colnames(prices_matrix) <- c(1:19)

fts_prices <- fts(c(1:40), prices_matrix, xname="Time", yname="Return")

f <- ftsm(fts_prices, order=4)

plot(forecast(ftsm(fts_prices, order=2), h=20), "components")
plot(fts_prices, col = gray(0.8), xlab = "Time",
     ylab = "Percentage Return",
     main = "Forecasted return")
plot(forecast(ftsm(fts_prices, order=4), h = 20), add = TRUE)
legend("topright", c("20", "39"), col = c("red", "blue"), lty = 1)

plot(fts_prices, col = gray(0.8), xlab = "Time",
     ylab = "Percentage Return",
     main = "Forecasted return")
# Plot the forecasts in rainbow color for Fig. 4(b)
plot(ftsmiterativeforecasts(fts_prices, components = 4, iteration = 20),
     add = TRUE)
legend("topright", c("20", "39"), col = c("red", "blue"), lty = 1)
