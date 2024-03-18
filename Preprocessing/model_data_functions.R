library(lubridate)

save_returns_data_matrix <- function(fileName) {
  df = read.csv(paste("ProcessedCSVData/", fileName, sep=""), stringsAsFactors = FALSE)
  df$returns <- as.numeric(df[,2])
  
  
  
  summary(df)
  
  any(is.na(df))
  
  matplot(df$returns, type="l")
  
  df$TimeStamp <- ymd_hms(df$TimeStamp)
  df$Date <- as.Date(df$TimeStamp)
  df$Time <- format(df$TimeStamp, "%H:%M:%S")
  
  returns_matrix = matrix(0, length(unique(df$Time)), length(unique(df$Date)))
  
  # iterate over each day
  for (i in 1:ncol(returns_matrix)) {
    # iterate over each minute interval
    for (j in 1:nrow(returns_matrix)) {
      returns_matrix[j, i] <- df$returns[(i-1)*nrow(returns_matrix) + j]
    }
  }
  
  matplot(returns_matrix, type="l")
  
  dimnames(returns_matrix)[[2]] <- paste('b', unique(df$Date), sep='')
  
  save(returns_matrix, file=paste("ProcessedRData/", sub('\\.csv$', '', fileName), "_matrix.RData", sep=""))
  
  
}
save_returns_data_matrix(fileName="2020_SPY_prices_2020-06-15_2020-08-11.csv")

save_returns_data_matrix(fileName="Ether_prices_2020-06-15_2020-08-11.csv")
# 
save_returns_data_matrix(fileName="WHEAT_prices_2020-06-15_2020-08-11.csv")
save_returns_data_matrix(fileName="CORN_prices_2020-06-15_2020-08-11.csv")
save_returns_data_matrix(fileName="SUGAR_prices_2020-06-15_2020-08-11.csv")
save_returns_data_matrix(fileName="GASOLINE_prices_2020-06-15_2020-08-11.csv")
