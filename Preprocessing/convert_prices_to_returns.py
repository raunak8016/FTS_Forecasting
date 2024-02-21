import pandas as pd
import numpy as np

def convert_prices_to_returns(prices: pd.DataFrame) -> pd.DataFrame:
    prices['returns'] = 0.0
    for index, row in prices.iterrows():
        if index == 0:
            continue
        else:
            prices.at[index, 'returns'] = (prices.at[index, prices.columns[1]] - prices.at[index-1, prices.columns[1]]) / prices.at[index-1, prices.columns[1]]
    prices = prices.drop(prices.columns[1], axis=1)
    return prices

def convert_prices_to_cidr(prices: pd.DataFrame) -> pd.DataFrame:
    prices['CIDR'] = 0.0
    # Add a temporary date column
    prices['Date'] = pd.to_datetime(prices.iloc[:,0]).dt.date 

    # Iterate over each unique date
    for date in prices['Date'].unique():
        first_value = prices[prices['Date'] == date].iloc[0, 1] 
        
        # Iterate over each row for the current date
        for _, row in prices[prices['Date'] == date].iterrows():
            time = row.iloc[0] 

            if pd.to_datetime(time) == pd.to_datetime(str(date) + ' 09:30:00'):
                continue
            
            # Calculate CIDR using index position
            current_price = row.iloc[1]  # Accesses the 'Ether' price
            prices.loc[prices['TimeStamp'] == time, 'CIDR'] = 100 * (np.log(current_price) - np.log(first_value))
    
    prices = prices.drop(prices.columns[1], axis=1)
    prices.drop(columns=['Date'], inplace=True)
    return prices

convert_prices_to_cidr(pd.read_csv('ProcessedCSVData/2020_SPY_prices.csv')).to_csv('ProcessedCSVData/2020_SPY_cidr.csv', index=False)

# aapl_prices_df = pd.read_csv('ProcessedCSVData/2020_AAPL_november.csv')
# print(aapl_prices_df.columns)
# aapl_prices_df = convert_prices_to_returns(aapl_prices_df)
# aapl_prices_df.to_csv('ProcessedCSVData/2020_AAPL_november_returns.csv', index=False)