import pandas as pd

def convert_prices_to_returns(prices: pd.DataFrame) -> pd.DataFrame:
    prices[prices.columns[1]] = prices[prices.columns[1]].pct_change()
    return prices

aapl_prices_df = pd.read_csv('ProcessedData/2020_AAPL_november.csv')
print(aapl_prices_df.columns)
aapl_prices_df = convert_prices_to_returns(aapl_prices_df)
aapl_prices_df.to_csv('ProcessedData/2020_AAPL_november_returns.csv', index=False)