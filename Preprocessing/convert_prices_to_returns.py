import pandas as pd

def convert_prices_to_returns(prices: pd.DataFrame) -> pd.DataFrame:
    # create a new column for returns
    prices['returns'] = 0.0
    for index, row in prices.iterrows():
        if index == 0:
            continue
        else:
            prices.at[index, 'returns'] = (prices.at[index, prices.columns[1]] - prices.at[index-1, prices.columns[1]]) / prices.at[index-1, prices.columns[1]]
    prices = prices.drop(prices.columns[1], axis=1)
    return prices

aapl_prices_df = pd.read_csv('ProcessedCSVData/2020_AAPL_november.csv')
print(aapl_prices_df.columns)
aapl_prices_df = convert_prices_to_returns(aapl_prices_df)
aapl_prices_df.to_csv('ProcessedCSVData/2020_AAPL_november_returns.csv', index=False)