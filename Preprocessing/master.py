import pandas as pd
from convert_prices_to_returns import convert_prices_to_returns
from parsing_raw_data import read_and_filter_files

def create_csv_return_data_by_stock(stock: str, year: int, create_returns_csv=True) -> None:
    directory = f'RawData/KagglePricingData/Zipped data {year}'
    df = read_and_filter_files(directory, stock)

    prices_csv_path = f'ProcessedCSVData/{year}_{stock}_prices.csv'
    df.to_csv(prices_csv_path, index=False)
    print("CSV file created successfully.")

    if create_returns_csv:
        prices_df = pd.read_csv(prices_csv_path)
        prices_df = convert_prices_to_returns(prices_df)
        prices_df.to_csv(f'ProcessedCSVData/{year}_{stock}_returns.csv', index=False)

create_csv_return_data_by_stock('SPY', 2020)

# create_csv_return_data_by_stock('AAPL', 2020)
# create_csv_return_data_by_stock('MSFT', 2020)