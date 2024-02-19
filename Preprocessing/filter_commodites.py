import pandas as pd
from splitting_data import split_data_by_dates

def read_prices_filter_by_commodity(path, commodity, start_date, end_date):
    df = pd.read_csv(path)
    df['TimeStamp'] = pd.to_datetime(df['Date'])

    # drop all columns except for TimeStamp and commodity price
    df = df[['TimeStamp', commodity]]
    save_path = f'ProcessedCSVData/{commodity}_prices.csv'
    df.to_csv(save_path, index=False)
    split_data_by_dates(save_path, start_date, end_date)

read_prices_filter_by_commodity('RawData/CommodityPricing/commodity_futures.csv',
                                      'WHEAT',
                                      '2020-08-03', '2020-09-30')