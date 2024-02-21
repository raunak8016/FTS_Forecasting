import pandas as pd
from splitting_data import split_data_by_dates

def read_prices_filter_by_commodity(path, commodity, start_date, end_date, exclude_dates=[]):
    df = pd.read_csv(path)
    df['TimeStamp'] = pd.to_datetime(df['Date'])
    # change timestamp format to match other data
    df['TimeStamp'] = df['TimeStamp'].dt.strftime('%Y-%m-%d %H:%M:%S')

    # drop all columns except for TimeStamp and commodity price
    df = df[['TimeStamp', commodity]]
    save_path = f'ProcessedCSVData/{commodity}_prices.csv'
    df.to_csv(save_path, index=False)
    split_data_by_dates(save_path, start_date, end_date, exclude_dates)

read_prices_filter_by_commodity('RawData/CommodityPricing/commodity_futures.csv',
                                      'WHEAT',
                                      '2020-06-15', '2020-08-11', ['2020-06-29','2020-07-03'])