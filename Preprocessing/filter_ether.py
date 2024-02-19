import pandas as pd
from splitting_data import split_data_by_dates

def filter_ether(path, start_date, end_date):
    df = pd.read_csv(path)

    # rename data column to TimeStamp and open column to Ether
    df.rename(columns={'date': 'TimeStamp', 'Open': 'Ether'}, inplace=True)
    df['TimeStamp'] = pd.to_datetime(df['TimeStamp'])
    
    # drop all other columns
    df = df[['TimeStamp', 'Ether']]

    df = df[(df['TimeStamp'].dt.weekday < 5) &
            (df['TimeStamp'].dt.time >= pd.to_datetime('9:30').time()) &
            (df['TimeStamp'].dt.time <= pd.to_datetime('16:00').time())]
    
    save_path = f'ProcessedCSVData/Ether_prices.csv'
    df.to_csv(save_path, index=False)
    split_data_by_dates(save_path, start_date, end_date)

filter_ether('RawData/CryptoPricing/ether_intraday_prices.csv', '2020-08-03', '2020-09-30')