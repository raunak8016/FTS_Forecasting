import pandas as pd

# Split data by dates
def split_data_by_dates(csv_file_path, start_date, end_date):
    st_date = pd.to_datetime(start_date)
    en_date = pd.to_datetime(end_date)
    df = pd.read_csv(csv_file_path)
    df['TimeStamp'] = pd.to_datetime(df['TimeStamp'])
    mask = (df['TimeStamp'] >= st_date) & (df['TimeStamp'] <= en_date)
    df = df.loc[mask]
    
    df['TimeStamp'] = df['TimeStamp'].dt.strftime('%Y-%m-%d %H:%M:%S')

    save_file_path = csv_file_path.split('.csv')[0] + f'_{start_date}_{end_date}.csv'
    df.to_csv(save_file_path, index=False)

# split_data_by_dates('ProcessedCSVData/2020_SPY_returns.csv', '2020-08-03', '2020-09-30')