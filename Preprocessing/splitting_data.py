import pandas as pd

# Split data by dates
def split_data_by_dates(csv_file_path, start_date, end_date, exclude_dates=[]):
    st_date = pd.to_datetime(start_date)
    en_date = pd.to_datetime(end_date)
    df = pd.read_csv(csv_file_path)
    df['TimeStamp'] = pd.to_datetime(df['TimeStamp'])
    mask = (df['TimeStamp'] >= st_date) & (df['TimeStamp'] <= en_date)
    df = df.loc[mask]
    
    df['TimeStamp'] = df['TimeStamp'].dt.strftime('%Y-%m-%d %H:%M:%S')

    # eclude dates
    for date in exclude_dates:
        df = df[df['TimeStamp'].str.contains(date) == False]
        
    save_file_path = csv_file_path.split('.csv')[0] + f'_{start_date}_{end_date}.csv'
    df.to_csv(save_file_path, index=False)

# split_data_by_dates('ProcessedCSVData/2020_SPY_cidr.csv', '2020-06-15', '2020-08-12')