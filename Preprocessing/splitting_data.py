import pandas as pd

def split_csv_by_month(csv_file, output_prefix):
    # Read the CSV file
    df = pd.read_csv(csv_file, parse_dates=['TimeStamp'])

    # Filter for November and December
    nov_df = df[(df['TimeStamp'].dt.month == 11) & 
                (df['TimeStamp'].dt.day != 26) &
                (df['TimeStamp'].dt.weekday < 5) & 
                (df['TimeStamp'].dt.time >= pd.to_datetime('9:30').time()) &
                (df['TimeStamp'].dt.time <= pd.to_datetime('16:00').time())]

    dec_df = df[(df['TimeStamp'].dt.month == 12) & 
                (df['TimeStamp'].dt.day != 25) &
                (df['TimeStamp'].dt.weekday < 5) &
                (df['TimeStamp'].dt.time >= pd.to_datetime('9:30').time()) &
                (df['TimeStamp'].dt.time <= pd.to_datetime('16:00').time())]

    nov_df.to_csv(f'{output_prefix}_november.csv', header=True, index=False)
    dec_df.to_csv(f'{output_prefix}_december.csv', header=True, index=False)

    print("Data split into November and December CSV files, excluding weekends, specific dates, and restricting times.") 

split_csv_by_month('FTS_Forecasting\processed_data\AAPL_Prices_November_2020.csv', r'FTS_Forecasting\processed_data\2020_AAPL')
