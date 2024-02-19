import pandas as pd
import os
from datetime import datetime, timedelta

def try_parsing_date(text):
    for fmt in ('%Y-%m-%d %H:%M:%S.%f', 
                '%m/%d/%Y %H:%M', 
                '%Y-%m-%d %H:%M:%S', 
                '%m/%d/%Y %H:%M:%S', 
                '%m/%d/%y', 
                '%Y-%m-%d %H:%M'):  # Added this format to handle '2020-08-13 5:24'
        try:
            return pd.to_datetime(text, format=fmt)
        except ValueError:
            continue
    raise ValueError('no valid date format found')

def read_and_filter_files(directory, stock):
    # delete temp_csv if it exists
    if os.path.exists('ProcessedCSVData/temp.csv'):
        os.remove('ProcessedCSVData/temp.csv')
    # create temp_csv file with headers
    with open('ProcessedCSVData/temp.csv', 'w') as f:
        f.write('TimeStamp,' + stock + '\n')

    if not os.path.exists(directory):
        raise FileNotFoundError(f"The directory {directory} does not exist.")
    index = 0
    length = len(os.listdir(directory))
    for filename in os.listdir(directory):
        if filename.endswith('.csv'):
            file_path = os.path.join(directory, filename)
            try:
                data = pd.read_csv(file_path)

                # drop all columns except 'TimeStamp' and the stock price
                data = data[['TimeStamp', stock]]

                data['TimeStamp'] = data['TimeStamp'].apply(try_parsing_date)

                data = filter_prices(data, stock)

                # append data to temp_csv file
                data.to_csv('ProcessedCSVData/temp.csv', mode='a', header=False, index=False)
                index += 1
                print(f"File {index}/{length} processed successfully.")
            except Exception as e:
                print(f"Error reading {file_path}: {e}")

    ret_df = pd.read_csv('ProcessedCSVData/temp.csv')
    ret_df = ret_df.sort_values(by='TimeStamp')
    # delete temp.csv
    os.remove('ProcessedCSVData/temp.csv')
    
    return ret_df



def filter_prices(df, stock):
    df = df.set_index('TimeStamp')
    df = df[stock].resample('10min').first().dropna().reset_index()
    
    df = df[(df['TimeStamp'].dt.weekday < 5) &
            (df['TimeStamp'].dt.time >= pd.to_datetime('9:30').time()) &
            (df['TimeStamp'].dt.time <= pd.to_datetime('16:00').time())]

    return df