import pandas as pd
import os
from datetime import datetime, timedelta

def try_parsing_date(text):
    for fmt in ('%Y-%m-%d %H:%M:%S.%f', '%m/%d/%Y %H:%M', '%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S'):
        try:
            return pd.to_datetime(text, format=fmt)
        except ValueError:
            continue
    raise ValueError('no valid date format found')

def is_december_2020(filename):
    try:
        date_str = filename.split('week ending ')[-1].rstrip('.csv')
        file_date = datetime.strptime(date_str, '%Y %m %d')
        return (file_date.year == 2020 and file_date.month == 12) or (file_date.year == 2021 and file_date.month == 1)
    except (ValueError, IndexError):
        return False

def read_and_filter_files(directory):
    all_data = []
    if not os.path.exists(directory):
        raise FileNotFoundError(f"The directory {directory} does not exist.")
    
    for filename in os.listdir(directory):
        if filename.endswith('.csv') and is_december_2020(filename):
            file_path = os.path.join(directory, filename)
            try:
                data = pd.read_csv(file_path)

                data['TimeStamp'] = data['TimeStamp'].apply(try_parsing_date)

                all_data.append(data)
            except Exception as e:
                print(f"Error reading {file_path}: {e}")

    if not all_data:
        raise ValueError("No valid CSV files found for December 2020.")
    return pd.concat(all_data)




def filter_aapl_prices(df):
    df = df.set_index('TimeStamp')
    df = df['AAPL'].resample('10T').first().dropna().reset_index()

    return df

def main():
    directory = 'FTS_Forecasting/data/Zipped data 2020'
    df = read_and_filter_files(directory)
    aapl_data = filter_aapl_prices(df)
    aapl_data.to_csv('FTS_Forecasting/processed_data/AAPL_Prices_November_2020.csv', index=False)
    print("CSV file created successfully.")

if __name__ == "__main__":
    main()