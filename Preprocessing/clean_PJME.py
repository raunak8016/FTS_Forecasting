import pandas as pd

def clean_PJME(path, start_year, end_year, end_month=3):
    df = pd.read_csv(path)


    # sort df
    df = df.sort_values(by='Datetime')

    # filter by start and end year
    df['Datetime'] = pd.to_datetime(df['Datetime'])
    df = df[(df['Datetime'].dt.year >= start_year) & (df['Datetime'].dt.year < end_year) & (df['Datetime'].dt.month < end_month)]

    save_path = f'ProcessedCSVData/PJME_prices.csv'
    df.to_csv(save_path, index=False)

clean_PJME('RawData/PJME_hourly.csv', 2010, 2011)