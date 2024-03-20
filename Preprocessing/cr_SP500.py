def calc_cr_sp_500(csv_path):
    import pandas as pd
    df = pd.read_csv(csv_path)

    df['TimeStamp'] = pd.to_datetime(df['TimeStamp'])

    # remove all 9:30 timestamps
    df = df[df['TimeStamp'].dt.strftime('%H:%M:%S') != '09:30:00']

    
    # calculate cumulative returns from price data
    df['Cumulative Return'] = (df['Price'] / df['Price'].iloc[0]) -1
    df.drop(columns=['Price'], inplace=True)

    df.to_csv('ProcessedCSVData/2020_SPY_cumulative_returns_2020-07-06_2020-09-02.csv', index=False)


csv_path = "ProcessedCSVData/2020_SPY_prices_july6-sep2.csv"
calc_cr_sp_500(csv_path)