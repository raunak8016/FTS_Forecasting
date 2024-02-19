import pandas as pd

path = 'RawData/KagglePricingData/Zipped data 2020/TOS Kaggle data week ending 2020 08 14.csv'
df = pd.read_csv(path)

for index in df.index:
    time = df.at[index, 'TimeStamp'].split(' ')
    date = time[0].split('/')
    month, day, year = date
    year = '20' + year

    formatted_time = f'{year}-{month.zfill(2)}-{day.zfill(2)}' + f' {time[1]}'
    df.at[index, 'TimeStamp'] = formatted_time

df.to_csv(path, index=False)