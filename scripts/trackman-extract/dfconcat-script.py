import pandas as pd
import os

folder_path = '2024NewTmData'

csv_files = [f for f in os.listdir(folder_path) if f.endswith('.csv')]

months = []

for file in csv_files:
    file_path = os.path.join(folder_path, file)
    df = pd.read_csv(file_path)
    months.append(df)

concatenated_df = pd.concat(months, ignore_index=True)

concatenated_df.to_csv('2024TMDataWhole.csv', index=False)
