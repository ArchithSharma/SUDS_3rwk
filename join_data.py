# Archive file, now is done in edmr_downloader.py

import pandas as pd
import glob

df = pd.concat((pd.read_csv(f) for f in glob.glob("edmr_csvs/*.csv")), ignore_index=True)
df.to_csv("edmr_csvs/combined_data.csv", index=False)