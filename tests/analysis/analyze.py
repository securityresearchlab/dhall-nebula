import warnings

import numpy as np
import pandas as pd
import sys

warnings.simplefilter(action='ignore', category=pd.errors.ParserWarning)

# try:
df = pd.read_csv(sys.argv[1], sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
# except:
#     print("error")

# if df is None or df.empty:
#     df = pd.read_csv(sys.argv[1], sep=';', header=None, names=['code', 'time'], on_bad_lines='skip', index_col=False).dropna()

# print(df.describe())

good = df[df['code'] == 200]
bad = df[df['code'] == 0]

mean_good = np.mean(good['time'].values)
mean_all = np.mean(df['time'].values)
std_good = np.std(good['time'].values)
std_all = np.std(df['time'].values)
perc = 100 * good.shape[0] / df.shape[0]

print(sys.argv[1] + ": "
        + str(df.shape[0]) + ' requests, goods were '
        + str(perc)
        + '%; avg good time ' + str(mean_good)
        + 's; avg time ' + str(mean_all)
        + 's; std good time ' + str(std_good)
        + 's; std time ' + str(std_all)
    )
