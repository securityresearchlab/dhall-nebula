import numpy as np
import pandas as pd
import sys

df = pd.read_csv(sys.argv[1], sep=';', header=None, names=['date', 'code', 'time'])

good = df[df['code'] != 0]
bad = df[df['code'] == 0]

mean = np.mean(good['time'].to_numpy())
perc = 100 * good.shape[0] / df.shape[0]

print(str(good.shape[0]) + ' good requests, over a total of ' + str(df.shape[0]) + ': ' + str(perc) + '%, with average time ' + str(mean) + 's')
