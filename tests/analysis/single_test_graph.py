import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

df = pd.read_csv(sys.argv[1], sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()

good = df[df['code'] != 0]
bad = df[df['code'] == 0]

df.plot(x ='date', y='time', kind = 'scatter', use_index = False, title='Risultati test con ' + sys.argv[2] + ' client')

plt.ylabel('time (s)')
plt.xlabel('timestamp')

plt.savefig(sys.argv[1] + ".png")
