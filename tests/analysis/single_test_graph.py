import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

def img(df, maxT, title, name):
	mean = [np.mean(df['time'])]*df.shape[0]
	_, ax = plt.subplots()
	plt.title(title)
	ax.set_ylim([0, maxT])
	ax.scatter(df['date'], df['time'], marker='o', s=3)
	ax.plot(df['date'], mean, label='Media', linestyle='--', color='red')
	plt.ylabel('time (s)')
	plt.xlabel('timestamp')
	plt.savefig(name + ".png")

nameWN = "all" + sys.argv[1] + ".txt"
nameNN = "all" + sys.argv[1] + "nn.txt"

dfWN = pd.read_csv(nameWN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
dfNN = pd.read_csv(nameNN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()

maxT = max(dfWN['time'].max(), dfNN['time'].max())

goodWN = dfWN[dfWN['code'] != 0]
badWN = dfWN[dfWN['code'] == 0]

img(dfWN, maxT, 'Risultati test con ' + sys.argv[1] + ' client', nameWN)
img(dfNN, maxT, 'Risultati test senza Nebula con ' + sys.argv[1] + ' client', nameNN)
