import datetime as dt
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

def img(df, good, bad, maxT, title, name):
	mean = [np.mean(df['time'])]*df.shape[0]
	_, ax = plt.subplots()
	plt.title(title)
	ax.set_ylim([0, maxT])
	ax.scatter(good['date'], good['time'], marker='o', s=1, color='blue', label='Successo')
	ax.scatter(bad['date'], bad['time'], marker='o', s=1, color='pink', label='Fallimento')
	ax.plot(df['date'], mean, label='Media', linestyle='--', color='red')
	plt.ylabel('time (s)')
	plt.xlabel('timestamp')
	plt.legend(loc=1)
	plt.savefig(name + ".png")
	plt.savefig(name + ".pdf")

nameWN = "all" + sys.argv[1] + ".txt"
nameNN = "all" + sys.argv[1] + "nn.txt"

dfWN = pd.read_csv(nameWN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
dfNN = pd.read_csv(nameNN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()

dfWN['date'] = dfWN['date'].map(dt.datetime.fromtimestamp)
dfNN['date'] = dfNN['date'].map(dt.datetime.fromtimestamp)

# print(dfWN.dtypes)

maxT = max(dfWN['time'].max(), dfNN['time'].max())

goodWN = dfWN[dfWN['code'] == 200]
badWN = dfWN[dfWN['code'] == 0]

goodNN = dfNN[dfNN['code'] == 200]
badNN = dfNN[dfNN['code'] == 0]

img(dfWN, goodWN, badWN, maxT, 'Risultati test con ' + sys.argv[1] + ' client', nameWN)
img(dfNN, goodNN, badNN, maxT, 'Risultati test senza Nebula con ' + sys.argv[1] + ' client', nameNN)
