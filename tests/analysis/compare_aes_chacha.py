import datetime as dt
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

nameAES = "all" + sys.argv[1] + ".txt"
nameChacha = "all" + sys.argv[1] + "chacha.txt"

dfAES = pd.read_csv(nameAES, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
dfChacha = pd.read_csv(nameChacha, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()

dfAES['date'] = dfAES['date'].map(dt.datetime.fromtimestamp)
dfChacha['date'] = dfChacha['date'].map(dt.datetime.fromtimestamp)

maxT = max(dfAES['time'].max(), dfChacha['time'].max())

goodAES = dfAES[dfAES['code'] == 200]
badAES = dfAES[dfAES['code'] != 200]

goodChacha = dfChacha[dfChacha['code'] == 200]
badChacha = dfChacha[dfChacha['code'] != 200]

meanAES = [np.mean(dfAES['time'])]*dfAES.shape[0]
meanChacha = [np.mean(dfChacha['time'])]*dfChacha.shape[0]
_, ax = plt.subplots()
plt.title("Confronto tra uso di AES e Chachapoly con 50 client")
ax.set_yscale('log')
# ax.set_ylim([0, maxT])
ax.scatter(dfAES.index, dfAES['time'], marker='o', s=1, color='lightblue', label='AES')
ax.scatter(dfChacha.index, dfChacha['time'], marker='o', s=1, color='pink', label='Chachapoly')
ax.plot(dfAES.index, meanAES, label='Media AES', linestyle='-', color='blue')
ax.plot(dfChacha.index, meanChacha, label='Media Chachapoly', linestyle='--', color='red')
plt.ylabel('time (s)')
plt.xlabel('timestamp')
plt.legend(loc=1)
plt.savefig(sys.argv[1] +"_ciphers.pdf")
