import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

tests = range(int(sys.argv[1]), int(sys.argv[2]) + 50, int(sys.argv[3]))
meanWNs = []
meanNNs = []
goodMeanWNs = []
goodMeanNNs = []
goodWNs = []
goodNNs = []
badWNs = []
badNNs = []
xs = []

for t in tests:
	nameWN = "all" + str(t) + ".txt"
	nameNN = "all" + str(t) + "nn.txt"
	dfWN = pd.read_csv(nameWN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
	dfNN = pd.read_csv(nameNN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
	goodWN = dfWN[dfWN['code'] != 0]
	goodNN = dfNN[dfNN['code'] != 0]
	badWN = dfWN[dfWN['code'] == 0]
	badNN = dfNN[dfNN['code'] == 0]
	meanWNs.append([np.mean(dfWN['time'])])
	meanNNs.append([np.mean(dfNN['time'])])
	goodMeanWNs.append([np.mean(goodWN['time'])])
	goodMeanNNs.append([np.mean(goodNN['time'])])
	goodWNs.append(goodWN.shape[0] / dfWN.shape[0] * 100)
	goodNNs.append(goodNN.shape[0] / dfNN.shape[0] * 100)
	badWNs.append(badWN.shape[0] / dfWN.shape[0] * 100)
	badNNs.append(badNN.shape[0] / dfNN.shape[0] * 100)
	xs.append(t)
	print(str(t) + " nebula client -- Total " + str(dfWN.shape[0]) + ", good " + str(goodWN.shape[0]) + " (" + str(goodWN.shape[0] / dfWN.shape[0] * 100) + "%)")
	print(str(t) + " non nebula client -- Total " + str(dfNN.shape[0]) + ", good " + str(goodNN.shape[0]) + " (" + str(goodNN.shape[0] / dfNN.shape[0] * 100) + "%)")

plt.title("Confronto tra test con e senza Nebula")

plt.plot(xs, np.array(meanWNs), label='Media con Nebula', linestyle='-', marker='x', color='red')
plt.plot(xs, np.array(meanNNs), label='Media senza Nebula', linestyle='-', marker='x', color='blue')

# plt.yscale('log')
plt.ylabel('tempo medio (s)')
plt.xlabel('numero di client')

plt.legend()
plt.savefig("cumulative.png")

plt.clf()
plt.title("Confronto tra test con e senza Nebula (solo successi)")

plt.plot(xs, np.array(goodMeanWNs), label='Media con Nebula', linestyle='-', marker='x', color='red')
plt.plot(xs, np.array(goodMeanNNs), label='Media senza Nebula', linestyle='-', marker='x', color='blue')

# plt.yscale('log')
plt.ylabel('tempo medio (s)')
plt.xlabel('numero di client')

plt.legend()

plt.savefig("cumulative_good.png")

x = np.arange(len(xs))
width = 0.2

fig, ax = plt.subplots()
rects1 = ax.bar(x - 0.3, np.array(goodWNs), width, label='200 con Nebula')
rects2 = ax.bar(x - 0.1, np.array(goodNNs), width, label='200 senza Nebula')
rects3 = ax.bar(x + 0.1, np.array(badWNs), width, label='Fallimenti con Nebula')
rects4 = ax.bar(x + 0.3, np.array(badNNs), width, label='Fallimenti senza Nebula')

# ax.bar_label(rects1, padding=3)
# ax.bar_label(rects2, padding=3)
# ax.bar_label(rects3, padding=3)
# ax.bar_label(rects4, padding=3)

ax.set_ylabel('% richieste')
ax.set_xlabel('Numero client')
ax.set_xticks(x, [str(t) for t in xs])
ax.legend()

# fig.tight_layout()

plt.title("Confronto percentuali successi con e senza Nebula")
plt.savefig("bars.png")
