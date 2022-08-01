import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

tests = range(int(sys.argv[1]), int(sys.argv[2]) + 50, int(sys.argv[3]))
meanWNs = []
meanNNs = []
xs = []

for t in tests:
	nameWN = "all" + str(t) + ".txt"
	nameNN = "all" + str(t) + "nn.txt"
	dfWN = pd.read_csv(nameWN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
	dfNN = pd.read_csv(nameNN, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
	meanWNs.append([np.mean(dfWN['time'])])
	meanNNs.append([np.mean(dfNN['time'])])
	xs.append(t)

plt.title("Confronto tra test con e senza Nebula")

plt.plot(xs, np.array(meanWNs), label='Media con Nebula', linestyle='-', marker='x', color='red')
plt.plot(xs, np.array(meanNNs), label='Media con Nebula', linestyle='-', marker='x', color='blue')

plt.ylabel('tempo medio (s)')
plt.xlabel('numero di client')

plt.savefig("cumulative.png")
