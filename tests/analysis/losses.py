import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import sys

tests = range(int(sys.argv[1]), int(sys.argv[2]) + 10, int(sys.argv[3]))
mean = []
failure_rate = []
xs = []

for t in tests:
	name = "perdite" + str(t) + ".txt"
	df = pd.read_csv(name, sep=';', header=None, names=['date', 'code', 'time'], on_bad_lines='skip', index_col=False).dropna()
	bad = df[df['code'] != 200]
	mean.append(np.mean(df['time']))
	failure_rate.append(bad.shape[0] / df.shape[0] * 100)
	xs.append(t)
	print(str(t) + " nebula client -- Total " + str(df.shape[0]) + ", bad " + str(bad.shape[0]) + " (" + str(bad.shape[0] / df.shape[0] * 100) + "%) -- avg " + str(np.mean(df['time'])) + "s")

plt.title("Tempi di risposta con perdita di pacchetti")

plt.plot(xs, np.array(mean), linestyle='-', marker='x', color='red')

# plt.yscale('log')
plt.ylabel('tempo medio (s)')
plt.xlabel('% pacchetti persi')

# plt.legend()

plt.savefig("times.pdf")
plt.clf()

xs.append(100)
failure_rate.append(100)
plt.title("Percentuale fallimenti in base alla peredita di pacchetti")

plt.plot(xs, np.array(failure_rate), linestyle='-', marker='x', color='red')

# plt.yscale('log')
plt.ylabel('% fallimenti')
plt.xlabel('% pacchetti persi')

plt.savefig("failures.pdf")
