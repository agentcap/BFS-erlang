import matplotlib.pyplot as plt
import os
import numpy as np


def generate_graph(proc, tseq, tsrd, t1d, t2d, title=None):
	plt.plot(proc, tseq, 'r', label='BFS Sequential')
	plt.plot(proc, tsrd, 'g', label='BFS Shared memory')
	plt.plot(proc, t1d, 'b', label='BFS 1d')
	plt.plot(proc, t2d, 'y', label='BFS 2d')
	plt.xlabel('Number of messages per iteration')
	plt.ylabel('Number of processes used')
	plt.title(title)
	plt.legend()
	plt.show()

## message complexity
proc = np.linspace(1, 1000,1000)

tseq = 0/proc
tsrd = proc
t1d = proc*proc
t2d = proc*np.sqrt(proc)

generate_graph(proc, tseq, tsrd, t1d, t2d, title='Message complexity of BFS algorithms')