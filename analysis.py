import matplotlib.pyplot as plt
import os


def generate_graph(proc, tseq, tsrd, t1d, t2d, title=None):
	plt.plot(proc, tseq, 'r', label='BFS Sequential')
	plt.plot(proc, tsrd, 'g', label='BFS Shared memory')
	plt.plot(proc, t1d, 'b', label='BFS 1d')
	plt.plot(proc, t2d, 'y', label='BFS 2d')
	plt.xlabel('Average execution time')
	plt.ylabel('Number of processes used')
	plt.title(title)
	plt.legend()
	plt.show()

## x: processes, y: average execution time, variation of different BFS algorithms for same given graph.
rp = [1, 5, 5, 5, 6, 10, 10, 10]
cp = [1, 1, 2, 4, 5, 5, 8, 10]
proc = [1, 5, 10, 20, 30, 50, 80, 100]



v1 = 10
k1 = 2
t1d = [1,2,3,4,5,6,1,2]
t2d = [6,5,3,3,2,1,1,4]
tseq = [2,3,4,5,1,2,1,5]
tsrd = [3,1,2,3,4,5,1,1]
# for i in range(len(proc)):
# 	print(i, proc[i])
# 	print('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('cd sequential && erlc *.erl && erl -noshell -s bfs main ../analysis/inpSeqMeta ../analysis/inpSeqAdj -s init stop > ../analysis/time.txt && cd ..')
# 	os.system('cd shared && erlc *.erl && erl -noshell -s bfs_shared main ../analysis/inpSrdMeta ../analysis/inpSrdAdj -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 1D && erlc *.erl && erl -noshell -s bfs_1d main ../analysis/inp1D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 2D && erlc *.erl && erl -noshell -s bfs_2d main ../analysis/inp2D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	with open('analysis/time.txt', 'r') as f:
# 		times = f.readlines()
# 		tseq.append(times[0].strip('\n'))
# 		tsrd.append(times[1].strip('\n'))
# 		t1d.append(times[2].strip('\n'))
# 		t2d.append(times[3].strip('\n'))

print(tseq, tsrd, t1d, t2d)
generate_graph(proc, tseq, tsrd, t1d, t2d, 'Vertices=' + str(v1) + ', Degree=' + str(k1))

# v2 = 10
# k2 = 20
# t1d = []
# t2d = []
# tseq = []
# tsrd = []
# for i in range(len(proc)):
# 	print(i, proc[i])
# 	print('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('cd sequential && erlc *.erl && erl -noshell -s bfs main ../analysis/inpSeqMeta ../analysis/inpSeqAdj -s init stop > ../analysis/time.txt && cd ..')
# 	os.system('cd shared && erlc *.erl && erl -noshell -s bfs_shared main ../analysis/inpSrdMeta ../analysis/inpSrdAdj -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 1D && erlc *.erl && erl -noshell -s bfs_1d main ../analysis/inp1D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 2D && erlc *.erl && erl -noshell -s bfs_2d main ../analysis/inp2D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	with open('analysis/time.txt', 'r') as f:
# 		times = f.readlines()
# 		tseq.append(times[0].strip('\n'))
# 		tsrd.append(times[1].strip('\n'))
# 		t1d.append(times[2].strip('\n'))
# 		t2d.append(times[3].strip('\n'))

# print(tseq, tsrd, t1d, t2d)
# generate_graph(proc, tseq, tsrd, t1d, t2d)

# v3 = 100
# k3 = 2
# t1d = []
# t2d = []
# tseq = []
# tsrd = []
# for i in range(len(proc)):
# 	print(i, proc[i])
# 	print('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('cd sequential && erlc *.erl && erl -noshell -s bfs main ../analysis/inpSeqMeta ../analysis/inpSeqAdj -s init stop > ../analysis/time.txt && cd ..')
# 	os.system('cd shared && erlc *.erl && erl -noshell -s bfs_shared main ../analysis/inpSrdMeta ../analysis/inpSrdAdj -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 1D && erlc *.erl && erl -noshell -s bfs_1d main ../analysis/inp1D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 2D && erlc *.erl && erl -noshell -s bfs_2d main ../analysis/inp2D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	with open('analysis/time.txt', 'r') as f:
# 		times = f.readlines()
# 		tseq.append(times[0].strip('\n'))
# 		tsrd.append(times[1].strip('\n'))
# 		t1d.append(times[2].strip('\n'))
# 		t2d.append(times[3].strip('\n'))

# print(tseq, tsrd, t1d, t2d)
# generate_graph(proc, tseq, tsrd, t1d, t2d)

# v4 = 100
# k4 = 200
# t1d = []
# t2d = []
# tseq = []
# tsrd = []
# for i in range(len(proc)):
# 	print(i, proc[i])
# 	print('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('python test.py ' +  str(v1) + ' ' + str(v1*k1) + ' ' + str(rp[i]) + ' ' + str(cp[i]) +  ' analysis')
# 	os.system('cd sequential && erlc *.erl && erl -noshell -s bfs main ../analysis/inpSeqMeta ../analysis/inpSeqAdj -s init stop > ../analysis/time.txt && cd ..')
# 	os.system('cd shared && erlc *.erl && erl -noshell -s bfs_shared main ../analysis/inpSrdMeta ../analysis/inpSrdAdj -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 1D && erlc *.erl && erl -noshell -s bfs_1d main ../analysis/inp1D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	os.system('cd 2D && erlc *.erl && erl -noshell -s bfs_2d main ../analysis/inp2D ../analysis -s init stop >> ../analysis/time.txt && cd ..')
# 	with open('analysis/time.txt', 'r') as f:
# 		times = f.readlines()
# 		tseq.append(times[0].strip('\n'))
# 		tsrd.append(times[1].strip('\n'))
# 		t1d.append(times[2].strip('\n'))
# 		t2d.append(times[3].strip('\n'))

# print(tseq, tsrd, t1d, t2d)
# generate_graph(proc, tseq, tsrd, t1d, t2d)