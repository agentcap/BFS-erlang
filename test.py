# python test.py <vertices> <edges> <r> <c> <inpdir>
# (inp1D, inp1D_m), (inp2D, inp2D_m_n), (inpSeqMeta, inpSeqAdj) (inpSrdMeta, inpSrdAdj) filenames respectively

import sys
from random import randint
from math import ceil
import os


## Common arguments
n = int(sys.argv[1])
m = int(sys.argv[2])
# m = randint(n-1,min(1000000, n*(n-1)/2))
r = int(sys.argv[3])
c = int(sys.argv[4])
p = r*c
inpdir = sys.argv[5]
src = randint(1, n)%n + 1


if not os.path.exists(inpdir):
	os.mkdir(inpdir)

## Filenames
f1D = 'inp1D'
f2D = 'inp2D'
fSrd1 = 'inpSrdMeta'
fSrd2 = 'inpSrdAdj'
fSeq1 = 'inpSeqMeta'
fSeq2 = 'inpSeqAdj'

## 1D test case generation

with open(inpdir + "/" + f1D, "w") as f:
	f.write(str(p) + " " + str(n) + " " + str(src) + "\n")

perfile = 0
if n%p == 0:
	perfile = n/p
else:
	perfile = (n-n%p)/p + 1

adj1 = {}
adj2 = {}
proc = [[{} for i in range(c+1)] for i in range(r+1)]

def find_column(b, r):
	return int(ceil(b/float(r)))

def find_row(a, r, c, mx):
	return int((ceil(a/float(mx))-1))%r + 1

mx = 0
my = 0

if n%(r*c) == 0:
	mx = n/(r*c)
else:
	mx = (n - n%(r*c))/(r*c) + 1

if n%c == 0:
	my = n/c
else:
	my = (n - n%c)/c + 1

for i in range(n):
	adj1[i+1] = []

for i in range(m):
	a = randint(1, n)%n + 1
	b = randint(1, n)%n + 1
	while a == b or str(a) + "_" + str(b) in adj2:
		a = randint(1, n)%n + 1
		b = randint(1, n)%n + 1
	adj2[str(a) + "_" + str(b)] = 1
	adj2[str(b) + "_" + str(a)] = 1
	adj1[a].append(b)
	adj1[b].append(a)
	zy = find_column(b, my)
	zx = find_row(a, r, c, mx)
	if a not in proc[zx][zy]:
		proc[zx][zy][a] = {}
	proc[zx][zy][a][b] = 1

	if b not in proc[zx][zy]:
		proc[zx][zy][b] = {}
	proc[zx][zy][b][a] = 1

	zy = find_column(a, my)
	zx = find_row(b, r, c, mx)
	if a not in proc[zx][zy]:
		proc[zx][zy][a] = {}
	proc[zx][zy][a][b] = 1

	if b not in proc[zx][zy]:
		proc[zx][zy][b] = {}
	proc[zx][zy][b][a] = 1

cnt = 0
cnt1 = 0
temp = ""
for i in range(n):
	temp += str(i+1)
	# temp += " " + str(len(adj1[i+1]))
	for j in range(len(adj1[i+1])):
		temp += " " + str(adj1[i+1][j])
	cnt = cnt + 1
	temp += "\n"
	if cnt == perfile or i == n-1:
		cnt1 += 1
		with open(inpdir + "/" + f1D + "_" + str(cnt1), "w") as f:
			f.write(temp)
		temp = ""
		cnt = 0



## 2D test case generation

with open(inpdir + "/" + f2D, "w") as f:
	temp = str(r) + " " + str(c) + " " + str(mx) + " " + str(src) + "\n"
	f.write(temp)

for i in range(1,r+1):
	for j in range(1,c+1):
		tmp = ""
		for key in proc[i][j]:
			tmp += str(key)
			for val in proc[i][j][key]:
				tmp += " " + str(val)
			tmp += "\n"
		with open(inpdir + "/" + f2D + "_" + str(i) + "_" + str(j), 'w') as f:
			f.write(tmp)

## Shared test case generation

tmp1 = ""
tmp1 += str(p) + " " + str(src) + "\n"
with open(inpdir + "/" + fSrd1, "w") as f:
	f.write(tmp1)

tmp2 = ""
for key in adj1:
	tmp2 += str(key)
	for ele in adj1[key]:
		tmp2 += " " + str(ele)
	tmp2 += "\n"

with open(inpdir + "/" + fSrd2, "w") as f:
	f.write(tmp2)


## Seq test case generation

with open(inpdir + "/" + fSeq1, "w") as f:
	f.write(str(src) + "\n")

with open(inpdir + "/" + fSeq2, "w") as f:
	f.write(tmp2)