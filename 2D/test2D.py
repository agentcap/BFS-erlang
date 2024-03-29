# python test2D.py <vertices> <r> <c> <filename> <inpdir>
# metafile <r> <c> <mx> <source>
import sys
from random import randint
from math import ceil

def find_column(b, r):
	return int(ceil(b/float(r)))

def find_row(a, r, c, mx):
	return int((ceil(a/float(mx))-1))%r + 1

n = int(sys.argv[1]) #vertices
r = int(sys.argv[2]) #r
c = int(sys.argv[3]) #c
name = sys.argv[4] #filename
inpdir = sys.argv[5] #inpdir

# m = n-1
m = randint(n-1,min(1000000, n*(n-1)/2))
src = randint(1, n)%n + 1
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

with open(name, "w") as f:
	temp = str(r) + " " + str(c) + " " + str(mx) + " " + str(src) + "\n"
	f.write(temp)

adj1 = {}
adj2 = {}
proc = [[{} for i in range(c+1)] for i in range(r+1)]

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

for i in range(1,r+1):
	for j in range(1,c+1):
		tmp = ""
		for key in proc[i][j]:
			tmp += str(key)
			for val in proc[i][j][key]:
				tmp += " " + str(val)
			tmp += "\n"
		with open(inpdir + "/" + str(i) + "_" + str(j), 'w') as f:
			f.write(tmp)

for key in adj1:
	print(key, adj1[key])