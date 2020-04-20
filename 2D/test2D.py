# python test2D.py <vertices> <r> <c> <filename> <inpdir>
# metafile <r> <c> <mx> <source>
import sys
from random import randint

def find_column(b, r):
	if b % r == 0:
		return b/r
	else:
		return (b - b%r)/r + 1

def find_row(a, r, c):
	tmp = 0
	if a % c == 0:
		tmp = a/c
	else:
		tmp = (a - a%c)/c + 1
	return a - (tmp-1)*r

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
	zx = find_row(a, r, c)
	if a not in proc[zx][zy]:
		proc[zx][zy][a] = {}
	proc[zx][zy][a][b] = 1

	if b not in proc[zx][zy]:
		proc[zx][zy][b] = {}
	proc[zx][zy][b][a] = 1

	zy = find_column(a, my)
	zx = find_row(b, r, c)
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
		with open(inpdir + "/" + name + "_" + str(i) + "_" + str(j), 'w') as f:
			f.write(tmp)