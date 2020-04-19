import sys
from random import randint

p = int(sys.argv[1]) #processes
n = int(sys.argv[2]) #vertices
name = sys.argv[3] #filename
inpdir = sys.argv[4] #inpdir

m = randint(n-1,min(1000000, n*(n-1)/2))
# print(m)
src = randint(1, n)%n + 1

with open(name, "w") as f:
	f.write(str(p) + " " + str(n) + " " + str(src) + "\n")
# print(str(p) + " " + str(n) + " " + str(src))

perfile = 0
if n%p == 0:
	perfile = n/p
else:
	perfile = (n-n%p)/p + 1

adj1 = {}
adj2 = {}

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
		with open(inpdir + "/" + str(cnt1), "w") as f:
			f.write(temp)
		temp = ""
		cnt = 0