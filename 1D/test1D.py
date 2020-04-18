import sys
from random import randint

n = int(sys.argv[2]) #vertices
p = int(sys.argv[1]) #processes

m = randint(n-1,min(1000000, n*(n-1)/2))
# print(m)
src = randint(1, n)%n + 1

print(str(p) + " " + str(n) + " " + str(src))

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

for i in range(n):
	temp = str(i+1)
	# temp += " " + str(len(adj1[i+1]))
	for j in range(len(adj1[i+1])):
		temp += " " + str(adj1[i+1][j])
	print(temp)