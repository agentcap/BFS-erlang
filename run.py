import os

os.system('python test.py 1000 2000 10 10 testcases')

os.system('echo "\nSeq" && cd sequential && erlc *.erl && erl -noshell -s bfs main ../testcases/inpSeqMeta ../testcases/inpSeqAdj time -s init stop && cd ..')

os.system('echo "\nShared" && cd shared && erlc *.erl && erl -noshell -s bfs_shared main ../testcases/inpSrdMeta ../testcases/inpSrdAdj time -s init stop && cd ..')

os.system('echo "\n1D" && cd 1D && erlc *.erl && erl -noshell -s bfs_1d main ../testcases/inp1D ../testcases time -s init stop && cd ..')

os.system('echo "\n2D" && cd 2D && erlc *.erl && erl -noshell -s bfs_2d main ../testcases/inp2D ../testcases time -s init stop && cd ..')