% erlc *.erl && erl -noshell -s bfs_1d main inp -s init stop
-module('bfs_1d').
-export([main/1]).

main([InpFile]) ->
	{ok, InpDevice} = file:open(InpFile, [read]),
	{NoProcess, N, AdjList} = input:read_input(InpDevice),
	utils:print_graph(AdjList).