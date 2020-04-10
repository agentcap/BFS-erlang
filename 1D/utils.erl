-module('utils').
-export([print_graph/1]).

print_graph([]) ->
	ok;
print_graph([Elem | AdjList]) -> 
	{Vertex, List} = Elem,
	io:format("Vertex ~w: ~w\n",[Vertex, List]),
	print_graph(AdjList).