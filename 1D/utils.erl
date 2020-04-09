-module('utils').
-export([print_graph/1]).

print_graph([]) ->
	ok;
print_graph([Elem | AdjList]) -> 
	{Vertex, List} = Elem,
	io:format("Vertex ~w: ",[Vertex]),
	print_list(List),
	io:format("\n"),
	print_graph(AdjList).

print_list([]) ->
	ok; 
print_list([Num|List]) ->
	io:format("~w ",[Num]),
	print_list(List).