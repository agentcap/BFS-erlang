-module('utils').
-export([print_graph/1, ceil/2]).

print_graph([]) ->
	ok;
print_graph([Elem | AdjList]) -> 
	{Vertex, List} = Elem,
	io:format("Vertex ~w: ~w\n",[Vertex, sets:to_list(List)]),
	print_graph(AdjList).

ceil(A,B) ->
	case A rem B of 
		0 ->
			round(A/B);
		Rem -> 
			round((A-Rem)/B) + 1
	end.