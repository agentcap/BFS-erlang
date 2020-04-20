-module('utils').
-export([print_graph/1, update_depth/3]).

print_graph([]) ->
	ok;
print_graph([Elem | AdjList]) -> 
	{Vertex, List} = Elem,
	io:format("Vertex ~w: ~w\n",[Vertex, sets:to_list(List)]),
	print_graph(AdjList).

update_depth([], Depth, _) ->
	Depth;
update_depth([V|N], Depth, L) ->
	update_depth(N, lists:keyreplace(V, 1, Depth, {V,L}), L).