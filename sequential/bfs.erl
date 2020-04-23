% erlc *.erl && erl -noshell -s bfs meta input depth -s init stop

-module('bfs').
-export([main/1]).

main([MetaFile, InputFile, Log]) ->
	PrevTime = erlang:monotonic_time(),

	Src = input:read_meta(MetaFile), % Reading Src of graph

	_AdjList = input:read_input(InputFile),
	AdjList = maps:from_list(_AdjList), % Reading the Adjaceny list and storing it in map for faster access.

	Visited = maps:from_list(lists:map(fun(V) -> {V,false} end, lists:seq(1, length(_AdjList)))), % Initialize Visited array

	Depth = run_iter(0, AdjList, Visited, [Src], []),

	TimeTaken = (erlang:monotonic_time() - PrevTime)/1000000000,

	case Log of
		time -> io:format("TimeTaken: ~w\n", [TimeTaken]);
		depth -> io:format("Depth: ~w\n",[Depth])
	end.

run_iter(_, _, _, [], Depth) -> % The algo terminates when the frontier set is empty.
	Depth;
run_iter(L, AdjList, _Visited, F, _Depth) ->
	Depth = lists:append(_Depth, lists:map(fun(V)-> {V,L} end, F)), % Update the level of the vertices in current frontier Set

	NewVisited = maps:from_list(lists:map(fun(V) -> {V,true} end, F)), % Update the visited array for the frontier set as true
	Visited = maps:merge(_Visited, NewVisited),

	% Get the neighbours of the vertices in frontier set using adjaceny list and get the union
	_Nf = sets:to_list(sets:union(lists:map(fun(V) -> maps:get(V, AdjList) end, F))), 
	Nf = lists:filter(fun(V) -> not maps:get(V, Visited) end, _Nf), % Filter the vertices to remove already visited vertices.

	run_iter(L+1, AdjList, Visited, Nf, Depth). % Run next iteration