% erlc *.erl && erl -noshell -s bfs meta input -s init stop

% Note: Makesure all vertices are present even if it has no neighbours or take N as input.
-module('bfs').
-export([main/1]).

main([MetaFile, InputFile]) ->
	% io:format("Hello"),
	PrevTime = erlang:monotonic_time(),

	Src = input:read_meta(MetaFile),

	_AdjList = input:read_input(InputFile),
	AdjList = maps:from_list(_AdjList),

	Visited = maps:from_list(lists:map(fun(V) -> {V,false} end, lists:seq(1, length(_AdjList)))),

	Depth = run_iter(0, AdjList, Visited, [Src], []),

	CurTime = erlang:monotonic_time(),

	TimeTaken = (CurTime - PrevTime)/1000000000,
	io:format("Seq -> ~w\n", [TimeTaken]).

	% io:format("Depth: ~w\n",[Depth]).


run_iter(_, _, _, [], Depth) ->
	Depth;
run_iter(L, AdjList, _Visited, F, _Depth) ->
	Depth = lists:append(_Depth, lists:map(fun(V)-> {V,L} end, F)),

	NewVisited = maps:from_list(lists:map(fun(V) -> {V,true} end, F)),
	Visited = maps:merge(_Visited, NewVisited),

	_Nf = sets:to_list(sets:union(lists:map(fun(V) -> maps:get(V, AdjList) end, F))),
	Nf = lists:filter(fun(V) -> not maps:get(V, Visited) end, _Nf),

	run_iter(L+1, AdjList, Visited, Nf, Depth).