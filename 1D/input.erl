-module('input').
-export([read_input/1]).

read_input(Input) ->
	{ok, Text} = file:read_file(Input),
	[MetaString | DataString] = string:tokens(string:strip(binary_to_list(Text), both, $\n), "\n"),

	[NoProcess, N, Src] = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, string:tokens(MetaString, " ")),
	AdjList = read_adjList(DataString, []),

	{NoProcess, N, Src, AdjList}.

read_adjList([], AdjList) -> 
	AdjList;
read_adjList([Cur|Rest], _AdjList) ->
	[Vertex | List] = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, string:tokens(Cur, " ")),
	AdjList = lists:append(_AdjList, [{Vertex, sets:from_list(List)}]),
	read_adjList(Rest, AdjList).