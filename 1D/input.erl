-module('input').
-export([read_input_separate/2, read_meta/1]).

read_adjList([], AdjList) -> 
	AdjList;
read_adjList([Cur|Rest], _AdjList) ->
	[Vertex | List] = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, string:tokens(Cur, " ")),
	AdjList = lists:append(_AdjList, [{Vertex, sets:from_list(List)}]),
	read_adjList(Rest, AdjList).

read_meta(Input) ->
	{ok, Text} = file:read_file(Input),
	MetaString = string:tokens(string:strip(binary_to_list(Text), both, $\n), " "),
	[NoProcess, N, Src] = lists:map(fun(X) -> {Int, _} = string:to_integer(X), Int end, MetaString),
	{NoProcess, N, Src}.

read_input_separate(InputDir, Pid) ->
	Input = filename:join([InputDir, integer_to_list(Pid)]),
	{ok, Text} = file:read_file(Input),
	DataString = string:tokens(string:strip(binary_to_list(Text), both, $\n), "\n"),
	read_adjList(DataString, []).