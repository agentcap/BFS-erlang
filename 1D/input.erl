-module('input').
-export([read_input/1]).

read_input(Device) ->
	{ok, [NoProcess]} = io:fread(Device, [], "~d"),
	{ok, [N]} = io:fread(Device, [], "~d"),
	AdjList = read_graph(N, [], Device),
	{NoProcess, N, AdjList}.

read_graph(0, AdjList, _) ->
	AdjList;
read_graph(N, AdjList, Device) ->
	{ok, [Vertex]} = io:fread(Device, [], "~d"),
	{ok, [Cnt]} = io:fread(Device, [], "~d"),
	read_graph(N-1, lists:append(AdjList, [{Vertex, read_adjacent(Cnt, [], Device)}]), Device).
	
read_adjacent(Cnt, Adj, _) when Cnt == 0 ->
	Adj;
read_adjacent(Cnt, Adj, Device) ->
	{ok, [Num]} = io:fread(Device, [], "~d"),
	read_adjacent(Cnt-1, [Num | Adj], Device).