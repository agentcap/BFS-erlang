-module('input').
-export([read_input/3, read_meta/1]).

read_input(Pi, Pj, InpDir) ->
	{ok, Device} = file:open(filename:join([InpDir, "inp_" ++ integer_to_list(Pi) ++ "_" ++ integer_to_list(Pj)]), [read]),
	{ok, [N]} = io:fread(Device, [], "~d"),
	% io:format("REad N \n").
	AdjList = read_graph(N, [], Device),
	AdjList.

read_graph(0, AdjList, _) ->
	AdjList;
read_graph(N, AdjList, Device) ->
	{ok, [Vertex]} = io:fread(Device, [], "~d"),
	{ok, [Cnt]} = io:fread(Device, [], "~d"),
	read_graph(N-1, lists:append(AdjList, [{Vertex, sets:from_list(read_adjacent(Cnt, [], Device))}]), Device).
	
read_adjacent(Cnt, Adj, _) when Cnt == 0 ->
	Adj;
read_adjacent(Cnt, Adj, Device) ->
	{ok, [Num]} = io:fread(Device, [], "~d"),
	read_adjacent(Cnt-1, [Num | Adj], Device).

read_meta(MetaFile) ->
	{ok, Device} = file:open(MetaFile, [read]),
	{ok, [R]} = io:fread(Device, [], "~d"),
	{ok, [C]} = io:fread(Device, [], "~d"),
	{ok, [M]} = io:fread(Device, [], "~d"),
	{ok, [Src]} = io:fread(Device, [], "~d"),
	{R, C, M, Src}.
