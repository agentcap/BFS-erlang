% erlc *.erl && erl -noshell -s bfs_1d main inp -s init stop
-module('bfs_1d').
-export([main/1, proc_func/5, run_iters/6, broadcast_all/6, collect_all/4]).

main([InpFile]) ->
	{NoProcess, N, Src, AdjList} = input:read_input(InpFile),
	
	io:format("--------- Input Graph ---------\n"),
	utils:print_graph(AdjList),
	
	io:format("---------   Output    ---------\n",[]),
	M = utils:get_m(N, NoProcess, N rem NoProcess),
	create_process(1, NoProcess, M, AdjList, Src).

% Create the required Number of process and distributes the vertices across them
create_process(Pid, NoProcess, M, AdjList, Src) when Pid == NoProcess ->
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn_link('bfs_1d', proc_func, [Pid, NoProcess, Src, M, AdjList]));
create_process(Pid, NoProcess, M, _AdjList, Src) -> 
	{AdjList, RestList} = lists:split(M, _AdjList),
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn_link('bfs_1d', proc_func, [Pid, NoProcess, Src, M, AdjList])),
	create_process(Pid+1, NoProcess, M, RestList, Src).

% Function which runs on each process and Initializes the Depth and runs the iterations
proc_func(Pid, NoProcess, Src, M, _AdjList) ->
	_Depth = lists:map(fun({V,_}) -> {V, inf} end, _AdjList),
	Depth = lists:keyreplace(Src, 1, _Depth, {Src, 0}), % Initialize src with 0

	AdjList = maps:from_list(_AdjList),
	run_iters(Pid, NoProcess, 0, Depth, M, AdjList).

run_iters(_, _, L, _, _, _) when L > 5 -> ok;
run_iters(Pid, NoProcess, L, Depth, M, AdjList) ->
	F = lists:filter(fun({_,D}) -> D == L end, Depth),
	FLength = length(F),

	GetNeighFunc = fun({V, _}) -> maps:get(V, AdjList) end,%Function to filter the vertices with depth L
	_N = lists:map(GetNeighFunc, F),

	N = sets:to_list(sets:union(_N)),

	broadcast_all(Pid, 1, NoProcess, M, N, FLength),
	{OtherN, OtherF} = collect_all(0, NoProcess, sets:new(), [FLength]),

	
	OwnerFunc = fun(X) -> (M*(Pid-1) < X) and (X =< M*Pid) end,
	MyN = sets:from_list(lists:filter(OwnerFunc, N)),
	NewN = sets:to_list(sets:union(MyN, OtherN)),

	NewDepth = utils:update_depth(NewN,Depth,L+1),
	
	Terminate = lists:all(fun(Y) -> Y == 0 end, OtherF),

	if
		Terminate ->
			io:format("Iter: ~w, Pid:~w, [{Vertex, Depth}] : ~w\n",[L, Pid, NewDepth]);
		true ->
			run_iters(Pid, NoProcess, L+1, NewDepth, M, AdjList)
	end.

% Brodcasts to all functions execpt itself
broadcast_all(_, SendPid, NoProcess, _, _,_) when SendPid > NoProcess ->
	ok;
broadcast_all(MyPid, SendPid, NoProcess, M, _N, FLength) when MyPid == SendPid ->
	broadcast_all(MyPid, SendPid + 1, NoProcess, M, _N, FLength);
broadcast_all(MyPid, SendPid, NoProcess, M, _N, FLength) ->
	OwnerFunc = fun(X) -> (M*(SendPid-1) < X) and (X =< M*SendPid) end,
	{N, NextN} = lists:partition(OwnerFunc, _N),
	list_to_atom("pid" ++ integer_to_list(SendPid)) ! {neighbours, N, FLength},
	broadcast_all(MyPid, SendPid + 1, NoProcess, M, NextN, FLength).

% Collects N send from all the other processes
collect_all(Cnt, NoProcess, _N, Fs) when Cnt >= NoProcess - 1 ->
	{_N, Fs};
collect_all(Cnt, NoProcess, _N, _Fs) ->
	receive
		{neighbours, List, F} ->
			N = sets:union(_N,sets:from_list(List)),
			Fs = [F | _Fs]
	end,
	collect_all(Cnt+1, NoProcess, N, Fs).
