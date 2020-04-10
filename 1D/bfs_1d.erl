% erlc *.erl && erl -noshell -s bfs_1d main inp -s init stop
-module('bfs_1d').
-export([main/1, proc_func/5, run_iters/6, broadcast_all/5, collect_all/3]).

main([InpFile]) ->
	{ok, InpDevice} = file:open(InpFile, [read]),
	{NoProcess, N, Src, AdjList} = input:read_input(InpDevice),
	
	io:format("--------- Input Graph ---------\n",[]),
	utils:print_graph(AdjList),
	
	io:format("--------- Creating Processes\n",[]),
	M = utils:get_m(N, NoProcess, N rem NoProcess),
	create_process(1, NoProcess, M, AdjList, Src).

% Create the required Number of process and distributes the vertices across them
create_process(Pid, NoProcess, M, AdjList, Src) when Pid == NoProcess ->
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn('bfs_1d', proc_func, [Pid, NoProcess, Src, M, AdjList]));
create_process(Pid, NoProcess, M, _AdjList, Src) -> 
	{AdjList, RestList} = lists:split(M, _AdjList),
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn('bfs_1d', proc_func, [Pid, NoProcess, Src, M, AdjList])),
	create_process(Pid+1, NoProcess, M, RestList, Src).

% Function which runs on each process and Initializes the Depth and runs the iterations
proc_func(Pid, NoProcess, Src, M, _AdjList) ->
	_Depth = lists:map(fun({V,_}) -> {V, inf} end, _AdjList),
	Depth = lists:keyreplace(Src, 1, _Depth, {Src, 0}), % Initialize src with 0

	AdjList = maps:from_list(_AdjList),
	run_iters(Pid, NoProcess, 0, Depth, M, AdjList).

run_iters(_, _, L, _, _, _) when L > 5 -> ok;
run_iters(Pid, NoProcess, L, Depth, M, AdjList) ->
	GetNeighFunc = fun({V, D}) -> %Function to filter the vertices with depth L 
			case D == L of
				true -> {true, maps:get(V, AdjList)};
				false -> false
			end
		end,

	_N = lists:filtermap(GetNeighFunc, Depth),
	N = sets:to_list(sets:union(_N)),

	broadcast_all(Pid, 1, NoProcess, M, N),
	OtherN = collect_all(0, NoProcess, sets:new()),
	
	OwnerFunc = fun(X) -> (M*(Pid-1) < X) and (X =< M*Pid) end,
	MyN = sets:from_list(lists:filter(OwnerFunc, N)),
	NewN = sets:to_list(sets:union(MyN, OtherN)),

	NewDepth = utils:update_depth(NewN,Depth,L+1),
	io:format("Iter: ~w , Pid:~w , New Depths : ~w\n",[L, Pid, NewDepth]),
	
	run_iters(Pid, NoProcess, L+1, NewDepth, M, AdjList).

% Brodcasts to all functions execpt itself
broadcast_all(_, SendPid, NoProcess, _, _) when SendPid > NoProcess ->
	ok;
broadcast_all(MyPid, SendPid, NoProcess, M, _N) when MyPid == SendPid ->
	broadcast_all(MyPid, SendPid + 1, NoProcess, M, _N);
broadcast_all(MyPid, SendPid, NoProcess, M, _N) ->
	OwnerFunc = fun(X) -> (M*(SendPid-1) < X) and (X =< M*SendPid) end,
	{N, NextN} = lists:partition(OwnerFunc, _N),
	list_to_atom("pid" ++ integer_to_list(SendPid)) ! {neighbours, N},
	broadcast_all(MyPid, SendPid + 1, NoProcess, M, NextN).

% Collects N send from all the other processes
collect_all(Cnt, NoProcess, _N) when Cnt >= NoProcess - 1 ->
	_N;
collect_all(Cnt, NoProcess, _N) ->
	receive
		{neighbours, List} ->
			N = sets:union(_N,sets:from_list(List))
	end,
	collect_all(Cnt+1, NoProcess, N).
