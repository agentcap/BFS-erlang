% erlc *.erl && erl -noshell -s bfs_1d main inp -s init stop
-module('bfs_1d').
-export([main/1, proc_func/6, run_iters/7, broadcast_all/6, collect_all/4]).

main([MetaFile, InpDir]) ->
	PrevTime = erlang:monotonic_time(),
	{NoProcess, N, Src} = input:read_meta(MetaFile),
	
	M = utils:get_m(N, NoProcess, N rem NoProcess),
	create_process(1, NoProcess, M, InpDir, Src),

	collect_and_send_status(1, NoProcess, 0, true),
	CurTime = erlang:monotonic_time(),

	TimeTaken = (CurTime - PrevTime)/1000000000,
	io:format("1D -> ~w\n", [TimeTaken]).


% Create the required Number of process and distributes the vertices across them
create_process(Pid, NoProcess, _, _, _) when Pid > NoProcess ->
	ok;
create_process(Pid, NoProcess, M, InpDir, Src) -> 
	Parent = self(),
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn_link('bfs_1d', proc_func, [Pid, NoProcess, Src, M, InpDir, Parent])),
	create_process(Pid+1, NoProcess, M, InpDir, Src).

% Function which runs on each process and Initializes the Depth and runs the iterations
proc_func(Pid, NoProcess, Src, M, InpDir, Parent) ->
	_AdjList = input:read_input_separate(InpDir, Pid),

	_Depth = lists:map(fun({V,_}) -> {V, inf} end, _AdjList),
	Depth = lists:keyreplace(Src, 1, _Depth, {Src, 0}), % Initialize src with 0

	AdjList = maps:from_list(_AdjList),

	run_iters(Pid, NoProcess, 0, Depth, M, AdjList, Parent).


run_iters(Pid, NoProcess, L, Depth, M, AdjList, Parent) ->
	F = lists:filter(fun({_,D}) -> D == L end, Depth),

	Parent ! {fsize, L, length(F) == 0},

	receive
		{L, continue} ->

			GetNeighFunc = fun({V, _}) -> maps:get(V, AdjList) end, % Function to filter the vertices with depth L
			_N = lists:map(GetNeighFunc, F),

			N = sets:to_list(sets:union(_N)),

			broadcast_all(Pid, 1, NoProcess, M, N, L),
			OtherN = collect_all(0, NoProcess, sets:new(), L),

			OwnerFunc = fun(X) -> (M*(Pid-1) < X) and (X =< M*Pid) end,
			MyN = sets:from_list(lists:filter(OwnerFunc, N)),
			NewN = sets:to_list(sets:union(MyN, OtherN)),

			{ReqUpdate, AlreadyUpdated} = lists:partition(fun({_,T})-> T == inf end, Depth),

			UpdatedDepth = utils:update_depth(NewN,ReqUpdate,L+1),

			NewDepth = lists:append(UpdatedDepth, AlreadyUpdated),
			run_iters(Pid, NoProcess, L+1, NewDepth, M, AdjList, Parent);

		{L, terminate} ->
			ok
				% io:format("Iter: ~w, Pid:~w, [{Vertex, Depth}] : ~w\n",[L, Pid, Depth])
				% io:format("Iter: ~w, Pid:~w Teminated\n",[L, Pid])
	end.


% Brodcasts to all functions execpt itself
broadcast_all(_, SendPid, NoProcess, _, _,_) when SendPid > NoProcess ->
	ok;
broadcast_all(MyPid, SendPid, NoProcess, M, _N,L) when MyPid == SendPid ->
	broadcast_all(MyPid, SendPid + 1, NoProcess, M, _N,L);
broadcast_all(MyPid, SendPid, NoProcess, M, _N,L) ->
	OwnerFunc = fun(X) -> (M*(SendPid-1) < X) and (X =< M*SendPid) end,
	{N, NextN} = lists:partition(OwnerFunc, _N),
	list_to_atom("pid" ++ integer_to_list(SendPid)) ! {neighbours, L, N},
	broadcast_all(MyPid, SendPid + 1, NoProcess, M, NextN,L).

% Collects N send from all the other processes
collect_all(Cnt, NoProcess, _N, _) when Cnt >= NoProcess - 1 ->
	_N;
collect_all(Cnt, NoProcess, _N, L) ->
	receive
		{neighbours, L, List} ->
			collect_all(Cnt+1, NoProcess, sets:union(_N,sets:from_list(List)), L)
	end.

collect_and_send_status(Cnt, NoProcess, L, Status) when Cnt > NoProcess ->
	case Status of
		false ->
			send_status(1, NoProcess, L, continue),
			collect_and_send_status(1, NoProcess, L+1, true);
		true ->
			send_status(1, NoProcess, L, terminate)
	end;
collect_and_send_status(Cnt, NoProcess, L, Status) ->
	receive
		{fsize, L, Value} ->
			collect_and_send_status(Cnt + 1, NoProcess, L, Status and Value)
	end.

send_status(Pid, NoProcess, _, _) when Pid > NoProcess ->
	ok;
send_status(Pid, NoProcess, L, Status) ->
	list_to_atom("pid" ++ integer_to_list(Pid)) ! {L, Status},
	send_status(Pid+1, NoProcess, L, Status).