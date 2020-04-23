% erlc *.erl && erl -noshell -s bfs_1d main meta inp depth -s init stop
-module('bfs_1d').
-export([main/1, proc_func/7, run_iters/8, broadcast_all/6, collect_all/4]).

main([MetaFile, InpDir, Log]) ->
	PrevTime = erlang:monotonic_time(),
	
	% Reading meta data and creating the process
	{NoProcess, N, Src} = input:read_meta(MetaFile),
	M = utils:get_m(N, NoProcess, N rem NoProcess),
	create_process(1, NoProcess, M, InpDir, Src, Log),

	% This check the status and informs the process whether to terminate or not
	collect_and_send_status(1, NoProcess, 0, true),


	TimeTaken = (erlang:monotonic_time() - PrevTime)/1000000000,
	case Log of
		time -> io:format("TimeTaken: ~w\n", [TimeTaken]);
		depth -> ok
	end.


% Create the required Number of process and registers the process with name pid<id>
create_process(Pid, NoProcess, _, _, _, _) when Pid > NoProcess ->
	ok;
create_process(Pid, NoProcess, M, InpDir, Src, Log) -> 
	Parent = self(),
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn_link('bfs_1d', proc_func, [Pid, NoProcess, Src, M, InpDir, Parent, Log])),
	create_process(Pid+1, NoProcess, M, InpDir, Src, Log).

% Function which runs on each process and Initializes the Depth and runs the iterations
proc_func(Pid, NoProcess, Src, M, InpDir, Parent, Log) ->
	_AdjList = input:read_input_separate(InpDir, Pid),

	% Initialize depth to inf for all except src with 0
	_Depth = lists:map(fun({V,_}) -> {V, inf} end, _AdjList),
	Depth = lists:keyreplace(Src, 1, _Depth, {Src, 0}), 

	AdjList = maps:from_list(_AdjList),

	run_iters(Pid, NoProcess, 0, Depth, M, AdjList, Parent, Log).


run_iters(Pid, NoProcess, L, Depth, M, AdjList, Parent, Log) ->
	F = lists:filter(fun({_,D}) -> D == L end, Depth),

	Parent ! {fsize, L, length(F) == 0}, % sends the size of the Frontier set to check the termination condition

	receive
		{L, continue} ->

			% Get the Frontier set
			GetNeighFunc = fun({V, _}) -> maps:get(V, AdjList) end, % Function to filter the vertices with depth L
			_N = lists:map(GetNeighFunc, F),
			N = sets:to_list(sets:union(_N)),

			% Broadcast the frontier set and collect the frontier set from all the process
			broadcast_all(Pid, 1, NoProcess, M, N, L),
			OtherN = collect_all(0, NoProcess, sets:new(), L),

			% Getting the Next frontier set
			OwnerFunc = fun(X) -> (M*(Pid-1) < X) and (X =< M*Pid) end,
			MyN = sets:from_list(lists:filter(OwnerFunc, N)),
			NewN = sets:to_list(sets:union(MyN, OtherN)),

			% Update the depth of vertices in the Next frontier set
			{ReqUpdate, AlreadyUpdated} = lists:partition(fun({_,T})-> T == inf end, Depth),
			UpdatedDepth = utils:update_depth(NewN,ReqUpdate,L+1),
			NewDepth = lists:append(UpdatedDepth, AlreadyUpdated),

			% Run next iteration
			run_iters(Pid, NoProcess, L+1, NewDepth, M, AdjList, Parent, Log);
		{L, terminate} ->
			case Log of
				depth -> io:format("Iter: ~w, Pid:~w, [{Vertex, Depth}] : ~w\n",[L, Pid, Depth]);
				time -> ok
			end
	end.



%% Util Functions to communicate across processes.
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