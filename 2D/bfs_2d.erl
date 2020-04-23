% erlc *.erl && erl -noshell -s bfs_2d main inp -s init stop
-module('bfs_2d').
-export([main/1, proc_func/9, broadcast_N_to_row/8]).

main([MetaFile, InpDir, Log]) ->
	PrevTime = erlang:monotonic_time(),

	% Reading meta data and creating the process
	{R, C, M, Src} = input:read_meta(MetaFile),
	create_process(1, R, C, M, Src, InpDir, Log),

	% This check the status and informs the process whether to terminate or not
	collect_and_send_status(0, R, C, 0, true),
	
	TimeTaken = (erlang:monotonic_time() - PrevTime)/1000000000,
	case Log of
		time -> io:format("TimeTaken: ~w\n", [TimeTaken]);
		depth -> ok
	end.

% Create the required Number of process (R*C) and registers the process with name pid<row>_<col>
create_process(Pi, R, _, _, _, _, _) when Pi > R ->
	ok;
create_process(Pi, R, C, M, Src, InpDir, Log) -> 
	create_process_row(Pi, 1, R, C, M, Src, InpDir, Log),
	create_process(Pi+1, R, C, M, Src, InpDir, Log).
create_process_row(_, Pj, _, C, _, _, _, _) when Pj > C ->
	ok;
create_process_row(Pi, Pj, R, C, M, Src, InpDir, Log) ->
	Parent = self(),
	register(list_to_atom("pid" ++ integer_to_list(Pi) ++ "_" ++ integer_to_list(Pj)), spawn('bfs_2d', proc_func, [Pi, Pj, R, C, M, Src, InpDir, Parent, Log])),
	create_process_row(Pi, Pj + 1, R, C, M, Src, InpDir, Log).

% Function which runs on each process and Initializes the Depth and runs the iterations
proc_func(Pi, Pj, R, C, M, Src, InpDir, Parent, Log) ->
	OwnedVertices = lists:seq( 1+((Pj-1)*R + Pi -1)*M, ((Pj-1)*R + Pi)*M ),
	_Depth = lists:map(fun(V) -> {V, inf} end, OwnedVertices),
	Depth = lists:keyreplace(Src, 1, _Depth, {Src, 0}),

	_AdjList = input:read_input_separate(Pi, Pj, InpDir), % Reading input
	AdjList = maps:from_list(_AdjList),

	run_iters(Pi, Pj, R, C, 0, Depth, M, AdjList, Parent, Log).

run_iters(Pi, Pj, R, C, L, Depth, M, AdjList, Parent, Log) ->
	_F = lists:filter(fun({_,D}) -> D == L end, Depth),
	F = lists:map(fun({V,_}) -> V end, _F), % Frontier set

	Parent ! {fsize, L, length(F) == 0}, % sends the size of the Frontier set to check the termination condition

	receive
		{L, continue} ->
				% Broadcast the frontier set to the processes in its column
				broadcast_F_to_column(Pi, Pj, 1, R, F, L),

				% Recieve the frontier from its column processs
				F_collected = collect_F_from_column(0, R, F, L),


				% Get the set of Neighbours
				F_filtered = lists:filter(fun(A) -> maps:is_key(A, AdjList) end, F_collected),
				GetNeighFunc = fun(A) -> maps:get(A, AdjList) end,
				_N = lists:map(GetNeighFunc, F_filtered),


				% Broadcast the neighbours (only the vertices which that process own) to the process in its row
				OwnerFunc = fun(X) -> (((Pj-1)*R + Pi -1)*M < X) and (X =< (((Pj-1)*R + Pi)*M)) end,
				{MyN, OtherN} = lists:partition(OwnerFunc, sets:to_list(sets:union(_N))),
				broadcast_N_to_row(Pi, Pj, 1, R, C, M, OtherN, L),

				% Collecte the neighbours from the processes in its row
				N_collected = collect_N_from_row(0, C, sets:from_list(MyN), L),
				N = sets:to_list(N_collected),

				% Update the depth of the vertices in the Next frontier with depth as L+1
				{ReqUpdate, AlreadyUpdated} = lists:partition(fun({_,T})-> T == inf end, Depth),
				UpdatedDepth = utils:update_depth(N, ReqUpdate, L+1),
				NewDepth = lists:append(UpdatedDepth, AlreadyUpdated),
				
				run_iters(Pi, Pj, R, C, L+1, NewDepth, M, AdjList, Parent, Log);
		{L, terminate} ->
				case Log of
					depth -> io:format("Iter: ~w, Pid:~w_~w, Depth: ~w\n",[L, Pi, Pj, Depth]);
					time -> ok
				end
	end.



% Broadcast N to row while filtering the vertices which are owned by that process
broadcast_N_to_row(_, _, ToPj, _, C, _, _, _) when ToPj > C ->
	ok;
broadcast_N_to_row(Pi, Pj, ToPj, R, C, M, _N, L) when ToPj == Pj ->
	broadcast_N_to_row(Pi, Pj, ToPj + 1, R, C, M, _N, L);
broadcast_N_to_row(Pi, Pj, ToPj, R, C, M, _N, L) ->
	OwnerFunc = fun(X) -> (((ToPj-1)*R + Pi -1)*M < X) and (X =< (((ToPj-1)*R + Pi)*M)) end,
	{N, NextN} = lists:partition(OwnerFunc, _N),

	list_to_atom("pid" ++ integer_to_list(Pi) ++ "_" ++ integer_to_list(ToPj)) ! {neighbours, L, N},
	broadcast_N_to_row(Pi, Pj, ToPj + 1, R, C, M, NextN, L).

% Collects the neighbours from the process in its row
collect_N_from_row(Cnt, C, N, _) when Cnt >= C - 1 ->
	N;
collect_N_from_row(Cnt, C, _N, L) ->
	receive
		{neighbours, L, N} ->
			collect_N_from_row(Cnt+1, C, sets:union(_N, sets:from_list(N)), L)
	end.


% Other util functions whoes functionality is alreay explained above.
broadcast_F_to_column(_, _, ToPi, R, _, _) when ToPi > R ->
	ok;
broadcast_F_to_column(Pi, Pj, ToPi, R, F, L) when ToPi == Pi ->
	broadcast_F_to_column(Pi, Pj, ToPi+1, R, F, L);
broadcast_F_to_column(Pi, Pj, ToPi, R, F, L) ->
	list_to_atom("pid" ++ integer_to_list(ToPi) ++ "_" ++ integer_to_list(Pj)) ! {f, L, F},
	broadcast_F_to_column(Pi, Pj, ToPi + 1, R, F, L).


collect_F_from_column(Cnt, R, F, _) when Cnt >= R - 1 ->
	F;
collect_F_from_column(Cnt, R, _F, L) ->
	receive
		{f, L, F} ->
			collect_F_from_column(Cnt+1, R, lists:append(_F, F), L)
	end.


collect_and_send_status(Cnt, R, C, L, Status) when Cnt == R*C ->
	case Status of
		false -> 
			send_status(1, R, C, L, continue),
			collect_and_send_status(0, R, C, L+1, true);
		true ->
			send_status(1, R, C, L, terminate)
	end;
collect_and_send_status(Cnt, R, C, L, Status) ->
	receive
		{fsize, L, Value} ->
			collect_and_send_status(Cnt + 1, R, C, L, Status and Value)
	end.

send_status(Pi, R, _, _, _) when Pi > R ->
	ok;
send_status(Pi, R, C, L, Status) ->
	send_status_row(Pi, 1, C, L, Status),
	send_status(Pi+1, R, C, L, Status).

send_status_row(_, Pj, C, _, _) when Pj > C ->
	ok;
send_status_row(Pi, Pj, C, L, Status) ->
	list_to_atom("pid" ++ integer_to_list(Pi) ++ "_" ++ integer_to_list(Pj)) ! {L, Status},
	send_status_row(Pi, Pj+1, C, L, Status).