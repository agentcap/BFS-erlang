% erlc *.erl && erl -noshell -s bfs_2d main inp -s init stop
-module('bfs_2d').
-export([main/1, proc_func/7]).

main([MetaFile, InpDir]) ->
	{R, C, M, Src} = input:read_meta(MetaFile),
	create_process(1, R, C, M, Src, InpDir).

create_process(Pi, R, _, _, _, _) when Pi > R ->
	ok;
create_process(Pi, R, C, M, Src, InpDir) -> 
	create_process_row(Pi, 1, R, C, M, Src, InpDir),
	create_process(Pi+1, R, C, M, Src, InpDir).

create_process_row(Pi, Pj, R, C, M, Src, InpDir) when Pj > C ->
	ok;
create_process_row(Pi, Pj, R, C, M, Src, InpDir) ->
	register(list_to_atom("pid" ++ integer_to_list(Pi) ++ integer_to_list(Pj)), spawn('bfs_2d', proc_func, [Pi, Pj, R, C, M, Src, InpDir])),
	create_process_row(Pi, Pj + 1, R, C, M, Src, InpDir).

proc_func(Pi, Pj, R, C, M, Src, InpDir) ->
	
	OwnedVertices = lists:seq( 1+((Pj-1)*R + Pi -1)*M, ((Pj-1)*R + Pi)*M ),
	_Depth = lists:map(fun(V) -> {V, inf} end, OwnedVertices),
	Depth = lists:keyreplace(Src, 1, _Depth, {Src, 0}),

	_AdjList = input:read_input(Pi, Pj, InpDir),
	AdjList = maps:from_list(_AdjList),

	% io:format("P~w~w AdjList: ~w\n", [Pi,Pj, AdjList]),

	run_iters(Pi, Pj, R, C, 0, Depth, M, AdjList).

run_iters(Pi, Pj, R, C, L, Depth, M, AdjList) ->
	F = lists:filter(fun({_,D}) -> D == L end, Depth),
	broadcast_F_to_column(Pi, Pj, 1, R, F).


broadcast_F_to_column(_, _, ToPi, R, _) when ToPi > R ->
	ok;
broadcast_F_to_column(Pi, Pj, ToPi, R, F) ->
	list_to_atom("pid" ++ integer_to_list(ToPi) ++ integer_to_list(Pj)) ! {f, F},
	broadcast_F_to_column(Pi, Pj, ToPi + 1, R, F).

% collect_F_from_column(Cnt, C, )