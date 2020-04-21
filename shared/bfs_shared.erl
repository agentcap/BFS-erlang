% erlc *.erl && erl -noshell -s bfs_shared main inp -s init stop

% Note: Makesure all vertices are present even if it has no neighbours or take N as input.
-module('bfs_shared').
-export([main/1, proc_func/2, run_iter/3]).

main([MetaFile, InputFile]) ->
	{NoProcess, Src} = input:read_meta(MetaFile),

	create_process(1, NoProcess, InputFile),

	collect_ready(0, NoProcess),

	Depth = main_iter(0,[Src], [{Src, 0}], NoProcess),

	io:format("Depth are ~w\n", [Depth]).

create_process(Pid, NoProcess, _) when Pid > NoProcess ->
	ok;
create_process(Pid, NoProcess, InputFile) ->
	Parent = self(),
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn_link('bfs_shared', proc_func, [InputFile, Parent])),
	create_process(Pid + 1, NoProcess, InputFile).

collect_ready(Cnt, NoProcess) when Cnt == NoProcess ->
	ok;
collect_ready(Cnt, NoProcess) ->
	receive
		{ready} ->
			collect_ready(Cnt+1, NoProcess)
	end.

main_iter(_, [], Depth, NoProcess) ->
	send_terminate(1, NoProcess),
	Depth;
main_iter(L, F, Depth, NoProcess) ->
	distribute_f(1, F, F, utils:ceil(length(F),NoProcess), NoProcess),
	_Nf = collect_nf(0, NoProcess, sets:new()),
	Nf = sets:to_list(_Nf),
	_Depth = lists:map(fun(V) -> {V, L+1} end, Nf),
	NewDepth = lists:append(Depth, _Depth),

	main_iter(L+1, Nf, NewDepth, NoProcess).

proc_func(InputFile, Parent) ->
	_AdjList = input:read_input(InputFile),
	N = length(_AdjList),
	% utils:print_graph(AdjList),
	AdjList = maps:from_list(_AdjList),
	Visited = maps:from_list(lists:map(fun(V) -> {V,false} end,lists:seq(1, N))),

	Parent ! {ready},

	run_iter(AdjList, Visited, Parent).

run_iter(AdjList, _Visited, Parent) ->
	receive
		{frontiers, F, _NewVisited} ->
			NewVisited = maps:from_list(lists:map(fun(V) -> {V,true} end, _NewVisited)),
			Visited = maps:merge(_Visited, NewVisited),

			_Nf = sets:to_list(sets:union(lists:map(fun(V) -> maps:get(V, AdjList) end, F))),

			Nf = lists:filter(fun(V) -> not maps:get(V, Visited) end, _Nf),

			Parent ! {nf, Nf},

			run_iter(AdjList, Visited, Parent);
		{terminate} ->
			ok
	end.

distribute_f(Pid, _, _, _, NoProcess) when Pid > NoProcess ->
	ok;
distribute_f(Pid, _F, Visited, M, NoProcess) ->
	{F, NextF} = lists:split(min(M, length(_F)), _F),
	list_to_atom("pid" ++ integer_to_list(Pid)) ! {frontiers, F, Visited},
	distribute_f(Pid+1, NextF, Visited, M, NoProcess).

collect_nf(Cnt, NoProcess, _Nf) when Cnt == NoProcess ->
	_Nf;
collect_nf(Cnt, NoProcess, _Nf) ->
	receive
		{nf, Nf} -> 
			collect_nf(Cnt+1, NoProcess, sets:union(_Nf, sets:from_list(Nf)))
	end.

send_terminate(Pid, NoProcess) when Pid > NoProcess ->
	ok;
send_terminate(Pid, NoProcess) ->
	list_to_atom("pid" ++ integer_to_list(Pid)) ! {terminate},
	send_terminate(Pid + 1, NoProcess).
