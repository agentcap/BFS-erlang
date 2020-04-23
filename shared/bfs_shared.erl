% erlc *.erl && erl -noshell -s bfs_shared main meta input depth -s init stop

-module('bfs_shared').
-export([main/1, proc_func/2, run_iter/3]).

main([MetaFile, InputFile, Log]) ->
	PrevTime = erlang:monotonic_time(),
	
	{NoProcess, Src} = input:read_meta(MetaFile),

	create_process(1, NoProcess, InputFile), % Create the process
	collect_ready(0, NoProcess), % wait for them to read the input

	Depth = main_iter(0,[Src], [{Src, 0}], NoProcess), 

	TimeTaken = (erlang:monotonic_time() - PrevTime)/1000000000,

	case Log of
		time -> io:format("TimeTaken: ~w\n", [TimeTaken]);
		depth -> io:format("Depth: ~w\n",[Depth])
	end.

	% io:format("Depth are ~w\n", [Depth]).

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


% This function is run on the main process which distributes the frontier vertices and collects the results
main_iter(_, [], Depth, NoProcess) -> % Terminated when frontier is empty and send terminate message to all process
	send_terminate(1, NoProcess),
	Depth;
main_iter(L, F, Depth, NoProcess) ->
	%Distributes frontier vertices, and also sends the updated Visited array
	distribute_f(1, F, F, utils:ceil(length(F),NoProcess), NoProcess), 
	_Nf = collect_nf(0, NoProcess, sets:new()),
	Nf = sets:to_list(_Nf),
	_Depth = lists:map(fun(V) -> {V, L+1} end, Nf),
	NewDepth = lists:append(Depth, _Depth),

	main_iter(L+1, Nf, NewDepth, NoProcess).


% Function which each process runs when spwaned
proc_func(InputFile, Parent) ->
	_AdjList = input:read_input(InputFile), % Reads input
	N = length(_AdjList),
	
	AdjList = maps:from_list(_AdjList), 
	Visited = maps:from_list(lists:map(fun(V) -> {V,false} end,lists:seq(1, N))), % Initialization

	Parent ! {ready}, % Informs parent its ready
	run_iter(AdjList, Visited, Parent).


run_iter(AdjList, _Visited, Parent) ->
	% On reciving the frontier set, visited array, it explores these vertices and sends back Next frontiers to the parent.
	receive
		{frontiers, F, _NewVisited} ->
			NewVisited = maps:from_list(lists:map(fun(V) -> {V,true} end, _NewVisited)),
			Visited = maps:merge(_Visited, NewVisited),

			_Nf = sets:to_list(sets:union(lists:map(fun(V) -> maps:get(V, AdjList) end, F))),

			Nf = lists:filter(fun(V) -> not maps:get(V, Visited) end, _Nf),

			Parent ! {nf, Nf},

			run_iter(AdjList, Visited, Parent);
		{terminate} -> % Algos is done and so terminates
			ok
	end.


% Functions to distribute and collect the frontiers repectively.
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