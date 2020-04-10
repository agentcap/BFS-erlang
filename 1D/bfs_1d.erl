% erlc *.erl && erl -noshell -s bfs_1d main inp -s init stop
-module('bfs_1d').
-export([main/1, proc_func/3]).

main([InpFile]) ->
	{ok, InpDevice} = file:open(InpFile, [read]),
	{NoProcess, N, Src, AdjList} = input:read_input(InpDevice),
	utils:print_graph(AdjList),
	M = get_m(N, NoProcess, N rem NoProcess),
	create_process(1, NoProcess, M, AdjList, Src).

get_m(N, NoProcess, 0) -> 
	round(N/NoProcess);
get_m(N, NoProcess, Rem) ->
	round((N-Rem)/NoProcess) + 1.

create_process(Pid, NoProcess, _, AdjList, Src) when Pid == NoProcess ->
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn('bfs_1d', proc_func, [Pid, Src, AdjList]));
create_process(Pid, NoProcess, M, _AdjList, Src) -> 
	{AdjList, RestList} = lists:split(M, _AdjList),
	register(list_to_atom("pid" ++ integer_to_list(Pid)), spawn('bfs_1d', proc_func, [Pid, Src, AdjList])),
	create_process(Pid+1, NoProcess, M, RestList, Src).

proc_func(Pid, Src, _AdjList) ->
	_Depth = lists:map(fun({V,_}) -> {V, inf} end, _AdjList),

	AdjList = maps:from_list(_AdjList),
	Depth = maps:from_list(_Depth),

	Flag = maps:is_key(Src, Depth),

	if 
		Flag ->
			NewDepth = maps:update(Src, 0, Depth);
		true -> 
			NewDepth = Depth
	end,

	io:format("Proc ~w : ~w\n",[Pid, maps:values(NewDepth)]).