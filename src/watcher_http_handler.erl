-module(watcher_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("../include/watcher.hrl").
-include_lib("stdlib/include/qlc.hrl").

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	{Get, Req3} = cowboy_req:qs_val(<<"get">>, Req2),
	io:format("method ~p~nget ~p~nreq ~p~n", [Method, Get, Req3]), 
	{ok, Req4} = get_handler(Method, Get, Req3),
	{ok, Req4, State}.

make_record(X) ->
	V= io_lib:format("~p",[X#watcher_history.value]),
	Value = list_to_binary(lists:flatten(V)),
	Var = case X#watcher_history.var of
		{func, M, F} -> 
			MM = atom_to_binary(M, latin1),
			FF = atom_to_binary(F, latin1),
			%<<"return of ", MM/binary, ":", FF/binary>>;
			[{func, [{module, MM}, {function, FF}]}];
		{return, M, F} -> 
			MM = atom_to_binary(M, latin1),
			FF = atom_to_binary(F, latin1),
			%<<"return of ", MM/binary, ":", FF/binary>>;
			[{return, [{module, MM}, {function, FF}]}];
		Rest ->
			Rest
	end,
	#{
		name => integer_to_binary(X#watcher_history.name),
		session => X#watcher_history.session,
		pid => list_to_binary(pid_to_list(X#watcher_history.pid)),
		module => atom_to_binary(X#watcher_history.module, latin1),
		function => atom_to_binary(X#watcher_history.function, latin1),
		line => X#watcher_history.line,
		same => X#watcher_history.same,
		time => X#watcher_history.time,
		var => Var,
		value => Value
	}.

to_map(Lines) ->
	to_map(Lines, #{}).

to_map([], Acc) ->
	Acc;

to_map([H|T],Acc) ->
	Name = maps:get(name, H),
	Value = maps:get(Name, Acc, []),
	NewAcc = maps:put(Name, Value ++ [H] ,Acc),
	to_map(T, NewAcc).

get_handler(<<"GET">>, <<"action">>, Req) ->
	%{Action_Id_From_, Req2} = cowboy_req:qs_val(<<"action_id_from">>, Req),
	%{Action_Id_To_, Req2} = cowboy_req:qs_val(<<"action_id_to">>, Req),
	{Pid_Id_, Req2} = cowboy_req:qs_val(<<"pid_id">>, Req),
	{Session_Id_, Req3} = cowboy_req:qs_val(<<"session_id">>, Req2),
	%{Module, Req4} = cowboy_req:qs_val(<<"module">>, Req3),
	io:format("Session_ ~p~n", [Session_Id_]),
	io:format("Pid_Id ~p~n", [Pid_Id_]),
	%Action_Id_From = binary_to_integer(Action_Id_From_),
	%Action_Id_To = binary_to_integer(Action_Id_To_),
	Session_Id = binary_to_integer(Session_Id_),
	Pid_Id = list_to_pid(binary_to_list(Pid_Id_)),
	%Module = list_to_atom(binary_to_list(Pid_Id_)),
	io:format("Session_ ~p~n", [Session_Id]),
	io:format("Pid_Id ~p~n", [Pid_Id]),

	%Modules = do(qlc:q([ X#watcher_history.module || X <- mnesia:table(watcher_history), X#watcher_history.name == Action_Id], [{unique, true}])),
	%[Module] = Modules,
	%[ModuleSources] = [
	%	begin
	%		%io:format("M ~p~n", [M]),
	%		[{{Session_Id, M}, ModuleLines}] = ets:lookup(watcher_sources, {Session_Id, M}),
	%		%io:format("ModuleLines ~p~n", [ModuleLines]),
	%		ModuleLines
	%	end
	%||
	%M <- Modules
	%],
	%io:format("Module Sources ~p~n", [ModuleSources]),

	Records = do(qlc:q([ make_record(X) || X <- mnesia:table(watcher_history), X#watcher_history.session == Session_Id, X#watcher_history.pid == Pid_Id])),
	%io:format("records ~p~n", [Records]),

	%[ActionLine] = do(qlc:q([ X#watcher_history.line || X <- mnesia:table(watcher_history), X#watcher_history.name == Action_Id])),

	%Query = do(qlc:info(qlc:q([ X#watcher_history.name || X <- mnesia:table(watcher_history), X#watcher_history.name > Action_Id], [{max_lookup, 1}]), [{n_elements, 1}])),
	%Query2 = qlc:info(Query, [{n_elements, 1}]),	
	%NextActionLine = Query,
	%io:format("NextActionLine ~p~n", [NextActionLine]),
	%Sorting = fun(#{name:=A}, #{name:=B}) ->
	%	A =< B
	%end,i

	%RecordsMap = to_map(Records),
	io:format("~n~nRecordMap~n~n~p~n", [to_map(Records)]),

	%Lines = [
	%S#{vars => 
	%||
	%V <- Records, S <- ModuleSources, maps:get(line, V) == maps:get(line, S)
	%],
	%io:format("~n~nLines~n~n~p~n", [Lines]),
	
	%Vars = [{session, Session_Id_}, {pid, Pid_Id_}, {action, Action_Id}, {lines, ModuleSources}],
	%{ok, Render} = sessions_dtl:render(Vars),
	%io:format("Render ~p~n", [Render]),

	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"application/json; charset=utf-8">>}],
		jsone:encode([{lines, to_map(Records)}]),
		Req3);

get_handler(<<"GET">>, <<"module_lines">>, Req) ->
	{Module_, Req2} = cowboy_req:qs_val(<<"module">>, Req),
	{Session_Id_, Req3} = cowboy_req:qs_val(<<"session_id">>, Req2),
	{Pid_Id_, Req4} = cowboy_req:qs_val(<<"pid_id">>, Req3),
	{Action_Id_, Req5} = cowboy_req:qs_val(<<"action_id">>, Req4),
	Session_Id = binary_to_integer(Session_Id_),
	Module = binary_to_atom(Module_, latin1),
	Action_Id = binary_to_integer(Action_Id_),
	Pid_Id = list_to_pid(binary_to_list(Pid_Id_)),
	io:format("Session_ ~p~n", [Session_Id]),
	io:format("Module ~p~n", [Module]),

	[{{Session_Id, Module}, ModuleLines}] = ets:lookup(watcher_sources, {Session_Id, Module}),

	Vars = [{session, Session_Id_}, {pid, Pid_Id_}, {module, atom_to_binary(Module, latin1)}, {action, Action_Id}, {lines, ModuleLines}],
	{ok, Render} = lines_dtl:render(Vars),

	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"text/html; charset=utf-8">>}],
		Render,
		Req5);

get_handler(<<"GET">>, <<"module">>, Req) ->
	{Action_Id_, Req2} = cowboy_req:qs_val(<<"action_id">>, Req),
	{Pid_Id_, Req3} = cowboy_req:qs_val(<<"pid_id">>, Req2),
	{Session_Id_, Req4} = cowboy_req:qs_val(<<"session_id">>, Req3),
	io:format("Session_ ~p~n", [Session_Id_]),
	io:format("Pid_Id ~p~n", [Pid_Id_]),
	Action_Id = binary_to_integer(Action_Id_),
	Session_Id = binary_to_integer(Session_Id_),
	Pid_Id = list_to_pid(binary_to_list(Pid_Id_)),
	io:format("Session_ ~p~n", [Session_Id]),
	io:format("Pid_Id ~p~n", [Pid_Id]),

	Modules = do(qlc:q([ X#watcher_history.module || X <- mnesia:table(watcher_history), X#watcher_history.name == Action_Id], [{unique, true}])),
	[Module] = Modules,
	[ModuleSources] = [
		begin
			io:format("M ~p~n", [M]),
			[{{Session_Id, M}, ModuleLines}] = ets:lookup(watcher_sources, {Session_Id, M}),
			io:format("ModuleLines ~p~n", [ModuleLines]),
			ModuleLines
		end
	||
	M <- Modules
	],
	io:format("Module Sources ~p~n", [ModuleSources]),

	%Lines = [
	%S#{vars => 
	%||
	%V <- Records, S <- ModuleSources, maps:get(line, V) == maps:get(line, S)
	%],
	%io:format("~n~nLines~n~n~p~n", [Lines]),
	
	Vars = [{session, Session_Id_}, {pid, Pid_Id_}, {module, atom_to_binary(Module, latin1)}, {action, Action_Id}, {lines, ModuleSources}],
	{ok, Render} = sessions_dtl:render(Vars),
	%io:format("Render ~p~n", [Render]),

	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"text/html; charset=utf-8">>}],
		Render,
		Req4);

get_handler(<<"GET">>, <<"pid_json">>, Req) ->
	{Session_Id_, Req2} = cowboy_req:qs_val(<<"session_id">>, Req),
	{Pid_Id_, Req3} = cowboy_req:qs_val(<<"pid_id">>, Req2),
	io:format("Session_ ~p~n", [Session_Id_]),
	io:format("Pid_Id ~p~n", [Pid_Id_]),
	Session_Id = binary_to_integer(Session_Id_),
	Pid_Id = list_to_pid(binary_to_list(Pid_Id_)),
	io:format("Session_ ~p~n", [Session_Id]),
	io:format("Pid_Id ~p~n", [Pid_Id]),
	Records = do(qlc:q([ make_record(X) || X <- mnesia:table(watcher_history), X#watcher_history.session == Session_Id, X#watcher_history.pid == Pid_Id], [{unique, true}])),
	io:format("records ~p~n", [Records]),
	
	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"application/json; charset=utf-8">>}],
		jsone:encode([{execution, Records}]),
		Req3);

get_handler(<<"GET">>, <<"pid">>, Req) ->
	{Session_Id_, Req2} = cowboy_req:qs_val(<<"session_id">>, Req),
	{Pid_Id_, Req3} = cowboy_req:qs_val(<<"pid_id">>, Req2),
	io:format("Session_ ~p~n", [Session_Id_]),
	io:format("Pid_Id ~p~n", [Pid_Id_]),
	Session_Id = binary_to_integer(Session_Id_),
	Pid_Id = list_to_pid(binary_to_list(Pid_Id_)),
	io:format("Session_ ~p~n", [Session_Id]),
	io:format("Pid_Id ~p~n", [Pid_Id]),
	Modules = do(qlc:q([ X#watcher_history.module || X <- mnesia:table(watcher_history), X#watcher_history.session == Session_Id, X#watcher_history.pid == Pid_Id], [{unique, true}])),
	%ModuleSources = [
	%	begin
	%		{{Session_Id, M}, ModuleLines} = ets:lookup(watcher_sources, M),
	%		{M, ModuleLines}
	%	end
	%||
	%M <- Modules
	%],
	%io:format("Module Sources ~p~n", [ModuleSources]),
	Records = do(qlc:q([ make_record(X) || X <- mnesia:table(watcher_history), X#watcher_history.session == Session_Id, X#watcher_history.pid == Pid_Id], [{unique, true}])),
	io:format("records ~p~n", [Records]),
	
	Sorting = fun(#{name:=A}, #{name:=B}) ->
		A >= B
	end,

	Vars = [{session, Session_Id_}, {pid, Pid_Id_}, {raws, lists:sort(Sorting, Records)}],
	{ok, Render} = sessions_dtl:render(Vars),
	io:format("Render ~p~n", [Render]),

	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"text/html; charset=utf-8">>}],
		Render,
		Req3);


get_handler(<<"GET">>, <<"session">>, Req) ->
	{Id_, Req2} = cowboy_req:qs_val(<<"id">>, Req),
	Id = binary_to_integer(Id_),
	io:format("Id ~p~n", [Id]),
	Pids = do(qlc:q([#{pid => list_to_binary(pid_to_list(X#watcher_history.pid))} || X <- mnesia:table(watcher_history), X#watcher_history.session == Id])),
	io:format("pids ~p~n", [Pids]),

	Vars = [{session, Id}, {pids, lists:usort(Pids)}],
	{ok, Render} = sessions_dtl:render(Vars),
	io:format("Render ~p~n", [Render]),

	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"text/html; charset=utf-8">>}],
		Render,
		Req2);

get_handler(<<"GET">>, undefined, Req) -> %show sessions
	Sessions = do(qlc:q([#{session => X#watcher_history.session} || X <- mnesia:table(watcher_history)])),
	
	io:format("Sessions ~p~n", [Sessions]),

	{ok, Render} = sessions_dtl:render([{sessions, lists:usort(Sessions)}]),
	io:format("Render ~p~n", [Render]),
		
	cowboy_req:reply(
		200, 
		[{<<"content-type">>, <<"text/html; charset=utf-8">>}],
		%<<"get all sessions">>,
		Render,
		Req).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.


echo(<<"GET">>, undefined, Req) ->
	cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(<<"GET">>, Echo, Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Echo, Req);
echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
	ok.
