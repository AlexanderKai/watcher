-module(watcher).
-behaviour(gen_server).

-compile([export_all]).

-include("../include/watcher.hrl").

start() -> watcher_sup:start_link().

start_link(Args) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).

stop() ->
	gen_server:stop({local, ?MODULE}).
	
init(_Args) ->
	process_flag(trap_exit, true),
	self() ! init,
	{ok, #{rules => []}}.

raw_rule(Rule) ->
	gen_server:call(?MODULE, {rule, Rule}).

get_session() ->
	[{settings, Settings}] = ets:lookup(watcher_settings, settings),
	SessionN = maps:get(session, Settings, 1),
	SessionN.

new_session() ->
	[{settings, Settings}] = ets:lookup(watcher_settings, settings),
	SessionN = maps:get(session, Settings, 1),
	NewSettings = Settings#{session := SessionN + 1},
	ets:insert(watcher_settings, [{settings, NewSettings}]).

compiler_options(Module, Options) ->
	gen_server:call(?MODULE, {options, {Module, Options}}).

%#{backend => console | mnesia}
settings(Settings) ->
	gen_server:call(?MODULE, {settings, {Settings}}).

get_backend() ->
	[{settings, Settings}] = ets:lookup(watcher_settings, settings),
	Backend = maps:get(backend, Settings, [mnesia]),
	Backend.

rule(Module) ->
	Send = case is_list(Module) of
		false ->
			[Module];
		_ ->
			Module
	end,
	gen_server:call(?MODULE, {rule, Send}).

%watcher:rule(my_module, [func1, func2]).
%watcher:rule(my_module, 'ALL']).
%watcher:rule(my_module, [{func1,['A', 'B']}, func2]).
rule(Module, FunctionsVariables) ->
	gen_server:call(?MODULE, {rule, {Module, FunctionsVariables}}).

find_source_in_line([H1|[H2|T]], Pos, LengthFull) ->
		Size = H1*256 + H2,
		case Size == Pos + 2 + LengthFull of 
			true ->
				Bin = binary:list_to_binary(T),
				binary:part(Bin, 1, Size);
			_ ->	
				find_source_in_line([H2|T], Pos + 1, LengthFull)
		end;

find_source_in_line([H|T], _, _) ->
		false.

cut_wtch({func, _, _} = V) ->
	V;
				   
cut_wtch({return, _, _} = V) ->
	V;
				   
cut_wtch(V) ->
	list_to_binary(string:replace(V, "_WTCH", "")).
				   
format([console, MF, Line, Variable, Value]) ->
	Var = cut_wtch(Variable),
	case is_list(Value) of
		true ->
			%Test = list_to_binary(io_lib:print(Value)),
			%Length = length(Value),
			%case byte_size(Test) > 50 orelse Length > 25  of
			%	true ->
					io:format("~148.148.-s~n",["-"]),
					io:format("|~69.s|~70.w|~5.w|~n", [Var, MF, Line]),
					io:format("~148.148.-s~n",["-"]),
					io:format("~p~n", [Value]);
			%	_ ->
			%		io:format("~50.w|~5.w|~30.s|~60.w~n",[MF, Line, Var, Value])
			%end;
		_ ->
			%io:format("~50.w|~5.w|~30.s|~60.w~n",[MF, Line, Variable, Value])
			io:format("~148.148.-s~n",["-"]),
			io:format("|~69.s|~70.w|~5.w|~n", [Var, MF, Line]),
			io:format("~148.148.-s~n",["-"]),
			io:format("~p~n", [Value])
	end;

format([mnesia, {M, F}, Line, Variable, Value, Same]) ->
	Var = cut_wtch(Variable),
	Session = get_session(),
	Res = mnesia:transaction(fun() -> mnesia:write(#watcher_history{name = erlang:unique_integer([monotonic]), session = Session, pid = self(), module = M, function = F, line = Line, time = erlang:monotonic_time(), var = Var, value = Value, same = list_to_binary(Same)}) end),
	io:format("Res format mnesia ~p~n", [Res]).

on() ->
	gen_server:call(?MODULE, {compile, trace}, infinity).

off() ->
	gen_server:call(?MODULE, {compile, original}, infinity).

compile_file([Erl, Beam], Type) ->
	ErlBackup = Erl ++ ".bak",
	BeamBackup = Beam ++ ".bak",
	{ok, ErlBinary} = file:read_file(Erl),
	file:copy(Erl, ErlBackup),
	file:copy(Beam, BeamBackup),
	case Type of
		original ->
			[];
		trace ->
			[]
			%file:write_file(Erl, [<<"-compile({parse_transform, watcher_pt}).\n">>, ErlBinary])
	end,
	Tokens = string:tokens(Beam, "/"),
	ShortName = hd(lists:reverse(Tokens)),
	[ModuleName, _]  = string:tokens(ShortName, "."),
	Outdir = lists:flatten(string:replace(Beam, ShortName, "", trailing)),

	Lines = string:split(ErlBinary, <<"\n">>, all),
	FormattedLines = format_lines(Lines),
	%io:format("FormattedLines ~p~n", [FormattedLines]),
	ets:insert(watcher_sources, [{{get_session(), list_to_atom(ModuleName)}, FormattedLines}]),
	%io:format("Compile Ebin dir ~p~n", [Outdir]),

	io:format("ShortName ~p~n", [list_to_atom(ModuleName)]),
	[{_,ModuleOptions}] = ets:lookup(watcher_module_options, list_to_atom(ModuleName)),
	Res = compile:file(Erl, [verbose, return_errors, {outdir,Outdir},  
							 			%{parse_transform, lager_transform},
							            %{parse_transform, cut},
							            %{parse_transform, boss_db_pmod_pt},
							            %{parse_transform, boss_db_pt},
							            %{parse_transform, do},
							            %{parse_transform, import_as},
							 			
										{parse_transform, watcher_pt} 
							] ++ ModuleOptions),
	case Res of
		error ->
			file:copy(BeamBackup, Beam);
		{error,_,_} ->
			file:copy(BeamBackup, Beam);
		_ ->
			[]
	end,

	io:format("compile res ~n~p~n~p~n", [ShortName, Res]),
	file:write_file(Erl, ErlBinary),
	file:delete(BeamBackup),
	file:delete(ErlBackup).

clear() ->
	gen_server:call(?MODULE, clear).

%format_lines(Lines) ->
%	format_lines_do(Lines, 1, #{}).
%
%format_lines_do([], _, Acc) ->
%	Acc;
%
%format_lines_do([H|T], Line, Acc) ->
%	format_lines_do(T, Line + 1, maps:put(Line,H,Acc)).
	
format_lines(Lines) ->
	format_lines_do(Lines, 1, []).

format_lines_do([], _, Acc) ->
	lists:reverse(Acc);

format_lines_do([H|T], Line, Acc) ->
	format_lines_do(T, Line + 1, [#{line => Line, string => H} | Acc]).
	
%parse_rule(Rule) ->

handle_call({settings, {Settings}}, _From, #{}=State) ->
	{settings, SettingsOld} = ets:lookup(watcher_settings, settings),
	ets:insert(watcher_settings, {settings, maps:merge(SettingsOld,Settings)}),
	{reply, ok, State};

handle_call({options, {Module, Options}}, _From, #{}=State) ->
	ets:insert(watcher_module_options, {Module, Options}),
	{reply, ok, State};

handle_call({rule, {Module, FunctionsVariables}}, _From, #{}=State) ->
	Insert = 
	[
	 	case is_tuple(Rule) of
			true ->
				RuleL = [Rule],
				[
					[
						{{Module, Func}, V}
					||
						V <- Variables
					]

				||
					{Func, Variables} <- RuleL
				];
			_ ->
				RuleL = [Rule],
				[
					{Module, Func}
				||
					Func <- RuleL
				]
		end
	||
		Rule <- FunctionsVariables
	],
	ets:insert(watcher, lists:flatten(Insert)),
	{reply, ok, State};	

handle_call({rule, Modules}, _From, #{}=State) ->
	[
		ets:insert(watcher, {Module})
	||
		Module <- Modules
	],
	{reply, ok, State};	

handle_call({compile, Type}, _From, #{}=State) ->
	io:format("compile ~p~n", [erlang:timestamp()]),
	ListORules = ets:tab2list(watcher),
	Modules = lists:usort(
	[
	 	atom_to_list(
			case T of
				{{M, F}, V} -> M;
				{M, F} -> M;
				{M} -> M			
			end)
	||
		T <- ListORules, element(1, T) =/= 'ALL'
	]),
	io:format("Modules ~p~n", [Modules]),

		
	io:format("find file [ ~p~n", [erlang:timestamp()]),
	Files = lib_find:find(Modules),
	io:format("find file ] ~p~n", [erlang:timestamp()]),
	io:format("Files ~p~n", [Files]),
	io:format("compile file [ ~p~n", [erlang:timestamp()]),

	[
		compile_file(File, Type)
	||
		File <- Files
	],
	io:format("compile file ] ~p~n", [erlang:timestamp()]),

	io:format("Loading and purging modules ~n"),
	Purge = [
	 	begin
			%case erlang:check_old_code(list_to_atom(Module)) of
			%	true ->
			%		code:purge(list_to_atom(Module));
			%	_ ->
			%		[]
			%end,
			%ResLoading = code:delete(list_to_atom(Module)),
			code:soft_purge(list_to_atom(Module)),
			ResLoading = code:load_file(list_to_atom(Module)),
			case ResLoading of
				{module, _} ->
					{list_to_atom(Module), code:soft_purge(list_to_atom(Module))};
				{_, Error} ->
					io:format("Error ~p~n", [Error]),
					{list_to_atom(Module), "Module "++Module++" was not loaded, it is not purged!"}
			end
		end
	||
		Module <- Modules
	],
	io:format("~p~n", [Purge]),

	io:format("end compile ~p~n", [erlang:timestamp()]),
	{reply, ok, State};	

handle_call(clear, _From, #{}=State) ->
	ets:delete_all_objects(watcher_settings),
	ets:delete_all_objects(watcher),
	ets:delete_all_objects(watcher_module_options),
	{reply, ok, State}.	

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, #{rules := Rules}=State) ->
	watcher_http_app:start(),
	application:ensure_all_started(mnesia),
	mnesia:create_table(watcher_history, 
		[{access_mode, read_write}, {record_name, watcher_history}, {attributes, [name, session, pid, module, function, line, time, var, value, same]}, {index, [pid, session, module, function, line, time, var, value, same]}, {type, ordered_set}]),
	ets:new(watcher_settings, [named_table, {read_concurrency, true}, set, public]),
	ets:insert(watcher_settings, {settings, #{session => 1}}),
	ets:new(watcher, [named_table, {read_concurrency, true}, bag, public]),
	ets:new(watcher_module_options, [named_table, {read_concurrency, true}, set, public]),
	ets:new(watcher_sources, [named_table, {read_concurrency, true}, set, public]),
    {noreply, State};

handle_info({check, From, Name, MFA}, #{rules := Rules}=State) ->
    {noreply, State}.

terminate(_Reason, _State) ->

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
