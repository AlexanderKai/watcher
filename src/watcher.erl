-module(watcher).
-behaviour(gen_server).

-export([start_link/1, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, raw_rule/1, rule/1, rule/2, clear/0, on/0, off/0, find_source_in_line/3, format/1]).


-record(state, {rules}).


start() -> watcher_sup:start_link().

start_link(Args) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, Args, []).
	
init(_Args) ->
	process_flag(trap_exit, true),
	self() ! init,
	{ok, #state{rules=[]}}.

raw_rule(Rule) ->
	gen_server:call(?MODULE, {rule, Rule}).

%watcher:rule(my_module).
rule(Module) ->
	Send = case is_list(module) of
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

format([MF, Line, Variable, Value]) ->
	case is_list(Value) of
		true ->
			Test = binary:list_to_bin(Value),
			Length = length(Value),
			case byte_size(Test) > 50 orelse Length > 28 of
				true ->
					io:format("~30.w|~5.w|~20.s|~60.p~n", [MF, Line, Variable, Value]);
				_ ->
					io:format("~30.w|~5.w|~20.s|~60.w~n",[MF, Line, Variable, Value])
			end;
		_ ->
			io:format("~30.w|~5.w|~20.s|~60.w~n",[MF, Line, Variable, Value])
	end.

on() ->
	gen_server:call(?MODULE, {compile, trace}, infinity).

off() ->
	gen_server:call(?MODULE, {compile, original}, infinity).

compile_file([F1, F2], Type) ->
	[Beam, Erl] = 
		case string:find(F1, ".beam", trailing) of
			nomatch ->
				[F2, F1];
			_ ->
				[F1, F2]
		end,
	ErlBackup = Erl ++ ".bak",
	{ok, ErlBinary} = file:read_file(Erl),
	file:copy(Erl, ErlBackup),
	case Type of
		original ->
			[];
		trace ->
			file:write_file(Erl, [<<"-compile({parse_transform, watcher_pt}).\n">>, ErlBinary])
	end,
	Tokens = string:tokens(Beam, "/"),
	ShortName = hd(lists:reverse(Tokens)),
	Outdir = lists:flatten(string:replace(Beam, ShortName, "", trailing)),
	io:format("Compile Ebin dir ~p~n", [Outdir]),
	compile:file(Erl, [verbose, {outdir,Outdir}]),
	file:write_file(Erl, ErlBinary),
	file:delete(ErlBackup).

clear() ->
	gen_server:call(?MODULE, clear).

%parse_rule(Rule) ->

handle_call({rule, {Module, FunctionsVariables}}, _From, #state{}=State) ->
	Insert = 
	[
	 	case is_tuple(Rule) of
			true ->
				RuleL = [Rule],
				io:format("Rule ~p~n", [Rule]),
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
	io:format("Rules flatten ~p~n", [lists:flatten(Insert)]),
	ets:insert(watcher, lists:flatten(Insert)),
	{reply, ok, State};	

handle_call({rule, Modules}, _From, #state{}=State) ->
	[
		ets:insert(watcher, {Module})
	||
		Module <- Modules
	],
	{reply, ok, State};	

handle_call({compile, Type}, _From, #state{}=State) ->
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

	{ok,CWD} = file:get_cwd(),
		
	Files = lib_find:files(CWD, Modules, true),
	io:format("Files ~p~n", [Files]),

	[
		compile_file(File, Type)
	||
		File <- Files
	],

	io:format("Compiling and purging modules ~n"),
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

	{reply, ok, State};	

handle_call(clear, _From, #state{}=State) ->
	ets:delete_all_objects(watcher),
	{reply, ok, State}.	

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(init, #state{rules=Rules}=State) ->
	ets:new(watcher, [named_table, {read_concurrency, true}, bag, public]),
    {noreply, State};

handle_info({check, From, Name, MFA}, #state{rules=Rules}=State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
