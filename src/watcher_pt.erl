-module(watcher_pt).
-export([parse_transform/2]).

-compile([{parse_transform, lager_transform}]).

get_filter(Module) ->
	All = ets:lookup(watcher, 'ALL'),
	case All of
		[] ->
			Res = ets:lookup(watcher, Module),
			%io:format("get_filter Module ~p~n", [Res]),
			case Res of
				[] ->
					false;
				_ ->
					%io:format("get_filter/1 Res ~p~n",[Res]),
					lists:max([
						begin
							%io:format("R ~p~n", [R]), 
						case R of
							{Mod, Func} ->
								false;
							{Mod} ->
								Module == Mod
						end
						end

					||
						R <- Res
					])
			end;
		_ ->
			true
	end.

get_filter(Module, Function) ->
	Res = ets:lookup(watcher, Module),
	%io:format("get_filter/2 Res ~p~n",[Res]),
	case lists:keymember('ALL', 2, Res) of
		false ->
			lists:keymember(Function, 2, Res);
		true ->
			true
	end.

get_filter(Module, Function, Variable) ->
	Res = ets:lookup(watcher, {Module, Function}),
	%io:format("get_filter/3 Res ~p~nModule ~p~nFunction ~p~nVariable ~p~n",[Res, Module, Function, Variable]),
	case lists:keymember('ALL', 2, Res) of
		false ->
			lists:keymember(Variable, 2, Res);
		true ->
			true
	end.

parse_transform(AST, _Options) -> 
	%io:format("~p~n", [ets:lookup(watcher, test)]),
	Module = find_module(AST),
	Perm = get_filter(Module),

	%io:format("Perm module ~p~n", [Perm]),
	%io:format("Transforming ~p~n", [Module]),
	lager:info("============================Original========================~n~p~n", [AST]),
	Res = try
	R = [parse({Perm, Module}, T) || T <- AST],
	%io:format("~60.w~60.w", [AST, Res]),
	R
	catch
		E1:E2 -> io:format("E1~p~nE2~p~n", [E1, E2]),
				 io:format("Stack ~p~n",[erlang:get_stacktrace()])
	end,
	lager:info("============================Modified========================~n~p~n", [Res]),
	Check = erl_lint:module(Res),
	io:format("============================Check========================~n~p~n", [Check]),
	file:write_file("orig", io_lib:format("~p.~n", [AST])),
	file:write_file("modf", io_lib:format("~p.~n", [Res])),
	Res. 

parse({ModulePerm, Module}, {function, Line, Function, Arity, Clauses}) ->
	Perm = ModulePerm orelse get_filter(Module, Function), 
	%io:format("Perm 2 ~p~nget_filter(Module, Function)~p~n", [Perm, get_filter(Module,Function)]), 
	NewClauses = lists:map(
		fun(TE) ->
			parse_func_clauses({Perm, Module, Function}, TE)
		end,
	Clauses),
	{function, Line, Function, Arity, NewClauses};

parse(_, T) -> 
	T.

find_module([{attribute, _, module, Module}|_T]) ->
	case is_tuple(Module) of
		true ->
			element(1, Module);
		_ ->
			Module
	end;

find_module([_H|T]) ->
	find_module(T).

parse_func_clauses({P,M,F}, {clause,Line,Arguments,GuardSeq,Expressions}) ->
	FuncPattern0 = Arguments,
	io:format("parse_func_clauses Arguments~p~n", [Arguments]),
	FunctionDefinition = [io_format({M, F}, {func, M, F}, Line)],
	%io:format("FunctionDefinition ~p~n", [FunctionDefinition]),
	FunctionMatching = lists:flatten(
		lists:map(
		fun(V) ->
			io:format("parse func_expressions V ~p~n~p~n~p~n~p~n", [P, M, F, V]),
			parse_func_expressions({P,M,F}, V)
		end,
	Arguments)),
	[Tail|Rest] = lists:reverse(Expressions),
	ExpressionsWithoutTail = lists:reverse(Rest),
	NewExpressions = 
		lists:map(
		fun(V) ->
			parse_expressions({P,M,F}, V)
		end,	
	ExpressionsWithoutTail),
	LastExpression = [parse_expressions_tail({P,M,F}, Tail)],
	%io:format("parse_func_expressions Result ~p~n", [FunctionMatching]),
	%FuncPattern = find_var(parse_expressions(MF, Arguments)),
	{clause, Line, Arguments, GuardSeq, FunctionDefinition ++ FunctionMatching ++ NewExpressions ++ LastExpression}.

parse_clauses({P,M,F}, {clause,Line,Arguments,GuardSeq,Expressions}) ->
	%io:format("ClausePattern ~p~n", [Expressions]),
	NewExpressions = lists:map(
		fun(V) -> 
			parse_expressions({P,M,F}, V) 
		end, 
	Expressions),
	{clause, Line, Arguments, GuardSeq, NewExpressions}.

%var_watcher_safe(V) ->
%	var_watcher_safe(V, []).
%
%var_watcher_safe(begin, Var) ->
%	var_watcher_safe(Var, [])
%

generate() ->
	["WTCH" | integer_to_list(rand:uniform(1000000000))].

var_watcher_safe({_, Line, _} = V) ->
	Res = var_watcher_safe_do(V),
	{{var, Line, generate()}, Res}.

var_watcher_safe_do(List) when is_list(List) ->
	lists:map(
		fun(V) -> 
			var_watcher_safe_do(V) 
		end, 
	List);

var_watcher_safe_do({string, Line, String}) ->
	{string, Line, String};

var_watcher_safe_do({atom, Line, Atom}) ->
	{atom, Line, Atom};

var_watcher_safe_do({var, Line, Var}) ->
	ListAtom = atom_to_list(Var),
	CheckUnbound = hd(ListAtom),
	%io:format("CheckUnbound ~p~n", [CheckUnbound]),
	case CheckUnbound of
		$_ ->
			{string, Line, ListAtom};
		_ ->
			{var, Line, list_to_atom(ListAtom ++ "_WTCH")}
	end;

%var_watcher_safe({var, Line, Var}) ->
%	{var, Line, list_to_atom(atom_to_list(Var) ++ "_WTCH")};

var_watcher_safe_do({cons, Line, Head, Tail}) ->
	{cons, Line, var_watcher_safe_do(Head), var_watcher_safe_do(Tail)};

var_watcher_safe_do({nil, Line}) ->
	{nil, Line};

var_watcher_safe_do({map, Line, Map, Elements}) ->
	{map, Line, Map, Elements};

var_watcher_safe_do({map, Line, Map}) ->
	Vars =lists:map(
		fun(V) -> 
			var_watcher_safe_do(V) 
		end, 
	Map),
	{map, Line, Vars};

var_watcher_safe_do({map_field_exact, Line, Key, Var}) ->
	{map_field_exact, Line, Key, var_watcher_safe_do(Var)};

var_watcher_safe_do({tuple, Line, Elements}) ->
	Vars =lists:map(
		fun(V) -> 
			var_watcher_safe_do(V) 
		end, 
	Elements),
	{tuple, Line, Vars};

var_watcher_safe_do(V) ->
	V.

var_watcher(List) when is_list(List) ->
	lists:map(
		fun(V) -> 
			var_watcher(V) 
		end, 
	List);

var_watcher({string, Line, String}) ->
	{string, Line, String};

var_watcher({atom, Line, Atom}) ->
	{atom, Line, Atom};

var_watcher({var, Line, Var}) ->
	ListAtom = atom_to_list(Var),
	CheckUnbound = hd(ListAtom),
	%io:format("CheckUnbound ~p~n", [CheckUnbound]),
	case CheckUnbound of
		%95 ->
		%	{string, Line, ListAtom};
		_ ->
			{var, Line, list_to_atom(ListAtom ++ "_WTCH")}
	end;

%var_watcher({var, Line, Var}) ->
%	{var, Line, list_to_atom(atom_to_list(Var) ++ "_WTCH")};

var_watcher({cons, Line, Head, Tail}) ->
	{cons, Line, var_watcher(Head), var_watcher(Tail)};

var_watcher({nil, Line}) ->
	{nil, Line};

var_watcher({map, Line, Map, Elements}) ->
	{map, Line, Map, Elements};

var_watcher({map, Line, Map}) ->
	Vars =lists:map(
		fun(V) -> 
			var_watcher(V) 
		end, 
	Map),
	{map, Line, Vars};

var_watcher({map_field_exact, Line, Key, Var}) ->
	{map_field_exact, Line, Key, var_watcher(Var)};

var_watcher({tuple, Line, Elements}) ->
	Vars =lists:map(
		fun(V) -> 
			var_watcher(V) 
		end, 
	Elements),
	{tuple, Line, Vars};

var_watcher(V) ->
	V.

find_var({map, _Line, Map}) ->
	%io:format("find_var map~n"),
	lists:map(
		fun(V) -> 
			find_var(V) 
		end, 
	Map);

find_var({map_field_exact, _Line, Key, Var}) ->
	%io:format("find_var map_field_exact~n"),
	find_var(Var);

find_var(List) when is_list(List) ->
	lists:map(
		fun(V) -> 
			find_var(V) 
		end, 
	List);

find_var({var, _Line, Var}) ->
	[Var];

find_var({cons, _Line, Head, Tail}) ->
	%[find_var(Head) | find_var(Tail)];
	find_var(Head) ++ find_var(Tail);

%find_var({record, _Line, Name, _Fields}) ->
%	Name;

find_var({tuple, _Line, Elements}) ->
	Vars = lists:map(
		fun(V) -> 
			find_var(V) 
		end, 
	Elements),
	Vars;

find_var(_V) ->
	[].

parse_func_expressions({Perm, Module, Function}, {match, Line, Var, Expression}) ->
	Exp = lists:flatten(find_var(Expression)),
	Vars = lists:flatten(find_var(Var)),
	%io:format("parse_func_expressions~n"),
	%io:format("Exps ~p~n", [Exp]),
	%io:format("Vars ~p~n", [Vars]),
	Same = generate(),
	Tracing = {block, Line,
			[
				io_format({Module, Function}, V, Line, Same)
			||
				V <- Vars, Perm orelse get_filter(Module, Function, V) == true
			] ++
			[
				io_format({Module, Function}, V, Line, Same)
			||
				V <- Exp, Perm orelse get_filter(Module, Function, V) == true
			]},
	%io:format("Tracing ~p~n", [Tracing]),
	case Tracing of
		{block, Line, []} -> 
		 	[];
		V ->
			V
	end;

%parse_func_expressions({Perm, Module, Function}, {map_field_exact, Line, Key, Value}) ->
%	%Vars = lists:flatten(find_var(Var)),
%	%io:format("Tracing ~p~n", [Tracing]),
%	parse_func_expressions({Perm, Module, Function}, Value);

%parse_func_expressions({Perm, Module, Function}, {map, Line, Map}) ->
%	%Vars = lists:flatten(find_var(Var)),
%	%io:format("Tracing ~p~n", [Tracing]),
%	parse_func_expressions({Perm, Module, Function}, Map);

parse_func_expressions({Perm, Module, Function}, {var, Line, '_'}) ->
	[];

parse_func_expressions({Perm, Module, Function}, {var, Line, Var}) ->
	%Vars = lists:flatten(find_var(Var)),
	%io:format("Tracing ~p~n", [Tracing]),
	Res = case Perm orelse get_filter(Module, Function, Var) == true of
		true ->
			{block, Line, [io_format({Module, Function}, Var, Line)]};
		false ->
			[]
	end,
	case Res of
		{block, Line, []} -> [];
		_ -> Res
	end;

parse_func_expressions(_, V) ->
	[].

parse_expressions_tail({Perm, Module, Function}, {match, Line, A, B}) ->
	%Var = list_to_atom("Res_" ++ atom_to_list(Module) ++ "_" ++atom_to_list(Function)),
	Var = {return, Module, Function},
	ResVar = {var, Line, Var},
	{block, Line,
		[
		 	%Exp
			%{match, Line, ResVar, Exp},
			{match, Line, ResVar, parse_expressions({Perm, Module, Function}, {match, Line, A, B}, 'not_trace')},
			io_format({Module, Function}, Var, Line),
			ResVar
		]
	};

parse_expressions_tail({Perm, Module, Function}, Exp) ->
	Line = element(2, Exp),
	Var = {return, Module, Function},
	%Var = list_to_atom("Res_" ++ atom_to_list(Module) ++ "_" ++atom_to_list(Function)),
	ResVar = {var, Line, Var},
	{block, Line,
		[
		 	%Exp
			%{match, Line, ResVar, Exp},
			{match, Line, ResVar, parse_expressions({Perm, Module, Function}, Exp)},
			io_format({Module, Function}, Var, Line),
			ResVar
		]
	}.

parse_expressions({Perm, Module, Function}, Exp) ->
	parse_expressions({Perm, Module, Function}, Exp, 'after').

parse_expressions({Perm, Module, Function}, {match, Line, Var, Expression}, Order) ->
	%Vars = lists:flatten(find_var(Var)),
	%io:format("Perm 3 ~p~n", [Perm]),
	Var_Watcher = try
		var_watcher(Var)
	catch
		E1:E2 ->
			io:format("E1 ~p~nE2 ~p~nStack ~p~n", [E1, E2, erlang:get_stacktrace()]),
		A = 1/0
	end,
	%io:format("Line ~p~nVar_Watcher ~p~n", [Line, Var_Watcher]),
	{MiddleVar, VarWatcherSafe} = var_watcher_safe(Var),
	%io:format("Var_Watcher_safe ~p~n", [VarWatcherSafe]),
	ListVarsWatcherSafe = lists:flatten(find_var(VarWatcherSafe)),
	%ListVarsWatcher = lists:flatten(Var_Watcher),
	%io:format("Vars ~p~n", [ListVarsWatcherSafe]),

	Same = generate(),
	Tracing = {block, Line,[
		begin
		%io:format("get_filter(Module, Function, V) ~p~n", [get_filter(Module, Function, V)]),
		io_format({Module, Function}, V, Line, Same)
		end
	||
		V <- ListVarsWatcherSafe, Perm orelse get_filter(Module, Function, V) == true
	]},
	%Tracing = {block, Line, []},
	%case Line > 90 andalso Line < 105 of
	%	true -> 
	%io:format("Vars ~p~n", [Vars]),
	%io:format("Tracing ~p~n", [Tracing]),
	%io:format("VarWatcher ~p~n", [Var_Watcher]),
	%io:format("ListVarWatcher ~p~n", [ListVarsWatcherSafe]),
	%io:format("------------------------------------~n"),
	%	_ ->
	%		[]
	%end,

	case Tracing of
		{block, Line, []} -> 
		 	{match, Line, Var, parse_expressions({Perm, Module, Function}, Expression)};
		_ ->
			case Order of
				'after' ->
					TT = {block, Line, 
					 	[
						 	{match, Line, MiddleVar, 
							 	{block, Line,
							 		[
										{match, Line, VarWatcherSafe, parse_expressions({Perm, Module, Function}, Expression, Order)}
									]
								}
							} % продолжаем дальше трассировку
							,
							Tracing,
							{block, Line,[
									{match, Line, Var, MiddleVar}]
							}
							%,
							%parse_expressions({Perm, Module, Function}, Expression, Order)
						] 
					},
					
					%case Line > 90 andalso Line < 105 of
					%	true -> 
					%		io:format("TT ~p~n", [TT]);
					%_ ->
					%	[]
					%end,
					TT
					;
				'not_trace' ->
					{block, Line, 
					 	[
						 	{match, Line, Var, parse_expressions({Perm, Module, Function}, Expression, Order)}
						] 
					}
			end
	end;

parse_expressions({P,M,F}, {'catch', Line, Exp}, Order) ->
	NewExp = parse_expressions({P,M,F}, Exp, Order),
	{'catch', Line, NewExp};

parse_expressions({P,M,F}, {'try', Line, Tries, Cases, Catch, After}, Order) ->
	%io:format("parse try~n"),
	NewTries = lists:map(
		fun(V) -> 
			%io:format("parse try clause ~n~p~n", [V]),
			parse_expressions({P,M,F}, V) 
		end, 
	Tries),
	NewCatch = lists:map(
		fun(V) -> 
			%io:format("parse try catch ~n~p~n", [V]),
			parse_clauses({P,M,F}, V) 
		end, 
	Catch),
	{'try', Line, NewTries, Cases, NewCatch, After};

parse_expressions({P,M,F}, {'case', Line, Var, Clauses}, _) ->
	NewClauses = lists:map(
		fun(V) -> 
			parse_clauses({P,M,F}, V) 
		end, 
	Clauses),
	{'case', Line, Var, NewClauses};

parse_expressions(_, Exp, _) ->
	Exp.

get_from_arguments(Args) ->
	[].

io_format({Module, Function}, Var, Line) ->
	io_format({Module, Function}, Var, Line, "0").

io_format({Module, Function}, Var, Line, Same) ->
	%{block, Line, []}.
	Backend = watcher:get_backend(),
	{block, Line, [
	gen_remote_call(watcher,format,
		[
			%{string,Line,"~30.w|~5.w|~20.s|~40.p|~n"}, 
			gen_list([
				{atom, Line, B},
				{tuple, Line,[{atom, Line, Module}, {atom, Line, Function}]},
				{integer, Line, Line},
			 	case Var of
					{func, M, F} ->
						{tuple, Line, [{atom, Line, func}, {atom, Line, M}, {atom, Line, F}]};
					{return, M, F} ->
						{tuple, Line, [{atom, Line, return}, {atom, Line, M}, {atom, Line, F}]};
					_ ->
						{string,Line,atom_to_list(Var)}
				end,
			 	format_var(Var, Line),
				{string, Line, Same}		
			], Line)
		], Line)
	||
	B <- Backend
	]}.
%	gen_remote_call(io,format,
%		[
%			{string,Line,"~30.w|~5.w|~20.s|~40.p|~n"}, 
%			gen_list([
%				{tuple, Line,[{atom, Line, Module}, {atom, Line, Function}]},
%				{integer, Line, Line},
%			 	{string,Line,atom_to_list(Var)},
%			 	format_var(Var, Line)			
%			], Line)
%		], Line).

gen_call(F, A, Line) ->
	{call, Line, {atom, Line, F}, A}.

gen_remote_call(M,F,A, Line) ->
	{call, Line, {remote, Line, {atom, Line, M}, {atom, Line, F}}, A}.

gen_list(List, Line) ->
	gen_list_do(lists:reverse(List), {nil, Line}, Line).

gen_list_do([], Acc, _Line) ->
	%io:format("gen_list ~p~n", [Acc]),
	Acc;

gen_list_do([H|T], Acc, Line) ->
	gen_list_do(T, {cons, Line, H, Acc}, Line).

%	NameRecord = element(1, R),
%	lists:zip(record_info(fields,NameRecord),tl(tuple_to_list(R)));


format_var({func, _, _}, Line) ->
	{string, Line, ""};

format_var('_', Line) ->
	{string, Line, "_"};

format_var(Var, Line) ->
	{var,Line,Var}.
	%gen_call(element, 
	%{call, Line, {remote, Line, {atom, Line, io_lib_pretty},{atom, Line, print}}, [{var,Line,Var}]}.
