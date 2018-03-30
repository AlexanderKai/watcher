-module(watcher_pt).
-export([parse_transform/2]).

get_filter(Module) ->
	All = ets:lookup(watcher, 'ALL'),
	case All of
		[] ->
			Res = ets:lookup(watcher, Module),
			io:format("get_filter Module ~p~n", [Res]),
			case Res of
				[] ->
					false;
				_ ->
					%io:format("get_filter/1 Res ~p~n",[Res]),
					lists:max([
						begin
							io:format("R ~p~n", [R]), 
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
	Res = [parse({Perm, Module}, T) || T <- AST],
	io:format("============================Original========================~n~p~n", [AST]),
	io:format("============================Modified========================~n~p~n", [Res]),
	%io:format("~60.w~60.w", [AST, Res]),
	file:write_file("orig", io_lib:format("~p.~n", [AST])),
	file:write_file("modf", io_lib:format("~p.~n", [Res])),
	try
	Check = erl_lint:module(Res),
	io:format("============================Check========================~n~p~n", [Check])
	catch
		E1:E2 -> io:format("E1~p~nE2~p~n", [E1, E2]),
				 io:format("Stack ~p~n",[erlang:get_stacktrace()])
	end,
	Res. 

parse({ModulePerm, Module}, {function, Line, Function, Arity, Clauses}) ->
	Perm = ModulePerm orelse get_filter(Module, Function), 
	io:format("Perm 2 ~p~nget_filter(Module, Function)~p~n", [Perm, get_filter(Module,Function)]), 
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
	%io:format("parse_func_clauses Arguments~p~n", [Arguments]),
	FunctionMatching = lists:flatten(
		lists:map(
		fun(V) ->
	%		io:format("parse func_expressions V ~p~n~p~n~p~n~p~n", [P, M, F, V]),
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
	{clause, Line, Arguments, GuardSeq, FunctionMatching ++ NewExpressions ++ LastExpression}.

parse_clauses({P,M,F}, {clause,Line,Arguments,GuardSeq,Expressions}) ->
	%io:format("ClausePattern ~p~n", [Expressions]),
	NewExpressions = lists:map(
		fun(V) -> 
			parse_expressions({P,M,F}, V) 
		end, 
	Expressions),
	{clause, Line, Arguments, GuardSeq, NewExpressions}.

%var_watcher(V) ->
%	var_watcher(V, []).

var_watcher({string, Line, String}) ->
	{string, Line, String};

var_watcher({atom, Line, Atom}) ->
	{atom, Line, Atom};

var_watcher({var, Line, Var}) ->
	ListAtom = atom_to_list(Var),
	CheckUnbound = hd(ListAtom),
	case CheckUnbound of
		"_" ->
			{string, Line, ListAtom};
		_ ->
			{var, Line, list_to_atom(ListAtom ++ "_WTCH")}
	end;

%var_watcher({var, Line, Var}) ->
%	{var, Line, list_to_atom(atom_to_list(Var) ++ "_WTCH")};

var_watcher({cons, Line, Head, Tail}) ->
	{cons, Line, var_watcher(Head), var_watcher(Tail)};

var_watcher({nil, Line}) ->
	{nil, Line};

var_watcher({tuple, Line, Elements}) ->
	lists:map(
		fun(V) -> 
			var_watcher(V) 
		end, 
	Elements).

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
	%io:format("Exps ~p~n", [Exp]),
	%io:format("Vars ~p~n", [Vars]),
	Tracing = {block, Line,
			[
				io_format({Module, Function}, V, Line)
			||
				V <- Vars, Perm orelse get_filter(Module, Function, V) == true
			] ++
			[
				io_format({Module, Function}, V, Line)
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
	Var = list_to_atom("Res_" ++ atom_to_list(Module) ++ "_" ++atom_to_list(Function)),
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
	Var = list_to_atom("Res_" ++ atom_to_list(Module) ++ "_" ++atom_to_list(Function)),
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
	Vars = lists:flatten(find_var(Var)),
	%io:format("Perm 3 ~p~n", [Perm]),
	io:format("Vars ~p~n", [Vars]),
	Var_Watcher = try
		var_watcher(Var)
	catch
		E1:E2 ->
			io:format("E1 ~p~nE2 ~p~n", [E1, E2]),
		A = 1/0
	end,
	ListVarsWatcher = lists:flatten(find_var(Var_Watcher)),

	Tracing = {block, Line,[
		begin
		%io:format("get_filter(Module, Function, V) ~p~n", [get_filter(Module, Function, V)]),
		io_format({Module, Function}, V, Line)
		end
	||
		V <- ListVarsWatcher, Perm orelse get_filter(Module, Function, V) == true
	]},
	io:format("Tracing ~p~n", [Tracing]),
	io:format("VarWatcher ~p~n", [Var_Watcher]),

	case Tracing of
		{block, Line, []} -> 
		 	{match, Line, Var, parse_expressions({Perm, Module, Function}, Expression)};
		_ ->
			case Order of
				'after' ->
					{block, Line, 
					 	[
						 	{match, Line, Var_Watcher, parse_expressions({Perm, Module, Function}, Expression, Order)}
							,
							Tracing,
							{block, Line,
								lists:map(
								fun(V) -> 
									{match, Line, Var, Var_Watcher}
								end, 
								Vars)
							}
							%,
							%parse_expressions({Perm, Module, Function}, Expression, Order)
						] 
					};
				'not_trace' ->
					{block, Line, 
					 	[
						 	{match, Line, Var, parse_expressions({Perm, Module, Function}, Expression, Order)}
						] 
					}
			end
	end;

parse_expressions({P,M,F}, {'case', Line, Var, Clauses}, _) ->
	NewClauses = lists:map(
		fun(V) -> 
			parse_clauses({P,M,F}, V) 
		end, 
	Clauses),
	{'case', Line, Var, NewClauses};

parse_expressions(_, T, _) ->
	T.

get_from_arguments(Args) ->
	[].

io_format({Module, Function}, Var, Line) ->
	%{block, Line, []}.
	gen_remote_call(watcher,format,
		[
			%{string,Line,"~30.w|~5.w|~20.s|~40.p|~n"}, 
			gen_list([
				{tuple, Line,[{atom, Line, Module}, {atom, Line, Function}]},
				{integer, Line, Line},
			 	{string,Line,atom_to_list(Var)},
			 	format_var(Var, Line)			
			], Line)
		], Line).
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


format_var('_', Line) ->
	{string, Line, "_"};

format_var(Var, Line) ->
	{var,Line,Var}.
	%gen_call(element, 
	%{call, Line, {remote, Line, {atom, Line, io_lib_pretty},{atom, Line, print}}, [{var,Line,Var}]}.
