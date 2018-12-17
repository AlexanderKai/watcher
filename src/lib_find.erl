-module(lib_find).
-export([files/3, files/5, find/1]).
-import(lists, [reverse/1]).
-include_lib("kernel/include/file.hrl" ).

find(Modules) ->
	S = lists:flatten(lists:join(",", Modules)),
	{ok, Cwd} = file:get_cwd(),
	Res = lists:filtermap(fun (E) -> 
	%io:fwrite("Path: ~s~n", [E]), 

	Find = case length(E) < 2 of
	true ->
		Cwd;
	_ ->
		{Ch1, Tail1} = lists:split(1, E),
		{Ch2, _Tail2} = lists:split(2, E),
		if 
			Ch2 == ".." ->
				Cwd;
			Ch2 == "./" ->
				Cwd ++ Tail1;
			Ch1 =/= "/" ->
				Cwd ++ "/" ++ E;
			true ->
				E
		end
	end,
	%io:fwrite("~p~n", [Find ++ "/{test_w,test_t}.{erl,beam}"]),
	F = filelib:wildcard(Find ++ "/{"++ S ++"}.{erl,beam}"), 
	%io:format("~p~n", [Find]), 
	io:format("~p~n", [F]), 
	FinF = lists:map(fun (E) -> list_to_binary(E) end, F),
	case F of [] -> false; _ -> {true, FinF} end end, code:get_path()),
	LF = lists:flatten(Res),
	io:format("LF ~n~p~n", [LF]),	
	FinRes = lists:usort(lists:filtermap(fun(E) ->
	case E of undefined -> false; _ -> {true, E} end end,
	[
	begin
		Sp1 = binary:split(L1, [<<"/">>, <<".">>], [global]),
		Sp2 = binary:split(L2, [<<"/">>, <<".">>], [global]),
		S1 = lists:nth(length(Sp1)-1, Sp1),
		S2 = lists:nth(length(Sp2)-1, Sp2),
		S1T = lists:nth(length(Sp1), Sp1),
		S2T = lists:nth(length(Sp2), Sp2),
		io:format("1 ~p~n", [Sp1]),
		io:format("2 ~p~n", [Sp2]),
		io:format("1 ~p~n", [S1]),
		io:format("2 ~p~n", [S2]),
		io:format("1 ~p~n", [S1T]),
		io:format("2 ~p~n", [S2T]),
		if 
			(S1 == S2 andalso (S1T == <<"erl">>) andalso (S2T == <<"beam">>)) == true ->
				[L1, L2];
			(S1 == S2 andalso (S2T == <<"erl">>) andalso (S1T == <<"beam">>)) == true ->
				[L2, L1];
			true ->
				undefined
		end
	end
	||
	L1 <- LF, L2 <- LF
	])),
	FinRes.

files(Dir, Re, Flag) ->
	%io:format("begin ~n", []),
	sort(
	  reverse(files(Dir, Re, Flag, fun(A, Acc) -> A ++ Acc end, []))
	  , Re)
	.

files(Dir, Reg, Recursive, Fun, Acc) ->
	%io:format("file:list_dir ~p~n", [file:list_dir(Dir)]),
	%io:format("file:list_dir ~p~n", [Dir]),
	case file:list_dir(Dir) of
		{ok, Files} -> 
			%io:format("Files ~p~n", [Files]),
			find_files(Files, Dir, Reg, Recursive, Fun, Acc);
		{error, _} -> Acc
	end.

sort(Files,RegExps) ->
	Found = [
	 	[
			case reg_exp(F, [R]) of
				[V] -> V;
				_ -> undefined
			end
		||
			F <- Files
		]
	||
		R <- RegExps
	],
	[
		[
		 	L
		||
			L <- F, L =/= undefined
		]
	||
		F <- Found
	].

reg_exp(FullName, Regs) ->
	Found = [
		case re:run(FullName, "^(.+?)/(" ++ Reg ++ ")([.]((erl)|(beam)))$", [{capture,none,list}]) of
			match ->
				FullName;
			_ ->
				undefined
		end
	||
		Reg <- Regs
	],
	[F || F <- Found, F =/= undefined].

find_files([File|T], Dir, Reg, Recursive, Fun, Acc0) ->
	FullName = filename:join([Dir,File]),
	%io:format("FullName ~p~n", [FullName]),
	%io:format("FileType ~p~n", [file_type(FullName)]),
	case file_type(FullName) of
		regular ->
			Found = reg_exp(FullName, Reg),
			Acc = Fun(Found, Acc0),
			find_files(T, Dir, Reg, Recursive, Fun, Acc);
			%io:format("re:run ~p~n", [re:run(FullName, Reg)]),
		directory ->
			case Recursive of
				true ->
					Acc1 = files(FullName, Reg, Recursive, Fun, Acc0),
					find_files(T, Dir, Reg, Recursive, Fun, Acc1);
				false ->
					find_files(T, Dir, Reg, Recursive, Fun, Acc0)
			end;
		error ->
			find_files(T, Dir, Reg, Recursive, Fun, Acc0)
	end;

find_files([], _, _, _, _, A) ->
	A.

file_type(File) ->
	case file:read_file_info(File) of
		{ok, Facts} ->
			case Facts#file_info.type of
				regular -> regular;
				directory -> directory;
				_ -> error
			end;
		_ ->
			error
	end.
