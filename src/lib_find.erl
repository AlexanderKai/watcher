-module(lib_find).
-export([files/3, files/5]).
-import(lists, [reverse/1]).
-include_lib("kernel/include/file.hrl" ).

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
			{match, _} ->
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
