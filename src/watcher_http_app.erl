-module(watcher_http_app).
-behaviour(application).

%% API.
-export([start/0, start/2]).
-export([stop/1]).

%% API.
start() ->
	start(undefined, undefined).

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", watcher_http_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(watcher_http_back, 100, [{port, 7890}], [
		{env, [{dispatch, Dispatch}]}
	]),
	watcher_http_sup:start_link().

stop(_State) ->
	ok.
