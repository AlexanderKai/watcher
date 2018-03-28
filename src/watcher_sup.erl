-module(watcher_sup).
-behaviour(supervisor).
-behaviour(application).
-export([init/1, start/2, stop/1, start_link/0]).

start() -> ?MODULE:start_link().
start(_,_) -> ?MODULE:start_link().
start_link() -> supervisor:start_link({local,?MODULE},?MODULE,[]).
stop(_)    -> ok.


init(Args) ->
	SupervisorSpecs =  #{strategy => one_for_one,
						    intensity => 10,
							   period => 1000},
	ChildSpec = [#{id => watcher,
				start => {watcher, start_link, [[]]},
				restart => permanent,
				shutdown => 2000,
				type => worker,
				modules => [watcher]
				}],
{ok, {SupervisorSpecs, ChildSpec}}.

