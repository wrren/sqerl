-module(sqerl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Props = [],
	{ ok, { { one_for_one, 1, 5 }, 
		Props } }.
