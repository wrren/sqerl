-module(sqerl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [ { 	trade_event, 
			{ gen_event, start_link, [ { global, trade_event } ] },
        		permanent, 5000, worker, [ dynamic ] } ],

	{ ok, { { one_for_one, 1, 5 }, Children } }.
