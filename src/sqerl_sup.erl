-module( sqerl_sup ).
-behaviour( supervisor ).

-export( [ start_link/0 ] ).
-export( [ init/1 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

init([]) ->
	Children = [ 	{ 	sqerl_trade_event, 
				{ gen_event, start_link, [ { global, sqerl_trade_event } ] },
        			permanent, 5000, worker, [ dynamic ] },
        		{	sqerl_rate_limiter,
        			{ sqerl_rate_limiter, start_link, [] },
        			permanent, 5000, worker, [ sqerl_rate_limiter ] } ],

	{ ok, { { one_for_one, 1, 5 }, Children } }.
