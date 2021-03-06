-module( sqerl_app ).
-behaviour( application ).

-export( [ start/2, stop/1 ]).

start( _Type, _Args ) ->
	{ ok, Listen }		= application:get_env( sqerl_listen_port ),
	{ ok, Hostname } 	= application:get_env( sqerl_db_hostname ),
	{ ok, Username } 	= application:get_env( sqerl_db_username ),
	{ ok, Password } 	= application:get_env( sqerl_db_password ),
	{ ok, Database } 	= application:get_env( sqerl_db_database ),
	{ ok, Port } 		= application:get_env( sqerl_db_port ),
	{ ok, PoolSize } 	= application:get_env( sqerl_db_pool_size ),

	emysql:add_pool( sqerl_db_pool, PoolSize, Username, Password, Hostname, Port, Database, utf8 ),

	Dispatch = cowboy_router:compile( [	{ '_', [ 	{ "/trade", sqerl_trade_handler, [ sqerl_trade_access ] }, 
								{ "/feed", sqerl_feed_handler, [] } ] }	] ),

	{ ok, _ } = cowboy:start_http( 	my_http_listener, 
					100, 
					[ { port, Listen } ],
					[ 	{ middlewares, [ sqerl_rate_limiter, cowboy_router, cowboy_handler ] },
						{ env, [ { dispatch, Dispatch } ] } ] ),

	lager:info( "sqerl listening on port ~p", [ Listen ] ),

	sqerl_sup:start_link().

stop( _State ) ->
	emysql:remove_pool( sqerl_db_pool ),
	ok.
