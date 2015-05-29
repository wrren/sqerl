-module( sqerl_app ).
-behaviour( application ).

-export( [ start/2, stop/1 ]).

start( _Type, _Args ) ->
	{ ok, Hostname } 	= application:get_env( sqerl_db_hostname ),
	{ ok, Username } 	= application:get_env( sqerl_db_username ),
	{ ok, Password } 	= application:get_env( sqerl_db_password ),
	{ ok, Database } 	= application:get_env( sqerl_db_database ),
	{ ok, Port } 		= application:get_env( sqerl_db_port ),
	{ ok, PoolSize } 	= application:get_env( sqerl_db_pool_size ),

	emysql:add_pool( sqerl_db_pool, PoolSize, Username, Password, Hostname, Port, Database, utf8 ),

	Dispatch = cowboy_router:compile( [
		{'_', [ { "/trades", sqerl_trade_handler, [ sqerl_trade_access ] } ] }
		]),
	{ ok, _ } = cowboy:start_http( my_http_listener, 100, [ { port, 8080 } ],
		[ { env, [ { dispatch, Dispatch } ] } ]
		),
	sqerl_sup:start_link().

stop( _State ) ->
	emysql:remove_pool( sqerl_db_pool ),
	ok.
