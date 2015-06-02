-module( sqerl_rate_limiter_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).

-export( [ all/0 ] ).
-export( [ limit/1 ] ).

all() -> [ limit ].

limit( _Config ) ->
	{ ok, Pid } = sqerl_rate_limiter:start_link(),

	{ ok, Rate }		= sqerl_rate_limiter:get( 1, 1, 60 ),
	sqerl_rate_limiter:increment( 1 ),
	{ ok, NewRate }		= sqerl_rate_limiter:get( 1, 1, 60 ),
	sqerl_rate_limiter:increment( 1 ),
	{ exceeded, ExRate }	= sqerl_rate_limiter:get( 1, 1, 60 ),

	gen_server:call( Pid, terminate ).
