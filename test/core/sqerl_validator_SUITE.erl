-module( sqerl_validator_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).

-export( [ all/0 ] ).
-export( [ validate/1 ] ).

all() -> [ validate ].

validate( _Config ) ->
	{ ok, 1 }		= sqerl_validator:validate( 1, 			[] ),
	{ ok, 1 }		= sqerl_validator:validate( 1, 			[ integer ] ),
	{ ok, 1 }		= sqerl_validator:validate( 1, 			[ numeric ] ),
	{ ok, 1.0 }		= sqerl_validator:validate( 1.0, 		[ numeric ] ),
	{ ok, 1 }		= sqerl_validator:validate( 1,			[ integer, { min, 1 } ] ),
	{ ok, "Hello" }		= sqerl_validator:validate( "Hello",		[ string, { length, 5 } ] ),
	{ ok, <<"Hello">> }	= sqerl_validator:validate( <<"Hello">>,	[ string, { length, 5 } ] ),
	{ error, 1, _ }		= sqerl_validator:validate( 1,			[ string ] ).
