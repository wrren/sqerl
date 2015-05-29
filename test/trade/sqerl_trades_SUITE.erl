-module( sqerl_trades_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).
-export( [ all/0 ] ).
-export( [ to_money/1, from_money/1 ] ).

all() -> [ to_money, from_money ].

to_money( _Config ) ->
	{ 15, 67 } 	= sqerl_trades:to_money( <<"15.67">> ),
	{ 10, 0 }	= sqerl_trades:to_money( <<"10">> ),
	error		= sqerl_trades:to_money( <<"10.0.45">> ).

from_money( _Config ) ->
	<<"10.76">>	= sqerl_trades:from_money( { 10, 76 } ),
	<<"15">>	= sqerl_trades:from_money( { 15, 0 } ),
	error 		= sqerl_trades:from_money( { 15, 17, 18 } ).