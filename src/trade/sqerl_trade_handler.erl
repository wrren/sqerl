-module( sqerl_trade_handler ).
-behaviour( cowboy_http_handler ).

-export( [ init/3, handle/2, terminate/3 ] ).

-record( state, {} ).

init( _, Req, _Opts ) ->
	{ cowboy_rest, Req2, #state{} }.

handle( Req, State = #state{} ) ->
	{ ok, Req2 } = cowboy_req:reply( 200, Req ),
	{ ok, Req2, State }.

terminate( _Reason, _Req, _State ) ->
	ok.
