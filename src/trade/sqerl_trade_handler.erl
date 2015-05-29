-module( sqerl_trade_handler ).
-behaviour( cowboy_http_handler ).

-export( [ init/3, rest_init/2, handle/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, from_json/2, terminate/3 ] ).

-record( state, { access } ).

init( _, _, _ ) ->
	{ upgrade, protocol, cowboy_rest }.

rest_init( Req, [ AccessModule ] ) ->
	 { ok, Req, #state{ access = AccessModule } }.

allowed_methods( Req, State ) ->
	{ [ <<"POST">> ], Req, State }.

content_types_provided( Req, State ) ->
	{ [ { { <<"application">>, <<"json">>, [] }, to_json } ], Req, State }.

from_json( Req, State = #state{ access = AccessModule } ) -> 
	{ ok, Body, Req2 } = cowboy_req:body( Req ),
	Trade = sqerl_trade:from_json( Body ),
	AccessModule:record( Trade ),
	{ true, Req2, State }.

content_types_accepted( Req, State ) ->
	{ [ { { <<"application">>, <<"json">>, [] }, from_json } ], Req, State }.

handle( Req, State = #state{} ) ->
	{ ok, Req2 } = cowboy_req:reply( 200, Req ),
	{ ok, Req2, State }.

terminate( _Reason, _Req, _State ) ->
	ok.
