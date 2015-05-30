-module( sqerl_trade_handler ).
-behaviour( cowboy_http_handler ).

-export( [ init/3, rest_init/2, handle/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, from_json/2, terminate/3 ] ).

-record( state, { access } ).

%%
%%	Initialize the handler, upgrade to the cowboy_rest protocol
%%
init( _, _, _ ) ->
	{ upgrade, protocol, cowboy_rest }.

%%
%%	Called after upgrading to the rest protocol, sets up module state
%%
rest_init( Req, [ AccessModule ] ) ->
	{ ok, Req, #state{ access = AccessModule } }.

%%
%%	We only accept POST messages
%%
allowed_methods( Req, State ) ->
	{ [ <<"POST">> ], Req, State }.

%%
%%	We only respond with JSON
%%
content_types_provided( Req, State ) ->
	{ [ { { <<"application">>, <<"json">>, [] }, to_json } ], Req, State }.

%%
%%	Handle a request with an application/json content type. Decode the trade data
%%	and record it using the access layer module.
%%
from_json( Req, State = #state{ access = AccessModule } ) ->
	{ ok, Body, Req2 } = cowboy_req:body( Req ),
	Trade = sqerl_trade:from_json( Body ),
	AccessModule:record( Trade ),
	{ true, Req2, State }.

%%
%%	The module only acceps JSON content
%%
content_types_accepted( Req, State ) ->
	{ [ { { <<"application">>, <<"json">>, [] }, from_json } ], Req, State }.

handle( Req, State = #state{} ) ->
	{ ok, Req, State }.

%%
%%	Execute any required cleanup
%%
terminate( _Reason, _Req, _State ) ->
	ok.
