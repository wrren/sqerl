-module( sqerl_trade_handler ).
-behaviour( cowboy_http_handler ).

%%
%%	Cowboy Handler Callbacks
%%
-export( [ init/3, rest_init/2, handle/2, allowed_methods/2, content_types_provided/2, content_types_accepted/2, from_json/2, terminate/3 ] ).

%%
%%	Module State
%%
-record( state, { access } ).

%%
%%	Relevant HTTP Status Codes
%%
-define( HTTP_BAD_REQUEST, 		400 ).
-define( HTTP_CREATED,			201 ).
-define( HTTP_UNPROCESSABLE_ENTITY,	422 ).

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
from_json( Req, State = #state{ access = AccessModule }, true ) ->
	{ ok, Body, Req2 } = cowboy_req:body( Req ),
	%% Parse the JSON body and create a sqerl_trade record
	case sqerl_trade:from_json( Body ) of
		{ [ id ], Trade } 			->
			%% Notify any event handlers that a new trade has been received
			sqerl_trade:notify( Trade ),
			%% Write the trade to storage
			AccessModule:record( Trade ),
			%% Respond
			{ ok, Req3 } = cowboy_req:reply( 	201, 
								[ {<<"content-type">>, <<"application/json">>} ], 
								jsx:encode( #{ <<"result">> => <<"success">> } ), 
								Req2 ),
			{ halt, Req3, State };
			
		{ Errors, Trade }	->
			lager:debug( "Missing or Invalid Fields During Trade Deserialization: ~p, ~p", [ Trade, Errors ] ),
			%% Trade data failed validation/deserialization, respond with an error
			{ ok, Req3 } = cowboy_req:reply( 	?HTTP_UNPROCESSABLE_ENTITY, 
								[ {<<"content-type">>, <<"application/json">>} ], 
								jsx:encode( #{ 	<<"result">> 	=> <<"failure">>,
										<<"error">>	=> <<"invalid trade data">> } ), 
								Req2 ),
			{ halt, Req3, State }
	end;

%%
%%	If there's no request body, respons with an error
%%
from_json( Req, State, false ) ->
	{ ok, Req2 } = cowboy_req:reply( 	?HTTP_BAD_REQUEST, 
						[ {<<"content-type">>, <<"application/json">>} ], 
						jsx:encode( #{ 	<<"result">> 	=> <<"failure">>,
								<<"error">>	=> <<"missing trade data">> } ), 
						Req ),
	{ halt, Req2, State }.


from_json( Req, State ) ->
	from_json( Req, State, cowboy_req:has_body( Req ) ).


%%
%%	The module only acceps JSON content
%%
content_types_accepted( Req, State ) ->
	{ [ { { <<"application">>, <<"json">>, [] }, from_json } ], Req, State }.

%%
%%	Unused Callback
%%
handle( Req, State ) ->
	{ ok, Req, State }.

%%
%%	Execute any required cleanup
%%
terminate( _Reason, _Req, _State ) ->
	ok.
