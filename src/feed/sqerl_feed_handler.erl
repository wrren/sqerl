-module( sqerl_feed_handler ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).
-behaviour( cowboy_http_handler ).
-behaviour( gen_event ).

-include( "trade/sqerl_trade.hrl" ).

%%
%%	http_handler/websocket_handler callbacks
%%
-export( [ init/3, websocket_init/3, handle/2, websocket_handle/3, websocket_info/3, websocket_terminate/3, terminate/3 ] ).

%%
%%	gen_event Handler Callbacks
%%
-export( [ init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3, terminate/2 ] ).

-record( state, {} ).

%%
%%	Initialize the handler, upgrade to the cowboy_websocket protocol
%%
init( _, _, _ ) ->
	{ upgrade, protocol, cowboy_websocket }.

%%
%%	Called after upgrading to the websocket protocol, sets up module state
%%
websocket_init( _TransportName, Req, _Opts ) ->
	sqerl_trade:subscribe( { sqerl_feed_handler, self() }, self() ),
	{ ok, Req, #state{} }.

%%
%%	Handle an incoming message from the web socket
%%
websocket_handle( { text, Content }, Req, State ) ->
	{ reply, { text, Content }, Req, State };

websocket_handle( _Content, Req, State ) ->
	{ ok, Req, State }.

%%
%%	Handle an erlang message from the system. If the incoming message is a 
%%	new trade broadcast by the gen_event server, send it to the connected
%%	client as JSON.
%%
websocket_info( { new_trade, Trade }, Req, State ) ->
	{ reply, { text, sqerl_trade:to_json( Trade ) }, Req, State };

websocket_info( _Message, Req, State ) ->
	{ ok, Req, State }.

%%
%%	Unused Callback
%%
handle( Req, State ) -> { ok, Req, State }.

%%
%%	Websocket-specific cleanup
%%
websocket_terminate( _Reason, _Req, _State ) ->
	sqerl_trade:unsubscribe( { sqerl_feed_handler, self() }, self() ),
	ok.

%%
%%	Execute any required cleanup
%%
terminate( _Reason, _Req, _State ) ->
	ok.

%%
%%	gen_event callbacks
%%

%%
%%	Initialize the Event Handler
%%
init( Pid ) ->
	{ ok, Pid }.

%%
%%	Handle an asynchronous event notification
%%
handle_event( Event = { new_trade, _ }, Pid ) ->
	Pid ! Event,
	{ ok, Pid };

handle_event( Event, State ) ->
	lager:debug( "Received Unknown Event :p", [ Event ] ),
	{ ok, State }.

%%
%%	Handle a synchronous event call
%%
handle_call( _Event, State) ->
	{ ok, ok, State }.

%%
%%	Handle an arbitrary message sent to the handler
%%
handle_info( _, State ) ->
	{ ok, State }.
 
%%
%%	Called on release update
%%
code_change( _OldVsn, State, _Extra ) ->
	{ ok, State }.

%%
%%	Terminate the event handler
%%
terminate( _Reason, _State ) ->
	ok.
