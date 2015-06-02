-module( sqerl_rate_limiter ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-behaviour( cowboy_middleware ).
-behaviour( gen_server ).

%%
%%	gen_server callbacks
%%
-export( [ start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ] ).

%%
%%	cowboy_middleware exports
%%
-export( [ execute/2, get/3, increment/1 ] ).

%%
%%	Set the ETS table ID to the name of this module
%%
-define( ETS_TABLE_ID, ?MODULE ).

%%
%%	Too Many Requests HTTP Status Code
%%
-define( TOO_MANY_REQUESTS, 429 ).

%%
%%	Server State
%%
-record( state, { table } ).

%%
%%	Per-client rate information
%%
-record( rate_info, { 	user 		:: term(),			%% User Key
			window		:: erlang:timestamp(),		%% Timestamp at which the window began
			requests 	:: non_neg_integer() } ).	%% Number of requests so far inside the window

%%
%%	User keying type is either an IP address tuple or integer user ID
%%
-type user_key() :: tuple() | integer().

%%
%%	Get the rate information associated with the client identified by the given key. 
%%
%%	If no rate information exists for the specified client, or the window associated with the
%%	client is older than WindowLength, a new rate_info record will be initialized and 
%%	returned, with the window start timestamp set to now and requests to zero. 
%%
%%	If there is a window associated with the client with a window age less than the given
%%	window length, it will be returned.
%%
%%	Returns a tuple indicating if the user has exceeded their limit and the rate info record.
%%
%% Parameters:
%%	Key		- Client Key
%%	Limit 		- Maximum number of requests allowed per window interval
%%	WindowLength	- Length of a rate limiting window in seconds
%%
%% Returns:
%%	{ ok, RateInfo }	- If the user's request rate is within limits
%%	{ exceeded, RateInfo }	- if the user has exceeded the rate limit
%%
-spec get( user_key(), non_neg_integer(), non_neg_integer() ) -> { ok, #rate_info{} } | { exceeded, #rate_info{} }.
get( Key, Limit, WindowLength ) ->
	case ets:lookup( ?ETS_TABLE_ID, Key ) of
		[ RateInfo = #rate_info{ window = Window, requests = Requests } ] -> 
			Diff = timer:now_diff( Window, os:timestamp() ),
			case { Diff > ( WindowLength * 1000000 ), Requests > Limit } of
				{ true, _ } 		-> 
					NewInfo = #rate_info{ user = Key, window = os:timestamp(), requests = 0 },
					ets:insert( ?ETS_TABLE_ID, NewInfo ),
					{ ok, NewInfo };
				{ false, true }		-> { exceeded, RateInfo };
				{ false, false }	-> { ok, RateInfo }
			end;

		[] -> 	NewInfo = #rate_info{ user = Key, window = os:timestamp(), requests = 0 },
			ets:insert( ?ETS_TABLE_ID, NewInfo ),
			{ ok, NewInfo }
	end.

%%
%%	Increment the request count for the user with the specified key
%%
-spec increment( integer() | tuple() ) -> integer().
increment( Key ) ->
	ets:update_counter( ?ETS_TABLE_ID, Key, { #rate_info.requests, 1 } ).

%%
%%	Parse the incoming request and determine whether it should be halted due to
%%	the rate limit being exceeded ( based on the limiting policy defined in the
%%	application configuration ). If the rate limit is exceeded, reply to the client
%%	with status code 429 ( Too Many Requests ) and JSON describing the rate
%%	limits.
%%
%% Parameters:
%%	Req	- Request Object
%%	Env	- Environment Object
%%
execute( Req, Env ) -> 
	{ ok, Factors } 	= application:get_env( sqerl, sqerl_rate_limit_factors ),
	{ ok, Limit }		= application:get_env( sqerl, sqerl_rate_limit ),
	{ ok, WindowLength }	= application:get_env( sqerl, sqerl_rate_limit_window ),
	execute( Req, Env, Factors, Limit, WindowLength ).

%% If the user's rate is within limit, increment request count and continue
execute( Req, Env, User, { ok, _RateInfo }, Factors, Limit, WindowLength ) ->
	increment( User ),
	execute( Req, Env, Factors, Limit, WindowLength );

%% If the user has exceeded the rate limit, return 429
execute( Req, _Env, _User, { exceeded, _RateInfo }, _Factors, _Limit, _WindowLength ) ->
	Body = jsx:encode( #{ 	<<"result">> 	=> <<"failure">>,
				<<"error">>	=> <<"rate limit exceeded">> } ),
	{ ok, Req2 }	= cowboy_req:reply( 	?TOO_MANY_REQUESTS,
						[ {<<"content-type">>, <<"application/json">> } ],
						Body, Req ),
	{ halt, Req2 }.

%%
%%	Limit the request rate based on the User ID encoded in the trade submission data. If
%%	we can't parse the data, fall back to remaining limiting factors.
%%
execute( Req, Env, [ user | Rest ], Limit, WindowLength ) ->
	{ Method, Req2 } 	= cowboy_req:method( Req ),
	{ Path, Req3 }		= cowboy_req:path( Req2 ),
	case { Method, Path } of
		%% Incoming trade submission, we can handle this
		{ <<"POST">>, <<"/trades">> } ->
			%% Destructively gets the body from the request, so we need to reset it later
			{ ok, Body, _Req4 } = cowboy_req:body( Req3 ),
			Trade 	= sqerl_trade:from_json( Body ),
			%% Interpret the userID as the limiting factor
			User 	= sqerl_trade:user( Trade ),
			%% Set the body again so that handlers can access it
			Req5 	= cowboy_req:set( [ { buffer, Body } ], Req3 ),
			execute( Req5, Env, User, get( User, Limit, WindowLength ), Rest, Limit, WindowLength );

		%% Route where we can't user per-user rate limiting, fall back to remaining factors
		_ -> 
			lager:debug( "Got Method ~p and Path ~p", [ Method, Path ] ),
			execute( Req3, Env, Rest, Limit, WindowLength )
	end;

execute( Req, Env, [ ip | Rest ], Limit, WindowLength ) ->
	{ { IP, _Port }, Req2 } = cowboy_req:peer( Req ),
	execute( Req2, Env, IP, get( IP, Limit, WindowLength ), Rest, Limit, WindowLength );

execute( Req, Env, [], _Limit, _WindowLength ) ->
	{ ok, Req, Env }.

%%
%%	Called by the application supervisor. Starts the module
%%	and links to it so that its state can be monitored.
%%
start_link() ->
	gen_server:start_link( ?MODULE, [], [] ).

%%
%%	Initialize the rate limiter
%%
init( _Args ) ->
	Table = ets:new( 	?ETS_TABLE_ID, 
				[ 	set,
					named_table,
					{ keypos, #rate_info.user },
					{ read_concurrency, true },
					{ write_concurrency, true },
					public ] ),
	{ ok, #state{ table = Table } }.

%%
%%	Handle a call to terminate the server
%%
handle_call( terminate, _From, State ) ->
	{ stop, normal, ok, State };

%%
%%	Handle a synchronous call. Since this module is only for wrapping the
%%	ets table, we don't do anything.
%%
handle_call( _Request, _From, State ) ->
	{ noreply, State }.

%%
%%	Handle an asynchronous call. Since this module is only for wrapping the
%%	ets table, we don't do anything.
%%
handle_cast( _Request, State ) ->
	{ noreply, State }.

%%
%%	Called when we receive any out-of-band messages. Ignored.
%%
handle_info( _Info, State ) ->
	{ noreply, State }.
	
%%
%%	Called on module code update, allows us to convert the state to a new format if needed
%%
code_change( _Old, State, _Ext ) ->
	{ ok, State }.

%%
%%	Terminate the server. Deletes the ets table used to hold client
%%	connection information.
%%	
terminate( _Reason, #state{ table = Table } ) ->
	ets:delete( Table ).