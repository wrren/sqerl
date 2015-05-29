-module( sqerl_trade_access ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include( "sqerl_trade.hrl" ).
-include_lib("emysql/include/emysql.hrl").

-export( [ by_id/1, by_trader/3, by_origin/3, record/1 ] ).

%%
%%	Convert a result set into a list of sqerl_trade records
%%
%% Parameters:
%%	result_packet	- emysql result packet
%%
%% Returns:
%%	List of sqerl_trade records
%%
convert( #result_packet{ rows = Rows } ) -> convert( Rows, [] );

convert( [ ID, Trader, Origin, FromCurrency, ToCurrency, FromAmount, ToAmount, Rate, Time ] ) ->
	#sqerl_trade{ 	id 		= ID, 
			trader 		= Trader, 
			origin 		= Origin, 
			from_currency 	= FromCurrency, 
			to_currency 	= ToCurrency,
			from_amount 	= sqerl_trade:to_money( FromAmount ),
			to_amount 	= sqerl_trade:to_money( ToAmount ),
			rate 		= sqerl_trade:to_money( Rate ),
			time 		= Time }.

convert( [ H | T ], Converted ) ->
	convert( T, [ convert( H ) | Converted ] );

convert( [], Converted ) -> lists:reverse( Converted ).

%%
%%	Retrieve a specific trade by ID
%%
%% Parameters:
%%	ID	- Trade ID
%%
%% Returns:
%%	{ ok, Trade } 		on success
%%	{ error, Reason	}	on failure
%%
-spec by_id( non_neg_integer() ) -> { ok, #sqerl_trade{} } | { error, binary() }.
by_id( ID ) -> 
	emysql:prepare( sqerl_trade_by_id, <<"	SELECT 	t.id 		AS id,
							tr.ext_id	AS trader,
							o.name		AS origin,
							fc.name		AS from_currency,
							tc.name		AS to_currency,
							t.from_amount	AS from_amount,
							t.to_amount	AS to_amount,
							t.rate		AS rate,
							t.time		AS time
						FROM trades AS t
							JOIN traders AS tr ON tr.id = t.trader
							JOIN origins AS o ON o.id = t.origin,
							JOIN currencies AS fc ON fc.id = t.from_currency,
							JOIN currencies AS tc ON rc.id = t.to_currency
						WHERE t.id = ? LIMIT 1">> ),

	case emysql:execute( sqerl_db_pool, sqerl_trade_by_id, [ ID ] ) of
		Result = #result_packet{}	-> { ok, lists:nth( 1, convert( Result ) ) };
		#error_packet{ msg = Message }	-> { error, Message }
	end.

%%
%%	Retrieve a list of trades made by the specified user in order of ascending trade time
%%
%% Parameters:
%%	Trader	- Trader External ID
%%	Limit 	- Limit on returned result set size
%%	Offset 	- Offset into result set where returned list should begin
%%
%% Returns:
%%	{ ok, [ Trades ] }	on success
%%	{ error, Reason	}	on failure
%%
-spec by_trader( non_neg_integer(), non_neg_integer(), non_neg_integer() ) -> { ok, [ #sqerl_trade{} ] } | { error, binary() }.
by_trader( Trader, Limit, Offset ) -> 
	emysql:prepare( sqerl_trade_by_trader, <<"	SELECT 	t.id 		AS id,
								tr.ext_id	AS trader,
								o.name		AS origin,
								fc.name		AS from_currency,
								tc.name		AS to_currency,
								t.from_amount	AS from_amount,
								t.to_amount	AS to_amount,
								t.rate		AS rate,
								t.time		AS time
							FROM trades AS t
								JOIN traders AS tr ON tr.id = t.trader
								JOIN origins AS o ON o.id = t.origin,
								JOIN currencies AS fc ON fc.id = t.from_currency,
								JOIN currencies AS tc ON rc.id = t.to_currency
							WHERE tr.ext_id = ? 
							ORDER BY t.time ASC
							LIMIT ? OFFSET ?">> ),

	case emysql:execute( sqerl_db_pool, sqerl_trade_by_trader, [ Trader, Limit, Offset ] ) of
		Result = #result_packet{}	-> { ok, convert( Result ) };
		#error_packet{ msg = Message }	-> { error, Message }
	end.

%%
%%	Retrieve a list of trades made in the specified origin in order of ascending trade time
%%
%% Parameters:
%%	Origin	- Origin ID
%%	Limit 	- Limit on returned result set size
%%	Offset 	- Offset into result set where returned list should begin
%%
%% Returns:
%%	{ ok, [ Trades ] }	on success
%%	{ error, Reason	}	on failure
%%
-spec by_origin( non_neg_integer(), non_neg_integer(), non_neg_integer() ) -> { ok, [ #sqerl_trade{} ] } | { error, binary() }.
by_origin( Origin, Limit, Offset ) ->
	emysql:prepare( sqerl_trade_by_origin, <<"	SELECT 	t.id 		AS id,
								tr.ext_id	AS trader,
								o.name		AS origin,
								fc.name		AS from_currency,
								tc.name		AS to_currency,
								t.from_amount	AS from_amount,
								t.to_amount	AS to_amount,
								t.rate		AS rate,
								t.time		AS time
							FROM trades AS t
								JOIN traders AS tr ON tr.id = t.trader
								JOIN origins AS o ON o.id = t.origin,
								JOIN currencies AS fc ON fc.id = t.from_currency,
								JOIN currencies AS tc ON rc.id = t.to_currency
							WHERE o.id = ? 
							ORDER BY t.time ASC
							LIMIT ? OFFSET ?">> ),

	case emysql:execute( sqerl_db_pool, sqerl_trade_by_origin, [ Origin, Limit, Offset ] ) of
		Result = #result_packet{}	-> { ok, convert( Result ) };
		#error_packet{ msg = Message }	-> { error, Message }
	end.

%%
%%	Record a trade in the database
%%
%% Parameters:
%%	Trade	- Trade Record
%%
%% Returns:
%%	ok			on success
%%	{ error, Reason	}	on failure
%%
-spec record( #sqerl_trade{} ) -> ok | { error, binary() }.
record( #sqerl_trade{ trader = Trader, origin = Origin, from_currency = FromCurrency, to_currency = ToCurrency, from_amount = FromAmount, to_amount = ToAmount, rate = Rate, time = Time } ) -> 
	emysql:prepare( sqerl_trade_record_trader, 	<<"INSERT INTO traders ( ext_id ) VALUES ( ? ) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID( id )">> ),
	emysql:prepare( sqerl_trade_record_origin, 	<<"INSERT INTO origins ( name ) VALUES ( ? ) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID( id )">> ),
	emysql:prepare( sqerl_trade_record_currency, 	<<"INSERT INTO currencies ( name ) VALUES ( ? ) ON DUPLICATE KEY UPDATE id = LAST_INSERT_ID( id )">> ),
	emysql:prepare( sqerl_trade_record_trade, 	<<"INSERT INTO trades ( trader, origin, from_currency, to_currency, from_amount, to_amount, rate, time ) VALUES ( ?, ?, ?, ?, ?, ?, ?, ? )">> ),

	#ok_packet{ insert_id = TraderID } 		= emysql:execute( sqerl_db_pool, sqerl_trade_record_trader, [ Trader ] ),
	#ok_packet{ insert_id = OriginID } 		= emysql:execute( sqerl_db_pool, sqerl_trade_record_origin, [ Origin ] ),
	#ok_packet{ insert_id = FromCurrencyID } 	= emysql:execute( sqerl_db_pool, sqerl_trade_record_currency, [ FromCurrency ] ),
	#ok_packet{ insert_id = ToCurrencyID } 		= emysql:execute( sqerl_db_pool, sqerl_trade_record_currency, [ ToCurrency ] ),
	#ok_packet{} 					= emysql:execute( sqerl_db_pool, sqerl_trade_record_trade, [ 	TraderID, 
															OriginID, 
															FromCurrencyID, 
															ToCurrencyID, 
															sqerl_trade:from_money( FromAmount ), 
															sqerl_trade:from_money( ToAmount ), 
															sqerl_trade:from_money( Rate ), 
															Time ] ),
	ok.