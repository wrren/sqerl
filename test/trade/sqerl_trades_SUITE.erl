-module( sqerl_trades_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).
-include( "trade/sqerl_trade.hrl" ).

-export( [ all/0 ] ).
-export( [ from_json/1, to_json/1 ] ).

all() -> [ from_json, to_json ].

from_json( _Config ) ->
	Trade = sqerl_trade:from_json( <<"{	\"userId\": \"134256\", 
						\"currencyFrom\": \"EUR\", 
						\"currencyTo\": \"GBP\", 
						\"amountSell\": 1000, 
						\"amountBuy\": 747.10, 
						\"rate\": 0.7471, 
						\"timePlaced\" : \"24-JAN-15 10:27:44\", 
						\"originatingCountry\" : \"FR\"}">> ),

	Trade = #sqerl_trade{	id		= undefined,
				trader		= <<"134256">>,
				from_currency	= <<"EUR">>,
				to_currency	= <<"GBP">>,
				from_amount	= 1000,
				to_amount	= 747.10,
				rate		= 0.7471,
				time		= <<"24-JAN-15 10:27:44">>,
				origin		= <<"FR">> }.

to_json( _Config ) ->
	Trade = #sqerl_trade{	id		= 123,
				trader		= <<"134256">>,
				from_currency	= <<"EUR">>,
				to_currency	= <<"GBP">>,
				from_amount	= 1000,
				to_amount	= 747.10,
				rate		= 0.7471,
				time		= <<"24-JAN-15 10:27:44">>,
				origin		= <<"FR">> },

	JSON = sqerl_trade:to_json( Trade ),

	{ _, Trade } = sqerl_record:from_json( JSON, #{}, sqerl_trade, record_info( fields, sqerl_trade ) ).