-module( sqerl_trades_SUITE ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-include_lib( "common_test/include/ct.hrl" ).
-export( [ all/0 ] ).
-export( [ from_json/1 ] ).

all() -> [ from_json ].

from_json( _Config ) ->
	sqerl_trade:from_json( <<"{	\"userId\": \"134256\", 
					\"currencyFrom\": \"EUR\", 
					\"currencyTo\": \"GBP\", 
					\"amountSell\": 1000, 
					\"amountBuy\": 747.10, 
					\"rate\": 0.7471, 
					\"timePlaced\" : \"24-JAN-15 10:27:44\", 
					\"originatingCountry\" : \"FR\"}">> ).