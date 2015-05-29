-module( sqerl_trade ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-export( [ to_money/1, from_money/1, from_json/1 ] ).

-include( "sqerl_trade.hrl" ).

{"userId": "134256", "currencyFrom": "EUR", "currencyTo": "GBP", "amountSell": 1000, "amountBuy": 747.10, "rate": 0.7471, "timePlaced" : "24-JAN-15 10:27:44", "originatingCountry" : "FR"}

%%
%%	Convert a monetary value in string form to a money tuple. This function will crash when provided with
%%	unparseable representations of money.
%%
%% Parameters:
%%	Value	- String representing an amount of money
%%
%% Returns:
%%	Value represented in the form of a money tuple
%%
from_json( [ { Key, Atom } | T ], Map, Trade = #sqerl_trade{} ) ->
	

from_json( JSON ) ->
	Converted = jsx:decode( JSON, [ return_maps ] ),
	Keys = [ 	{ <<"userId">>, trader }, 
			{ <<"currencyFrom">>, from_currency },
			{ <<"currencyTo">>, to_currency },
			{ <<"amountSell">>, from_amount },
			{ <<"amountBuy">>, to_amount },
			{ <<"rate">>, rate },
			{ <<"timePlaced">>, tim },
			{ <<"originatingCountry">>, origin } ],
	from_json( Keys, JSON, #sqerl_trade{} ).



%%
%%	Convert a monetary value in string form to a money tuple. This function will crash when provided with
%%	unparseable representations of money.
%%
%% Parameters:
%%	Value	- String representing an amount of money
%%
%% Returns:
%%	Value represented in the form of a money tuple
%%
to_money( [ IntegerString ] )	-> 
	{ Integer, _Rest } = string:to_integer( IntegerString ),
	{ Integer, 0 };

to_money( [ IntegerString, FractionalString ] ) ->
	{ Integer, _Rest } 	= string:to_integer( IntegerString ),
	{ Fractional, _Rest }	= string:to_integer( FractionalString ),
	{ Integer, Fractional };

%% If we get more than two tokens, it's not a valid input
to_money( Tokens ) when is_list( Tokens ) -> error;

to_money( Value ) when is_binary( Value ) -> to_money( string:tokens( string:strip( binary_to_list( Value ) ), "." ) ).

%%
%%	Convert a monetary value in string form to a money tuple
%%
%% Parameters:
%%	Value	- String representing an amount of money
%%
%% Returns:
%%	Value represented in the form of a money tuple
%%
from_money( { Integer, 0 } ) 		-> integer_to_binary( Integer );

from_money( { Integer, Fractional } ) 	-> 
	IB = integer_to_binary( Integer ),
	FB = integer_to_binary( Fractional ),
	<<IB/binary, ".", FB/binary>>;

%% Not a money tuple
from_money( _Value ) -> error.