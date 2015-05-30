-module( sqerl_trade ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-export( [ from_json/1 ] ).

-include( "sqerl_trade.hrl" ).

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
-spec from_json( binary() ) -> #sqerl_trade{}.
from_json( JSON ) ->
	
	%% Mapping of JSON keys to record field names
	Keys = #{ 	trader		=> <<"userId">>, 
			from_currency	=> <<"currencyFrom">>,
			to_currency	=> <<"currencyTo">>,
			from_amount	=> <<"amountSell">>,
			to_amount	=> <<"amountBuy">>,
			rate		=> <<"rate">>,
			time		=> <<"timePlaced">>,
			origin		=> <<"originatingCountry">> },

	%% Incoming JSON can't have a DB ID field, but everything else should be present
	{ [ id ], Trade } = sqerl_record:from_json( JSON, Keys, sqerl_trade, record_info( fields, sqerl_trade ) ),
	Trade.