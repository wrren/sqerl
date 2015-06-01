-module( sqerl_trade ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-export( [ from_json/1, to_json/1, subscribe/2, unsubscribe/2, notify/1, user/1 ] ).

-include( "trade/sqerl_trade.hrl" ).

%%
%%	Convert a binary string containing JSON describing a trade to a
%%	sqerl_trade record.
%%
%% Parameters:
%%	JSON	- Binary string containing JSON trade data
%%
%% Returns:
%%	sqerl_trade record
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


%%
%%	Convenience function that extracts and returns the trader ID from a trade record so that the
%%	calling module doesn't need visibility on the trade record's internals.
%%
user( #sqerl_trade{ trader = Trader } ) -> Trader.

%%
%%	Convert a sqerl_trade record to a binary string containing the trade data
%%	encoded using JSON
%%
%% Parameters:
%%	Trade	- sqerl_trade record
%%
%% Returns:
%%	sqerl_trade record encoded to JSON in binary string form
%%
-spec to_json( #sqerl_trade{} ) -> binary().
to_json( Trade = #sqerl_trade{} ) ->
	sqerl_record:to_json( Trade, #{}, record_info( fields, sqerl_trade ) ).

%%
%%	Notify subscribed handlers that new trade data has been received.
%%
notify( Trade ) ->
	gen_event:notify( { global, sqerl_trade_event }, { new_trade, Trade } ).

%%
%%	Convenience event subscription function. Adds the provided module as an event handler to the
%%	trade gen_event manager, passing the provided arguments in the add_sup_handler call.
%%
subscribe( Module, Args ) ->
	gen_event:add_sup_handler( { global, sqerl_trade_event }, Module, Args ).

%%
%%	Convenience event unsubscribe function. Deletes the provided event handler from the
%%	trade gen_event manager handler list.
%%
unsubscribe( Module, Args ) ->
	gen_event:delete_handler( { global, sqerl_trade_event }, Module, Args ).