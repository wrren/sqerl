%%
%%	Represents the data for a single trade
%%
-record( sqerl_trade, {		id 		:: non_neg_integer(),		%% ID generate when inserted into the database
				trader 		:: non_neg_integer(),		%% Trader ID submitted during POST
				origin 		:: binary(),			%% Country in which the trade took place
				from_currency	:: binary(),			%% Trade source currency
				to_currency 	:: binary(),			%% Trade destination currency
				from_amount 	:: binary(),			%% Amount being traded in source currency
				to_amount	:: binary(),			%% Amount received in destination currency
				rate 		:: binary(),			%% Conversion Rate
				time		:: integer()			%% Timestamp at which the trade took place
} ).