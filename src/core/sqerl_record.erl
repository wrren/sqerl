-module( sqerl_record ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-export( [ from_json/4 ] ).

%%
%%	Given JSON in binary form, a mapping of
%%	JSON keys to record field atoms and the output of record_info
%%	called on the target record, generate a record whose values are
%%	set from the given JSON data.
%%
%% Parameters:
%%	JSON		- JSON in binary form
%%	KeyMap		- Map of record keys as atoms to JSON keys as binary strings
%%	RecordName	- Record name atom
%%	RecordFields	- List of record fields returned from record_info call
%%
%% Returns:
%%	{ NotFound, Record }	- List of fields that were not found and the output record
%%
-spec from_json( binary(), #{}, atom(), [ atom() ] ) -> { [ atom() ], tuple() }.
from_json( JSON, KeyMap, RecordName, RecordFields ) ->
	from_json( RecordFields, jsx:decode( JSON, [ return_maps ] ), KeyMap, [], RecordName, [] ).

-spec from_json( [ atom() ], #{}, #{}, [ atom() ], atom(), [ term() ] ) -> { [ atom() ], tuple() }.
from_json( [ Key | Fields ], JSON, KeyMap, NotFound, RecordName, Result ) ->
	case maps:find( Key, KeyMap ) of
		{ ok, JSONKey } ->
			case maps:find( JSONKey, JSON ) of
				{ ok, Value } 	-> from_json( Fields, JSON, KeyMap, NotFound, RecordName, [ Value | Result ] );
				_		-> from_json( Fields, JSON, KeyMap, [ Key | NotFound ], RecordName, [ undefined | Result ] )
			end;
		_ -> from_json( Fields, JSON, KeyMap, [ Key | NotFound ], RecordName, [ undefined | Result ] )
	end;

from_json( [], _, _, NotFound, RecordName, Result ) -> { NotFound, list_to_tuple( [ RecordName | lists:reverse( Result ) ] ) }.