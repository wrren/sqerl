-module( sqerl_record ).
-author( "Warren Kenny <warren.kenny@gmail.com>" ).

-export( [ from_json/4, to_json/3 ] ).

%%
%%	Given JSON in binary form, a mapping of
%%	record field atoms to JSON key strings, with optional validation specifications, and the output of record_info
%%	called on the target record, generate a record whose values are
%%	set from the given JSON data.
%%
%% Parameters:
%%	JSON		- JSON in binary form
%%	KeyMap		- Map of record keys as atoms to JSON keys as binary strings with validation parameters
%%	RecordName	- Record name atom
%%	RecordFields	- List of record fields returned from record_info call
%%
%% Returns:
%%	{ Errors, Record }	- List of fields that failed to validate and the output record
%%
-spec from_json( binary(), #{}, atom(), [ atom() ] ) -> { [ atom() ], tuple() }.
from_json( JSON, KeyMap, RecordName, RecordFields ) ->
	from_json( RecordFields, jsx:decode( JSON, [ return_maps ] ), KeyMap, [], RecordName, [] ).

-spec from_json( [ atom() ], #{}, #{}, [ atom() ], atom(), [ term() ] ) -> { [ atom() ], tuple() }.
from_json( [ Key | Fields ], JSON, KeyMap, Errors, RecordName, Result ) ->
	case maps:find( Key, KeyMap ) of

		%% If the key includes a validation spec, apply it before continuing
		{ ok, { JSONKey, ValidationSpec } } ->
			case maps:find( JSONKey, JSON ) of
				{ ok, Value } 	-> 
					case sqerl_validator:validate( Value, ValidationSpec ) of 
						{ ok, Value } 		-> from_json( Fields, JSON, KeyMap, Errors, RecordName, [ Value | Result ] );
						{ error, Value, E }	-> from_json( Fields, JSON, KeyMap, [ { Key, E } | Errors ], RecordName, [ Value | Result ] )
					end;
				_		-> 
					case sqerl_validator:validate( undefined, ValidationSpec ) of 
						{ ok, Value } 		-> from_json( Fields, JSON, KeyMap, Errors, RecordName, [ Value | Result ] );
						{ error, Value, E }	-> from_json( Fields, JSON, KeyMap, [ { Key, E } | Errors ], RecordName, [ Value | Result ] )
					end
			end;

		%% If there's no validation spec, continue as normal
		{ ok, JSONKey } ->
			case maps:find( JSONKey, JSON ) of
				{ ok, Value } 	-> from_json( Fields, JSON, KeyMap, Errors, RecordName, [ Value | Result ] );
				_		-> from_json( Fields, JSON, KeyMap, [ Key | Errors ], RecordName, [ undefined | Result ] )
			end;

		%% If there's no key, attempt to retrieve a key with the same name as the record field name
		_ -> 
			case maps:find( atom_to_binary( Key, utf8 ), JSON ) of
				{ ok, Value } 	-> from_json( Fields, JSON, KeyMap, Errors, RecordName, [ Value | Result ] );
				_		-> from_json( Fields, JSON, KeyMap, [ Key | Errors ], RecordName, [ undefined | Result ] )
			end
	end;

from_json( [], _, _, Errors, RecordName, Result ) -> { Errors, list_to_tuple( [ RecordName | lists:reverse( Result ) ] ) }.

%%
%%	Given a record, an optional mapping between record field names and JSON key name, and
%%	the list of contained fields, generate a binary string containing the record's data
%%	encoded to JSON.
%%
%% Parameters:
%%	Record		- Record to be encoded
%%	KeyMap		- Mapping of record field atoms to binary string JSON keys
%%	RecordFields	- List of fields in the record, obtained from record_info call
%%
%% Returns:
%%	Record converted to JSON binary string
%%
-spec to_json( tuple(), #{}, [ atom() ] ) -> binary().
to_json( Record, KeyMap, RecordFields )->
	ValueList = tuple_to_list( Record ),
	[ _RecordName | Values ] = ValueList,
	to_json( Values, KeyMap, RecordFields, #{} ).

-spec to_json( [ term() ], #{}, [ atom() ], #{} ) -> binary().
to_json( [ Value | VT ], KeyMap, [ FieldName | FT ], RecordMap ) ->
	case maps:find( FieldName, KeyMap ) of
		{ ok, JSONKey }		-> to_json( VT, KeyMap, FT, maps:put( JSONKey, Value, RecordMap ) );
		_			-> to_json( VT, KeyMap, FT, maps:put( atom_to_binary( FieldName, utf8 ), Value, RecordMap ) )
	end;

to_json( [], _, _, RecordMap ) -> jsx:encode( RecordMap ). 

