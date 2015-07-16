-module( feck_replace ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-include( "feck.hrl" ).

-export( [ replace/2 ] ).

-spec replace( unicode:chardata(), feck:replacement() ) -> unicode:chardata().
replace( _String, garbled ) ->
	list_shuffle( "$@!#%" );

replace( String, stars ) ->
	replace( String, { repeat, $* } );

replace( String, vowels ) ->
	replace_regex( String, <<"[aeiou]">> );

replace( String, nonconsonants ) ->
	replace_regex( String, <<"[^bcdfghjklmnpqrstvwxyz]">> );

replace( String, { repeat, Char } ) ->
	lists:duplicate( char_length( String ), Char );

replace( String, keep_first_letter ) ->
	replace( String, { keep_first_letter, $* } );

replace( String, { keep_first_letter, Char } ) ->
	[ First | Rest ] = unicode:characters_to_list( String ),
	[ First | replace( Rest, { repeat, Char } ) ];

replace( _String, Replacement ) ->
	Replacement.

-spec replace_regex( unicode:chardata(), unicode:chardata() ) -> unicode:chardata().
replace_regex( String, Regex ) ->
	re:replace( String, Regex, <<"*">>, ?REGEX_UNICODE_OPTIONS ++ [ caseless, global ] ).

-spec char_length( unicode:chardata() ) -> non_neg_integer().
char_length( String ) ->
	length( unicode:characters_to_list( String ) ).

-spec list_shuffle( [ term() ] ) -> [ term() ].
list_shuffle( List ) ->
	element( 2, lists:unzip( lists:sort( [ { random:uniform(), Element } || Element <- List ] ) ) ).
