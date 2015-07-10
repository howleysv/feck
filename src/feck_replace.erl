-module( feck_replace ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-export( [ replace/2 ] ).

-spec replace( unicode:chardata(), stars ) -> unicode:chardata().
replace( String, stars ) ->
	lists:duplicate( char_length( String ), $* ).

-spec char_length( unicode:chardata() ) -> non_neg_integer().
char_length( String ) ->
	length( unicode:characters_to_list( String ) ).

