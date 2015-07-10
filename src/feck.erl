-module( feck ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-export( [ sanitize/2 ] ).

-export_type( [ replacement/0, option/0, config/0 ] ).

-type replacement()	:: garbled
			|  stars
			|  vowels
			|  nonconsonants
			|  string
			|  { repeat, char() }
			|  keep_first_letter
			|  { keep_first_letter, char() }.

-type option()		:: { blacklist, [ unicode:chardata() ] }
			|  { whitelist, [ unicode:chardata() ] }
			|  { replacement, feck:replacement() }.

-opaque config()	:: feck_config:config().

-spec sanitize	( unicode:charlist(), feck_config:config() ) -> unicode:charlist();
		( unicode:unicode_binary(), feck_config:config() ) -> unicode:unicode_binary().
sanitize( String, Config ) ->
	Replaced = do_replace( re:split( String, feck_config:regex( Config ) ), [], feck_config:replacement( Config ) ),
	case is_binary( String ) of
		true ->		unicode:characters_to_binary( Replaced );
		false ->	unicode:characters_to_list( Replaced )
	end.

-spec do_replace( [ unicode:chardata(),... ], [ unicode:chardata() ], replacement() ) -> unicode:chardata().
do_replace( [ NoReplace ], Processed, _ ) ->
	lists:reverse( [ NoReplace | Processed ] );

do_replace( [ NoReplace, Replace | Rest ], Processed, ReplacementType ) ->
	do_replace( Rest, [ feck_replace:replace( Replace, ReplacementType ), NoReplace | Processed ], ReplacementType ).

