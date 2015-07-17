-module( feck ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-export( [ configure/1, configure/2, profane/2, profanities/2, sanitize/2 ] ).

-export_type( [ replacement/0, option/0, config/0 ] ).

-type replacement()	:: garbled
			|  stars
			|  vowels
			|  nonconsonants
			|  unicode:chardata()
			|  { repeat, char() }
			|  keep_first_letter
			|  { keep_first_letter, char() }.

-type option()		:: { blacklist, [ unicode:chardata() ] }
			|  { whitelist, [ unicode:chardata() ] }
			|  { replacement, feck:replacement() }.

-type config()		:: feck_config:config().

-spec configure( [ option() ] ) -> config().
configure( Options ) ->
	feck_config:new( Options ).

-spec configure( [ option() ], config() ) -> config().
configure( Options, Config ) ->
	feck_config:update( Options, Config ).

-spec profane( unicode:chardata(), config() ) -> boolean().
profane( String, Config ) ->
	match =:= re:run( String, feck_config:regex( Config ), [ { capture, none } ] ).

-spec profanities	( unicode:charlist(), config() ) -> [ unicode:charlist() ];
			( unicode:unicode_binary(), config() ) -> [ unicode:unicode_binary() ].
profanities( String, Config ) ->
	Matches = case re:run( String, feck_config:regex( Config ), [ global, { capture, first, binary } ] ) of
		{ match, List } ->	List;
		_ ->			[]
	end,
	case is_binary( String ) of
		true ->		[ unicode:characters_to_binary( M ) || M <- Matches ];
		false ->	[ unicode:characters_to_list( M ) || M <- Matches ]
	end.

-spec sanitize	( unicode:charlist(), config() ) -> unicode:charlist();
		( unicode:unicode_binary(), config() ) -> unicode:unicode_binary().
sanitize( String, Config ) ->
	Binary = unicode:characters_to_binary( String ), %% Pre-convert to binary to workaround bug in re:split with precompiled regex
	Replaced = do_replace( re:split( Binary, feck_config:regex( Config ) ), [], feck_config:replacement( Config ) ),
	case is_binary( String ) of
		true ->		unicode:characters_to_binary( Replaced );
		false ->	unicode:characters_to_list( Replaced )
	end.

-spec do_replace( [ unicode:chardata(),... ], [ unicode:chardata() ], replacement() ) -> unicode:chardata().
do_replace( [ NoReplace ], Processed, _ ) ->
	lists:reverse( [ NoReplace | Processed ] );

do_replace( [ NoReplace, Replace | Rest ], Processed, ReplacementType ) ->
	do_replace( Rest, [ feck_replace:replace( Replace, ReplacementType ), NoReplace | Processed ], ReplacementType ).
