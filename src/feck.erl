-module( feck ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-export( [ 	configure/1, configure/2, set_default/1, default_config/0,
		profane/1, profane/2, profanities/1, profanities/2, sanitize/1, sanitize/2 ] ).

-export_type( [ replacement/0, option/0, config/0, word_list/0 ] ).

-define( APP, ?MODULE ).

-type replacement()	:: garbled
			|  stars
			|  vowels
			|  nonconsonants
			|  unicode:chardata()
			|  { repeat, char() }
			|  keep_first_letter
			|  { keep_first_letter, char() }.

-type word_list()	:: [ unicode:chardata() ]
			|  atom()
			|  { M :: module(), F :: atom(), A ::[ term() ] }.

-type option()		:: { blacklist, word_list() }
			|  { whitelist, word_list() }
			|  { replacement, feck:replacement() }.

-type config()		:: feck_config:config().

-spec configure( [ option() ] ) -> config().
configure( Options ) ->
	feck_config:new( Options ).

-spec configure( [ option() ], config() ) -> config().
configure( Options, Config ) ->
	feck_config:update( Options, Config ).

-spec set_default( config() ) -> ok.
set_default( Config ) ->
	application:set_env( ?APP, default_config, Config ).

-spec profane( unicode:chardata() ) -> boolean().
profane( String ) ->
	profane( String, default_config() ).

-spec profane( unicode:chardata(), config() ) -> boolean().
profane( String, Config ) ->
	match =:= re:run( String, feck_config:regex( Config ), [ { capture, none } ] ).

-spec profanities	( unicode:charlist() ) -> [ unicode:charlist() ];
			( unicode:unicode_binary() ) -> [ unicode:unicode_binary() ].
profanities( String ) ->
	profanities( String, default_config() ).

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

-spec sanitize	( unicode:charlist() ) -> unicode:charlist();
		( unicode:unicode_binary() ) -> unicode:unicode_binary().
sanitize( String ) ->
	sanitize( String, default_config() ).

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

-spec default_config() -> config().
default_config() ->
	case application:get_env( ?APP, default_config ) of
		{ ok, Config } ->
			Config;
		undefined ->
			Options = case application:get_env( ?APP, options ) of
				{ ok, O } ->	O;
				undefined ->	[]
			end,
			Config = configure( Options ),
			set_default( Config ),
			Config
	end.
