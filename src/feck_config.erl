-module( feck_config ).
-author( "Shane Howley <howleysv@gmail.com>" ).
-include( "feck.hrl" ).

-export( [ new/1, update/2, blacklist/1, whitelist/1, replacement/1, match/1, regex/1 ] ).

-export_type( [ config/0 ] ).

-type regex() 		:: tuple().

-define( STATE, 	?MODULE ).

-record( ?STATE, {	blacklist = []		:: [ unicode:chardata() ],
			whitelist = []		:: [ unicode:chardata() ],
			replacement = stars 	:: feck:replacement(),
			match = word_boundaries	:: word_boundaries | any,
			compiled_regex 		:: regex() } ).

-opaque config() 	:: #?STATE{}.

-spec new( [ feck:option() ] ) -> config().
new( Options ) ->
	update( Options, #?STATE{} ).

-spec update( [ feck:option() ], config() ) -> config().
update( Options, Config ) ->
	update( lists:foldl( fun( O, C ) -> update_config( O, C ) end, Config, Options ) ).

-spec blacklist( config() ) -> [ unicode:chardata() ].
blacklist( #?STATE{ blacklist = Blacklist } ) ->
	Blacklist.

-spec whitelist( config() ) -> [ unicode:chardata() ].
whitelist( #?STATE{ whitelist = Whitelist } ) ->
	Whitelist.

-spec replacement( config() ) -> feck:replacement().
replacement( #?STATE{ replacement = Replacement } ) ->
	Replacement.

-spec match( config() ) -> word_boundaries | any.
match( #?STATE{ match = Match } ) ->
	Match.

-spec regex( config() ) -> regex().
regex( #?STATE{ compiled_regex = Regex } ) ->
	Regex.

-spec update_config( feck:option(), config() ) -> config().
update_config( { blacklist, Blacklist }, Config ) ->		Config#?STATE{ blacklist = resolve_word_list( Blacklist ) };
update_config( { whitelist, Whitelist }, Config ) ->		Config#?STATE{ whitelist = resolve_word_list( Whitelist ) };
update_config( { replacement, Replacement }, Config ) ->	Config#?STATE{ replacement = Replacement };
update_config( { match, Match }, Config ) ->			Config#?STATE{ match = Match }.

-spec update( config() ) -> config().
update( #?STATE{ blacklist = Blacklist, whitelist = Whitelist, match = Match } = Config ) ->
	Filtered = lists:usort( Blacklist ) -- lists:usort( Whitelist ),
	Regex = compile_regex( Filtered, Match ),
	Config#?STATE{ compiled_regex = Regex }.

-spec compile_regex( [ unicode:chardata() ], word_boundaries | any ) -> regex().
compile_regex( WordList, Match ) ->
	Escaped = [ escape( W ) || W <- WordList ],
	Regex = build_regex( Escaped, Match ),
	{ ok, Compiled } = re:compile( Regex, ?REGEX_UNICODE_OPTIONS ++ [ caseless ] ),
	Compiled.

-spec escape( unicode:chardata() ) -> unicode:chardata().
escape( Word ) ->
	re:replace( Word, <<"[.^$*+?()[{\\\|\s#]">>, <<"\\\\&">>, ?REGEX_UNICODE_OPTIONS ++ [ global ] ).

-spec build_regex( [ unicode:chardata() ], word_boundaries | any ) -> unicode:chardata().
build_regex( [], _ ) ->				<<"$.">>;
build_regex( Words, word_boundaries ) ->	[ <<"\\b">>, build_regex_capture( Words, [] ), <<"\\b">> ];
build_regex( Words, any ) ->			build_regex_capture( Words, [] ).

-spec build_regex_capture( [ unicode:chardata() ], [ unicode:chardata(),... ] ) -> unicode:chardata().
build_regex_capture( [], Regex ) -> 			lists:reverse( [ <<")">> | Regex ] );
build_regex_capture( [ Word | Rest ], [] ) ->		build_regex_capture( Rest, [ Word, <<"(">> ] );
build_regex_capture( [ Word | Rest ], Regex ) ->	build_regex_capture( Rest, [ Word, <<"|">> | Regex ] ).

-spec resolve_word_list( feck:word_list() ) -> [ unicode:chardata() ].
resolve_word_list( List ) when is_list( List ) ->	List;
resolve_word_list( FunName ) when is_atom( FunName ) ->	feck_blacklist:FunName();
resolve_word_list( { M, F, A } ) ->			apply( M, F, A ).
