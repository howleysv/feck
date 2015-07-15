-module( feck_config ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-export( [ new/1, update/2, blacklist/1, whitelist/1, replacement/1, regex/1 ] ).

-export_type( [ config/0 ] ).

-type regex() 		:: tuple().

-define( STATE, 	?MODULE ).

-record( ?STATE, {	blacklist = []		:: [ unicode:chardata() ],
			whitelist = []		:: [ unicode:chardata() ],
			replacement = stars 	:: feck:replacement(),
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

-spec regex( config() ) -> regex().
regex( #?STATE{ compiled_regex = Regex } ) ->
	Regex.

-spec update_config( feck:option(), config() ) -> config().
update_config( { blacklist, Blacklist }, Config ) ->		Config#?STATE{ blacklist = Blacklist };
update_config( { whitelist, Whitelist }, Config ) ->		Config#?STATE{ whitelist = Whitelist };
update_config( { replacement, Replacement }, Config ) ->	Config#?STATE{ replacement = Replacement }.

-spec update( config() ) -> config().
update( #?STATE{ blacklist = Blacklist, whitelist = Whitelist } = Config ) ->
	Filtered = lists:usort( Blacklist ) -- lists:usort( Whitelist ),
	Regex = compile_regex( Filtered ),
	Config#?STATE{ compiled_regex = Regex }.

-spec compile_regex( [ unicode:chardata() ] ) -> regex().
compile_regex( WordList ) ->
	Escaped = [ escape( W ) || W <- WordList ],
	Regex = build_regex( Escaped ),
	{ ok, Compiled } = re:compile( Regex, [ unicode, caseless ] ),
	Compiled.

-spec escape( unicode:chardata() ) -> unicode:chardata().
escape( Word ) ->
	re:replace( Word, <<"[.^$*+?()[{\\\|\s#]">>, <<"\\\\&">>, [ unicode, global ] ).

-spec build_regex( [ unicode:chardata() ] ) -> unicode:chardata().
build_regex( [] ) ->				<<"$.">>;
build_regex( [ First | Words ] ) -> 		build_regex( Words, [ First, <<"\\b(">> ] ).

-spec build_regex( [ unicode:chardata() ], [ unicode:chardata(),... ] ) -> unicode:chardata().
build_regex( [], Regex ) -> 			lists:reverse( [ <<")\\b">> | Regex ] );
build_regex( [ Word | Rest ], Regex ) ->	build_regex( Rest, [ Word, <<"|">> | Regex ] ).
