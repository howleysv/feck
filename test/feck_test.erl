-module( feck_test ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-include_lib( "eunit/include/eunit.hrl" ).

-define( T( F ), { setup, fun setup/0, F } ).

setup() ->
	feck:configure( [ { blacklist, [ "very", "bad", "words" ] }, { whitelist, [ "words" ] }, { replacement, stars } ] ).

feck_test_() ->
	[
		{ "single word in blacklist", ?T( fun blacklist/1 ) },
		{ "single word in whitelist", ?T( fun whitelist/1 ) },
		{ "presence of non-word characters around the profanity", ?T( fun bordering_non_word_chars/1 ) },
		{ "words in a string", ?T( fun word_string/1 ) },
		{ "bad words with whitespace in a string", ?T( fun blacklist_whitespace/1 ) },
		{ "find expletives in a string", ?T( fun profanity_list/1 ) },
		{ "replace expletives with stars", ?T( fun replace_stars/1 ) },
		{ "replace expletives in binary string", ?T( fun replace_binary/1 ) },
		{ "leaves input untouched when no profanities are found", ?T( fun return_original/1 ) },
		{ "replace expletives vowels with stars", ?T( fun replace_vowels/1 ) },
		{ "replace expletives non-consonants with stars", ?T( fun replace_nonconsonants/1 ) },
		{ "replace expletives with garbled text", ?T( fun replace_garbled/1 ) },
		{ "replace expletives with a custom string", ?T( fun replace_string/1 ) },
		{ "replace expletives with a repeated custom character", ?T( fun replace_repeat/1 ) },
		{ "replace expletives with stars, except the first letter", ?T( fun replace_keep_first/1 ) },
		{ "replace expletives with the given character, except the first letter", ?T( fun replace_keep_first_char/1 ) },
		{ "non-latin characters", ?T( fun sanitize_utf8/1 ) },
		{ "empty blacklist", ?T( fun empty_blacklist/1 ) },
		{ "empty blacklist because of whitelist", ?T( fun cancelled_out_blacklist/1 ) }
	].


blacklist( Config ) ->
	[
		?_assert( feck:profane( "bad", Config ) ),
		?_assert( feck:profane( "BAD", Config ) ),
		?_assert( feck:profane( "BaD", Config ) ),
		?_assertNot( feck:profane( "safe", Config ) )
	].

whitelist( Config ) ->
	[
		?_assertNot( feck:profane( "words", Config ) ),
		?_assertNot( feck:profane( "WORDS", Config ) ),
		?_assertNot( feck:profane( "WoRdS", Config ) )
	].

bordering_non_word_chars( Config ) ->
	Config2 = feck:configure( [ { blacklist, [ "bad-ass" | feck_config:blacklist( Config ) ] } ], Config ),
	[
		?_assert( feck:profane( "#bad!", Config ) ),
		?_assert( feck:profane( "#bad-ass!", Config2 ) )
	].

word_string( Config ) ->
	[
		?_assert( feck:profane( "some bad words", Config ) ),
		?_assertNot( feck:profane( "good words", Config ) )
	].

blacklist_whitespace( Config ) ->
	Config2 = feck:configure( [ { blacklist, [ "bad word" | feck_config:blacklist( Config ) ] } ], Config ),
	[
		?_assert( feck:profane( "a bad word in a string", Config2 ) ),
		?_assert( feck:profane( "a BaD WoRd in a string", Config2 ) )
	].

profanity_list( Config ) ->
	[
		?_assertEqual( [ "very", "bad" ], feck:profanities( "very bad words", Config ) ),
		?_assertEqual( [ "very", "bad", "BAD" ], feck:profanities( "very bad words are BAD", Config ) ),
		?_assertEqual( [], feck:profanities( "none to be found", Config ) )
	].

replace_stars( Config ) ->
	[ ?_assertEqual( "there are ***, **** *** words", feck:sanitize( "there are bad, VERY BAD words", Config ) ) ].

replace_binary( Config ) ->
	[ ?_assertEqual( <<"there are ***, **** *** words">>, feck:sanitize( <<"there are bad, VERY BAD words">>, Config ) ) ].

return_original( Config ) ->
	String = "none to be found",
	[ ?_assertEqual( String, feck:sanitize( String, Config ) ) ].

replace_vowels( Config ) ->
	Config2 = feck:configure( [ { replacement, vowels } ], Config ),
	[ ?_assertEqual( "there are b*d, V*RY B*D words", feck:sanitize( "there are bad, VERY BAD words", Config2 ) ) ].

replace_nonconsonants( Config ) ->
	Config2 = feck:configure( [ { replacement, nonconsonants }, { blacklist, [ "b444d" | feck_config:blacklist( Config ) ] } ], Config ),
	[ ?_assertEqual( "there are b*d, V*RY B***D words", feck:sanitize( "there are bad, VERY B444D words", Config2 ) ) ].

replace_garbled( Config ) ->
	Config2 = feck:configure( [ { replacement, garbled } ], Config ),
	Regex = "there are [$@!#%]{5}, [$@!#%]{5} [$@!#%]{5} words",
	[ ?_assertEqual( match, re:run( feck:sanitize( "there are bad, VERY BAD words", Config2 ), Regex, [ { capture, none } ] ) ) ].

replace_string( Config ) ->
	Config2 = feck:configure( [ { replacement, "💩" } ], Config ),
	[ ?_assertEqual( "there are 💩, 💩 💩 words", feck:sanitize( "there are bad, VERY BAD words", Config2 ) ) ].

replace_repeat( Config ) ->
	Config2 = feck:configure( [ { replacement, { repeat, $- } } ], Config ),
	[ ?_assertEqual( "there are ---, ---- --- words", feck:sanitize( "there are bad, VERY BAD words", Config2 ) ) ].

replace_keep_first( Config ) ->
	Config2 = feck:configure( [ { replacement, keep_first_letter } ], Config ),
	[ ?_assertEqual( "there are b**, V*** B** words", feck:sanitize( "there are bad, VERY BAD words", Config2 ) ) ].

replace_keep_first_char( Config ) ->
	Config2 = feck:configure( [ { replacement, { keep_first_letter, $- } } ], Config ),
	[ ?_assertEqual( "there are b--, V--- B-- words", feck:sanitize( "there are bad, VERY BAD words", Config2 ) ) ].

sanitize_utf8( _ ) ->
	Config = feck:configure( [ { replacement, stars }, { blacklist, [ "тест" ] } ] ),
	String = "This is a тест",
	[
		?_assert( feck:profane( String, Config ) ),
		?_assertEqual( [ "тест" ], feck:profanities( String, Config ) ),
		?_assertEqual( "This is a ****", feck:sanitize( String, Config ) )
	].

empty_blacklist( _ ) ->
	Config = feck:configure( [ { blacklist, [] } ] ),
	[ ?_assertNot( feck:profane( "none to be found", Config ) ) ].

cancelled_out_blacklist( _ ) ->
	Config = feck:configure( [ { blacklist, [ "bad" ] }, { whitelist, [ "some", "bad", "words" ] } ] ),
	[ ?_assertNot( feck:profane( "none to be found", Config ) ) ].
