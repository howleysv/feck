-module( feck ).
-author( "Shane Howley <howleysv@gmail.com>" ).

-export( [] ).

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
