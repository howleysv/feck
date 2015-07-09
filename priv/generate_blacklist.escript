#!/usr/bin/env escript

-define( SCRIPT_PATH,	filename:dirname( escript:script_name() ) ).
-define( DICT_PATH, 	filename:join( ?SCRIPT_PATH, "dictionary" ) ).
-define( DICT_EXT,	".txt" ).
-define( TEMPLATE_EXT,	".template" ).
-define( TARGET_FILE, 	"feck_blacklist.erl" ).
-define( DEST_PATH, 	filename:join( ?SCRIPT_PATH, "../src" ) ).

main( [] ) ->
	{ ok, Files } = file:list_dir( ?DICT_PATH ),
	TxtFiles = lists:filter( fun( F ) -> filename:extension( F ) =:= ?DICT_EXT end, Files ),
	Dictionaries = [ { list_to_atom( filename:basename( F, ?DICT_EXT ) ), read_dict( filename:join( ?DICT_PATH, F ) ) } || F <- TxtFiles ],
	write_output( ?DEST_PATH, ?TARGET_FILE, Dictionaries ).

read_dict( Path ) ->
	{ ok, OpenFile } = file:open( Path, [ read, binary ] ),
	Words = read_lines( OpenFile, [] ),
	file:close( OpenFile ),
	Words.

read_lines( OpenFile, Lines ) ->
	case io:get_line( OpenFile, "" ) of
		eof ->	lists:usort( Lines ) -- [<<>>];
		Line ->	read_lines( OpenFile, [ re:replace( Line, <<"\"?(.*?)\"?\n">>, <<"\\1">>, [ unicode, { return, binary } ] ) | Lines ] )
	end.

write_output( DestPath, FileName, Dictionaries ) ->
	Dest = filename:join( DestPath, FileName ),
	{ ok, _ } = file:copy( filename:join( ?SCRIPT_PATH, FileName ++ ?TEMPLATE_EXT ), Dest ),
	{ ok, OpenFile } = file:open( Dest, [ append ] ),
	ok = file:write( OpenFile, generate_export( element( 1, lists:unzip( Dictionaries ) ) ) ),
	[ ok = file:write( OpenFile, generate_function( D ) ) || D <- Dictionaries ],
	file:close( OpenFile ).

generate_export( [ First | Atoms ] ) ->
	[
		io_lib:format( "-export( [ ~w/0", [ First ] ),
		[ io_lib:format( ", ~w/0", [ A ] ) || A <- Atoms ],
		" ] ).\n\n"
	].

generate_function( { Name, Words } ) ->
	[
		io_lib:format( "~w() ->~n\t[~n\t\t", [ Name ] ),
		string:join( [ io_lib:format( "<<\"~ts\"/utf8>>", [ W ] ) || W <- Words ], ",\n\t\t" ),
		"\n\t].\n\n"
	].
