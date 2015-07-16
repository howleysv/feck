%% Platform support for Unicode Character Properties
-ifdef( ucp_not_supported ).
-define( REGEX_UNICODE_OPTIONS,	[ unicode ] ).
-else.
-define( REGEX_UNICODE_OPTIONS,	[ unicode, ucp ] ).
-endif.
