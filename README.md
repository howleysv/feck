# feck [![TravisCI](https://travis-ci.org/howleysv/feck.svg?branch=master)](https://travis-ci.org/howleysv/feck)

An obscenity detection and filtering library for Erlang, ported from [Expletive](https://github.com/xavier/expletive).

## Installation

Add **feck** to your `.app` or `.app.src` file:

```erlang
{ applications, [ feck ] }
```

## Usage

All **feck** functions expect a configuration to be passed:

```erlang
Config = feck:configure( [ { blacklist, [ "very", "bad", "words" ] } ] )

feck:profane( "this is bad!", Config )
%% => true
feck:profane( "perfectly safe", Config )
%% => false

feck:profanities( "this is bad, so BAD!", Config )
%% => [ "bad", "BAD" ]
```

### Sanitization

The library offers a fairly wide variety of profanity replacement strategies which can be defined at configuration time.

```erlang
feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, garbled } ], Config ) )
%% => "This is $#!@%, so %$@!#!"

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, stars } ], Config ) )
%% => "This is ***, so ***!"

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, vowels } ], Config ) )
%% => "This is b*d, so B*D!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, ":poop:" } ], Config ) )
%% => "This is :poop:, so :poop:!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, { repeat, $- } } ], Config ) )
%% => "This is ---, so ---!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, keep_first_letter } ], Config ) )
%% => "This is b**, so B**!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, { keep_first_letter, $- } } ], Config ) )
%% => "This is b--, so B--!
```

### Whitelisting

If you wish to allow some words present in the blacklist, you can add exceptions to a whitelist at configuration time:

```erlang
Config = feck:configure( [ { blacklist, [ "very", "bad", "words" ] }, { whitelist, [ "words" ] } ] )

feck:profane( "words", Config )
%% => false
```

### Built-in blacklists

The library comes with a couple of word lists ready to use that are compiled from `priv/dictionary/<name>.txt`:

```erlang
Config = feck:configure( [ { blacklist, english } ] )

feck:profane( "this is batshit crazy!", Config )
%% => true

Config = feck:configure( [ { blacklist, international } ] )

feck:profanities( "ceci n'est pas une pipe", Config )
%% => [ "pipe" ]
```

### MFA word lists

The black and whitelist can also be specified as a `{ Module, Function, Args }` tuple that returns a word list.

```erlang
Config = feck:configure( [ { blacklist, { string, tokens, [ "very bad words", " " ] } } ] )

feck:profanities( "this is bad, so BAD!", Config )
%% => [ "bad", "BAD" ].
```

### Matching strategy

By default, only exact whole word matches are detected, but matches as substrings of other words can also be found with the `{ match, any }` option:

```erlang
Config = feck:configure( [ { match, word_boundaries }, { blacklist, [ "very", "bad", "words" ] } ] )
AnyConfig = feck:configure( [ { match, any } ], Config )

feck:profanities( "this is bad!", Config )
%% => [ "bad" ]

feck:profanities( "this is superbadly!", Config )
%% => []

feck:profanities( "this is superbadly!", AnyConfig )
%% => [ "bad" ]
```

### Default config

The config parameter can be omitted from `profane/2`, `profanities/2` & `sanitize/2` in order to use the application-level default settings. These settings are read once, the first time the default config is used, after which the compiled config is cached as an environmental variable. The default options can be overridden via your sys.config:

```erlang
{   feck,
	{ options,
	[
		{ blacklist, english },
		{ whitelist, [] },
		{ replacement, stars },
		{ match, word_boundaries }
	] }
}
```

```erlang
feck:sanitize( "this is batshit crazy!" )
%% => "this is ******* crazy!"
```

The default config can also be overwritten at runtime:
```erlang
NewConfig = feck:configure( [ { replacement, garbled } ], feck:default_config() )
feck:set_default( NewConfig )

feck:sanitize( "this is batshit crazy!" )
%% => "this is $#!@% crazy!"
```

## Known Limitations

### Unicode support in Erlang R16 and below

Erlang 17 introduced the `ucp` option to the `re` module:
>ucp
>Specifies that Unicode Character Properties should be used when resolving \B, \b, \D, \d, \S, \s, \W and \w. Without this flag, only ISO-Latin-1 properties are used. Using Unicode properties hurts performance, but is semantically correct when working with Unicode characters beyond the ISO-Latin-1 range.

Without this option (R16 and below), when using `{ match, word_boundaries }` finding words will fail for words containing characters with codepoints outside the Latin-1 range.

The length of words being replaced is calculated by `length( unicode:characters_to_list( String ) )` which is inconsistent in R16 and below when dealing with characters with codepoints outside the Latin-1 range.

### I18n concerns

A couple of replacement strategies (`vowels` and `nonconsonants`) are currently limited to the english language.
