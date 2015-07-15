# feck [![TravisCI](https://travis-ci.org/howleysv/feck.svg?branch=master)](https://travis-ci.org/howleysv/feck)

An obscenity detection and filtering library for Erlang, ported from [Expletive](https://github.com/xavier/expletive).

## Installation

Add **feck** as a dependency to your project rebar.config

## Usage

All feck functions expect a configuration to be passed:

```erlang

Config = feck:configure( [ { blacklist, [ "very", "bad", "words" ] } ] )

feck:profane( "this is bad!", Config )
# => true
feck:profane( "perfectly safe", Config )
# => false

feck:profanities( "this is bad, so BAD!", Config )
# => [ "bad", "BAD" ]
```

### Sanitization

The library offers a fairly wide variety of profanity replacement strategies which can be defined at configuration time.

```erlang
feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, garbled } ], Config ) )
# => "This is $#!@%, so %$@!#!"

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, stars } ], Config ) )
# => "This is ***, so ***!"

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, vowels } ], Config ) )
# => "This is b*d, so B*D!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, ":poop:" } ], Config ) )
# => "This is :poop:, so :poop:!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, { repeat, $- } } ], Config ) )
# => "This is ---, so ---!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, keep_first_letter } ], Config ) )
# => "This is b**, so B**!

feck:sanitize( "this is bad, so BAD!", feck:configure( [ { replacement, { keep_first_letter, $- } } ], Config ) )
# => "This is b--, so B--!

```

### Whitelisting

If you wish to allow some words present in the blacklist, you can add exceptions to a whitelist at configuration time:

```erlang
Config = feck:configure( [ { blacklist: [ "very", "bad", "words" ] }, { whitelist, [ "words" ] } ] )

feck:profane( "words", Config )
# => false

```

### Built-in blacklists

The library comes with a couple of word lists ready to use:

```erlang

Config = feck:configure( [ { blacklist, feck_blacklist:english() } ] )

feck:profane( "this is batshit crazy!", Config )
# => true

Config = feck:configure( [ { blacklist, feck_blacklist:international() } ] )

feck:profanities( "ceci n'est pas une pipe", Config )
# => [ "pipe" ]

```

## Known Limitations

### I18n concerns

A couple of replacement strategies (`vowels` and `nonconsonants`) are currently limited to the english language.
