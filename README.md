# Erlang Key/Value Storage Backend Behaviors

I've been thinking about what might be a proper abstraction of storage backend functionality for [Basho's][basho] Riak products.

Implicit in this is the ability to push some higher-level functionality into the backend without tying products to specific implementations.

There's also a desire to be able to implement the backends themselves using dirty NIFs in a manner that allows the Erlang VM to handle most (or preferably all) scheduling.

## Status - Wildly Experimental

This is all blue-sky dreaming about what a Key/Value backend abstraction should look like.

There is no reason at all to believe that this will end up in any product ... or that it won't ;)

## Build

This builds with the current release of [rebar3][], which will be downloaded to the project directory.

Other than running the `check` and `docs` make targets, there's not much to do with it.

## License

Refer to the [license][] for details.


 [basho]:   http://www.basho.com/
 [rebar3]:  http://www.rebar3.org/
 [license]: LICENSE
