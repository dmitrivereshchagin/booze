-module(booze_uri).

-export([normalize/1,
         normalize/2,
         recompose/1,
         resolve/2,
         resolve/3,
         compose_query/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type uri() :: uri_string() | uri_map().
-export_type([uri/0]).

-type uri_string() :: uri_string:uri_string().
-export_type([uri_string/0]).

-type uri_map() :: uri_string:uri_map().
-export_type([uri_map/0]).

-type query() :: [{unicode:chardata(), unicode:chardata() | true}].
-export_type([query/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec normalize(uri()) -> uri_string().
normalize(URI) ->
    maybe_error(uri_string:normalize(URI), [URI]).

-spec normalize(uri(), [return_map]) -> uri().
normalize(URI, Options) ->
    maybe_error(uri_string:normalize(URI, Options), [URI, Options]).

-spec recompose(uri_map()) -> uri_string().
recompose(URIMap) ->
    maybe_error(uri_string:recompose(URIMap), [URIMap]).

-spec resolve(Ref :: uri(), Base :: uri()) -> uri_string().
resolve(RefURI, BaseURI) ->
    maybe_error(uri_string:resolve(RefURI, BaseURI), [RefURI, BaseURI]).

-spec resolve(Ref :: uri(), Base :: uri(), [return_map]) -> uri().
resolve(RefURI, BaseURI, Options) ->
    maybe_error(uri_string:resolve(RefURI, BaseURI, Options),
                [RefURI, BaseURI, Options]).

-spec compose_query(query()) -> uri_string().
compose_query(Query) ->
    maybe_error(uri_string:compose_query(Query), [Query]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-compile({inline, maybe_error/2}).
-spec maybe_error(Error | Result, Args) -> no_return() | Result when
      Error :: uri_string:error(),
      Args  :: [term()] | none.
maybe_error({error, Type, Extra}, Args) ->
    erlang:error({Type, Extra}, Args);
maybe_error(Result, _Args) ->
    Result.
