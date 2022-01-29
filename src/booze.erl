-module(booze).

-export([client/0,
         client/1]).

-export([delete/2,
         delete/3,
         get/2,
         get/3,
         head/2,
         head/3,
         options/2,
         options/3,
         patch/2,
         patch/3,
         post/2,
         post/3,
         put/2,
         put/3]).

-export([request/3,
         request/4,
         send/2,
         send/3]).

-export([handler/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(booze_client, {config :: options()}).
-opaque client() :: #booze_client{}.
-export_type([client/0]).

-type options() :: #{handler  => handler(),
                     base_uri => maybe(booze_uri:uri()),
                     headers  => maybe(booze_message:request_headers()),
                     query    => maybe(binary() | booze_uri:query()),
                     body     => maybe(body()),
                     atom()   => maybe(term())}.
-export_type([options/0]).

-type body() :: {form, booze_uri:query()} |
                {json, booze_json:json_term()} |
                iodata().

-type handler() :: fun((booze_message:request(), options()) ->
                              {ok, booze_message:response()} |
                              {error, term()}).
-export_type([handler/0]).

-type middleware() :: fun((handler()) -> handler()).
-export_type([middleware/0]).

-type maybe(T) :: T | undefined.

%%%===================================================================
%%% API
%%%===================================================================

-spec client() -> client().
client() -> client(#{}).

-spec client(options()) -> client().
client(Options) ->
    #booze_client{config = Options}.

-spec delete(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
delete(URI, Client) -> delete(URI, Client, #{}).

-spec delete(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
delete(URI, Client, Options) ->
    request("DELETE", URI, Client, Options).

-spec get(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
get(URI, Client) -> get(URI, Client, #{}).

-spec get(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
get(URI, Client, Options) ->
    request("GET", URI, Client, Options).

-spec head(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
head(URI, Client) -> head(URI, Client, #{}).

-spec head(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
head(URI, Client, Options) ->
    request("HEAD", URI, Client, Options).

-spec options(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
options(URI, Client) -> options(URI, Client, #{}).

-spec options(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
options(URI, Client, Options) ->
    request("OPTIONS", URI, Client, Options).

-spec patch(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
patch(URI, Client) -> patch(URI, Client, #{}).

-spec patch(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
patch(URI, Client, Options) ->
    request("PATCH", URI, Client, Options).

-spec post(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
post(URI, Client) -> post(URI, Client, #{}).

-spec post(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
post(URI, Client, Options) ->
    request("POST", URI, Client, Options).

-spec put(booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
put(URI, Client) -> put(URI, Client, #{}).

-spec put(booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
put(URI, Client, Options) ->
    request("PUT", URI, Client, Options).

-spec request(iodata(), booze_uri:uri_string(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
request(Method, URI, Client) ->
    request(Method, URI, Client, #{}).

-spec request(iodata(), booze_uri:uri_string(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
request(Method, URI, Client, Options) ->
    Request = booze_message:request(Method, URI),
    send(Request, Client, Options).

-spec send(booze_message:request(), client()) ->
          {ok, booze_message:response()} |
          {error, term()}.
send(Request, Client) ->
    send(Request, Client, #{}).

-spec send(booze_message:request(), client(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
send(Request, #booze_client{config = Defaults}, Options) ->
    Options1 = apply_defaults(Defaults, Options),
    {Request1, Options2} = update_request(Request, Options1),
    case maps:find(handler, Options1) of
        error ->
            erlang:error({?MODULE, missing_handler});
        {ok, Handler} ->
            Handler(Request1, Options2)
    end.

-spec handler([middleware()], handler()) -> handler().
handler([Middleware | Rest], Handler) ->
    Middleware(handler(Rest, Handler));
handler([], Handler) ->
    Handler.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec apply_defaults(options(), options()) -> options().
apply_defaults(Defaults, Options) ->
    Defaults1 =
        case maps:find(headers, Options) of
            {ok, undefined} ->
                Defaults;
            _ ->
                move(headers, '$headers', Defaults)
        end,
    maps:filter(fun(_, V) -> V =/= undefined end,
                maps:merge(Defaults1, Options)).

-spec update_request(booze_message:request(), options()) ->
          {booze_message:request(), options()}.
update_request(Request, Options) ->
    Method = booze_message:method(Request),
    {URI, Options1} = update_uri(booze_message:uri(Request), Options),
    {Body, Options2} = take(body, Options1, booze_message:body(Request)),
    {DefaultHeaders, Options3} = take('$headers', Options2, []),
    {BodyHeaders, ComposedBody} = compose_body(Body),
    {Headers, Options4} = take(headers, Options3, []),
    MergedHeaders = merge_headers([DefaultHeaders,
                                   BodyHeaders,
                                   booze_message:headers(Request),
                                   Headers]),
    Request1 = booze_message:request(Method, URI, MergedHeaders, ComposedBody),
    {Request1, Options4}.

-spec update_uri(booze_uri:uri(), options()) ->
          {booze_uri:uri_string(), options()}.
update_uri(URI, Options) ->
    URI1 = booze_uri:normalize(URI, [return_map]),
    URI2 =
        case maps:find(base_uri, Options) of
            {ok, BaseURI} ->
                booze_uri:resolve(URI1, BaseURI, [return_map]);
            error ->
                URI1
        end,
    {Query, Options1} = take(query, Options, []),
    ComposedQuery = compose_query(Query),
    URI3 =
        case string:is_empty(ComposedQuery) of
            false ->
                URI2#{query => ComposedQuery};
            true ->
                URI2
        end,
    URI4 = booze_uri:recompose(URI3),
    {URI4, Options1}.

-spec compose_query(binary() | booze_uri:query()) -> booze_uri:uri_string().
compose_query(Query) when is_list(Query) ->
    booze_uri:compose_query(Query);
compose_query(Query) when is_binary(Query) ->
    Query.

-spec compose_body(body()) -> {booze_message:request_headers(), iodata()}.
compose_body({form, Query}) ->
    {[{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
     booze_uri:compose_query(Query)};
compose_body({json, JSONTerm}) ->
    {[{<<"content-type">>, <<"application/json">>}],
     booze_json:encode(JSONTerm)};
compose_body(Body) ->
    {[], Body}.

-spec merge_headers([Headers, ...]) -> Headers when
      Headers :: booze_message_headers:headers(_).
merge_headers([Headers1, Headers2 | Rest]) ->
    merge_headers([merge_headers(Headers1, Headers2) | Rest]);
merge_headers([Headers]) ->
    Headers.

-spec merge_headers(Headers, Headers) -> Headers when
      Headers :: booze_message_headers:headers(_).
merge_headers(Headers1, Headers2) ->
    Fun = fun({Name, _}, Headers) ->
                  booze_message_headers:remove(Name, Headers)
          end,
    lists:foldl(Fun, Headers1, Headers2) ++ Headers2.

-spec move(term(), term(), map()) -> map().
move(FromKey, ToKey, Map) ->
    case maps:take(FromKey, Map) of
        {Value, Map1} ->
            Map1#{ToKey => Value};
        error ->
            Map
    end.

-spec take(term(), map(), term()) -> {term(), map()}.
take(Key, Map, Default) ->
    case maps:take(Key, Map) of
        {_, _} = Tuple ->
            Tuple;
        error ->
            {Default, Map}
    end.
