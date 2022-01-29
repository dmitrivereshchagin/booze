-module(booze_httpc).

-export([handler/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: #{httpc_profile   => atom() | pid(),
                     timeout         => timeout(),
                     connect_timeout => timeout()}.
-export_type([options/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handler() -> booze:handler().
handler() -> fun send/2.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send(booze_message:request(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
send(Request, Options) ->
    case httpc:request(method(Request), request(Request),
                       http_options(Options), [{body_format, binary}],
                       profile(Options)) of
        {ok, {StatusLine, Headers, Body}} ->
            Response = response(StatusLine, Headers, Body),
            {ok, Response};
        {error, _} = Error ->
            Error
    end.

-spec method(booze_message:request()) -> atom().
method(Request) ->
    Method = booze_message:method(Request),
    method_from_binary(iolist_to_binary(Method)).

-spec method_from_binary(binary()) -> atom().
method_from_binary(<<"DELETE">>)  -> delete;
method_from_binary(<<"GET">>)     -> get;
method_from_binary(<<"HEAD">>)    -> head;
method_from_binary(<<"OPTIONS">>) -> options;
method_from_binary(<<"PATCH">>)   -> patch;
method_from_binary(<<"POST">>)    -> post;
method_from_binary(<<"PUT">>)     -> put.

-spec request(booze_message:request()) -> tuple().
request(Request) ->
    URL = url(Request),
    Headers = headers(Request),
    case body(Request) of
        Body when byte_size(Body) > 0 ->
            ContentType = content_type(Request),
            {URL, Headers, ContentType, Body};
        _ ->
            {URL, Headers}
    end.

-spec url(booze_message:request()) -> string().
url(Request) ->
    iolist_to_list(booze_message:uri(Request)).

-spec headers(booze_message:request()) -> list().
headers(Request) ->
    ConvertHeader = fun({N, V}) -> {binary_to_list(N), V} end,
    lists:map(ConvertHeader, booze_message:headers(Request)).

-spec content_type(booze_message:request()) -> string().
content_type(Request) ->
    Line = booze_message:header_line(<<"content-type">>, Request),
    binary_to_list(Line).

-spec body(booze_message:request()) -> binary().
body(Request) ->
    iolist_to_binary(booze_message:body(Request)).

-spec http_options(options()) -> proplists:proplist().
http_options(Options) ->
    Timeout = maps:get(timeout, Options, infinity),
    ConnectTimout = maps:get(connect_timeout, Options, Timeout),
    [{timeout, Timeout}, {connect_timeout, ConnectTimout}].

-spec profile(options()) -> atom() | pid().
profile(#{httpc_profile := Profile}) ->
    Profile;
profile(_Options) ->
    httpc:default_profile().

-spec response(tuple(), list(), binary()) -> booze_message:response().
response({_, Status, _}, Headers, Body) ->
    ConvertHeader = fun({N, V}) -> {list_to_binary(N), list_to_binary(V)} end,
    booze_message:response(Status, lists:map(ConvertHeader, Headers), Body).

-spec iolist_to_list(iolist() | binary()) -> list().
iolist_to_list(IoListOrBinary) ->
    binary_to_list(iolist_to_binary(IoListOrBinary)).
