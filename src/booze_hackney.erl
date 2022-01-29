-module(booze_hackney).

-export([handler/0,
         handler/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: #{hackney_pool    => atom(),
                     connect_timeout => timeout(),
                     recv_timeout    => timeout()}.
-export_type([options/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handler() -> booze:handler().
handler() -> fun send/2.

-spec handler(reference()) -> booze:handler().
handler(ConnRef) ->
    fun(Request, _Options) -> send_using(ConnRef, Request) end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send(booze_message:request(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
send(Request, Options) ->
    Result = hackney:request(booze_message:method(Request),
                             booze_message:uri(Request),
                             booze_message:headers(Request),
                             booze_message:body(Request),
                             [with_body | options(Options)]),
    result(Result).

-spec send_using(reference(), booze_message:request()) ->
          {ok, booze_message:response()} |
          {error, term()}.
send_using(ConnRef, Request) ->
    Result = hackney:send_request(ConnRef, request(Request), [with_body]),
    result(Result).

-spec options(options()) -> proplists:proplist().
options(Options) ->
    [{pool, maps:get(hackney_pool, Options, default)},
     {connect_timeout, maps:get(connect_timeout, Options, 8000)},
     {recv_timeout, maps:get(recv_timeout, Options, 5000)}].

-spec request(booze_message:request()) -> tuple().
request(Request) ->
    Method  = booze_message:method(Request),
    Path    = iolist_to_binary(booze_message:uri(Request)),
    Headers = booze_message:headers(Request),
    Body    = booze_message:body(Request),
    {Method, Path, Headers, Body}.

-spec result(tuple()) ->
          {ok, booze_message:response()} |
          {error, term()}.
result({ok, Status, Headers}) ->
    Response = booze_message:response(Status, Headers),
    {ok, Response};
result({ok, Status, Headers, Body}) ->
    Response = booze_message:response(Status, Headers, Body),
    {ok, Response};
result({error, Reason}) ->
    {error, Reason}.
