-module(booze_gun).

-export([handler/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type options() :: #{await_timeout => timeout()}.
-export_type([options/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec handler(pid()) -> booze:handler().
handler(ConnPid) ->
    fun(Request, Options) -> send_using(ConnPid, Request, Options) end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec send_using(pid(), booze_message:request(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
send_using(ConnPid, Request, Options) ->
    StreamRef = gun:request(ConnPid,
                            booze_message:method(Request),
                            booze_message:uri(Request),
                            booze_message:headers(Request),
                            booze_message:body(Request)),
    await_response(ConnPid, StreamRef, Options).

-spec await_response(pid(), reference(), options()) ->
          {ok, booze_message:response()} |
          {error, term()}.
await_response(ConnPid, StreamRef, Options) ->
    Timeout = maps:get(await_timeout, Options, 5000),
    case gun:await(ConnPid, StreamRef, Timeout) of
        {response, fin, Status, Headers} ->
            Response = booze_message:response(Status, Headers),
            {ok, Response};
        {response, nofin, Status, Headers} ->
            case gun:await_body(ConnPid, StreamRef, Timeout) of
                {ok, Body} ->
                    Response = booze_message:response(Status, Headers, Body),
                    {ok, Response};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.
