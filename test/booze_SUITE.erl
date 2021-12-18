-module(booze_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [request_sent,
     middleware_applied].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(booze),
    {ok, _} = bookish_spork:start_server(),
    [{uri, <<"http://localhost:32002">>} | Config].

end_per_suite(_Config) ->
    bookish_spork:stop_server().

%%%===================================================================
%%% Test cases
%%%===================================================================

request_sent(Config) ->
    bookish_spork:stub_request(),
    {ok, Resp} = booze:send_request(request(Config), booze:client()),
    {ok, _} = bookish_spork:capture_request(),
    ?assertEqual(204, booze_message:code(Resp)).

middleware_applied(Config) ->
    Client1 = booze:client(#{middlewares => [fun status_writer/1],
                             env => #{status => <<"rockin">>}}),

    bookish_spork:stub_request(),
    {ok, _} = booze:send_request(request(Config), Client1),
    {ok, Req1} = bookish_spork:capture_request(),
    ?assertEqual(<<"rockin">>,
                 bookish_spork_request:header(Req1, <<"X-Status">>)),

    Client2 = booze:set_env(status, <<"rollin">>, Client1),

    bookish_spork:stub_request(),
    {ok, _} = booze:send_request(request(Config), Client2),
    {ok, Req2} = bookish_spork:capture_request(),
    ?assertEqual(<<"rollin">>,
                 bookish_spork_request:header(Req2, <<"X-Status">>)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

request(Config) ->
    booze_message:request(?config(uri, Config)).

status_writer(Handler) ->
    fun(Req0, #{status := Status} = Env) ->
            Req = booze_message:add_header(<<"X-Status">>, Status, Req0),
            Handler(Req, Env)
    end.
