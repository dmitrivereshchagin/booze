-module(booze_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(GROUPS, [gun, hackney, hackney_connect, httpc]).

-define(BASE_URI, "http://localhost:32002").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() ->
    [{group, Group} || Group <- ?GROUPS].

groups() ->
    [{Group, [],
      [request_with_get_method,
       request_with_head_method,
       request_with_body,
       request_without_response]}
     || Group <- ?GROUPS].

init_per_group(gun, Config) ->
    {ok, _} = bookish_spork:start_server(),
    {ok, Started} = application:ensure_all_started(gun),
    Options =
        fun() ->
                #{host := Host, port := Port} =
                    booze_uri:normalize(?BASE_URI, [return_map]),
                {ok, ConnPid} = gun:open(Host, Port),
                {ok, http} = gun:await_up(ConnPid),
                #{handler => booze_gun:handler(ConnPid)}
        end,
    [{started, Started}, {options, Options} | Config];
init_per_group(hackney, Config) ->
    {ok, _} = bookish_spork:start_server(),
    {ok, Started} = application:ensure_all_started(hackney),
    Options = #{handler  => booze_hackney:handler(),
                base_uri => ?BASE_URI},
    [{started, Started}, {options, Options} | Config];
init_per_group(hackney_connect, Config) ->
    {ok, _} = bookish_spork:start_server(),
    {ok, Started} = application:ensure_all_started(hackney),
    Options =
        fun() ->
                {ok, ConnRef} = hackney:connect(?BASE_URI),
                #{handler => booze_hackney:handler(ConnRef)}
        end,
    [{started, Started}, {options, Options} | Config];
init_per_group(httpc, Config) ->
    {ok, _} = bookish_spork:start_server(),
    {ok, Started} = application:ensure_all_started(inets),
    Options = #{handler  => booze_httpc:handler(),
                base_uri => ?BASE_URI},
    [{started, Started}, {options, Options} | Config].

end_per_group(_Group, Config) ->
    lists:foreach(fun application:stop/1,
                  lists:reverse(?config(started, Config))),
    bookish_spork:stop_server().

init_per_testcase(_TestCase, Config) ->
    Client =
        booze:client(
          case ?config(options, Config) of
              Fun when is_function(Fun) ->
                  Fun();
              Options ->
                  Options
          end),
    [{client, Client} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================

request_with_get_method(Config) ->
    bookish_spork:stub_request([200,
                                #{<<"content-type">> => <<"text/plain">>},
                                <<"hello">>]),
    {ok, Response} = booze:get("/", ?config(client, Config)),
    ?assertEqual(200, booze_message:status(Response)),
    ?assertEqual([<<"text/plain">>],
                 booze_message:header_values(<<"content-type">>, Response)),
    ?assertEqual(<<"hello">>, booze_message:body(Response)),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual(get, bookish_spork_request:method(Request)).

request_with_head_method(Config) ->
    bookish_spork:stub_request(),
    {ok, Response} = booze:head("/", ?config(client, Config)),
    ?assertEqual(204, booze_message:status(Response)),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual(head, bookish_spork_request:method(Request)).

request_with_body(Config) ->
    bookish_spork:stub_request(),
    {ok, _} = booze:post("/", ?config(client, Config),
                         #{headers => [{<<"content-type">>, <<"text/plain">>}],
                           body    => <<"hello">>}),
    {ok, Request} = bookish_spork:capture_request(),
    ?assertEqual(<<"text/plain">>,
                 bookish_spork_request:header(Request, <<"content-type">>)),
    ?assertEqual(<<"hello">>, bookish_spork_request:body(Request)).

request_without_response(Config) ->
    ?assertMatch({error, _}, booze:get("/", ?config(client, Config))).
