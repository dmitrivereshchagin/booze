-module(booze_mock_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

start_empty_test() ->
    Mock = booze_mock:start(),
    ?assertEqual(0, booze_mock:count(Mock)).

start_with_single_result_test() ->
    Mock = booze_mock:start({reply, 200}),
    ?assertEqual(1, booze_mock:count(Mock)).

start_with_multiple_results_test() ->
    Mock = booze_mock:start([{reply, 200}, {reply, 200}]),
    ?assertEqual(2, booze_mock:count(Mock)).

append_single_result_test() ->
    Mock = booze_mock:start(),
    booze_mock:append({reply, 200}, Mock),
    ?assertEqual(1, booze_mock:count(Mock)).

append_multiple_results_test() ->
    Mock = booze_mock:start(),
    booze_mock:append([{reply, 204}, {reply, 404}], Mock),
    ?assertEqual(2, booze_mock:count(Mock)),
    Handler = booze_mock:handler(Mock),
    {ok, Resp1} = Handler(booze_message:request("DELETE", "/"), #{}),
    ?assertEqual(204, booze_message:status(Resp1)),
    ?assertEqual(1, booze_mock:count(Mock)),
    {ok, Resp2} = Handler(booze_message:request("DELETE", "/"), #{}),
    ?assertEqual(404, booze_message:status(Resp2)),
    ?assertEqual(0, booze_mock:count(Mock)).

reset_test() ->
    Mock = booze_mock:start({reply, 200}),
    booze_mock:reset(Mock),
    ?assertEqual(0, booze_mock:count(Mock)).

handler_test() ->
    Mock = booze_mock:start({reply, 200}),
    Handler = booze_mock:handler(Mock),
    {ok, Resp} = Handler(booze_message:request("GET", "/"), #{}),
    ?assertEqual(200, booze_message:status(Resp)),
    ?assertEqual(0, booze_mock:count(Mock)).

empty_fails_test() ->
    Mock = booze_mock:start(),
    Handler = booze_mock:handler(Mock),
    ?assertError({booze_mock, empty_queue},
                 Handler(booze_message:request("GET", "/"), #{})).

last_request_undefined_test() ->
    Mock = booze_mock:start(),
    ?assertEqual(undefined, booze_mock:last_request(Mock)).

last_request_captured_test() ->
    Req = booze_message:request("GET", "/"),
    Mock = booze_mock:start({reply, 200}),
    _ = (booze_mock:handler(Mock))(Req, #{}),
    ?assertEqual(Req, booze_mock:last_request(Mock)).

last_options_undefined_test() ->
    Mock = booze_mock:start(),
    ?assertEqual(undefined, booze_mock:last_options(Mock)).

last_options_captured_test() ->
    Mock = booze_mock:start({reply, 200}),
    Options = #{foo => bar},
    _ = (booze_mock:handler(Mock))(booze_message:request("GET", "/"), Options),
    ?assertEqual(Options, booze_mock:last_options(Mock)).

response_test() ->
    Mock = booze_mock:start({reply, 404}),
    Handler = booze_mock:handler(Mock),
    {ok, Resp} = Handler(booze_message:request("GET", "/"), #{}),
    ?assertEqual(404, booze_message:status(Resp)).

appended_response_test() ->
    Mock = booze_mock:start(),
    booze_mock:append({reply, 404}, Mock),
    Handler = booze_mock:handler(Mock),
    {ok, Resp} = Handler(booze_message:request("GET", "/"), #{}),
    ?assertEqual(404, booze_message:status(Resp)).

error_test() ->
    Mock = booze_mock:start({fail, timeout}),
    Handler = booze_mock:handler(Mock),
    ?assertEqual({error, timeout},
                 Handler(booze_message:request("GET", "/"), #{})).

message_response_test() ->
    Resp = booze_message:response(503),
    Mock = booze_mock:start(Resp),
    Handler = booze_mock:handler(Mock),
    ?assertEqual({ok, Resp},
                 Handler(booze_message:request("GET", "/"), #{})).
