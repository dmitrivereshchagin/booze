-module(booze_middleware_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

map_request_test() ->
    M = booze_middleware:map_request(
          fun(Req) -> booze_message:set_method("HEAD", Req) end),
    H = M(fun(Req, _) -> {ok, pack(Req)} end),
    {ok, Resp} = H(booze_message:request("GET", "/"), #{}),
    ?assertEqual("HEAD", booze_message:method(unpack(Resp))).

map_response_test() ->
    M = booze_middleware:map_response(
          fun(Resp) -> booze_message:set_status(204, Resp) end),
    H = M(reply(booze_message:response(200))),
    {ok, Resp} = H(booze_message:request("GET", "/"), #{}),
    ?assertEqual(204, booze_message:status(Resp)).

map_response2_test() ->
    M = booze_middleware:map_response(fun(Resp) -> Resp end),
    H = M(fail(closed)),
    {error, Reason} = H(booze_message:request("GET", "/"), #{}),
    ?assertEqual(closed, Reason).

tap_test() ->
    P = self(),
    Ms = [booze_middleware:tap(fun(_, _)    -> P ! 1 end,
                               fun(_, _, _) -> P ! 4 end),
          booze_middleware:tap(fun(_, _, _) -> P ! 3 end),
          booze_middleware:tap(fun(_, _)    -> P ! 2 end)],
    H = booze:handler(Ms, reply(booze_message:response(200))),
    {ok, _} = H(booze_message:request("GET", "/"), #{}),
    ?assertEqual([1, 2, 3, 4], receive_all()).

retry_test() ->
    Mock = booze_mock:start(lists:duplicate(3, {reply, 503})),
    Decider = fun(Retries, _Req, _Result) -> Retries < 2 end,
    Handler = booze:handler([retry_middleware(Decider)],
                            booze_mock:handler(Mock)),
    {ok, _} = Handler(booze_message:request("GET", "/"), #{}),
    ?assertEqual(0, booze_mock:count(Mock)),
    booze_mock:stop(Mock).

retry_bad_response_test() ->
    Mock = booze_mock:start([{reply, 503}, {reply, 200}]),
    Decider = fun(_Retries, _Req, {ok, #{status := Status}}) -> Status =/= 200 end,
    Handler = booze:handler([retry_middleware(Decider)],
                            booze_mock:handler(Mock)),
    {ok, _} = Handler(booze_message:request("GET", "/"), #{}),
    ?assertEqual(0, booze_mock:count(Mock)),
    booze_mock:stop(Mock).

retry_reqest_test() ->
    Mock = booze_mock:start([{reply, 200}]),
    Request = booze_message:request("GET", "/"),
    Decider = fun(_Retries, Req, _Result) ->
                      ?assertEqual(Req, Request),
                      false
              end,
    Handler = booze:handler([retry_middleware(Decider)],
                            booze_mock:handler(Mock)),
    {ok, _} = Handler(Request, #{}),
    ?assertEqual(0, booze_mock:count(Mock)),
    booze_mock:stop(Mock).

exp_delay_test_() ->
    [?_assertEqual(timer:seconds(0), booze_middleware:exp_delay(0)),
     ?_assertEqual(timer:seconds(1), booze_middleware:exp_delay(1)),
     ?_assertEqual(timer:seconds(2), booze_middleware:exp_delay(2)),
     ?_assertEqual(timer:seconds(4), booze_middleware:exp_delay(3)),
     ?_assertEqual(timer:seconds(8), booze_middleware:exp_delay(4))].

%%%===================================================================
%%% Helper functions
%%%===================================================================

reply(Resp) ->
    fun(_, _) -> {ok, Resp} end.

fail(Reason) ->
    fun(_, _) -> {error, Reason} end.

pack(Term) ->
    booze_message:response(200, [], term_to_binary(Term)).

unpack(Resp) ->
    binary_to_term(booze_message:body(Resp)).

receive_all() -> receive_all([]).

receive_all(Msgs) ->
    receive
        Msg -> receive_all([Msg | Msgs])
    after 0 ->
            lists:reverse(Msgs)
    end.

retry_middleware(Decider) ->
    booze_middleware:retry(Decider, fun(_) -> 0 end).
