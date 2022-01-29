-module(booze_message_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

create_request_test() ->
    Requests =
        [booze_message:request("GET", "http://example.com"),
         booze_message:request("GET", "http://example.com", []),
         booze_message:request("GET", "http://example.com", [], <<>>)],
    [begin
         ?assertEqual("GET", booze_message:method(Request)),
         ?assertEqual("http://example.com", booze_message:uri(Request)),
         ?assertEqual([], booze_message:headers(Request)),
         ?assertEqual(<<>>, booze_message:body(Request))
     end || Request <- Requests].

create_response_test() ->
    Responses =
        [booze_message:response(200),
         booze_message:response(200, []),
         booze_message:response(200, [], <<>>)],
    [begin
         ?assertEqual(200, booze_message:status(Response)),
         ?assertEqual([], booze_message:headers(Response)),
         ?assertEqual(<<>>, booze_message:body(Response))
     end || Response <- Responses].

message_with_no_headers_test() ->
    Msg = message_with_headers([]),
    ?assertEqual([], booze_message:headers(Msg)),
    ?assertEqual([], booze_message:header_values(<<"name">>, Msg)),
    ?assertNot(booze_message:has_header(<<"name">>, Msg)).

message_with_single_header_test() ->
    Msg = message_with_headers([{<<"name">>, <<"value">>}]),
    ?assertEqual([{<<"name">>, <<"value">>}], booze_message:headers(Msg)),
    ?assertEqual([<<"value">>], booze_message:header_values(<<"name">>, Msg)),
    ?assertEqual([<<"value">>], booze_message:header_values(<<"NAME">>, Msg)),
    ?assert(booze_message:has_header(<<"name">>, Msg)),
    ?assert(booze_message:has_header(<<"NAME">>, Msg)).

add_header_with_multiple_values_test() ->
    Msg1 = message_with_headers([]),
    ?assertEqual([], booze_message:headers(Msg1)),
    Msg2 = booze_message:add_header(<<"name">>, <<"value1">>, Msg1),
    ?assertEqual([{<<"name">>, <<"value1">>}], booze_message:headers(Msg2)),
    ?assertEqual([<<"value1">>], booze_message:header_values(<<"name">>, Msg2)),
    Msg3 = booze_message:add_header(<<"name">>, <<"value2">>, Msg2),
    ?assertEqual([{<<"name">>, <<"value1">>},
                  {<<"name">>, <<"value2">>}], booze_message:headers(Msg3)),
    ?assertEqual([<<"value1">>,
                  <<"value2">>], booze_message:header_values(<<"name">>, Msg3)).

remove_header_test() ->
    Msg1 = message_with_headers([{<<"name">>, <<"value1">>},
                                 {<<"NAME">>, <<"value2">>}]),
    Msg2 = booze_message:remove_header(<<"name">>, Msg1),
    ?assertEqual([], booze_message:headers(Msg2)).

set_new_header_value_test() ->
    Msg1 = message_with_headers([]),
    ?assertEqual([], booze_message:headers(Msg1)),
    Msg2 = booze_message:set_header(<<"name">>, <<"value">>, Msg1),
    ?assertEqual([{<<"name">>, <<"value">>}], booze_message:headers(Msg2)).

replace_existing_header_value_test() ->
    Msg1 = message_with_headers([{<<"name">>, <<"value1">>}]),
    ?assertEqual([{<<"name">>, <<"value1">>}], booze_message:headers(Msg1)),
    Msg2 = booze_message:set_header(<<"name">>, <<"value2">>, Msg1),
    ?assertEqual([{<<"name">>, <<"value2">>}], booze_message:headers(Msg2)).

header_line_test() ->
    Msg = message_with_headers([{<<"name">>, <<"value1">>},
                                {<<"NAME">>, <<"value2">>}]),
    ?assertEqual(<<"value1, value2">>,
                 booze_message:header_line(<<"name">>, Msg)).

request_target_test_() ->
    [begin
         Req = booze_message:request("GET", URI),
         ?_assertEqual(Target, booze_message:target(Req))
     end || {URI, Target} <- [{"http://example.com?name=ferret", "/?name=ferret"},
                              {"http://example.com", "/"},
                              {"/?name=ferret", "/?name=ferret"},
                              {"/over/there", "/over/there"}]].

%%%===================================================================
%%% Helper functions
%%%===================================================================

message_with_headers(Headers) ->
    booze_message:response(200, Headers).
