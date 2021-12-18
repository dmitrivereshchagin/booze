-module(booze_message_tests).

-include_lib("eunit/include/eunit.hrl").

request1_test() ->
    Req = booze_message:request(<<"http://example.com">>),
    ?assertEqual(<<"GET">>, booze_message:method(Req)),
    ?assertEqual(<<"http://example.com">>, booze_message:uri(Req)),
    ?assertEqual([], booze_message:headers(Req)),
    ?assertEqual(<<>>, booze_message:body(Req)).

request2_test() ->
    Req = booze_message:request(<<"HEAD">>, <<"http://example.com">>),
    ?assertEqual(<<"HEAD">>, booze_message:method(Req)),
    ?assertEqual(<<"http://example.com">>, booze_message:uri(Req)),
    ?assertEqual([], booze_message:headers(Req)),
    ?assertEqual(<<>>, booze_message:body(Req)).

request3_test() ->
    Req = booze_message:request(<<"GET">>, <<"http://example.com">>, [{<<"X-Answer">>, <<"42">>}]),
    ?assertEqual(<<"GET">>, booze_message:method(Req)),
    ?assertEqual(<<"http://example.com">>, booze_message:uri(Req)),
    ?assertEqual([{<<"X-Answer">>, <<"42">>}], booze_message:headers(Req)),
    ?assertEqual(<<>>, booze_message:body(Req)).

request4_test() ->
    Req = booze_message:request(<<"POST">>, <<"http://example.com">>, [], <<"Hello">>),
    ?assertEqual(<<"POST">>, booze_message:method(Req)),
    ?assertEqual(<<"http://example.com">>, booze_message:uri(Req)),
    ?assertEqual([], booze_message:headers(Req)),
    ?assertEqual(<<"Hello">>, booze_message:body(Req)).

set_method_test() ->
    Req0 = booze_message:request(<<"http://example.com">>),
    Req1 = booze_message:set_method(<<"HEAD">>, Req0),
    ?assertEqual(<<"HEAD">>, booze_message:method(Req1)).

set_uri_test() ->
    Req0 = booze_message:request(<<"http://example.com">>),
    Req1 = booze_message:set_uri(<<"http://example.org">>, Req0),
    ?assertEqual(<<"http://example.org">>, booze_message:uri(Req1)).

set_headers_test() ->
    Req0 = booze_message:request(<<"http://example.com">>),
    Req1 = booze_message:set_headers([{<<"X-Answer">>, <<"42">>}], Req0),
    ?assertEqual([{<<"X-Answer">>, <<"42">>}], booze_message:headers(Req1)).

set_body_test() ->
    Req0 = booze_message:request(<<"http://example.com">>),
    Req1 = booze_message:set_body(<<"Hello">>, Req0),
    ?assertEqual(<<"Hello">>, booze_message:body(Req1)).

add_header_test() ->
    Req0 = booze_message:request(<<"http://example.com">>),
    Req1 = booze_message:set_headers([{<<"x-foo">>, <<"foo">>}], Req0),
    Req2 = booze_message:add_header(<<"X-FOO">>, <<"foo">>, Req1),
    ?assertEqual([{<<"X-FOO">>, <<"foo">>},
                  {<<"x-foo">>, <<"foo">>}], booze_message:headers(Req2)).

remove_header_test() ->
    Req0 = booze_message:request(<<"http://example.com">>),
    Req1 = booze_message:set_headers([{<<"X-FOO">>, <<"foo">>},
                                      {<<"x-foo">>, <<"foo">>}], Req0),
    Req2 = booze_message:remove_header(<<"X-Foo">>, Req1),
    ?assertEqual([], booze_message:headers(Req2)).

response_test() ->
    Resp = booze_message:response(200, [{<<"X-Answer">>, <<"42">>}], <<"Hello">>),
    ?assertEqual(200, booze_message:code(Resp)),
    ?assertEqual([{<<"X-Answer">>, <<"42">>}], booze_message:headers(Resp)),
    ?assertEqual(<<"Hello">>, booze_message:body(Resp)).

set_code_test() ->
    Resp0 = booze_message:response(200, [], <<>>),
    Resp1 = booze_message:set_code(404, Resp0),
    ?assertEqual(404, booze_message:code(Resp1)).
