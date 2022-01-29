-module(booze_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

with_mock_test_() ->
    {foreach,
     fun() -> booze_mock:start({reply, 204}) end,
     fun booze_mock:stop/1,
     [{with, [T]} ||
         T <- [fun uri_test/1,
               fun honor_base_uri_parameter_test/1,
               fun override_base_uri_parameter_test/1,
               fun ignore_base_uri_parameter_test/1,
               fun set_query_parameter_test/1,
               fun accept_query_as_list_test/1,
               fun default_query_test/1,
               fun override_default_query_test/1,
               fun default_headers_test/1,
               fun headers_test/1,
               fun undefined_default_test/1,
               fun override_default_header_test/1,
               %%
               fun delete_method_test/1,
               fun get_method_test/1,
               fun head_method_test/1,
               fun options_method_test/1,
               fun patch_method_test/1,
               fun post_method_test/1,
               fun put_method_test/1]]}.

uri_test(Mock) ->
    _ = booze:get("http://example.com/", mclient(Mock)),
    ?assertEqual("http://example.com/", booze_mock:last_uri(Mock)).

honor_base_uri_parameter_test(Mock) ->
    _ = booze:get("foo", mclient(Mock, #{base_uri => "http://example.com"})),
    ?assertEqual("http://example.com/foo", booze_mock:last_uri(Mock)).

override_base_uri_parameter_test(Mock) ->
    Client = mclient(Mock, #{base_uri => "http://example.com"}),
    _ = booze:get("foo", Client, #{base_uri => "http://example.org"}),
    ?assertEqual("http://example.org/foo", booze_mock:last_uri(Mock)).

ignore_base_uri_parameter_test(Mock) ->
    Client = mclient(Mock, #{base_uri => "http://example.com"}),
    _ = booze:get("/index.html", Client, #{base_uri => undefined}),
    ?assertEqual("/index.html", booze_mock:last_uri(Mock)).

set_query_parameter_test(Mock) ->
    _ = booze:get("/index.html", mclient(Mock), #{query => <<"foo=bar">>}),
    ?assertEqual("/index.html?foo=bar", booze_mock:last_uri(Mock)).

accept_query_as_list_test(Mock) ->
    _ = booze:get("/index.html", mclient(Mock), #{query => [{"foo", "bar"}]}),
    ?assertEqual("/index.html?foo=bar", booze_mock:last_uri(Mock)).

default_query_test(Mock) ->
    _ = booze:get("/index.html", mclient(Mock, #{query => [{"foo", "bar"}]})),
    ?assertEqual("/index.html?foo=bar", booze_mock:last_uri(Mock)).

override_default_query_test(Mock) ->
    Client = mclient(Mock, #{query => [{"foo", "bar"}]}),
    _ = booze:get("/index.html", Client, #{query => undefined}),
    ?assertEqual("/index.html", booze_mock:last_uri(Mock)).

default_headers_test(Mock) ->
    _ = booze:get("/", mclient(Mock, #{headers => [{<<"x-foo">>, "bar"}]})),
    Headers = booze_mock:last_headers(Mock),
    ?assert(booze_message_headers:has(<<"x-foo">>, Headers)).

headers_test(Mock) ->
    Client = mclient(Mock, #{headers => [{<<"x-foo">>, "bar"}]}),
    _ = booze:get("/", Client, #{headers => [{<<"x-bar">>, "bar"}]}),
    Headers = booze_mock:last_headers(Mock),
    ?assert(booze_message_headers:has(<<"x-foo">>, Headers)),
    ?assert(booze_message_headers:has(<<"x-bar">>, Headers)).

undefined_default_test(Mock) ->
    _ = booze:get("/", mclient(Mock, #{base_uri => undefined})),
    ?assertEqual("/", booze_mock:last_uri(Mock)).

override_default_header_test(Mock) ->
    Client = mclient(Mock, #{headers => [{<<"x-foo">>, "bar"}]}),
    _ = booze:get("/", Client, #{headers => [{<<"x-foo">>, "baz"}]}),
    Headers = booze_mock:last_headers(Mock),
    ?assertEqual(["baz"], booze_message_headers:values(<<"x-foo">>, Headers)).

override_with_multiple_header_values_test() ->
    Client = client(#{headers => [{<<"x-foo">>, "bar"}]}),
    {ok, Resp} = booze:get("/", Client, #{headers => [{<<"x-foo">>, "baz"},
                                                      {<<"x-foo">>, "qux"}]}),
    ?assertEqual(["baz", "qux"],
                 booze_message:header_values(<<"x-foo">>, unpack(Resp))).

request_headers_override_defaults_test() ->
    Client = client(#{headers => [{<<"x-foo">>, "bar"}]}),
    Req = booze_message:request(<<"GET">>, "/", [{<<"x-foo">>, "baz"}]),
    {ok, Resp} = booze:send(Req, Client),
    ?assertEqual(["baz"], booze_message:header_values(<<"x-foo">>, unpack(Resp))).

ignore_default_headers_test() ->
    Client = client(#{headers => [{<<"x-foo">>, "bar"}]}),
    {ok, Resp} = booze:get("/", Client, #{headers => undefined}),
    ?assertNot(booze_message:has_header(<<"x-foo">>, unpack(Resp))).

body_test() ->
    Req = booze_message:request(<<"GET">>, "/", [], <<"Hello">>),
    {ok, Resp} = booze:send(Req, client()),
    Body = booze_message:body(unpack(Resp)),
    ?assertEqual(<<"Hello">>, Body).

body_from_defaults_test() ->
    Client = client(#{body => <<"Hello">>}),
    Req = booze_message:request(<<"GET">>, "/"),
    {ok, Resp} = booze:send(Req, Client),
    Body = booze_message:body(unpack(Resp)),
    ?assertEqual(<<"Hello">>, Body).

override_default_body_test() ->
    Client = client(#{body => <<"Hello">>}),
    Req = booze_message:request(<<"GET">>, "/"),
    {ok, Resp} = booze:send(Req, Client, #{body => <<"Bye">>}),
    Body = booze_message:body(unpack(Resp)),
    ?assertEqual(<<"Bye">>, Body).

ignore_default_body_test() ->
    Client = client(#{body => <<"Hello">>}),
    Req = booze_message:request(<<"GET">>, "/", [], <<"Bye">>),
    {ok, Resp} = booze:send(Req, Client, #{body => undefined}),
    Body = booze_message:body(unpack(Resp)),
    ?assertEqual(<<"Bye">>, Body).

json_body_test() ->
    Req = booze_message:request(<<"GET">>, "/"),
    {ok, Resp} = booze:send(Req, client(), #{body => {json, #{answer => 42}}}),
    ?assertEqual([<<"application/json">>],
                 booze_message:header_values(<<"content-type">>, unpack(Resp))),
    Body = booze_message:body(unpack(Resp)),
    ?assertEqual(<<"{\"answer\":42}">>, Body).

form_body_test() ->
    {ok, Resp} = booze:get("/", client(), #{body => {form, [{"foo", "bar"}]}}),
    ?assertEqual([<<"application/x-www-form-urlencoded">>],
                 booze_message:header_values(<<"content-type">>, unpack(Resp))),
    Body = booze_message:body(unpack(Resp)),
    ?assertEqual("foo=bar", Body).

middleware_order_test() ->
    Handler =
        handler(
          [fun(Handler) ->
                   fun(Req, Options) ->
                           Req1 = booze_message:add_header(<<"name">>, <<"value">>, Req),
                           Handler(Req1, Options)
                   end
           end,
           fun(Handler) ->
                   fun(Req, Options) ->
                           ?assert(booze_message:has_header(<<"name">>, Req)),
                           Handler(Req, Options)
                   end
           end]),
    {ok, Resp} = booze:get("/", client(#{handler => Handler})),
    ?assert(booze_message:has_header(<<"name">>, unpack(Resp))).

options_test() ->
    Handler = fun(_Req, Options) -> {ok, pack(Options)} end,
    Client = client(#{handler => Handler, headers => []}),
    {ok, Resp} = booze:get("/", Client, #{query => [], headers => [], body => <<>>}),
    ?assertEqual(#{handler => Handler}, unpack(Resp)).

delete_method_test(Mock) ->
    _ = booze:delete("/", mclient(Mock)),
    ?assertEqual("DELETE", booze_mock:last_method(Mock)).

get_method_test(Mock) ->
    _ = booze:get("/", mclient(Mock)),
    ?assertEqual("GET", booze_mock:last_method(Mock)).

head_method_test(Mock) ->
    _ = booze:head("/", mclient(Mock)),
    ?assertEqual("HEAD", booze_mock:last_method(Mock)).

options_method_test(Mock) ->
    _ = booze:options("/", mclient(Mock)),
    ?assertEqual("OPTIONS", booze_mock:last_method(Mock)).

patch_method_test(Mock) ->
    _ = booze:patch("/", mclient(Mock)),
    ?assertEqual("PATCH", booze_mock:last_method(Mock)).

post_method_test(Mock) ->
    _ = booze:post("/", mclient(Mock)),
    ?assertEqual("POST", booze_mock:last_method(Mock)).

put_method_test(Mock) ->
    _ = booze:put("/", mclient(Mock)),
    ?assertEqual("PUT", booze_mock:last_method(Mock)).

%%%===================================================================
%%% Helper functions
%%%===================================================================

mclient(Mock) -> mclient(Mock, #{}).

mclient(Mock, Options) ->
    Options1 = maps:merge(#{handler => mhandler(Mock)}, Options),
    booze:client(Options1).

mhandler(Mock) -> booze_mock:handler(Mock).

client() -> client(#{}).

client(Options) ->
    Options1 = maps:merge(#{handler => handler([])}, Options),
    booze:client(Options1).

handler(Middlewares) ->
    Handler = fun(Req, _Options) -> {ok, pack(Req)} end,
    booze:handler(Middlewares, Handler).

pack(Term) ->
    booze_message:response(200, [], term_to_binary(Term)).

unpack(Resp) ->
    binary_to_term(booze_message:body(Resp)).
