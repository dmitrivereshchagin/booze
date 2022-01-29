# booze

Basic but extensible HTTP client inspired by [Guzzle][].  All heavy
lifting is done by [hackney][], [httpc][] or [Gun][].

## Example

    {ok, _} = application:ensure_all_started(inets),

    AddFoo =
        fun(Req) -> booze_message:add_header(<<"x-foo">>, "foo", Req) end,

    Client = booze:client(
               #{base_uri => "http://httpbin.org",
                 handler => booze:handler(
                              [booze_middleware:map_request(AddFoo),
                               booze_middleware:log(notice)],
                              booze_httpc:handler()),
                 headers => [{<<"x-bar">>, "bar"}]}),

    {ok, #{status := 200}} = booze:head("anything", Client),

    {ok, #{body := Body}} =
        booze:put("anything", Client, #{body => {json, #{answer => 42}}}),

    {ok, _} = booze_json:decode(Body).

[Gun]: https://github.com/ninenines/gun
[Guzzle]: https://github.com/guzzle/guzzle
[hackney]: https://github.com/benoitc/hackney
[httpc]: https://www.erlang.org/doc/apps/inets/http_client.html
