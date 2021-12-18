-module(booze).

-export([client/0,
         client/1,
         send_request/2,
         set_env/3]).

-export_type([client/0,
              config/0,
              middleware/0,
              handler/0,
              env/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(client, {handler :: handler(), env :: env()}).
-opaque client() :: #client{}.

-type config() :: #{handler     => handler(),
                    middlewares => [middleware()],
                    env         => env()}.

-type handler() :: fun((booze_message:request(), env()) ->
                              {ok, booze_message:response()} | {error, term()}).

-type middleware() :: fun((handler()) -> handler()).

-type env() :: #{atom() => term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec client() -> client().
client() ->
    client(#{}).

-spec client(config()) -> client().
client(Config0) ->
    Defaults = #{handler => fun handler/2, middlewares => [], env => #{}},
    Config = maps:merge(Defaults, Config0),
    #{handler := Handler0, middlewares := Middlewares, env := Env} = Config,
    Handler = lists:foldr(fun(M, H) -> M(H) end, Handler0, Middlewares),
    #client{handler = Handler, env = Env}.

-spec send_request(booze_message:request(), client()) ->
          {ok, booze_message:response()} | {error, term()}.
send_request(Req, #client{handler = Handler, env = Env}) ->
    Handler(Req, Env).

-spec set_env(Name, Value, client()) -> client() when
      Name :: atom(), Value :: term().
set_env(Name, Value, #client{env = Env} = Client) ->
    Client#client{env = Env#{Name => Value}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handler(booze_message:request(), env()) ->
          {ok, booze_message:response()} | {error, term()}.
handler(#{method := Method, uri := URI, headers := Headers, body := Body}, _Env) ->
    case hackney:request(Method, URI, Headers, Body, [with_body]) of
        {ok, Code, RespHeaders, RespBody} ->
            Resp = booze_message:response(Code, RespHeaders, RespBody),
            {ok, Resp};
        {error, _} = Error ->
            Error
    end.
