-module(booze_middleware).

-include_lib("kernel/include/logger.hrl").

-export([map_request/1,
         map_response/1]).

-export([logger/1,
         retrier/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec map_request(fun((Req) -> Req)) -> booze:middleware() when
      Req :: booze_message:request().
map_request(Fun) ->
    fun(Handler) ->
            fun(Req, Env) -> Handler(Fun(Req), Env) end
    end.

-spec map_response(fun((Resp) -> Resp)) -> booze:middleware() when
      Resp :: booze_message:response().
map_response(Fun) ->
    fun(Handler) ->
            fun(Req, Env) ->
                    case Handler(Req, Env) of
                        {ok, Resp} -> {ok, Fun(Resp)};
                        Result     -> Result
                    end
            end
    end.

-spec logger(booze:handler()) -> booze:handler().
logger(Handler) ->
    fun(#{method := Method, uri := URI} = Req, Env) ->
            Meta = maps:get(logger_metadata, Env, #{}),
            Result = Handler(Req, Env),
            case Result of
                {ok, #{code := Code}} ->
                    ?LOG_INFO("request: ~s ~s: response: ~b",
                              [Method, URI, Code], Meta);
                {error, Reason} ->
                    ?LOG_INFO("request: ~s ~s: error: ~0p",
                              [Method, URI, Reason], Meta)
            end,
            Result
    end.

-spec retrier(booze:handler()) -> booze:handler().
retrier(Handler) ->
    fun Retry(Req, #{tries_left := 1} = Env) ->
            Handler(Req, Env);
        Retry(Req, #{tries_left := N} = Env) when N > 1 ->
            case Handler(Req, Env) of
                {ok, #{code := Code}} when Code =:= 429;
                                           Code =:= 503 ->
                    timer:sleep(timer:seconds(1)),
                    Retry(Req, Env#{tries_left := N - 1});
                Result ->
                    Result
            end;
        Retry(Req, Env) ->
            Retry(Req, Env#{tries_left => 5})
    end.
