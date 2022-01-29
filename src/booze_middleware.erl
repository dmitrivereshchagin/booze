-module(booze_middleware).

-export([map_request/1,
         map_response/1]).

-export([tap/1,
         tap/2]).

-export([retry/1,
         retry/2]).

-export([log/0,
         log/1,
         log/2,
         log/3]).

-ifdef(TEST).
-export([exp_delay/1]).
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

-type tap_before() :: fun((booze_message:request(),
                           #{atom() => term()}) ->
                                 term()).
-export_type([tap_before/0]).

-type tap_after() :: fun((booze_message:request(),
                          #{atom() => term()},
                          {ok, booze_message:response()} |
                          {error, term()}) ->
                                term()).
-export_type([tap_after/0]).

-type retry_decider() :: fun((Retries :: non_neg_integer(),
                              booze_message:request(),
                              {ok, booze_message:response()} |
                              {error, term()}) ->
                                    boolean()).
-export_type([retry_decider/0]).

-type retry_delay() :: fun((Retries :: non_neg_integer()) ->
                                  non_neg_integer()).
-export_type([retry_delay/0]).

-type log_msg_fun() :: fun((booze_message:request(),
                            {ok, booze_message:response()} |
                            {error, term()}) ->
                                  log_msg_fun_return() |
                                  {log_msg_fun_return(), logger:metadata()}).
-export_type([log_msg_fun/0]).

-type log_msg_fun_return() :: {io:format(), list()} |
                              logger:report() |
                              unicode:chardata() |
                              ignore.

%%%===================================================================
%%% API
%%%===================================================================

-spec map_request(fun((Request) -> Request)) -> booze:middleware() when
      Request :: booze_message:request().
map_request(Fun) ->
    fun(Handler) ->
            fun(Request, Options) ->
                    Handler(Fun(Request), Options)
            end
    end.

-spec map_response(fun((Response) -> Response)) -> booze:middleware() when
      Response :: booze_message:response().
map_response(Fun) ->
    fun(Handler) ->
            fun(Request, Options) ->
                    case Handler(Request, Options) of
                        {ok, Response} ->
                            {ok, Fun(Response)};
                        Result ->
                            Result
                    end
            end
    end.

-spec tap(Arg) -> booze:middleware() when
      Arg :: tap_before() | tap_after().
tap(Before) when is_function(Before, 2) ->
    After = fun(_Request, _Options, _Result) -> ok end,
    tap(Before, After);
tap(After) when is_function(After, 3) ->
    Before = fun(_Result, _Options) -> ok end,
    tap(Before, After).

-spec tap(tap_before(), tap_after()) -> booze:middleware().
tap(Before, After) ->
    fun(Handler) ->
            fun(Request, Options) ->
                    Before(Request, Options),
                    Result = Handler(Request, Options),
                    After(Request, Options, Result),
                    Result
            end
    end.

-spec retry(retry_decider()) -> booze:middleware().
retry(Decider) -> retry(Decider, fun exp_delay/1).

-spec retry(retry_decider(), retry_delay()) -> booze:middleware().
retry(Decider, Delay) ->
    fun(Handler) ->
            fun Retry(Request, Options) ->
                    Retries = maps:get(retries, Options, 0),
                    timer:sleep(Delay(Retries)),
                    Result = Handler(Request, Options),
                    case Decider(Retries, Request, Result) of
                        true ->
                            Retry(Request, Options#{retries => Retries + 1});
                        false ->
                            Result
                    end
            end
    end.

-spec log() -> booze:middleware().
log() ->
    log(info, fun log_report/2, #{}).

-spec log(Arg) -> booze:middleware() when
      Arg :: logger:level() | log_msg_fun() | logger:metadata().
log(Level) when is_atom(Level) ->
    log(Level, fun log_report/2, #{});
log(Fun) when is_function(Fun, 2) ->
    log(info, Fun, #{});
log(Metadata) ->
    log(info, fun log_report/2, Metadata).

-spec log(logger:level(), Arg) -> booze:middleware() when
      Arg :: log_msg_fun() | logger:metadata().
log(Level, Fun) when is_function(Fun, 2) ->
    log(Level, Fun, #{});
log(Level, Metadata) ->
    log(Level, fun log_report/2, Metadata).

-spec log(logger:level(), log_msg_fun(), logger:metadata()) ->
          booze:middleware().
log(Level, Fun, Metadata) ->
    fun(Handler) ->
            fun(Request, Options) ->
                    Result = Handler(Request, Options),
                    logger:log(
                      case Result of
                          {ok, _}    -> Level;
                          {error, _} -> error
                      end,
                      fun(_) -> Fun(Request, Result) end, [],
                      Metadata),
                    Result
            end
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec exp_delay(non_neg_integer()) -> non_neg_integer().
exp_delay(Retries) ->
    timer:seconds(erlang:floor(math:pow(2, Retries - 1))).

-spec log_report(Request, Result) -> logger:report() when
      Request :: booze_message:request(),
      Result  :: {ok, booze_message:response()} |
                 {error, term()}.
log_report(Request, Result) ->
    [{method, booze_message:method(Request)},
     {target, booze_message:target(Request)},
     case Result of
         {ok, Response} ->
             {status, booze_message:status(Response)};
         {error, Reason} ->
             {error, Reason}
     end].
