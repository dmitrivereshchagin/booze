-module(booze_mock).

-behaviour(gen_server).

%% API
-export([start/0,
         start/1,
         stop/1,
         handler/1,
         last_request/1,
         last_method/1,
         last_uri/1,
         last_headers/1,
         last_body/1,
         last_options/1,
         append/2,
         reset/1,
         count/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type mock() :: pid().
-export_type([mock/0]).

-type result() :: {fail, Reason :: term()} |
                  {reply, booze_message:status()} |
                  booze_message:response().
-export_type([result/0]).

-record(state, {result_queue :: queue:queue(result()),
                last_request :: maybe(booze_message:request()),
                last_options :: maybe(options())}).
-type state() :: #state{}.

-type options() :: #{atom() => term()}.

-type maybe(T) :: T | undefined.

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> mock().
start() -> start([]).

-spec start(result() | [result()]) -> mock().
start(Results) ->
    {ok, Mock} = gen_server:start(?MODULE, ensure_list(Results), []),
    Mock.

-spec stop(mock()) -> ok.
stop(Mock) ->
    gen_server:stop(Mock).

-spec handler(mock()) -> booze:handler().
handler(Mock) ->
    fun(Request, Options) ->
            case gen_server:call(Mock, {result, Request, Options}) of
                {value, {fail, Reason}} ->
                    {error, Reason};
                {value, {reply, Status}} ->
                    {ok, booze_message:response(Status)};
                {value, Response} ->
                    {ok, Response};
                empty ->
                    erlang:error({?MODULE, empty_queue})
            end
    end.

-spec last_request(mock()) -> maybe(booze_message:request()).
last_request(Mock) ->
    gen_server:call(Mock, last_request).

-spec last_method(mock()) -> maybe(iodata()).
last_method(Mock) ->
    with_last_request(fun booze_message:method/1, Mock).

-spec last_uri(mock()) -> maybe(iodata()).
last_uri(Mock) ->
    with_last_request(fun booze_message:uri/1, Mock).

-spec last_headers(mock()) -> maybe(booze_message:request_headers()).
last_headers(Mock) ->
    with_last_request(fun booze_message:headers/1, Mock).

-spec last_body(mock()) -> maybe(iodata()).
last_body(Mock) ->
    with_last_request(fun booze_message:body/1, Mock).

-spec last_options(mock()) -> maybe(options()).
last_options(Mock) ->
    gen_server:call(Mock, last_options).

-spec append(result() | [result()], mock()) -> ok.
append(Results, Mock) ->
    gen_server:cast(Mock, {append, ensure_list(Results)}).

-spec reset(mock()) -> ok.
reset(Mock) ->
    gen_server:cast(Mock, reset).

-spec count(mock()) -> non_neg_integer().
count(Mock) ->
    gen_server:call(Mock, count).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([result()]) -> {ok, state()}.
init(Results) ->
    Queue = queue:from_list(Results),
    {ok, #state{result_queue = Queue}}.

-spec handle_call(_, _, state()) -> {reply, _, state()}.
handle_call({result, Request, Options}, _From, State) ->
    {Reply, Queue} = queue:out(State#state.result_queue),
    {reply, Reply, #state{result_queue = Queue,
                          last_request = Request,
                          last_options = Options}};
handle_call(last_request, _From, State) ->
    Reply = State#state.last_request,
    {reply, Reply, State};
handle_call(last_options, _From, State) ->
    Reply = State#state.last_options,
    {reply, Reply, State};
handle_call(count, _From, State) ->
    Reply = queue:len(State#state.result_queue),
    {reply, Reply, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast({append, Results}, State) ->
    Queue = lists:foldl(fun queue:in/2, State#state.result_queue, Results),
    {noreply, State#state{result_queue = Queue}};
handle_cast(reset, State) ->
    Queue = queue:new(),
    {noreply, State#state{result_queue = Queue}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_list(term()) -> list().
ensure_list(T) when is_list(T) -> T;
ensure_list(T) -> [T].

-spec with_last_request(Fun, mock()) -> maybe(T) when
      Fun :: fun((booze_message:request()) -> T).
with_last_request(Fun, Mock) ->
    case last_request(Mock) of
        Request when Request =/= undefined ->
            Fun(Request);
        _ ->
            undefined
    end.
