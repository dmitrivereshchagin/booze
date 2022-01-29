-module(booze_json).

-export([encode/1,
         decode/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type json_term() :: jsx:json_term().
-export_type([json_term/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec encode(json_term()) -> binary().
encode(Term) ->
    jsx:encode(Term).

-spec decode(binary()) -> {ok, json_term()} | error.
decode(Binary) ->
    try
        {ok, jsx:decode(Binary)}
    catch
        error:badarg ->
            error
    end.
