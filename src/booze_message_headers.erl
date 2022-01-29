-module(booze_message_headers).

-export([has/2,
         values/2,
         set/3,
         add/3,
         remove/2]).

%%%===================================================================
%%% Types
%%%===================================================================

-type headers(Value) :: [{binary(), Value}].
-export_type([headers/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec has(binary(), headers(_)) -> boolean().
has(Name, Headers) ->
    lists:any(fun({N, _}) -> equal_names(Name, N) end, Headers).

-spec values(binary(), headers(Value)) -> [Value].
values(Name, Headers) ->
    lists:filtermap(
      fun({N, V}) ->
              case equal_names(Name, N) of
                  true ->
                      {true, V};
                  false ->
                      false
              end
      end,
      Headers).

-spec set(binary(), Value, headers(Value)) -> headers(Value).
set(Name, Value, Headers) ->
    add(Name, Value, remove(Name, Headers)).

-spec add(binary(), Value, headers(Value)) -> headers(Value).
add(Name, Value, Headers) ->
    Headers ++ [{Name, Value}].

-spec remove(binary(), headers(Value)) -> headers(Value).
remove(Name, Headers) ->
    lists:filter(fun({N, _}) -> not equal_names(Name, N) end, Headers).

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec equal_names(binary(), binary()) -> boolean().
equal_names(Name1, Name2) ->
    string:equal(Name1, Name2, true).
