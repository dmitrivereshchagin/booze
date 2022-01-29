-module(booze_json_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test cases
%%%===================================================================

encode_test() ->
    ?assertEqual(<<"{\"answer\":42}">>,
                 booze_json:encode(#{answer => 42})).

decode_test() ->
    ?assertEqual({ok, #{<<"answer">> => 42}},
                 booze_json:decode(<<"{\"answer\":42}">>)).

fail_to_decode_test() ->
    ?assertEqual(error, booze_json:decode(<<>>)).
