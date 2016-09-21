-module(dp_util).

-export([expand_tags/1, ddb_config/0, ddb_c/1]).

expand_tags(M = #{tags := Tags, key := Metric}) ->
    L = {<<"ddb">>, <<"key_length">>,
         integer_to_binary(length(Metric))},
    M#{tags => add_tags(1, Metric, [L | Tags])}.

add_tags(_, [], Tags) ->
    Tags;
add_tags(N, [E | R], Tags) ->
    PosBin = integer_to_binary(N),
    T = {<<"ddb">>, <<"part_", PosBin/binary>>, E},
    add_tags(N + 1, R, [T | Tags]).


ddb_config() ->
    application:get_env(ddb_connection, backend, {"127.0.0.1", 5555}).

ddb_c({ok, C}) ->
    C;
ddb_c({error, _, C}) ->
    C.
