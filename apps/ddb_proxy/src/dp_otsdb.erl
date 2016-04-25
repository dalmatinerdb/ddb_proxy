-module(dp_otsdb).
-behaviour(dp_decoder).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1]).

-spec parse(In::binary()) ->
                   dp_decoder:metric().
parse(<<"put ", In/binary>>) ->
    M = #{
      metric => [],
      key => [],
      tags => [],
      time => 0,
      value => 0
     },
    parse_metric(In, <<>>, M).

parse_metric(<<" ", R/binary>>, Part,
             M = #{key := Ks}) ->
    Ks1 = [Part | Ks],
    M1 = M#{metric := lists:reverse(Ks1),
            key := Ks1},
    parse_time(R, <<>>, M1);

parse_metric(<<".", R/binary>>, Part,
             M = #{key := Ks}) ->
    M1 = M#{key := [Part | Ks]},
    parse_metric(R, <<>>, M1);

parse_metric(<<C, R/binary>>, Part, M) ->
    parse_metric(R, <<Part/binary, C>>, M).


parse_time(<<" ", R/binary>>, T, M) ->
    Ti = binary_to_integer(T),
    M1 = M#{time := Ti},
    parse_value(R, <<>>, M1);

parse_time(<<C, R/binary>>, Part, M) ->
    parse_time(R, <<Part/binary, C>>, M).

parse_value(<<" ", R/binary>>, V, M) ->
    Vi = binary_to_float(V),
    M1 = M#{value := round(Vi)},
    parse_tags(R, <<>>, M1);

parse_value(<<C, R/binary>>, Part, M) ->
    parse_value(R, <<Part/binary, C>>, M).


parse_tags(<<>>, Tag, M = #{tags := Tags}) ->
    {K, V} = parse_tag(Tag, <<>>),
    M#{tags := lists:sort([{<<"">>, K, V} | Tags])};

parse_tags(<<" ", R/binary>>, Tag, M = #{tags := Tags}) ->
    {K, V} = parse_tag(Tag, <<>>),
    M1 = M#{tags := [{<<"">>, K, V} | Tags]},
    parse_tags(R, <<>>, M1);

parse_tags(<<C, R/binary>>, Tag, M) ->
    parse_tags(R, <<Tag/binary, C>>, M).

parse_tag(<<"=", V/binary>>, K) ->
    {K, V};
parse_tag(<<C, R/binary>>, K) ->
    parse_tag(R, <<K/binary, C>>).






-ifdef(TEST).

example_test() ->
    In = <<"put sys.cpu.user 1356998400 42.5 host=webserver01 cpu=0">>,
    Metric = [<<"sys">>, <<"cpu">>, <<"user">>],
    Key = [<<"sys">>, <<"cpu">>, <<"user">>,<<"cpu=0">>,
           <<"host=webserver01">>],
    Tags = [{<<>>, <<"cpu">>, <<"0">>},
            {<<>>, <<"host">>, <<"webserver01">>}],
    Time = 1356998400,
    Value = 43, %% note we round here, since we don't have floats :(
    #{
       metric := RMetric,
       tags := RTags,
       time := RTime,
       value := RValue
     } = parse(In),
    ?assertEqual(Metric, RMetric),
    ?assertEqual(Tags, RTags),
    ?assertEqual(Time, RTime),
    ?assertEqual(Value, RValue).
-endif.

