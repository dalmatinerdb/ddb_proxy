-module(dp_metrics2).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1]).

-type metric() :: #{
              metric => [binary()],
              tags => [{binary(), binary(), binary()}],
              time => pos_integer(),
              value => integer()
             }.

-spec parse(binary()) -> metric().
parse(In) ->
    M = #{
      metric => [],
      tags => [],
      time => 0,
      value => 0
     },
    parse_metric(In, <<>>, M).

parse_metric(<<" ", R/binary>>, Tag,
             M = #{metric := Ms, tags := Tags}) ->
    {K, V} = parse_tag(Tag, <<>>),
    M1 = M#{tags := [{<<>>, K, V} | Tags],
            metric := lists:sort([Tag | Ms])},
    case R of
        <<" ", R1/binary>> ->
            parse_metadata(R1, <<>>, M1);
        _ ->
            parse_metric(R, <<>>, M1)
    end;
parse_metric(<<C, R/binary>>, Tag, M) ->
    parse_metric(R, <<Tag/binary, C>>, M).

parse_metadata(<<" ", R/binary>>, Tag, 
               M = #{tags := Tags}) ->
    {K, V} = parse_tag(Tag, <<>>),
    M1 = M#{tags := lists:sort([{<<"metadata">>, K, V} | Tags])},
    case R of
        <<X, R1/binary>> when X >= $0, X =< $9 ->
            parse_time(R1, <<X>>, M1);
        _ ->
            parse_metadata(R, <<>>, M1)
    end;
parse_metadata(<<C, R/binary>>, Tag, M) ->
    parse_metadata(R, <<Tag/binary, C>>, M).

parse_tag(<<"=", V/binary>>, K) ->
    {K, V};
parse_tag(<<C, R/binary>>, K) ->
    parse_tag(R, <<K/binary, C>>).



parse_time(<<" ", T/binary>>, V, M) ->
    Vi = binary_to_integer(V),
    Ti = binary_to_integer(T),
    M#{time := Ti, value := Vi};

parse_time(<<C, R/binary>>, V, M) ->
    parse_time(R, <<V/binary, C>>, M).


-ifdef(TEST).

example_test() ->
    In = <<"mountpoint=/srv/node/dfs3 what=disk_space server=dfs4",
           " target_type=gauge type=used unit=B  agent=diamond2",
           " 48929424224 1234567890">>,
    Metric = [<<"mountpoint=/srv/node/dfs3">>,<<"server=dfs4">>,
              <<"target_type=gauge">>,<<"type=used">>,<<"unit=B">>,
              <<"what=disk_space">>],
    Tags = [{<<>>, <<"mountpoint">>, <<"/srv/node/dfs3">>},
            {<<>>, <<"server">>, <<"dfs4">>},
            {<<>>, <<"target_type">>, <<"gauge">>},
            {<<>>, <<"type">>, <<"used">>},
            {<<>>, <<"unit">>, <<"B">>},
            {<<>>, <<"what">>, <<"disk_space">>},
            {<<"metadata">>, <<"agent">>, <<"diamond2">>}],
    Time = 1234567890,
    Value = 48929424224,
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

