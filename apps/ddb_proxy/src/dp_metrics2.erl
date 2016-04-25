-module(dp_metrics2).
-behaviour(dp_decoder).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([protocol/0, parse/1]).

-spec parse(In::binary()) ->
                   dp_decoder:metric().
parse(In) ->
    M = #{
      metric => [<<"metric">>],
      key => [],
      tags => [],
      time => 1,
      value => 0
     },
    parse_key(In, <<>>, M).

-spec protocol() -> dp_line_proto.
protocol() ->
    dp_line_proto.

-spec parse_key(binary(), binary(), db_decoder:metric()) ->
                       db_decoder:metric().
parse_key(<<" ", R/binary>>, Tag,
             M = #{key := Ms, tags := Tags}) ->
    {K, V} = parse_tag(Tag, <<>>),
    M1 = M#{tags := [{<<>>, K, V} | Tags],
            key := lists:sort([Tag | Ms])},
    case R of
        <<" ", R1/binary>> ->
            parse_metadata(R1, <<>>, M1);
        _ ->
            parse_key(R, <<>>, M1)
    end;
parse_key(<<C, R/binary>>, Tag, M) ->
    parse_key(R, <<Tag/binary, C>>, M).

-spec parse_metadata(binary(), binary(), db_decoder:metric()) ->
                            db_decoder:metric().
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

-spec parse_tag(binary(), binary()) ->
                       {binary(), binary()}.
parse_tag(<<"=", V/binary>>, K) ->
    {K, V};
parse_tag(<<C, R/binary>>, K) ->
    parse_tag(R, <<K/binary, C>>).



-spec parse_time(binary(), binary(), db_decoder:metric()) ->
                            db_decoder:metric().
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
    Metric = [<<"metric">>],
    Key = [<<"mountpoint=/srv/node/dfs3">>,<<"server=dfs4">>,
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
       key := RKey,
       tags := RTags,
       time := RTime,
       value := RValue
     } = parse(In),
    ?assertEqual(Key, RKey),
    ?assertEqual(Metric, RMetric),
    ?assertEqual(Tags, RTags),
    ?assertEqual(Time, RTime),
    ?assertEqual(Value, RValue).
-endif.

