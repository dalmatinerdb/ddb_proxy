-module(dp_line_proto).

-export([send/3]).

send(Acc, Line, State) ->
    fold_lines(Acc, Line, State).

fold_lines(<<"\r\n", R/binary>>, Line, State) ->
    State1 = decode_metrics(Line, State),
    fold_lines(R, <<>>, State1);
fold_lines(<<"\n", R/binary>>, Line, State) ->
    State1 = decode_metrics(Line, State),
    fold_lines(R, <<>>, State1);
fold_lines(<<>>, Line, State) ->
    {Line, State};
fold_lines(<<C, R/binary>>, Line, State) ->
    fold_lines(R, <<Line/binary, C>>, State).

decode_metrics(Line,  State = #{decoder := Decoder}) ->
    {ok, Decoded} = Decoder:parse(Line),
    lists:foldl(fun decode_metric/2, State, Decoded).

decode_metric(Metric,  State = #{bucket := Bucket,
                                  seen := Seen, ddb := C}) ->
    #{time := Time, key := Key, value := Value} = Metric,
    KeyBin = dproto:metric_from_list(Key),

    Points = mmath_bin:from_list([Value]),
    C1 = dp_util:ddb_c(ddb_tcp:send(KeyBin, Time, Points, C)),
    State1 = State#{ddb => C1},
    case gb_sets:is_element(KeyBin, Seen) of
        true ->
            State1;
        false ->
            #{metric := MetricParts, tags := Tags} = dp_util:expand_tags(Metric),
            MetricBin = dproto:metric_from_list(MetricParts),
            dqe_idx:add(Bucket, MetricBin, Bucket, KeyBin, Tags),
            State1#{seen => gb_sets:add_element(MetricBin, Seen)}
    end.
