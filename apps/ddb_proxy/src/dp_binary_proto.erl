-module(dp_binary_proto).

-export([send/3]).

send(Data, _, State) ->
    State1 = decode_metrics(Data, State),
    {<<>>, State1}.

decode_metrics(Line,  State = #{decoder := Decoder}) ->
    {ok, Decoded} = Decoder:parse(Line),
    lists:foldl(fun decode_metric/2, State, Decoded).

decode_metric(Metric,  State = #{bucket := Bucket, ddb := C,
                                 res := Res}) ->
    #{time := Time, key := Key, value := Value} = Metric,
    KeyBin = dproto:metric_from_list(Key),
    Points = mmath_bin:from_list([Value]),
    C1 = dp_util:ddb_c(ddb_tcp:send(KeyBin, Time div Res, Points, C)),
    dp_index:add(Bucket, Metric),
    State#{ddb => C1}.
