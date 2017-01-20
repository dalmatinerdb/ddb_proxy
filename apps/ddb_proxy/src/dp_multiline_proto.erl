-module(dp_multiline_proto).

-export([send/3]).

-ignore_xref([send/3]).

send(Line, Acc, State) ->
    send1(<<Acc/binary, Line/binary>>, State).

send1(<<>>, State) ->
    {<<>>, State};

send1(Line, State = #{decoder := Decoder}) ->
    case Decoder:parse(Line) of
        {ok, Decoded, R} ->
            State1 = send_metrics(Decoded, State),
            send1(R, State1);
        {ok, Decoded} ->
            State1 = send_metrics(Decoded, State),
            send1(<<>>, State1);
        {incomplete, R} ->
            {R, State}
    end.

send_metrics([], State) ->
    State;
send_metrics([#{ time := T} | _ ] = Ms, State = #{ddb := C, res := R}) ->
    C1 = dp_util:ddb_c(ddb_tcp:batch_start(T, C)),
    send_metrics(Ms, T div R, [], State#{ddb => C1}).

send_metrics([M = #{time := Tin, key := Key, value := Value} | Ms],
             T, Acc, State = #{bucket := Bucket, res := R})
  when Tin div R =:= T ->
    dp_index:add(Bucket, M),
    KeyBin = dproto:metric_from_list(Key),
    Points = mmath_bin:from_list([Value]),
    send_metrics(Ms, T, [{KeyBin, Points} | Acc], State);
send_metrics([M = #{time := T, key := Key, value := Value} | Ms],
             _, Acc, State = #{ddb := C, bucket := Bucket, res := R}) ->
    %%lager:info("Batch size: ~p", [length(Acc)]),
    C1 = dp_util:ddb_c(ddb_tcp:batch(Acc, C)),
    C2 = dp_util:ddb_c(ddb_tcp:batch_end(C1)),
    C3 = dp_util:ddb_c(ddb_tcp:batch_start(T, C2)),
    KeyBin = dproto:metric_from_list(Key),
    Points = mmath_bin:from_list([Value]),
    dp_index:add(Bucket, M),
    send_metrics(Ms, T div R, [{KeyBin, Points}], State#{ddb => C3});
send_metrics([], _, Acc, State = #{ddb := C}) ->
    %%lager:info("Batch size: ~p", [length(Acc)]),
    C1 = dp_util:ddb_c(ddb_tcp:batch(Acc, C)),
    C2 = dp_util:ddb_c(ddb_tcp:batch_end(C1)),
    State#{ddb => C2}.
