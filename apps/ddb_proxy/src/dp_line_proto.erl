-module(dp_line_proto).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, M = #{bucket := Bucket}) ->
    io:format("Starting line protocol ~s for bucket ~s~n",
              [Transport, Bucket]),
    ok = ranch:accept_ack(Ref),
    {Host, Port} = dp_util:ddb_config(),
    C = dp_util:ddb_c(ddb_tcp:connect(Host,Port)),
    C1 = dp_util:ddb_c(ddb_tcp:stream_mode(Bucket, 5, C)),
    loop(Socket, Transport, <<>>,
         M#{count => 0, ddb => C1, seen => btrie:new()}).

loop(Socket, Transport, Acc, State) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Acc1 = <<Acc/binary, Data/binary>>,
            {Acc2, State1} = fold_lines(Acc1, <<>>, State),
            loop(Socket, Transport, Acc2, State1);
        {error, timeout} ->
            loop(Socket, Transport, Acc, State);
        _ ->
            ok = Transport:close(Socket)
    end.

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

decode_metrics(Line,  State = #{decoder := Decoder, count := Count}) ->
    {ok, Decoded} = Decoder:parse(Line),
    Count1 = Count + length(Decoded),
    _State1 = State#{count => Count1}.%,
    %%lists:foldl(fun decode_metric/2, State1, Decoded).


%% decode_metric(Metric,  State = #{bucket := Bucket,
%%                                  seen := Seen, ddb := C}) ->
%%     #{time := Time, key := Key, value := Value} = Metric,
%%     KeyBin = dproto:metric_from_list(Key),
%%     Points = mmath_bin:from_list([Value]),
%%     C1 = dp_util:ddb_c(ddb_tcp:send(KeyBin, Time, Points, C)),
%%     State1 = State#{ddb => C1},
%%     case btrie:is_key(KeyBin, Seen) of
%%         true ->
%%             State1;
%%         false ->
%%             #{metric := MetricParts, tags := Tags} =
%%                 dp_util:expand_tags(Metric),
%%             MetricBin = dproto:metric_from_list(MetricParts),
%%             dqe_idx:add(Bucket, MetricBin, Bucket, KeyBin, Tags),
%%             State1#{seen => btrie:store(KeyBin, Seen)}
%%     end.
