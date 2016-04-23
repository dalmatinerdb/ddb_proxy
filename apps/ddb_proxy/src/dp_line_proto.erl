-module(dp_line_proto).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, M = #{bucket := Bucket}) ->
    Bucket = Bucket,
    ok = ranch:accept_ack(Ref),
    {Host, Port} = dp_util:ddb_config(),
    C = dp_util:ddb_c(ddb_tcp:connect(Host,Port)),
    C1 = dp_util:ddb_c(ddb_tcp:stream_mode(Bucket, 5, C)),
    loop(Socket, Transport, <<>>, M#{ddb => C1, seen => gb_sets:new()}).

loop(Socket, Transport, Acc, State) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Acc1 = <<Data/binary, Acc/binary>>,
            {Acc2, State1} = fold_lines(Acc1, <<>>, State),
            loop(Socket, Transport, Acc2, State1);
        _ ->
            ok = Transport:close(Socket)
    end.

fold_lines(<<"\r\n", R/binary>>, Line, State) ->
    State1 = decode_metric(Line, State),
    fold_lines(R, <<>>, State1);
fold_lines(<<"\n", R/binary>>, Line, State) ->
    State1 = decode_metric(Line, State),
    fold_lines(R, <<>>, State1);
fold_lines(<<>>, Line, State) ->
    {Line, State};
fold_lines(<<C, R/binary>>, Line, State) ->
    fold_lines(R, <<Line/binary, C>>, State).


decode_metric(Line,  State = #{bucket := Bucket, decoder := Decoder,
                               seen := Seen, ddb := C}) ->
    Decoded = Decoder:parse(Line),
    Decoded1 = dp_util:expand_tags(Decoded),
    #{ metric := Metric, time := Time,
       value := Value, tags := Tags} = Decoded1,
    MetricBin = dproto:metric_from_list(Metric),
    Points = mmath_bin:from_list([Value]),
    C1 = dp_util:ddb_c(ddb_tcp:send(Metric, Time, Points, C)),
    State1 = State#{ddb => C1},
    %% TODO: store!
    case gb_sets:is_element(MetricBin, Seen) of
        true ->
            io:format("we know: ~p", [Metric]),
            State1;
        false ->
            dqe_idx:add(Bucket, <<"metric">>, Bucket, MetricBin, Tags),
            io:format("New: ~p~n", [Decoded1]),
            %% TODO: Store metrics
            State1#{seen => gb_sets:add_element(MetricBin, Seen)}
    end.
