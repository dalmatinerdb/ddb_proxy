-module(dp_prom_writer).

-export([protocol/0, parse/1]).
-include("remote_pb.hrl").

parse(Data) ->
    Uncompressed = unsnap(Data),
    Msg = remote_pb:decode_msg(Uncompressed, 'WriteRequest'),

    Metrics = lists:flatten([decode(TS) || TS <- Msg#'WriteRequest'.timeseries]),

    {ok, Metrics}.

protocol() ->
    dp_binary_proto.


unsnap(<<255,6,0,0, "sNaPpY", Data/binary>>) ->
    << << (unsnap_chunk(T, E))/binary >> ||
        <<T, S:24/little-unsigned-integer, E:S/binary >> <= Data >>;

unsnap(Compressed) ->
    {ok, C} = snappiest:decompress(Compressed),
    C.

-define(SNAPPY_CHUNK, 16#00).
-define(UNCOMPRESSED_CHUNK, 16#01).
-define(PADDING_CHUNK, 16#fe).

-define(UNSKIPPLE_START, 16#02).
-define(UNSKIPPLE_END, 16#7f).

-define(SKIPPLE_START, 16#80).
-define(SKIPPLE_END, 16#fd).

unsnap_chunk(?SNAPPY_CHUNK, <<_CrC:32, Compressed/binary>>) ->
    {ok, C} = snappiest:decompress(Compressed),
    C;
unsnap_chunk(?UNCOMPRESSED_CHUNK, D) ->
    D;
unsnap_chunk(?PADDING_CHUNK, _) ->
    <<>>;
unsnap_chunk(T, _) when T >= ?SKIPPLE_START,
                           T =< ?SKIPPLE_END ->
    <<>>;
unsnap_chunk(T, _) when T >= ?UNSKIPPLE_START,
                        T =< ?UNSKIPPLE_END ->
    error(badarg).

decode(#'TimeSeries'{labels = Labels, samples = Samples}) ->
    Tags = lists:sort([decode_label(Label) || Label <- Labels]),
    Metric = get_name(Tags),
    Key = make_key(Tags),
    Base = #{metric => Metric, key => Key, tags => Tags},
    apply_samples(Base, Samples).


decode_label(#'LabelPair'{name = Name, value = Value}) ->
    {<<>>, Name, Value}.

get_name(Tags) ->
    {_, _, Name} = lists:keyfind(<<"__name__">>, 2, Tags),
    [Name].

make_key(Tags) ->
    [<<N/binary, $=, V/binary>> || {_, N, V} <- Tags].


apply_samples(Base, Samples) ->
    [Base#{value => V, time => T div 1000} ||
        #'Sample'{value = V, timestamp_ms = T} <- Samples,
        is_number(V)].
