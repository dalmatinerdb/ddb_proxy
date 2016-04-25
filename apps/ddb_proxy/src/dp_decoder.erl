-module(dp_decoder).

-export([recombine_tags/1, to_number/1, listener/1]).
-export_type([metric/0]).

-type metric() :: #{
              metric => [binary()],
              key => [binary()],
              tags => [{binary(), binary(), binary()}],
              time => pos_integer(),
              value => integer()
             }.

-type protocol() :: dp_line_proto.

-callback parse(In::binary()) ->
    dp_decoder:metric().

-callback protocol() ->
    dp_decoder:protocol().

recombine_tags(Tags) ->
    [<<K/binary, "=", V/binary>> || {_,K,V} <- Tags].

to_number(X) ->
    try
        F = binary_to_float(X),
        round(F)
    catch
        _:_ ->
            binary_to_integer(X)
    end.


listener({Decoder, Bucket, Port} = L)
  when is_atom(Decoder),
       is_binary(Bucket),
       is_integer(Port),
       Port > 0 ->
    Name  = listener_name(L),
    Proto = Decoder:protocol(),
    State = #{bucket => Bucket, decoder => Decoder},
    {ok, _} = ranch:start_listener(Name, 100,
                                   ranch_tcp, [{port, Port}],
                                   Proto, State),
    ok.

listener_name({Decoder, Bucket, Port}) ->
    DecoderB = atom_to_binary(Decoder, utf8),
    PortB = integer_to_binary(Port),
    BName = <<DecoderB/binary, "_", Bucket/binary, "_", PortB/binary>>,
    binary_to_atom(BName, utf8).
