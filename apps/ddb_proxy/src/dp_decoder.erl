-module(dp_decoder).

-export([recombine_tags/1, to_number/1]).
-export_type([metric/0]).

-type metric() :: #{
              metric => [binary()],
              key => [binary()],
              tags => [{binary(), binary(), binary()}],
              time => pos_integer(),
              value => integer()
             }.

-callback parse(In::binary()) ->
    dp_decoder:metric().

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
