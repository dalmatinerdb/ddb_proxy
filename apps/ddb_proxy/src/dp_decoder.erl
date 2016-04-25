-module(dp_decoder).

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
