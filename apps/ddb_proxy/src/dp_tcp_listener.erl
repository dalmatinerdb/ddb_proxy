-module(dp_tcp_listener).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, M = #{bucket := Bucket}) ->
    ok = ranch:accept_ack(Ref),
    {Host, Port} = dp_util:ddb_config(),
    C = dp_util:ddb_c(ddb_tcp:connect(Host,Port)),
    C1 = dp_util:ddb_c(ddb_tcp:stream_mode(Bucket, 5, C)),
    loop(Socket, Transport, <<>>, M#{ddb => C1, seen => gb_sets:new()}).

loop(Socket, Transport, Acc, State) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, Data} ->
            Acc1 = <<Acc/binary, Data/binary>>,
            {Acc2, State1} = dp_line_proto:send(Acc1, <<>>, State),
            loop(Socket, Transport, Acc2, State1);
        {error, timeout} ->
            loop(Socket, Transport, Acc, State);
        _ ->
            ok = Transport:close(Socket)
    end.
