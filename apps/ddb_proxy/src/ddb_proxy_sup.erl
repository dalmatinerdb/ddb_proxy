%%%-------------------------------------------------------------------
%% @doc ddb_proxy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ddb_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(ACCEPTOR_COUNT, 100).

%%====================================================================
%% API functions
%%====================================================================

listener_name({Decoder, Bucket, Port, Protocol}) ->
    DecoderB = atom_to_binary(Decoder, utf8),
    PortB = integer_to_binary(Port),
    ProtocolB = atom_to_binary(Protocol, utf8),
    BName = <<DecoderB/binary, "_", Bucket/binary, "_",
              PortB/binary, "_", ProtocolB/binary>>,
    binary_to_atom(BName, utf8).

listener({Decoder, Bucket, Port, Protocol} = L)
  when is_atom(Decoder),
       is_binary(Bucket),
       is_integer(Port),
       Port > 0 ->
    Name  = listener_name(L),
    State = #{bucket => Bucket, decoder => Decoder,
              proto => Decoder:protocol()},
    lager:info("[listener:~s] Adding listener on bucket: ~p and port ~p~n",
                [Name, Bucket, Port]),
    start_listener(Protocol, Name, Port, State).

start_listener(tcp, Name, Port, State) ->
    Proto = dp_tcp_listener,
    {ok, _} = ranch:start_listener(Name, ?ACCEPTOR_COUNT,
                                   ranch_tcp, [{port, Port}],
                                   Proto, State),
    ok;
start_listener(http, Name, Port, State) ->
    Proto = dp_http_listener,
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", Proto, State}]}]),
    {ok, _} = cowboy:start_http(Name, ?ACCEPTOR_COUNT,
                                [{port, Port}],
                                [{env, [{dispatch, Dispatch}]},
                                 {max_keepalive, 5},
                                 {timeout, 50000}]),
    ok.

start_link() ->
    R = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    {ok, Listeners} = application:get_env(ddb_proxy, listeners),
    dqe_idx_pg:init(),
    [listener(L) || L <- Listeners],
    R.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = supervisor,
    Prom = {ddb_proxy_prom_sup,
            {ddb_proxy_prom_sup, start_link, []},
            Restart, Shutdown, Type, [ddb_proxy_prom_sup]},
    Idx = {dp_index,
           {dp_index, start_link, []},
           Restart, Shutdown, worker, [dp_index]},
    {ok, { {one_for_one, 0, 1}, [Prom, Idx]} }.

%%====================================================================
%% Internal functions
%%====================================================================
