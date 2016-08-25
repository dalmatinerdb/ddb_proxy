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

%%====================================================================
%% API functions
%%====================================================================

listener_name({Decoder, Bucket, Port}) ->
    DecoderB = atom_to_binary(Decoder, utf8),
    PortB = integer_to_binary(Port),
    BName = <<DecoderB/binary, "_", Bucket/binary, "_", PortB/binary>>,
    binary_to_atom(BName, utf8).

listener({Decoder, Bucket, Port} = L)
  when is_atom(Decoder),
       is_binary(Bucket),
       is_integer(Port),
       Port > 0 ->
    Name  = listener_name(L),
    Proto = dp_line_proto,
    State = #{bucket => Bucket, decoder => Decoder},
    lager:info("[listener:~s] Adding listener on bucket: ~p and port ~p~n",
		[Name, Bucket, Port]),
    {ok, _} = ranch:start_listener(Name, 100,
                                   ranch_tcp, [{port, Port}],
                                   Proto, State),
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
    C = {ddb_proxy_prom_sup,
         {ddb_proxy_prom_sup, start_link, []},
         Restart, Shutdown, Type, [ddb_proxy_prom_sup]},
    {ok, { {one_for_all, 0, 1}, [C]} }.

%%====================================================================
%% Internal functions
%%====================================================================
