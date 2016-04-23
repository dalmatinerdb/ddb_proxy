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

start_link() ->
    R = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    {ok, _} = ranch:start_listener(dp_metrics2, 100,
                                   ranch_tcp, [{port, 5555}],
                                   dp_line_proto, #{bucket => <<"bucket">>,
                                                    decoder => dp_metrics2}),
    R.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, []} }.

%%====================================================================
%% Internal functions
%%====================================================================
