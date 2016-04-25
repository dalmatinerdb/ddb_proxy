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
    Dflt = [{dp_metrics2, <<"bucket">>, 5555}],
    Listeners = applicaiton:get_env(ddb_proxy, listeners, Dflt),
    [dp_decoder:listener(L) || L <- Listeners],
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
