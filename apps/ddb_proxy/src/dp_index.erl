%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dp_index).

-behaviour(gen_server).

%% API
-export([start_link/0, add/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/0]).

-define(SERVER, ?MODULE).

-record(state, {
          seen = btrie:new(),
          last_seen_update = 10*60 % 10m
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------x
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add(Bucket, Metric, Time) ->
    case erlang:process_info(whereis(?SERVER), message_queue_len) of
        {message_queue_len, N} when N > 100 ->
            gen_server:call(?SERVER, {tags, Bucket, Metric, Time});
        _ ->
            gen_server:cast(?SERVER, {tags, Bucket, Metric, Time})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({tags, Bucket, Metric, Time}, _From, State) ->
    State1 = do_add(Bucket, Metric, Time, State),
    {reply, ok, State1};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({tags, Bucket, Metric, Time}, State) ->
    State1 = do_add(Bucket, Metric, Time, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_add(Bucket, Metric = #{key := Key}, Time, State = #state{seen = Seen, last_seen_update = TTL}) ->
    KeyBin = dproto:metric_from_list(Key),
    K = <<Bucket/binary, 0,  KeyBin/binary>>,
    case btrie:find(K, Seen) of
        {ok, Last} when Time - Last < TTL ->
            State;
        {ok, _Last} ->
            dqe_idx:touch([{Bucket, Key, Time}]),
            State#state{seen = btrie:store(K, Time, Seen)};
        error ->
            #{metric := MetricParts, tags := Tags} =
                dp_util:expand_tags(Metric),
            dqe_idx:add(Bucket, MetricParts, Bucket, Key, Time, Tags),
            State#state{seen = btrie:store(K, Time, Seen)}
    end.
