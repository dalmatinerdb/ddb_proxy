%%%-------------------------------------------------------------------
%%% @author Heinz Nikolaus Gies <heinz@licenser.net>
%%% @copyright (C) 2016, Heinz Nikolaus Gies
%%% @doc
%%%
%%% @end
%%% Created :  7 Jun 2016 by Heinz Nikolaus Gies <heinz@licenser.net>
%%%-------------------------------------------------------------------
-module(dqe_proxy_prom_scraper).

-behaviour(gen_server).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([start_link/4]).

-define(SERVER, ?MODULE).

-record(state, {url :: string(),
                freq :: pos_integer(),
                bucket :: binary(),
                ddb}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Bucket, URL, Freq) ->
    gen_server:start_link(?MODULE, [Name, Bucket, URL, Freq], []).

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
init([Name, Bucket, URL, Freq]) ->
    lager:info("[prom:~s] Adding scraper on ~s with an interval of ~p",
               [Name, URL, Freq]),
    erlang:send_after(Freq, self(), scrape),
    {Host, Port} = dp_util:ddb_config(),
    C = dp_util:ddb_c(ddb_tcp:connect(Host,Port)),
    C1 = dp_util:ddb_c(ddb_tcp:stream_mode(Bucket, 5, C)),

    {ok, #state{bucket = Bucket, url = URL, freq = Freq, ddb = C1}}.

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

handle_info(scrape, State = #state{url = URL, freq = F}) ->
    State1 = case hackney:get(URL) of
                 {ok, 200, _Hdrs, Client} ->
                     {ok, Body} = hackney:body(Client),
                     L = binary:split(Body, [<<"\r\n">>, <<"\n">>], [global]),
                     Metrics = [dp_prometheus:parse(E) || E <- L],
                     Metrics2 = lists:flatten([M || {ok, M} <- Metrics]),
                     lager:info("scrape: ~s -> ~p~n", [URL, length(Metrics2)]),
                     lists:foldl(fun do_send/2, State, Metrics2);
                 _ ->
                     lager:error("scrape error on: ~s~n", [URL]),
                     State
             end,
    erlang:send_after(F, self(), scrape),
    {noreply, State1};

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
do_send(Decoded = #{time := Time, key := Key, value := Value},
        State = #state{bucket = Bucket, ddb = C}) ->
    KeyBin = dproto:metric_from_list(Key),
    Points = mmath_bin:from_list([Value]),
    C1 = dp_util:ddb_c(ddb_tcp:send(KeyBin, Time, Points, C)),
    dp_index:add(Bucket, Decoded),
    State#state{ddb = C1}.
