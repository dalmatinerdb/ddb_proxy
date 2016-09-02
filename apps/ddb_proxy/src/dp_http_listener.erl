-module(dp_http_listener).
-behaviour(cowboy_http_handler).

%% -export([init/3, handle/2, terminate/3]).
-compile(export_all).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, State = #{bucket := Bucket}) ->
    {Host, Port} = dp_util:ddb_config(),
    C = dp_util:ddb_c(ddb_tcp:connect(Host, Port)),
    C1 = dp_util:ddb_c(ddb_tcp:stream_mode(Bucket, 5, C)),
    {ok, Req, State#{ddb => C1, seen => gb_sets:new()}}.

-dialyzer({no_opaque, handle/2}).
handle(Req, State) ->
    case cowboy_req:body_length(Req) of
        {_L, Req1} when _L > 0 ->
            {ok, Body, Req2} = cowboy_req:body(Req1),
            lager:info("Body: ~p~n", [Body]),
            {_Acc, State1} = dp_line_proto:send(Body, <<>>, State),
            {ok, Req2, State1};
        %% This is dirty - some output plugins such as telegraf expect to be
        %% able to create a database.  The success result is a lie.
        {_, Req1} ->
            D = [{<<"results">>, [#{}]}],
            {ok, Req2} = cowboy_req:reply(
                           200, [{<<"content-type">>, <<"application/json">>}],
                           jsone:encode(D), Req1),
            {ok, Req2, State}
    end.

terminate(_Reason, _Req, _State = #{ddb := C}) ->
    ddb_tcp:close(C),
    ok.
