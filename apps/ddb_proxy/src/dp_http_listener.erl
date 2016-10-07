-module(dp_http_listener).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-ignore_xref([init/3, handle/2, terminate/3]).

init(_Transport, Req, State = #{bucket := Bucket}) ->
    {Host, Port} = dp_util:ddb_config(),
    C = dp_util:ddb_c(ddb_tcp:connect(Host, Port)),
    {ok, {Res, _, _}, C1} = ddb_tcp:bucket_info(Bucket, C),
    Res1 = Res div 1000,
    C2 = dp_util:ddb_c(ddb_tcp:stream_mode(Bucket, 5 , C1)),
    {ok, Req, State#{ddb => C2, res => Res1}}.

-dialyzer({no_opaque, handle/2}).
handle(Req, State = #{proto := Proto}) ->
    case cowboy_req:body_length(Req) of
        {_L, Req1} when _L > 0 ->
            {ok, Body, Req2} = cowboy_req:body(Req1),
            {_Acc, State1} = Proto:send(Body, <<>>, State),
            {ok, Req2, State1};
        %% This is dirty - some output plugins such as telegraf expect to be
        %% able to create a database.  The success result is a lie.
        {_, Req1} ->
            ReqX =
                case cowboy_req:qs(Req1) of
                    {<<"q=show%20databases">>, Req2} ->
                        S = #{name => <<"databases">>,
                              columns => [<<"name">>],
                              values => []},
                        D = #{results =>
                                  [#{series => [S]}]
                             },
                        json_reply(D, Req2);
                    {S, Req2} ->
                        lager:info("Query: ~s", [S]),
                        D = [{<<"results">>, [#{}]}],
                        json_reply(D, Req2)
                end,
            {ok, ReqX, State}
    end.

terminate(_Reason, _Req, _State = #{ddb := C}) ->
    ddb_tcp:close(C),
    ok.

json_reply(JSON, Req) ->
    {ok, Req1} =
        cowboy_req:reply(
          200, [{<<"content-type">>, <<"application/json">>}],
          jsone:encode(JSON), Req),
    Req1.
