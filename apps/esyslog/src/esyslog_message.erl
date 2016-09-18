-module(esyslog_message).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([parse/1, format/1]).
-export_types([message/0, facility/0, severity/0]).

%% @type priority() = integer().
%% An integer encoding the facility and priority of a syslog message.
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>
-type priority() :: 0..999.

%% @type facility() = integer().
%% An integer between 0 and 23 inclusive that indicates the syslog facility.
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>
-type facility() ::
        kern |
        user |
        mail |
        system |
        auth |
        internal |
        lpr |
        nns |
        uucp |
        clock |
        authpriv |
        ftp |
        ntp |
        audit |
        alert |
        clock2 |
        local0 |
        local1 |
        local2 |
        local3 |
        local4 |
        local5 |
        local6 |
        local7 |
        undefined.

%% @type severity() = integer().
%% An integer between 0 and 7 inclusive that indicates the syslog severity.
%% Defined in <a href="http://tools.ietf.org/html/rfc3164#section-4.1.1">RFC3164</a>
-type severity() ::
        emerg |
        alert |
        crit |
        err |
        warn |
        notice |
        info |
        debug |
        undefined.
%% @type msg() = {Priority, DateTime, Host, Tag, Body}
%%      Priority = priority(),
%%      DateTime = datetime(),
%%      Host     = string(),
%%      Tag      = string(),
%%      Body     = string().

-type message() ::
        #{
           priority => priority(),
           facility => facility(),
           severity => severity(),
           timestamp => pos_integer(),
           host => binary(),
           tag => binary(),
           body => binary()
         }.


%% @spec parse(Message::string()) -> msg() | bad_message
%% @doc Parses a string into a syslog message tuple
-spec parse(string() | binary()) ->
                   message() |
                   bad_message.

parse(Message) when is_binary(Message) ->
    parse(binary_to_list(Message));

parse(Message) when is_list(Message) ->
    try
        {ok, Tokens, _} = esyslog_message_lexer:string(Message),
        {ok, ParsedMessage} = esyslog_message_parser:parse(Tokens),
        ParsedMessage
    catch error:{badmatch, Error} ->
            io:format("Couldn't parse: ~p~n~p~n", [Message, Error]),
            bad_message
    end.

%% @spec format(Msg::msg()) -> string()
%% @doc Pretty-print a syslog message tuple
format({Priority, Timestamp, Host, Tag, Body}) ->
    string:join([httpd_util:rfc1123_date(Timestamp), integer_to_list(Priority), Host, Tag, Body], " ").

-ifdef(TEST).

to_nano(Month, Day, Hour, Minute, Second) ->
    {{Year, _, _}, _} = erlang:localtime(),
    Local = {{Year, Month, Day}, {Hour, Minute, Second}},
    Universal = erlang:localtime_to_universaltime(Local),
    Seconds = erlang:universaltime_to_posixtime(Universal),
    erlang:convert_time_unit(Seconds, seconds, nano_seconds).

parse_test() ->
    Expected1 = #{
      priority => 30,
      facility => system,
      severity => info,
      timestamp => to_nano(12, 13, 19, 40, 50),
      host => <<"localhost">>, % host has been left off, so assume localhost
      tag => <<"thttpd[1340]">>,
      body => <<"192.168.1.138 - admin \"GET /cgi-bin/Qdownload/html/"
                "1260751250.rcsv HTTP/1.1\" 200 138 \"http://illmatic:8080/"
                "cgi-bin/Qdownload/html/rlist.html\" \"Mozilla/5.0 (Macintosh;"
                " U; Intel Mac OS X 10.6; en-US; rv:1.9.1.5) Gecko/2009110\"">>
     },
    Parsed1 = parse("<30>Dec 13 19:40:50 thttpd[1340]: 192.168.1.138 - admin "
                    "\"GET /cgi-bin/Qdownload/html/1260751250.rcsv HTTP/1.1\" "
                    "200 138 \"http://illmatic:8080/cgi-bin/Qdownload/html/"
                    "rlist.html\" \"Mozilla/5.0 (Macintosh; U; Intel Mac OS X "
                    "10.6; en-US; rv:1.9.1.5) Gecko/2009110\""),

    Expected2 = #{
      priority => 30,
      facility => system,
      severity => info,
      timestamp => to_nano(12, 13, 19, 41, 03),
      host => <<"localhost">>, % host has been left off, so assume localhost
      tag => <<"thttpd[1340]">>,
      body => <<"spawned CGI process 24156 for file "
                "'cgi-bin/Qdownload/refresh.cgi'">>
     },
    Parsed2 = parse("<30>Dec 13 19:41:03 thttpd[1340]: spawned CGI process "
                    "24156 for file 'cgi-bin/Qdownload/refresh.cgi'"),

    Expected3 = #{
      priority => 147,
      facility => local2,
      severity => err,
      timestamp => to_nano(11, 18, 19, 17, 55),
      host => <<"myhost">>,
      tag => <<"mytag[909]">>,
      body => <<"yo what's really real">>
     },
    Parsed3 = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's "
                    "really real"),

    Expected4 = #{
      priority => 147,
      facility => local2,
      severity => err,
      timestamp => to_nano(11, 18, 19, 17, 55),
      host => <<"myhost">>,
      tag => <<"mytag[909]">>,
      body => <<"yo">>
     },
    Parsed4 = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo"),

    %%{4,
    %% {{Year, 12, 20}, {16, 27, 32}},
    %% "ccabanilla-mac",
    %% "com.apple.launchd.peruser.501[522] (org.apache.couchdb[59972])",
    %% "Exited with exit code: 1"} = parse("<4>Dec 20 16:27:32 ccabanilla-mac com.apple.launchd.peruser.501[522] (org.apache.couchdb[59972]): Exited with exit code: 1"),

    %%{5,
    %% {{Year, 12, 20}, {16, 27, 32}},
    %% "ccabanilla-mac",
    %% "[0x0-0x99099].com.fluidapp.FluidInstance.Gmail[32480]",
    %% "Sun Dec 20 16:27:32 ccabanilla-mac FluidInstance[32480] <Error>: kCGErrorIllegalArgument: CGSGetWindowBounds: NULL window"} = parse("<5>Dec 20 16:27:32 ccabanilla-mac [0x0-0x99099].com.fluidapp.FluidInstance.Gmail[32480]: Sun Dec 20 16:27:32 ccabanilla-mac FluidInstance[32480] <Error>: kCGErrorIllegalArgument: CGSGetWindowBounds: NULL window"),

    Expected5 = bad_message,
    Parsed5 = parse("asdf"),

    ?assertEqual(Expected1, Parsed1),
    ?assertEqual(Expected2, Parsed2),
    ?assertEqual(Expected3, Parsed3),
    ?assertEqual(Expected4, Parsed4),
    ?assertEqual(Expected5, Parsed5).

-endif.
