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
    {ok, Parsed1} = parse("<30>Dec 13 19:40:50 thttpd[1340]: 192.168.1.138 - "
                          "admin \"GET /cgi-bin/Qdownload/html/1260751250.rcsv "
                          "HTTP/1.1\" 200 138 "
                          "\"http://illmatic:8080/cgi-bin/Qdownload/html/"
                          "rlist.html\" \"Mozilla/5.0 (Macintosh; U; Intel Mac "
                          "OS X 10.6; en-US; rv:1.9.1.5) Gecko/2009110\""),

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
    {ok, Parsed2} = parse("<30>Dec 13 19:41:03 thttpd[1340]: spawned CGI "
                          "process 24156 for file "
                          "'cgi-bin/Qdownload/refresh.cgi'"),

    Expected3 = #{
      priority => 147,
      facility => local2,
      severity => err,
      timestamp => to_nano(11, 18, 19, 17, 55),
      host => <<"myhost">>,
      tag => <<"mytag[909]">>,
      body => <<"yo what's really real">>
     },
    {ok, Parsed3} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo what's "
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
    {ok, Parsed4} = parse("<147>Nov 18 19:17:55 myhost mytag[909]: yo"),

    Expected5 = #{
      priority => 12,
      facility => user,
      severity => warn,
      timestamp => to_nano(9, 19, 3, 55, 25),
      host => <<"Schroedinger">>,
      tag => <<"com.apple.xpc.launchd[1] (com.adobe.ARMDCHelper."
               "cc24aef4a1b90ed56a725c38014c95072f92651fb65e1bf9c"
               "8e43c37a23d420d[60845])">>,
      body => <<"Service exited with abnormal code:111\n ">>
     },
    {ok, Parsed5} = parse("<12>Sep 19 03:55:25 Schroedinger com.apple.xpc."
                          "launchd[1] (com.adobe.ARMDCHelper.cc24aef4a1b90ed56"
                          "a725c38014c95072f92651fb65e1bf9c8e43c37a23d420d"
                          "[60845]): Service exited with abnormal code: 111\n"),

    Expected6 = #{
      priority => 5,
      facility => kern,
      severity => notice,
      timestamp => to_nano(9, 19, 4, 2, 59),
      host => <<"Schroedinger">>,
      tag => <<"sandboxd[155] ([531])">>,
      body => <<"Paste(531) deny file-read-data "
                "/Applications/Emacs.app/Contents/PkgInfo">>
     },
    {ok, Parsed6} = parse("<5>Sep 19 04:02:59 Schroedinger sandboxd[155] "
                          "([531]): Paste(531) deny file-read-data "
                          "/Applications/Emacs.app/Contents/PkgInfo\n"),

    ?assertEqual(Expected1, Parsed1),
    ?assertEqual(Expected2, Parsed2),
    ?assertEqual(Expected3, Parsed3),
    ?assertEqual(Expected4, Parsed4),
    ?assertEqual(Expected5, Parsed5),
    ?assertEqual(Expected6, Parsed6),
    ?assertEqual(bad_message, parse("asdf")).

-endif.
