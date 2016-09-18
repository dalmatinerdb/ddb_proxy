Nonterminals
message header body any priority_month day hour minute second host tag .

Terminals
word ':'.

Rootsymbol message.

message -> header ':' body : cleanup('$1', cleanup_body('$3')).

header -> priority_month day hour ':' minute ':' second host tag
              : cleanup_header({'$1', '$2', '$3', '$5', '$7', '$8', '$9'}).
header -> priority_month day hour ':' minute ':' second tag
              : cleanup_header({'$1', '$2', '$3', '$5', '$7', '$8'}).

body -> any      : ['$1'].
body -> any body : lists:append(['$1'], '$2').

any -> word : '$1'.
any -> ':' : '$1'.

priority_month -> word : '$1'.
day -> word : '$1'.
hour -> word : '$1'.
minute -> word : '$1'.
second -> word : '$1'.
host -> word : '$1'.
tag -> word : '$1'.


Erlang code.

cleanup_header({{word, PriorityMonth}, {word, Day}, {word, Hour}, {word, Minute}, {word, Second}, {word, Host}, {word, Tag}}) ->
    [PriorityStr, MonthStr] = string:tokens(PriorityMonth, "<>"),

    Priority = list_to_integer(PriorityStr),
    {Facility, Severity} = decode_priority(Priority),

    {{Year, _, _}, _} = erlang:localtime(),

    Month = case MonthStr of
                "Jan" -> 1;
                "Feb" -> 2;
                "Mar" -> 3;
                "Apr" -> 4;
                "May" -> 5;
                "Jun" -> 6;
                "Jul" -> 7;
                "Aug" -> 8;
                "Sep" -> 9;
                "Oct" -> 10;
                "Nov" -> 11;
                "Dec" -> 12
            end,

    Local = {
      {Year, Month, list_to_integer(Day)},
      list_to_tuple(lists:map(fun(E) -> list_to_integer(E) end,
                              [Hour, Minute, Second]))
     },
    Universal = erlang:localtime_to_universaltime(Local),
    Seconds = erlang:universaltime_to_posixtime(Universal),
    Nano = erlang:convert_time_unit(Seconds, seconds, nano_seconds),
    #{
       priority => Priority,
       facility => Facility,
       severity => Severity,
       timestamp => Nano,
       host => list_to_binary(Host),
       tag => list_to_binary(Tag)
     };

cleanup_header({PriorityMonth, Day, Hour, Minute, Second, Tag}) ->
    cleanup_header({PriorityMonth, Day, Hour, Minute, Second, {word, "localhost"}, Tag}).

cleanup_body(BodyTokens) ->
    case length(BodyTokens) of
        0 ->
            "";
        1 ->
            [{_, Body}] = BodyTokens,
            Body;
        _ ->
            [Current, Next | Rest] = BodyTokens,
            cleanup_body(Current, Next, Rest, [])
    end.

cleanup_body_merge_colon({_, Current}, [{':', Next} | Rest]) ->
    cleanup_body_merge_colon({word, string:concat(Current, Next)}, Rest);

cleanup_body_merge_colon({_, Current}, [{word, Next} | Rest]) ->
    {string:concat(Current, Next), Rest};

cleanup_body_merge_colon({_, Current}, Rest = []) ->
    {Current, Rest}.

cleanup_body(Previous, Current, [], Result) ->
    Last = case {Previous, Current} of
               {{word, W1}, {word, W2}} -> [W1, W2];
               {{':',  W1}, {word, W2}} -> [string:concat(atom_to_list(W1), W2)];
               {{word, W1}, {':',  W2}} -> [string:concat(W1, atom_to_list(W2))];
               {{':',  W1}, {':',  W2}} -> [string:concat(atom_to_list(W1), atom_to_list(W2))]
           end,

    string:join(lists:append(Result, Last), " ");

cleanup_body(_Previous = {word, W1}, Current = {word, _W2}, [Next | Rest], Result) ->
    cleanup_body(Current, Next, Rest, lists:append(Result, [W1]));

cleanup_body(_Previous = {':', W1}, _Current = {word, W2}, [Next | Rest], Result) ->
    cleanup_body({word, string:concat(atom_to_list(W1), W2)}, Next, Rest, Result);

cleanup_body(_Previous = {word, W1}, _Current = {':', W2}, Rest, Result) ->
    {NewWord, Rest2} = cleanup_body_merge_colon({word, string:concat(W1, W2)}, Rest),

    {Next2, Rest3} = case Rest2 of
                         [] -> {{word, ""}, []};
                         Rest2 -> {hd(Rest2), tl(Rest2)}
                     end,

    cleanup_body({word, NewWord}, Next2, Rest3, Result);

cleanup_body(_Previous = {':', W1}, _Current = {':', W2}, [Next | Rest], Result) ->
    cleanup_body({word, string:concat(atom_to_list(W1), atom_to_list(W2))}, Next, Rest, Result).

cleanup(Header, Body) ->
    Header#{body => list_to_binary(Body)}.

%% @spec decode_priority(Priority::priority()) -> {facility(), severity()}
%% @doc Decodes a priority value into facility and severity
decode_priority(Priority) ->
    {decode_facility(Priority div 8),
     decode_severity(Priority rem 8)}.

-spec decode_facility(non_neg_integer()) ->
                             esyslog_message:facility().
decode_facility(Facility) ->
    case Facility of
        0 -> kern;
        1 -> user;
        2 -> mail;
        3 -> system;
        4 -> auth;
        5 -> internal;
        6 -> lpr;
        7 -> nns;
        8 -> uucp;
        9 -> clock;
        10 -> authpriv;
        11 -> ftp;
        12 -> ntp;
        13 -> audit;
        14 -> alert;
        15 -> clock2;
        16 -> local0;
        17 -> local1;
        18 -> local2;
        19 -> local3;
        20 -> local4;
        21 -> local5;
        22 -> local6;
        23 -> local7;
        _ -> undefined
    end.

-spec decode_severity(non_neg_integer()) ->
                             esyslog_message:severity().

decode_severity(Severity) ->
    case Severity of
        0 -> emerg;
        1 -> alert;
        2 -> crit;
        3 -> err;
        4 -> warn;
        5 -> notice;
        6 -> info;
        7 -> debug;
        _ -> undefined
    end.
