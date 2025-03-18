%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(logger_formatter).
-moduledoc """
Default formatter for Logger.

Each Logger handler has a configured formatter specified as a module and a
configuration term. The purpose of the formatter is to translate the log events
to a final printable string ([`unicode:chardata()`](`t:unicode:chardata/0`))
which can be written to the output device of the handler. See sections
[Handlers](logger_chapter.md#handlers) and
[Formatters](logger_chapter.md#formatters) in the Kernel User's Guide for more
information.

`m:logger_formatter` is the default formatter used by Logger.

## See Also

`m:calendar`, `m:error_logger`, `m:io`, `m:io_lib`, `m:logger`, `m:maps`,
[`sasl(6)`](`e:sasl:sasl_app.md`), `m:unicode`
""".
-moduledoc(#{since => "OTP 21.0"}).

-export([format/2]).
-export([check_config/1]).

-include("logger_internal.hrl").

%%%-----------------------------------------------------------------
%%% Types
-doc """
The configuration term for `logger_formatter` is a [map](`m:maps`), and the
following keys can be set as configuration parameters:

- **`chars_limit = integer() > 0 | unlimited`{: #chars_limit }** - A positive
  integer representing the value of the option with the same name to be used
  when calling `io_lib:format/3`. This value limits the total number of
  characters printed for each log event. Notice that this is a soft limit. For a
  hard truncation limit, see option `max_size`.

  Defaults to `unlimited`.

- **`depth = integer() > 0 | unlimited`{: #depth }** - A positive integer
  representing the maximum depth to which terms shall be printed by this
  formatter. Format strings passed to this formatter are rewritten. The format
  controls ~p and ~w are replaced with ~P and ~W, respectively, and the value is
  used as the depth parameter. For details, see [`io:format/2,3`](`io:format/2`)
  in STDLIB.

  Defaults to `unlimited`.

- **`legacy_header = boolean()`** - If set to `true` a header field is added to
  logger_formatter's part of `Metadata`. The value of this field is a string
  similar to the header created by the old `m:error_logger` event handlers. It
  can be included in the log event by adding the list
  `[logger_formatter,header]` to the template. See the description of the
  `t:template/0` type for more information.

  Defaults to `false`.

- **`max_size = integer() > 0 | unlimited`{: #max_size }** - A positive integer
  representing the absolute maximum size a string returned from this formatter
  can have. If the formatted string is longer, after possibly being limited by
  `chars_limit` or `depth`, it is truncated.

  Defaults to `unlimited`.

- **`report_cb = ` `t:logger:report_cb/0`** - A report callback is used by the
  formatter to transform log messages on report form to a format string and
  arguments. The report callback can be specified in the metadata for the log
  event. If no report callback exists in metadata, `logger_formatter` will use
  `logger:format_report/1` as default callback.

  If this configuration parameter is set, it replaces both the default report
  callback, and any report callback found in metadata. That is, all reports are
  converted by this configured function.

- **`single_line = boolean()`** - If set to `true`, each log event is printed as
  a single line. To achieve this, `logger_formatter` sets the field width to `0`
  for all `~p` and `~P` control sequences in the format a string (see
  `io:format/2`), and replaces all newlines in the message with `", "`. White
  spaces following directly after newlines are removed. Notice that newlines
  added by the `template` parameter are not replaced.

  Defaults to `true`.

- **`template = `{: #template }`t:template/0`** - The template describes how the
  formatted string is composed by combining different data values from the log
  event. See the description of the `t:template/0` type for more information
  about this.

- **`time_designator = byte()`** - Timestamps are formatted according to
  RFC3339, and the time designator is the character used as date and time
  separator.

  Defaults to `$T`.

  The value of this parameter is used as the `time_designator` option to
  `calendar:system_time_to_rfc3339/2`.

- **`time_offset = integer() | [byte()]`** - The time offset, either a string or
  an integer, to be used when formatting the timestamp.

  An empty string is interpreted as local time. The values `"Z"`, `"z"` or `0`
  are interpreted as Universal Coordinated Time (UTC).

  Strings, other than `"Z"`, `"z"`, or `""`, must be of the form `±[hh]:[mm]`,
  for example `"-02:00"` or `"+00:00"`.

  Integers must be in microseconds, meaning that the offset `7200000000` is
  equivalent to `"+02:00"`.

  Defaults to an empty string, meaning that timestamps are displayed in local
  time. However, for backwards compatibility, if the SASL configuration
  parameter [`utc_log`](`e:sasl:sasl_app.md#utc_log`)`=true`, the default is
  changed to `"Z"`, meaning that timestamps are displayed in UTC.

  The value of this parameter is used as the `offset` option to
  `calendar:system_time_to_rfc3339/2`.
""".
-type config() :: #{chars_limit     => pos_integer() | unlimited,
                    depth           => pos_integer() | unlimited,
                    legacy_header   => boolean(),
                    max_size        => pos_integer() | unlimited,
                    report_cb       => logger:report_cb(),
                    single_line     => boolean(),
                    template        => template(),
                    time_designator => byte(),
                    time_offset     => integer() | [byte()]}.
-doc """
The template to be used by a logger formatter.

The template is a list of atoms, atom lists, tuples and strings. The atoms
`level` or `msg`, are treated as placeholders for the severity level and the log
message, respectively. Other atoms or atom lists are interpreted as placeholders
for metadata, where atoms are expected to match top level keys, and atom lists
represent paths to sub keys when the metadata is a nested map. For example the
list `[key1,key2]` is replaced by the value of the `key2` field in the nested
map below. The atom `key1` on its own is replaced by the complete value of the
`key1` field. The values are converted to strings.

```text
#{key1 => #{key2 => my_value,
            ...}
  ...}
```

Tuples in the template express if-exist tests for metadata keys. For example,
the following tuple says that if `key1` exists in the metadata map, print
`"key1=Value"`, where `Value` is the value that `key1` is associated with in the
metadata map. If `key1` does not exist, print nothing.

```text
{key1, ["key1=",key1], []}
```

Strings in the template are printed literally.

The default value for the `template` configuration parameter depends on the
value of the `single_line` and `legacy_header` configuration parameters as
follows.

The log event used in the examples is:

```text
?LOG_ERROR("name: ~p~nexit_reason: ~p", [my_name, "It crashed"])
```

- **`legacy_header = true, single_line = false`** - Default template:
  `[[logger_formatter,header],"\n",msg,"\n"]`

  Example log entry:

  ```text
  =ERROR REPORT==== 17-May-2018::18:30:19.453447 ===
  name: my_name
  exit_reason: "It crashed"
  ```

  Notice that all eight levels can occur in the heading, not only `ERROR`,
  `WARNING` or `INFO` as `m:error_logger` produces. And microseconds are added
  at the end of the timestamp.

- **`legacy_header = true, single_line = true`** - Default template:
  `[[logger_formatter,header],"\n",msg,"\n"]`

  Notice that the template is here the same as for `single_line=false`, but the
  resulting log entry differs in that there is only one line after the heading:

  ```text
  =ERROR REPORT==== 17-May-2018::18:31:06.952665 ===
  name: my_name, exit_reason: "It crashed"
  ```

- **`legacy_header = false, single_line = true`** - Default template:
  `[time," ",level,": ",msg,"\n"]`

  Example log entry:

  ```text
  2018-05-17T18:31:31.152864+02:00 error: name: my_name, exit_reason: "It crashed"
  ```

- **`legacy_header = false, single_line = false`** - Default template:
  `[time," ",level,":\n",msg,"\n"]`

  Example log entry:

  ```text
  2018-05-17T18:32:20.105422+02:00 error:
  name: my_name
  exit_reason: "It crashed"
  ```
""".
-type template() :: [metakey() | {metakey(),template(),template()} | unicode:chardata()].
-doc "".
-type metakey() :: atom() | [atom()].

%%%-----------------------------------------------------------------
%%% Callbacks
-doc """
The function is called by a Logger when formatter configuration is set or
modified. The formatter must validate the given configuration and return `ok` if
it is correct, and `{error,Reason}` if it is faulty.

The following Logger API functions can trigger this callback:

- `logger:add_handler/3`
- [`logger:set_handler_config/2,3`](`logger:set_handler_config/2`)
- [`logger:update_handler_config/2,3`](`logger:update_handler_config/2`)
- `logger:update_formatter_config/2`

See `m:logger_formatter` for an example implementation. `m:logger_formatter` is
the default formatter used by Logger.
""".
-doc(#{group => <<"Formatter Callback Functions">>,since => <<"OTP 21.0">>}).
-callback check_config(FConfig) -> ok | {error, Reason} when
      FConfig :: logger:formatter_config(),
      Reason :: term().

-doc """
The function can be called by a log handler to convert a log event term to a
printable string. The returned value can, for example, be printed as a log entry
to the console or a file using [`io:put_chars/1,2`](`io:put_chars/1`).

See `m:logger_formatter` for an example implementation. `m:logger_formatter` is
the default formatter used by Logger.
""".
-doc(#{group => <<"Formatter Callback Functions">>,since => <<"OTP 21.0">>}).
-callback format(LogEvent, FConfig) -> FormattedLogEntry when
      LogEvent :: logger:log_event(),
      FConfig :: logger:formatter_config(),
      FormattedLogEntry :: unicode:chardata().

%%%-----------------------------------------------------------------
%%% API
-doc """
This the formatter callback function to be called from handlers.

The log event is processed as follows:

- If the message is on report form, it is converted to `{Format,Args}` by
  calling the report callback. See section
  [Log Message](logger_chapter.md#log-message) in the Kernel User's Guide for
  more information about report callbacks and valid forms of log messages.
- The message size is limited according to the values of configuration
  parameters [`chars_limit`](`m:logger_formatter#chars_limit`) and
  [`depth`](`m:logger_formatter#depth`).
- The full log entry is composed according to the
  [`template`](`m:logger_formatter#template`).
- If the final string is too long, it is truncated according to the value of
  configuration parameter [`max_size`](`m:logger_formatter#max_size`).
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec format(LogEvent,Config) -> unicode:chardata() when
      LogEvent :: logger:log_event(),
      Config :: config().
format(#{level:=Level,msg:=Msg0,meta:=Meta},Config0)
  when is_map(Config0) ->
    Config = add_default_config(Config0),
    Meta1 = maybe_add_legacy_header(Level,Meta,Config),
    Template = maps:get(template,Config),
    LinearTemplate = linearize_template(Meta1,Template),
    {BT,AT0} = lists:splitwith(fun(msg) -> false; (_) -> true end, LinearTemplate),
    {DoMsg,AT} =
        case AT0 of
            [msg|Rest] -> {true,Rest};
            _ ->{false,AT0}
        end,
    B = do_format(Level,Meta1,BT,Config),
    A = do_format(Level,Meta1,AT,Config),
    MsgStr =
        if DoMsg ->
                Config1 =
                    case maps:get(chars_limit,Config) of
                        unlimited ->
                            Config;
                        Size0 ->
                            Size =
                                case Size0 - io_lib:chars_length([B,A]) of
                                    S when S>=0 -> S;
                                    _ -> 0
                                end,
                            Config#{chars_limit=>Size}
                    end,
                MsgStr0 = format_msg(Msg0,Meta1,Config1),
                case maps:get(single_line,Config) of
                    true ->
                        %% Trim leading and trailing whitespaces, and replace
                        %% newlines with ", "
                        T = lists:reverse(
                              trim(
                                lists:reverse(
                                  trim(MsgStr0,false)),true)),
                        re:replace(T,",?\r?\n\s*",", ",
                                   [{return,list},global,unicode]);
                    _false ->
                        MsgStr0
                end;
           true ->
                ""
        end,
    truncate(B,MsgStr,A,maps:get(max_size,Config)).

linearize_template(Data,[{Key,IfExist,Else}|Format]) ->
    BranchForUse =
        case value(Key,Data) of
            {ok,_Value} -> linearize_template(Data,IfExist);
            error -> linearize_template(Data,Else)
        end,
    BranchForUse ++ linearize_template(Data,Format);
linearize_template(Data,[StrOrKey|Format]) ->
    [StrOrKey|linearize_template(Data,Format)];
linearize_template(_Data,[]) ->
    [].

trim([H|T],Rev) when H==$\s; H==$\r; H==$\n ->
    trim(T,Rev);
trim([H|T],false) when is_list(H) ->
    case trim(H,false) of
        [] ->
            trim(T,false);
        TrimmedH ->
            [TrimmedH|T]
    end;
trim([H|T],true) when is_list(H) ->
    case trim(lists:reverse(H),true) of
        [] ->
            trim(T,true);
        TrimmedH ->
            [lists:reverse(TrimmedH)|T]
    end;
trim(String,_) ->
    String.

do_format(Level,Data,[level|Format],Config) ->
    [to_string(level,Level,Config)|do_format(Level,Data,Format,Config)];
do_format(Level,Data,[Key|Format],Config)
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    String =
        case value(Key,Data) of
            {ok,Value} -> to_string(Key,Value,Config);
            error -> ""
        end,
    [String|do_format(Level,Data,Format,Config)];
do_format(Level,Data,[Str|Format],Config) ->
    [Str|do_format(Level,Data,Format,Config)];
do_format(_Level,_Data,[],_Config) ->
    [].

value(Key,Meta) when is_map_key(Key,Meta) ->
    {ok,maps:get(Key,Meta)};
value([Key|Keys],Meta) when is_map_key(Key,Meta) ->
    value(Keys,maps:get(Key,Meta));
value([],Value) ->
    {ok,Value};
value(_,_) ->
    error.

to_string(time,Time,Config) ->
    format_time(Time,Config);
to_string(mfa,MFA,Config) ->
    format_mfa(MFA,Config);
to_string(_,Value,Config) ->
    to_string(Value,Config).

to_string(X,_) when is_atom(X) ->
    atom_to_list(X);
to_string(X,_) when is_integer(X) ->
    integer_to_list(X);
to_string(X,_) when is_pid(X) ->
    pid_to_list(X);
to_string(X,_) when is_reference(X) ->
    ref_to_list(X);
to_string(X,Config) when is_list(X) ->
    case printable_list(lists:flatten(X)) of
        true -> X;
        _ -> io_lib:format(p(Config),[X])
    end;
to_string(X,Config) ->
    io_lib:format(p(Config),[X]).

printable_list([]) ->
    false;
printable_list(X) ->
    io_lib:printable_list(X).

format_msg({string,Chardata},Meta,Config) ->
    format_msg({"~ts",[Chardata]},Meta,Config);
format_msg({report,_}=Msg,Meta,#{report_cb:=Fun}=Config)
  when is_function(Fun,1); is_function(Fun,2) ->
    format_msg(Msg,Meta#{report_cb=>Fun},maps:remove(report_cb,Config));
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,1) ->
    try Fun(Report) of
        {Format,Args} when is_list(Format), is_list(Args) ->
            format_msg({Format,Args},maps:remove(report_cb,Meta),Config);
        Other ->
            P = p(Config),
            format_msg({"REPORT_CB/1 ERROR: "++P++"; Returned: "++P,
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            P = p(Config),
            format_msg({"REPORT_CB/1 CRASH: "++P++"; Reason: "++P,
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,2) ->
    try Fun(Report,maps:with([depth,chars_limit,single_line],Config)) of
        Chardata when ?IS_STRING(Chardata) ->
            try chardata_to_list(Chardata) % already size limited by report_cb
            catch _:_ ->
                    P = p(Config),
                    format_msg({"REPORT_CB/2 ERROR: "++P++"; Returned: "++P,
                                [Report,Chardata]},Meta,Config)
            end;
        Other ->
            P = p(Config),
            format_msg({"REPORT_CB/2 ERROR: "++P++"; Returned: "++P,
                        [Report,Other]},Meta,Config)
    catch C:R:S ->
            P = p(Config),
            format_msg({"REPORT_CB/2 CRASH: "++P++"; Reason: "++P,
                        [Report,{C,R,logger:filter_stacktrace(?MODULE,S)}]},
                       Meta,Config)
    end;
format_msg({report,Report},Meta,Config) ->
    format_msg({report,Report},
               Meta#{report_cb=>fun logger:format_report/1},
               Config);
format_msg(Msg,_Meta,#{depth:=Depth,chars_limit:=CharsLimit,
                       single_line:=Single}) ->
    Opts = chars_limit_to_opts(CharsLimit),
    format_msg(Msg, Depth, Opts, Single).

chars_limit_to_opts(unlimited) -> [];
chars_limit_to_opts(CharsLimit) -> [{chars_limit,CharsLimit}].

format_msg({Format0,Args},Depth,Opts,Single) ->
    try
        Format1 = io_lib:scan_format(Format0, Args),
        Format = reformat(Format1, Depth, Single),
        io_lib:build_text(Format,Opts)
    catch C:R:S ->
            P = p(Single),
            FormatError = "FORMAT ERROR: "++P++" - "++P,
            case Format0 of
                FormatError ->
                    %% already been here - avoid failing cyclically
                    erlang:raise(C,R,S);
                _ ->
                    format_msg({FormatError,[Format0,Args]},Depth,Opts,Single)
            end
    end.

reformat(Format,unlimited,false) ->
    Format;
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $p ->
    [limit_depth(M#{width => 0}, Depth)|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, true) when C =:= $P ->
    [M#{width => 0}|reformat(T, Depth, true)];
reformat([#{control_char:=C}=M|T], Depth, Single) when C =:= $p; C =:= $w ->
    [limit_depth(M, Depth)|reformat(T, Depth, Single)];
reformat([H|T], Depth, Single) ->
    [H|reformat(T, Depth, Single)];
reformat([], _, _) ->
    [].

limit_depth(M0, unlimited) ->
    M0;
limit_depth(#{control_char:=C0, args:=Args}=M0, Depth) ->
    C = C0 - ($a - $A),				%To uppercase.
    M0#{control_char:=C,args:=Args++[Depth]}.

chardata_to_list(Chardata) ->
    case unicode:characters_to_list(Chardata,unicode) of
        List when is_list(List) ->
            List;
        Error ->
            throw(Error)
    end.

truncate(B,Msg,A,unlimited) ->
    [B,Msg,A];
truncate(B,Msg,A,Size) ->
    String = [B,Msg,A],
    Length = io_lib:chars_length(String),
    if Length>Size ->
            {Last,FlatString} =
                case A of
                    [] ->
                        case Msg of
                            [] ->
                                {get_last(B),lists:flatten(B)};
                            _ ->
                                {get_last(Msg),lists:flatten([B,Msg])}
                        end;
                    _ ->
                        {get_last(A),lists:flatten(String)}
                end,
            case Last of
                $\n->
                    lists:sublist(FlatString,1,Size-4)++"...\n";
                _ ->
                    lists:sublist(FlatString,1,Size-3)++"..."
            end;
       true ->
            String
    end.

get_last(L) ->
    get_first(lists:reverse(L)).

get_first([]) ->
    error;
get_first([C|_]) when is_integer(C) ->
    C;
get_first([L|Rest]) when is_list(L) ->
    case get_last(L) of
        error -> get_first(Rest);
        First -> First
    end.

%% SysTime is the system time in microseconds
format_time(SysTime,#{time_offset:=Offset,time_designator:=Des})
  when is_integer(SysTime) ->
    calendar:system_time_to_rfc3339(SysTime,[{unit,microsecond},
                                             {offset,Offset},
                                             {time_designator,Des}]).

%% SysTime is the system time in microseconds
timestamp_to_datetimemicro(SysTime,Config) when is_integer(SysTime) ->
    Micro = SysTime rem 1000000,
    Sec = SysTime div 1000000,
    UniversalTime =  erlang:posixtime_to_universaltime(Sec),
    {{Date,Time},UtcStr} =
        case offset_to_utc(maps:get(time_offset,Config)) of
            true -> {UniversalTime,"UTC "};
            _ -> {erlang:universaltime_to_localtime(UniversalTime),""}
        end,
    {Date,Time,Micro,UtcStr}.

format_mfa({M,F,A},_) when is_atom(M), is_atom(F), is_integer(A) ->
    io_lib:fwrite("~tw:~tw/~w", [M, F, A]);
format_mfa({M,F,A},Config) when is_atom(M), is_atom(F), is_list(A) ->
    format_mfa({M,F,length(A)},Config);
format_mfa(MFA,Config) ->
    to_string(MFA,Config).

maybe_add_legacy_header(Level,
                        #{time:=Timestamp}=Meta,
                        #{legacy_header:=true}=Config) ->
    #{title:=Title}=MyMeta = add_legacy_title(Level,Meta,Config),
    {{Y,Mo,D},{H,Mi,S},Micro,UtcStr} =
        timestamp_to_datetimemicro(Timestamp,Config),
    Header =
        io_lib:format("=~ts==== ~w-~s-~4w::~2..0w:~2..0w:~2..0w.~6..0w ~s===",
                      [Title,D,month(Mo),Y,H,Mi,S,Micro,UtcStr]),
    Meta#{?MODULE=>MyMeta#{header=>Header}};
maybe_add_legacy_header(_,Meta,_) ->
    Meta.

add_legacy_title(_Level,#{?MODULE:=#{title:=_}=MyMeta},_) ->
    MyMeta;
add_legacy_title(Level,Meta,Config) ->
    case maps:get(?MODULE,Meta,#{}) of
        #{title:=_}=MyMeta ->
            MyMeta;
        MyMeta ->
            TitleLevel =
                case (Level=:=notice andalso maps:find(error_logger,Meta)) of
                    {ok,_} ->
                        maps:get(error_logger_notice_header,Config);
                    _ ->
                        Level
                end,
            Title = string:uppercase(atom_to_list(TitleLevel)) ++ " REPORT",
            MyMeta#{title=>Title}
    end.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% Ensure that all valid configuration parameters exist in the final
%% configuration map
add_default_config(Config0) ->
    Default =
        #{chars_limit=>unlimited,
          error_logger_notice_header=>info,
          legacy_header=>false,
          single_line=>true,
          time_designator=>$T},
    MaxSize = get_max_size(maps:get(max_size,Config0,undefined)),
    Depth = get_depth(maps:get(depth,Config0,undefined)),
    Offset = get_offset(maps:get(time_offset,Config0,undefined)),
    add_default_template(maps:merge(Default,Config0#{max_size=>MaxSize,
                                                     depth=>Depth,
                                                     time_offset=>Offset})).

add_default_template(#{template:=_}=Config) ->
    Config;
add_default_template(Config) ->
    Config#{template=>default_template(Config)}.

default_template(#{legacy_header:=true}) ->
    ?DEFAULT_FORMAT_TEMPLATE_HEADER;
default_template(#{single_line:=true}) ->
    ?DEFAULT_FORMAT_TEMPLATE_SINGLE;
default_template(_) ->
    ?DEFAULT_FORMAT_TEMPLATE.

get_max_size(undefined) ->
    unlimited;
get_max_size(S) ->
    max(10,S).

get_depth(undefined) ->
    error_logger:get_format_depth();
get_depth(S) ->
    max(5,S).

get_offset(undefined) ->
    utc_to_offset(get_utc_config());
get_offset(Offset) ->
    Offset.

utc_to_offset(true) ->
    "Z";
utc_to_offset(false) ->
    "".

get_utc_config() ->
    %% SASL utc_log overrides stdlib config - in order to have uniform
    %% timestamps in log messages
    case application:get_env(sasl, utc_log) of
        {ok, Val} when is_boolean(Val) -> Val;
        _ ->
            case application:get_env(stdlib, utc_log) of
                {ok, Val} when is_boolean(Val) -> Val;
                _ -> false
            end
    end.

offset_to_utc(Z) when Z=:=0; Z=:="z"; Z=:="Z" ->
    true;
offset_to_utc([$+|Tz]) ->
    case io_lib:fread("~d:~d", Tz) of
        {ok, [0, 0], []} ->
            true;
        _ ->
            false
    end;
offset_to_utc(_) ->
    false.

-doc """
The function is called by Logger when the formatter configuration for a handler
is set or modified. It returns `ok` if the configuration is valid, and
`{error,term()}` if it is faulty.

The following Logger API functions can trigger this callback:

- `logger:add_handler/3`
- [`logger:set_handler_config/2,3`](`logger:set_handler_config/2`)
- `logger:update_handler_config/2`
- `logger:update_formatter_config/2`
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec check_config(Config) -> ok | {error,term()} when
      Config :: config().
check_config(Config) when is_map(Config) ->
    do_check_config(maps:to_list(Config));
check_config(Config) ->
    {error,{invalid_formatter_config,?MODULE,Config}}.

do_check_config([{Type,L}|Config]) when Type == chars_limit;
                                        Type == depth;
                                        Type == max_size ->
    case check_limit(L) of
        ok -> do_check_config(Config);
        error -> {error,{invalid_formatter_config,?MODULE,{Type,L}}}
    end;
do_check_config([{single_line,SL}|Config]) when is_boolean(SL) ->
    do_check_config(Config);
do_check_config([{legacy_header,LH}|Config]) when is_boolean(LH) ->
    do_check_config(Config);
do_check_config([{error_logger_notice_header,ELNH}|Config]) when ELNH == info;
                                                                 ELNH == notice ->
    do_check_config(Config);
do_check_config([{report_cb,RCB}|Config]) when is_function(RCB,1);
                                               is_function(RCB,2) ->
    do_check_config(Config);
do_check_config([{template,T}|Config]) ->
    case check_template(T) of
        ok -> do_check_config(Config);
        error -> {error,{invalid_formatter_template,?MODULE,T}}
    end;
do_check_config([{time_offset,Offset}|Config]) ->
    case check_offset(Offset) of
        ok ->
            do_check_config(Config);
        error ->
            {error,{invalid_formatter_config,?MODULE,{time_offset,Offset}}}
    end;
do_check_config([{time_designator,Char}|Config]) when Char>=0, Char=<255 ->
    case io_lib:printable_latin1_list([Char]) of
        true ->
            do_check_config(Config);
        false ->
            {error,{invalid_formatter_config,?MODULE,{time_designator,Char}}}
    end;
do_check_config([C|_]) ->
    {error,{invalid_formatter_config,?MODULE,C}};
do_check_config([]) ->
    ok.

check_limit(L) when is_integer(L), L>0 ->
    ok;
check_limit(unlimited) ->
    ok;
check_limit(_) ->
    error.

check_template([Key|T]) when is_atom(Key) ->
    check_template(T);
check_template([Key|T]) when is_list(Key), is_atom(hd(Key)) ->
    case lists:all(fun(X) when is_atom(X) -> true;
                      (_) -> false
                   end,
                   Key) of
        true ->
            check_template(T);
        false ->
            error
    end;
check_template([{Key,IfExist,Else}|T])
  when is_atom(Key) orelse
       (is_list(Key) andalso is_atom(hd(Key))) ->
    case check_template(IfExist) of
        ok ->
            case check_template(Else) of
                ok ->
                    check_template(T);
                error ->
                    error
            end;
        error ->
            error
    end;
check_template([Str|T]) when is_list(Str) ->
    case io_lib:printable_unicode_list(Str) of
        true -> check_template(T);
        false -> error
    end;
check_template([Bin|T]) when is_binary(Bin) ->
    case unicode:characters_to_list(Bin) of
        Str when is_list(Str) -> check_template([Str|T]);
        _Error -> error
    end;
check_template([]) ->
    ok;
check_template(_) ->
    error.

check_offset(I) when is_integer(I) ->
    ok;
check_offset(Tz) when Tz=:=""; Tz=:="Z"; Tz=:="z" ->
    ok;
check_offset([Sign|Tz]) when Sign=:=$+; Sign=:=$- ->
    check_timezone(Tz);
check_offset(_) ->
    error.

check_timezone(Tz) ->
    try io_lib:fread("~d:~d", Tz) of
        {ok, [_, _], []} ->
            ok;
        _ ->
            error
    catch _:_ ->
            error
    end.

p(#{single_line:=Single}) ->
    p(Single);
p(true) ->
    "~0tp";
p(false) ->
    "~tp".
