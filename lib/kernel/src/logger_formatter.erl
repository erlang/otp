%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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

-export([format/2]).

-include("logger_internal.hrl").

%%%-----------------------------------------------------------------
%%% Types
-type template() :: [atom()|tuple()|string()].

%%%-----------------------------------------------------------------
%%% API
-spec format(Log,Config) -> String when
      Log :: logger:log(),
      Config :: #{single_line=>boolean(),
                  legacy_header=>boolean(),
                  report_cb=>fun((logger:report()) -> {io:format(),[term()]}),
                  max_size=>pos_integer() | unlimited,
                  depth=>pos_integer() | unlimited,
                  template=>template(),
                  utc=>boolean()},
      String :: string().
format(#{level:=Level,msg:=Msg0,meta:=Meta},Config0)
  when is_map(Config0) ->
    Config = add_default_config(Config0),
    Meta1 = maybe_add_legacy_header(Level,Meta,Config),
    MsgStr0 = format_msg(Msg0,Meta1,Config),
    MsgStr =
        case maps:get(single_line,Config) of
            true ->
                %% Trim leading and trailing whitespaces, and replace
                %% newlines with ", "
                re:replace(string:trim(MsgStr0),",?\r?\n\s*",", ",
                           [{return,list},global]);
            _false2 ->
                MsgStr0
        end,
    String = do_format(Level,MsgStr,Meta1,maps:get(template,Config),Config),
    truncate(String,maps:get(max_size,Config)).

do_format(Level,Msg,Data,[level|Format],Config) ->
    [to_string(level,Level,Config)|do_format(Level,Msg,Data,Format,Config)];
do_format(Level,Msg,Data,[msg|Format],Config) ->
    [Msg|do_format(Level,Msg,Data,Format,Config)];
do_format(Level,Msg,Data,[Key|Format],Config) when is_atom(Key); is_tuple(Key) ->
    Value = value(Key,Data),
    [to_string(Key,Value,Config)|do_format(Level,Msg,Data,Format,Config)];
do_format(Level,Msg,Data,[Str|Format],Config) ->
    [Str|do_format(Level,Msg,Data,Format,Config)];
do_format(_Level,_Msg,_Data,[],_Config) ->
    [].

value(Key,Meta) when is_atom(Key), is_map(Meta) ->
    maps:get(Key,Meta,"");
value(Key,_) when is_atom(Key) ->
    "";
value(Keys,Meta) when is_tuple(Keys) ->
    value(tuple_to_list(Keys),Meta);
value([Key|Keys],Meta) ->
    value(Keys,value(Key,Meta));
value([],Value) ->
    Value.

to_string(time,Time,Config) ->
    format_time(Time,Config);
to_string(mfa,MFA,_Config) ->
    format_mfa(MFA);
to_string(_,Value,_Config) ->
    to_string(Value).

to_string(X) when is_atom(X) ->
    atom_to_list(X);
to_string(X) when is_integer(X) ->
    integer_to_list(X);
to_string(X) when is_pid(X) ->
    pid_to_list(X);
to_string(X) when is_reference(X) ->
    ref_to_list(X);
to_string(X) when is_list(X) ->
    case io_lib:printable_unicode_list(lists:flatten(X)) of
        true -> X;
        _ -> io_lib:format("~tp",[X])
    end;
to_string(X) ->
    io_lib:format("~tp",[X]).

format_msg({string,Chardata},Meta,Config) ->
    try unicode:characters_to_list(Chardata)
    catch _:_ -> format_msg({"INVALID STRING: ~tp",[Chardata]},Meta,Config)
    end;
format_msg({report,_}=Msg,Meta,#{report_cb:=Fun}=Config) when is_function(Fun,1) ->
    format_msg(Msg,Meta#{report_cb=>Fun},maps:remove(report_cb,Config));
format_msg({report,Report},#{report_cb:=Fun}=Meta,Config) when is_function(Fun,1) ->
    try Fun(Report) of
        {Format,Args} when is_list(Format), is_list(Args) ->
            format_msg({Format,Args},maps:remove(report_cb,Meta),Config);
        Other ->
            format_msg({"REPORT_CB ERROR: ~tp; Returned: ~tp",
                        [Report,Other]},Meta,Config)
    catch C:R ->
            format_msg({"REPORT_CB CRASH: ~tp; Reason: ~tp",
                        [Report,{C,R}]},Meta,Config)
    end;
format_msg({report,Report},Meta,Config) ->
    format_msg({report,Report},
               Meta#{report_cb=>fun logger:format_report/1},
               Config);
format_msg(Msg,_Meta,Config) ->
    limit_depth(Msg, maps:get(depth,Config)).

limit_depth(Msg,false) ->
    Depth = logger:get_format_depth(),
    limit_depth(Msg,Depth);
limit_depth({Format,Args},unlimited) ->
    try io_lib:format(Format,Args)
    catch _:_ ->
            io_lib:format("FORMAT ERROR: ~tp - ~tp",[Format,Args])
    end;
limit_depth({Format0,Args},Depth) ->
    try
        Format1 = io_lib:scan_format(Format0, Args),
        Format = limit_format(Format1, Depth),
        io_lib:build_text(Format)
    catch _:_ ->
            limit_depth({"FORMAT ERROR: ~tp - ~tp",[Format0,Args]},Depth)
    end.

limit_format([#{control_char:=C0}=M0|T], Depth) when C0 =:= $p;
						     C0 =:= $w ->
    C = C0 - ($a - $A),				%To uppercase.
    #{args:=Args} = M0,
    M = M0#{control_char:=C,args:=Args++[Depth]},
    [M|limit_format(T, Depth)];
limit_format([H|T], Depth) ->
    [H|limit_format(T, Depth)];
limit_format([], _) ->
    [].

truncate(String,unlimited) ->
    String;
truncate(String,false) ->
    Size = logger:get_max_size(),
    truncate(String,Size);
truncate(String,Size) ->
    Length = string:length(String),
    if Length>Size ->
            string:slice(String,0,Size-3)++"...";
       true ->
            String
    end.

format_time(Timestamp,Config) when is_integer(Timestamp) ->
    {Date,Time,Micro} = timestamp_to_datetimemicro(Timestamp,Config),
    format_time(Date,Time,Micro);
format_time(Other,_Config) ->
    %% E.g. a string
    to_string(Other).

format_time({Y,M,D},{H,Min,S},Micro) ->
    io_lib:format("~4w-~2..0w-~2..0w ~2w:~2..0w:~2..0w.~6..0w",
                  [Y,M,D,H,Min,S,Micro]).

%% Assuming this is monotonic time in microseconds
timestamp_to_datetimemicro(Timestamp,Config) when is_integer(Timestamp) ->
    SysTime = Timestamp + erlang:time_offset(microsecond),
    Micro = SysTime rem 1000000,
    Sec = SysTime div 1000000,
    UniversalTime =  erlang:posixtime_to_universaltime(Sec),
    {Date,Time} =
        case Config of
            #{utc:=true} -> UniversalTime;
            _ -> erlang:universaltime_to_localtime(UniversalTime)
        end,
    {Date,Time,Micro}.

format_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    atom_to_list(M)++":"++atom_to_list(F)++"/"++integer_to_list(A);
format_mfa({M,F,A}) when is_atom(M), is_atom(F), is_list(A) ->
    format_mfa({M,F,length(A)});
format_mfa(MFA) ->
    to_string(MFA).

maybe_add_legacy_header(Level,
                        #{time:=Timestamp}=Meta,
                        #{legacy_header:=true}=Config) ->
    #{title:=Title}=MyMeta = add_legacy_title(Level,maps:get(?MODULE,Meta,#{})),
    {{Y,Mo,D},{H,Mi,S},Micro} = timestamp_to_datetimemicro(Timestamp,Config),
    Header = io_lib:format("=~ts==== ~w-~s-~4w::~2..0w:~2..0w:~2..0w.~6..0w ~s===",
                           [Title,D,month(Mo),Y,H,Mi,S,Micro,utcstr(Config)]),
    Meta#{?MODULE=>MyMeta#{header=>Header}};
maybe_add_legacy_header(_,Meta,_) ->
    Meta.

add_legacy_title(_Level,#{title:=_}=MyMeta) ->
    MyMeta;
add_legacy_title(Level,MyMeta) ->
    Title = string:uppercase(atom_to_list(Level)) ++ " REPORT",
    MyMeta#{title=>Title}.

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

utcstr(#{utc:=true}) -> "UTC ";
utcstr(_) -> "".

add_default_config(#{utc:=_}=Config) ->
    Default =
        #{legacy_header=>false,
          single_line=>false,
          max_size=>false,
          depth=>false},
    add_default_template(maps:merge(Default,Config));
add_default_config(Config) ->
    add_default_config(Config#{utc=>logger:get_utc_config()}).

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
