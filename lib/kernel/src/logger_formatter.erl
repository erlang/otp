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
-export([check_config/1]).

-include("logger_internal.hrl").

%%%-----------------------------------------------------------------
%%% Types
-type template() :: [atom()|tuple()|string()].

%%%-----------------------------------------------------------------
%%% API
-spec format(Log,Config) -> unicode:chardata() when
      Log :: logger:log(),
      Config :: #{single_line=>boolean(),
                  legacy_header=>boolean(),
                  report_cb=>fun((logger:report()) -> {io:format(),[term()]}),
                  chars_limit=>pos_integer()| unlimited,
                  max_size=>pos_integer() | unlimited,
                  depth=>pos_integer() | unlimited,
                  template=>template(),
                  time_designator=>byte(),
                  time_offset=>integer()|[byte()]}.
format(#{level:=Level,msg:=Msg0,meta:=Meta},Config0)
  when is_map(Config0) ->
    Config = add_default_config(Config0),
    Meta1 = maybe_add_legacy_header(Level,Meta,Config),
    Template = maps:get(template,Config),
    {BT,AT0} = lists:splitwith(fun(msg) -> false; (_) -> true end, Template),
    {DoMsg,AT} =
        case AT0 of
            [msg|Rest] -> {true,Rest};
            _ ->{false,AT0}
        end,
    B = do_format(Level,"",Meta1,BT,Config),
    A = do_format(Level,"",Meta1,AT,Config),
    MsgStr =
        if DoMsg ->
                Config1 =
                    case maps:get(chars_limit,Config) of
                        unlimited ->
                            Config;
                        Size0 ->
                            Size =
                                case Size0 - string:length([B,A]) of
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
                        re:replace(string:trim(MsgStr0),",?\r?\n\s*",", ",
                                   [{return,list},global]);
                    _false ->
                        MsgStr0
                end;
           true ->
                ""
        end,
    truncate(B ++ MsgStr ++ A,maps:get(max_size,Config)).

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
format_msg(Msg,_Meta,#{depth:=Depth,chars_limit:=CharsLimit}) ->
    limit_size(Msg, Depth, CharsLimit).

limit_size(Msg,Depth,unlimited) ->
    limit_size(Msg,Depth,[]);
limit_size(Msg,Depth,CharsLimit) when is_integer(CharsLimit) ->
    limit_size(Msg,Depth,[{chars_limit,CharsLimit}]);
limit_size({Format,Args},unlimited,Opts) when is_list(Opts) ->
    try io_lib:format(Format,Args,Opts)
    catch _:_ ->
            io_lib:format("FORMAT ERROR: ~tp - ~tp",[Format,Args],Opts)
    end;
limit_size({Format0,Args},Depth,Opts) when is_integer(Depth) ->
    try
        Format1 = io_lib:scan_format(Format0, Args),
        Format = limit_format(Format1, Depth),
        io_lib:build_text(Format,Opts)
    catch _:_ ->
            limit_size({"FORMAT ERROR: ~tp - ~tp",[Format0,Args]},Depth,Opts)
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
truncate(String,Size) ->
    Length = string:length(String),
    if Length>Size ->
            case lists:reverse(lists:flatten(String)) of
                [$\n|_] ->
                    string:slice(String,0,Size-4)++"...\n";
                _ ->
                    string:slice(String,0,Size-3)++"..."
            end;
       true ->
            String
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
    {{Y,Mo,D},{H,Mi,S},Micro,UtcStr} =
        timestamp_to_datetimemicro(Timestamp,Config),
    Header =
        io_lib:format("=~ts==== ~w-~s-~4w::~2..0w:~2..0w:~2..0w.~6..0w ~s===",
                      [Title,D,month(Mo),Y,H,Mi,S,Micro,UtcStr]),
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

%% Ensure that all valid configuration parameters exist in the final
%% configuration map
add_default_config(Config0) ->
    Default =
        #{legacy_header=>false,
          single_line=>true,
          chars_limit=>unlimited,
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
do_check_config([{report_cb,RCB}|Config]) when is_function(RCB,1) ->
    do_check_config(Config);
do_check_config([{template,T}|Config]) when is_list(T) ->
    case lists:all(fun(X) when is_atom(X) -> true;
                      (X) when is_tuple(X), is_atom(element(1,X)) -> true;
                      (X) when is_list(X) -> io_lib:printable_unicode_list(X);
                      (_) -> false
                   end,
                   T) of
        true ->
            do_check_config(Config);
        false ->
            {error,{invalid_formatter_template,?MODULE,T}}
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
