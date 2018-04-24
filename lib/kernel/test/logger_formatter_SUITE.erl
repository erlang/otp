%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
-module(logger_formatter_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

-define(TRY(X), my_try(fun() -> X end)).

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(Case, Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

groups() ->
    [].

all() -> 
    [default,
     legacy_header,
     single_line,
     template,
     format_msg,
     report_cb,
     max_size,
     depth,
     format_mfa,
     format_time,
     level_or_msg_in_meta,
     faulty_log,
     faulty_config,
     faulty_msg].

default(_Config) ->
    String1 = format(info,{"~p",[term]},#{},#{}),
    ct:log(String1),
    [_Date,_Time,"info:\nterm\n"] = string:lexemes(String1," "),

    Time = timestamp(),
    ExpectedTimestamp = default_time_format(Time),
    String2 = format(info,{"~p",[term]},#{time=>Time},#{}),
    ct:log(String2),
    " info:\nterm\n" = string:prefix(String2,ExpectedTimestamp), 
    ok.

legacy_header(_Config) ->
    Time = timestamp(),
    String1 = format(info,{"~p",[term]},#{time=>Time},#{legacy_header=>true}),
    ct:log(String1),
    "=INFO REPORT==== "++Rest = String1,
    [Timestamp,"\nterm\n"] = string:lexemes(Rest," ="),
    [D,M,Y,H,Min,S,Micro] = string:lexemes(Timestamp,"-:."),
    integer(D,31),
    integer(Y,2018,infinity),
    integer(H,23),
    integer(Min,59),
    integer(S,59),
    integer(Micro,999999),
    true = lists:member(M,["Jan","Feb","Mar","Apr","May","Jun",
                           "Jul","Aug","Sep","Oct","Nov","Dec"]),

    String2 = format(info,{"~p",[term]},#{time=>Time},#{legacy_header=>false}),
    ct:log(String2),
    ExpectedTimestamp = default_time_format(Time),
    " info:\nterm\n" = string:prefix(String2,ExpectedTimestamp), 
    
    String3 = format(info,{"~p",[term]},#{time=>Time},#{legacy_header=>bad}),
    ct:log(String3),
    String3 = String2,

    String4 = format(info,{"~p",[term]},#{time=>Time},
                     #{legacy_header=>true,
                       single_line=>true}), % <---ignored
    ct:log(String4),
    String4 = String1,

    String5 = format(info,{"~p",[term]},#{}, % <--- no time
                     #{legacy_header=>true}),
    ct:log(String5),
    "=INFO REPORT==== "++_ = String5,
    ok.

single_line(_Config) ->
    Time = timestamp(),
    ExpectedTimestamp = default_time_format(Time),
    String1 = format(info,{"~p",[term]},#{time=>Time},#{single_line=>true}),
    ct:log(String1),
    " info: term\n" = string:prefix(String1,ExpectedTimestamp), 

    String2 = format(info,{"~p",[term]},#{time=>Time},#{single_line=>false}),
    ct:log(String2),
    " info:\nterm\n" = string:prefix(String2,ExpectedTimestamp), 

    String2 = format(info,{"~p",[term]},#{time=>Time},#{single_line=>bad}),
    ok.

template(_Config) ->
    Time = timestamp(),

    Template1 = [msg],
    String1 = format(info,{"~p",[term]},#{time=>Time},#{template=>Template1}),
    ct:log(String1),
    "term" = String1,

    Template2 = [msg,unknown],
    String2 = format(info,{"~p",[term]},#{time=>Time},#{template=>Template2}),
    ct:log(String2),
    "term" = String2,

    Template3 = ["string"],
    String3 = format(info,{"~p",[term]},#{time=>Time},#{template=>Template3}),
    ct:log(String3),
    "string" = String3,

    Template4 = ["string\nnewline"],
    String4 = format(info,{"~p",[term]},#{time=>Time},#{template=>Template4,
                                                      single_line=>true}),
    ct:log(String4),
    "string\nnewline" = String4,

    Template5 = [],
    String5 = format(info,{"~p",[term]},#{time=>Time},#{template=>Template5}),
    ct:log(String5),
    "" = String5,

    Ref6 = erlang:make_ref(),
    Meta6 = #{atom=>some_atom,
              integer=>632,
              list=>[list,"string",4321,#{},{tuple}],
              mfa=>{mod,func,0},
              pid=>self(),
              ref=>Ref6,
              string=>"some string",
              time=>Time,
              tuple=>{1,atom,"list"},
              nested=>#{subkey=>subvalue}},
    Template6 = lists:join(";",maps:keys(maps:remove(nested,Meta6)) ++
                               [{nested,subkey}]),
    String6 = format(info,{"~p",[term]},Meta6,#{template=>Template6,
                                                single_line=>true}),
    ct:log(String6),
    SelfStr = pid_to_list(self()),
    RefStr6 = ref_to_list(Ref6),
    ListStr = "[list,\"string\",4321,#{},{tuple}]",
    ExpectedTime6 = default_time_format(Time),
    ["some_atom",
     "632",
     ListStr,
     "mod:func/0",
     SelfStr,
     RefStr6,
     "some string",
     ExpectedTime6,
     "{1,atom,\"list\"}",
     "subvalue"] = string:lexemes(String6,";"),

    Meta7 = #{time=>Time,
              nested=>#{key1=>#{subkey1=>value1},
                        key2=>value2}},
    Template7 = lists:join(";",[nested,
                                {nested,key1},
                                {nested,key1,subkey1},
                                {nested,key2},
                                {nested,key2,subkey2},
                                {nested,key3},
                                {nested,key3,subkey3}]),
    String7 = format(info,{"~p",[term]},Meta7,#{template=>Template7,
                                                single_line=>true}),
    ct:log(String7),
    [MultipleKeysStr,
     "#{subkey1 => value1}",
     "value1",
     "value2",
     "",
     "",
     ""] = string:split(String7,";",all),
    %% Order of keys is not fixed
    case MultipleKeysStr of
        "#{key2 => value2,key1 => #{subkey1 => value1}}" -> ok;
        "#{key1 => #{subkey1 => value1},key2 => value2}" -> ok;
        _ -> ct:fail({full_nested_map_unexpected,MultipleKeysStr})
    end,
    ok.

format_msg(_Config) ->
    Template = [msg],

    String1 = format(info,{"~p",[term]},#{},#{template=>Template}),
    ct:log(String1),
    "term" = String1,

    String2 = format(info,{"list",[term]},#{},#{template=>Template}),
    ct:log(String2),
    "FORMAT ERROR: \"list\" - [term]" = String2,
    
    String3 = format(info,{report,term},#{},#{template=>Template}),
    ct:log(String3),
    "term" = String3,

    String4 = format(info,{report,term},
                     #{report_cb=>fun(_)-> {"formatted",[]} end},
                     #{template=>Template}),
    ct:log(String4),
    "formatted" = String4,

    String5 = format(info,{report,term},
                     #{report_cb=>fun(_)-> faulty_return end},
                     #{template=>Template}),
    ct:log(String5),
    "REPORT_CB ERROR: term; Returned: faulty_return" = String5,

    String6 = format(info,{report,term},
                     #{report_cb=>fun(_)-> erlang:error(fun_crashed) end},
                     #{template=>Template}),
    ct:log(String6),
    "REPORT_CB CRASH: term; Reason: {error,fun_crashed}" = String6,

    %% strings are not formatted
    String7 = format(info,{string,"string"},
                     #{report_cb=>fun(_)-> {"formatted",[]} end},
                     #{template=>Template}),
    ct:log(String7),
    "string" = String7,

    String8 = format(info,{string,['not',printable,list]},
                     #{report_cb=>fun(_)-> {"formatted",[]} end},
                     #{template=>Template}),
    ct:log(String8),
    "INVALID STRING: ['not',printable,list]" = String8,

    String9 = format(info,{string,"string"},#{},#{template=>Template}),
    ct:log(String9),
    "string" = String9,

    ok.

report_cb(_Config) ->
    Template = [msg],
    MetaFun = fun(_) -> {"meta_rcb",[]} end,
    ConfigFun = fun(_) -> {"config_rcb",[]} end,
    "term" = format(info,{report,term},#{},#{template=>Template}),
    "meta_rcb" =
        format(info,{report,term},#{report_cb=>MetaFun},#{template=>Template}),
    "config_rcb" =
        format(info,{report,term},#{},#{template=>Template,
                                        report_cb=>ConfigFun}),
    "config_rcb" =
        format(info,{report,term},#{report_cb=>MetaFun},#{template=>Template,
                                                          report_cb=>ConfigFun}),
    ok.

max_size(_Config) ->
    Template = [msg],
    "12345678901234567890" =
        format(info,{"12345678901234567890",[]},#{},#{template=>Template}),
    application:set_env(kernel,logger_max_size,11),
    "12345678901234567890" = % min value is 50, so this is not limited
        format(info,{"12345678901234567890",[]},#{},#{template=>Template}),
    "12345678901234567890123456789012345678901234567..." = % 50
        format(info,
               {"123456789012345678901234567890123456789012345678901234567890",
                []},
               #{},
               #{template=>Template}),
    application:set_env(kernel,logger_max_size,53),
    "12345678901234567890123456789012345678901234567890..." = %53
        format(info,
               {"123456789012345678901234567890123456789012345678901234567890",
                []},
               #{},
               #{template=>Template}),
    "1234567..." =
        format(info,{"12345678901234567890",[]},#{},#{template=>Template,
                                                      max_size=>10}),
    "12345678901234567890" =
        format(info,{"12345678901234567890",[]},#{},#{template=>Template,
                                                      max_size=>unlimited}),
    ok.
max_size(cleanup,_Config) ->
    application:unset_env(kernel,logger_max_size),
    ok.

depth(_Config) ->
    Template = [msg],
    "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]" =
        format(info,
               {"~p",[[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]]},
               #{},
               #{template=>Template}),
    application:set_env(kernel,error_logger_format_depth,11),
    "[1,2,3,4,5,6,7,8,9,0|...]" =
        format(info,
               {"~p",[[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]]},
               #{},
               #{template=>Template}),
    application:set_env(kernel,logger_format_depth,12),
    "[1,2,3,4,5,6,7,8,9,0,1|...]" =
        format(info,
               {"~p",[[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]]},
               #{},
               #{template=>Template}),
    "[1,2,3,4,5,6,7,8,9,0,1,2|...]" =
        format(info,
               {"~p",[[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]]},
               #{},
               #{template=>Template,
                 depth=>13}),
    "[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]" =
        format(info,
               {"~p",[[1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0]]},
               #{},
               #{template=>Template,
                 depth=>unlimited}),
    ok.
depth(cleanup,_Config) ->
    application:unset_env(kernel,logger_format_depth),
    ok.

format_mfa(_Config) ->
    Template = [mfa],

    Meta1 = #{mfa=>{mod,func,0}},
    String1 = format(info,{"~p",[term]},Meta1,#{template=>Template}),
    ct:log(String1),
    "mod:func/0" = String1,

    Meta2 = #{mfa=>{mod,func,[]}},
    String2 = format(info,{"~p",[term]},Meta2,#{template=>Template}),
    ct:log(String2),
    "mod:func/0" = String2,

    Meta3 = #{mfa=>"mod:func/0"},
    String3 = format(info,{"~p",[term]},Meta3,#{template=>Template}),
    ct:log(String3),
    "mod:func/0" = String3,

    Meta4 = #{mfa=>othermfa},
    String4 = format(info,{"~p",[term]},Meta4,#{template=>Template}),
    ct:log(String4),
    "othermfa" = String4,

    ok.
    
format_time(_Config) ->
    Time1 = timestamp(),
    ExpectedTimestamp1 = default_time_format(Time1),
    String1 = format(info,{"~p",[term]},#{time=>Time1},#{}),
    ct:log(String1),
    " info:\nterm\n" = string:prefix(String1,ExpectedTimestamp1),

    Time2 = timestamp(),
    ExpectedTimestamp2 = default_time_format(Time2,true),
    String2 = format(info,{"~p",[term]},#{time=>Time2},#{utc=>true}),
    ct:log(String2),
    " info:\nterm\n" = string:prefix(String2,ExpectedTimestamp2),

    application:set_env(kernel,logger_utc,true),
    Time3 = timestamp(),
    ExpectedTimestamp3 = default_time_format(Time3,true),
    String3 = format(info,{"~p",[term]},#{time=>Time3},#{}),
    ct:log(String3),
    " info:\nterm\n" = string:prefix(String3,ExpectedTimestamp3),

    ok.

format_time(cleanup,_Config) ->
    application:unset_env(kernel,logger_utc),
    ok.

level_or_msg_in_meta(_Config) ->
    %% The template contains atoms to pick out values from meta,
    %% or level/msg to add these from the log event. What if you have
    %% a key named 'level' or 'msg' in meta and want to display
    %% its value?
    %% For now we simply ignore Meta on this and display the
    %% actual level and msg from the log event.

    Meta = #{level=>mylevel,
                 msg=>"metamsg"},
    Template = [level,";",msg],
    String = format(info,{"~p",[term]},Meta,#{template=>Template}),
    ct:log(String),
    "info;term" = String, % so mylevel and "metamsg" are ignored
    
    ok.

faulty_log(_Config) ->
    %% Unexpected log (should be type logger:log()) - print error
    {error,
     function_clause,
     {logger_formatter,format,[_,_],_}} =
        ?TRY(logger_formatter:format(unexp_log,#{})),
    ok.

faulty_config(_Config) ->
    {error,
     function_clause,
     {logger_formatter,format,[_,_],_}} =
        ?TRY(logger_formatter:format(#{level=>info,
                                       msg=>{"~p",[term]},
                                       meta=>#{time=>timestamp()}},
                                     unexp_config)),
    ok.

faulty_msg(_Config) ->
    {error,
     function_clause,
     {logger_formatter,_,[_,_],_}} =
        ?TRY(logger_formatter:format(#{level=>info,
                                       msg=>term,
                                       meta=>#{time=>timestamp()}},
                                     #{})),
    ok.

%%%-----------------------------------------------------------------
%%% Internal
format(Level,Msg,Meta,Config) ->
    format(#{level=>Level,msg=>Msg,meta=>add_time(Meta)},Config).

format(Log,Config) ->
    lists:flatten(logger_formatter:format(Log,Config)).

default_time_format(Timestamp) ->
    default_time_format(Timestamp,false).

default_time_format(Timestamp0,Utc) when is_integer(Timestamp0) ->
    Timestamp=Timestamp0+erlang:time_offset(microsecond),
    %% calendar:system_time_to_rfc3339(Time,[{unit,microsecond}]).
    Micro = Timestamp rem 1000000,
    Sec = Timestamp div 1000000,
    UniversalTime =  erlang:posixtime_to_universaltime(Sec),
    {Date,Time} =
        if Utc -> UniversalTime;
           true -> erlang:universaltime_to_localtime(UniversalTime)
        end,
    default_time_format(Date,Time,Micro).

default_time_format({Y,M,D},{H,Min,S},Micro) ->
    lists:flatten(
      io_lib:format("~4w-~2..0w-~2..0w ~2w:~2..0w:~2..0w.~6..0w",
                    [Y,M,D,H,Min,S,Micro])).

integer(Str) ->
    is_integer(list_to_integer(Str)).
integer(Str,Max) ->
    integer(Str,0,Max).
integer(Str,Min,Max) ->
    Int = list_to_integer(Str),
    Int >= Min andalso Int =<Max.

%%%-----------------------------------------------------------------
%%% Called by macro ?TRY(X)
my_try(Fun) ->
    try Fun() catch C:R:S -> {C,R,hd(S)} end.

timestamp() ->
    erlang:monotonic_time(microsecond).

%% necessary?
add_time(#{time:=_}=Meta) ->
    Meta;
add_time(Meta) ->
    Meta#{time=>timestamp()}.
