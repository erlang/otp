%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
-module(error_logger_warn_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 basic/1,warnings_info/1,warnings_errors/1,
	 rb_basic/1,rb_warnings_info/1,rb_warnings_errors/1,
	 rb_trunc/1,rb_utc/1,file_utc/1]).

%% Internal exports.
-export([init/1,handle_event/2,handle_info/2,handle_call/2]).

-include_lib("common_test/include/ct.hrl").

-define(EXPECT(Pattern),
	(fun() ->
		 receive
		     Pattern = X ->
			 erlang:display({got_expected,?MODULE,?LINE,X}),
			 ok
		 after 5000 ->
			 exit({timeout_in_expect,?MODULE,?LINE})
		 end
	 end)()).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [basic, warnings_info, warnings_errors, rb_basic,
     rb_warnings_info, rb_warnings_errors, rb_trunc,
     rb_utc, file_utc].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%% Tests basic error logger functionality.
basic(Config) when is_list(Config) ->
    put(elw_config,Config),
    basic().

%% Tests mapping warnings to info functionality.
warnings_info(Config) when is_list(Config) ->
    put(elw_config,Config),
    warnings_info().

%% Tests mapping warnings to errors functionality.
warnings_errors(Config) when is_list(Config) ->
    put(elw_config,Config),
    warnings_errors().

%% Tests basic rb functionality.
rb_basic(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_basic().

%% Tests warnings as info rb functionality.
rb_warnings_info(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_warnings_info().

%% Tests warnings as errors rb functionality.
rb_warnings_errors(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_warnings_errors().

%% Tests rb functionality on truncated data.
rb_trunc(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_trunc().

%% Tests UTC mapping in rb (-sasl utc_log true).
rb_utc(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_utc().

%% Tests UTC mapping in file logger (-stdlib utc_log true).
file_utc(Config) when is_list(Config) ->
    put(elw_config,Config),
    file_utc().


%% a small gen_event

init([Pid]) ->
    {ok, Pid}.

handle_event(Event,Pid) ->
    Pid ! {handle_event,Event},
    {ok,Pid}.

handle_info(Unexpected,Pid) ->
    Pid ! {unexpected_info,Unexpected},
    {ok,Pid}.

handle_call(Unexpected, Pid) ->
    Pid ! {unexpected_call, Unexpected},
    {ok,Pid}.

start_node(Name,Args) ->
    MyDir = filename:dirname(code:which(?MODULE)),
    element(2,test_server:start_node(Name, slave, [{args, Args ++ " -pa " ++ MyDir}])).

stop_node(Name) ->
    test_server:stop_node(Name).

install_relay(Node) ->
    rpc:call(Node,error_logger,add_report_handler,[?MODULE,[self()]]).


warning_map(Node) ->
    rpc:call(Node,error_logger,warning_map,[]).

format(Node,A,B) ->
    rpc:call(Node,error_logger,format,[A,B]).
error_msg(Node,A,B) ->
    rpc:call(Node,error_logger,error_msg,[A,B]).
error_report(Node,B) ->
    rpc:call(Node,error_logger,error_report,[B]).
warning_msg(Node,A,B) ->
    rpc:call(Node,error_logger,warning_msg,[A,B]).
warning_report(Node,B) ->
    rpc:call(Node,error_logger,warning_report,[B]).
info_msg(Node,A,B) ->
    rpc:call(Node,error_logger,info_msg,[A,B]).
info_report(Node,B) ->
    rpc:call(Node,error_logger,info_report,[B]).

nn() ->
    error_logger_warn_suite_helper.



    
basic() ->
    Node = start_node(nn(),[]),
    ok = install_relay(Node),
    Self = self(),
    GL = group_leader(),
    warning = warning_map(Node),
    format(Node,"~p~n",[Self]),
    ?EXPECT({handle_event,{error,GL,{_,"~p~n",[Self]}}}),
    error_msg(Node,"~p~n",[Self]),
    ?EXPECT({handle_event,{error,GL,{_,"~p~n",[Self]}}}),
    warning_msg(Node,"~p~n",[Self]),
    ?EXPECT({handle_event,{warning_msg,GL,{_,"~p~n",[Self]}}}),
    info_msg(Node,"~p~n",[Self]),
    ?EXPECT({handle_event,{info_msg,GL,{_,"~p~n",[Self]}}}),
    Report = [{self,Self},{gl,GL},make_ref()],
    error_report(Node,Report),
    ?EXPECT({handle_event,{error_report,GL,{_,std_error,Report}}}),
    warning_report(Node,Report),
    ?EXPECT({handle_event,{warning_report,GL,{_,std_warning,Report}}}),
    info_report(Node,Report),
    ?EXPECT({handle_event,{info_report,GL,{_,std_info,Report}}}),
    stop_node(Node),
    ok.
    
warnings_info() ->
    Node = start_node(nn(),"+Wi"),
    ok = install_relay(Node),
    Self = self(),
    GL = group_leader(),
    info = warning_map(Node),
    Report = [{self,Self},{gl,GL},make_ref()],
    warning_msg(Node,"~p~n",[Self]),
    ?EXPECT({handle_event,{info_msg,GL,{_,"~p~n",[Self]}}}),
    warning_report(Node,Report),
    ?EXPECT({handle_event,{info_report,GL,{_,std_info,Report}}}),
    stop_node(Node),
    ok.

warnings_errors() ->    
    Node = start_node(nn(),"+We"),
    ok = install_relay(Node),
    Self = self(),
    GL = group_leader(),
    error = warning_map(Node),
    Report = [{self,Self},{gl,GL},make_ref()],
    warning_msg(Node,"~p~n",[Self]),
    ?EXPECT({handle_event,{error,GL,{_,"~p~n",[Self]}}}),
    warning_report(Node,Report),
    ?EXPECT({handle_event,{error_report,GL,{_,std_error,Report}}}),
    stop_node(Node),
    ok.

%% RB...

quote(String) ->
    case os:type() of
        {win32,_} ->
            "\\\""++String++"\\\"";
        _ ->
            "'\""++String++"\"'"
    end.

iquote(String) ->
    case os:type() of
        {win32,_} ->
            "\\\""++String++"\\\"";
        _ ->
            "\""++String++"\""
    end.

oquote(String) ->
    case os:type() of
        {win32,_} ->
            "\""++String++"\"";
        _ ->
            "'"++String++"'"
    end.
    

findstr(String,FileName) ->
    File=binary_to_list(element(2,file:read_file(FileName))),
    findstrc(String,File).

findstrc(String,File) ->
    case string:str(File,String) of
        N when is_integer(N),
               N > 0 ->
            S2 = lists:sublist(File,N,length(File)),
            case string:str(S2,"\n") of
                0 ->
                    1;
                M ->
                    S3 = lists:sublist(S2,M,length(S2)),
                    1 + findstrc(String,S3)
            end;
        _ ->
            0
    end.

%% Doesn't count empty lines
lines(File) ->
    length(
      string:tokens(
	binary_to_list(
	  element(2,file:read_file(File))),
	"\n")).

%% Directories and filenames
ld() ->
    Config = get(elw_config),
    PrivDir = proplists:get_value(priv_dir, Config),
    filename:absname(PrivDir).

lf() ->
    filename:join([ld(),"logfile.txt"]).
rd() ->
    Config = get(elw_config),
    PrivDir = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(PrivDir,"log"),
    file:make_dir(LogDir),
    filename:absname(LogDir).
rf() ->
    filename:join([rd(),"1"]).

nice_stop_node(Name) ->
    erlang:monitor_node(Name, true),
    rpc:call(Name, init, stop, []),
    receive
	{nodedown,Name} -> ok
    end.

%% Clean out rd() before each report test in order to get only one file...
clean_rd() ->
    {ok,L} = file:list_dir(rd()),
    lists:foreach(fun(F) ->
			  file:delete(F)
		  end,
		  [filename:append(rd(),X) || X <- L]),
    ok.

fake_gl(Node,What,A) ->
    Fun = fun() -> 
		   group_leader(self(),self()),
		   error_logger:What(A)
	   end,
    rpc:call(Node,erlang,apply,[Fun,[]]).
fake_gl(Node,What,A,B) ->
    Fun = fun() -> 
		   group_leader(self(),self()),
		   error_logger:What(A,B)
	   end,
    rpc:call(Node,erlang,apply,[Fun,[]]).


one_rb_lines(Param) ->
    file:delete(lf()),
    rb:start_log(lf()),
    apply(rb,show,Param),
    rb:stop_log(),
    lines(lf()).

one_rb_findstr(Param,String) ->
    file:delete(lf()),
    rb:start_log(lf()),
    apply(rb,show,Param),
    rb:stop_log(),
    findstr(String,lf()).

%% Tests
rb_basic() ->
    clean_rd(),
    %% Behold, the magic parameters to activate rb logging...
    Node = start_node(nn(),"-boot start_sasl -sasl error_logger_mf_dir "++
                      quote(rd())++" error_logger_mf_maxbytes 5000 "
                      "error_logger_mf_maxfiles 5"),
    Self = self(),
    GL = group_leader(),
    warning = warning_map(Node),
    Report = [{self,Self},{gl,GL},make_ref()],
    fake_gl(Node,warning_msg,"~p~n",[Self]),
    fake_gl(Node,warning_report,Report),
    nice_stop_node(Node),
    application:start(sasl),
    rb:start([{report_dir, rd()}]),
    rb:list(),
    true = (one_rb_lines([error]) =:= 0),
    true = (one_rb_lines([error_report]) =:= 0),
    0 = one_rb_findstr([error],pid_to_list(Self)),
    0 = one_rb_findstr([error_report],pid_to_list(Self)),
    1 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    1 = one_rb_findstr([warning_report],pid_to_list(Self)),
    0 = one_rb_findstr([info_msg],pid_to_list(Self)),
    0 = one_rb_findstr([info_report],pid_to_list(Self)),
    2 = one_rb_findstr([],pid_to_list(Self)),
    true = (one_rb_findstr([progress],"===") > 3),
    rb:stop(),
    application:stop(sasl),
    stop_node(Node),
    ok.

rb_warnings_info() ->
    clean_rd(),
    Node = start_node(nn(),"+W i -boot start_sasl -sasl error_logger_mf_dir "++
                      quote(rd())++" error_logger_mf_maxbytes 5000 "
                      "error_logger_mf_maxfiles 5"),
    Self = self(),
    GL = group_leader(),
    info = warning_map(Node),
    Report = [{self,Self},{gl,GL},make_ref()],
    fake_gl(Node,warning_msg,"~p~n",[Self]),
    fake_gl(Node,warning_report,Report),
    nice_stop_node(Node),
    application:start(sasl),
    rb:start([{report_dir, rd()}]),
    rb:list(),
    true = (one_rb_lines([error]) =:= 0),
    true = (one_rb_lines([error_report]) =:= 0),
    0 = one_rb_findstr([error],pid_to_list(Self)),
    0 = one_rb_findstr([error_report],pid_to_list(Self)),
    0 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    0 = one_rb_findstr([warning_report],pid_to_list(Self)),
    1 = one_rb_findstr([info_msg],pid_to_list(Self)),
    1 = one_rb_findstr([info_report],pid_to_list(Self)),
    2 = one_rb_findstr([],pid_to_list(Self)),
    true = (one_rb_findstr([progress],"===") > 3),
    rb:stop(),
    application:stop(sasl),
    stop_node(Node),
    ok.

rb_warnings_errors() ->
    clean_rd(),
    Node = start_node(nn(),"+W e -boot start_sasl -sasl error_logger_mf_dir "++
                      quote(rd())++" error_logger_mf_maxbytes 5000 "
                      "error_logger_mf_maxfiles 5"),
    Self = self(),
    GL = group_leader(),
    error = warning_map(Node),
    Report = [{self,Self},{gl,GL},make_ref()],
    fake_gl(Node,warning_msg,"~p~n",[Self]),
    fake_gl(Node,warning_report,Report),
    nice_stop_node(Node),
    application:start(sasl),
    rb:start([{report_dir, rd()}]),
    rb:list(),
    true = (one_rb_lines([error]) > 1),
    true = (one_rb_lines([error_report]) > 1),
    1 = one_rb_findstr([error],pid_to_list(Self)),
    1 = one_rb_findstr([error_report],pid_to_list(Self)),
    0 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    0 = one_rb_findstr([warning_report],pid_to_list(Self)),
    0 = one_rb_findstr([info_msg],pid_to_list(Self)),
    0 = one_rb_findstr([info_report],pid_to_list(Self)),
    2 = one_rb_findstr([],pid_to_list(Self)),
    true = (one_rb_findstr([progress],"===") > 3),
    rb:stop(),
    application:stop(sasl),
    stop_node(Node),
    ok.

rb_trunc() ->
    clean_rd(),
    Node = start_node(nn(),"-boot start_sasl -sasl error_logger_mf_dir "++
                      quote(rd())++" error_logger_mf_maxbytes 5000 "
                      "error_logger_mf_maxfiles 5"),
    Self = self(),
    GL = group_leader(),
    Report = [{self,Self},{gl,GL},make_ref()],
    fake_gl(Node,warning_msg,"~p~n",[Self]),
    fake_gl(Node,warning_report,Report),
    nice_stop_node(Node),
    application:start(sasl),
    {ok,File} = file:read_file(rf()),
    S=byte_size(File)-2,
    <<TFile:S/binary,_/binary>>=File,
    file:write_file(rf(),TFile),
    rb:start([{report_dir, rd()}]),
    rb:list(),
    true = (one_rb_lines([error]) =:= 0),
    true = (one_rb_lines([error_report]) =:= 0),
    0 = one_rb_findstr([error],pid_to_list(Self)),
    0 = one_rb_findstr([error_report],pid_to_list(Self)),
    1 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    0 = one_rb_findstr([warning_report],pid_to_list(Self)),
    0 = one_rb_findstr([info_msg],pid_to_list(Self)),
    0 = one_rb_findstr([info_report],pid_to_list(Self)),
    1 = one_rb_findstr([],pid_to_list(Self)),
    true = (one_rb_findstr([progress],"===") > 3),
    rb:stop(),
    application:stop(sasl),
    stop_node(Node),
    ok.

rb_utc() ->
    clean_rd(),
    Node = start_node(nn(),"-boot start_sasl -sasl error_logger_mf_dir "++
                      quote(rd())++" error_logger_mf_maxbytes 5000 "
                      "error_logger_mf_maxfiles 5 -sasl utc_log true"),
    Self = self(),
    GL = group_leader(),
    Report = [{self,Self},{gl,GL},make_ref()],
    fake_gl(Node,warning_msg,"~p~n",[Self]),
    fake_gl(Node,warning_report,Report),
    nice_stop_node(Node),
    application:stop(sasl),
    UtcLog=case application:get_env(sasl,utc_log) of
               {ok,true} ->
                   true;
               _AllOthers ->
                   application:set_env(sasl,utc_log,true),
                   false
           end,
    application:start(sasl),
    rb:start([{report_dir, rd()}]),
    rb:list(),
    Pr=one_rb_findstr([progress],"==="),
    Wm=one_rb_findstr([warning_msg],"==="),
    Wr=one_rb_findstr([warning_report],"==="),
    Sum=Pr+Wm+Wr,
    Sum=one_rb_findstr([],"UTC"),
    rb:stop(),
    application:stop(sasl),
    application:set_env(sasl,utc_log,UtcLog),
    stop_node(Node),
    ok.
    
file_utc() ->
    file:delete(lf()),
    SS="-stdlib utc_log true -kernel error_logger "++ oquote("{file,"++iquote(lf())++"}"),
    Node = start_node(nn(),SS),
    Self = self(),
    GL = group_leader(),
    fake_gl(Node,error_msg,"~p~n",[Self]),
    fake_gl(Node,warning_msg,"~p~n",[Self]),
    fake_gl(Node,info_msg,"~p~n",[Self]),
    Report = [{self,Self},{gl,GL},make_ref()],
    fake_gl(Node,error_report,Report),
    fake_gl(Node,warning_report,Report),
    fake_gl(Node,info_report,Report),
    nice_stop_node(Node),
    receive after 5000 -> ok end, % Let the node die, needed
    6 = findstr("UTC",lf()),
    2 = findstr("WARNING",lf()),
    2 = findstr("ERROR",lf()),
    2 = findstr("INFO",lf()),
    stop_node(Node),
    ok.
