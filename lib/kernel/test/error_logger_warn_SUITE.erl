%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(error_logger_warn_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 basic/1,warnings_info/1,warnings_warnings/1,
	 rb_basic/1,rb_warnings_info/1,rb_warnings_warnings/1,
	 rb_trunc/1,rb_utc/1,file_utc/1]).

%% Internal exports.
-export([init/1,handle_event/2,handle_info/2,handle_call/2]).

-include_lib("test_server/include/test_server.hrl").

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

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, warnings_info, warnings_warnings, rb_basic,
     rb_warnings_info, rb_warnings_warnings, rb_trunc,
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
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

basic(doc) ->
    ["Tests basic error logger functionality"];
basic(Config) when is_list(Config) ->
    put(elw_config,Config),
    basic().

warnings_info(doc) ->
    ["Tests mapping warnings to info functionality"];
warnings_info(Config) when is_list(Config) ->
    put(elw_config,Config),
    warnings_info().

warnings_warnings(doc) ->
    ["Tests mapping warnings to warnings functionality"];
warnings_warnings(Config) when is_list(Config) ->
    put(elw_config,Config),
    warnings_warnings().

rb_basic(doc) ->
    ["Tests basic rb functionality"];
rb_basic(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_basic().

rb_warnings_info(doc) ->
    ["Tests warnings as info rb functionality"];
rb_warnings_info(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_warnings_info().

rb_warnings_warnings(doc) ->
    ["Tests warnings as warnings rb functionality"];
rb_warnings_warnings(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_warnings_warnings().

rb_trunc(doc) ->
    ["Tests rb functionality on truncated data"];
rb_trunc(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_trunc().

rb_utc(doc) ->
    ["Tests UTC mapping in rb (-sasl utc_log true)"];
rb_utc(Config) when is_list(Config) ->
    put(elw_config,Config),
    rb_utc().

file_utc(doc) ->
    ["Tests UTC mapping in file logger (-stdlib utc_log true)"];
file_utc(Config) when is_list(Config) ->
    put(elw_config,Config),
    file_utc().


% a small gen_event

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
    ?line Node = start_node(nn(),[]),
    ?line ok = install_relay(Node),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line format(Node,"~p~n",[Self]),
    ?line ?EXPECT({handle_event,{error,GL,{_,"~p~n",[Self]}}}),
    ?line error_msg(Node,"~p~n",[Self]),
    ?line ?EXPECT({handle_event,{error,GL,{_,"~p~n",[Self]}}}),
    ?line warning_msg(Node,"~p~n",[Self]),
    ?line ?EXPECT({handle_event,{error,GL,{_,"~p~n",[Self]}}}),
    ?line info_msg(Node,"~p~n",[Self]),
    ?line ?EXPECT({handle_event,{info_msg,GL,{_,"~p~n",[Self]}}}),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line error_report(Node,Report),
    ?line ?EXPECT({handle_event,{error_report,GL,{_,std_error,Report}}}),
    ?line warning_report(Node,Report),
    ?line ?EXPECT({handle_event,{error_report,GL,{_,std_error,Report}}}),
    ?line info_report(Node,Report),
    ?line ?EXPECT({handle_event,{info_report,GL,{_,std_info,Report}}}),

    ?line stop_node(Node),
    ok.
    
warnings_info() ->
    ?line Node = start_node(nn(),"+Wi"),
    ?line ok = install_relay(Node),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line warning_msg(Node,"~p~n",[Self]),
    ?line ?EXPECT({handle_event,{info_msg,GL,{_,"~p~n",[Self]}}}),
    ?line warning_report(Node,Report),
    ?line ?EXPECT({handle_event,{info_report,GL,{_,std_info,Report}}}),
    ?line stop_node(Node),
    ok.

warnings_warnings() ->    
    ?line Node = start_node(nn(),"+Ww"),
    ?line ok = install_relay(Node),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line warning_msg(Node,"~p~n",[Self]),
    ?line ?EXPECT({handle_event,{warning_msg,GL,{_,"~p~n",[Self]}}}),
    ?line warning_report(Node,Report),
    ?line ?EXPECT({handle_event,{warning_report,GL,{_,std_warning,Report}}}),
    ?line stop_node(Node),
    ok.
    
    
% RB...

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

% Doesn't count empty lines
lines(File) ->
    length(
      string:tokens(
	binary_to_list(
	  element(2,file:read_file(File))),
	"\n")).

%directories anf filenames
ld() ->
    Config = get(elw_config),
    PrivDir = ?config(priv_dir, Config),
    filename:absname(PrivDir).

lf() ->
    filename:join([ld(),"logfile.txt"]).
rd() ->
    Config = get(elw_config),
    PrivDir = ?config(priv_dir, Config),
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

%clean out rd() before each report test in order to get only one file...
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

% Tests
rb_basic() ->
    ?line clean_rd(),
    % Behold, the magic parameters to activate rb logging...
    ?line Node = start_node(nn(),"-boot start_sasl -sasl error_logger_mf_dir "++
		      quote(rd())++" error_logger_mf_maxbytes 5000 "
		      "error_logger_mf_maxfiles 5"),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line fake_gl(Node,warning_msg,"~p~n",[Self]),
    ?line fake_gl(Node,warning_report,Report),
    ?line nice_stop_node(Node),
    ?line application:start(sasl),
    ?line rb:start([{report_dir, rd()}]),
    ?line rb:list(),
    ?line true = (one_rb_lines([error]) > 1),
    ?line true = (one_rb_lines([error_report]) > 1),
    ?line 1 = one_rb_findstr([error],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([error_report],pid_to_list(Self)),
    ?line 2 = one_rb_findstr([],pid_to_list(Self)),
    ?line true = (one_rb_findstr([progress],"===") > 4),
    ?line rb:stop(),
    ?line application:stop(sasl),
    ?line stop_node(Node),
    ok.

rb_warnings_info() ->
    ?line clean_rd(),
    ?line Node = start_node(nn(),"+W i -boot start_sasl -sasl error_logger_mf_dir "++
		      quote(rd())++" error_logger_mf_maxbytes 5000 "
		      "error_logger_mf_maxfiles 5"),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line fake_gl(Node,warning_msg,"~p~n",[Self]),
    ?line fake_gl(Node,warning_report,Report),
    ?line nice_stop_node(Node),
    ?line application:start(sasl),
    ?line rb:start([{report_dir, rd()}]),
    ?line rb:list(),
    ?line true = (one_rb_lines([error]) =:= 0),
    ?line true = (one_rb_lines([error_report]) =:= 0),
    ?line 0 = one_rb_findstr([error],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([error_report],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([warning_report],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([info_msg],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([info_report],pid_to_list(Self)),
    ?line 2 = one_rb_findstr([],pid_to_list(Self)),
    ?line true = (one_rb_findstr([progress],"===") > 4),
    ?line rb:stop(),
    ?line application:stop(sasl),
    ?line stop_node(Node),
    ok.

rb_warnings_warnings() ->
    ?line clean_rd(),
    ?line Node = start_node(nn(),"+W w -boot start_sasl -sasl error_logger_mf_dir "++
		      quote(rd())++" error_logger_mf_maxbytes 5000 "
		      "error_logger_mf_maxfiles 5"),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line fake_gl(Node,warning_msg,"~p~n",[Self]),
    ?line fake_gl(Node,warning_report,Report),
    ?line nice_stop_node(Node),
    ?line application:start(sasl),
    ?line rb:start([{report_dir, rd()}]),
    ?line rb:list(),
    ?line true = (one_rb_lines([error]) =:= 0),
    ?line true = (one_rb_lines([error_report]) =:= 0),
    ?line 0 = one_rb_findstr([error],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([error_report],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([warning_report],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([info_msg],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([info_report],pid_to_list(Self)),
    ?line 2 = one_rb_findstr([],pid_to_list(Self)),
    ?line true = (one_rb_findstr([progress],"===") > 4),
    ?line rb:stop(),
    ?line application:stop(sasl),
    ?line stop_node(Node),
    ok.

rb_trunc() ->
    ?line clean_rd(),
    ?line Node = start_node(nn(),"+W w -boot start_sasl -sasl error_logger_mf_dir "++
		      quote(rd())++" error_logger_mf_maxbytes 5000 "
		      "error_logger_mf_maxfiles 5"),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line fake_gl(Node,warning_msg,"~p~n",[Self]),
    ?line fake_gl(Node,warning_report,Report),
    ?line nice_stop_node(Node),
    ?line application:start(sasl),
    ?line {ok,File} = file:read_file(rf()),
    ?line S=byte_size(File)-2,
    ?line <<TFile:S/binary,_/binary>>=File,
    ?line file:write_file(rf(),TFile),
    ?line rb:start([{report_dir, rd()}]),
    ?line rb:list(),
    ?line true = (one_rb_lines([error]) =:= 0),
    ?line true = (one_rb_lines([error_report]) =:= 0),
    ?line 0 = one_rb_findstr([error],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([error_report],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([warning_msg],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([warning_report],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([info_msg],pid_to_list(Self)),
    ?line 0 = one_rb_findstr([info_report],pid_to_list(Self)),
    ?line 1 = one_rb_findstr([],pid_to_list(Self)),
    ?line true = (one_rb_findstr([progress],"===") > 4),
    ?line rb:stop(),
   ?line  application:stop(sasl),
    ?line stop_node(Node),
    ok.

rb_utc() ->
    ?line clean_rd(),
    ?line Node = start_node(nn(),"+W w -boot start_sasl -sasl error_logger_mf_dir "++
		      quote(rd())++" error_logger_mf_maxbytes 5000 "
		      "error_logger_mf_maxfiles 5 -sasl utc_log true"),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line fake_gl(Node,warning_msg,"~p~n",[Self]),
    ?line fake_gl(Node,warning_report,Report),
    ?line nice_stop_node(Node),
    ?line application:stop(sasl),
    ?line UtcLog=case application:get_env(sasl,utc_log) of
		     {ok,true} ->
			 true;
		     _AllOthers ->
			 application:set_env(sasl,utc_log,true),
			 false
	   end,
    ?line application:start(sasl),
    ?line rb:start([{report_dir, rd()}]),
    ?line rb:list(),
    ?line Pr=one_rb_findstr([progress],"==="),
    ?line Wm=one_rb_findstr([warning_msg],"==="),
    ?line Wr=one_rb_findstr([warning_report],"==="),
    ?line Sum=Pr+Wm+Wr,
    ?line Sum=one_rb_findstr([],"UTC"),
    ?line rb:stop(),
    ?line application:stop(sasl),
    ?line application:set_env(sasl,utc_log,UtcLog),
    ?line stop_node(Node),
    ok.
    
file_utc() ->
    ?line file:delete(lf()),
    ?line SS="+W w -stdlib utc_log true -kernel error_logger "++ oquote("{file,"++iquote(lf())++"}"),
    %erlang:display(SS),
    ?line Node = start_node(nn(),SS),
    %erlang:display(rpc:call(Node,application,get_env,[kernel,error_logger])),
    ?line Self = self(),
    ?line GL = group_leader(),
    ?line fake_gl(Node,error_msg,"~p~n",[Self]),
    ?line fake_gl(Node,warning_msg,"~p~n",[Self]),
    ?line fake_gl(Node,info_msg,"~p~n",[Self]),
    ?line Report = [{self,Self},{gl,GL},make_ref()],
    ?line fake_gl(Node,error_report,Report),
    ?line fake_gl(Node,warning_report,Report),
    ?line fake_gl(Node,info_report,Report),
    ?line nice_stop_node(Node),
    ?line receive after 5000 -> ok end, % Let the node die, needed
    ?line 6 = findstr("UTC",lf()),
    ?line 2 = findstr("WARNING",lf()),
    ?line 2 = findstr("ERROR",lf()),
    ?line 2 = findstr("INFO",lf()),
    ?line stop_node(Node),
    ok.
