%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2011. All Rights Reserved.
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

%%% @doc Logging functionality for Common Test Framework.
%%%
%%% <p>This module implements
%%% <ul>
%%% <li>Internal logging of activities in Common Test Framework</li>
%%% <li>Compilation of test results into index pages on several levels</li>
%%% </ul>
%%% </p>

-module(ct_logs).

-export([init/1,close/2,init_tc/1,end_tc/1]).
-export([get_log_dir/0,get_log_dir/1]).
-export([log/3,start_log/1,cont_log/2,end_log/0]).
-export([set_stylesheet/2,clear_stylesheet/1]).
-export([add_external_logs/1,add_link/3]).
-export([make_last_run_index/0]).
-export([make_all_suites_index/1,make_all_runs_index/1]).
-export([get_ts_html_wrapper/3]).

%% Logging stuff directly from testcase
-export([tc_log/3,tc_print/3,tc_pal/3,ct_log/3,
	 basic_html/0]).

%% Simulate logger process for use without ct environment running
-export([simulate/0]).

-include("ct_event.hrl").
-include("ct_util.hrl").
-include_lib("kernel/include/file.hrl").

-define(suitelog_name,"suite.log").
-define(run_summary, "suite.summary").
-define(logdir_ext, ".logs").
-define(ct_log_name, "ctlog.html").
-define(all_runs_name, "all_runs.html").
-define(index_name, "index.html").
-define(totals_name, "totals.info").
-define(css_default, "ct_default.css").

-define(table_color1,"#ADD8E6").
-define(table_color2,"#E4F0FE").
-define(table_color3,"#F0F8FF").

-define(testname_width, 60).

-define(abs(Name), filename:absname(Name)).

%%%-----------------------------------------------------------------
%%% @spec init(Mode) -> Result
%%%   Mode = normal | interactive
%%%   Result = {StartTime,LogDir}
%%%   StartTime = term()
%%%   LogDir = string()
%%%
%%% @doc Initiate the logging mechanism (tool-internal use only).
%%%
%%% <p>This function is called by ct_util.erl when testing is
%%% started. A new directory named ct_run.&lt;timestamp&gt; is created
%%% and all logs are stored under this directory.</p>
%%%
init(Mode) ->
    Self = self(),
    Pid = spawn_link(fun() -> logger(Self,Mode) end),
    MRef = erlang:monitor(process,Pid),
    receive 
	{started,Pid,Result} -> 
	    erlang:demonitor(MRef, [flush]),
	    Result;
	{'DOWN',MRef,process,_,Reason} ->
	    exit({could_not_start_process,?MODULE,Reason})
    end.
    
make_dirname({{YY,MM,DD},{H,M,S}}) -> 
    io_lib:format(logdir_node_prefix()++".~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w",
		  [YY,MM,DD,H,M,S]).

logdir_prefix() ->
    "ct_run".
logdir_node_prefix() ->
    logdir_prefix()++"."++atom_to_list(node()).

%%%-----------------------------------------------------------------
%%% @spec close(Info, StartDir) -> ok
%%%
%%% @doc Create index pages with test results and close the CT Log
%%% (tool-internal use only).
close(Info, StartDir) ->
    make_last_run_index(),

    ct_event:notify(#event{name=stop_logging,node=node(),data=[]}),

    case whereis(?MODULE) of
	Pid when is_pid(Pid) ->
	    MRef = erlang:monitor(process,Pid),
	    ?MODULE ! stop,
	    receive
		{'DOWN',MRef,process,_,_} ->
		    ok
	    end;
	undefined ->
	    ok
    end,

    if Info == clean ->
	    case cleanup() of
		ok ->
		    ok;
		Error ->
		    io:format("Warning! Cleanup failed: ~p~n", [Error])
	    end,
	    make_all_suites_index(stop),
	    make_all_runs_index(stop);
       true -> 
	    file:set_cwd(".."),
	    make_all_suites_index(stop),
	    make_all_runs_index(stop),
	    case ct_util:get_profile_data(browser, StartDir) of
		undefined ->
		    ok;
		BrowserData ->
		    case {proplists:get_value(prog, BrowserData),
			  proplists:get_value(args, BrowserData),
			  proplists:get_value(page, BrowserData)} of
			{Prog,Args,Page} when is_list(Args),
					      is_list(Page) ->
			    URL = "\"file://" ++ ?abs(Page) ++ "\"",
			    ct_util:open_url(Prog, Args, URL);
			_ ->
			    ok
		    end
	    end
    end,
    ok.

%%%-----------------------------------------------------------------
%%% @spec set_stylesheet(TC,SSFile) -> ok
set_stylesheet(TC, SSFile) ->
    cast({set_stylesheet,TC,SSFile}).

%%%-----------------------------------------------------------------
%%% @spec clear_stylesheet(TC) -> ok
clear_stylesheet(TC) ->
    cast({clear_stylesheet,TC}).

%%%-----------------------------------------------------------------
%%% @spec get_log_dir() -> {ok,Dir} | {error,Reason}
get_log_dir() ->
    call({get_log_dir,false}).

%%%-----------------------------------------------------------------
%%% @spec get_log_dir(ReturnAbsName) -> {ok,Dir} | {error,Reason}
get_log_dir(ReturnAbsName) ->
    call({get_log_dir,ReturnAbsName}).

%%%-----------------------------------------------------------------
%%% make_last_run_index() -> ok
make_last_run_index() ->
    call(make_last_run_index).

call(Msg) ->
    case whereis(?MODULE) of
	undefined ->
	    {error,does_not_exist};
	Pid ->
	    MRef = erlang:monitor(process,Pid),
	    Ref = make_ref(),
	    ?MODULE ! {Msg,{self(),Ref}},
	    receive
		{Ref, Result} -> 
		    erlang:demonitor(MRef, [flush]),
		    Result;
		{'DOWN',MRef,process,_,Reason}  -> 
		    {error,{process_down,?MODULE,Reason}}
	    end
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result}.

cast(Msg) ->
    case whereis(?MODULE) of
	undefined ->
	    {error,does_not_exist};
	_Pid ->
	    ?MODULE ! Msg
    end.

%%%-----------------------------------------------------------------
%%% @spec init_tc(RefreshLog) -> ok
%%%
%%% @doc Test case initiation (tool-internal use only).
%%%
%%% <p>This function is called by ct_framework:init_tc/3</p>
init_tc(RefreshLog) ->
    call({init_tc,self(),group_leader(),RefreshLog}),
    io:format(xhtml("", "<br />")),
    ok.

%%%-----------------------------------------------------------------
%%% @spec end_tc(TCPid) -> ok | {error,Reason}
%%%
%%% @doc Test case clean up (tool-internal use only).
%%%
%%% <p>This function is called by ct_framework:end_tc/3</p>
end_tc(TCPid) ->
    io:format(xhtml("<br>", "<br />")),
    %% use call here so that the TC process will wait and receive
    %% possible exit signals from ct_logs before end_tc returns ok 
    call({end_tc,TCPid}).

%%%-----------------------------------------------------------------
%%% @spec log(Heading,Format,Args) -> ok
%%%
%%% @doc Log internal activity (tool-internal use only).
%%%
%%% <p>This function writes an entry to the currently active log,
%%% i.e. either the CT log or a test case log.</p>
%%%
%%% <p><code>Heading</code> is a short string indicating what type of
%%% activity it is. <code>Format</code> and <code>Args</code> is the
%%% data to log (as in <code>io:format(Format,Args)</code>).</p>
log(Heading,Format,Args) ->
    cast({log,self(),group_leader(),
	  [{int_header(),[log_timestamp(now()),Heading]},
	   {Format,Args},
	   {int_footer(),[]}]}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec start_log(Heading) -> ok
%%% 
%%% @doc Starts the logging of an activity (tool-internal use only).
%%%
%%% <p>This function must be used in combination with
%%% <code>cont_log/2</code> and <code>end_log/0</code>. The intention
%%% is to call <code>start_log</code> once, then <code>cont_log</code>
%%% any number of times and finally <code>end_log</code> once.</p>
%%% 
%%% <p>For information about the parameters, see <code>log/3</code>.</p>
%%%
%%% @see log/3
%%% @see cont_log/2
%%% @see end_log/0
start_log(Heading) ->
    cast({log,self(),group_leader(),
	  [{int_header(),[log_timestamp(now()),Heading]}]}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec cont_log(Format,Args) -> ok
%%% 
%%% @doc Adds information about an activity (tool-internal use only).
%%%
%%% @see start_log/1
%%% @see end_log/0
cont_log([],[]) ->
    ok;
cont_log(Format,Args) ->
    maybe_log_timestamp(),
    cast({log,self(),group_leader(),[{Format,Args}]}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec end_log() -> ok
%%% 
%%% @doc Ends the logging of an activity (tool-internal use only).
%%%
%%% @see start_log/1
%%% @see cont_log/2
end_log() ->
    cast({log,self(),group_leader(),[{int_footer(), []}]}),
    ok.
    

%%%-----------------------------------------------------------------
%%% @spec add_external_logs(Logs) -> ok
%%%      Logs = [Log]
%%%      Log = string()
%%%
%%% @doc Print a link to each given <code>Log</code> in the test case
%%% log.
%%%
%%% <p>The given <code>Logs</code> must exist in the priv dir of the
%%% calling test suite.</p>
add_external_logs(Logs) ->
    start_log("External Logs"),
    [cont_log("<a href=~p>~s</a>\n",
	      [filename:join("log_private",Log),Log]) || Log <- Logs],
    end_log().

%%%-----------------------------------------------------------------
%%% @spec add_link(Heading,File,Type) -> ok
%%%      Heading = string()
%%%      File = string()
%%%      Type = string()
%%%
%%% @doc Print a link to a given file stored in the priv_dir of the
%%% calling test suite.
add_link(Heading,File,Type) ->
    log(Heading,"<a href=~p type=~p>~s</a>\n",
	[filename:join("log_private",File),Type,File]).



%%%-----------------------------------------------------------------
%%% @spec tc_log(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Printout from a testcase. 
%%%
%%% <p>This function is called by <code>ct</code> when logging
%%% stuff directly from a testcase (i.e. not from within the CT
%%% framework).</p>
tc_log(Category,Format,Args) ->
    cast({log,self(),group_leader(),[{div_header(Category),[]},
				     {Format,Args},
				     {div_footer(),[]}]}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec tc_print(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Console printout from a testcase. 
%%%
%%% <p>This function is called by <code>ct</code> when printing
%%% stuff a testcase on the user console.</p>
tc_print(Category,Format,Args) ->
    print_heading(Category),
    io:format(user,Format,Args),
    io:format(user,"\n\n",[]),
    ok.

print_heading(default) ->
    io:format(user,
	      "----------------------------------------------------\n~s\n",
	      [log_timestamp(now())]);
print_heading(Category) ->
    io:format(user,
	      "----------------------------------------------------\n~s  ~w\n",
	      [log_timestamp(now()),Category]).    
    

%%%-----------------------------------------------------------------
%%% @spec tc_pal(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Print and log from a testcase. 
%%%
%%% <p>This function is called by <code>ct</code> when logging
%%% stuff directly from a testcase. The info is written both in the
%%% log and on the console.</p>
tc_pal(Category,Format,Args) ->
    tc_print(Category,Format,Args),
    cast({log,self(),group_leader(),[{div_header(Category),[]},
				     {Format,Args},
				     {div_footer(),[]}]}),
    ok.


%%%-----------------------------------------------------------------
%%% @spec tc_pal(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Print and log to the ct framework log
%%%
%%% <p>This function is called by internal ct functions to
%%% force logging to the ct framework log</p>
ct_log(Category,Format,Args) ->
    cast({ct_log,[{div_header(Category),[]},
		  {Format,Args},
		  {div_footer(),[]}]}),
    ok.


%%%=================================================================
%%% Internal functions
int_header() ->
    "<div class=\"ct_internal\"><b>*** CT ~s *** ~s</b>".
int_footer() ->
    "</div>".

div_header(Class) ->
    "<div class=\"" ++ atom_to_list(Class) ++ "\"><b>*** User  " ++   
	log_timestamp(now()) ++ " ***</b>".
div_footer() ->
    "</div>".


maybe_log_timestamp() ->
    {MS,S,US} = now(),
    case get(log_timestamp) of
	{MS,S,_} ->
	    ok;
	_ ->
	    cast({log,self(),group_leader(),
		  [{"<i>~s</i>",[log_timestamp({MS,S,US})]}]})
    end.

log_timestamp({MS,S,US}) ->
    put(log_timestamp, {MS,S,US}),
    {{Year,Month,Day}, {Hour,Min,Sec}} =
        calendar:now_to_local_time({MS,S,US}),
    MilliSec = trunc(US/1000),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year,Month,Day,Hour,Min,Sec,MilliSec])).

%%%-----------------------------------------------------------------
%%% The logger server
-record(logger_state,{parent,
		      log_dir,
		      start_time,
		      orig_GL,
		      ct_log_fd,
		      tc_groupleaders,
		      stylesheet}).

logger(Parent,Mode) ->
    register(?MODULE,self()),

    %%! Below is a temporary workaround for the limitation of
    %%! max one test run per second. 
    %%! --->
    Time0 = calendar:local_time(),
    Dir0 = make_dirname(Time0),
    {Time,Dir} = 
	case filelib:is_dir(Dir0) of
	    true ->
		timer:sleep(1000),
		Time1 = calendar:local_time(),
		Dir1 = make_dirname(Time1),
		{Time1,Dir1};
	    false ->
		{Time0,Dir0}
	end,
    %%! <---

    file:make_dir(Dir),
    AbsDir = ?abs(Dir),
    put(ct_run_dir, AbsDir),

    case basic_html() of
	true ->
	    put(basic_html, true);
	BasicHtml ->
	    put(basic_html, BasicHtml),
	    %% copy stylesheet to log dir (both top dir and test run
	    %% dir) so logs are independent of Common Test installation
	    {ok,Cwd} = file:get_cwd(),
	    CTPath = code:lib_dir(common_test),
	    CSSFileSrc = filename:join(filename:join(CTPath, "priv"),
				       ?css_default),
	    CSSFileDestTop = filename:join(Cwd, ?css_default),
	    CSSFileDestRun = filename:join(AbsDir, ?css_default),
	    case file:copy(CSSFileSrc, CSSFileDestTop) of
		{error,Reason0} ->
		    io:format(user, "ERROR! "++
			      "CSS file ~p could not be copied to ~p. "++
			      "Reason: ~p~n",
			      [CSSFileSrc,CSSFileDestTop,Reason0]),
		    exit({css_file_error,CSSFileDestTop});
		_ ->
		    case file:copy(CSSFileSrc, CSSFileDestRun) of
			{error,Reason1} ->
			    io:format(user, "ERROR! "++
				      "CSS file ~p could not be copied to ~p. "++
				      "Reason: ~p~n",
				      [CSSFileSrc,CSSFileDestRun,Reason1]),
			    exit({css_file_error,CSSFileDestRun});
			_ ->
			    ok
		    end
	    end
    end,
    ct_event:notify(#event{name=start_logging,node=node(),
			   data=AbsDir}),
    make_all_runs_index(start),
    make_all_suites_index(start),
    case Mode of
	interactive -> interactive_link();
	_ -> ok
    end,
    file:set_cwd(Dir),
    make_last_run_index(Time),
    CtLogFd = open_ctlog(),
    io:format(CtLogFd,int_header()++int_footer(),
	      [log_timestamp(now()),"Common Test Logger started"]),
    Parent ! {started,self(),{Time,filename:absname("")}},
    set_evmgr_gl(CtLogFd),
    logger_loop(#logger_state{parent=Parent,
			      log_dir=AbsDir,
			      start_time=Time,
			      orig_GL=group_leader(),
			      ct_log_fd=CtLogFd,
			      tc_groupleaders=[]}).

logger_loop(State) ->
    receive
	{log,Pid,GL,List} ->
	    case get_groupleader(Pid,GL,State) of
		{tc_log,TCGL,TCGLs} ->
		    case erlang:is_process_alive(TCGL) of
			true ->
			    %% we have to build one io-list of all strings
			    %% before printing, or other io printouts (made in
			    %% parallel) may get printed between this header 
			    %% and footer 
			    Fun = 
				fun({Str,Args},IoList) ->
					case catch io_lib:format(Str,Args) of
					    {'EXIT',_Reason} ->
						Fd = State#logger_state.ct_log_fd,
						io:format(Fd, 
							  "Logging fails! "
							  "Str: ~p, Args: ~p~n",
							  [Str,Args]),
						%% stop the testcase, we need
						%% to see the fault
						exit(Pid,{log_printout_error,Str,Args}),
						[];
					    IoStr when IoList == [] ->
						[IoStr];
					    IoStr ->
						[IoList,"\n",IoStr]
					end
				end,
			    io:format(TCGL,"~s",[lists:foldl(Fun,[],List)]),
			    logger_loop(State#logger_state{tc_groupleaders=TCGLs});
			false ->
			    %% Group leader is dead, so write to the CtLog instead
			    Fd = State#logger_state.ct_log_fd,
			    [begin io:format(Fd,Str,Args),io:nl(Fd) end || 
				{Str,Args} <- List],
			    logger_loop(State)			    
		    end;
		{ct_log,Fd,TCGLs} ->
		    [begin io:format(Fd,Str,Args),io:nl(Fd) end || {Str,Args} <- List],
		    logger_loop(State#logger_state{tc_groupleaders=TCGLs})
	    end;
	{{init_tc,TCPid,GL,RefreshLog},From} ->
	    print_style(GL, State#logger_state.stylesheet),
	    set_evmgr_gl(GL),
	    TCGLs = add_tc_gl(TCPid,GL,State),
	    if not RefreshLog ->
		    ok;
	       true ->
		    make_last_run_index(State#logger_state.start_time)
	    end,
	    return(From,ok),
	    logger_loop(State#logger_state{tc_groupleaders=TCGLs});
	{{end_tc,TCPid},From} ->
	    set_evmgr_gl(State#logger_state.ct_log_fd),
	    return(From,ok),
	    logger_loop(State#logger_state{tc_groupleaders=rm_tc_gl(TCPid,State)});
	{{get_log_dir,true},From} ->
	    return(From,{ok,State#logger_state.log_dir}),
	    logger_loop(State);
	{{get_log_dir,false},From} ->
	    return(From,{ok,filename:basename(State#logger_state.log_dir)}),
	    logger_loop(State);
	{make_last_run_index,From} ->
	    make_last_run_index(State#logger_state.start_time),
	    return(From,filename:basename(State#logger_state.log_dir)),
	    logger_loop(State);
	{set_stylesheet,_,SSFile} when State#logger_state.stylesheet == SSFile ->
	    logger_loop(State);
	{set_stylesheet,TC,SSFile} ->
	    Fd = State#logger_state.ct_log_fd,
	    io:format(Fd, "~p loading external style sheet: ~s~n", [TC,SSFile]),
	    logger_loop(State#logger_state{stylesheet=SSFile});
	{clear_stylesheet,_} when State#logger_state.stylesheet == undefined ->
	    logger_loop(State);
	{clear_stylesheet,_} ->
	    logger_loop(State#logger_state{stylesheet=undefined});
	{ct_log, List} ->
	    Fd = State#logger_state.ct_log_fd,
	    [begin io:format(Fd,Str,Args),io:nl(Fd) end ||
				{Str,Args} <- List],
	    logger_loop(State);
	stop ->
	    io:format(State#logger_state.ct_log_fd,
		      int_header()++int_footer(),
		      [log_timestamp(now()),"Common Test Logger finished"]),
	    close_ctlog(State#logger_state.ct_log_fd),
	    ok
    end.

%% #logger_state.tc_groupleaders == [{Pid,{Type,GLPid}},...]
%% Type = tc | io
%%
%% Pid can either be a test case process (tc), an IO process (io)
%% spawned by a test case process, or a common test process (never
%% registered by an init_tc msg). An IO process gets registered the
%% first time it sends data and will be stored in the list until the
%% last TC process associated with the same group leader gets 
%% unregistered.
%%
%% If a process that has not been spawned by a test case process
%% sends a log request, the data will be printed to a test case
%% log file *if* there exists one registered process only in the 
%% tc_groupleaders list. If multiple test case processes are
%% running, the data gets printed to the CT framework log instead. 
%%
%% Note that an external process must not be registered as an IO
%% process since it could then accidentally be associated with
%% the first test case process that starts in a group of parallel
%% cases (if the log request would come in between the registration
%% of the first and second test case process).

get_groupleader(Pid,GL,State) ->
    TCGLs = State#logger_state.tc_groupleaders,
    %% check if Pid is registered either as a TC or IO process
    case proplists:get_value(Pid,TCGLs) of
	undefined ->
	    %% this could be a process spawned by the test case process,
	    %% if so they have the same registered group leader
	    case lists:keysearch({tc,GL},2,TCGLs) of
		{value,_} ->
		    %% register the io process
		    {tc_log,GL,[{Pid,{io,GL}}|TCGLs]};
		false ->
		    %% check if only one test case is executing,
		    %% if so return the group leader for it
		    case [TCGL || {_,{Type,TCGL}} <- TCGLs, Type == tc] of
			[TCGL] ->
			    %% an external process sending the log
			    %% request, don't register
			    {tc_log,TCGL,TCGLs};
			_ ->
			    {ct_log,State#logger_state.ct_log_fd,TCGLs}
		    end
	    end;
	{_,GL} ->
	    {tc_log,GL,TCGLs};
	_ ->
	    %% special case where a test case io process has changed
	    %% its group leader to an non-registered GL process
	    TCGLs1 = proplists:delete(Pid,TCGLs),
	    case [TCGL || {_,{Type,TCGL}} <- TCGLs1, Type == tc] of
		[TCGL] ->
		    {tc_log,TCGL,TCGLs1};
		_ ->
		    {ct_log,State#logger_state.ct_log_fd,TCGLs1}
	    end    
    end.

add_tc_gl(TCPid,GL,State) ->
    TCGLs = State#logger_state.tc_groupleaders,
    [{TCPid,{tc,GL}} | lists:keydelete(TCPid,1,TCGLs)].

rm_tc_gl(TCPid,State) ->
    TCGLs = State#logger_state.tc_groupleaders,
    case proplists:get_value(TCPid,TCGLs) of	
	{tc,GL} ->
	    TCGLs1 = lists:keydelete(TCPid,1,TCGLs),
	    case lists:keysearch({tc,GL},2,TCGLs1) of
		{value,_} ->
		    %% test cases using GL remain, keep associated IO processes
		    TCGLs1;
		false ->
		    %% last test case using GL, delete all associated IO processes
		    lists:filter(fun({_,{io,GLPid}}) when GL == GLPid -> false;
				    (_) -> true
				 end, TCGLs1)
	    end;
	_ ->
	    %% add_tc_gl has not been called for this Pid, ignore
	    TCGLs
    end.

set_evmgr_gl(GL) ->
    case whereis(?CT_EVMGR_REF) of
	undefined -> ok;
	EvMgrPid -> group_leader(GL,EvMgrPid)
    end.

open_ctlog() ->
    {ok,Fd} = file:open(?ct_log_name,[write]),
    io:format(Fd, header("Common Test Framework Log"), []),
    case file:consult(ct_run:variables_file_name("../")) of
	{ok,Vars} ->
	    io:format(Fd, config_table(Vars), []);
	{error,Reason} ->
	    {ok,Cwd} = file:get_cwd(),
	    Dir = filename:dirname(Cwd),
	    Variables = ct_run:variables_file_name(Dir),
	    io:format(Fd,
		      "Can not read the file \'~s\' Reason: ~w\n"
		      "No configuration found for test!!\n",
		      [Variables,Reason])
    end,
    print_style(Fd,undefined),
    io:format(Fd, 
	      xhtml("<br><br><h2>Progress Log</h2>\n<pre>\n",
		    "<br /><br /><h4>PROGRESS LOG</h4>\n<pre>\n"), []),
    Fd.

print_style(Fd,undefined) ->
    case basic_html() of
	true ->
	    io:format(Fd,
		      "<style>\n"
		      "div.ct_internal { background:lightgrey; color:black; }\n"
		      "div.default     { background:lightgreen; color:black; }\n"
		      "</style>\n",
		      []);
	_ ->
	    ok
    end;

print_style(Fd,StyleSheet) ->
    case file:read_file(StyleSheet) of
	{ok,Bin} ->
	    Str = binary_to_list(Bin),
	    Pos0 = case string:str(Str,"<style>") of
		       0 -> string:str(Str,"<STYLE>");
		       N0 -> N0
		   end,
	    case Pos0 of
		0 -> print_style_error(Fd,StyleSheet,missing_style_tag);
		_ -> 
		    Pos1 = case string:str(Str,"</style>") of
			       0 -> string:str(Str,"</STYLE>");
			       N1 -> N1
			   end,
		    case Pos1 of
			0 -> 
			    print_style_error(Fd,StyleSheet,missing_style_end_tag);
			_ -> 
			    Style = string:sub_string(Str,Pos0,Pos1+7),
			    io:format(Fd,"~s\n",[Style])
		    end
	    end;
	{error,Reason} ->
	    print_style_error(Fd,StyleSheet,Reason)  
    end.

%% Simple link version, doesn't work with all browsers unfortunately. :-(
%% print_style(Fd, StyleSheet) ->
%%    io:format(Fd,
%%	      "<link href=~p rel=\"stylesheet\" type=\"text/css\">",
%%	      [StyleSheet]).

print_style_error(Fd,StyleSheet,Reason) ->
    io:format(Fd,"\n<!-- Failed to load stylesheet ~s: ~p -->\n",
	      [StyleSheet,Reason]),
    print_style(Fd,undefined).    

close_ctlog(Fd) ->
    io:format(Fd,"\n</pre>\n",[]),
    io:format(Fd,footer(),[]),
    file:close(Fd).



%%%-----------------------------------------------------------------
%%% Make an index page for the last run
make_last_run_index(StartTime) ->
    IndexName = ?index_name,
    AbsIndexName = ?abs(IndexName),
    case catch make_last_run_index1(StartTime,IndexName) of
	{'EXIT', Reason} ->
	    io:put_chars("CRASHED while updating " ++ AbsIndexName ++ "!\n"),
	    io:format("~p~n", [Reason]),
	    {error, Reason};
	{error, Reason} ->
	    io:put_chars("FAILED while updating " ++ AbsIndexName ++ "\n"),
	    io:format("~p~n", [Reason]),
	    {error, Reason};
	ok ->
%	    io:put_chars("done\n"),
	    ok;
	Err ->
	    io:format("Unknown internal error while updating ~s. "
		      "Please report.\n(Err: ~p, ID: 1)",
		      [AbsIndexName,Err]),
	    {error, Err}
    end.

make_last_run_index1(StartTime,IndexName) ->
    %% this manoeuvre is to ensure the tests get logged 
    %% in correct order of time (the 1 sec resolution
    %% of the dirnames may be too big)
    Logs1 =
	case filelib:wildcard([$*|?logdir_ext]) of
	    [Log] ->				% first test
		[Log];
	    Logs ->
		case read_totals_file(?totals_name) of
		    {_Node,_Lbl,Logs0,_Totals} ->
			insert_dirs(Logs,Logs0);
		    _ ->
			%% someone deleted the totals file!?
			Logs
		end
	end,
    Missing =
	case file:read_file(?missing_suites_info) of
	    {ok,Bin} -> binary_to_term(Bin);
	    _ -> []
	end,
    Label = case application:get_env(common_test, test_label) of
		{ok,Lbl} -> Lbl;
		_ -> undefined
	    end,
    {ok,Index0,Totals} = make_last_run_index(Logs1,
					     index_header(Label,StartTime),
					     0, 0, 0, 0, 0, Missing),
    %% write current Totals to file, later to be used in all_runs log
    write_totals_file(?totals_name,Label,Logs1,Totals),
    Index = [Index0|index_footer()],
    case force_write_file(IndexName, Index) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error,{index_write_error, Reason}}
    end.

insert_dirs([NewDir|NewDirs],Dirs) ->
    Dirs1 = insert_dir(NewDir,Dirs),
    insert_dirs(NewDirs,Dirs1);
insert_dirs([],Dirs) ->
    Dirs.
insert_dir(D,Dirs=[D|_]) ->
    Dirs;
insert_dir(D,[D1|Ds]) ->
    [D1|insert_dir(D,Ds)];
insert_dir(D,[]) ->
    [D].

make_last_run_index([Name|Rest], Result, TotSucc, TotFail, UserSkip, AutoSkip,
		    TotNotBuilt, Missing) ->
    case last_test(Name) of
	false ->
	    %% Silently skip.
	    make_last_run_index(Rest, Result, TotSucc, TotFail, UserSkip, AutoSkip,
				TotNotBuilt, Missing);
	LastLogDir ->
	    SuiteName = filename:rootname(filename:basename(Name)),
	    case make_one_index_entry(SuiteName, LastLogDir, "-", false, Missing) of
		{Result1,Succ,Fail,USkip,ASkip,NotBuilt} ->
		    %% for backwards compatibility
		    AutoSkip1 = case catch AutoSkip+ASkip of
				    {'EXIT',_} -> undefined;
				    Res -> Res
				end,
		    make_last_run_index(Rest, [Result|Result1], TotSucc+Succ, 
					TotFail+Fail, UserSkip+USkip, AutoSkip1,
					TotNotBuilt+NotBuilt, Missing);
		error ->
		    make_last_run_index(Rest, Result, TotSucc, TotFail, UserSkip, AutoSkip, 
					TotNotBuilt, Missing)
	    end
    end;
make_last_run_index([], Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt, _) ->
    {ok, [Result|total_row(TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt, false)],
     {TotSucc,TotFail,UserSkip,AutoSkip,TotNotBuilt}}.

make_one_index_entry(SuiteName, LogDir, Label, All, Missing) ->
    case count_cases(LogDir) of
	{Succ,Fail,UserSkip,AutoSkip} ->
	    NotBuilt = not_built(SuiteName, LogDir, All, Missing),
	    NewResult = make_one_index_entry1(SuiteName, LogDir, Label, Succ, Fail,
					      UserSkip, AutoSkip, NotBuilt, All,
					      normal),
	    {NewResult,Succ,Fail,UserSkip,AutoSkip,NotBuilt};
	error ->
	    error
    end.

make_one_index_entry1(SuiteName, Link, Label, Success, Fail, UserSkip, AutoSkip,
		      NotBuilt, All, Mode) ->
    LogFile = filename:join(Link, ?suitelog_name ++ ".html"),
    CrashDumpLink = case Mode of
			cached ->
			    "";
			normal ->
			    CrashDumpName = SuiteName ++ "_erl_crash.dump",
			    case filelib:is_file(CrashDumpName) of
				true ->
				    ["&nbsp;<a href=\"", CrashDumpName,
				     "\">(CrashDump)</a>"];
				false ->
				    ""
			    end
		    end,
    CtRunDir = filename:dirname(filename:dirname(Link)),
    {Lbl,Timestamp,Node,AllInfo} =
	case All of
	    {true,OldRuns} -> 
		[_Prefix,NodeOrDate|_] = string:tokens(Link,"."),
		Node1 = case string:chr(NodeOrDate,$@) of
			    0 -> "-";
			    _ -> NodeOrDate
			end,
		TS = timestamp(CtRunDir),
		N = xhtml(["<td align=right><font size=\"-1\">",Node1,
			   "</font></td>\n"],
			  ["<td align=right>",Node1,"</td>\n"]),
		L = xhtml(["<td align=center><font size=\"-1\"><b>",Label,
			   "</font></b></td>\n"],
			  ["<td align=center><b>",Label,"</b></td>\n"]),
		T = xhtml(["<td><font size=\"-1\">",TS,"</font></td>\n"],
			  ["<td>",TS,"</td>\n"]),
		CtLogFile = filename:join(CtRunDir,?ct_log_name),
		OldRunsLink = 
		    case OldRuns of
			[] -> "none";
			_ ->  "<a href=\""++?all_runs_name++"\">Old Runs</a>"
		    end,
		A = xhtml(["<td><font size=\"-1\"><a href=\"",CtLogFile,
			   "\">CT Log</a></font></td>\n",
			   "<td><font size=\"-1\">",OldRunsLink,"</font></td>\n"],
			  ["<td><a href=\"",CtLogFile,"\">CT Log</a></td>\n",
			   "<td>",OldRunsLink,"</td>\n"]),
		{L,T,N,A};
	    false ->
		{"","","",""}
	end,
    NotBuiltStr =
	if NotBuilt == 0 ->
		["<td align=right>",integer_to_list(NotBuilt),"</td>\n"];
	   true ->
		["<td align=right><a href=\"",filename:join(CtRunDir,?ct_log_name),"\">",
		integer_to_list(NotBuilt),"</a></td>\n"]
	end,
    FailStr =
	if Fail > 0 ->  
		["<font color=\"red\">",
		 integer_to_list(Fail),"</font>"];
	   true ->
		integer_to_list(Fail)
	end,
    {AllSkip,UserSkipStr,AutoSkipStr} = 
	if AutoSkip == undefined -> {UserSkip,"?","?"};
	   true ->
		ASStr = if AutoSkip > 0 ->
				["<font color=\"brown\">",
				 integer_to_list(AutoSkip),"</font>"];
			   true -> integer_to_list(AutoSkip)
			end,
		{UserSkip+AutoSkip,integer_to_list(UserSkip),ASStr}
	end,
    [xhtml("<tr valign=top>\n",
	   ["<tr class=\"",odd_or_even(),"\">\n"]),
     xhtml("<td><font size=\"-1\"><a href=\"", "<td><a href=\""),
     LogFile,"\">",SuiteName,"</a>", CrashDumpLink,
     xhtml("</font></td>\n", "</td>\n"),
     Lbl, Timestamp,
     "<td align=right>",integer_to_list(Success),"</td>\n",
     "<td align=right>",FailStr,"</td>\n",
     "<td align=right>",integer_to_list(AllSkip),
     " (",UserSkipStr,"/",AutoSkipStr,")</td>\n",  
     NotBuiltStr, Node, AllInfo, "</tr>\n"].

total_row(Success, Fail, UserSkip, AutoSkip, NotBuilt, All) ->
    {Label,TimestampCell,AllInfo} =
	case All of
	    true ->
		{"<td>&nbsp;</td>\n",
		 "<td>&nbsp;</td>\n",
		 "<td>&nbsp;</td>\n"
		 "<td>&nbsp;</td>\n"
		 "<td>&nbsp;</td>\n"};
	    false ->
		{"","",""}
	end,

    {AllSkip,UserSkipStr,AutoSkipStr} =
	if AutoSkip == undefined -> {UserSkip,"?","?"};
	   true -> {UserSkip+AutoSkip,
		    integer_to_list(UserSkip),integer_to_list(AutoSkip)}
	end,
    [xhtml("<tr valign=top>\n", 
	   ["<tr class=\"",odd_or_even(),"\">\n"]),
     "<td><b>Total</b></td>\n", Label, TimestampCell,
     "<td align=right><b>",integer_to_list(Success),"<b></td>\n",
     "<td align=right><b>",integer_to_list(Fail),"<b></td>\n",
     "<td align=right>",integer_to_list(AllSkip),
     " (",UserSkipStr,"/",AutoSkipStr,")</td>\n",  
     "<td align=right><b>",integer_to_list(NotBuilt),"<b></td>\n",
     AllInfo, "</tr>\n"].

not_built(_BaseName,_LogDir,_All,[]) ->
    0;
not_built(BaseName,_LogDir,_All,Missing) ->
    %% find out how many suites didn't compile
    %% BaseName = 
    %%            Top.ObjDir | Top.ObjDir.suites | Top.ObjDir.Suite | 
    %%            Top.ObjDir.Suite.cases | Top.ObjDir.Suite.Case    
    Failed =
	case string:tokens(BaseName,".") of
	    [T,O] when is_list(T) ->		% all under Top.ObjDir
		locate_info({T,O},all,Missing);
	    [T,O,"suites"] ->
		locate_info({T,O},suites,Missing);
	    [T,O,S] ->
		locate_info({T,O},list_to_atom(S),Missing);
	    [T,O,S,_] ->
		locate_info({T,O},list_to_atom(S),Missing);
	    _ ->				% old format - don't crash
		[]
	end,
    length(Failed).    

locate_info(Path={Top,Obj},AllOrSuite,[{{Dir,Suite},Failed}|Errors]) ->
    case lists:reverse(filename:split(Dir)) of
	["test",Obj,Top|_] ->
	    get_missing_suites(AllOrSuite,{Suite,Failed}) ++
		locate_info(Path,AllOrSuite,Errors);
	[Obj,Top|_] ->
	    get_missing_suites(AllOrSuite,{Suite,Failed}) ++
		locate_info(Path,AllOrSuite,Errors);
	_ ->
	    locate_info(Path,AllOrSuite,Errors)
    end;
locate_info(_,_,[]) ->
    [].

get_missing_suites(all,{"all",Failed}) ->
    Failed;
get_missing_suites(suites,{_Suite,Failed}) ->
    Failed;
get_missing_suites(Suite,{Suite,Failed}) ->
    Failed;
get_missing_suites(_,_) ->
    [].

term_to_text(Term) ->
    lists:flatten(io_lib:format("~p.\n", [Term])).


%%% Headers and footers.

index_header(Label, StartTime) ->
    Head =
	case Label of
	    undefined ->
		header("Test Results", format_time(StartTime));
	    _ ->
		header("Test Results for '" ++ Label ++ "'",
		       format_time(StartTime))
	end,
    [Head |
     ["<center>\n",
      xhtml(["<p><a href=\"",?ct_log_name,
	     "\">Common Test Framework Log</a></p>"],
	    ["<br />"
	     "<div id=\"button_holder\" class=\"btn\">\n"
	     "<a href=\"",?ct_log_name,
	     "\">COMMON TEST FRAMEWORK LOG</a>\n</div>"]),
      xhtml("<br>\n", "<br /><br /><br />\n"),
      xhtml(["<table border=\"3\" cellpadding=\"5\" "
	     "bgcolor=\"",?table_color3,"\">\n"], "<table>\n"),
      "<th><b>Test Name</b></th>\n",
      xhtml(["<th><font color=\"",?table_color3,"\">_</font>Ok"
	     "<font color=\"",?table_color3,"\">_</font></th>\n"],
	    "<th>Ok</th>\n"),
      "<th>Failed</th>\n",
      "<th>Skipped", xhtml("<br>", "<br />"), "(User/Auto)</th>\n"
      "<th>Missing", xhtml("<br>", "<br />"), "Suites</th>\n"
      "\n"]].

all_suites_index_header() ->
    {ok,Cwd} = file:get_cwd(),
    all_suites_index_header(Cwd).

all_suites_index_header(IndexDir) ->
    LogDir = filename:basename(IndexDir),
    AllRuns = xhtml(["All test runs in \"" ++ LogDir ++ "\""],
		    "ALL RUNS"),
    AllRunsLink = xhtml(["<a href=\"",?all_runs_name,"\">",AllRuns,"</a>\n"],
			["<div id=\"button_holder\" class=\"btn\">\n"
			 "<a href=\"",?all_runs_name,"\">",AllRuns,"</a>\n</div>"]),
    [header("Test Results") | 
     ["<center>\n",
      AllRunsLink,
      xhtml("<br><br>\n", "<br /><br />\n"),
      xhtml(["<table border=\"3\" cellpadding=\"5\" "
	     "bgcolor=\"",?table_color2,"\">\n"], "<table>\n"),
      "<th>Test Name</th>\n",
      "<th>Label</th>\n",
      "<th>Test Run Started</th>\n",
      xhtml(["<th><font color=\"",?table_color2,"\">_</font>Ok"
	     "<font color=\"",?table_color2,"\">_</font></th>\n"],
	    "<th>Ok</th>\n"),
      "<th>Failed</th>\n",
      "<th>Skipped<br>(User/Auto)</th>\n"
      "<th>Missing<br>Suites</th>\n"
      "<th>Node</th>\n",
      "<th>CT Log</th>\n",
      "<th>Old Runs</th>\n",
      "\n"]].

all_runs_header() ->
    {ok,Cwd} = file:get_cwd(),
    LogDir = filename:basename(Cwd),
    Title = "All test runs in \"" ++ LogDir ++ "\"",
    IxLink = [xhtml(["<p><a href=\"",?index_name,
		     "\">Test Index Page</a></p>"],
		    ["<div id=\"button_holder\" class=\"btn\">\n"
		     "<a href=\"",?index_name,
		     "\">TEST INDEX PAGE</a>\n</div>"]),
	      xhtml("<br>\n", "<br /><br />\n")],
    [header(Title) |
     ["<center>\n", IxLink,
      xhtml(["<table border=\"3\" cellpadding=\"5\" "
	     "bgcolor=\"",?table_color1,"\">\n"], "<table>\n"),
      "<th><b>History</b></th>\n"
      "<th><b>Node</b></th>\n"
      "<th><b>Label</b></th>\n"
      "<th>Tests</th>\n"
      "<th><b>Test Names</b></th>\n"
      "<th>Total</th>\n",
      xhtml(["<th><font color=\"",?table_color1,"\">_</font>Ok"
	     "<font color=\"",?table_color1,"\">_</font></th>\n"],
	    "<th>Ok</th>\n"),
      "<th>Failed</th>\n"
      "<th>Skipped<br>(User/Auto)</th>\n"
      "<th>Missing<br>Suites</th>\n"
      "\n"]].

header(Title) ->
    header1(Title, "").
header(Title, SubTitle) ->
    header1(Title, SubTitle).

header1(Title, SubTitle) ->
    SubTitleHTML = if SubTitle =/= "" ->
			   ["<center>\n",
			    "<h3>" ++ SubTitle ++ "</h3>\n",
			    xhtml("</center>\n<br>\n", "</center>\n<br />\n")];
		      true -> xhtml("<br>\n", "<br />\n")
		   end,
    CSSFile = xhtml(fun() -> "" end, 
		    fun() -> make_relative(locate_default_css_file()) end),
    [xhtml(["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n",
	    "<html>\n"],
	   ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n",
	    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
	    "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"]),
    "<!-- autogenerated by '"++atom_to_list(?MODULE)++"' -->\n",
    "<head>\n",
    "<title>" ++ Title ++ " " ++ SubTitle ++ "</title>\n",
    "<meta http-equiv=\"cache-control\" content=\"no-cache\">\n",
    xhtml("",
	  ["<link rel=\"stylesheet\" href=\"",CSSFile,"\" type=\"text/css\">"]),
    "</head>\n",
    body_tag(),
    "<center>\n",
    "<h1>" ++ Title ++ "</h1>\n",
    "</center>\n",
    SubTitleHTML,"\n"].

index_footer() ->
    ["</table>\n"
     "</center>\n" | footer()].

footer() ->
     ["<center>\n",
      xhtml("<br><br>\n<hr>\n", "<br /><br />\n"),
      xhtml("<p><font size=\"-1\">\n", "<div class=\"copyright\">"),
      "Copyright &copy; ", year(),
      " <a href=\"http://www.erlang.org\">Open Telecom Platform</a>",
      xhtml("<br>\n", "<br />\n"),
      "Updated: <!date>", current_time(), "<!/date>",
      xhtml("<br>\n", "<br />\n"),
      xhtml("</font></p>\n", "</div>\n"),
      "</center>\n"
      "</body>\n"].


body_tag() ->
    CTPath = code:lib_dir(common_test),
    TileFile = filename:join(filename:join(CTPath,"priv"),"tile1.jpg"),
    xhtml("<body background=\"" ++ TileFile ++
	      "\" bgcolor=\"#FFFFFF\" text=\"#000000\" link=\"#0000FF\" "
	  "vlink=\"#800080\" alink=\"#FF0000\">\n",
	  "<body>\n").

current_time() ->
    format_time(calendar:local_time()).

format_time({{Y, Mon, D}, {H, Min, S}}) ->
    Weekday = weekday(calendar:day_of_the_week(Y, Mon, D)),
    lists:flatten(io_lib:format("~s ~s ~p ~w ~2.2.0w:~2.2.0w:~2.2.0w",
				[Weekday, month(Mon), D, Y, H, Min, S])).

weekday(1) -> "Mon";
weekday(2) -> "Tue";
weekday(3) -> "Wed";
weekday(4) -> "Thu";
weekday(5) -> "Fri";
weekday(6) -> "Sat";
weekday(7) -> "Sun".

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

year() ->
    {Y, _, _} = date(),
    integer_to_list(Y).


%% Count test cases in the given directory (a directory of the type
%% run.1997-08-04_09.58.52).

count_cases(Dir) ->
    SumFile = filename:join(Dir, ?run_summary),
    case read_summary(SumFile, [summary]) of
	{ok, [{Succ,Fail,Skip}]} ->
	    {Succ,Fail,Skip,undefined};
	{ok, [Summary]} ->
	    Summary;
	{error, _} ->
	    LogFile = filename:join(Dir, ?suitelog_name),
	    case file:read_file(LogFile) of
		{ok, Bin} ->
		    case count_cases1(binary_to_list(Bin), 
				      {undefined,undefined,undefined,undefined}) of
			{error,not_complete} ->
			    %% The test is not complete - dont write summary
			    %% file yet.
			    {0,0,0,0};
			Summary ->
			    write_summary(SumFile, Summary),
			    Summary
		    end;
		{error, _Reason} ->
		    io:format("\nFailed to read ~p (skipped)\n", [LogFile]),
		    error
	    end
    end.

write_summary(Name, Summary) ->
    File = [term_to_text({summary, Summary})],
    force_write_file(Name, File).

read_summary(Name, Keys) ->
    case file:consult(Name) of
	{ok, []} ->
	    {error, "Empty summary file"};
	{ok, Terms} ->
	    {ok, lists:map(fun(Key) -> {value, {_, Value}} = 
					   lists:keysearch(Key, 1, Terms),
				       Value end,
			   Keys)};
	{error, Reason} ->
	    {error, Reason}
    end.

count_cases1("=failed" ++ Rest, {Success, _Fail, UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Count, UserSkip,AutoSkip});
count_cases1("=successful" ++ Rest, {_Success, Fail, UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Count, Fail, UserSkip,AutoSkip});
count_cases1("=skipped" ++ Rest, {Success, Fail, _UserSkip,_AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Fail, Count,undefined});
count_cases1("=user_skipped" ++ Rest, {Success, Fail, _UserSkip,AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Fail, Count,AutoSkip});
count_cases1("=auto_skipped" ++ Rest, {Success, Fail, UserSkip,_AutoSkip}) ->
    {NextLine, Count} = get_number(Rest),
    count_cases1(NextLine, {Success, Fail, UserSkip,Count});
count_cases1([], {Su,F,USk,_ASk}) when Su==undefined;F==undefined;
				       USk==undefined ->
    {error,not_complete};
count_cases1([], Counters) ->
    Counters;
count_cases1(Other, Counters) ->
    count_cases1(skip_to_nl(Other), Counters).

get_number([$\s|Rest]) ->
    get_number(Rest);
get_number([Digit|Rest]) when $0 =< Digit, Digit =< $9 ->
    get_number(Rest, Digit-$0).

get_number([Digit|Rest], Acc) when $0 =< Digit, Digit =< $9 ->
    get_number(Rest, Acc*10+Digit-$0);
get_number([$\n|Rest], Acc) ->
    {Rest, Acc};
get_number([_|Rest], Acc) ->
    get_number(Rest, Acc).

skip_to_nl([$\n|Rest]) ->
    Rest;
skip_to_nl([_|Rest]) ->
    skip_to_nl(Rest);
skip_to_nl([]) ->
    [].


config_table(Vars) ->
    [config_table_header()|config_table1(Vars)].

config_table_header() ->
    [
     xhtml(["<h2>Configuration</h2>\n"
	    "<table border=\"3\" cellpadding=\"5\" bgcolor=\"",?table_color1,"\"\n"],
	   "<h4>CONFIGURATION</h4>\n<table>\n"),
     "<tr><th>Key</th><th>Value</th></tr>\n"].

config_table1([{Key,Value}|Vars]) ->
    [xhtml(["<tr><td>", atom_to_list(Key), "</td>\n",
	   "<td><pre>",io_lib:format("~p",[Value]),"</pre></td></tr>\n"],
	   ["<tr class=\"", odd_or_even(), "\">\n",
	    "<td>", atom_to_list(Key), "</td>\n",
	    "<td>", io_lib:format("~p",[Value]), "</td>\n</tr>\n"]) | 
     config_table1(Vars)];
config_table1([]) ->
    ["</table>\n"].


make_all_runs_index(When) ->
    put(basic_html, basic_html()),
    AbsName = ?abs(?all_runs_name),
    notify_and_lock_file(AbsName),
    if When == start -> ok;
       true -> io:put_chars("Updating " ++ AbsName ++ "... ")
    end,
    Dirs = filelib:wildcard(logdir_prefix()++"*.*"),
    DirsSorted = (catch sort_all_runs(Dirs)),
    Header = all_runs_header(),
    Index = [runentry(Dir) || Dir <- DirsSorted],
    Result = file:write_file(AbsName,Header++Index++index_footer()),
    if When == start -> ok;
       true -> io:put_chars("done\n")
    end,
    notify_and_unlock_file(AbsName),
    Result.

sort_all_runs(Dirs) ->
    %% sort on time string, always last and on the format:
    %% "YYYY-MM-DD_HH.MM.SS"
    KeyList =
	lists:map(fun(Dir) ->
			  case lists:reverse(string:tokens(Dir,[$.,$_])) of
			      [SS,MM,HH,Date|_] ->
				  {{Date,HH,MM,SS},Dir};
			      _Other ->
				  throw(Dirs)
			  end
		  end,Dirs),
    lists:reverse(lists:map(fun({_,Dir}) ->
				    Dir
			    end,lists:keysort(1,KeyList))).


interactive_link() ->
    [Dir|_] = lists:reverse(filelib:wildcard(logdir_prefix()++"*.*")),
    CtLog = filename:join(Dir,"ctlog.html"),
    Body = ["Log from last interactive run: <a href=\"",CtLog,"\">",
	    timestamp(Dir),"</a>"],
    file:write_file("last_interactive.html",Body),
    io:format("~n~nUpdated ~s\n"
	      "Any CT activities will be logged here\n",
	      [?abs("last_interactive.html")]).

runentry(Dir) ->
    TotalsFile = filename:join(Dir,?totals_name),
    TotalsStr =
	case read_totals_file(TotalsFile) of
	    {Node,Label,Logs,{TotSucc,TotFail,UserSkip,AutoSkip,NotBuilt}} ->
		TotFailStr =
		    if TotFail > 0 ->  
			    ["<font color=\"red\">",
			     integer_to_list(TotFail),"</font>"];
		       true ->
			    integer_to_list(TotFail)
		    end,
		{AllSkip,UserSkipStr,AutoSkipStr} = 
		    if AutoSkip == undefined -> {UserSkip,"?","?"};
		       true ->
			    ASStr = if AutoSkip > 0 ->
					    ["<font color=\"brown\">",
					     integer_to_list(AutoSkip),"</font>"];
				       true -> integer_to_list(AutoSkip)
				    end,
			    {UserSkip+AutoSkip,integer_to_list(UserSkip),ASStr}
		    end,
		NoOfTests = case length(Logs) of
				0 -> "-";
				N -> integer_to_list(N)
			    end,
		StripExt = 
		    fun(File) ->
			    string:sub_string(File,1,
					      length(File)-
					      length(?logdir_ext)) ++ ", "
		    end,
		Polish =  fun(S) -> case lists:reverse(S) of
					[32,$,|Rev] -> lists:reverse(Rev);
					[$,|Rev] -> lists:reverse(Rev);
					_ -> S
				    end 
			  end,
		TestNames = Polish(lists:flatten(lists:map(StripExt,Logs))),
		TestNamesTrunc =
		    if TestNames=="" -> 
			    "";
		       length(TestNames) < ?testname_width ->
			    TestNames;
		       true ->
			    Trunc = Polish(string:substr(TestNames,1,?testname_width-3)),
			    lists:flatten(io_lib:format("~s...",[Trunc]))
		    end,
		Total = TotSucc+TotFail+AllSkip,
		A = xhtml(["<td align=center><font size=\"-1\">",Node,
			   "</font></td>\n",
			   "<td align=center><font size=\"-1\"><b>",Label,
			   "</b></font></td>\n",
			   "<td align=right>",NoOfTests,"</td>\n"],
			  ["<td align=center>",Node,"</td>\n",
			   "<td align=center><b>",Label,"</b></td>\n",
			   "<td align=right>",NoOfTests,"</td>\n"]),
		B = xhtml(["<td align=center title='",TestNames,"'><font size=\"-1\"> ",
			   TestNamesTrunc,"</font></td>\n"],
			  ["<td align=center title='",TestNames,"'> ",
			   TestNamesTrunc,"</td>\n"]),
		C = ["<td align=right>",integer_to_list(Total),"</td>\n",
		     "<td align=right>",integer_to_list(TotSucc),"</td>\n",
		     "<td align=right>",TotFailStr,"</td>\n",
		     "<td align=right>",integer_to_list(AllSkip),
		     " (",UserSkipStr,"/",AutoSkipStr,")</td>\n",
		     "<td align=right>",integer_to_list(NotBuilt),"</td>\n"],
		A++B++C;
	    _ ->
		A = xhtml(["<td align=center><font size=\"-1\" color=\"red\">"
			   "Test data missing or corrupt</font></td>\n",
			   "<td align=center><font size=\"-1\">?</font></td>\n",
			   "<td align=right>?</td>\n"],
			  ["<td align=center><font color=\"red\">"
			   "Test data missing or corrupt</font></td>\n",
			   "<td align=center>?</td>\n",
			   "<td align=right>?</td>\n"]),
		B = xhtml(["<td align=center><font size=\"-1\">?</font></td>\n"],
			  ["<td align=center>?</td>\n"]),
		C = ["<td align=right>?</td>\n",
		     "<td align=right>?</td>\n",
		     "<td align=right>?</td>\n",
		     "<td align=right>?</td>\n",
		     "<td align=right>?</td>\n"],
		A++B++C
	end,	    
    Index = filename:join(Dir,?index_name),
    [xhtml("<tr>\n", ["<tr class=\"",odd_or_even(),"\">\n"]),
     xhtml(["<td><font size=\"-1\"><a href=\"",Index,"\">",timestamp(Dir),"</a>",
	    TotalsStr,"</font></td>\n"],
	   ["<td><a href=\"",Index,"\">",timestamp(Dir),"</a>",TotalsStr,"</td>\n"]),
     "</tr>\n"].

write_totals_file(Name,Label,Logs,Totals) ->
    AbsName = ?abs(Name),
    notify_and_lock_file(AbsName),
    force_write_file(AbsName,
		     term_to_binary({atom_to_list(node()),
				     Label,Logs,Totals})),
    notify_and_unlock_file(AbsName).

%% this function needs to convert from old formats to new so that old
%% test results (prev ct versions) can be listed together with new
read_totals_file(Name) ->
    AbsName = ?abs(Name),
    notify_and_lock_file(AbsName),
    Result = 
	case file:read_file(AbsName) of
	    {ok,Bin} ->
		case catch binary_to_term(Bin) of
		    {'EXIT',_Reason} ->		% corrupt file
			{"-",[],undefined};
		    {Node,Label,Ls,Tot} ->	% all info available
			Label1 = case Label of
				     undefined -> "-";
				     _         -> Label
				 end,
			case Tot of
			    {_Ok,_Fail,_USkip,_ASkip,_NoBuild} ->  % latest format
				{Node,Label1,Ls,Tot};
			    {TotSucc,TotFail,AllSkip,NotBuilt} ->
				{Node,Label1,Ls,{TotSucc,TotFail,AllSkip,undefined,NotBuilt}}
			end;
		    {Node,Ls,Tot} ->		% no label found
			case Tot of
			    {_Ok,_Fail,_USkip,_ASkip,_NoBuild} ->  % latest format
				{Node,"-",Ls,Tot};
			    {TotSucc,TotFail,AllSkip,NotBuilt} ->
				{Node,"-",Ls,{TotSucc,TotFail,AllSkip,undefined,NotBuilt}}
			end;
		    %% for backwards compatibility
		    {Ls,Tot}    -> {"-",Ls,Tot};
		    Tot         -> {"-",[],Tot}
		end;
	    Error ->
		Error
	end,
    notify_and_unlock_file(AbsName),
    Result.

force_write_file(Name,Contents) ->
    force_delete(Name),
    file:write_file(Name,Contents).

force_delete(Name) ->
    case file:delete(Name) of
	{error,eacces} ->
	    force_rename(Name,Name++".old.",0);
	Other ->
	    Other
    end.

force_rename(From,To,Number) ->
    Dest = [To|integer_to_list(Number)],
    case file:read_file_info(Dest) of
	{ok,_} ->
	    force_rename(From,To,Number+1);
	{error,_} ->
	    file:rename(From,Dest)
    end.


timestamp(Dir) ->
    TsR = lists:reverse(string:tokens(Dir,".-_")),
    [S,Min,H,D,M,Y] = [list_to_integer(N) || N <- lists:sublist(TsR,6)],
    format_time({{Y,M,D},{H,Min,S}}).

%% ----------------------------- NOTE --------------------------------------
%% The top level index file is generated based on the file contents under
%% logdir. This takes place initially when the test run starts (When = start)
%% and an update takes place at the end of the test run, or when the user
%% requests an explicit refresh (When = refresh).
%% The index file needs to be updated also at the start of each individual
%% test (in order for the user to be able to track test progress by refreshing
%% the browser). Since it would be too expensive to generate a new file from
%% scratch every time (by reading the data from disk), a copy of the dir tree
%% is cached as a result of the first index file creation. This copy is then
%% used for all top level index page updates that occur during the test run.
%% This means that any changes to the dir tree under logdir during the test
%% run will not show until after the final refresh.
%% -------------------------------------------------------------------------

%% Creates the top level index file. When == start | refresh.
%% A copy of the dir tree under logdir is cached as a result.
make_all_suites_index(When) when is_atom(When) ->
    put(basic_html, basic_html()),
    AbsIndexName = ?abs(?index_name),
    notify_and_lock_file(AbsIndexName),
    LogDirs = filelib:wildcard(logdir_prefix()++".*/*"++?logdir_ext),
    Sorted = sort_logdirs(LogDirs, []),
    Result = make_all_suites_index1(When, AbsIndexName, Sorted),
    notify_and_unlock_file(AbsIndexName),
    Result;

%% This updates the top level index file using cached data from
%% the initial index file creation.
make_all_suites_index(NewTestData = {_TestName,DirName}) ->
    put(basic_html, basic_html()),
    %% AllLogDirs = [{TestName,Label,Missing,{LastLogDir,Summary},OldDirs}|...]
    {AbsIndexName,LogDirData} = ct_util:get_testdata(test_index),

    CtRunDirPos = length(filename:split(AbsIndexName)),
    CtRunDir = filename:join(lists:sublist(filename:split(DirName),
					   CtRunDirPos)),

    Label = case read_totals_file(filename:join(CtRunDir, ?totals_name)) of
		{_,"-",_,_} -> "...";
		{_,Lbl,_,_} -> Lbl;
		_           -> "..."
	    end,
    notify_and_lock_file(AbsIndexName),
    Result =
	case catch make_all_suites_ix_cached(AbsIndexName,
					     NewTestData,
					     Label,
					     LogDirData) of
	    {'EXIT',Reason} ->
		io:put_chars("CRASHED while updating " ++ AbsIndexName ++ "!\n"),
		io:format("~p~n", [Reason]),
		{error,Reason};
	    {error,Reason} ->
		io:put_chars("FAILED while updating " ++ AbsIndexName ++ "\n"),
		io:format("~p~n", [Reason]),
		{error,Reason};
	    ok ->
		ok;
	    Err ->
		io:format("Unknown internal error while updating ~s. "
			  "Please report.\n(Err: ~p, ID: 1)",
			  [AbsIndexName,Err]),
		{error, Err}
	end,
    notify_and_unlock_file(AbsIndexName),
    Result.

sort_logdirs([Dir|Dirs],Groups) ->
    TestName = filename:rootname(filename:basename(Dir)),
    case filelib:wildcard(filename:join(Dir,"run.*")) of
	[RunDir] ->
	    Groups1 = insert_test(TestName,{filename:basename(RunDir),RunDir},Groups),
	    sort_logdirs(Dirs,Groups1);
	_ ->					% ignore missing run directory
	    sort_logdirs(Dirs,Groups)
    end;
sort_logdirs([],Groups) ->
    lists:keysort(1,sort_each_group(Groups)).

insert_test(Test,IxDir,[{Test,IxDirs}|Groups]) ->
    [{Test,[IxDir|IxDirs]}|Groups];
insert_test(Test,IxDir,[]) ->
    [{Test,[IxDir]}];
insert_test(Test,IxDir,[TestDir|Groups]) ->
    [TestDir|insert_test(Test,IxDir,Groups)].
		 
sort_each_group([{Test,IxDirs}|Groups]) ->
    Sorted = lists:reverse([Dir || {_,Dir} <- lists:keysort(1,IxDirs)]),
    [{Test,Sorted}| sort_each_group(Groups)];
sort_each_group([]) ->
    [].

make_all_suites_index1(When, AbsIndexName, AllLogDirs) ->
    IndexName = ?index_name,
    if When == start -> ok;
       true -> io:put_chars("Updating " ++ AbsIndexName ++ "... ")
    end,
    case catch make_all_suites_index2(IndexName, AllLogDirs) of
	{'EXIT', Reason} ->
	    io:put_chars("CRASHED while updating " ++ AbsIndexName ++ "!\n"),
	    io:format("~p~n", [Reason]),
	    {error, Reason};
	{error, Reason} ->
	    io:put_chars("FAILED while updating " ++ AbsIndexName ++ "\n"),
	    io:format("~p~n", [Reason]),
	    {error, Reason};
	{ok,CacheData} ->
	    case When of
		start ->
		    ct_util:set_testdata_async({test_index,{AbsIndexName,
							    CacheData}}),
		    ok;
		_ ->
		    io:put_chars("done\n"),
		    ok
	    end;
	Err ->
	    io:format("Unknown internal error while updating ~s. "
		      "Please report.\n(Err: ~p, ID: 1)",
		      [AbsIndexName,Err]),
	    {error, Err}
    end.

make_all_suites_index2(IndexName, AllTestLogDirs) ->
    {ok,Index0,_Totals,CacheData} =
	make_all_suites_index3(AllTestLogDirs,
			       all_suites_index_header(),
			       0, 0, 0, 0, 0, [], []),
    Index = [Index0|index_footer()],
    case force_write_file(IndexName, Index) of
	ok ->
	    {ok,CacheData};
	{error, Reason} ->
	    {error,{index_write_error, Reason}}
    end.

make_all_suites_index3([{TestName,[LastLogDir|OldDirs]}|Rest],
		       Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt,
		       Labels, CacheData) ->
    [EntryDir|_] = filename:split(LastLogDir),
    Missing = 
	case file:read_file(filename:join(EntryDir, ?missing_suites_info)) of
	    {ok,Bin} -> binary_to_term(Bin);
	    _ -> []
	end,
    {Label,Labels1} =
	case proplists:get_value(EntryDir, Labels) of
	    undefined ->
		case read_totals_file(filename:join(EntryDir, ?totals_name)) of
		    {_,Lbl,_,_} -> {Lbl,[{EntryDir,Lbl}|Labels]};
		    _           -> {"-",[{EntryDir,"-"}|Labels]}
		end;
	    Lbl ->
		{Lbl,Labels}
	end,
    case make_one_index_entry(TestName, LastLogDir, Label, {true,OldDirs}, Missing) of
	{Result1,Succ,Fail,USkip,ASkip,NotBuilt} ->
	    %% for backwards compatibility
	    AutoSkip1 = case catch AutoSkip+ASkip of
			    {'EXIT',_} -> undefined;
			    Res -> Res
			end,
	    IxEntry = {TestName,Label,Missing,
		       {LastLogDir,{Succ,Fail,USkip,ASkip}},OldDirs},
	    make_all_suites_index3(Rest, [Result|Result1], TotSucc+Succ, 
				   TotFail+Fail, UserSkip+USkip, AutoSkip1,
				   TotNotBuilt+NotBuilt, Labels1,
				   [IxEntry|CacheData]);
	error ->
	    IxEntry = {TestName,Label,Missing,{LastLogDir,error},OldDirs},
	    make_all_suites_index3(Rest, Result, TotSucc, TotFail, 
				   UserSkip, AutoSkip, TotNotBuilt, Labels1,
				   [IxEntry|CacheData])
    end;
make_all_suites_index3([], Result, TotSucc, TotFail, UserSkip, AutoSkip, 
		       TotNotBuilt, _, CacheData) ->
    {ok, [Result|total_row(TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt,true)], 
     {TotSucc,TotFail,UserSkip,AutoSkip,TotNotBuilt}, lists:reverse(CacheData)}.


make_all_suites_ix_cached(AbsIndexName, NewTestData, Label, AllTestLogDirs) ->
    AllTestLogDirs1 = insert_new_test_data(NewTestData, Label, AllTestLogDirs),
    IndexDir = filename:dirname(AbsIndexName),
    Index0 = make_all_suites_ix_cached1(AllTestLogDirs1,
					all_suites_index_header(IndexDir),
					0, 0, 0, 0, 0),
    Index = [Index0|index_footer()],
    case force_write_file(AbsIndexName, Index) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error,{index_write_error, Reason}}
    end.

insert_new_test_data({NewTestName,NewTestDir}, NewLabel, AllTestLogDirs) ->
    AllTestLogDirs1 =
	case lists:keysearch(NewTestName, 1, AllTestLogDirs) of
	    {value,{_,_,_,{LastLogDir,_},OldDirs}} ->
		[{NewTestName,NewLabel,[],{NewTestDir,{0,0,0,0}},
		  [LastLogDir|OldDirs]} |
		 lists:keydelete(NewTestName, 1, AllTestLogDirs)];
	    false ->
		[{NewTestName,NewLabel,[],{NewTestDir,{0,0,0,0}},[]} |
		 AllTestLogDirs]
	end,
    lists:keysort(1, AllTestLogDirs1).

make_all_suites_ix_cached1([{TestName,Label,Missing,LastLogDirData,OldDirs}|Rest],
			   Result, TotSucc, TotFail, UserSkip, AutoSkip,
			   TotNotBuilt) ->

    case make_one_ix_entry_cached(TestName, LastLogDirData,
				  Label, {true,OldDirs}, Missing) of
	{Result1,Succ,Fail,USkip,ASkip,NotBuilt} ->
	    %% for backwards compatibility
	    AutoSkip1 = case catch AutoSkip+ASkip of
			    {'EXIT',_} -> undefined;
			    Res -> Res
			end,
	    make_all_suites_ix_cached1(Rest, [Result|Result1], TotSucc+Succ,
				       TotFail+Fail, UserSkip+USkip, AutoSkip1,
				       TotNotBuilt+NotBuilt);
	error ->
	    make_all_suites_ix_cached1(Rest, Result, TotSucc, TotFail,
				       UserSkip, AutoSkip, TotNotBuilt)
    end;
make_all_suites_ix_cached1([], Result, TotSucc, TotFail, UserSkip, AutoSkip,
			   TotNotBuilt) ->
    [Result|total_row(TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt, true)].

make_one_ix_entry_cached(TestName, {LogDir,Summary}, Label, All, Missing) ->
    case Summary of
	{Succ,Fail,UserSkip,AutoSkip} ->
	    NotBuilt = not_built(TestName, LogDir, All, Missing),
	    NewResult = make_one_index_entry1(TestName, LogDir, Label,
					      Succ, Fail, UserSkip, AutoSkip,
					      NotBuilt, All, cached),
	    {NewResult,Succ,Fail,UserSkip,AutoSkip,NotBuilt};
	error ->
	    error
    end.

%%-----------------------------------------------------------------
%% Remove log files.
%% Cwd should always be set to the root logdir when finished. 
cleanup() ->
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd("../"),
    {ok,Top} = file:get_cwd(),
    Result =
	case catch try_cleanup(Cwd) of
	    ok ->
		ok;
	    {'EXIT',Reason} ->
		{error,Reason};
	    Error ->
		{error,Error}
	end,
    ok = file:set_cwd(Top),
    Result.

try_cleanup(CTRunDir) ->
    %% ensure we're removing the ct_run directory
    case lists:reverse(filename:split(CTRunDir)) of
	[[$c,$t,$_,$r,$u,$n,$.|_]|_] ->
	    case filelib:wildcard(filename:join(CTRunDir,"ct_run.*")) of
		[] ->				% "double check"
		    rm_dir(CTRunDir);
		_ ->
		    unknown_logdir
	    end;
	_ ->
	    unknown_logdir
    end.

rm_dir(Dir) ->
    case file:list_dir(Dir) of
	{error,Errno} ->
	    exit({ls_failed,Dir,Errno});
	{ok,Files} ->
	    rm_files([filename:join(Dir, F) || F <- Files]),
	    case file:del_dir(Dir) of
		{error,Errno} ->
		    exit({rmdir_failed,Errno});
		ok ->
		    ok
	    end
    end.

rm_files([F | Fs]) ->
    Base = filename:basename(F),
    if Base == "." ; Base == ".." ->
	    rm_files(Fs);
       true ->
	    case file:read_file_info(F) of
		{ok,#file_info{type=directory}} ->
		    rm_dir(F),
		    rm_files(Fs);
		{ok,_Regular} ->
		    case file:delete(F) of
			ok ->
			    rm_files(Fs);
			{error,Errno} ->
			    exit({del_failed,F,Errno})
		    end
	    end
    end;
rm_files([]) ->
    ok.    

%%%-----------------------------------------------------------------
%%% @spec simulate() -> pid()
%%%
%%% @doc Simulate the logger process.
%%%
%%% <p>Simulate the logger process - for use when testing code using
%%% ct_logs logging mechanism without using the ct
%%% environment. (E.g. when testing code with ts)</p>
simulate() ->
    cast(stop),
    S = self(),
    Pid = spawn(fun() -> 
			register(?MODULE,self()),
			S ! {self(),started},
			simulate_logger_loop() 
		end),
    receive {Pid,started} -> Pid end.


simulate_logger_loop() ->
    receive 
    	{log,_,_,List} ->
	    S = [[io_lib:format(Str,Args),io_lib:nl()] || {Str,Args} <- List],
	    io:format("~s",[S]),
	    simulate_logger_loop();
	stop ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% @spec notify_and_lock_file(Files) -> ok
%%%
%%% @doc
%%%
notify_and_lock_file(File) ->    
    case ct_event:is_alive() of
	true ->
	    ct_event:sync_notify(#event{name=start_write_file,
					node=node(),
					data=File});
	false ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% @spec notify_and_unlock_file(Files) -> ok
%%%
%%% @doc
%%%
notify_and_unlock_file(File) ->    
    case ct_event:is_alive() of
	true ->
	    ct_event:sync_notify(#event{name=finished_write_file,
					node=node(),
					data=File});
	false ->
	    ok
    end.

%%%-----------------------------------------------------------------
%%% @spec last_test(Dir) -> string() | false
%%%
%%% @doc
%%%
last_test(Dir) ->
    last_test(filelib:wildcard(filename:join(Dir, "run.[1-2]*")), false).

last_test([Run|Rest], false) ->
    last_test(Rest, Run);
last_test([Run|Rest], Latest) when Run > Latest ->
    last_test(Rest, Run);
last_test([_|Rest], Latest) ->
    last_test(Rest, Latest);
last_test([], Latest) ->
    Latest.

%%%-----------------------------------------------------------------
%%% @spec xhtml(HTML, XHTML) -> HTML | XHTML
%%%
%%% @doc
%%%
xhtml(HTML, XHTML) when is_function(HTML),
			is_function(XHTML) ->
    case get(basic_html) of
	true -> HTML();
	_    -> XHTML()
    end;
xhtml(HTML, XHTML) ->
    case get(basic_html) of
	true -> HTML;
	_    -> XHTML
    end.

%%%-----------------------------------------------------------------
%%% @spec odd_or_even() -> "odd" | "even"
%%%
%%% @doc
%%%
odd_or_even() ->
    case get(odd_or_even) of
	even ->
	    put(odd_or_even, odd),
	    "even";
	_ ->
	    put(odd_or_even, even),
	    "odd"
    end.

%%%-----------------------------------------------------------------
%%% @spec basic_html() -> true | false
%%%
%%% @doc
%%%
basic_html() ->
    case application:get_env(common_test, basic_html) of
	{ok,true} ->
	    true;
	_ ->
	    false
    end.

%%%-----------------------------------------------------------------
%%% @spec locate_default_css_file() -> CSSFile
%%%
%%% @doc
%%%
locate_default_css_file() ->
    {ok,CWD} = file:get_cwd(),
    CSSFileInCwd = filename:join(CWD, ?css_default),
    case filelib:is_file(CSSFileInCwd) of
	true ->
	    CSSFileInCwd;
	false ->
	    CSSResultFile =
		case {whereis(?MODULE),self()} of
		    {Self,Self} ->
			%% executed on the ct_logs process
			filename:join(get(ct_run_dir), ?css_default);
		    _ ->			
			%% executed on other process than ct_logs
			{ok,RunDir} = get_log_dir(true),
			filename:join(RunDir, ?css_default)
		end,
	    case filelib:is_file(CSSResultFile) of
		true ->
		    CSSResultFile;
		false ->
		    %% last resort, try use css file in CT installation
		    CTPath = code:lib_dir(common_test),		
		    filename:join(filename:join(CTPath, "priv"), ?css_default)
	    end
    end.

%%%-----------------------------------------------------------------
%%% @spec make_relative(AbsDir, Cwd) -> RelDir
%%%
%%% @doc Return directory path to File (last element of AbsDir), which
%%%      is the path relative to Cwd. Examples when Cwd == "/ldisk/test/logs":
%%%      make_relative("/ldisk/test/logs/run/trace.log") -> "run/trace.log"
%%%      make_relative("/ldisk/test/trace.log") -> "../trace.log"
%%%      make_relative("/ldisk/test/logs/trace.log") -> "trace.log"
make_relative(AbsDir) ->
    {ok,Cwd} = file:get_cwd(),
    make_relative(AbsDir, Cwd).

make_relative(AbsDir, Cwd) ->
    DirTokens = filename:split(AbsDir),
    CwdTokens = filename:split(Cwd),
    filename:join(make_relative1(DirTokens, CwdTokens)).

make_relative1([T | DirTs], [T | CwdTs]) ->
    make_relative1(DirTs, CwdTs);
make_relative1(Last = [_File], []) ->
    Last;
make_relative1(Last = [_File], CwdTs) ->
    Ups = ["../" || _ <- CwdTs],
    Ups ++ Last;
make_relative1(DirTs, []) ->
    DirTs;
make_relative1(DirTs, CwdTs) ->
    Ups = ["../" || _ <- CwdTs],
    Ups ++ DirTs.

%%%-----------------------------------------------------------------
%%% @spec get_ts_html_wrapper(TestName, PrintLabel, Cwd) -> {Mode,Header,Footer}
%%%
%%% @doc
%%%
get_ts_html_wrapper(TestName, PrintLabel, Cwd) ->
    TestName1 = if is_list(TestName) ->
			lists:flatten(TestName);
		   true ->
			lists:flatten(io_lib:format("~p", [TestName]))
		end,
    Basic = basic_html(),
    LabelStr =
	if not PrintLabel ->
		"";
	   true ->
		case {Basic,application:get_env(common_test, test_label)} of
		    {true,{ok,Lbl}} when Lbl =/= undefined ->
			"<h1><font color=\"green\">" ++ Lbl ++ "</font></h1>\n";
		    {_,{ok,Lbl}} when Lbl =/= undefined ->
			"<div class=\"label\">'" ++ Lbl ++ "'</div>\n";
		    _ ->
			""
		end
	end,
    CTPath = code:lib_dir(common_test),
    {ok,CtLogdir} = get_log_dir(true),
    AllRuns = make_relative(filename:join(filename:dirname(CtLogdir),
					  ?all_runs_name), Cwd),
    TestIndex = make_relative(filename:join(filename:dirname(CtLogdir),
					    ?index_name), Cwd),
    case Basic of
	true ->
	    TileFile = filename:join(filename:join(CTPath,"priv"),"tile1.jpg"),
	    Bgr = " background=\"" ++ TileFile ++ "\"",
	    Copyright =
		     ["<p><font size=\"-1\">\n",
		      "Copyright &copy; ", year(),
		      " <a href=\"http://www.erlang.org\">",
		      "Open Telecom Platform</a><br>\n",
		      "Updated: <!date>", current_time(), "<!/date>",
		      "<br>\n</font></p>\n"],
	    {basic_html,
	     ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n",
	      "<html>\n",
	      "<head><title>", TestName1, "</title>\n",
	      "<meta http-equiv=\"cache-control\" content=\"no-cache\">\n",
	      "</head>\n",
	      "<body", Bgr, " bgcolor=\"white\" text=\"black\" ",
	      "link=\"blue\" vlink=\"purple\" alink=\"red\">\n",
	      LabelStr, "\n"],
	     ["<center>\n<br><hr><p>\n",
	      "<a href=\"", AllRuns,
	      "\">Test run history\n</a>  |  ",
	      "<a href=\"", TestIndex,
	      "\">Top level test index\n</a>\n</p>\n",
	      Copyright,"</center>\n</body>\n</html>\n"]};
	_ ->
	    Copyright = 
		["<div class=\"copyright\">",
		 "Copyright &copy; ", year(),
		 " <a href=\"http://www.erlang.org\">",
		 "Open Telecom Platform</a><br />\n",
		 "Updated: <!date>", current_time(), "<!/date>",
		 "<br />\n</div>\n"],
	    CSSFile = xhtml(fun() -> "" end, 
			    fun() -> make_relative(locate_default_css_file(), Cwd) end),
	    {xhtml,
	     ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n",
	      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
	      "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n",
	      "<head>\n<title>", TestName1, "</title>\n",
	      "<meta http-equiv=\"cache-control\" content=\"no-cache\">\n",
	      "<link rel=\"stylesheet\" href=\"", CSSFile, "\" type=\"text/css\">",
	      "</head>\n","<body>\n", 
	      LabelStr, "\n"],
	     ["<center>\n<br /><hr /><p>\n",
	      "<a href=\"", AllRuns,
	      "\">Test run history\n</a>  |  ",
	      "<a href=\"", TestIndex,
	      "\">Top level test index\n</a>\n</p>\n",
	      Copyright,"</center>\n</body>\n</html>\n"]}
    end.
