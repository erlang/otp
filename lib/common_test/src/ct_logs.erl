%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2017. All Rights Reserved.
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

%%% @doc Logging functionality for Common Test Framework.
%%%
%%% <p>This module implements
%%% <ul>
%%% <li>Internal logging of activities in Common Test Framework</li>
%%% <li>Compilation of test results into index pages on several levels</li>
%%% </ul>
%%% </p>

-module(ct_logs).

-export([init/2, close/2, init_tc/1, end_tc/1]).
-export([register_groupleader/2, unregister_groupleader/1]).
-export([get_log_dir/0, get_log_dir/1]).
-export([log/3, start_log/1, cont_log/2, cont_log_no_timestamp/2, end_log/0]).
-export([set_stylesheet/2, clear_stylesheet/1]).
-export([add_external_logs/1, add_link/3]).
-export([make_last_run_index/0]).
-export([make_all_suites_index/1,make_all_runs_index/1]).
-export([get_ts_html_wrapper/5, escape_chars/1]).
-export([xhtml/2, locate_priv_file/1, make_relative/1]).
-export([insert_javascript/1]).
-export([uri/1]).
-export([parse_keep_logs/1]).

%% Logging stuff directly from testcase
-export([tc_log/3, tc_log/4, tc_log/5, tc_log/6,
	 tc_log_async/3, tc_log_async/5,
	 tc_print/3, tc_print/4, tc_print/5,
	 tc_pal/3, tc_pal/4, tc_pal/5, ct_log/3,
	 basic_html/0]).

%% Simulate logger process for use without ct environment running
-export([simulate/0]).

-include("ct.hrl").
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
-define(log_cache_name, "ct_log_cache").
-define(misc_io_log, "misc_io.log.html").
-define(coverlog_name, "cover.html"). % must be same as in test_server_ctrl

-define(table_color1,"#ADD8E6").
-define(table_color2,"#E4F0FE").
-define(table_color3,"#F0F8FF").

-define(testname_width, 60).

-define(abs(Name), filename:absname(Name)).

-define(now, os:timestamp()).

-record(log_cache, {version,
		    all_runs = [],
		    tests = []}).

%%%-----------------------------------------------------------------
%%% @spec init(Mode, Verbosity) -> Result
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
init(Mode, Verbosity) ->
    Self = self(),
    Pid = spawn_link(fun() -> logger(Self, Mode, Verbosity) end),
    MRef = erlang:monitor(process,Pid),
    receive 
	{started,Pid,Result} -> 
	    erlang:demonitor(MRef, [flush]),
	    Result;
	{'DOWN',MRef,process,_,Reason} ->
	    exit({could_not_start_process,?MODULE,Reason})
    end.
    
date2str({{YY,MM,DD},{H,M,S}}) ->
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w_~2.2.0w.~2.2.0w.~2.2.0w",
				[YY,MM,DD,H,M,S])).
logdir_prefix() ->
    "ct_run".
logdir_node_prefix() ->
    logdir_prefix() ++ "." ++ atom_to_list(node()).

make_dirname(DateTime) ->
    logdir_node_prefix() ++ "." ++ date2str(DateTime).

datestr_from_dirname([Y1,Y2,Y3,Y4,$-,Mo1,Mo2,$-,D1,D2,$_,
		      H1,H2,$.,M1,M2,$.,S1,S2 | _]) ->
    [Y1,Y2,Y3,Y4,$-,Mo1,Mo2,$-,D1,D2,$_,
     H1,H2,$.,M1,M2,$.,S1,S2];
datestr_from_dirname([_Ch | Rest]) ->
    datestr_from_dirname(Rest);
datestr_from_dirname([]) ->
    "".

%%%-----------------------------------------------------------------
%%% @spec close(Info, StartDir) -> ok
%%%
%%% @doc Create index pages with test results and close the CT Log
%%% (tool-internal use only).
close(Info, StartDir) ->
    %% close executes on the ct_util process, not on the logger process
    %% so we need to use a local copy of the log cache data
    LogCacheBin = 
	case make_last_run_index() of
	    {error, Reason} ->  % log server not responding
		io:format("Warning! ct_logs not responding: ~tp~n", [Reason]),
		undefined;
	    LCB ->
		LCB
	end,
    put(ct_log_cache,LogCacheBin),
    Cache2File = fun() ->
			 case get(ct_log_cache) of
			     undefined ->
				 ok;
			     CacheBin ->
				 %% save final version of the log cache to file
				 _ = file:write_file(?log_cache_name,CacheBin),
				 put(ct_log_cache,undefined)
			 end
		 end,
				 
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
		    io:format("Warning! Cleanup failed: ~tp~n", [Error])
	    end,
	    _ = make_all_suites_index(stop),
	    make_all_runs_index(stop),
	    Cache2File();
       true -> 
	    ok = file:set_cwd(".."),
	    _ = make_all_suites_index(stop),
	    make_all_runs_index(stop),
	    Cache2File(),
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
    get_log_dir(false).

%%%-----------------------------------------------------------------
%%% @spec get_log_dir(ReturnAbsName) -> {ok,Dir} | {error,Reason}
get_log_dir(ReturnAbsName) ->
    case call({get_log_dir,ReturnAbsName}) of
	{error,does_not_exist} when ReturnAbsName == true ->
	    {ok,filename:absname(".")};
	{error,does_not_exist} ->
	    {ok,"."};
	Result ->
	    Result
    end.

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
	    Pid ! {Msg,{self(),Ref}},
	    receive
		{Ref, Result} -> 
		    erlang:demonitor(MRef, [flush]),
		    Result;
		{'DOWN',MRef,process,_,Reason}  -> 
		    {error,{process_down,?MODULE,Reason}}
	    end
    end.

return({To,Ref},Result) ->
    To ! {Ref, Result},
    ok.

cast(Msg) ->
    case whereis(?MODULE) of
	undefined ->
	    io:format("Warning: ct_logs not started~n"),
	    {_,_,_,_,_,_,Content,_} = Msg,
	    FormatArgs = get_format_args(Content),
	    _ = [io:format(Format, Args) || {Format, Args} <- FormatArgs],
	    ok;
	_Pid ->
	    ?MODULE ! Msg,
	    ok
    end.

get_format_args(Content) ->
    lists:map(fun(C) ->
		  case C of
		      {_, FA, _} -> FA;
		      {_, _} -> C
		  end
	      end, Content).

%%%-----------------------------------------------------------------
%%% @spec init_tc(RefreshLog) -> ok
%%%
%%% @doc Test case initiation (tool-internal use only).
%%%
%%% <p>This function is called by ct_framework:init_tc/3</p>
init_tc(RefreshLog) ->
    call({init_tc,self(),group_leader(),RefreshLog}),
    tc_io_format(group_leader(), xhtml("", "<br />"), []),
    ok.

%%%-----------------------------------------------------------------
%%% @spec end_tc(TCPid) -> ok
%%%
%%% @doc Test case clean up (tool-internal use only).
%%%
%%% <p>This function is called by ct_framework:end_tc/3</p>
end_tc(TCPid) ->
    %% use call here so that the TC process will wait and receive
    %% possible exit signals from ct_logs before end_tc returns ok 
    call({end_tc,TCPid}).

%%%-----------------------------------------------------------------
%%% @spec register_groupleader(Pid,GroupLeader) -> ok
%%%
%%% @doc To enable logging to a group leader (tool-internal use only).
%%%
%%% <p>This function is called by ct_framework:report/2</p>
register_groupleader(Pid,GroupLeader) ->
    call({register_groupleader,Pid,GroupLeader}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec unregister_groupleader(Pid) -> ok
%%%
%%% @doc To disable logging to a group leader (tool-internal use only).
%%%
%%% <p>This function is called by ct_framework:report/2</p>
unregister_groupleader(Pid) ->
    call({unregister_groupleader,Pid}),
    ok.

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
    cast({log,sync,self(),group_leader(),ct_internal,?MAX_IMPORTANCE,
	  [{hd,int_header(),[log_timestamp(?now),Heading]},
	   {Format,Args},
	   {ft,int_footer(),[]}],
	 true}),
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
    cast({log,sync,self(),group_leader(),ct_internal,?MAX_IMPORTANCE,
	  [{hd,int_header(),[log_timestamp(?now),Heading]}],false}),
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
    cast({log,sync,self(),group_leader(),ct_internal,?MAX_IMPORTANCE,
	  [{Format,Args}],true}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec cont_log_no_timestamp(Format,Args) -> ok
%%% 
%%% @doc Adds information about an activity (tool-internal use only).
%%%
%%% @see start_log/1
%%% @see end_log/0
cont_log_no_timestamp([],[]) ->
    ok;
cont_log_no_timestamp(Format,Args) ->
    cast({log,sync,self(),group_leader(),ct_internal,?MAX_IMPORTANCE,
	  [{Format,Args}],true}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec end_log() -> ok
%%% 
%%% @doc Ends the logging of an activity (tool-internal use only).
%%%
%%% @see start_log/1
%%% @see cont_log/2
end_log() ->
    cast({log,sync,self(),group_leader(),ct_internal,?MAX_IMPORTANCE,
	  [{ft,int_footer(), []}],false}),
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
    [cont_log("<a href=\"~ts\">~ts</a>\n",
	      [uri(filename:join("log_private",Log)),Log]) || Log <- Logs],
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
    log(Heading,"<a href=\"~ts\" type=~tp>~ts</a>\n",
	[uri(filename:join("log_private",File)),Type,File]).


%%%-----------------------------------------------------------------
%%% @spec tc_log(Category,Format,Args) -> ok
%%% @equiv tc_log(Category,?STD_IMPORTANCE,Format,Args)
tc_log(Category,Format,Args) ->
    tc_log(Category,?STD_IMPORTANCE,"User",Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec tc_log(Category,Importance,Format,Args) -> ok
%%% @equiv tc_log(Category,Importance,"User",Format,Args)
tc_log(Category,Importance,Format,Args) ->
    tc_log(Category,Importance,"User",Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec tc_log(Category,Importance,Format,Args) -> ok
%%% @equiv tc_log(Category,Importance,"User",Format,Args)
tc_log(Category,Importance,Format,Args,Opts) ->
    tc_log(Category,Importance,"User",Format,Args,Opts).

%%%-----------------------------------------------------------------
%%% @spec tc_log(Category,Importance,Heading,Format,Args,Opts) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Heading = string()
%%%      Format = string()
%%%      Args = list()
%%%      Opts = list()
%%%
%%% @doc Printout from a testcase. 
%%%
%%% <p>This function is called by <code>ct</code> when logging
%%% stuff directly from a testcase (i.e. not from within the CT
%%% framework).</p>
tc_log(Category,Importance,Heading,Format,Args,Opts) ->
    Data = 
	case lists:member(no_css, Opts) of
	    true ->
		[{Format,Args}];
	    false ->
                Heading1 =
                    case proplists:get_value(heading, Opts) of
                        undefined -> Heading;
                        Str       -> Str
                    end,
		[{hd,div_header(Category,Heading1),[]},
		 {Format,Args},
		 {ft,div_footer(),[]}]
	end,
    cast({log,sync,self(),group_leader(),Category,Importance,Data,
	  lists:member(esc_chars, Opts)}),
    ok.

%%%-----------------------------------------------------------------
%%% @spec tc_log_async(Category,Format,Args) -> ok
%%% @equiv tc_log_async(Category,?STD_IMPORTANCE,"User",Format,Args)
tc_log_async(Category,Format,Args) ->
    tc_log_async(Category,?STD_IMPORTANCE,"User",Format,Args).

%%%-----------------------------------------------------------------
%%% @spec tc_log_async(Category,Importance,Format,Args) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Heading = string()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Internal use only.
%%%
%%% <p>This function is used to perform asynchronous printouts
%%% towards the test server IO handler. This is necessary in order
%%% to avoid deadlocks when e.g. the hook that handles SASL printouts
%%% prints to the test case log file at the same time test server
%%% asks ct_logs for an html wrapper.</p>
tc_log_async(Category,Importance,Heading,Format,Args) ->
    cast({log,async,self(),group_leader(),Category,Importance,
	  [{hd,div_header(Category,Heading),[]},
	   {Format,Args},
	   {ft,div_footer(),[]}],
	  true}),
    ok.
%%%-----------------------------------------------------------------
%%% @spec tc_print(Category,Format,Args)
%%% @equiv tc_print(Category,?STD_IMPORTANCE,Format,Args,[])
tc_print(Category,Format,Args) ->
    tc_print(Category,?STD_IMPORTANCE,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec tc_print(Category,Importance,Format,Args)
%%% @equiv tc_print(Category,Importance,Format,Args,[])
tc_print(Category,Importance,Format,Args) ->
    tc_print(Category,Importance,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec tc_print(Category,Importance,Format,Args,Opts) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Format = string()
%%%      Args = list()
%%%      Opts = list()
%%%
%%% @doc Console printout from a testcase. 
%%%
%%% <p>This function is called by <code>ct</code> when printing
%%% stuff from a testcase on the user console.</p>
tc_print(Category,Importance,Format,Args,Opts) ->
    VLvl = case ct_util:get_verbosity(Category) of
	       undefined -> 
		   ct_util:get_verbosity('$unspecified');
	       {error,bad_invocation} ->
		   ?MAX_VERBOSITY;
	       {error,_Failure} ->
		   ?MAX_VERBOSITY;
	       Val ->
		   Val
	   end,
    if Importance >= (100-VLvl) ->
            Heading =
                case proplists:get_value(heading, Opts) of
                    undefined -> atom_to_list(Category);
                    Hd        -> Hd
                end,
            Str = lists:concat([get_header(Heading),Format,"\n\n"]),
            try
                io:format(?def_gl, Str, Args)
            catch
                %% default group leader probably not started, or has stopped
                _:_ -> io:format(user, Str, Args)
            end,
	    ok;
       true ->
	    ok
    end.

get_header("default") ->
    io_lib:format("\n-----------------------------"
		  "-----------------------\n~s\n",
		  [log_timestamp(?now)]);
get_header(Heading) ->
    io_lib:format("\n-----------------------------"
		  "-----------------------\n~ts ~s\n",
		  [Heading,log_timestamp(?now)]).    
    

%%%-----------------------------------------------------------------
%%% @spec tc_pal(Category,Format,Args) -> ok
%%% @equiv tc_pal(Category,?STD_IMPORTANCE,Format,Args,[]) -> ok
tc_pal(Category,Format,Args) ->
    tc_pal(Category,?STD_IMPORTANCE,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec tc_pal(Category,Importance,Format,Args) -> ok
%%% @equiv tc_pal(Category,Importance,Format,Args,[]) -> ok
tc_pal(Category,Importance,Format,Args) ->
    tc_pal(Category,Importance,Format,Args,[]).

%%%-----------------------------------------------------------------
%%% @spec tc_pal(Category,Importance,Format,Args,Opts) -> ok
%%%      Category = atom()
%%%      Importance = integer()
%%%      Format = string()
%%%      Args = list()
%%%      Opts = list()
%%%
%%% @doc Print and log from a testcase. 
%%%
%%% <p>This function is called by <code>ct</code> when logging
%%% stuff directly from a testcase. The info is written both in the
%%% log and on the console.</p>
tc_pal(Category,Importance,Format,Args,Opts) ->
    tc_print(Category,Importance,Format,Args,Opts),
    tc_log(Category,Importance,"User",Format,Args,[esc_chars|Opts]).

%%%-----------------------------------------------------------------
%%% @spec ct_log(Category,Format,Args) -> ok
%%%      Category = atom()
%%%      Format = string()
%%%      Args = list()
%%%
%%% @doc Print to the ct framework log
%%%
%%% <p>This function is called by internal ct functions to
%%% force logging to the ct framework log</p>
ct_log(Category,Format,Args) ->
    cast({ct_log,[{hd,div_header(Category),[]},
		  {Format,Args},
		  {ft,div_footer(),[]}],
	  true}),
    ok.


%%%=================================================================
%%% Internal functions
int_header() ->
    "</pre>\n<div class=\"ct_internal\"><pre><b>*** CT ~s *** ~ts</b>".
int_footer() ->
    "</pre></div>\n<pre>".

div_header(Class) ->
    div_header(Class,"User").
div_header(Class,Heading) ->
    "\n</pre>\n<div class=\"" ++ atom_to_list(Class) ++ "\"><pre><b>*** "
	++ Heading ++ " " ++ log_timestamp(?now) ++ " ***</b>".
div_footer() ->
    "</pre></div>\n<pre>".

maybe_log_timestamp() ->
    {MS,S,US} = ?now,
    case get(log_timestamp) of
	{MS,S,_} ->
	    ok;
	_ ->
	    cast({log,sync,self(),group_leader(),ct_internal,?MAX_IMPORTANCE,
		  [{hd,"<i>~s</i>",[log_timestamp({MS,S,US})]}],false})
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
		      stylesheet,
		      async_print_jobs,
		      tc_esc_chars}).

logger(Parent, Mode, Verbosity) ->
    register(?MODULE,self()),
    ct_util:mark_process(),
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

    _ = file:make_dir(Dir),
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
	    PrivFiles = [?css_default,?jquery_script,?tablesorter_script],
	    PrivFilesSrc = [filename:join(filename:join(CTPath, "priv"), F) ||
			       F <- PrivFiles],
	    PrivFilesDestTop = [filename:join(Cwd, F) || F <- PrivFiles],
	    PrivFilesDestRun = [filename:join(AbsDir, F) || F <- PrivFiles],
	    case copy_priv_files(PrivFilesSrc, PrivFilesDestTop) of
		{error,Src1,Dest1,Reason1} ->
		    io:format(?def_gl, "ERROR! "++
				  "Priv file ~tp could not be copied to ~tp. "++
				  "Reason: ~tp~n",
			      [Src1,Dest1,Reason1]),
		    exit({priv_file_error,Dest1});
		ok ->
		    case copy_priv_files(PrivFilesSrc, PrivFilesDestRun) of
			{error,Src2,Dest2,Reason2} ->
			    io:format(?def_gl,
				      "ERROR! "++
				      "Priv file ~tp could not be copied to ~tp. "
				      ++"Reason: ~tp~n",
				      [Src2,Dest2,Reason2]),
			    exit({priv_file_error,Dest2});
			ok ->
			    ok
		    end
	    end
    end,

    _ = test_server_io:start_link(),
    MiscIoName = filename:join(Dir, ?misc_io_log),
    {ok,MiscIoFd} = file:open(MiscIoName,
			      [write,{encoding,utf8}]),
    test_server_io:set_fd(unexpected_io, MiscIoFd),

    {MiscIoHeader,MiscIoFooter} =
	case get_ts_html_wrapper("Pre/post-test I/O log", Dir, false,
				 Dir, undefined, utf8) of
	    {basic_html,UH,UF} ->
		{UH,UF};
	    {xhtml,UH,UF} ->
		{UH,UF}
	end,
    io:put_chars(MiscIoFd,
		 [MiscIoHeader,
		  "<a name=\"pretest\"></a>\n",
		  xhtml("<br>\n<h2>Pre-test Log</h2>",
			"<br />\n<h3>PRE-TEST LOG</h3>"),
		  "\n<pre>\n"]),
    MiscIoDivider =
	"\n<a name=\"posttest\"></a>\n"++
	xhtml("</pre>\n<br><h2>Post-test Log</h2>\n<pre>\n",
	      "</pre>\n<br />\n<h3>POST-TEST LOG</h3>\n<pre>\n"),
    ct_util:set_testdata_async({misc_io_log,{filename:absname(MiscIoName),
					     MiscIoDivider,MiscIoFooter}}),

    ct_event:notify(#event{name=start_logging,node=node(),
			   data=AbsDir}),
    make_all_runs_index(start),
    _ = make_all_suites_index(start),
    case Mode of
	interactive -> interactive_link();
	_ -> ok
    end,
    ok = file:set_cwd(Dir),
    _ = make_last_run_index(Time),
    CtLogFd = open_ctlog(?misc_io_log),
    io:format(CtLogFd,int_header()++int_footer(),
	      [log_timestamp(?now),"Common Test Logger started"]),
    Parent ! {started,self(),{Time,filename:absname("")}},
    set_evmgr_gl(CtLogFd),

    %% save verbosity levels in dictionary for fast lookups
    io:format(CtLogFd, "\nVERBOSITY LEVELS:\n", []),
    case proplists:get_value('$unspecified', Verbosity) of
	undefined -> ok;
	GenLvl    -> io:format(CtLogFd, "~-25s~3w~n",
			       ["general level",GenLvl])
    end,
    _ = [begin put({verbosity,Cat},VLvl),
	     if Cat == '$unspecified' ->
		   ok;
		true ->
		   io:format(CtLogFd, "~-25w~3w~n", [Cat,VLvl])
	     end
	 end || {Cat,VLvl} <- Verbosity],
    io:nl(CtLogFd),
    TcEscChars = case application:get_env(common_test, esc_chars) of
		   {ok,ECBool} -> ECBool;
		   _           -> true
	       end,
    logger_loop(#logger_state{parent=Parent,
			      log_dir=AbsDir,
			      start_time=Time,
			      orig_GL=group_leader(),
			      ct_log_fd=CtLogFd,
			      tc_groupleaders=[],
			      async_print_jobs=[],
			      tc_esc_chars=TcEscChars}).

copy_priv_files([SrcF | SrcFs], [DestF | DestFs]) ->
    case file:copy(SrcF, DestF) of
	{error,Reason} ->
	    {error,SrcF,DestF,Reason};
	_ ->
	    copy_priv_files(SrcFs, DestFs)
    end;
copy_priv_files([], []) ->
    ok.

logger_loop(State) ->
    receive
	{log,SyncOrAsync,Pid,GL,Category,Importance,Content,EscChars} ->
	    VLvl = case Category of
		       ct_internal ->
			   ?MAX_VERBOSITY;
		       _ ->
			   case get({verbosity,Category}) of
			       undefined -> get({verbosity,'$unspecified'});
			       Val       -> Val
			end
		end,
	    if Importance >= (100-VLvl) ->
		    CtLogFd = State#logger_state.ct_log_fd,
		    DoEscChars = State#logger_state.tc_esc_chars and EscChars,
		    case get_groupleader(Pid, GL, State) of
			{tc_log,TCGL,TCGLs} ->
			    case erlang:is_process_alive(TCGL) of
				true ->
				    State1 = print_to_log(SyncOrAsync, Pid,
							  Category, TCGL, Content,
							  DoEscChars, State),
				    logger_loop(State1#logger_state{
						  tc_groupleaders = TCGLs});
				false ->
				    %% Group leader is dead, so write to the
				    %% CtLog or unexpected_io log instead
				    unexpected_io(Pid, Category, Importance,
						  Content, CtLogFd, DoEscChars),

				    logger_loop(State)			    
			    end;
			{ct_log,_Fd,TCGLs} ->
			    %% If category is ct_internal then write
			    %% to ct_log, else write to unexpected_io
			    %% log
			    unexpected_io(Pid, Category, Importance, Content,
					  CtLogFd, DoEscChars),
			    logger_loop(State#logger_state{
					  tc_groupleaders = TCGLs})
		    end;
	       true ->
		    logger_loop(State)
	    end;			
	{{init_tc,TCPid,GL,RefreshLog},From} ->
	    %% make sure no IO for this test case from the
	    %% CT logger gets rejected
	    test_server:permit_io(GL, self()),
	    IoFormat = fun tc_io_format/3,
	    print_style(GL, IoFormat, State#logger_state.stylesheet),
	    set_evmgr_gl(GL),
	    TCGLs = add_tc_gl(TCPid,GL,State),
	    _ = if not RefreshLog ->
		    ok;
	       true ->
		    make_last_run_index(State#logger_state.start_time)
	    end,
	    return(From,ok),
	    logger_loop(State#logger_state{tc_groupleaders = TCGLs});
	{{end_tc,TCPid},From} ->
	    set_evmgr_gl(State#logger_state.ct_log_fd),
	    return(From,ok),
	    logger_loop(State#logger_state{tc_groupleaders =
					       rm_tc_gl(TCPid,State)});
	{{register_groupleader,Pid,GL},From} ->
	    GLs = add_tc_gl(Pid,GL,State),
	    return(From,ok),
	    logger_loop(State#logger_state{tc_groupleaders = GLs});
	{{unregister_groupleader,Pid},From} ->
	    return(From,ok),
	    logger_loop(State#logger_state{tc_groupleaders =
					       rm_tc_gl(Pid,State)});
	{{get_log_dir,true},From} ->
	    return(From,{ok,State#logger_state.log_dir}),
	    logger_loop(State);
	{{get_log_dir,false},From} ->
	    return(From,{ok,filename:basename(State#logger_state.log_dir)}),
	    logger_loop(State);
	{make_last_run_index,From} ->
	    _ = make_last_run_index(State#logger_state.start_time),
	    return(From,get(ct_log_cache)),
	    logger_loop(State);
	{set_stylesheet,_,SSFile} when State#logger_state.stylesheet ==
				       SSFile ->
	    logger_loop(State);
	{set_stylesheet,TC,SSFile} ->
	    Fd = State#logger_state.ct_log_fd,
	    io:format(Fd, "~tp loading external style sheet: ~ts~n",
		      [TC,SSFile]),
	    logger_loop(State#logger_state{stylesheet = SSFile});
	{clear_stylesheet,_} when State#logger_state.stylesheet == undefined ->
	    logger_loop(State);
	{clear_stylesheet,_} ->
	    logger_loop(State#logger_state{stylesheet = undefined});
	{ct_log,Content,EscChars} ->
	    Str = lists:map(fun({_HdOrFt,Str,Args}) ->
				    [io_lib:format(Str,Args),io_lib:nl()];
			       ({Str,Args}) when EscChars ->
				    Io = io_lib:format(Str,Args),
				    [escape_chars(Io),io_lib:nl()];
			       ({Str,Args}) ->
				    [io_lib:format(Str,Args),io_lib:nl()]
			    end, Content),
	    Fd = State#logger_state.ct_log_fd,
	    io:format(Fd, "~ts", [Str]),
	    logger_loop(State);
	{'DOWN',Ref,_,_Pid,_} ->
	    %% there might be print jobs executing in parallel with ct_logs
	    %% and whenever one is finished (indicated by 'DOWN'), the
	    %% next job should be spawned
	    case lists:delete(Ref, State#logger_state.async_print_jobs) of
		[] ->
		    logger_loop(State#logger_state{async_print_jobs = []});
		Jobs ->
		    [Next|JobsRev] = lists:reverse(Jobs),
		    Jobs1 = [print_next(Next)|lists:reverse(JobsRev)],
		    logger_loop(State#logger_state{async_print_jobs = Jobs1})
	    end;
	stop ->
	    io:format(State#logger_state.ct_log_fd,
		      int_header()++int_footer(),
		      [log_timestamp(?now),"Common Test Logger finished"]),
	    close_ctlog(State#logger_state.ct_log_fd),
	    ok
    end.

create_io_fun(FromPid, CtLogFd, EscChars) ->
    %% we have to build one io-list of all strings
    %% before printing, or other io printouts (made in
    %% parallel) may get printed between this header 
    %% and footer
    fun(FormatData, IoList) ->
	    {Escapable,Str,Args} =
		case FormatData of
		    {_HdOrFt,S,A} -> {false,S,A};
		    {S,A}         -> {true,S,A}
		end,
	    try io_lib:format(Str, Args) of
		IoStr when Escapable, EscChars, IoList == [] ->
		    escape_chars(IoStr);
		IoStr when Escapable, EscChars ->
		    [IoList,"\n",escape_chars(IoStr)];
		IoStr when IoList == [] ->
		    IoStr;
		IoStr ->
		    [IoList,"\n",IoStr]
	    catch
		_:_Reason ->
		    io:format(CtLogFd, "Logging fails! Str: ~tp, Args: ~tp~n",
			      [Str,Args]),
		    %% stop the testcase, we need to see the fault
		    exit(FromPid, {log_printout_error,Str,Args}),
		    []
	    end
    end.

escape_chars([Bin | Io]) when is_binary(Bin) ->
    [Bin | escape_chars(Io)];
escape_chars([List | Io]) when is_list(List) ->
    [escape_chars(List) | escape_chars(Io)];
escape_chars([$< | Io]) ->
    ["&lt;" | escape_chars(Io)];
escape_chars([$> | Io]) ->
    ["&gt;" | escape_chars(Io)];
escape_chars([$& | Io]) ->
    ["&amp;" | escape_chars(Io)];
escape_chars([Char | Io]) when is_integer(Char) ->
    [Char | escape_chars(Io)];
escape_chars([]) ->
    [];
escape_chars(Bin) ->
    Bin.

print_to_log(sync, FromPid, Category, TCGL, Content, EscChars, State) ->
    %% in some situations (exceptions), the printout is made from the
    %% test server IO process and there's no valid group leader to send to
    CtLogFd = State#logger_state.ct_log_fd,
    if FromPid /= TCGL ->
	    IoFun = create_io_fun(FromPid, CtLogFd, EscChars),
	    IoList = lists:foldl(IoFun, [], Content),
	    try tc_io_format(TCGL, "~ts", [IoList]) of
		ok -> ok
	    catch
		_:_ ->
		    io:format(TCGL,"~ts", [IoList])
	    end;
       true ->
	    unexpected_io(FromPid, Category, ?MAX_IMPORTANCE, Content,
			  CtLogFd, EscChars)
    end,
    State;

print_to_log(async, FromPid, Category, TCGL, Content, EscChars, State) ->
    %% in some situations (exceptions), the printout is made from the
    %% test server IO process and there's no valid group leader to send to
    CtLogFd = State#logger_state.ct_log_fd,
    Printer =
	if FromPid /= TCGL ->
		IoFun = create_io_fun(FromPid, CtLogFd, EscChars),
		fun() ->
                        ct_util:mark_process(),
			test_server:permit_io(TCGL, self()),

			%% Since asynchronous io gets can get buffered if
			%% the file system is slow, there is also a risk that
			%% the group leader has terminated before we get to
			%% the io:format(GL, ...) call. We check this and
			%% print "expired" messages to the unexpected io
			%% log instead (best we can do).

			case erlang:is_process_alive(TCGL) of
			    true ->
				try tc_io_format(TCGL, "~ts",
					      [lists:foldl(IoFun,[],Content)]) of
				    _ -> ok
				catch
				    _:terminated ->
					unexpected_io(FromPid, Category,
						      ?MAX_IMPORTANCE,
						      Content, CtLogFd, EscChars);
				    _:_ ->
					io:format(TCGL, "~ts",
						  [lists:foldl(IoFun,[],Content)])
				end;
			    false ->
				unexpected_io(FromPid, Category,
					      ?MAX_IMPORTANCE,
					      Content, CtLogFd, EscChars)
			end
		end;
	   true ->
		fun() ->
                        ct_util:mark_process(),
			unexpected_io(FromPid, Category, ?MAX_IMPORTANCE,
				      Content, CtLogFd, EscChars)
		end
	end,
    case State#logger_state.async_print_jobs of
	[] ->
	    {_Pid,Ref} = spawn_monitor(Printer),
	    State#logger_state{async_print_jobs = [Ref]};
	Queue ->
	    State#logger_state{async_print_jobs = [Printer|Queue]}
    end.

print_next(PrintFun) ->
    {_Pid,Ref} = spawn_monitor(PrintFun),
    Ref.

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

open_ctlog(MiscIoName) ->
    {ok,Fd} = file:open(?ct_log_name,[write,{encoding,utf8}]),
    io:format(Fd, header("Common Test Framework Log", {[],[1,2],[]}), []),
    case file:consult(ct_run:variables_file_name("../")) of
	{ok,Vars} ->
	    io:format(Fd, config_table(Vars), []);
	{error,Reason} ->
	    {ok,Cwd} = file:get_cwd(),
	    Dir = filename:dirname(Cwd),
	    Variables = ct_run:variables_file_name(Dir),
	    io:format(Fd,
		      "Can not read the file \'~ts\' Reason: ~tw\n"
		      "No configuration found for test!!\n",
		      [Variables,Reason])
    end,
    io:format(Fd, 
	      xhtml("<br><br><h2>Pre/post-test I/O Log</h2>\n",
		    "<br /><br />\n<h4>PRE/POST TEST I/O LOG</h4>\n"), []),    
    io:format(Fd,
	      "\n<ul>\n"
	      "<li><a href=\"~ts#pretest\">"
	      "View I/O logged before the test run</a></li>\n"
	      "<li><a href=\"~ts#posttest\">"
	      "View I/O logged after the test run</a></li>\n</ul>\n",
	      [MiscIoName,MiscIoName]),

    print_style(Fd, fun io:format/3, undefined),
    io:format(Fd, 
	      xhtml("<br><h2>Progress Log</h2>\n<pre>\n",
		    "<br />\n<h4>PROGRESS LOG</h4>\n<pre>\n"), []),
    Fd.

print_style(Fd, IoFormat, undefined) ->
    case basic_html() of
	true ->
	    Style = "<style>\n
		div.ct_internal { background:lightgrey; color:black; }\n
		div.default     { background:lightgreen; color:black; }\n
		</style>\n",
	    IoFormat(Fd, Style, []);
	_ ->
	    ok
    end;

print_style(Fd, IoFormat, StyleSheet) ->
    case file:read_file(StyleSheet) of
	{ok,Bin} ->
	    Str = b2s(Bin,encoding(StyleSheet)),
            case re:run(Str,"<style>.*</style>",
                        [dotall,caseless,{capture,all,list}]) of
                nomatch ->
                    case re:run(Str,"</?style>",[caseless,{capture,all,list}]) of
                        nomatch ->
                            IoFormat(Fd,"<style>\n~ts</style>\n",[Str]);
                        {match,["</"++_]} ->
                            print_style_error(Fd, IoFormat,
                                              StyleSheet,
                                              missing_style_start_tag);
                        {match,[_]} ->
                            print_style_error(Fd, IoFormat,
                                              StyleSheet,missing_style_end_tag)
                    end;
                {match,[Style]} ->
                    IoFormat(Fd,"~ts\n",[Style])
            end;
	{error,Reason} ->
	    print_style_error(Fd,IoFormat,StyleSheet,Reason)
    end.

print_style_error(Fd, IoFormat, StyleSheet, Reason) ->
    IO = io_lib:format("\n<!-- Failed to load stylesheet ~ts: ~tp -->\n",
		       [StyleSheet,Reason]),
    IoFormat(Fd, IO, []),
    print_style(Fd, IoFormat, undefined).

close_ctlog(Fd) ->
    io:format(Fd, "\n</pre>\n", []),
    io:format(Fd, [xhtml("<br><br>\n", "<br /><br />\n") | footer()], []),
    ok = file:close(Fd).

%%%-----------------------------------------------------------------
%%% tc_io_format/3
%%% Tell common_test's IO server (group leader) not to escape
%%% HTML characters.

-spec tc_io_format(io:device(), io:format(), [term()]) -> 'ok'.

tc_io_format(Fd, Format0, Args) ->
    %% We know that the specially wrapped format string is handled
    %% by our IO server, but Dialyzer does not and would tell us
    %% that the call to io:format/3 would fail. Therefore, we must
    %% fool dialyzer.

    Format = case cloaked_true() of
		 true -> ["$tc_html",Format0];
		 false -> Format0		%Never happens.
	     end,
    io:format(Fd, Format, Args).

%% Return 'true', but let dialyzer think that a boolean is returned.
cloaked_true() ->
    is_process_alive(self()).


%%%-----------------------------------------------------------------
%%% Make an index page for the last run
make_last_run_index(StartTime) ->
    IndexName = ?index_name,
    AbsIndexName = ?abs(IndexName),
    Result =
	case catch make_last_run_index1(StartTime,IndexName) of
	    {'EXIT', Reason} ->
		io:put_chars("CRASHED while updating " ++ AbsIndexName ++ "!\n"),
		io:format("~tp~n", [Reason]),
		{error, Reason};
	    {error, Reason} ->
		io:put_chars("FAILED while updating " ++ AbsIndexName ++ "\n"),
		io:format("~tp~n", [Reason]),
		{error, Reason};
	    ok ->
		ok;
	    Err ->
		io:format("Unknown internal error while updating ~ts. "
			  "Please report.\n(Err: ~p, ID: 1)",
			  [AbsIndexName,Err]),
		{error, Err}
	end,
    Result.

make_last_run_index1(StartTime,IndexName) ->
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
    Index = [Index0|last_run_index_footer()],

    case force_write_file(IndexName, unicode:characters_to_binary(Index)) of
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

make_last_run_index([Name|Rest], Result, TotSucc, TotFail,
		    UserSkip, AutoSkip, TotNotBuilt, Missing) ->
    case get_run_dirs(Name) of
	false ->
	    %% Silently skip.
	    make_last_run_index(Rest, Result, TotSucc, TotFail,
				UserSkip, AutoSkip, TotNotBuilt, Missing);
	LogDirs ->
	    SuiteName = filename:rootname(filename:basename(Name)),
	    {Result1,TotSucc1,TotFail1,UserSkip1,AutoSkip1,TotNotBuilt1} = 
		make_last_run_index1(SuiteName, LogDirs, Result,
				     TotSucc, TotFail,
				     UserSkip, AutoSkip,
				     TotNotBuilt, Missing),
	    make_last_run_index(Rest, Result1, TotSucc1, TotFail1,
				UserSkip1, AutoSkip1,
				TotNotBuilt1, Missing)
    end;

make_last_run_index([], Result, TotSucc, TotFail, UserSkip, AutoSkip,
		    TotNotBuilt, _) ->
    {ok, [Result|total_row(TotSucc, TotFail, UserSkip, AutoSkip,
			   TotNotBuilt, false)],
     {TotSucc,TotFail,UserSkip,AutoSkip,TotNotBuilt}}.
	    
make_last_run_index1(SuiteName, [LogDir | LogDirs], Result, TotSucc, TotFail,
		     UserSkip, AutoSkip, TotNotBuilt, Missing) ->
    case make_one_index_entry(SuiteName, LogDir, "-", false,
			      Missing, undefined) of
	{Result1,Succ,Fail,USkip,ASkip,NotBuilt,_URIs1} ->
	    %% for backwards compatibility
	    AutoSkip1 = case catch AutoSkip+ASkip of
			    {'EXIT',_} -> undefined;
			    Res -> Res
			end,
	    make_last_run_index1(SuiteName, LogDirs, [Result|Result1],
				 TotSucc+Succ, 
				 TotFail+Fail, UserSkip+USkip, AutoSkip1,
				 TotNotBuilt+NotBuilt, Missing);
	error ->
	    make_last_run_index1(SuiteName, LogDirs, Result, TotSucc, TotFail,
				 UserSkip, AutoSkip, TotNotBuilt, Missing)
    end;
make_last_run_index1(_, [], Result, TotSucc, TotFail,
		     UserSkip, AutoSkip, TotNotBuilt, _) ->
    {Result,TotSucc,TotFail,UserSkip,AutoSkip,TotNotBuilt}.

make_one_index_entry(SuiteName, LogDir, Label, All, Missing, URIs) ->
    case count_cases(LogDir) of
	{Succ,Fail,UserSkip,AutoSkip} ->
	    NotBuilt = not_built(SuiteName, LogDir, All, Missing),
	    {NewResult,URIs1} = make_one_index_entry1(SuiteName, LogDir, Label,
						      Succ, Fail,
						      UserSkip, AutoSkip,
						      NotBuilt, All,
						      normal, URIs),
	    {NewResult,Succ,Fail,UserSkip,AutoSkip,NotBuilt,URIs1};
	error ->
	    error
    end.

make_one_index_entry1(SuiteName, Link, Label, Success, Fail, UserSkip, AutoSkip,
		      NotBuilt, All, Mode, URIs) ->
    LogFile = filename:join(Link, ?suitelog_name ++ ".html"),
    CtRunDir = filename:dirname(filename:dirname(Link)),
    CrashDumpName = SuiteName ++ "_erl_crash.dump",

    URIs1 = {CtRunLogURI,LogFileURI,CrashDumpURI} =
	case URIs of
	    undefined ->
		{uri(filename:join(CtRunDir,?ct_log_name)),
		 uri(LogFile),
		 uri(CrashDumpName)};
	    _ -> 
		URIs
	end,
    
    CrashDumpLink = case Mode of
			temp ->
			    "";
			normal ->
			    case filelib:is_file(CrashDumpName) of
				true ->
				    ["&nbsp;<a href=\"", CrashDumpURI,
				     "\">(CrashDump)</a>"];
				false ->
				    ""
			    end
		    end,

    {Lbl,Timestamp,Node,AllInfo} =
	case All of
	    {true,OldRuns} -> 
		[_Prefix,NodeOrDate|_] = string:lexemes(Link,"."),
		Node1 = case string:find(NodeOrDate,[$@]) of
			    nomatch -> "-";
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
		
		OldRunsLink = 
		    case OldRuns of
			[] -> "none";
			_ ->  "<a href=\""++?all_runs_name++"\">Old Runs</a>"
		    end,

		A = xhtml(["<td><font size=\"-1\"><a href=\"",CtRunLogURI,
			   "\">CT Log</a></font></td>\n",
			   "<td><font size=\"-1\">",OldRunsLink,
			   "</font></td>\n"],
			  ["<td><a href=\"",CtRunLogURI,
			   "\">CT Log</a></td>\n",
			   "<td>",OldRunsLink,"</td>\n"]),
		{L,T,N,A};
	    false ->
		{"","","",""}
	end,

    NotBuiltStr =
	if NotBuilt == 0 ->
		["<td align=right>",integer_to_list(NotBuilt),"</td>\n"];
	   true ->
		["<td align=right><a href=\"",CtRunLogURI,"\">",
		 integer_to_list(NotBuilt),"</a></td>\n"]
	end,
    FailStr =
	if (Fail > 0) or (NotBuilt > 0) or
	   ((Success+Fail+UserSkip+AutoSkip) == 0) ->  
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
    {[xhtml("<tr valign=top>\n",
	    ["<tr class=\"",odd_or_even(),"\">\n"]),
      xhtml("<td><font size=\"-1\"><a href=\"", "<td><a href=\""),
      LogFileURI,"\">",SuiteName,"</a>", CrashDumpLink,
      xhtml("</font></td>\n", "</td>\n"),
      Lbl, Timestamp,
      "<td align=right>",integer_to_list(Success),"</td>\n",
      "<td align=right>",FailStr,"</td>\n",
      "<td align=right>",integer_to_list(AllSkip),
      " (",UserSkipStr,"/",AutoSkipStr,")</td>\n",  
      NotBuiltStr, Node, AllInfo, "</tr>\n"], URIs1}.

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
	   ["</tbody>\n<tfoot>\n<tr class=\"",odd_or_even(),"\">\n"]),
     "<td><b>Total</b></td>\n", Label, TimestampCell,
     "<td align=right><b>",integer_to_list(Success),"</b></td>\n",
     "<td align=right><b>",integer_to_list(Fail),"</b></td>\n",
     "<td align=right>",integer_to_list(AllSkip),
     " (",UserSkipStr,"/",AutoSkipStr,")</td>\n",  
     "<td align=right><b>",integer_to_list(NotBuilt),"</b></td>\n",
     AllInfo, "</tr>\n",
     xhtml("","</tfoot>\n")].

not_built(_BaseName,_LogDir,_All,[]) ->
    0;
not_built(BaseName,_LogDir,_All,Missing) ->
    %% find out how many suites didn't compile
    %% BaseName = 
    %%            Top.ObjDir | Top.ObjDir.suites | Top.ObjDir.Suite | 
    %%            Top.ObjDir.Suite.cases | Top.ObjDir.Suite.Case    
    Failed =
	case string:lexemes(BaseName,".") of
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
    lists:flatten(io_lib:format("~tp.\n", [Term])).


%%% Headers and footers.

index_header(Label, StartTime) ->
    Head =
	case Label of
	    undefined ->
		header("Test Results", format_time(StartTime),
		       {[],[1],[2,3,4,5]});
	    _ ->
		header("Test Results for '" ++ Label ++ "'",
		       format_time(StartTime),
		       {[],[1],[2,3,4,5]})
	end,
    Cover =
	case filelib:is_regular(?abs(?coverlog_name)) of
	    true ->
		xhtml(["<p><a href=\"",?coverlog_name,
		       "\">Cover Log</a></p><br>\n"],
		      ["<br />"
		       "<div id=\"button_holder\" class=\"btn\">\n"
		       "<a href=\"",?coverlog_name,
		       "\">COVER LOG</a>\n</div><br /><br />"]);
	    false ->
		xhtml("<br>\n", "<br /><br /><br />\n")
	end,

    [Head |
     ["<center>\n",
      xhtml(["<p><a href=\"",?ct_log_name,
	     "\">Common Test Framework Log</a></p>"],
	    ["<br />"
	     "<div id=\"button_holder\" class=\"btn\">\n"
	     "<a href=\"",?ct_log_name,
	     "\">COMMON TEST FRAMEWORK LOG</a>\n</div><br>\n"]),
      Cover,
      xhtml(["<table border=\"3\" cellpadding=\"5\" "
	     "bgcolor=\"",?table_color3,"\">\n"],
	    ["<table id=\"",?sortable_table_name,"\">\n",
	     "<thead>\n<tr>\n"]),
      "<th><b>Test Name</b></th>\n",
      xhtml(["<th><font color=\"",?table_color3,"\">_</font>Ok"
	     "<font color=\"",?table_color3,"\">_</font></th>\n"],
	    "<th>Ok</th>\n"),
      "<th>Failed</th>\n",
      "<th>Skipped", xhtml("<br>", "<br />"), "(User/Auto)</th>\n"
      "<th>Missing", xhtml("<br>", "<br />"), "Suites</th>\n",
      xhtml("", "</tr>\n</thead>\n<tbody>\n")]].

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
    [header("Test Results", {[3],[1,2,8,9,10],[4,5,6,7]}) | 
     ["<center>\n",
      AllRunsLink,
      xhtml("<br><br>\n", "<br /><br />\n"),
      xhtml(["<table border=\"3\" cellpadding=\"5\" "
	     "bgcolor=\"",?table_color2,"\">\n"],
	    ["<table id=\"",?sortable_table_name,"\">\n",
	     "<thead>\n<tr>\n"]),
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
      xhtml("", "</tr>\n</thead>\n<tbody>\n")]].

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
    [header(Title, {[1],[2,3,5],[4,6,7,8,9,10]}) |
     ["<center>\n", IxLink,
      xhtml(["<table border=\"3\" cellpadding=\"5\" "
	     "bgcolor=\"",?table_color1,"\">\n"],
	    ["<table id=\"",?sortable_table_name,"\">\n",
	     "<thead>\n<tr>\n"]),
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
      "<th>Missing<br>Suites</th>\n",
      xhtml("", "</tr>\n</thead>\n<tbody>\n")]].

header(Title, TableCols) ->
    header1(Title, "", TableCols).
header(Title, SubTitle, TableCols) ->
    header1(Title, SubTitle, TableCols).

header1(Title, SubTitle, TableCols) ->
    SubTitleHTML = if SubTitle =/= "" ->
			   ["<center>\n",
			    "<h3>" ++ SubTitle ++ "</h3>\n",
			    xhtml("</center>\n<br>\n", "</center>\n<br />\n")];
		      true -> xhtml("<br>", "<br />")
		   end,
    CSSFile = xhtml(fun() -> "" end, 
		    fun() -> make_relative(locate_priv_file(?css_default)) end),
    JQueryFile =
	xhtml(fun() -> "" end, 
	      fun() -> make_relative(locate_priv_file(?jquery_script)) end),
    TableSorterFile =
	xhtml(fun() -> "" end, 
	      fun() -> make_relative(locate_priv_file(?tablesorter_script)) end),
    [xhtml(["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n",
	    "<html>\n"],
	   ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n",
	    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
	    "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n"]),
     "<!-- autogenerated by '"++atom_to_list(?MODULE)++"' -->\n",
     "<head>\n",
     "<title>" ++ Title ++ " " ++ SubTitle ++ "</title>\n",
     "<meta http-equiv=\"cache-control\" content=\"no-cache\"></meta>\n",
     "<meta http-equiv=\"content-type\" content=\"text/html; "
            "charset=utf-8\"></meta>\n",
     xhtml("",
	   ["<link rel=\"stylesheet\" href=\"",uri(CSSFile),
	    "\" type=\"text/css\"></link>\n"]),
     xhtml("",
	   ["<script type=\"text/javascript\" src=\"",JQueryFile,
	    "\"></script>\n"]),
     xhtml("",
	   ["<script type=\"text/javascript\" src=\"",TableSorterFile,
	    "\"></script>\n"]),
     xhtml(fun() -> "" end,
	   fun() -> insert_javascript({tablesorter,?sortable_table_name,
				       TableCols})
	   end),
     "</head>\n",
     body_tag(),
     "<center>\n",
     "<h1>" ++ Title ++ "</h1>\n",
     "</center>\n",
     SubTitleHTML,"\n"].

last_run_index_footer() ->
    AllRuns = filename:join("../",?all_runs_name),
    TestIndex = filename:join("../",?index_name),
    ["</table>\n",
     xhtml("<br><hr><p>\n", "<br /><hr /><p>\n"),
     "<a href=\"", uri(AllRuns),
     "\">Test run history\n</a>  |  ",
     "<a href=\"", uri(TestIndex),
     "\">Top level test index\n</a>\n</p>\n",
     "</center>\n" | footer()].

all_suites_index_footer() ->
    ["</table>\n",
     "</center>\n",
     xhtml("<br><br>\n", "<br /><br />\n") | footer()].

all_runs_index_footer() ->
    [xhtml("", "</tbody>\n"),
     "</table>\n",
     "</center>\n",
     xhtml("<br><br>\n", "<br /><br />\n") | footer()].

footer() ->
     ["<center>\n",
      xhtml("<hr>\n", ""),
      xhtml("<p><font size=\"-1\">\n", "<div class=\"copyright\">"),
      "Copyright &copy; ", year(),
      " <a href=\"http://www.erlang.org\">Open Telecom Platform</a>",
      xhtml("<br>\n", "<br />\n"),
      "Updated: <!--date-->", current_time(), "<!--/date-->",
      xhtml("<br>\n", "<br />\n"),
      xhtml("</font></p>\n", "</div>\n"),
      "</center>\n"
      "</body>\n"
      "</html>\n"].

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
    lists:flatten(io_lib:format("~s ~s ~2.2.0w ~w ~2.2.0w:~2.2.0w:~2.2.0w",
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
		    case count_cases1(b2s(Bin),
				      {undefined,undefined,undefined,undefined}) of
			{error,not_complete} ->
			    %% The test is not complete - dont write summary
			    %% file yet.
			    {0,0,0,0};
			Summary ->
			    _ = write_summary(SumFile, Summary),
			    Summary
		    end;
		{error, Reason} ->
		    io:format("\nFailed to read ~tp: ~tp (skipped)\n",
			      [LogFile,Reason]),
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
	    "<table border=\"3\" cellpadding=\"5\" bgcolor=\"",?table_color1,"\">\n"],
	   ["<h4>CONFIGURATION</h4>\n",
	    "<table id=\"",?sortable_table_name,"\">\n",
	    "<thead>\n"]),
     "<tr><th>Key</th><th>Value</th></tr>\n",
     xhtml("", "</thead>\n<tbody>\n")
    ].

config_table1([{Key,Value}|Vars]) ->
    [xhtml(["<tr><td>", atom_to_list(Key), "</td>\n",
	   "<td><pre>",io_lib:format("~tp",[Value]),"</pre></td></tr>\n"],
	   ["<tr class=\"", odd_or_even(), "\">\n",
	    "<td>", atom_to_list(Key), "</td>\n",
	    "<td>", io_lib:format("~tp",[Value]), "</td>\n</tr>\n"]) |
     config_table1(Vars)];
config_table1([]) ->
    [xhtml("","</tbody>\n"),"</table>\n"].


make_all_runs_index(When) ->
    put(basic_html, basic_html()),
    AbsName = ?abs(?all_runs_name),
    notify_and_lock_file(AbsName),
    if When == start -> ok;
       true -> io:put_chars("Updating " ++ AbsName ++ " ... ")
    end,

    %% check if log cache should be used, and if it exists
    UseCache =
	if When == refresh ->
		save_only;
	   true ->
		case application:get_env(common_test, disable_log_cache) of
		    {ok,true} ->
			disabled;
		    _ ->
			case get(ct_log_cache) of
			    undefined ->
				file:read_file(?log_cache_name);
			    LogCacheBin ->
				{ok,LogCacheBin}
			end
		end
	end,	

    Dirs = filelib:wildcard(logdir_prefix()++"*.*"),
    DirsSorted0 = (catch sort_all_runs(Dirs)),
    DirsSorted =
        if When == start -> DirsSorted0;
           true -> maybe_delete_old_dirs(DirsSorted0)
        end,

    LogCacheInfo = get_cache_data(UseCache),
	
    Result =
	case LogCacheInfo of
	    {ok,LogCache} ->
		%% use the log cache file to generate the index
		make_all_runs_from_cache(AbsName,DirsSorted,LogCache);
	    
	    _WhyNot ->
		%% no cache file exists (or feature has been disabled)
		Header = all_runs_header(),
		GetLogResult =
		    fun(Dir,{RunData,LogTxt}) ->
			    {Tot,XHTML,IxLink} = runentry(Dir,
							  undefined,
							  undefined),
			    {[{Dir,Tot,IxLink}|RunData],[XHTML|LogTxt]}
		    end,
		{AllRunsData,Index} =
		    lists:foldr(GetLogResult,{[],[]},DirsSorted),

		%% update cache with result unless the cache is disabled
		if UseCache == disabled -> ok;
		   true -> update_all_runs_in_cache(AllRunsData)
		end,
		%% write all_runs log file
		ok = file:write_file(AbsName,
				     unicode:characters_to_binary(
				       Header++Index++
					   all_runs_index_footer()))
	end,
    notify_and_unlock_file(AbsName),
    if When == start -> ok;
       true -> io:put_chars("done\n")
    end,
    Result.

make_all_runs_from_cache(AbsName, Dirs, LogCache) ->
    Header = all_runs_header(),

    %% Note that both Dirs and the cache is sorted!
    AllRunsDirs = dir_diff_all_runs(Dirs, LogCache),

    GetLogResult =
	fun({Dir,no_test_data,IxLink},{RunData,LogTxt}) ->
		{Tot,XHTML,_} = runentry(Dir,undefined,IxLink),
		{[{Dir,Tot,IxLink}|RunData],[XHTML|LogTxt]};
	   ({Dir,CachedTotals,IxLink},{RunData,LogTxt}) ->
		%% create log entry using cached data
		{Tot,XHTML,_} = runentry(Dir,CachedTotals,IxLink),
		{[{Dir,Tot,IxLink}|RunData],[XHTML|LogTxt]};
	    (Dir,{RunData,LogTxt}) ->
		%% create log entry from scratch
		{Tot,XHTML,IxLink} = runentry(Dir,undefined,undefined),
		{[{Dir,Tot,IxLink}|RunData],[XHTML|LogTxt]}
	end,
    {AllRunsData,Index} = lists:foldr(GetLogResult,{[],[]},AllRunsDirs),
    %% update cache with result
    update_all_runs_in_cache(AllRunsData,LogCache),
    %% write all_runs log file
    ok = file:write_file(AbsName,
			 unicode:characters_to_binary(
			   Header++Index++
			       all_runs_index_footer())).

update_all_runs_in_cache(AllRunsData) ->
    case get(ct_log_cache) of
	undefined ->
	    LogCache = #log_cache{version = cache_vsn(),
				  all_runs = AllRunsData},
	    case {self(),whereis(?MODULE)} of
		{_Pid,_Pid} ->
		    %% save the cache in RAM so it doesn't have to be
		    %% read from file as long as this logger process is alive
		    put(ct_log_cache,term_to_binary(LogCache));
		_ ->
		    file:write_file(?log_cache_name,term_to_binary(LogCache))
	    end;		    
	SavedLogCache ->
	    update_all_runs_in_cache(AllRunsData,binary_to_term(SavedLogCache))
    end.

update_all_runs_in_cache(AllRunsData, LogCache) ->
    LogCache1 = LogCache#log_cache{all_runs = AllRunsData},    
    case {self(),whereis(?MODULE)} of
	{_Pid,_Pid} ->
	    %% save the cache in RAM so it doesn't have to be
	    %% read from file as long as this logger process is alive
	    put(ct_log_cache,term_to_binary(LogCache1));
	_ ->
	    file:write_file(?log_cache_name,term_to_binary(LogCache1))
    end.

sort_all_runs(Dirs) ->
    %% sort on time string, always last and on the format:
    %% "YYYY-MM-DD_HH.MM.SS"
    lists:sort(fun(Dir1,Dir2) ->
		       [SS1,MM1,HH1,Date1|_] =
			   lists:reverse(string:lexemes(Dir1,[$.,$_])),
		       [SS2,MM2,HH2,Date2|_] =
			   lists:reverse(string:lexemes(Dir2,[$.,$_])),
		       {Date1,HH1,MM1,SS1} > {Date2,HH2,MM2,SS2}
	       end, Dirs).

sort_ct_runs(Dirs) ->
    %% Directory naming: <Prefix>.NodeName.Date_Time[/...]
    %% Sort on Date_Time string: "YYYY-MM-DD_HH.MM.SS"
    lists:sort(
      fun(Dir1,Dir2) ->
	      [SS1,MM1,DateHH1 | _] =
		  lists:reverse(string:lexemes(filename:dirname(Dir1),[$.])),
	      [SS2,MM2,DateHH2 | _] =
		  lists:reverse(string:lexemes(filename:dirname(Dir2),[$.])),
	      {DateHH1,MM1,SS1} =< {DateHH2,MM2,SS2}
      end, Dirs).

parse_keep_logs([Str="all"]) ->
    parse_keep_logs(list_to_atom(Str));
parse_keep_logs([NStr]) ->
    parse_keep_logs(list_to_integer(NStr));
parse_keep_logs(all) ->
    all;
parse_keep_logs(N) when is_integer(N), N>0 ->
    N.

maybe_delete_old_dirs(Sorted) ->
    {Keep,Delete} =
        case application:get_env(common_test, keep_logs) of
            {ok,MaxN} when is_integer(MaxN), length(Sorted)>MaxN ->
                lists:split(MaxN,Sorted);
            _ ->
                {Sorted,[]}
        end,
    delete_old_dirs(Delete),
    Keep.

delete_old_dirs([]) ->
    ok;
delete_old_dirs(Dirs) ->
    io:put_chars("\n  Removing old test directories:\n"),
    [begin
         io:put_chars("    " ++ Dir ++ "\n"),
         rm_dir(Dir)
     end|| Dir <- Dirs],
    ok.

dir_diff_all_runs(Dirs, LogCache) ->
    case LogCache#log_cache.all_runs of
	[] ->
	    Dirs;
	Cached = [{CDir,_,_}|_] ->
	    AllRunsDirs =
		dir_diff_all_runs(Dirs, Cached, datestr_from_dirname(CDir), []),
	    lists:reverse(AllRunsDirs)
    end.

dir_diff_all_runs(LogDirs=[Dir|Dirs], Cached=[CElem|CElems],
		  LatestInCache, AllRunsDirs) ->
    DirDate = datestr_from_dirname(Dir),
    if DirDate > LatestInCache ->
	    %% Dir is a new run entry (not cached)
	    dir_diff_all_runs(Dirs, Cached, LatestInCache,
			      [Dir|AllRunsDirs]);  
       DirDate == LatestInCache, CElems /= [] ->
	    %% Dir is an existing (cached) run entry

	    %% Only add the cached element instead of Dir if the totals
	    %% are "non-empty" (a test might be executing on a different
	    %% node and results haven't been saved yet)
	    ElemToAdd =
		case CElem of
		    {_CDir,{_NodeStr,_Label,_Logs,{0,0,0,0,0}},_IxLink} ->
			%% "empty" element in cache - this could be an
			%% incomplete test and should be checked again
			Dir;
		    _ ->
			CElem
		end,
	    dir_diff_all_runs(Dirs, CElems,
			      datestr_from_dirname(element(1,hd(CElems))),
			      [ElemToAdd|AllRunsDirs]);
       DirDate == LatestInCache, CElems == [] ->
	    %% we're done, Dirs must all be new
	    lists:reverse(Dirs)++[CElem|AllRunsDirs];
       CElems /= [] -> % DirDate < LatestInCache
	    %% current CDir not in Dirs, update timestamp and check next
	    dir_diff_all_runs(LogDirs, CElems,
			      datestr_from_dirname(element(1,hd(CElems))),
			      AllRunsDirs);
       CElems == [] ->
	    %% we're done, LogDirs must all be new
	    lists:reverse(LogDirs)++AllRunsDirs
    end;

dir_diff_all_runs([], _Cached, _, AllRunsDirs) ->
    AllRunsDirs.

interactive_link() ->
    [Dir|_] = lists:reverse(filelib:wildcard(logdir_prefix()++"*.*")),
    CtLog = filename:join(Dir,"ctlog.html"),
    Body =
	[xhtml(
	   ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n",
	    "<html>\n"],
	   ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n",
	    "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
	    "<html xmlns=\"http://www.w3.org/1999/xhtml\" ",
	    "xml:lang=\"en\" lang=\"en\">\n"]),
	 "<!-- autogenerated by '"++atom_to_list(?MODULE)++"' -->\n",
	 "<head>\n",
	 "<title>Last interactive run</title>\n",
	 "<meta http-equiv=\"cache-control\" content=\"no-cache\"></meta>\n",
	 "<meta http-equiv=\"content-type\" content=\"text/html; "
	        "charset=utf-8\"></meta>\n",
	 "</head>\n",
	 "<body>\n",
	 "Log from last interactive run: <a href=\"",uri(CtLog),"\">",
	 timestamp(Dir),"</a>",
	 "</body>\n",
	 "</html>\n"
	],
    _ = file:write_file("last_interactive.html",unicode:characters_to_binary(Body)),
    io:format("~n~nUpdated ~ts\n"
	      "Any CT activities will be logged here\n",
	      [?abs("last_interactive.html")]).

%% use if cache disabled or non-existing
runentry(Dir, undefined, _) ->
    TotalsFile = filename:join(Dir,?totals_name),
    Index = uri(filename:join(Dir,?index_name)),
    runentry(Dir, read_totals_file(TotalsFile), Index);

%% use cached data
runentry(Dir, Totals={Node,Label,Logs,
		      {TotSucc,TotFail,UserSkip,AutoSkip,NotBuilt}}, Index) ->
    TotFailStr =
	if (TotFail > 0) or (NotBuilt > 0) or
	   ((TotSucc+TotFail+UserSkip+AutoSkip) == 0) ->
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
				 integer_to_list(AutoSkip),
				 "</font>"];
			   true -> integer_to_list(AutoSkip)
			end,
		{UserSkip+AutoSkip,integer_to_list(UserSkip),ASStr}
	end,
    NoOfTests = case length(Logs) of
		    0 -> "-";
		    N -> integer_to_list(N)
		end,

    RootNames = lists:map(fun(F) -> filename:rootname(F,?logdir_ext) end, Logs),
    TestNames = lists:flatten(lists:join(", ", RootNames)),
    TestNamesTrunc =
	if length(TestNames) < ?testname_width ->
		TestNames;
	   true ->
		Trunc = string:trim(string:slice(TestNames,0,?testname_width-3),
                                    trailing,",\s"),
		lists:flatten(io_lib:format("~ts...",[Trunc]))
	end,
    TotMissingStr =
	if NotBuilt > 0 ->
		["<font color=\"red\">",
		 integer_to_list(NotBuilt),"</font>"];
	   true ->
		integer_to_list(NotBuilt)
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
    B = xhtml(["<td align=center title='",TestNames,
	       "'><font size=\"-1\"> ",
	       TestNamesTrunc,"</font></td>\n"],
	      ["<td align=center title='",TestNames,"'> ",
	       TestNamesTrunc,"</td>\n"]),
    C = ["<td align=right>",integer_to_list(Total),"</td>\n",
	 "<td align=right>",integer_to_list(TotSucc),"</td>\n",
	 "<td align=right>",TotFailStr,"</td>\n",
	 "<td align=right>",integer_to_list(AllSkip),
	 " (",UserSkipStr,"/",AutoSkipStr,")</td>\n",
	 "<td align=right>",TotMissingStr,"</td>\n"],
    TotalsStr = A++B++C,
    
    XHTML = [xhtml("<tr>\n", ["<tr class=\"",odd_or_even(),"\">\n"]),
	     xhtml(["<td><font size=\"-1\"><a href=\"",Index,"\">",
		    timestamp(Dir),"</a>",
		    TotalsStr,"</font></td>\n"],
		   ["<td><a href=\"",Index,"\">",timestamp(Dir),"</a>",TotalsStr,
		    "</td>\n"]),
	     "</tr>\n"],
    {Totals,XHTML,Index};

%% handle missing or corrupt data (missing e.g. if the test is in progress)
runentry(Dir, _, _) ->
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
    TotalsStr = A++B++C,

    Index = uri(filename:join(Dir,?index_name)),

    XHTML = [xhtml("<tr>\n", ["<tr class=\"",odd_or_even(),"\">\n"]),
	     xhtml(["<td><font size=\"-1\"><a href=\"",Index,"\">",
		    timestamp(Dir),"</a>",
		    TotalsStr,"</font></td>\n"],
		   ["<td><a href=\"",Index,"\">",timestamp(Dir),"</a>",TotalsStr,
		    "</td>\n"]),
	     "</tr>\n"],
    {no_test_data,XHTML,Index}.

write_totals_file(Name,Label,Logs,Totals) ->
    AbsName = ?abs(Name),
    notify_and_lock_file(AbsName),
    _ = force_write_file(AbsName,
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
			    {_Ok,_Fail,_USkip,_ASkip,_NoBuild} -> % latest format
				{Node,Label1,Ls,Tot};
			    {TotSucc,TotFail,AllSkip,NotBuilt} ->
				{Node,Label1,Ls,
				 {TotSucc,TotFail,AllSkip,undefined,NotBuilt}}
			end;
		    {Node,Ls,Tot} ->		% no label found
			case Tot of
			    {_Ok,_Fail,_USkip,_ASkip,_NoBuild} -> % latest format
				{Node,"-",Ls,Tot};
			    {TotSucc,TotFail,AllSkip,NotBuilt} ->
				{Node,"-",Ls,
				 {TotSucc,TotFail,AllSkip,undefined,NotBuilt}}
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
    _ = force_delete(Name),
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
    TsR = lists:reverse(string:lexemes(Dir,".-_")),
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

%% Creates the top level index file. When == start | stop | refresh.
%% A copy of the dir tree under logdir is saved temporarily as a result.
make_all_suites_index(When) when is_atom(When) ->
    put(basic_html, basic_html()),
    AbsIndexName = ?abs(?index_name),
    notify_and_lock_file(AbsIndexName),

    %% check if log cache should be used, and if it exists
    UseCache =
	if When == refresh ->
		save_only;	   
	   true ->
		case application:get_env(common_test, disable_log_cache) of
		    {ok,true} ->
			disabled;
		    _ ->
			case get(ct_log_cache) of
			    undefined ->
				file:read_file(?log_cache_name);
			    LogCacheBin ->
				{ok,LogCacheBin}
			end
		end
	end,	

    Wildcard = logdir_prefix()++".*/*"++?logdir_ext,
    LogDirs = sort_ct_runs(filelib:wildcard(Wildcard)),

    LogCacheInfo = get_cache_data(UseCache),

    Result =
	case LogCacheInfo of
	    {ok,LogCache} ->
		%% use the log cache file to generate the index
		make_all_suites_index_from_cache(When,AbsIndexName,
						 LogDirs,LogCache);
	    _WhyNot ->
		%% no cache file exists (or feature has been disabled)
		Sorted = sort_and_filter_logdirs(LogDirs),
		TempData = make_all_suites_index1(When,AbsIndexName,Sorted),
		notify_and_unlock_file(AbsIndexName),
		
		%% save new cache file unless the feature is disabled
		if UseCache == disabled -> ok;
		   true -> update_tests_in_cache(TempData)
		end,
		TempData
	end,
        
    case Result of
	Error = {error,_} -> Error;
	_                 -> ok
    end;
		
%% This updates the top level index file using data from the initial
%% index file creation, saved temporarily in a table.
make_all_suites_index(NewTestData = {_TestName,DirName}) ->    
    put(basic_html, basic_html()),

    %% AllLogDirs = [{TestName,Label,Missing,
    %%                {LastLogDir,Summary,URIs},OldDirs}|...]

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
	case catch make_all_suites_ix_temp(AbsIndexName,
					   NewTestData,
					   Label,
					   LogDirData) of
	    {'EXIT',Reason} ->
		io:put_chars("CRASHED while updating " ++ AbsIndexName ++ "!\n"),
		io:format("~tp~n", [Reason]),
		{error,Reason};
	    {error,Reason} ->
		io:put_chars("FAILED while updating " ++ AbsIndexName ++ "\n"),
		io:format("~tp~n", [Reason]),
		{error,Reason};
	    ok ->
		ok;
	    Err ->
		io:format("Unknown internal error while updating ~ts. "
			  "Please report.\n(Err: ~tp, ID: 1)",
			  [AbsIndexName,Err]),
		{error, Err}
	end,
    notify_and_unlock_file(AbsIndexName),        
    Result.

make_all_suites_index_from_cache(When, AbsIndexName, LogDirs, LogCache) ->

    %% The structure of the cache:
    %%
    %% #log_cache{tests = {TestName,Label,Missing,
    %%                     {LastLogDir,Summary,URIs},OldDirs}
    %%           }
    %% Summary = {Succ,Fail,USkip,ASkip} | error
    %% 

    {NewAdded,OldTests} = dir_diff_tests(LogDirs,LogCache),
    
    LogCache1 = delete_tests_from_cache(OldTests,LogCache),
    Sorted = sort_and_filter_logdirs(NewAdded,
				     LogCache1#log_cache.tests),
    TempData =
	if Sorted /= [] ->
		make_all_suites_index1(When,AbsIndexName,
				       Sorted);
	   true ->
		Data = LogCache1#log_cache.tests,
		ct_util:set_testdata_async({test_index,{AbsIndexName,
							Data}}),
		Data
	end,
    
    notify_and_unlock_file(AbsIndexName),
    
    update_tests_in_cache(TempData,LogCache1),	    
    TempData.

sort_and_filter_logdirs(NewDirs,CachedTests) when CachedTests /= [] ->
    NewSorted = sort_and_filter_logdirs1(NewDirs,[]),
    sort_and_filter_logdirs(NewSorted,CachedTests,[]);

sort_and_filter_logdirs(NewDirs,_CachedTests) ->
    sort_and_filter_logdirs(NewDirs).

%% sort latest dirs found and combine them with cached entries
sort_and_filter_logdirs([{TestName,IxDirs}|Tests],CachedTests,Combined) ->
    case lists:keysearch(TestName,1,CachedTests) of
	{value,{TestName,_,_,{IxDir0,_,_},IxDirs0}} ->
	    Groups = sort_and_filter_logdirs2(TestName,
					      IxDirs++[IxDir0|IxDirs0],
					      []),
	    sort_and_filter_logdirs(Tests,CachedTests,Groups++Combined);
	_ ->
	    IxDirs1 = lists:map(fun(Elem = {_,_}) ->
					Elem;
				   (RunDir) ->
					{filename:basename(RunDir),RunDir}
				end, IxDirs),
	    sort_and_filter_logdirs(Tests,CachedTests,
				    [{TestName,IxDirs1}|Combined])
    end;
sort_and_filter_logdirs([],CachedTests,Combined) ->
    Cached1 = lists:foldl(fun({TestName,_},Cached) ->
				  lists:keydelete(TestName,1,Cached)
			  end, CachedTests, Combined),
    lists:keysort(1,sort_each_group(Combined)++Cached1).

sort_and_filter_logdirs(Dirs) ->
    sort_and_filter_logdirs1(Dirs, []).

%% sort and filter directories (no cache)
sort_and_filter_logdirs1([Dir|Dirs],Groups) ->
    TestName = filename:rootname(filename:basename(Dir)),
    case filelib:wildcard(filename:join(Dir,"run.*")) of
	RunDirs = [_|_] ->
	    Groups1 = sort_and_filter_logdirs2(TestName,RunDirs,Groups),
	    sort_and_filter_logdirs1(Dirs,Groups1);
	_ ->					% ignore missing run directory
	    sort_and_filter_logdirs1(Dirs,Groups)
    end;
sort_and_filter_logdirs1([],Groups) ->
    lists:keysort(1,sort_each_group(Groups)).

sort_and_filter_logdirs2(TestName,[RunDir|RunDirs],Groups) ->
    Groups1 = insert_test(TestName,{filename:basename(RunDir),RunDir},Groups),
    sort_and_filter_logdirs2(TestName,RunDirs,Groups1);
sort_and_filter_logdirs2(_,[],Groups) ->
    Groups.

%% new rundir for Test found, add to (not sorted) list of prev rundirs
insert_test(Test,IxDir,[{Test,IxDirs}|Groups]) ->
    [{Test,[IxDir|IxDirs]}|Groups];
%% first occurance of Test
insert_test(Test,IxDir,[]) ->
    [{Test,[IxDir]}];
insert_test(Test,IxDir,[TestDir|Groups]) ->
    [TestDir|insert_test(Test,IxDir,Groups)].

%% sort the list of rundirs for each Test
sort_each_group([{Test,IxDirs}|Groups]) ->
    Sorted = lists:reverse([Dir || {_,Dir} <- lists:keysort(1,IxDirs)]),
    [{Test,Sorted}|sort_each_group(Groups)];
sort_each_group([]) ->
    [].

dir_diff_tests(LogDirs, #log_cache{tests = CachedTests}) ->
    AllTestNames =
	[TestName || {TestName,_,_,_,_} <- CachedTests],
    dir_diff_tests(LogDirs, CachedTests, [], AllTestNames, [], []).

dir_diff_tests([LogDir|LogDirs], CachedTests, NewAdded, DeletedTests,
	       ValidLast, InvalidLast) ->
    TestName = filename:rootname(filename:basename(LogDir)),
    Time = datestr_from_dirname(LogDir),
    %% check if the test already exists in the cache
    {New,DeletedTests1,ValidLast1,InvalidLast1} =
	case lists:keysearch(TestName,1,CachedTests) of
	    {value,{_,_,_,{LastLogDir,_,_},_PrevLogDirs}} ->
		LastLogTime = datestr_from_dirname(LastLogDir),
		if Time > LastLogTime ->
			%% this is a new test run, not in cache
			{[LogDir|NewAdded],			 
			 lists:delete(TestName,DeletedTests),
			 ValidLast,[{TestName,LastLogDir}|InvalidLast]};
		   Time == LastLogTime ->
			%% this is the latest test run, already in cache
			TDir = {TestName,LastLogDir},
			{NewAdded,
			 lists:delete(TestName,DeletedTests),
			 [TDir|ValidLast],InvalidLast};
		   true ->
			%% this is an old test run
			{[],
			 lists:delete(TestName,DeletedTests),
			 ValidLast,[{TestName,LastLogDir}|InvalidLast]}
		end;
	    _ ->
		%% this is a test run for a new test, not in cache
		{[LogDir|NewAdded],
		 DeletedTests,ValidLast,InvalidLast}
	end,
    dir_diff_tests(LogDirs, CachedTests, New, DeletedTests1,
		   ValidLast1,InvalidLast1);

dir_diff_tests([], _CachedTests, NewAdded, DeletedTests,
	       ValidLast, InvalidLast) ->
    %% We have to check if LastLogDir still exists or if it's been
    %% deleted. InvalidLast contains all log dirs that should be deleted,
    %% if not present in ValidLast.
    InvalidLast1 =
	lists:foldl(fun(TDir,IL) ->
			    case lists:member(TDir,ValidLast) of
				true ->
				    [TD || TD <- IL, TD /= TDir];
				false ->
				    [TDir | [TD || TD <- IL, TD /= TDir]]
			    end
		    end, InvalidLast, InvalidLast),
    
    %% Collect all tests for which LastLogDir has been deleted.
    DeletedTests1 = [T || {T,_} <- InvalidLast1] ++ DeletedTests,

    %% Make sure that directories for tests that are to be deleted are
    %% saved in NewAdded so that tests don't disappear from the log if
    %% older run dirs for them exist.
    NewAdded1 = lists:map(fun({_TestName,RunDir}) ->
				  [TopDir,TestDir|_] = filename:split(RunDir),
				  filename:join(TopDir,TestDir)
			  end, InvalidLast1) ++ NewAdded,

    {NewAdded1,DeletedTests1}.

delete_tests_from_cache(OldTests, LogCache=#log_cache{tests=Tests}) ->
    Tests2 = lists:foldl(fun(T,Tests1) ->
				 lists:keydelete(T,1,Tests1)
			 end, Tests, OldTests),
    LogCache#log_cache{tests = Tests2}.

update_tests_in_cache(TempData) ->
    case get(ct_log_cache) of
	undefined ->
	    update_tests_in_cache(TempData,#log_cache{version = cache_vsn(),
						      tests=[]});
	SavedLogCache ->
	    update_tests_in_cache(TempData,binary_to_term(SavedLogCache))
    end.

update_tests_in_cache(TempData,LogCache=#log_cache{tests=Tests}) ->
    Cached1 =
	if Tests == [] ->
		[];
	   true ->
		lists:foldl(fun({TestName,_,_,_,_},Cached) ->
				    lists:keydelete(TestName,1,Cached)
			    end, Tests, TempData)
	end,
    Tests1 = lists:keysort(1,TempData++Cached1),
    CacheBin = term_to_binary(LogCache#log_cache{tests = Tests1}),
    case {self(),whereis(?MODULE)} of
	{_Pid,_Pid} ->
	    put(ct_log_cache,CacheBin);
	_ ->
	    file:write_file(?log_cache_name,CacheBin)
    end.

%%
%% AllTestLogDirs =
%%   [{TestName,[IxDir|IxDirs]} | ...] (non-cached), or
%%   [{TestName,Label,Missing,{IxDir,Summary,URIs},IxDirs} | ...] (cached)
%%
make_all_suites_index1(When, AbsIndexName, AllTestLogDirs) ->
    IndexName = ?index_name,
    if When == start -> ok;
       true -> io:put_chars("Updating " ++ AbsIndexName ++ " ... ")
    end,
    case catch make_all_suites_index2(IndexName, AllTestLogDirs) of
	{'EXIT', Reason} ->
	    io:put_chars("CRASHED while updating " ++ AbsIndexName ++ "!\n"),
	    io:format("~tp~n", [Reason]),
	    {error, Reason};
	{error, Reason} ->
	    io:put_chars("FAILED while updating " ++ AbsIndexName ++ "\n"),
	    io:format("~tp~n", [Reason]),
	    {error, Reason};
	{ok,TempData} ->
	    case When of
		start ->
		    ct_util:set_testdata_async({test_index,{AbsIndexName,
							    TempData}}),
		    TempData;
		_ ->
		    io:put_chars("done\n"),
		    TempData
	    end;
	Err ->
	    io:format("Unknown internal error while updating ~ts. "
		      "Please report.\n(Err: ~tp, ID: 1)",
		      [AbsIndexName,Err]),
	    {error, Err}
    end.

make_all_suites_index2(IndexName, AllTestLogDirs) ->
    {ok,Index0,_Totals,TempData} =
	make_all_suites_index3(AllTestLogDirs,
			       all_suites_index_header(),
			       0, 0, 0, 0, 0, [], []),
    Index = [Index0|all_suites_index_footer()],
    case force_write_file(IndexName, unicode:characters_to_binary(Index)) of
	ok ->
	    {ok,TempData};
	{error, Reason} ->
	    {error,{index_write_error, Reason}}
    end.

%%
%% AllTestLogDirs = [{TestName,Label,Missing,{LogDir,Summary,URIs},OldDirs}]
%% Summary = {Succ,Fail,UserSkip,AutoSkip} | error
%% URIs = {CtRunLogURI,LogFileURI,CrashDumpURI} | undefined
%%
%% this clause is for handling entries in the log cache
make_all_suites_index3([IxEntry = {TestName,Label,Missing,
				   {LastLogDir,Summary,URIs},OldDirs} | Rest],
		       Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt,
		       Labels, TempData) ->
    [EntryDir|_] = filename:split(LastLogDir),
    Labels1 = [{EntryDir,Label}|Labels],
    case Summary of
	{Succ,Fail,USkip,ASkip} ->
	    All = {true,OldDirs},
	    NotBuilt = not_built(TestName, LastLogDir, All, Missing),

	    {Result1,_} = make_one_index_entry1(TestName, LastLogDir, Label,
						Succ, Fail, USkip, ASkip,
						NotBuilt, All, temp, URIs),

	    AutoSkip1 = case catch AutoSkip+ASkip of
			    {'EXIT',_} -> undefined;
			    Res -> Res
			end,
	    make_all_suites_index3(Rest, [Result|Result1], TotSucc+Succ, 
				   TotFail+Fail, UserSkip+USkip, AutoSkip1,
				   TotNotBuilt+NotBuilt, Labels1,
				   [IxEntry|TempData]);
	error ->
	    make_all_suites_index3(Rest, Result, TotSucc, TotFail, 
				   UserSkip, AutoSkip, TotNotBuilt, Labels1,
				   [IxEntry|TempData])
    end;

%% this clause is for handling non-cached directories
make_all_suites_index3([{TestName,[LastLogDir|OldDirs]}|Rest],
		       Result, TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt,
		       Labels, TempData) ->
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
    case make_one_index_entry(TestName, LastLogDir, Label,
			      {true,OldDirs}, Missing, undefined) of
	{Result1,Succ,Fail,USkip,ASkip,NotBuilt,URIs} ->
	    %% for backwards compatibility
	    AutoSkip1 = case catch AutoSkip+ASkip of
			    {'EXIT',_} -> undefined;
			    Res -> Res
			end,
	    IxEntry = {TestName,Label,Missing,
		       {LastLogDir,{Succ,Fail,USkip,ASkip},URIs},OldDirs},

	    make_all_suites_index3(Rest, [Result|Result1], TotSucc+Succ, 
				   TotFail+Fail, UserSkip+USkip, AutoSkip1,
				   TotNotBuilt+NotBuilt, Labels1,
				   [IxEntry|TempData]);
	error ->
	    IxEntry = {TestName,Label,Missing,
		       {LastLogDir,error,undefined},OldDirs},
	    make_all_suites_index3(Rest, Result, TotSucc, TotFail, 
				   UserSkip, AutoSkip, TotNotBuilt, Labels1,
				   [IxEntry|TempData])
    end;

%% something wrong with this test dir, ignore
make_all_suites_index3([_|Rest], Result, TotSucc, TotFail, UserSkip, AutoSkip,
		       TotNotBuilt, Labels, TempData) ->
    make_all_suites_index3(Rest, Result, TotSucc, TotFail, 
			   UserSkip, AutoSkip, TotNotBuilt, Labels,
			   TempData);

make_all_suites_index3([], Result, TotSucc, TotFail, UserSkip, AutoSkip, 
		       TotNotBuilt, _, TempData) ->
    {ok, [Result|total_row(TotSucc, TotFail, UserSkip, AutoSkip,
			   TotNotBuilt,true)], 
     {TotSucc,TotFail,UserSkip,AutoSkip,TotNotBuilt}, lists:reverse(TempData)}.


make_all_suites_ix_temp(AbsIndexName, NewTestData, Label, AllTestLogDirs) ->
    AllTestLogDirs1 = insert_new_test_data(NewTestData, Label, AllTestLogDirs),
    IndexDir = filename:dirname(AbsIndexName),
    Index0 = make_all_suites_ix_temp1(AllTestLogDirs1,
				      all_suites_index_header(IndexDir),
				      0, 0, 0, 0, 0),
    Index = [Index0|all_suites_index_footer()],
    case force_write_file(AbsIndexName, unicode:characters_to_binary(Index)) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error,{index_write_error, Reason}}
    end.

insert_new_test_data({NewTestName,NewTestDir}, NewLabel, AllTestLogDirs) ->
    AllTestLogDirs1 =
	case lists:keysearch(NewTestName, 1, AllTestLogDirs) of
	    {value,{_,_,_,{LastLogDir,_,_},OldDirs}} ->
		[{NewTestName,NewLabel,[],{NewTestDir,{0,0,0,0},undefined},
		  [LastLogDir|OldDirs]} |
		 lists:keydelete(NewTestName, 1, AllTestLogDirs)];
	    false ->
		[{NewTestName,NewLabel,[],{NewTestDir,{0,0,0,0},undefined},[]} |
		 AllTestLogDirs]
	end,
    lists:keysort(1, AllTestLogDirs1).

make_all_suites_ix_temp1([{TestName,Label,Missing,LastLogDirData,OldDirs}|Rest],
			 Result, TotSucc, TotFail, UserSkip, AutoSkip,
			 TotNotBuilt) ->
    case make_one_ix_entry_temp(TestName, LastLogDirData,
				Label, {true,OldDirs}, Missing) of
	{Result1,Succ,Fail,USkip,ASkip,NotBuilt,_URIs} ->
	    %% for backwards compatibility
	    AutoSkip1 = case catch AutoSkip+ASkip of
			    {'EXIT',_} -> undefined;
			    Res -> Res
			end,
	    make_all_suites_ix_temp1(Rest, [Result|Result1], TotSucc+Succ,
				     TotFail+Fail, UserSkip+USkip, AutoSkip1,
				     TotNotBuilt+NotBuilt);
	error ->
	    make_all_suites_ix_temp1(Rest, Result, TotSucc, TotFail,
				     UserSkip, AutoSkip, TotNotBuilt)
    end;
make_all_suites_ix_temp1([], Result, TotSucc, TotFail, UserSkip, AutoSkip,
			 TotNotBuilt) ->
    [Result|total_row(TotSucc, TotFail, UserSkip, AutoSkip, TotNotBuilt, true)].

make_one_ix_entry_temp(TestName, {LogDir,Summary,URIs}, Label, All, Missing) ->
    case Summary of
	{Succ,Fail,UserSkip,AutoSkip} ->
	    NotBuilt = not_built(TestName, LogDir, All, Missing),
	    {NewResult,URIs1} = make_one_index_entry1(TestName, LogDir, Label,
						      Succ, Fail,
						      UserSkip, AutoSkip,
						      NotBuilt, All, temp, URIs),
	    {NewResult,Succ,Fail,UserSkip,AutoSkip,NotBuilt,URIs1};
	error ->
	    error
    end.

%%%-----------------------------------------------------------------
%%% 
get_cache_data({ok,CacheBin}) ->
    case binary_to_term(CacheBin) of
	CacheRec when is_record(CacheRec,log_cache) ->
	    %% make sure we don't use a cache on old format
	    case is_correct_cache_vsn(CacheRec) of
		true ->
		    {ok,CacheRec};
		false ->
		    _ = file:delete(?log_cache_name),
		    {error,old_cache_file}
	    end;				    
	_ ->
	    _ = file:delete(?log_cache_name),
	    {error,invalid_cache_file}
    end;
get_cache_data(NoCache) ->
    NoCache.

cache_vsn() ->
    _ = application:load(common_test),
    case application:get_key(common_test,vsn) of
	{ok,VSN} ->
	    VSN;
	_ ->
	    EbinDir = filename:dirname(code:which(ct)),
	    VSNfile = filename:join([EbinDir,"..","vsn.mk"]),
	    case file:read_file(VSNfile) of
		{ok,Bin} ->
		    [_,VSN] = string:lexemes(binary_to_list(Bin),[$=,$\n,$ ]),
		    VSN;
		_ ->
		    undefined
	    end
    end.

is_correct_cache_vsn(#log_cache{version = CVSN}) ->
    case cache_vsn() of
	CVSN -> true;
	_    -> false
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
                        ct_util:mark_process(),
			S ! {self(),started},
			simulate_logger_loop() 
		end),
    receive {Pid,started} -> Pid end.


simulate_logger_loop() ->
    receive 
    	{log,_,_,_,_,_,Content,_} ->
	    S = lists:map(fun({_,Str,Args}) ->
				  [io_lib:format(Str,Args),io_lib:nl()];
			     ({Str,Args}) ->
				  [io_lib:format(Str,Args),io_lib:nl()]
			  end, Content),
	    io:format("~ts",[S]),
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
%%% @spec get_run_dirs(Dir) -> [string()] | false
%%%
%%% @doc
%%%
get_run_dirs(Dir) ->
    case filelib:wildcard(filename:join(Dir, "run.[1-2]*")) of
	[] ->
	    false;
	RunDirs ->
	    lists:sort(RunDirs)
    end.

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
%%% @spec locate_priv_file(FileName) -> PrivFile
%%%
%%% @doc
%%%
locate_priv_file(FileName) ->
    {ok,CWD} = file:get_cwd(),
    PrivFileInCwd = filename:join(CWD, FileName),
    case filelib:is_file(PrivFileInCwd) of
	true ->
	    PrivFileInCwd;
	false ->
	    PrivResultFile =
		case {whereis(?MODULE),self()} of
		    {Self,Self} ->
			%% executed on the ct_logs process
			filename:join(get(ct_run_dir), FileName);
		    _ ->			
			%% executed on other process than ct_logs
			{ok,LogDir} = get_log_dir(true),
			filename:join(LogDir, FileName)
		end,
	    case filelib:is_file(PrivResultFile) of
		true ->
		    PrivResultFile;
		false ->
		    %% last resort, try use css file in CT installation
		    CTPath = code:lib_dir(common_test),		
		    filename:join(filename:join(CTPath, "priv"), FileName)
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
%%% @spec get_ts_html_wrapper(TestName, PrintLabel, Cwd, TableCols, Encoding)
%%%           -> {Mode,Header,Footer}
%%%
%%% @doc
%%%
get_ts_html_wrapper(TestName, PrintLabel, Cwd, TableCols, Encoding) ->
    get_ts_html_wrapper(TestName, undefined, PrintLabel, Cwd, TableCols, Encoding).

get_ts_html_wrapper(TestName, Logdir, PrintLabel, Cwd, TableCols, Encoding) ->
    TestName1 = if is_list(TestName) ->
			lists:flatten(TestName);
		   true ->
			lists:flatten(io_lib:format("~tp", [TestName]))
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

    {ok,CtLogdir} =
	if Logdir == undefined -> get_log_dir(true);
	   true -> {ok,Logdir}
	end,

    AllRuns = make_relative(filename:join(filename:dirname(CtLogdir),
					  ?all_runs_name), Cwd),
    TestIndex = make_relative(filename:join(filename:dirname(CtLogdir),
					    ?index_name), Cwd),
    LatestTest = make_relative(filename:join(filename:dirname(CtLogdir),
                                           ?suitelog_name++".latest.html"),
                             Cwd),

    case Basic of
	true ->
	    TileFile = filename:join(filename:join(CTPath,"priv"),"tile1.jpg"),
	    Bgr = " background=\"" ++ TileFile ++ "\"",
	    Copyright =
		     ["<p><font size=\"-1\">\n",
		      "Copyright &copy; ", year(),
		      " <a href=\"http://www.erlang.org\">",
		      "Open Telecom Platform</a><br>\n",
		      "Updated: <!--date-->", current_time(), "<!--/date-->",
		      "<br>\n</font></p>\n"],
	    {basic_html,
	     ["<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2 Final//EN\">\n",
	      "<html>\n",
	      "<head><title>", TestName1, "</title>\n",
	      "<meta http-equiv=\"cache-control\" content=\"no-cache\"></meta>\n",
	      "<meta http-equiv=\"content-type\" content=\"text/html; charset=",
	      html_encoding(Encoding),"\"></meta>\n",
	      "</head>\n",
	      "<body", Bgr, " bgcolor=\"white\" text=\"black\" ",
	      "link=\"blue\" vlink=\"purple\" alink=\"red\">\n",
	      LabelStr, "\n"],
	     ["<center>\n<br><hr><p>\n",
	      "<a href=\"", uri(AllRuns),
	      "\">Test run history\n</a>  |  ",
	      "<a href=\"", uri(TestIndex),
	      "\">Top level test index\n</a>  |  ",
	      "<a href=\"", uri(LatestTest),
              "\">Latest test result</a>\n</p>\n",
	      Copyright,"</center>\n</body>\n</html>\n"]};
	_ ->
	    Copyright = 
		["<div class=\"copyright\">",
		 "Copyright &copy; ", year(),
		 " <a href=\"http://www.erlang.org\">",
		 "Open Telecom Platform</a><br />\n",
		 "Updated: <!--date-->", current_time(), "<!--/date-->",
		 "<br />\n</div>\n"],
	    CSSFile =
		xhtml(fun() -> "" end, 
		      fun() -> make_relative(locate_priv_file(?css_default),
					     Cwd)
		      end),
	    JQueryFile =
		xhtml(fun() -> "" end, 
		      fun() -> make_relative(locate_priv_file(?jquery_script),
					     Cwd)
		      end),
	    TableSorterFile =
		xhtml(fun() -> "" end, 
		      fun() -> make_relative(locate_priv_file(?tablesorter_script),
					    Cwd)
		      end),
	    TableSorterScript =
		xhtml(fun() -> "" end, 
		      fun() -> insert_javascript({tablesorter,
						  ?sortable_table_name,
						  TableCols}) end),
	    {xhtml,
	     ["<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n",
	      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n",
	      "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n",
	      "<head>\n<title>", TestName1, "</title>\n",
	      "<meta http-equiv=\"cache-control\" content=\"no-cache\"></meta>\n",
	      "<meta http-equiv=\"content-type\" content=\"text/html; ",
	      "charset=utf-8\"></meta>\n",
	      "<link rel=\"stylesheet\" href=\"", uri(CSSFile),
	      "\" type=\"text/css\"></link>\n",
	      "<script type=\"text/javascript\" src=\"", JQueryFile, "\"></script>\n",
	      "<script type=\"text/javascript\" src=\"", TableSorterFile, "\"></script>\n"] ++
	      TableSorterScript ++ ["</head>\n","<body>\n", LabelStr, "\n"],
	     ["<center>\n<br /><hr /><p>\n",
	      "<a href=\"", uri(AllRuns),
	      "\">Test run history\n</a>  |  ",
	      "<a href=\"", uri(TestIndex),
	      "\">Top level test index\n</a>  |  ",
	      "<a href=\"", uri(LatestTest),
              "\">Latest test result</a>\n</p>\n",
	      Copyright,"</center>\n</body>\n</html>\n"]}
    end.

insert_javascript({tablesorter,_TableName,undefined}) ->
    [];

insert_javascript({tablesorter,TableName,
		   {DateCols,TextCols,ValCols}}) ->
    Headers =
	lists:flatten(
	  lists:sort(
	    lists:flatmap(fun({Sorter,Cols}) ->
				  [lists:flatten(
				     io_lib:format("      ~w: "
						   "{ sorter: '~s' },\n",
						   [Col-1,Sorter])) || Col<-Cols]
			  end, [{"CTDateSorter",DateCols},
				{"CTTextSorter",TextCols},
				{"CTValSorter",ValCols}]))),
    Headers1 = string:trim(Headers, trailing, ",\n"),

    ["<script type=\"text/javascript\">\n",
     "// Parser for date format, e.g: Wed Jul 4 2012 11:24:15\n",
     "var monthNames = {};\n",
     "monthNames[\"Jan\"] = \"01\"; monthNames[\"Feb\"] = \"02\";\n",
     "monthNames[\"Mar\"] = \"03\"; monthNames[\"Apr\"] = \"04\";\n",
     "monthNames[\"May\"] = \"05\"; monthNames[\"Jun\"] = \"06\";\n",
     "monthNames[\"Jul\"] = \"07\"; monthNames[\"Aug\"] = \"08\";\n",
     "monthNames[\"Sep\"] = \"09\"; monthNames[\"Oct\"] = \"10\";\n",
     "monthNames[\"Nov\"] = \"11\"; monthNames[\"Dec\"] = \"12\";\n",
     "$.tablesorter.addParser({\n",
     "  id: 'CTDateSorter',\n",
     "  is: function(s) {\n",
     "      return false; },\n",
     "  format: function(s) {\n",
     %% place empty cells, "-" and "?" at the bottom
     "      if (s.length < 2) return 999999999;\n",
     "      else {\n",
     %% match out each date element
     "          var date = s.match(/(\\w{3})\\s(\\w{3})\\s(\\d{2})\\s(\\d{4})\\s(\\d{2}):(\\d{2}):(\\d{2})/);\n",
     "          var y = date[4]; var mo = monthNames[date[2]]; var d = String(date[3]);\n",
     "          var h = String(date[5]); var mi = String(date[6]); var sec = String(date[7]);\n",
     "          return (parseInt('' + y + mo + d + h + mi + sec)); }},\n",
     "  type: 'numeric' });\n",

     "// Parser for general text format\n",
     "$.tablesorter.addParser({\n",
     "  id: 'CTTextSorter',\n",
     "  is: function(s) {\n",
     "    return false; },\n",
     "  format: function(s) {\n",
     %% place empty cells, "?" and "-" at the bottom
     "    if (s.length < 1) return 'zzzzzzzz';\n",
     "    else if (s == \"?\") return 'zzzzzzz';\n",
     "    else if (s == \"-\") return 'zzzzzz';\n",
     "    else if (s == \"FAILED\") return 'A';\n",
     "    else if (s == \"SKIPPED\") return 'B';\n",
     "    else if (s == \"OK\") return 'C';\n",
     "    else return '' + s; },\n",
     "  type: 'text' });\n",

     "// Parser for numerical values\n",
     "$.tablesorter.addParser({\n",
     "  id: 'CTValSorter',\n",
     "  is: function(s) {\n",
     "    return false; },\n",
     "  format: function(s) {\n"
     %% place empty cells and "?" at the bottom
     "    if (s.length < 1) return '-2';\n",
     "    else if (s == \"?\") return '-1';\n",
     %% look for skip value, eg "3 (2/1)"
     "    else if ((s.search(/(\\d{1,})\\s/)) >= 0) {\n",
     "      var num = s.match(/(\\d{1,})\\s/);\n",
     %% return only the total skip value for sorting
     "      return (parseInt('' + num[1])); }\n",
     "    else if ((s.search(/(\\d{1,})\\.(\\d{3})s/)) >= 0) {\n",
     "      var num = s.match(/(\\d{1,})\\.(\\d{3})/);\n",
     "      if (num[1] == \"0\") return (parseInt('' + num[2]));\n",
     "      else return (parseInt('' + num[1] + num[2])); }\n",
     "    else return '' + s; },\n",
     "  type: 'numeric' });\n",

     "$(document).ready(function() {\n",
     "  $(\"#",TableName,"\").tablesorter({\n",
     "    headers: { \n", Headers1, "\n    }\n  });\n",
     "  $(\"#",TableName,"\").trigger(\"update\");\n",
     "  $(\"#",TableName,"\").trigger(\"appendCache\");\n",
     "});\n</script>\n"].

uri("") ->
    "";
uri(Href) ->
    test_server_ctrl:uri_encode(Href).

%% Read magic comment to get encoding of text file.
%% If no magic comment exists, assume default encoding
encoding(File) ->
    case epp:read_encoding(File) of
	none ->
	    epp:default_encoding();
	E ->
	    E
    end.

%% Convert binary to string using default encoding
b2s(Bin) ->
    b2s(Bin,epp:default_encoding()).

%% Convert binary to string using given encoding
b2s(Bin,Encoding) ->
    unicode:characters_to_list(Bin,Encoding).

html_encoding(latin1) ->
    "iso-8859-1";
html_encoding(utf8) ->
    "utf-8".

unexpected_io(Pid, ct_internal, _Importance, Content, CtLogFd, EscChars) ->
    IoFun = create_io_fun(Pid, CtLogFd, EscChars),
    io:format(CtLogFd, "~ts", [lists:foldl(IoFun, [], Content)]);
unexpected_io(Pid, _Category, _Importance, Content, CtLogFd, EscChars) ->
    IoFun = create_io_fun(Pid, CtLogFd, EscChars),
    Data = io_lib:format("~ts", [lists:foldl(IoFun, [], Content)]),
    test_server_io:print_unexpected(Data),
    ok.
