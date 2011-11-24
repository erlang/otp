%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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

-module(rb_SUITE).
-include("test_server.hrl").

-compile(export_all).

-define(SUP,rb_SUITE_sup).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    no_group_cases() ++ [{group,running_error_logger}].

no_group_cases() ->
    [help,
     start_error_stop].

groups() ->
    [{running_error_logger,[shuffle],[show,
				      list,
				      rescan,
				      start_stop_log,
				      grep,
				      filter_filter,
				      filter_date,
				      filter_filter_and_date,
				      filter_re_no
				     ]}].


all(suite) -> 
    no_group_cases() ++
	[{conf, 
	  install_mf_h, 
	  element(3,lists:keyfind(running_error_logger,1,groups())), 
	  remove_mf_h}
	].


init_per_suite(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line RbDir = filename:join(PrivDir,rb),
    ?line ok = file:make_dir(RbDir),
    NewConfig = [{rb_dir,RbDir}|Config],
    reset_sasl(NewConfig),
    NewConfig.

end_per_suite(_Config) ->
    ok.

init_per_group(running_error_logger,Config) ->
    install_mf_h(Config).

end_per_group(running_error_logger,Config) ->
    remove_mf_h(Config).

init_per_testcase(_Case,Config) ->
    case whereis(?SUP) of
	undefined -> ok;
	Pid -> kill(Pid)
    end,
    empty_error_logs(Config),
    Config.

kill(Pid) ->
    Ref = erlang:monitor(process,Pid),
    exit(Pid,kill),
    receive {'DOWN', Ref, process, Pid, _Info} -> ok end.

end_per_testcase(Case,Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.


%%%-----------------------------------------------------------------

help() -> help(suite).
help(suite) -> [];
help(_Config) ->
    ?line Help = capture(fun() -> rb:h() end),
    ?line "Report Browser Tool - usage" = hd(Help),
    ?line "rb:stop            - stop the rb_server" = lists:last(Help),
    ok.


start_error_stop() -> start_error_stop(suite).
start_error_stop(suite) -> [];
start_error_stop(Config) ->
    ?line RbDir = ?config(rb_dir,Config),

    ?line {error,{"cannot locate report directory",_}} = rb:start(),


    ?line ok = application:set_env(sasl,error_logger_mf_dir,"invaliddir"),
    ?line ok = application:set_env(sasl,error_logger_mf_maxbytes,1000),
    ?line ok = application:set_env(sasl,error_logger_mf_maxfiles,2),
    ?line restart_sasl(),
    ?line {error,{"cannot read the index file",_}} = rb:start(),
    ?line ok = application:set_env(sasl,error_logger_mf_dir,RbDir),
    ?line restart_sasl(),
    ?line {ok,_} = rb:start(),

    ?line ok = rb:stop(),
    ok.


%% start_opts(suite) -> [];
%% start_opts(Config) ->
%%     PrivDir = ?config(priv_dir,Config),
%%     RbDir = filename:join(PrivDir,rb_opts),
%%     ok = file:make_dir(RbDir),
       

install_mf_h(Config) ->
    ?line RbDir = ?config(rb_dir,Config),
    ?line ok = application:set_env(sasl,error_logger_mf_dir,RbDir),
    ?line ok = application:set_env(sasl,error_logger_mf_maxbytes,5000),
    ?line ok = application:set_env(sasl,error_logger_mf_maxfiles,2),
    ?line restart_sasl(),
    Config.

remove_mf_h(_Config) ->
    ok.



show() -> show(suite).
show(suite) -> [];
show(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),
    
    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ?line ok = start_rb(OutFile),

    %% Show all reports
    ?line All = check_report(fun() -> rb:show() end,OutFile),

    %% Show by number
    ?line [{_,First}] = check_report(fun() -> rb:show(1) end,OutFile),
    ?line {1,First} = lists:keyfind(1,1,All),    

    %% Show by type
    ?line [{_,CR}] = check_report(fun() -> rb:show(crash_report) end,OutFile),
    ?line true = contains(CR,"rb_test_crash"),
    ?line [{_,EC},{_,EM}] = check_report(fun() -> rb:show(error) end,OutFile),
    ?line true = contains(EC,"rb_test_crash"),
    ?line true = contains(EM,"rb_test_error_msg"),
    ?line [{_,ER}] = check_report(fun() -> rb:show(error_report) end,OutFile),
    ?line true = contains(ER,"rb_test_error"),
    ?line [{_,IR}] = check_report(fun() -> rb:show(info_report) end,OutFile),
    ?line true = contains(IR,"rb_test_info"),
    ?line [{_,IM}] = check_report(fun() -> rb:show(info_msg) end,OutFile),
    ?line true = contains(IM,"rb_test_info_msg"),
    ?line [_|_] = check_report(fun() -> rb:show(progress) end,OutFile),
    ?line [{_,SR}] = check_report(fun() -> rb:show(supervisor_report) end,
				   OutFile),
    ?line true = contains(SR,"child_terminated"),
    ?line true = contains(SR,"{rb_SUITE,rb_test_crash}"),

    ok.

list() -> list(suite).
list(suite) -> [];
list(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ?line ok = start_rb(OutFile),

    ?line All = capture(fun() -> rb:list() end),
    ?line [{crash_report,[_]=CR},
	   {error,[_,_]=EM},
	   {error_report,[_]=ER},
	   {info_msg,[_]=IM},
	   {info_report,[_]=IR},
	   {progress,[_|_]=P},
	   {supervisor_report,[_]=SR}] = sort_list(All),

    ?line [{crash_report,CR}] = 
	sort_list(capture(fun() -> rb:list(crash_report) end)),
    ?line [{error,EM}] = 
	sort_list(capture(fun() -> rb:list(error) end)),
    ?line [{error_report,ER}] = 
	sort_list(capture(fun() -> rb:list(error_report) end)),
    ?line [{info_msg,IM}] = 
	sort_list(capture(fun() -> rb:list(info_msg) end)),
    ?line [{info_report,IR}] = 
	sort_list(capture(fun() -> rb:list(info_report) end)),
    ?line [{progress,P}] = 
	sort_list(capture(fun() -> rb:list(progress) end)),
    ?line [{supervisor_report,SR}] = 
	sort_list(capture(fun() -> rb:list(supervisor_report) end)),
    
    ok.


grep() -> grep(suite).
grep(suite) -> [];
grep(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ?line ok = start_rb(OutFile),

    ?line [{_,S},
	   {_,CR},
	   {_,EC},
	   {_,IM},
	   {_,IR},
	   {_,EM},
	   {_,ER}]= check_report(fun() -> rb:grep("rb_test_") end,OutFile),
    ?line true = contains(S, "rb_test_crash"),
    ?line true = contains(CR, "rb_test_crash"),
    ?line true = contains(EC, "rb_test_crash"),
    ?line true = contains(IM, "rb_test_info_msg"),
    ?line true = contains(IR, "rb_test_info"),
    ?line true = contains(EM, "rb_test_error_msg"),
    ?line true = contains(ER, "rb_test_error"),
    ok.


filter_filter() -> filter_filter(suite).
filter_filter(suite) -> [];
filter_filter(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ?line ok = start_rb(OutFile),

    ?line All = check_report(fun() -> rb:show() end,OutFile),

    ?line ER = [_] = rb_filter([{rb_SUITE,rb_test_error}],OutFile),
    ?line [] = rb_filter([{rb_SUITE,rb_test}],OutFile),
    ?line _E = [_,_] = rb_filter([{rb_SUITE,"rb_test",re}],OutFile),
    ?line AllButER = rb_filter([{rb_SUITE,rb_test_error,no}],OutFile),

    {_,AllRep} = lists:unzip(All),
    {_,ERRep} = lists:unzip(ER),
    {_,AllButERRep} = lists:unzip(AllButER),
    ?line AllButERRep = AllRep -- ERRep,

    ok.

filter_date() -> filter_date(suite).
filter_date(suite) -> [];
filter_date(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),


    %% Insert some reports in the error log and start rb
    init_error_logs(),
    Between1 = calendar:local_time(),
    timer:sleep(1000),
    Between2 = calendar:local_time(),
    ?line ok = start_rb(OutFile),

    ?line All = check_report(fun() -> rb:show() end,OutFile),

    Before = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 10),
    After = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 1),

    ?line All = rb_filter([],{Before,from},OutFile),
    ?line All = rb_filter([],{After,to},OutFile),
    ?line [] = rb_filter([],{Before,to},OutFile),
    ?line [] = rb_filter([],{After,from},OutFile),
    ?line All = rb_filter([],{Before,After},OutFile),

    %%?t:format("~p~n",[All]),
    ?line AllButLast = [{N-1,R} || {N,R} <- tl(All)],
    ?line AllButLast = rb_filter([],{Before,Between1},OutFile),

    ?line Last = hd(All),
    ?line [Last] = rb_filter([],{Between2,After},OutFile),

    ok.

filter_filter_and_date() -> filter_filter_and_date(suite).
filter_filter_and_date(suite) -> [];
filter_filter_and_date(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),


    %% Insert some reports in the error log and start rb
    init_error_logs(),
    Between1 = calendar:local_time(),
    timer:sleep(1000),
    Between2 = calendar:local_time(),
    ?line error_logger:error_report([{rb_SUITE,rb_test_filter}]),    
    ?line ok = start_rb(OutFile),

    Before = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 10),
    After = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 1),

    ?line All = check_report(fun() -> rb:show() end,OutFile),
    ?line Last = hd(All),

    ?line [_,_,_] = rb_filter([{rb_SUITE,"rb_test",re}],{Before,After},OutFile),
    ?line [_,_] = rb_filter([{rb_SUITE,"rb_test",re}],{Before,Between1},OutFile),
    ?line [_] = rb_filter([{rb_SUITE,"rb_test",re}],{Between2,After},OutFile),
    ?line [_] = rb_filter([{rb_SUITE,rb_test_filter}],{Before,After},OutFile),
    ?line [] = rb_filter([{rb_SUITE,rb_test_filter}],{Before,Between1},OutFile),
    ?line [Last] = rb_filter([{rb_SUITE,rb_test_filter,no}],{Between2,After},OutFile),
    ?line {_,Str} = Last,
    ?line false = contains(Str,"rb_test_filter"),

    ok.


filter_re_no() -> filter_re_no(suite).
filter_re_no(suite) -> [];
filter_re_no(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ?line ok = start_rb(OutFile),

    ?line All = check_report(fun() -> rb:show() end,OutFile),

    ?line E = [_,_] = rb_filter([{rb_SUITE,"rb_test",re}],OutFile),
    ?line AllButE = rb_filter([{rb_SUITE,"rb_test",re,no}],OutFile),

    {_,AllRep} = lists:unzip(All),
    {_,ERep} = lists:unzip(E),
    {_,AllButERep} = lists:unzip(AllButE),
    ?line AllButERep = AllRep -- ERep,

    ok.


rescan() -> rescan(suite).
rescan(suite) -> [];
rescan(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),
    
    %% Start rb
    ?line ok = start_rb(OutFile),

    %% Insert one more report and check that the list is longer. Note
    %% that there might be two more reports, since the progress report
    %% from starting rb_server might not be included before the rescan.
    ?line AllBefore = capture(fun() -> rb:list() end),
    ?line error_logger:error_report([{rb_SUITE,rb_test_rescan}]),
    ?line ok = rb:rescan(),
    ?line AllAfter = capture(fun() -> rb:list() end),
    ?line Diff = length(AllAfter) - length(AllBefore),
    ?line true = (Diff >= 1),

    ok.


start_stop_log() -> start_stop_log(suite).
start_stop_log(suite) -> [];
start_stop_log(Config) ->
    ?line PrivDir = ?config(priv_dir,Config),
    ?line OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),
    ?line ok = file:write_file(OutFile,[]),

    %% Start rb and check that show is printed to standard_io
    ?line ok = start_rb(),
    ?line StdioResult = [_|_] = capture(fun() -> rb:show(1) end),
    ?line {ok,<<>>} = file:read_file(OutFile),
    
    %% Start log and check that show is printed to log and not to standad_io
    ?line ok = rb:start_log(OutFile),
    ?line [] = capture(fun() -> rb:show(1) end),
    ?line {ok,Bin} = file:read_file(OutFile),
    ?line true = (Bin =/= <<>>),

    %% Stop log and check that show is printed to standard_io and not to log
    ?line ok = rb:stop_log(),
    ?line ok = file:write_file(OutFile,[]),
    ?line StdioResult = capture(fun() -> rb:show(1) end),
    ?line {ok,<<>>} = file:read_file(OutFile),

    %% Test that standard_io is used if log file can not be opened
    ?line ok = rb:start_log(filename:join(nonexistingdir,"newfile.txt")),
    ?line StdioResult = capture(fun() -> rb:show(1) end),
    ?line {ok,<<>>} = file:read_file(OutFile),    

    ok.


%%%-----------------------------------------------------------------
%%% INTERNAL FUNCTIONS

restart_sasl() ->
    application:stop(sasl),
    ok = application:start(sasl),
    wait_for_sasl().

reset_sasl(Config) ->
    application:unset_env(sasl,error_logger_mf_dir),
    application:unset_env(sasl,error_logger_mf_maxbytes),
    application:unset_env(sasl,error_logger_mf_maxfiles),
    empty_error_logs(Config).

empty_error_logs(Config) ->
    application:stop(sasl),
    catch delete_content(?config(rb_dir, Config)),
    ok = application:start(sasl),
    wait_for_sasl().
    
wait_for_sasl() ->
    wait_for_sasl(50).
wait_for_sasl(0) ->
    ?t:fail("sasl application did not start within 5 seconds");
wait_for_sasl(N) ->
    case lists:keymember(sasl,1,application:which_applications()) of
	true ->
	    ok;
	false ->
	    timer:sleep(100),
	    wait_for_sasl(N-1)
    end.
    
start_rb(OutFile) ->
    do_start_rb([{start_log,OutFile}]).
start_rb() ->
    do_start_rb([]).

do_start_rb(Opts) ->
    {ok,Pid} = rb:start(Opts),

    %% Wait for process to started, then wait a little bit more
    sys:get_status(Pid),
    timer:sleep(500),

    %% Make sure printouts (e.g. from rb:list(), come to the test log,
    %% and that they can be captured.
    group_leader(group_leader(),Pid),
    ok.


delete_tree(Dir) ->
    case filelib:is_dir(Dir) of
	true ->
	    delete_content(Dir),
	    file:del_dir(Dir);
	false ->
	    ok = file:delete(Dir)
    end.

delete_content(Dir) ->
    {ok,Files} = file:list_dir(Dir),
    lists:foreach(fun(File) -> delete_tree(filename:join(Dir,File)) end, 
		  Files).

init_error_logs() ->        
    ?line error_logger:error_report([{rb_SUITE,rb_test_error}]),
    ?line error_logger:error_msg("rb_test_error_msg"),
    ?line error_logger:info_report([{rb_SUITE,rb_test_info}]),
    ?line error_logger:info_msg("rb_test_info_msg"),
    ?line _Pid = start(),
    ?line Ref = erlang:monitor(process,?MODULE),
    ?line gen_server:cast(?MODULE,crash),
    ?line receive {'DOWN',Ref,process,_,{rb_SUITE,rb_test_crash}} -> ok 
	  after 2000 -> 
		  ?t:format("Got: ~p~n",[process_info(self(),messages)]),
		  ?t:fail("rb_SUITE server never died")
	  end,
    ?line erlang:demonitor(Ref),
    ?line wait_for_server(),
    ok.

wait_for_server() ->
    case whereis(?MODULE) of
	undefined ->
	    wait_for_server();
	Pid ->
	    timer:sleep(100), % allow the supervisor report to be written
	    Pid
    end.

capture(Fun) ->
    ?t:capture_start(),
    ok = Fun(),
    timer:sleep(1000),
    ?t:capture_stop(),
    string:tokens(lists:append(?t:capture_get()),"\n").


rb_filter(Filter,OutFile) ->
    check_report(fun() -> rb:filter(Filter) end, OutFile).
rb_filter(Filter,Dates,OutFile) ->
    check_report(fun() -> rb:filter(Filter,Dates) end, OutFile).


%% This function first empties the given report file, then executes
%% the fun and returns a list of {N,Report}, where Report is a report
%% read from the file and N is an integer. The newest report has the
%% lowest number.
%% If the fun was a call to rb:show() (i.e. with no arguments), then
%% the numbering (N) will be the same as rb's own numbering (as shown
%% by rb:list()).
check_report(Fun,File) ->
    file:delete(File),
    rb:rescan([{start_log,File}]),
    ok = Fun(),
    {ok,Bin} = file:read_file(File),
    Reports = split_reports(binary_to_list(Bin),[],[]),
    lists:zip(lists:seq(1,length(Reports)),Reports).

-define(report_header_line,"\n===============================================================================\n").
split_reports([],Report,Reports) ->
    add_report(Report,Reports);
split_reports(Text,Report,Reports) ->
    case Text of
	?report_header_line++Rest ->
	    {Heading,PrevReport} = lists:splitwith(fun($\n) -> false;
						      (_) -> true
						   end,
						   Report),
	    split_reports(Rest,
			  ?report_header_line++Heading,
			  add_report(PrevReport,Reports));
	[Ch|Rest] ->
	    split_reports(Rest,[Ch|Report],Reports)
    end.

add_report(Report,Reports) ->    
    case string:strip(Report,both,$\n) of
	[] -> Reports;
	Report1 -> [lists:reverse(Report1)|Reports]
    end.

%% Returns true if Substr is a substring of Str.
contains(Str,Substr) ->
    0 =/= string:str(Str,Substr).

%% Sort the result of rb_list after report type
sort_list(List) ->
    sort_list(List,dict:new()).
sort_list([H|T],D) ->
    case re:run(H,"\s+[0-9]+\s+([a-z_]+)",[{capture,all_but_first,list}]) of
	nomatch ->
	    sort_list(T,D);
	{match,[TypeStr]} ->
	    sort_list(T,dict:append(list_to_atom(TypeStr),H,D))
    end;
sort_list([],D) ->
    lists:sort(dict:to_list(D)).


%%%-----------------------------------------------------------------
%%% A dummy supervisor and gen_server used for creating crash- and
%%% supervisor reports
start() ->
    {ok,Pid} =
	supervisor:start_link({local, ?SUP}, ?MODULE, i_am_supervisor),
    unlink(Pid),
    Pid.
start_server() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, i_am_server, []).
init(i_am_server) ->
    {ok, state};
init(i_am_supervisor) ->
    AChild = {?SUP,{?MODULE,start_server,[]},
	      permanent,2000,worker,[?MODULE]},
    {ok,{{one_for_all,1,1}, [AChild]}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(crash, State) ->
    exit({rb_SUITE,rb_test_crash}),
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.

