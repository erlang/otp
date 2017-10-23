%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

-module(rb_SUITE).
-include_lib("common_test/include/ct.hrl").


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
                                      show_other,
				      list,
				      rescan,
				      start_stop_log,
				      grep,
				      filter_filter,
				      filter_date,
				      filter_filter_and_date,
				      filter_re_no
				     ]}].


init_per_suite(Config) ->
    PrivDir = ?config(priv_dir,Config),
    RbDir = filename:join(PrivDir,rb),
    ok = file:make_dir(RbDir),
    NewConfig = [{rb_dir,RbDir}|Config],
    reset_sasl(NewConfig),
    NewConfig.

end_per_suite(_Config) ->
    ok.

init_per_group(running_error_logger,Config) ->
    %% Install log_mf_h
    RbDir = ?config(rb_dir,Config),
    ok = application:set_env(sasl,error_logger_mf_dir,RbDir),
    ok = application:set_env(sasl,error_logger_mf_maxbytes,5000),
    ok = application:set_env(sasl,error_logger_mf_maxfiles,2),
    restart_sasl(),
    Config.

end_per_group(running_error_logger,_Config) ->
    %% Remove log_mf_h???
    ok.


init_per_testcase(_Case,Config) ->
    case whereis(?SUP) of
	undefined ->
	    ok;
	Sup ->
	    Server = whereis(?MODULE),
	    exit(Sup,kill),
	    wait_for_down([Server,Sup])
    end,
    empty_error_logs(Config),
    Config.

wait_for_down([]) ->
    ok;
wait_for_down([undefined|Rest]) ->
    wait_for_down(Rest);
wait_for_down([Pid|Rest]) ->
    Ref = erlang:monitor(process,Pid),
    receive {'DOWN', Ref, process, Pid, _Info} -> ok end,
    wait_for_down(Rest).

end_per_testcase(Case,Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.


%%%-----------------------------------------------------------------
%%% Test cases

help(_Config) ->
    Help = capture(fun() -> rb:h() end),
    %% Check that first and last line is there
    true = lists:member("Report Browser Tool - usage", Help),
    true = lists:member("rb:stop            - stop the rb_server", Help),
    ok.

%% Test that all three sasl env vars must be set for a successful start of rb
%% Then stop rb.
start_error_stop(Config) ->
    RbDir = ?config(rb_dir,Config),

    {error,{"cannot locate report directory",_}} = rb:start(),


    ok = application:set_env(sasl,error_logger_mf_dir,"invaliddir"),
    ok = application:set_env(sasl,error_logger_mf_maxbytes,1000),
    ok = application:set_env(sasl,error_logger_mf_maxfiles,2),
    restart_sasl(),
    {error,{"cannot read the index file",_}} = rb:start(),
    ok = application:set_env(sasl,error_logger_mf_dir,RbDir),
    restart_sasl(),
    {ok,_} = rb:start(),

    ok = rb:stop(),
    ok.

show(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ok = start_rb(OutFile),

    %% Show all reports
    All = check_report(fun() -> rb:show() end,OutFile),

    %% Show by number
    [{_,First}] = check_report(fun() -> rb:show(1) end,OutFile),
    {1,First} = lists:keyfind(1,1,All),

    %% Show by type
    [{_,CR}] = check_report(fun() -> rb:show(crash_report) end,OutFile),
    true = contains(CR,"rb_test_crash"),
    [{_,EC},{_,EM}] = check_report(fun() -> rb:show(error) end,OutFile),
    true = contains(EC,"rb_test_crash"),
    true = contains(EM,"rb_test_error_msg"),
    [{_,ER}] = check_report(fun() -> rb:show(error_report) end,OutFile),
    true = contains(ER,"rb_test_error"),
    [{_,IR}] = check_report(fun() -> rb:show(info_report) end,OutFile),
    true = contains(IR,"rb_test_info"),
    [{_,IM}] = check_report(fun() -> rb:show(info_msg) end,OutFile),
    true = contains(IM,"rb_test_info_msg"),
    [_|_] = check_report(fun() -> rb:show(progress) end,OutFile),
    [{_,SR}] = check_report(fun() -> rb:show(supervisor_report) end,
			    OutFile),
    true = contains(SR,"child_terminated"),
    true = contains(SR,"{rb_SUITE,rb_test_crash}"),

    ok.

show_other(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    error_logger:info_report([rb_test_term_in_list]),
    error_logger:info_report(rb_test_term_no_list),
    ok = start_rb(OutFile),

    %% Show by type and check content
    [{_,I1},{_,I2}] = check_report(fun() -> rb:show(info_report) end,OutFile),

    true = contains(I1,"rb_test_term_no_list"),
    true = contains(I2,"rb_test_term_in_list"),

    ok.

list(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ok = start_rb(OutFile),

    All = capture(fun() -> rb:list() end),
    [{crash_report,[_]=CR},
     {error,[_,_]=EM},
     {error_report,[_]=ER},
     {info_msg,[_]=IM},
     {info_report,[_]=IR},
     {progress,[_|_]=P},
     {supervisor_report,[_]=SR}] = sort_list(All),

    [{crash_report,CR}] =
	sort_list(capture(fun() -> rb:list(crash_report) end)),
    [{error,EM}] =
	sort_list(capture(fun() -> rb:list(error) end)),
    [{error_report,ER}] =
	sort_list(capture(fun() -> rb:list(error_report) end)),
    [{info_msg,IM}] =
	sort_list(capture(fun() -> rb:list(info_msg) end)),
    [{info_report,IR}] =
	sort_list(capture(fun() -> rb:list(info_report) end)),
    [{progress,P}] =
	sort_list(capture(fun() -> rb:list(progress) end)),
    [{supervisor_report,SR}] =
	sort_list(capture(fun() -> rb:list(supervisor_report) end)),

    ok.

grep(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ok = start_rb(OutFile),

    [{_,S},
     {_,CR},
     {_,EC},
     {_,IM},
     {_,IR},
     {_,EM},
     {_,ER}]= check_report(fun() -> rb:grep("rb_test_") end,OutFile),
    true = contains(S, "rb_test_crash"),
    true = contains(CR, "rb_test_crash"),
    true = contains(EC, "rb_test_crash"),
    true = contains(IM, "rb_test_info_msg"),
    true = contains(IR, "rb_test_info"),
    true = contains(EM, "rb_test_error_msg"),
    true = contains(ER, "rb_test_error"),
    ok.

filter_filter(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ok = start_rb(OutFile),

    All = check_report(fun() -> rb:show() end,OutFile),

    ER = [_] = rb_filter([{rb_SUITE,rb_test_error}],OutFile),
    [] = rb_filter([{rb_SUITE,rb_test}],OutFile),
    _E = [_,_] = rb_filter([{rb_SUITE,"rb_test",re}],OutFile),
    AllButER = rb_filter([{rb_SUITE,rb_test_error,no}],OutFile),

    {_,AllRep} = lists:unzip(All),
    {_,ERRep} = lists:unzip(ER),
    {_,AllButERRep} = lists:unzip(AllButER),
    AllButERRep = AllRep -- ERRep,

    ok.

filter_date(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),


    %% Insert some reports in the error log and start rb
    init_error_logs(),
    Between1 = calendar:local_time(),
    timer:sleep(1000),
    Between2 = calendar:local_time(),
    ok = start_rb(OutFile),

    All = check_report(fun() -> rb:show() end,OutFile),

    Before = calendar:gregorian_seconds_to_datetime(
	       calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 10),
    After = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 1),

    All = rb_filter([],{Before,from},OutFile),
    All = rb_filter([],{After,to},OutFile),
    [] = rb_filter([],{Before,to},OutFile),
    [] = rb_filter([],{After,from},OutFile),
    All = rb_filter([],{Before,After},OutFile),

    %%?t:format("~p~n",[All]),
    AllButLast = [{N-1,R} || {N,R} <- tl(All)],
    AllButLast = rb_filter([],{Before,Between1},OutFile),

    Last = hd(All),
    [Last] = rb_filter([],{Between2,After},OutFile),

    ok.

filter_filter_and_date(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),


    %% Insert some reports in the error log and start rb
    init_error_logs(),
    Between1 = calendar:local_time(),
    timer:sleep(1000),
    Between2 = calendar:local_time(),
    error_logger:error_report([{rb_SUITE,rb_test_filter}]),
    ok = start_rb(OutFile),

    Before = calendar:gregorian_seconds_to_datetime(
	       calendar:datetime_to_gregorian_seconds(calendar:local_time()) - 10),
    After = calendar:gregorian_seconds_to_datetime(
	      calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 1),

    All = check_report(fun() -> rb:show() end,OutFile),
    Last = hd(All),

    [_,_,_] = rb_filter([{rb_SUITE,"rb_test",re}],{Before,After},OutFile),
    [_,_] = rb_filter([{rb_SUITE,"rb_test",re}],{Before,Between1},OutFile),
    [_] = rb_filter([{rb_SUITE,"rb_test",re}],{Between2,After},OutFile),
    [_] = rb_filter([{rb_SUITE,rb_test_filter}],{Before,After},OutFile),
    [] = rb_filter([{rb_SUITE,rb_test_filter}],{Before,Between1},OutFile),
    [Last] = rb_filter([{rb_SUITE,rb_test_filter,no}],{Between2,After},OutFile),
    {_,Str} = Last,
    false = contains(Str,"rb_test_filter"),

    ok.


filter_re_no(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Insert some reports in the error log and start rb
    init_error_logs(),
    ok = start_rb(OutFile),

    All = check_report(fun() -> rb:show() end,OutFile),

    E = [_,_] = rb_filter([{rb_SUITE,"rb_test",re}],OutFile),
    AllButE = rb_filter([{rb_SUITE,"rb_test",re,no}],OutFile),

    {_,AllRep} = lists:unzip(All),
    {_,ERep} = lists:unzip(E),
    {_,AllButERep} = lists:unzip(AllButE),
    AllButERep = AllRep -- ERep,

    ok.


rescan(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),

    %% Start rb
    ok = start_rb(OutFile),

    %% Insert one more report and check that the list is longer. Note
    %% that there might be two more reports, since the progress report
    %% from starting rb_server might not be included before the rescan.
    AllBefore = capture(fun() -> rb:list() end),
    error_logger:error_report([{rb_SUITE,rb_test_rescan}]),
    ok = rb:rescan(),
    AllAfter = capture(fun() -> rb:list() end),
    Diff = length(AllAfter) - length(AllBefore),
    true = (Diff >= 1),

    ok.


start_stop_log(Config) ->
    PrivDir = ?config(priv_dir,Config),
    OutFile = filename:join(PrivDir,"rb_SUITE_log.txt"),
    ok = file:write_file(OutFile,[]),

    %% Start rb and check that show is printed to standard_io
    ok = start_rb(),
    StdioResult = [_|_] = capture(fun() -> rb:show(1) end),
    {ok,<<>>} = file:read_file(OutFile),

    %% Start log and check that show is printed to log and not to standard_io
    ok = rb:start_log(OutFile),
    [] = capture(fun() -> rb:show(1) end),
    {ok,Bin} = file:read_file(OutFile),
    true = (Bin =/= <<>>),

    %% Start log with atom standard_io and check that show is printed to standard_io
    ok = rb:stop_log(),
    ok = file:write_file(OutFile,[]),
    ok = rb:start_log(standard_io),
    StdioResult = [_|_] = capture(fun() -> rb:show(1) end),
    {ok,<<>>} = file:read_file(OutFile),

    %% Start log and check that show is printed to iodevice log and not to standard_io
    ok = rb:stop_log(),
    ok = file:write_file(OutFile,[]),
    {ok, IoOutFile} = file:open(OutFile,[write]),
    ok = rb:start_log(IoOutFile),
    [] = capture(fun() -> rb:show(1) end),
    {ok,Bin} = file:read_file(OutFile),
    true = (Bin =/= <<>>),
    ok = file:close(IoOutFile),

    %% Stop log and check that show is printed to standard_io and not to log
    ok = rb:stop_log(),
    ok = file:write_file(OutFile,[]),
    StdioResult = capture(fun() -> rb:show(1) end),
    {ok,<<>>} = file:read_file(OutFile),

    %% Start log and check that list is printed to log and not to standard_io
    ok = file:write_file(OutFile,[]),
    ok = rb:start_log(OutFile),
    [] = capture(fun() -> rb:log_list() end),
    {ok,Bin2} = file:read_file(OutFile),
    true = (Bin2 =/= <<>>),

    %% Stop log and check that list is printed to standard_io and not to log
    ok = rb:stop_log(),
    ok = file:write_file(OutFile,[]),
    StdioResult2 = capture(fun() -> rb:log_list() end),
    {ok,<<>>} = file:read_file(OutFile),
    
    %% Test that standard_io is used if log file can not be opened
    ok = rb:start_log(filename:join(nonexistingdir,"newfile.txt")),
    StdioResult = capture(fun() -> rb:show(1) end),
    {ok,<<>>} = file:read_file(OutFile),

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
    error_logger:error_report([{rb_SUITE,rb_test_error}]),
    error_logger:error_msg("rb_test_error_msg"),
    error_logger:info_report([{rb_SUITE,rb_test_info}]),
    error_logger:info_msg("rb_test_info_msg"),
    _Pid = start(),
    Ref = erlang:monitor(process,?MODULE),
    gen_server:cast(?MODULE,crash),
    receive {'DOWN',Ref,process,_,{rb_SUITE,rb_test_crash}} -> ok
    after 2000 ->
	    ?t:format("Got: ~p~n",[process_info(self(),messages)]),
	    ?t:fail("rb_SUITE server never died")
    end,
    erlang:demonitor(Ref),
    wait_for_server(),
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

