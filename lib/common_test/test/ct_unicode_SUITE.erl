%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_unicode_SUITE
%%%
%%% Description:
%%% Test that common_test handles and logs unicode strings and atoms
%%% correctly.
%%%
%%% The suite used for the test is located in the data directory.
%%%-------------------------------------------------------------------
-module(ct_unicode_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-define(eh, ct_test_support_eh).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Description: Since Common Test starts another Test Server
%% instance, the tests need to be performed on a separate node (or
%% there will be clashes with logging processes etc).
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case file:native_name_encoding() of
        latin1 -> {skip,"Test is not applicable on latin1 file system"};
        _ ->
            ct_test_support:init_per_suite([{printable_range,unicode}|Config])
    end.

end_per_suite(Config) ->
    ct_test_support:end_per_suite(Config).

init_per_testcase(TestCase, Config) ->
    ct_test_support:init_per_testcase(TestCase, Config).

end_per_testcase(TestCase, Config) ->
    ct_test_support:end_per_testcase(TestCase, Config).

suite() -> [].

all() ->
    [unicode_atoms_SUITE,
     unicode_spec].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------
unicode_atoms_SUITE(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    run_test(unicode_atoms_SUITE,
             [{dir,DataDir},{suite,unicode_atoms_SUITE}], Config).

unicode_spec(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    CfgName = "unicode_αβ.cfg",
    Cfg = io_lib:format("{~tw,[{~tw,\"~ts\"}]}.~n",
                        ['key_αβ','subkey_αβ',"value_αβ"]),
    ok = file:write_file(filename:join(PrivDir,CfgName),
                         unicode:characters_to_binary(Cfg)),
    TestSpec = [{cases, DataDir, unicode_atoms_SUITE, ['config_αβ']},
                {config, PrivDir, CfgName}],
    TestSpecName = ct_test_support:write_testspec(TestSpec, PrivDir,
                                                  "unicode_αβ.spec"),
    run_test(unicode_spec,[{spec,TestSpecName}],Config).

%%%-----------------------------------------------------------------
%%% HELP FUNCTIONS
%%%-----------------------------------------------------------------
run_test(Label, Test, Config) ->
    {Opts,ERPid} = setup_env([{label,Label}|Test], Config),
    ok = ct_test_support:run(Opts, Config),
    TestEvents = ct_test_support:get_events(ERPid, Config),
    ct_test_support:log_events(Label,
			       reformat_events(TestEvents, ?eh),
			       ?config(priv_dir, Config),
			       Opts),
    ExpEvents = events_to_check(Label),
    ok = ct_test_support:verify_events(ExpEvents, TestEvents, Config),
    check_logs([_,_]=get_log_dirs(TestEvents)).

setup_env(Test, Config) ->
    Opts0 = ct_test_support:get_opts(Config),
    Level = ?config(trace_level, Config),
    EvHArgs = [{cbm,ct_test_support},{trace_level,Level}],
    Opts = Opts0 ++ [{event_handler,{?eh,EvHArgs}} | Test],
    ERPid = ct_test_support:start_event_receiver(Config),
    {Opts,ERPid}.

reformat_events(Events, EH) ->
    ct_test_support:reformat(Events, EH).

get_log_dirs([{?eh,#event{name=start_logging,data=LogDir}}|Events]) ->
    [LogDir|get_log_dirs(Events)];
get_log_dirs([_|Events]) ->
    get_log_dirs(Events);
get_log_dirs([]) ->
    [].

%%%-----------------------------------------------------------------
%%% TEST EVENTS
%%%-----------------------------------------------------------------
events_to_check(Test) ->
    %% 2 tests (ct:run_test + script_start) is default
    events_to_check(Test, 2).

events_to_check(_, 0) ->
    [];
events_to_check(Test, N) ->
    test_events(Test) ++ events_to_check(Test, N-1).

test_events(unicode_atoms_SUITE) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,6}},
     {?eh,tc_start,{unicode_atoms_SUITE,init_per_suite}},
     {?eh,tc_done,{unicode_atoms_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{unicode_atoms_SUITE,'test_αβ'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'test_αβ',ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,'fail_αβ_1'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'fail_αβ_1','_'}},
     {?eh,test_stats,{1,1,{0,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,'fail_αβ_2'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'fail_αβ_2','_'}},
     {?eh,test_stats,{1,2,{0,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,'fail_αβ_3'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'fail_αβ_3','_'}},
     {?eh,test_stats,{1,3,{0,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,'fail_αβ_4'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'fail_αβ_4','_'}},
     {?eh,test_stats,{1,4,{0,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,'skip_αβ'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'skip_αβ','_'}},
     {?eh,test_stats,{1,4,{1,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,end_per_suite}},
     {?eh,tc_done,{unicode_atoms_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ];
test_events(unicode_spec) ->
    [
     {?eh,start_logging,{'DEF','RUNDIR'}},
     {?eh,test_start,{'DEF',{'START_TIME','LOGDIR'}}},
     {?eh,start_info,{1,1,1}},
     {?eh,tc_start,{unicode_atoms_SUITE,init_per_suite}},
     {?eh,tc_done,{unicode_atoms_SUITE,init_per_suite,ok}},
     {?eh,tc_start,{unicode_atoms_SUITE,'config_αβ'}},
     {?eh,tc_done,{unicode_atoms_SUITE,'config_αβ',ok}},
     {?eh,test_stats,{1,0,{0,0}}},
     {?eh,tc_start,{unicode_atoms_SUITE,end_per_suite}},
     {?eh,tc_done,{unicode_atoms_SUITE,end_per_suite,ok}},
     {?eh,test_done,{'DEF','STOP_TIME'}},
     {?eh,stop_logging,[]}
    ].

%%%-----------------------------------------------------------------
%%% Check logs for escaped unicode characters
check_logs(Dirs) ->
    ct:log("Checking logs for escaped unicode characters (αβ).~nDirs:~n~tp",
           [Dirs]),
    {ok,RE} = re:compile(<<"x{3B[12]}"/utf8>>),
    case check_logs1(RE,Dirs,[]) of
        [] ->
            ok;
        Match ->
            MatchStr = lists:join("\n",Match),
            ct:log("ERROR: Escaped unicode characters found in:~n~ts",[MatchStr]),
            ct:fail(escaped_unicode_characters_found)
    end.

check_logs1(RE,[F|Fs],Match) ->
    New = case filelib:is_dir(F) of
              true ->
                  {ok,Files} = file:list_dir(F),
                  check_logs1(RE,[filename:join(F,File)||File<-Files],[]);
              false ->
                  check_log(RE,F)
          end,
    check_logs1(RE,Fs,New++Match);
check_logs1(_RE,[],Match) ->
    Match.

check_log(RE,F) ->
    {ok,Bin} = file:read_file(F),
    case re:run(Bin,RE,[{capture,none}]) of
        match ->
            [F];
        nomatch ->
            []
    end.
