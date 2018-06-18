%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2018. All Rights Reserved.
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
-module(sasl_report_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([gen_server_crash/1, gen_server_crash_unicode/1]).

-export([crash_me/0,start_link/0,init/1,handle_cast/2,terminate/2]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [gen_server_crash, gen_server_crash_unicode].

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

gen_server_crash(Config) ->
    gen_server_crash(Config, latin1).

gen_server_crash_unicode(Config) ->
    gen_server_crash(Config, unicode).

gen_server_crash(Config, Encoding) ->
    StopFilter = {fun(_,_) -> stop end, ok},
    logger:add_handler_filter(default,stop_all,StopFilter),
    logger:add_handler_filter(cth_log_redirect,stop_all,StopFilter),
    try
	do_gen_server_crash(Config, Encoding)
    after
        ok = application:unset_env(kernel, logger_sasl_compatible),
	ok = application:unset_env(sasl, sasl_error_logger),
	ok = application:unset_env(kernel, error_logger_format_depth),
        logger:remove_handler_filter(default,stop_all),
        logger:remove_handler_filter(cth_log_redirect,stop_all)
    end,
    ok.

do_gen_server_crash(Config, Encoding) ->
    PrivDir = ?config(priv_dir, Config),
    LogDir = filename:join(PrivDir, ?MODULE),
    KernelLog = filename:join(LogDir, "kernel.log"),
    SaslLog = filename:join(LogDir, "sasl.log"),
    ok = filelib:ensure_dir(SaslLog),

    application:stop(sasl),
    Modes = [write, {encoding, Encoding}],
    ok = application:set_env(kernel, logger_sasl_compatible, true),
    ok = application:set_env(sasl, sasl_error_logger, {file,SaslLog,Modes},
			     [{persistent,true}]),
    application:set_env(kernel, error_logger_format_depth, 30),
    error_logger:logfile({open,KernelLog}),
    application:start(sasl),
    ct:log("Logger config:~n~p",[logger:get_config()]),
    ct:log("error_logger handlers: ~p",[error_logger:which_report_handlers()]),

    crash_me(),


    error_logger:logfile(close),
    application:stop(sasl),

    check_file(KernelLog, utf8, 70000, 150000),
    check_file(SaslLog, Encoding, 70000, 150000),

    ok = file:delete(KernelLog),
    ok = file:delete(SaslLog),
    ok.

check_file(File, Encoding, Min, Max) ->
    {ok,Bin} = file:read_file(File),
    Base = filename:basename(File),
    io:format("*** Contents of ~s ***\n", [Base]),
    case Encoding of
        latin1 -> io:format("~s\n", [Bin]);
        _ -> io:format("~ts\n", [Bin])
    end,
    Sz = byte_size(Bin),
    io:format("Size: ~p (allowed range is ~p..~p)\n",
	      [Sz,Min,Max]),
    if
	Sz < Min ->
	    %% Truncated? Other problem?
	    ?t:fail({too_short,Base});
	Sz > Max ->
	    %% Truncation doesn't work?
	    ?t:fail({too_big,Base});
	true ->
	    ok
    end.

%%%
%%% gen_server that crashes.
%%%

crash_me() ->
    {ok,SuperPid} = supervisor:start_link(sasl_report_suite_supervisor, []),
    [{Id,Pid,_,_}] = supervisor:which_children(SuperPid),
    HugeData = gb_sets:from_list(lists:seq(1, 100000)),
    SomeData1 = list_to_atom([246]),
    SomeData2 = list_to_atom([1024]),
    gen_server:cast(Pid, {HugeData,SomeData1,SomeData2}),
    Ref = monitor(process, Pid),
    receive
	{'DOWN',Ref,process,Pid,_} ->
	    supervisor:terminate_child(SuperPid, Id),
	    unlink(SuperPid),
	    exit(SuperPid, kill),
	    ok
    end.

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init(_) ->
    St = <<0:100000/unit:8>>,
    {ok,St}.

handle_cast(Big, St) ->
    Seq = lists:seq(1, 10000),
    Latin1Atom = list_to_atom([246]),
    UnicodeAtom = list_to_atom([1024]),
    put(Latin1Atom, Latin1Atom),
    put(UnicodeAtom, UnicodeAtom),
    self() ! Latin1Atom,
    self() ! UnicodeAtom,
    self() ! Seq,
    self() ! Seq,
    self() ! Seq,
    self() ! Seq,
    self() ! Seq,
    x = Big,
    {noreply,St}.

terminate(_, _) ->
    ok.
