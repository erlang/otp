%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(eprof_SUITE).

-include("test_server.hrl").

-export([all/1,tiny/1,eed/1]).

all(suite) -> [tiny,eed].


tiny(suite) -> [];
tiny(Config) when is_list(Config) -> 
    ?line ensure_eprof_stopped(),
    ?line {ok, OldCurDir} = file:get_cwd(),
    Datadir = ?config(data_dir, Config),
    Privdir = ?config(priv_dir, Config),
    ?line TTrap=?t:timetrap(60*1000),
    % (Trace)Compile to priv_dir and make sure the correct version is loaded.
    ?line {ok,eprof_suite_test} = compile:file(filename:join(Datadir,
							     "eprof_suite_test"),
					       [trace,{outdir, Privdir}]),
    ?line ok = file:set_cwd(Privdir),
    ?line code:purge(eprof_suite_test),
    ?line {module,eprof_suite_test} = code:load_file(eprof_suite_test),
    ?line {ok,_Pid} = eprof:start(),
    ?line nothing_to_analyse = eprof:analyse(),
    ?line nothing_to_analyse = eprof:total_analyse(),
    ?line eprof:profile([], eprof_suite_test, test, [Config]),
    ?line ok = eprof:analyse(),
    ?line ok = eprof:total_analyse(),
    ?line ok = eprof:log("eprof_SUITE_logfile"),
    ?line stopped = eprof:stop(),
    ?line ?t:timetrap_cancel(TTrap),
    ?line ok = file:set_cwd(OldCurDir),
    ok.

eed(suite) -> [];
eed(Config) when is_list(Config) ->
    ?line ensure_eprof_stopped(),
    ?line Datadir = ?config(data_dir, Config),
    ?line Privdir = ?config(priv_dir, Config),
    ?line TTrap=?t:timetrap(5*60*1000),

    %% (Trace)Compile to priv_dir and make sure the correct version is loaded.
    ?line code:purge(eed),
    ?line {ok,eed} = c:c(filename:join(Datadir, "eed"), [trace,{outdir,Privdir}]),
    ?line {ok,_Pid} = eprof:start(),
    ?line Script = filename:join(Datadir, "ed.script"),
    ?line ok = file:set_cwd(Datadir),
    ?line {T1,_} = statistics(runtime),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line ok = eed:file(Script),
    ?line {T2,_} = statistics(runtime),
    ?line {ok,ok} = eprof:profile([], eed, file, [Script]),
    ?line {T3,_} = statistics(runtime),
    ?line profiling_already_stopped = eprof:stop_profiling(),
    ?line ok = eprof:analyse(),
    ?line ok = eprof:total_analyse(),
    ?line ok = eprof:log("eprof_SUITE_logfile"),
    ?line stopped = eprof:stop(),
    ?line ?t:timetrap_cancel(TTrap),
    S = lists:flatten(io_lib:format("~p times slower", [10*(T3-T2)/(T2-T1)])),
    {comment,S}.

ensure_eprof_stopped() ->
    Pid = whereis(eprof),
    case whereis(eprof) of
	undefined ->
	    ok;
	Pid ->
	    ?line stopped=eprof:stop()
    end.
