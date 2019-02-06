%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @copyright 2004-2009 Mickaël Rémond, Richard Carlsson
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @version {@version}, {@date} {@time}
%% @doc This module is the main EUnit user interface.

-module(eunit).

-include("eunit.hrl").
-include("eunit_internal.hrl").

%% Official exports
-export([start/0, stop/0, test/1, test/2]).

%% Experimental; may be removed or relocated
-export([start/1, stop/1, test/3, submit/1, submit/2, submit/3, watch/1,
	 watch/2, watch/3, watch_path/1, watch_path/2, watch_path/3,
	 watch_regexp/1, watch_regexp/2, watch_regexp/3, watch_app/1,
	 watch_app/2, watch_app/3]).

%% EUnit entry points

%% TODO: Command line interface similar to that of edoc?

%% @doc Starts the EUnit server. Normally, you don't need to call this
%% function; it is started automatically.
start() ->
    start(?SERVER).

%% @private
%% @doc See {@link start/0}.
start(Server) ->
    eunit_server:start(Server).

%% @doc Stops the EUnit server. Normally, you don't need to call this
%% function.
stop() ->
    stop(?SERVER).

%% @private
%% @doc See {@link stop/0}.
stop(Server) ->
    eunit_server:stop(Server).

%% @private
watch(Target) ->
    watch(Target, []).

%% @private
watch(Target, Options) ->
    watch(?SERVER, Target, Options).

%% @private
watch(Server, Target, Options) ->
    eunit_server:watch(Server, Target, Options).

%% @private
watch_path(Target) ->
    watch_path(Target, []).

%% @private
watch_path(Target, Options) ->
    watch_path(?SERVER, Target, Options).

%% @private
watch_path(Server, Target, Options) ->
    eunit_server:watch_path(Server, Target, Options).

%% @private
watch_regexp(Target) ->
    watch_regexp(Target, []).

%% @private
watch_regexp(Target, Options) ->
    watch_regexp(?SERVER, Target, Options).

%% @private
watch_regexp(Server, Target, Options) ->
    eunit_server:watch_regexp(Server, Target, Options).

%% @private
watch_app(Name) ->
    watch_app(Name, []).

%% @private
watch_app(Name, Options) ->
    watch_app(?SERVER, Name, Options).

%% @private
watch_app(Server, Name, Options) ->
    case code:lib_dir(Name) of
	Path when is_list(Path) ->
	    watch_path(Server, filename:join(Path, "ebin"), Options);
	_ ->
	    error
    end.

%% @equiv test(Tests, [])
test(Tests) ->
    test(Tests, []).

%% @spec test(Tests::term(), Options::[term()]) -> ok | {error, term()}
%% @doc Runs a set of tests. The format of `Tests' is described in the
%% section <a
%% href="overview-summary.html#EUnit_test_representation">EUnit test
%% representation</a> of the overview.
%%
%% Example: ```eunit:test(fred)''' runs all tests in the module `fred'
%% and also any tests in the module `fred_tests', if that module exists.
%%
%% Options:
%% <dl>
%% <dt>`verbose'</dt>
%% <dd>Displays more details about the running tests.</dd>
%% </dl>
%%
%% Options in the environment variable EUNIT are also included last in
%% the option list, i.e., have lower precedence than those in `Options'.
%% @see test/1
test(Tests, Options) ->
    test(?SERVER, Tests, all_options(Options)).

%% @private
%% @doc See {@link test/2}.
test(Server, Tests, Options) ->
    Listeners = listeners(Options),
    Serial = eunit_serial:start(Listeners),
    case eunit_server:start_test(Server, Serial, Tests, Options) of
	{ok, Reference} -> test_run(Reference, Listeners);
	{error, R} -> {error, R}
    end.

test_run(Reference, Listeners) ->
    receive
	{start, Reference} ->
	    cast(Listeners, {start, Reference})
    end,
    receive
	{done, Reference} ->
	    cast(Listeners, {stop, Reference, self()}),
            wait_until_listeners_have_terminated(Listeners),
	    receive
		{result, Reference, Result} ->
		    Result
	    end
    end.

cast([P | Ps], Msg) ->
    P ! Msg,
    cast(Ps, Msg);
cast([], _Msg) ->
    ok.

wait_until_listeners_have_terminated([P | Ps]) ->
    MRef = erlang:monitor(process, P),
    receive
        {'DOWN', MRef, process, P, _} ->
            wait_until_listeners_have_terminated(Ps)
    end;
wait_until_listeners_have_terminated([]) ->
    ok.

%% TODO: functions that run tests on a given node, not a given server
%% TODO: maybe some functions could check for a globally registered server?
%% TODO: some synchronous but completely quiet interface function

%% @private
submit(T) ->
    submit(T, []).

%% @private
submit(T, Options) ->
    submit(?SERVER, T, Options).

%% @private
submit(Server, T, Options) ->
    Dummy = spawn(fun devnull/0),
    eunit_server:start_test(Server, Dummy, T, Options).

listeners(Options) ->
    %% note that eunit_tty must always run, because it sends the final
    %% {result,...} message that the test_run() function is waiting for
    Ls = [{eunit_tty, Options} | proplists:get_all_values(report, Options)],
    Ps = start_listeners(Ls),
    %% the event_log option is for debugging, to view the raw events
    case proplists:get_value(event_log, Options) of
	undefined ->
	    Ps;
	X ->
	    LogFile = if is_list(X) -> X;
			 true -> "eunit-events.log"
		      end,
	    [spawn_link(fun () -> event_logger(LogFile) end) | Ps]
    end.

start_listeners([P | Ps]) when is_pid(P) ; is_atom(P) ->
    [P | start_listeners(Ps)];
start_listeners([{Mod, Opts} | Ps]) when is_atom(Mod) ->
    [Mod:start(Opts) | start_listeners(Ps)];	    
start_listeners([]) ->
    [].

%% TODO: make this report file errors
event_logger(LogFile) ->
    case file:open(LogFile, [write]) of
	{ok, FD} ->
	    receive
		{start, Reference} ->
		    event_logger_loop(Reference, FD)
	    end;
	Error ->
	    exit(Error)
    end.

event_logger_loop(Reference, FD) ->
    receive
	{status, _Id, _Info}=Msg ->
	    io:fwrite(FD, "~tp.\n", [Msg]),
	    event_logger_loop(Reference, FD);
	{stop, Reference, _ReplyTo} ->
	    %% no need to reply, just exit
	    file:close(FD),
	    exit(normal)
    end.

%% TODO: make a proper logger for asynchronous execution with submit/3

devnull() ->
    receive _ -> devnull() end.

%% including options from EUNIT environment variable

all_options(Opts) ->
    try os:getenv("EUNIT") of
	false -> Opts;
	S ->
	    {ok, Ts, _} = erl_scan:string(S),
	    {ok, V} = erl_parse:parse_term(Ts ++ [{dot,erl_anno:new(1)}]),
	    if is_list(V) -> Opts ++ V;
	       true -> Opts ++ [V]
	    end
    catch
	_:_ -> Opts
    end.
