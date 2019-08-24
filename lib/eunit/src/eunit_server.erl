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
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc EUnit server process

-module(eunit_server).

-export([start/1, stop/1, start_test/4, watch/3, watch_path/3,
	 watch_regexp/3]).

-export([main/1]).  % private

-include("eunit.hrl").
-include("eunit_internal.hrl").


-define(AUTO_TIMEOUT, 60000).   %% auto test time limit

%% TODO: pass options to server, such as default timeout?

start(Server) when is_atom(Server) ->
    ensure_started(Server).

stop(Server) ->
    command(Server, stop).


-record(job, {super, test, options}).

%% The `Super' process will receive a stream of status messages; see
%% eunit_proc:status_message/3 for details.

start_test(Server, Super, T, Options) ->
    command(Server, {start, #job{super = Super,
				 test = T,
				 options = Options}}).

watch(Server, Module, Opts) when is_atom(Module) ->
    command(Server, {watch, {module, Module}, Opts}).

watch_path(Server, Path, Opts) ->
    command(Server, {watch, {path, filename:flatten(Path)}, Opts}).

%% note that the user must use $ at the end to match whole paths only
watch_regexp(Server, Regex, Opts) ->
    case re:compile(Regex,[anchored]) of
	{ok, R} ->
	    command(Server, {watch, {regexp, R}, Opts});
	{error, _}=Error ->
	    Error
    end.

%% This makes sure the server is started before sending the command, and
%% returns {ok, Result} if the server accepted the command or {error,
%% server_down} if the server process crashes. If the server does not
%% reply, this function will wait until the server is killed.

command(Server, Cmd) ->
    if is_atom(Server), Cmd /= stop -> ensure_started(Server);
       true -> ok
    end,
    if is_pid(Server) -> command_1(Server, Cmd);
       true ->
	    case whereis(Server) of
		undefined -> {error, server_down};
		Pid -> command_1(Pid, Cmd)
	    end
    end.

command_1(Pid, Cmd) when is_pid(Pid) ->
    Pid ! {command, self(), Cmd},
    command_wait(Pid, 1000, undefined).

command_wait(Pid, Timeout, Monitor) ->
    receive
	{Pid, Result} -> Result;
	{'DOWN', Monitor, process, Pid, _R} -> {error, server_down}
    after Timeout ->
	    %% avoid creating a monitor unless some time has passed
	    command_wait(Pid, infinity, erlang:monitor(process, Pid))
    end.

%% Starting the server

ensure_started(Name) ->
    ensure_started(Name, 5).

ensure_started(Name, N) when N > 0 ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_start(Name, Parent) end),
	    receive
		{Pid, ok} ->
		    Pid;
		{Pid, error} ->
		    receive after 200 -> ensure_started(Name, N - 1) end
	    end;
	Pid ->
	    Pid
    end;
ensure_started(_, _) ->
    throw(no_server).

server_start(undefined = Name, Parent) ->
    %% anonymous server
    server_start_1(Name, Parent);
server_start(Name, Parent) ->
    try register(Name, self()) of
	true -> server_start_1(Name, Parent)
    catch
	_:_ ->
	    Parent ! {self(), error},
	    exit(error)
    end.

server_start_1(Name, Parent) ->
    Parent ! {self(), ok},
    server_init(Name).

-record(state, {name,
		stopped,
		jobs,
		queue,
		auto_test,
		modules,
		paths,
		regexps}).

server_init(Name) ->
    server(#state{name = Name,
		  stopped = false,
		  jobs = dict:new(),
		  queue = queue:new(),
		  auto_test = queue:new(),
		  modules = sets:new(),
		  paths = sets:new(),
		  regexps = sets:new()}).

server(St) ->
    server_check_exit(St),
    ?MODULE:main(St).

%% @private
main(St) ->
    receive
	{done, auto_test, _Pid} ->
	    server(auto_test_done(St));
	{done, Reference, _Pid} ->
	    server(handle_done(Reference, St));
	{command, From, _Cmd} when St#state.stopped ->
	    From ! {self(), stopped};
	{command, From, Cmd} ->
	    server_command(From, Cmd, St);
	{code_monitor, {loaded, M, _Time}} ->
	    case is_watched(M, St) of
		true -> 
		    server(new_auto_test(self(), M, St));
		false ->
		    server(St)
	    end
    end.

server_check_exit(St) ->
    case dict:size(St#state.jobs) of
	0 when St#state.stopped -> exit(normal);
	_ -> ok
    end.

server_command(From, {start, Job}, St) ->
    Reference = make_ref(),
    St1 = case proplists:get_bool(enqueue, Job#job.options) of
	      true ->
		  enqueue(Job, From, Reference, St);
	      false ->
		  start_job(Job, From, Reference, St)
	  end,
    server_command_reply(From, {ok, Reference}),
    server(St1);
server_command(From, stop, St) ->
    %% unregister the server name and let remaining jobs finish
    server_command_reply(From, {error, stopped}),
    catch unregister(St#state.name),
    server(St#state{stopped = true});
server_command(From, {watch, Target, _Opts}, St) ->
    %% the code watcher is only started on demand
    %% TODO: this is disabled for now
    %%code_monitor:monitor(self()),
    %% TODO: propagate options to testing stage
    St1 = add_watch(Target, St),
    server_command_reply(From, ok),
    server(St1);
server_command(From, {forget, Target}, St) ->
    St1 = delete_watch(Target, St),
    server_command_reply(From, ok),
    server(St1);
server_command(From, Cmd, St) ->
    server_command_reply(From, {error, {unknown_command, Cmd}}),
    server(St).

server_command_reply(From, Result) ->
    From ! {self(), Result}.

enqueue(Job, From, Reference, St) ->
    case dict:size(St#state.jobs) of
	0 ->
	    start_job(Job, From, Reference, St);
	_ ->
	    St#state{queue = queue:in({Job, From, Reference},
				      St#state.queue)}
    end.

dequeue(St) ->
    case queue:out(St#state.queue) of
	{empty, _} ->
	    St;
	{{value, {Job, From, Reference}}, Queue} ->
	    start_job(Job, From, Reference, St#state{queue = Queue})
    end.

start_job(Job, From, Reference, St) ->
    From ! {start, Reference},
    %% The default is to run tests in order unless otherwise specified
    Order = proplists:get_value(order, Job#job.options, inorder),
    eunit_proc:start(Job#job.test, Order, Job#job.super, Reference),
    St#state{jobs = dict:store(Reference, From, St#state.jobs)}.

handle_done(Reference, St) ->
    case dict:find(Reference, St#state.jobs) of
	{ok, From} ->
	    From ! {done, Reference},
	    dequeue(St#state{jobs = dict:erase(Reference,
					       St#state.jobs)});
	error ->
	    St
    end.

%% Adding and removing watched modules or paths

add_watch({module, M}, St) ->
    St#state{modules = sets:add_element(M, St#state.modules)};
add_watch({path, P}, St) ->
    St#state{paths = sets:add_element(P, St#state.paths)};
add_watch({regexp, R}, St) ->
    St#state{regexps = sets:add_element(R, St#state.regexps)}.

delete_watch({module, M}, St) ->
    St#state{modules = sets:del_element(M, St#state.modules)};
delete_watch({path, P}, St) ->
    St#state{paths = sets:del_element(P, St#state.paths)};
delete_watch({regexp, R}, St) ->
    St#state{regexps = sets:del_element(R, St#state.regexps)}.

%% Checking if a module is being watched

is_watched(M, St) when is_atom(M) ->
    sets:is_element(M, St#state.modules) orelse
	is_watched(code:which(M), St);
is_watched(Path, St) ->
    sets:is_element(filename:dirname(Path), St#state.paths) orelse
	match_any(sets:to_list(St#state.regexps), Path).

match_any([R | Rs], Str) ->
    case re:run(Str, R, [{capture,none}]) of
	match -> true;
	_ -> match_any(Rs, Str)
    end;
match_any([], _Str) -> false.

%% Running automatic tests when a watched module is loaded.
%% Uses a queue in order to avoid overlapping output when several
%% watched modules are loaded simultaneously. (The currently running
%% automatic test is kept in the queue until it is done. An empty queue
%% means that no automatic test is running.)

new_auto_test(Server, M, St) ->
    case queue:is_empty(St#state.auto_test) of
	true ->
	    start_auto_test(Server, M);
	false ->
	    ok
    end,
    St#state{auto_test = queue:in({Server, M}, St#state.auto_test)}.

auto_test_done(St) ->
    %% remove finished test from queue before checking for more
    {_, Queue} = queue:out(St#state.auto_test),
    case queue:out(Queue) of
	{{value, {Server, M}}, _} ->
	    %% this is just lookahead - the item is not removed
	    start_auto_test(Server, M);
	{empty, _} ->
	    ok
    end,
    St#state{auto_test = Queue}.

start_auto_test(Server, M) ->
    spawn(fun () -> auto_super(Server, M) end).

auto_super(Server, M) ->
    process_flag(trap_exit, true),
    %% Give the user a short delay before any output is produced
    receive after 333 -> ok end,
    %% Make sure output is sent to console on server node
    group_leader(whereis(user), self()),
    Pid = spawn_link(fun () -> auto_proc(Server, M) end),
    receive
	{'EXIT', Pid, _} ->
	    ok
    after ?AUTO_TIMEOUT	->
	    exit(Pid, kill),
	    io:put_chars("\n== EUnit: automatic test was aborted ==\n"),
	    io:put_chars("\n> ")
    end,
    Server ! {done, auto_test, self()}.

auto_proc(Server, M) ->
    %% Make the output start on a new line instead of on the same line
    %% as the current shell prompt.
    io:fwrite("\n== EUnit: testing module ~w ==\n", [M]),
    eunit:test(Server, M, [enqueue]),
    %% Make sure to print a dummy prompt at the end of the output, most
    %% of all so that the Emacs mode realizes that input is active.
    io:put_chars("\n-> ").
