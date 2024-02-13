%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(user_sup).
-moduledoc false.

%% ---------------------------------------------
%% This is a supervisor bridge hiding the process
%% details of the user/group implementation.
%% ---------------------------------------------

-behaviour(supervisor_bridge).

-export([start/0]).

%% Internal exports.
-export([init/1, terminate/2, relay/1]).

-spec start() -> {'error', {'already_started', pid()}} | {'ok', pid()}.

start() ->
    supervisor_bridge:start_link(user_sup, []).

-spec init([]) -> 'ignore' | {'error', 'nouser'} | {'ok', pid(), pid()}.

init([]) ->
    init(init:get_arguments());
init(Flags) ->
    case get_user(Flags) of
	nouser ->
	    ignore;
	{master, Master} ->
	    Pid = start_relay(Master),
	    {ok, Pid, Pid};
	{M, F, A} ->
	    case start_user(M, F, A) of
		{ok, Pid} ->
		    {ok, Pid, Pid};
		Error ->
		    Error
	    end
    end.

start_relay(Master) ->
    case rpc:call(Master, erlang, whereis, [user]) of
	User when is_pid(User) ->
	    spawn(?MODULE, relay, [User]);
	_ ->
	    error_logger:error_msg("Cannot get remote user", []),
	    receive after 1000 -> true end,
	    halt()
    end.

-spec relay(pid()) -> no_return().

relay(Pid) ->
    register(user, self()),
    relay1(Pid).

relay1(Pid) ->
    receive
        X ->
            Pid ! X,
	    relay1(Pid)
    end.


%%-----------------------------------------------------------------
%% Sleep a while in order to let user write all (some) buffered 
%% information before termination.
%%-----------------------------------------------------------------

-spec terminate(term(), pid()) -> 'ok'.

terminate(_Reason, UserPid) ->
    receive after 1000 -> ok end,
    exit(UserPid, kill),
    ok.

%%-----------------------------------------------------------------
%% If there is a user, wait for it to register itself.  (But wait
%% no more than 10 seconds).  This is so the application_controller
%% is guaranteed that the user is started.
%%-----------------------------------------------------------------

start_user(Mod, Func, A) ->
    apply(Mod, Func, A),
    wait_for_user_p(100).

wait_for_user_p(0) ->
    {error, nouser};
wait_for_user_p(N) ->
    case whereis(user) of
	Pid when is_pid(Pid) ->
	    link(Pid),
	    {ok, Pid};
	_ ->
	    receive after 100 -> ok end,
	    wait_for_user_p(N-1)
    end.

get_user(Flags) ->
    check_flags(Flags, lists:keymember(detached, 1, Flags), {user_drv, start, []}).

%% These flags depend upon what arguments the erl script passes on
%% to erl91.
check_flags([{nouser, []} |T], Attached, _) -> check_flags(T, Attached, nouser);
check_flags([{user, [User]} | T], Attached, _) ->
    check_flags(T, Attached, {list_to_atom(User), start, []});
check_flags([{noshell, []} | T], Attached, _) ->
    check_flags(T, Attached, {user_drv, start, [#{ initial_shell => noshell }]});
check_flags([{oldshell, []} | T], false, _) ->
    %% When running in detached mode, we ignore any -oldshell flags as we do not
    %% want input => true to be set as they may halt the node (on bsd)
    check_flags(T, false, {user_drv, start, [#{ initial_shell => oldshell }]});
check_flags([{noinput, []} | T], Attached, _) ->
    check_flags(T, Attached, {user_drv, start, [#{ initial_shell => noshell, input => false }]});
check_flags([{master, [Node]} | T], Attached, _) ->
    check_flags(T, Attached, {master, list_to_atom(Node)});
check_flags([_H | T], Attached, User) -> check_flags(T, Attached, User);
check_flags([], _Attached, User) -> User.
