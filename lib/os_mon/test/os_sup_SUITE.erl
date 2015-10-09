%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
-module(os_sup_SUITE).
-include_lib("test_server/include/test_server.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([message/1]).
-export([config/1, port/1]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

-define(TAG, test_tag).
-define(MFA, {?MODULE, test_mfa, [?TAG]}).

-export([test_mfa/2]).

init_per_suite(Config) when is_list(Config) ->
    spawn(fun() -> message_receptor() end),
    ?line application:load(os_mon),
    ?line ok = application:set_env(os_mon, start_os_sup, true),
    ?line ok = application:set_env(os_mon, os_sup_mfa, ?MFA),
    ?line ok = application:set_env(os_mon, os_sup_enable, false),
    ?line ok = application:start(os_mon),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ?line application:stop(os_mon),
    ?line ok = application:set_env(os_mon, start_os_sup, false),
    MFA = {os_sup, error_report, [std_error]},
    ?line ok = application:set_env(os_mon, os_sup_mfa, MFA),
    ?line ok = application:set_env(os_mon, os_sup_enable, true),
    ?line exit(whereis(message_receptor), done),
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    case test_server:os_type() of
	{unix, sunos} -> [message, config, port];
	{win32, _OSname} -> [message];
	OS ->
	    Str = io_lib:format("os_sup not available for ~p",
				[OS]),
	    {skip, lists:flatten(Str)}
    end.

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


message(suite) ->
    [];
message(doc) ->
    ["Test OS message handling"];
message(Config) when is_list(Config) ->

    %% Fake an OS message
    Data = "10H11386278426HSystem4HTest5HError5HTesto",
    ?line os_sup_server ! {faked_port, {data, Data}},

    %% Check with message_receptor that it has been received
    ?t:sleep(?t:seconds(1)),
    Msg =
	case ?t:os_type() of
	    {unix, sunos} ->
		{?TAG, Data};
	    {win32, _} ->
		{?TAG,{{1138,627842,0},"System","Test","Error","Testo"}}
	end,
    ?line message_receptor ! {check, self(), Msg},
    receive
	{result, true} ->
	    ok;
	{result, Rec} ->
	    ?t:fail({no_message, Rec})
    end,

    ok.

config(suite) ->
    [];
config(doc) ->
    ["Test configuration"];
config(Config) when is_list(Config) ->

    %% os_sup_enable==true and os_sup_own/os_sup_syslogconf cannot
    %% be tested as test_server is not running is root

    %% os_sup_mfa is already tested, sort of (in init_per_suite)

    %% os_sup_errortag should be tested, however

    ok.

port(suite) ->
    [];
port(doc) ->
    ["Test that os_sup handles a terminating port program"];
port(Config) when is_list(Config) ->
    ?line Str = os:cmd("ps -e | grep '[f]errule'"),
    case io_lib:fread("~s", Str) of
	 {ok, [Pid], _Rest} ->

	    %% Monitor os_sup_server
	    ?line MonRef = erlang:monitor(process, os_sup_server),

	    %% Kill the port program
	    case os:cmd("kill -9 " ++ Pid) of
		[] ->

		    %% os_sup_server should now terminate
		    receive
			{'DOWN', MonRef, _, _, {port_died, _Reason}} ->
			    ok;
			{'DOWN', MonRef, _, _, Reason} ->
			    ?line ?t:fail({unexpected_exit_reason, Reason})
		    after
			3000 ->
			    ?line ?t:fail(still_alive)
		    end,

		    %% Give os_mon_sup time to restart os_sup
		    ?t:sleep(?t:seconds(3)),
		    ?line true = is_pid(whereis(os_sup_server)),

		    ok;

		Line ->
		    erlang:demonitor(MonRef),
		    {skip, {not_killed, Line}}
	    end;
	_ ->
	    {skip, {os_pid_not_found}}
    end.

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------

test_mfa(Message, Tag) ->
    message_receptor ! {Tag, Message}.

message_receptor() ->
    register(message_receptor, self()),
    message_receptor([]).

message_receptor(Received) ->
    receive
	%% Check if a certain message has been received
	{check, From, Msg} ->
	    case lists:member(Msg, Received) of
		true ->
		    From ! {result, true},
		    message_receptor(lists:delete(Msg, Received));
		false ->
		    From ! {result, Received},
		    message_receptor(Received)
	    end;

	%% Save all other messages
	Msg ->
	    message_receptor([Msg|Received])
    end.
