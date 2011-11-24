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
-module(alarm_handler_SUITE).

-include_lib("test_server/include/test_server.hrl").

%%-----------------------------------------------------------------
%% We will add an own alarm handler in order to verify that the
%% alarm_handler deliver the expected events.
%%-----------------------------------------------------------------

-export([init_per_suite/1, end_per_suite/1, all/0,groups/0,
	 init_per_group/2,end_per_group/2,
	 set_alarm/1, clear_alarm/1, swap/1]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).


init_per_suite(Config) ->
    application:start(sasl),
    Config.

end_per_suite(_Config) ->
    ok.

all() -> 
    [set_alarm, clear_alarm, swap].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%-----------------------------------------------------------------

set_alarm(suite) -> [];
set_alarm(Config) when is_list(Config) ->
    ?line gen_event:add_handler(alarm_handler, ?MODULE, self()),
    Alarm1 = {alarm1, "this is the alarm"},
    Alarm2 = {"alarm2", this_is_the_alarm},
    Alarm3 = {{alarm3}, {this_is,"the_alarm"}},
    ?line ok = alarm_handler:set_alarm(Alarm1),
    reported(set_alarm, Alarm1),
    ?line ok = alarm_handler:set_alarm(Alarm2),
    reported(set_alarm, Alarm2),
    ?line ok = alarm_handler:set_alarm(Alarm3),
    reported(set_alarm, Alarm3),

    ?line [Alarm3,Alarm2,Alarm1] = alarm_handler:get_alarms(),
    alarm_handler:clear_alarm(alarm1),
    alarm_handler:clear_alarm("alarm2"),
    alarm_handler:clear_alarm({alarm3}),
    ?line [] = alarm_handler:get_alarms(),

    test_server:messages_get(),
    ?line my_yes = gen_event:delete_handler(alarm_handler, ?MODULE, []),
    ok.

%%-----------------------------------------------------------------

clear_alarm(suite) -> [];
clear_alarm(Config) when is_list(Config) ->
    ?line gen_event:add_handler(alarm_handler, ?MODULE, self()),
    Alarm1 = {alarm1, "this is the alarm"},
    Alarm2 = {"alarm2", this_is_the_alarm},
    Alarm3 = {{alarm3}, {this_is,"the_alarm"}},
    alarm_handler:set_alarm(Alarm1),
    alarm_handler:set_alarm(Alarm2),
    alarm_handler:set_alarm(Alarm3),
    test_server:messages_get(),

    ?line ok = alarm_handler:clear_alarm(alarm1),
    reported(clear_alarm, alarm1),
    ?line ok = alarm_handler:clear_alarm("alarm2"),
    reported(clear_alarm, "alarm2"),
    ?line ok = alarm_handler:clear_alarm({alarm3}),
    reported(clear_alarm, {alarm3}),
    ?line [] = alarm_handler:get_alarms(),

    ?line my_yes = gen_event:delete_handler(alarm_handler, ?MODULE, []),
    ok.

%%-----------------------------------------------------------------

swap(suite) -> [];
swap(Config) when is_list(Config) ->
    ?line Alarm1 = {alarm1, "this is the alarm"},
    ?line Alarm2 = {"alarm2", this_is_the_alarm},
    ?line Alarm3 = {{alarm3}, {this_is,"the_alarm"}},
    ?line alarm_handler:set_alarm(Alarm1),
    ?line alarm_handler:set_alarm(Alarm2),
    ?line alarm_handler:set_alarm(Alarm3),

    ?line foo,
    case gen_event:which_handlers(alarm_handler) of
	[alarm_handler] ->
	    ?line ok = gen_event:swap_handler(alarm_handler,
					      {alarm_handler, swap},
					      {?MODULE, self()}),
	    ?line [?MODULE] = gen_event:which_handlers(alarm_handler),
	    Alarms = [Alarm3, Alarm2, Alarm1],
	    reported(swap_alarms, Alarms),

	    %% get_alarms is only valid with the default handler installed.
	    ?line {error, _} = alarm_handler:get_alarms(),

	    ?line my_yes = gen_event:delete_handler(alarm_handler,
						    ?MODULE, []),
	    ?line gen_event:add_handler(alarm_handler, alarm_handler, []),
	    ok;
	_ ->
	    alarm_handler:clear_alarm(alarm1),
            alarm_handler:clear_alarm("alarm2"),
	    alarm_handler:clear_alarm({alarm3}),
	    ok
    end.

%%-----------------------------------------------------------------
%% Check that the alarm has been received.
%%-----------------------------------------------------------------
reported(Tag, Data) ->
    receive
	{Tag, Data} ->
	    test_server:messages_get(),
	    ok
    after 1000 ->
	    test_server:fail(no_alarm_received)
    end.

%%-----------------------------------------------------------------
%% The error_logger handler (gen_event behaviour).
%% Sends a notification to the Tester process about the events
%% generated by the Tester process.
%%-----------------------------------------------------------------
init(Tester) when is_pid(Tester) ->
    {ok, Tester};
init({Tester, {alarm_handler,Alarms}}) -> % Swap from default handler.
    Tester ! {swap_alarms, Alarms},
    {ok, Tester}.

handle_event({set_alarm, Alarm}, Tester) ->
    Tester ! {set_alarm, Alarm},
    {ok, Tester};
handle_event({clear_alarm, AlarmId}, Tester) ->
    Tester ! {clear_alarm, AlarmId},
    {ok, Tester};
handle_event(_Event, Tester) ->
    {ok, Tester}.

handle_info(_, Tester) ->
    {ok, Tester}.

handle_call(_Query, Tester) -> {ok, {error, bad_query}, Tester}.

terminate(_Reason, _Tester) ->
    my_yes.
