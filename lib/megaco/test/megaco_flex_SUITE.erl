%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Test various aspects of the flex scanner handling
%%
%% Test:    ts:run(megaco, megaco_flex_SUITE, [batch]).
%%
%%----------------------------------------------------------------------

-module(megaco_flex_SUITE).

-include("megaco_test_lib.hrl").

-export([
         suite/0, all/0, groups/0,
	 init_per_suite/1,    end_per_suite/1, 
         init_per_group/2,    end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,

	 plain/1,
	 port_exit/1,
	 garbage_in/1

	]).


%%======================================================================
%% Common Test interface functions
%%======================================================================

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     plain,
     port_exit,
     garbage_in
    ].


groups() -> 
    [].



%%
%% -----
%%

init_per_suite(suite) ->
    [];
init_per_suite(doc) ->
    [];
init_per_suite(Config0) when is_list(Config0) ->

    ?ANNOUNCE_SUITE_INIT(),

    p("init_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->

            case megaco_flex_scanner:is_enabled() of
                true ->

                    %% We need a (local) monitor on this node also
                    megaco_test_sys_monitor:start(),

                    p("init_per_suite -> end when"
                      "~n      Config: ~p"
                      "~n      Nodes:  ~p", [Config1, erlang:nodes()]),
                    
                    Config1;
                false ->
                    ?SKIP(flex_scanner_not_enabled)
            end
    end.

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config0) when is_list(Config0) ->

    p("end_per_suite -> entry with"
      "~n      Config: ~p"
      "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    megaco_test_sys_monitor:stop(),
    Config1 = ?LIB:end_per_suite(Config0),

    p("end_per_suite -> end when"
      "~n      Nodes:  ~p", [erlang:nodes()]),

    Config1.


%%
%% -----
%%

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%
%% -----
%%

init_per_testcase(Case, Config) ->

    p("init_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),
    
    megaco_test_global_sys_monitor:reset_events(),

    megaco_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->

    p("end_per_testcase -> entry with"
      "~n   Config: ~p"
      "~n   Nodes:  ~p", [Config, erlang:nodes()]),

    p("system events during test: "
      "~n   ~p", [megaco_test_global_sys_monitor:events()]),

    megaco_test_lib:end_per_testcase(Case, Config).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

plain(suite) ->
    [];
plain(doc) ->
    ["This is to simply test that it is possible to start and stop the "
     "flex handler."];
plain(Config) when is_list(Config) ->
    put(tc, plain), 
    p("begin"),
    process_flag(trap_exit, true),
    p("start the flex handler"),
    {ok, Pid, _PortInfo} = flex_scanner_handler_start(), 
    p("stop handler"),
    flex_scanner_handler_stop(Pid),
    p("end"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

port_exit(suite) ->
    [];
port_exit(doc) ->
    ["Test that the handler detects and handles an exiting port."];
port_exit(Config) when is_list(Config) ->
    put(tc, port_exit), 
    p("begin"),
    process_flag(trap_exit, true),

    p("start the flex handler"),
    {ok, Pid, {flex, PortOrPorts}} = flex_scanner_handler_start(), 
    Port = case PortOrPorts of
	       P when is_port(P) ->
		   P;
	       Ports when is_tuple(Ports) ->
		   %% It does not matter which of the ports we choose
		   element(1, PortOrPorts);
	       Ports when is_list(Ports) ->
		   %% It does not matter which of the ports we choose
		   hd(Ports)
	   end,

    p("simulate crash"),
    exit(Port, simulated_crash), 
    
    p("await handler exit"),
    receive
	{'EXIT', Pid, _} ->
	    p("end"),
	    ok
    after 5000 ->
	    p("timeout - stop handler"),
	    flex_scanner_handler_stop(Pid),
	    p("end after timeout"),
	    {error, timeout}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

garbage_in(suite) ->
    [];
garbage_in(doc) ->
    ["Send in various unexpected messages and requeststo the handler "
     "to see that it does die on us. "];
garbage_in(Config) when is_list(Config) ->
    put(tc, garbage_in), 
    p("begin"),
    process_flag(trap_exit, true),

    p("start the flex handler"),
    {ok, Pid, _PortInfo} = flex_scanner_handler_start(), 

    p("make an invalid call"),
    {error, _} = gen_server:call(Pid, garbage_request), 
    p("make an invalid cast"),
    gen_server:cast(Pid, garbage_msg),
    p("send an unknown message"),
    Pid ! garbage_info,

    p("wait for any garbage response"),
    receive
	Any ->
	    p("end with unexpected message: ~p", [Any]),
	    {error, {unexpected_msg, Any}}
    after 1000 ->
	    p("end with nothing received - stop handler"),
	    flex_scanner_handler_stop(Pid),
	    ok
    end.



%% ------- Misc functions --------

flex_scanner_handler_start() ->
    case megaco_flex_scanner_handler:start_link() of
	{error, {failed_starting_scanner, {error, {load_driver, _}}}} ->
	    p("failed loading driver"),
	    ?SKIP(could_not_load_driver);
	{error, {failed_starting_scanner, {load_driver, _}}} ->
	    p("failed loading driver"),
	    ?SKIP(could_not_load_driver);
	{error, {failed_starting_scanner, {load_driver, _}, _}} ->
	    p("failed loading driver"),
	    ?SKIP(could_not_load_driver);
	Else ->
	    p("driver load result: ~p", [Else]),
	    Else
    end.

flex_scanner_handler_stop(Pid) ->
    megaco_flex_scanner_handler:stop(Pid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(F) ->
    p(F, []).

p(F, A) ->
    case get(tc) of
        undefined ->
            io:format("*** [~s] ~p ***"
                      "~n" ++ F ++ "~n", [?FTS(), self() | A]);
        TC ->
            io:format("*** [~s] ~p ~w ***"
                      "~n   " ++ F ++ "~n",
                      [?FTS(), self(), TC | A])
    end.

