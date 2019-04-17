%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-module(ssh_chan_behaviours_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("ssh/src/ssh.hrl").
-include("ssh_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,60}}].

all() -> 
    [
     noexist_subsystem,
     undefined_subsystem,
     defined_subsystem,
     subsystem_client
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ?CHECK_CRYPTO(
       begin
           ssh:start(),
           Config
       end).

end_per_suite(_Config) ->
    {Time,R} = timer:tc(ssh, stop, []),
    ct:log("Stop ssh: ~p ms",[(100*(Time div 1000)) / 100]),
    R.

init_per_testcase(_TC, Config) ->
    SubSystems = [
                  {"bad_cb", {ssh_chan_behaviours_undefined, []}}, % A non-existing file
                  {"ch1",    {ssh_chan_behaviours_server,    [self(),true]}}
                 ],
    {Pid, Host, Port} = ssh_test_lib:std_daemon(Config, [{subsystems,SubSystems}]),
    C = ssh_test_lib:std_connect(Config, Host, Port, []),
    [{connref,C}, {daemon_pid,Pid}| Config].

end_per_testcase(_TC, Config) ->
    {Time,_} = timer:tc(ssh, stop_daemon, [proplists:get_value(daemon_pid,Config)]),
    ct:log("Stop daemon: ~p ms",[(100*(Time div 1000)) / 100]),
    case flush() of
        [] -> ok;
        Msgs -> ct:pal("Unhandled messages:~n~p", [Msgs])
    end.
    

-define(EXPECT(What, Bind),
        Bind =
            (fun() ->
                     receive What ->
                             ct:log("~p:~p ~p got ~p",[?MODULE,?LINE,self(),What]),
                             Bind
                     after 5000 ->
                             ct:log("~p:~p ~p Flushed:~n~p",[?MODULE,?LINE,self(),flush()]),
                             ct:fail("Timeout!",[])
                     end
             end)()
        ).

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
%% Try start a subsystem whos name is not known by the server
noexist_subsystem(Config) ->
    C = proplists:get_value(connref, Config),
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    failure = ssh_connection:subsystem(C, Ch, "noexist", infinity),
    ok = ssh_connection:close(C, Ch),
    ?EXPECT({ssh_cm,C,{closed,Ch}},[]),
    ok.

%% Try to start a subsystem with a known name, but without any callback file
undefined_subsystem(Config) ->
    C = proplists:get_value(connref, Config),
    {ok, Ch} = ssh_connection:session_channel(C, infinity),
    failure = ssh_connection:subsystem(C, Ch, "bad_cb", infinity),
    ok = ssh_connection:close(C, Ch),
    ?EXPECT({ssh_cm,C,{closed,Ch}},[]), % self() is instead of a proper channel handler
    ok.

%% Try to start and stop a subsystem with known name and defined callback file
defined_subsystem(Config) ->
    C = proplists:get_value(connref, Config),
    {ok, Ch1} = ssh_connection:session_channel(C, infinity),

    success = ssh_connection:subsystem(C, Ch1, "ch1", infinity),
    IDsrv = ?EXPECT({{_Csrv,_Ch1srv}, {ssh_channel_up,_Ch1srv,_Csrv}}, {_Csrv,_Ch1srv}),

    ok = ssh_connection:close(C, Ch1),
    ?EXPECT({IDsrv, {terminate,normal}}, []),
    ?EXPECT({ssh_cm, C, {closed,Ch1}}, []), % self() is instead of a proper channel handler
    ok.

%% Try to start and stop a subsystem from a ssh_client_channel behviour
subsystem_client(Config) ->
    C = proplists:get_value(connref, Config),

    {ok,ChRef} = ssh_chan_behaviours_client:start_link(C),
    IDclt = ?EXPECT({{C,_Ch1clt},     {ssh_channel_up,_Ch1clt,C}},     {C,_Ch1clt}),
    IDsrv = ?EXPECT({{_Csrv,_Ch1srv}, {ssh_channel_up,_Ch1srv,_Csrv}}, {_Csrv,_Ch1srv}),

    ok = ssh_chan_behaviours_client:stop(ChRef),
    ?EXPECT({IDclt, {terminate,normal}}, []), % From the proper channel handler
    ?EXPECT({IDsrv, {terminate,normal}}, []),
    ok.

%%%================================================================
%%%
%%%

flush() -> lists:reverse(flush([])).

flush(Acc) ->
    receive
        M ->
            flush([M|Acc])
    after 0 ->
            Acc
    end.
            
