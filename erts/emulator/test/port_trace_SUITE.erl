%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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


-module(port_trace_SUITE).

-export([all/0, suite/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2]).
-export([port_specs/1, ports/1, open_close/1,
         command/1, control/1, connect/1, call/1,
         output/1, output2/1, output_binary/1,
         outputv/1, set_timer/1, failure_eof/1,
         failure_atom/1, failure_posix/1,
         failure/1, output_term/1,
         driver_output_term/1,
         send_term/1, driver_send_term/1,
         driver_remote_send_term/1]).

-define(ECHO_DRV_NOOP, 0).
-define(ECHO_DRV_OUTPUT, 1).
-define(ECHO_DRV_OUTPUT2, 2).
-define(ECHO_DRV_OUTPUT_BINARY, 3).
-define(ECHO_DRV_OUTPUTV, 4).
-define(ECHO_DRV_SET_TIMER, 5).
-define(ECHO_DRV_FAILURE_EOF, 6).
-define(ECHO_DRV_FAILURE_ATOM, 7).
-define(ECHO_DRV_FAILURE_POSIX, 8).
-define(ECHO_DRV_FAILURE, 9).
-define(ECHO_DRV_OUTPUT_TERM, 10).
-define(ECHO_DRV_DRIVER_OUTPUT_TERM, 11).
-define(ECHO_DRV_SEND_TERM, 12).
-define(ECHO_DRV_DRIVER_SEND_TERM, 13).
-define(ECHO_DRV_SAVE_CALLER, 14).
-define(ECHO_DRV_REMOTE_SEND_TERM, 15).

suite() -> [{ct_hooks,[ts_install_cth]},
            {timetrap, {minutes, 2}}].

all() ->
    [port_specs, ports, open_close,
     command, control, connect, call,
     output, output2, output_binary,
     outputv, set_timer, failure_eof,
     failure_atom, failure_posix,
     failure, output_term,
     driver_output_term,
     send_term, driver_send_term,
     driver_remote_send_term].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    erlang:trace(all, false, [all]),
    os:unsetenv("OUTPUTV"),
    reload_drv(Config),
    Config.

end_per_testcase(_Func, _Config) ->
    erlang:trace(all, false, [all]),
    ok.

%% Test the first argument of trace/3
port_specs(_Config) ->

    S = self(),

    Tracer = fun F() ->
                     receive
                         stop ->
                             ok;
                         M ->
                             S ! M,
                             F()
                     end
             end,

    Test = fun(TraceSpec, Info1, Info2) ->
                   {TracerPid,Ref} = spawn_monitor(Tracer),
                   Prt1 = erlang:open_port({spawn, echo_drv}, [binary]),
                   erlang:trace(TraceSpec, true, ['receive', {tracer, TracerPid}]),
                   %% We disable trace messages from the testcase process
                   erlang:trace(self(), false, ['receive']),
                   Prt2 = erlang:open_port({spawn, echo_drv}, [binary]),

                   InfoCheck =
                       fun(Info, Prt) ->
                               if
                                   Info ->
                                       {tracer, TracerPid} = erlang:trace_info(Prt, tracer),
                                       {flags,['receive']} = erlang:trace_info(Prt, flags);
                                   not Info ->
                                       {tracer,[]} = erlang:trace_info(Prt, tracer),
                                       {flags,[]} = erlang:trace_info(Prt, flags)
                               end
                       end,
                   InfoCheck(Info1, Prt1),
                   InfoCheck(Info2, Prt2),

                   %% These may create trace messages
                   erlang:port_command(Prt1, <<?ECHO_DRV_NOOP>>),
                   erlang:port_command(Prt2, <<?ECHO_DRV_NOOP>>),

                   %% Test what happens when the tracer dies
                   trace_delivered(),
                   TracerPid ! stop,
                   receive {'DOWN', Ref, process, TracerPid, normal} -> ok end,

                   %% These should not generate any trace messages
                   erlang:port_command(Prt1, <<?ECHO_DRV_NOOP>>),
                   erlang:port_command(Prt2, <<?ECHO_DRV_NOOP>>),

                   InfoCheck(false, Prt1),
                   InfoCheck(false, Prt2),

                   erlang:port_close(Prt1),
                   erlang:port_close(Prt2),
                   erlang:trace(all, false, [all]),
                   {Prt1, Prt2}
           end,

    {_Prt11, Prt12} = Test(new, false, true),
    [{trace, Prt12, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt12),

    {_Prt21, Prt22} = Test(new_ports, false, true),
    [{trace, Prt22, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt22),

    {Prt31, _Prt32} = Test(existing, true, false),
    [{trace, Prt31, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt31),

    {Prt41, _Prt42} = Test(existing_ports, true, false),
    [{trace, Prt41, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt41),

    {Prt51, Prt52} = Test(all, true, true),
    [{trace, Prt51, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt51),
    [{trace, Prt52, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt52),

    {Prt61, Prt62} = Test(ports, true, true),
    [{trace, Prt61, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt61),
    [{trace, Prt62, 'receive', {S, {command,<<?ECHO_DRV_NOOP>>}}}]
        = flush(Prt62),

    ok.

%% Test that the 'ports' trace flag works
ports(_Config) ->

    {Prt, S} = trace_and_open([ports],[binary]),

    [{trace, Prt, open, S, echo_drv},
     {trace, Prt, getting_linked, S}] = flush(),

    register(?MODULE, Prt),
    unregister(?MODULE),
    register(?MODULE, Prt),

    [{trace,Prt,register,port_trace_SUITE},
     {trace,Prt,unregister,port_trace_SUITE},
     {trace,Prt,register,port_trace_SUITE}] = flush(),

    unlink(Prt),
    link(Prt),

    [{trace,Prt,getting_unlinked,S},
     {trace,Prt,getting_linked,S}] = flush(),

    erlang:port_close(Prt),

    [{trace,Prt,closed,normal},
     {trace,Prt,unregister,port_trace_SUITE}] = flush(),

    ok.

%% Test that port_close and ! close generate correct trace messages
open_close(_Config) ->

    S = trace_ports([send,'receive']),

    Prt = erlang:open_port({spawn, echo_drv}, [binary]),
    erlang:port_close(Prt),
    [{trace, Prt, 'receive', {S, close}}] = flush(),

    Prt2 = erlang:open_port({spawn, echo_drv}, [binary]),
    Prt2 ! {S, close},
    recv({Prt2, closed}),
    [{trace, Prt2, 'receive', {S, close}},
     {trace, Prt2, send, closed, S}] = flush(),

    catch erlang:port_close(Prt2),
    [] = flush(),

    ok.

%% Test that port_command and ! command generate correct trace messages
command(Config) ->

    Flags = [send,'receive'],
    S = trace_ports(Flags),
    Prt = erlang:open_port({spawn, echo_drv}, [binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_NOOP:8>>),
    [{trace, Prt, 'receive', {S, {command, <<?ECHO_DRV_NOOP:8>>}}}] = flush(),

    erlang:port_command(Prt, [?ECHO_DRV_NOOP, <<0:8>>]),
    [{trace, Prt, 'receive', {S, {command, <<?ECHO_DRV_NOOP:8,0:8>>}}}] = flush(),

    Prt ! {S, {command, <<?ECHO_DRV_NOOP:8>>}},
    [{trace, Prt, 'receive', {S, {command, <<?ECHO_DRV_NOOP:8>>}}}] = flush(),

    OutputMsg = <<?ECHO_DRV_NOOP:8,0:(8*512)>>,
    Prt ! {S, {command, OutputMsg}},
    [{trace, Prt, 'receive', {S, {command, OutputMsg}}}] = flush(),

    close(Prt, Flags),

    os:putenv("OUTPUTV","true"),
    reload_drv(Config),

    Prt2 = erlang:open_port({spawn, echo_drv}, [binary]),
    OutputvMsg = [<<0:8>>,<<0:(8*512)>>,<<0:(8*256)>>,<<0:8>>],

    erlang:port_command(Prt2, OutputvMsg),
    [{trace, Prt2, 'receive', {S, {command, OutputvMsg}}}] = flush(),

    Prt2 ! {S, {command, OutputvMsg}},
    [{trace, Prt2, 'receive', {S, {command, OutputvMsg}}}] = flush(),

    close(Prt2, Flags),

    os:unsetenv("OUTPUTV"),

    ok.

%% Test that port_control generate correct trace messages
control(_Config) ->

    Flags = [send,'receive'],
    {Prt, S} = trace_and_open(Flags,[binary]),

    [0] = erlang:port_control(Prt, 1, <<?ECHO_DRV_NOOP:8, 0:8>>),
    [{trace, Prt, 'receive', {S, {control, {1, <<?ECHO_DRV_NOOP:8, 0:8>>}}}},
     {trace, Prt, send, {Prt, {control, <<0:8>>}}, S}] = flush(),

    [0] = erlang:port_control(Prt, (1 bsl 32) - 1, <<?ECHO_DRV_NOOP:8, 0:8>>),
    [{trace, Prt, 'receive', {S, {control, {(1 bsl 32) - 1, <<?ECHO_DRV_NOOP:8, 0:8>>}}}},
     {trace, Prt, send, {Prt, {control, <<0:8>>}}, S}] = flush(),

    Msg = <<?ECHO_DRV_NOOP:8, 0:(8*512)>>,
    Pat = lists:duplicate(512, 0),
    Pat = erlang:port_control(Prt, 1, Msg),
    [{trace, Prt, 'receive', {S, {control, {1, Msg}}}},
     {trace, Prt, send, {Prt, {control, <<0:(8*512)>>}}, S}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that port_connect and ! connect generate correct trace messages
%% This includes that the proper getting_linked messages are sent
connect(_Config) ->


    {Prt, S} = trace_and_open([send, 'receive', ports],[binary]),

    flush(),

    {Pid,Ref} = spawn_monitor(
                  fun() ->
                          receive
                              go ->
                                  Prt ! {self(), {connect, S}},
                                  receive {Prt, connected} -> unlink(Prt) end
                          end
                  end),
    erlang:trace(Pid, true, [send, 'receive', procs]),

    erlang:port_connect(Prt, Pid),
    unlink(Prt),

    [{trace,Prt,getting_linked,Pid},
     {trace,Prt,'receive',{S,{connect,Pid}}},
     {trace,Prt,send,{Prt,connected},S},
     {trace,Prt,getting_unlinked, S}] = flush(Prt),

    [{trace,Pid,getting_linked,Prt}] = flush(),

    Pid ! go,
    recv({'DOWN',Ref,process,Pid,normal}),

    [{trace,Prt,'receive',{Pid,{connect,S}}},
     {trace,Prt,send,{Prt,connected},Pid},
     {trace,Prt,getting_unlinked,Pid}] = flush(Prt),

    [{trace,Pid,'receive',go},
     {trace,Pid,send,{Pid,{connect,S}}, Prt},
     {trace,Pid,'receive',{Prt,connected}},
     {trace,Pid,unlink,Prt},
     {trace,Pid,exit,normal}] = flush(),

    erlang:port_close(Prt),
    [{trace, Prt, 'receive', {S, close}},
     {trace, Prt, closed, normal}] = flush(),
    ok.

%% Test that port_call generate correct trace messages
call(_Config) ->

    Flags = [send,'receive'],
    {Prt, S} = trace_and_open(Flags,[binary]),

    Test = fun(Msg) ->
                   BinMsg = term_to_binary(Msg),

                   Msg = erlang:port_call(Prt, 0, Msg),
                   [{trace, Prt, 'receive', {S, {call, {0, BinMsg}}}},
                    {trace, Prt, send, {Prt, {call, BinMsg}}, S}] = flush()
           end,

    Test({hello, world, make_ref()}),
    Test({hello, world, lists:seq(1,1000)}),

    close(Prt, Flags),

    ok.

%% Test that driver_output generate correct trace messages
output(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_OUTPUT, 123456:32>>),
    recv({Prt,{data,<<123456:32>>}}),

    [{trace, Prt, send, {Prt, {data, <<123456:32>>}}, S}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_output2 generate correct trace messages
output2(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_OUTPUT2, 123456:32>>),
    recv({Prt,{data,[$a|<<123456:32>>]}}),
    [{trace, Prt, send, {Prt, {data, [$a|<<123456:32>>]}}, S}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_output_binary generate correct trace messages
output_binary(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_OUTPUT_BINARY, 0, 123456:32>>),
    recv({Prt,{data,[$a|<<123456:32>>]}}),
    [{trace, Prt, send, {Prt, {data, [$a|<<123456:32>>]}}, S}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_outputv generate correct trace messages
outputv(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_OUTPUTV, 123456:32>>),
    recv({Prt,{data,[$a|<<123456:32>>]}}),

    [{trace, Prt, send, {Prt, {data, [$a|<<123456:32>>]}}, S}] = flush(),

    erlang:port_close(Prt),
    [] = flush(),

    ok.

%% Test that driver_set_timer generate correct trace messages
set_timer(_Config) ->

    Flags = [send,'receive'],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_SET_TIMER>>),
    timer:sleep(100),
    [{trace, Prt, 'receive', {S, {command, <<?ECHO_DRV_SET_TIMER>>}}},
     {trace, Prt, 'receive', timeout}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_failure* generate correct trace messages
failure_eof(_Config) ->

    Flags = [send,'receive', ports],
    S = trace_ports(Flags),

    Prt = erlang:open_port({spawn, echo_drv}, [eof, binary]),
    [{trace, Prt, open, S, echo_drv},
     {trace, Prt, getting_linked, S}] = flush(),

    erlang:port_command(Prt, <<?ECHO_DRV_FAILURE_EOF>>),
    recv({Prt,eof}),
    [{trace, Prt, 'receive', {S, {command, <<?ECHO_DRV_FAILURE_EOF>>}}},
     {trace, Prt, send, {Prt, eof}, S}] = flush(),

    close(Prt, Flags),

    %% Run same test without eof option
    failure_test(<<?ECHO_DRV_FAILURE_EOF>>, normal).

failure_atom(_Config) ->
    failure_test(<<?ECHO_DRV_FAILURE_ATOM, "failure\0">>, failure).
failure_posix(_Config) ->
    failure_test(<<?ECHO_DRV_FAILURE_POSIX>>, eagain).
failure(_Config) ->
    failure_test(<<?ECHO_DRV_FAILURE, 1>>, 1).

failure_test(Failure, Reason) ->

    {Prt, S} = trace_and_open([send, 'receive', ports],[binary]),

    [{trace, Prt, open, S, echo_drv},
     {trace, Prt, getting_linked, S}] = flush(),

    process_flag(trap_exit, true),
    erlang:port_command(Prt, Failure),
    try
        recv({'EXIT',Prt,Reason})
    after
        process_flag(trap_exit, false)
    end,
    [{trace, Prt, 'receive', {S, {command, Failure}}},
     {trace, Prt, closed, Reason}] = flush(),

    ok.

%% Test that erl_drv_output_term generate correct trace messages
output_term(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_OUTPUT_TERM, 123456:32>>),
    recv({echo, Prt, <<123456:32>>}),
    [{trace, Prt, send, {echo, Prt, <<123456:32>>}, S}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_output_term generate correct trace messages
driver_output_term(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_DRIVER_OUTPUT_TERM, 123456:32>>),
    recv({echo, Prt, <<123456:32>>}),
    [{trace, Prt, send, {echo, Prt, <<123456:32>>}, S}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that erl_drv_send_term generate correct trace messages
send_term(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_SEND_TERM, 123456:32>>),
    recv({echo, Prt, <<123456:32>>}),
    [{trace, Prt, send, {echo, Prt, <<123456:32>>}, S}] = flush(),

    {Pid, Ref} = spawn_monitor(fun() -> erlang:port_command(Prt, <<?ECHO_DRV_SAVE_CALLER>>) end),
    recv({'DOWN',Ref,process,Pid,normal}),
    erlang:port_command(Prt, <<?ECHO_DRV_SEND_TERM, 123456:32>>),
    [{trace, Prt, send_to_non_existing_process, {echo, Prt, <<123456:32>>}, Pid}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_send_term generate correct trace messages
driver_send_term(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_DRIVER_SEND_TERM, 123456:32>>),
    recv({echo, Prt, <<123456:32>>}),
    [{trace, Prt, send, {echo, Prt, <<123456:32>>}, S}] = flush(),

    {Pid, Ref} = spawn_monitor(fun() -> erlang:port_command(Prt, <<?ECHO_DRV_SAVE_CALLER>>) end),
    recv({'DOWN',Ref,process,Pid,normal}),
    erlang:port_command(Prt, <<?ECHO_DRV_SEND_TERM, 123456:32>>),
    [{trace, Prt, send_to_non_existing_process, {echo, Prt, <<123456:32>>}, Pid}] = flush(),

    close(Prt, Flags),

    ok.

%% Test that driver_send_term from non-scheduler thread does not
%% generate trace messages.
driver_remote_send_term(_Config) ->

    Flags = [send],
    {Prt, S} = trace_and_open(Flags,[binary]),

    erlang:port_command(Prt, <<?ECHO_DRV_REMOTE_SEND_TERM, 123456:32>>),
    recv({echo, Prt, <<123456:32>>}),
    [] = flush(),

    Pid = spawn_link(
            fun() ->
                    erlang:port_command(Prt, <<?ECHO_DRV_SAVE_CALLER>>),
                    S ! ok,
                    receive M -> S ! M end
            end),
    recv(ok),
    erlang:trace(Pid, true, ['receive']),

    erlang:port_command(Prt, <<?ECHO_DRV_REMOTE_SEND_TERM, 123456:32>>),
    recv({echo, Prt, <<123456:32>>}),
    [{trace, Pid, 'receive', {echo, Prt, <<123456:32>>}}] = flush(),

    close(Prt, Flags),

    ok.

%%%%%%%%%%%%%%%%%%%
%% Helper functions
%%%%%%%%%%%%%%%%%%%

trace_ports(TraceFlags) ->
    erlang:trace(new_ports, true, TraceFlags),
    self().

trace_and_open(TraceFlags, OpenFlags) ->
    S = self(),
    Ports = proplists:get_value(ports, TraceFlags),
    [trace_ports(TraceFlags) || Ports],
    Prt = erlang:open_port({spawn, echo_drv}, OpenFlags),
    [erlang:trace(Prt, true, TraceFlags) || Ports == undefined],
    {Prt, S}.

close(Prt, Flags) ->
    Recv = proplists:get_value('receive', Flags),
    Ports = proplists:get_value(ports, Flags),
    S = self(),

    erlang:port_close(Prt),

    if Recv, Ports ->
            [{trace, Prt, 'receive', {S, close}},
             {trace, Prt, closed, normal}] = flush();
       Recv ->
            [{trace, Prt, 'receive', {S, close}}] = flush();
       Ports ->
            [{trace, Prt, closed, normal}] = flush();
       true ->
            [] = flush()
    end.

trace_delivered() ->
    Ref = erlang:trace_delivered(all),
    receive {trace_delivered, all, Ref} -> ok end.

flush() ->
    flush(all).
flush(From) ->
    trace_delivered(),
    f(From).

f(From) ->
    receive
        M when From =:= all; element(2, M) == From ->
            [M | f(From)]
    after 0 ->
            []
    end.

recv(Msg) ->
    receive Msg -> ok after 1000 -> ct:fail({did_not_get_data,Msg,flush()}) end.

load_drv(Config) ->
    Path = proplists:get_value(data_dir, Config),
    case erl_ddll:load_driver(Path, echo_drv) of
        ok -> ok;
        {error, Error} = Res ->
            io:format("~s\n", [erl_ddll:format_error(Error)]),
	    ct:fail(Res)
    end.

reload_drv(Config) ->
    erl_ddll:unload_driver(echo_drv),
    load_drv(Config).
