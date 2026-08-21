%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2026. All Rights Reserved.
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

-module(tftp_SUITE).

-compile([export_all, nowarn_export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Includes and defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("common_test/include/ct.hrl").
-include("tftp_test_lib.hrl").

-define(START_DAEMON(Options),
        begin
            {{ok, Pid}} = ?TRY(tftp:start([{port, 0} | Options])),
            {{ok, ActualOptions}} = ?TRY(tftp:info(Pid)),
            {value, {port, ActualPort}} =
                lists:keysearch(port, 1, ActualOptions),
            {ActualPort, Pid}
        end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(Case, Config) ->
    io:format("\n ", []),
    ?TRY(application:stop(tftp)),
    Config.

end_per_testcase(Case, Config) when is_list(Config) ->
    ?TRY(application:stop(tftp)),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     simple,
     root_dir,
     extra,
     reuse_connection,
     resend_client,
     resend_server,
     large_file,
     app,
     appup
    ].

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


app() ->
    [{doc, "Test that the tftp app file is ok"}].
app(Config) when is_list(Config) ->
    ok = test_server:app_test(tftp).

%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the tftp appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = test_server:appup_test(tftp).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple(doc) ->
    ["Start the daemon and perform simple a read and write."];
simple(suite) ->
    [];
simple(Config) when is_list(Config) ->
    {ok} = ?TRY(application:start(tftp)),

    {{Port, DaemonPid}} = ?TRY(?START_DAEMON([{debug, brief}])),

    %% Read fail
    RemoteFilename = "tftp_temporary_remote_test_file.txt",
    LocalFilename = "tftp_temporary_local_test_file.txt",
    Blob = list_to_binary(lists:duplicate(2000, $1)),
    %% Blob = <<"Some file contents\n">>,
    Size = size(Blob),
    ?TRY(file:delete(RemoteFilename)),
    {{error, {client_open, enoent, _}}} =
            ?TRY(tftp:read_file(RemoteFilename, binary, [{port, Port}])),

    %% Write and read
    {{ok, Size}} = ?TRY(tftp:write_file(RemoteFilename, Blob, [{port, Port}])),
    {{ok, Blob}} = ?TRY(tftp:read_file(RemoteFilename, binary, [{port, Port}])),
    ?TRY(file:delete(LocalFilename)),
    {{ok, Size}} = ?TRY(tftp:read_file(RemoteFilename, LocalFilename, [{port, Port}])),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    {ok} = ?TRY(file:delete(LocalFilename)),
    {ok} = ?TRY(file:delete(RemoteFilename)),
    {ok} = ?TRY(application:stop(tftp)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% root_dir
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

root_dir(doc) ->
    ["Start the daemon and check the root_dir option."];
root_dir(suite) ->
    [];
root_dir(Config) when is_list(Config) ->
    {ok} = ?TRY(application:start(tftp)),
    PrivDir = get_conf(priv_dir, Config),
    Root    = hd(filename:split(PrivDir)),
    Up      = "..",
    Remote  = "remote.txt",
    Local   = "tftp_temporary_local_test_file.txt",
    SideDir = fn_jn(PrivDir,tftp_side),
    RootDir = fn_jn(PrivDir,tftp_root),
    ?TRY(file:del_dir_r(RootDir)),
    ?TRY(file:del_dir_r(SideDir)),
    ok = filelib:ensure_path(fn_jn(RootDir,sub)),
    ok = filelib:ensure_path(SideDir),
    Blob = binary:copy(<<$1>>, 2000),
    Size = byte_size(Blob),
    ok = file:write_file(fn_jn(SideDir,Remote), Blob),
    {{Port, DaemonPid}} =
        ?TRY(?START_DAEMON([{debug, brief},
                            {callback,
                             {"", tftp_file, [{root_dir, RootDir}]}}])),
    try
        %% Outside root_dir
        {{error, {client_open, badop, _}}} =
            ?TRY(tftp:read_file(
                   fn_jn([Up,tftp_side,Remote]), binary, [{port, Port}])),
        {{error, {client_open, badop, _}}} =
            ?TRY(tftp:write_file(
                   fn_jn([Up,tftp_side,Remote]), Blob, [{port, Port}])),
        %% Nonexistent
        {{error, {client_open, enoent, _}}} =
            ?TRY(tftp:read_file(
                   fn_jn(sub,Remote), binary, [{port, Port}])),
        {{error, {client_open, enoent, _}}} =
            ?TRY(tftp:write_file(
                   fn_jn(nonexistent,Remote), Blob, [{port, Port}])),
        %% Write and read
        {{ok, Size}} =
            ?TRY(tftp:write_file(
                   fn_jn(sub,Remote), Blob, [{port, Port}])),
        {{ok, Blob}} =
            ?TRY(tftp:read_file(
                   fn_jn([Root,sub,Remote]), binary, [{port, Port}])),
        {{ok, Size}} =
            ?TRY(tftp:read_file(
                   fn_jn(sub,Remote), Local, [{port, Port}])),
        {{ok, Blob}} = ?TRY(file:read_file(Local)),
        {ok} = ?TRY(file:delete(Local)),
        {ok} = ?TRY(application:stop(tftp))
    after
        %% Cleanup
        unlink(DaemonPid),
        exit(DaemonPid, kill),
        ?TRY(file:del_dir_r(SideDir)),
        ?TRY(file:del_dir_r(RootDir)),
        ?TRY(application:stop(tftp))
    end,
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extra
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extra(doc) ->
    ["Verify new stuff for IS 1.2."];
extra(suite) ->
    [];
extra(Config) when is_list(Config) ->
    {'EXIT', {badarg,{fake_key, fake_flag}}} =
        ?TRY(tftp:start([{port, 0}, {fake_key, fake_flag}])),

    {{Port, DaemonPid}} = ?TRY(?START_DAEMON([{debug, brief}])),

    RemoteFilename = "tftp_extra_temporary_remote_test_file.txt",
    LocalFilename = "tftp_extra_temporary_local_test_file.txt",
    Blob = <<"Some file contents\n">>,
    Size = size(Blob),
    Host = "127.0.0.1",
    Peer = {inet, Host, Port},
    Generic =
        [
         {state,   []},
         {prepare, fun extra_prepare/6},
         {open,    fun extra_open/6},
         {read,    fun extra_read/1},
         {write,   fun extra_write/2},
         {abort,   fun extra_abort/3 }
        ],
    Options = [{host, Host},
               {port, Port},
               %%{ debug,all},
               {callback, {".*", tftp_test_lib, Generic}}],
    {ok} = ?TRY(file:write_file(LocalFilename, Blob)),
    {{ok, [{count, Size}, Peer]}} =
        ?TRY(tftp:write_file(RemoteFilename, LocalFilename, Options)),
    {ok} = ?TRY(file:delete(LocalFilename)),

    {{ok,[{bin, Blob}, Peer]}} =
        ?TRY(tftp:read_file(RemoteFilename, LocalFilename, Options)),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    {ok} = ?TRY(file:delete(LocalFilename)),
    {ok} = ?TRY(file:delete(RemoteFilename)),
    ok.

-record(extra_state,  {file, blksize, count, acc, peer}).

%%-------------------------------------------------------------------
%% Prepare
%%-------------------------------------------------------------------

extra_prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, []) ->
    %% Client side
    BlkSize = list_to_integer(tftp_test_lib:lookup_option("blksize", "512", SuggestedOptions)),
    State = #extra_state{blksize = BlkSize, peer = Peer},
    extra_open(Peer, Access, LocalFilename, Mode, SuggestedOptions, State),
    {ok, SuggestedOptions, State};
extra_prepare(_Peer, _Access, _Bin, _Mode, _SuggestedOptions, _Initial) ->
    {error, {undef, "Illegal callback options."}}.

%%-------------------------------------------------------------------
%% Open
%%-------------------------------------------------------------------

extra_open(Peer, Access, LocalFilename, Mode, SuggestedOptions, []) ->
    %% Server side
    case extra_prepare(Peer, Access, LocalFilename, Mode, SuggestedOptions, []) of
        {ok, AcceptedOptions, []} ->
            BlkSize = list_to_integer(tftp_test_lib:lookup_option("blksize", "512", AcceptedOptions)),
            State = #extra_state{blksize = BlkSize, peer = Peer},
            extra_open(Peer, Access, LocalFilename, Mode, AcceptedOptions, State);
        {error, {Code, Text}} ->
            {error, {Code, Text}}
    end;
extra_open(_Peer, Access, LocalFilename, _Mode, NegotiatedOptions, #extra_state{} = State) ->
    {File, Acc} =
        case Access of
            read ->
                if
                    is_binary(LocalFilename) ->
                        {undefined, LocalFilename};
                    is_list(LocalFilename) ->
                        {ok, Bin} = file:read_file(LocalFilename),
                        {LocalFilename, Bin}
            end;
            write ->
                {LocalFilename, []}
        end,
    %% Both sides
    State2 = State#extra_state{file = File, acc = Acc, count = 0},
    {ok, NegotiatedOptions, State2}.

%%-------------------------------------------------------------------
%% Read
%%-------------------------------------------------------------------

extra_read(#extra_state{acc = Bin} = State) when is_binary(Bin) ->
    BlkSize = State#extra_state.blksize,
    Count = State#extra_state.count + size(Bin),
    if
        size(Bin) >= BlkSize ->
            <<Block:BlkSize/binary, Bin2/binary>> = Bin,
            State2 = State#extra_state{acc = Bin2, count = Count},
            {more, Block, State2};
        size(Bin) < BlkSize ->
            Res = [{count, Count}, State#extra_state.peer],
            {last, Bin, Res}
    end.

%%-------------------------------------------------------------------
%% Write
%%-------------------------------------------------------------------

extra_write(Bin, #extra_state{acc = List} = State) when is_binary(Bin), is_list(List) ->
    Size = size(Bin),
    BlkSize = State#extra_state.blksize,
    if
        Size == BlkSize ->
            {more, State#extra_state{acc = [Bin | List]}};
        Size < BlkSize ->
            Bin2 = list_to_binary(lists:reverse([Bin | List])),
            Res = [{bin,  Bin2}, State#extra_state.peer],
            file:write_file(State#extra_state.file, Bin2),
            {last, Res}
    end.

%%-------------------------------------------------------------------
%% Abort
%%-------------------------------------------------------------------

extra_abort(_Code, _Text, #extra_state{}) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Re-send client
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resend_client(doc) ->
    ["Verify that the server behaves correctly when the client re-sends packets."];
resend_client(suite) ->
    [];
resend_client(Config) when is_list(Config) ->
    Host = {127, 0, 0, 1},
    {{Port, DaemonPid}} = ?TRY(?START_DAEMON([{debug, all}])),

    try

    ok = resend_read_client(Host, Port, 10),
    ok = resend_read_client(Host, Port, 512),
    ok = resend_read_client(Host, Port, 1025),

    ok = resend_write_client(Host, Port, 10),
    ok = resend_write_client(Host, Port, 512),
    ok = resend_write_client(Host, Port, 1025)

    after
        %% Cleanup
        unlink(DaemonPid),
        exit(DaemonPid, kill)
    end,
    ok.

resend_read_client(Host, Port, BlkSize) ->
    RemoteFilename = "tftp_resend_read_client.tmp",
    Block1 = lists:duplicate(BlkSize, $1),
    Block2 = lists:duplicate(BlkSize, $2),
    Block3 = lists:duplicate(BlkSize, $3),
    Block4 = lists:duplicate(BlkSize, $4),
    Block5 = lists:duplicate(BlkSize, $5),
    Blocks = [Block1, Block2, Block3, Block4, Block5],
    Blob = list_to_binary(Blocks),
    {ok} = ?TRY(file:write_file(RemoteFilename, Blob)),

    Timeout = timer:seconds(3),
    {timeout} = ?TRY(recv(0)),

    %% Open socket
    {{ok, Socket}} = ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),

    ReadList = [0, 1, RemoteFilename, 0, "octet", 0],
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    NewPort =
        if
            BlkSize =:= 512 ->
                %% Send READ
                ReadBin = list_to_binary(ReadList),
                {ok} = ?TRY(gen_udp:send(Socket, Host, Port, ReadBin)),

                %% Sleep a while in order to provoke the server to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                {{udp, Socket, Host, NewPort0, Data1Bin}} = ?TRY(recv(Timeout)),
                NewPort0;
            true ->
                %% Send READ
                BlkSizeList = integer_to_list(BlkSize),
                Options = ["blksize", 0, BlkSizeList, 0],
                ReadBin = list_to_binary([ReadList | Options]),
                {ok} = ?TRY(gen_udp:send(Socket, Host, Port, ReadBin)),

                %% Recv OACK
                OptionAckBin = list_to_binary([0, 6 | Options]),
                {{udp, Socket, Host, NewPort0, OptionAckBin}} =
                    ?TRY(recv(Timeout)),

                %% Send ACK #0
                Ack0Bin = <<0, 4, 0, 0>>,
                {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort0, Ack0Bin)),

                %% Send ACK #0 AGAIN (pretend that we timed out)
                timer:sleep(timer:seconds(1)),
                {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort0, Ack0Bin)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                {{udp, Socket, Host, NewPort0, Data1Bin}} = ?TRY(recv(Timeout)),
                NewPort0
        end,

    %% Recv DATA #1 AGAIN (the re-sent package)
    {{udp, Socket, Host, NewPort, Data1Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack1Bin)),

    %% Recv DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    {{udp, Socket, Host, NewPort, Data2Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack2Bin)),

    %% Recv DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    {{udp, Socket, Host, NewPort, Data3Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack3Bin)),

    %% Send ACK #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack3Bin)),

    %% Recv DATA #4 (the packet that the server think that we have lost)
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    {{udp, Socket, Host, NewPort, Data4Bin}} = ?TRY(recv(Timeout)),

    %% Recv DATA #4 AGAIN (the re-sent package)
    {{udp, Socket, Host, NewPort, Data4Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #2 which is out of range
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack2Bin)),

    %% Send ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack4Bin)),

    %% Recv DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    {{udp, Socket, Host, NewPort, Data5Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack5Bin)),

    %% Recv ACK #6
    {{udp, Socket, Host, NewPort, <<0,3,0,6>>}} = ?TRY(recv(Timeout)),

    %% Close socket
    {ok} = ?TRY(gen_udp:close(Socket)),

    {timeout} = ?TRY(recv(Timeout)),
    {ok} = ?TRY(file:delete(RemoteFilename)),
    ok.

resend_write_client(Host, Port, BlkSize) ->
    RemoteFilename = "tftp_resend_write_client.tmp",
    Block1 = lists:duplicate(BlkSize, $1),
    Block2 = lists:duplicate(BlkSize, $2),
    Block3 = lists:duplicate(BlkSize, $3),
    Block4 = lists:duplicate(BlkSize, $4),
    Block5 = lists:duplicate(BlkSize, $5),
    Blocks = [Block1, Block2, Block3, Block4, Block5],
    Blob = list_to_binary(Blocks),
    ?TRY(file:delete(RemoteFilename)),
    {{error, enoent}} = ?TRY(file:read_file(RemoteFilename)),

    Timeout = timer:seconds(3),
    {timeout} = ?TRY(recv(0)),

    %% Open socket
    {{ok, Socket}} =
        ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),

    WriteList = [0, 2, RemoteFilename, 0, "octet", 0],
    NewPort =
        if
            BlkSize =:= 512 ->
                %% Send WRITE
                WriteBin = list_to_binary(WriteList),
                {ok} = ?TRY(gen_udp:send(Socket, Host, Port, WriteBin)),

                %% Sleep a while in order to provoke the server to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv ACK #0 (the packet that the server think that we have lost)
                Ack0Bin = <<0, 4, 0, 0>>,
                {{udp, Socket, Host, _, Ack0Bin}} = ?TRY(recv(Timeout)),

                %% Recv ACK #0  AGAIN (the re-sent package)
                {{udp, Socket, Host, NewPort0, Ack0Bin}} = ?TRY(recv(Timeout)),
                NewPort0;
            true ->
                %% Send WRITE
                BlkSizeList = integer_to_list(BlkSize),
                WriteBin = list_to_binary([WriteList, "blksize", 0, BlkSizeList, 0]),
                {ok} = ?TRY(gen_udp:send(Socket, Host, Port, WriteBin)),

                %% Sleep a while in order to provoke the server to re-send the packet
                timer:sleep(timer:seconds(1)),

                %% Recv OACK (the packet that the server think that we have lost)
                OptionAckBin = list_to_binary([0, 6, "blksize",0, BlkSizeList, 0]),
                {{udp, Socket, Host, _, OptionAckBin}} = ?TRY(recv(Timeout)),

                %% Recv OACK AGAIN (the re-sent package)
                {{udp, Socket, Host, NewPort0, OptionAckBin}} =
                    ?TRY(recv(Timeout)),
                NewPort0
        end,

    %% Send DATA #1
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data1Bin)),

    %% Recv ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    {{udp, Socket, Host, NewPort, Ack1Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data2Bin)),

    %% Recv ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    {{udp, Socket, Host, NewPort, Ack2Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data3Bin)),

    %% Recv ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    {{udp, Socket, Host, NewPort, Ack3Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data3Bin)),

    %% Recv ACK #3 AGAIN (the packet that the server think that we have lost)
    {{udp, Socket, Host, NewPort, Ack3Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #2 which is out of range
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data2Bin)),

    %% Send DATA #4
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data4Bin)),

    %% Recv ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    {{udp, Socket, Host, NewPort, Ack4Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Data5Bin)),

    %% Recv ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    {{udp, Socket, Host, NewPort, Ack5Bin}} = ?TRY(recv(Timeout)),

    %% Close socket
    {ok} = ?TRY(gen_udp:close(Socket)),

    {timeout} = ?TRY(recv(Timeout)),
    {{ok, Blob}} = ?TRY(file:read_file(RemoteFilename)),
    {ok} = ?TRY(file:delete(RemoteFilename)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Re-send server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resend_server(doc) ->
    ["Verify that the server behaves correctly when the server re-sends packets."];
resend_server(suite) ->
    [];
resend_server(Config) when is_list(Config) ->
    Host = {127, 0, 0, 1},

    ok = resend_read_server(Host, 10),
    ok = resend_read_server(Host, 512),
    ok = resend_read_server(Host, 1025),

    ok = resend_write_server(Host, 10),
    ok = resend_write_server(Host, 512),
    ok = resend_write_server(Host, 1025).

resend_read_server(Host, BlkSize) ->
    RemoteFilename = "tftp_resend_read_server.tmp",
    Block1 = lists:duplicate(BlkSize, $1),
    Block2 = lists:duplicate(BlkSize, $2),
    Block3 = lists:duplicate(BlkSize, $3),
    Block4 = lists:duplicate(BlkSize, $4),
    Block5 = lists:duplicate(BlkSize, $5),
    Block6 = [],
    Blocks = [Block1, Block2, Block3, Block4, Block5, Block6],
    Blob = list_to_binary(Blocks),

    Timeout = timer:seconds(3),
    {timeout} = ?TRY(recv(0)),

    %% Open daemon socket
    {{ok, DaemonSocket}} =
        ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    {{ok, DaemonPort}} = ?TRY(inet:port(DaemonSocket)),

    %% Open server socket
    {{ok, ServerSocket}} =
        ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    ?TRY(inet:port(ServerSocket)),

    %% Prepare client process
    ReplyTo = self(),
    ClientFun =
        fun(Extra) ->
                Options = [{port, DaemonPort}, {debug, brief}] ++ Extra,
                {{ok, Blob} = Res} =
                    ?TRY(tftp:read_file(RemoteFilename, binary, Options)),
                ReplyTo ! {self(), {tftp_client_reply, Res}},
                exit(normal)
        end,

    ReadList = [0, 1, RemoteFilename, 0, "octet", 0],
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    Ack1Bin = <<0, 4, 0, 1>>,
    {ClientPort, ClientPid} =
        if
            BlkSize =:= 512 ->
                %% Start client process
                ClientPid0 = spawn_link(fun() -> ClientFun([]) end),

                %% Recv READ
                ReadBin = list_to_binary(ReadList),
                {{udp, DaemonSocket, Host, ClientPort0, ReadBin}} =
                    ?TRY(recv(Timeout)),

                %% Send DATA #1
                {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort0, Data1Bin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv ACK #1 (the packet that the server think that we have lost)
                {{udp, ServerSocket, Host, ClientPort0, Ack1Bin}} =
                    ?TRY(recv(Timeout)),

                %% Recv ACK #1 AGAIN (the re-sent package)
                {{udp, ServerSocket, Host, _, Ack1Bin}} = ?TRY(recv(Timeout)),
                {ClientPort0, ClientPid0};
            true ->
                %% Start client process
                BlkSizeList = integer_to_list(BlkSize),
                ClientPid0 = spawn_link(fun() -> ClientFun([{"blksize", BlkSizeList}]) end),

                %% Recv READ
                Options = ["blksize", 0, BlkSizeList, 0],
                ReadBin = list_to_binary([ReadList | Options]),
                {{udp, DaemonSocket, Host, ClientPort0, ReadBin}} =
                    ?TRY(recv(Timeout)),

                %% Send OACK
                BlkSizeList = integer_to_list(BlkSize),
                OptionAckBin = list_to_binary([0, 6, "blksize",0, BlkSizeList, 0]),
                {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort0, OptionAckBin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv ACK #0 (the packet that the server think that we have lost)
                Ack0Bin = <<0, 4, 0, 0>>,
                {{udp, ServerSocket, Host, ClientPort0, Ack0Bin}} =
                    ?TRY(recv(Timeout)),

                %% Recv ACK #0 AGAIN (the re-sent package)
                {{udp, ServerSocket, Host, ClientPort0, Ack0Bin}} =
                    ?TRY(recv(Timeout)),

                %% Send DATA #1
                {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort0, Data1Bin)),

                %% Recv ACK #1
                {{udp, ServerSocket, Host, _, Ack1Bin}} = ?TRY(recv(Timeout)),
                {ClientPort0, ClientPid0}
        end,

    %% Send DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data2Bin)),

    %% Recv ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    {{udp, ServerSocket, Host, ClientPort, Ack2Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data3Bin)),

    %% Recv ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    {{udp, ServerSocket, Host, ClientPort, Ack3Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data3Bin)),

    %% Recv ACK #3 AGAIN (the packet that the server think that we have lost)
    {{udp, ServerSocket, Host, ClientPort, Ack3Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #4
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data4Bin)),

    %% Recv ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    {{udp, ServerSocket, Host, ClientPort, Ack4Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #3 which is out of range
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data3Bin)),

    %% Send DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data5Bin)),

    %% Recv ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    {{udp, ServerSocket, Host, ClientPort, Ack5Bin}} = ?TRY(recv(Timeout)),

    %% Send DATA #6
    Data6Bin = list_to_binary([0, 3, 0, 6 | Block6]),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Data6Bin)),

    %% Recv ACK #6
    Ack6Bin = <<0, 4, 0, 6>>,
    {{udp, ServerSocket, Host, ClientPort, Ack6Bin}} = ?TRY(recv(Timeout)),

    %% Close daemon and server sockets
    {ok} = ?TRY(gen_udp:close(ServerSocket)),
    {ok} = ?TRY(gen_udp:close(DaemonSocket)),

    {{ClientPid, {tftp_client_reply, {ok, Blob}}}} =
        ?TRY(recv(2 * (Timeout + timer:seconds(1)))),

    {timeout} = ?TRY(recv(Timeout)),
    ok.

resend_write_server(Host, BlkSize) ->
    RemoteFilename = "tftp_resend_write_server.tmp",
    Block1 = lists:duplicate(BlkSize, $1),
    Block2 = lists:duplicate(BlkSize, $2),
    Block3 = lists:duplicate(BlkSize, $3),
    Block4 = lists:duplicate(BlkSize, $4),
    Block5 = lists:duplicate(BlkSize, $5),
    Block6 = [],
    Blocks = [Block1, Block2, Block3, Block4, Block5, Block6],
    Blob = list_to_binary(Blocks),
    Size = size(Blob),

    Timeout = timer:seconds(3),
    {timeout} = ?TRY(recv(0)),

    %% Open daemon socket
    {{ok, DaemonSocket}} =
        ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    {{ok, DaemonPort}} = ?TRY(inet:port(DaemonSocket)),

    %% Open server socket
    {{ok, ServerSocket}} =
        ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    ?TRY(inet:port(ServerSocket)),

    %% Prepare client process
    ReplyTo = self(),
    ClientFun =
        fun(Extra) ->
                Options = [{port, DaemonPort}, {debug, brief}] ++ Extra,
                {{ok, Size} = Res} =
                    ?TRY(tftp:write_file(RemoteFilename, Blob, Options)),
                ReplyTo ! {self(), {tftp_client_reply, Res}},
                exit(normal)
        end,

    WriteList = [0, 2, RemoteFilename, 0, "octet", 0],
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    {ClientPort, ClientPid} =
        if
            BlkSize =:= 512 ->
                %% Start client process
                ClientPid0 = spawn_link(fun() -> ClientFun([]) end),

                %% Recv WRITE
                WriteBin = list_to_binary(WriteList),
                io:format("WriteBin ~p\n", [WriteBin]),
                {{udp, DaemonSocket, Host, ClientPort0, WriteBin}} =
                    ?TRY(recv(Timeout)),

                %% Send ACK #1
                Ack0Bin = <<0, 4, 0, 0>>,
                {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort0, Ack0Bin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                {{udp, ServerSocket, Host, ClientPort0, Data1Bin}} =
                    ?TRY(recv(Timeout)),

                %% Recv DATA #1 AGAIN (the re-sent package)
                {{udp, ServerSocket, Host, _, Data1Bin}} = ?TRY(recv(Timeout)),
                {ClientPort0, ClientPid0};
            true ->
                %% Start client process
                BlkSizeList = integer_to_list(BlkSize),
                ClientPid0 = spawn_link(fun() -> ClientFun([{"blksize", BlkSizeList}]) end),

                %% Recv WRITE
                Options = ["blksize", 0, BlkSizeList, 0],
                WriteBin = list_to_binary([WriteList | Options]),
                {{udp, DaemonSocket, Host, ClientPort0, WriteBin}} =
                    ?TRY(recv(Timeout)),

                %% Send OACK
                BlkSizeList = integer_to_list(BlkSize),
                OptionAckBin = list_to_binary([0, 6, "blksize",0, BlkSizeList, 0]),
                {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort0, OptionAckBin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                {{udp, ServerSocket, Host, ClientPort0, Data1Bin}} =
                    ?TRY(recv(Timeout)),

                %% Recv DATA #1 AGAIN (the re-sent package)
                {{udp, ServerSocket, Host, ClientPort0, Data1Bin}} =
                    ?TRY(recv(Timeout)),
                {ClientPort0, ClientPid0}
        end,

    %% Send ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack1Bin)),

    %% Recv DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    {{udp, ServerSocket, Host, ClientPort, Data2Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack2Bin)),

    %% Recv DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    {{udp, ServerSocket, Host, ClientPort, Data3Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack3Bin)),

    %% Send ACK #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack3Bin)),

    %% Recv DATA #4 (the packet that the server think that we have lost)
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    {{udp, ServerSocket, Host, ClientPort, Data4Bin}} = ?TRY(recv(Timeout)),

    %% Recv DATA #4 AGAIN (the re-sent package)
    {{udp, ServerSocket, Host, ClientPort, Data4Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack4Bin)),

    %% Recv DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    {{udp, ServerSocket, Host, ClientPort, Data5Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #3 which is out of range
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack3Bin)),

    %% Send ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack5Bin)),

    %% Recv DATA #6
    Data6Bin = list_to_binary([0, 3, 0, 6 | Block6]),
    {{udp, ServerSocket, Host, ClientPort, Data6Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #6
    Ack6Bin = <<0, 4, 0, 6>>,
    {ok} = ?TRY(gen_udp:send(ServerSocket, Host, ClientPort, Ack6Bin)),

    %% Close daemon and server sockets
    {ok} = ?TRY(gen_udp:close(ServerSocket)),
    {ok} = ?TRY(gen_udp:close(DaemonSocket)),

    {{ClientPid, {tftp_client_reply, {ok, Size}}}} = ?TRY(recv(Timeout)),

    {timeout} = ?TRY(recv(Timeout)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reuse_connection(doc) ->
    ["Verify that the server can reuse an ongiong connection when same client resends request."];
reuse_connection(suite) ->
    [];
reuse_connection(Config) when is_list(Config) ->
    Host = {127, 0, 0, 1},
    {{Port, DaemonPid}} = ?TRY(?START_DAEMON([{debug, all}])),

    RemoteFilename = "reuse_connection.tmp",
    BlkSize = 512,
    Block1 = lists:duplicate(BlkSize, $1),
    Block2 = lists:duplicate(BlkSize div 2, $2),
    Blocks = [Block1, Block2],
    Blob = list_to_binary(Blocks),
    {ok} = ?TRY(file:write_file(RemoteFilename, Blob)),

    Seconds = 3,
    Timeout = timer:seconds(Seconds),
    {timeout} = ?TRY(recv(0)),

    %% Open socket
    {{ok, Socket}} =
        ?TRY(gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),

    ReadList = [0, 1, RemoteFilename, 0, "octet", 0],
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),

    %% Send READ
    TimeoutList = integer_to_list(Seconds),
    Options = ["timeout", 0, TimeoutList, 0],
    ReadBin = list_to_binary([ReadList | Options]),
    {ok} = ?TRY(gen_udp:send(Socket, Host, Port, ReadBin)),

    %% Send yet another READ for same file
    {ok} = ?TRY(gen_udp:send(Socket, Host, Port, ReadBin)),

    %% Recv OACK
    OptionAckBin = list_to_binary([0, 6 | Options]),
    {{udp, Socket, Host, NewPort, OptionAckBin}} = ?TRY(recv(Timeout)),

    %% Send ACK #0
    Ack0Bin = <<0, 4, 0, 0>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack0Bin)),

    %% Recv DATA #1
    {{udp, Socket, Host, NewPort, Data1Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack1Bin)),

    %% Recv DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    {{udp, Socket, Host, NewPort, Data2Bin}} = ?TRY(recv(Timeout)),

    %% Send ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    {ok} = ?TRY(gen_udp:send(Socket, Host, NewPort, Ack2Bin)),

    %% Close socket
    {ok} = ?TRY(gen_udp:close(Socket)),

    {timeout} = ?TRY(recv(Timeout)),
    {ok} = ?TRY(file:delete(RemoteFilename)),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Large file: transfer > 65535 blocks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

large_file(doc) ->
    ["Start the daemon and test transfer of files greater than 32M."];
large_file(suite) ->
    [];
large_file(Config) when is_list(Config) ->
    {ok} = ?TRY(application:start(tftp)),

    {{Port, DaemonPid}} = ?TRY(?START_DAEMON([{debug, brief}])),

    %% Read fail
    RemoteFilename = "tftp_temporary_large_file_remote_test_file.txt",
    LocalFilename = "tftp_temporary_large_file_local_test_file.txt",

    {ok, FH} = file:open(LocalFilename, [write,exclusive]),
    {ok, Size} = file:position(FH, {eof, 2*512*65535}),
    ok = file:truncate(FH),
    ?TRY(file:close(FH)),

    %% Write and read
    {{ok, Size}} =
        ?TRY(tftp:write_file(RemoteFilename, LocalFilename, [{port, Port}])),
    ?TRY(file:delete(LocalFilename)),
    {{ok, Size}} = ?TRY(tftp:read_file(RemoteFilename, LocalFilename, [{port, Port}])),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    {ok} = ?TRY(file:delete(LocalFilename)),
    {ok} = ?TRY(file:delete(RemoteFilename)),
    {ok} = ?TRY(application:stop(tftp)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Goodies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recv(Timeout) ->
    receive
        Msg ->
            Msg
    after Timeout ->
            timeout
    end.

get_conf(Key, Config) ->
    Default = make_ref(),
    case proplists:get_value(Key, Config, Default) of
        Default ->
            erlang:error({no_key, Key});
        Value ->
            Value
    end.

fn_jn(A, B) -> filename:join(A, B).
fn_jn(P) -> filename:join(P).
