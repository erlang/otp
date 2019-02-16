%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2018. All Rights Reserved.
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

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Includes and defines
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include_lib("common_test/include/ct.hrl").
-include("tftp_test_lib.hrl").

-define(START_DAEMON(Port, Options),
        begin
            {ok, Pid} = ?VERIFY({ok, _Pid}, tftp:start([{port, Port} | Options])),
            if
                Port == 0 ->
                    {ok, ActualOptions} = ?IGNORE(tftp:info(Pid)),
                    {value, {port, ActualPort}} =
                        lists:keysearch(port, 1, ActualOptions),
                    {ActualPort, Pid};
                true ->
                    {Port, Pid}
            end
        end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t() -> 
    tftp_test_lib:t([{?MODULE, all}]).

t(Cases) ->
    tftp_test_lib:t(Cases, default_config()).

t(Cases, Config) ->
    tftp_test_lib:t(Cases, Config).

default_config() ->
    [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(Case, Config) ->
    tftp_test_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) when is_list(Config) ->
    tftp_test_lib:end_per_testcase(Case, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [
     simple,
     extra,
     reuse_connection,
     resend_client,
     resend_server,
     large_file,
     app,
     appup,
     start_tftpd
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
    ok = ?t:app_test(tftp).

%%--------------------------------------------------------------------
appup() ->
    [{doc, "Test that the tftp appup file is ok"}].
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(tftp).

start_tftpd() ->
    [{doc, "Start/stop of tfpd service"}].
start_tftpd(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    ok = tftp:start(),
    {ok, Pid0} = tftp:start_service([{host, "localhost"}, {port, 0}]),
    Pids0 =  [ServicePid || {_, ServicePid} <- tftp:services()],
    true = lists:member(Pid0, Pids0),
    {ok, [_|_]} = tftp:service_info(Pid0),
    tftp:stop_service(Pid0),
    ct:sleep(100),
    Pids1 =  [ServicePid || {_, ServicePid} <- tftp:services()],
    false = lists:member(Pid0, Pids1),

    {ok, Pid1} =
	tftp:start_standalone([{host, "localhost"}, {port, 0}]),
    Pids2 =  [ServicePid || {_, ServicePid} <- tftp:services()],
    false = lists:member(Pid1, Pids2),
    %% Standalone service is not supervised
    {error,not_found} = tftp:stop_service(Pid1),
    ok = tftp:stop(),

    application:load(tftp),
    application:set_env(tftp, services, [{tftpd, [{host, "localhost"},
                                                  {port, 0}]}]),
    ok = tftp:start(),
    1 = length(tftp:services()),
    application:unset_env(tftp, services),
    ok = tftp:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple(doc) ->
    ["Start the daemon and perform simple a read and write."];
simple(suite) ->
    [];
simple(Config) when is_list(Config) ->
    ?VERIFY(ok, application:start(tftp)),

    {Port, DaemonPid} = ?IGNORE(?START_DAEMON(0, [{debug, brief}])),

    %% Read fail
    RemoteFilename = "tftp_temporary_remote_test_file.txt",
    LocalFilename = "tftp_temporary_local_test_file.txt",
    Blob = list_to_binary(lists:duplicate(2000, $1)),
    %% Blob = <<"Some file contents\n">>,
    Size = size(Blob),
    ?IGNORE(file:delete(RemoteFilename)),
    ?VERIFY({error, {client_open, enoent, _}},
            tftp:read_file(RemoteFilename, binary, [{port, Port}])),
    
    %% Write and read
    ?VERIFY({ok, Size}, tftp:write_file(RemoteFilename, Blob, [{port, Port}])),
    ?VERIFY({ok, Blob}, tftp:read_file(RemoteFilename, binary, [{port, Port}])),
    ?IGNORE(file:delete(LocalFilename)),
    ?VERIFY({ok, Size}, tftp:read_file(RemoteFilename, LocalFilename, [{port, Port}])),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    ?VERIFY(ok, file:delete(LocalFilename)),
    ?VERIFY(ok, file:delete(RemoteFilename)),
    ?VERIFY(ok, application:stop(tftp)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Extra
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extra(doc) ->
    ["Verify new stuff for IS 1.2."];
extra(suite) ->
    [];
extra(Config) when is_list(Config) ->
    ?VERIFY({'EXIT', {badarg,{fake_key, fake_flag}}},
            tftp:start([{port, 0}, {fake_key, fake_flag}])),

    {Port, DaemonPid} = ?IGNORE(?START_DAEMON(0, [{debug, brief}])),
    
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
    ?VERIFY(ok, file:write_file(LocalFilename, Blob)),
    ?VERIFY({ok, [{count, Size}, Peer]},
            tftp:write_file(RemoteFilename, LocalFilename, Options)),
    ?VERIFY(ok, file:delete(LocalFilename)),
    
    ?VERIFY({ok,[{bin, Blob}, Peer]}, 
            tftp:read_file(RemoteFilename, LocalFilename, Options)),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    ?VERIFY(ok, file:delete(LocalFilename)),
    ?VERIFY(ok, file:delete(RemoteFilename)),
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
    {Port, DaemonPid} = ?IGNORE(?START_DAEMON(0, [{debug, all}])),

    ?VERIFY(ok, resend_read_client(Host, Port, 10)),
    ?VERIFY(ok, resend_read_client(Host, Port, 512)),
    ?VERIFY(ok, resend_read_client(Host, Port, 1025)),

    ?VERIFY(ok, resend_write_client(Host, Port, 10)),
    ?VERIFY(ok, resend_write_client(Host, Port, 512)),
    ?VERIFY(ok, resend_write_client(Host, Port, 1025)),
    
    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
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
    ?VERIFY(ok, file:write_file(RemoteFilename, Blob)),

    Timeout = timer:seconds(3),
    ?VERIFY(timeout, recv(0)),

    %% Open socket
    {ok, Socket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),

    ReadList = [0, 1, RemoteFilename, 0, "octet", 0],
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    NewPort =
        if
            BlkSize =:= 512 ->
                %% Send READ
                ReadBin = list_to_binary(ReadList),
                ?VERIFY(ok, gen_udp:send(Socket, Host, Port, ReadBin)),

                %% Sleep a while in order to provoke the server to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                {udp, _, _, NewPort0, _} = ?VERIFY({udp, Socket, Host, _, Data1Bin}, recv(Timeout)),
                NewPort0;
            true ->
                %% Send READ
                BlkSizeList = integer_to_list(BlkSize),
                Options = ["blksize", 0, BlkSizeList, 0],
                ReadBin = list_to_binary([ReadList | Options]),
                ?VERIFY(ok, gen_udp:send(Socket, Host, Port, ReadBin)),

                %% Recv OACK
                OptionAckBin = list_to_binary([0, 6 | Options]),
                {udp, _, _, NewPort0, _} = ?VERIFY({udp, Socket, Host, _, OptionAckBin}, recv(Timeout)),

                %% Send ACK #0
                Ack0Bin = <<0, 4, 0, 0>>,
                ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort0, Ack0Bin)),

                %% Send ACK #0 AGAIN (pretend that we timed out)
                timer:sleep(timer:seconds(1)),
                ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort0, Ack0Bin)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                ?VERIFY({udp, Socket, Host, NewPort0, Data1Bin}, recv(Timeout)),
                NewPort0
        end,

    %% Recv DATA #1 AGAIN (the re-sent package)
    ?VERIFY({udp, Socket, Host, NewPort, Data1Bin}, recv(Timeout)),

    %% Send ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack1Bin)),

    %% Recv DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    ?VERIFY({udp, Socket, Host, NewPort, Data2Bin}, recv(Timeout)),

    %% Send ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack2Bin)),

    %% Recv DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    ?VERIFY({udp, Socket, Host, NewPort, Data3Bin}, recv(Timeout)),

    %% Send ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack3Bin)),

    %% Send ACK #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack3Bin)),

    %% Recv DATA #4 (the packet that the server think that we have lost)
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    ?VERIFY({udp, Socket, Host, NewPort, Data4Bin}, recv(Timeout)),

    %% Recv DATA #4 AGAIN (the re-sent package)
    ?VERIFY({udp, Socket, Host, NewPort, Data4Bin}, recv(Timeout)),

    %% Send ACK #2 which is out of range
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack2Bin)),

    %% Send ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack4Bin)),

    %% Recv DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    ?VERIFY({udp, Socket, Host, NewPort, Data5Bin}, recv(Timeout)),

    %% Send ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack5Bin)),

    %% Close socket
    ?VERIFY(ok, gen_udp:close(Socket)),

    ?VERIFY(timeout, recv(Timeout)),
    ?VERIFY(ok, file:delete(RemoteFilename)),
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
    ?IGNORE(file:delete(RemoteFilename)),
    ?VERIFY({error, enoent}, file:read_file(RemoteFilename)),

    Timeout = timer:seconds(3),
    ?VERIFY(timeout, recv(0)),

    %% Open socket
    {ok, Socket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),

    WriteList = [0, 2, RemoteFilename, 0, "octet", 0],
    NewPort =
        if
            BlkSize =:= 512 ->
                %% Send WRITE
                WriteBin = list_to_binary(WriteList),
                ?VERIFY(ok,  gen_udp:send(Socket, Host, Port, WriteBin)),

                %% Sleep a while in order to provoke the server to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv ACK #0 (the packet that the server think that we have lost)
                Ack0Bin = <<0, 4, 0, 0>>,
                ?VERIFY({udp, Socket, Host, _, Ack0Bin}, recv(Timeout)),

                %% Recv ACK #0  AGAIN (the re-sent package)
                {udp, _, _, NewPort0, _} = ?VERIFY({udp, Socket, Host, _, Ack0Bin}, recv(Timeout)),
                NewPort0;
            true ->
                %% Send WRITE
                BlkSizeList = integer_to_list(BlkSize),
                WriteBin = list_to_binary([WriteList, "blksize", 0, BlkSizeList, 0]),
                ?VERIFY(ok,  gen_udp:send(Socket, Host, Port, WriteBin)),

                %% Sleep a while in order to provoke the server to re-send the packet
                timer:sleep(timer:seconds(1)),

                %% Recv OACK (the packet that the server think that we have lost)
                OptionAckBin = list_to_binary([0, 6, "blksize",0, BlkSizeList, 0]),
                ?VERIFY({udp, Socket, Host, _, OptionAckBin}, recv(Timeout)),
                
                %% Recv OACK AGAIN (the re-sent package)
                {udp, _, _, NewPort0, _} = ?VERIFY({udp, Socket, Host, _, OptionAckBin}, recv(Timeout)),
                NewPort0
        end,

    %% Send DATA #1
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data1Bin)),

    %% Recv ACK #1 
    Ack1Bin = <<0, 4, 0, 1>>,
    ?VERIFY({udp, Socket, Host, NewPort, Ack1Bin}, recv(Timeout)),

    %% Send DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data2Bin)),

    %% Recv ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    ?VERIFY({udp, Socket, Host, NewPort, Ack2Bin}, recv(Timeout)),

    %% Send DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data3Bin)),

    %% Recv ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    ?VERIFY({udp, Socket, Host, NewPort, Ack3Bin}, recv(Timeout)),

    %% Send DATA #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data3Bin)),

    %% Recv ACK #3 AGAIN (the packet that the server think that we have lost)
    ?VERIFY({udp, Socket, Host, NewPort, Ack3Bin}, recv(Timeout)),

    %% Send DATA #2 which is out of range
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data2Bin)),

    %% Send DATA #4
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data4Bin)),

    %% Recv ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    ?VERIFY({udp, Socket, Host, NewPort, Ack4Bin}, recv(Timeout)),

    %% Send DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Data5Bin)),

    %% Recv ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    ?VERIFY({udp, Socket, Host, NewPort, Ack5Bin}, recv(Timeout)),

    %% Close socket
    ?VERIFY(ok, gen_udp:close(Socket)),

    ?VERIFY(timeout, recv(Timeout)),
    ?VERIFY({ok, Blob}, file:read_file(RemoteFilename)),
    ?VERIFY(ok, file:delete(RemoteFilename)),
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

    ?VERIFY(ok, resend_read_server(Host, 10)),
    ?VERIFY(ok, resend_read_server(Host, 512)),
    ?VERIFY(ok, resend_read_server(Host, 1025)),
    
    ?VERIFY(ok, resend_write_server(Host, 10)),
    ?VERIFY(ok, resend_write_server(Host, 512)),
    ?VERIFY(ok, resend_write_server(Host, 1025)),
    ok.

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
    ?VERIFY(timeout, recv(0)),

    %% Open daemon socket
    {ok, DaemonSocket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    {ok, DaemonPort} = ?IGNORE(inet:port(DaemonSocket)),

    %% Open server socket
    {ok, ServerSocket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    ?IGNORE(inet:port(ServerSocket)),

    %% Prepare client process
    ReplyTo = self(),
    ClientFun =
        fun(Extra) ->
                Options = [{port, DaemonPort}, {debug, brief}] ++ Extra,
                Res = ?VERIFY({ok, Blob}, tftp:read_file(RemoteFilename, binary, Options)),
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
                {udp, _, _, ClientPort0, _} = ?VERIFY({udp, DaemonSocket, Host, _, ReadBin}, recv(Timeout)),

                %% Send DATA #1
                ?VERIFY(ok,  gen_udp:send(ServerSocket, Host, ClientPort0, Data1Bin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv ACK #1 (the packet that the server think that we have lost)
                ?VERIFY({udp, ServerSocket, Host, ClientPort0, Ack1Bin}, recv(Timeout)),

                %% Recv ACK #1 AGAIN (the re-sent package)
                ?VERIFY({udp, ServerSocket, Host, _, Ack1Bin}, recv(Timeout)),
                {ClientPort0, ClientPid0};
            true ->
                %% Start client process
                BlkSizeList = integer_to_list(BlkSize),
                ClientPid0 = spawn_link(fun() -> ClientFun([{"blksize", BlkSizeList}]) end),
                
                %% Recv READ
                Options = ["blksize", 0, BlkSizeList, 0],
                ReadBin = list_to_binary([ReadList | Options]),
                {udp, _, _, ClientPort0, _} = ?VERIFY({udp, DaemonSocket, Host, _, ReadBin}, recv(Timeout)),

                %% Send OACK
                BlkSizeList = integer_to_list(BlkSize),
                OptionAckBin = list_to_binary([0, 6, "blksize",0, BlkSizeList, 0]),
                ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort0, OptionAckBin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv ACK #0 (the packet that the server think that we have lost)
                Ack0Bin = <<0, 4, 0, 0>>,
                ?VERIFY({udp, ServerSocket, Host, ClientPort0, Ack0Bin}, recv(Timeout)),

                %% Recv ACK #0 AGAIN (the re-sent package)
                ?VERIFY({udp, ServerSocket, Host, ClientPort0, Ack0Bin}, recv(Timeout)),

                %% Send DATA #1
                ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort0, Data1Bin)),

                %% Recv ACK #1
                ?VERIFY({udp, ServerSocket, Host, _, Ack1Bin}, recv(Timeout)),
                {ClientPort0, ClientPid0}
        end,

    %% Send DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data2Bin)),

    %% Recv ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Ack2Bin}, recv(Timeout)),

    %% Send DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data3Bin)),

    %% Recv ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Ack3Bin}, recv(Timeout)),

    %% Send DATA #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data3Bin)),

    %% Recv ACK #3 AGAIN (the packet that the server think that we have lost)
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Ack3Bin}, recv(Timeout)),

    %% Send DATA #4
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data4Bin)),

    %% Recv ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Ack4Bin}, recv(Timeout)),

    %% Send DATA #3 which is out of range
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data3Bin)),

    %% Send DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data5Bin)),

    %% Recv ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Ack5Bin}, recv(Timeout)),

    %% Send DATA #6
    Data6Bin = list_to_binary([0, 3, 0, 6 | Block6]),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Data6Bin)),

    %% Close daemon and server sockets
    ?VERIFY(ok, gen_udp:close(ServerSocket)),
    ?VERIFY(ok, gen_udp:close(DaemonSocket)),

    ?VERIFY({ClientPid, {tftp_client_reply, {ok, Blob}}}, recv(Timeout)),

    ?VERIFY(timeout, recv(Timeout)),
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
    ?VERIFY(timeout, recv(0)),

    %% Open daemon socket
    {ok, DaemonSocket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    {ok, DaemonPort} = ?IGNORE(inet:port(DaemonSocket)),

    %% Open server socket
    {ok, ServerSocket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    ?IGNORE(inet:port(ServerSocket)),

    %% Prepare client process
    ReplyTo = self(),
    ClientFun =
        fun(Extra) ->
                Options = [{port, DaemonPort}, {debug, brief}] ++ Extra,
                Res = ?VERIFY({ok, Size}, tftp:write_file(RemoteFilename, Blob, Options)),
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
                {udp, _, _, ClientPort0, _} = ?VERIFY({udp, DaemonSocket, Host, _, WriteBin}, recv(Timeout)),

                %% Send ACK #1
                Ack0Bin = <<0, 4, 0, 0>>,
                ?VERIFY(ok,  gen_udp:send(ServerSocket, Host, ClientPort0, Ack0Bin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                ?VERIFY({udp, ServerSocket, Host, ClientPort0, Data1Bin}, recv(Timeout)),

                %% Recv DATA #1 AGAIN (the re-sent package)
                ?VERIFY({udp, ServerSocket, Host, _, Data1Bin}, recv(Timeout)),
                {ClientPort0, ClientPid0};
            true ->
                %% Start client process
                BlkSizeList = integer_to_list(BlkSize),
                ClientPid0 = spawn_link(fun() -> ClientFun([{"blksize", BlkSizeList}]) end),
                
                %% Recv WRITE
                Options = ["blksize", 0, BlkSizeList, 0],
                WriteBin = list_to_binary([WriteList | Options]),
                {udp, _, _, ClientPort0, _} = ?VERIFY({udp, DaemonSocket, Host, _, WriteBin}, recv(Timeout)),

                %% Send OACK
                BlkSizeList = integer_to_list(BlkSize),
                OptionAckBin = list_to_binary([0, 6, "blksize",0, BlkSizeList, 0]),
                ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort0, OptionAckBin)),

                %% Sleep a while in order to provoke the client to re-send the packet
                timer:sleep(Timeout + timer:seconds(1)),

                %% Recv DATA #1 (the packet that the server think that we have lost)
                ?VERIFY({udp, ServerSocket, Host, ClientPort0, Data1Bin}, recv(Timeout)),

                %% Recv DATA #1 AGAIN (the re-sent package)
                ?VERIFY({udp, ServerSocket, Host, ClientPort0, Data1Bin}, recv(Timeout)),
                {ClientPort0, ClientPid0}
        end,

    %% Send ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack1Bin)),

    %% Recv DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Data2Bin}, recv(Timeout)),

    %% Send ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack2Bin)),

    %% Recv DATA #3
    Data3Bin = list_to_binary([0, 3, 0, 3 | Block3]),
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Data3Bin}, recv(Timeout)),

    %% Send ACK #3
    Ack3Bin = <<0, 4, 0, 3>>,
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack3Bin)),

    %% Send ACK #3 AGAIN (pretend that we timed out)
    timer:sleep(timer:seconds(1)),
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack3Bin)),

    %% Recv DATA #4 (the packet that the server think that we have lost)
    Data4Bin = list_to_binary([0, 3, 0, 4 | Block4]),
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Data4Bin}, recv(Timeout)),

    %% Recv DATA #4 AGAIN (the re-sent package)
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Data4Bin}, recv(Timeout)),

    %% Send ACK #4
    Ack4Bin = <<0, 4, 0, 4>>,
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack4Bin)),

    %% Recv DATA #5
    Data5Bin = list_to_binary([0, 3, 0, 5 | Block5]),
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Data5Bin}, recv(Timeout)),

    %% Send ACK #3 which is out of range
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack3Bin)),

    %% Send ACK #5
    Ack5Bin = <<0, 4, 0, 5>>,
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack5Bin)),

    %% Recv DATA #6
    Data6Bin = list_to_binary([0, 3, 0, 6 | Block6]),
    ?VERIFY({udp, ServerSocket, Host, ClientPort, Data6Bin}, recv(Timeout)),

    %% Send ACK #6
    Ack6Bin = <<0, 4, 0, 6>>,
    ?VERIFY(ok, gen_udp:send(ServerSocket, Host, ClientPort, Ack6Bin)),

    %% Close daemon and server sockets
    ?VERIFY(ok, gen_udp:close(ServerSocket)),
    ?VERIFY(ok, gen_udp:close(DaemonSocket)),

    ?VERIFY({ClientPid, {tftp_client_reply, {ok, Size}}}, recv(Timeout)),

    ?VERIFY(timeout, recv(Timeout)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reuse_connection(doc) ->
    ["Verify that the server can reuse an ongiong connection when same client resends request."];
reuse_connection(suite) ->
    [];
reuse_connection(Config) when is_list(Config) ->
    Host = {127, 0, 0, 1},
    {Port, DaemonPid} = ?IGNORE(?START_DAEMON(0, [{debug, all}])),

    RemoteFilename = "reuse_connection.tmp",
    BlkSize = 512,
    Block1 = lists:duplicate(BlkSize, $1),
    Block2 = lists:duplicate(BlkSize div 2, $2),
    Blocks = [Block1, Block2],
    Blob = list_to_binary(Blocks),
    ?VERIFY(ok, file:write_file(RemoteFilename, Blob)),
    
    Seconds = 3,
    Timeout = timer:seconds(Seconds),
    ?VERIFY(timeout, recv(0)),
    
    %% Open socket
    {ok, Socket} = ?VERIFY({ok, _}, gen_udp:open(0, [binary, {reuseaddr, true}, {active, true}])),
    
    ReadList = [0, 1, RemoteFilename, 0, "octet", 0],
    Data1Bin = list_to_binary([0, 3, 0, 1 | Block1]),
    
    %% Send READ
    TimeoutList = integer_to_list(Seconds),
    Options = ["timeout", 0, TimeoutList, 0],
    ReadBin = list_to_binary([ReadList | Options]),
    ?VERIFY(ok, gen_udp:send(Socket, Host, Port, ReadBin)),

    %% Send yet another READ for same file
    ?VERIFY(ok, gen_udp:send(Socket, Host, Port, ReadBin)),

    %% Recv OACK
    OptionAckBin = list_to_binary([0, 6 | Options]),
    {udp, _, _, NewPort, _} = ?VERIFY({udp, Socket, Host, _, OptionAckBin}, recv(Timeout)),

    %% Send ACK #0
    Ack0Bin = <<0, 4, 0, 0>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack0Bin)),

    %% Recv DATA #1
    ?VERIFY({udp, Socket, Host, NewPort, Data1Bin}, recv(Timeout)),

    %% Send ACK #1
    Ack1Bin = <<0, 4, 0, 1>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack1Bin)),

    %% Recv DATA #2
    Data2Bin = list_to_binary([0, 3, 0, 2 | Block2]),
    ?VERIFY({udp, Socket, Host, NewPort, Data2Bin}, recv(Timeout)),

    %% Send ACK #2
    Ack2Bin = <<0, 4, 0, 2>>,
    ?VERIFY(ok, gen_udp:send(Socket, Host, NewPort, Ack2Bin)),

    %% Close socket
    ?VERIFY(ok, gen_udp:close(Socket)),

    ?VERIFY(timeout, recv(Timeout)),
    ?VERIFY(ok, file:delete(RemoteFilename)),

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
    ?VERIFY(ok, application:start(tftp)),

    {Port, DaemonPid} = ?IGNORE(?START_DAEMON(0, [{debug, brief}])),

    %% Read fail
    RemoteFilename = "tftp_temporary_large_file_remote_test_file.txt",
    LocalFilename = "tftp_temporary_large_file_local_test_file.txt",

    {ok, FH} = file:open(LocalFilename, [write,exclusive]),
    {ok, Size} = file:position(FH, {eof, 2*512*65535}),
    ok = file:truncate(FH),
    ?IGNORE(file:close(FH)),

    %% Write and read
    ?VERIFY({ok, Size}, tftp:write_file(RemoteFilename, LocalFilename, [{port, Port}])),
    ?IGNORE(file:delete(LocalFilename)),
    ?VERIFY({ok, Size}, tftp:read_file(RemoteFilename, LocalFilename, [{port, Port}])),

    %% Cleanup
    unlink(DaemonPid),
    exit(DaemonPid, kill),
    ?VERIFY(ok, file:delete(LocalFilename)),
    ?VERIFY(ok, file:delete(RemoteFilename)),
    ?VERIFY(ok, application:stop(tftp)),
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
