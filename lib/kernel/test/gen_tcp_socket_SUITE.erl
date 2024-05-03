%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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

-module(gen_tcp_socket_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct_event.hrl").

all() ->
    [{group, smoketest}].

groups() ->
    [{smoketest,    [{group,small}]},
     {benchmark,    [{group,small}, {group,medium},
                     {group,large}, {group,huge}]},
     %%
     {dev,          [{group,dev_direct},
                     {group,dev_inet}, {group,dev_socket}]},
     {dev_inet,     testcases(dev)},
     {dev_socket,   testcases(dev)},
     {dev_direct,   testcases(direct)},
     %%
     {small,        backend_groups()},
     {medium,       backend_groups()},
     {large,        backend_groups()},
     {huge,         backend_groups()},
     %%
     {inet,         testcases(active)},
     {socket,       testcases(active)},
     {direct,       testcases(direct)}].

backend_groups() ->
    [{group,inet}, {group,socket}, {group,direct}].

testcases(active) ->
    [active_raw, active_false, active_true,
     active_once, active_1, active_5, active_20];
testcases(dev) ->
    [active_raw, active_false, active_true,
     active_once, active_20];
testcases(direct) ->
    [socket_raw, socket_packet, socket_packet_buf, socket_packet_cheat].

-define(SUITE, "gen_tcp_socket").
-define(DOMAIN, inet).
%% *_EXP is a power of 2 exponent so the resulting value is
%% 1 bsl ?*_EXP
-define(NORM_EXP, 30). % Total byte size norm: packet size * number of packets
-define(BUFSIZE_EXP, 17).
-define(BUFSIZE, (1 bsl ?BUFSIZE_EXP)).

init_per_suite(Config) ->
    case socket:is_supported(protocols, tcp) of
        true ->
            ct:pal("socket:info():~n    ~p~n", [socket:info()]),
            {ok, BindAddr} = kernel_test_lib:which_local_addr(?DOMAIN),
            [{bind_addr, #{ family => ?DOMAIN, addr   => BindAddr }}
            | Config];
        false ->
            {skip, "Socket not supported"}
    end.

end_per_suite(_Config) ->
    ct:pal("socket:info():~n    ~p~n", [socket:info()]),
    ok.


init_per_group(Nm, Config) ->
    case Nm of
        smoketest   -> [{burden,0} | Config];
        benchmark   -> [{burden,2} | Config]; % 4 times the total size
        %%
        dev         -> init_per_group(dev_size, [{burden,3} | Config]);
        dev_inet    -> init_per_group(inet,     Config);
        dev_socket  -> init_per_group(socket,   Config);
        dev_direct  -> init_per_group(direct,   Config);
        %%
        dev_size    ->
            init_per_group_size(
              ?BUFSIZE_EXP + 4, ?NORM_EXP - (?BUFSIZE_EXP + 4), Config);
        %% For small packets we cannot have the total size 2^?NORM_EXP
        %% since the message passing overhead (latency) starts to dominate
        %% so we have to subtract some from the exponent to get
        %% roughly the same running time
        small       -> init_per_group_size(7, ?NORM_EXP - 7 - 3, Config);
        medium      -> init_per_group_size(10, ?NORM_EXP - 10 - 1, Config);
        large       -> init_per_group_size(15, ?NORM_EXP - 15, Config);
        huge ->
            init_per_group_size(
              ?BUFSIZE_EXP + 2, ?NORM_EXP - (?BUFSIZE_EXP + 2), Config);
        %%
        _ when Nm =:= inet;
               Nm =:= socket;
               Nm =:= direct -> [{backend,Nm} | Config]
    end.

init_per_group_size(K, L, Config) ->
    {_, Burden} = proplists:lookup(burden, Config),
    M = L + Burden,
    N = 1 bsl M,
    %% 2^K     = Mean packet size
    %% 2^(K+1) = Max packet size
    %% N       = Number of packets;
    StopTag = spawn_testdata_server(K+1, N),
     %%
    {MeanSize, SizeSuffix} = size_and_suffix(1 bsl K),
    {PacketCount, CountSuffix} = size_and_suffix(N),
    ct:pal("Packet mean size: ~w ~sByte, packet count: ~w ~s",
           [MeanSize, SizeSuffix, PacketCount, CountSuffix]),
    %%
    [{testdata_server, StopTag},
     {testdata_size,   {K, M}} | Config].

end_per_group(Nm, Config) ->
    case Nm of
        dev -> proplists:delete(burden, end_per_group(dev_size, Config));
        dev_inet    -> end_per_group(inet, Config);
        dev_socket  -> end_per_group(socket, Config);
        dev_direct  -> end_per_group(direct, Config);
        _ when Nm =:= smoketest;
               Nm =:= dev;
               Nm =:= benchmark -> proplists:delete(burden, Config);
        _ when Nm =:= dev_size;
               Nm =:= small;
               Nm =:= medium;
               Nm =:= large;
               Nm =:= huge ->
            {_, StopTag} = proplists:lookup(testdata_server, Config),
            stop_testdata_server(StopTag),
            proplists:delete(
              testdata_server, proplists:delete(testdata_size, Config));
        _ when Nm =:= inet;
               Nm =:= socket;
               Nm =:= direct -> proplists:delete(backend, Config)
    end.


size_and_suffix(P) ->
    size_and_suffix(P, 1, ["", "K", "M", "T", "Z"]).
%%
size_and_suffix(P, _, []) ->
    {P, ""};
size_and_suffix(P, Q, Suffixes) ->
    Q_1 = Q bsl 10,
    if
        is_integer(P), Q_1 =< P, P band (Q_1 - 1) =:= 0 ->
            size_and_suffix(P, Q_1, tl(Suffixes));
        Q bsl 13 =< P ->
            size_and_suffix(P, Q_1, tl(Suffixes));
        true ->
            {round(P / Q), hd(Suffixes)}
    end.

%% -------
%% Testcases

-define(
   XFER(Name),
   Name(Config) -> xfer(Config, ?FUNCTION_NAME)).

?XFER(active_raw).
?XFER(active_false).
?XFER(active_true).
?XFER(active_once).
?XFER(active_1).
?XFER(active_5).
?XFER(active_20).
?XFER(socket_raw).
?XFER(socket_packet).
?XFER(socket_packet_buf).
?XFER(socket_packet_cheat).

%% -------

tc2active(TC) ->
    case TC of
        active_raw      -> raw;
        active_false    -> false;
        active_true     -> true;
        active_once     -> once;
        active_1        -> 1;
        active_5        -> 5;
        active_20       -> 20
    end.

xfer(Config, TC) when is_list(Config) ->
    {_, Backend}      = proplists:lookup(backend,       Config),
    {_, BindAddr}     = proplists:lookup(bind_addr,     Config),
    {_, TestdataSize} = proplists:lookup(testdata_size, Config),
    run_xfer(TC, Backend, BindAddr, TestdataSize,  testdata()).

run_xfer(
  TC, Backend, BindAddr, {K, _} = TestdataSize,
  #{iovecs       := Iovecs,
    packet_sizes := PacketSizes,
    total_size   := TotalSize}) ->
    %%
    Parent = self(),
    Tag = make_ref(),
    {Sender, Mref} =
        spawn_opt(
          fun () ->
                  try
                      %% Send iovecs efficiently
                      {ok, L} =
                          gen_tcp:listen(
                            0, [{ifaddr,BindAddr}, {sndbuf, 2 bsl K}]),
                      {ok, {IP,Port}} = inet:sockname(L),
                      Sockaddr =
                          #{family => inet, addr => IP, port => Port},
                      Parent ! {Tag, Sockaddr},
                      {ok, A} = gen_tcp:accept(L),
                      ok = gen_tcp:close(L),
                      send_loop(A, Iovecs),
                      ok = gen_tcp:close(A)
                  catch Class : Reason : Stacktrace ->
                          ct:pal(
                            "Sender crash [~w] ~w : ~p~n    ~p~n",
                            [self(), Class, Reason, Stacktrace]),
                          erlang:raise(Class, Reason, Stacktrace)
                  end
          end, [monitor]),
    receive
        {Tag, Sockaddr} ->
            ct:pal("try connect to ~p"
                   "~n   Backend: ~p"
                   "~n   TC:      ~p"
                  "~n", [Sockaddr, Backend, TC]),
            C = case connect(Backend, Sockaddr, TC) of
                    {ok, CSock} ->
                        CSock;
                    {error, eaddrnotavail = CReason} ->
                        exit({skip, CReason});
                    {error, CReason} ->
                        ct:fail({connect_failed, CReason})
                end,
            try
                T1 = erlang:monotonic_time(),
                assert({ok, TotalSize}, recv_loop(C, PacketSizes, TC)),
                T2 = erlang:monotonic_time(),
                T = erlang:convert_time_unit(T2 - T1, native, millisecond),
                report_MByte_s(Backend, TestdataSize, TC, TotalSize, T)
            catch Class : Reason : Stacktrace ->
                    ct:pal(
                      "Receiver crash [~w] ~w : ~p~n    ~p~n",
                      [self(), Class, Reason, Stacktrace]),
                    exit(Sender, receiver_crash),
                    %% spawn_link of Sender does not work since
                    %% ct catches the below failure so this process
                    %% doesn't die and the link doesn't trigger,
                    %% therefore the explicit exit/2 above
                    %%
                    erlang:raise(Class, Reason, Stacktrace)
            after
                close(Backend, C),
                receive {'DOWN',Mref,_,_,_} -> ok end
            end;
        {'DOWN',Mref,_,_,Reason} ->
            error({sender_died,Reason})
    end.

send_loop(_S, []) ->
    ok;
send_loop(S, [Iovec | Iovecs]) ->
    ok = gen_tcp:send(S, Iovec),
    send_loop(S, Iovecs).


connect(direct, Sockaddr, _) ->
    {ok, S} = socket:open(?DOMAIN, stream),
    ok = socket:bind(S, any),
    ok = socket:setopt(S, {socket,rcvbuf}, ?BUFSIZE),
    ok = socket:setopt(S, {otp,rcvbuf},    ?BUFSIZE),
    io:format("socket:connect [~p].~n", [Sockaddr]),
    case socket:connect(S, Sockaddr) of
        ok ->
            {ok, S};
        Error ->
            Error
    end;
connect(Backend, Sockaddr, active_raw) -> % {active,true}, {packet,raw}
    Opts =
        [{inet_backend,Backend}, binary, {active,true},
         ?DOMAIN, {recbuf,?BUFSIZE}],
    io:format("gen_tcp:connect(~p, ~p).~n", [Sockaddr, Opts]),
    gen_tcp:connect(Sockaddr, Opts);
connect(Backend, Sockaddr, TC) ->
    Opts =
        [{inet_backend,Backend}, binary, {active,tc2active(TC)}, {packet,4},
         ?DOMAIN, {recbuf,?BUFSIZE}],
    io:format("gen_tcp:connect(~p, ~p).~n", [Sockaddr, Opts]),
    gen_tcp:connect(Sockaddr, Opts).


close(direct, S) ->
    socket:close(S);
close(_, S) ->
    gen_tcp:close(S).


report_MByte_s(Backend, {K, M}, TC, Size, Time) ->
    ct:log("Size: ~w. Time: ~w.", [Size, Time]),
    Name = io_lib:format("~w 2^(~w+~w)-~w", [Backend, K, M, TC]),
    {Value, Suffix} = size_and_suffix(Size * 1000 / Time),
    report(Name, Value, Suffix++"Byte/s").


spawn_testdata_server(K, N) ->
    Spawner   = self(),
    Tag       = make_ref(),
    {_, Mref} =
        spawn_opt(
          fun () ->
                  register(?MODULE, self()),
                  Testdata = generate_testdata(K, N),
                  Spawner ! Tag,
                  testdata_server(Tag, Testdata)
          end, [monitor]),
    receive
        Tag ->
            demonitor(Mref, [flush]),
            Tag;
        {'DOWN', Mref, _, _, Info} ->
            error(Info)
    end.

testdata_server(Tag, Testdata) ->
    receive
        {get, From} ->
            From ! {From, Testdata},
            testdata_server(Tag, Testdata);
        Tag ->
            ok
    end.

stop_testdata_server(Tag) when is_reference(Tag) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            Ref = monitor(process, Pid),
            Pid ! Tag,
            receive {'DOWN', Ref, _, _, _} -> ok end
    end.

testdata() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            Ref = monitor(process, ?MODULE, [{alias,reply_demonitor}]),
            Pid ! {get, Ref},
            receive
                {Ref, Testdata} ->
                    Testdata;
                {'DOWN', Ref, _, _, Info} ->
                    error(Info)
            end
    end.

generate_testdata(K, N) ->
    %% 2^K = Max packet size
    %% N   = Number of packets
    %%
    Offsets        =  generate_offsets(K, N),
    DataBlock      =  create_data_block(1 bsl K),
    Iovecs         =  generate_iovecs(Offsets, DataBlock, 1000),
    DataBlockSize  =  byte_size(DataBlock),
    PacketSizes    =  packet_sizes(Offsets, DataBlockSize),
    TotalSize      =  total_size(Offsets, DataBlockSize),
    #{iovecs       => Iovecs,
      packet_sizes => PacketSizes,
      total_size   => TotalSize}.


packet_sizes(Offsets, DataBlockSize) ->
    [DataBlockSize - Offset - 4 || Offset <- Offsets].

total_size([], _DataBlockSize) -> 0;
total_size([Offset | Offsets], DataBlockSize) ->
    DataBlockSize - Offset + total_size(Offsets, DataBlockSize).

generate_iovecs([], _DataBlock, _ChunkSize) -> [];
generate_iovecs(Offsets, DataBlock, ChunkSize) ->
    {Offsets_1, Iovecs} = generate_iovec(Offsets, DataBlock, ChunkSize),
    [Iovecs | generate_iovecs(Offsets_1, DataBlock, ChunkSize)].

generate_iovec(Offsets, DataBlock, N) ->
    generate_iovec(Offsets, DataBlock, N, []).
%%
generate_iovec(Offsets, _DataBlock, N, Acc)
  when N =:= 0;
       Offsets =:= [], is_integer(N), 0 < N ->
    {Offsets, lists:reverse(Acc)};
generate_iovec([Offset | Offsets], DataBlock, N, Acc)
  when is_integer(N), 0 < N ->
    <<_:Offset/binary, Bin/binary>> = DataBlock,
    generate_iovec(Offsets, DataBlock, N - 1, [Bin | Acc]).

%% 0 =< Offset < 2^K - 1
%% Offset mod 4 == 0
generate_offsets(K, N) when is_integer(K), 6 =< K ->
    %% 3 here corresponds to the 8 iterations below,
    %% and 2 corresponds to the bsl 2 that makes mod 4 == 0
    Range = 1 bsl (K - (3 + 2)),
    generate_offsets(Range, N, []).
%%
generate_offsets(_Range, 0, Offsets) ->
    Offsets;
generate_offsets(Range, N, Offsets) when is_integer(N), 0 < N ->
    Offset = (rand_sum(Range, 8) - 8) bsl 2,
    generate_offsets(Range, N - 1, [Offset | Offsets]).

%% Sum I number of rand:uniform values range N
%%
rand_sum(_, 0) -> 0;
rand_sum(N, I) -> rand:uniform(N) + rand_sum(N, I - 1).

%% Create a data block that at every offset mod 4 == 0
%% contains that number of bytes after, so at every
%% such offset up to the end of the data block
%% there is a valid packet 4 chunk
%%
create_data_block(M) -> create_data_block(M, <<>>).
%%
create_data_block(M, Bin) when is_integer(M), 4 =< M ->
    %% M   :: Desired chunk size
    %% Bin :: Accumulator, grows at the end
    create_data_block(M - 4, <<Bin/binary, M:32>>);
create_data_block(3 = M, Bin) -> <<Bin/binary, M:32, 3, 2, 1>>;
create_data_block(2 = M, Bin) -> <<Bin/binary, M:32, 2, 1>>;
create_data_block(1 = M, Bin) -> <<Bin/binary, M:32, 1>>;
create_data_block(0 = M, Bin) -> <<Bin/binary, M:32>>.


%% Receive all packets on the stream, return the total payload size
%%
recv_loop(S, Sizes, TC) ->
    case TC of
        socket_raw ->
            Recv = fun (Socket) -> socket:recv(Socket, 0) end,
            recv_loop_raw(S, 0, Recv);
        socket_packet ->
            recv_loop_packet(S, Sizes, 0, fun socket:recv/2);
        socket_packet_buf ->
            recv_loop_packet_buf(S, Sizes, 0, fun socket:recv/2);
        socket_packet_cheat ->
            recv_loop_packet_cheat(S, Sizes, 0, fun socket:recv/2);
        active_raw ->
            recv_loop_active_raw(S, 0);
        active_false ->
            Recv = fun (Socket) -> gen_tcp:recv(Socket, 0) end,
            recv_loop_active_false(S, Sizes, 0, Recv);
        active_true ->
            recv_loop_active_true(S, Sizes, 0);
        active_once ->
            recv_loop_active_once(S, Sizes, 0);
        _ ->
            recv_loop_active_n(S, Sizes, 0, tc2active(TC))
    end.

%% -------
%% These ignore packet borders and just count the total number of bytes

recv_loop_active_raw(S, M) ->
    receive
        {tcp, S, Data} ->
            DataSize = byte_size(Data),
            recv_loop_active_raw(S, M + DataSize);
        {tcp_closed, S} ->
            {ok, M};
        {tcp_error, S, Reason} ->
            {error, Reason}
    end.

recv_loop_raw(S, M, Recv) ->
    case Recv(S) of
        {ok, Data} ->
            DataSize = byte_size(Data),
            recv_loop_raw(S, M + DataSize, Recv);
        {error, closed} ->
            {ok, M};
        {error, _} = Error ->
            Error
    end.

%% -------
%% These implement {packet,4}

%% Read packet header then packet body with separate Recv calls
%%
recv_loop_packet(_S, [], M, _Recv) ->
    {ok, M};
recv_loop_packet(S, [Size|Sizes], M, Recv) ->
    case Recv(S, 4) of
        {ok, <<Size:32>>} ->
            case Recv(S, Size) of
                {ok, Bin} when is_binary(Bin) ->
                    recv_loop_packet(S, Sizes, M + 4 + Size, Recv);
                Err2 ->
                    Err2
            end;
        Err1 ->
            Err1
    end.

%% Use Recv(0) and then parse the received binary into packets.
%%
recv_loop_packet_buf(S, Sizes, M, Recv) ->
    case Recv(S, 0) of
        {ok, Data} when 0 < byte_size(Data) ->
            recv_loop_packet_buf(S, Sizes, M, Recv, Data);
        {error, closed} ->
            [] = Sizes,
            {ok, M};
        {error, _} = Error ->
            Error
    end.
%%
recv_loop_packet_buf(S, Sizes, M, Recv, Buf) ->
    %% Buf is a binary(), no fancy queue
    %%
    case Buf of
        <<PacketSize:32, _:PacketSize/binary, Rest/binary>> ->
            assert(PacketSize, hd(Sizes)),
            NewM = M + 4 + PacketSize,
            recv_loop_packet_buf(S, tl(Sizes), NewM, Recv, Rest);
        <<PacketSize:32, Start/binary>> ->
            %% Partial packet.
            %% Keep it simple and read the rest of the packet with Recv(N).
            %% We could also Recv(0) to get more data but that
            %% would complicate handling of Start + Rest.
            %% This is hopefully a rare case for small packets
            %% and a small overhead for large packets..
            RestSize = PacketSize - byte_size(Start),
            case Recv(S, RestSize) of
                {ok, Rest} when byte_size(Rest) =:= RestSize->
                    Packet = <<Start/binary, Rest/binary>>,
                    assert(PacketSize, byte_size(Packet)),
                    NewM = M + 4 + PacketSize,
                    assert(PacketSize, hd(Sizes)),
                    recv_loop_packet_buf(S, tl(Sizes), NewM, Recv);
                {error, _} = Error ->
                    Error
            end;
        <<>> ->
            recv_loop_packet_buf(S, Sizes, M, Recv);
        <<Head/binary>> ->
            %% Partial header.
            %% Keep it simple and just Recv(N) the rest of the header.
            %% Using Recv(0) would complicate the code greatly.
            %% This is hopefully a rare case for small packets
            %% and a small overhead for large packets..
            RestSize = 4 - byte_size(Head),
            case Recv(S, RestSize) of
                {ok, Rest} when byte_size(Rest) =:= RestSize->
                    Data = <<Head/binary, Rest/binary>>,
                    recv_loop_packet_buf(S, Sizes, M, Recv, Data);
                {error, _} = Error ->
                    Error
            end
    end.

%% Cheating - we know the next packet size and receive exactly
%% that in one Recv call, and verify the packet header
%%
recv_loop_packet_cheat(_S, [], M, _Recv) ->
    {ok, M};
recv_loop_packet_cheat(S, [Size|Sizes], M, Recv) ->
    case Recv(S, 4 + Size) of
        {ok, <<Size:32, _:Size/binary>>} ->
            recv_loop_packet_cheat(S, Sizes, M + 4 + Size, Recv);
        {ok, Data} ->
            {error,{bad_packet,Data,Size}};
        Err1 ->
            Err1
    end.

%% -------
%% These assume that the socket is in {packet,4} mode

recv_loop_active_false(S, Sizes, M, Recv) ->
    case Recv(S) of
        {ok, Data} ->
            DataSize = byte_size(Data),
            assert(DataSize, hd(Sizes)),
            recv_loop_active_false(S, tl(Sizes), M + DataSize + 4, Recv);
        {error, closed} ->
            [] = Sizes,
            {ok, M};
        {error, _} = Error ->
            Error
    end.

recv_loop_active_true(S, Sizes, M) ->
    receive
        {tcp, S, Data} ->
            DataSize = byte_size(Data),
            assert(DataSize, hd(Sizes)),
            recv_loop_active_true(S, tl(Sizes), M + DataSize + 4);
        {tcp_closed, S} ->
            Sizes = [],
            {ok, M};
        {tcp_error, S, Reason} ->
            {error, Reason}
    end.

recv_loop_active_once(S, Sizes, M) ->
    receive
        {tcp, S, Data} ->
            DataSize = byte_size(Data),
            assert(DataSize, hd(Sizes)),
            ok = inet:setopts(S, [{active,once}]),
            recv_loop_active_once(S, tl(Sizes), M + DataSize + 4);
        {tcp_closed, S} ->
            Sizes = [],
            {ok, M};
        {tcp_error, S, Reason} ->
            {error, Reason}
    end.

recv_loop_active_n(S, Sizes, M, N) ->
    receive
        {tcp, S, Data} ->
            DataSize = byte_size(Data),
            assert(DataSize, hd(Sizes)),
            recv_loop_active_n(S, tl(Sizes), M + DataSize + 4, N);
        {tcp_passive, S} ->
            ok = inet:setopts(S, [{active,N}]),
            recv_loop_active_n(S, Sizes, M, N);
        {tcp_closed, S} ->
            Sizes = [],
            {ok, M};
        {tcp_error, S, Reason} ->
            {error, Reason}
    end.

%% -------

report(Name, Value, Suffix) ->
    ct:pal("### ~s: ~w ~s", [Name, Value, Suffix]),
    ct_event:notify(
      #event{
         name = benchmark_data,
         data = [{value, Value}, {suite, ?SUITE}, {name, Name}]}),
    {comment, term_to_string(Value) ++ " " ++ Suffix}.

term_to_string(Term) ->
    unicode:characters_to_list(io_lib:write(Term, [{encoding, unicode}])).

-compile({inline, [assert/2]}).
assert(X, X) -> ok;
assert(X, Y) -> error({assert, X, Y}).
