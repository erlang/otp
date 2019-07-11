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

-module(socket_test_lib).

-export([
         pi/1, pi/2, pi/3,

         %% Time stuff
         timestamp/0,
         tdiff/2,
         formated_timestamp/0,
         format_timestamp/1,

         %% String and format
         f/2,

         %% Generic 'has support' test function(s)
         has_support_ipv6/0,

         which_local_host_info/1,
         which_local_addr/1,

         %% Skipping
         not_yet_implemented/0,
         skip/1
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FAIL(R), exit(R)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi(Item) when is_atom(Item) ->
    pi(self(), Item).

pi(Pid, Item) when is_pid(Pid) andalso is_atom(Item) ->
    {Item, Info} = process_info(Pid, Item),
    Info;
pi(Node, Pid) when is_pid(Pid) ->
    rpc:call(Node, erlang, process_info, [Pid]).

pi(Node, Pid, Item) when is_pid(Pid) andalso is_atom(Item) ->
    rpc:call(Node, erlang, process_info, [Pid, Item]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    os:timestamp().


tdiff({A1, B1, C1} = _T1x, {A2, B2, C2} = _T2x) ->
    T1 = A1*1000000000+B1*1000+(C1 div 1000), 
    T2 = A2*1000000000+B2*1000+(C2 div 1000), 
    T2 - T1.


formated_timestamp() ->
    format_timestamp(os:timestamp()).

format_timestamp({_N1, _N2, _N3} = TS) ->
    {_Date, Time}   = calendar:now_to_local_time(TS),
    %% {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    %% FormatTS = 
    %%     io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w.~w",
    %%                   [YYYY, MM, DD, Hour, Min, Sec, N3]),  
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec]),  
    lists:flatten(FormatTS).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_support_ipv6() ->
    case socket:supports(ipv6) of
        true ->
            ok;
        false ->
            skip("IPv6 Not Supported")
    end,
    Domain = inet6,
    LocalAddr =
        case which_local_addr(Domain) of
            {ok, Addr} ->
                Addr;
            {error, R1} ->
                skip(f("Local Address eval failed: ~p", [R1]))
        end,
    ServerSock =
        case socket:open(Domain, dgram, udp) of
            {ok, SS} ->
                SS;
            {error, R2} ->
                skip(f("(server) socket open failed: ~p", [R2]))
        end,
    LocalSA = #{family => Domain, addr => LocalAddr},
    ServerPort =
        case socket:bind(ServerSock, LocalSA) of
            {ok, P1} ->
                P1;
            {error, R3} ->
                socket:close(ServerSock),
                skip(f("(server) socket bind failed: ~p", [R3]))
        end,
    ServerSA = LocalSA#{port => ServerPort},
    ClientSock =
        case socket:open(Domain, dgram, udp) of
            {ok, CS} ->
                CS;
            {error, R4} ->
                skip(f("(client) socket open failed: ~p", [R4]))
        end,
    case socket:bind(ClientSock, LocalSA) of
        {ok, _} ->
            ok;
        {error, R5} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            skip(f("(client) socket bind failed: ~p", [R5]))
    end,
    case socket:sendto(ClientSock, <<"hejsan">>, ServerSA) of
        ok ->
            ok;
        {error, R6} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            skip(f("failed socket sendto test: ~p", [R6]))
    end,
    case socket:recvfrom(ServerSock) of
        {ok, {_, <<"hejsan">>}} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            ok;
        {error, R7} ->
            socket:close(ServerSock),
            socket:close(ClientSock),
            skip(f("failed socket recvfrom test: ~p", [R7]))
   end.
            
            


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This gets the local address (not {127, _} or {0, ...} or {16#fe80, ...})
%% We should really implement this using the (new) net module,
%% but until that gets the necessary functionality...
which_local_addr(Domain) ->
    case which_local_host_info(Domain) of
        {ok, #{addr := Addr}} ->
            {ok, Addr};
        {error, _Reason} = ERROR ->
            ERROR
    end.
    

%% Returns the interface (name), flags and address (not 127...)
%% of the local host.
which_local_host_info(Domain) ->
    case inet:getifaddrs() of
        {ok, IFL} ->
            which_local_host_info(Domain, IFL);
        {error, _} = ERROR ->
            ERROR
    end.

which_local_host_info(_Domain, []) ->
    {error, no_address};
which_local_host_info(Domain, [{"lo" ++ _, _}|IFL]) ->
    which_local_host_info(Domain, IFL);
which_local_host_info(Domain, [{"docker" ++ _, _}|IFL]) ->
    which_local_host_info(Domain, IFL);
which_local_host_info(Domain, [{"br-" ++ _, _}|IFL]) ->
    which_local_host_info(Domain, IFL);
which_local_host_info(Domain, [{Name, IFO}|IFL]) ->
    try which_local_host_info2(Domain, IFO) of
        Info ->
            {ok, Info#{name => Name}}
    catch
        throw:_:_ ->
            which_local_host_info(Domain, IFL)
    end;
which_local_host_info(Domain, [_|IFL]) ->
    which_local_host_info(Domain, IFL).

%% which_local_host_info2(Domain, IFO) ->
%%     case lists:keysearch(flags, 1, IFO) of
%%         {value, {flags, Flags}} ->
%%             which_local_host_info2(Domain, IFO, Flags);
%%         false ->
%%             {error, no_flags}
%%     end.


%% which_local_host_info2(_Domain, [], _Flags) ->
%%     {error, no_address};
%% which_local_host_info2(inet = _Domain, [{addr, Addr}|_IFO], Flags)
%%   when (size(Addr) =:= 4) andalso (element(1, Addr) =/= 127) ->
%%     {ok, {Flags, Addr}};
%% which_local_host_info2(inet6 = _Domain, [{addr, Addr}|_IFO], Flags)
%%   when (size(Addr) =:= 8) andalso 
%%        (element(1, Addr) =/= 0) andalso
%%        (element(1, Addr) =/= 16#fe80) ->
%%     {ok, {Flags, Addr}};
%% which_local_host_info2(Domain, [_|IFO], Flags) ->
%%     which_local_host_info2(Domain, IFO, Flags).

%% foo(Info, inet = Domain, IFO) ->
%%     foo(Info, Domain, IFO, [flags, addr, netmask, broadaddr, hwaddr]);
%% foo(Info, inet6 = Domain, IFO) ->
%%     foo(Info, Domain, IFO, [flags, addr, netmask, hwaddr]).

which_local_host_info2(inet = _Domain, IFO) ->
    Addr      = which_local_host_info3(addr,  IFO,
                                       fun({A, _, _, _}) when (A =/= 127) -> true;
                                          (_) -> false
                                       end),
    NetMask   = which_local_host_info3(netmask,  IFO,
                                       fun({_, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    BroadAddr = which_local_host_info3(broadaddr,  IFO,
                                       fun({_, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    Flags     = which_local_host_info3(flags, IFO, fun(_) -> true end),
    #{flags     => Flags,
      addr      => Addr,
      broadaddr => BroadAddr,
      netmask   => NetMask};
which_local_host_info2(inet6 = _Domain, IFO) ->
    Addr    = which_local_host_info3(addr,  IFO,
                                     fun({A, _, _, _, _, _, _, _}) 
                                           when (A =/= 0) andalso 
                                                (A =/= 16#fe80) -> true;
                                        (_) -> false
                                     end),
    NetMask = which_local_host_info3(netmask,  IFO,
                                       fun({_, _, _, _, _, _, _, _}) -> true;
                                          (_) -> false
                                       end),
    Flags   = which_local_host_info3(flags, IFO, fun(_) -> true end),
    #{flags   => Flags,
      addr    => Addr,
      netmask => NetMask}.

which_local_host_info3(_Key, [], _) ->
    throw({error, no_address});
which_local_host_info3(Key, [{Key, Val}|IFO], Check) ->
    case Check(Val) of
        true ->
            Val;
        false ->
            which_local_host_info3(Key, IFO, Check)
    end;
which_local_host_info3(Key, [_|IFO], Check) ->
    which_local_host_info3(Key, IFO, Check).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    skip("not yet implemented").

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
