%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2023. All Rights Reserved.
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
         %% Process info
         pi/1, pi/2, pi/3,

         %% Proxy call
         pcall/3,

         %% OS commands
         os_cmd/1, os_cmd/2,

         %% Time stuff
         timestamp/0,
         tdiff/2,
         formated_timestamp/0,
         format_timestamp/1,

         %% String and format
         f/2,
         print/1, print/2,

         %% Generic 'has support' test function(s)
         has_support_ipv4/0,
         has_support_ipv6/0,

	 mk_unique_path/0,
         which_local_host_info/1,
         which_local_addr/1,

         %% Skipping
         not_yet_implemented/0,
         skip/1
        ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(LIB,     kernel_test_lib).
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

pcall(F, Timeout, Default) ->
    kernel_test_lib:proxy_call(F, Timeout, Default).


os_cmd(C) ->
    kernel_test_lib:os_cmd(C).

os_cmd(C, T) ->
    kernel_test_lib:os_cmd(C, T).


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
    {Hour,Min,Sec} = Time,
    FormatTS = io_lib:format("~.2.0w:~.2.0w:~.2.0w", [Hour, Min, Sec]),  
    lists:flatten(FormatTS).

   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).


print(F) ->
    print(F, []).

print(F, A) ->
    io:format("~s ~p " ++ F ++ "~n", [formated_timestamp(), self() | A]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_support_ipv4() ->
    case which_local_addr(inet) of
        {ok, _Addr} ->
            ok;
        {error, R1} ->
            skip(f("Local Address eval failed: ~p", [R1]))
    end.

has_support_ipv6() ->
    try
        begin
            case socket:is_supported(ipv6) of
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
                    ok ->
                        {ok, #{port := P1}} = socket:sockname(ServerSock),
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
                ok ->
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
            end
        end
    catch
        error : notsup ->
            skip("Not supported: socket")
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_unique_path() ->
    {OSF, _} = os:type(),
    mk_unique_path(OSF).
       
mk_unique_path(win32) ->
    [NodeName | _] = string:tokens(atom_to_list(node()), [$@]),
    Path = f("esock_~s_~w", [NodeName, erlang:system_time(nanosecond)]),
    ensure_unique_path(Path, ".sock");
mk_unique_path(_) ->
    [NodeName | _] = string:tokens(atom_to_list(node()), [$@]),
    Path = f("/tmp/esock_~s_~w", [NodeName, erlang:system_time(nanosecond)]),
    ensure_unique_path(Path, "").

ensure_unique_path(Path, Ext) ->
    NewPath = Path ++ Ext,
    case file:read_file_info(NewPath) of
        {ok, _} -> % Ouch, append a unique ID and try again
            ensure_unique_path(Path, Ext, 1);
        {error, _} ->
            %% We assume this means it does not exist yet...
            %% If we have several process in parallel trying to create
            %% (unique) path's, then we are in trouble. To *really* be
            %% on the safe side we should have a (central) path registry...
            encode_path(NewPath)
    end.

ensure_unique_path(Path, Ext, ID) when (ID < 100) -> % If this is not enough...
    NewPath = f("~s_~w", [Path, ID]) ++ Ext,
    case file:read_file_info(NewPath) of
        {ok, _} -> % Ouch, this also existed, increment and try again
            ensure_unique_path(Path, Ext, ID + 1);
        {error, _} -> % We assume this means it does not exist yet...
            encode_path(NewPath)
    end;
ensure_unique_path(_, _, _) -> 
    skip("Could not create unique path").

encode_path(Path) ->
    unicode:characters_to_binary(Path, file:native_name_encoding()).
            


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
    case ?LIB:which_local_host_info(Domain) of
        {ok, [H|_]} ->
	    {ok, H};
        {error, _} = ERROR ->
            ERROR
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_yet_implemented() ->
    skip("not yet implemented").

skip(Reason) ->
    exit({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
