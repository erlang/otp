%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(epmd_SUITE).
-include("test_server.hrl").
-include_lib("kernel/include/file.hrl").


% Timeout for test cases (rather long to work on slow machines)
-define(SHORT_TEST_TIMEOUT, ?t:seconds(30)).	% Default
-define(MEDIUM_TEST_TIMEOUT, ?t:minutes(3)).
-define(LONG_TEST_TIMEOUT, ?t:minutes(10)).

% Delay inserted into code
-define(SHORT_PAUSE, 100).
-define(MEDIUM_PAUSE, ?t:seconds(1)).
-define(LONG_PAUSE, ?t:seconds(5)).

% Test server specific exports
-export([all/1, init_per_testcase/2, fin_per_testcase/2]).

-export(
   [
    register_name/1,
    register_names_1/1,
    register_names_2/1,
    register_duplicate_name/1,
    get_port_nr/1,
    slow_get_port_nr/1,
    unregister_others_name_1/1,
    unregister_others_name_2/1,
    register_overflow/1,
    name_with_null_inside/1,
    name_null_terminated/1,
    stupid_names_req/1,

    no_data/1,
    one_byte/1,
    two_bytes/1,
    partial_packet/1,
    zero_length/1,
    too_large/1,
    alive_req_too_small_1/1,
    alive_req_too_small_2/1,
    alive_req_too_large/1
   ]).


% Port we use for testing
-define(PORT,2243).
-define(EPMDARGS,"-packet_timeout 1").

-define(DUMMY_PORT, 1000).			% Port number to register
						% not in real use.

% Timeouts etc inside test cases. Time is in milliseconds.
-define(CONN_RETRY, 4).				% Times to retry connecting
-define(CONN_SLEEP, 500).
-define(CONN_TIMEOUT, 100).
-define(RECV_TIMEOUT, 2000).
-define(REG_REPEAT_LIM,1000).

% Message codes in epmd protocol
-define(EPMD_ALIVE_REQ,	$a).
-define(EPMD_ALIVE_OK_RESP, $Y).
-define(EPMD_PORT_REQ,	$p).
-define(EPMD_NAMES_REQ,	$n).
-define(EPMD_DUMP_REQ,	$d).
-define(EPMD_KILL_REQ,	$k).
-define(EPMD_STOP_REQ,	$s).

%%
%% all/1
%%

all(suite) ->
    [
     register_name,
     register_names_1,
     register_names_2,
     register_duplicate_name,
     get_port_nr,
     slow_get_port_nr,
     unregister_others_name_1,
     unregister_others_name_2,
     register_overflow,
     name_with_null_inside,
     name_null_terminated,
     stupid_names_req,

     no_data,
     one_byte,
     two_bytes,
     partial_packet,
     zero_length,
     too_large,
     alive_req_too_small_1,
     alive_req_too_small_2,
     alive_req_too_large
    ].

%%
%% Run before and after each test case
%%

init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(?SHORT_TEST_TIMEOUT),
    cleanup(),
    [{watchdog, Dog} | Config].

fin_per_testcase(_Func, Config) ->
    cleanup(),
    Dog = ?config(watchdog, Config),
    catch test_server:timetrap_cancel(Dog),	% We may have canceled already
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_name(doc) ->
    ["Register a name"];
register_name(suite) ->
    [];
register_name(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = register_node("foobar"),
    ?line ok = close(Sock),			% Unregister
    ok.

register_names_1(doc) ->
    ["Register and unregister two nodes"];
register_names_1(suite) ->
    [];
register_names_1(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock1} = register_node("foobar"),
    ?line {ok,Sock2} = register_node("foozap"),
    ?line ok = close(Sock1),			% Unregister
    ?line ok = close(Sock2),			% Unregister
    ok.

register_names_2(doc) ->
    ["Register and unregister two nodes"];
register_names_2(suite) ->
    [];
register_names_2(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock1} = register_node("foobar"),
    ?line {ok,Sock2} = register_node("foozap"),
    ?line ok = close(Sock2),			% Unregister
    ?line ok = close(Sock1),			% Unregister
    ok.

register_duplicate_name(doc) ->
    ["Two nodes with the same name"];
register_duplicate_name(suite) ->
    [];
register_duplicate_name(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = register_node("foobar"),
    ?line error = register_node("foobar"),
    ?line ok = close(Sock),			% Unregister
    ok.

% Internal function to register a node name, no close, i.e. unregister

register_node(Name) ->
    register_node(Name,?DUMMY_PORT).

register_node(Name, Port) ->
    case connect() of
	{ok,Sock} ->
	    M = [?EPMD_ALIVE_REQ, put16(Port), Name],
	    case send(Sock, [size16(M), M]) of
		ok ->
		    case recv(Sock,3) of
			{ok, [?EPMD_ALIVE_OK_RESP,_D1,_D0]} ->
			    {ok,Sock};
			Other ->
			    test_server:format("recv on sock ~w: ~p~n",
					       [Sock,Other]),
			    error
		    end;
		Other ->
		    test_server:format("send on sock ~w: ~w~n",[Sock,Other]),
		    error
	    end;
	Other ->
	    test_server:format("Connect on port ~w: ~p~n",[Port,Other]),
	    error
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_with_null_inside(doc) ->
    ["Register a name with a null char in it"];
name_with_null_inside(suite) ->
    [];
name_with_null_inside(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line error = register_node("foo\000bar"),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

name_null_terminated(doc) ->
    ["Register a name with terminating null byte"];
name_null_terminated(suite) ->
    [];
name_null_terminated(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = register_node("foobar\000"),
    ?line error = register_node("foobar"),
    ?line ok = close(Sock),			% Unregister
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stupid_names_req(doc) ->
    ["Read names from epmd in a stupid way"];
stupid_names_req(suite) ->
    [];
stupid_names_req(Config) when list(Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    LongDog = test_server:timetrap(?MEDIUM_TEST_TIMEOUT),
    ?line ok = epmdrun(),
    ?line [FirstConn | Conn] = register_many(1, ?REG_REPEAT_LIM, "foo"),
    ?line unregister_many([FirstConn]),
    sleep(?MEDIUM_PAUSE),
    ?line ok = check_names(Conn),
    ?line ok = unregister_many(Conn),
    test_server:timetrap_cancel(LongDog),
    ok.

check_names(Conn) ->
    ?line {ok,Sock} = connect_active(),
    ?line {ok,Reply} = do_get_names(Sock),
    ?line SortConn  = lists:sort(Conn),
    ?line SortReply = lists:sort(Reply),
    ?line ok = check_names_cmp(SortConn, SortReply),
    ok.
    

% Compare if the result was the same as was registered

check_names_cmp([], []) ->
    ok;
check_names_cmp([{Name,Port,_Sock} | Conn], [{Name,Port} | Reply]) ->
    check_names_cmp(Conn, Reply).


% This code is taken directly from "erl_epmd.erl" in R3A01

-define(int16(X), [(X bsr 8) band 16#ff, X band 16#ff]).
-define(u32(X1,X2,X3,X4), 
	(((X1) bsl 24) bor ((X2) bsl 16) bor ((X3) bsl 8) bor X4)).

do_get_names(Socket) ->
    inet_tcp:send(Socket, [?int16(1),?EPMD_NAMES_REQ]),
    receive
	{tcp, Socket, [P0,P1,P2,P3 | T]} ->
	    EpmdPort = ?u32(P0,P1,P2,P3),
	    if EpmdPort == ?PORT ->
		    names_loop(Socket, T, []);
	       true ->
		    close(Socket),
		    {error, address}
	    end;
	{tcp_closed, Socket} ->
	    {ok, []}
    end.

names_loop(Socket, Acc, Ps) ->
    receive
	{tcp, Socket, Bytes} ->
	    {NAcc, NPs} = scan_names(Acc ++ Bytes, Ps),
	    names_loop(Socket, NAcc, NPs);
	{tcp_closed, Socket} ->
	    {_, NPs} = scan_names(Acc, Ps),	% Really needed?
	    {ok, NPs}
    end.

scan_names(Buf, Ps) ->
    case scan_line(Buf, []) of
	{Line, NBuf} ->
	    case parse_line(Line) of
		{ok, Entry} -> 
		    scan_names(NBuf, [Entry | Ps]);
		error ->
		    scan_names(NBuf, Ps)
	    end;
	[] -> {Buf, Ps}
    end.

scan_line([$\n | Buf], Line) -> {lists:reverse(Line), Buf};
scan_line([C | Buf], Line) -> scan_line(Buf, [C|Line]);
scan_line([], _) -> [].

parse_line([$n,$a,$m,$e,$ | Buf0]) ->
    case parse_name(Buf0, []) of
	{Name, Buf1}  ->
	    case Buf1 of
		[$a,$t,$ ,$p,$o,$r,$t,$ | Buf2] ->
		    case catch list_to_integer(Buf2) of
			{'EXIT', _} -> error;
			Port -> {ok, {Name, Port}}
		    end;
		_ -> error
	    end;
	error -> error
    end;
parse_line(_) -> error.


parse_name([$  | Buf], Name) -> {lists:reverse(Name), Buf};
parse_name([C | Buf], Name) -> parse_name(Buf, [C|Name]);
parse_name([], _Name) -> error.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_port_nr(doc) ->
    ["Register a name on a port and ask about port nr"];
get_port_nr(suite) ->
    [];
get_port_nr(Config) when list(Config) ->
    port_request([?EPMD_PORT_REQ,"foo"]).

slow_get_port_nr(doc) ->
    ["Register with slow write and ask about port nr"];
slow_get_port_nr(suite) ->
    [];
slow_get_port_nr(Config) when list(Config) ->
    port_request([?EPMD_PORT_REQ,d,$f,d,$o,d,$o]).


% Internal function used above

port_request(M) ->
    ?line ok = epmdrun(),
    Port = 1042,
    ?line {ok,RSock} = register_node("foo", Port),
    ?line {ok,Sock} = connect(),
    ?line ok = send(Sock,[size16(M),M]),
    R = put16(Port),
    ?line {ok,R} = recv(Sock, length(R)),
    ?line ok = close(RSock),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unregister_others_name_1(doc) ->
    ["Unregister name of other node"];
unregister_others_name_1(suite) ->
    [];
unregister_others_name_1(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,RSock} = register_node("foo"),
    ?line {ok,Sock} = connect(),
    M = [?EPMD_STOP_REQ,"foo"],
    ?line ok = send(Sock,[size16(M),M]),
    R = "STOPPED",
    ?line {ok,R} = recv(Sock,length(R)),
    ?line ok = close(RSock),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unregister_others_name_2(doc) ->
    ["Unregister name of other node"];
unregister_others_name_2(suite) ->
    [];
unregister_others_name_2(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    M = [?EPMD_STOP_REQ,"xxx42"],
    ?line ok = send(Sock,[size16(M),M]),
    R = "NOEXIST",
    ?line {ok,R} = recv(Sock,length(R)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_overflow(doc) ->
    ["Register too many, clean and redo 10 times"];
register_overflow(suite) ->
    [];
register_overflow(Config) when list(Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    LongDog = test_server:timetrap(?LONG_TEST_TIMEOUT),
    ?line ok = epmdrun(),
    ?line Conn = register_many(1, ?REG_REPEAT_LIM, "foo"),
    Count = length(Conn),
    ?line ok = unregister_many(Conn),
    sleep(?MEDIUM_PAUSE),
    test_server:format("Limit was ~w names, now reg/unreg all 10 times~n",
		       [Count]),
    ?line ok = register_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = rregister_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = register_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = rregister_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = register_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = rregister_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = register_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = rregister_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = register_repeat(Count),
    sleep(?MEDIUM_PAUSE),
    ?line ok = rregister_repeat(Count),
    test_server:timetrap_cancel(LongDog),
    ok.

register_repeat(Count) ->
    Conn = register_many(1, ?REG_REPEAT_LIM, "foo"),
    ok = unregister_many(Conn),
    if
	length(Conn) == Count ->
	    ok;
	true ->
	    error
    end.

rregister_repeat(Count) ->
    Conn = register_many(1, ?REG_REPEAT_LIM, "foo"),
    ok = unregister_many(lists:reverse(Conn)),
    if
	length(Conn) == Count ->
	    ok;
	true ->
	    error
    end.

% Return count of successful registrations

register_many(I, N, _Prefix) when I > N ->
    test_server:format("Done with all ~n", []),
    [];
register_many(I, N, Prefix) ->
    Name = gen_name(Prefix, I),
    Port = ?DUMMY_PORT + I,				% Just make it up
    case register_node(Name, Port) of
	{ok,Sock} ->
	    [{Name,Port,Sock} | register_many(I + 1, N, Prefix)];
	Any ->
	    test_server:format("Can't register: ~w of 1..~w ~w~n", 
			       [Name,N,Any]),
	    []
    end.

unregister_many([]) ->
    ok;
unregister_many([{Name,_Port,Sock} | Socks]) ->
    case close(Sock) of
	ok ->
	    unregister_many(Socks);
	Any ->
	    test_server:format("Can't unregister: ~w reason ~w~n", [Name,Any]),
	    error
    end.

gen_name(Str,Int) ->
    Str ++ integer_to_list(Int).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

no_data(doc) ->
    ["Open but send no data"];
no_data(suite) ->
    [];
no_data(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    sleep(?LONG_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

one_byte(doc) ->
    ["Send one byte only"];
one_byte(suite) ->
    [];
one_byte(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    ?line ok = send(Sock,[0]),
    sleep(?LONG_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

two_bytes(doc) ->
    ["Send packet size only"];
two_bytes(suite) ->
    [];
two_bytes(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    ?line ok = send(Sock,[put16(3)]),
    sleep(?LONG_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

partial_packet(doc) ->
    ["Got only part of a packet"];
partial_packet(suite) ->
    [];
partial_packet(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    ?line ok = send(Sock,[put16(100),"only a few bytes"]),
    sleep(?LONG_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zero_length(doc) ->
    ["Invalid zero packet size"];
zero_length(suite) ->
    [];
zero_length(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    ?line ok = send(Sock,[0,0,0,0,0,0,0,0,0,0]),
    sleep(?MEDIUM_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

too_large(doc) ->
    ["Invalid large packet"];
too_large(suite) ->
    [];
too_large(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    Size = 63000,
    M = lists:duplicate(Size, $z),
    ?line ok = send(Sock,[put16(Size),M]),
    sleep(?MEDIUM_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alive_req_too_small_1(doc) ->
    ["Try to register but not enough data"];
alive_req_too_small_1(suite) ->
    [];
alive_req_too_small_1(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    M = [?EPMD_ALIVE_REQ, 42],
    ?line ok = send(Sock, [size16(M), M]),
    sleep(?MEDIUM_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alive_req_too_small_2(doc) ->
    ["Try to register but not enough data"];
alive_req_too_small_2(suite) ->
    [];
alive_req_too_small_2(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    M = [?EPMD_ALIVE_REQ, put16(?DUMMY_PORT)],
    ?line ok = send(Sock, [size16(M), M]),
    sleep(?MEDIUM_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alive_req_too_large(doc) ->
    ["Try to register but node name too large"];
alive_req_too_large(suite) ->
    [];
alive_req_too_large(Config) when list(Config) ->
    ?line ok = epmdrun(),
    ?line {ok,Sock} = connect(),
    L = [
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
	 "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
	 ],
    M = [?EPMD_ALIVE_REQ, put16(?DUMMY_PORT), L],
    ?line ok = send(Sock, [size16(M), M]),
    sleep(?MEDIUM_PAUSE),
    ?line closed = recv(Sock,1),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminate all tests with killing epmd.

cleanup() ->
    sleep(?MEDIUM_PAUSE),
    case connect() of
	{ok,Sock} ->
	    M = [?EPMD_KILL_REQ],
	    send(Sock, [size16(M), M]),
	    recv(Sock,length("OK")),
	    close(Sock),
	    sleep(?MEDIUM_PAUSE);
	_ ->
	    true
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Normal debug start of epmd

epmdrun() ->
    case os:find_executable(epmd) of
	false ->
	    {error, {could_not_find_epmd_in_path}};
	Path ->
	    epmdrun(Path)
    end.

epmdrun(Epmd) ->
  %% test_server:format("epmdrun() => Epmd = ~p",[Epmd]),
  osrun(Epmd ++ " " ?EPMDARGS " -port " ++ integer_to_list(?PORT)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Start an external process

osrun(Cmd) ->
    _ = open_port({spawn, Cmd}, []),
    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Wrappers of TCP functions

% These two functions is the interface for connect.
% Passive mode is the default

connect() ->
    connect(?PORT, passive).

connect_active() ->
    connect(?PORT, active).


% Try a few times before giving up

connect(Port, Mode) ->
    case connect_repeat(?CONN_RETRY, Port, Mode) of
	{ok,Sock} ->
	    {ok,Sock};
	{error,timeout} ->
	    timeout;
	{error,Reason} ->
	    test_server:format("connect: error: ~w~n",[Reason]),
	    error;
	Any ->
	    test_server:format("connect: unknown message: ~w~n",[Any]),
	    exit(1)
    end.


% Try a few times before giving up. Pause a small time between
% each try.

connect_repeat(1, Port, Mode) ->
    connect_mode(Port, Mode);
connect_repeat(Retry, Port, Mode) ->
    case connect_mode(Port, Mode) of
	{ok,Sock} ->
	    {ok,Sock};
	{error,Reason} ->
	    test_server:format("connect: error: ~w~n",[Reason]),
	    timer:sleep(?CONN_SLEEP),
	    connect_repeat(Retry - 1, Port, Mode);
	Any ->
	    test_server:format("connect: unknown message: ~w~n",[Any]),
	    exit(1)
    end.

connect_mode(Port, active) ->
    gen_tcp:connect("localhost", Port, [{packet, 0}], ?CONN_TIMEOUT);
connect_mode(Port, passive) ->
    gen_tcp:connect("localhost", Port, [{packet, 0}, {active, false}],
		    ?CONN_TIMEOUT).


close(Sock) ->
    case gen_tcp:close(Sock) of
	{error,_} ->
	    error;
	ok ->
	    ok;
	Any ->
	    test_server:format("unknown message: ~w~n",[Any]),
	    exit(1)
    end.

recv(Sock, Len) ->
    recv(Sock, Len, ?RECV_TIMEOUT).

recv(Sock, Len, Timeout) ->
    case gen_tcp:recv(Sock, Len, Timeout) of
	{ok,[]} ->				% Should not be the case
	    recv(Sock, 1, 1);			% any longer
	{ok,Data} ->
	    {ok,Data};
	{error,timeout} ->
	    timeout;
	{error,closed} ->
	    closed;
	{error,_}=Error ->
	    Error;
	Any ->
	    test_server:format("unknown message: ~w~n",[Any]),
	    exit(1)
    end.

%% Send data to socket. The list can be non flat and contain
%% the atom 'd' or tuple {d,Seconds} where this is delay
%% put in between the sent characters.

send(Sock, SendSpec) ->
    case send(SendSpec, [], Sock) of
	{ok,[]} ->
	    ok;
	{ok,RevBytes} ->
	    send_direct(Sock, lists:reverse(RevBytes));
	Any ->
	    Any
    end.


% If an error, return immediately
% Collect real characters in the first argument to form
% a string to send. Only perform "actions", like a delay,
% when this argument is empty.

send([], RevBytes, _Sock) ->
    {ok,RevBytes};
send([Byte | Spec], RevBytes, Sock) when integer(Byte) ->
    send(Spec, [Byte | RevBytes], Sock);
send([List | Spec], RevBytes, Sock) when list(List) ->
    case send(List, RevBytes, Sock) of
	{ok,Left} ->
	    send(Spec, Left, Sock);
	Other ->
	    Other
    end;
send([d | Spec], RevBytes, Sock) ->
    send([{d,1000} | Spec], RevBytes, Sock);
send([{d,S} | Spec], RevBytes, Sock) ->
    case send_direct(Sock, lists:reverse(RevBytes)) of
	ok ->
	    timer:sleep(S),
	    send(Spec, [], Sock);
	Any ->
	    Any
    end.

%%%%

send_direct(Sock, Bytes) ->
    case gen_tcp:send(Sock, Bytes) of
	ok ->
	    ok;
	{error, closed} ->
	    closed;
	{error, _Reason} ->
	    error;
	Any ->
	    test_server:format("unknown message: ~w~n",[Any]),
	    Any
    end.

sleep(MilliSeconds) ->
    timer:sleep(MilliSeconds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put16(N) ->
    [N bsr 8, N band 16#ff].

size16(List) ->
    N = flat_count(List, 0),
    [N bsr 8, N  band 16#ff].

flat_count([H|T], N) when is_integer(H) ->
    flat_count(T, N+1);
flat_count([H|T], N) when is_list(H) ->
    flat_count(T, flat_count(H, N));
flat_count([_|T], N) ->
    flat_count(T, N);
flat_count([], N) -> N.
    
