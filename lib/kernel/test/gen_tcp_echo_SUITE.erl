%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(gen_tcp_echo_SUITE).

-include_lib("common_test/include/ct.hrl").

%%-compile(export_all).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 active_echo/1, passive_echo/1, active_once_echo/1,
	 slow_active_echo/1, slow_passive_echo/1,
	 limit_active_echo/1, limit_passive_echo/1,
	 large_limit_active_echo/1, large_limit_passive_echo/1]).

-define(TPKT_VRSN, 3).
-define(LINE_LENGTH, 1023). % (default value of gen_tcp option 'recbuf') - 1

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,5}}].

all() -> 
    [active_echo, passive_echo, active_once_echo,
     slow_active_echo, slow_passive_echo, limit_active_echo,
     limit_passive_echo, large_limit_active_echo,
     large_limit_passive_echo].

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


init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in active mode).
active_echo(Config) when is_list(Config) ->
    echo_test([], fun active_echo/4, [{echo, fun echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in passive mode).
passive_echo(Config) when is_list(Config) ->
    echo_test([{active, false}], fun passive_echo/4,
	      [{echo, fun echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in active once mode).
active_once_echo(Config) when is_list(Config) ->
    echo_test([{active, once}], fun active_once_echo/4,
	      [{echo, fun echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in active mode).
%% The echo server is a special one that delays between every character.
slow_active_echo(Config) when is_list(Config) ->
    echo_test([], fun active_echo/4,
	      [slow_echo, {echo, fun slow_echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to an echo server and receiving them again (socket in passive mode).
%% The echo server is a special one that delays between every character.
slow_passive_echo(Config) when is_list(Config) ->
    echo_test([{active, false}], fun passive_echo/4,
	      [slow_echo, {echo, fun slow_echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in active mode)
%% with packet_size limitation.
limit_active_echo(Config) when is_list(Config) ->
    echo_test([{packet_size, 10}],
	      fun active_echo/4,
	      [{packet_size, 10}, {echo, fun echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in passive mode)
%% with packet_size limitation.
limit_passive_echo(Config) when is_list(Config) ->
    echo_test([{packet_size, 10},{active, false}],
	      fun passive_echo/4,
	      [{packet_size, 10}, {echo, fun echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in active mode)
%% with large packet_size limitation.
large_limit_active_echo(Config) when is_list(Config) ->
    echo_test([{packet_size, 10}],
	      fun active_echo/4,
	      [{packet_size, (1 bsl 32)-1},
	       {echo, fun echo_server/0}]).

%% Test sending packets of various sizes and various packet types
%% to the echo port and receiving them again (socket in passive mode)
%% with large packet_size limitation.
large_limit_passive_echo(Config) when is_list(Config) ->
    echo_test([{packet_size, 10},{active, false}],
	      fun passive_echo/4,
	      [{packet_size, (1 bsl 32) -1},
	       {echo, fun echo_server/0}]).

echo_test(SockOpts, EchoFun, Config0) ->
    echo_test_1(SockOpts, EchoFun, Config0),
    io:format("\nrepeating test with {delay_send,true}"),
    echo_test_1([{delay_send,true}|SockOpts], EchoFun, Config0).

echo_test_1(SockOpts, EchoFun, Config0) ->
    EchoSrvFun = proplists:get_value(echo, Config0),
    {ok, EchoPort} = EchoSrvFun(),
    Config = [{echo_port, EchoPort}|Config0],

    echo_packet([{packet, 1}|SockOpts], EchoFun, Config),
    echo_packet([{packet, 2}|SockOpts], EchoFun, Config),
    echo_packet([{packet, 4}|SockOpts], EchoFun, Config),
    echo_packet([{packet, sunrm}|SockOpts], EchoFun, Config),
    echo_packet([{packet, cdr}|SockOpts], EchoFun,
		[{type, {cdr, big}}|Config]),
    echo_packet([{packet, cdr}|SockOpts], EchoFun,
		[{type, {cdr, little}}|Config]),
    case lists:keymember(packet_size, 1, SockOpts) of
	false ->
	    %% This is cheating, we should test that packet_size
	    %% also works for line and http.
	    echo_packet([{packet, line}|SockOpts], EchoFun, Config),
	    echo_packet([{packet, http}|SockOpts], EchoFun, Config),
	    echo_packet([{packet, http_bin}|SockOpts], EchoFun, Config);

	true -> ok
    end,
    echo_packet([{packet, tpkt}|SockOpts], EchoFun, Config),

    ShortTag = [16#E0],
    LongTag = [16#1F, 16#83, 16#27],
    echo_packet([{packet, asn1}|SockOpts], EchoFun,
		[{type, {asn1, short, ShortTag}}|Config]),
    echo_packet([{packet, asn1}|SockOpts], EchoFun,
		[{type, {asn1, long, ShortTag}}|Config]),
    echo_packet([{packet, asn1}|SockOpts], EchoFun,
		[{type, {asn1, short, LongTag}}|Config]),
    echo_packet([{packet, asn1}|SockOpts], EchoFun,
		[{type, {asn1, long, LongTag}}|Config]),
    ok.

echo_packet(SockOpts, EchoFun, Opts) ->
    Type = case lists:keysearch(type, 1, Opts) of
	       {value, {type, T}} ->
		   T;
	       _ ->
		   {value, {packet, T}} = lists:keysearch(packet, 1, SockOpts),
		   T
	   end,

    %% Connect to the echo server.
    EchoPort = proplists:get_value(echo_port, Opts),
    {ok, Echo} = gen_tcp:connect(localhost, EchoPort, SockOpts),

    SlowEcho = lists:member(slow_echo, Opts),

    case Type of
	http ->
	    echo_packet_http(Echo, Type, EchoFun);
	http_bin ->
	    echo_packet_http(Echo, Type, EchoFun);
	_ ->
	    echo_packet0(Echo, Type, EchoFun, SlowEcho, Opts)
    end.

echo_packet_http(Echo, Type, EchoFun) ->
    lists:foreach(fun(Uri)-> P1 = http_request(Uri),
			     EchoFun(Echo, Type, P1, http_reply(P1, Type))
		  end,
		  http_uri_variants()),
    P2 = http_response(),
    EchoFun(Echo, Type, P2, http_reply(P2, Type)).

echo_packet0(Echo, Type, EchoFun, SlowEcho, Opts) ->
    PacketSize =
	case lists:keysearch(packet_size, 1, Opts) of
	    {value,{packet_size,Sz}} when Sz < 10 -> Sz;
	    {value,{packet_size,_}} -> 10;
	    false -> 0
	end,
    %% Echo small packets first.
    echo_packet1(Echo, Type, EchoFun, 0),
    echo_packet1(Echo, Type, EchoFun, 1),
    echo_packet1(Echo, Type, EchoFun, 2),
    echo_packet1(Echo, Type, EchoFun, 3),
    echo_packet1(Echo, Type, EchoFun, 4),
    echo_packet1(Echo, Type, EchoFun, 7),
    if PacketSize =/= 0 ->
	    echo_packet1(Echo, Type, EchoFun,
			 {PacketSize-1, PacketSize}),
	    echo_packet1(Echo, Type, EchoFun,
			 {PacketSize, PacketSize}),
	    echo_packet1(Echo, Type, EchoFun,
			 {PacketSize+1, PacketSize});
       not SlowEcho -> % Go on with bigger packets if not slow echo server.
	    echo_packet1(Echo, Type, EchoFun, 10),
	    echo_packet1(Echo, Type, EchoFun, 13),
	    echo_packet1(Echo, Type, EchoFun, 126),
	    echo_packet1(Echo, Type, EchoFun, 127),
	    echo_packet1(Echo, Type, EchoFun, 128),
	    echo_packet1(Echo, Type, EchoFun, 255),
	    echo_packet1(Echo, Type, EchoFun, 256),
	    echo_packet1(Echo, Type, EchoFun, 1023),
	    echo_packet1(Echo, Type, EchoFun, 3747),
	    echo_packet1(Echo, Type, EchoFun, 32767),
	    echo_packet1(Echo, Type, EchoFun, 32768),
	    echo_packet1(Echo, Type, EchoFun, 65531),
	    echo_packet1(Echo, Type, EchoFun, 65535),
	    echo_packet1(Echo, Type, EchoFun, 65536),
	    echo_packet1(Echo, Type, EchoFun, 70000),
	    echo_packet1(Echo, Type, EchoFun, infinite);
       true -> ok
    end,
    gen_tcp:close(Echo),
    ok.

echo_packet1(EchoSock, Type, EchoFun, Size) ->
    case packet(Size, Type) of
	false ->
	    ok;
	Packet ->
	    io:format("Type ~p, size ~p, time ~p",
		      [Type, Size, time()]),
	    case EchoFun(EchoSock, Type, Packet, [Packet]) of
		ok ->
		    case Size of
			{N, Max} when N > Max ->
			    ct:fail(
			      {packet_through, {N, Max}});
			_ -> ok
		    end;
		{error, emsgsize} ->
		    case Size of
			{N, Max} when N > Max ->
			    io:format(" Blocked!");
			_ ->
			    ct:fail(
			      {packet_blocked, Size})
		    end;
		Error ->
		    ct:fail(Error)
	    end
    end.

active_echo(Sock, Type, Packet, PacketEchos) ->
    ok = gen_tcp:send(Sock, Packet),
    active_recv(Sock, Type, PacketEchos).

active_recv(_, _, []) ->
    ok;
active_recv(Sock, Type, [PacketEcho|Tail]) ->
    Tag = case Type of 
	      http -> http;
	      http_bin -> http;
	      _ -> tcp
	  end,
    receive Recv->Recv end,
    %%io:format("Active received: ~p\n",[Recv]),
    case Recv of
	{Tag, Sock, PacketEcho} ->
	    active_recv(Sock, Type, Tail);
	{Tag, Sock, Bad} ->
	    ct:fail({wrong_data, Bad, expected, PacketEcho});
	{tcp_error, Sock, Reason} ->
	    {error, Reason};
	Other ->
	    ct:fail({unexpected_message, Other, Tag})
    end.

passive_echo(Sock, _Type, Packet, PacketEchos) ->
    ok = gen_tcp:send(Sock, Packet),
    passive_recv(Sock, PacketEchos).

passive_recv(_, []) ->
    ok;
passive_recv(Sock, [PacketEcho | Tail]) ->
    Recv = gen_tcp:recv(Sock, 0),
    %%io:format("Passive received: ~p\n",[Recv]),
    case Recv of
	{ok, PacketEcho} ->
	    passive_recv(Sock, Tail);
	{ok, Bad} ->
	    io:format("Expected: ~p\nGot: ~p\n",[PacketEcho,Bad]),
	    ct:fail({wrong_data, Bad});
	{error,PacketEcho} ->
	    passive_recv(Sock, Tail); % expected error
	{error, _}=Error ->
	    Error;
	Other ->
	    ct:fail({unexpected_message, Other})
    end.

active_once_echo(Sock, Type, Packet, PacketEchos) ->
    ok = gen_tcp:send(Sock, Packet),
    active_once_recv(Sock, Type, PacketEchos).

active_once_recv(_, _, []) ->
    ok;
active_once_recv(Sock, Type, [PacketEcho | Tail]) ->
    Tag = case Type of
	      http -> http;
	      http_bin -> http;
	      _ -> tcp
	  end,
    receive
	{Tag, Sock, PacketEcho} ->
	    inet:setopts(Sock, [{active, once}]),
	    active_once_recv(Sock, Type, Tail);
	{Tag, Sock, Bad} ->
	    ct:fail({wrong_data, Bad});
	{tcp_error, Sock, Reason} ->
	    {error, Reason};
	Other ->
	    ct:fail({unexpected_message, Other, expected, {Tag, Sock, PacketEcho}})
    end.

%%% Building of random packets.

packet(infinite, {asn1, _, Tag}) ->
    Tag++[16#80];
packet(infinite, _) ->
    false;
packet({Size, _RecvLimit}, Type) ->
    packet(Size, Type);
packet(Size, 1) when Size > 255 ->
    false;
packet(Size, 2) when Size > 65535 ->
    false;
packet(Size, {asn1, _, Tag}) when Size < 128 ->
    Tag++[Size|random_packet(Size)];
packet(Size, {asn1, short, Tag}) when Size < 256 ->
    Tag++[16#81, Size|random_packet(Size)];
packet(Size, {asn1, short, Tag}) when Size < 65536 ->
    Tag++[16#82|put_int16(Size, big, random_packet(Size))];
packet(Size, {asn1, _, Tag}) ->
    Tag++[16#84|put_int32(Size, big, random_packet(Size))];
packet(Size, {cdr, Endian}) ->
    [$G, $I, $O, $P,				% magic
     1, 0,					% major minor
     if Endian == big -> 0; true -> 1 end,	% flags: byte order
     0 |					% message type
     put_int32(Size, Endian, random_packet(Size))];
packet(Size, sunrm) ->
    put_int32(Size, big, random_packet(Size));
packet(Size, line) when Size > ?LINE_LENGTH ->
    false;
packet(Size, line) ->
    random_packet(Size, "\n");
packet(Size, tpkt) ->
    HeaderSize = 4,
    PacketSize = HeaderSize + Size,
    if PacketSize < 65536 ->
	    Header = [?TPKT_VRSN, 0 | put_int16(PacketSize, big)],
	    HeaderSize = length(Header), % Just to assert cirkular dependency
	    Header ++ random_packet(Size);
       true ->
	    false
    end;
packet(Size, _Type) ->
    random_packet(Size).



random_packet(Size) ->
    random_packet(Size, "", random_char()).

random_packet(Size, Tail) ->
    random_packet(Size, Tail, random_char()).

random_packet(0, Result, _NextChar) ->
    Result;
random_packet(Left, Result, NextChar0) ->
    NextChar =
	if
	    NextChar0 >= 126 ->
		33;
	    true ->
		NextChar0+1
	end,
    random_packet(Left-1, [NextChar0|Result], NextChar).

random_char() ->
    random_char("abcdefghijklmnopqrstuvxyzABCDEFGHIJKLMNOPQRSTUVXYZ0123456789").

random_char(Chars) ->
    lists:nth(uniform(length(Chars)), Chars).

uniform(N) ->
    rand:uniform(N).

put_int32(X, big, List) ->
    [ (X bsr 24) band 16#ff, 
      (X bsr 16) band 16#ff,
      (X bsr 8) band 16#ff,
      (X) band 16#ff | List ];
put_int32(X, little, List) ->
    [ (X) band 16#ff,
      (X bsr 8) band 16#ff,
      (X bsr 16) band 16#ff,
      (X bsr 24) band 16#ff | List].

put_int16(X, ByteOrder) ->
    put_int16(X, ByteOrder, []).

put_int16(X, big, List) ->
    [ (X bsr 8) band 16#ff,
      (X) band 16#ff | List ];
put_int16(X, little, List) ->
    [ (X) band 16#ff,
      (X bsr 8) band 16#ff | List ].

%%% A normal echo server, for systems that don't have one.

echo_server() ->
    Self = self(),
    spawn_link(fun() -> echo_server(Self) end),
    receive
	{echo_port, Port} ->
	    {ok, Port}
    end.

echo_server(ReplyTo) ->
    {ok, S} = gen_tcp:listen(0, [{active, false}, binary]),
    {ok, {_, Port}} = inet:sockname(S),
    ReplyTo ! {echo_port, Port},
    echo_server_loop(S).

echo_server_loop(Sock) ->
    {ok, E} = gen_tcp:accept(Sock),
    Self = self(),
    spawn_link(fun() -> echoer(E, Self) end),
    echo_server_loop(Sock).

echoer(Sock, Parent) ->
    unlink(Parent),
    echoer_loop(Sock).

echoer_loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    ok = gen_tcp:send(Sock, Data),
	    echoer_loop(Sock);
	{error, closed} ->
	    ok
    end.

%%% A "slow" echo server, which will echo data with a short delay
%%% between each character.

slow_echo_server() ->
    Self = self(),
    spawn_link(fun() -> slow_echo_server(Self) end),
    receive
	{echo_port, Port} ->
	    {ok, Port}
    end.

slow_echo_server(ReplyTo) ->
    {ok, S} = gen_tcp:listen(0, [{active, false}, {nodelay, true}]),
    {ok, {_, Port}} = inet:sockname(S),
    ReplyTo ! {echo_port, Port},
    slow_echo_server_loop(S).

slow_echo_server_loop(Sock) ->
    {ok, E} = gen_tcp:accept(Sock),
    spawn_link(fun() -> slow_echoer(E, self()) end),
    slow_echo_server_loop(Sock).

slow_echoer(Sock, Parent) ->
    unlink(Parent),
    slow_echoer_loop(Sock).

slow_echoer_loop(Sock) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    slow_send(Sock, Data),
	    slow_echoer_loop(Sock);
	{error, closed} ->
	    ok
    end.

slow_send(Sock, [C|Rest]) ->
    ok = gen_tcp:send(Sock, [C]),
    receive after 1 ->
		    slow_send(Sock, Rest)
	    end;
slow_send(_, []) ->
    ok.

http_request(Uri) ->
    list_to_binary(["POST ", Uri, <<" HTTP/1.1\r\n"
				    "Connection: close\r\n"
				    "Host: localhost:8000\r\n"
				    "User-Agent: perl post\r\n"
				    "Content-Length: 4\r\n"
				    "Content-Type: text/xml; charset=utf-8\r\n"
				    "Other-Field: with some text\r\n"
				    "Multi-Line: Once upon a time in a land far far away,\r\n"
				    " there lived a princess imprisoned in the highest tower\r\n"
				    " of the most haunted castle.\r\n"
				    "Invalid line without a colon\r\n"
				    "\r\n">>]).

http_uri_variants() ->
    ["*",
     "http://tools.ietf.org/html/rfcX3986",
     "http://otp.ericsson.se:8000/product/internal/",
     "https://example.com:8042/over/there?name=ferret#nose",
     "ftp://cnn.example.com&story=breaking_news@10.0.0.1/top_story.htm",
     "/some/absolute/path",
     "something_else", "something_else"].

http_response() ->
    <<"HTTP/1.0 404 Object Not Found\r\n"
      "Server: inets/4.7.16\r\n"
      "Date: Fri, 04 Jul 2008 17:16:22 GMT\r\n"
      "Content-Type: text/html\r\n"
      "Content-Length: 207\r\n"
      "\r\n">>.

http_reply(Bin, Type) ->
    {ok, Line, Rest} = erlang:decode_packet(Type,Bin,[]),
    HType = case Type of
		http -> httph;
		http_bin -> httph_bin
	    end,
    Ret = lists:reverse(http_reply(Rest,[Line],HType)),
    io:format("HTTP: ~p\n",[Ret]),
    Ret.

http_reply(<<>>, Acc, _) ->
    Acc;
http_reply(Bin, Acc, HType) ->
    {ok, Line, Rest} = erlang:decode_packet(HType,Bin,[]),	
    http_reply(Rest, [Line | Acc], HType).
