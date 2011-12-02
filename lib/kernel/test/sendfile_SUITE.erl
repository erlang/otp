%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2011. All Rights Reserved.
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

-module(sendfile_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-compile(export_all).

all() ->
    [t_sendfile_small
     ,t_sendfile_big
     ,t_sendfile_partial
     ,t_sendfile_offset
     ,t_sendfile_sendafter
     ,t_sendfile_recvafter
     ,t_sendfile_sendduring
     ,t_sendfile_recvduring
    ].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    SFilename = filename:join(Priv, "sendfile_small.html"),
    {ok, DS} = file:open(SFilename,[write,raw]),
    file:write(DS,"yo baby yo"),
    file:sync(DS),
    file:close(DS),
    BFilename = filename:join(Priv, "sendfile_big.html"),
    {ok, DB} = file:open(BFilename,[write,raw]),
    [file:write(DB,[<<0:(10*8*1024*1024)>>]) || _I <- lists:seq(1,51)],
    file:sync(DB),
    file:close(DB),
    [{small_file, SFilename},
     {file_opts,[raw,binary]},
     {big_file, BFilename}|Config].

end_per_suite(Config) ->
    file:delete(proplists:get_value(big_file, Config)).

init_per_testcase(TC,Config) when TC == t_sendfile_recvduring;
				  TC == t_sendfile_sendduring ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {_Size, Data} = sendfile_file_info(Filename),
		   {ok,D} = file:open(Filename, [raw,binary,read]),
		   prim_file:sendfile(D, Sock, 0, 0, 0,
				      [],[],false,false,false),
		   Data
	   end,

    %% Check if sendfile is supported on this platform
    case catch sendfile_send(Send) of
	ok ->
	    Config;
	Error ->
	    ct:log("Error: ~p",[Error]),
	    {skip,"Not supported"}
    end;
init_per_testcase(_Tc,Config) ->
    Config.


t_sendfile_small(Config) when is_list(Config) ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok, Size} = file:sendfile(Filename, Sock),
		   Data
	   end,

    ok = sendfile_send(Send).

t_sendfile_big(Config) when is_list(Config) ->
    Filename = proplists:get_value(big_file, Config),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   {ok, Size} = file:sendfile(Filename, Sock),
		   Size
	   end,

    ok = sendfile_send("localhost", Send, 0).

t_sendfile_partial(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),

    SendSingle = fun(Sock) ->
			 {_Size, <<Data:5/binary,_/binary>>} =
			     sendfile_file_info(Filename),
			 {ok,D} = file:open(Filename,[read|FileOpts]),
			 {ok,5} = file:sendfile(D,Sock,0,5,[]),
			 file:close(D),
			 Data
		 end,
    ok = sendfile_send(SendSingle),

    {_Size, <<FData:5/binary,SData:3/binary,_/binary>>} =
	sendfile_file_info(Filename),
    {ok,D} = file:open(Filename,[read|FileOpts]),
    {ok, <<FData/binary>>} = file:read(D,5),
    FSend = fun(Sock) ->
		    {ok,5} = file:sendfile(D,Sock,0,5,[]),
		    FData
	    end,

    ok = sendfile_send(FSend),

    SSend = fun(Sock) ->
		    {ok,3} = file:sendfile(D,Sock,5,3,[]),
		    SData
	    end,

    ok = sendfile_send(SSend),

    {ok, <<SData/binary>>} = file:read(D,3),

    file:close(D).

t_sendfile_offset(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),

    Send = fun(Sock) ->
		   {_Size, <<_:5/binary,Data:3/binary,_/binary>> = AllData} =
		       sendfile_file_info(Filename),
		   {ok,D} = file:open(Filename,[read|FileOpts]),
		   {ok,3} = file:sendfile(D,Sock,5,3,[]),
		   {ok, AllData} = file:read(D,100),
		   file:close(D),
		   Data
	   end,
    ok = sendfile_send(Send).


t_sendfile_sendafter(Config) ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok, Size} = file:sendfile(Filename, Sock),
		   ok = gen_tcp:send(Sock, <<2>>),
		   <<Data/binary,2>>
	   end,

    ok = sendfile_send(Send).

t_sendfile_recvafter(Config) ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok, Size} = file:sendfile(Filename, Sock),
		   ok = gen_tcp:send(Sock, <<1>>),
		   {ok,<<1>>} = gen_tcp:recv(Sock, 1),
		   <<Data/binary,1>>
	   end,

    ok = sendfile_send(Send).

t_sendfile_sendduring(Config) ->
    Filename = proplists:get_value(big_file, Config),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   spawn_link(fun() ->
				      timer:sleep(10),
				      ok = gen_tcp:send(Sock, <<2>>)
			      end),
		   {ok, Size} = file:sendfile(Filename, Sock),
		   Size+1
	   end,

    ok = sendfile_send("localhost", Send, 0).

t_sendfile_recvduring(Config) ->
    Filename = proplists:get_value(big_file, Config),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   spawn_link(fun() ->
				      timer:sleep(10),
				      ok = gen_tcp:send(Sock, <<1>>),
				      {ok,<<1>>} = gen_tcp:recv(Sock, 1)
			      end),
		   {ok, Size} = file:sendfile(Filename, Sock),
		   timer:sleep(1000),
		   Size+1
	   end,

    ok = sendfile_send("localhost", Send, 0).

%% TODO: consolidate tests and reduce code
sendfile_send(Send) ->
    sendfile_send("localhost",Send).
sendfile_send(Host, Send) ->
    sendfile_send(Host, Send, []).
sendfile_send(Host, Send, Orig) ->
    spawn_link(?MODULE, sendfile_server, [self(), Orig]),
    receive
	{server, Port} ->
	    {ok, Sock} = gen_tcp:connect(Host, Port,
					       [binary,{packet,0},
						{active,false}]),
	    Data = Send(Sock),
	    ok = gen_tcp:close(Sock),
	    receive
		{ok, Bin} ->
		    Data = Bin,
		    ok
	    end
    end.

sendfile_server(ClientPid, Orig) ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 0},
				     {active, true},
				     {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    ClientPid ! {server, Port},
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = sendfile_do_recv(Sock, Orig),
    ClientPid ! {ok, Bin},
    gen_tcp:send(Sock, <<1>>).

-define(SENDFILE_TIMEOUT, 10000).
%% f(),{ok, S} = gen_tcp:connect("localhost",7890,[binary]),file:sendfile("/ldisk/lukas/otp/sendfiletest.dat",S).
sendfile_do_recv(Sock, Bs) ->
    receive
	{tcp, Sock, B} ->
	    case binary:match(B,<<1>>) of
		nomatch when is_list(Bs) ->
		    sendfile_do_recv(Sock, [B|Bs]);
		nomatch when is_integer(Bs) ->
		    sendfile_do_recv(Sock, byte_size(B) + Bs);
		_ when is_list(Bs) ->
		    ct:log("Stopped due to a 1"),
		    {ok, iolist_to_binary(lists:reverse([B|Bs]))};
		_ when is_integer(Bs) ->
		    ct:log("Stopped due to a 1"),
		    {ok, byte_size(B) + Bs}
	    end;
	{tcp_closed, Sock} when is_list(Bs) ->
	    ct:log("Stopped due to close"),
	    {ok, iolist_to_binary(lists:reverse(Bs))};
	{tcp_closed, Sock} when is_integer(Bs) ->
	    ct:log("Stopped due to close"),
	    {ok, Bs}
    after ?SENDFILE_TIMEOUT ->
	    ct:log("Sendfile timeout"),
	    timeout
    end.

sendfile_file_info(File) ->
    {ok, #file_info{size = Size}} = file:read_file_info(File),
    {ok, Data} = file:read_file(File),
    {Size, Data}.
