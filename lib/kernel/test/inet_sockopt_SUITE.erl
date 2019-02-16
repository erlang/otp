%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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
-module(inet_sockopt_SUITE).

-include_lib("common_test/include/ct.hrl").


-define(C_GET_IPPROTO_TCP,1).
-define(C_GET_IPPROTO_IP,2).
-define(C_GET_SOL_SOCKET,3).
-define(C_GET_SOL_IP,4).

-define(C_GET_TCP_KEEPIDLE,11).
-define(C_GET_TCP_LINGER2,12).
-define(C_GET_TCP_INFO,13).
-define(C_GET_SO_REUSEADDR,14).
-define(C_GET_SO_KEEPALIVE,15).
-define(C_GET_SO_LINGER,16).

-define(C_GET_LINGER_SIZE,21).
-define(C_GET_TCP_INFO_SIZE,22).

-define(C_GET_OFF_LINGER_L_ONOFF,31).
-define(C_GET_OFF_LINGER_L_LINGER,32).
-define(C_GET_OFF_TCPI_SACKED,33).
-define(C_GET_OFF_TCPI_OPTIONS,34).

-define(C_GET_SIZ_LINGER_L_ONOFF,41).
-define(C_GET_SIZ_LINGER_L_LINGER,42).
-define(C_GET_SIZ_TCPI_SACKED,43).
-define(C_GET_SIZ_TCPI_OPTIONS,44).

-define(C_QUIT,99).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 simple/1, loop_all/1, simple_raw/1, simple_raw_getbin/1, 
	 multiple_raw/1, multiple_raw_getbin/1,
	 doc_examples_raw/1,doc_examples_raw_getbin/1,
	 large_raw/1,large_raw_getbin/1,combined/1,combined_getbin/1,
	 ipv6_v6only_udp/1, ipv6_v6only_tcp/1, ipv6_v6only_sctp/1,
	 use_ipv6_v6only_udp/1,
	 type_errors/1]).

-export([init_per_testcase/2, end_per_testcase/2]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [simple, loop_all, simple_raw, simple_raw_getbin,
     multiple_raw, multiple_raw_getbin,
     doc_examples_raw, doc_examples_raw_getbin, large_raw,
     large_raw_getbin, combined, combined_getbin,
     ipv6_v6only_udp, ipv6_v6only_tcp, ipv6_v6only_sctp,
     use_ipv6_v6only_udp,
     type_errors].

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

%% Test inet:setopt/getopt simple functionality.
simple(Config) when is_list(Config) ->
    XOpt = case os:type() of
	       {unix,_} -> [{reuseaddr,true}];
	       _ -> []
	   end,
    Opt = [{nodelay,true},
	   {keepalive,true},{packet,4},
	   {active,false}|XOpt],
    OptTags = [X || {X,_} <- Opt],
    {S1,S2} = create_socketpair(Opt, Opt),
    {ok,Opt} = inet:getopts(S1,OptTags),
    {ok,Opt} = inet:getopts(S2,OptTags),
    NoPushOpt = case os:type() of
                    {unix, Osname} when Osname =:= linux; Osname =:= freebsd -> {nopush, true};
                    {_,_} -> {nopush, false}
                end,
    COpt = [{X,case X of nodelay -> false;_ -> Y end} || {X,Y} <- [NoPushOpt|Opt]],
    COptTags = [X || {X,_} <- COpt],
    inet:setopts(S1,COpt),
    {ok,COpt} = inet:getopts(S1,COptTags),
    {ok,Opt} = inet:getopts(S2,OptTags),
    gen_tcp:close(S1),
    gen_tcp:close(S2),
    ok.

%% Loop through all socket options and check that they work.
loop_all(Config) when is_list(Config) ->
    ListenFailures =
	lists:foldr(make_check_fun(listen,1),[],all_listen_options()),
    ConnectFailures =
	lists:foldr(make_check_fun(connect,2),[],all_connect_options()),
    case ListenFailures++ConnectFailures of
	[] ->
	    ok;
	Failed ->
	    {comment,lists:flatten(
		       io_lib:format("Non mandatory failed:~w",
				     [Failed]))}
    end.



%% Test simple setopt/getopt of raw options.
simple_raw(Config) when is_list(Config) ->
    do_simple_raw(Config,false).

%% Test simple setopt/getopt of raw options, with binaries in getopt.
simple_raw_getbin(Config) when is_list(Config) ->
    do_simple_raw(Config,true).

do_simple_raw(Config,Binary) when is_list(Config) ->
    Port = start_helper(Config),
    SolSocket = ask_helper(Port,?C_GET_SOL_SOCKET),
    SoKeepAlive = ask_helper(Port,?C_GET_SO_KEEPALIVE),
    OptionTrue = {raw,SolSocket,SoKeepAlive,<<1:32/native>>},
    OptionFalse = {raw,SolSocket,SoKeepAlive,<<0:32/native>>},
    {S1,S2} = create_socketpair([OptionTrue],[{keepalive,true}]),
    {ok,[{keepalive,true}]} = inet:getopts(S1,[keepalive]),
    {ok,[{keepalive,true}]} = inet:getopts(S2,[keepalive]),
    {ok,[{raw,SolSocket,SoKeepAlive,X1B}]} =
	inet:getopts(S1,[{raw,SolSocket,SoKeepAlive,binarify(4,Binary)}]),
    X1 = nintbin2int(X1B),
    {ok,[{raw,SolSocket,SoKeepAlive,X2B}]} =
	inet:getopts(S2,[{raw,SolSocket,SoKeepAlive,binarify(4,Binary)}]),
    X2 = nintbin2int(X2B),
    true = X1 > 0,
    true = X2 > 0,
    inet:setopts(S1,[{keepalive,false}]),
    inet:setopts(S2,[OptionFalse]),
    {ok,[{keepalive,false}]} = inet:getopts(S1,[keepalive]),
    {ok,[{keepalive,false}]} = inet:getopts(S2,[keepalive]),
    {ok,[{raw,SolSocket,SoKeepAlive,Y1B}]} =
	inet:getopts(S1,[{raw,SolSocket,SoKeepAlive,binarify(4,Binary)}]),
    Y1 = nintbin2int(Y1B),
    {ok,[{raw,SolSocket,SoKeepAlive,Y2B}]} =
	inet:getopts(S2,[{raw,SolSocket,SoKeepAlive,binarify(4,Binary)}]),
    Y2 = nintbin2int(Y2B),
    true = Y1 == 0,
    true = Y2 == 0,
    gen_tcp:close(S1),
    gen_tcp:close(S2),
    stop_helper(Port),
    ok.

nintbin2int(<<Int:32/native>>) -> Int;
nintbin2int(<<Int:24/native>>) -> Int;
nintbin2int(<<Int:16/native>>) -> Int;
nintbin2int(<<Int:8/native>>) -> Int;
nintbin2int(<<>>) -> 0.



%% Test setopt/getopt of multiple raw options.
multiple_raw(Config) when is_list(Config) ->
    do_multiple_raw(Config,false).

%% Test setopt/getopt of multiple raw options, with binaries in
%% getopt.
multiple_raw_getbin(Config) when is_list(Config) ->
    do_multiple_raw(Config,true).

do_multiple_raw(Config, Binary) ->
    Port = start_helper(Config),
    SolSocket = ask_helper(Port, ?C_GET_SOL_SOCKET),
    SoKeepalive = ask_helper(Port, ?C_GET_SO_KEEPALIVE),
    SoKeepaliveTrue = {raw,SolSocket,SoKeepalive,<<1:32/native>>},
    SoKeepaliveFalse = {raw,SolSocket,SoKeepalive,<<0:32/native>>},
    SoReuseaddr = ask_helper(Port, ?C_GET_SO_REUSEADDR),
    SoReuseaddrTrue = {raw,SolSocket,SoReuseaddr,<<1:32/native>>},
    SoReuseaddrFalse = {raw,SolSocket,SoReuseaddr,<<0:32/native>>},
    {S1,S2} =
	create_socketpair(
	  [SoReuseaddrFalse,SoKeepaliveTrue],
	  [SoKeepaliveFalse,SoReuseaddrTrue]),
    {ok,[{reuseaddr,false},{keepalive,true}]} =
	inet:getopts(S1, [reuseaddr,keepalive]),
    {ok,
     [{raw,SolSocket,SoReuseaddr,S1R1},
      {raw,SolSocket,SoKeepalive,S1K1}]} =
	inet:getopts(
	  S1,
	  [{raw,SolSocket,SoReuseaddr,binarify(4, Binary)},
	   {raw,SolSocket,SoKeepalive,binarify(4, Binary)}]),
    true = nintbin2int(S1R1) =:= 0,
    true = nintbin2int(S1K1) =/= 0,
    {ok,[{keepalive,false},{reuseaddr,true}]} =
	inet:getopts(S2, [keepalive,reuseaddr]),
    {ok,
     [{raw,SolSocket,SoKeepalive,S2K1},
      {raw,SolSocket,SoReuseaddr,S2R1}]} =
	inet:getopts(
	  S2,
	  [{raw,SolSocket,SoKeepalive,binarify(4, Binary)},
	   {raw,SolSocket,SoReuseaddr,binarify(4, Binary)}]),
    true = nintbin2int(S2K1) =:= 0,
    true = nintbin2int(S2R1) =/= 0,
    %%
    ok = inet:setopts(
	   S1, [SoReuseaddrTrue,SoKeepaliveFalse]),
    ok = inet:setopts(
	   S2, [SoKeepaliveTrue,SoReuseaddrFalse]),
    {ok,
     [{raw,SolSocket,SoReuseaddr,S1R2},
      {raw,SolSocket,SoKeepalive,S1K2}]} =
	inet:getopts(
	  S1,
	  [{raw,SolSocket,SoReuseaddr,binarify(4, Binary)},
	   {raw,SolSocket,SoKeepalive,binarify(4, Binary)}]),
    true = nintbin2int(S1R2) =/= 0,
    true = nintbin2int(S1K2) =:= 0,
    {ok,
     [{raw,SolSocket,SoKeepalive,S2K2},
      {raw,SolSocket,SoReuseaddr,S2R2}]} =
	inet:getopts(
	  S2,
	  [{raw,SolSocket,SoKeepalive,binarify(4, Binary)},
	   {raw,SolSocket,SoReuseaddr,binarify(4, Binary)}]),
    true = nintbin2int(S2K2) =/= 0,
    true = nintbin2int(S2R2) =:= 0,
    %%
    gen_tcp:close(S1),
    gen_tcp:close(S2),
    stop_helper(Port),
    ok.



%% Test that the example code from the documentation works.
doc_examples_raw(Config) when is_list(Config) ->
    do_doc_examples_raw(Config,false).

%% Test that the example code from the documentation works when getopt
%% uses binaries.
doc_examples_raw_getbin(Config) when is_list(Config) ->
    do_doc_examples_raw(Config,true).

do_doc_examples_raw(Config,Binary) when is_list(Config) ->
    Port = start_helper(Config),
    Proto = ask_helper(Port,?C_GET_IPPROTO_TCP),
    TcpInfo = ask_helper(Port,?C_GET_TCP_INFO),
    TcpInfoSize = ask_helper(Port,?C_GET_TCP_INFO_SIZE),
    TcpiSackedOffset = ask_helper(Port,?C_GET_OFF_TCPI_SACKED),
    TcpiOptionsOffset = ask_helper(Port,?C_GET_OFF_TCPI_OPTIONS),
    TcpiSackedSize = ask_helper(Port,?C_GET_SIZ_TCPI_SACKED),
    TcpiOptionsSize = ask_helper(Port,?C_GET_SIZ_TCPI_OPTIONS),
    TcpLinger2 = ask_helper(Port,?C_GET_TCP_LINGER2),
    stop_helper(Port),
    case all_ok([Proto,TcpInfo,TcpInfoSize,TcpiSackedOffset,
		 TcpiOptionsOffset,TcpiSackedSize,TcpiOptionsSize,
		 TcpLinger2]) of
	false ->
	    {skipped,"Does not run on this OS."};
	true ->
	    {Sock,I} = create_socketpair([],[]),
	    {ok,[{raw,Proto,TcpLinger2,<<OrigLinger:32/native>>}]} =
		inet:getopts(Sock,[{raw,Proto,TcpLinger2,binarify(4,Binary)}]),
	    NewLinger = OrigLinger div 2,
	    ok = inet:setopts(Sock,[{raw,Proto,TcpLinger2,
				     <<NewLinger:32/native>>}]),
	    {ok,[{raw,Proto,TcpLinger2,<<NewLinger:32/native>>}]} =
		inet:getopts(Sock,[{raw,Proto,TcpLinger2,binarify(4,Binary)}]),
	    ok = inet:setopts(Sock,[{raw,Proto,TcpLinger2,
				     <<OrigLinger:32/native>>}]),
	    {ok,[{raw,Proto,TcpLinger2,<<OrigLinger:32/native>>}]} =
		inet:getopts(Sock,[{raw,Proto,TcpLinger2,binarify(4,Binary)}]),
	    {ok,[{raw,_,_,Info}]} =
		inet:getopts(Sock,[{raw,Proto,TcpInfo,
				    binarify(TcpInfoSize,Binary)}]),
	    Bit1 = TcpiSackedSize * 8,
            <<_:TcpiSackedOffset/binary,
	      TcpiSacked:Bit1/native,_/binary>> =
		Info,
	    0 = TcpiSacked,
	    Bit2 = TcpiOptionsSize * 8,
            <<_:TcpiOptionsOffset/binary,
	      TcpiOptions:Bit2/native,_/binary>> =
		Info,
	    true = TcpiOptions =/= 0,
	    gen_tcp:close(Sock),
	    gen_tcp:close(I),
	    ok
    end.

%% Test structs and large/too large buffers when raw.
large_raw(Config) when is_list(Config) ->
    do_large_raw(Config,false).

%% Test structs and large/too large buffers when raw
%% using binaries to getopts.
large_raw_getbin(Config) when is_list(Config) ->
    do_large_raw(Config,true).

do_large_raw(Config,Binary) when is_list(Config) ->
    Port = start_helper(Config),
    Proto = ask_helper(Port,?C_GET_SOL_SOCKET),
    Linger = ask_helper(Port,?C_GET_SO_LINGER),
    LingerSize = ask_helper(Port,?C_GET_LINGER_SIZE),
    LingerOnOffOffset = ask_helper(Port,?C_GET_OFF_LINGER_L_ONOFF),
    LingerLingerOffset = ask_helper(Port,?C_GET_OFF_LINGER_L_LINGER),
    LingerOnOffSize = ask_helper(Port,?C_GET_SIZ_LINGER_L_ONOFF),
    LingerLingerSize = ask_helper(Port,?C_GET_SIZ_LINGER_L_LINGER),
    stop_helper(Port),
    case all_ok([Proto,Linger,LingerSize,LingerOnOffOffset,
		 LingerLingerOffset,LingerOnOffSize,LingerLingerSize]) of
	false ->
	    {skipped,"Does not run on this OS."};
	true ->
	    {Sock1,Sock2} = create_socketpair([{linger,{true,10}}],
					      [{linger,{false,0}}]),
	    LargeSize = 1024,  % Solaris can take up to 1024*9,
						% linux 1024*63...
	    TooLargeSize = 1024*64,
	    {ok,[{raw,Proto,Linger,Linger1}]} =
		inet:getopts(Sock1,[{raw,Proto,Linger,
				     binarify(LargeSize,Binary)}]),
	    {ok,[{raw,Proto,Linger,Linger2}]} =
		inet:getopts(Sock2,[{raw,Proto,Linger,
				     binarify(LingerSize,Binary)}]),
	    true = byte_size(Linger1) =:= LingerSize,
	    LingerLingerBits = LingerLingerSize * 8,
	    LingerOnOffBits = LingerOnOffSize * 8,
	    <<_:LingerLingerOffset/binary,
	      Ling1:LingerLingerBits/native,_/binary>> = Linger1,
	    <<_:LingerOnOffOffset/binary,
	      Off1:LingerOnOffBits/native,_/binary>> = Linger1,
	    <<_:LingerOnOffOffset/binary,
	      Off2:LingerOnOffBits/native,_/binary>> = Linger2,
	    true = Off1 =/= 0,
	    true = Off2 == 0,
	    true = Ling1 == 10,
	    {error,einval} =
		inet:getopts(Sock1,[{raw,Proto,Linger,TooLargeSize}]),
	    gen_tcp:close(Sock1),
	    gen_tcp:close(Sock2),
	    ok
    end.

%% Test raw structs combined w/ other options .
combined(Config) when is_list(Config) ->
    do_combined(Config,false).

%% Test raw structs combined w/ other options and
%% binarise in getopts.
combined_getbin(Config) when is_list(Config) ->
    do_combined(Config,true).

do_combined(Config,Binary) when is_list(Config) ->
    Port = start_helper(Config),
    Proto = ask_helper(Port,?C_GET_SOL_SOCKET),
    Linger = ask_helper(Port,?C_GET_SO_LINGER),
    LingerSize = ask_helper(Port,?C_GET_LINGER_SIZE),
    LingerOnOffOffset = ask_helper(Port,?C_GET_OFF_LINGER_L_ONOFF),
    LingerLingerOffset = ask_helper(Port,?C_GET_OFF_LINGER_L_LINGER),
    LingerOnOffSize = ask_helper(Port,?C_GET_SIZ_LINGER_L_ONOFF),
    LingerLingerSize = ask_helper(Port,?C_GET_SIZ_LINGER_L_LINGER),
    stop_helper(Port),
    case all_ok([Proto,Linger,LingerSize,LingerOnOffOffset,
		 LingerLingerOffset,LingerOnOffSize,LingerLingerSize]) of
	false ->
	    {skipped,"Does not run on this OS."};
	true ->
	    LingerLingerBits = LingerLingerSize * 8,
	    LingerOnOffBits = LingerOnOffSize * 8,
	    {LingerOn,LingerOff} =
		case LingerOnOffOffset <  LingerLingerOffset of
		    true ->
			Pad1 =
			    list_to_binary(
			      lists:duplicate(LingerOnOffOffset,
					      0)),
			Pad2Siz =
			    LingerLingerOffset - LingerOnOffSize -
			    LingerOnOffOffset,
			Pad2 =
			    list_to_binary(
			      lists:duplicate(Pad2Siz,
					      0)),
			Pad3Siz = LingerSize - LingerLingerSize -
			    LingerLingerOffset,
			Pad3 = list_to_binary(
				 lists:duplicate(Pad3Siz,
						 0)),
			{<<Pad1/binary,1:LingerOnOffBits/native,
			   Pad2/binary,10:LingerLingerBits/native,
			   Pad3/binary>>,
			 <<Pad1/binary,0:LingerOnOffBits/native,
			   Pad2/binary,0:LingerLingerBits/native,
			   Pad3/binary>>};
		    false ->
			Pad1 =
			    list_to_binary(
			      lists:duplicate(LingerLingerOffset,
					      0)),
			Pad2Siz =
			    LingerOnOffOffset - LingerLingerSize -
			    LingerLingerOffset,
			Pad2 =
			    list_to_binary(
			      lists:duplicate(Pad2Siz,
					      0)),
			Pad3Siz = LingerSize - LingerOnOffSize -
			    LingerOnOffOffset,
			Pad3 = list_to_binary(
				 lists:duplicate(Pad3Siz,
						 0)),
			{<<Pad1/binary,1:LingerLingerBits/native,
			   Pad2/binary,10:LingerOnOffBits/native,
			   Pad3/binary>>,
			 <<Pad1/binary,0:LingerLingerBits/native,
			   Pad2/binary,0:LingerOnOffBits/native,
			   Pad3/binary>>}
		end,
	    RawLingerOn = {raw,Proto,Linger,LingerOn},
	    RawLingerOff = {raw,Proto,Linger,LingerOff},
	    {Sock1,Sock2} =
		create_socketpair([{keepalive,true},
				   RawLingerOn],
				  [{keepalive,false},
				   RawLingerOff]),
	    {ok,[{raw,Proto,Linger,Linger1},{keepalive,Keep1}]} =
		inet:getopts(Sock1,[{raw,Proto,Linger, 
				     binarify(LingerSize,Binary)},keepalive]),
	    {ok,[{raw,Proto,Linger,Linger2},{keepalive,Keep2}]} =
		inet:getopts(Sock2,[{raw,Proto,Linger,
				     binarify(LingerSize,Binary)},keepalive]),
	    true = byte_size(Linger1) =:= LingerSize,
	    <<_:LingerLingerOffset/binary,
	      Ling1:LingerLingerBits/native,_/binary>> = Linger1,
	    <<_:LingerOnOffOffset/binary,
	      Off1:LingerOnOffBits/native,_/binary>> = Linger1,
	    <<_:LingerOnOffOffset/binary,
	      Off2:LingerOnOffBits/native,_/binary>> = Linger2,
	    true = Off1 =/= 0,
	    true = Off2 == 0,
	    true = Ling1 == 10,
	    true = Keep1 =:= true,
	    true = Keep2 =:= false,
	    {Sock3,Sock4} =
		create_socketpair([RawLingerOn,{keepalive,true}],
				  [RawLingerOff,{keepalive,false}]),
	    {ok,[{raw,Proto,Linger,Linger3},{keepalive,Keep3}]} =
		inet:getopts(Sock3,[{raw,Proto,Linger,
				     binarify(LingerSize,Binary)},keepalive]),
	    {ok,[{raw,Proto,Linger,Linger4},{keepalive,Keep4}]} =
		inet:getopts(Sock4,[{raw,Proto,Linger,
				     binarify(LingerSize,Binary)},keepalive]),
	    true = byte_size(Linger3) =:= LingerSize,
	    <<_:LingerLingerOffset/binary,
	      Ling3:LingerLingerBits/native,_/binary>> = Linger3,
	    <<_:LingerOnOffOffset/binary,
	      Off3:LingerOnOffBits/native,_/binary>> = Linger3,
	    <<_:LingerOnOffOffset/binary,
	      Off4:LingerOnOffBits/native,_/binary>> = Linger4,
	    true = Off3 =/= 0,
	    true = Off4 == 0,
	    true = Ling3 == 10,
	    true = Keep3 =:= true,
	    true = Keep4 =:= false,
	    {Sock5,Sock6} =
		create_socketpair([{packet,4},RawLingerOn,{keepalive,true}],
				  [{packet,2},RawLingerOff,{keepalive,false}]),
	    {ok,[{packet,Pack5},{raw,Proto,Linger,Linger5},
		 {keepalive,Keep5}]} =
		inet:getopts(Sock5,[packet,{raw,Proto,Linger,
					    binarify(LingerSize,Binary)},
				    keepalive]),
	    {ok,[{packet,Pack6},{raw,Proto,Linger,Linger6},
		 {keepalive,Keep6}]} =
		inet:getopts(Sock6,[packet,{raw,Proto,Linger,
					    binarify(LingerSize,Binary)},
				    keepalive]),
	    true = byte_size(Linger5) =:= LingerSize,
	    <<_:LingerLingerOffset/binary,
	      Ling5:LingerLingerBits/native,_/binary>> = Linger5,
	    <<_:LingerOnOffOffset/binary,
	      Off5:LingerOnOffBits/native,_/binary>> = Linger5,
	    <<_:LingerOnOffOffset/binary,
	      Off6:LingerOnOffBits/native,_/binary>> = Linger6,
	    true = Off5 =/= 0,
	    true = Off6 == 0,
	    true = Ling5 == 10,
	    true = Keep5 =:= true,
	    true = Keep6 =:= false,
	    true = Pack5 =:= 4,
	    true = Pack6 =:= 2,
	    inet:setopts(Sock6,[{packet,4},RawLingerOn,
				{keepalive,true}]),
	    {ok,[{packet,Pack7},{raw,Proto,Linger,Linger7},
		 {keepalive,Keep7}]} =
		inet:getopts(Sock6,[packet,{raw,Proto,Linger,
					    binarify(LingerSize,Binary)},
				    keepalive]),
	    <<_:LingerOnOffOffset/binary,
	      Off7:LingerOnOffBits/native,_/binary>> = Linger7,
	    true = Off7 =/= 0,
	    true = Keep7 =:= true,
	    true = Pack7 =:= 4,
	    gen_tcp:close(Sock1),
	    gen_tcp:close(Sock2),
	    gen_tcp:close(Sock3),
	    gen_tcp:close(Sock4),
	    gen_tcp:close(Sock5),
	    gen_tcp:close(Sock6),
	    ok
    end.



%% Test socket option ipv6_v6only for UDP.
ipv6_v6only_udp(Config) when is_list(Config) ->
    ipv6_v6only(Config, gen_udp).

%% Test socket option ipv6_v6only for TCP.
ipv6_v6only_tcp(Config) when is_list(Config) ->
    ipv6_v6only(Config, gen_tcp).

%% Test socket option ipv6_v6only for SCTP.
ipv6_v6only_sctp(Config) when is_list(Config) ->
    ipv6_v6only(Config, gen_sctp).

ipv6_v6only(Config, Module) when is_list(Config) ->
    case ipv6_v6only_open(Module, []) of
	{ok,S1} ->
	    case inet:getopts(S1, [ipv6_v6only]) of
		{ok,[{ipv6_v6only,Default}]}
		  when is_boolean(Default) ->
		    ok =
			ipv6_v6only_close(Module, S1),
		    ipv6_v6only(Config, Module, Default);
		{ok,[]} ->
		    io:format("Not implemented.~n", []),
		    %% This list of OS:es where the option is
		    %% supposed to be not implemented is just
		    %% a guess, and may grow with time.
		    case {os:type(),os:version()} of
			{{unix,linux},{2,M,_}}
			  when M =< 4 -> ok
		    end,
		    %% At least this should work
		    {ok,S2} =
			ipv6_v6only_open(
			  Module,
			  [{ipv6_v6only,true}]),
		    ok =
			ipv6_v6only_close(Module, S2)
	    end;
	{error,_} ->
	    {skipped,"Socket type not supported"}
    end.

ipv6_v6only(Config, Module, Default) when is_list(Config) ->
    io:format("Default ~w.~n", [Default]),
    {ok,S1} =
	ipv6_v6only_open(Module, [{ipv6_v6only,Default}]),
    {ok,[{ipv6_v6only,Default}]} =
	inet:getopts(S1, [ipv6_v6only]),
    ok =
	ipv6_v6only_close(Module, S1),
    NotDefault = not Default,
    case ipv6_v6only_open(Module, [{ipv6_v6only,NotDefault}]) of
	{ok,S2} ->
	    io:format("Read-write.~n", []),
	    {ok,[{ipv6_v6only,NotDefault}]} =
		inet:getopts(S2, [ipv6_v6only]),
	    ok;
	{error,einval} ->
	    io:format("Read-only.~n", []),
	    %% This option is known to be read-only and true
	    %% on Windows and OpenBSD
	    case os:type() of
		{unix,openbsd} when Default =:= true -> ok;
		{win32,_} when Default =:= true -> ok
	    end
    end.

ipv6_v6only_open(Module, Opts) ->
    Module:case Module of
	       gen_tcp -> listen;
	       _ -> open
	   end(0, [inet6|Opts]).

ipv6_v6only_close(Module, Socket) ->
    Module:close(Socket).


%% Test using socket option ipv6_v6only for UDP.
use_ipv6_v6only_udp(Config) when is_list(Config) ->
    case gen_udp:open(0, [inet6,{ip,{0,0,0,0,0,0,0,1}}, {ipv6_v6only,true}]) of
	{ok,S6} ->
	    case inet:getopts(S6, [ipv6_v6only]) of
		{ok,[{ipv6_v6only,true}]} ->
		    use_ipv6_v6only_udp(Config, S6);
		{ok,Other} ->
		    {skipped,{getopts,Other}}
	    end;
	{error,_} ->
	    {skipped,"Socket type not supported"}
    end.

use_ipv6_v6only_udp(_Config, S6) ->
    {ok,Port} = inet:port(S6),
    {ok,S4} = gen_udp:open(Port, [inet]),
    E6 = " IPv6-echo.",
    E4 = " IPv4-echo.",
    Sender =
	spawn_link(fun () -> use_ipv6_v6only_udp_sender(Port, E6, E4) end),
    use_ipv6_v6only_udp_listener(
      S6, S4, E6, E4, monitor(process, Sender)).

use_ipv6_v6only_udp_listener(S6, S4, E6, E4, Mref) ->
    receive
	{udp,S6,IP,P,Data} ->
	    ok = gen_udp:send(S6, IP, P, [Data|E6]),
	    use_ipv6_v6only_udp_listener(S6, S4, E6, E4, Mref);
	{udp,S4,IP,P,Data} ->
	    ok = gen_udp:send(S4, IP, P, [Data|E4]),
	    use_ipv6_v6only_udp_listener(S6, S4, E6, E4, Mref);
	{'DOWN',Mref,_,_,normal} ->
	    ok;
	{'DOWN',Mref,_,_,Result} ->
	    %% Since we are linked we will never arrive here
	    Result;
	Other ->
	    exit({failed,{listener_unexpected,Other}})
    end.

use_ipv6_v6only_udp_sender(Port, E6, E4) ->
    D6 = "IPv6-send.",
    D4 = "IPv4-send.",
    R6 = D6 ++ E6,
    R4 = D4 ++ E4,
    R6 = sndrcv({0,0,0,0,0,0,0,1}, Port, [inet6], D6),
    R4 = sndrcv({127,0,0,1}, Port, [inet], D4),
    ok.

sndrcv(Ip, Port, Opts, Data) ->
    {ok,S} = gen_udp:open(0, Opts),
    io:format("[~w:~w] ! ~s~n", [Ip,Port,Data]),
    ok = gen_udp:send(S, Ip, Port, Data),
    receive
	{udp,S,Ip,Port,RecData} ->
	    io:format("[~w:~w] : ~s~n", [Ip,Port,RecData]),
	    RecData;
	Other ->
	    exit({failed,{sndrcv_unexpectec,Other}})
    end.



%% Test that raw data requests are not executed for bad types.
type_errors(Config) when is_list(Config) ->
    BadSetOptions =
	[
	 {raw,x,3,<<1:32>>},
	 {raw,1,tre,<<1:32>>},
	 {raw,1,3,ko},
	 {raw,1,3,5},
	 {raw,1,3},
	 {raw,1},
	 {raw},
	 {raw,ett},
	 {raw,ett,tre},
	 {raw,{true,10}},
	 {raw,{ett,tre,<<1:32>>}},
	 {rav,1,3,<<1:32>>},
	 raw,
	 rav,
	 {linger,banan}
	],
    BadGetOptions =
	[
	 {raw,x,3,<<1:32>>},
	 {raw,1,tre,<<1:32>>},
	 {raw,1,3,ko},
	 {raw,1,3,5.1},
	 {raw,1,3,-3},
	 {raw,1,3},
	 {raw,1},
	 {raw},
	 {raw,ett},
	 {raw,ett,tre},
	 {raw,{true,10}},
	 {raw,{ett,tre,<<1:32>>}},
	 {rav,1,3,<<1:32>>},
	 raw,
	 rav,
	 {linger,banan}
	],
    lists:foreach(fun(Option) ->
			  case
			      catch create_socketpair([Option],[]) of
			      {'EXIT',badarg} ->
				  ok;
			      Unexpected1 ->
				  exit({unexpected,
					Unexpected1})
			  end,
			  case
			      catch create_socketpair([],[Option]) of
			      {'EXIT',badarg} ->
				  ok;
			      Unexpected2 ->
				  exit({unexpected,
					Unexpected2})
			  end,
			  {Sock1,Sock2} = create_socketpair([],[]),
			  case inet:setopts(Sock1, [Option]) of
			      {error,einval} ->
				  ok;
			      Unexpected3 ->
				  exit({unexpected,
					Unexpected3})
			  end,
			  gen_tcp:close(Sock1),
			  gen_tcp:close(Sock2)
		  end,BadSetOptions),
    {Sock1,Sock2} = create_socketpair([],[]),
    lists:foreach(fun(Option) ->
			  case inet:getopts(Sock1, [Option]) of
			      {error,einval} ->
				  ok;
			      Unexpected ->
				  exit({unexpected,
					Unexpected})
			  end
		  end,BadGetOptions),
    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    ok.

all_ok([]) ->
    true;
all_ok([H|T]) when H >= 0 ->
    all_ok(T);
all_ok(_) ->
    false.


make_check_fun(Type,Element) ->
    fun({Name,V1,V2,Mand,Chang},Acc) ->
	    {LO1,CO1} = setelement(Element,{[],[]}, [{Name,V1}]),
	    {LO2,CO2} = setelement(Element,{[],[]}, [{Name,V2}]),
	    {X1,Y1} = create_socketpair(LO1,CO1),
	    {X2,Y2} = create_socketpair(LO2,CO2),
	    S1 = element(Element,{X1,Y1}),
	    S2 = element(Element,{X2,Y2}),
	    {ok,[{Name,R1}]} = inet:getopts(S1,[Name]),
	    {ok,[{Name,R2}]} = inet:getopts(S2,[Name]),
	    NewAcc =
		case R1 =/= R2 of
		    true ->
			case Chang of
			    true ->
				inet:setopts(S1,[{Name,V2}]),
				{ok,[{Name,R3}]} =
				    inet:getopts(S1,[Name]),
				case {R3 =/= R1, R3 =:= R2} of
				    {true,true} ->
					Acc;
				    _ ->
					case Mand of
					    true ->
						exit
						  ({failed_sockopt,
						    {change,
						     Name}});
					    false ->
						[{change,Name}|Acc]
					end
				end;
			    false ->
				Acc
			end;
		    false ->
			case Mand of
			    true ->
				exit({failed_sockopt,
				      {Type,Name}});
			    false ->
				[{Type,Name}|Acc]
			end
		end,
	    gen_tcp:close(X1),
	    gen_tcp:close(Y1),
	    gen_tcp:close(X2),
	    gen_tcp:close(Y2),
	    NewAcc
    end.

%% {OptionName,Value1,Value2,Mandatory,Changeable}
all_listen_options() ->
    [{tos,0,1,false,true}, 
     {priority,0,1,false,true}, 
     {reuseaddr,false,true,false,true}, 
     {keepalive,false,true,true,true}, 
     {linger, {false,10}, {true,10},true,true},
     {sndbuf,2048,4096,false,true}, 
     {recbuf,2048,4096,false,true}, 
     {nodelay,false,true,true,true},
     {header,2,4,true,true}, 
     {active,false,true,true,false}, 
     {packet,2,4,true,true}, 
     {buffer,1000,2000,true,true}, 
     {mode,list,binary,true,true}, 
     {deliver,term,port,true,true}, 
     {exit_on_close, true, false, true, true},
     {high_watermark,4096,8192,true,true}, 
     {low_watermark,2048,4096,true,true}, 
     {high_msgq_watermark,4096,8192,true,true}, 
     {low_msgq_watermark,2048,4096,true,true}, 
     {send_timeout,infinity,1000,true,true},
     {send_timeout_close,false,true,true,true},
     {delay_send,false,true,true,true}, 
     {packet_size,0,4,true,true}
    ].
all_connect_options() ->
    [{tos,0,1,false,true}, 
     {priority,0,1,false,true}, 
     {reuseaddr,false,true,false,true}, 
     {keepalive,false,true,true,true}, 
     {linger, {false,10}, {true,10},true,true},
     {sndbuf,2048,4096,false,true}, 
     {recbuf,2048,4096,false,true}, 
     {nodelay,false,true,true,true},
     {header,2,4,true,true}, 
     {active,false,true,true,false}, 
     {packet,2,4,true,true}, 
     {buffer,1000,2000,true,true}, 
     {mode,list,binary,true,true}, 
     {deliver,term,port,true,true}, 
     {exit_on_close, true, false, true, true},
     {high_watermark,4096,8192,false,true}, 
     {low_watermark,2048,4096,false,true}, 
     {high_msgq_watermark,4096,8192,true,true}, 
     {low_msgq_watermark,2048,4096,true,true}, 
     {send_timeout,infinity,1000,true,true},
     {send_timeout_close,false,true,true,true},
     {delay_send,false,true,true,true}, 
     {packet_size,0,4,true,true}
    ].


create_socketpair(ListenOptions,ConnectOptions) ->
    {ok,LS}=gen_tcp:listen(0,ListenOptions),
    {ok,Port}=inet:port(LS),
    {ok,CS}=gen_tcp:connect(localhost,Port,ConnectOptions),
    {ok,AS}=gen_tcp:accept(LS),
    gen_tcp:close(LS),
    {AS,CS}.


start_helper(Config) ->
    Progname = filename:join(proplists:get_value(data_dir, Config), "sockopt_helper"),
    Port = open_port({spawn,Progname},[eof,line]),
    Port.

ask_helper(Port,Code) ->
    Com = integer_to_list(Code)++"\n",
    Port ! {self(),{command,Com}},
    receive
	{Port,{data,{eol,Text}}} ->
	    list_to_integer(Text);
	Other ->
	    exit({error,{unexpected_data_from_helper,Other}})
    after 3000 ->
	    exit({error,helper_timeout})
    end.

stop_helper(Port) ->
    catch ask_helper(Port,?C_QUIT),
    receive
	{Port,eof} ->
	    Port ! {self(), close},
	    receive
		{Port,closed} ->
		    ok
	    after 1000 ->
		    timeout
	    end
    after 1000 ->
	    timeout
    end.

binarify(Size,Binary) when Binary =:= true ->
    <<0:Size/unit:8>>;
binarify(Size,Binary) when Binary =:= false ->
    Size.
