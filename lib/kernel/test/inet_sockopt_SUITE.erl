%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2023. All Rights Reserved.
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
-include("kernel_test_lib.hrl").


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
-define(C_GET_SO_DONTROUTE,17).

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

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
	 
         simple/1,
         loop_all/1,
         simple_raw/1, simple_raw_getbin/1, 
	 multiple_raw/1, multiple_raw_getbin/1,
	 doc_examples_raw/1, doc_examples_raw_getbin/1,
	 large_raw/1, large_raw_getbin/1,
         combined/1, combined_getbin/1,
	 ipv6_v6only_udp/1, ipv6_v6only_tcp/1, ipv6_v6only_sctp/1,
	 use_ipv6_v6only_udp/1,
	 type_errors/1,
         windows_reuseaddr/1]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    %% This is a temporary measure to ensure that we can 
    %% test the socket backend without effecting *all*
    %% applications on *all* machines.
    %% This flag is set only for *one* host.
    case ?TEST_INET_BACKENDS() of
        true ->
            [
             {group, inet_backend_default},
             {group, inet_backend_inet},
             {group, inet_backend_socket}
            ];
        _ ->
            [
             {group, inet_backend_default}
            ]
    end.


groups() -> 
    [
     {inet_backend_default,   [], inet_backend_default_cases()},
     {inet_backend_inet,      [], inet_backend_inet_cases()},
     {inet_backend_socket,    [], inet_backend_socket_cases()}
    ].

inet_backend_default_cases() ->
    all_std_cases().

inet_backend_inet_cases() ->
    all_std_cases().

inet_backend_socket_cases() ->
    all_std_cases().

all_std_cases() ->
    [
     simple,
     loop_all,
     simple_raw, simple_raw_getbin,
     multiple_raw, multiple_raw_getbin,
     doc_examples_raw, doc_examples_raw_getbin,
     large_raw, large_raw_getbin,
     combined, combined_getbin,
     ipv6_v6only_udp, ipv6_v6only_tcp, ipv6_v6only_sctp,
     use_ipv6_v6only_udp,
     type_errors,
     windows_reuseaddr
    ].

init_per_suite(Config0) ->

    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite([{allow_skip, false} | Config0]) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            
            %% We need a monitor on this node also
            kernel_test_sys_monitor:start(),

            ?P("init_per_suite -> end when "
               "~n      Config: ~p", [Config1]),

            Config1
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    %% Stop the local monitor
    ?P("init_per_suite -> try stop system monitor"),
    kernel_test_sys_monitor:stop(),

    Config1 = ?LIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
            "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.

init_per_group(inet_backend_default = _GroupName, Config) ->
    [{socket_create_opts, []} | Config];
init_per_group(inet_backend_inet = _GroupName, Config) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{socket_create_opts, [{inet_backend, inet}]} | Config]
    end;
init_per_group(inet_backend_socket = _GroupName, Config) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{socket_create_opts, [{inet_backend, socket}]} | Config]
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Func, Config) ->
    ?P("init_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config, erlang:nodes(), links(), monitors()]),

    kernel_test_global_sys_monitor:reset_events(),

    ?P("init_per_testcase -> done when"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), links(), monitors()]),
    Config.

end_per_testcase(_Func, Config) ->
    ?P("end_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config, erlang:nodes(), links(), monitors()]),

    ?P("system events during test: "
       "~n   ~p", [kernel_test_global_sys_monitor:events()]),

    ?P("end_per_testcase -> done with"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), links(), monitors()]),
    ok.

%% Test inet:setopt/getopt simple functionality.
simple(Config) when is_list(Config) ->
    Cond = fun() ->
                   %% case ?IS_SOCKET_BACKEND(Config) of
                   %%     true ->
                   %%         {skip, "'nopush' opt not (yet) implemented"};
                   %%     false ->
                   %%         ok
                   %% end
                   ok
           end,
    Pre  = fun() -> #{} end,
    Case = fun(State) -> do_simple(Config, State) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME, Cond, Pre, Case, Post).


do_simple(Config, _) when is_list(Config) ->
    ?P("begin"),
    XOpts = case os:type() of
                {unix,_} -> [{reuseaddr,true}];
                _ -> []
            end,
    Opts      = [{nodelay,   true},
                 {keepalive, true},
                 {packet,    4},
                 {active,    false} | XOpts],
    Tags   = [X || {X,_} <- Opts],

    ?P("create socket pair"),
    {S1, S2}  = create_socketpair(Config, Opts, Opts),
    ?P("socket pair created: "
       "~n   S1:      ~p"
       "~n   S1 info: ~p"
       "~n   S2:      ~p"
       "~n   S2 info: ~p", [S1, inet:info(S1), S2, inet:info(S2)]),

    ?P("S1: get options: "
       "~n   ~p", [Tags]),
    case inet:getopts(S1, Tags) of
        {ok, Opts} ->
            ?P("S1 (getopts) success");
        {ok, Opts_S1_Invalid_1} ->
            ?P("S1 incorrect (getopts) success: "
               "~n  Expected opts:          ~p"
               "~n  UnExpected opts:        ~p"
               "~n  Exp Opts -- UnExp Opts: ~p"
               "~n  UnExp Opts -- Exp Opts: ~p",
               [Opts, Opts_S1_Invalid_1,
                Opts -- Opts_S1_Invalid_1,
                Opts_S1_Invalid_1 -- Opts]),
            ct:fail({incorrect_success, s1, 1, Opts, Opts_S1_Invalid_1});
        {error, Reason_S1_1} ->
            ?P("<ERROR> Failed get S1 options: "
               "~n  Reason: ~p", [Reason_S1_1]),
            ct:fail({unexpected_failure, s1, 1, Reason_S1_1})
    end,

    ?P("S2: get options: "
       "~n   ~p", [Tags]),
    case inet:getopts(S2, Tags) of
        {ok, Opts} ->
            ?P("S2 (getopts) success");
        {ok, Opts_S2_Invalid_1} ->
            ?P("S2 incorrect (getopts) success: "
               "~n  Expected opts:   ~p"
               "~n  UnExpected opts: ~p", [Opts, Opts_S2_Invalid_1]),
            ct:fail({incorrect_success, s2, 1, Opts, Opts_S2_Invalid_1});
        {error, Reason_S2_1} ->
            ?P("<ERROR> Failed get S2 options: "
               "~n  Reason: ~p", [Reason_S2_1]),
            ct:fail({unexpected_failure, s2, 1, Reason_S2_1})
    end,

    NoPushOpt = case os:type() of
                    {unix, Osname} when Osname =:= linux;
                                        Osname =:= freebsd -> {nopush, true};
                    {_,_} -> {nopush, false}
                end,
    COpt = [{X,case X of nodelay -> false;_ -> Y end} ||
               {X,Y} <- [NoPushOpt|Opts]],
    COptTags = [X || {X,_} <- COpt],
    ?P("S1: set options:"
       "~n   ~p", [COpt]),
    inet:setopts(S1,COpt),

    ?P("S1: get options: "
       "~n   ~p", [COptTags]),
    case inet:getopts(S1, COptTags) of
        {ok, COpt} ->
            ?P("S1: (getopts) success"),
            ok;
        {ok, COptErr_S1} ->
            ?P("S1: incorrect (getopts) success:"
               "~n   Expected: ~p"
               "~n   Received: ~p", [COpt, COptErr_S1]),
            ct:fail({incorrect_success, s1, 2, COpt, COptErr_S1});
        {error, Reason_S1_2} ->
            ?P("S1: unexpected failure:"
               "~n   ~p", [Reason_S1_2]),
            ct:fail({unexpected_failure, s1, 2, Reason_S1_2})
    end,

    ?P("S2: get options: "
       "~n   ~p", [Tags]),
    case inet:getopts(S2,Tags) of
        {ok, Opts} ->
            ?P("S2: (getopts) success"),
            ok;
        {ok, OptErr_S2} ->
            ?P("S2: incorrect (getopts) success:"
               "~n   Expected: ~p"
               "~n   Received: ~p", [Opts, OptErr_S2]),
            ct:fail({incorrect_success, s2, 2, Opts, OptErr_S2});
        {error, Reason_S2_2} ->
            ?P("S2: unexpected failure:"
               "~n   ~p", [Reason_S2_2]),
            ct:fail({unexpected_failure, s2, 2, Reason_S2_2})
    end,

    ?P("cleanup"),
    gen_tcp:close(S1),
    gen_tcp:close(S2),

    ?P("done"),
    ok.

%% Loop through all socket options and check that they work.
loop_all(Config) when is_list(Config) ->
    Cond = fun() ->
                   case ?IS_SOCKET_BACKEND(Config) of
                       true ->
                           {skip, "Watermark options not (yet) implemented"};
                       false ->
                           ok
                   end
           end,
    Pre  = fun() -> #{} end,
    Case = fun(State) -> do_loop_all(Config, State) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME, Cond, Pre, Case, Post).

do_loop_all(Config, _) ->
    ListenFailures =
	lists:foldr(make_check_fun(Config, listen),[],all_listen_options()),
    AcceptFailures =
	lists:foldr(make_check_fun(Config, accept),[],all_accept_options()),
    ConnectFailures =
	lists:foldr(make_check_fun(Config, connect),[],all_connect_options()),
    UdpFailures =
	lists:foldr(make_check_fun(Config),[],all_udp_options()),
    case ListenFailures++AcceptFailures++ConnectFailures++UdpFailures of
	[] ->
	    ok;
	Failed ->
	    {comment,lists:flatten(
		       io_lib:format("Non mandatory failed:~w",
				     [Failed]))}
    end.



%% Test simple setopt/getopt of raw options.
simple_raw(Config) when is_list(Config) ->
    do_simple_raw(Config, false).

%% Test simple setopt/getopt of raw options, with binaries in getopt.
simple_raw_getbin(Config) when is_list(Config) ->
    do_simple_raw(Config, true).

do_simple_raw(Config, Binary) when is_list(Config) ->
    Port = start_helper(Config),
    SolSocket = ask_helper(Port,?C_GET_SOL_SOCKET),
    SoKeepAlive = ask_helper(Port,?C_GET_SO_KEEPALIVE),
    OptionTrue = {raw,SolSocket,SoKeepAlive,<<1:32/native>>},
    OptionFalse = {raw,SolSocket,SoKeepAlive,<<0:32/native>>},
    {S1,S2} = create_socketpair(Config, [OptionTrue],[{keepalive,true}]),
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
    Cond = fun() -> is_not_openbsd() end,
    Pre  = fun() -> false end,
    Case = fun(State) -> do_multiple_raw(Config, State) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME, Cond, Pre, Case, Post).
    

%% Test setopt/getopt of multiple raw options, with binaries in
%% getopt.
multiple_raw_getbin(Config) when is_list(Config) ->
    Cond = fun() -> is_not_openbsd() end,
    Pre  = fun() -> true end,
    Case = fun(State) -> do_multiple_raw(Config, State) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME, Cond, Pre, Case, Post).

do_multiple_raw(Config, Binary) ->
    Port             = start_helper(Config),
    SolSocket        = ask_helper(Port, ?C_GET_SOL_SOCKET),
    SoKeepalive      = ask_helper(Port, ?C_GET_SO_KEEPALIVE),
    SoKeepaliveTrue  = {raw,SolSocket,SoKeepalive,<<1:32/native>>},
    SoKeepaliveFalse = {raw,SolSocket,SoKeepalive,<<0:32/native>>},
    SoDontroute      = ask_helper(Port, ?C_GET_SO_DONTROUTE),
    SoDontrouteTrue  = {raw,SolSocket,SoDontroute,<<1:32/native>>},
    SoDontrouteFalse = {raw,SolSocket,SoDontroute,<<0:32/native>>},
    {S1,S2} =
	create_socketpair(
          Config,
	  [SoDontrouteFalse,SoKeepaliveTrue],
	  [SoKeepaliveFalse,SoDontrouteTrue]),
    {ok,[{dontroute,false},{keepalive,true}]} =
	inet:getopts(S1, [dontroute,keepalive]),
    {ok,
     [{raw,SolSocket,SoDontroute,S1R1},
      {raw,SolSocket,SoKeepalive,S1K1}]} =
	inet:getopts(
	  S1,
	  [{raw,SolSocket,SoDontroute,binarify(4, Binary)},
	   {raw,SolSocket,SoKeepalive,binarify(4, Binary)}]),
    true = nintbin2int(S1R1) =:= 0,
    true = nintbin2int(S1K1) =/= 0,
    {ok,[{keepalive,false},{dontroute,true}]} =
	inet:getopts(S2, [keepalive,dontroute]),
    {ok,
     [{raw,SolSocket,SoKeepalive,S2K1},
      {raw,SolSocket,SoDontroute,S2R1}]} =
	inet:getopts(
	  S2,
	  [{raw,SolSocket,SoKeepalive,binarify(4, Binary)},
	   {raw,SolSocket,SoDontroute,binarify(4, Binary)}]),
    true = nintbin2int(S2K1) =:= 0,
    true = nintbin2int(S2R1) =/= 0,
    %%
    ok = inet:setopts(
	   S1, [SoDontrouteTrue,SoKeepaliveFalse]),
    ok = inet:setopts(
	   S2, [SoKeepaliveTrue,SoDontrouteFalse]),
    {ok,
     [{raw,SolSocket,SoDontroute,S1R2},
      {raw,SolSocket,SoKeepalive,S1K2}]} =
	inet:getopts(
	  S1,
	  [{raw,SolSocket,SoDontroute,binarify(4, Binary)},
	   {raw,SolSocket,SoKeepalive,binarify(4, Binary)}]),
    true = nintbin2int(S1R2) =/= 0,
    true = nintbin2int(S1K2) =:= 0,
    {ok,
     [{raw,SolSocket,SoKeepalive,S2K2},
      {raw,SolSocket,SoDontroute,S2R2}]} =
	inet:getopts(
	  S2,
	  [{raw,SolSocket,SoKeepalive,binarify(4, Binary)},
	   {raw,SolSocket,SoDontroute,binarify(4, Binary)}]),
    true = nintbin2int(S2K2) =/= 0,
    true = nintbin2int(S2R2) =:= 0,
    %%
    gen_tcp:close(S1),
    gen_tcp:close(S2),
    stop_helper(Port),
    ok.



%% Test that the example code from the documentation works.
doc_examples_raw(Config) when is_list(Config) ->
    do_doc_examples_raw(Config, false).

%% Test that the example code from the documentation works when getopt
%% uses binaries.
doc_examples_raw_getbin(Config) when is_list(Config) ->
    do_doc_examples_raw(Config,true).

do_doc_examples_raw(Config, Binary) when is_list(Config) ->
    Port              = start_helper(Config),
    Proto             = ask_helper(Port,?C_GET_IPPROTO_TCP),
    TcpInfo           = ask_helper(Port,?C_GET_TCP_INFO),
    TcpInfoSize       = ask_helper(Port,?C_GET_TCP_INFO_SIZE),
    TcpiSackedOffset  = ask_helper(Port,?C_GET_OFF_TCPI_SACKED),
    TcpiOptionsOffset = ask_helper(Port,?C_GET_OFF_TCPI_OPTIONS),
    TcpiSackedSize    = ask_helper(Port,?C_GET_SIZ_TCPI_SACKED),
    TcpiOptionsSize   = ask_helper(Port,?C_GET_SIZ_TCPI_OPTIONS),
    TcpLinger2        = ask_helper(Port,?C_GET_TCP_LINGER2),
    stop_helper(Port),
    case all_ok([Proto,TcpInfo,TcpInfoSize,TcpiSackedOffset,
		 TcpiOptionsOffset,TcpiSackedSize,TcpiOptionsSize,
		 TcpLinger2]) of
	false ->
	    {skipped,"Does not run on this OS."};
	true ->
	    {Sock,I} = create_socketpair(Config, [], []),
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
	    {Sock1,Sock2} = create_socketpair(Config,
                                              [{linger,{true,10}}],
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
            case {?IS_SOCKET_BACKEND(Config),
                  inet:getopts(Sock1,[{raw,Proto,Linger,TooLargeSize}])} of
                {false, {error,einval}} ->
                    ok;
                {true,  {ok, _}} ->
                    ok;
                {_, Unexpected} ->
                    ?P("unexpected result: "
                       "~n   ~p", [Unexpected]),
                    ct:fail({unexpected, Unexpected})
            end,
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
		create_socketpair(Config,
                                  [{keepalive,true},
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
		create_socketpair(Config,
                                  [RawLingerOn,{keepalive,true}],
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
		create_socketpair(Config,
                                  [{packet,4},RawLingerOn,{keepalive,true}],
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
    ?TC_TRY(use_ipv6_v6only_udp,
	    fun() -> do_use_ipv6_v6only_udp(Config) end).

do_use_ipv6_v6only_udp(Config) ->
    case gen_udp:open(0, [inet6,{ip,{0,0,0,0,0,0,0,1}}, {ipv6_v6only,true}]) of
	{ok,S6} ->
	    ?P("IPv6 socket option created - ensure ipv6_v6only"),
	    case inet:getopts(S6, [ipv6_v6only]) of
		{ok,[{ipv6_v6only,true}]} ->
		    use_ipv6_v6only_udp(Config, S6);
		{ok, Other} ->
		    ?P("IPv6 socket option created - "
		       "failed ensuring ipv6_v6only"),
		    exit({skip, {getopts, Other}})
	    end;
	{error, OReason} ->
	    ?P("failed create IPv6 socket: "
	       "~n   ~p", [OReason]),
	    exit({skip, "Socket type not supported"})
    end.

use_ipv6_v6only_udp(_Config, S6) ->
    {ok, Port} = inet:port(S6),
    ?P("IPv6 socket port number: ~w", [Port]),
    S4 = case gen_udp:open(Port, [inet]) of
	     {ok, Sock4} ->
		 ?P("IPv4 socket created with port number ~w", [Port]),
		 Sock4;
	     {error, eaddrinuse = Reason} ->
		 ?P("failed create IPv4 socket: ~p => skip", [Reason]),
		 exit({skip, Reason});
	     {error, Reason} ->
		 ?P("failed create IPv4 socket: "
		    "~n   ~p", [Reason]),
		 ct:fail({failed_open, inet, Reason})
	 end,
    E6 = " IPv6-echo.",
    E4 = " IPv4-echo.",
    {Sender, MRef} =
	spawn_monitor(fun () ->
			      use_ipv6_v6only_udp_sender(Port, E6, E4)
		      end),
    use_ipv6_v6only_udp_listener(
      S6, S4, E6, E4, Sender, MRef).

use_ipv6_v6only_udp_listener(S6, S4, E6, E4, Sender, Mref) ->
    ?P("await upd message"),
    receive
	{udp,S6,IP,P,Data} ->
	    ?P("received (IPv6) upd message"),
	    ok = gen_udp:send(S6, IP, P, [Data|E6]),
	    use_ipv6_v6only_udp_listener(S6, S4, E6, E4, Sender, Mref);
	{udp,S4,IP,P,Data} ->
	    ?P("received (IPv4) upd message"),
	    ok = gen_udp:send(S4, IP, P, [Data|E4]),
	    use_ipv6_v6only_udp_listener(S6, S4, E6, E4, Sender, Mref);
	{'DOWN', Mref,_,_, normal} ->
	    ?P("received expected normal down message"),
	    ok;
	{'DOWN', Mref,_,_, Result} ->
	    %% Since we are linked we will never arrive here
	    ?P("received expected down message: "
	       "~n   ~p", [Result]),
	    Result;
	Other ->
	    ?P("received unexpected message: "
	       "~n   ~p", [Other]),
	    exit({failed, {listener_unexpected, Other}})
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
    ?P("[~w:~w] ! ~s", [Ip, Port, Data]),
    ok = gen_udp:send(S, Ip, Port, Data),
    receive
	{udp, S, Ip, Port, RecData} ->
	    ?P("[~w:~w] : ~s", [Ip, Port, RecData]),
	    RecData;
	Other ->
	    ?P("received unexpected message: "
	       "~n   ~p", [Other]),
	    exit({failed, {sndrcv_unexpectec, Other}})
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
			      catch create_socketpair(Config, [Option], []) of
			      {'EXIT',badarg} ->
				  ok;
			      Unexpected1 ->
				  exit({unexpected,
					Unexpected1})
			  end,
			  case
			      catch create_socketpair(Config, [], [Option]) of
			      {'EXIT', badarg} ->
				  ok;
			      Unexpected2 ->
				  exit({unexpected,
					Unexpected2})
			  end,
			  {Sock1,Sock2} = create_socketpair(Config, [], []),
			  case inet:setopts(Sock1, [Option]) of
			      {error, einval} ->
				  ok;
			      Unexpected3 ->
				  exit({unexpected,
					Unexpected3})
			  end,
			  gen_tcp:close(Sock1),
			  gen_tcp:close(Sock2)
		  end,BadSetOptions),
    {Sock1,Sock2} = create_socketpair(Config, [], []),
    lists:foreach(fun(Option) ->
			  case inet:getopts(Sock1, [Option]) of
			      {error, einval} ->
				  ok;
			      Unexpected ->
				  exit({unexpected,
					Unexpected})
			  end
		  end,BadGetOptions),
    gen_tcp:close(Sock1),
    gen_tcp:close(Sock2),
    ok.

windows_reuseaddr(Config) when is_list(Config) ->
    %% Check that emulation of reuseaddr and reuseport on Windows
    %% works as expected. That is, only set SO_REUSEADDR if both
    %% reuseaddr and reuseport are set.
    case os:type() of
        {win32, _} ->
            Port = start_helper(Config),
            Def = {ask_helper(Port,?C_GET_SOL_SOCKET),
                   ask_helper(Port,?C_GET_SO_REUSEADDR)},
            stop_helper(Port),
            {false, false} = windows_reuseaddr_test(Config,
                                                    Def,
                                                    [{reuseaddr,false},{reuseport,false}],
                                                    [{reuseaddr,false},{reuseport,false}]),
            {false, false} = windows_reuseaddr_test(Config,
                                                    Def,
                                                    [{reuseaddr,true},{reuseport,false}],
                                                    [{reuseaddr,true},{reuseport,false}]),
            {false, false} = windows_reuseaddr_test(Config,
                                                    Def,
                                                    [{reuseaddr,false},{reuseport,true}],
                                                    [{reuseaddr,false},{reuseport,true}]),
            {true, true} = windows_reuseaddr_test(Config,
                                                  Def,
                                                  [{reuseaddr,true},{reuseport,true}],
                                                  [{reuseaddr,true},{reuseport,true}]),
            ok;
        _ ->
            {skipped, "Test for Windows only"}
    end.

windows_reuseaddr_test(Config, {SolSocket, SoReuseaddr}, LOpts, COpts) ->
    OptNames = fun (Opts) ->
                       lists:map(fun ({Name,_}) -> Name end, Opts)
               end,
    {L,A,C} = create_socketpair_init(Config, LOpts, COpts),
    {ok, LOpts} = inet:getopts(L, OptNames(LOpts)),
    {ok, COpts} = inet:getopts(L, OptNames(COpts)),
    RawOpts = [{raw, SolSocket, SoReuseaddr, 4}],
    {ok,[{raw,SolSocket,SoReuseaddr,LRes}]} = inet:getopts(L, RawOpts),
    LReuseaddr = case nintbin2int(LRes) of
                     0 -> false;
                     _ -> true
                 end,
    {ok,[{raw,SolSocket,SoReuseaddr,CRes}]} = inet:getopts(L, RawOpts),
    CReuseaddr = case nintbin2int(CRes) of
                     0 -> false;
                     _ -> true
                 end,
    gen_tcp:close(L),
    gen_tcp:close(A),
    gen_tcp:close(C),
    {LReuseaddr, CReuseaddr}.


all_ok([]) ->
    true;
all_ok([H|T]) when H >= 0 ->
    all_ok(T);
all_ok(_) ->
    false.


make_check_fun(Config, Type) ->
    fun({Name,V1,V2,Mand,Chang},Acc) ->
            {LO1,CO1} = case Type of
                            connect -> {[],[{Name,V1}]};
                            _ -> {[{Name,V1}],[]}
                        end,
            {LO2,CO2} = case Type of
                            connect -> {[],[{Name,V2}]};
                            _ -> {[{Name,V2}],[]}
                        end,
	    {X1,Y1,Z1} = create_socketpair_init(Config, LO1, CO1),
	    {X2,Y2,Z2} = create_socketpair_init(Config, LO2, CO2),
            {S1,S2} = case Type of
                          listen -> {X1,X2};
                          accept -> {Y1,Y2};
                          connect -> {Z1,Z2}
                      end,
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
	    gen_tcp:close(Z1),
	    gen_tcp:close(X2),
	    gen_tcp:close(Y2),
	    gen_tcp:close(Z2),
	    NewAcc
    end.

make_check_fun(_Config) ->
    fun ({Name,V1,V2,Mand,Chang} = Spec, Acc) ->
            try
                {ok,S1} = gen_udp:open(0, [{Name,V1}]),
                {ok,S2} = gen_udp:open(0, [{Name,V2}]),
                try
                    {ok,[{Name,R1}]} = inet:getopts(S1, [Name]),
                    {ok,[{Name,R2}]} = inet:getopts(S2, [Name]),
                    case R1 =/= R2 of
                        true ->
                            case Chang of
                                true ->
                                    ok = inet:setopts(S1, [{Name,V2}]),
                                    {ok,[{Name,R3}]} =
                                        inet:getopts(S1, [Name]),
                                    if
                                        R3 =/= R1, R3 =:= R2 ->
                                            Acc;
                                        true ->
                                            case Mand of
                                                true ->
                                                    exit(
                                                      {failed_sockopt,
                                                       {change,Name}});
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
                                    exit({failed_sockopt, {udp,Name}});
                                false ->
                                    [{udp,Name}|Acc]
                            end
                    end
                after
                    gen_udp:close(S1),
                    gen_udp:close(S2)
                end
            catch Class : Reason : Stacktrace ->
                    erlang:raise(Class, {fail,Reason,Spec}, Stacktrace)
            end
    end.

%% {OptionName,Value1,Value2,Mandatory,Changeable}
all_listen_options() ->
    OsType = os:type(),
    OsVersion = os:version(),
    [{tos,0,1,false,true}, 
     {priority,0,1,false,true}, 
     {reuseaddr,false,true,mandatory_reuseaddr(OsType,OsVersion),false},
     {reuseport,false,true,mandatory_reuseport(OsType,OsVersion),false},
     {reuseport_lb,false,true,mandatory_reuseport_lb(OsType,OsVersion),false},
     {exclusiveaddruse,false,true,mandatory_exclusiveaddruse(OsType,OsVersion),false},
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

all_accept_options() ->
    A0 = lists:keydelete(exclusiveaddruse, 1, all_listen_options()),
    A1 = lists:keydelete(reuseaddr, 1, A0),
    A2 = lists:keydelete(reuseport, 1, A1),
    lists:keydelete(reuseport_lb, 1, A2).

all_connect_options() ->
    OsType = os:type(),
    OsVersion = os:version(),
    [{tos,0,1,false,true}, 
     {priority,0,1,false,true}, 
     {reuseaddr,false,true,mandatory_reuseaddr(OsType,OsVersion),false},
     {reuseport,false,true,mandatory_reuseport(OsType,OsVersion),false},
     {reuseport_lb,false,true,mandatory_reuseport_lb(OsType,OsVersion),false},
     {exclusiveaddruse,false,true,mandatory_exclusiveaddruse(OsType,OsVersion),false},
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


all_udp_options() ->
    OsType = os:type(),
    OsVersion = os:version(),
    [{tos,0,1,false,true},
     {priority,0,1,false,true},
     {reuseaddr,false,true,mandatory_reuseaddr(OsType,OsVersion),false},
     {reuseport,false,true,mandatory_reuseport(OsType,OsVersion),false},
     {reuseport_lb,false,true,mandatory_reuseport_lb(OsType,OsVersion),false},
     {exclusiveaddruse,false,true,
      mandatory_exclusiveaddruse(OsType,OsVersion),false},
     {sndbuf,2048,4096,false,true},
     {recbuf,2048,4096,false,true},
     {header,2,4,true,true},
     {active,false,true,true,false},
     {buffer,1000,2000,true,true},
     {mode,list,binary,true,true},
     {deliver,term,port,true,true},
     {broadcast,true,false,true,true},
     {dontroute,true,false,
      lists:member(OsType, [{unix,linux},{unix,freebsd},{unix,darwin}]),
      true},
     %% multicast_if
     %% multicast_ttl
     %% multicast_loop
     %% add_membership
     %% drop_membership
     {read_packets,6,7,true,true},
     {high_msgq_watermark,4096,8192,true,true},
     {low_msgq_watermark,2048,4096,true,true}].


%% Mandatory on a lot of system other than those listed below. Please add more...
mandatory_reuseaddr({unix, linux}, _OsVersion) ->
    true;
mandatory_reuseaddr({unix, freebsd}, _OsVersion) ->
    true;
mandatory_reuseaddr({unix, darwin}, _OsVersion) ->
    true;
mandatory_reuseaddr({win32, _}, _OsVersion) ->
    true; %% reuseaddr and reuseport are emulated by the inet-driver
mandatory_reuseaddr(_OsType, _OsVersion) ->
    false.

%% Mandatory an a lot of system other than those listed below. Please add more...
mandatory_reuseport({win32, _}, _OsVersion) ->
    true; %% reuseaddr and reuseport are emulated by the inet-driver
mandatory_reuseport({unix, linux}, {X,Y,_Z}) when X > 4 orelse X == 4 andalso Y >= 6 ->
    true;
mandatory_reuseport({unix, freebsd}, {X,Y,_Z}) when X > 11 orelse X == 11 andalso Y >= 4 ->
    %% I know that it is available on 11.4, but it may be available earlier...
    true;
mandatory_reuseport({unix, darwin}, {X,Y,_Z}) when X > 26 orelse X == 26 andalso Y >= 6 ->
    %% I know that it is available on 26.6, but it may be available earlier...
    true;
mandatory_reuseport(_OsType, _OsVersion) ->
    false.

%% Perhaps mandatory an system other than those listed below. Please add more...
mandatory_reuseport_lb({unix, linux}, {X,Y,_Z}) when X > 4 orelse X == 4 andalso Y >= 6 ->
    true;
mandatory_reuseport_lb({unix, freebsd}, {X,Y,_Z}) when X > 13 orelse X == 13 andalso Y >= 1 ->
    %% I know that it is available on 13.1, but it may be available earlier...
    true;
mandatory_reuseport_lb(_OsType, _OsVersion) ->
    false.

mandatory_exclusiveaddruse({win32, _}, {X,Y,_Z}) when X > 5 orelse X == 5 andalso Y >= 2 ->
    true;
mandatory_exclusiveaddruse(_OsType, _OsVersion) ->
    false.

create_socketpair_init(Config, ListenOptions, ConnectOptions) ->
    {ok,LS}   = ?LISTEN(Config, 0, ListenOptions),
    {ok,Port} = inet:port(LS),
    {ok,CS}   = ?CONNECT(Config, localhost, Port, ConnectOptions),
    {ok,AS}   = gen_tcp:accept(LS),
    {LS,AS,CS}.

create_socketpair(Config, ListenOptions, ConnectOptions) ->
    {LS,AS,CS} = create_socketpair_init(Config, ListenOptions, ConnectOptions),
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Here are all the *general* test case condition functions.

is_not_openbsd() ->
    is_not_platform(openbsd, "OpenBSD").

is_not_platform(Platform, PlatformStr)
  when is_atom(Platform) andalso is_list(PlatformStr) ->
      case os:type() of
          {unix, Platform} ->
              skip("This does not work on " ++ PlatformStr);
        _ ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

links() ->
    pi(links).

monitors() ->
    pi(monitors).

pi(Item) ->
    {Item, Val} = process_info(self(), Item),
    Val.
    
