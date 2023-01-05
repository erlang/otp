%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2023. All Rights Reserved.
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
-module(inet_tcp_dist).
-feature(maybe_expr, enable).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/1, listen/2, accept/1, accept_connection/5,
	 setup/5, close/1, select/1, address/0, is_node_name/1]).

%% Optional
-export([setopts/2, getopts/2]).

%% Generalized dist API
-export([gen_listen/3, gen_accept/2, gen_accept_connection/6,
	 gen_setup/6, gen_select/2, gen_address/1]).
-export([fam_select/2, fam_address/1, fam_listen/4, fam_setup/4]).
%% OTP internal (e.g ssl)
-export([gen_hs_data/2, nodelay/0]).

-export([merge_options/2, merge_options/3]).

%% internal exports

-export([accept_loop/3,do_accept/7,do_setup/7,getstat/1,tick/2]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").

-include("dist.hrl").
-include("dist_util.hrl").

-define(DRIVER, inet_tcp).
-define(PROTOCOL, tcp).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    gen_select(?DRIVER, Node).

gen_select(Driver, Node) ->
    fam_select(Driver:family(), Node).

fam_select(Family, Node) ->
    case dist_util:split_node(Node) of
	{node, Name, Host} ->
            EpmdMod = net_kernel:epmd_module(),
            case
                call_epmd_function(
                  EpmdMod, address_please, [Name, Host, Family])
            of
                {ok, _Addr} -> true;
                {ok, _Addr, _Port, _Creation} -> true;
                _ -> false
            end;
	_ -> false
    end.

%% ------------------------------------------------------------
%% Get the address family that this distribution uses
%% ------------------------------------------------------------
address() ->
    gen_address(?DRIVER).

gen_address(Driver) ->
    fam_address(Driver:family()).

fam_address(Family) ->
    {ok, Host} = inet:gethostname(),
    #net_address{
       host = Host,
       protocol = ?PROTOCOL,
       family = Family
      }.

%% ------------------------------------------------------------
%% Set up the general fields in #hs_data{}
%% ------------------------------------------------------------
gen_hs_data(Driver, Socket) ->
    %% The only thing Driver actually is used for is to
    %% implement non-blocking send of distribution tick
    Nodelay = nodelay(),
    #hs_data{
       socket = Socket,
       f_send = fun Driver:send/2,
       f_recv = fun Driver:recv/3,
       f_setopts_pre_nodeup =
           fun (S) ->
                   inet:setopts(
                     S,
                     [{active, false}, {packet, 4}, Nodelay])
           end,
       f_setopts_post_nodeup =
           fun (S) ->
                   inet:setopts(
                     S,
                     [{active, true}, {packet,4},
                      {deliver, port}, binary, Nodelay])
           end,
       f_getll    = fun inet:getll/1,
       mf_tick    = fun (S) -> ?MODULE:tick(Driver, S) end,
       mf_getstat = fun ?MODULE:getstat/1,
       mf_setopts = fun ?MODULE:setopts/2,
       mf_getopts = fun ?MODULE:getopts/2}.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

%% Keep this function for third-party dist controllers reusing this API
listen(Name) ->
    {ok, Host} = inet:gethostname(),
    listen(Name, Host).

listen(Name, Host) ->
    gen_listen(?DRIVER, Name, Host).

gen_listen(Driver, Name, Host) ->
    ForcedOptions = [{active, false}, {packet,2}, {nodelay, true}],
    ListenFun =
        fun (First, Last, ListenOptions) ->
                listen_loop(
                  Driver, First, Last,
                  merge_options(ListenOptions, ForcedOptions))
        end,
    Family = Driver:family(),
    maybe
        %%
        {ok, {ListenSocket, Address, Creation}} ?=
            fam_listen(Family, Name, Host, ListenFun),
        NetAddress =
            #net_address{
               host = Host,
               protocol = ?PROTOCOL,
               family = Family,
               address = Address},
        {ok, {ListenSocket, NetAddress, Creation}}
    end.

listen_loop(_Driver, First, Last, _Options) when First > Last ->
    {error,eaddrinuse};
listen_loop(Driver, First, Last, Options) ->
    case Driver:listen(First, Options) of
        {error, eaddrinuse} ->
	    listen_loop(Driver, First+1, Last, Options);
        Other ->
            Other
    end.


fam_listen(Family, Name, Host, ListenFun) ->
    maybe
        EpmdMod = net_kernel:epmd_module(),
        %%
        {ok, ListenSocket} ?=
            case
                call_epmd_function(
                  EpmdMod, listen_port_please, [Name, Host])
            of
                {ok, 0} ->
                    {First,Last} = get_port_range(),
                    ListenFun(First, Last, listen_options());
                {ok, PortNum} ->
                    ListenFun(PortNum, PortNum, listen_options())
            end,
        {ok, {_IP,Port} = Address} = inet:sockname(ListenSocket),
        %%
        {ok, Creation} ?=
            EpmdMod:register_node(Name, Port, Family),
        {ok, {ListenSocket, Address, Creation}}
    end.

get_port_range() ->
    case application:get_env(kernel,inet_dist_listen_min) of
        {ok,N} when is_integer(N) ->
            case application:get_env(kernel,inet_dist_listen_max) of
                {ok,M} when is_integer(M) ->
                    {N,M};
                _ ->
                    {N,N}
            end;
        _ ->
            {0,0}
    end.


listen_options() ->
    DefaultOpts = [{reuseaddr, true}, {backlog, 128}],
    ForcedOpts =
        case application:get_env(kernel, inet_dist_use_interface) of
            {ok, Ip}  -> [{ip, Ip}];
            undefined -> []
        end,
    InetDistListenOpts =
        case application:get_env(kernel, inet_dist_listen_options) of
            {ok, Opts} -> Opts;
            undefined  -> []
        end,
    merge_options(InetDistListenOpts, ForcedOpts, DefaultOpts).


merge_options(Opts, ForcedOpts) ->
    merge_options(Opts, ForcedOpts, []).
%%
merge_options(Opts, ForcedOpts, DefaultOpts) ->
    Forced = merge_options(ForcedOpts),
    Default = merge_options(DefaultOpts),
    ForcedOpts ++ merge_options(Opts, Forced, DefaultOpts, Default).

%% Collect expanded 2-tuple options in a map
merge_options(Opts) ->
    lists:foldr(
      fun (Opt, Acc) ->
              case expand_option(Opt) of
                  {OptName, OptVal} ->
                      maps:put(OptName, OptVal, Acc);
                  _ ->
                      Acc
              end
      end, #{}, Opts).

%% Pass through all options that are not forced,
%% which we already have prepended,
%% and remove options that we see from the Default map
%%
merge_options([Opt | Opts], Forced, DefaultOpts, Default) ->
    case expand_option(Opt) of
        {OptName, _} ->
            %% Remove from the Default map
            Default_1 = maps:remove(OptName, Default),
            if
                is_map_key(OptName, Forced) ->
                    %% Forced option - do not pass through
                    merge_options(Opts, Forced, DefaultOpts, Default_1);
                true ->
                    %% Pass through
                    [Opt |
                     merge_options(Opts, Forced, DefaultOpts, Default_1)]
            end;
        _ ->
            %% Unhandled options e.g {raw, ...} - pass through
            [Opt | merge_options(Opts, Forced, DefaultOpts, Default)]
    end;
merge_options([], _Forced, DefaultOpts, Default) ->
    %% Append the needed default options (that we have not seen)
    [Opt ||
        Opt <- DefaultOpts,
        is_map_key(element(1, expand_option(Opt)), Default)].

%% Expand an atom option into its tuple equivalence,
%% pass through others
expand_option(Opt) ->
    if
        Opt =:= list; Opt =:= binary ->
            {mode, Opt};
        Opt =:= inet; Opt =:= inet6; Opt =:= local ->
            %% 'family' is not quite an option name, but could/should be
            {family, Opt};
        true ->
            Opt
    end.

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    gen_accept(?DRIVER, Listen).

gen_accept(Driver, Listen) ->
    spawn_opt(?MODULE, accept_loop, [Driver, self(), Listen], [link, {priority, max}]).

accept_loop(Driver, Kernel, Listen) ->
    case Driver:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,Driver:family(),?PROTOCOL},
	    _ = controller(Driver, Kernel, Socket),
	    accept_loop(Driver, Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Driver, Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    Driver:controlling_process(Socket, Pid),
	    flush_controller(Pid, Socket),
	    Pid ! {self(), controller};
	{Kernel, unsupported_protocol} ->
	    exit(unsupported_protocol)
    end.

flush_controller(Pid, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Pid ! {tcp, Socket, Data},
	    flush_controller(Pid, Socket);
	{tcp_closed, Socket} ->
	    Pid ! {tcp_closed, Socket},
	    flush_controller(Pid, Socket)
    after 0 ->
	    ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    gen_accept_connection(?DRIVER, AcceptPid, Socket, MyNode, Allowed, SetupTime).

gen_accept_connection(Driver, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    spawn_opt(?MODULE, do_accept,
	      [Driver, self(), AcceptPid, Socket, MyNode, Allowed, SetupTime],
	      dist_util:net_ticker_spawn_options()).

do_accept(Driver, Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    receive
	{AcceptPid, controller} ->
	    Timer = dist_util:start_timer(SetupTime),
	    case check_ip(Driver, Socket) of
		true ->
                    Family = Driver:family(),
                    HSData =
                        (gen_hs_data(Driver, Socket))
                        #hs_data{
                          kernel_pid = Kernel,
                          this_node = MyNode,
                          timer = Timer,
                          this_flags = 0,
                          allowed = Allowed,
                          f_address =
                              fun (S, Node) ->
                                      get_remote_id(Family, S, Node)
                              end},
		    dist_util:handshake_other_started(HSData);
		{false,IP} ->
		    error_msg("** Connection attempt from "
			      "disallowed IP ~w ** ~n", [IP]),
		    ?shutdown(no_node)
	    end
    end.


%% we may not always want the nodelay behaviour
%% for performance reasons

nodelay() ->
    case application:get_env(kernel, dist_nodelay) of
	undefined ->
	    {nodelay, true};
	{ok, true} ->
	    {nodelay, true};
	{ok, false} ->
	    {nodelay, false};
	_ ->
	    {nodelay, true}
    end.

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------
get_remote_id(Family, Socket, Node) ->
    case inet:peername(Socket) of
	{ok,Address} ->
	    case split_node(atom_to_list(Node), $@, []) of
		[_,Host] ->
		    #net_address{address=Address,host=Host,
				 protocol=?PROTOCOL,family=Family};
		_ ->
		    %% No '@' or more than one '@' in node name.
		    ?shutdown(no_node)
	    end;
	{error, _Reason} ->
	    ?shutdown(no_node)
    end.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    gen_setup(?DRIVER, Node, Type, MyNode, LongOrShortNames, SetupTime).

gen_setup(Driver, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    spawn_opt(?MODULE, do_setup,
	      [Driver, self(), Node, Type, MyNode, LongOrShortNames, SetupTime],
	      dist_util:net_ticker_spawn_options()).

do_setup(Driver, Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    ?trace("~p~n",[{?MODULE,self(),setup,Node}]),
    Timer = dist_util:start_timer(SetupTime),
    Family = Driver:family(),
    {#net_address{ address = {Ip, TcpPort} } = NetAddress,
     ConnectOptions,
     Version} =
        fam_setup(
          Family, Node, LongOrShortNames, fun Driver:parse_address/1),
    dist_util:reset_timer(Timer),
    case Driver:connect(Ip, TcpPort, ConnectOptions) of
        {ok, Socket} ->
            HSData =
                (gen_hs_data(Driver, Socket))
                #hs_data{
                  kernel_pid = Kernel,
                  other_node = Node,
                  this_node = MyNode,
                  timer = Timer,
                  this_flags = 0,
                  other_version = Version,
                  f_address =
                      fun(_,_) ->
                              NetAddress
                      end,
                  request_type = Type},
            dist_util:handshake_we_started(HSData);
        _ ->
            %% Other Node may have closed since
            %% discovery !
            ?trace("other node (~p) "
                   "closed since discovery (port_please).~n",
                   [Node]),
            ?shutdown(Node)
    end.

fam_setup(Family, Node, LongOrShortNames, ParseAddress) ->
    ?trace("~p~n",[{?MODULE,self(),?FUNCTION_NAME,Node}]),
    [Name, Host] = splitnode(ParseAddress, Node, LongOrShortNames),
    ErlEpmd = net_kernel:epmd_module(),
    case
        call_epmd_function(
          ErlEpmd, address_please, [Name, Host, Family])
    of
	{ok, Ip, TcpPort, Version} ->
            ?trace("address_please(~p) -> version ~p~n", [Node,Version]),
            fam_setup(Family, Host, Ip, TcpPort, Version);
        {ok, Ip} ->
	    case ErlEpmd:port_please(Name, Ip) of
		{port, TcpPort, Version} ->
		    ?trace("port_please(~p) -> version ~p~n",
			   [Node,Version]),
                    fam_setup(Family, Host, Ip, TcpPort, Version);
		_ ->
		    ?trace("port_please (~p) failed.~n", [Node]),
		    ?shutdown(Node)
	    end;
	_Other ->
	    ?trace("inet_getaddr(~p) failed (~p).~n", [Node,_Other]),
	    ?shutdown(Node)
    end.

fam_setup(Family, Host, Ip, TcpPort, Version) ->
    NetAddress =
        #net_address{
           address = {Ip, TcpPort},
           host = Host,
           protocol = ?PROTOCOL,
           family = Family},
    {NetAddress, connect_options(), Version}.

connect_options() ->
    merge_options(
      case application:get_env(kernel, inet_dist_connect_options) of
          {ok, ConnectOpts} ->
              ConnectOpts;
          _ ->
              []
      end, [{active, false}, {packet, 2}]).


%%
%% Close a socket.
%%
close(Socket) ->
    ?DRIVER:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(ParseAddress, Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail =/= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames =:= longnames ->
                    case ParseAddress(Host) of
                        {ok, _} ->
                            [Name, Host];
                        _ ->
                            error_msg("** System running to use "
                                      "fully qualified "
                                      "hostnames **~n"
                                      "** Hostname ~ts is illegal **~n",
                                      [Host]),
                            ?shutdown(Node)
                    end;
		L when length(L) > 1, LongOrShortNames =:= shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~ts is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		_ ->
		    [Name, Host]
	    end;
	[_] ->
	    error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

%% ------------------------------------------------------------
%% Determine if EPMD module supports the called functions.
%% If not call the builtin erl_epmd
%% ------------------------------------------------------------
call_epmd_function(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true -> apply(Mod,Fun,Args);
        _    -> apply(erl_epmd, Fun, Args)
    end.

%% ------------------------------------------------------------
%% Do only accept new connection attempts from nodes at our
%% own LAN, if the check_ip environment parameter is true.
%% ------------------------------------------------------------
check_ip(Driver, Socket) ->
    case application:get_env(check_ip) of
	{ok, true} ->
	    case get_ifs(Socket) of
		{ok, IFs, IP} ->
		    check_ip(Driver, IFs, IP);
		_ ->
		    ?shutdown(no_node)
	    end;
	_ ->
	    true
    end.

get_ifs(Socket) ->
    case inet:peername(Socket) of
	{ok, {IP, _}} ->
	    case inet:getif(Socket) of
		{ok, IFs} -> {ok, IFs, IP};
		Error     -> Error
	    end;
	Error ->
	    Error
    end.

check_ip(Driver, [{OwnIP, _, Netmask}|IFs], PeerIP) ->
    case {Driver:mask(Netmask, PeerIP), Driver:mask(Netmask, OwnIP)} of
	{M, M} -> true;
	_      -> check_ip(Driver, IFs, PeerIP)
    end;
check_ip(_Driver, [], PeerIP) ->
    {false, PeerIP}.
    
is_node_name(Node) when is_atom(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, _Host] -> true;
	_ -> false
    end;
is_node_name(_Node) ->
    false.

tick(Driver, Socket) ->
    case Driver:send(Socket, [], [force]) of
	{error, closed} ->
	    self() ! {tcp_closed, Socket},
	    {error, closed};
	R ->
	    R
    end.

getstat(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
	{ok, Stat} ->
	    split_stat(Stat,0,0,0);
	Error ->
	    Error
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.


setopts(S, Opts) ->
    case [Opt || {K,_}=Opt <- Opts,
		 K =:= active orelse K =:= deliver orelse K =:= packet] of
	[] -> inet:setopts(S,Opts);
	Opts1 -> {error, {badopts,Opts1}}
    end.

getopts(S, Opts) ->
    inet:getopts(S, Opts).
