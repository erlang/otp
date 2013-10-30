%% @doc Example file showing how to use net_kernel's transport
%%      callback feature. Pass:
%%          `-proto_dist_mf dist_selector select' or
%%          `-proto_dist_mf dist_selector select_first'
%%      to apply custom logic to selecting the transport used
%%      to connect to a remote node.
%% @author Serge Aleynikov <saleyn@gmail.com>
-module(dist_selector).
-export([select/4, select_first/4, is_local_address/2]).

-type port_mod() :: {Port::integer(), Mod::atom()}.

%% @doc This is a sample callback invoked by net_kernel
%%      when it attempts to establish connection to a peer node.
%%      It returns first distribution transport handling module/port
%%      from the `PeerPortMods' list.
-spec select_first(LocalProtoMods::[Mod::atom()],
                   PeerNode::atom(), PeerIp::tuple(),
                   PeerPortMods::[port_mod()]) ->
                   {ok, port_mod()} | false.
select_first(_LocalProtoMods, _PeerNode, _PeerIp, PeerPortMods) ->
    {ok, hd(PeerPortMods)}.


%% @doc This is a sample callback invoked by net_kernel
%%      when it attempts to establish connection to a peer node.
%%      Is returns TCP transport port/module for nodes in local subnet
%%      and SSL transport port/module for remote subnets.
-spec select(LocalProtoMods::[Mod::atom()],
             PeerNode::atom(), PeerIp::tuple(),
             PeerPortMods::[port_mod()]) ->
                {ok, port_mod()} | false.
select(LocalProtoMods, _PeerNode, PeerIp, PeerPortMods) ->
    {ok, IFs} = inet:getif(),
    LocAddrs = [{A, M} || {A,_,M} <- IFs],
    %Out = io_lib:format("Connecting ~w -> ~w at ~w\n"
    %                    "  LocalAddrs:  ~p\n"
    %                    "  PeerPorts:   ~p\n",
    %    [node(), PeerNode, PeerIp, LocAddrs, PeerPortMods]),

    % First see if there's a local support of SSL enabled
    case has_local_ssl(LocalProtoMods) of
    true ->
        R = try_select_peer_ssl(LocAddrs, PeerIp, PeerPortMods);
    false ->
        R = find_remote_tcp_portmod(PeerPortMods)
    end,
    %io:format("~s\n  Result: ~p\n", [Out, R1]),
    {ok, R}.

has_local_ssl(List) ->
    lists:member(inet_tls_dist, List).

try_select_peer_ssl(LocalAddrs, PeerIp, PeerPortMods) ->
    % See if the connection attempt is to the local subnet (Class C)
    case is_local_address(PeerIp, LocalAddrs) of
    true ->
        find_remote_tcp_portmod(PeerPortMods);
    false ->
        find_remote_ssl_portmod(PeerPortMods)
    end.

is_local_address(_, []) ->
    false;
is_local_address(RA, [{A,M}|T]) when tuple_size(RA) =:= tuple_size(A)
                                   , tuple_size(M)  =:= tuple_size(A) ->
    case match_local_address(RA, A, M, tuple_size(M)) of
    true  -> true;
    false -> is_local_address(RA, T)
    end;
is_local_address(RA, [_|T]) ->
    is_local_address(RA, T).

match_local_address(_, _, _, 0) ->
    true;
match_local_address(RA, A, M, N) ->
    ME  = element(N, M),
    RAE = element(N, RA) band ME,
    LAE = element(N, A)  band ME,
    case RAE of
    LAE -> match_local_address(RA, A, M, N-1);
    _   -> false
    end.

find_remote_ssl_portmod(PortMods) ->
    case lists:keyfind(inet_ssl_dist, 2, PortMods) of
    Value when is_tuple(Value) ->
        Value;
    false ->
        find_remote_tcp_portmod(PortMods)
    end.

find_remote_tcp_portmod(PortMods) ->
    case lists:keyfind(inet_tcp_dist, 2, PortMods) of
    Value when is_tuple(Value) ->
        Value;
    false ->
        % Give up and just take first available transport port
        hd(PortMods)
    end.
