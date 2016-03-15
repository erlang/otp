%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%
%%-----------------------------------------------------------------
%% File: orber_env.erl
%%
%% Description:
%%    Handling environment parameters for Orber.
%%
%% Creation date: 040723
%%
%%-----------------------------------------------------------------
-module(orber_env).

-behaviour(gen_server).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, configure/2, configure/3, configure_override/2,
	 multi_configure/1, get_env/1, set_env/2, get_keys/0, env/1,
	 info/0, info/1]).

-export([iiop_acl/0, iiop_port/0, nat_iiop_port/0, nat_iiop_port/1, iiop_out_ports/0,
	 domain/0, ip_address_variable_defined/0, nat_host/0, nat_host/1, host/0,
	 ip_address/0, ip_address/1, giop_version/0, iiop_timeout/0, iiop_out_ports_random/0,
	 iiop_connection_timeout/0, iiop_setup_connection_timeout/0, iiop_out_ports_attempts/0,
	 iiop_in_connection_timeout/0, iiop_max_fragments/0, iiop_max_in_requests/0,
	 iiop_max_in_connections/0, iiop_backlog/0, objectkeys_gc_time/0,
	 get_ORBInitRef/0, get_ORBDefaultInitRef/0, get_interceptors/0,
	 get_local_interceptors/0, get_cached_interceptors/0,
	 set_interceptors/1, is_lightweight/0, get_lightweight_nodes/0, secure/0,
	 iiop_ssl_backlog/0, iiop_ssl_port/0, nat_iiop_ssl_port/0, nat_iiop_ssl_port/1,
	 ssl_server_options/0, ssl_client_options/0, set_ssl_client_options/1,
	 ssl_server_certfile/0, ssl_client_certfile/0, set_ssl_client_certfile/1,
	 ssl_server_verify/0, ssl_client_verify/0, set_ssl_client_verify/1,
	 ssl_server_depth/0, ssl_client_depth/0, set_ssl_client_depth/1,
	 ssl_server_cacertfile/0, ssl_client_cacertfile/0,
	 set_ssl_client_cacertfile/1, ssl_client_password/0,
	 ssl_server_password/0, ssl_client_keyfile/0, ssl_server_keyfile/0,
	 ssl_client_ciphers/0, ssl_server_ciphers/0, ssl_client_cachetimeout/0,
	 ssl_server_cachetimeout/0,
	 get_flags/0, typechecking/0,
	 exclude_codeset_ctx/0, exclude_codeset_component/0, partial_security/0,
	 use_CSIv2/0, use_FT/0, ip_version/0, light_ifr/0, bidir_context/0,
	 get_debug_level/0, getaddrstr/2, addr2str/1, iiop_packet_size/0,
	 iiop_ssl_ip_address_local/0, ip_address_local/0, iiop_in_keepalive/0,
	 iiop_out_keepalive/0, iiop_ssl_in_keepalive/0, iiop_ssl_out_keepalive/0,
	 iiop_ssl_accept_timeout/0, ssl_generation/0]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3]).
-export([handle_cast/2, handle_info/2, code_change/3]).

%%-----------------------------------------------------------------
%% Record Definitions Etc.
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

-define(FORMAT(_F, _A), {error, lists:flatten(io_lib:format(_F, _A))}).
-define(EFORMAT(_F, _A), exit(lists:flatten(io_lib:format(_F, _A)))).

-define(ENV_DB, orber_env_db).

-define(ENV_KEYS,
	[flags, iiop_port, nat_iiop_port, iiop_out_ports, domain, ip_address,
	 nat_ip_address, giop_version, iiop_timeout, iiop_connection_timeout,
	 iiop_setup_connection_timeout, iiop_in_connection_timeout, iiop_acl,
	 iiop_max_fragments, iiop_max_in_requests, iiop_max_in_connections,
	 iiop_backlog, objectkeys_gc_time, orbInitRef, orbDefaultInitRef,
	 interceptors, local_interceptors, lightweight, ip_address_local,
	 secure, iiop_ssl_ip_address_local, iiop_ssl_backlog,
	 iiop_ssl_port, nat_iiop_ssl_port, ssl_server_certfile,
	 ssl_client_certfile, ssl_server_verify, ssl_client_verify, ssl_server_depth,
	 ssl_client_depth, ssl_server_cacertfile, ssl_client_cacertfile,
	 ssl_client_password, ssl_server_password, ssl_client_keyfile,
	 ssl_server_keyfile, ssl_client_ciphers, ssl_server_ciphers,
	 ssl_client_cachetimeout, ssl_server_cachetimeout, orber_debug_level,
	 iiop_packet_size, iiop_in_keepalive, iiop_out_keepalive,
	 iiop_ssl_in_keepalive, iiop_ssl_out_keepalive, iiop_ssl_accept_timeout,
	 ssl_server_options, ssl_client_options]).

%% The 'flags' parameter must be first in the list.
%-define(ENV_KEYS,
%	[{flags, ?ORB_ENV_INIT_FLAGS}, {iiop_port, 4001}, nat_iiop_port,
%	 {iiop_out_ports, 0}, {domain, "ORBER"}, ip_address, nat_ip_address,
%	 {giop_version, {1, 1}}, {iiop_timeout, infinity},
%	 {iiop_connection_timeout, infinity}, {iiop_setup_connection_timeout, infinity},
%	 {iiop_in_connection_timeout, infinity}, {iiop_acl, []},
%	 {iiop_max_fragments, infinity}, {iiop_max_in_requests, infinity},
%	 {iiop_max_in_connections, infinity}, {iiop_backlog, 5},
%	 {objectkeys_gc_time, infinity},
%	 {orbInitRef, undefined}, {orbDefaultInitRef, undefined},
%	 {interceptors, false}, {local_interceptors, false}, {lightweight, false},
%	 {secure, no}, {iiop_ssl_backlog, 5}, {iiop_ssl_port, 4002},
%	 nat_iiop_ssl_port, {ssl_server_certfile, []}, {ssl_client_certfile, []},
%	 {ssl_server_verify, 0}, {ssl_client_verify, 0}, {ssl_server_depth, 1},
%	 {ssl_client_depth, 1}, {ssl_server_cacertfile, []},
%	 {ssl_client_cacertfile, []}, {ssl_client_password, []},
%	 {ssl_server_password, []}, {ssl_client_keyfile, []},
%	 {ssl_server_keyfile, []}, {ssl_client_ciphers, []},
%	 {ssl_server_ciphers, []}, {ssl_client_cachetimeout, infinity},
%	 {ssl_server_cachetimeout, infinity}, {orber_debug_level, 0}]).

-record(parameters, {key, value, flags = 0}).

-record(env, {acl, parameters, flags = 0}).


%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% function :
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_env}, ?MODULE, Opts, []).

%%-----------------------------------------------------------------
%% function : get_keys
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
get_keys() ->
    ?ENV_KEYS.

%%-----------------------------------------------------------------
%% function : get_env
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
get_env(Key) when is_atom(Key) ->
    case catch ets:lookup(?ENV_DB, Key) of
	[#parameters{value = Val}] ->
	    {ok, Val};
	_ ->
	    undefined
    end.

%%-----------------------------------------------------------------
%% function : get_env
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
set_env(Key, Value) when is_atom(Key) ->
    case catch ets:insert(?ENV_DB, #parameters{key = Key, value = Value}) of
	true ->
	    ok;
	_ ->
	    undefined
    end.


%%-----------------------------------------------------------------
%% function : info
%% Arguments: IoDervice - info_msg | string | io | {io, Dev}
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
info() ->
    info(info_msg).

info(IoDevice) ->
    Info =
	case orber_tb:is_running() of
	    true ->
		Info1 = create_main_info(),
		Info2 = create_flag_info(Info1),
		create_security_info(secure(), Info2);
	    _ ->
		lists:flatten(
		  io_lib:format("======= Orber Execution Environment ======~n"
				"   *** Orber is not running ***~n"
				"==========================================~n",
				[]))
	end,
    case IoDevice of
	info_msg ->
	    error_logger:info_msg(Info);
	string ->
	    Info;
	io ->
	    io:format("~s", [Info]);
	{io, Dev} ->
	    io:format(Dev, "~s", [Info]);
	_ ->
	    exit("Bad parameter")
    end.

create_main_info() ->
    {Major, Minor} = giop_version(),
    {orber, _, OrberVsn} = lists:keyfind(orber, 1, application:loaded_applications()),
    [io_lib:format("======= Orber Execution Environment ======~n"
		   "Orber version.................: ~s~n"
		   "Orber domain..................: ~s~n"
		   "IIOP port number..............: ~p~n"
		   "IIOP NAT port number..........: ~p~n"
		   "Interface(s)..................: ~p~n"
		   "Interface(s) NAT..............: ~p~n"
		   "Local Interface (default).....: ~p~n"
		   "Nodes in domain...............: ~p~n"
		   "GIOP version (default)........: ~p.~p~n"
		   "IIOP out timeout..............: ~p msec~n"
		   "IIOP out connection timeout...: ~p msec~n"
		   "IIOP setup connection timeout.: ~p msec~n"
		   "IIOP out ports................: ~p~n"
		   "IIOP out ports attempts.......: ~p~n"
		   "IIOP out ports random.........: ~p~n"
		   "IIOP out connections..........: ~p~n"
		   "IIOP out connections (pending): ~p~n"
		   "IIOP out keepalive............: ~p~n"
		   "IIOP in connections...........: ~p~n"
		   "IIOP in connection timeout....: ~p msec~n"
		   "IIOP in keepalive.............: ~p~n"
		   "IIOP max fragments............: ~p~n"
		   "IIOP max in requests..........: ~p~n"
		   "IIOP max in connections.......: ~p~n"
		   "IIOP backlog..................: ~p~n"
		   "IIOP ACL......................: ~p~n"
		   "IIOP maximum packet size......: ~p~n"
		   "Object Keys GC interval.......: ~p~n"
		   "Using Interceptors............: ~p~n"
		   "Using Local Interceptors......: ~p~n"
		   "Debug Level...................: ~p~n"
		   "orbInitRef....................: ~p~n"
		   "orbDefaultInitRef.............: ~p~n",
		   [OrberVsn, domain(), iiop_port(), nat_iiop_port(), host(),
		    nat_host(), ip_address_local(),
		    orber:orber_nodes(), Major, Minor,
		    iiop_timeout(), iiop_connection_timeout(),
		    iiop_setup_connection_timeout(), iiop_out_ports(),
		    iiop_out_ports_attempts(), iiop_out_ports_random(),
		    orber:iiop_connections(out), orber:iiop_connections_pending(),
		    iiop_out_keepalive(), orber:iiop_connections(in),
		    iiop_in_connection_timeout(), iiop_in_keepalive(),
		    iiop_max_fragments(), iiop_max_in_requests(),
		    iiop_max_in_connections(), iiop_backlog(), iiop_acl(),
		    iiop_packet_size(), objectkeys_gc_time(), get_interceptors(),
		    get_local_interceptors(), get_debug_level(), get_ORBInitRef(),
		    get_ORBDefaultInitRef()])].

create_flag_info(Info) ->
    case get_flags() of
	?ORB_ENV_INIT_FLAGS ->
	    [Info, "System Flags Set..............: -\n"];
	Flags ->
	    FlagData = check_flags(?ORB_ENV_FLAGS, Flags, []),
	    [Info, "System Flags Set..............: \n", FlagData, "\n"]
    end.

check_flags([], _, Acc) ->
    Acc;
check_flags([{Flag, Txt}|T], Flags, Acc) when ?ORB_FLAG_TEST(Flags, Flag) ->
    check_flags(T, Flags, ["   - ", Txt, "\n"|Acc]);
check_flags([_|T], Flags, Acc) ->
    check_flags(T, Flags, Acc).


create_security_info(no, Info) ->
    lists:flatten([Info, "=========================================\n"]);
create_security_info(ssl, Info) ->
    lists:flatten([Info,
		   io_lib:format("ORB security..................: ssl~n"
				 "SSL generation................: ~p~n"
				 "SSL IIOP in keepalive.........: ~p~n"
				 "SSL IIOP out keepalive........: ~p~n"
				 "SSL IIOP port number..........: ~p~n"
				 "SSL IIOP NAT port number......: ~p~n"
				 "SSL IIOP accept timeout.......: ~p~n"
				 "SSL IIOP backlog..............: ~p~n"
				 "SSL IIOP Local Interface......: ~p~n"
				 "SSL server options............: ~p~n"
				 "SSL server certfile...........: ~p~n"
				 "SSL server verification type..: ~p~n"
				 "SSL server verification depth.: ~p~n"
				 "SSL server cacertfile.........: ~p~n"
				 "SSL server keyfile............: ~p~n"
				 "SSL server password...........: ~p~n"
				 "SSL server ciphers............: ~p~n"
				 "SSL server cachetimeout.......: ~p~n"
				 "SSL client options............: ~p~n"
				 "SSL client certfile...........: ~p~n"
				 "SSL client verification type..: ~p~n"
				 "SSL client verification depth.: ~p~n"
				 "SSL client cacertfile.........: ~p~n"
				 "SSL client keyfile............: ~p~n"
				 "SSL client password...........: ~p~n"
				 "SSL client ciphers............: ~p~n"
				 "SSL client cachetimeout.......: ~p~n"
				 "=========================================~n",
				 [ssl_generation(), iiop_ssl_port(),
				  iiop_ssl_in_keepalive(), iiop_ssl_out_keepalive(),
				  nat_iiop_ssl_port(), iiop_ssl_accept_timeout(),
				  iiop_ssl_backlog(), iiop_ssl_ip_address_local(),
				  ssl_server_options(),
				  ssl_server_certfile(), ssl_server_verify(),
				  ssl_server_depth(), ssl_server_cacertfile(),
				  ssl_server_keyfile(), ssl_server_password(),
				  ssl_server_ciphers(), ssl_server_cachetimeout(),
				  ssl_client_options(),
				  ssl_client_certfile(), ssl_client_verify(),
				  ssl_client_depth(), ssl_client_cacertfile(),
				  ssl_client_keyfile(), ssl_client_password(),
				  ssl_client_ciphers(), ssl_client_cachetimeout()])]).


%%-----------------------------------------------------------------
%% function : iiop_acl
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
iiop_acl() ->
    case application:get_env(orber, iiop_acl) of
	{ok, ACL} when is_list(ACL) ->
	    ACL;
	_ ->
	    []
    end.

iiop_packet_size() ->
    case application:get_env(orber, iiop_packet_size) of
	{ok, Max} when is_integer(Max) andalso Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.


iiop_port() ->
    case application:get_env(orber, iiop_port) of
	{ok, Port} when is_integer(Port) andalso Port >= 0 ->
	    Port;
	_ ->
	    4001
    end.

nat_iiop_port() ->
    case application:get_env(orber, nat_iiop_port) of
	{ok, Port} when is_integer(Port) andalso Port > 0 ->
	    Port;
	{ok, {local, Default, _NATList}} ->
	    Default;
	_ ->
	    iiop_port()
    end.

nat_iiop_port(LocalPort) ->
    case application:get_env(orber, nat_iiop_port) of
	{ok, Port} when is_integer(Port) andalso Port > 0 ->
	    Port;
	{ok, {local, Default, NATList}} ->
	    orber_tb:keysearch(LocalPort, NATList, Default);
	_ ->
	    iiop_port()
    end.

iiop_out_ports() ->
    case application:get_env(orber, iiop_out_ports) of
	{ok, {Min, Max}} when is_integer(Min) andalso is_integer(Max) andalso Min =< Max ->
	    {Min, Max};
	{ok, {Max, Min}} when is_integer(Min) andalso is_integer(Max) andalso Min < Max ->
	    {Min, Max};
	_ ->
	    0
    end.

iiop_out_ports_random() ->
    case application:get_env(orber, iiop_out_ports_random) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.

iiop_out_ports_attempts() ->
    case application:get_env(orber, iiop_out_ports_attempts) of
	{ok, No} when is_integer(No) andalso No > 0 ->
	    No;
	_ ->
	    1
    end.


domain() ->
    case application:get_env(orber, domain) of
	{ok, Domain} when is_list(Domain) ->
	    Domain;
	{ok, Domain} when is_atom(Domain) ->
	    atom_to_list(Domain);
	_ ->
	    "ORBER"
    end.

ip_address_variable_defined() ->
    case application:get_env(orber, ip_address) of
	undefined ->
	    false;
	{ok,{multiple, _}} ->
	    false;
	_ ->
	    [Host] = host(),
	    Host
    end.

nat_host() ->
    case application:get_env(orber, nat_ip_address) of
	{ok,I} when is_list(I) ->
	    [I];
	{ok,{multiple, [I|_] = IList}} when is_list(I) ->
	    IList;
	{ok,{local, Default, _NATList}} ->
	    [Default];
	_ ->
	    host()
    end.

nat_host([Host]) ->
    case application:get_env(orber, nat_ip_address) of
	{ok,I} when is_list(I) ->
	    [I];
	{ok,{multiple, [I|_] = IList}} when is_list(I) ->
	    IList;
	{ok,{local, Default, NATList}} ->
	    [orber_tb:keysearch(Host, NATList, Default)];
	_ ->
	    host()
    end.


host() ->
    case application:get_env(orber, ip_address) of
	{ok,I} when is_list(I) ->
	    [I];
	{ok,{multiple, [I|_] = IList}} when is_list(I) ->
	    IList;
	%% IPv4. For IPv6 we only accept a string, but we must support this format
	%% for IPv4
	{ok, {A1, A2, A3, A4}} when is_integer(A1+A2+A3+A4) ->
	    [integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	     ++ "." ++ integer_to_list(A4)];
	_ ->
	    Flags = get_flags(),
	    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_HOSTNAME_IN_IOR) of
		true ->
		    {ok, Hostname} = inet:gethostname(),
		    [Hostname];
		_ ->
		    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_IPV6) of
			false ->
			    [ip_address(inet)];
			true ->
			    [ip_address(inet6)]
		    end
	    end
    end.

ip_address_local() ->
    case application:get_env(orber, ip_address_local) of
	{ok,I} when is_list(I) ->
	    [I];
	_ ->
	    []
    end.


ip_address() ->
    ip_address(ip_version()).

ip_address(inet) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	++ "." ++ integer_to_list(A4);
ip_address(inet6) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4, A5, A6, A7, A8}} = inet:getaddr(Hostname, inet6),
    int16_to_hex(A1) ++ ":" ++int16_to_hex(A2) ++ ":" ++
	int16_to_hex(A3) ++ ":" ++ int16_to_hex(A4) ++ ":" ++
	int16_to_hex(A5) ++ ":" ++ int16_to_hex(A6) ++ ":" ++
	int16_to_hex(A7) ++ ":" ++ int16_to_hex(A8).

getaddrstr(Hostname, inet) ->
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	++ "." ++ integer_to_list(A4);
getaddrstr(Hostname, inet6) ->
    {ok, {A1, A2, A3, A4, A5, A6, A7, A8}} = inet:getaddr(Hostname, inet6),
    int16_to_hex(A1) ++ ":" ++int16_to_hex(A2) ++ ":" ++
	int16_to_hex(A3) ++ ":" ++ int16_to_hex(A4) ++ ":" ++
	int16_to_hex(A5) ++ ":" ++ int16_to_hex(A6) ++ ":" ++
	int16_to_hex(A7) ++ ":" ++ int16_to_hex(A8).

addr2str({A1, A2, A3, A4}) ->
    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	++ "." ++ integer_to_list(A4);
addr2str({A1, A2, A3, A4, A5, A6, A7, A8}) ->
    int16_to_hex(A1) ++ ":" ++int16_to_hex(A2) ++ ":" ++
	int16_to_hex(A3) ++ ":" ++ int16_to_hex(A4) ++ ":" ++
	int16_to_hex(A5) ++ ":" ++ int16_to_hex(A6) ++ ":" ++
	int16_to_hex(A7) ++ ":" ++ int16_to_hex(A8).


int16_to_hex(0) ->
    [$0];
int16_to_hex(I) ->
    N1 = ((I bsr 8) band 16#ff),
    N2 = (I band 16#ff),
    [code_character(N1 div 16), code_character(N1 rem 16),
     code_character(N2 div 16), code_character(N2 rem 16)].

code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $A + (N - 10).

giop_version() ->
    case application:get_env(orber, giop_version) of
	{ok, {Major, Minor}} ->
	    {Major, Minor};
	_ ->
	    {1, 1}
    end.

iiop_timeout() ->
    case application:get_env(orber, iiop_timeout) of
	{ok, Int} when is_integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'iiop_timeout' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.

iiop_connection_timeout() ->
    case application:get_env(orber, iiop_connection_timeout) of
	{ok, Int} when is_integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'iiop_connection_timeout' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.

iiop_setup_connection_timeout() ->
    case application:get_env(orber, iiop_setup_connection_timeout) of
	{ok, Int} when is_integer(Int) ->
            %% Convert to msec.
	    Int*1000;
	_ ->
	    infinity
    end.

iiop_in_connection_timeout() ->
    case application:get_env(orber, iiop_in_connection_timeout) of
	{ok, Int} when is_integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'iiop_connection_timeout' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.

iiop_max_fragments() ->
    case application:get_env(orber, iiop_max_fragments) of
	{ok, Max} when is_integer(Max) andalso Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.

iiop_max_in_requests() ->
    case application:get_env(orber, iiop_max_in_requests) of
	{ok, Max} when is_integer(Max) andalso Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.

iiop_max_in_connections() ->
    case application:get_env(orber, iiop_max_in_connections) of
	{ok, Max} when is_integer(Max) andalso Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.

iiop_backlog() ->
    case application:get_env(orber, iiop_backlog) of
	{ok, Int} when is_integer(Int) andalso Int >= 0 ->
	    Int;
	_ ->
	    5
    end.

iiop_in_keepalive() ->
    case application:get_env(orber, iiop_in_keepalive) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.

iiop_out_keepalive() ->
    case application:get_env(orber, iiop_out_keepalive) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.



get_flags() ->
    case get(oe_orber_flags) of
	undefined ->
	    case application:get_env(orber, flags) of
		undefined ->
		    put(oe_orber_flags, ?ORB_ENV_INIT_FLAGS),
		    ?ORB_ENV_INIT_FLAGS;
		{ok, Flags} ->
		    put(oe_orber_flags, Flags),
		    Flags
	    end;
	Flags when is_integer(Flags) ->
	    Flags
    end.

typechecking() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_LOCAL_TYPECHECKING).

exclude_codeset_ctx() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_EXCLUDE_CODESET_CTX).

exclude_codeset_component() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_EXCLUDE_CODESET_COMPONENT).

partial_security() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_PARTIAL_SECURITY).

use_CSIv2() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_CSIV2).

use_FT() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_FT).

ip_version() ->
    case ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_IPV6) of
	false ->
	    inet;
	true ->
	    inet6
    end.

light_ifr() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_LIGHT_IFR).

bidir_context() ->
    Flags = get_flags(),
    if
	?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_BI_DIR_IIOP) ->
	    [#'IOP_ServiceContext'
	     {context_id=?IOP_BI_DIR_IIOP,
	      context_data =
	      #'IIOP_BiDirIIOPServiceContext'{listen_points =
					      [#'IIOP_ListenPoint'{host=host(),
								   port=iiop_port()}]}}];
	true ->
	    []
    end.

objectkeys_gc_time() ->
    case application:get_env(orber, objectkeys_gc_time) of
	{ok, Int} when is_integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'objectkeys_gc_time' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    Int
	    end;
	_ ->
	    infinity
    end.

get_ORBInitRef() ->
    case application:get_env(orber, orbInitRef) of
	{ok, Ref} when is_list(Ref) ->
	    Ref;
	_ ->
	    undefined
    end.

get_ORBDefaultInitRef() ->
    case application:get_env(orber, orbDefaultInitRef) of
	{ok, Ref} when is_list(Ref) ->
	    Ref;
	_ ->
	    undefined
    end.

get_debug_level() ->
    case application:get_env(orber, orber_debug_level) of
	{ok, Level} when is_integer(Level)  ->
	    Level;
	_ ->
	    0
    end.


%%-----------------------------------------------------------------
%% Interceptor opertaions (see orber_pi.erl)
%%-----------------------------------------------------------------
get_interceptors() ->
    case application:get_env(orber, interceptors) of
	{ok, {native, PIs}} when is_list(PIs) ->
	    {native, PIs};
	{ok, {portable, PIs}} when is_list(PIs) ->
	    {portable, PIs};
	_ ->
	    false
    end.

get_local_interceptors() ->
    case application:get_env(orber, local_interceptors) of
	{ok, {native, PIs}} when is_list(PIs) ->
	    {native, PIs};
	{ok, {portable, PIs}} when is_list(PIs) ->
	    {portable, PIs};
	_ ->
	    false
    end.


get_cached_interceptors() ->
    case get(oe_orber_interceptor_cache) of
	undefined ->
	    PIs = case application:get_env(orber, local_interceptors) of
		      {ok, {native, LPIs}} when is_list(LPIs) ->
			  {native, LPIs};
		      {ok, {portable, LPIs}} when is_list(LPIs) ->
			  {portable, LPIs};
		      _ ->
			  get_interceptors()
		  end,
	    put(oe_orber_interceptor_cache, PIs),
	    PIs;
	PIs ->
	    PIs
    end.


set_interceptors({Type, InterceptorList}) when is_list(InterceptorList) ->
    configure(interceptors, {Type, InterceptorList});
set_interceptors(_) ->
    exit({error, "Usage: {Type, ModuleList}"}).


%%-----------------------------------------------------------------
%% Light weight Orber operations
%%-----------------------------------------------------------------
is_lightweight() ->
    case application:get_env(orber, lightweight) of
	{ok, L} when is_list(L) ->
	    true;
	_ ->
	    false
    end.
get_lightweight_nodes() ->
    case application:get_env(orber, lightweight) of
	{ok, L} when is_list(L) ->
	    L;
	_ ->
	    false
    end.


%%-----------------------------------------------------------------
%% Security access operations (SSL)
%%-----------------------------------------------------------------
secure() ->
    case application:get_env(orber, secure) of
	{ok, V} ->
	    V;
	_ ->
	    no
    end.

ssl_generation() ->
    case application:get_env(orber, ssl_generation) of
	{ok, V} ->
	    V;
	_ ->
	    2
    end.

iiop_ssl_ip_address_local() ->
    case application:get_env(orber, iiop_ssl_ip_address_local) of
	{ok,I} when is_list(I) ->
	    [I];
	_ ->
	    []
    end.

iiop_ssl_backlog() ->
    case application:get_env(orber, iiop_ssl_backlog) of
	{ok, Int} when is_integer(Int), Int >= 0 ->
	    Int;
	_ ->
	    5
    end.

iiop_ssl_in_keepalive() ->
    case application:get_env(orber, iiop_ssl_in_keepalive) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.

iiop_ssl_out_keepalive() ->
    case application:get_env(orber, iiop_ssl_out_keepalive) of
	{ok, true} ->
	    true;
	_ ->
	    false
    end.

iiop_ssl_accept_timeout() ->
    case application:get_env(orber, iiop_ssl_accept_timeout) of
	{ok, N} when is_integer(N) ->
	    N * 1000;
	_  ->
	    infinity
    end.

iiop_ssl_port() ->
    case application:get_env(orber, secure) of
	{ok, ssl} ->
	    case application:get_env(orber, iiop_ssl_port) of
		{ok, Port} when is_integer(Port) ->
		    Port;
		_ ->
		    4002
	    end;
	_ ->
	    -1
    end.

nat_iiop_ssl_port() ->
    case application:get_env(orber, secure) of
	{ok, ssl} ->
	    case application:get_env(orber, nat_iiop_ssl_port) of
		{ok, Port} when is_integer(Port) andalso Port > 0 ->
		    Port;
		{ok, {local, Default, _NATList}} ->
		    Default;
		_ ->
		    iiop_ssl_port()
	    end;
	_ ->
	    -1
    end.

nat_iiop_ssl_port(LocalPort) ->
    case application:get_env(orber, secure) of
	{ok, ssl} ->
	    case application:get_env(orber, nat_iiop_ssl_port) of
		{ok, Port} when is_integer(Port) andalso Port > 0 ->
		    Port;
		{ok, {local, Default, NATList}} ->
		    orber_tb:keysearch(LocalPort, NATList, Default);
		_ ->
		    iiop_ssl_port()
	    end;
	_ ->
	    -1
    end.

ssl_server_options() ->
    case application:get_env(orber, ssl_server_options) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_options() ->
    case application:get_env(orber, ssl_client_options) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

check_ssl_opts(Value) ->
    check_ssl_opts(Value, []).
check_ssl_opts([], []) ->
    ok;
check_ssl_opts([], Acc) ->
    {error, Acc};
check_ssl_opts([{active, _} |T], Acc) ->
    check_ssl_opts(T, [active |Acc]);
check_ssl_opts([{packet, _} |T], Acc) ->
    check_ssl_opts(T, [packet |Acc]);
check_ssl_opts([{mode, _} |T], Acc) ->
    check_ssl_opts(T, [mode |Acc]);
check_ssl_opts([list |T], Acc) ->
    check_ssl_opts(T, [list |Acc]);
check_ssl_opts([binary |T], Acc) ->
    check_ssl_opts(T, [binary |Acc]);
check_ssl_opts([_ |T], Acc) ->
    check_ssl_opts(T, Acc).

set_ssl_client_options(Value) when is_list(Value) ->
    case check_ssl_opts(Value) of
	ok ->
	    ok;
	{error, List} ->
	    exit(lists:flatten(
		   io_lib:format("TCP options ~p is not allowed in set_ssl_client_options()",
				 [List])))
    end,
    put(ssl_client_options, Value), ok.

ssl_server_certfile() ->
    case application:get_env(orber, ssl_server_certfile) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	{ok, V2}  when is_atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    []
    end.

ssl_client_certfile() ->
    case get(ssl_client_certfile) of
	undefined ->
	    case application:get_env(orber, ssl_client_certfile) of
		{ok, V1}  when is_list(V1) ->
		    V1;
		{ok, V2}  when is_atom(V2) ->
		    atom_to_list(V2);
		_ ->
		    []
	    end;
	V ->
	    V
    end.

set_ssl_client_certfile(Value) when is_list(Value) ->
    put(ssl_client_certfile, Value).

ssl_server_verify() ->
    Verify = case application:get_env(orber, ssl_server_verify) of
	{ok, V} when is_integer(V) ->
	    V;
	_ ->
	    0
    end,
    if
	Verify =< 2, Verify >= 0 ->
	    Verify;
	true ->
	   0
    end.

ssl_client_verify() ->
    Verify = case get(ssl_client_verify) of
		 undefined ->
		     case application:get_env(orber, ssl_client_verify) of
			 {ok, V1} when is_integer(V1) ->
			     V1;
			 _ ->
			     0
		     end;
		 V2 ->
		     V2
	     end,
    if
	Verify =< 2, Verify >= 0 ->
	    Verify;
	true ->
	   0
    end.

set_ssl_client_verify(Value) when is_integer(Value) andalso Value =< 2 andalso Value >= 0 ->
    put(ssl_client_verify, Value), ok.

ssl_server_depth() ->
    case application:get_env(orber, ssl_server_depth) of
	{ok, V1} when is_integer(V1) ->
	    V1;
	_ ->
	    1
    end.

ssl_client_depth() ->
    case get(ssl_client_depth) of
	undefined ->
	    case application:get_env(orber, ssl_client_depth) of
		{ok, V1} when is_integer(V1) ->
		    V1;
		_ ->
		    1
	    end;
	V2 ->
	    V2
    end.

set_ssl_client_depth(Value) when is_integer(Value) ->
    put(ssl_client_depth, Value), ok.



ssl_server_cacertfile() ->
    case application:get_env(orber, ssl_server_cacertfile) of
	{ok, V1}  when is_list(V1) ->
	    V1;
	{ok, V2}  when is_atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    []
    end.

ssl_client_cacertfile() ->
    case get(ssl_client_cacertfile) of
	undefined ->
	    case application:get_env(orber, ssl_client_cacertfile) of
		{ok, V1}  when is_list(V1) ->
		    V1;
		{ok, V2}  when is_atom(V2) ->
		    atom_to_list(V2);
		_ ->
		    []
	    end;
	V3 ->
	    V3
    end.

set_ssl_client_cacertfile(Value) when is_list(Value) ->
    put(ssl_client_cacertfile, Value), ok.


ssl_client_password() ->
    case application:get_env(orber, ssl_client_password) of
	{ok, V1} when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_server_password() ->
    case application:get_env(orber, ssl_server_password) of
	{ok, V1} when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_keyfile() ->
    case application:get_env(orber, ssl_client_keyfile) of
	{ok, V1} when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_server_keyfile() ->
    case application:get_env(orber, ssl_server_keyfile) of
	{ok, V1} when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_ciphers() ->
    case application:get_env(orber, ssl_client_ciphers) of
	{ok, V1} when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_server_ciphers() ->
    case application:get_env(orber, ssl_server_ciphers) of
	{ok, V1} when is_list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_cachetimeout() ->
    case application:get_env(orber, ssl_client_cachetimeout) of
	{ok, V1} when is_integer(V1) ->
	    V1;
	_ ->
	    infinity
    end.

ssl_server_cachetimeout() ->
    case application:get_env(orber, ssl_server_cachetimeout) of
	{ok, V1} when is_integer(V1) ->
	    V1;
	_ ->
	    infinity
    end.

%%-----------------------------------------------------------------
%% function : configure
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
configure(Key, Value) when is_atom(Key) ->
    configure(Key, Value, check);
configure(Key, _) ->
    ?EFORMAT("Given key (~p) not an atom.", [Key]).

configure_override(Key, Value)  when is_atom(Key) ->
    configure(Key, Value, loaded);
configure_override(Key, _) ->
    ?EFORMAT("Given key (~p) not an atom.", [Key]).

%%-----------------------------------------------------------------
%% function : multi_configure
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
multi_configure(KeyValueList) when is_list(KeyValueList) ->
    case orber_tb:is_loaded() of
	false ->
	    application:load(orber),
	    multi_configure_helper(KeyValueList, loaded);
	true ->
	    case orber_tb:is_running() of
		false ->
		    multi_configure_helper(KeyValueList, loaded);
		true ->
		    multi_configure_helper(KeyValueList, running)
	    end
    end;
multi_configure(KeyValueList) ->
    ?EFORMAT("Given configuration parameters not a Key-Value-pair list: ~p",
	     [KeyValueList]).

multi_configure_helper([], _) ->
    ok;
multi_configure_helper([{Key, Value}|T], Status) ->
    configure(Key, Value, Status),
    multi_configure_helper(T, Status);
multi_configure_helper([What|_], _) ->
    ?EFORMAT("Incorrect configuration parameters supplied: ~p", [What]).


%%------ Keys we can update at any time -----
%% Initial Services References
configure(orbDefaultInitRef, String, Status) when is_list(String) ->
    do_configure(orbDefaultInitRef, String, Status);
configure(orbDefaultInitRef, undefined, Status) ->
    do_configure(orbDefaultInitRef, undefined, Status);
configure(orbInitRef, String, Status) when is_list(String) ->
    do_configure(orbInitRef, String, Status);
configure(orbInitRef, undefined, Status) ->
    do_configure(orbInitRef, undefined, Status);
%% IIOP-version
configure(giop_version, {1, 0}, Status) ->
    do_configure(giop_version, {1, 0}, Status);
configure(giop_version, {1, 1}, Status) ->
    do_configure(giop_version, {1, 1}, Status);
configure(giop_version, {1, 2}, Status) ->
    do_configure(giop_version, {1, 2}, Status);
%% configure 'iiop_timout' will only have effect on new requests.
configure(iiop_timeout, infinity, Status) ->
    do_configure(iiop_timeout, infinity, Status);
configure(iiop_timeout, Value, Status) when is_integer(Value) andalso Value =< 1000000 ->
    do_configure(iiop_timeout, Value, Status);
%% Backlog
configure(iiop_backlog, Value, Status) when is_integer(Value) andalso Value >= 0 ->
    do_configure(iiop_backlog, Value, Status);
%% configure 'iiop_in_keepalive' will only have effect on new connections.
configure(iiop_in_keepalive, true, Status) ->
    do_configure(iiop_in_keepalive, true, Status);
configure(iiop_in_keepalive, false, Status) ->
    do_configure(iiop_in_keepalive, false, Status);
%% configure 'iiop_out_keepalive' will only have effect on new connections.
configure(iiop_out_keepalive, true, Status) ->
    do_configure(iiop_out_keepalive, true, Status);
configure(iiop_out_keepalive, false, Status) ->
    do_configure(iiop_out_keepalive, false, Status);
%% configure 'iiop_connection_timout' will only have effect on new connections.
configure(iiop_connection_timeout, infinity, Status) ->
    do_configure(iiop_connection_timeout, infinity, Status);
configure(iiop_connection_timeout, Value, Status) when is_integer(Value) andalso Value =< 1000000 ->
    do_configure(iiop_connection_timeout, Value, Status);
%% configure 'iiop_in_connection_timout' will only have effect on new connections.
configure(iiop_in_connection_timeout, infinity, Status) ->
    do_configure(iiop_in_connection_timeout, infinity, Status);
configure(iiop_in_connection_timeout, Value, Status) when is_integer(Value) andalso Value =< 1000000 ->
    do_configure(iiop_in_connection_timeout, Value, Status);
%% configure 'iiop_setup_connection_timeout' will only have effect on new connections.
configure(iiop_setup_connection_timeout, infinity, Status) ->
    do_configure(iiop_setup_connection_timeout, infinity, Status);
configure(iiop_setup_connection_timeout, Value, Status) when is_integer(Value) ->
    do_configure(iiop_setup_connection_timeout, Value, Status);
%% configure 'iiop_max_fragments' will only have effect on new connections.
configure(iiop_max_fragments, infinity, Status) ->
    do_configure(iiop_max_fragments, infinity, Status);
configure(iiop_max_fragments, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_configure(iiop_max_fragments, Value, Status);
%% configure 'iiop_max_in_requests' will only have effect on new connections.
configure(iiop_max_in_requests, infinity, Status) ->
    do_configure(iiop_max_in_requests, infinity, Status);
configure(iiop_max_in_requests, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_configure(iiop_max_in_requests, Value, Status);
%% configure 'iiop_max_in_connections' will only have effect on new connections.
configure(iiop_max_in_connections, infinity, Status) ->
    do_configure(iiop_max_in_connections, infinity, Status);
configure(iiop_max_in_connections, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_configure(iiop_max_in_connections, Value, Status);
%% Garbage Collect the object keys DB.
configure(objectkeys_gc_time, infinity, Status) ->
    do_configure(objectkeys_gc_time, infinity, Status);
configure(objectkeys_gc_time, Value, Status) when is_integer(Value) andalso Value =< 1000000 ->
    do_configure(objectkeys_gc_time, Value, Status);
%% Orber debug printouts
configure(orber_debug_level, Value, Status) when is_integer(Value) ->
    do_configure(orber_debug_level, Value, Status);

%%------ Keys we cannot change if Orber is running -----
%% Set the listen port
configure(iiop_port, Value, Status) when is_integer(Value) ->
    do_safe_configure(iiop_port, Value, Status);
%% Set the NAT listen port
configure(nat_iiop_port, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_safe_configure(nat_iiop_port, Value, Status);
configure(nat_iiop_port, {local, Value1, Value2}, Status) when is_integer(Value1) andalso
							       Value1 > 0 andalso
							       is_list(Value2) ->
    do_safe_configure(nat_iiop_port, {local, Value1, Value2}, Status);
%% Set Maximum Packet Size
configure(iiop_packet_size, Max, Status) when is_integer(Max) andalso Max > 0 ->
    do_safe_configure(iiop_packet_size, Max, Status);
%% IIOP interceptors
configure(interceptors, Value, Status) when is_tuple(Value) ->
    do_safe_configure(interceptors, Value, Status);
%% Local interceptors
configure(local_interceptors, Value, Status) when is_tuple(Value) ->
    do_safe_configure(local_interceptors, Value, Status);
%% Orber Domain
configure(domain, Value, Status) when is_list(Value) ->
    do_safe_configure(domain, Value, Status);
%% Set the IP-address we should use
configure(ip_address, Value, Status) when is_list(Value) ->
    do_safe_configure(ip_address, Value, Status);
configure(ip_address, {multiple, Value}, Status) when is_list(Value) ->
    do_safe_configure(ip_address, {multiple, Value}, Status);
configure(ip_address_local, Value, Status) when is_list(Value) ->
    do_safe_configure(ip_address_local, Value, Status);
%% Set the NAT IP-address we should use
configure(nat_ip_address, Value, Status) when is_list(Value) ->
    do_safe_configure(nat_ip_address, Value, Status);
configure(nat_ip_address, {multiple, Value}, Status) when is_list(Value) ->
    do_safe_configure(nat_ip_address, {multiple, Value}, Status);
configure(nat_ip_address, {local, Value1, Value2}, Status) when is_list(Value1) andalso
								is_list(Value2) ->
    do_safe_configure(nat_ip_address, {local, Value1, Value2}, Status);
%% Set the range of ports we may use on this machine when connecting to a server.
configure(iiop_out_ports, {Min, Max}, Status) when is_integer(Min) andalso is_integer(Max) ->
    do_safe_configure(iiop_out_ports, {Min, Max}, Status);
configure(iiop_out_ports_attempts, Max, Status) when is_integer(Max) andalso Max > 0 ->
    do_safe_configure(iiop_out_ports_attempts, Max, Status);
configure(iiop_out_ports_random, true, Status) ->
    do_safe_configure(iiop_out_ports_random, true, Status);
configure(iiop_out_ports_random, false, Status) ->
    do_safe_configure(iiop_out_ports_random, false, Status);
%% Set the lightweight option.
configure(lightweight, Value, Status) when is_list(Value) ->
    do_safe_configure(lightweight, Value, Status);
%% Configre the System Flags
configure(flags, Value, Status) when is_integer(Value) ->
    do_safe_configure(flags, Value, Status);
%% Configre the ACL
configure(iiop_acl, Value, Status) when is_list(Value) ->
    do_safe_configure(iiop_acl, Value, Status);

%% SSL settings
%% configure 'iiop_in_keepalive' will only have effect on new connections.
configure(iiop_ssl_in_keepalive, true, Status) ->
    do_configure(iiop_ssl_in_keepalive, true, Status);
configure(iiop_ssl_in_keepalive, false, Status) ->
    do_configure(iiop_ssl_in_keepalive, false, Status);
%% configure 'iiop_ssl_out_keepalive' will only have effect on new connections.
configure(iiop_ssl_out_keepalive, true, Status) ->
    do_configure(iiop_ssl_out_keepalive, true, Status);
configure(iiop_ssl_out_keepalive, false, Status) ->
    do_configure(iiop_ssl_out_keepalive, false, Status);
configure(iiop_ssl_accept_timeout, infinity, Status) ->
    do_configure(iiop_ssl_accept_timeout, infinity, Status);
configure(iiop_ssl_accept_timeout, Value, Status) when is_integer(Value) andalso Value >= 0 ->
    do_configure(iiop_ssl_accept_timeout, Value, Status);
configure(ssl_generation, Generation, Status) when is_integer(Generation) andalso Generation >= 2 ->
    do_safe_configure(ssl_generation, Generation, Status);
configure(secure, ssl, Status) ->
    do_safe_configure(secure, ssl, Status);
configure(iiop_ssl_ip_address_local, Value, Status) when is_list(Value) ->
    do_safe_configure(iiop_ssl_ip_address_local, Value, Status);
configure(iiop_ssl_backlog, Value, Status) when is_integer(Value) andalso Value >= 0 ->
    do_safe_configure(iiop_ssl_backlog, Value, Status);
configure(nat_iiop_ssl_port, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_safe_configure(nat_iiop_ssl_port, Value, Status);
configure(nat_iiop_ssl_port, {local, Value1, Value2}, Status) when is_integer(Value1) andalso
								   Value1 > 0 andalso
								   is_list(Value2) ->
    do_safe_configure(nat_iiop_ssl_port, {local, Value1, Value2}, Status);
configure(iiop_ssl_port, Value, Status) when is_integer(Value) ->
    do_safe_configure(iiop_ssl_port, Value, Status);

%% New SSL options
configure(ssl_server_options, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_server_options, Value, Status);
configure(ssl_client_options, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_client_options, Value, Status);

%% Old SSL options
configure(ssl_server_certfile, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_server_certfile, Value, Status);
configure(ssl_server_certfile, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_server_certfile, atom_to_list(Value), Status);
configure(ssl_client_certfile, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_client_certfile, Value, Status);
configure(ssl_client_certfile, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_client_certfile, atom_to_list(Value), Status);
configure(ssl_server_verify, Value, Status) when is_integer(Value) ->
    do_safe_configure(ssl_server_verify, Value, Status);
configure(ssl_client_verify, Value, Status) when is_integer(Value) ->
    do_safe_configure(ssl_client_verify, Value, Status);
configure(ssl_server_depth, Value, Status) when is_integer(Value) ->
    do_safe_configure(ssl_server_depth, Value, Status);
configure(ssl_client_depth, Value, Status) when is_integer(Value) ->
    do_safe_configure(ssl_client_depth, Value, Status);
configure(ssl_server_cacertfile, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_server_cacertfile, Value, Status);
configure(ssl_server_cacertfile, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_server_cacertfile, atom_to_list(Value), Status);
configure(ssl_client_cacertfile, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_client_cacertfile, Value, Status);
configure(ssl_client_cacertfile, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_client_cacertfile, atom_to_list(Value), Status);
configure(ssl_client_password, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_client_password, Value, Status);
configure(ssl_client_password, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_client_password, atom_to_list(Value), Status);
configure(ssl_client_keyfile, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_client_keyfile, Value, Status);
configure(ssl_client_keyfile, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_client_keyfile, atom_to_list(Value), Status);
configure(ssl_server_password, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_server_password, Value, Status);
configure(ssl_client_password, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_server_password, atom_to_list(Value), Status);
configure(ssl_server_keyfile, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_server_keyfile, Value, Status);
configure(ssl_server_keyfile, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_server_keyfile, atom_to_list(Value), Status);
configure(ssl_server_ciphers, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_server_ciphers, Value, Status);
configure(ssl_server_ciphers, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_server_ciphers, atom_to_list(Value), Status);
configure(ssl_client_ciphers, Value, Status) when is_list(Value) ->
    do_safe_configure(ssl_client_ciphers, Value, Status);
configure(ssl_client_ciphers, Value, Status) when is_atom(Value) ->
    do_safe_configure(ssl_client_ciphers, atom_to_list(Value), Status);
configure(ssl_client_cachetimeout, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_safe_configure(ssl_client_cachetimeout, Value, Status);
configure(ssl_server_cachetimeout, Value, Status) when is_integer(Value) andalso Value > 0 ->
    do_safe_configure(ssl_server_cachetimeout, Value, Status);

configure(Key, Value, _) ->
    ?EFORMAT("Bad configuration parameter: {~p, ~p}", [Key, Value]).

%% This function may be used as long as it is safe to change a value at any time.
do_configure(Key, Value, check) ->
    case orber_tb:is_loaded() of
	false ->
	    application:load(orber),
	    application:set_env(orber, Key, Value);
	true ->
	    application:set_env(orber, Key, Value)
    end;
do_configure(Key, Value, _) ->
    application:set_env(orber, Key, Value).

%% This function MUST(!!) be used when we cannot change a value if Orber is running.
do_safe_configure(_, _, running) ->
    exit("Orber already running, the given key may not be updated!");
do_safe_configure(Key, Value, check) ->
    case orber_tb:is_loaded() of
	false ->
	    application:load(orber),
	    application:set_env(orber, Key, Value);
	true ->
	    case orber_tb:is_running() of
		false ->
		    application:set_env(orber, Key, Value);
		true ->
		    ?EFORMAT("Orber already running. {~p, ~p} may not be updated!",
			     [Key, Value])
	    end
    end;
do_safe_configure(Key, Value, loaded) ->
    application:set_env(orber, Key, Value).


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
init(_Opts) ->
    {ok, #env{acl = orber_acl:init_acl(iiop_acl()),
	      parameters = init_env()}}.

terminate(_Reason, _State) ->
	    ok.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply,  State}.

handle_info(_, State) ->
    {noreply,  State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% function : env
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   : Used when Key always exists (Default Value)
%%-----------------------------------------------------------------
env(Key) ->
    [#parameters{value = Val}] = ets:lookup(?ENV_DB, Key),
    Val.

%%-----------------------------------------------------------------
%% function : init_env
%% Arguments:
%% Returns  :
%% Exception:
%% Effect   :
%%-----------------------------------------------------------------
init_env() ->
    application:load(orber),
    DB = ets:new(?ENV_DB, [set, public, named_table, {keypos, 2}]),
%    init_env(?ENV_KEYS),
    DB.

%init_env([{H,D}|T]) ->
%    case application:get_env(orber, H) of
%	{ok, V} ->
%	    ets:insert(?ENV_DB, #parameters{key = H, value = V, flags = 0}),
%	    init_env(T);
%	_ ->
%	    ets:insert(?ENV_DB, #parameters{key = H, value = D, flags = 0}),
%	    init_env(T)
%    end;
%init_env([H|T]) ->
%    case application:get_env(orber, H) of
%	{ok, V} ->
%	    ets:insert(?ENV_DB, #parameters{key = H, value = V, flags = 0}),
%	    init_env(T);
%	_ ->
%	    ets:insert(?ENV_DB, #parameters{key = H, value = undefined, flags = 0}),
%	    init_env(T)
%    end;
%init_env([]) ->
%    ok.

%%-----------------------------------------------------------------
%%------------- END OF MODULE -------------------------------------
%%-----------------------------------------------------------------
