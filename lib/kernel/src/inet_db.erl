%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2020. All Rights Reserved.
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

-module(inet_db).

%% Store info about ip addresses, names, aliases host files resolver
%% options

%% If the macro DEBUG is defined during compilation, 
%% debug printouts are done through erlang:display/1.
%% Activate this feature by starting the compiler 
%% with> erlc -DDEBUG ... 
%% or by> setenv ERL_COMPILER_FLAGS DEBUG 
%% before running make (in the OTP make system)
%% (the example is for tcsh)

%% External exports
-export([start/0, start_link/0, stop/0, reset/0, clear_cache/0]).
-export([add_rr/1, add_rr/5, del_rr/4]).
-export([add_ns/1, add_ns/2]).
-export([ins_alt_ns/2, del_alt_ns/2]).
-export([add_search/1, ins_search/1]).
-export([set_lookup/1, set_recurse/1]).
-export([set_socks_server/1, set_socks_port/1, add_socks_methods/1,
	 del_socks_methods/1, del_socks_methods/0,
	 add_socks_noproxy/1, del_socks_noproxy/1]).
-export([set_cache_size/1, set_cache_refresh/1]).
-export([set_timeout/1, set_retry/1, set_inet6/1, set_usevc/1]).
-export([set_edns/1, set_udp_payload_size/1]).
-export([set_resolv_conf/1, set_hosts_file/1, get_hosts_file/0]).
-export([tcp_module/0, set_tcp_module/1]).
-export([udp_module/0, set_udp_module/1]).
-export([sctp_module/0,set_sctp_module/1]).
-export([register_socket/2, unregister_socket/1, lookup_socket/1]).

%% Host name & domain
-export([set_hostname/1, set_domain/1]).
-export([gethostname/0]).

%% file interface
-export([add_host/2, del_host/1, clear_hosts/0, add_hosts/1]).
-export([add_resolv/1]).
-export([add_rc/1, add_rc_bin/1, add_rc_list/1, get_rc/0]).

-export([res_option/1, res_option/2, res_check_option/2]).
-export([socks_option/1]).
-export([getbyname/2, get_searchlist/0]).
-export([gethostbyaddr/1]).
-export([res_gethostbyaddr/2,res_hostent_by_domain/3]).
-export([res_update_conf/0, res_update_hosts/0]).
%% inet help functions
-export([tolower/1]).
-ifdef(DEBUG).
-define(dbg(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmd, Args), ok).
-endif.

-include_lib("kernel/include/file.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, 
	{db,                %% resolver data
	 cache,             %% bag of resource records
	 hosts_byname,      %% hosts table
	 hosts_byaddr,      %% hosts table
	 hosts_file_byname, %% hosts table from system file
	 hosts_file_byaddr, %% hosts table from system file
	 cache_timer        %% timer reference for refresh
	}).
-type state() :: #state{}.

-include("inet.hrl").
-include("inet_int.hrl").
-include("inet_res.hrl").
-include("inet_dns.hrl").
-include("inet_config.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start() ->
    case gen_server:start({local, inet_db}, inet_db, [], []) of
	{ok, _Pid}=Ok -> inet_config:init(), Ok;
	Error -> Error
    end.


start_link() ->
    case gen_server:start_link({local, inet_db}, inet_db, [], []) of
	{ok, _Pid}=Ok -> inet_config:init(), Ok;
	Error -> Error
    end.
	       
call(Req) -> 
    gen_server:call(inet_db, Req, infinity).

stop() ->
    call(stop).

reset() ->
    call(reset).


%% insert all resolve options from this file (MAY GO)
add_resolv(File) ->
    case inet_parse:resolv(File) of
	{ok, Res} -> add_rc_list(Res);
	Error -> Error
    end.

%% add all aliases from this hosts file (MAY GO)
add_hosts(File) ->
    case inet_parse:hosts(File) of
	{ok, Res} ->
	    lists:foreach(
	      fun({IP, Name, Aliases}) -> add_host(IP, [Name|Aliases]) end,
	      Res);
	Error -> Error
    end.

add_host(IP, Names) -> call({add_host, IP, Names}).

del_host(IP) ->  call({del_host, IP}).

clear_hosts() -> call(clear_hosts).

%% add to the end of name server list
add_ns(IP) -> 
    add_ns(IP,?NAMESERVER_PORT).
add_ns(IP,Port) ->
    call({listop, nameservers, add, {IP,Port}}).

%% insert at head of name server list
ins_alt_ns(IP,Port) ->
    call({listop, alt_nameservers, ins, {IP,Port}}).

%% delete this name server entry
del_alt_ns(IP, Port) ->
    call({listop, alt_nameservers, del, {IP,Port}}).

%% add this domain to the search list
add_search(Domain) when is_list(Domain) -> 
    call({listop, search, add, Domain}).

ins_search(Domain) when is_list(Domain) ->
    call({listop, search, ins, Domain}).

%% set host name used by inet
%% Should only be used by inet_config at startup!
set_hostname(Name) ->
    call({set_hostname, Name}).

%% set default domain
set_domain(Domain) -> res_option(domain, Domain).

%% set lookup methods
set_lookup(Methods) -> res_option(lookup, Methods).

%% resolver
set_recurse(Flag) -> res_option(recurse, Flag).

set_timeout(Time) -> res_option(timeout, Time).

set_retry(N) -> res_option(retry, N).

set_inet6(Bool) -> res_option(inet6, Bool).

set_usevc(Bool) -> res_option(usevc, Bool).

set_edns(Version) -> res_option(edns, Version).

set_udp_payload_size(Size) -> res_option(udp_payload_size, Size).

set_resolv_conf(Fname) when is_list(Fname) ->
    res_option(resolv_conf, Fname).

set_hosts_file(Fname) when is_list(Fname) ->
    res_option(hosts_file, Fname).

get_hosts_file() ->
    get_rc_hosts([], [], inet_hosts_file_byaddr).

%% set socks options
set_socks_server(Server) -> call({set_socks_server, Server}).

set_socks_port(Port) -> call({set_socks_port, Port}).

add_socks_methods(Ms) -> call({add_socks_methods,Ms}).

del_socks_methods(Ms) -> call({del_socks_methods,Ms}).

del_socks_methods() -> call(del_socks_methods).

add_socks_noproxy({Net,Mask}) -> call({add_socks_noproxy, {Net,Mask}}).

del_socks_noproxy(Net) -> call({del_socks_noproxy, Net}).

%% cache options
set_cache_size(Limit) -> call({set_cache_size, Limit}).

set_cache_refresh(Time) -> call({set_cache_refresh, Time}).

clear_cache() -> call(clear_cache).


set_tcp_module(Module) -> call({set_tcp_module, Module}).

tcp_module() -> db_get(tcp_module).

set_udp_module(Module) -> call({set_udp_module, Module}).

udp_module() -> db_get(udp_module).

set_sctp_module(Family)-> call({set_sctp_module,Family}).

sctp_module()-> db_get(sctp_module).

%% Add an inetrc file
add_rc(File) -> 
    case file:consult(File) of
	{ok, List} -> add_rc_list(List);
	Error -> Error
    end.

%% Add an inetrc binary term must be a rc list
add_rc_bin(Bin) ->
    case catch binary_to_term(Bin) of
	List when is_list(List) ->
	    add_rc_list(List);
	_ ->
	    {error, badarg}
    end.

add_rc_list(List) -> call({add_rc_list, List}).



%% All kind of flavors !
translate_lookup(["bind" | Ls]) -> [dns | translate_lookup(Ls)];
translate_lookup(["dns" | Ls]) -> [dns | translate_lookup(Ls)];
translate_lookup(["hosts" | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["files" | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["file"  | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["yp" | Ls]) -> [yp | translate_lookup(Ls)];
translate_lookup(["nis" | Ls]) -> [nis | translate_lookup(Ls)];
translate_lookup(["nisplus" | Ls]) -> [nisplus | translate_lookup(Ls)];
translate_lookup(["native" | Ls]) -> [native | translate_lookup(Ls)];
translate_lookup([M | Ls]) when is_atom(M) -> translate_lookup([atom_to_list(M) | Ls]);
translate_lookup([_ | Ls]) -> translate_lookup(Ls);
translate_lookup([]) -> [].

valid_lookup() -> [dns, file, yp, nis, nisplus, native].
    

%% Reconstruct an inetrc sturcture from inet_db
get_rc() -> 
    get_rc([hosts, domain, nameservers, search, alt_nameservers,
	    timeout, retry, inet6, usevc,
	    edns, udp_payload_size, resolv_conf, hosts_file,
	    socks5_server,  socks5_port, socks5_methods, socks5_noproxy,
	    udp, sctp, tcp, host, cache_size, cache_refresh, lookup], []).

get_rc([K | Ks], Ls) ->
    case K of
	hosts      -> get_rc_hosts(Ks, Ls, inet_hosts_byaddr);
	domain     -> get_rc(domain, res_domain, "", Ks, Ls);
	nameservers -> get_rc_ns(db_get(res_ns),nameservers,Ks,Ls);
	alt_nameservers -> get_rc_ns(db_get(res_alt_ns),alt_nameservers,Ks,Ls);
	search  -> get_rc(search, res_search, [], Ks, Ls);
	timeout -> get_rc(timeout,res_timeout,?RES_TIMEOUT, Ks,Ls);
	retry   -> get_rc(retry, res_retry, ?RES_RETRY, Ks, Ls);
	inet6   -> get_rc(inet6, res_inet6, false, Ks, Ls);
	usevc   -> get_rc(usevc, res_usevc, false, Ks, Ls);
	edns    -> get_rc(edns, res_edns, false, Ks, Ls);
	udp_payload_size -> get_rc(udp_payload_size, res_udp_payload_size,
				   ?DNS_UDP_PAYLOAD_SIZE, Ks, Ls);
	resolv_conf -> get_rc(resolv_conf, res_resolv_conf, undefined, Ks, Ls);
	hosts_file -> get_rc(hosts_file, res_hosts_file, undefined, Ks, Ls);
	tcp     -> get_rc(tcp,  tcp_module,  ?DEFAULT_TCP_MODULE,  Ks, Ls); 
	udp     -> get_rc(udp,  udp_module,  ?DEFAULT_UDP_MODULE,  Ks, Ls);
	sctp	-> get_rc(sctp, sctp_module, ?DEFAULT_SCTP_MODULE, Ks, Ls);
	lookup  -> get_rc(lookup, res_lookup, [native,file], Ks, Ls);
	cache_size -> get_rc(cache_size, cache_size, ?CACHE_LIMIT, Ks, Ls);
	cache_refresh ->
	    get_rc(cache_refresh, cache_refresh_interval,?CACHE_REFRESH,Ks,Ls);
	socks5_server -> get_rc(socks5_server, socks5_server, "", Ks, Ls);
	socks5_port    -> get_rc(socks5_port,socks5_port,?IPPORT_SOCKS,Ks,Ls);
	socks5_methods -> get_rc(socks5_methods,socks5_methods,[none],Ks,Ls);
	socks5_noproxy ->
	    case db_get(socks5_noproxy) of
		[] -> get_rc(Ks, Ls);
		NoProxy -> get_rc_noproxy(NoProxy, Ks, Ls)
	    end;
	_ ->
	    get_rc(Ks, Ls)
    end;
get_rc([], Ls) -> 
    lists:reverse(Ls).

get_rc(Name, Key, Default, Ks, Ls) ->
    case db_get(Key) of
	Default -> get_rc(Ks, Ls);
	Value -> get_rc(Ks, [{Name, Value} | Ls])
    end.

get_rc_noproxy([{Net,Mask} | Ms], Ks, Ls) ->
    get_rc_noproxy(Ms, Ks, [{socks5_noproxy, Net, Mask} | Ls]);
get_rc_noproxy([], Ks, Ls) -> get_rc(Ks, Ls).

get_rc_ns([{IP,?NAMESERVER_PORT} | Ns], Tag, Ks, Ls) ->
    get_rc_ns(Ns, Tag, Ks, [{Tag, IP} | Ls]);
get_rc_ns([{IP,Port} | Ns], Tag, Ks, Ls) ->
    get_rc_ns(Ns, Tag, Ks, [{Tag, IP, Port} | Ls]);
get_rc_ns([], _Tag, Ks, Ls) ->
    get_rc(Ks, Ls).

get_rc_hosts(Ks, Ls, Tab) ->
    case ets:tab2list(Tab) of
	[] -> get_rc(Ks, Ls);
	Hosts -> get_rc(Ks, [ [{host, IP, Names} || {{_Fam, IP}, Names} <- Hosts] | Ls])
    end.

%%
%% Resolver options
%%
res_option(next_id)        ->
    Cnt = ets:update_counter(inet_db, res_id, 1),
    case Cnt band 16#ffff of
	0 ->
	    ets:update_counter(inet_db, res_id, -Cnt),
	    0;
	Id ->
	    Id
    end;
res_option(Option) ->
    case res_optname(Option) of
	undefined ->
	    erlang:error(badarg, [Option]);
	ResOptname ->
	    db_get(ResOptname)
    end.

res_option(Option, Value) ->
    case res_optname(Option) of
	undefined ->
	    erlang:error(badarg, [Option,Value]);
	_ ->
	    call({res_set,Option,Value})
    end.

res_optname(nameserver) -> res_ns;     %% Legacy
res_optname(alt_nameserver) -> res_alt_ns; %% Legacy
res_optname(nameservers) -> res_ns;
res_optname(alt_nameservers) -> res_alt_ns;
res_optname(domain) -> res_domain;
res_optname(lookup) -> res_lookup;
res_optname(recurse) -> res_recurse;
res_optname(search) -> res_search;
res_optname(retry) -> res_retry;
res_optname(timeout) -> res_timeout;
res_optname(inet6) -> res_inet6;
res_optname(usevc) -> res_usevc;
res_optname(edns) -> res_edns;
res_optname(udp_payload_size) -> res_udp_payload_size;
res_optname(resolv_conf) -> res_resolv_conf;
res_optname(resolv_conf_name) -> res_resolv_conf;
res_optname(hosts_file) -> res_hosts_file;
res_optname(hosts_file_name) -> res_hosts_file;
res_optname(_) -> undefined.

res_check_option(nameserver, NSs) -> %% Legacy
    res_check_list(NSs, fun res_check_ns/1);
res_check_option(alt_nameserver, NSs) -> %% Legacy
    res_check_list(NSs, fun res_check_ns/1);
res_check_option(nameservers, NSs) ->
    res_check_list(NSs, fun res_check_ns/1);
res_check_option(alt_nameservers, NSs) ->
    res_check_list(NSs, fun res_check_ns/1);
res_check_option(domain, Dom) ->
    inet_parse:visible_string(Dom);
res_check_option(lookup, Methods) ->
    try lists_subtract(Methods, valid_lookup()) of
	[] -> true;
	_ -> false
    catch
	error:_ -> false
    end;
res_check_option(recurse, R) when R =:= 0; R =:= 1 -> true; %% Legacy
res_check_option(recurse, R) when is_boolean(R) -> true;
res_check_option(search, SearchList) ->
    res_check_list(SearchList, fun res_check_search/1);
res_check_option(retry, N) when is_integer(N), N > 0 -> true;
res_check_option(timeout, T) when is_integer(T), T > 0 -> true;
res_check_option(inet6, Bool) when is_boolean(Bool) -> true;
res_check_option(usevc, Bool) when is_boolean(Bool) -> true;
res_check_option(edns, V) when V =:= false; V =:= 0 -> true;
res_check_option(udp_payload_size, S) when is_integer(S), S >= 512 -> true;
res_check_option(resolv_conf, "") -> true;
res_check_option(resolv_conf, F) ->
    res_check_option_absfile(F);
res_check_option(resolv_conf_name, "") -> true;
res_check_option(resolv_conf_name, F) ->
    res_check_option_absfile(F);
res_check_option(hosts_file, "") -> true;
res_check_option(hosts_file, F) ->
    res_check_option_absfile(F);
res_check_option(hosts_file_name, "") -> true;
res_check_option(hosts_file_name, F) ->
    res_check_option_absfile(F);
res_check_option(_, _) -> false.

res_check_option_absfile(F) ->
    try filename:pathtype(F) of
	absolute -> true;
	_ -> false
    catch
	_:_ -> false
    end.

res_check_list([], _Fun) -> true;
res_check_list([H|T], Fun) ->
    Fun(H) andalso res_check_list(T, Fun);
res_check_list(_, _Fun) -> false.

res_check_ns({{A,B,C,D,E,F,G,H}, Port})
  when ?ip6(A,B,C,D,E,F,G,H), Port band 65535 =:= Port -> true;
res_check_ns({{A,B,C,D}, Port})
  when ?ip(A,B,C,D), Port band 65535 =:= Port -> true;
res_check_ns(_) -> false.

res_check_search("") -> true;
res_check_search(Dom) -> inet_parse:visible_string(Dom).

socks_option(server)  -> db_get(socks5_server);
socks_option(port)    -> db_get(socks5_port);
socks_option(methods) -> db_get(socks5_methods);
socks_option(noproxy) -> db_get(socks5_noproxy).

gethostname()         -> db_get(hostname).

res_update_conf() ->
    res_update(resolv_conf, res_resolv_conf_tm).

res_update_hosts() ->
    res_update(hosts_file, res_hosts_file_tm).

res_update(Option, TagTm) ->
    case db_get(TagTm) of
	undefined -> ok;
	Tm ->
	    case times() of
		Now when Now >= Tm + ?RES_FILE_UPDATE_TM ->
                    %% Enough time has passed - request server to update
                    res_option(Option, Tm);
		_ -> ok
	    end
    end.

db_get(Name) ->
    try ets:lookup_element(inet_db, Name, 2)
    catch error:badarg -> undefined
    end.

add_rr(RR) ->
    call({add_rr, RR}).

add_rr(Domain, Class, Type, TTL, Data) ->
    call({add_rr, #dns_rr { domain = Domain, class = Class,
		       type = Type, ttl = TTL, data = Data}}).

del_rr(Domain, Class, Type, Data) ->
    call({del_rr, #dns_rr { domain = Domain, class = Class,
		       type = Type, cnt = '_', tm = '_', ttl = '_',
		       bm = '_', func = '_', data = Data}}).

res_cache_answer(Rec) ->
    lists:foreach( fun(RR) -> add_rr(RR) end, Rec#dns_rec.anlist).

    


%%
%% getbyname (cache version)
%%
%% This function and inet_res:res_getbyname/3 must look up names
%% in the same manner, but not from the same places.
%%
getbyname(Name, Type) ->
    {EmbeddedDots, TrailingDot} = inet_parse:dots(Name),
    Dot = if TrailingDot -> ""; true -> "." end,
    if  TrailingDot ->
	    hostent_by_domain(Name, Type);
	EmbeddedDots =:= 0 ->
	    getbysearch(Name, Dot, get_searchlist(), Type, {error,nxdomain});
	true ->
	    case hostent_by_domain(Name, Type) of
		{error,_}=Error ->
		    getbysearch(Name, Dot, get_searchlist(), Type, Error);
		Other -> Other
	    end
    end.

getbysearch(Name, Dot, [Dom | Ds], Type, _) ->
    case hostent_by_domain(Name ++ Dot ++ Dom, Type) of
	{ok, _HEnt}=Ok -> Ok;
	Error -> getbysearch(Name, Dot, Ds, Type, Error)
    end;
getbysearch(_Name, _Dot, [], _Type, Error) ->
    Error.


%%
%% get_searchlist
%%
get_searchlist() ->
    case res_option(search) of
	[] -> [res_option(domain)];
	L -> L
    end.


make_hostent(Name, Addrs, Aliases, ?S_A) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet,
	      h_length = 4,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Addrs, Aliases, ?S_AAAA) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet6,
	      h_length = 16,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Datas, Aliases, Type) ->
    %% Use #hostent{} for other Types as well !
    #hostent {
	      h_name = Name,
	      h_addrtype = Type,
	      h_length = length(Datas),
	      h_addr_list = Datas,
	      h_aliases = Aliases
	     }.

hostent_by_domain(Domain, Type) ->
    ?dbg("hostent_by_domain: ~p~n", [Domain]),
    hostent_by_domain(stripdot(Domain), [], [], Type).

hostent_by_domain(Domain, Aliases, LAliases, Type) ->
    case lookup_type(Domain, Type) of
	[] ->
	    case lookup_cname(Domain) of
		[] ->  
		    {error, nxdomain};
		[CName | _] ->
		    LDomain = tolower(Domain),
		    case lists:member(CName, [LDomain | LAliases]) of
                        true -> 
			    {error, nxdomain};
                        false ->
			    hostent_by_domain(CName, [Domain | Aliases],
					      [LDomain | LAliases], Type)
		    end
	    end;
	Addrs ->
	    {ok, make_hostent(Domain, Addrs, Aliases, Type)}
    end.

%% lookup address record
lookup_type(Domain, Type) ->
    [R#dns_rr.data || R <- lookup_rr(Domain, in, Type) ].

%% lookup canonical name
lookup_cname(Domain) ->
    [R#dns_rr.data || R <- lookup_rr(Domain, in, ?S_CNAME) ].

%% Have to do all lookups (changes to the db) in the
%% process in order to make it possible to refresh the cache.
lookup_rr(Domain, Class, Type) ->
    match_rr(#dns_rr{domain = tolower(Domain), class = Class,type = Type,
		     cnt = '_', tm = '_', ttl = '_',
		     bm = '_', func = '_', data = '_'}).

match_rr(RR) ->
    filter_rr(ets:match_object(inet_cache, RR), times()).

%%
%% hostent_by_domain (newly resolved version)
%% match data field directly and cache RRs.
%%
res_hostent_by_domain(Domain, Type, Rec) ->
    RRs = lists:map(fun lower_rr/1, Rec#dns_rec.anlist),
    res_cache_answer(Rec#dns_rec{anlist = RRs}),
    ?dbg("res_hostent_by_domain: ~p - ~p~n", [Domain, RRs]),
    res_hostent_by_domain(stripdot(Domain), [], [], Type, RRs).

res_hostent_by_domain(Domain, Aliases, LAliases, Type, RRs) ->
    LDomain = tolower(Domain),
    case res_lookup_type(LDomain, Type, RRs) of
	[] ->
	    case res_lookup_type(LDomain, ?S_CNAME, RRs) of
		[] ->  
		    {error, nxdomain};
		[CName | _] ->
		    case lists:member(tolower(CName), [LDomain | LAliases]) of
			true -> 
			    {error, nxdomain};
			false ->
			    res_hostent_by_domain(CName, [Domain | Aliases],
						  [LDomain | LAliases], Type,
						  RRs)
		    end
	    end;
	Addrs ->
	    {ok, make_hostent(Domain, Addrs, Aliases, Type)}
    end.

%% newly resolved lookup address record
res_lookup_type(Domain,Type,RRs) ->
    [R#dns_rr.data || R <- RRs,
		      R#dns_rr.domain =:= Domain,
		      R#dns_rr.type =:= Type].

%%
%% gethostbyaddr (cache version)
%% match data field directly
%%
gethostbyaddr(IP) ->
    case dnip(IP) of
	{ok, {IP1, HType, HLen, DnIP}} ->
	    RRs = match_rr(#dns_rr { domain = DnIP, class = in, type = ptr,
				     cnt = '_', tm = '_', ttl = '_',
				     bm = '_', func = '_', data = '_' }),
	    ent_gethostbyaddr(RRs,  IP1, HType, HLen);
	Error -> Error
    end.

%%
%% res_gethostbyaddr (newly resolved version)
%% match data field directly and cache RRs.
%%
res_gethostbyaddr(IP, Rec) ->
    {ok, {IP1, HType, HLen}} = dnt(IP),
    RRs = lists:map(fun lower_rr/1, Rec#dns_rec.anlist),
    res_cache_answer(Rec#dns_rec{anlist = RRs}),
    ent_gethostbyaddr(Rec#dns_rec.anlist, IP1, HType, HLen).

ent_gethostbyaddr(RRs, IP, AddrType, Length) ->
    case RRs of
	[] -> {error, nxdomain};
	[RR|TR] ->
	    %% debug
	    if TR =/= [] ->
		    ?dbg("gethostbyaddr found extra=~p~n", [TR]);
	       true -> ok
	    end,
	    Domain = RR#dns_rr.data,
	    H = #hostent { h_name = Domain,
			   h_aliases = lookup_cname(Domain),
			   h_addr_list = [IP],
			   h_addrtype = AddrType,
			   h_length = Length },
	    {ok, H}
    end.

dnip(IP) ->
    case dnt(IP) of
	{ok,{IP1 = {A,B,C,D}, inet, HLen}} ->
	    {ok,{IP1, inet, HLen, dn_in_addr_arpa(A,B,C,D)}};
	{ok,{IP1 = {A,B,C,D,E,F,G,H}, inet6, HLen}} ->
	    {ok,{IP1, inet6, HLen, dn_ip6_int(A,B,C,D,E,F,G,H)}};
	_ ->
	    {error, formerr}
    end.


dnt(IP = {A,B,C,D}) when ?ip(A,B,C,D) ->
    {ok, {IP, inet, 4}};
dnt({0,0,0,0,0,16#ffff,G,H}) when is_integer(G+H) ->
    A = G div 256, B = G rem 256, C = H div 256, D = H rem 256,
    {ok, {{A,B,C,D}, inet, 4}};
dnt(IP = {A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    {ok, {IP, inet6, 16}};
dnt(_) ->
    {error, formerr}.

%%
%% Register socket Modules
%%
register_socket(Socket, Module) when is_port(Socket), is_atom(Module) ->
    try erlang:port_set_data(Socket, Module)
    catch
	error:badarg -> false
    end.

unregister_socket(Socket) when is_port(Socket) ->
    ok. %% not needed any more

lookup_socket(Socket) when is_port(Socket) ->
    try erlang:port_get_data(Socket) of
	Module when is_atom(Module) -> {ok,Module};
	_                           -> {error,closed}
    catch
	error:badarg                -> {error,closed}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------

%% INET DB ENTRY TYPES:
%%
%% KEY            VALUE           - DESCRIPTION
%%
%% hostname       String          - SHORT host name
%%
%% Resolver options
%% ----------------
%% res_ns         [Nameserver]    - list of name servers
%% res_alt_ns     [AltNameServer] - list of alternate name servers (nxdomain)
%% res_search     [Domain]        - list of domains for short names
%% res_domain     Domain          - local domain for short names
%% res_recurse    Bool            - recursive query 
%% res_usevc      Bool            - use tcp only
%% res_id         Integer         - NS query identifier
%% res_retry      Integer         - Retry count for UDP query
%% res_timeout    Integer         - UDP query timeout before retry
%% res_inet6      Bool            - address family inet6 for gethostbyname/1
%% res_usevc      Bool            - use Virtual Circuit (TCP)
%% res_edns       false|Integer   - false or EDNS version
%% res_udp_payload_size Integer   - size for EDNS, both query and reply
%% res_resolv_conf Filename       - file to watch for resolver config i.e
%%                                  {res_ns, res_search}
%% res_hosts_file Filename        - file to watch for hosts config
%%
%% Socks5 options
%% --------------
%% socks5_server  Server          - IP address of the socks5 server
%% socks5_port    Port            - TCP port of the socks5 server
%% socks5_methods Ls              - List of authentication methods
%% socks5_noproxy IPs             - List of {Net,Subnetmask}
%%
%% Generic tcp/udp options
%% -----------------------
%% tcp_module     Module          - The default gen_tcp  module
%% udp_module     Module          - The default gen_udp  module
%% sctp_module	  Module	  - The default gen_sctp module
%%
%% Distribution options
%% --------------------
%% {node_auth,N}  Ls              - List of authentication for node N
%% {node_crypt,N} Ls              - List of encryption methods for node N
%% node_auth      Ls              - Default authenication
%% node_crypt     Ls              - Default encryption
%%

-spec init([]) -> {'ok', state()}.

init([]) ->
    process_flag(trap_exit, true),
    case application:get_env(kernel, inet_backend) of
        {ok, Flag}
          when Flag =:= inet;
               Flag =:= socket ->
            persistent_term:put({kernel, inet_backend}, Flag);
        _ -> ok
    end,
    Db = ets:new(inet_db, [public, named_table]),
    reset_db(Db),
    CacheOpts = [public, bag, {keypos,#dns_rr.domain}, named_table],
    Cache = ets:new(inet_cache, CacheOpts),
    HostsByname = ets:new(inet_hosts_byname, [named_table]),
    HostsByaddr = ets:new(inet_hosts_byaddr, [named_table]),
    HostsFileByname = ets:new(inet_hosts_file_byname, [named_table]),
    HostsFileByaddr = ets:new(inet_hosts_file_byaddr, [named_table]),
    {ok, #state{db = Db,
		cache = Cache,
		hosts_byname = HostsByname,
		hosts_byaddr = HostsByaddr,
		hosts_file_byname = HostsFileByname,
		hosts_file_byaddr = HostsFileByaddr,
		cache_timer = init_timer() }}.

reset_db(Db) ->
    ets:insert(
      Db,
      [{hostname, []},
       {res_ns, []},
       {res_alt_ns, []},
       {res_search, []},
       {res_domain, ""},
       {res_lookup, []},
       {res_recurse, true},
       {res_usevc, false},
       {res_id, 0},
       {res_retry, ?RES_RETRY},
       {res_timeout, ?RES_TIMEOUT},
       {res_inet6, false},
       {res_edns, false},
       {res_udp_payload_size, ?DNS_UDP_PAYLOAD_SIZE},
       {cache_size, ?CACHE_LIMIT},
       {cache_refresh_interval,?CACHE_REFRESH},
       {socks5_server, ""},
       {socks5_port, ?IPPORT_SOCKS},
       {socks5_methods, [none]},
       {socks5_noproxy, []},
       {tcp_module,  ?DEFAULT_TCP_MODULE},
       {udp_module,  ?DEFAULT_UDP_MODULE},
       {sctp_module, ?DEFAULT_SCTP_MODULE}]).

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------

-spec handle_call(term(), {pid(), term()}, state()) ->
        {'reply', term(), state()} | {'stop', 'normal', 'ok', state()}.

handle_call(Request, From, #state{db=Db}=State) ->
    case Request of
	{load_hosts_file,IPNmAs} when is_list(IPNmAs) ->
	    load_hosts_list(IPNmAs, State#state.hosts_file_byname, State#state.hosts_file_byaddr),
	    {reply, ok, State};

	{add_host,{A,B,C,D}=IP,[N|As]=Names}
	when ?ip(A,B,C,D), is_list(N), is_list(As) ->
	    do_add_host(State#state.hosts_byname,
			State#state.hosts_byaddr,
			Names, inet, IP),
	    {reply, ok, State};
	{add_host,{A,B,C,D,E,F,G,H}=IP,[N|As]=Names}
	when ?ip6(A,B,C,D,E,F,G,H), is_list(N), is_list(As) ->
	    do_add_host(State#state.hosts_byname,
			State#state.hosts_byaddr,
			Names, inet6, IP),
	    {reply, ok, State};

	{del_host,{A,B,C,D}=IP}	when ?ip(A,B,C,D) ->
	    do_del_host(State#state.hosts_byname,
			State#state.hosts_byaddr,
			IP),
	    {reply, ok, State};
	{del_host,{A,B,C,D,E,F,G,H}=IP}	when ?ip6(A,B,C,D,E,F,G,H) ->
	    do_del_host(State#state.hosts_byname,
			State#state.hosts_byaddr,
			IP),
	    {reply, ok, State};

	{add_rr, RR} when is_record(RR, dns_rr) ->
	    ?dbg("add_rr: ~p~n", [RR]),
	    do_add_rr(RR, Db, State),
	    {reply, ok, State};

	{del_rr, RR} when is_record(RR, dns_rr) ->
	    %% note. del_rr will handle wildcards !!!
	    Cache = State#state.cache,
	    ets:match_delete(Cache, RR),
	    {reply, ok, State};

	{listop, Opt, Op, E} ->
	    El = [E],
	    case res_check_option(Opt, El) of
		true ->
		    Optname = res_optname(Opt),
		    Es = ets:lookup_element(Db, Optname, 2),
		    NewEs = case Op of
				ins -> [E | lists_delete(E, Es)];
				add -> lists_delete(E, Es) ++ El;
				del -> lists_delete(E, Es)
			    end,
		    ets:insert(Db, {Optname, NewEs}),
		    {reply,ok,State};
		false ->
		    {reply,error,State}
	    end;

	{listreplace, Opt, Els} ->
	    case res_check_option(Opt, Els) of
		true ->
		    ets:insert(Db, {res_optname(Opt), Els}),
		    {reply,ok,State};
		false ->
		    {reply,error,State}
	    end;

	{set_hostname, Name} ->
	    case inet_parse:visible_string(Name) of
		true ->
		    ets:insert(Db, {hostname, Name}),
		    {reply, ok, State};
		false ->
		    {reply, error, State}
	    end;

	{res_set, hosts_file_name=Option, Fname} ->
	    handle_set_file(
	      Option, Fname, res_hosts_file_tm, res_hosts_file_info,
	      undefined, From, State);
	{res_set, resolv_conf_name=Option, Fname} ->
	    handle_set_file(
	      Option, Fname, res_resolv_conf_tm, res_resolv_conf_info,
	      undefined, From, State);

	{res_set, hosts_file=Option, Fname_or_Tm} ->
	    handle_set_file(
	      Option, Fname_or_Tm, res_hosts_file_tm, res_hosts_file_info,
	      fun (File, Bin) ->
		      case inet_parse:hosts(
			     File, {chars,Bin}) of
			  {ok,Opts} ->
			      [{load_hosts_file,Opts}];
			  _ -> error
		      end
	      end,
	      From, State);
	%%
	{res_set, resolv_conf=Option, Fname_or_Tm} ->
	    handle_set_file(
	      Option, Fname_or_Tm, res_resolv_conf_tm, res_resolv_conf_info,
	      fun (File, Bin) ->
		      case inet_parse:resolv(
			     File, {chars,Bin}) of
			  {ok,Opts} ->
			      Search =
				  lists:foldl(
				    fun ({search,L}, _) ->
					    L;
					({domain,""}, S) ->
					    S;
					({domain,D}, _) ->
					    [D];
					(_, S) ->
					    S
				    end, [], Opts),
			      NSs = [{NS,?NAMESERVER_PORT} || {nameserver,NS} <- Opts],
			      [{replace_search,Search},
			       {replace_ns,NSs},
			       clear_cache];
			  _ -> error
		      end
	      end,
	      From, State);
	%%
	{res_set, Opt, Value} ->
	    case res_optname(Opt) of
		undefined ->
		    {reply, error, State};
		Optname ->
		    case res_check_option(Opt, Value) of
			true ->
			    ets:insert(Db, {Optname, Value}),
			    {reply, ok, State};
			false ->
			    {reply, error, State}
		    end
	    end;

	{set_resolv_conf_tm, TM} ->
	    ets:insert(Db, {res_resolv_conf_tm, TM}),
	    {reply, ok, State};

	{set_hosts_file_tm, TM} ->
	    ets:insert(Db, {res_hosts_file_tm, TM}),
	    {reply, ok, State};

	{set_socks_server, {A,B,C,D}} when ?ip(A,B,C,D) ->
	    ets:insert(Db, {socks5_server, {A,B,C,D}}),
	    {reply, ok, State};

	{set_socks_port, Port} when is_integer(Port) ->
	    ets:insert(Db, {socks5_port, Port}),
	    {reply, ok, State};

	{add_socks_methods, Ls} -> 
	    As = ets:lookup_element(Db, socks5_methods, 2),
	    As1 = lists_subtract(As, Ls),
	    ets:insert(Db, {socks5_methods, As1 ++ Ls}),
	    {reply, ok, State};
	    
	{del_socks_methods, Ls} ->
	    As = ets:lookup_element(Db, socks5_methods, 2),
	    As1 = lists_subtract(As, Ls),
	    case lists:member(none, As1) of
		false -> ets:insert(Db, {socks5_methods, As1 ++ [none]});
		true  -> ets:insert(Db, {socks5_methods, As1})
	    end,
	    {reply, ok, State};
	
	del_socks_methods ->
	    ets:insert(Db, {socks5_methods, [none]}),
	    {reply, ok, State};

	{add_socks_noproxy, {{A,B,C,D},{MA,MB,MC,MD}}} 
	when ?ip(A,B,C,D), ?ip(MA,MB,MC,MD) ->
	    As = ets:lookup_element(Db, socks5_noproxy, 2),
	    ets:insert(Db, {socks5_noproxy, As++[{{A,B,C,D},{MA,MB,MC,MD}}]}),
	    {reply, ok, State};

	{del_socks_noproxy, {A,B,C,D}=IP} when ?ip(A,B,C,D) ->
	    As = ets:lookup_element(Db, socks5_noproxy, 2),
	    ets:insert(Db, {socks5_noproxy, lists_keydelete(IP, 1, As)}),
	    {reply, ok, State};

	{set_tcp_module, Mod} when is_atom(Mod) ->
	    ets:insert(Db, {tcp_module, Mod}), %% check/load module ?
	    {reply, ok, State};

	{set_udp_module, Mod} when is_atom(Mod) ->
	    ets:insert(Db, {udp_module, Mod}), %% check/load module ?
	    {reply, ok, State};

	{set_sctp_module, Fam} when is_atom(Fam) ->
	    ets:insert(Db, {sctp_module, Fam}), %% check/load module ?
	    {reply, ok, State};

	{set_cache_size, Size} when is_integer(Size), Size >= 0 ->
	    ets:insert(Db, {cache_size, Size}),
	    {reply, ok, State};
	
	{set_cache_refresh, Time} when is_integer(Time), Time > 0 ->
	    Time1 = ((Time+999) div 1000)*1000, %% round up
	    ets:insert(Db, {cache_refresh_interval, Time1}),
	    _ = stop_timer(State#state.cache_timer),
	    {reply, ok, State#state{cache_timer = init_timer()}};

	clear_hosts ->
	    ets:delete_all_objects(State#state.hosts_byname),
	    ets:delete_all_objects(State#state.hosts_byaddr),
	    {reply, ok, State};

	clear_cache ->
	    ets:match_delete(State#state.cache, '_'),
	    {reply, ok, State};

	reset ->
	    reset_db(Db),
	    _ = stop_timer(State#state.cache_timer),
	    {reply, ok, State#state{cache_timer = init_timer()}};

	{add_rc_list, List} ->
	    handle_rc_list(List, From, State);

	stop ->
	    {stop, normal, ok, State};

	_ ->
	    {reply, error, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

-spec handle_cast(term(), state()) -> {'noreply', state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

-spec handle_info(term(), state()) -> {'noreply', state()}.

handle_info(refresh_timeout, State) ->
    do_refresh_cache(State#state.cache),
    {noreply, State#state{cache_timer = init_timer()}};

handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, State) ->
    _ = stop_timer(State#state.cache_timer),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

handle_set_file(
  Option, Tm, TagTm, TagInfo, ParseFun, From, #state{db=Db}=State)
  when is_integer(Tm) ->
    %%
    %% Maybe update file content
    %%
    try ets:lookup_element(Db, TagTm, 2) of
        Tm ->
            %% Current update request
            File = ets:lookup_element(Db, res_optname(Option), 2),
            Finfo = ets:lookup_element(Db, TagInfo, 2),
            handle_update_file(
              Finfo, File, TagTm, TagInfo, ParseFun, From, State);
        _ ->
            %% Late request - ignore update
            {reply, ok, State}
    catch error:badarg ->
            %% Option no longer set - ignore update
            {reply, ok, State}
    end;
handle_set_file(
  Option, Fname, TagTm, TagInfo, ParseFun, From, #state{db=Db}=State) ->
    case res_check_option(Option, Fname) of
	true when Fname =:= "" ->
            %% Delete file content and monitor
	    ets:insert(Db, {res_optname(Option), Fname}),
	    ets:delete(Db, TagInfo),
	    ets:delete(Db, TagTm),
	    handle_set_file(ParseFun, Fname, <<>>, From, State);
	true when ParseFun =:= undefined ->
            %% Set file name and monitor
	    File = filename:flatten(Fname),
	    ets:insert(Db, {res_optname(Option), File}),
	    ets:insert(Db, {TagInfo, undefined}),
	    TimeZero = times() - (?RES_FILE_UPDATE_TM + 1), % Early enough
	    ets:insert(Db, {TagTm, TimeZero}),
	    {reply,ok,State};
	true ->
            %% Set file name and monitor, read content
	    File = filename:flatten(Fname),
	    ets:insert(Db, {res_optname(Option), File}),
            handle_update_file(
              undefined, File, TagTm, TagInfo, ParseFun, From, State);
	false -> {reply,error,State}
    end.

handle_set_file(ParseFun, File, Bin, From, State) ->
    case ParseFun(File, Bin) of
	error ->
	    {reply,error,State};
	Opts ->
	    handle_rc_list(Opts, From, State)
    end.

handle_update_file(
  Finfo, File, TagTm, TagInfo, ParseFun, From, #state{db = Db} = State) ->
    %%
    %% Update file content if file has been updated
    %%
    case erl_prim_loader:read_file_info(File) of
        {ok, Finfo} ->
            %% No file update - we are done
            {reply, ok, State};
        {ok, Finfo_1} ->
            %% File updated - read content
            ets:insert(Db, {TagInfo, Finfo_1}),
            ets:insert(Db, {TagTm, times()}),
            Bin =
                case erl_prim_loader:get_file(File) of
                    {ok, B, _} -> B;
                    _ -> <<>>
                end,
            handle_set_file(ParseFun, File, Bin, From, State);
        _ ->
            %% No file - clear content and reset monitor
            ets:insert(Db, {TagInfo, undefined}),
            ets:insert(Db, {TagTm, times()}),
            handle_set_file(ParseFun, File, <<>>, From, State)
    end.

%% Byname has lowercased names while Byaddr keep the name casing.
%% This is to be able to reconstruct the original /etc/hosts entry.

do_add_host(Byname, Byaddr, Names, Type, IP) ->
    Nms = [tolower(Nm) || Nm <- Names],
    add_ip_bynms(Byname, Type, IP, Nms, Names),
    Key = {Type, IP},
    try ets:lookup_element(Byaddr, Key, 2) of
        Names_0 ->
            %% Delete IP address from byname entries
            NmsSet = % Set of new tolower(Name)s
                lists:foldl(
                  fun (Nm, Set) ->
                          maps:put(Nm, [], Set)
                  end, #{}, Nms),
            del_ip_bynms(
              Byname, Type, IP,
              [Nm || Nm <- [tolower(Name) || Name <- Names_0],
                     not maps:is_key(Nm, NmsSet)])
    catch error:badarg ->
            ok
    end,
    %% Replace the entry in the byaddr table
    ets:insert(Byaddr, {Key, Names}),
    ok.

do_del_host(Byname, Byaddr, IP) ->
    Fam = inet_family(IP),
    Key = {Fam, IP},
    try ets:lookup_element(Byaddr, Key, 2) of
        Names ->
            %% Delete IP address from byname entries
            del_ip_bynms(
              Byname, Fam, IP,
              [tolower(Name) || Name <- Names]),
            %% Delete from byaddr table
            true = ets:delete(Byaddr, Key),
            ok
    catch error:badarg ->
            ok
    end.


add_ip_bynms(Byname, Fam, IP, Nms, Names) ->
    lists:foreach(
      fun (Nm) ->
              Key = {Fam, Nm},
              case ets:lookup(Byname, Key) of
                  [{_Key, [IP | _] = IPs, _Names_1}] ->
                      %% Replace names in the byname entry
                      true =
                          ets:insert(
                            Byname,
                            {Key, IPs, Names});
                  [{_Key, IPs, Names_0}] ->
                      case lists:member(IP, IPs) of
                          true ->
                              ok;
                          false ->
                              %% Add the IP address
                              true =
                                  ets:insert(
                                    Byname,
                                    {Key, IPs ++ [IP], Names_0})
                      end;
                  [] ->
                      %% Create a new byname entry
                      true =
                          ets:insert(Byname, {Key, [IP], Names})
              end
      end, Nms).

del_ip_bynms(Byname, Fam, IP, Nms) ->
    lists:foreach(
      fun (Nm) ->
              Key = {Fam, Nm},
              case ets:lookup(Byname, Key) of
                  [{_Key, [IP], _Names}] ->
                      %% Delete whole entry
                      true = ets:delete(Byname, Key);
                  [{_Key, IPs_0, Names_0}] ->
                      case lists:member(IP, IPs_0) of
                          true ->
                              %% Delete the IP address from list
                              IPs = lists:delete(IP, IPs_0),
                              true =
                                  ets:insert(
                                    Byname, {Key, IPs, Names_0});
                          false ->
                              ok
                      end;
                  [] ->
                      ok
              end
      end, Nms).


inet_family(T) when tuple_size(T) =:= 4 -> inet;
inet_family(T) when tuple_size(T) =:= 8 -> inet6.


%% Hosts =  [ {IP, Name, Aliases}, ... ]
%% ByaddrMap = #{ {Fam, IP} := rev(Names) }
%% BynameMap = #{ {Fam, tolower(Name)} := {rev([IP, ...]), Names}}

%% Synchronises internal tables with .hosts/aliases file
load_hosts_list(Hosts, Byname, Byaddr) ->
    %% Create byaddr and byname maps
    {ByaddrMap, BynameMap} = load_hosts_list(Hosts),
    %% Insert or overwrite existing keys
    ets:insert(
      Byaddr,
      [{Addr, lists:reverse(NamesR)}
       || {Addr, NamesR} <- maps:to_list(ByaddrMap)]),
    ets:insert(
      Byname,
      [{Fam_Nm, lists:reverse(IPsR), Names}
       || {Fam_Nm, {IPsR, Names}} <- maps:to_list(BynameMap)]),
    %% Delete no longer existing keys
    ets_clean_map_keys(Byaddr, ByaddrMap),
    ets_clean_map_keys(Byname, BynameMap).

load_hosts_list(Hosts) ->
    load_hosts_list_byaddr(Hosts, #{}, []).

load_hosts_list_byaddr(
  [], ByaddrMap, Addrs) ->
    %% Now for the byname table...
    load_hosts_list_byname(lists:reverse(Addrs), ByaddrMap, #{});
%% Traverse hosts list, create byaddr map and insertion order list
load_hosts_list_byaddr(
  [{IP, Name, Aliases} | Hosts], ByaddrMap, Addrs) ->
    Addr = {inet_family(IP), IP},
    case ByaddrMap of
        #{Addr := NamesR} ->
            %% Concatenate names to existing IP address entry
            load_hosts_list_byaddr(
              Hosts,
              ByaddrMap#{Addr := lists:reverse(Aliases, [Name | NamesR])},
              Addrs);
        #{} ->
            %% First entry for an IP address
            load_hosts_list_byaddr(
              Hosts,
              ByaddrMap#{Addr => lists:reverse(Aliases, [Name])},
              [Addr | Addrs])
    end.

%% Traverse in insertion order from byaddr pass
load_hosts_list_byname(
  [], ByaddrMap, BynameMap) ->
    {ByaddrMap, BynameMap};
load_hosts_list_byname(
  [{Fam, IP} = Addr | Addrs], ByaddrMap, BynameMap) ->
    Names = lists:reverse(maps:get(Addr, ByaddrMap)),
    %% Traverse all names for this IP address
    load_hosts_list_byname(
      Addrs, ByaddrMap,
      load_hosts_list_byname(Fam, IP, BynameMap, Names, Names)).

load_hosts_list_byname(_Fam, _IP, BynameMap, _Names_0, []) ->
    BynameMap;
load_hosts_list_byname(
  Fam, IP, BynameMap, Names_0, [Name | Names]) ->
    Key = {Fam, tolower(Name)},
    case BynameMap of
        #{Key := {IPsR, Names_1}} ->
            %% Add IP address to existing name entry
            load_hosts_list_byname(
              Fam, IP,
              BynameMap#{Key := {[IP | IPsR], Names_1}},
              Names_0, Names);
        #{} ->
            %% First entry for a name
            load_hosts_list_byname(
              Fam, IP,
              BynameMap#{Key => {[IP], Names_0}},
              Names_0, Names)
    end.

ets_clean_map_keys(Tab, Map) ->
    true = ets:safe_fixtable(Tab, true),
    ets_clean_map_keys(Tab, Map, ets:first(Tab)),
    true = ets:safe_fixtable(Tab, false),
    ok.
%%
ets_clean_map_keys(_Tab, _Map, '$end_of_table') ->
    ok;
ets_clean_map_keys(Tab, Map, Key) ->
    case maps:is_key(Key, Map) of
        true ->
            ets_clean_map_keys(Tab, Map, ets:next(Tab, Key));
        false ->
            true = ets:delete(Tab, Key),
            ets_clean_map_keys(Tab, Map, ets:next(Tab, Key))
    end.


%% Loop over .inetrc option list and call handle_call/3 for each
%%
handle_rc_list([], _From, State) ->
    {reply, ok, State};
handle_rc_list([Opt|Opts], From, State) ->
    case rc_opt_req(Opt) of
	undefined ->
	    {reply, {error,{badopt,Opt}}, State};
	Req ->
	    case handle_calls(Req, From, State) of
		{reply, ok, NewState} ->
		    handle_rc_list(Opts, From, NewState);
		Result -> Result
	    end
    end;
handle_rc_list(_, _From, State) ->
    {reply, error, State}.

handle_calls([], _From, State) ->
    {reply, ok, State};
handle_calls([Req|Reqs], From, State) ->
    case handle_call(Req, From, State) of
	{reply, ok, NewState} ->
	    handle_calls(Reqs, From, NewState);
	{reply, _, NewState} ->
	    {reply, error, NewState}
	    %% {noreply,_} is currently not returned by handle_call/3
    end;
handle_calls(Req, From, State) ->
    handle_call(Req, From, State).

%% Translate .inetrc option into gen_server request
%%
rc_opt_req({nameserver, Ns}) ->
    {listop,nameservers,add,{Ns,?NAMESERVER_PORT}};
rc_opt_req({nameserver, Ns, Port}) ->
    {listop,nameservers,add,{Ns,Port}};
rc_opt_req({alt_nameserver, Ns}) ->
    {listop,alt_nameservers,add,{Ns,?NAMESERVER_PORT}};
rc_opt_req({alt_nameserver, Ns, Port}) ->
    {listop,alt_nameservers,add,{Ns,Port}};
rc_opt_req({socks5_noproxy, IP, Mask}) ->
    {add_socks_noproxy, {IP, Mask}};
rc_opt_req({search, Ds}) when is_list(Ds) ->
    try	[{listop,search,add,D} || D <- Ds]
    catch error:_ -> undefined
    end;
rc_opt_req({host, IP, Aliases}) -> {add_host, IP, Aliases};
rc_opt_req({load_hosts_file, _}=Req) -> Req;
rc_opt_req({lookup, Ls}) ->
    try {res_set, lookup, translate_lookup(Ls)}
    catch error:_ -> undefined
    end;
rc_opt_req({replace_ns,Ns}) ->
    {listreplace,nameservers,Ns};
rc_opt_req({replace_search,Search}) ->
    {listreplace,search,Search};
rc_opt_req({Name,Arg}) ->
    case rc_reqname(Name) of
	undefined ->
	    case is_res_set(Name) of
		true -> {res_set,Name,Arg};
		false -> undefined
	    end;
	Req -> {Req, Arg}
    end;
rc_opt_req(clear_ns) ->
    [{listreplace,nameservers,[]},{listreplace,alt_nameservers,[]}];
rc_opt_req(clear_search) ->
    {listreplace,search,[]};
rc_opt_req(Opt) when is_atom(Opt) ->
    case is_reqname(Opt) of
	true -> Opt;
	false -> undefined
    end;
rc_opt_req(_) -> undefined.
%%
rc_reqname(socks5_server) -> set_socks_server;
rc_reqname(socks5_port) -> set_socks_port;
rc_reqname(socks5_methods) -> set_socks_methods;
rc_reqname(cache_refresh) -> set_cache_refresh;
rc_reqname(cache_size) -> set_cache_size;
rc_reqname(udp) -> set_udp_module;
rc_reqname(sctp) -> set_sctp_module;
rc_reqname(tcp) -> set_tcp_module;
rc_reqname(_) -> undefined.
%%
is_res_set(domain) -> true;
is_res_set(lookup) -> true;
is_res_set(timeout) -> true;
is_res_set(retry) -> true;
is_res_set(inet6) -> true;
is_res_set(usevc) -> true;
is_res_set(edns) -> true;
is_res_set(udp_payload_size) -> true;
is_res_set(resolv_conf) -> true;
is_res_set(hosts_file) -> true;
is_res_set(_) -> false.
%%
is_reqname(reset) -> true;
is_reqname(clear_cache) -> true;
is_reqname(clear_hosts) -> true;
is_reqname(_) -> false.

%% Add a resource record to the cache if there are space left.
%% If the cache is full this function first deletes old entries,
%% i.e. entries with oldest latest access time.
%% #dns_rr.cnt is used to store the access time instead of number of
%% accesses.
do_add_rr(RR, Db, State) ->
    CacheDb = State#state.cache,
    TM = times(),
    case alloc_entry(Db, CacheDb, TM) of
	true ->
	    cache_rr(Db, CacheDb, RR#dns_rr{tm = TM, cnt = TM});
	_ ->
	    false
    end.

cache_rr(_Db, Cache, RR) ->
    %% delete possible old entry
    ets:match_delete(Cache, RR#dns_rr{cnt = '_', tm = '_', ttl = '_',
				      bm = '_', func = '_'}),
    ets:insert(Cache, RR).

times() ->
    erlang:monotonic_time(second).
    %% erlang:convert_time_unit(erlang:monotonic_time() - erlang:system_info(start_time),
    %%     		     native, second).

%% filter old resource records and update access count

filter_rr([RR | RRs], Time) when RR#dns_rr.ttl =:= 0 -> %% at least once
    ets:match_delete(inet_cache, RR),
    [RR | filter_rr(RRs, Time)];
filter_rr([RR | RRs], Time) when RR#dns_rr.tm + RR#dns_rr.ttl < Time ->
    ets:match_delete(inet_cache, RR),
    filter_rr(RRs, Time);
filter_rr([RR | RRs], Time) ->
    ets:match_delete(inet_cache, RR),
    ets:insert(inet_cache, RR#dns_rr { cnt = Time }),
    [RR | filter_rr(RRs, Time)];
filter_rr([], _Time) ->  [].

%% Lower case the domain name before storage.
%%
lower_rr(#dns_rr{domain=Domain}=RR) when is_list(Domain) ->
    RR#dns_rr { domain = tolower(Domain) };
lower_rr(RR) -> RR.

%%
%% Case fold upper-case to lower-case according to RFC 4343
%% "Domain Name System (DNS) Case Insensitivity Clarification".
%%
%% NOTE: this code is in kernel and we don't want to relay
%% to much on stdlib. Furthermore string:to_lower/1
%% does not follow RFC 4343.
%%
tolower([]) -> [];
tolower([C|Cs]) when is_integer(C) ->
    if  C >= $A, C =< $Z ->
	    [(C-$A)+$a|tolower(Cs)];
	true ->
	    [C|tolower(Cs)]
    end.

dn_ip6_int(A,B,C,D,E,F,G,H) ->
    dnib(H) ++ dnib(G) ++ dnib(F) ++ dnib(E) ++ 
	dnib(D) ++ dnib(C) ++ dnib(B) ++ dnib(A) ++ "ip6.int".

dn_in_addr_arpa(A,B,C,D) ->
    integer_to_list(D) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(A) ++ ".in-addr.arpa".

dnib(X) ->
    [hex(X), $., hex(X bsr 4), $., hex(X bsr 8), $., hex(X bsr 12), $.].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $a
    end.

%% Strip trailing dot, do not produce garbage unless necessary.
%%
stripdot(Name) ->
    case stripdot_1(Name) of
	false -> Name;
	N     -> N
    end.
%%
stripdot_1([$.])  -> [];
stripdot_1([])    -> false;
stripdot_1([H|T]) ->
    case stripdot_1(T) of
	false     -> false;
	N         -> [H|N]
    end.

%% -------------------------------------------------------------------
%% Refresh cache at regular intervals, i.e. delete expired #dns_rr's.
%% -------------------------------------------------------------------
init_timer() ->
    erlang:send_after(cache_refresh(), self(), refresh_timeout).

stop_timer(undefined) ->
    undefined;
stop_timer(Timer) ->
    erlang:cancel_timer(Timer).

cache_refresh() ->
    case db_get(cache_refresh_interval) of
	undefined -> ?CACHE_REFRESH;
	Val       -> Val
    end.

%% Delete all entries with expired TTL.
%% Returns the access time of the entry with the oldest access time
%% in the cache.
do_refresh_cache(CacheDb) ->
    Now = times(),
    do_refresh_cache(ets:first(CacheDb), CacheDb, Now, Now).

do_refresh_cache('$end_of_table', _, _, OldestT) ->
    OldestT;
do_refresh_cache(Key, CacheDb, Now, OldestT) ->
    Fun = fun(RR, T) when RR#dns_rr.tm + RR#dns_rr.ttl < Now ->
		  ets:match_delete(CacheDb, RR),
		  T;
	     (#dns_rr{cnt = C}, T) when C < T ->
		  C;
	     (_, T) ->
		  T
	  end,
    Next = ets:next(CacheDb, Key),
    OldT = lists:foldl(Fun, OldestT, ets:lookup(CacheDb, Key)),
    do_refresh_cache(Next, CacheDb, Now, OldT).

%% -------------------------------------------------------------------
%% Allocate room for a new entry in the cache.
%% Deletes entries with expired TTL and all entries with latest
%% access time older than
%% trunc((TM - OldestTM) * 0.3) + OldestTM from the cache if it
%% is full. Does not delete more than 10% of the entries in the cache
%% though, unless they there deleted due to expired TTL.
%% Returns: true if space for a new entry otherwise false.
%% -------------------------------------------------------------------
alloc_entry(Db, CacheDb, TM) ->
    CurSize = ets:info(CacheDb, size),
    case ets:lookup_element(Db, cache_size, 2) of
	Size when Size =< CurSize, Size > 0 ->
	    alloc_entry(CacheDb, CurSize, TM, trunc(Size * 0.1) + 1);
	Size when Size =< 0 ->
	    false;
	_Size ->
	    true
    end.

alloc_entry(CacheDb, OldSize, TM, N) ->
    OldestTM = do_refresh_cache(CacheDb),     % Delete timedout entries
    case ets:info(CacheDb, size) of
	OldSize ->
	    %% No entrys timedout
	    delete_n_oldest(CacheDb, TM, OldestTM, N);
	_ ->
	    true
    end.

delete_n_oldest(CacheDb, TM, OldestTM, N) ->
    DelTM = trunc((TM - OldestTM) * 0.3) + OldestTM,
    delete_older(CacheDb, DelTM, N) =/= 0.

%% Delete entries with latest access time older than TM.
%% Delete max N number of entries.
%% Returns the number of deleted entries.
delete_older(CacheDb, TM, N) ->
    delete_older(ets:first(CacheDb), CacheDb, TM, N, 0).

delete_older('$end_of_table', _, _, _, M) ->
    M;
delete_older(_, _, _, N, M) when N =< M ->
    M;
delete_older(Domain, CacheDb, TM, N, M) ->
    Next = ets:next(CacheDb, Domain),
    Fun = fun(RR, MM) when RR#dns_rr.cnt =< TM ->
		  ets:match_delete(CacheDb, RR),
		  MM + 1;
	     (_, MM) ->
		  MM
	  end,
    M1 = lists:foldl(Fun, M, ets:lookup(CacheDb, Domain)),
    delete_older(Next, CacheDb, TM, N, M1).


%% as lists:delete/2, but delete all exact matches
%%
lists_delete(_, []) -> [];
lists_delete(E, [E|Es]) ->
    lists_delete(E, Es);
lists_delete(E, [X|Es]) ->
    [X|lists_delete(E, Es)].

%% as '--'/2 aka lists:subtract/2 but delete all exact matches
lists_subtract(As0, Bs) ->
    lists:foldl(fun (E, As) -> lists_delete(E, As) end, As0, Bs).

%% as lists:keydelete/3, but delete all _exact_ key matches
lists_keydelete(_, _, []) -> [];
lists_keydelete(K, N, [T|Ts]) when element(N, T) =:= K ->
    lists_keydelete(K, N, Ts);
lists_keydelete(K, N, [X|Ts]) ->
    [X|lists_keydelete(K, N, Ts)].
