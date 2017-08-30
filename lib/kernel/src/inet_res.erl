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
%% RFC 1035, 2671, 2782, 2915.
%%
-module(inet_res).

%-compile(export_all).

-export([gethostbyname/1, gethostbyname/2, gethostbyname/3,
	 gethostbyname_tm/3]).
-export([gethostbyaddr/1, gethostbyaddr/2,
	 gethostbyaddr_tm/2]).
-export([getbyname/2, getbyname/3,
	 getbyname_tm/3]).

-export([resolve/3, resolve/4, resolve/5]).
-export([lookup/3, lookup/4, lookup/5]).
-export([dns_msg/1]).

-export([nslookup/3, nslookup/4]).
-export([nnslookup/4, nnslookup/5]).

-include_lib("kernel/include/inet.hrl").
-include("inet_res.hrl").
-include("inet_dns.hrl").
-include("inet_int.hrl").

-define(verbose(Cond, Format, Args),
	case begin Cond end of
	    true -> io:format(begin Format end, begin Args end);
	    false -> ok
	end).

-type res_option() ::
        {alt_nameservers, [nameserver()]}
      | {edns, 0 | false}
      | {inet6, boolean()}
      | {nameservers, [nameserver()]}
      | {recurse, boolean()}
      | {retry, integer()}
      | {timeout, integer()}
      | {udp_payload_size, integer()}
      | {usevc, boolean()}.

-type nameserver() :: {inet:ip_address(), Port :: 1..65535}.

-type res_error() :: formerr | qfmterror | servfail | nxdomain |
                     notimp | refused | badvers | timeout.

-type dns_name() :: string().

-type rr_type() :: a | aaaa | cname | gid | hinfo | ns | mb | md | mg | mf
                 | minfo | mx | naptr | null | ptr | soa | spf | srv | txt
                 | uid | uinfo | unspec | wks.

-type dns_class() :: in | chaos | hs | any.

-type dns_msg() :: term().

-type dns_data() ::
        dns_name()
      | inet:ip4_address()
      | inet:ip6_address()
      | {MName :: dns_name(),
         RName :: dns_name(),
         Serial :: integer(),
         Refresh :: integer(),
         Retry :: integer(),
         Expiry :: integer(),
         Minimum :: integer()}
      | {inet:ip4_address(), Proto :: integer(), BitMap :: binary()}
      | {CpuString :: string(), OsString :: string()}
      | {RM :: dns_name(), EM :: dns_name()}
      | {Prio :: integer(), dns_name()}
      | {Prio :: integer(),Weight :: integer(),Port :: integer(),dns_name()}
      | {Order :: integer(),Preference :: integer(),Flags :: string(),
         Services :: string(),Regexp :: string(), dns_name()}
      | [string()]
      | binary().

%% --------------------------------------------------------------------------
%% resolve:
%%
%% Nameserver query
%%

-spec resolve(Name, Class, Type) -> {ok, dns_msg()} | Error when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Error :: {error, Reason} | {error,{Reason,dns_msg()}},
      Reason :: inet:posix() | res_error().

resolve(Name, Class, Type) ->
    resolve(Name, Class, Type, [], infinity).

-spec resolve(Name, Class, Type, Opts) ->
                     {ok, dns_msg()} | Error when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Opts :: [Opt],
      Opt :: res_option() | verbose | atom(),
      Error :: {error, Reason} | {error,{Reason,dns_msg()}},
      Reason :: inet:posix() | res_error().

resolve(Name, Class, Type, Opts) ->
    resolve(Name, Class, Type, Opts, infinity).

-spec resolve(Name, Class, Type, Opts, Timeout) ->
                     {ok, dns_msg()} | Error when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Opts :: [Opt],
      Opt :: res_option() | verbose | atom(),
      Timeout :: timeout(),
      Error :: {error, Reason} | {error,{Reason,dns_msg()}},
      Reason :: inet:posix() | res_error().

resolve(Name, Class, Type, Opts, Timeout) ->
    case nsdname(Name) of
	{ok, Nm} ->
	    Timer = inet:start_timer(Timeout),
	    Res = res_query(Nm, Class, Type, Opts, Timer),
	    _ = inet:stop_timer(Timer),
	    Res;
	Error ->
	    Error
    end.

%% --------------------------------------------------------------------------
%% lookup:
%%
%% Convenience wrapper to resolve/3,4,5 that filters out all answer data
%% fields of the class and type asked for.

-spec lookup(Name, Class, Type) -> [dns_data()] when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type().

lookup(Name, Class, Type) ->
    lookup(Name, Class, Type, []).

-spec lookup(Name, Class, Type, Opts) -> [dns_data()] when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Opts :: [res_option() | verbose].

lookup(Name, Class, Type, Opts) ->
    lookup(Name, Class, Type, Opts, infinity).

-spec lookup(Name, Class, Type, Opts, Timeout) -> [dns_data()] when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Opts :: [res_option() | verbose],
      Timeout :: timeout().

lookup(Name, Class, Type, Opts, Timeout) ->
    lookup_filter(resolve(Name, Class, Type, Opts, Timeout),
		     Class, Type).

lookup_filter({ok,#dns_rec{anlist=Answers}}, Class, Type) ->
    [A#dns_rr.data || A <- Answers,
		      Class =:= any orelse A#dns_rr.class =:= Class,
		      Type =:= any orelse A#dns_rr.type =:= Type];
lookup_filter({error,_}, _, _) -> [].

%% --------------------------------------------------------------------------
%% nslookup:
%%
%% Do a general nameserver lookup
%%
%% Perform nslookup on standard config !!    
%%
%% To be deprecated

-spec nslookup(Name, Class, Type) -> {ok, dns_msg()} | {error, Reason} when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Reason :: inet:posix() | res_error().

nslookup(Name, Class, Type) ->
    do_nslookup(Name, Class, Type, [], infinity).

-spec nslookup(Name, Class, Type, Timeout) ->
                      {ok, dns_msg()} | {error, Reason} when
                  Name :: dns_name() | inet:ip_address(),
                  Class :: dns_class(),
                  Type :: rr_type(),
                  Timeout :: timeout(),
                  Reason :: inet:posix() | res_error();
              (Name, Class, Type, Nameservers) ->
                      {ok, dns_msg()} | {error, Reason} when
                  Name :: dns_name() | inet:ip_address(),
                  Class :: dns_class(),
                  Type :: rr_type(),
                  Nameservers :: [nameserver()],
                  Reason :: inet:posix() | res_error().

nslookup(Name, Class, Type, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    do_nslookup(Name, Class, Type, [], Timeout);
nslookup(Name, Class, Type, NSs) ->             % For backwards compatibility
    nnslookup(Name, Class, Type, NSs).          % with OTP R6B only

-spec nnslookup(Name, Class, Type, Nameservers) ->
                      {ok, dns_msg()} | {error, Reason} when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Nameservers :: [nameserver()],
      Reason :: inet:posix().

nnslookup(Name, Class, Type, NSs) ->
    nnslookup(Name, Class, Type, NSs, infinity).

-spec nnslookup(Name, Class, Type, Nameservers, Timeout) ->
                      {ok, dns_msg()} | {error, Reason} when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: rr_type(),
      Timeout :: timeout(),
      Nameservers :: [nameserver()],
      Reason :: inet:posix().

nnslookup(Name, Class, Type, NSs, Timeout) ->
    do_nslookup(Name, Class, Type, [{nameservers,NSs}], Timeout).

do_nslookup(Name, Class, Type, Opts, Timeout) ->
    case resolve(Name, Class, Type, Opts, Timeout) of
	{error,{qfmterror,_}} -> {error,einval};
	{error,{Reason,_}} -> {error,Reason};
	Result -> Result
    end.    

%% --------------------------------------------------------------------------
%% options record
%%
-record(options, { % These must be sorted!
	  alt_nameservers,edns,inet6,nameservers,recurse,
	  retry,timeout,udp_payload_size,usevc,
	  verbose}). % this is a local option, not in inet_db
%%
%% Opts when is_list(Opts) -> #options{}
make_options(Opts0) ->
    Opts = [if is_atom(Opt) ->
		    case atom_to_list(Opt) of
			"no"++X -> {list_to_atom(X),false};
			_ -> {Opt,true}
		    end;
	       true -> Opt
	    end || Opt <- Opts0],
    %% If the caller gives the nameservers option, the inet_db
    %% alt_nameservers option should be regarded as empty, i.e
    %% use only the nameservers the caller supplies.
    SortedOpts = 
	lists:ukeysort(1,
		      case lists:keymember(nameservers, 1, Opts) of
			  true ->
			      case lists:keymember(alt_nameservers, 1, Opts) of
				  false ->
				      [{alt_nameservers,[]}|Opts];
				  true ->
				      Opts
			      end;
			  false ->
			      Opts
		      end),
    SortedNames = record_info(fields, options),
    inet_db:res_update_conf(),
    list_to_tuple([options|make_options(SortedOpts, SortedNames)]).

make_options([_|_]=Opts0, []=Names0) ->
    erlang:error(badarg, [Opts0,Names0]);
make_options([], []) -> [];
make_options([{verbose,Val}|Opts]=Opts0, [verbose|Names]=Names0) ->
    if is_boolean(Val) ->
	    [Val|make_options(Opts, Names)];
       true ->
	    erlang:error(badarg, [Opts0,Names0])
    end;
make_options([{Opt,Val}|Opts]=Opts0, [Opt|Names]=Names0) ->
    case inet_db:res_check_option(Opt, Val) of
	true ->
	    [Val|make_options(Opts, Names)];
	false ->
	    erlang:error(badarg, [Opts0,Names0])
    end;
make_options(Opts, [verbose|Names]) ->
    [false|make_options(Opts, Names)];
make_options(Opts, [Name|Names]) ->
    [inet_db:res_option(Name)|make_options(Opts, Names)].


%% --------------------------------------------------------------------------
%%
%% gethostbyaddr(ip_address()) => {ok, hostent()} | {error, Reason}
%%
%% where ip_address() is {A,B,C,D} ipv4 address
%%                    |  {A,B,C,D,E,F,G,H}  ipv6 address
%%                    | string versions of the above
%%                    | atom version
%%
%% --------------------------------------------------------------------------

-spec gethostbyaddr(Address) -> {ok, Hostent} | {error, Reason} when
      Address :: inet:ip_address(),
      Hostent :: inet:hostent(),
      Reason :: inet:posix() | res_error().

gethostbyaddr(IP) -> gethostbyaddr_tm(IP,false).

-spec gethostbyaddr(Address, Timeout) -> {ok, Hostent} | {error, Reason} when
      Address :: inet:ip_address(),
      Timeout :: timeout(),
      Hostent :: inet:hostent(),
      Reason :: inet:posix() | res_error().

gethostbyaddr(IP,Timeout) ->
    Timer = inet:start_timer(Timeout),
    Res = gethostbyaddr_tm(IP,Timer),
    _ = inet:stop_timer(Timer),
    Res.    

gethostbyaddr_tm({A,B,C,D} = IP, Timer) when ?ip(A,B,C,D) ->
    inet_db:res_update_conf(),
    case inet_db:gethostbyaddr(IP) of
	{ok, HEnt} -> {ok, HEnt};
	_ -> res_gethostbyaddr(dn_in_addr_arpa(A,B,C,D), IP, Timer)
    end;
%% ipv4  only ipv6 address
gethostbyaddr_tm({0,0,0,0,0,16#ffff,G,H},Timer) when is_integer(G+H) ->
    gethostbyaddr_tm({G div 256, G rem 256, H div 256, H rem 256},Timer);
gethostbyaddr_tm({A,B,C,D,E,F,G,H} = IP, Timer) when ?ip6(A,B,C,D,E,F,G,H) ->
    inet_db:res_update_conf(),
    case inet_db:gethostbyaddr(IP) of
	{ok, HEnt} -> {ok, HEnt};
	_ -> res_gethostbyaddr(dn_ip6_int(A,B,C,D,E,F,G,H), IP, Timer)
    end;
gethostbyaddr_tm(Addr,Timer) when is_list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, IP} -> gethostbyaddr_tm(IP,Timer);
	_Error -> {error, formerr}
    end;
gethostbyaddr_tm(Addr,Timer) when is_atom(Addr) ->
    gethostbyaddr_tm(atom_to_list(Addr),Timer);
gethostbyaddr_tm(_,_) -> {error, formerr}.

%%
%%  Send the gethostbyaddr query to:
%%  1. the list of normal names servers
%%  2. the list of alternative name servers
%%
res_gethostbyaddr(Addr, IP, Timer) ->
    case res_query(Addr, in, ptr, [], Timer) of
	{ok, Rec} ->
	    inet_db:res_gethostbyaddr(IP, Rec);
	{error,{qfmterror,_}} -> {error,einval};
	{error,{Reason,_}} -> {error,Reason};
	Error ->
	    Error
    end.

%% --------------------------------------------------------------------------
%%
%% gethostbyname(domain_name()[,family [,Timer]) 
%%      => {ok, hostent()} | {error, Reason}
%%
%% where domain_name() is domain string or atom
%%
%% Caches the answer.
%% --------------------------------------------------------------------------

-spec gethostbyname(Name) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Hostent :: inet:hostent(),
      Reason :: inet:posix() | res_error().

gethostbyname(Name) ->
    case inet_db:res_option(inet6) of
	true ->
	    gethostbyname_tm(Name, inet6, false);
	false ->
	    gethostbyname_tm(Name, inet, false)
    end.

-spec gethostbyname(Name, Family) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Hostent :: inet:hostent(),
      Family :: inet:address_family(),
      Reason :: inet:posix() | res_error().

gethostbyname(Name,Family) ->
    gethostbyname_tm(Name,Family,false).

-spec gethostbyname(Name, Family, Timeout) ->
                           {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Hostent :: inet:hostent(),
      Timeout :: timeout(),
      Family :: inet:address_family(),
      Reason :: inet:posix() | res_error().

gethostbyname(Name,Family,Timeout) ->
    Timer = inet:start_timer(Timeout),    
    Res = gethostbyname_tm(Name,Family,Timer),
    _ = inet:stop_timer(Timer),
    Res.
    
gethostbyname_tm(Name,inet,Timer) ->
    getbyname_tm(Name,?S_A,Timer);
gethostbyname_tm(Name,inet6,Timer) ->
    case getbyname_tm(Name,?S_AAAA,Timer) of
	{ok,HEnt} -> {ok,HEnt};
	{error,nxdomain} ->
	    case getbyname_tm(Name, ?S_A,Timer) of
		{ok, HEnt} ->
		    %% rewrite to a ipv4 only ipv6 address
		    {ok,
		     HEnt#hostent {
		       h_addrtype = inet6,
		       h_length = 16,
		       h_addr_list = 
		       lists:map(
			 fun({A,B,C,D}) ->
				 {0,0,0,0,0,16#ffff,A*256+B,C*256+D}
			 end, HEnt#hostent.h_addr_list)
		      }};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end;
gethostbyname_tm(_Name, _Family, _Timer) ->
    {error, einval}.
	    
%% --------------------------------------------------------------------------
%%
%% getbyname(domain_name(), Type) => {ok, hostent()} | {error, Reason}
%%
%% where domain_name() is domain string and Type is ?S_A, ?S_MX ...
%%
%% Caches the answer.
%% --------------------------------------------------------------------------

-spec getbyname(Name, Type) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Type :: rr_type(),
      Hostent :: inet:hostent(),
      Reason :: inet:posix() | res_error().

getbyname(Name, Type) -> 
    getbyname_tm(Name,Type,false).

-spec getbyname(Name, Type, Timeout) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Type :: rr_type(),
      Timeout :: timeout(),
      Hostent :: inet:hostent(),
      Reason :: inet:posix() | res_error().

getbyname(Name, Type, Timeout) ->
    Timer = inet:start_timer(Timeout),
    Res = getbyname_tm(Name, Type, Timer),
    _ = inet:stop_timer(Timer),
    Res.

getbyname_tm(Name, Type, Timer) when is_list(Name) ->
    case type_p(Type) of
	true ->
	    case inet_parse:visible_string(Name) of
		false -> {error, formerr};
		true ->
		    inet_db:res_update_conf(),
		    case inet_db:getbyname(Name, Type) of
			{ok, HEnt} -> {ok, HEnt};
			_ -> res_getbyname(Name, Type, Timer)
		    end
	    end;
	false ->
	    {error, formerr}
    end;
getbyname_tm(Name,Type,Timer) when is_atom(Name) ->
    getbyname_tm(atom_to_list(Name), Type,Timer);
getbyname_tm(_, _, _) -> {error, formerr}.

type_p(Type) ->
    lists:member(Type, [?S_A, ?S_AAAA, ?S_MX, ?S_NS,
		        ?S_MD, ?S_MF, ?S_CNAME, ?S_SOA,
		        ?S_MB, ?S_MG, ?S_MR, ?S_NULL,
		        ?S_WKS, ?S_HINFO, ?S_TXT, ?S_SRV, ?S_NAPTR, ?S_SPF,
		        ?S_UINFO, ?S_UID, ?S_GID]).



%% This function and inet_db:getbyname/2 must look up names
%% in the same manner, but not from the same places.
%%
%% Assuming search path, i.e return value from inet_db:get_searchlist()
%% to be ["dom1", "dom2"]:
%%
%% Old behaviour (not this code but the previous version):
%% * For Name = "foo"
%%       Name = "foo."      try "foo.dom1", "foo.dom2" at normal nameservers
%% * For Name = "foo.bar"
%%       Name = "foo.bar."  try "foo.bar" at normal then alt. nameservers
%%                          then try "foo.bar.dom1", "foo.bar.dom2"
%%                                   at normal nameservers
%%
%% New behaviour (this code), honoring the old behaviour but
%% doing better for absolute names:
%% * For Name = "foo"       try "foo.dom1", "foo.dom2" at normal nameservers
%% * For Name = "foo.bar"   try "foo.bar" at normal then alt. nameservers
%%                          then try "foo.bar.dom1", "foo.bar.dom2"
%%                                   at normal nameservers
%% * For Name = "foo."      try "foo" at normal then alt. nameservers
%% * For Name = "foo.bar."  try "foo.bar" at normal then alt. nameservers
%%
%%
%% FIXME This is probably how it should be done:
%% Common behaviour (Solaris resolver) is:
%% * For Name = "foo."      try "foo"
%% * For Name = "foo.bar."  try "foo.bar"
%% * For Name = "foo"       try "foo.dom1", "foo.dom2", "foo"
%% * For Name = "foo.bar"   try "foo.bar.dom1", "foo.bar.dom2", "foo.bar"
%% That is to try Name as it is as a last resort if it is not absolute.
%%
res_getbyname(Name, Type, Timer) ->
    {EmbeddedDots, TrailingDot} = inet_parse:dots(Name),
    Dot = if TrailingDot -> ""; true -> "." end,
    if  TrailingDot ->
	    res_getby_query(Name, Type, Timer);
	EmbeddedDots =:= 0 ->
	    res_getby_search(Name, Dot,
			     inet_db:get_searchlist(),
			     nxdomain, Type, Timer);
	true ->
	    case res_getby_query(Name, Type, Timer) of
		{error,_Reason}=Error ->
		    res_getby_search(Name, Dot,
				     inet_db:get_searchlist(),
				     Error, Type, Timer);
		Other -> Other
	    end
    end.

res_getby_search(Name, Dot, [Dom | Ds], _Reason, Type, Timer) ->
    case res_getby_query(Name++Dot++Dom, Type, Timer,
			 inet_db:res_option(nameservers)) of
	{ok, HEnt}         -> {ok, HEnt};
	{error, NewReason} ->
	    res_getby_search(Name, Dot, Ds, NewReason, Type, Timer)
    end;
res_getby_search(_Name, _, [], Reason,_,_) ->
    {error, Reason}.

res_getby_query(Name, Type, Timer) ->
    case res_query(Name, in, Type, [], Timer) of
	{ok, Rec} ->
	    inet_db:res_hostent_by_domain(Name, Type, Rec);
	{error,{qfmterror,_}} -> {error,einval};
	{error,{Reason,_}} -> {error,Reason};
	Error -> Error
    end.

res_getby_query(Name, Type, Timer, NSs) ->
    case res_query(Name, in, Type, [], Timer, NSs) of
	{ok, Rec} ->
	    inet_db:res_hostent_by_domain(Name, Type, Rec);
	{error,{qfmterror,_}} -> {error,einval};
	{error,{Reason,_}} -> {error,Reason};
	Error -> Error
    end.



%% --------------------------------------------------------------------------
%% query record
%%
-record(q, {options,edns,dns}).



%% Query first nameservers list then alt_nameservers list
res_query(Name, Class, Type, Opts, Timer) ->
    #q{options=#options{nameservers=NSs}}=Q = 
	make_query(Name, Class, Type, Opts),
    case do_query(Q, NSs, Timer) of
	{error,nxdomain}=Error ->
	    res_query_alt(Q, Error, Timer);
	{error,{nxdomain,_}}=Error ->
	    res_query_alt(Q, Error, Timer);
	{ok,#dns_rec{anlist=[]}}=Reply ->
	    res_query_alt(Q, Reply, Timer);
	Reply -> Reply
    end.

%% Query just the argument nameservers list
res_query(Name, Class, Type, Opts, Timer, NSs) ->
    Q = make_query(Name, Class, Type, Opts),
    do_query(Q, NSs, Timer).

res_query_alt(#q{options=#options{alt_nameservers=NSs}}=Q, Reply, Timer) ->
    case NSs of
	[] -> Reply;
	_ ->
	    do_query(Q, NSs, Timer)
    end.

make_query(Dname, Class, Type, Opts) ->
    Options = make_options(Opts),
    case Options#options.edns of
	false ->
	    #q{options=Options,
	       edns=undefined,
	       dns=make_query(Dname, Class, Type, Options, false)};
	Edns ->
	    #q{options=Options,
	       edns=make_query(Dname, Class, Type, Options, Edns),
	       dns=fun () ->
			   make_query(Dname, Class, Type, Options, false)
		   end}
    end.

%% XXX smarter would be to always construct both queries,
%% but make the EDNS query point into the DNS query binary.
%% It is only the header ARList length that need to be changed,
%% and the OPT record appended.
make_query(Dname, Class, Type, Options, Edns) ->
    Id = inet_db:res_option(next_id),
    Recurse = Options#options.recurse,
    ARList = case Edns of
		 false -> [];
		 _ ->
		     PSz = Options#options.udp_payload_size,
		     [#dns_rr_opt{udp_payload_size=PSz,
				  version=Edns}]
	     end,
    Msg = #dns_rec{header=#dns_header{id=Id, 
				      opcode='query',
				      rd=Recurse,
				      rcode=?NOERROR},
		    qdlist=[#dns_query{domain=Dname, 
				       type=Type, 
				       class=Class}],
		   arlist=ARList},
    ?verbose(Options#options.verbose, "Query: ~p~n", [dns_msg(Msg)]),
    Buffer = inet_dns:encode(Msg),
    {Id, Buffer}.

%% --------------------------------------------------------------------------
%% socket helpers
%%
-record(sock, {inet=undefined, inet6=undefined}).

udp_open(#sock{inet6=I}=S, {A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    case I of
	undefined ->
	    case gen_udp:open(0, [{active,false},binary,inet6]) of
		{ok,J} ->
		    {ok,S#sock{inet6=J}};
		Error ->
		    Error
	    end;
	_ ->
	    {ok,S}
    end;
udp_open(#sock{inet=I}=S, {A,B,C,D}) when ?ip(A,B,C,D) ->
    case I of
	undefined ->
	    case gen_udp:open(0, [{active,false},binary,inet]) of
		{ok,J} ->
		    {ok,S#sock{inet=J}};
		Error ->
		    Error
	    end;
	_ ->
	    {ok,S}
    end.

udp_connect(#sock{inet6=I}, {A,B,C,D,E,F,G,H}=IP, Port)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    gen_udp:connect(I, IP, Port);
udp_connect(#sock{inet=I}, {A,B,C,D}=IP, Port)
  when ?ip(A,B,C,D) ->
    gen_udp:connect(I, IP, Port).

udp_send(#sock{inet6=I}, {A,B,C,D,E,F,G,H}=IP, Port, Buffer)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    gen_udp:send(I, IP, Port, Buffer);
udp_send(#sock{inet=I}, {A,B,C,D}=IP, Port, Buffer)
  when ?ip(A,B,C,D), ?port(Port) ->
    gen_udp:send(I, IP, Port, Buffer).

udp_recv(#sock{inet6=I}, {A,B,C,D,E,F,G,H}=IP, Port, Timeout, Decode)
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port) ->
    do_udp_recv(I, IP, Port, Timeout, Decode, time_now(), Timeout);
udp_recv(#sock{inet=I}, {A,B,C,D}=IP, Port, Timeout, Decode)
  when ?ip(A,B,C,D), ?port(Port) ->
    do_udp_recv(I, IP, Port, Timeout, Decode, time_now(), Timeout).

do_udp_recv(_I, _IP, _Port, 0, _Decode, _Start, _T) ->
    timeout;
do_udp_recv(I, IP, Port, Timeout, Decode, Start, T) ->
    case gen_udp:recv(I, 0, T) of
	{ok,Reply} ->
	    case Decode(Reply) of
		false when T =:= 0 ->
		    %% This is a compromize between the hard way i.e
		    %% in the clause below if NewT becomes 0 bailout
		    %% immediately and risk that the right reply lies
		    %% ahead after some bad id replies, and the
		    %% forgiving way i.e go on with Timeout 0 until
		    %% the right reply comes or no reply (timeout)
		    %% which opens for a DOS attack by a malicious
		    %% DNS server flooding with bad id replies causing
		    %% an infinite loop here.
		    %%
		    %% Timeout is used as a sanity limit counter
		    %% just to put an end to the loop.
		    NewTimeout = erlang:max(0, Timeout - 50),
		    do_udp_recv(I, IP, Port, NewTimeout, Decode, Start, T);
		false ->
		    Now = time_now(),
		    NewT = erlang:max(0, Timeout - now_ms(Now, Start)),
		    do_udp_recv(I, IP, Port, Timeout, Decode, Start, NewT);
		Result ->
		    Result
	    end;
	Error -> Error
    end.

udp_close(#sock{inet=I,inet6=I6}) ->
    if I =/= undefined -> gen_udp:close(I); true -> ok end,
    if I6 =/= undefined -> gen_udp:close(I6); true -> ok end,
    ok.

%%
%% Send a query to the nameserver and return a reply
%% We first use socket server then we add the udp version
%%
%% Algorithm: (from manual page for dig)
%%  for i = 0 to retry - 1
%%     for j = 1 to num_servers
%%           send_query
%%           wait((time * (2**i)) / num_servers)
%%     end
%%  end
%%
%% But that man page also says dig always use num_servers = 1.
%%
%% Our man page says: timeout/retry, then double for next retry, i.e
%%  for i = 0 to retry - 1
%%     foreach nameserver
%%        send query
%%        wait((time * (2**i)) / retry)
%%     end
%%  end
%%
%% And that is what the code seems to do, now fixed, hopefully...

do_query(_Q, [], _Timer) ->
    {error,nxdomain};
do_query(#q{options=#options{retry=Retry}}=Q, NSs, Timer) ->
    query_retries(Q, NSs, Timer, Retry, 0, #sock{}).

query_retries(_Q, _NSs, _Timer, Retry, Retry, S) ->
    udp_close(S),
    {error,timeout};
query_retries(_Q, [], _Timer, _Retry, _I, S) ->
    udp_close(S),
    {error,timeout};
query_retries(Q, NSs, Timer, Retry, I, S0) ->
    case query_nss(Q, NSs, Timer, Retry, I, S0, []) of
	{S,{noanswer,ErrNSs}} -> %% remove unreachable nameservers
	    query_retries(Q, NSs--ErrNSs, Timer, Retry, I+1, S);
	{S,Result} ->
	    udp_close(S),
	    Result
    end.

query_nss(_Q, [], _Timer, _Retry, _I, S, ErrNSs) ->
    {S,{noanswer,ErrNSs}};
query_nss(#q{edns=undefined}=Q, NSs, Timer, Retry, I, S, ErrNSs) ->
    query_nss_dns(Q, NSs, Timer, Retry, I, S, ErrNSs);
query_nss(Q, NSs, Timer, Retry, I, S, ErrNSs) ->
    query_nss_edns(Q, NSs, Timer, Retry, I, S, ErrNSs).

query_nss_edns(
  #q{options=#options{udp_payload_size=PSz}=Options,edns={Id,Buffer}}=Q,
  [{IP,Port}=NS|NSs]=NSs0, Timer, Retry, I, S0, ErrNSs) ->
    {S,Res}=Reply =
	query_ns(S0, Id, Buffer, IP, Port, Timer, Retry, I, Options, PSz),
    case Res of
	timeout -> {S,{error,timeout}}; % Bailout timeout
	{ok,_} -> Reply;
	{error,{nxdomain,_}} -> Reply;
	{error,{E,_}} when E =:= qfmterror; E =:= notimp; E =:= servfail;
			   E =:= badvers ->
	    query_nss_dns(Q, NSs0, Timer, Retry, I, S, ErrNSs);
	{error,E} when E =:= fmt; E =:= enetunreach; E =:= econnrefused ->
	    query_nss(Q, NSs, Timer, Retry, I, S, [NS|ErrNSs]);
	_Error ->
	    query_nss(Q, NSs, Timer, Retry, I, S, ErrNSs)
    end.

query_nss_dns(
  #q{dns=Qdns}=Q0,
  [{IP,Port}=NS|NSs], Timer, Retry, I, S0, ErrNSs) ->
    #q{options=Options,dns={Id,Buffer}}=Q =
	if
	    is_function(Qdns, 0) -> Q0#q{dns=Qdns()};
	    true -> Q0
	end,
    {S,Res}=Reply =
	query_ns(
	  S0, Id, Buffer, IP, Port, Timer, Retry, I, Options, ?PACKETSZ),
    case Res of
	timeout -> {S,{error,timeout}}; % Bailout timeout
	{ok,_} -> Reply;
	{error,{E,_}} when E =:= nxdomain; E =:= qfmterror -> Reply;
	{error,E} when E =:= fmt; E =:= enetunreach; E =:= econnrefused ->
	    query_nss(Q, NSs, Timer, Retry, I, S, [NS|ErrNSs]);
	_Error ->
	    query_nss(Q, NSs, Timer, Retry, I, S, ErrNSs)
    end.

query_ns(S0, Id, Buffer, IP, Port, Timer, Retry, I,
	 #options{timeout=Tm,usevc=UseVC,verbose=Verbose},
	 PSz) ->
    case UseVC orelse iolist_size(Buffer) > PSz of
	true ->
	    TcpTimeout = inet:timeout(Tm*5, Timer),
	    {S0,query_tcp(TcpTimeout, Id, Buffer, IP, Port, Verbose)};
	false ->
	    case udp_open(S0, IP) of
		{ok,S} ->
		    Timeout =
			inet:timeout( (Tm * (1 bsl I)) div Retry, Timer),
		    {S,
		     case query_udp(
			    S, Id, Buffer, IP, Port, Timeout, Verbose) of
			 {ok,#dns_rec{header=H}} when H#dns_header.tc ->
			     TcpTimeout = inet:timeout(Tm*5, Timer),
			     query_tcp(
			       TcpTimeout, Id, Buffer, IP, Port, Verbose);
			 Reply -> Reply
		     end};
		Error ->
		    {S0,Error}
	    end
    end.

query_udp(_S, _Id, _Buffer, _IP, _Port, 0, _Verbose) ->
    timeout;
query_udp(S, Id, Buffer, IP, Port, Timeout, Verbose) ->
    ?verbose(Verbose, "Try UDP server : ~p:~p (timeout=~w)\n",
	     [IP,Port,Timeout]),
    case
	case udp_connect(S, IP, Port) of
	    ok ->
		udp_send(S, IP, Port, Buffer);
	    E1 ->
		E1 end of
	ok ->
	    Decode =
		fun ({RecIP,RecPort,Answer})
		      when RecIP =:= IP, RecPort =:= Port ->
			case decode_answer(Answer, Id, Verbose) of
			    {error,badid} ->
				false;
			    Reply ->
				Reply
			end;
		    ({_,_,_}) ->
			false
		end,
	    case udp_recv(S, IP, Port, Timeout, Decode) of
		{ok,_}=Result ->
		    Result;
		E2 ->
		    ?verbose(Verbose, "UDP server error: ~p\n", [E2]),
		    E2
	    end;
	E3 ->
	    ?verbose(Verbose, "UDP send failed: ~p\n", [E3]),
	    {error,econnrefused}
    end.

query_tcp(0, _Id, _Buffer, _IP, _Port, _Verbose) ->
    timeout;
query_tcp(Timeout, Id, Buffer, IP, Port, Verbose) ->
    ?verbose(Verbose, "Try TCP server : ~p:~p (timeout=~w)\n",
	     [IP, Port, Timeout]),
    Family = case IP of
		 {A,B,C,D} when ?ip(A,B,C,D) -> inet;
		 {A,B,C,D,E,F,G,H} when ?ip6(A,B,C,D,E,F,G,H) -> inet6
	     end,
    try gen_tcp:connect(IP, Port, 
			[{active,false},{packet,2},binary,Family], 
			Timeout) of
	{ok, S} ->
	    case gen_tcp:send(S, Buffer) of
		ok ->
		    case gen_tcp:recv(S, 0, Timeout) of
			{ok, Answer} ->
			    gen_tcp:close(S),
			    case decode_answer(Answer, Id, Verbose) of
				{ok, _} = OK -> OK;
				{error, badid} -> {error, servfail};
				Error -> Error
			    end;
			Error ->
			    gen_tcp:close(S),
			    ?verbose(Verbose, "TCP server recv error: ~p\n",
				     [Error]),
			    Error
		    end;
		Error ->
		    gen_tcp:close(S),
		    ?verbose(Verbose, "TCP server send error: ~p\n",
			     [Error]),
		    Error
	    end;
	Error ->
	    ?verbose(Verbose, "TCP server error: ~p\n", [Error]),
	    Error
    catch
	_:_ -> {error, einval}
    end.

decode_answer(Answer, Id, Verbose) ->
    case inet_dns:decode(Answer) of
	{ok, Msg} ->
	    ?verbose(Verbose, "Got reply: ~p~n", [dns_msg(Msg)]),
	    E = case lists:keyfind(dns_rr_opt, 1, Msg#dns_rec.arlist) of
		    false -> 0;
		    #dns_rr_opt{ext_rcode=ExtRCode} -> ExtRCode
		end,
	    H = Msg#dns_rec.header,
	    RCode = (E bsl 4) bor H#dns_header.rcode,
	    case RCode of
		?NOERROR ->
		    if H#dns_header.id =/= Id ->
			    {error,badid};
		       length(Msg#dns_rec.qdlist) =/= 1 ->
			    {error,{noquery,Msg}};
		       true ->
			    {ok, Msg}
		    end;
		?FORMERR  -> {error,{qfmterror,Msg}};
		?SERVFAIL -> {error,{servfail,Msg}};
		?NXDOMAIN -> {error,{nxdomain,Msg}};
		?NOTIMP   -> {error,{notimp,Msg}};
		?REFUSED  -> {error,{refused,Msg}};
		?BADVERS  -> {error,{badvers,Msg}};
		_ -> {error,{unknown,Msg}}
	    end;
	Error ->
	    ?verbose(Verbose, "Got reply: ~p~n", [Error]),
	    Error
    end.

%%
%% Transform domain name or address
%% 1.  "a.b.c"    => 
%%       "a.b.c"
%% 2.  "1.2.3.4"  =>  
%%       "4.3.2.1.IN-ADDR.ARPA"
%% 3.  "4321:0:1:2:3:4:567:89ab" =>
%%      "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.1.2.3.4.IP6.ARPA"
%% 4.  {1,2,3,4} => as 2.
%% 5.  {1,2,3,4,5,6,7,8} => as 3.
%%
nsdname({A,B,C,D}) -> 
    {ok, dn_in_addr_arpa(A,B,C,D)};
nsdname({A,B,C,D,E,F,G,H}) -> 
    {ok, dn_ip6_int(A,B,C,D,E,F,G,H)};
nsdname(Name) when is_list(Name) ->
    case inet_parse:visible_string(Name) of
	true ->
	    case inet_parse:address(Name) of
		{ok, Addr} -> 
		    nsdname(Addr);
		_ ->
		    {ok, Name}
	    end;
	_ -> {error, formerr}
    end;
nsdname(Name) when is_atom(Name) ->
    nsdname(atom_to_list(Name));
nsdname(_) -> {error, formerr}.

dn_in_addr_arpa(A,B,C,D) ->
    integer_to_list(D) ++
	("." ++	integer_to_list(C) ++ 
	 ("." ++ integer_to_list(B) ++
	  ("." ++ integer_to_list(A) ++ ".IN-ADDR.ARPA"))).

dn_ip6_int(A,B,C,D,E,F,G,H) ->
    dnib(H) ++ 
	(dnib(G) ++ 
	 (dnib(F) ++ 
	  (dnib(E) ++ 
	   (dnib(D) ++ 
	    (dnib(C) ++ 
	     (dnib(B) ++ 
	      (dnib(A) ++ "IP6.ARPA"))))))).



-compile({inline, [dnib/1, dnib/3]}).
dnib(X) ->
    L = erlang:integer_to_list(X, 16),
    dnib(4-length(L), L, []).
%%
dnib(0, [], Acc) -> Acc;
dnib(0, [C|Cs], Acc) ->
    dnib(0, Cs, [C,$.|Acc]);
dnib(N, Cs, Acc) ->
    dnib(N-1, Cs, [$0,$.|Acc]).



dns_msg([]) -> [];
dns_msg([{Field,Msg}|Fields]) ->
    [{Field,dns_msg(Msg)}|dns_msg(Fields)];
dns_msg([Msg|Msgs]) ->
    [dns_msg(Msg)|dns_msg(Msgs)];
dns_msg(Msg) ->
    case inet_dns:record_type(Msg) of
	undefined -> Msg;
	Type ->
	    Fields = inet_dns:Type(Msg),
	    {Type,dns_msg(Fields)}
    end.

-compile({inline, [now_ms/2]}).
now_ms(Int1, Int0) ->
    Int1 - Int0.

-compile({inline, [time_now/0]}).
time_now() ->
	erlang:monotonic_time(1000).
