%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2021. All Rights Reserved.
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

-export_type([res_option/0,
              res_error/0,
              nameserver/0]).

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

-type rr_type() :: a | aaaa | caa | cname | gid | hinfo | ns | mb | md | mg
                 | mf | minfo | mx | naptr | null | ptr | soa | spf | srv
                 | txt | uid | uinfo | unspec | uri | wks.

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
	  retry,servfail_retry_timeout,timeout,udp_payload_size,usevc,
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
    getbyname_tm(Name,?S_AAAA,Timer);
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
		        ?S_UINFO, ?S_UID, ?S_GID, ?S_URI, ?S_CAA]).



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
%% request algorithm (parallel)
%%
%% Start a Reaper that all Queries (processes) link to.
%% The Reaper monitors this Caller process, and if it
%% should die the Reaper exits and brings down all Queries.
%%
%% The Queries are started with a StartDelay between each other
%% so nameservers earlier in the NSs list gets a head start.
%%
%% At the first definite answer the Caller closes all
%% Queries still in progress, and the Reaper,
%% then returns the answer.
%%
%% While there is no answer or just 'timeout' answers,
%% the Caller awaits a better answer and keeps spawning
%% new Queries until the nameservers list (NSs) is empty.
%%
%% This Caller finishes When all Queries are done
%% and the nameservers list is empty.
%%

-record(state, {timer, reaper, reply_tag, start_delay}).

do_query(#q{}, [], _Timer) ->
    %% We have no name server to ask, pretend nxdomain
    {error,nxdomain};
do_query(
  #q{options = #options{retry = Retry, timeout = Tm}} = Q, NSs, Timer)
  when is_integer(Retry), 1 < Retry, is_integer(Tm), 1 < Tm ->
    %%
    Caller = self(),
    Reaper =
        spawn(
          fun () ->
                  process_flag(trap_exit, true),
                  CallerMref = monitor(process, Caller),
                  receive
                      {'DOWN', CallerMref, _, _, _} ->
                          exit(caller_down)
                end
          end),
    %%
    StartDelay =
        %% Half of first UDP timeout, rounded division
        ((Tm div Retry) + 1) div 2,
    ReplyTag = make_ref(),
    Queries = #{}, %% #{Mref => Pid}
    State =
        #state{
           timer = Timer, reaper = Reaper,
           reply_tag = ReplyTag, start_delay = StartDelay},
    R = timeout, % Latest known Reason
    do_queries(Q, NSs, Queries, State, R).

%% Loop top: spawn a new Query process
%%
do_queries(
  Q, [NS|NSs], Queries,
  #state{reaper = Reaper, reply_tag = ReplyTag} = State, R) ->
    %%
    {Pid, Mref} =
        spawn_monitor(
          fun () ->
                  true = link(Reaper),
                  Result = query_ns(Q, NS),
                  %% Return the Result in a tagged exit reason
                  exit({ReplyTag, Result})
          end),
    %%
    StartDelay =
        case NSs of
            [] ->
                infinity;
            _ ->
                State#state.start_delay
        end,
    do_queries_recv(
      Q, NSs, Queries#{Mref => Pid},
      State, R, time(StartDelay), StartDelay).

%% Wait for result from any Query process,
%% for the call timer timeout, or for time to spawn a new Query process
%%
do_queries_recv(
  Q, NSs, Queries,
  #state{timer = Timer, reply_tag = ReplyTag} = State, R,
  StartTime, StartDelay) ->
    receive
        {'DOWN', Mref, process, _, Reason}
          when is_map_key(Mref, Queries) ->
            Queries_1 = maps:remove(Mref, Queries),
            case Reason of
                {ReplyTag, Result} ->
                    %% We have a Result
                    do_queries_result(
                      Q, NSs, Queries_1, State, R,
                      StartTime, Result);
                _ ->
                    %% Internal error in query_ns/3
                    %% - exit like the Query process did
                    do_queries_done(Queries_1, State),
                    exit(Reason)
            end;
        {timeout, Timer, _} = Msg when Timer =/= false ->
            %% Call timer timeout - wrap it up
            do_queries_done(Queries, State),
            self() ! Msg, % The Caller would expect this message
            {error, R}
    after StartDelay ->
            %% Spawn a new Query process
            do_queries(Q, NSs, Queries, State, R)
    end.

%% Check Result to see if we are done,
%% should wait for a better result,
%% or should spawn a new Query process
%%
do_queries_result(Q, NSs, Queries, State, R, StartTime, Result) ->
    case Result of
        {ok, _} = Result ->
            %% Definite result
            do_queries_done(Queries, State),
            Result;
        {error, {nxdomain = E, _}} ->
            %% Definite result
            do_queries_done(Queries, State),
            {error, E};
        {error, E} ->
            R_1 =
                case E of
                    timeout -> R; % Anything beats 'timeout'
                    _       -> E
                end,
            case NSs of
                [] ->
                    %% No more nameserver to ask
                    case map_size(Queries) of
                        0 ->
                            %% No queries in progress
                            %% - wrap it up
                            do_queries_done(Queries, State),
                            {error, R_1};
                        _ ->
                            %% Still queries in progress
                            %% - wait for a better result
                            do_queries_recv(
                              Q, NSs, Queries, State, R_1,
                              StartTime, timeout(StartTime))
                    end;
                _ ->
                    %% Spawn a new request for the next nameserver
                    do_queries(Q, NSs, Queries, State, R_1)
            end
    end.

%% Kill all still running processes and wait for them to exit,
%% except no need to wait for the Reaper
%%
do_queries_done(Queries, #state{reaper = Reaper}) ->
    %% Stop all queries still in progress
    _ = maps:fold(
          fun (_Mref, Pid, _) ->
                  exit(Pid, done)
          end, [], Queries),
    _ = maps:fold(
          fun (Mref, _Pid, _) ->
                  receive {'DOWN', Mref, process, _, _} -> ok end
          end, [], Queries),
    exit(Reaper, done).


%% ----------------------------------------
%% straight sequence towards one nameserver
%%
%% For UDP: Timeout is (#options.timeout * 2**i) / #options.retry,
%%          i = 0...#options.retry-1
%% For TCP: Timeout is 5 * #options.timeout
%%
%% S contains the result of gen_udp:open() or 'undefined'.
%% TCP opens its own socket during a request, but first
%% closes an open UDP socket to save resources.
%% UDP keeps its socket for future retransmissions.

query_ns(Q, NS) ->
    I = 0,
    case Q of
        #q{edns = undefined, dns = {Id, Buffer}} ->
            S = undefined,
            query_ns_dns(Q, NS, Id, Buffer, S, I);
        #q{edns = {Id, Buffer}} ->
            query_ns_edns(Q, NS, Id, Buffer, I)
    end.

query_ns_dns(Q, NS, Id, Buffer, S, I) ->
    Options = Q#q.options,
    case
        Options#options.usevc
        orelse
        ?PACKETSZ < iolist_size(Buffer)
    of
        true ->
            gen_udp_close(S),
            query_ns_tcp(Q, NS, Id, Buffer, Options, I);
        false ->
            S_1 = gen_udp_connect(NS, S),
            {S_2, Result} = query_ns_udp(Q, NS, Id, Buffer, S_1, I, Options),
            gen_udp_close(S_2),
            Result
    end.

query_ns_edns(#q{options = Options} = Q, NS, Id, Buffer, I) ->
    S = undefined,
    case
        case
            Options#options.usevc
            orelse
            Options#options.udp_payload_size < iolist_size(Buffer)
        of
            true ->
                {S,
                 query_ns_tcp(Q, NS, Id, Buffer, Options, I)};
            false ->
                S_1 = gen_udp_connect(NS, S),
                query_ns_udp(Q, NS, Id, Buffer, S_1, I, Options)
        end
    of
        {S_2,
         {error, {E,_}}}
          when E =:= qfmterror;
               E =:= notimp;
               E =:= servfail;
               E =:= badvers ->
            %% The server did not like that,
            %% ignore that error and try plain DNS.
            %%
            %% Do not use servfail_retry_timeout since
            %% a DNS query might be accepted immediately
            {Id_1, Buffer_1} = (Q#q.dns)(),
            query_ns_dns(Q, NS, Id_1, Buffer_1, S_2, I);
        {S_2, Result} ->
            gen_udp_close(S_2),
            Result
    end.


%% Open a connected UDP socket if we do not have one
%%
gen_udp_connect({IP, Port}, S) ->
    case S of
        undefined ->
            AF = address_family(IP),
            case gen_udp:open(0, [{active,false}, binary, AF]) of
                {ok, Socket} = Result ->
                    case gen_udp:connect(Socket, IP, Port) of
                        ok ->
                            Result;
                        Error ->
                            _ = gen_udp:close(Socket),
                            Error
                    end;
                {error, _} = Result ->
                    Result
            end;
        {ok, _Socket} ->
            S
    end.

gen_udp_close(S) ->
    case S of
        undefined ->
            ok;
        {ok, Socket} ->
            _ = gen_udp:close(Socket),
            ok
    end.

address_family(IP) ->
    case IP of
        {A,B,C,D} when ?ip(A,B,C,D) ->
            inet;
        {A,B,C,D,E,F,G,H} when ?ip6(A,B,C,D,E,F,G,H) ->
            inet6
    end.


query_ns_udp(
  _Q, _NS, _Id, _Buffer, {error,_} = S, _I, _Options) ->
    %% gen_udp_connect/2 failed
    Error = S,
    S_1 = undefined,
    {S_1, Error};
query_ns_udp(
  Q, {IP, Port} = NS, Id, Buffer, {ok, Socket} = S, I,
  #options{timeout = Tm, retry = Retry, verbose = Verbose} = Options) ->
    %%
    Timeout = (Tm * (1 bsl I)) div Retry,
    ?verbose(Verbose, "Try UDP server : ~p:~p (timeout=~w)\n",
             [IP,Port,Timeout]),
    case gen_udp:send(Socket, IP, Port, Buffer) of
        ok ->
            Time = time(Timeout),
            PollCnt = 5,
            case
                query_ns_udp_recv(
                  Socket, NS, Id, Verbose, Time, Timeout, PollCnt)
            of
                {ok, #dns_rec{header = H}}
                  when H#dns_header.tc ->
                    %% The answer was truncated - switch to TCP
                    gen_udp_close(S),
                    S_1 = undefined,
                    {S_1, query_ns_tcp(Q, NS, Id, Buffer, Options, I)};
                Result ->
                    I_1 = I + 1,
                    case
                        I_1 < Retry
                        andalso
                        query_ns_should_retry(Options, Result)
                    of
                        true ->
                            query_ns_udp(
                              Q, NS, Id, Buffer, S, I_1, Options);
                        false ->
                            {S, Result}
                    end
            end;
        {error, _} = Error ->
            ?verbose(Verbose, "UDP send to ~p:~p failed: ~p\n",
                     [IP,Port,Error]),
            {S, Error}
    end.

query_ns_udp_recv(
  Socket, {IP, Port} = NS, Id, Verbose, Time, Timeout, PollCnt)
  when 0 < PollCnt ->
    case gen_udp:recv(Socket, 0, Timeout) of
        {ok, {IP, Port, Answer}} ->
            Result = decode_answer(Answer, Id),
            ?verbose(Verbose,
                     "UDP decode from ~p:~p result: ~p\n",
                     [IP,Port,Result]),
            case Result of
                {error,badid} ->
                    %% The answer was not to our question;
                    %% very strange since we are connected
                    %% - try recv again
                    query_ns_udp_recv(
                      Socket, NS, Id, Verbose,
                      Time, timeout(Time),
                      poll_cnt(PollCnt, Timeout));
                _ ->
                    Result
            end;
        {ok, {WrongIP, WrongPort, _Answer}} ->
            ?verbose(Verbose,
                     "UDP recv from ~p:~p wrong server: ~p:~p\n",
                     [IP,Port,WrongIP,WrongPort]),
            %% The answer is from the wrong address;
            %% very strange since the socket is connected
            %% - try recv again
            query_ns_udp_recv(
              Socket, NS, Id, Verbose,
              Time, timeout(Time),
              poll_cnt(PollCnt, Timeout));
        {error, _} = Error ->
            ?verbose(Verbose,
                     "UDP recv from ~p:~p error: ~p\n",
                     [IP,Port,Error]),
            Error
    end;
query_ns_udp_recv(
  _Socket, {IP, Port} =_NS, _Id, Verbose, _Time, _Timeout, _PollCnt) ->
    %% Out of PollCnt:s
    Error = {error, timeout},
    ?verbose(Verbose, "UDP recv ~p:~p bailout\n", [IP,Port]),
    Error.

-compile({inline, [poll_cnt/2]}).
%% We try some extra succesful gen_udp:recv calls when Timeout
%% is down to 0, just in case the answer awaits in the buffer
%%
poll_cnt(PollCnt, 0) -> PollCnt - 1;
poll_cnt(PollCnt, _Timeout) -> PollCnt.


query_ns_tcp(
  Q, {IP, Port} = NS, Id, Buffer,
  #options{timeout = Tm, retry = Retry, verbose = Verbose} = Options, I) ->
    %%
    Timeout = (Tm * 5) div Retry,
    Time = time(Timeout),
    ?verbose(Verbose, "Try TCP server : ~p:~p (timeout=~w)\n",
             [IP, Port, Timeout]),
    Result =
        query_ns_tcp_req(Port, IP, Id, Buffer, Verbose, Time, Timeout),
    I_1 = I + 1,
    case
        I_1 < Retry
        andalso
        query_ns_should_retry(Options, Result)
    of
        true ->
            query_ns_tcp(Q, NS, Id, Buffer, Options, I_1);
        false ->
            Result
    end.

query_ns_tcp_req(Port, IP, Id, Buffer, Verbose, Time, Timeout) ->
    AF = address_family(IP),
    try
        gen_tcp:connect(
          IP, Port, [{active,false},{packet,2},binary,AF], Timeout)
    of
	{ok, Socket} ->
	    case gen_tcp:send(Socket, Buffer) of
		ok ->
                    Recv = gen_tcp:recv(Socket, 0, timeout(Time)),
                    _ = gen_tcp:close(Socket),
		    case Recv of
			{ok, Answer} ->
			    Result = decode_answer(Answer, Id),
			    ?verbose(Verbose,
                                     "TCP recv from ~p:~p result: ~p\n",
				     [IP,Port,Result]),
                            case Result of
                                {error, badid} ->
                                    %% Bad decode or server mishap
                                    %% - pretend timeout to try again
                                    {error, timeout};
                                Result ->
                                    Result
                            end;
			RecvError ->
			    ?verbose(Verbose,
                                     "TCP recv from ~p:~p error: ~p\n",
				     [IP,Port,RecvError]),
			    RecvError
		    end;
		SendError ->
		    _ = gen_tcp:close(Socket),
		    ?verbose(Verbose, "TCP send to ~p:~p error: ~p\n",
			     [IP, Port,SendError]),
		    SendError
	    end;
	ConnectError ->
	    ?verbose(Verbose, "TCP connect to ~p:~p error: ~p\n",
                     [IP,Port,ConnectError]),
	    ConnectError
    catch
	_:_ -> {error, einval}
    end.


%% Return 'true' if we should retry and then also
%% wait for servfail_retry_timeout if we should,
%% return 'false' if the result is final
query_ns_should_retry(Options, Result) ->
    case Result of
        {ok, _} ->
            false;
        %%
        {error, {noquery,_}} ->
            %% We count this as retryable since the code before
            %% the rewrite did that, possibly by accident, though
            true;
        {error, {servfail,_}} ->
            %% Wait servfail_retry_timeout and retry
            receive
            after
                Options#options.servfail_retry_timeout ->
                    true
            end;
        {error, {E,_}} when is_atom(E) ->
            %% The rest on this form are terminal errors
            %% from decode_answer/3
            false;
        %%
        %% {error, badid} % Should not arrive here
        {error, E}
          when E =:= formerr;
               E =:= enetunreach;
               E =:= econnrefused;
               E =:= ehostunreach;
               E =:= einval ->
            %% Terminal errors
            false;
        {error, E} when is_atom(E) ->
            %% Retryable network errors and 'timeout'
            true
    end.


decode_answer(Answer, Id) ->
    case inet_dns:decode(Answer) of
	{ok, Msg} ->
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
		_ ->         {error,{unknown,Msg}}
	    end;
	{error, formerr} = Error ->
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


-compile({inline, [time/1, timeout/1]}).

%% Calculate the Time for Timeout [ms]
time(infinity = Timeout) ->
    Timeout;
time(0) ->
    zero;
time(Timeout)
  when is_integer(Timeout), 0 < Timeout ->
    erlang:monotonic_time(1000) + Timeout.

%% Return the Timeout time to Time [ms] >= 0
timeout(infinity = Time) ->
    Time;
timeout(zero) ->
    0;
timeout(Time) when is_integer(Time) ->
    TimeNow = erlang:monotonic_time(1000),
    if
        TimeNow < Time ->
            Time - TimeNow;
        true ->
            0
    end.
