%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
%% RFC 1035, 2782, 2915, 6891.
%%
-module(inet_res).

-moduledoc """
A rudimentary DNS client.

This module performs DNS name resolving towards recursive name servers.

See also [ERTS User's Guide: Inet Configuration](`e:erts:inet_cfg.md`)
or more information about how to configure an Erlang runtime system for IP
communication, and how to enable this DNS client by defining `'dns'`
as a lookup method. The DNS client then acts as a backend for
the resolving functions in `m:inet`.

This DNS client can resolve DNS records even if it is not used
for normal name resolving in the node.

This is not a full-fledged resolver, only a DNS client that relies on asking
trusted recursive name servers.

## Name Resolving

UDP queries are used unless resolver option `usevc` is `true`,
which forces TCP queries.  If the query is too large for UDP,
TCP is used instead. For regular DNS queries, 512 bytes is the size limit.

When EDNS is enabled (resolver option `edns` is set to the EDNS version
(that is; `0` instead of `false`), resolver option `udp_payload_size`
sets the payload size limit.  If a name server replies with the TC bit set
(truncation), indicating that the answer is incomplete, the query is retried
towards the same name server using TCP.  Resolver option `udp_payload_size`
also sets the advertised size for the maximum allowed reply size,
if EDNS is enabled, otherwise the name server uses the limit 512 bytes.
If the reply is larger, it gets truncated, forcing a TCP requery.

For UDP queries, resolver options `timeout` and `retry` control
retransmission.  Each name server in the `nameservers` list is tried
with a time-out of `timeout`/`retry`. Then all name servers are tried again,
doubling the time-out, for a total of `retry` times.

[](){: #servfail_retry_timeout }

But before all name servers are tried again, there is a (user configurable)
time-out, `servfail_retry_timeout`. The point of this is to prevent
the new query to be handled by a server's servfail cache (a client
that is too eager will actually only get what is in the servfail cache).
If there is too little time left of the resolver call's time-out
to do a retry, the resolver call may return before the call's time-out
has expired.

For queries not using the `search` list, if the query to all `nameservers`
results in `{error,nxdomain}` or an empty answer, the same query is tried for
`alt_nameservers`.

## Resolver Types

The following data types concern the resolver:

## DNS Types

The following data types concern the DNS client:

## Example

This access functions example shows how `lookup/3` can be implemented using
`resolve/3` from outside the module:

```erlang
example_lookup(Name, Class, Type) ->
    case inet_res:resolve(Name, Class, Type) of
        {ok,Msg} ->
            [inet_dns:rr(RR, data)
             || RR <- inet_dns:msg(Msg, anlist),
                 inet_dns:rr(RR, type) =:= Type,
                 inet_dns:rr(RR, class) =:= Class];
        {error,_} ->
            []
     end.
```
""".

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
              nameserver/0,
              hostent/0]).

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
      | {dnssec_ok, boolean()}
      | {usevc, boolean()}
      | {nxdomain_reply, boolean()}.

-type nameserver() :: {inet:ip_address(), Port :: 1..65535}.

-type res_error() :: formerr | qfmterror | servfail | nxdomain |
                     notimp | refused | badvers | timeout.

-doc "A string with no adjacent dots.".
-type dns_name() :: string().

-type dns_rr_type() :: a | aaaa | caa | cname | gid | hinfo | ns | mb | md | mg
                 | mf | minfo | mx | naptr | null | ptr | soa | spf | srv
                 | txt | uid | uinfo | unspec | uri | wks.

-type dns_class() :: in | chaos | hs | any.

-doc """
A DNS message.

This is the start of a hierarchy of opaque data structures that can be
examined with access functions in `inet_dns`, which return lists of
`{Field,Value}` tuples. The arity 2 functions return the value
for a specified field.

```erlang
dns_msg() = DnsMsg
    inet_dns:msg(DnsMsg) ->
        [ {header, dns_header()}
        | {qdlist, dns_query()}
        | {anlist, dns_rr()}
        | {nslist, dns_rr()}
        | {arlist, dns_rr()} ]
    inet_dns:msg(DnsMsg, header) -> dns_header() % for example
    inet_dns:msg(DnsMsg, Field) -> Value

dns_header() = DnsHeader
    inet_dns:header(DnsHeader) ->
        [ {id, integer()}
        | {qr, boolean()}
        | {opcode, query | iquery | status | integer()}
        | {aa, boolean()}
        | {tc, boolean()}
        | {rd, boolean()}
        | {ra, boolean()}
        | {pr, boolean()}
        | {rcode, integer(0..16)} ]
    inet_dns:header(DnsHeader, Field) -> Value

query_type() = axfr | mailb | maila | any | dns_rr_type()

dns_query() = DnsQuery
    inet_dns:dns_query(DnsQuery) ->
        [ {domain, dns_name()}
        | {type, query_type()}
        | {class, dns_class()} ]
    inet_dns:dns_query(DnsQuery, Field) -> Value

dns_rr() = DnsRr
    inet_dns:rr(DnsRr) -> DnsRrFields | DnsRrOptFields
    DnsRrFields = [ {domain, dns_name()}
                  | {type, dns_rr_type()}
                  | {class, dns_class()}
                  | {ttl, integer()}
                  | {data, dns_data()} ]
    DnsRrOptFields = [ {domain, dns_name()}
                     | {type, opt}
                     | {udp_payload_size, integer()}
                     | {ext_rcode, integer()}
                     | {version, integer()}
                     | {z, integer()}
                     | {data, dns_data()} ]
    inet_dns:rr(DnsRr, Field) -> Value
```

There is an information function for the types above:

```erlang
inet_dns:record_type(dns_msg()) -> msg;
inet_dns:record_type(dns_header()) -> header;
inet_dns:record_type(dns_query()) -> dns_query;
inet_dns:record_type(dns_rr()) -> rr;
inet_dns:record_type(_) -> undefined.
```

So, `inet_dns:(inet_dns:record_type(X))(X)` converts any of these data
structures into a `{Field,Value}` list.
""".
-type dns_msg() :: term().

-doc """
DNS record data (content)

The basic type of each data element is specified in this type.

`Regexp` is a UTF-8 `t:string/0`.  The other `t:string/0`s
are actually Latin-1 strings.
""".
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

-doc(#{equiv => resolve(Name, Class, Type, [], infinity)}).
-spec resolve(Name, Class, Type) -> {ok, dns_msg()} | Error when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
      Error :: {error, Reason} | {error,{Reason,dns_msg()}},
      Reason :: inet:posix() | res_error().

resolve(Name, Class, Type) ->
    resolve(Name, Class, Type, [], infinity).

-doc(#{equiv => resolve(Name, Class, Type, Opts, infinity)}).
-spec resolve(Name, Class, Type, Opts) ->
                     {ok, dns_msg()} | Error when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
      Opts :: [Opt],
      Opt :: res_option() | verbose | atom(),
      Error :: {error, Reason} | {error,{Reason,dns_msg()}},
      Reason :: inet:posix() | res_error().

resolve(Name, Class, Type, Opts) ->
    resolve(Name, Class, Type, Opts, infinity).

-doc """
Resolve a DNS query.

Resolves a DNS query for the specified `Type`, `Class`, and  `Name`,
into a DNS message possibly containing Resource Records.
The returned `t:dns_msg/0` can be examined using access functions
in `inet_db`, as described in section in [DNS Types](#module-dns-types).

If `Name` is an `ip_address()`, the domain name to query about is generated
as the standard reverse `".IN-ADDR.ARPA."` name for an IPv4 address, or the
`".IP6.ARPA."` name for an IPv6 address.  In this case, you most probably
want to use `Class = in` and `Type = ptr`, but it is not done automatically.

`Opts` overrides the corresponding resolver options. If option `nameservers`
is specified, it is assumed that it is the complete list of name serves,
so resolver option `alt_nameserves` is ignored. However, if option
`alt_nameserves` is also specified to this function, it is used.

Option `verbose` (or rather `{verbose,true}`) causes diagnostics printout
through [`io:format/2`](`io:format/3`) of queries, replies, retransmissions,
and so on, similar to utilities such as `dig` and `nslookup`.

Option `nxdomain_reply` (or rather `{nxdomain_reply, true}`) causes NXDOMAIN
errors from DNS servers to be returned as `{error, {nxdomain, dns_msg()}}`.
`t:dns_msg/0` contains the additional sections that where included by the
answering server. This is mainly useful to inspect the SOA record
to get the TTL for negative caching.

If `Opt` is any atom, it is interpreted as `{Opt,true}` unless
the atom string starts with `"no"`, making the interpretation `{Opt,false}`.
For example, `usevc` is an alias for `{usevc, true}` and `nousevc`
is an alias for `{usevc, false}`.

Option `inet6` has no effect on this function. You probably want to use
`Type = a | aaaa` instead.
""".
-spec resolve(Name, Class, Type, Opts, Timeout) ->
                     {ok, dns_msg()} | Error when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
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
	{error, _} = Error ->
	    Error
    end.

%% --------------------------------------------------------------------------
%% lookup:
%%
%% Convenience wrapper to resolve/3,4,5 that filters out all answer data
%% fields of the class and type asked for.

-doc(#{equiv => lookup(Name, Class, Type, [], infinity)}).
-spec lookup(Name, Class, Type) -> [dns_data()] when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type().

lookup(Name, Class, Type) ->
    lookup(Name, Class, Type, []).

-doc(#{equiv => lookup(Name, Class, Type, Opts, infinity)}).
-spec lookup(Name, Class, Type, Opts) -> [dns_data()] when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
      Opts :: [res_option() | verbose].

lookup(Name, Class, Type, Opts) ->
    lookup(Name, Class, Type, Opts, infinity).

-doc """
Look up DNS data.

Resolves the DNS data for the record `Name` of the specified
`Type` and `Class`. On success, filters out the answer records
with the correct `Class` and `Type`, and returns a list of their data fields.
So, a lookup for type `any` gives an empty answer, as the answer records
have specific types that are not `any`. An empty answer or a failed lookup
returns an empty list.

Calls [`resolve/*`](`resolve/3`) with the same arguments and filters the result,
so `Opts` is described for those functions.
""".
-spec lookup(Name, Class, Type, Opts, Timeout) -> [dns_data()] when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
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

-doc(#{equiv => nslookup(Name, Class, Type, infinity)}).
-doc(#{group => <<"Legacy Functions">>}).
-spec nslookup(Name, Class, Type) -> {ok, dns_msg()} | {error, Reason} when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
      Reason :: inet:posix() | res_error().

nslookup(Name, Class, Type) ->
    do_nslookup(Name, Class, Type, [], infinity).

-doc """
Resolve a DNS query.

This function is a legacy wrapper to `resolve/5` that simplifies
errors matching `{error, {Reason, _}}` into `{error, Reason}`
or `{error, einval}`.

With argument `Timeout` calls `resolve/5` with `Opts = []`.

With argument `Nameservers` calls `resolve/5` with
`Opts = [{nameservers, Nameservers}]` and `Timeout = infinity`.
""".
-doc(#{group => <<"Legacy Functions">>}).
-spec nslookup(Name, Class, Type, Timeout) ->
                      {ok, dns_msg()} | {error, Reason} when
                  Name :: dns_name() | inet:ip_address(),
                  Class :: dns_class(),
                  Type :: dns_rr_type(),
                  Timeout :: timeout(),
                  Reason :: inet:posix() | res_error();
              (Name, Class, Type, Nameservers) ->
                      {ok, dns_msg()} | {error, Reason} when
                  Name :: dns_name() | inet:ip_address(),
                  Class :: dns_class(),
                  Type :: dns_rr_type(),
                  Nameservers :: [nameserver()],
                  Reason :: inet:posix() | res_error().

nslookup(Name, Class, Type, Timeout)
  when is_integer(Timeout), Timeout >= 0;
       Timeout =:= infinity ->
    do_nslookup(Name, Class, Type, [], Timeout);
nslookup(Name, Class, Type, NSs) ->             % For backwards compatibility
    nnslookup(Name, Class, Type, NSs).          % with OTP R6B only

-doc(#{equiv => nnslookup(Name, Class, Type, NSs, infinity)}).
-doc(#{group => <<"Legacy Functions">>}).
-spec nnslookup(Name, Class, Type, Nameservers) ->
                      {ok, dns_msg()} | {error, Reason} when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
      Nameservers :: [nameserver()],
      Reason :: inet:posix().

nnslookup(Name, Class, Type, NSs) ->
    nnslookup(Name, Class, Type, NSs, infinity).

-doc """
Resolve a DNS query.

Like `nslookup/4` but calls `resolve/5` with both the arguments
`Opts = [{nameservers, Nameservers}]` and `Timeout`.
""".
-doc(#{group => <<"Legacy Functions">>}).
-spec nnslookup(Name, Class, Type, Nameservers, Timeout) ->
                      {ok, dns_msg()} | {error, Reason} when
      Name :: dns_name() | inet:ip_address(),
      Class :: dns_class(),
      Type :: dns_rr_type(),
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
	  alt_nameservers,dnssec_ok,edns,inet6,nameservers,
          nxdomain_reply, % this is a local option, not in inet_db
          recurse,retry,servfail_retry_timeout,timeout,
          udp_payload_size,usevc,
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
make_options([{Opt,Val}|Opts]=Opts0, [Opt|Names]=Names0)
  when Opt =:= nxdomain_reply;
       Opt =:= verbose ->
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
%% Handling default values (for options not in Opts)
make_options(Opts, [nxdomain_reply|Names]) ->
    [false|make_options(Opts, Names)];
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

-doc(#{equiv => gethostbyaddr(Address, infinity)}).
-spec gethostbyaddr(Address) -> {ok, Hostent} | {error, Reason} when
      Address :: inet:ip_address(),
      Hostent :: inet:hostent(),
      Reason :: inet:posix() | res_error().

gethostbyaddr(IP) -> gethostbyaddr_tm(IP,false).

-doc "Backend function used by `inet:gethostbyaddr/1`.".
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


-doc false.
gethostbyaddr_tm(Addr, Timer) when is_atom(Addr) ->
    gethostbyaddr_tm(atom_to_list(Addr), Timer);
gethostbyaddr_tm(Addr, Timer) when is_list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, IP} -> gethostbyaddr_tm(IP, Timer);
	_Error -> {error, formerr}
    end;
gethostbyaddr_tm(IP, Timer) ->
    %% The call to norm_ip/1 here translates a lookup of
    %% ::ffff:A.B.C.D (AAAA in ...ip6.arpa) into a plain
    %% A.B.C.D (A in ...in-addr.arpa) lookup, and pretends
    %% the result as if it was from the original IPv6 lookup
    %%
    case dn_ip(norm_ip(IP)) of
        {error, _} = Error ->
            Error;
        {ok, Name} ->
            %% Try cached first
            inet_db:res_update_conf(),
            case inet_db:gethostbyaddr(Name, IP) of
                {ok, _HEnt} = Result ->
                    Result;
                {error, nxdomain} ->
                    %% Do a resolver lookup
                    case res_query(Name, in, ?S_PTR, [], Timer) of
                        {ok, Rec} ->
                            %% Process and cache DNS Record
                            inet_db:res_gethostbyaddr(Name, IP, Rec);
                        {error,{qfmterror,_}} ->
                            {error,einval};
                        {error,{Reason,_}} ->
                            {error,Reason};
                        Error ->
                            Error
                    end
            end
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

-doc """
Backend functions used by [`inet:gethostbyname/1,2`](`inet:gethostbyname/1`).

If resolver option `inet6` is `true`, equivalent to
[`gethostbyname(Name, inet6, infinity)`](`gethostbyname/3`),
otherwise [`gethostbyname(Name, inet, infinity)`](`gethostbyname/3`).
""".
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

-doc(#{equiv => gethostbyname(Name, Family, infinity)}).
-spec gethostbyname(Name, Family) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Hostent :: inet:hostent(),
      Family :: inet:address_family(),
      Reason :: inet:posix() | res_error().

gethostbyname(Name,Family) ->
    gethostbyname_tm(Name,Family,false).

-doc """
Backend functions used by [`inet:gethostbyname/1,2`](`inet:gethostbyname/1`).

This function uses resolver option `search` just like
[`getbyname/2,3`](`getbyname/2`).
""".
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

-doc false.
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

%% Duplicate of inet.hrl: #hostent{}, but with DNS RR types in h_addrtype
%% and dns_data() in h_addr_list.
-doc """
Extended variant of `t:inet:hostent/0`.

Allows `t:dns_rr_type/0` for the
[`#hostent{}.h_addrtype`](`t:inet:hostent/0`) field, and
`[`[`dns_data/0`](`t:dns_data/0`)`]` for the
[`#hostent{}.h_addr_list`](`t:inet:hostent/0`) field.
""".
-type hostent() ::
        inet:hostent() |
        {'hostent',
         H_name      :: inet:hostname(),
         H_aliases   :: [inet:hostname()],
         H_addrtype  :: dns_rr_type(),
         H_length    :: non_neg_integer(),
         H_addr_list :: [dns_data()]}.

-doc(#{equiv => getbyname(Name, Type, infinity)}).
-spec getbyname(Name, Type) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Type :: dns_rr_type(),
      Hostent :: inet:hostent() | hostent(),
      Reason :: inet:posix() | res_error().

getbyname(Name, Type) ->
    getbyname_tm(Name,Type,false).

-doc """
Resolve a DNS query.

Resolves a DNS query of the specified `Type` for the specified host,
of class`in`.  Returns, on success, when resolving a `Type = a|aaaa`
DNS record, a `#hostent{}` record with `#hostent.h_addrtype = inet|inet6`,
respectively; see `t:inet:hostent/0`.

When resolving other `Type = dns_rr_type()`:s (of class `in`), also returns
a `#hostent{}` record but with `t:dns_rr_type/0` in `#hostent.h_addrtype`,
and the resolved `t:dns_data/0` in `#hostent.h_addr_list`; see `t:hostent/0`.

This function uses resolver option `search` that is a list of domain names.
If the name to resolve contains no dots, it is prepended to each domain
name in the search list, and they are tried in order.  If the name
contains dots, it is first tried as an absolute name and if that fails,
the search list is used. If the name has a trailing dot, it is supposed
to be an absolute name and the search list is not used.
""".
-spec getbyname(Name, Type, Timeout) -> {ok, Hostent} | {error, Reason} when
      Name :: dns_name(),
      Type :: dns_rr_type(),
      Timeout :: timeout(),
      Hostent :: inet:hostent() | hostent(),
      Reason :: inet:posix() | res_error().

getbyname(Name, Type, Timeout) ->
    Timer = inet:start_timer(Timeout),
    Res = getbyname_tm(Name, Type, Timer),
    _ = inet:stop_timer(Timer),
    Res.

-doc false.
getbyname_tm(Name, Type, Timer) when is_list(Name) ->
    case type_p(Type) of
	true ->
	    case inet_parse:visible_string(Name) of
		false ->
                    {error, formerr};
		true ->
                    %% Try cached first
		    inet_db:res_update_conf(),
		    case inet_db:getbyname(Name, Type) of
			{ok, HEnt} ->
                            {ok, HEnt};
			_ ->
                            %% Do a resolver lookup
                            res_getbyname(Name, Type, Timer)
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
    if
        TrailingDot ->
	    res_getby_query(lists:droplast(Name), Type, Timer);
	EmbeddedDots =:= 0 ->
	    res_getby_search(Name, inet_db:get_searchlist(),
			     nxdomain, Type, Timer);
	true ->
	    case res_getby_query(Name, Type, Timer) of
		{error,_Reason}=Error ->
		    res_getby_search(Name, inet_db:get_searchlist(),
				     Error, Type, Timer);
		Other -> Other
	    end
    end.

res_getby_search(Name, [Dom | Ds], _Reason, Type, Timer) ->
    QueryName =
        %% Join Name and Dom with a single dot.
        %% Allow Dom to be "." or "", but not to lead with ".".
        if
            Dom =:= "."; Dom =:= "" ->
                Name;
            Name =/= "", hd(Dom) =/= $. ->
                Name ++ "." ++ Dom;
            Name =:= "", hd(Dom) =/= $. ->
                Dom;
            true ->
                erlang:error({if_clause, Name, Dom})
        end,
    case res_getby_query(QueryName, Type, Timer,
			 inet_db:res_option(nameservers)) of
	{ok, HEnt}         -> {ok, HEnt};
	{error, NewReason} ->
	    res_getby_search(Name, Ds, NewReason, Type, Timer)
    end;
res_getby_search(_Name, [], Reason,_,_) ->
    {error, Reason}.

res_getby_query(Name, Type, Timer) ->
    case res_query(Name, in, Type, [], Timer) of
	{ok, Rec} ->
            %% Process and cache DNS Record
	    inet_db:res_hostent_by_domain(Name, Type, Rec);
	{error,{qfmterror,_}} -> {error,einval};
	{error,{Reason,_}} -> {error,Reason};
	Error -> Error
    end.

res_getby_query(Name, Type, Timer, NSs) ->
    case res_query(Name, in, Type, [], Timer, NSs) of
	{ok, Rec} ->
            %% Process and cache DNS Record
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
    RD = Recurse =:= 1 orelse Recurse =:= true, % (0 | 1 | true | false)
    ARList = case Edns of
		 false -> [];
		 _ ->
                     #options{
                        udp_payload_size = PSz,
                        dnssec_ok = DnssecOk } = Options,
		     [#dns_rr_opt{udp_payload_size=PSz,
				  version=Edns,
                                  do=DnssecOk}]
	     end,
    Msg = #dns_rec{header=#dns_header{id=Id,
                                      qr=false,
				      opcode='query',
				      rd=RD,
				      rcode=?NOERROR},
                   qdlist=[#dns_query{domain=Dname,
                                      type=Type,
                                      class=Class}],
		   arlist=ARList},
    ?verbose(Options#options.verbose, "Query: ~p~n", [dns_msg(Msg)]),
    Buffer = inet_dns:encode(Msg, false),
    {Msg, Buffer}.

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
  when ?ip6(A,B,C,D,E,F,G,H), ?port(Port), 0 =< Timeout ->
    do_udp_recv(I, IP, Port, Timeout, Decode, time(Timeout), Timeout);
udp_recv(#sock{inet=I}, {A,B,C,D}=IP, Port, Timeout, Decode)
  when ?ip(A,B,C,D), ?port(Port), 0 =< Timeout ->
    do_udp_recv(I, IP, Port, Timeout, Decode, time(Timeout), Timeout).

do_udp_recv(_I, _IP, _Port, 0, _Decode, _Time, PollCnt)
  when PollCnt =< 0 ->
    timeout;
do_udp_recv(I, IP, Port, Timeout, Decode, Time, PollCnt) ->
    case gen_udp:recv(I, 0, Timeout) of
	{ok,Reply} ->
	    case Decode(Reply) of
		false when Timeout =:= 0 ->
		    %% This is a compromise between the hard way i.e
		    %% in the clause below if Timeout becomes 0 bailout
		    %% immediately and risk that the right reply lies
		    %% ahead after some bad id replies, and the
		    %% forgiving way i.e go on with Timeout 0 until
		    %% the right reply comes or no reply (time-out)
		    %% which opens for a DOS attack by a malicious
		    %% DNS server flooding with bad id replies causing
		    %% an infinite loop here.
                    %%
		    do_udp_recv(
                      I, IP, Port, Timeout, Decode, Time, PollCnt-50);
		false ->
		    do_udp_recv(
                      I, IP, Port, timeout(Time), Decode, Time, PollCnt);
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
%% Our man page says: time-out/retry, then double for next retry, i.e
%%  for i = 0 to retry - 1
%%     foreach nameserver
%%        send query
%%        wait((time * (2**i)) / retry)
%%     end
%%  end
%%
%% And that is what the code seems to do, now fixed, hopefully...

do_query(_Q, [], _Timer) ->
    %% We have no name server to ask, so say nxdomain
    {error,nxdomain};
do_query(#q{options=#options{retry=Retry}}=Q, NSs, Timer) ->
    %% We have at least one name server,
    %% so a failure will be a time-out,
    %% unless a name server says otherwise
    Reason = timeout,
    %% Verify that the nameservers list contains only 2-tuples
    %% to protect our internal servfail_retry mechanism from surprises
    lists:all(
      fun (NS) when tuple_size(NS) =:= 2 -> true;
          (_) -> false
      end, NSs) orelse
        erlang:error(badarg, [Q,NSs,Timer]),
    query_retries(Q, NSs, Timer, Retry, 0, #sock{}, Reason).

%% Loop until out of retries or name servers
%%
query_retries(Q, _NSs, _Timer, Retry, I, S, Reason) when Retry =:= I ->
    query_retries_error(Q, S, Reason);
query_retries(Q, [], _Timer, _Retry, _I, S, Reason) ->
    query_retries_error(Q, S, Reason);
query_retries(Q, NSs, Timer, Retry, I, S, Reason) ->
    query_nss(Q, NSs, Timer, Retry, I, S, Reason, []).

%% For each name server:
%%     If EDNS is enabled, try that first,
%%     and for selected failures fall back to plain DNS.
%%
query_nss(Q, [], Timer, Retry, I, S, Reason, RetryNSs) ->
    %% End of name servers list, do a new retry
    %% with the remaining name servers
    query_retries(Q, lists:reverse(RetryNSs), Timer, Retry, I+1, S, Reason);
query_nss(#q{edns = undefined}=Q, NSs, Timer, Retry, I, S, Reason, RetryNSs) ->
    query_nss_dns(Q, NSs, Timer, Retry, I, S, Reason, RetryNSs);
query_nss(Q, NSs, Timer, Retry, I, S, Reason, RetryNSs) ->
    query_nss_edns(Q, NSs, Timer, Retry, I, S, Reason, RetryNSs).

query_nss_edns(
  #q{options =
         #options{
            udp_payload_size = PSz}=Options,
     edns = EDNSQuery}=Q,
  [NsSpec|NSs], Timer, Retry, I, S_0, Reason, RetryNSs) ->
    %%
    {IP,Port} = NS = servfail_retry_wait(NsSpec),
    {S,Result} =
	query_ns(
          S_0, EDNSQuery, IP, Port, Timer, Retry, I, Options, PSz),
    case Result of
	{error,{E,_}}
          when E =:= qfmterror;
               E =:= notimp;
               E =:= servfail;
               E =:= badvers ->
            %% The server did not like that.
            %% Ignore that error and try plain DNS.
            %%
            %% We ignore the servfail_retry_timeout here,
            %% assuming that if the servfail was due to us using EDNS,
            %% a DNS query might work, therefore we do not
            %% count this failure as a try.
	    query_nss_dns(
              Q, [NS|NSs], Timer, Retry, I, S, Reason, RetryNSs);
        _ ->
	    query_nss_result(
              Q, NSs, Timer, Retry, I, S, Reason, RetryNSs, NS, Result)
    end.

query_nss_dns(
  #q{dns = DNSQuery_0}=Q_0,
  [NsSpec|NSs], Timer, Retry, I, S_0, Reason, RetryNSs) ->
    %%
    {IP,Port} = NS = servfail_retry_wait(NsSpec),
    #q{options = Options,
       dns = DNSQuery}=Q =
	if
	    is_function(DNSQuery_0, 0) ->
                Q_0#q{dns=DNSQuery_0()};
	    true ->
                Q_0
	end,
    {S,Result} =
	query_ns(
	  S_0, DNSQuery, IP, Port, Timer, Retry, I, Options, ?PACKETSZ),
    query_nss_result(
      Q, NSs, Timer, Retry, I, S, Reason, RetryNSs, NS, Result).


%% servfail_retry NsSpec handling.
%%
%% A NsSpec is either a NS = {IP, Port},
%% or for a servfail_retry_timeout, the nameserver wrapped
%% in a tuple with the earliest time to contact
%% the nameserver again.
%%
%% When unwrapping; wait until it is time before returning
%% the nameserver.

%% Wrap with retry time
servfail_retry_time(RetryTimeout, NS) ->
    {servfail_retry, time(RetryTimeout), NS}.

%% Unwrap and wait
servfail_retry_wait(NsSpec) ->
    case NsSpec of
        {servfail_retry, Time, NS} ->
            wait(timeout(Time)),
            NS;
        {_IP,_Port} = NS->
            NS
    end.


query_nss_result(Q, NSs, Timer, Retry, I, S, Reason, RetryNSs, NS, Result) ->
    case Result of
	{ok,_} ->
            _ = udp_close(S),
            Result;
	timeout -> % Out of total time time-out
            query_retries_error(Q, S, Reason); % The best reason we have
	{error,{nxdomain,_} = E} ->
            query_retries_error(Q, S, E); % Definite answer
	{error,{E,_}=NewReason}
          when E =:= qfmterror;
               E =:= notimp;
               E =:= refused;
               E =:= badvers;
               E =:= unknown ->
            %% The server did not like that.
            %% Do not retry this server since
            %% it will not answer differently on the next retry.
	    query_nss(Q, NSs, Timer, Retry, I, S, NewReason, RetryNSs);
	{error,E=NewReason}
          when E =:= formerr;
               E =:= enetunreach;
               E =:= econnrefused ->
            %% Could not decode answer, or network problem.
            %% Do not retry this server.
	    query_nss(Q, NSs, Timer, Retry, I, S, NewReason, RetryNSs);
	{error,timeout} -> % Query time-out
            %% Try next server, may retry this server
	    query_nss(Q, NSs, Timer, Retry, I, S, Reason, [NS|RetryNSs]);
        {error,{servfail,_}=NewReason} ->
            RetryTimeout = Q#q.options#options.servfail_retry_timeout,
            case inet:timeout(RetryTimeout, Timer) of
                RetryTimeout ->
                    NsSpec = servfail_retry_time(RetryTimeout, NS),
                    query_nss(
                      Q, NSs, Timer, Retry, I, S, NewReason,
                      [NsSpec|RetryNSs]);
                _ ->
                    %% No time for a new retry with this server
                    %% - do not retry this server
                    query_nss(
                      Q, NSs, Timer, Retry, I, S, NewReason, RetryNSs)
            end;
	{error,NewReason} ->
            %% NewReason =
            %%     {error,badid} |
            %%     {error,{noquery,Msg}} |
            %%     {error,OtherSocketError}
            %% Try next server, may retry this server
	    query_nss(Q, NSs, Timer, Retry, I, S, NewReason, [NS|RetryNSs])
    end.

query_retries_error(#q{options=#options{nxdomain_reply=NxReply}}, S, Reason) ->
    _ = udp_close(S),
    case Reason of
        {nxdomain, _} when not NxReply ->
            {error, nxdomain};
        _ ->
            {error, Reason}
    end.


query_ns(S0, {Msg, Buffer}, IP, Port, Timer, Retry, I,
	 #options{timeout=Tm,usevc=UseVC,verbose=Verbose},
	 PSz) ->
    case UseVC orelse iolist_size(Buffer) > PSz of
	true ->
	    TcpTimeout = inet:timeout(Tm*5, Timer),
	    {S0,
             query_tcp(TcpTimeout, Msg, Buffer, IP, Port, Verbose)};
	false ->
	    case udp_open(S0, IP) of
		{ok,S} ->
		    UdpTimeout =
			inet:timeout( (Tm * (1 bsl I)) div Retry, Timer),
		     case
                         query_udp(
                           S, Msg, Buffer, IP, Port, UdpTimeout, Verbose)
                     of
			 {ok,#dns_rec{header=H}} when H#dns_header.tc ->
			     TcpTimeout = inet:timeout(Tm*5, Timer),
			     {S,
                              query_tcp(
                                TcpTimeout, Msg, Buffer, IP, Port, Verbose)};
			{error, econnrefused} = Err ->
                            ok = udp_close(S),
	                    {#sock{}, Err};
			Reply -> {S, Reply}
		     end;
		Error ->
		    {S0,Error}
	    end
    end.

query_udp(_S, _Msg, _Buffer, _IP, _Port, 0, _Verbose) ->
    timeout;
query_udp(S, Msg, Buffer, IP, Port, Timeout, Verbose) ->
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
			case decode_answer(Answer, Msg, Verbose) of
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

query_tcp(0, _Msg, _Buffer, _IP, _Port, _Verbose) ->
    timeout;
query_tcp(Timeout, Msg, Buffer, IP, Port, Verbose) ->
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
			    case decode_answer(Answer, Msg, Verbose) of
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

decode_answer(Answer, Q_Msg, Verbose) ->
    case inet_dns:decode(Answer, false) of
	{ok, #dns_rec{header = H, arlist = ARList} = Msg} ->
	    ?verbose(Verbose, "Got reply: ~p~n", [dns_msg(Msg)]),
	    T = case lists:keyfind(dns_rr_tsig, 1, ARList) of
		    false -> false;
		    #dns_rr_tsig{error=?NOERROR} -> false;
		    #dns_rr_tsig{error=TsigError} -> TsigError
		end,
	    E = case lists:keyfind(dns_rr_opt, 1, ARList) of
		    false -> 0;
		    #dns_rr_opt{ext_rcode=ExtRCode} -> ExtRCode
		end,
	    RCode = T orelse (E bsl 4) bor H#dns_header.rcode,
	    case RCode of
		?NOERROR  -> decode_answer_noerror(Q_Msg, Msg, H);
		?FORMERR  -> {error,{qfmterror,Msg}};
		?SERVFAIL -> {error,{servfail,Msg}};
		?NXDOMAIN -> {error,{nxdomain,Msg}};
		?NOTIMP   -> {error,{notimp,Msg}};
		?REFUSED  -> {error,{refused,Msg}};
		?YXDOMAIN -> {error,{yxdomain,Msg}};
		?YXRRSET  -> {error,{yxrrset,Msg}};
		?NXRRSET  -> {error,{nxrrset,Msg}};
		?NOTAUTH  -> {error,{noauth,Msg}};
		?NOTZONE  -> {error,{nozone,Msg}};
		?BADVERS when not T -> {error,{badvers,Msg}};
		?BADSIG   -> {error,{badsig,Msg}};
		?BADKEY   -> {error,{badkey,Msg}};
		?BADTIME  -> {error,{badtime,Msg}};
		?BADTRUNC -> {error,{badtrunc,Msg}};
		_         -> {error,{unknown,Msg}}
	    end;
	{error, formerr} = Error ->
	    ?verbose(Verbose, "Got reply: decode format error~n", []),
	    Error
    end.

decode_answer_noerror(
  #dns_rec{header = Q_H, qdlist = [Q_RR]},
  #dns_rec{qdlist = QDList} = Msg,
  H) ->
    %% Validate the reply
    if
        H#dns_header.id     =/= Q_H#dns_header.id ->
            {error,badid};
        H#dns_header.qr     =/= true;
        H#dns_header.opcode =/= Q_H#dns_header.opcode;
        H#dns_header.rd andalso not Q_H#dns_header.rd ->
            {error,{unknown,Msg}};
        true ->
            case QDList of
                [RR] ->
                    case
                        (RR#dns_query.class =:= Q_RR#dns_query.class)
                        andalso
                        (RR#dns_query.type =:= Q_RR#dns_query.type)
                        andalso
                        inet_db:eq_domains(
                          RR#dns_query.domain, Q_RR#dns_query.domain)
                    of
                        true ->
                            {ok, Msg};
                        false ->
                            {error,{noquery,Msg}}
                    end;
                _ when is_list(QDList) ->
                    {error,{noquery,Msg}}
            end
    end.

%%
%% Transform domain name or address
%% 1.  "a.b.c"    =>
%%       "a.b.c"
%% 2.  "1.2.3.4"  =>
%%       "4.3.2.1.in-addr.arpa"
%% 3.  "4321:0:1:2:3:4:567:89ab" =>
%%      "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.1.2.3.4.ip6.arpa"
%% 4.  {1,2,3,4} => as 2.
%% 5.  {1,2,3,4,5,6,7,8} => as 3.
%% 6.  Atom -> Recurse(String(Atom))
%% 7.  Term => {error, formerr}
%%
nsdname(Name) when is_atom(Name) ->
    nsdname(atom_to_list(Name));
nsdname(Name) when is_list(Name) ->
    case inet_parse:visible_string(Name) of
	true ->
	    case inet_parse:address(Name) of
		{ok, IP} ->
                    dn_ip(IP);
		_ ->
		    {ok, Name}
	    end;
	_ -> {error, formerr}
    end;
nsdname(IP) ->
    dn_ip(IP).

%% Return the domain name for a PTR lookup of
%% the argument IP address
%%
dn_ip({A,B,C,D}) when ?ip(A,B,C,D) ->
    dn_ipv4([A,B,C,D], "in-addr.arpa");
dn_ip({A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    dn_ipv6([A,B,C,D,E,F,G,H], "ip6.arpa");
dn_ip(_) ->
    {error, formerr}.

dn_ipv4([], Dn) ->
    {ok, Dn};
dn_ipv4([A | As], Dn_0) when is_integer(A), A =< 255 ->
    Dn = [$. | Dn_0],
    if
        A < 10 ->
            dn_ipv4(As, dn_dec(A, Dn));
        A < 100 ->
            dn_ipv4(As, dn_dec(A div 10, dn_dec(A rem 10, Dn)));
        true ->
            B = A rem 100,
            dn_ipv4(
              As,
              dn_dec(A div 100, dn_dec(B div 10, dn_dec(B rem 10, Dn))))
    end.

dn_ipv6([], Dn) ->
    {ok, Dn};
dn_ipv6([W | Ws], Dn) when is_integer(W), W =< 16#ffff ->
    D = W band 16#f,   W_1 = W bsr 4,
    C = W_1 band 16#f, W_2 = W_1 bsr 4,
    B = W_2 band 16#f,
    A = W_2 bsr 4,
    dn_ipv6(Ws, dn_hex(D, dn_hex(C, dn_hex(B, dn_hex(A, Dn))))).

-compile({inline, [dn_dec/2, dn_hex/2]}).
dn_dec(N, Tail) when is_integer(N) ->
    [N + $0 | Tail].

dn_hex(N, Tail) when is_integer(N) ->
    if
        N < 10 ->
            [N + $0, $. | Tail];
        true ->
            [(N - 10) + $a, $. | Tail]
end.

%% Normalize an IPv4-compatible IPv6 address
%% into a plain IPv4 address
%%
norm_ip({0,0,0,0,0,16#ffff,G,H}) ->
    A = G bsr 8, B = G band 16#ff, C = H bsr 8, D = H band 16#ff,
    {A,B,C,D};
norm_ip(IP) ->
    IP.



-doc false.
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



-compile({inline, [time/1, timeout/1, wait/1]}).

%% What Time is the Timeout? [ms]
%%
time(Timeout) ->
    erlang:monotonic_time(1000) + Timeout.

%% How long Timeout to Time? [ms] >= 0
%%
timeout(Time) ->
    TimeNow = erlang:monotonic_time(1000),
    if
        TimeNow < Time ->
            Time - TimeNow;
        true ->
            0
    end.

%% receive after Timeout but do not yield for 0
%%
wait(0) ->
    ok;
wait(Timeout) ->
    receive
    after Timeout ->
            ok
    end.
