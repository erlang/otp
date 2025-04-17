%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: MIT
%%
%% Copyright (c) 2010, Torbjörn Törnkvist
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% %CopyrightEnd%
%% 
-module(eldap).
-moduledoc """
LDAP Client

This module provides a client api to the Lightweight Directory Access Protocol
(LDAP).

References:

- RFC 4510 - RFC 4519
- RFC 2830

The above publications can be found at [IETF](http://www.ietf.org).

Terminology abbreviations:

- Dn. An LDAPDN is defined to be the representation of a Distinguished Name
   (DN) after encoding according to the specification in [RFC4514].

""".

-doc """
The LDAP socket and the transport protocol, TCP or TLS (SSL), used by the ldap connection
""".
-type connection_info() :: #{socket := ssl:sslsocket() | gen_tcp:socket(), socket_type := tcp | ssl}.

-doc """
A requested set of serach options for seaching the directory
""".
-type search_option() ::
    {base, string()} |
    {filter, filter()} |
    {scope, scope()} |
    {attributes, [string()]} |
    {deref, dereference()} |
    {types_only, boolean()} |
    {timeout, integer()}.

-doc "An opaque handle unique for the connection".
-type handle() :: term().

-doc "The attributes of an entry".
-type attribute() :: {Type :: string(), Value :: [string()]}.

-doc """
Entry modification operations.
See `mod_add/2`, `mod_delete/2`, `mod_replace/2`
""".
-type modify_op() :: term().

-doc """
Scope of a search.
See `baseObject/0`, `singleLevel/0`, `wholeSubtree/0`
""".
-opaque scope() :: baseObject | singleLevel | wholeSubtree.

-doc """
How to handle aliases during a search.
See `neverDerefAliases/0`, `derefInSearching/0`, `derefFindingBaseObj/0`, `derefAlways/0`
""".
-opaque dereference() :: neverDerefAliases | derefInSearching | derefFindingBaseObj | derefAlways.

-doc """
An opaque type representing a filter operation.

See the filter creation functions for more info, including,
`present/1`, `substrings/2`, `equalityMatch/2`,
`greaterOrEqual/2`, `lessOrEqual/2`,
`approxMatch/2`, `extensibleMatch/2`,
`'and'/1`, `'or'/1`, `'not'/1`
""".
-type filter() :: term().

-doc """
The LDAP Server Address.
The contents of `Address` is server dependent.
""".
-type referrals() :: [Address :: string()].

-export_type([scope/0, dereference/0]).

-moduledoc(#{since => "OTP R15B01"}).
%%% --------------------------------------------------------------------
%%% Created:  12 Oct 2000 by Tobbe <tnt@home.se>
%%% Function: Erlang client LDAP implementation according RFC 2251,2253
%%%           and 2255. The interface is based on RFC 1823, and
%%%           draft-ietf-asid-ldap-c-api-00.txt
%%%
%%% Copyright (c) 2010 Torbjorn Tornkvist
%%% Copyright Ericsson AB 2011-2013. All Rights Reserved.
%%% See MIT-LICENSE at the top dir for licensing information.
%%% --------------------------------------------------------------------
-vc('$Id$ ').
-export([open/1, open/2,
	 simple_bind/3, simple_bind/4,
	 controlling_process/2,
	 start_tls/2, start_tls/3, start_tls/4,
         modify_password/3, modify_password/4, modify_password/5,
	 getopts/2,
	 baseObject/0,singleLevel/0,wholeSubtree/0,close/1,
	 equalityMatch/2,greaterOrEqual/2,lessOrEqual/2,
	 extensibleMatch/2,
	 search/2, search/3,
	 approxMatch/2,substrings/2,present/1,
	 'and'/1,'or'/1,'not'/1,mod_add/2, mod_delete/2,
	 mod_replace/2,
	 modify/3, modify/4,
	 add/3, add/4,
	 delete/2, delete/3,
	 modify_dn/5,parse_dn/1,
	 parse_ldap_url/1,
	 paged_result_control/1,
	 paged_result_control/2,
	 paged_result_cookie/1,
         info/1]).

-export([neverDerefAliases/0, derefInSearching/0,
         derefFindingBaseObj/0, derefAlways/0]).

%% for upgrades
-export([loop/2]).

-import(lists,[concat/1]).

-include("ELDAPv3.hrl").
-include("eldap.hrl").

-define(LDAP_VERSION, 3).
-define(LDAP_PORT, 389).
-define(LDAPS_PORT, 636).

-record(eldap, {version = ?LDAP_VERSION,
		host,                % Host running LDAP server
		port = ?LDAP_PORT,   % The LDAP server port
		fd,                  % Socket filedescriptor.
		prev_fd,	     % Socket that was upgraded by start_tls
		binddn = "",         % Name of the entry to bind as
		passwd,              % Password for (above) entry
		id = 0,              % LDAP Request ID
		log,                 % User provided log function
		timeout = infinity,  % Request timeout
		anon_auth = false,   % Allow anonymous authentication
		ldaps = false,       % LDAP/LDAPS
		using_tls = false,   % true if LDAPS or START_TLS executed
		tls_opts = [],       % ssl:ssloption()
		tcp_opts = []        % inet6 support
	       }).

%%% For debug purposes
%%-define(PRINT(S, A), io:fwrite("~w(~w): " ++ S, [?MODULE,?LINE|A])).
-define(PRINT(S, A), true).

-define(elog(S, A), error_logger:info_msg("~w(~w): "++S,[?MODULE,?LINE|A])).

%%% ====================================================================
%%% Exported interface
%%% ====================================================================

%%% --------------------------------------------------------------------
%%% open(Hosts [,Opts] )
%%% --------------------
%%% Setup a connection to on of the Hosts in the argument
%%% list. Stop at the first successful connection attempt.
%%% Valid Opts are:      Where:
%%%
%%%    {port, Port}        - Port is the port number
%%%    {log, F}            - F(LogLevel, FormatString, ListOfArgs)
%%%    {timeout, milliSec} - Server request timeout
%%%
%%% --------------------------------------------------------------------
-doc """
Setup a connection to an LDAP server, the `HOST`'s are tried in order.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec open(Hosts) -> {ok, Handle} | {error, Reason} when
    Hosts :: [Host],
    Host  :: inet:socket_address() | inet:hostname(),
    Handle :: handle(),
    Reason :: term().
open(Hosts) ->
    open(Hosts, []).

-doc """
Setup a connection to an LDAP server, the `HOST`'s are tried in order.

The log function takes three arguments,
`fun(Level, FormatString, [FormatArg]) end`.

Timeout set the maximum time in milliseconds that each server request may take.

All TCP socket options are accepted except `active`, `binary`, `deliver`,
`list`, `mode` and `packet`
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec open(Hosts, Options) -> {ok, Handle} | {error, Reason} when
    Hosts :: [Host],
    Host  :: inet:socket_address() | inet:hostname(),
    Options ::
        [{port, integer()} |
	 {log, function()} |
	 {timeout, timeout()} |
	 {ssl, boolean()} |
	 {sslopts, [ssl:tls_client_option()]} |
	 {tcpopts, [inet:inet_backend() | gen_tcp:connect_option()]}],
    Handle :: handle(),
    Reason :: term().

open(Hosts, Opts) when is_list(Hosts), is_list(Opts) ->
    Self = self(),
    Pid = spawn_link(fun() -> init(Hosts, Opts, Self) end),
    recv(Pid).

%%% --------------------------------------------------------------------
%%% Upgrade an existing connection to TLS
%%% --------------------------------------------------------------------
-doc """
Same as start_tls(Handle, Options, infinity)
""".
-doc(#{since => <<"OTP R16B03">>}).
-spec start_tls(Handle, TlsOptions) -> ok |{ok, Refs} | {error, Reason} when
    Handle :: handle(),
    TlsOptions :: [ssl:tls_client_option()],
    Refs :: {referral, referrals()},
    Reason :: term().
start_tls(Handle, TlsOptions) ->
    start_tls(Handle, TlsOptions, infinity).

-doc """
Upgrade the connection associated with `Handle` to a TLS connection if possible.

The upgrade is done in two phases: first the server is asked for permission to
upgrade. Second, if the request is acknowledged, the upgrade to TLS is
performed.

Error responses from phase one will not affect the current encryption state of
the connection. Those responses are:

- **`tls_already_started`** - The connection is already encrypted. The
  connection is not affected.

- **`{response,ResponseFromServer}`** - The upgrade was refused by the LDAP
  server. The `ResponseFromServer` is an atom delivered byt the LDAP server
  explained in section 2.3 of rfc 2830. The connection is not affected, so it is
  still un-encrypted.

Errors in the second phase will however end the connection:

- **`Error`** - Any error responded from ssl:connect/3

The `Timeout` parameter is for the actual TLS upgrade (phase 2) while the
timeout in [eldap:open/2](`open/2`) is used for the initial negotiation about
upgrade (phase 1).
""".
-doc(#{since => <<"OTP R16B03">>}).
-spec start_tls(Handle, TlsOptions, Timeout) -> ok | {ok, Refs} | {error, Reason} when
    Handle :: handle(),
    TlsOptions :: [ssl:tls_client_option()],
    Timeout :: infinity | pos_integer(),
    Refs :: {referral, referrals()},
    Reason :: term().
start_tls(Handle, TlsOptions, Timeout) ->
    start_tls(Handle, TlsOptions, Timeout, asn1_NOVALUE).

-doc false.
start_tls(Handle, TlsOptions, Timeout, Controls) ->
    send(Handle, {start_tls,TlsOptions,Timeout,Controls}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Modify the password of a user.
%%%
%%% Dn        - Name of the entry to modify. If empty, the session user.
%%% NewPasswd - New password. If empty, the server returns a new password.
%%% OldPasswd - Original password for server verification, may be empty.
%%%
%%% Returns: ok | {ok, Refs} | {ok, GenPasswd} | {error, term()}
%%% --------------------------------------------------------------------
-doc """
Modify the password of a user. See `modify_password/4`.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec modify_password(Handle, Dn, NewPasswd) ->
    ok | {ok, Refs} | {error, term()} | {ok, GenPasswd} when
    Handle :: handle(),
    Dn :: string(),
    NewPasswd :: string(),
    Refs :: {referral, referrals()},
    GenPasswd :: string().
modify_password(Handle, Dn, NewPasswd) ->
    modify_password(Handle, Dn, NewPasswd, []).

-doc """
Modify the password of a user.

- `Dn`. The user to modify. Should be "" if the modify request is for the user
  of the LDAP session.
- `NewPasswd`. The new password to set. Should be "" if the server is to
  generate the password. In this case, the result will be `{ok, GenPasswd}`.
- `OldPasswd`. Sometimes required by server policy for a user to change their
  password. If not required, use `modify_password/3`.
""".
-doc(#{since => <<"OTP 18.0">>}).
-spec modify_password(Handle, Dn, NewPasswd, OldPasswd) ->
    ok | {ok, Refs} | {error, term()} | {ok, GenPasswd} when
    Handle :: handle(),
    Dn :: string(),
    NewPasswd :: string(),
    OldPasswd :: string(),
    Refs :: {referral, referrals()},
    GenPasswd :: string().
modify_password(Handle, Dn, NewPasswd, OldPasswd)
  when is_pid(Handle), is_list(Dn), is_list(NewPasswd), is_list(OldPasswd) ->
    modify_password(Handle, Dn, NewPasswd, OldPasswd, asn1_NOVALUE).

-doc false.
modify_password(Handle, Dn, NewPasswd, OldPasswd, Controls)
  when is_pid(Handle), is_list(Dn), is_list(NewPasswd), is_list(OldPasswd) ->
    send(Handle, {passwd_modify,optional(Dn),optional(NewPasswd),optional(OldPasswd),Controls}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Ask for option values on the socket.
%%% Warning: This is an undocumented function for testing purposes only.
%%%          Use at own risk...
%%% --------------------------------------------------------------------
-doc false.
getopts(Handle, OptNames) when is_pid(Handle), is_list(OptNames) ->
    send(Handle, {getopts, OptNames}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Shutdown connection (and process) asynchronous.
%%% --------------------------------------------------------------------

-doc """
Shutdown the connection after sending an unbindRequest to the server.

If the connection is TLS the connection will be closed with `ssl:close/1`, otherwise
with `gen_tcp:close/1`.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec close(Handle) -> ok when
    Handle :: handle().
close(Handle) when is_pid(Handle) ->
    send(Handle, close),
    ok.

%%% --------------------------------------------------------------------
%%% Set who we should link ourselves to
%%% --------------------------------------------------------------------

-doc false.
controlling_process(Handle, Pid) when is_pid(Handle), is_pid(Pid)  ->
    link(Pid),
    send(Handle, {cnt_proc, Pid}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Return LDAP socket information
%%% --------------------------------------------------------------------
-doc """
Currently available information reveals the socket and the transport protocol,
TCP or TLS (SSL), used by the LDAP connection.
""".
-doc(#{since => <<"OTP 25.3.1">>}).
-spec info(Handle) -> connection_info() when
    Handle :: handle().
info(Handle) when is_pid(Handle) ->
    send(Handle, info),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Authenticate ourselves to the Directory
%%% using simple authentication.
%%%
%%%  Dn      -  The name of the entry to bind as
%%%  Passwd  -  The password to be used
%%%
%%%  Returns: - ok | {ok, Refs} | {error, Reason}
%%% --------------------------------------------------------------------
-doc """
Authenticate the connection using simple authentication.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec simple_bind(Handle, Dn, Password) -> ok | {ok, Refs} | {error, Reason} when
    Handle :: handle(),
    Dn :: string(),
    Password :: string(),
    Refs :: {referral, referrals()},
    Reason :: term().
simple_bind(Handle, Dn, Passwd) when is_pid(Handle)  ->
    simple_bind(Handle, Dn, Passwd, asn1_NOVALUE).

-doc false.
simple_bind(Handle, Dn, Passwd, Controls) when is_pid(Handle)  ->
    send(Handle, {simple_bind, Dn, Passwd, Controls}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Add an entry. The entry field MUST NOT exist for the AddRequest
%%% to succeed. The parent of the entry MUST exist.
%%% Example:
%%%
%%%  add(Handle,
%%%         "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%         [{"objectclass", ["person"]},
%%%          {"cn", ["Bill Valentine"]},
%%%          {"sn", ["Valentine"]},
%%%          {"telephoneNumber", ["545 555 00"]}]
%%%     )
%%% --------------------------------------------------------------------
-doc """
Add an entry. The entry must not exist.

```erlang
  add(Handle,
      "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com",
       [{"objectclass", ["person"]},
        {"cn", ["Bill Valentine"]},
        {"sn", ["Valentine"]},
        {"telephoneNumber", ["545 555 00"]}]
     )
```
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec add(Handle, Dn, Attributes) -> ok | {ok, Refs} | {error, Reason} when
    Handle :: handle(),
    Dn :: string(),
    Attributes :: [attribute()],
    Refs :: {referral, referrals()},
    Reason :: term().
add(Handle, Entry, Attributes) when is_pid(Handle),is_list(Entry),is_list(Attributes) ->
    add(Handle, Entry, Attributes, asn1_NOVALUE).

-doc false.
add(Handle, Entry, Attributes, Controls) when is_pid(Handle),is_list(Entry),is_list(Attributes) ->
    send(Handle, {add, Entry, add_attrs(Attributes), Controls}),
    recv(Handle).

%%% Do sanity check !
add_attrs(Attrs) ->
    F = fun({Type,Vals}) when is_list(Type),is_list(Vals) ->
		%% Confused ? Me too... :-/
		{'AddRequest_attributes',Type, Vals}
	end,
    case catch lists:map(F, Attrs) of
	{'EXIT', _} -> throw({error, attribute_values});
	Else        -> Else
    end.

%%% --------------------------------------------------------------------
%%% Delete an entry. The entry consists of the DN of
%%% the entry to be deleted.
%%% Example:
%%%
%%%  delete(Handle,
%%%         "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com"
%%%        )
%%% --------------------------------------------------------------------
-doc """
Delete an entry.

```text
  delete(Handle, "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com")
```
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec delete(Handle, Dn) -> ok | {ok, Refs} |{error, Reason} when
    Handle :: handle(),
    Dn :: string(),
    Refs :: {referral, referrals()},
    Reason :: term().
delete(Handle, Entry) when is_pid(Handle), is_list(Entry) ->
    delete(Handle, Entry, asn1_NOVALUE).

-doc false.
delete(Handle, Entry, Controls)  when is_pid(Handle), is_list(Entry) ->
    send(Handle, {delete, Entry, Controls}),
    recv(Handle).

%%% --------------------------------------------------------------------
%%% Modify an entry. Given an entry a number of modification
%%% operations can be performed as one atomic operation.
%%% Example:
%%%
%%%  modify(Handle,
%%%         "cn=Torbjorn Tornkvist, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%         [mod_replace("telephoneNumber", ["555 555 00"]),
%%%          mod_add("description", ["LDAP hacker"])]
%%%        )
%%% --------------------------------------------------------------------
-doc """
Modify an entry.

```erlang
  modify(Handle, "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com",
         [eldap:mod_replace("telephoneNumber", ["555 555 00"]),
	  eldap:mod_add("description", ["LDAP Hacker"]) ])
```
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec modify(Handle, Dn, ModifyOps) -> ok | {ok, Refs} | {error, Reason} when
    Handle :: handle(),
    Dn :: string(),
    ModifyOps :: [term()],
    Refs :: {referral, referrals()},
    Reason :: term().
modify(Handle, Object, Mods) when is_pid(Handle), is_list(Object), is_list(Mods) ->
    modify(Handle, Object, Mods, asn1_NOVALUE).

-doc false.
modify(Handle, Object, Mods, Controls) when is_pid(Handle), is_list(Object), is_list(Mods) ->
    send(Handle, {modify, Object, Mods, Controls}),
    recv(Handle).

%%%
%%% Modification operations.
%%% Example:
%%%            mod_replace("telephoneNumber", ["555 555 00"])
%%%
-doc """
Create an add modification operation.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec mod_add(Type, Values) -> modify_op() when
    Type :: string(),
    Values :: [string()].
mod_add(Type, Values) when is_list(Type), is_list(Values)     -> m(add, Type, Values).

-doc """
Create a delete modification operation.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec mod_delete(Type, Values) -> modify_op() when
    Type :: string(),
    Values :: [string()].
mod_delete(Type, Values) when is_list(Type), is_list(Values)  -> m(delete, Type, Values).

-doc """
Create a replace modification operation.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec mod_replace(Type, Values) -> modify_op() when
    Type :: string(),
    Values :: [string()].
mod_replace(Type, Values) when is_list(Type), is_list(Values) -> m(replace, Type, Values).

m(Operation, Type, Values) ->
    #'ModifyRequest_changes_SEQOF'{
       operation = Operation,
       modification = #'PartialAttribute'{
	 type = Type,
	 vals = Values}}.

%%% --------------------------------------------------------------------
%%% Modify an entry. Given an entry a number of modification
%%% operations can be performed as one atomic operation.
%%% Example:
%%%
%%%  modify_dn(Handle,
%%%    "cn=Bill Valentine, ou=people, o=Bluetail AB, dc=bluetail, dc=com",
%%%    "cn=Ben Emerson",
%%%    true,
%%%    ""
%%%        )
%%% --------------------------------------------------------------------
-doc """
Modify the DN of an entry.

`DeleteOldRDN` indicates whether the current RDN
should be removed from the attribute list after the operation. `NewSupDN` is the
new parent that the RDN shall be moved to. If the old parent should remain as
parent, `NewSupDN` shall be "".

```text
  modify_dn(Handle, "cn=Bill Valentine, ou=people, o=Example Org, dc=example, dc=com ",
            "cn=Bill Jr Valentine", true, "")
```
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec modify_dn(Handle, Dn, NewRDN, DeleteOldRDN, NewSupDN) -> ok | {ok, Refs} | {error, Reason} when
    Handle :: handle(),
    Dn :: string(),
    NewRDN :: string(),
    DeleteOldRDN :: boolean(),
    NewSupDN :: string(),
    Refs :: {referral, referrals()},
    Reason :: term().
modify_dn(Handle, Entry, NewRDN, DelOldRDN, NewSup)
  when is_pid(Handle),is_list(Entry),is_list(NewRDN),is_atom(DelOldRDN),is_list(NewSup) ->
    modify_dn(Handle, Entry, NewRDN, DelOldRDN, NewSup, asn1_NOVALUE).

modify_dn(Handle, Entry, NewRDN, DelOldRDN, NewSup, Controls)
  when is_pid(Handle),is_list(Entry),is_list(NewRDN),is_atom(DelOldRDN),is_list(NewSup) ->
    send(Handle, {modify_dn, Entry, NewRDN,
		  bool_p(DelOldRDN), optional(NewSup), Controls}),
    recv(Handle).

%%% Sanity checks !

bool_p(Bool) when is_boolean(Bool) -> Bool.

optional([])    -> asn1_NOVALUE;
optional(Value) -> Value.

%%% --------------------------------------------------------------------
%%% Synchronous search of the Directory returning a
%%% requested set of attributes.
%%%
%%%  Example:
%%%
%%%	Filter = eldap:substrings("cn", [{any,"o"}]),
%%%	eldap:search(S, [{base, "dc=bluetail, dc=com"},
%%%	                 {filter, Filter},
%%%			 {attributes,["cn"]}])),
%%%
%%% Returned result:  {ok, #eldap_search_result{}}
%%%
%%% Example:
%%%
%%%  {ok,{eldap_search_result,
%%%        [{eldap_entry,
%%%           "cn=Magnus Froberg, dc=bluetail, dc=com",
%%%           [{"cn",["Magnus Froberg"]}]},
%%%         {eldap_entry,
%%%           "cn=Torbjorn Tornkvist, dc=bluetail, dc=com",
%%%           [{"cn",["Torbjorn Tornkvist"]}]}],
%%%        []}}
%%%
%%% --------------------------------------------------------------------
-doc """
Search the directory with the supplied the SearchOptions.

The base and filter
options must be supplied. Default values: scope is `wholeSubtree/0`, deref is
`derefAlways/0`, types_only is `false` and timeout is `0` (meaning infinity).

```erlang
  Filter = eldap:substrings("cn", [{any,"V"}]),
  search(Handle, [{base, "dc=example, dc=com"}, {filter, Filter}, {attributes, ["cn"]}]),
```

The `timeout` option in the `SearchOptions` is for the ldap server, while the
timeout in [eldap:open/2](`open/2`) is used for each individual request in the
search operation.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec search(Handle, SearchOptions) ->
    {ok, #eldap_search_result{entries :: term(), referrals :: term(), controls :: term()}} |
    {ok, Refs} |
    {error, Reason} when
    Handle :: handle(),
    SearchOptions ::
        #eldap_search{base :: term(),
                      filter :: term(),
                      size_limit :: term(),
                      scope :: term(),
                      deref :: term(),
                      attributes :: term(),
                      types_only :: term(),
                      timeout :: term()} |
        [search_option()],
    Refs :: {referral, referrals()},
    Reason :: term().
search(Handle, X) when is_pid(Handle), is_record(X,eldap_search) ; is_list(X) ->
    search(Handle, X, asn1_NOVALUE).
    
-doc false.
search(Handle, A, Controls) when is_pid(Handle), is_record(A, eldap_search) ->
    call_search(Handle, A, Controls);
search(Handle, L, Controls) when is_pid(Handle), is_list(L) ->
    case catch parse_search_args(L) of
	{error, Emsg}                  -> {error, Emsg};
	A when is_record(A, eldap_search) -> call_search(Handle, A, Controls)
    end.

call_search(Handle, A, Controls) ->
    send(Handle, {search, A, Controls}),
    recv(Handle).

parse_search_args(Args) ->
    parse_search_args(Args,
		      #eldap_search{scope = wholeSubtree,
				    deref = derefAlways}).

parse_search_args([{base, Base}|T],A) ->
    parse_search_args(T,A#eldap_search{base = Base});
parse_search_args([{filter, Filter}|T],A) ->
    parse_search_args(T,A#eldap_search{filter = Filter});
parse_search_args([{size_limit, SizeLimit}|T],A) when is_integer(SizeLimit) ->
    parse_search_args(T,A#eldap_search{size_limit = SizeLimit});
parse_search_args([{scope, Scope}|T],A) ->
    parse_search_args(T,A#eldap_search{scope = Scope});
parse_search_args([{deref, Deref}|T],A) ->
    parse_search_args(T,A#eldap_search{deref = Deref});
parse_search_args([{attributes, Attrs}|T],A) ->
    parse_search_args(T,A#eldap_search{attributes = Attrs});
parse_search_args([{types_only, TypesOnly}|T],A) ->
    parse_search_args(T,A#eldap_search{types_only = TypesOnly});
parse_search_args([{timeout, Timeout}|T],A) when is_integer(Timeout) ->
    parse_search_args(T,A#eldap_search{timeout = Timeout});
parse_search_args([H|_],_) ->
    throw({error,{unknown_arg, H}});
parse_search_args([],A) ->
    A.

%%%
%%% The Scope parameter
%%%
-doc """
Search baseobject only.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec baseObject()   -> scope().
baseObject()   -> baseObject.
-doc """
Search the specified level only, i.e. do not recurse.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec singleLevel()  -> scope().
singleLevel()  -> singleLevel.
-doc """
Search the entire subtree.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec wholeSubtree() -> scope().
wholeSubtree() -> wholeSubtree.

%%
%% The derefAliases parameter
%%
-doc """
Never dereference aliases, treat aliases as entries.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec neverDerefAliases() -> dereference().
neverDerefAliases()   -> neverDerefAliases.
-doc """
Dereference aliases only when searching.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec derefInSearching() -> dereference().
derefInSearching()    -> derefInSearching.
-doc """
Dereference aliases only in finding the base.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec derefFindingBaseObj() -> dereference().
derefFindingBaseObj() -> derefFindingBaseObj.
-doc """
Always dereference aliases.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec derefAlways() -> dereference().
derefAlways()         -> derefAlways.

%%%
%%% Boolean filter operations
%%%
-doc """
Creates a filter where all `Filter` must be true.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec 'and'(ListOfFilters) -> filter() when ListOfFilters :: [filter()].
'and'(ListOfFilters) when is_list(ListOfFilters) -> {'and',ListOfFilters}.

-doc """
Create a filter where at least one of the `Filter` must be true.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec 'or'(ListOfFilters) -> filter() when ListOfFilters :: [filter()].
'or'(ListOfFilters)  when is_list(ListOfFilters) -> {'or', ListOfFilters}.

-doc """
Negate a filter.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec 'not'(Filter) -> filter() when Filter :: {filter()}.
'not'(Filter)        when is_tuple(Filter)       -> {'not',Filter}.

%%%
%%% The following Filter parameters consist of an attribute
%%% and an attribute value. Example: F("uid","tobbe")
%%%
-doc """
Create a equality filter.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec equalityMatch(Type, Value) -> filter() when
    Type :: string(),
    Value :: string().
equalityMatch(Desc, Value)   -> {equalityMatch, av_assert(Desc, Value)}.
-doc """
Create a greater or equal filter.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec greaterOrEqual(Type, Value) -> filter() when
    Type :: string(),
    Value :: string().
greaterOrEqual(Desc, Value)  -> {greaterOrEqual, av_assert(Desc, Value)}.
-doc """
Create a less or equal filter.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec lessOrEqual(Type, Value) -> filter() when
    Type :: string(),
    Value :: string().
lessOrEqual(Desc, Value)     -> {lessOrEqual, av_assert(Desc, Value)}.
-doc """
Create a approximation match filter.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec approxMatch(Type, Value) -> filter() when
    Type :: string(),
    Value :: string().

approxMatch(Desc, Value)     -> {approxMatch, av_assert(Desc, Value)}.

av_assert(Desc, Value) ->
    #'AttributeValueAssertion'{attributeDesc  = Desc,
			       assertionValue = Value}.

%%%
%%% Filter to check for the presence of an attribute
%%%
-doc """
Create a filter which filters on attribute type presence.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec present(Type) -> filter() when Type :: string().
present(Attribute) when is_list(Attribute) ->
    {present, Attribute}.


%%%
%%% A substring filter seem to be based on a pattern:
%%%
%%%   InitValue*AnyValue*FinalValue
%%%
%%% where all three parts seem to be optional (at least when
%%% talking with an OpenLDAP server). Thus, the arguments
%%% to substrings/2 looks like this:
%%%
%%% Type   ::= string( <attribute> )
%%% SubStr ::= listof( {initial,Value} | {any,Value}, {final,Value})
%%%
%%% Example: substrings("sn",[{initial,"To"},{any,"kv"},{final,"st"}])
%%% will match entries containing:  'sn: Tornkvist'
%%%
-doc """
Create a filter which filters on substrings.
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec substrings(Type, SubStrings) -> filter() when
    Type :: string(),
    SubStrings :: [{initial, string()} | {any, string()} |{final, string()}].
substrings(Type, SubStr) when is_list(Type), is_list(SubStr) ->
    Ss = v_substr(SubStr),
    {substrings,#'SubstringFilter'{type = Type,
				   substrings = Ss}}.

%%%
%%% Filter for extensibleMatch
%%%
-doc """
Creates an extensible match filter. For example,

```erlang
  eldap:extensibleMatch("Bar", [{type,"sn"}, {matchingRule,"caseExactMatch"}]))
```

creates a filter which performs a `caseExactMatch` on the attribute `sn` and
matches with the value `"Bar"`. The default value of `dnAttributes` is `false`.
""".
-doc(#{since => <<"OTP 17.4">>}).
-spec extensibleMatch(MatchValue, OptionalAttrs) -> filter() when
    MatchValue :: string(),
    OptionalAttrs :: [{matchingRule, string()} | {type, string()} | {dnAttributes, boolean()}].
extensibleMatch(MatchValue, OptArgs) ->
    MatchingRuleAssertion =  
	mra(OptArgs, #'MatchingRuleAssertion'{matchValue = MatchValue}),
    {extensibleMatch, MatchingRuleAssertion}.

mra([{matchingRule,Val}|T], Ack) when is_list(Val) ->
    mra(T, Ack#'MatchingRuleAssertion'{matchingRule=Val});
mra([{type,Val}|T], Ack) when is_list(Val) ->
    mra(T, Ack#'MatchingRuleAssertion'{type=Val});
mra([{dnAttributes,true}|T], Ack) ->
    mra(T, Ack#'MatchingRuleAssertion'{dnAttributes=true});
mra([{dnAttributes,false}|T], Ack) ->
    mra(T, Ack#'MatchingRuleAssertion'{dnAttributes=false});
mra([H|_], _) ->
    throw({error,{extensibleMatch_arg,H}});
mra([], Ack) -> 
    Ack.

%%% --------------------------------------------------------------------
%%% Worker process. We keep track of a controlling process to
%%% be able to terminate together with it.
%%% --------------------------------------------------------------------

init(Hosts, Opts, Cpid) ->
    Data = parse_args(Opts, Cpid, #eldap{}),
    case try_connect(Hosts, Data) of
	{ok,Data2} ->
	    send(Cpid, {ok,self()}),
	    ?MODULE:loop(Cpid, Data2);
	Else ->
 	    send(Cpid, Else),
	    unlink(Cpid),
	    exit(Else)
    end.

parse_args([{port, Port}|T], Cpid, Data) when is_integer(Port) ->
    parse_args(T, Cpid, Data#eldap{port = Port});
parse_args([{timeout, Timeout}|T], Cpid, Data) when is_integer(Timeout),Timeout>0 ->
    parse_args(T, Cpid, Data#eldap{timeout = Timeout});
parse_args([{anon_auth, true}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data#eldap{anon_auth = true});
parse_args([{anon_auth, _}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data);
parse_args([{ssl, true}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data#eldap{ldaps = true, using_tls=true});
parse_args([{ssl, _}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data);
parse_args([{sslopts, Opts}|T], Cpid, Data) when is_list(Opts) ->
    parse_args(T, Cpid, Data#eldap{ldaps = true, using_tls=true, tls_opts = Opts ++ Data#eldap.tls_opts});
parse_args([{sslopts, _}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data);
parse_args([{tcpopts, Opts}|T], Cpid, Data) when is_list(Opts) ->
    parse_args(T, Cpid, Data#eldap{tcp_opts = tcp_opts(Opts,Cpid,Data#eldap.tcp_opts)});
parse_args([{log, F}|T], Cpid, Data) when is_function(F) ->
    parse_args(T, Cpid, Data#eldap{log = F});
parse_args([{log, _}|T], Cpid, Data) ->
    parse_args(T, Cpid, Data);
parse_args([H|_], Cpid, _) ->
    send(Cpid, {error,{wrong_option,H}}),
    unlink(Cpid),
    exit(wrong_option);
parse_args([], _, Data) ->
    Data.

tcp_opts([Opt|Opts], Cpid, Acc) -> 
    Key = if is_atom(Opt) -> Opt;
	     is_tuple(Opt) -> element(1,Opt)
	  end,
    case lists:member(Key,[active,binary,deliver,list,mode,packet]) of
	false ->
	    tcp_opts(Opts, Cpid, [Opt|Acc]);
	true ->
	    tcp_opts_error(Opt, Cpid)
    end;
tcp_opts([], _Cpid, Acc) -> Acc.
    
tcp_opts_error(Opt, Cpid) ->
    send(Cpid, {error, {{forbidden_tcp_option,Opt}, 
			"This option affects the eldap functionality and can't be set by user"}}),
    unlink(Cpid),
    exit(forbidden_tcp_option).

%%% Try to connect to the hosts in the listed order,
%%% and stop with the first one to which a successful
%%% connection is made.

try_connect([Host|Hosts], Data) ->
    TcpOpts = [{packet, asn1}, {active,false}],
    try do_connect(Host, Data, TcpOpts) of
	{ok,Fd} -> {ok,Data#eldap{host = Host, fd   = Fd}};
	Err    ->
	    log2(Data, "Connect: ~p failed ~p~n",[Host, Err]),
	    try_connect(Hosts, Data)
    catch _:Err ->
	    log2(Data, "Connect: ~p failed ~p~n",[Host, Err]),
	    try_connect(Hosts, Data)
    end;
try_connect([],_) ->
    {error,"connect failed"}.

do_connect(Host, Data, Opts) when Data#eldap.ldaps == false ->
    gen_tcp:connect(Host, Data#eldap.port, Opts ++ Data#eldap.tcp_opts,
		    Data#eldap.timeout);
do_connect(Host, Data, Opts) when Data#eldap.ldaps == true ->
    ssl:connect(Host, Data#eldap.port,
		Opts ++ Data#eldap.tls_opts ++ Data#eldap.tcp_opts,
		Data#eldap.timeout).

-doc false.
loop(Cpid, Data) ->
    receive

	{From, {search, A, Controls}} ->
	    {Res,NewData} = do_search(Data, A, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

	{From, {modify, Obj, Mod, Controls}} ->
	    {Res,NewData} = do_modify(Data, Obj, Mod, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

	{From, {modify_dn, Obj, NewRDN, DelOldRDN, NewSup, Controls}} ->
	    {Res,NewData} = do_modify_dn(Data, Obj, NewRDN, DelOldRDN, NewSup, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

	{From, {add, Entry, Attrs, Controls}} ->
	    {Res,NewData} = do_add(Data, Entry, Attrs, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

	{From, {delete, Entry, Controls}} ->
	    {Res,NewData} = do_delete(Data, Entry, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

	{From, {simple_bind, Dn, Passwd, Controls}} ->
	    {Res,NewData} = do_simple_bind(Data, Dn, Passwd, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

	{From, {cnt_proc, NewCpid}} ->
	    unlink(Cpid),
	    send(From,ok),
	    ?PRINT("New Cpid is: ~p~n",[NewCpid]),
	    ?MODULE:loop(NewCpid, Data);

	{From, {start_tls,TlsOptions,Timeout,Controls}} ->
	    {Res,NewData} = do_start_tls(Data, TlsOptions, Timeout, Controls),
	    send(From,Res),
	    ?MODULE:loop(Cpid, NewData);

        {From, {passwd_modify,Dn,NewPasswd,OldPasswd,Controls}} ->
            {Res,NewData} = do_passwd_modify(Data, Dn, NewPasswd, OldPasswd, Controls),
            send(From, Res),
            ?MODULE:loop(Cpid, NewData);

	{_From, close} ->
	    % Ignore tcp error if connection is already closed.
	    try do_unbind(Data) of
	        {no_reply,_NewData} -> ok
	    catch
	        throw:{gen_tcp_error, _TcpErr} -> ok
	    end,
	    unlink(Cpid),
	    exit(closed);

	{From, {getopts, OptNames}} ->
	    Result = 
		try
		    [case OptName of
			 port ->    {port,    Data#eldap.port};
			 log ->     {log,     Data#eldap.log};
			 timeout -> {timeout, Data#eldap.timeout};
			 ssl ->     {ssl,     Data#eldap.ldaps};
			 {sslopts, SslOptNames} when Data#eldap.using_tls==true -> 
			     case ssl:getopts(Data#eldap.fd, SslOptNames) of
				 {ok,SslOptVals} -> {sslopts, SslOptVals};
				 {error,Reason} -> throw({error,Reason})
			     end;
			 {sslopts, _} ->
			     throw({error,no_tls});
			 {tcpopts, TcpOptNames} -> 
			     case inet:getopts(Data#eldap.fd, TcpOptNames) of
				 {ok,TcpOptVals} -> {tcpopts, TcpOptVals};
				 {error,Posix} -> throw({error,Posix})
			     end
		     end || OptName <- OptNames]
		of
		    OptsList -> {ok,OptsList}
		catch
		    throw:Error -> Error;
		    Class:Error -> {error,{Class,Error}}
		end,
	    send(From, Result),
	    ?MODULE:loop(Cpid, Data);

        {From, info} ->
            SocketType =
                case Data#eldap.ldaps of
                    true ->
                        ssl;
                    false ->
                        tcp
                end,
            Res = #{socket => Data#eldap.fd, socket_type => SocketType},
            send(From, Res),
            ?MODULE:loop(Cpid, Data);

	{Cpid, 'EXIT', Reason} ->
	    ?PRINT("Got EXIT from Cpid, reason=~p~n",[Reason]),
	    exit(Reason);

	_XX ->
	    ?PRINT("loop got: ~p~n",[_XX]),
	    ?MODULE:loop(Cpid, Data)

    end.

%%% --------------------------------------------------------------------
%%% startTLS Request
%%% --------------------------------------------------------------------
do_start_tls(Data=#eldap{using_tls=true}, _, _, _) ->
    {{error,tls_already_started}, Data};
do_start_tls(Data=#eldap{fd=FD} , TlsOptions, Timeout, Controls) ->
    case catch exec_start_tls(Data, Controls) of
	{ok,NewData} ->
	    case ssl:connect(FD,TlsOptions,Timeout) of
		{ok, SslSocket} ->
		    {ok, NewData#eldap{prev_fd = FD,
				       fd = SslSocket,
				       using_tls = true
				      }};
		{error,Error} ->
		    {{error,Error}, Data}
	    end;
	{{ok,Val},NewData} -> {{ok,Val},NewData};
	{error,Error}      -> {{error,Error},Data};
	Else               -> {{error,Else},Data}
    end.

-define(START_TLS_OID, "1.3.6.1.4.1.1466.20037").

exec_start_tls(Data, Controls) ->
    Req = #'ExtendedRequest'{requestName = ?START_TLS_OID},
    Reply = request(Data#eldap.fd, Data, Data#eldap.id, {extendedReq, Req, Controls}),
    exec_extended_req_reply(Data, Reply).

exec_extended_req_reply(Data, {ok,Msg}) when
  Msg#'LDAPMessage'.messageID == Data#eldap.id ->
    case Msg#'LDAPMessage'.protocolOp of
	{extendedResp, Result} ->
	    case Result#'ExtendedResponse'.resultCode of
		success ->
		    {ok,Data};
		referral ->
		    {{ok, {referral,Result#'ExtendedResponse'.referral}}, Data};
		Error ->
		    {error, {response,Error}}
	    end;
	Other -> {error, Other}
    end;
exec_extended_req_reply(_, Error) ->
    {error, Error}.

%%% --------------------------------------------------------------------
%%% bindRequest
%%% --------------------------------------------------------------------

%%% Authenticate ourselves to the directory using
%%% simple authentication.

do_simple_bind(Data, anon, anon, Controls) ->   %% For testing
    do_the_simple_bind(Data, "", "", Controls);
do_simple_bind(Data, Dn, _Passwd,_) when Dn=="",Data#eldap.anon_auth==false ->
    {{error,anonymous_auth},Data};
do_simple_bind(Data, _Dn, Passwd,_) when Passwd=="",Data#eldap.anon_auth==false ->
    {{error,anonymous_auth},Data};
do_simple_bind(Data, Dn, Passwd, Controls) ->
    do_the_simple_bind(Data, Dn, Passwd, Controls).

do_the_simple_bind(Data, Dn, Passwd, Controls) ->
    case catch exec_simple_bind(Data#eldap{binddn = Dn,
					   passwd = Passwd,
					   id     = bump_id(Data)},
			       Controls) of
	{ok,NewData}       -> {ok,NewData};
	{{ok,Val},NewData} -> {{ok,Val},NewData};
	{error,Emsg}       -> {{error,Emsg},Data};
	Else               -> {{error,Else},Data}
    end.

exec_simple_bind(Data, Controls) ->
    Req = #'BindRequest'{version        = Data#eldap.version,
			 name           = Data#eldap.binddn,
			 authentication = {simple, Data#eldap.passwd}},
    log2(Data, "bind request = ~p~n", [Req]),
    Reply = request(Data#eldap.fd, Data, Data#eldap.id, {bindRequest, Req, Controls}),
    log2(Data, "bind reply = ~p~n", [Reply]),
    exec_simple_bind_reply(Data, Reply).

exec_simple_bind_reply(Data, {ok,Msg}) when
  Msg#'LDAPMessage'.messageID == Data#eldap.id ->
    case Msg#'LDAPMessage'.protocolOp of
	{bindResponse, Result} ->
	    case Result#'BindResponse'.resultCode of
		success -> {ok,Data};
		referral -> {{ok, {referral,Result#'BindResponse'.referral}}, Data};
		Error   -> {error, Error}
	    end;
	Other -> {error, Other}
    end;
exec_simple_bind_reply(_, Error) ->
    {error, Error}.


%%% --------------------------------------------------------------------
%%% searchRequest
%%% --------------------------------------------------------------------

do_search(Data, A, Controls) ->
    case catch do_search_0(Data, A, Controls) of
	{error,Emsg}         -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error}       -> {ldap_closed_p(Data, Error),Data};
	{{ok,Val},NewData}   -> {{ok,Val},NewData};
	{ok,Res,Ref,ResultControls,NewData} -> {{ok,polish(Res, Ref, ResultControls)},NewData};
	{{error,Reason},NewData} -> {{error,Reason},NewData};
	Else                 -> {ldap_closed_p(Data, Else),Data}
    end.

%%%
%%% Polish the returned search result
%%%

polish(Res, Ref, Controls) ->
    R = polish_result(Res),
    %%% No special treatment of referrals at the moment.
    #eldap_search_result{entries = R,
			 referrals = Ref, controls = Controls}.

polish_result([H|T]) when is_record(H, 'SearchResultEntry') ->
    ObjectName = H#'SearchResultEntry'.objectName,
    F = fun({_,A,V}) -> {A,V} end,
    Attrs = lists:map(F, H#'SearchResultEntry'.attributes),
    [#eldap_entry{object_name = ObjectName,
		  attributes  = Attrs}|
     polish_result(T)];
polish_result([]) ->
    [].

do_search_0(Data, A, Controls) ->
    Req = #'SearchRequest'{baseObject = A#eldap_search.base,
			   scope = v_scope(A#eldap_search.scope),
			   derefAliases = v_deref(A#eldap_search.deref),
			   sizeLimit = v_size_limit(A#eldap_search.size_limit),
			   timeLimit = v_timeout(A#eldap_search.timeout),
			   typesOnly = v_bool(A#eldap_search.types_only),
			   filter = v_filter(A#eldap_search.filter),
			   attributes = v_attributes(A#eldap_search.attributes)
			  },
    Id = bump_id(Data),
    collect_search_responses(Data#eldap{id=Id}, Req, Id, Controls).

%%% The returned answers cames in one packet per entry
%%% mixed with possible referals

collect_search_responses(Data, Req, ID, Controls) ->
    S = Data#eldap.fd,
    log2(Data, "search request = ~p~n", [Req]),
    send_request(S, Data, ID, {searchRequest, Req, Controls}),
    Resp = recv_response(S, Data),
    log2(Data, "search reply = ~p~n", [Resp]),
    collect_search_responses(Data, S, ID, Resp, [], []).

collect_search_responses(Data, S, ID, {ok,Msg}, Acc, Ref)
  when is_record(Msg,'LDAPMessage') ->
    case Msg#'LDAPMessage'.protocolOp of
	{'searchResDone',R} ->
            case R#'LDAPResult'.resultCode of
                success ->
                    log2(Data, "search reply = searchResDone ~n", []),
                    {ok,Acc,Ref,Msg#'LDAPMessage'.controls,Data};
                sizeLimitExceeded ->
                     log2(Data, "[TRUNCATED] search reply = searchResDone ~n", []),
                     {ok,Acc,Ref,Msg#'LDAPMessage'.controls,Data};
		referral -> 
		    {{ok, {referral,R#'LDAPResult'.referral}}, Data};
                Reason ->
                    {{error,Reason},Data}
            end;
	{'searchResEntry',R} when is_record(R,'SearchResultEntry') ->
	    Resp = recv_response(S, Data),
	    log2(Data, "search reply = ~p~n", [Resp]),
	    collect_search_responses(Data, S, ID, Resp, [R|Acc], Ref);
	{'searchResRef',R} ->
	    %% At the moment we don't do anything sensible here since
	    %% I haven't been able to trigger the server to generate
	    %% a response like this.
	    Resp = recv_response(S, Data),
	    log2(Data, "search reply = ~p~n", [Resp]),
	    collect_search_responses(Data, S, ID, Resp, Acc, [R|Ref]);
	Else ->
	    throw({error,Else})
    end;
collect_search_responses(_, _, _, Else, _, _) ->
    throw({error,Else}).

%%% --------------------------------------------------------------------
%%% addRequest
%%% --------------------------------------------------------------------

do_add(Data, Entry, Attrs, Controls) ->
    case catch do_add_0(Data, Entry, Attrs, Controls) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	{{ok,Val},NewData} -> {{ok,Val},NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_add_0(Data, Entry, Attrs, Controls) ->
    Req = #'AddRequest'{entry = Entry,
			attributes = Attrs},
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "add request = ~p~n", [Req]),
    Resp = request(S, Data, Id, {addRequest, Req, Controls}),
    log2(Data, "add reply = ~p~n", [Resp]),
    check_reply(Data#eldap{id = Id}, Resp, addResponse).


%%% --------------------------------------------------------------------
%%% deleteRequest
%%% --------------------------------------------------------------------

do_delete(Data, Entry, Controls) ->
    case catch do_delete_0(Data, Entry, Controls) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	{{ok,Val},NewData} -> {{ok,Val},NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_delete_0(Data, Entry, Controls) ->
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "del request = ~p~n", [Entry]),
    Resp = request(S, Data, Id, {delRequest, Entry, Controls}),
    log2(Data, "del reply = ~p~n", [Resp]),
    check_reply(Data#eldap{id = Id}, Resp, delResponse).


%%% --------------------------------------------------------------------
%%% modifyRequest
%%% --------------------------------------------------------------------

do_modify(Data, Obj, Mod, Controls) ->
    case catch do_modify_0(Data, Obj, Mod, Controls) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	{{ok,Val},NewData} -> {{ok,Val},NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_modify_0(Data, Obj, Mod, Controls) ->
    v_modifications(Mod),
    Req = #'ModifyRequest'{object = Obj,
			   changes = Mod},
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "modify request = ~p~n", [Req]),
    Resp = request(S, Data, Id, {modifyRequest, Req, Controls}),
    log2(Data, "modify reply = ~p~n", [Resp]),
    check_reply(Data#eldap{id = Id}, Resp, modifyResponse).

%%% --------------------------------------------------------------------
%%% PasswdModifyRequest
%%% --------------------------------------------------------------------

-define(PASSWD_MODIFY_OID, "1.3.6.1.4.1.4203.1.11.1").

do_passwd_modify(Data, Dn, NewPasswd, OldPasswd, Controls) ->
    case catch do_passwd_modify_0(Data, Dn, NewPasswd, OldPasswd, Controls) of
	{error,Emsg}        -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error}      -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}        -> {ok,NewData};
	{{ok,Val},NewData}  -> {{ok,Val},NewData};
        {ok,Passwd,NewData} -> {{ok, Passwd},NewData};
	Else                -> {ldap_closed_p(Data, Else),Data}
    end.

do_passwd_modify_0(Data, Dn, NewPasswd, OldPasswd, Controls) ->
    Req = #'PasswdModifyRequestValue'{userIdentity = Dn,
                                      oldPasswd = OldPasswd,
                                      newPasswd = NewPasswd},
    log2(Data, "modify password request = ~p~n", [Req]),
    {ok, Bytes} = 'ELDAPv3':encode('PasswdModifyRequestValue', Req),
    ExtReq = #'ExtendedRequest'{requestName = ?PASSWD_MODIFY_OID,
                             requestValue = Bytes},
    Id = bump_id(Data),
    log2(Data, "extended request = ~p~n", [ExtReq]),
    Reply = request(Data#eldap.fd, Data, Id, {extendedReq, ExtReq, Controls}),
    log2(Data, "modify password reply = ~p~n", [Reply]),
    exec_passwd_modify_reply(Data#eldap{id = Id}, Reply).

exec_passwd_modify_reply(Data, {ok,Msg}) when
  Msg#'LDAPMessage'.messageID == Data#eldap.id ->
    case Msg#'LDAPMessage'.protocolOp of
	{extendedResp, Result} ->
	    case Result#'ExtendedResponse'.resultCode of
		success ->
                    case Result#'ExtendedResponse'.responseValue of
                        asn1_NOVALUE ->
                            {ok, Data};
                        Value ->
                            case 'ELDAPv3':decode('PasswdModifyResponseValue', Value) of
                                {ok,#'PasswdModifyResponseValue'{genPasswd = Passwd}} ->
                                    {ok, Passwd, Data};
                                Error ->
                                    throw(Error)
                            end
                    end;
		referral ->
		    {{ok, {referral,Result#'ExtendedResponse'.referral}}, Data};
		Error ->
		    {error, {response,Error}}
	    end;
	Other -> {error, Other}
    end;
exec_passwd_modify_reply(_, Error) ->
    {error, Error}.

%%% --------------------------------------------------------------------
%%% modifyDNRequest
%%% --------------------------------------------------------------------

do_modify_dn(Data, Entry, NewRDN, DelOldRDN, NewSup, Controls) ->
    case catch do_modify_dn_0(Data, Entry, NewRDN, DelOldRDN, NewSup, Controls) of
	{error,Emsg}   -> {ldap_closed_p(Data, Emsg),Data};
	{'EXIT',Error} -> {ldap_closed_p(Data, Error),Data};
	{ok,NewData}   -> {ok,NewData};
	{{ok,Val},NewData} -> {{ok,Val},NewData};
	Else           -> {ldap_closed_p(Data, Else),Data}
    end.

do_modify_dn_0(Data, Entry, NewRDN, DelOldRDN, NewSup, Controls) ->
    Req = #'ModifyDNRequest'{entry = Entry,
			     newrdn = NewRDN,
			     deleteoldrdn = DelOldRDN,
			     newSuperior = NewSup},
    S = Data#eldap.fd,
    Id = bump_id(Data),
    log2(Data, "modify DN request = ~p~n", [Req]),
    Resp = request(S, Data, Id, {modDNRequest, Req, Controls}),
    log2(Data, "modify DN reply = ~p~n", [Resp]),
    check_reply(Data#eldap{id = Id}, Resp, modDNResponse).

%%%--------------------------------------------------------------------
%%% unbindRequest
%%%--------------------------------------------------------------------
do_unbind(Data) ->
    Req = "",
    log2(Data, "unbind request = ~p (has no reply)~n", [Req]),
    _ = case Data#eldap.using_tls of
            true ->
                send_request(Data#eldap.fd, Data, Data#eldap.id, {unbindRequest, Req}),
                ssl:close(Data#eldap.fd);
            false ->
                OldTrapExit = process_flag(trap_exit, true),
                catch send_request(Data#eldap.fd, Data, Data#eldap.id, {unbindRequest, Req}),
                catch gen_tcp:close(Data#eldap.fd),
                receive
                    {'EXIT', _From, _Reason} -> ok
                after 0 -> ok
                end,
                process_flag(trap_exit, OldTrapExit)
        end,
    {no_reply, Data#eldap{binddn = (#eldap{})#eldap.binddn,
			  passwd = (#eldap{})#eldap.passwd,
			  fd     = (#eldap{})#eldap.fd,
			  using_tls = false
			 }}.


%%% --------------------------------------------------------------------
%%% Send an LDAP request and receive the answer
%%% --------------------------------------------------------------------
request(S, Data, ID, Request) ->
    send_request(S, Data, ID, Request),
    recv_response(S, Data).

send_request(S, Data, Id, {T,P}) ->
    send_the_LDAPMessage(S, Data, #'LDAPMessage'{messageID = Id,
						 protocolOp = {T,P}});
send_request(S, Data, Id, {T,P,asn1_NOVALUE}) ->
    send_the_LDAPMessage(S, Data, #'LDAPMessage'{messageID = Id,
						 protocolOp = {T,P}});
send_request(S, Data, Id, {T,P,Controls0}) ->
    Controls = [#'Control'{controlType=F1,
			   criticality=F2,
			   controlValue=F3} || {control,F1,F2,F3} <- Controls0],
    send_the_LDAPMessage(S, Data, #'LDAPMessage'{messageID = Id,
						 protocolOp = {T,P},
						 controls = Controls}).

send_the_LDAPMessage(S, Data, LDAPMessage) ->
    {ok,Bytes} = 'ELDAPv3':encode('LDAPMessage', LDAPMessage),
    case do_send(S, Data, Bytes) of
	{error,Reason} -> throw({gen_tcp_error,Reason});
	Else           -> Else
    end.

do_send(S, Data, Bytes) when Data#eldap.using_tls == false ->
    gen_tcp:send(S, Bytes);
do_send(S, Data, Bytes) when Data#eldap.using_tls == true ->
    ssl:send(S, Bytes).

do_recv(S, #eldap{using_tls=false, timeout=Timeout}, Len) ->
    gen_tcp:recv(S, Len, Timeout);
do_recv(S, #eldap{using_tls=true, timeout=Timeout}, Len) ->
    ssl:recv(S, Len, Timeout).

recv_response(S, Data) ->
    case do_recv(S, Data, 0) of
	{ok, Packet} ->
	    case 'ELDAPv3':decode('LDAPMessage', Packet) of
		{ok,Resp} -> {ok,Resp};
		Error     -> throw(Error)
	    end;
	{error,Reason} ->
	    throw({gen_tcp_error, Reason})
    end.

%%% Check for expected kind of reply
check_reply(Data, {ok,Msg}, Op) when
  Msg#'LDAPMessage'.messageID == Data#eldap.id ->
    case Msg#'LDAPMessage'.protocolOp of
	{Op, Result} ->
	    case Result#'LDAPResult'.resultCode of
		success -> {ok,Data};
		referral -> {{ok, {referral,Result#'LDAPResult'.referral}}, Data};
		Error   -> {error, Error}
	    end;
	Other -> {error, Other}
    end;
check_reply(_, Error, _) ->
    {error, Error}.


%%% --------------------------------------------------------------------
%%% Verify the input data
%%% --------------------------------------------------------------------

v_filter({'and',L})           -> {'and',L};
v_filter({'or', L})           -> {'or',L};
v_filter({'not',L})           -> {'not',L};
v_filter({equalityMatch,AV})  -> {equalityMatch,AV};
v_filter({greaterOrEqual,AV}) -> {greaterOrEqual,AV};
v_filter({lessOrEqual,AV})    -> {lessOrEqual,AV};
v_filter({approxMatch,AV})    -> {approxMatch,AV};
v_filter({present,A})         -> {present,A};
v_filter({substrings,S}) when is_record(S,'SubstringFilter') -> {substrings,S};
v_filter({extensibleMatch,S}) when is_record(S,'MatchingRuleAssertion') -> {extensibleMatch,S};
v_filter(_Filter) -> throw({error,concat(["unknown filter: ",_Filter])}).

v_modifications(Mods) ->
    F = fun({_,Op,_}) ->
		case lists:member(Op,[add,delete,replace]) of
		    true -> true;
		    _    -> throw({error,{mod_operation,Op}})
		end
	end,
    lists:foreach(F, Mods).

v_substr([{Key,Str}|T]) when is_list(Str),Key==initial;Key==any;Key==final ->
    [{Key,Str}|v_substr(T)];
v_substr([H|_]) ->
    throw({error,{substring_arg,H}});
v_substr([]) ->
    [].
v_scope(baseObject)   -> baseObject;
v_scope(singleLevel)  -> singleLevel;
v_scope(wholeSubtree) -> wholeSubtree;
v_scope(_Scope)       -> throw({error,concat(["unknown scope: ",_Scope])}).

v_deref(DR = neverDerefAliases)   -> DR;
v_deref(DR = derefInSearching)    -> DR;
v_deref(DR = derefFindingBaseObj) -> DR;
v_deref(DR = derefAlways )        -> DR.

v_bool(true)  -> true;
v_bool(false) -> false;
v_bool(_Bool) -> throw({error,concat(["not Boolean: ",_Bool])}).

v_size_limit(I) when is_integer(I), I>=0 -> I;
v_size_limit(_I) -> throw({error,concat(["size_limit not positive integer: ",_I])}).

v_timeout(I) when is_integer(I), I>=0 -> I;
v_timeout(_I) -> throw({error,concat(["timeout not positive integer: ",_I])}).

v_attributes(Attrs) ->
    F = fun(A) when is_list(A) -> A;
	   (A) -> throw({error,concat(["attribute not String: ",A])})
	end,
    lists:map(F,Attrs).


%%% --------------------------------------------------------------------
%%% Log routines. Call a user provided log routine F.
%%% --------------------------------------------------------------------

%log1(Data, Str, Args) -> log(Data, Str, Args, 1).
log2(Data, Str, Args) -> log(Data, Str, Args, 2).

log(Data, Str, Args, Level) when is_function(Data#eldap.log) ->
    catch (Data#eldap.log)(Level, Str, Args);
log(_, _, _, _) ->
    ok.


%%% --------------------------------------------------------------------
%%% Misc. routines
%%% --------------------------------------------------------------------

send(To,Msg) ->
    To ! {self(), Msg},
    ok.

recv(From)   ->
    receive
	{From, Msg} -> Msg;
	{'EXIT', From, Reason} ->
	    {error, {internal_error, Reason}}
    end.

ldap_closed_p(Data, Emsg) when Data#eldap.using_tls == true ->
    %% Check if the SSL socket seems to be alive or not
    case catch ssl:sockname(Data#eldap.fd) of
	{error, _} ->
	    _ = ssl:close(Data#eldap.fd),
	    {error, ldap_closed};
	{ok, _} ->
	    {error, Emsg};
	_ ->
	    %% sockname crashes if the socket pid is not alive
	    {error, ldap_closed}
    end;
ldap_closed_p(Data, Emsg) ->
    %% non-SSL socket
    case inet:port(Data#eldap.fd) of
	{error,_} -> {error, ldap_closed};
	_         -> {error,Emsg}
    end.

bump_id(Data) -> Data#eldap.id + 1.


%%% --------------------------------------------------------------------
%%% parse_dn/1  -  Implementation of RFC 2253:
%%%
%%%   "UTF-8 String Representation of Distinguished Names"
%%%
%%% Test cases:
%%%
%%%  The simplest case:
%%%
%%%  1> eldap:parse_dn("CN=Steve Kille,O=Isode Limited,C=GB").
%%%  {ok,[[{attribute_type_and_value,"CN","Steve Kille"}],
%%%       [{attribute_type_and_value,"O","Isode Limited"}],
%%%       [{attribute_type_and_value,"C","GB"}]]}
%%%
%%%  The first RDN is multi-valued:
%%%
%%%  2> eldap:parse_dn("OU=Sales+CN=J. Smith,O=Widget Inc.,C=US").
%%%  {ok,[[{attribute_type_and_value,"OU","Sales"},
%%%        {attribute_type_and_value,"CN","J. Smith"}],
%%%       [{attribute_type_and_value,"O","Widget Inc."}],
%%%       [{attribute_type_and_value,"C","US"}]]}
%%%
%%%  Quoting a comma:
%%%
%%%  3> eldap:parse_dn("CN=L. Eagle,O=Sue\\, Grabbit and Runn,C=GB").
%%%  {ok,[[{attribute_type_and_value,"CN","L. Eagle"}],
%%%       [{attribute_type_and_value,"O","Sue\\, Grabbit and Runn"}],
%%%       [{attribute_type_and_value,"C","GB"}]]}
%%%
%%%  A value contains a carriage return:
%%%
%%%  4> eldap:parse_dn("CN=Before
%%%  4> After,O=Test,C=GB").
%%%  {ok,[[{attribute_type_and_value,"CN","Before\nAfter"}],
%%%       [{attribute_type_and_value,"O","Test"}],
%%%       [{attribute_type_and_value,"C","GB"}]]}
%%%
%%%  5> eldap:parse_dn("CN=Before\\0DAfter,O=Test,C=GB").
%%%  {ok,[[{attribute_type_and_value,"CN","Before\\0DAfter"}],
%%%       [{attribute_type_and_value,"O","Test"}],
%%%       [{attribute_type_and_value,"C","GB"}]]}
%%%
%%%  An RDN in OID form:
%%%
%%%  6> eldap:parse_dn("1.3.6.1.4.1.1466.0=#04024869,O=Test,C=GB").
%%%  {ok,[[{attribute_type_and_value,"1.3.6.1.4.1.1466.0","#04024869"}],
%%%       [{attribute_type_and_value,"O","Test"}],
%%%       [{attribute_type_and_value,"C","GB"}]]}
%%%
%%%
%%% --------------------------------------------------------------------

-doc false.
parse_dn("") -> % empty DN string
    {ok,[]};
parse_dn([H|_] = Str) when H=/=$, -> % 1:st name-component !
    case catch parse_name(Str,[]) of
	{'EXIT',Reason} -> {parse_error,internal_error,Reason};
	Else            -> Else
    end.

parse_name("",Acc)  ->
    {ok,lists:reverse(Acc)};
parse_name([$,|T],Acc) -> % N:th name-component !
    parse_name(T,Acc);
parse_name(Str,Acc) ->
    {Rest,NameComponent} = parse_name_component(Str),
    parse_name(Rest,[NameComponent|Acc]).

parse_name_component(Str) ->
    parse_name_component(Str,[]).

parse_name_component(Str,Acc) ->
    case parse_attribute_type_and_value(Str) of
	{[$+|Rest], ATV} ->
	    parse_name_component(Rest,[ATV|Acc]);
	{Rest,ATV} ->
	    {Rest,lists:reverse([ATV|Acc])}
    end.

parse_attribute_type_and_value(Str) ->
    case parse_attribute_type(Str) of
	{_Rest,[]} ->
	    parse_error(expecting_attribute_type,Str);
	{Rest,Type} ->
	    Rest2 = parse_equal_sign(Rest),
	    {Rest3,Value} = parse_attribute_value(Rest2),
	    {Rest3,{attribute_type_and_value,Type,Value}}
    end.

-define(IS_ALPHA(X) , X>=$a,X=<$z;X>=$A,X=<$Z ).
-define(IS_DIGIT(X) , X>=$0,X=<$9 ).
-define(IS_SPECIAL(X) , X==$,;X==$=;X==$+;X==$<;X==$>;X==$#;X==$; ).
-define(IS_QUOTECHAR(X) , X=/=$\\,X=/=$" ).
-define(IS_STRINGCHAR(X) ,
	X=/=$,,X=/=$=,X=/=$+,X=/=$<,X=/=$>,X=/=$#,X=/=$;,?IS_QUOTECHAR(X) ).
-define(IS_HEXCHAR(X) , ?IS_DIGIT(X);X>=$a,X=<$f;X>=$A,X=<$F ).

parse_attribute_type([H|T]) when ?IS_ALPHA(H) ->
    %% NB: It must be an error in the RFC in the definition
    %% of 'attributeType', should be: (ALPHA *keychar)
    {Rest,KeyChars} = parse_keychars(T),
    {Rest,[H|KeyChars]};
parse_attribute_type([H|_] = Str) when ?IS_DIGIT(H) ->
    parse_oid(Str);
parse_attribute_type(Str) ->
    parse_error(invalid_attribute_type,Str).



%%% Is a hexstring !
parse_attribute_value([$#,X,Y|T]) when ?IS_HEXCHAR(X),?IS_HEXCHAR(Y) ->
    {Rest,HexString} = parse_hexstring(T),
    {Rest,[$#,X,Y|HexString]};
%%% Is a "quotation-sequence" !
parse_attribute_value([$"|T]) ->
    {Rest,Quotation} = parse_quotation(T),
    {Rest,[$"|Quotation]};
%%% Is a stringchar , pair or Empty !
parse_attribute_value(Str) ->
    parse_string(Str).

parse_hexstring(Str) ->
    parse_hexstring(Str,[]).

parse_hexstring([X,Y|T],Acc) when ?IS_HEXCHAR(X),?IS_HEXCHAR(Y) ->
    parse_hexstring(T,[Y,X|Acc]);
parse_hexstring(T,Acc) ->
    {T,lists:reverse(Acc)}.

parse_quotation([$"|T]) -> % an empty: ""  is ok !
    {T,[$"]};
parse_quotation(Str) ->
    parse_quotation(Str,[]).

%%% Parse to end of quotation
parse_quotation([$"|T],Acc) ->
    {T,lists:reverse([$"|Acc])};
parse_quotation([X|T],Acc) when ?IS_QUOTECHAR(X) ->
    parse_quotation(T,[X|Acc]);
parse_quotation([$\\,X|T],Acc) when ?IS_SPECIAL(X) ->
    parse_quotation(T,[X,$\\|Acc]);
parse_quotation([$\\,$\\|T],Acc) ->
    parse_quotation(T,[$\\,$\\|Acc]);
parse_quotation([$\\,$"|T],Acc) ->
    parse_quotation(T,[$",$\\|Acc]);
parse_quotation([$\\,X,Y|T],Acc) when ?IS_HEXCHAR(X),?IS_HEXCHAR(Y) ->
    parse_quotation(T,[Y,X,$\\|Acc]);
parse_quotation(T,_) ->
    parse_error(expecting_double_quote_mark,T).

parse_string(Str) ->
    parse_string(Str,[]).

parse_string("",Acc) ->
    {"",lists:reverse(Acc)};
parse_string([H|T],Acc) when ?IS_STRINGCHAR(H) ->
    parse_string(T,[H|Acc]);
parse_string([$\\,X|T],Acc) when ?IS_SPECIAL(X) -> % is a pair !
    parse_string(T,[X,$\\|Acc]);
parse_string([$\\,$\\|T],Acc)                   -> % is a pair !
    parse_string(T,[$\\,$\\|Acc]);
parse_string([$\\,$" |T],Acc)                   -> % is a pair !
    parse_string(T,[$" ,$\\|Acc]);
parse_string([$\\,X,Y|T],Acc) when ?IS_HEXCHAR(X),?IS_HEXCHAR(Y) -> % is a pair!
    parse_string(T,[Y,X,$\\|Acc]);
parse_string(T,Acc) ->
    {T,lists:reverse(Acc)}.

parse_equal_sign([$=|T]) -> T;
parse_equal_sign(T)      -> parse_error(expecting_equal_sign,T).

parse_keychars(Str) -> parse_keychars(Str,[]).

parse_keychars([H|T],Acc) when ?IS_ALPHA(H) -> parse_keychars(T,[H|Acc]);
parse_keychars([H|T],Acc) when ?IS_DIGIT(H) -> parse_keychars(T,[H|Acc]);
parse_keychars([$-|T],Acc)                  -> parse_keychars(T,[$-|Acc]);
parse_keychars(T,Acc)                       -> {T,lists:reverse(Acc)}.

parse_oid(Str) -> parse_oid(Str,[]).

parse_oid([H,$.|T], Acc) when ?IS_DIGIT(H) ->
    parse_oid(T,[$.,H|Acc]);
parse_oid([H|T], Acc) when ?IS_DIGIT(H) ->
    parse_oid(T,[H|Acc]);
parse_oid(T, Acc) ->
    {T,lists:reverse(Acc)}.

parse_error(Emsg,Rest) ->
    throw({parse_error,Emsg,Rest}).


%%% --------------------------------------------------------------------
%%% Parse LDAP url according to RFC 2255
%%%
%%% Test case:
%%%
%%%  2> eldap:parse_ldap_url("ldap://10.42.126.33:389/cn=Administrative%20CA,o=Post%20Danmark,c=DK?certificateRevokationList;binary").
%%%  {ok,{{10,42,126,33},389},
%%%      [[{attribute_type_and_value,"cn","Administrative%20CA"}],
%%%       [{attribute_type_and_value,"o","Post%20Danmark"}],
%%%       [{attribute_type_and_value,"c","DK"}]],
%%%      {attributes,["certificateRevokationList;binary"]}}
%%%
%%% --------------------------------------------------------------------

-doc false.
parse_ldap_url("ldap://" ++ Rest1 = Str) ->
    {Rest2,HostPort} = parse_hostport(Rest1),
    %% Split the string into DN and Attributes+etc
    {Sdn,Rest3} = split_string(rm_leading_slash(Rest2),$?),
    case parse_dn(Sdn) of
	{parse_error,internal_error,_Reason} ->
	    {parse_error,internal_error,{Str,[]}};
	{parse_error,Emsg,Tail} ->
	    Head = get_head(Str,Tail),
	    {parse_error,Emsg,{Head,Tail}};
	{ok,DN} ->
            %% We stop parsing here for now and leave
            %% 'scope', 'filter' and 'extensions' to
            %% be implemented later if needed.
	    {_Rest4,Attributes} = parse_attributes(Rest3),
	    {ok,HostPort,DN,Attributes}
    end.

rm_leading_slash([$/|Tail]) -> Tail;
rm_leading_slash(Tail)      -> Tail.

parse_attributes([$?|Tail]) ->
    case split_string(Tail,$?) of
        {[],Attributes} ->
	    {[],{attributes,string:lexemes(Attributes,",")}};
        {Attributes,Rest} ->
            {Rest,{attributes,string:lexemes(Attributes,",")}}
    end.

parse_hostport(Str) ->
    {HostPort,Rest} = split_string(Str,$/),
    case split_string(HostPort,$:) of
	{Shost,[]} ->
	    {Rest,{parse_host(Rest,Shost),?LDAP_PORT}};
	{Shost,[$:|Sport]} ->
	    {Rest,{parse_host(Rest,Shost),
		   parse_port(Rest,Sport)}}
    end.

parse_port(Rest,Sport) ->
    try	list_to_integer(Sport)
    catch _:_ -> parse_error(parsing_port,Rest)
    end.

parse_host(Rest,Shost) ->
    case catch validate_host(Shost) of
	{parse_error,Emsg,_} -> parse_error(Emsg,Rest);
	Host -> Host
    end.

validate_host(Shost) ->
    case inet_parse:address(Shost) of
	{ok,Host} -> Host;
	_ ->
	    case inet_parse:domain(Shost) of
		true -> Shost;
		_    -> parse_error(parsing_host,Shost)
	    end
    end.


split_string(Str,Key) ->
    Pred = fun(X) when X==Key -> false; (_) -> true end,
    lists:splitwith(Pred, Str).

get_head(Str,Tail) ->
    get_head(Str,Tail,[]).

%%% Should always succeed !
get_head([H|Tail],Tail,Rhead) -> lists:reverse([H|Rhead]);
get_head([H|Rest],Tail,Rhead) -> get_head(Rest,Tail,[H|Rhead]).

%%% --------------------------------------------------------------------
%%% Return a paged result control as described by RFC2696
%%% https://www.rfc-editor.org/rfc/rfc2696.txt
%%% --------------------------------------------------------------------

-doc """
Paged results is an extension to the LDAP protocol specified by RFC2696

This function creates a control with the specified page size for use in
`search/3`, for example:

```erlang
Control = eldap:paged_result_control(50),
{ok, SearchResults} = search(Handle, [{base, "dc=example, dc=com"}], [Control]),
```
""".
-doc(#{since => <<"OTP 24.3">>}).
-spec paged_result_control(PageSize) ->
    {control, ControlOID, true, binary()} when
    PageSize :: integer(),
    ControlOID :: string().
paged_result_control(PageSize) when is_integer(PageSize) ->
    paged_result_control(PageSize, "").

-doc """
Paged results is an extension to the LDAP protocol specified by RFC2696

This function creates a control with the specified page size and cookie for use
in `search/3` to retrieve the next results page.

For example:

```erlang
PageSize = 50,
Control1 = eldap:paged_result_control(PageSize),
{ok, SearchResults1} = search(Handle, [{base, "dc=example, dc=com"}], [Control1]),
%% retrieve the returned cookie from the search results
{ok, Cookie1} = eldap:paged_result_cookie(SearchResults1),
Control2 = eldap:paged_result_control(PageSize, Cookie1),
{ok, SearchResults2} = eldap:search(Handle, [{base, "dc=example,dc=com"}], [Control2]),
%% etc
```
""".
-doc(#{since => <<"OTP 24.3">>}).
-spec paged_result_control(PageSize, Cookie) ->
   {control, ControlOID, true, binary()} when
    PageSize :: integer(),
    Cookie :: string(),
    ControlOID :: string().
paged_result_control(PageSize, Cookie) when is_integer(PageSize) ->
    RSCV = #'RealSearchControlValue'{size=PageSize, cookie=Cookie},
    {ok, ControlValue} = 'ELDAPv3':encode('RealSearchControlValue', RSCV),

    {control, "1.2.840.113556.1.4.319", true, ControlValue}.


%%% --------------------------------------------------------------------
%%% Extract the returned cookie from search results in order to
%%% retrieve the next set of results from the server according to
%%% RFC2696
%%%
%%% https://www.rfc-editor.org/rfc/rfc2696.txt
%%% --------------------------------------------------------------------

-doc """
paged_result_cookie(SearchResult)

Paged results is an extension to the LDAP protocol specified by RFC2696.

This function extracts the cookie returned from the server as a result of a
paged search result.

If the returned cookie is the empty string `""`, then these search results
represent the last in the series.
""".
-doc(#{since => <<"OTP 24.3">>}).
-spec paged_result_cookie(#eldap_search_result{controls :: maybe_improper_list(),
                                         entries :: term(),
                                         referrals :: term()}) ->
    {error, no_cookie} |
    {ok, asn1_NOVALUE |
     [[any()] | byte() | {_, _} | {_, _, _}] |
     #'AttributeValueAssertion'{attributeDesc :: [any()],
				assertionValue :: [any()]}}.

paged_result_cookie(#eldap_search_result{controls=Controls}) ->
    find_paged_result_cookie(Controls).

find_paged_result_cookie([]) ->
    {error, no_cookie};

find_paged_result_cookie([C|Controls]) ->
    case C of
        #'Control'{controlType="1.2.840.113556.1.4.319",controlValue=ControlValue} ->
            {ok, #'RealSearchControlValue'{cookie=Cookie}} =
                'ELDAPv3':decode('RealSearchControlValue', ControlValue),
            {ok, Cookie};
        _ ->
            find_paged_result_cookie(Controls)
    end.
