%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2019. All Rights Reserved.
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

-module(net).

%% We should really ifdef this module depending on if we actually built
%% the system with esock support (socket and prim_net), but our doc-building
%% can't handle the "variables" we need (USE_ESOCK). So instead, we just
%% leave everything hanging...
%% If one of the "hanging" functions is called when esock has been disabled,
%% the function will through a 'notsup' error (erlang:error/1).

%% Administrative and utility functions
-export([
	 info/0,
         command/1
        ]).

-export([
         gethostname/0,
         getnameinfo/1, getnameinfo/2,
         getaddrinfo/1, getaddrinfo/2,

         if_name2index/1,
         if_index2name/1,
         if_names/0
        ]).

%% Deprecated functions from the "old" net module
-export([call/4,
	 cast/4,
	 broadcast/3,
	 ping/1,
	 relay/1,
	 sleep/1]).

%% Should we define these here or refer to the prim_net module
-export_type([
              address_info/0,
              name_info/0,

              name_info_flags/0,
              name_info_flag/0,
              name_info_flag_ext/0,

              network_interface_name/0,
              network_interface_index/0
             ]).


-deprecated({call,      4, eventually}).
-deprecated({cast,      4, eventually}).
-deprecated({broadcast, 3, eventually}).
-deprecated({ping,      1, eventually}).
-deprecated({relay,     1, eventually}).
-deprecated({sleep,     1, eventually}).


-type name_info_flags()         :: [name_info_flag()|name_info_flag_ext()].
-type name_info_flag()          :: namereqd |
                                   dgram |
                                   nofqdn |
                                   numerichost |
                                   nomericserv.
-type name_info_flag_ext()      :: idn |
                                   idna_allow_unassigned |
                                   idna_use_std3_ascii_rules.
-type name_info()               :: #{host    := string(),
                                     service := string()}.
-type address_info()            :: #{family   := socket:domain(),
                                     socktype := socket:type(),
                                     protocol := socket:protocol(),
                                     address  := socket:sockaddr()}.
-type network_interface_name()  :: string().
-type network_interface_index() :: non_neg_integer().


%% ===========================================================================
%%
%% D E P R E C A T E D   F U N C T I O N S
%%
%% ===========================================================================

call(N,M,F,A) -> rpc:call(N,M,F,A).
cast(N,M,F,A) -> rpc:cast(N,M,F,A).
broadcast(M,F,A) -> rpc:eval_everywhere(M,F,A).
ping(Node) -> net_adm:ping(Node).
sleep(T) -> receive after T -> ok end.
relay(X) -> slave:relay(X).


%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

-spec info() -> list().

-ifdef(USE_ESOCK).
info() ->
    prim_net:info().
-else.
-dialyzer({nowarn_function, info/0}).
info() ->
    erlang:error(notsup).
-endif.


-spec command(Cmd :: term()) -> term().

-ifdef(USE_ESOCK).
command(Cmd) ->
    prim_net:command(Cmd).
-else.
-dialyzer({nowarn_function, command/1}).
command(_Cmd) ->
    erlang:error(notsup).
-endif.



%% ===========================================================================
%%
%% The proper net API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% gethostname - Get the name of the current host.
%%
%%

-spec gethostname() -> {ok, HostName} | {error, Reason} when
      HostName :: string(),
      Reason   :: term().

-ifdef(USE_ESOCK).
gethostname() ->
    prim_net:gethostname().
-else.
-dialyzer({nowarn_function, gethostname/0}).
gethostname() ->
    erlang:error(notsup).
-endif.


%% ===========================================================================
%%
%% getnameinfo - Address-to-name translation in protocol-independent manner.
%%
%%

-spec getnameinfo(SockAddr) -> {ok, Info} | {error, Reason} when
      SockAddr :: socket:sockaddr(),
      Info     :: name_info(),
      Reason   :: term().

getnameinfo(SockAddr) ->
    getnameinfo(SockAddr, undefined).

-spec getnameinfo(SockAddr, Flags) -> {ok, Info} | {error, Reason} when
      SockAddr :: socket:sockaddr(),
      Flags    :: name_info_flags() | undefined,
      Info     :: name_info(),
      Reason   :: term().

-ifdef(USE_ESOCK).
getnameinfo(SockAddr, [] = _Flags) ->
    getnameinfo(SockAddr, undefined);
getnameinfo(#{family := Fam, addr := _Addr} = SockAddr, Flags)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) andalso 
       (is_list(Flags) orelse (Flags =:= undefined)) ->
    prim_net:getnameinfo(socket:ensure_sockaddr(SockAddr), Flags);
getnameinfo(#{family := Fam, path := _Path} = SockAddr, Flags)
  when (Fam =:= local) andalso (is_list(Flags) orelse (Flags =:= undefined)) ->
    prim_net:getnameinfo(SockAddr, Flags).
-else.
-dialyzer({nowarn_function, getnameinfo/2}).
getnameinfo(SockAddr, [] = _Flags) ->
    getnameinfo(SockAddr, undefined);
getnameinfo(#{family := Fam, addr := _Addr} = _SockAddr, Flags)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) andalso 
       (is_list(Flags) orelse (Flags =:= undefined)) ->
    erlang:error(notsup);
getnameinfo(#{family := Fam, path := _Path} = _SockAddr, Flags)
  when (Fam =:= local) andalso (is_list(Flags) orelse (Flags =:= undefined)) ->
    erlang:error(notsup).
-endif.


%% ===========================================================================
%%
%% getaddrinfo - Network address and service translation
%%
%% There is also a "hint" argument that we "at some point" should implement.

-spec getaddrinfo(Host) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Info    :: [address_info()],
      Reason  :: term().

getaddrinfo(Host) when is_list(Host) ->
    getaddrinfo(Host, undefined).


-spec getaddrinfo(Host, undefined) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Info    :: [address_info()],
      Reason  :: term()
                 ; (undefined, Service) -> {ok, Info} | {error, Reason} when
      Service :: string(),
      Info    :: [address_info()],
      Reason  :: term()
                 ; (Host, Service) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Service :: string(),
      Info    :: [address_info()],
      Reason  :: term().

-ifdef(USE_ESOCK).
getaddrinfo(Host, Service)
  when (is_list(Host) orelse (Host =:= undefined)) andalso
       (is_list(Service) orelse (Service =:= undefined)) andalso
       (not ((Service =:= undefined) andalso (Host =:= undefined))) ->
    prim_net:getaddrinfo(Host, Service).
-else.
-dialyzer({nowarn_function, getaddrinfo/2}).
getaddrinfo(Host, Service)
  when (is_list(Host) orelse (Host =:= undefined)) andalso
       (is_list(Service) orelse (Service =:= undefined)) andalso
       (not ((Service =:= undefined) andalso (Host =:= undefined))) ->
    erlang:error(notsup).
-endif.




%% ===========================================================================
%%
%% if_name2index - Mappings between network interface names and indexes:
%%                 name -> idx
%%
%%

-spec if_name2index(Name) -> {ok, Idx} | {error, Reason} when
      Name   :: network_interface_name(),
      Idx    :: network_interface_index(),
      Reason :: term().

-ifdef(USE_ESOCK).
if_name2index(If) when is_list(If) ->
    prim_net:if_name2index(If).
-else.
-dialyzer({nowarn_function, if_name2index/1}).
if_name2index(If) when is_list(If) ->
    erlang:error(notsup).
-endif.



%% ===========================================================================
%%
%% if_index2name - Mappings between network interface index and names:
%%                 idx -> name
%%
%%

-spec if_index2name(Idx) -> {ok, Name} | {error, Reason} when
      Idx    :: network_interface_index(),
      Name   :: network_interface_name(),
      Reason :: term().

-ifdef(USE_ESOCK).
if_index2name(Idx) when is_integer(Idx) ->
    prim_net:if_index2name(Idx).
-else.
-dialyzer({nowarn_function, if_index2name/1}).
if_index2name(Idx) when is_integer(Idx) ->
    erlang:error(notsup).
-endif.



%% ===========================================================================
%%
%% if_names - Get network interface names and indexes
%%
%%

-spec if_names() -> Names | {error, Reason} when
      Names  :: [{Idx, If}],
      Idx    :: network_interface_index(),
      If     :: network_interface_name(),
      Reason :: term().

-ifdef(USE_ESOCK).
if_names() ->
    prim_net:if_names().
-else.
-dialyzer({nowarn_function, if_names/0}).
if_names() ->
    erlang:error(notsup).
-endif.


