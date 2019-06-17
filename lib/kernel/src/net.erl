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

-ifdef(USE_ESOCK).
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
-endif.

%% Deprecated functions from the "old" net module
-export([call/4,
	 cast/4,
	 broadcast/3,
	 ping/1,
	 relay/1,
	 sleep/1]).

-ifdef(USE_ESOCK).

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
-endif.

-deprecated({call,      4, eventually}).
-deprecated({cast,      4, eventually}).
-deprecated({broadcast, 3, eventually}).
-deprecated({ping,      1, eventually}).
-deprecated({relay,     1, eventually}).
-deprecated({sleep,     1, eventually}).


-ifdef(USE_ESOCK).

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

-endif.


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


-ifdef(USE_ESOCK).

%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

-spec info() -> list().

info() ->
    prim_net:info().


-spec command(Cmd :: term()) -> term().

command(Cmd) ->
    prim_net:command(Cmd).



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

gethostname() ->
    prim_net:gethostname().


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

getnameinfo(SockAddr, [] = _Flags) ->
    getnameinfo(SockAddr, undefined);
getnameinfo(#{family := Fam, addr := _Addr} = SockAddr, Flags)
  when ((Fam =:= inet) orelse (Fam =:= inet6)) andalso 
       (is_list(Flags) orelse (Flags =:= undefined)) ->
    prim_net:getnameinfo(socket:ensure_sockaddr(SockAddr), Flags);
getnameinfo(#{family := Fam, path := _Path} = SockAddr, Flags)
  when (Fam =:= local) andalso (is_list(Flags) orelse (Flags =:= undefined)) ->
    prim_net:getnameinfo(SockAddr, Flags).



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

getaddrinfo(Host, Service)
  when (is_list(Host) orelse (Host =:= undefined)) andalso
       (is_list(Service) orelse (Service =:= undefined)) andalso
       (not ((Service =:= undefined) andalso (Host =:= undefined))) ->
    prim_net:getaddrinfo(Host, Service).



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

if_name2index(If) when is_list(If) ->
    prim_net:if_name2index(If).



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

if_index2name(Idx) when is_integer(Idx) ->
    prim_net:if_index2name(Idx).



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

if_names() ->
    prim_net:if_names().



-endif.
