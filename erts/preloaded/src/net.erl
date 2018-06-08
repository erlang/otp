%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

%% Administrative and "global" utility functions
-export([
	 on_load/0, on_load/1, on_load/2,
	 info/0
        ]).

-export([
         getnameinfo/1, getnameinfo/2,
         getaddrinfo/2,

         if_name2index/1,
         if_index2name/1,
         if_names/0
        ]).

-export_type([
              ip_address/0,
              ip4_address/0,
              ip6_address/0,
              in_sockaddr/0,
              in4_sockaddr/0,
              in6_sockaddr/0,
              port_number/0,

              address_info/0,
              name_info/0,

              name_info_flags/0,
              name_info_flag/0,
              name_info_flag_ext/0,

              network_interface_name/0,
              network_interface_index/0
             ]).


%% Many of these should be moved to the socket module.
-type ip_address()  :: ip4_address() | ip6_address().
-type ip4_address() :: {0..255, 0..255, 0..255, 0..255}.
-type ip6_address() ::
           {0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535,
            0..65535}.
-type uint20()        :: 0..16#FFFFF.
-type uint32()        :: 0..16#FFFFFFFF.
-type in6_flow_info() :: uint20().
-type in6_scope_id()  :: uint32().
-record(in4_sockaddr, {port = 0   :: port_number(),
                       addr = any :: any | loopback | ip4_address()}).
-type in4_sockaddr() :: #in4_sockaddr{}.
-record(in6_sockaddr, {port     = 0   :: port_number(),
                       addr     = any :: any | loopback | ip6_address(),
                       flowinfo = 0   :: in6_flow_info(),
                       scope_id = 0   :: in6_scope_id()}).
-type in6_sockaddr() :: #in6_sockaddr{}.

-type in_sockaddr() :: in4_sockaddr() | in6_sockaddr().

-type port_number() :: 0..65535.

-type name_info_flags()         :: [name_info_flag()|name_info_flag_ext()].
-type name_info_flag()          :: namereqd |
                                   dgram |
                                   nofqdn |
                                   numerichost |
                                   nomericserv.
-type name_info_flag_ext()      :: idn |
                                   idna_allow_unassigned |
                                   idna_use_std3_ascii_rules.
-record(name_info,    {host, service}).
-type name_info()               :: #name_info{}.
-record(address_info, {family, socktype, protocol, addr}).
-type address_info()            :: #address_info{}.
-type network_interface_name()  :: string().
-type network_interface_index() :: non_neg_integer().



%% ===========================================================================
%%
%% Administrative and utility API
%%
%% ===========================================================================

-spec on_load() -> ok.

%% Should we require that the Extra arg is a map?
on_load() ->
    on_load(#{}).

-spec on_load(Extra) -> ok when
      Extra :: maps:map().

on_load(Extra) when is_map(Extra) ->
    on_load(atom_to_list(?MODULE), Extra).

-spec on_load(Path, Extra) -> ok when
      Path  :: string(),
      Extra :: maps:map().

on_load(Path, Extra) when is_list(Path) andalso is_map(Extra) ->
    on_load(nif_is_loaded(), Path, Extra).

on_load(true, _Path, _Extra) ->
    ok;
on_load(false, Path, Extra) ->
    ok = erlang:load_nif(Path, Extra).



-spec info() -> list().

info() ->
    nif_info().



%% ===========================================================================
%%
%% The proper net API
%%
%% ===========================================================================

%% ===========================================================================
%%
%% getnameinfo - Address-to-name translation in protocol-independent manner.
%%
%%

-spec getnameinfo(SockAddr) -> {ok, Info} | {error, Reason} when
      SockAddr :: in_sockaddr(),
      Info     :: name_info(),
      Reason   :: term().

getnameinfo(SockAddr)
  when is_record(SockAddr, in4_sockaddr) orelse
       is_record(SockAddr, in6_sockaddr) ->
    getnameinfo(SockAddr, []).

-spec getnameinfo(SockAddr, Flags) -> {ok, Info} | {error, Reason} when
      SockAddr :: in_sockaddr(),
      Flags    :: name_info_flags(),
      Info     :: name_info(),
      Reason   :: term().

getnameinfo(SockAddr, Flags)
  when (is_record(SockAddr, in4_sockaddr) orelse
        is_record(SockAddr, in6_sockaddr)) andalso
       is_list(Flags) ->
    nif_getnameinfo(SockAddr, Flags).


%% ===========================================================================
%%
%% getaddrinfo - Network address and service translation
%%
%% There is also a "hint" argument that we "at some point" should implement.

-spec getaddrinfo(Host, Service) -> {ok, Info} | {error, Reason} when
      Host    :: string(),
      Service :: string(),
      Info    :: [address_info()],
      Reason  :: term().

getaddrinfo(Host, Service)
  when (is_list(Host) orelse (Host =:= undefined)) andalso
       (is_list(Service) orelse (Service =:= undefined)) andalso
       (not ((Service =:= undefined) andalso (Host =:= undefined))) ->
    nif_getaddrinfo(Host, Service, undefined).



%% ===========================================================================
%%
%% if_name2index - Mappings between network interface names and indexes:
%%                 name -> idx
%%
%%

-spec if_name2index(Name) -> {ok, Idx} | {error, Reason} when
      Name   :: string(),
      Idx    :: non_neg_integer(),
      Reason :: term().

if_name2index(If) when is_list(If) ->
    nif_if_name2index(If).



%% ===========================================================================
%%
%% if_index2name - Mappings between network interface names and indexes:
%%                 idx -> name
%%
%%

-spec if_index2name(Idx) -> {ok, Name} | {error, Reason} when
      Idx    :: non_neg_integer(),
      Name   :: string(),
      Reason :: term().

if_index2name(Idx) when is_integer(Idx) ->
    nif_if_index2name(Idx).



%% ===========================================================================
%%
%% if_names - Get network interface names and indexes
%%
%%

-spec if_names() -> Names | {error, Reason} when
      Names  :: [{Idx, If}],
      Idx    :: non_neg_integer(),
      If     :: string(),
      Reason :: term().

if_names() ->
    nif_if_names().



%% ===========================================================================
%%
%% Misc utility functions
%%
%% ===========================================================================

%% formated_timestamp() ->
%%     format_timestamp(os:timestamp()).

%% format_timestamp(Now) ->
%%     N2T = fun(N) -> calendar:now_to_local_time(N) end,
%%     format_timestamp(Now, N2T, true).

%% format_timestamp({_N1, _N2, N3} = N, N2T, true) ->
%%     FormatExtra = ".~.2.0w",
%%     ArgsExtra   = [N3 div 10000],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra);
%% format_timestamp({_N1, _N2, _N3} = N, N2T, false) ->
%%     FormatExtra = "",
%%     ArgsExtra   = [],
%%     format_timestamp(N, N2T, FormatExtra, ArgsExtra).

%% format_timestamp(N, N2T, FormatExtra, ArgsExtra) ->
%%     {Date, Time}   = N2T(N),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate =
%%         io_lib:format("~.4w-~.2.0w-~.2.0w ~.2.0w:~.2.0w:~.2.0w" ++ FormatExtra,
%%                       [YYYY, MM, DD, Hour, Min, Sec] ++ ArgsExtra),
%%     lists:flatten(FormatDate).


%% ===========================================================================
%%
%% The actual NIF-functions.
%%
%% ===========================================================================

nif_is_loaded() ->
    false.

nif_info() ->
    erlang:error(badarg).

nif_getnameinfo(_Addr, _Flags) ->
    erlang:error(badarg).

nif_getaddrinfo(_Host, _Service, _Hints) ->
    erlang:error(badarg).

nif_if_name2index(_Name) ->
    erlang:error(badarg).

nif_if_index2name(_Id) ->
    erlang:error(badarg).

nif_if_names() ->
    erlang:error(badarg).
