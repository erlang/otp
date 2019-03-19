%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018-2019. All Rights Reserved.
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

-compile(no_native).

%% Administrative and "global" utility functions
-export([
	 on_load/0, on_load/1,
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

-spec on_load() -> ok.

%% Should we require that the Extra arg is a map?
on_load() ->
    on_load(#{}).

-spec on_load(Extra) -> ok when
      Extra :: map().

on_load(Extra) ->
    ok = erlang:load_nif(atom_to_list(?MODULE), Extra).


-spec info() -> list().

info() ->
    nif_info().


-spec command(Cmd :: term()) -> term().

command(Cmd) ->
    nif_command(Cmd).



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
    nif_gethostname().


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
    nif_getnameinfo(socket:ensure_sockaddr(SockAddr), Flags);
getnameinfo(#{family := Fam, path := _Path} = SockAddr, Flags)
  when (Fam =:= local) andalso (is_list(Flags) orelse (Flags =:= undefined)) ->
    nif_getnameinfo(SockAddr, Flags).



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
    nif_getaddrinfo(Host, Service, undefined).



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
    nif_if_name2index(If).



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
    nif_if_index2name(Idx).



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

nif_info() ->
    erlang:nif_error(undef).

nif_command(_Cmd) ->
    erlang:nif_error(undef).

nif_gethostname() ->
    erlang:nif_error(undef).

nif_getnameinfo(_Addr, _Flags) ->
    erlang:nif_error(undef).

nif_getaddrinfo(_Host, _Service, _Hints) ->
    erlang:nif_error(undef).

nif_if_name2index(_Name) ->
    erlang:nif_error(undef).

nif_if_index2name(_Id) ->
    erlang:nif_error(undef).

nif_if_names() ->
    erlang:nif_error(undef).

