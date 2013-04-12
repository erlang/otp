%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%% A library module that factors out commonality in the example
%% Diameter peers.
%%

-module(peer).

-include_lib("diameter/include/diameter.hrl").
-include_lib("diameter/include/diameter_gen_base_rfc3588.hrl").

-export([start/2,
         listen/2,
         connect/2,
         stop/1]).

-type service_name()
   :: term().

-type protocol()
   :: tcp | sctp.

-type ip_address()
   :: default
    | inet:ip_address().

-type server_config()
   :: protocol()
    | {protocol(), ip_address(), non_neg_integer()}.

-type client_config()
   :: protocol()
    | {protocol(), ip_address(), non_neg_integer()}
    | {protocol(), ip_address(), ip_address(), non_neg_integer()}.

-define(DEFAULT_PORT, 3868).

%% ---------------------------------------------------------------------------
%% Interface functions
%% ---------------------------------------------------------------------------

%% start/2

-spec start(service_name(), list())
   -> ok
    | {error, term()}.

start(Name, Opts)
  when is_atom(Name), is_list(Opts) ->
    diameter:start_service(Name, Opts).

%% connect/2

-spec connect(service_name(), client_config())
   -> {ok, reference()}
    | {error, term()}.

connect(Name, T) ->
    diameter:add_transport(Name, {connect, [{reconnect_timer, 5000}
                                            | client(T)]}).

%% listen/2

-spec listen(service_name(), server_config())
   -> {ok, reference()}
    | {error, term()}.

listen(Name, T) ->
    diameter:add_transport(Name, {listen, server(T)}).

%% stop/1

-spec stop(service_name())
   -> ok
    | {error, term()}.

stop(Name) ->
    diameter:stop_service(Name).

%% ---------------------------------------------------------------------------
%% Internal functions
%% ---------------------------------------------------------------------------

%% server/1
%%
%% Return config for a listening transport.

server({T, Addr, Port}) ->
    [{transport_module, tmod(T)},
     {transport_config, [{reuseaddr, true},
                         {ip, addr(Addr)},
                         {port, Port}]}];

server(T) ->
    server({T, loopback, ?DEFAULT_PORT}).

%% client/1
%%
%% Return config for a connecting transport.

client({all, LA, RA, RP}) ->
    [[M,{K,C}], T]
        = [client({P, LA, RA, RP}) || P <- [sctp,tcp]],
    [M, {K,C,2000} | T];

client({T, LA, RA, RP}) ->
    [{transport_module, tmod(T)},
     {transport_config, [{raddr, addr(RA)},
                         {rport, RP},
                         {reuseaddr, true}
                         | ip(LA)]}];

client({T, RA, RP}) ->
    client({T, default, RA, RP});

client(T) ->
    client({T, loopback, loopback, ?DEFAULT_PORT}).

tmod(tcp)  -> diameter_tcp;
tmod(sctp) -> diameter_sctp.

ip(default) ->
    [];
ip(loopback) ->
    [{ip, {127,0,0,1}}];
ip(Addr) ->
    [{ip, Addr}].

addr(loopback) ->
    {127,0,0,1};
addr(A) ->
    A.
