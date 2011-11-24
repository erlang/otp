%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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

-module(diameter_peer).

-behaviour(gen_server).

%% Interface towards transport modules ...
-export([recv/2,
         up/1,
         up/2]).

%% ... and the stack.
-export([start/3,
         send/2,
         close/1,
         abort/1,
         notify/2]).

%% Server start.
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

%% debug
-export([state/0,
         uptime/0]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

%% Registered name of the server.
-define(SERVER, ?MODULE).

%% Server state.
-record(state, {id = now()}).

%%% ---------------------------------------------------------------------------
%%% # notify/2
%%% ---------------------------------------------------------------------------

notify(SvcName, T) ->
    rpc:abcast(nodes(), ?SERVER, {notify, SvcName, T}).

%%% ---------------------------------------------------------------------------
%%% # start/3
%%% ---------------------------------------------------------------------------

start(T, Opts, #diameter_service{} = Svc) ->
    {Mod, Cfg} = split_transport(Opts),
    apply(Mod, start, [T, Svc, Cfg]).

%%% ---------------------------------------------------------------------------
%%% # up/[12]
%%% ---------------------------------------------------------------------------

up(Pid) ->  %% accepting transport
    ifc_send(Pid, {self(), connected}).

up(Pid, Remote) ->  %% connecting transport
    ifc_send(Pid, {self(), connected, Remote}).

%%% ---------------------------------------------------------------------------
%%% # recv/2
%%% ---------------------------------------------------------------------------

recv(Pid, Pkt) ->
    ifc_send(Pid, {recv, Pkt}).

%%% ---------------------------------------------------------------------------
%%% # send/2
%%% ---------------------------------------------------------------------------

send(Pid, #diameter_packet{transport_data = undefined,
			   bin = Bin}) ->
    send(Pid, Bin);

send(Pid, Pkt) ->
    ifc_send(Pid, {send, Pkt}).

%%% ---------------------------------------------------------------------------
%%% # close/1
%%% ---------------------------------------------------------------------------

close(Pid) ->
    ifc_send(Pid, {close, self()}).

%%% ---------------------------------------------------------------------------
%%% # abort/1
%%% ---------------------------------------------------------------------------

abort(Pid) ->
    exit(Pid, shutdown).

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

start_link() ->
    ServerName = {local, ?SERVER},
    Module     = ?MODULE,
    Args       = [],
    Options    = [{spawn_opt, diameter_lib:spawn_opts(server, [])}],
    gen_server:start_link(ServerName, Module, Args, Options).

state() ->
    call(state).

uptime() ->
    call(uptime).

%%% ----------------------------------------------------------
%%% # init(Role)
%%% ----------------------------------------------------------

init([]) ->
    {ok, #state{}}.

%%% ----------------------------------------------------------
%%% # handle_call(Request, From, State)
%%% ----------------------------------------------------------

handle_call(state, _, State) ->
    {reply, State, State};

handle_call(uptime, _, #state{id = Time} = State) ->
    {reply, diameter_lib:now_diff(Time), State};

handle_call(Req, From, State) ->
    ?UNEXPECTED([Req, From]),
    {reply, nok, State}.

%%% ----------------------------------------------------------
%%% # handle_cast(Request, State)
%%% ----------------------------------------------------------

handle_cast(Msg, State) ->
    ?UNEXPECTED([Msg]),
    {noreply, State}.

%%% ----------------------------------------------------------
%%% # handle_info(Request, State)
%%% ----------------------------------------------------------

%% Remote service is distributing a message.
handle_info({notify, SvcName, T}, S) ->
    bang(diameter_service:whois(SvcName), T),
    {noreply, S};

handle_info(Info, State) ->
    ?UNEXPECTED([Info]),
    {noreply, State}.

%% ----------------------------------------------------------
%% terminate(Reason, State)
%% ----------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%% ----------------------------------------------------------
%% code_change(OldVsn, State, Extra)
%% ----------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------
%% INTERNAL FUNCTIONS
%% ---------------------------------------------------------

%% ifc_send/2
%%
%% Send something over the transport interface.

ifc_send(Pid, T) ->
    Pid ! {diameter, T}.

%% bang/2

bang(undefined = No, _) ->
    No;
bang(Pid, T) ->
    Pid ! T.

%% split_transport/1
%%
%% Split options into transport module, transport config and
%% remaining options.

split_transport(Opts) ->
    {[M,C], _} = proplists:split(Opts, [transport_module,
                                        transport_config]),
    {value(M, diameter_tcp), value(C, [])}.

value([{_,V}], _) ->
    V;
value([], V) ->
    V.

%% call/1

call(Request) ->
    gen_server:call(?SERVER, Request, infinity).
