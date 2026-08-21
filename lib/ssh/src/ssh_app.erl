%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

%%

%%%=========================================================================
%%% Purpose : Application master and top supervisors for SSH.
%%%
%%%  -----> ssh_sup -----+-----> sshc_sup --+--> "connection sup" (etc)
%%%                      |                  |
%%%                      |                  +--> "connection sup" (etc)
%%%                      |                  :
%%%                      |                  +--> "connection sup" (etc)
%%%                      |
%%%                      +-----> sshd_sup --+--> "lsocket sup" (etc)
%%%                                         |
%%%                                         +--> "system sup" (etc)
%%%                                         :
%%%                                         +--> "system sup" (etc)

-module(ssh_app).
-moduledoc false.

-behaviour(application).
-behaviour(supervisor).

%% 'application' export:
-export([start/2, stop/1]).

%% 'supervisor' export:
-export([init/1]).


%%%=========================================================================
%%%  Application callback
%%%=========================================================================
start(_Type, _State) ->
    supervisor:start_link({local,ssh_sup}, ?MODULE, [ssh_sup]).

stop(_State) ->
    ok.

%%%=========================================================================
%%%  Supervisor callback
%%%=========================================================================
init([ssh_sup]) ->
    add_logger_filter(),
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600},
    ChildSpecs = [#{id       => SupName,
                    start    => {supervisor, start_link,
                                 [{local,SupName}, ?MODULE, [SupName]]},
                    type     => supervisor}
                  || SupName <- [sshd_sup, sshc_sup]],
    {ok, {SupFlags,ChildSpecs}};

init([sshd_sup]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600},
    ChildSpecs = [#{id       => ssh_lsocket_sup,
                    start    => {ssh_lsocket_sup, start_link, []},
                    type     => supervisor}],
    {ok, {SupFlags,ChildSpecs}};
init([sshc_sup]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity =>   10,
                 period    => 3600},
    ChildSpecs = [],
    {ok, {SupFlags,ChildSpecs}}.


%%%================================================================
add_logger_filter() ->
    DefAct = application:get_env(ssh, default_filter, rm),
    DefF = start_link,
    ModulesActions =
        lists:map(fun(M) when is_atom(M) ->
                          {M,{DefF,DefAct}};

                     ({M,Act}) when is_atom(M),
                                    (Act == rm orelse
                                     Act == filter) ->
                          {M,{DefF,Act}};

                     ({M,F}) when is_atom(M), is_atom(F) ->
                          {M,{F,DefAct}};

                     ({M,F,Act}) when is_atom(M), is_atom(F),
                                      (Act == rm orelse
                                       Act == filter) ->
                          {M,{F,Act}}
                  end, application:get_env(ssh, filter_modules, [])),
    logger:add_primary_filter(ssh_filter, {fun ssh_filter/2, ModulesActions}).


ssh_filter(Ev = #{msg := {report, R=#{report := Rep}}},
           ModulesActions = [_|_]) when is_list(Rep) ->
    %% io:format("Ev = ~p~n", [Ev]),
    try
        Ev#{msg := {report, R#{report := remove_sensitive(Rep, ModulesActions)}}}
    catch
        throw:{ssh_filter_return,Ret} -> 
            %% io:format("ssh_filter returns ~p~n", [Ret]),
            Ret;
        _C:_E ->
            %% io:format("*** ~p ~p~n", [_C,_E]),
            stop
    end;
ssh_filter(OtherEv, _) ->
    %% io:format("OtherEv = ~p~n", [OtherEv]),
    OtherEv.


remove_sensitive(L, ModActs) when is_list(L) -> rs(L, ModActs);
remove_sensitive(_, _) -> throw({ssh_filter_return,ignore}).


rs([{K,V0}|T], ModActs) when is_list(V0) ->
    case proplists:get_value(mfargs, V0) of
        {M,F,A} ->
            MFA1 = filter(proplists:get_value(M,ModActs), {M,F,A}),
            V = lists:keyreplace(mfargs, 1, V0, {mfargs,MFA1}),
            [{K,V} | T];
        _ ->
            [{K,V0} | rs(T,ModActs)]
    end;

rs([H|T], ModActs) ->
    [H | rs(T,ModActs)];

rs(Other, _) ->
    Other.


filter({F,Act}, {M,F,A}) -> {M, F, ssh_options:no_sensitive(Act,A)};
filter(stop, _) -> throw({ssh_filter_return,stop});
filter(_, _) -> throw({ssh_filter_return,ignore}).

