%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2011-2025. All Rights Reserved.
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
-module(pubkey_ssh).
-moduledoc false.

-include("pubkey_moduli.hrl").

-export([dh_gex_group/4, 
	 dh_gex_group_sizes/0
	]).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
%% Description: Returns Generator and Modulus given MinSize, WantedSize
%%              and MaxSize
%%--------------------------------------------------------------------
dh_gex_group(Min, N, Max, undefined) ->
    dh_gex_group(Min, N, Max, ?dh_default_groups);
dh_gex_group(Min, N, Max, Groups) ->
    case select_by_keylen(Min-10, N, Max+10, Groups) of
	{ok,{Sz,GPs}} ->
            Rnd = rand:uniform( length(GPs) ),
            %% 1 =< Rnd =< length(GPs)
	    {ok, {Sz, lists:nth(Rnd,GPs)}};
	Other ->
	    Other
    end.

dh_gex_group_sizes()->
    [KeyLen || {KeyLen,_} <- ?dh_default_groups].


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% Select the one with K closest to N but within the interval [Min,Max]
    
select_by_keylen(Min, N, Max, [{K,_Gs}|Groups]) when K < Min ->
    select_by_keylen(Min, N, Max, Groups);
select_by_keylen(Min, N, Max, [{K,Gs}|Groups]) when K =< Max ->
    {ok, select_by_keylen(Min, N, Max, Groups, {K,Gs})};
select_by_keylen(_Min, _N, _Max, _) ->
    {error,no_group_found}.

select_by_keylen(_Min, _N, Max, [{K,_Gs}|_Groups], GPprev) when K > Max ->
    GPprev;
select_by_keylen(Min, N, Max, [{K,Gs}|Groups], {Kprev,GsPrev}) ->
    if
	N == K -> {K,Gs};
	N > K -> select_by_keylen(Min, N, Max, Groups, {K,Gs});
	N < K, (K-N) < (N-Kprev) -> {K,Gs};
	N < K -> {Kprev,GsPrev}
    end;
select_by_keylen(_Min, _N, _Max, [],GPprev) ->
    %% is between Min and Max
    GPprev.
