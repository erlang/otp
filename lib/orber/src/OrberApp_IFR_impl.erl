%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File    : OrberApp_IFR_impl.erl
%% Purpose : 
%%-----------------------------------------------------------------

-module('OrberApp_IFR_impl').

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

%%--------------- IMPORTS ------------------------------------

%%--------------- EXPORTS ------------------------------------
%% External
-export([get_absolute_name/3, get_user_exception_type/3]).

%%--------------- gen_server specific exports ----------------
-export([init/1, terminate/2, code_change/3]).

%%--------------- LOCAL DEFINITIONS --------------------------
-define(DEBUG_LEVEL, 6).


init(State) ->
    {ok, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : get_absolute_name
%% Arguments: TypeID - string()
%% Returns  : Fully scooped name - string()
%%-----------------------------------------------------------

get_absolute_name(_OE_THIS, _State, []) ->
    orber:dbg("[~p] OrberApp_IFR_impl:get_absolute_name(); no TypeID supplied.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 11), completion_status=?COMPLETED_MAYBE});

get_absolute_name(_OE_THIS, State, TypeID) ->
    Rep = orber_ifr:find_repository(),
    Key = orber_ifr:'Repository_lookup_id'(Rep, TypeID),
    [$:, $: |N] = orber_ifr:'Contained__get_absolute_name'(Key),
    {reply, change_colons_to_underscore(N, []), State}.

change_colons_to_underscore([$:, $: | T], Acc) ->
    change_colons_to_underscore(T, [$_ |Acc]);
change_colons_to_underscore([H |T], Acc) ->
    change_colons_to_underscore(T, [H |Acc]);
change_colons_to_underscore([], Acc) ->
    lists:reverse(Acc).

%%----------------------------------------------------------%
%% function : get_user_exception_type
%% Arguments: TypeID - string()
%% Returns  : Fully scooped name - string()
%%-----------------------------------------------------------

get_user_exception_type(_OE_THIS, _State, []) -> 
    orber:dbg("[~p] OrberApp_IFR_impl:get_user_exception_type(); no TypeID supplied.", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'MARSHAL'{minor=(?ORBER_VMCID bor 11), completion_status=?COMPLETED_MAYBE});

get_user_exception_type(_OE_THIS, State, TypeId) -> 
    Rep = orber_ifr:find_repository(),
    ExceptionDef = orber_ifr:'Repository_lookup_id'(Rep, TypeId),
    ContainedDescr = orber_ifr_exceptiondef:describe(ExceptionDef),
    ExceptionDescr = ContainedDescr#contained_description.value,
    {reply, ExceptionDescr#exceptiondescription.type, State}.


%%--------------- LOCAL FUNCTIONS ----------------------------
%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
