%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%% File: lname.erl
%%-----------------------------------------------------------------
-module(lname).

-include_lib("orber/include/corba.hrl").
-include("CosNaming.hrl").
-include("lname.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create/0, insert_component/3, get_component/2, delete_component/2,
	 num_component/1, equal/2, less_than/2,
	 to_idl_form/1, from_idl_form/1, check_name/1, new/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%% DEBUG INFO
-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
create() ->
    [].

insert_component(_, I, _) when I < 1->
    corba:raise(#'LName_NoComponent'{});
insert_component([], I, _) when I > 1->
    corba:raise(#'LName_NoComponent'{});
insert_component(Name, 1, Component) when is_record(Component,
						    'CosNaming_NameComponent') ->
    [Component |Name];
insert_component([H|T], I, Component) when is_record(Component,
						     'CosNaming_NameComponent') ->
    [H |insert_component(T, I-1, Component)];
insert_component(_, _, Component) -> 
    orber:dbg("[~p] ~p:insert_component(~p); Not a NameComponent.~n", 
	      [?LINE, ?MODULE, Component], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

get_component(_, I) when I < 1->
    corba:raise(#'LName_NoComponent'{});
get_component([], _) ->
    corba:raise(#'LName_NoComponent'{});
get_component([H|_T], 1) ->
    H;
get_component([_|T], I) ->
    get_component(T, I-1).

delete_component(_, I) when I < 1->
    corba:raise(#'LName_NoComponent'{});
delete_component([], _) ->
    corba:raise(#'LName_NoComponent'{});
delete_component([_|T], 1) ->
    T;
delete_component([H|T], I) ->
    [H | delete_component(T, I-1)].

num_component(Name) ->
    num_component(Name, 0).

equal(Name, N) ->
    N == Name.

less_than(Name, N) ->
    Name < N.

to_idl_form(Name) ->
    case check_name(Name) of
	false ->
	    corba:raise(#'LName_InvalidName'{});
	true ->
	    Name
    end.

from_idl_form(Name) ->
    Name.
	
%%destroy() -> % not needed in erlang
%%    ok.

%%-----------------------------------------------------------------
%% External Functions not in the CosNaming standard
%%-----------------------------------------------------------------
new([]) ->
    [];
new([{Id, Kind} | List]) ->
    [lname_component:new(Id, Kind) | new(List)];
new([Id |List]) when is_list(Id) ->
    [lname_component:new(Id) | new(List)].

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------
num_component([], N) ->
    N;
num_component([_|T], N) ->
    num_component(T, N+1).

check_name([]) ->
    true;
check_name([H|T]) ->
    case catch lname_component:get_id(H) of
	{'EXCEPTION', _E} ->
	    false;
	_ ->
	    check_name(T)
    end.
