%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%% File: any.erl
%% Description:
%%    This file conatins the interface for the any type
%%
%%-----------------------------------------------------------------
-module(any).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create/0, create/2,
	 set_typecode/2, get_typecode/1,
	 set_value/2, get_value/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
create() ->
    #any{}.

create(TC, V) ->
    case orber_tc:check_tc(TC) of
	true ->
	    #any{typecode=TC, value=V};
	false ->
	    corba:raise(#'BAD_TYPECODE'{completion_status=?COMPLETED_NO})
    end.

set_typecode(Any, TC) ->
    case orber_tc:check_tc(TC) of
	true ->
	    Any#any{typecode=TC};
	false ->
	    corba:raise(#'BAD_TYPECODE'{completion_status=?COMPLETED_NO})
    end.

get_typecode(Any) ->
    Any#any.typecode.

set_value(Any, V) ->
     Any#any{value=V}.

get_value(Any) ->
    Any#any.value.

