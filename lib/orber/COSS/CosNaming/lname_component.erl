%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: lname_component.erl
%%-----------------------------------------------------------------
-module(lname_component).

-include_lib("orber/include/corba.hrl").
-include("lname.hrl").
-include("CosNaming.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([get_id/1, set_id/2, get_kind/1, set_kind/2, create/0, new/1, new/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
create() ->
    #'CosNaming_NameComponent'{id="", kind=""}.

get_id(NC) when is_record(NC, 'CosNaming_NameComponent'),
                NC#'CosNaming_NameComponent'.id == undefined ->
    corba:raise(#'LNameComponent_NotSet'{});
get_id(NC) when is_record(NC, 'CosNaming_NameComponent'),
                NC#'CosNaming_NameComponent'.id == "" ->
    corba:raise(#'LNameComponent_NotSet'{});
get_id(NC) when is_record(NC, 'CosNaming_NameComponent') ->
    NC#'CosNaming_NameComponent'.id.

set_id(NC, Id) when is_record(NC, 'CosNaming_NameComponent') andalso is_list(Id)->
    NC#'CosNaming_NameComponent'{id=Id}.

get_kind(NC) when is_record(NC, 'CosNaming_NameComponent') andalso
		  NC#'CosNaming_NameComponent'.kind == undefined ->
    corba:raise(#'LNameComponent_NotSet'{});
get_kind(NC) when is_record(NC, 'CosNaming_NameComponent') andalso
		  NC#'CosNaming_NameComponent'.kind == "" ->
    corba:raise(#'LNameComponent_NotSet'{});
get_kind(NC) when is_record(NC, 'CosNaming_NameComponent') ->
    NC#'CosNaming_NameComponent'.kind.

set_kind(NC, Kind) when is_record(NC, 'CosNaming_NameComponent') andalso is_list(Kind) ->
    NC#'CosNaming_NameComponent'{kind=Kind}.

%%destroy() -> % not needed in erlang
%%    true.

%%-----------------------------------------------------------------
%% External Functions not in the CosNaming standard
%%-----------------------------------------------------------------
new(Id) when is_list(Id) ->
    #'CosNaming_NameComponent'{id=Id, kind=""}.
new(Id, Kind) when is_list(Id) andalso is_list(Kind) ->
    #'CosNaming_NameComponent'{id=Id, kind=Kind}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------
