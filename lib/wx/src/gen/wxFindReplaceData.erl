%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxFindReplaceData).
-include("wxe.hrl").
-export([destroy/1,getFindString/1,getFlags/1,getReplaceString/1,new/0,new/1,
  setFindString/2,setFlags/2,setReplaceString/2]).

%% inherited exports
-export([parent_class/1]).

-type wxFindReplaceData() :: wx:wx_object().
-export_type([wxFindReplaceData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxFindReplaceData().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatawxfindreplacedata">external documentation</a>.
-spec new([Option]) -> wxFindReplaceData() when
	Option :: {'flags', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxFindReplaceData_new),
  wxe_util:rec(?wxFindReplaceData_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatagetfindstring">external documentation</a>.
-spec getFindString(This) -> unicode:charlist() when
	This::wxFindReplaceData().
getFindString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:queue_cmd(This,?get_env(),?wxFindReplaceData_GetFindString),
  wxe_util:rec(?wxFindReplaceData_GetFindString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatagetreplacestring">external documentation</a>.
-spec getReplaceString(This) -> unicode:charlist() when
	This::wxFindReplaceData().
getReplaceString(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:queue_cmd(This,?get_env(),?wxFindReplaceData_GetReplaceString),
  wxe_util:rec(?wxFindReplaceData_GetReplaceString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatagetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxFindReplaceData().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:queue_cmd(This,?get_env(),?wxFindReplaceData_GetFlags),
  wxe_util:rec(?wxFindReplaceData_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatasetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxFindReplaceData(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxFindReplaceData_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatasetfindstring">external documentation</a>.
-spec setFindString(This, Str) -> 'ok' when
	This::wxFindReplaceData(), Str::unicode:chardata().
setFindString(#wx_ref{type=ThisT}=This,Str)
 when ?is_chardata(Str) ->
  ?CLASS(ThisT,wxFindReplaceData),
  Str_UC = unicode:characters_to_binary(Str),
  wxe_util:queue_cmd(This,Str_UC,?get_env(),?wxFindReplaceData_SetFindString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatasetreplacestring">external documentation</a>.
-spec setReplaceString(This, Str) -> 'ok' when
	This::wxFindReplaceData(), Str::unicode:chardata().
setReplaceString(#wx_ref{type=ThisT}=This,Str)
 when ?is_chardata(Str) ->
  ?CLASS(ThisT,wxFindReplaceData),
  Str_UC = unicode:characters_to_binary(Str),
  wxe_util:queue_cmd(This,Str_UC,?get_env(),?wxFindReplaceData_SetReplaceString).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFindReplaceData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFindReplaceData),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
