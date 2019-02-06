%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html">wxFindReplaceData</a>.
%% @type wxFindReplaceData().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxFindReplaceData).
-include("wxe.hrl").
-export([destroy/1,getFindString/1,getFlags/1,getReplaceString/1,new/0,new/1,
  setFindString/2,setFlags/2,setReplaceString/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxFindReplaceData/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxFindReplaceData() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatawxfindreplacedata">external documentation</a>.
-spec new() -> wxFindReplaceData().
new() ->
  wxe_util:construct(?wxFindReplaceData_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatawxfindreplacedata">external documentation</a>.
-spec new(Flags) -> wxFindReplaceData() when
	Flags::integer().
new(Flags)
 when is_integer(Flags) ->
  wxe_util:construct(?wxFindReplaceData_new_1,
  <<Flags:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatagetfindstring">external documentation</a>.
-spec getFindString(This) -> unicode:charlist() when
	This::wxFindReplaceData().
getFindString(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:call(?wxFindReplaceData_GetFindString,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatagetreplacestring">external documentation</a>.
-spec getReplaceString(This) -> unicode:charlist() when
	This::wxFindReplaceData().
getReplaceString(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:call(?wxFindReplaceData_GetReplaceString,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatagetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxFindReplaceData().
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:call(?wxFindReplaceData_GetFlags,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatasetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxFindReplaceData(), Flags::integer().
setFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxFindReplaceData),
  wxe_util:cast(?wxFindReplaceData_SetFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatasetfindstring">external documentation</a>.
-spec setFindString(This, Str) -> 'ok' when
	This::wxFindReplaceData(), Str::unicode:chardata().
setFindString(#wx_ref{type=ThisT,ref=ThisRef},Str)
 when ?is_chardata(Str) ->
  ?CLASS(ThisT,wxFindReplaceData),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxFindReplaceData_SetFindString,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfindreplacedata.html#wxfindreplacedatasetreplacestring">external documentation</a>.
-spec setReplaceString(This, Str) -> 'ok' when
	This::wxFindReplaceData(), Str::unicode:chardata().
setReplaceString(#wx_ref{type=ThisT,ref=ThisRef},Str)
 when ?is_chardata(Str) ->
  ?CLASS(ThisT,wxFindReplaceData),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxFindReplaceData_SetReplaceString,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFindReplaceData()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFindReplaceData),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
