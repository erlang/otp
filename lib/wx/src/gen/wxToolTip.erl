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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html">wxToolTip</a>.
%% @type wxToolTip().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxToolTip).
-include("wxe.hrl").
-export([destroy/1,enable/1,getTip/1,getWindow/1,new/1,setDelay/1,setTip/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxToolTip/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxToolTip() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html#wxtooltipenable">external documentation</a>.
-spec enable(Flag) -> 'ok' when
	Flag::boolean().
enable(Flag)
 when is_boolean(Flag) ->
  wxe_util:cast(?wxToolTip_Enable,
  <<(wxe_util:from_bool(Flag)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html#wxtooltipsetdelay">external documentation</a>.
-spec setDelay(Msecs) -> 'ok' when
	Msecs::integer().
setDelay(Msecs)
 when is_integer(Msecs) ->
  wxe_util:cast(?wxToolTip_SetDelay,
  <<Msecs:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html#wxtooltipwxtooltip">external documentation</a>.
-spec new(Tip) -> wxToolTip() when
	Tip::unicode:chardata().
new(Tip)
 when ?is_chardata(Tip) ->
  Tip_UC = unicode:characters_to_binary([Tip,0]),
  wxe_util:construct(?wxToolTip_new,
  <<(byte_size(Tip_UC)):32/?UI,(Tip_UC)/binary, 0:(((8- ((4+byte_size(Tip_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html#wxtooltipsettip">external documentation</a>.
-spec setTip(This, Tip) -> 'ok' when
	This::wxToolTip(), Tip::unicode:chardata().
setTip(#wx_ref{type=ThisT,ref=ThisRef},Tip)
 when ?is_chardata(Tip) ->
  ?CLASS(ThisT,wxToolTip),
  Tip_UC = unicode:characters_to_binary([Tip,0]),
  wxe_util:cast(?wxToolTip_SetTip,
  <<ThisRef:32/?UI,(byte_size(Tip_UC)):32/?UI,(Tip_UC)/binary, 0:(((8- ((0+byte_size(Tip_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html#wxtooltipgettip">external documentation</a>.
-spec getTip(This) -> unicode:charlist() when
	This::wxToolTip().
getTip(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolTip),
  wxe_util:call(?wxToolTip_GetTip,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtooltip.html#wxtooltipgetwindow">external documentation</a>.
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxToolTip().
getWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolTip),
  wxe_util:call(?wxToolTip_GetWindow,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxToolTip()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxToolTip),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
