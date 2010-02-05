%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html">wxToolTip</a>.
%% @type wxToolTip().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxToolTip).
-include("wxe.hrl").
-export([destroy/1,enable/1,getTip/1,getWindow/1,new/1,setDelay/1,setTip/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Flag::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html#wxtooltipenable">external documentation</a>.
enable(Flag)
 when is_boolean(Flag) ->
  wxe_util:cast(?wxToolTip_Enable,
  <<(wxe_util:from_bool(Flag)):32/?UI>>).

%% @spec (Msecs::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html#wxtooltipsetdelay">external documentation</a>.
setDelay(Msecs)
 when is_integer(Msecs) ->
  wxe_util:cast(?wxToolTip_SetDelay,
  <<Msecs:32/?UI>>).

%% @spec (Tip::string()) -> wxToolTip()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html#wxtooltipwxtooltip">external documentation</a>.
new(Tip)
 when is_list(Tip) ->
  Tip_UC = unicode:characters_to_binary([Tip,0]),
  wxe_util:construct(?wxToolTip_new,
  <<(byte_size(Tip_UC)):32/?UI,(Tip_UC)/binary, 0:(((8- ((4+byte_size(Tip_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxToolTip(), Tip::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html#wxtooltipsettip">external documentation</a>.
setTip(#wx_ref{type=ThisT,ref=ThisRef},Tip)
 when is_list(Tip) ->
  ?CLASS(ThisT,wxToolTip),
  Tip_UC = unicode:characters_to_binary([Tip,0]),
  wxe_util:cast(?wxToolTip_SetTip,
  <<ThisRef:32/?UI,(byte_size(Tip_UC)):32/?UI,(Tip_UC)/binary, 0:(((8- ((0+byte_size(Tip_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxToolTip()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html#wxtooltipgettip">external documentation</a>.
getTip(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolTip),
  wxe_util:call(?wxToolTip_GetTip,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolTip()) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtooltip.html#wxtooltipgetwindow">external documentation</a>.
getWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolTip),
  wxe_util:call(?wxToolTip_GetWindow,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolTip()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxToolTip),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
