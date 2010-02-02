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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html">wxEvent</a>.
%% @type wxEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxEvent).
-include("wxe.hrl").
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,resumePropagation/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventgetid">external documentation</a>.
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:call(?wxEvent_GetId,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventgetskipped">external documentation</a>.
getSkipped(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:call(?wxEvent_GetSkipped,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventgettimestamp">external documentation</a>.
getTimestamp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:call(?wxEvent_GetTimestamp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventiscommandevent">external documentation</a>.
isCommandEvent(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:call(?wxEvent_IsCommandEvent,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxEvent(), PropagationLevel::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventresumepropagation">external documentation</a>.
resumePropagation(#wx_ref{type=ThisT,ref=ThisRef},PropagationLevel)
 when is_integer(PropagationLevel) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:cast(?wxEvent_ResumePropagation,
  <<ThisRef:32/?UI,PropagationLevel:32/?UI>>).

%% @spec (This::wxEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventshouldpropagate">external documentation</a>.
shouldPropagate(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:call(?wxEvent_ShouldPropagate,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxEvent()) -> ok
%% @equiv skip(This, [])
skip(This)
 when is_record(This, wx_ref) ->
  skip(This, []).

%% @spec (This::wxEvent(), [Option]) -> ok
%% Option = {skip, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventskip">external documentation</a>.
skip(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxEvent),
  MOpts = fun({skip, Skip}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Skip)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxEvent_Skip,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxevent.html#wxeventstoppropagation">external documentation</a>.
stopPropagation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:call(?wxEvent_StopPropagation,
  <<ThisRef:32/?UI>>).

