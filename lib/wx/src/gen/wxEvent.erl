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

-module(wxEvent).
-include("wxe.hrl").
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,resumePropagation/2,
  shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% inherited exports
-export([parent_class/1]).

-type wxEvent() :: wx:wx_object().
-export_type([wxEvent/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventgetid">external documentation</a>.
-spec getId(This) -> integer() when
	This::wxEvent().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_GetId),
  wxe_util:rec(?wxEvent_GetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventgetskipped">external documentation</a>.
-spec getSkipped(This) -> boolean() when
	This::wxEvent().
getSkipped(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_GetSkipped),
  wxe_util:rec(?wxEvent_GetSkipped).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventgettimestamp">external documentation</a>.
-spec getTimestamp(This) -> integer() when
	This::wxEvent().
getTimestamp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_GetTimestamp),
  wxe_util:rec(?wxEvent_GetTimestamp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventiscommandevent">external documentation</a>.
-spec isCommandEvent(This) -> boolean() when
	This::wxEvent().
isCommandEvent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_IsCommandEvent),
  wxe_util:rec(?wxEvent_IsCommandEvent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventresumepropagation">external documentation</a>.
-spec resumePropagation(This, PropagationLevel) -> 'ok' when
	This::wxEvent(), PropagationLevel::integer().
resumePropagation(#wx_ref{type=ThisT}=This,PropagationLevel)
 when is_integer(PropagationLevel) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,PropagationLevel,?get_env(),?wxEvent_ResumePropagation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventshouldpropagate">external documentation</a>.
-spec shouldPropagate(This) -> boolean() when
	This::wxEvent().
shouldPropagate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_ShouldPropagate),
  wxe_util:rec(?wxEvent_ShouldPropagate).

%% @equiv skip(This, [])
-spec skip(This) -> 'ok' when
	This::wxEvent().

skip(This)
 when is_record(This, wx_ref) ->
  skip(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventskip">external documentation</a>.
-spec skip(This, [Option]) -> 'ok' when
	This::wxEvent(),
	Option :: {'skip', boolean()}.
skip(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxEvent),
  MOpts = fun({skip, _skip} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxEvent_Skip).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxevent.html#wxeventstoppropagation">external documentation</a>.
-spec stopPropagation(This) -> integer() when
	This::wxEvent().
stopPropagation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxEvent_StopPropagation),
  wxe_util:rec(?wxEvent_StopPropagation).

