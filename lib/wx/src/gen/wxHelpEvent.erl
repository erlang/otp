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

-module(wxHelpEvent).
-include("wxe.hrl").
-export([getOrigin/1,getPosition/1,setOrigin/2,setPosition/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxHelpEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxHelpEventType() :: 'help' | 'detailed_help'.
-export_type([wxHelpEvent/0, wxHelp/0, wxHelpEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventgetorigin">external documentation</a>.
%%<br /> Res = ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
-spec getOrigin(This) -> wx:wx_enum() when
	This::wxHelpEvent().
getOrigin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxHelpEvent_GetOrigin),
  wxe_util:rec(?wxHelpEvent_GetOrigin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxHelpEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxHelpEvent_GetPosition),
  wxe_util:rec(?wxHelpEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventsetorigin">external documentation</a>.
%%<br /> Origin = ?wxHelpEvent_Origin_Unknown | ?wxHelpEvent_Origin_Keyboard | ?wxHelpEvent_Origin_HelpButton
-spec setOrigin(This, Origin) -> 'ok' when
	This::wxHelpEvent(), Origin::wx:wx_enum().
setOrigin(#wx_ref{type=ThisT}=This,Origin)
 when is_integer(Origin) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,Origin,?get_env(),?wxHelpEvent_SetOrigin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhelpevent.html#wxhelpeventsetposition">external documentation</a>.
-spec setPosition(This, Pt) -> 'ok' when
	This::wxHelpEvent(), Pt::{X::integer(), Y::integer()}.
setPosition(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxHelpEvent),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxHelpEvent_SetPosition).

 %% From wxEvent
%% @hidden
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
skip(This) -> wxEvent:skip(This).
%% @hidden
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
getId(This) -> wxEvent:getId(This).
