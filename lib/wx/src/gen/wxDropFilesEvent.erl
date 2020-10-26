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

-module(wxDropFilesEvent).
-include("wxe.hrl").
-export([getFiles/1,getNumberOfFiles/1,getPosition/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxDropFilesEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxDropFilesEventType() :: 'drop_files'.
-export_type([wxDropFilesEvent/0, wxDropFiles/0, wxDropFilesEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdropfilesevent.html#wxdropfileseventgetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxDropFilesEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDropFilesEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxDropFilesEvent_GetPosition),
  wxe_util:rec(?wxDropFilesEvent_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdropfilesevent.html#wxdropfileseventgetnumberoffiles">external documentation</a>.
-spec getNumberOfFiles(This) -> integer() when
	This::wxDropFilesEvent().
getNumberOfFiles(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDropFilesEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxDropFilesEvent_GetNumberOfFiles),
  wxe_util:rec(?wxDropFilesEvent_GetNumberOfFiles).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdropfilesevent.html#wxdropfileseventgetfiles">external documentation</a>.
-spec getFiles(This) -> [unicode:charlist()] when
	This::wxDropFilesEvent().
getFiles(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDropFilesEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxDropFilesEvent_GetFiles),
  wxe_util:rec(?wxDropFilesEvent_GetFiles).

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
