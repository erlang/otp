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

-module(wxCloseEvent).
-include("wxe.hrl").
-export([canVeto/1,getLoggingOff/1,setCanVeto/2,setLoggingOff/2,veto/1,veto/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxCloseEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxCloseEventType() :: 'close_window' | 'end_session' | 'query_end_session'.
-export_type([wxCloseEvent/0, wxClose/0, wxCloseEventType/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcloseevent.html#wxcloseeventcanveto">external documentation</a>.
-spec canVeto(This) -> boolean() when
	This::wxCloseEvent().
canVeto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCloseEvent_CanVeto),
  wxe_util:rec(?wxCloseEvent_CanVeto).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcloseevent.html#wxcloseeventgetloggingoff">external documentation</a>.
-spec getLoggingOff(This) -> boolean() when
	This::wxCloseEvent().
getLoggingOff(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCloseEvent_GetLoggingOff),
  wxe_util:rec(?wxCloseEvent_GetLoggingOff).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcloseevent.html#wxcloseeventsetcanveto">external documentation</a>.
-spec setCanVeto(This, CanVeto) -> 'ok' when
	This::wxCloseEvent(), CanVeto::boolean().
setCanVeto(#wx_ref{type=ThisT}=This,CanVeto)
 when is_boolean(CanVeto) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,CanVeto,?get_env(),?wxCloseEvent_SetCanVeto).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcloseevent.html#wxcloseeventsetloggingoff">external documentation</a>.
-spec setLoggingOff(This, LoggingOff) -> 'ok' when
	This::wxCloseEvent(), LoggingOff::boolean().
setLoggingOff(#wx_ref{type=ThisT}=This,LoggingOff)
 when is_boolean(LoggingOff) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,LoggingOff,?get_env(),?wxCloseEvent_SetLoggingOff).

%% @equiv veto(This, [])
-spec veto(This) -> 'ok' when
	This::wxCloseEvent().

veto(This)
 when is_record(This, wx_ref) ->
  veto(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcloseevent.html#wxcloseeventveto">external documentation</a>.
-spec veto(This, [Option]) -> 'ok' when
	This::wxCloseEvent(),
	Option :: {'veto', boolean()}.
veto(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCloseEvent),
  MOpts = fun({veto, _veto} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxCloseEvent_Veto).

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
