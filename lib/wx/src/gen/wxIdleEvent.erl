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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxidleevent.html">wxIdleEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>idle</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxIdle(). #wxIdle{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxIdleEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxIdleEvent).
-include("wxe.hrl").
-export([canSend/1,getMode/0,moreRequested/1,requestMore/1,requestMore/2,setMode/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Win::wxWindow:wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxidleevent.html#wxidleeventcansend">external documentation</a>.
canSend(#wx_ref{type=WinT,ref=WinRef}) ->
  ?CLASS(WinT,wxWindow),
  wxe_util:call(?wxIdleEvent_CanSend,
  <<WinRef:32/?UI>>).

%% @spec () -> WxIdleMode
%% WxIdleMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxidleevent.html#wxidleeventgetmode">external documentation</a>.
%%<br /> WxIdleMode is one of ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
getMode() ->
  wxe_util:call(?wxIdleEvent_GetMode,
  <<>>).

%% @spec (This::wxIdleEvent()) -> ok
%% @equiv requestMore(This, [])
requestMore(This)
 when is_record(This, wx_ref) ->
  requestMore(This, []).

%% @spec (This::wxIdleEvent(), [Option]) -> ok
%% Option = {needMore, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxidleevent.html#wxidleeventrequestmore">external documentation</a>.
requestMore(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxIdleEvent),
  MOpts = fun({needMore, NeedMore}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(NeedMore)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxIdleEvent_RequestMore,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxIdleEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxidleevent.html#wxidleeventmorerequested">external documentation</a>.
moreRequested(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxIdleEvent),
  wxe_util:call(?wxIdleEvent_MoreRequested,
  <<ThisRef:32/?UI>>).

%% @spec (Mode::WxIdleMode) -> ok
%% WxIdleMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxidleevent.html#wxidleeventsetmode">external documentation</a>.
%%<br /> WxIdleMode is one of ?wxIDLE_PROCESS_ALL | ?wxIDLE_PROCESS_SPECIFIED
setMode(Mode)
 when is_integer(Mode) ->
  wxe_util:cast(?wxIdleEvent_SetMode,
  <<Mode:32/?UI>>).

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
