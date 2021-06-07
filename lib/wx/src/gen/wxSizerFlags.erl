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

-module(wxSizerFlags).
-include("wxe.hrl").
-export([align/2,border/1,border/2,border/3,center/1,centre/1,destroy/1,expand/1,
  left/1,new/0,new/1,proportion/2,right/1]).

%% inherited exports
-export([parent_class/1]).

-type wxSizerFlags() :: wx:wx_object().
-export_type([wxSizerFlags/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxSizerFlags().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagswxsizerflags">external documentation</a>.
-spec new([Option]) -> wxSizerFlags() when
	Option :: {'proportion', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxSizerFlags_new),
  wxe_util:rec(?wxSizerFlags_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsalign">external documentation</a>.
-spec align(This, Alignment) -> wxSizerFlags() when
	This::wxSizerFlags(), Alignment::integer().
align(#wx_ref{type=ThisT}=This,Alignment)
 when is_integer(Alignment) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,Alignment,?get_env(),?wxSizerFlags_Align),
  wxe_util:rec(?wxSizerFlags_Align).

%% @equiv border(This, [])
-spec border(This) -> wxSizerFlags() when
	This::wxSizerFlags().

border(This)
 when is_record(This, wx_ref) ->
  border(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsborder">external documentation</a>.
-spec border(This, [Option]) -> wxSizerFlags() when
	This::wxSizerFlags(),
	Option :: {'direction', integer()}.
border(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizerFlags),
  MOpts = fun({direction, _direction} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxSizerFlags_Border_1),
  wxe_util:rec(?wxSizerFlags_Border_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsborder">external documentation</a>.
-spec border(This, Direction, Borderinpixels) -> wxSizerFlags() when
	This::wxSizerFlags(), Direction::integer(), Borderinpixels::integer().
border(#wx_ref{type=ThisT}=This,Direction,Borderinpixels)
 when is_integer(Direction),is_integer(Borderinpixels) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,Direction,Borderinpixels,?get_env(),?wxSizerFlags_Border_2),
  wxe_util:rec(?wxSizerFlags_Border_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagscenter">external documentation</a>.
-spec centre(This) -> wxSizerFlags() when
	This::wxSizerFlags().

centre(This)
 when is_record(This, wx_ref) ->
  center(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagscenter">external documentation</a>.
-spec center(This) -> wxSizerFlags() when
	This::wxSizerFlags().
center(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerFlags_Center),
  wxe_util:rec(?wxSizerFlags_Center).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsexpand">external documentation</a>.
-spec expand(This) -> wxSizerFlags() when
	This::wxSizerFlags().
expand(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerFlags_Expand),
  wxe_util:rec(?wxSizerFlags_Expand).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsleft">external documentation</a>.
-spec left(This) -> wxSizerFlags() when
	This::wxSizerFlags().
left(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerFlags_Left),
  wxe_util:rec(?wxSizerFlags_Left).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsproportion">external documentation</a>.
-spec proportion(This, Proportion) -> wxSizerFlags() when
	This::wxSizerFlags(), Proportion::integer().
proportion(#wx_ref{type=ThisT}=This,Proportion)
 when is_integer(Proportion) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,Proportion,?get_env(),?wxSizerFlags_Proportion),
  wxe_util:rec(?wxSizerFlags_Proportion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsright">external documentation</a>.
-spec right(This) -> wxSizerFlags() when
	This::wxSizerFlags().
right(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:queue_cmd(This,?get_env(),?wxSizerFlags_Right),
  wxe_util:rec(?wxSizerFlags_Right).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxSizerFlags()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSizerFlags),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxSizerFlags_destroy),
  ok.
