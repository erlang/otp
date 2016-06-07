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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html">wxSizerFlags</a>.
%% @type wxSizerFlags().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxSizerFlags).
-include("wxe.hrl").
-export([align/2,border/1,border/2,border/3,center/1,centre/1,destroy/1,expand/1,
  left/1,new/0,new/1,proportion/2,right/1]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxSizerFlags/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxSizerFlags() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxSizerFlags().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagswxsizerflags">external documentation</a>.
-spec new([Option]) -> wxSizerFlags() when
	Option :: {'proportion', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxSizerFlags_new,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsalign">external documentation</a>.
-spec align(This, Alignment) -> wxSizerFlags() when
	This::wxSizerFlags(), Alignment::integer().
align(#wx_ref{type=ThisT,ref=ThisRef},Alignment)
 when is_integer(Alignment) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Align,
  <<ThisRef:32/?UI,Alignment:32/?UI>>).

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
border(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizerFlags),
  MOpts = fun({direction, Direction}, Acc) -> [<<1:32/?UI,Direction:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizerFlags_Border_1,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsborder">external documentation</a>.
-spec border(This, Direction, BorderInPixels) -> wxSizerFlags() when
	This::wxSizerFlags(), Direction::integer(), BorderInPixels::integer().
border(#wx_ref{type=ThisT,ref=ThisRef},Direction,BorderInPixels)
 when is_integer(Direction),is_integer(BorderInPixels) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Border_2,
  <<ThisRef:32/?UI,Direction:32/?UI,BorderInPixels:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagscenter">external documentation</a>.
-spec center(This) -> wxSizerFlags() when
	This::wxSizerFlags().
center(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Center,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagscentre">external documentation</a>.
-spec centre(This) -> wxSizerFlags() when
	This::wxSizerFlags().
centre(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Centre,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsexpand">external documentation</a>.
-spec expand(This) -> wxSizerFlags() when
	This::wxSizerFlags().
expand(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Expand,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsleft">external documentation</a>.
-spec left(This) -> wxSizerFlags() when
	This::wxSizerFlags().
left(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Left,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsproportion">external documentation</a>.
-spec proportion(This, Proportion) -> wxSizerFlags() when
	This::wxSizerFlags(), Proportion::integer().
proportion(#wx_ref{type=ThisT,ref=ThisRef},Proportion)
 when is_integer(Proportion) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Proportion,
  <<ThisRef:32/?UI,Proportion:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizerflags.html#wxsizerflagsright">external documentation</a>.
-spec right(This) -> wxSizerFlags() when
	This::wxSizerFlags().
right(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Right,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxSizerFlags()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSizerFlags),
  wxe_util:destroy(?wxSizerFlags_destroy,Obj),
  ok.
