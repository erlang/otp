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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html">wxSizerFlags</a>.
%% @type wxSizerFlags().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxSizerFlags).
-include("wxe.hrl").
-export([align/2,border/1,border/2,border/3,center/1,centre/1,destroy/1,expand/1,
  left/1,new/0,new/1,proportion/2,right/1]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxSizerFlags()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxSizerFlags()
%% Option = {proportion, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagswxsizerflags">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxSizerFlags_new,
  <<BinOpt/binary>>).

%% @spec (This::wxSizerFlags(), Alignment::integer()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsalign">external documentation</a>.
align(#wx_ref{type=ThisT,ref=ThisRef},Alignment)
 when is_integer(Alignment) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Align,
  <<ThisRef:32/?UI,Alignment:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> wxSizerFlags()
%% @equiv border(This, [])
border(This)
 when is_record(This, wx_ref) ->
  border(This, []).

%% @spec (This::wxSizerFlags(), [Option]) -> wxSizerFlags()
%% Option = {direction, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsborder">external documentation</a>.
border(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizerFlags),
  MOpts = fun({direction, Direction}, Acc) -> [<<1:32/?UI,Direction:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizerFlags_Border_1,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizerFlags(), Direction::integer(), BorderInPixels::integer()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsborder">external documentation</a>.
border(#wx_ref{type=ThisT,ref=ThisRef},Direction,BorderInPixels)
 when is_integer(Direction),is_integer(BorderInPixels) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Border_2,
  <<ThisRef:32/?UI,Direction:32/?UI,BorderInPixels:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagscenter">external documentation</a>.
center(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Center,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagscentre">external documentation</a>.
centre(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Centre,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsexpand">external documentation</a>.
expand(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Expand,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsleft">external documentation</a>.
left(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Left,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerFlags(), Proportion::integer()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsproportion">external documentation</a>.
proportion(#wx_ref{type=ThisT,ref=ThisRef},Proportion)
 when is_integer(Proportion) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Proportion,
  <<ThisRef:32/?UI,Proportion:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> wxSizerFlags()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizerflags.html#wxsizerflagsright">external documentation</a>.
right(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizerFlags),
  wxe_util:call(?wxSizerFlags_Right,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizerFlags()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxSizerFlags),
  wxe_util:destroy(?wxSizerFlags_destroy,Obj),
  ok.
