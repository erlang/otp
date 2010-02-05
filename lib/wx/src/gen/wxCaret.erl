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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html">wxCaret</a>.
%% @type wxCaret().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxCaret).
-include("wxe.hrl").
-export([create/3,create/4,destroy/1,getBlinkTime/0,getPosition/1,getSize/1,
  getWindow/1,hide/1,isOk/1,isVisible/1,move/2,move/3,new/2,new/3,setBlinkTime/1,
  setSize/2,setSize/3,show/1,show/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Window::wxWindow:wxWindow(), Size::{W::integer(),H::integer()}) -> wxCaret()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretwxcaret">external documentation</a>.
new(#wx_ref{type=WindowT,ref=WindowRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:construct(?wxCaret_new_2,
  <<WindowRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (Window::wxWindow:wxWindow(), Width::integer(), Height::integer()) -> wxCaret()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretwxcaret">external documentation</a>.
new(#wx_ref{type=WindowT,ref=WindowRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:construct(?wxCaret_new_3,
  <<WindowRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxCaret(), Window::wxWindow:wxWindow(), Size::{W::integer(),H::integer()}) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxCaret),
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxCaret_Create_2,
  <<ThisRef:32/?UI,WindowRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxCaret(), Window::wxWindow:wxWindow(), Width::integer(), Height::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxCaret),
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxCaret_Create_3,
  <<ThisRef:32/?UI,WindowRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec () -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretgetblinktime">external documentation</a>.
getBlinkTime() ->
  wxe_util:call(?wxCaret_GetBlinkTime,
  <<>>).

%% @spec (This::wxCaret()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretgetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:call(?wxCaret_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCaret()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretgetsize">external documentation</a>.
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:call(?wxCaret_GetSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCaret()) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretgetwindow">external documentation</a>.
getWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:call(?wxCaret_GetWindow,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCaret()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcarethide">external documentation</a>.
hide(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:cast(?wxCaret_Hide,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCaret()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:call(?wxCaret_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCaret()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretisvisible">external documentation</a>.
isVisible(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:call(?wxCaret_IsVisible,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCaret(), Pt::{X::integer(),Y::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretmove">external documentation</a>.
move(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:cast(?wxCaret_Move_1,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxCaret(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretmove">external documentation</a>.
move(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:cast(?wxCaret_Move_2,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (Milliseconds::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretsetblinktime">external documentation</a>.
setBlinkTime(Milliseconds)
 when is_integer(Milliseconds) ->
  wxe_util:cast(?wxCaret_SetBlinkTime,
  <<Milliseconds:32/?UI>>).

%% @spec (This::wxCaret(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretsetsize">external documentation</a>.
setSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:cast(?wxCaret_SetSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxCaret(), Width::integer(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretsetsize">external documentation</a>.
setSize(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxCaret),
  wxe_util:cast(?wxCaret_SetSize_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxCaret()) -> ok
%% @equiv show(This, [])
show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @spec (This::wxCaret(), [Option]) -> ok
%% Option = {show, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcaret.html#wxcaretshow">external documentation</a>.
show(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCaret),
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxCaret_Show,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxCaret()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCaret),
  wxe_util:destroy(?wxCaret_destruct,Obj),
  ok.
