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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html">wxSizer</a>.
%% @type wxSizer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxSizer).
-include("wxe.hrl").
-export([add/2,add/3,add/4,addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,
  calcMin/1,clear/1,clear/2,detach/2,fit/2,fitInside/2,getChildren/1,getItem/2,
  getItem/3,getMinSize/1,getPosition/1,getSize/1,hide/2,hide/3,insert/3,
  insert/4,insert/5,insertSpacer/3,insertStretchSpacer/2,insertStretchSpacer/3,
  isShown/2,layout/1,prepend/2,prepend/3,prepend/4,prependSpacer/2,prependStretchSpacer/1,
  prependStretchSpacer/2,recalcSizes/1,remove/2,replace/3,replace/4,
  setDimension/5,setItemMinSize/3,setItemMinSize/4,setMinSize/2,setMinSize/3,
  setSizeHints/2,setVirtualSizeHints/2,show/2,show/3]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> wxSizerItem:wxSizerItem()
%% @equiv add(This,Window, [])
add(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  add(This,Window, []).

%% @spec (This::wxSizer(),X::integer()|term(),X::integer()|term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizeradd">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% add(This::wxSizer(), Width::integer(), Height::integer()) -> add(This,Width,Height, []) </c></p>
%% <p><c>
%% add(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), [Option]) -> wxSizerItem:wxSizerItem() </c>
%%<br /> Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% </p>
%% <p><c>
%% add(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags()) -> wxSizerItem:wxSizerItem() </c>
%% </p>

add(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  add(This,Width,Height, []);
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_Add_2_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_Add_2_0
     end,
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>);
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=FlagsT,ref=FlagsRef}) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizer_Add_2_3;
     _ -> ?CLASS(WindowT,wxSizer),
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizer_Add_2_2
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,FlagsRef:32/?UI>>).

%% @spec (This::wxSizer(), Width::integer(), Height::integer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizeradd">external documentation</a>.
add(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_Add_3,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizer(), Size::integer()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizeraddspacer">external documentation</a>.
addSpacer(#wx_ref{type=ThisT,ref=ThisRef},Size)
 when is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_AddSpacer,
  <<ThisRef:32/?UI,Size:32/?UI>>).

%% @spec (This::wxSizer()) -> wxSizerItem:wxSizerItem()
%% @equiv addStretchSpacer(This, [])
addStretchSpacer(This)
 when is_record(This, wx_ref) ->
  addStretchSpacer(This, []).

%% @spec (This::wxSizer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {prop, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizeraddstretchspacer">external documentation</a>.
addStretchSpacer(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, Prop}, Acc) -> [<<1:32/?UI,Prop:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_AddStretchSpacer,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizercalcmin">external documentation</a>.
calcMin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_CalcMin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer()) -> ok
%% @equiv clear(This, [])
clear(This)
 when is_record(This, wx_ref) ->
  clear(This, []).

%% @spec (This::wxSizer(), [Option]) -> ok
%% Option = {delete_windows, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerclear">external documentation</a>.
clear(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({delete_windows, Delete_windows}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Delete_windows)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxSizer_Clear,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizer(),X::integer()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerdetach">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% detach(This::wxSizer(), Index::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% detach(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> bool() </c>
%% </p>
detach(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_Detach_1_0,
  <<ThisRef:32/?UI,Index:32/?UI>>);
detach(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_Detach_1_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_Detach_1_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerfit">external documentation</a>.
fit(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxSizer_Fit,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerfitinside">external documentation</a>.
fitInside(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizer_FitInside,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizer()) -> [wxSizerItem:wxSizerItem()]
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizergetchildren">external documentation</a>.
getChildren(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetChildren,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer(),X::term()|integer()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizergetitem">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% getItem(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> getItem(This,Window, []) </c></p>
%% <p><c>
%% getItem(This::wxSizer(), Index::integer()) -> wxSizerItem:wxSizerItem() </c>
%% </p>

getItem(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  getItem(This,Window, []);
getItem(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetItem_1,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {recursive, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizergetitem">external documentation</a>.
getItem(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_GetItem_2_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_GetItem_2_0
     end,
  MOpts = fun({recursive, Recursive}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Recursive)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxSizer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizergetsize">external documentation</a>.
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizergetposition">external documentation</a>.
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetPosition,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizergetminsize">external documentation</a>.
getMinSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetMinSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer(),X::term()|integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerhide">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% hide(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> hide(This,Window, []) </c></p>
%% <p><c>
%% hide(This::wxSizer(), Index::integer()) -> bool() </c>
%% </p>

hide(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  hide(This,Window, []);
hide(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_Hide_1,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), [Option]) -> bool()
%% Option = {recursive, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerhide">external documentation</a>.
hide(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_Hide_2_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_Hide_2_0
     end,
  MOpts = fun({recursive, Recursive}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Recursive)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxSizer(),Index::integer(),X::term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% insert(This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer()) -> insert(This,Index,Window, []) </c></p>
%% <p><c>
%% insert(This::wxSizer(), Index::integer(), Item::wxSizerItem:wxSizerItem()) -> wxSizerItem:wxSizerItem() </c>
%% </p>

insert(This,Index,Window)
 when is_record(This, wx_ref),is_integer(Index),is_record(Window, wx_ref) ->
  insert(This,Index,Window, []);
insert(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=ItemT,ref=ItemRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(ItemT,wxSizerItem),
  wxe_util:call(?wxSizer_Insert_2,
  <<ThisRef:32/?UI,Index:32/?UI,ItemRef:32/?UI>>).

%% @spec (This::wxSizer(),Index::integer(),X::integer()|term(),X::integer()|term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% insert(This::wxSizer(), Index::integer(), Width::integer(), Height::integer()) -> insert(This,Index,Width,Height, []) </c></p>
%% <p><c>
%% insert(This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer(), [Option]) -> wxSizerItem:wxSizerItem() </c>
%%<br /> Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% </p>
%% <p><c>
%% insert(This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags()) -> wxSizerItem:wxSizerItem() </c>
%% </p>

insert(This,Index,Width,Height)
 when is_record(This, wx_ref),is_integer(Index),is_integer(Width),is_integer(Height) ->
  insert(This,Index,Width,Height, []);
insert(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_Insert_3_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_Insert_3_0
     end,
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,Index:32/?UI,WindowRef:32/?UI, 0:32,BinOpt/binary>>);
insert(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=FlagsT,ref=FlagsRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizer_Insert_3_3;
     _ -> ?CLASS(WindowT,wxSizer),
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizer_Insert_3_2
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,Index:32/?UI,WindowRef:32/?UI,FlagsRef:32/?UI>>).

%% @spec (This::wxSizer(), Index::integer(), Width::integer(), Height::integer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
insert(#wx_ref{type=ThisT,ref=ThisRef},Index,Width,Height, Options)
 when is_integer(Index),is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_Insert_4,
  <<ThisRef:32/?UI,Index:32/?UI,Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @spec (This::wxSizer(), Index::integer(), Size::integer()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerinsertspacer">external documentation</a>.
insertSpacer(#wx_ref{type=ThisT,ref=ThisRef},Index,Size)
 when is_integer(Index),is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_InsertSpacer,
  <<ThisRef:32/?UI,Index:32/?UI,Size:32/?UI>>).

%% @spec (This::wxSizer(), Index::integer()) -> wxSizerItem:wxSizerItem()
%% @equiv insertStretchSpacer(This,Index, [])
insertStretchSpacer(This,Index)
 when is_record(This, wx_ref),is_integer(Index) ->
  insertStretchSpacer(This,Index, []).

%% @spec (This::wxSizer(), Index::integer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {prop, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerinsertstretchspacer">external documentation</a>.
insertStretchSpacer(#wx_ref{type=ThisT,ref=ThisRef},Index, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, Prop}, Acc) -> [<<1:32/?UI,Prop:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_InsertStretchSpacer,
  <<ThisRef:32/?UI,Index:32/?UI, BinOpt/binary>>).

%% @spec (This::wxSizer(),X::integer()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerisshown">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% isShown(This::wxSizer(), Index::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% isShown(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> bool() </c>
%% </p>
isShown(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_IsShown_1_0,
  <<ThisRef:32/?UI,Index:32/?UI>>);
isShown(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_IsShown_1_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_IsShown_1_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerlayout">external documentation</a>.
layout(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_Layout,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer(),X::term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% prepend(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> prepend(This,Window, []) </c></p>
%% <p><c>
%% prepend(This::wxSizer(), Item::wxSizerItem:wxSizerItem()) -> wxSizerItem:wxSizerItem() </c>
%% </p>

prepend(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  prepend(This,Window, []);
prepend(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(ItemT,wxSizerItem),
  wxe_util:call(?wxSizer_Prepend_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @spec (This::wxSizer(),X::integer()|term(),X::integer()|term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% prepend(This::wxSizer(), Width::integer(), Height::integer()) -> prepend(This,Width,Height, []) </c></p>
%% <p><c>
%% prepend(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), [Option]) -> wxSizerItem:wxSizerItem() </c>
%%<br /> Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% </p>
%% <p><c>
%% prepend(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags()) -> wxSizerItem:wxSizerItem() </c>
%% </p>

prepend(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  prepend(This,Width,Height, []);
prepend(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_Prepend_2_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_Prepend_2_0
     end,
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>);
prepend(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=FlagsT,ref=FlagsRef}) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizer_Prepend_2_3;
     _ -> ?CLASS(WindowT,wxSizer),
         ?CLASS(FlagsT,wxSizerFlags),
       ?wxSizer_Prepend_2_2
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,FlagsRef:32/?UI>>).

%% @spec (This::wxSizer(), Width::integer(), Height::integer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
prepend(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_Prepend_3,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizer(), Size::integer()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerprependspacer">external documentation</a>.
prependSpacer(#wx_ref{type=ThisT,ref=ThisRef},Size)
 when is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_PrependSpacer,
  <<ThisRef:32/?UI,Size:32/?UI>>).

%% @spec (This::wxSizer()) -> wxSizerItem:wxSizerItem()
%% @equiv prependStretchSpacer(This, [])
prependStretchSpacer(This)
 when is_record(This, wx_ref) ->
  prependStretchSpacer(This, []).

%% @spec (This::wxSizer(), [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {prop, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerprependstretchspacer">external documentation</a>.
prependStretchSpacer(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, Prop}, Acc) -> [<<1:32/?UI,Prop:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_PrependStretchSpacer,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerrecalcsizes">external documentation</a>.
recalcSizes(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_RecalcSizes,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxSizer(),X::integer()|wxSizer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerremove">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% remove(This::wxSizer(), Index::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% remove(This::wxSizer(), Sizer::wxSizer()) -> bool() </c>
%% </p>
remove(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_Remove_1_0,
  <<ThisRef:32/?UI,Index:32/?UI>>);
remove(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=SizerT,ref=SizerRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(SizerT,wxSizer),
  wxe_util:call(?wxSizer_Remove_1_1,
  <<ThisRef:32/?UI,SizerRef:32/?UI>>).

%% @spec (This::wxSizer(),X::term()|integer(),X::term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerreplace">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% replace(This::wxSizer(), Oldwin::wxWindow:wxWindow() | wxSizer(), Newwin::wxWindow:wxWindow() | wxSizer()) -> replace(This,Oldwin,Newwin, []) </c></p>
%% <p><c>
%% replace(This::wxSizer(), Index::integer(), Newitem::wxSizerItem:wxSizerItem()) -> bool() </c>
%% </p>

replace(This,Oldwin,Newwin)
 when is_record(This, wx_ref),is_record(Oldwin, wx_ref),is_record(Newwin, wx_ref) ->
  replace(This,Oldwin,Newwin, []);
replace(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=NewitemT,ref=NewitemRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(NewitemT,wxSizerItem),
  wxe_util:call(?wxSizer_Replace_2,
  <<ThisRef:32/?UI,Index:32/?UI,NewitemRef:32/?UI>>).

%% @spec (This::wxSizer(), Oldwin::wxWindow:wxWindow() | wxSizer(), Newwin::wxWindow:wxWindow() | wxSizer(), [Option]) -> bool()
%% Option = {recursive, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizerreplace">external documentation</a>.
replace(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=OldwinT,ref=OldwinRef},#wx_ref{type=NewwinT,ref=NewwinRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  OldwinOP = case ?CLASS_T(OldwinT,wxWindow) of
     true ->
         ?CLASS(NewwinT,wxWindow),
       ?wxSizer_Replace_3_1;
     _ -> ?CLASS(OldwinT,wxSizer),
         ?CLASS(NewwinT,wxSizer),
       ?wxSizer_Replace_3_0
     end,
  MOpts = fun({recursive, Recursive}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Recursive)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(OldwinOP,
  <<ThisRef:32/?UI,OldwinRef:32/?UI,NewwinRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxSizer(), X::integer(), Y::integer(), Width::integer(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetdimension">external documentation</a>.
setDimension(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_SetDimension,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxSizer(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetminsize">external documentation</a>.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_SetMinSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxSizer(), Width::integer(), Height::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetminsize">external documentation</a>.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_SetMinSize_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxSizer(),X::integer()|term(),Size::{W::integer(),H::integer()}) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetitemminsize">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setItemMinSize(This::wxSizer(), Index::integer(), Size::{W::integer(),H::integer()}) -> bool() </c>
%% </p>
%% <p><c>
%% setItemMinSize(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Size::{W::integer(),H::integer()}) -> bool() </c>
%% </p>
setItemMinSize(#wx_ref{type=ThisT,ref=ThisRef},Index,{SizeW,SizeH})
 when is_integer(Index),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_SetItemMinSize_2_0,
  <<ThisRef:32/?UI,Index:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>);
setItemMinSize(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_SetItemMinSize_2_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_SetItemMinSize_2_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxSizer(),X::integer()|term(),Width::integer(),Height::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetitemminsize">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setItemMinSize(This::wxSizer(), Index::integer(), Width::integer(), Height::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% setItemMinSize(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Width::integer(), Height::integer()) -> bool() </c>
%% </p>
setItemMinSize(#wx_ref{type=ThisT,ref=ThisRef},Index,Width,Height)
 when is_integer(Index),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_SetItemMinSize_3_0,
  <<ThisRef:32/?UI,Index:32/?UI,Width:32/?UI,Height:32/?UI>>);
setItemMinSize(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_SetItemMinSize_3_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_SetItemMinSize_3_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetsizehints">external documentation</a>.
setSizeHints(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizer_SetSizeHints,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizer(), Window::wxWindow:wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizersetvirtualsizehints">external documentation</a>.
setVirtualSizeHints(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizer_SetVirtualSizeHints,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxSizer(),X::integer()|term()|bool()) -> bool()|bool()|ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizershow">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% show(This::wxSizer(), Index::integer()) -> show(This,Index, []) </c></p>
%% <p><c>
%% show(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer()) -> show(This,Window, []) </c></p>
%% <p><c>
%% show(This::wxSizer(), Show::bool()) -> ok </c>
%% </p>

show(This,Index)
 when is_record(This, wx_ref),is_integer(Index) ->
  show(This,Index, []);

show(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  show(This,Window, []);
show(#wx_ref{type=ThisT,ref=ThisRef},Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_Show_1,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>).

%% @spec (This::wxSizer(),X::integer()|term(),[Option]) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxsizer.html#wxsizershow">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% show(This::wxSizer(), Index::integer(), [Option]) -> bool() </c>
%%<br /> Option = {show, bool()}
%% </p>
%% <p><c>
%% show(This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), [Option]) -> bool() </c>
%%<br /> Option = {show, bool()} | {recursive, bool()}
%% </p>
show(#wx_ref{type=ThisT,ref=ThisRef},Index, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_Show_2_0,
  <<ThisRef:32/?UI,Index:32/?UI, BinOpt/binary>>);
show(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxSizer_Show_2_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxSizer_Show_2_1
     end,
  MOpts = fun({show, Show}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Show)):32/?UI>>|Acc];
          ({recursive, Recursive}, Acc) -> [<<2:32/?UI,(wxe_util:from_bool(Recursive)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>).

