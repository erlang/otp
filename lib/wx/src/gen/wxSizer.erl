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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html">wxSizer</a>.
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

-export_type([wxSizer/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxSizer() :: wx:wx_object().
%% @equiv add(This,Window, [])
-spec add(This, Window) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer().

add(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  add(This,Window, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeradd">external documentation</a>.
%% <br /> Also:<br />
%% add(This, Window, [Option]) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()};<br />
%%       (This, Window, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags().<br />
%% 
-spec add(This, Width, Height) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer();
      (This, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()};
      (This, Window, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags().

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeradd">external documentation</a>.
-spec add(This, Width, Height, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeraddspacer">external documentation</a>.
-spec addSpacer(This, Size) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Size::integer().
addSpacer(#wx_ref{type=ThisT,ref=ThisRef},Size)
 when is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_AddSpacer,
  <<ThisRef:32/?UI,Size:32/?UI>>).

%% @equiv addStretchSpacer(This, [])
-spec addStretchSpacer(This) -> wxSizerItem:wxSizerItem() when
	This::wxSizer().

addStretchSpacer(This)
 when is_record(This, wx_ref) ->
  addStretchSpacer(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeraddstretchspacer">external documentation</a>.
-spec addStretchSpacer(This, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(),
	Option :: {'prop', integer()}.
addStretchSpacer(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, Prop}, Acc) -> [<<1:32/?UI,Prop:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_AddStretchSpacer,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizercalcmin">external documentation</a>.
-spec calcMin(This) -> {W::integer(), H::integer()} when
	This::wxSizer().
calcMin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_CalcMin,
  <<ThisRef:32/?UI>>).

%% @equiv clear(This, [])
-spec clear(This) -> 'ok' when
	This::wxSizer().

clear(This)
 when is_record(This, wx_ref) ->
  clear(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerclear">external documentation</a>.
-spec clear(This, [Option]) -> 'ok' when
	This::wxSizer(),
	Option :: {'delete_windows', boolean()}.
clear(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({delete_windows, Delete_windows}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Delete_windows)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxSizer_Clear,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerdetach">external documentation</a>.
%% <br /> Also:<br />
%% detach(This, Window) -> boolean() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer().<br />
%% 
-spec detach(This, Index) -> boolean() when
	This::wxSizer(), Index::integer();
      (This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerfit">external documentation</a>.
-spec fit(This, Window) -> {W::integer(), H::integer()} when
	This::wxSizer(), Window::wxWindow:wxWindow().
fit(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxSizer_Fit,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerfitinside">external documentation</a>.
-spec fitInside(This, Window) -> 'ok' when
	This::wxSizer(), Window::wxWindow:wxWindow().
fitInside(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizer_FitInside,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetchildren">external documentation</a>.
-spec getChildren(This) -> [wxSizerItem:wxSizerItem()] when
	This::wxSizer().
getChildren(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetChildren,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetitem">external documentation</a>.
%% <br /> Also:<br />
%% getItem(This, Index) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer().<br />
%% 
-spec getItem(This, Window) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer();
      (This, Index) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer().

getItem(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  getItem(This,Window, []);
getItem(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetItem_1,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetitem">external documentation</a>.
-spec getItem(This, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),
	Option :: {'recursive', boolean()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxSizer().
getSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxSizer().
getPosition(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetPosition,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetminsize">external documentation</a>.
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxSizer().
getMinSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_GetMinSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerhide">external documentation</a>.
%% <br /> Also:<br />
%% hide(This, Index) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer().<br />
%% 
-spec hide(This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer();
      (This, Index) -> boolean() when
	This::wxSizer(), Index::integer().

hide(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  hide(This,Window, []);
hide(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_Hide_1,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerhide">external documentation</a>.
-spec hide(This, Window, [Option]) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),
	Option :: {'recursive', boolean()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
-spec insert(This, Index, Item) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Item::wxSizerItem:wxSizerItem().
insert(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=ItemT,ref=ItemRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(ItemT,wxSizerItem),
  wxe_util:call(?wxSizer_Insert_2,
  <<ThisRef:32/?UI,Index:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Index, Window, [Option]) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()};<br />
%%       (This, Index, Window, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags().<br />
%% 
-spec insert(This, Index, Width, Height) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer();
      (This, Index, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()};
      (This, Index, Window, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags().

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
-spec insert(This, Index, Width, Height, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsertspacer">external documentation</a>.
-spec insertSpacer(This, Index, Size) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Size::integer().
insertSpacer(#wx_ref{type=ThisT,ref=ThisRef},Index,Size)
 when is_integer(Index),is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_InsertSpacer,
  <<ThisRef:32/?UI,Index:32/?UI,Size:32/?UI>>).

%% @equiv insertStretchSpacer(This,Index, [])
-spec insertStretchSpacer(This, Index) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer().

insertStretchSpacer(This,Index)
 when is_record(This, wx_ref),is_integer(Index) ->
  insertStretchSpacer(This,Index, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsertstretchspacer">external documentation</a>.
-spec insertStretchSpacer(This, Index, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(),
	Option :: {'prop', integer()}.
insertStretchSpacer(#wx_ref{type=ThisT,ref=ThisRef},Index, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, Prop}, Acc) -> [<<1:32/?UI,Prop:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_InsertStretchSpacer,
  <<ThisRef:32/?UI,Index:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerisshown">external documentation</a>.
%% <br /> Also:<br />
%% isShown(This, Window) -> boolean() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer().<br />
%% 
-spec isShown(This, Index) -> boolean() when
	This::wxSizer(), Index::integer();
      (This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerlayout">external documentation</a>.
-spec layout(This) -> 'ok' when
	This::wxSizer().
layout(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_Layout,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
-spec prepend(This, Item) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Item::wxSizerItem:wxSizerItem().
prepend(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(ItemT,wxSizerItem),
  wxe_util:call(?wxSizer_Prepend_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Window, [Option]) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()};<br />
%%       (This, Window, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags().<br />
%% 
-spec prepend(This, Width, Height) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer();
      (This, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()};
      (This, Window, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Flags::wxSizerFlags:wxSizerFlags().

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
-spec prepend(This, Width, Height, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprependspacer">external documentation</a>.
-spec prependSpacer(This, Size) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Size::integer().
prependSpacer(#wx_ref{type=ThisT,ref=ThisRef},Size)
 when is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:call(?wxSizer_PrependSpacer,
  <<ThisRef:32/?UI,Size:32/?UI>>).

%% @equiv prependStretchSpacer(This, [])
-spec prependStretchSpacer(This) -> wxSizerItem:wxSizerItem() when
	This::wxSizer().

prependStretchSpacer(This)
 when is_record(This, wx_ref) ->
  prependStretchSpacer(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprependstretchspacer">external documentation</a>.
-spec prependStretchSpacer(This, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(),
	Option :: {'prop', integer()}.
prependStretchSpacer(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, Prop}, Acc) -> [<<1:32/?UI,Prop:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxSizer_PrependStretchSpacer,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerrecalcsizes">external documentation</a>.
-spec recalcSizes(This) -> 'ok' when
	This::wxSizer().
recalcSizes(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_RecalcSizes,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerremove">external documentation</a>.
%% <br /> Also:<br />
%% remove(This, Sizer) -> boolean() when<br />
%% 	This::wxSizer(), Sizer::wxSizer().<br />
%% 
-spec remove(This, Index) -> boolean() when
	This::wxSizer(), Index::integer();
      (This, Sizer) -> boolean() when
	This::wxSizer(), Sizer::wxSizer().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerreplace">external documentation</a>.
%% <br /> Also:<br />
%% replace(This, Index, Newitem) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer(), Newitem::wxSizerItem:wxSizerItem().<br />
%% 
-spec replace(This, Oldwin, Newwin) -> boolean() when
	This::wxSizer(), Oldwin::wxWindow:wxWindow() | wxSizer(), Newwin::wxWindow:wxWindow() | wxSizer();
      (This, Index, Newitem) -> boolean() when
	This::wxSizer(), Index::integer(), Newitem::wxSizerItem:wxSizerItem().

replace(This,Oldwin,Newwin)
 when is_record(This, wx_ref),is_record(Oldwin, wx_ref),is_record(Newwin, wx_ref) ->
  replace(This,Oldwin,Newwin, []);
replace(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=NewitemT,ref=NewitemRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(NewitemT,wxSizerItem),
  wxe_util:call(?wxSizer_Replace_2,
  <<ThisRef:32/?UI,Index:32/?UI,NewitemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerreplace">external documentation</a>.
-spec replace(This, Oldwin, Newwin, [Option]) -> boolean() when
	This::wxSizer(), Oldwin::wxWindow:wxWindow() | wxSizer(), Newwin::wxWindow:wxWindow() | wxSizer(),
	Option :: {'recursive', boolean()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetdimension">external documentation</a>.
-spec setDimension(This, X, Y, Width, Height) -> 'ok' when
	This::wxSizer(), X::integer(), Y::integer(), Width::integer(), Height::integer().
setDimension(#wx_ref{type=ThisT,ref=ThisRef},X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_SetDimension,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetminsize">external documentation</a>.
-spec setMinSize(This, Size) -> 'ok' when
	This::wxSizer(), Size::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_SetMinSize_1,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetminsize">external documentation</a>.
-spec setMinSize(This, Width, Height) -> 'ok' when
	This::wxSizer(), Width::integer(), Height::integer().
setMinSize(#wx_ref{type=ThisT,ref=ThisRef},Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:cast(?wxSizer_SetMinSize_2,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetitemminsize">external documentation</a>.
%% <br /> Also:<br />
%% setItemMinSize(This, Window, Size) -> boolean() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec setItemMinSize(This, Index, Size) -> boolean() when
	This::wxSizer(), Index::integer(), Size::{W::integer(), H::integer()};
      (This, Window, Size) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Size::{W::integer(), H::integer()}.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetitemminsize">external documentation</a>.
%% <br /> Also:<br />
%% setItemMinSize(This, Window, Width, Height) -> boolean() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Width::integer(), Height::integer().<br />
%% 
-spec setItemMinSize(This, Index, Width, Height) -> boolean() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer();
      (This, Window, Width, Height) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(), Width::integer(), Height::integer().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetsizehints">external documentation</a>.
-spec setSizeHints(This, Window) -> 'ok' when
	This::wxSizer(), Window::wxWindow:wxWindow().
setSizeHints(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizer_SetSizeHints,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetvirtualsizehints">external documentation</a>.
-spec setVirtualSizeHints(This, Window) -> 'ok' when
	This::wxSizer(), Window::wxWindow:wxWindow().
setVirtualSizeHints(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:cast(?wxSizer_SetVirtualSizeHints,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizershow">external documentation</a>.
%% <br /> Also:<br />
%% show(This, Window) -> boolean() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer();<br />
%%       (This, Show) -> 'ok' when<br />
%% 	This::wxSizer(), Show::boolean().<br />
%% 
-spec show(This, Index) -> boolean() when
	This::wxSizer(), Index::integer();
      (This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer();
      (This, Show) -> 'ok' when
	This::wxSizer(), Show::boolean().

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizershow">external documentation</a>.
%% <br /> Also:<br />
%% show(This, Window, [Option]) -> boolean() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),<br />
%% 	Option :: {'show', boolean()}<br />
%% 		 | {'recursive', boolean()}.<br />
%% 
-spec show(This, Index, [Option]) -> boolean() when
	This::wxSizer(), Index::integer(),
	Option :: {'show', boolean()};
      (This, Window, [Option]) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer(),
	Option :: {'show', boolean()}
		 | {'recursive', boolean()}.
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

