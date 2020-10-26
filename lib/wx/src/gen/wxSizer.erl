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

-module(wxSizer).
-include("wxe.hrl").
-export([add/2,add/3,add/4,addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,
  calcMin/1,clear/1,clear/2,detach/2,fit/2,fitInside/2,getChildren/1,getItem/2,
  getItem/3,getMinSize/1,getPosition/1,getSize/1,hide/2,hide/3,insert/3,
  insert/4,insert/5,insertSpacer/3,insertStretchSpacer/2,insertStretchSpacer/3,
  isShown/2,layout/1,prepend/2,prepend/3,prepend/4,prependSpacer/2,prependStretchSpacer/1,
  prependStretchSpacer/2,recalcSizes/1,remove/2,replace/3,replace/4,
  setDimension/3,setDimension/5,setItemMinSize/3,setItemMinSize/4,setMinSize/2,
  setMinSize/3,setSizeHints/2,setVirtualSizeHints/2,show/2,show/3,showItems/2]).

%% inherited exports
-export([parent_class/1]).

-type wxSizer() :: wx:wx_object().
-export_type([wxSizer/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv add(This,Window, [])
-spec add(This, Window) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer().

add(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  add(This,Window, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeradd">external documentation</a>.
%% <br /> Also:<br />
%% add(This, Window, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();<br />
%%       (This, Window, [Option]) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()}.<br />
%% 
-spec add(This, Width, Height) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer();
      (This, Window, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();
      (This, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.

add(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  add(This,Width,Height, []);
add(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,#wx_ref{type=FlagsT}=Flags) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   ?CLASS(FlagsT,wxSizerFlags),   wxWindow;
    IswxSizer ->   ?CLASS(FlagsT,wxSizerFlags),   wxSizer;
    true -> error({badarg, WindowT})
  end,
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType),Flags,?get_env(),?wxSizer_Add_2_0),
  wxe_util:rec(?wxSizer_Add_2_0);
add(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizer_Add_2_1),
  wxe_util:rec(?wxSizer_Add_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeradd">external documentation</a>.
%% <br /> Also:<br />
%% add(This, Width, Height, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().<br />
%% 
-spec add(This, Width, Height, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()};
      (This, Width, Height, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().
add(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxSizer_Add_3_0),
  wxe_util:rec(?wxSizer_Add_3_0);
add(#wx_ref{type=ThisT}=This,Width,Height,#wx_ref{type=FlagsT}=Flags)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(This,Width,Height,Flags,?get_env(),?wxSizer_Add_3_1),
  wxe_util:rec(?wxSizer_Add_3_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizeraddspacer">external documentation</a>.
-spec addSpacer(This, Size) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Size::integer().
addSpacer(#wx_ref{type=ThisT}=This,Size)
 when is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxSizer_AddSpacer),
  wxe_util:rec(?wxSizer_AddSpacer).

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
addStretchSpacer(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, _prop} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxSizer_AddStretchSpacer),
  wxe_util:rec(?wxSizer_AddStretchSpacer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizercalcmin">external documentation</a>.
-spec calcMin(This) -> {W::integer(), H::integer()} when
	This::wxSizer().
calcMin(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxSizer_CalcMin),
  wxe_util:rec(?wxSizer_CalcMin).

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
clear(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({delete_windows, _delete_windows} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxSizer_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerdetach">external documentation</a>.
%% <br /> Also:<br />
%% detach(This, Index) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer().<br />
%% 
-spec detach(This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer();
      (This, Index) -> boolean() when
	This::wxSizer(), Index::integer().
detach(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType),?get_env(),?wxSizer_Detach_1_0),
  wxe_util:rec(?wxSizer_Detach_1_0);
detach(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxSizer_Detach_1_1),
  wxe_util:rec(?wxSizer_Detach_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerfit">external documentation</a>.
-spec fit(This, Window) -> {W::integer(), H::integer()} when
	This::wxSizer(), Window::wxWindow:wxWindow().
fit(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSizer_Fit),
  wxe_util:rec(?wxSizer_Fit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerfitinside">external documentation</a>.
-spec setVirtualSizeHints(This, Window) -> 'ok' when
	This::wxSizer(), Window::wxWindow:wxWindow().

setVirtualSizeHints(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  fitInside(This,Window).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerfitinside">external documentation</a>.
-spec fitInside(This, Window) -> 'ok' when
	This::wxSizer(), Window::wxWindow:wxWindow().
fitInside(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSizer_FitInside).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetchildren">external documentation</a>.
-spec getChildren(This) -> [wxSizerItem:wxSizerItem()] when
	This::wxSizer().
getChildren(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxSizer_GetChildren),
  wxe_util:rec(?wxSizer_GetChildren).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetitem">external documentation</a>.
%% <br /> Also:<br />
%% getItem(This, Index) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer().<br />
%% 
-spec getItem(This, Window) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer();
      (This, Index) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer().

getItem(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  getItem(This,Window, []);
getItem(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxSizer_GetItem_1),
  wxe_util:rec(?wxSizer_GetItem_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetitem">external documentation</a>.
-spec getItem(This, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'recursive', boolean()}.
getItem(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({recursive, _recursive} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizer_GetItem_2),
  wxe_util:rec(?wxSizer_GetItem_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetsize">external documentation</a>.
-spec getSize(This) -> {W::integer(), H::integer()} when
	This::wxSizer().
getSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxSizer_GetSize),
  wxe_util:rec(?wxSizer_GetSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetposition">external documentation</a>.
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxSizer().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxSizer_GetPosition),
  wxe_util:rec(?wxSizer_GetPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizergetminsize">external documentation</a>.
-spec getMinSize(This) -> {W::integer(), H::integer()} when
	This::wxSizer().
getMinSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxSizer_GetMinSize),
  wxe_util:rec(?wxSizer_GetMinSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerhide">external documentation</a>.
%% <br /> Also:<br />
%% hide(This, Index) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer().<br />
%% 
-spec hide(This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer();
      (This, Index) -> boolean() when
	This::wxSizer(), Index::integer().

hide(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  hide(This,Window, []);
hide(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxSizer_Hide_1),
  wxe_util:rec(?wxSizer_Hide_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerhide">external documentation</a>.
-spec hide(This, Window, [Option]) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'recursive', boolean()}.
hide(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({recursive, _recursive} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizer_Hide_2),
  wxe_util:rec(?wxSizer_Hide_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
-spec insert(This, Index, Item) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Item::wxSizerItem:wxSizerItem().
insert(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=ItemT}=Item)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(ItemT,wxSizerItem),
  wxe_util:queue_cmd(This,Index,Item,?get_env(),?wxSizer_Insert_2),
  wxe_util:rec(?wxSizer_Insert_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Index, Window, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();<br />
%%       (This, Index, Window, [Option]) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()}.<br />
%% 
-spec insert(This, Index, Width, Height) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer();
      (This, Index, Window, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();
      (This, Index, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.

insert(This,Index,Width,Height)
 when is_record(This, wx_ref),is_integer(Index),is_integer(Width),is_integer(Height) ->
  insert(This,Index,Width,Height, []);
insert(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=WindowT}=Window,#wx_ref{type=FlagsT}=Flags)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   ?CLASS(FlagsT,wxSizerFlags),   wxWindow;
    IswxSizer ->   ?CLASS(FlagsT,wxSizerFlags),   wxSizer;
    true -> error({badarg, WindowT})
  end,
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(This,Index,wx:typeCast(Window, WindowType),Flags,?get_env(),?wxSizer_Insert_3_0),
  wxe_util:rec(?wxSizer_Insert_3_0);
insert(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=WindowT}=Window, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Index,wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizer_Insert_3_1),
  wxe_util:rec(?wxSizer_Insert_3_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Index, Width, Height, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Index::integer(), Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().<br />
%% 
-spec insert(This, Index, Width, Height, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()};
      (This, Index, Width, Height, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().
insert(#wx_ref{type=ThisT}=This,Index,Width,Height, Options)
 when is_integer(Index),is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Index,Width,Height, Opts,?get_env(),?wxSizer_Insert_4_0),
  wxe_util:rec(?wxSizer_Insert_4_0);
insert(#wx_ref{type=ThisT}=This,Index,Width,Height,#wx_ref{type=FlagsT}=Flags)
 when is_integer(Index),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(This,Index,Width,Height,Flags,?get_env(),?wxSizer_Insert_4_1),
  wxe_util:rec(?wxSizer_Insert_4_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerinsertspacer">external documentation</a>.
-spec insertSpacer(This, Index, Size) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Index::integer(), Size::integer().
insertSpacer(#wx_ref{type=ThisT}=This,Index,Size)
 when is_integer(Index),is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,Size,?get_env(),?wxSizer_InsertSpacer),
  wxe_util:rec(?wxSizer_InsertSpacer).

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
insertStretchSpacer(#wx_ref{type=ThisT}=This,Index, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, _prop} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Index, Opts,?get_env(),?wxSizer_InsertStretchSpacer),
  wxe_util:rec(?wxSizer_InsertStretchSpacer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerisshown">external documentation</a>.
%% <br /> Also:<br />
%% isShown(This, Index) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer().<br />
%% 
-spec isShown(This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer();
      (This, Index) -> boolean() when
	This::wxSizer(), Index::integer().
isShown(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType),?get_env(),?wxSizer_IsShown_1_0),
  wxe_util:rec(?wxSizer_IsShown_1_0);
isShown(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxSizer_IsShown_1_1),
  wxe_util:rec(?wxSizer_IsShown_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerlayout">external documentation</a>.
-spec recalcSizes(This) -> 'ok' when
	This::wxSizer().

recalcSizes(This)
 when is_record(This, wx_ref) ->
  layout(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerlayout">external documentation</a>.
-spec layout(This) -> 'ok' when
	This::wxSizer().
layout(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxSizer_Layout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
-spec prepend(This, Item) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Item::wxSizerItem:wxSizerItem().
prepend(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(ItemT,wxSizerItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxSizer_Prepend_1),
  wxe_util:rec(?wxSizer_Prepend_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Window, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();<br />
%%       (This, Window, [Option]) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),<br />
%% 	Option :: {'proportion', integer()}<br />
%% 		 | {'flag', integer()}<br />
%% 		 | {'border', integer()}<br />
%% 		 | {'userData', wx:wx_object()}.<br />
%% 
-spec prepend(This, Width, Height) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer();
      (This, Window, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Flags::wxSizerFlags:wxSizerFlags();
      (This, Window, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()}.

prepend(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  prepend(This,Width,Height, []);
prepend(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,#wx_ref{type=FlagsT}=Flags) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   ?CLASS(FlagsT,wxSizerFlags),   wxWindow;
    IswxSizer ->   ?CLASS(FlagsT,wxSizerFlags),   wxSizer;
    true -> error({badarg, WindowT})
  end,
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType),Flags,?get_env(),?wxSizer_Prepend_2_0),
  wxe_util:rec(?wxSizer_Prepend_2_0);
prepend(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizer_Prepend_2_1),
  wxe_util:rec(?wxSizer_Prepend_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Width, Height, Flags) -> wxSizerItem:wxSizerItem() when<br />
%% 	This::wxSizer(), Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().<br />
%% 
-spec prepend(This, Width, Height, [Option]) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer(),
	Option :: {'proportion', integer()}
		 | {'flag', integer()}
		 | {'border', integer()}
		 | {'userData', wx:wx_object()};
      (This, Width, Height, Flags) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Width::integer(), Height::integer(), Flags::wxSizerFlags:wxSizerFlags().
prepend(#wx_ref{type=ThisT}=This,Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          ({flag, _flag} = Arg) -> Arg;
          ({border, _border} = Arg) -> Arg;
          ({userData, #wx_ref{type=UserDataT}} = Arg) ->   ?CLASS(UserDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Width,Height, Opts,?get_env(),?wxSizer_Prepend_3_0),
  wxe_util:rec(?wxSizer_Prepend_3_0);
prepend(#wx_ref{type=ThisT}=This,Width,Height,#wx_ref{type=FlagsT}=Flags)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(FlagsT,wxSizerFlags),
  wxe_util:queue_cmd(This,Width,Height,Flags,?get_env(),?wxSizer_Prepend_3_1),
  wxe_util:rec(?wxSizer_Prepend_3_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerprependspacer">external documentation</a>.
-spec prependSpacer(This, Size) -> wxSizerItem:wxSizerItem() when
	This::wxSizer(), Size::integer().
prependSpacer(#wx_ref{type=ThisT}=This,Size)
 when is_integer(Size) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxSizer_PrependSpacer),
  wxe_util:rec(?wxSizer_PrependSpacer).

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
prependStretchSpacer(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({prop, _prop} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxSizer_PrependStretchSpacer),
  wxe_util:rec(?wxSizer_PrependStretchSpacer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerremove">external documentation</a>.
%% <br /> Also:<br />
%% remove(This, Sizer) -> boolean() when<br />
%% 	This::wxSizer(), Sizer::wxSizer().<br />
%% 
-spec remove(This, Index) -> boolean() when
	This::wxSizer(), Index::integer();
      (This, Sizer) -> boolean() when
	This::wxSizer(), Sizer::wxSizer().
remove(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxSizer_Remove_1_0),
  wxe_util:rec(?wxSizer_Remove_1_0);
remove(#wx_ref{type=ThisT}=This,#wx_ref{type=SizerT}=Sizer) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(SizerT,wxSizer),
  wxe_util:queue_cmd(This,Sizer,?get_env(),?wxSizer_Remove_1_1),
  wxe_util:rec(?wxSizer_Remove_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerreplace">external documentation</a>.
%% <br /> Also:<br />
%% replace(This, Index, Newitem) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer(), Newitem::wxSizerItem:wxSizerItem().<br />
%% 
-spec replace(This, Oldwin, Newwin) -> boolean() when
	This::wxSizer(), Oldwin::wxWindow:wxWindow() | wxSizer:wxSizer(), Newwin::wxWindow:wxWindow() | wxSizer:wxSizer();
      (This, Index, Newitem) -> boolean() when
	This::wxSizer(), Index::integer(), Newitem::wxSizerItem:wxSizerItem().

replace(This,Oldwin,Newwin)
 when is_record(This, wx_ref),is_record(Oldwin, wx_ref),is_record(Newwin, wx_ref) ->
  replace(This,Oldwin,Newwin, []);
replace(#wx_ref{type=ThisT}=This,Index,#wx_ref{type=NewitemT}=Newitem)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(NewitemT,wxSizerItem),
  wxe_util:queue_cmd(This,Index,Newitem,?get_env(),?wxSizer_Replace_2),
  wxe_util:rec(?wxSizer_Replace_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizerreplace">external documentation</a>.
-spec replace(This, Oldwin, Newwin, [Option]) -> boolean() when
	This::wxSizer(), Oldwin::wxWindow:wxWindow() | wxSizer:wxSizer(), Newwin::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'recursive', boolean()}.
replace(#wx_ref{type=ThisT}=This,#wx_ref{type=OldwinT}=Oldwin,#wx_ref{type=NewwinT}=Newwin, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(OldwinT,wxWindow),
  IswxSizer = ?CLASS_T(OldwinT,wxSizer),
  OldwinType = if
    IswxWindow ->   ?CLASS(NewwinT,wxWindow),   wxWindow;
    IswxSizer ->   ?CLASS(NewwinT,wxSizer),   wxSizer;
    true -> error({badarg, OldwinT})
  end,
  IswxWindow = ?CLASS_T(NewwinT,wxWindow),
  IswxSizer = ?CLASS_T(NewwinT,wxSizer),
  NewwinType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, NewwinT})
  end,
  MOpts = fun({recursive, _recursive} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,wx:typeCast(Oldwin, OldwinType),wx:typeCast(Newwin, NewwinType), Opts,?get_env(),?wxSizer_Replace_3),
  wxe_util:rec(?wxSizer_Replace_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetdimension">external documentation</a>.
-spec setDimension(This, Pos, Size) -> 'ok' when
	This::wxSizer(), Pos::{X::integer(), Y::integer()}, Size::{W::integer(), H::integer()}.
setDimension(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos,{SizeW,SizeH} = Size)
 when is_integer(PosX),is_integer(PosY),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Pos,Size,?get_env(),?wxSizer_SetDimension_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetdimension">external documentation</a>.
-spec setDimension(This, X, Y, Width, Height) -> 'ok' when
	This::wxSizer(), X::integer(), Y::integer(), Width::integer(), Height::integer().
setDimension(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxSizer_SetDimension_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetminsize">external documentation</a>.
-spec setMinSize(This, Size) -> 'ok' when
	This::wxSizer(), Size::{W::integer(), H::integer()}.
setMinSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxSizer_SetMinSize_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetminsize">external documentation</a>.
-spec setMinSize(This, Width, Height) -> 'ok' when
	This::wxSizer(), Width::integer(), Height::integer().
setMinSize(#wx_ref{type=ThisT}=This,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Width,Height,?get_env(),?wxSizer_SetMinSize_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetitemminsize">external documentation</a>.
%% <br /> Also:<br />
%% setItemMinSize(This, Index, Size) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer(), Size::{W::integer(), H::integer()}.<br />
%% 
-spec setItemMinSize(This, Window, Size) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Size::{W::integer(), H::integer()};
      (This, Index, Size) -> boolean() when
	This::wxSizer(), Index::integer(), Size::{W::integer(), H::integer()}.
setItemMinSize(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType),Size,?get_env(),?wxSizer_SetItemMinSize_2_0),
  wxe_util:rec(?wxSizer_SetItemMinSize_2_0);
setItemMinSize(#wx_ref{type=ThisT}=This,Index,{SizeW,SizeH} = Size)
 when is_integer(Index),is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,Size,?get_env(),?wxSizer_SetItemMinSize_2_1),
  wxe_util:rec(?wxSizer_SetItemMinSize_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetitemminsize">external documentation</a>.
%% <br /> Also:<br />
%% setItemMinSize(This, Index, Width, Height) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer(), Width::integer(), Height::integer().<br />
%% 
-spec setItemMinSize(This, Window, Width, Height) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Width::integer(), Height::integer();
      (This, Index, Width, Height) -> boolean() when
	This::wxSizer(), Index::integer(), Width::integer(), Height::integer().
setItemMinSize(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,Width,Height)
 when is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType),Width,Height,?get_env(),?wxSizer_SetItemMinSize_3_0),
  wxe_util:rec(?wxSizer_SetItemMinSize_3_0);
setItemMinSize(#wx_ref{type=ThisT}=This,Index,Width,Height)
 when is_integer(Index),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Index,Width,Height,?get_env(),?wxSizer_SetItemMinSize_3_1),
  wxe_util:rec(?wxSizer_SetItemMinSize_3_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizersetsizehints">external documentation</a>.
-spec setSizeHints(This, Window) -> 'ok' when
	This::wxSizer(), Window::wxWindow:wxWindow().
setSizeHints(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxSizer),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxSizer_SetSizeHints).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizershow">external documentation</a>.
%% <br /> Also:<br />
%% show(This, Index) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer();<br />
%%       (This, Show) -> 'ok' when<br />
%% 	This::wxSizer(), Show::boolean().<br />
%% 
-spec show(This, Window) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer();
      (This, Index) -> boolean() when
	This::wxSizer(), Index::integer();
      (This, Show) -> 'ok' when
	This::wxSizer(), Show::boolean().

show(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  show(This,Window, []);

show(This,Index)
 when is_record(This, wx_ref),is_integer(Index) ->
  show(This,Index, []);
show(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxSizer_Show_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizershow">external documentation</a>.
%% <br /> Also:<br />
%% show(This, Index, [Option]) -> boolean() when<br />
%% 	This::wxSizer(), Index::integer(),<br />
%% 	Option :: {'show', boolean()}.<br />
%% 
-spec show(This, Window, [Option]) -> boolean() when
	This::wxSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(),
	Option :: {'show', boolean()}
		 | {'recursive', boolean()};
      (This, Index, [Option]) -> boolean() when
	This::wxSizer(), Index::integer(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  IswxWindow = ?CLASS_T(WindowT,wxWindow),
  IswxSizer = ?CLASS_T(WindowT,wxSizer),
  WindowType = if
    IswxWindow ->   wxWindow;
    IswxSizer ->   wxSizer;
    true -> error({badarg, WindowT})
  end,
  MOpts = fun({show, _show} = Arg) -> Arg;
          ({recursive, _recursive} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,wx:typeCast(Window, WindowType), Opts,?get_env(),?wxSizer_Show_2_0),
  wxe_util:rec(?wxSizer_Show_2_0);
show(#wx_ref{type=ThisT}=This,Index, Options)
 when is_integer(Index),is_list(Options) ->
  ?CLASS(ThisT,wxSizer),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Index, Opts,?get_env(),?wxSizer_Show_2_1),
  wxe_util:rec(?wxSizer_Show_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsizer.html#wxsizershowitems">external documentation</a>.
-spec showItems(This, Show) -> 'ok' when
	This::wxSizer(), Show::boolean().
showItems(#wx_ref{type=ThisT}=This,Show)
 when is_boolean(Show) ->
  ?CLASS(ThisT,wxSizer),
  wxe_util:queue_cmd(This,Show,?get_env(),?wxSizer_ShowItems).

