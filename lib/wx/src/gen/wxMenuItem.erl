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

-module(wxMenuItem).
-include("wxe.hrl").
-export([check/1,check/2,destroy/1,enable/1,enable/2,getBitmap/1,getHelp/1,getId/1,
  getItemLabel/1,getItemLabelText/1,getKind/1,getLabel/1,getLabelFromText/1,
  getLabelText/1,getMenu/1,getSubMenu/1,getText/1,isCheckable/1,isChecked/1,
  isEnabled/1,isSeparator/1,isSubMenu/1,new/0,new/1,setBitmap/2,setHelp/2,
  setItemLabel/2,setMenu/2,setSubMenu/2,setText/2]).

%% inherited exports
-export([parent_class/1]).

-type wxMenuItem() :: wx:wx_object().
-export_type([wxMenuItem/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxMenuItem().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemwxmenuitem">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec new([Option]) -> wxMenuItem() when
	Option :: {'parentMenu', wxMenu:wxMenu()}
		 | {'id', integer()}
		 | {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}
		 | {'subMenu', wxMenu:wxMenu()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({parentMenu, #wx_ref{type=ParentMenuT}} = Arg) ->   ?CLASS(ParentMenuT,wxMenu),Arg;
          ({id, _id} = Arg) -> Arg;
          ({text, Text}) ->   Text_UC = unicode:characters_to_binary(Text),{text,Text_UC};
          ({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          ({kind, _kind} = Arg) -> Arg;
          ({subMenu, #wx_ref{type=SubMenuT}} = Arg) ->   ?CLASS(SubMenuT,wxMenu),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxMenuItem_new),
  wxe_util:rec(?wxMenuItem_new).

%% @equiv check(This, [])
-spec check(This) -> 'ok' when
	This::wxMenuItem().

check(This)
 when is_record(This, wx_ref) ->
  check(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemcheck">external documentation</a>.
-spec check(This, [Option]) -> 'ok' when
	This::wxMenuItem(),
	Option :: {'check', boolean()}.
check(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({check, _check} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMenuItem_Check).

%% @equiv enable(This, [])
-spec enable(This) -> 'ok' when
	This::wxMenuItem().

enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemenable">external documentation</a>.
-spec enable(This, [Option]) -> 'ok' when
	This::wxMenuItem(),
	Option :: {'enable', boolean()}.
enable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxMenuItem_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetbitmap">external documentation</a>.
-spec getBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxMenuItem().
getBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetBitmap),
  wxe_util:rec(?wxMenuItem_GetBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgethelp">external documentation</a>.
-spec getHelp(This) -> unicode:charlist() when
	This::wxMenuItem().
getHelp(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetHelp),
  wxe_util:rec(?wxMenuItem_GetHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetid">external documentation</a>.
-spec getId(This) -> integer() when
	This::wxMenuItem().
getId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetId),
  wxe_util:rec(?wxMenuItem_GetId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetkind">external documentation</a>.
%%<br /> Res = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec getKind(This) -> wx:wx_enum() when
	This::wxMenuItem().
getKind(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetKind),
  wxe_util:rec(?wxMenuItem_GetKind).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetlabeltext">external documentation</a>.
-spec getLabelFromText(Text) -> unicode:charlist() when
	Text::unicode:chardata().

getLabelFromText(Text)
 when ?is_chardata(Text) ->
  getLabelText(Text).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetlabeltext">external documentation</a>.
-spec getLabelText(Text) -> unicode:charlist() when
	Text::unicode:chardata().
getLabelText(Text)
 when ?is_chardata(Text) ->
  Text_UC = unicode:characters_to_binary(Text),
  wxe_util:queue_cmd(Text_UC,?get_env(),?wxMenuItem_GetLabelText),
  wxe_util:rec(?wxMenuItem_GetLabelText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabel">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxMenuItem().

getText(This)
 when is_record(This, wx_ref) ->
  getItemLabel(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabel">external documentation</a>.
-spec getItemLabel(This) -> unicode:charlist() when
	This::wxMenuItem().
getItemLabel(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetItemLabel),
  wxe_util:rec(?wxMenuItem_GetItemLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabeltext">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxMenuItem().

getLabel(This)
 when is_record(This, wx_ref) ->
  getItemLabelText(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetitemlabeltext">external documentation</a>.
-spec getItemLabelText(This) -> unicode:charlist() when
	This::wxMenuItem().
getItemLabelText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetItemLabelText),
  wxe_util:rec(?wxMenuItem_GetItemLabelText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetmenu">external documentation</a>.
-spec getMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetMenu),
  wxe_util:rec(?wxMenuItem_GetMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetsubmenu">external documentation</a>.
-spec getSubMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getSubMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_GetSubMenu),
  wxe_util:rec(?wxMenuItem_GetSubMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemischeckable">external documentation</a>.
-spec isCheckable(This) -> boolean() when
	This::wxMenuItem().
isCheckable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsCheckable),
  wxe_util:rec(?wxMenuItem_IsCheckable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemischecked">external documentation</a>.
-spec isChecked(This) -> boolean() when
	This::wxMenuItem().
isChecked(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsChecked),
  wxe_util:rec(?wxMenuItem_IsChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemisenabled">external documentation</a>.
-spec isEnabled(This) -> boolean() when
	This::wxMenuItem().
isEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsEnabled),
  wxe_util:rec(?wxMenuItem_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemisseparator">external documentation</a>.
-spec isSeparator(This) -> boolean() when
	This::wxMenuItem().
isSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsSeparator),
  wxe_util:rec(?wxMenuItem_IsSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemissubmenu">external documentation</a>.
-spec isSubMenu(This) -> boolean() when
	This::wxMenuItem().
isSubMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuItem_IsSubMenu),
  wxe_util:rec(?wxMenuItem_IsSubMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetbitmap">external documentation</a>.
-spec setBitmap(This, Bmp) -> 'ok' when
	This::wxMenuItem(), Bmp::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(This,Bmp,?get_env(),?wxMenuItem_SetBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsethelp">external documentation</a>.
-spec setHelp(This, HelpString) -> 'ok' when
	This::wxMenuItem(), HelpString::unicode:chardata().
setHelp(#wx_ref{type=ThisT}=This,HelpString)
 when ?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenuItem),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,HelpString_UC,?get_env(),?wxMenuItem_SetHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetmenu">external documentation</a>.
-spec setMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxMenuItem_SetMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetsubmenu">external documentation</a>.
-spec setSubMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setSubMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxMenuItem_SetSubMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetitemlabel">external documentation</a>.
-spec setText(This, Label) -> 'ok' when
	This::wxMenuItem(), Label::unicode:chardata().

setText(This,Label)
 when is_record(This, wx_ref),?is_chardata(Label) ->
  setItemLabel(This,Label).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetitemlabel">external documentation</a>.
-spec setItemLabel(This, Label) -> 'ok' when
	This::wxMenuItem(), Label::unicode:chardata().
setItemLabel(#wx_ref{type=ThisT}=This,Label)
 when ?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuItem),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Label_UC,?get_env(),?wxMenuItem_SetItemLabel).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMenuItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuItem),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
