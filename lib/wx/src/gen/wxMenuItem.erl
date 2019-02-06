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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html">wxMenuItem</a>.
%% @type wxMenuItem().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxMenuItem).
-include("wxe.hrl").
-export([check/1,check/2,destroy/1,enable/1,enable/2,getBitmap/1,getHelp/1,getId/1,
  getKind/1,getLabel/1,getLabelFromText/1,getMenu/1,getSubMenu/1,getText/1,
  isCheckable/1,isChecked/1,isEnabled/1,isSeparator/1,isSubMenu/1,new/0,
  new/1,setBitmap/2,setHelp/2,setMenu/2,setSubMenu/2,setText/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxMenuItem/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxMenuItem() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxMenuItem().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemwxmenuitem">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec new([Option]) -> wxMenuItem() when
	Option :: {'parentMenu', wxMenu:wxMenu()}
		 | {'id', integer()}
		 | {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}
		 | {'subMenu', wxMenu:wxMenu()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({parentMenu, #wx_ref{type=ParentMenuT,ref=ParentMenuRef}}, Acc) ->   ?CLASS(ParentMenuT,wxMenu),[<<1:32/?UI,ParentMenuRef:32/?UI>>|Acc];
          ({id, Id}, Acc) -> [<<2:32/?UI,Id:32/?UI>>|Acc];
          ({text, Text}, Acc) ->   Text_UC = unicode:characters_to_binary([Text,0]),[<<3:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<4:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({kind, Kind}, Acc) -> [<<5:32/?UI,Kind:32/?UI>>|Acc];
          ({subMenu, #wx_ref{type=SubMenuT,ref=SubMenuRef}}, Acc) ->   ?CLASS(SubMenuT,wxMenu),[<<6:32/?UI,SubMenuRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxMenuItem_new,
  <<BinOpt/binary>>).

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
check(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({check, Check}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Check)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxMenuItem_Check,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

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
enable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxMenuItem_Enable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetbitmap">external documentation</a>.
-spec getBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxMenuItem().
getBitmap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetBitmap,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgethelp">external documentation</a>.
-spec getHelp(This) -> unicode:charlist() when
	This::wxMenuItem().
getHelp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetHelp,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetid">external documentation</a>.
-spec getId(This) -> integer() when
	This::wxMenuItem().
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetId,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetkind">external documentation</a>.
%%<br /> Res = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec getKind(This) -> wx:wx_enum() when
	This::wxMenuItem().
getKind(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetKind,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetlabel">external documentation</a>.
-spec getLabel(This) -> unicode:charlist() when
	This::wxMenuItem().
getLabel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetLabel,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetlabelfromtext">external documentation</a>.
-spec getLabelFromText(Text) -> unicode:charlist() when
	Text::unicode:chardata().
getLabelFromText(Text)
 when ?is_chardata(Text) ->
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxMenuItem_GetLabelFromText,
  <<(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetmenu">external documentation</a>.
-spec getMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetMenu,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgettext">external documentation</a>.
-spec getText(This) -> unicode:charlist() when
	This::wxMenuItem().
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetText,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemgetsubmenu">external documentation</a>.
-spec getSubMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuItem().
getSubMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetSubMenu,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemischeckable">external documentation</a>.
-spec isCheckable(This) -> boolean() when
	This::wxMenuItem().
isCheckable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsCheckable,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemischecked">external documentation</a>.
-spec isChecked(This) -> boolean() when
	This::wxMenuItem().
isChecked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsChecked,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemisenabled">external documentation</a>.
-spec isEnabled(This) -> boolean() when
	This::wxMenuItem().
isEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsEnabled,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemisseparator">external documentation</a>.
-spec isSeparator(This) -> boolean() when
	This::wxMenuItem().
isSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsSeparator,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemissubmenu">external documentation</a>.
-spec isSubMenu(This) -> boolean() when
	This::wxMenuItem().
isSubMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsSubMenu,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetbitmap">external documentation</a>.
-spec setBitmap(This, Bitmap) -> 'ok' when
	This::wxMenuItem(), Bitmap::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:cast(?wxMenuItem_SetBitmap,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsethelp">external documentation</a>.
-spec setHelp(This, Str) -> 'ok' when
	This::wxMenuItem(), Str::unicode:chardata().
setHelp(#wx_ref{type=ThisT,ref=ThisRef},Str)
 when ?is_chardata(Str) ->
  ?CLASS(ThisT,wxMenuItem),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxMenuItem_SetHelp,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetmenu">external documentation</a>.
-spec setMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:cast(?wxMenuItem_SetMenu,
  <<ThisRef:32/?UI,MenuRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsetsubmenu">external documentation</a>.
-spec setSubMenu(This, Menu) -> 'ok' when
	This::wxMenuItem(), Menu::wxMenu:wxMenu().
setSubMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:cast(?wxMenuItem_SetSubMenu,
  <<ThisRef:32/?UI,MenuRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenuitem.html#wxmenuitemsettext">external documentation</a>.
-spec setText(This, Str) -> 'ok' when
	This::wxMenuItem(), Str::unicode:chardata().
setText(#wx_ref{type=ThisT,ref=ThisRef},Str)
 when ?is_chardata(Str) ->
  ?CLASS(ThisT,wxMenuItem),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxMenuItem_SetText,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMenuItem()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuItem),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
