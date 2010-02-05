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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html">wxMenuItem</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxMenuItem()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxMenuItem()
%% Option = {parentMenu, wxMenu:wxMenu()} | {id, integer()} | {text, string()} | {help, string()} | {kind, WxItemKind} | {subMenu, wxMenu:wxMenu()}
%% WxItemKind = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemwxmenuitem">external documentation</a>.
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
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

%% @spec (This::wxMenuItem()) -> ok
%% @equiv check(This, [])
check(This)
 when is_record(This, wx_ref) ->
  check(This, []).

%% @spec (This::wxMenuItem(), [Option]) -> ok
%% Option = {check, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemcheck">external documentation</a>.
check(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({check, Check}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Check)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxMenuItem_Check,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxMenuItem()) -> ok
%% @equiv enable(This, [])
enable(This)
 when is_record(This, wx_ref) ->
  enable(This, []).

%% @spec (This::wxMenuItem(), [Option]) -> ok
%% Option = {enable, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemenable">external documentation</a>.
enable(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxMenuItem),
  MOpts = fun({enable, Enable}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxMenuItem_Enable,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxMenuItem()) -> wxBitmap:wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetbitmap">external documentation</a>.
getBitmap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetBitmap,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgethelp">external documentation</a>.
getHelp(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetHelp,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetid">external documentation</a>.
getId(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetId,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> WxItemKind
%% WxItemKind = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetkind">external documentation</a>.
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
getKind(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetKind,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetlabel">external documentation</a>.
getLabel(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetLabel,
  <<ThisRef:32/?UI>>).

%% @spec (Text::string()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetlabelfromtext">external documentation</a>.
getLabelFromText(Text)
 when is_list(Text) ->
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxMenuItem_GetLabelFromText,
  <<(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxMenuItem()) -> wxMenu:wxMenu()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetmenu">external documentation</a>.
getMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetMenu,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgettext">external documentation</a>.
getText(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetText,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> wxMenu:wxMenu()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemgetsubmenu">external documentation</a>.
getSubMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_GetSubMenu,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemischeckable">external documentation</a>.
isCheckable(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsCheckable,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemischecked">external documentation</a>.
isChecked(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsChecked,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemisenabled">external documentation</a>.
isEnabled(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsEnabled,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemisseparator">external documentation</a>.
isSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsSeparator,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemissubmenu">external documentation</a>.
isSubMenu(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  wxe_util:call(?wxMenuItem_IsSubMenu,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenuItem(), Bitmap::wxBitmap:wxBitmap()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemsetbitmap">external documentation</a>.
setBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:cast(?wxMenuItem_SetBitmap,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @spec (This::wxMenuItem(), Str::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemsethelp">external documentation</a>.
setHelp(#wx_ref{type=ThisT,ref=ThisRef},Str)
 when is_list(Str) ->
  ?CLASS(ThisT,wxMenuItem),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxMenuItem_SetHelp,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxMenuItem(), Menu::wxMenu:wxMenu()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemsetmenu">external documentation</a>.
setMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:cast(?wxMenuItem_SetMenu,
  <<ThisRef:32/?UI,MenuRef:32/?UI>>).

%% @spec (This::wxMenuItem(), Menu::wxMenu:wxMenu()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemsetsubmenu">external documentation</a>.
setSubMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}) ->
  ?CLASS(ThisT,wxMenuItem),
  ?CLASS(MenuT,wxMenu),
  wxe_util:cast(?wxMenuItem_SetSubMenu,
  <<ThisRef:32/?UI,MenuRef:32/?UI>>).

%% @spec (This::wxMenuItem(), Str::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenuitem.html#wxmenuitemsettext">external documentation</a>.
setText(#wx_ref{type=ThisT,ref=ThisRef},Str)
 when is_list(Str) ->
  ?CLASS(ThisT,wxMenuItem),
  Str_UC = unicode:characters_to_binary([Str,0]),
  wxe_util:cast(?wxMenuItem_SetText,
  <<ThisRef:32/?UI,(byte_size(Str_UC)):32/?UI,(Str_UC)/binary, 0:(((8- ((0+byte_size(Str_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxMenuItem()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuItem),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
