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

-module(wxMenu).
-include("wxe.hrl").
-export(['Destroy'/2,append/2,append/3,append/4,append/5,appendCheckItem/3,appendCheckItem/4,
  appendRadioItem/3,appendRadioItem/4,appendSeparator/1,break/1,check/3,
  delete/2,destroy/1,enable/3,findItem/2,findItemByPosition/2,getHelpString/2,
  getLabel/2,getMenuItemCount/1,getMenuItems/1,getTitle/1,insert/3,insert/4,
  insert/5,insert/6,insertCheckItem/4,insertCheckItem/5,insertRadioItem/4,
  insertRadioItem/5,insertSeparator/2,isChecked/2,isEnabled/2,new/0,
  new/1,new/2,prepend/2,prepend/3,prepend/4,prepend/5,prependCheckItem/3,
  prependCheckItem/4,prependRadioItem/3,prependRadioItem/4,prependSeparator/1,
  remove/2,setHelpString/3,setLabel/3,setTitle/2]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxMenu() :: wx:wx_object().
-export_type([wxMenu/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new() -> wxMenu().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenu_new_0),
  wxe_util:rec(?wxMenu_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new([Option]) -> wxMenu() when
	Option :: {'style', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxMenu_new_1),
  wxe_util:rec(?wxMenu_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new(Title, [Option]) -> wxMenu() when
	Title::unicode:chardata(),
	Option :: {'style', integer()}.
new(Title, Options)
 when ?is_chardata(Title),is_list(Options) ->
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Title_UC, Opts,?get_env(),?wxMenu_new_2),
  wxe_util:rec(?wxMenu_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
-spec append(This, MenuItem) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), MenuItem::wxMenuItem:wxMenuItem().
append(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuItemT}=MenuItem) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(MenuItemT,wxMenuItem),
  wxe_util:queue_cmd(This,MenuItem,?get_env(),?wxMenu_Append_1),
  wxe_util:rec(?wxMenu_Append_1).

%% @equiv append(This,Id,Item, [])
-spec append(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

append(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  append(This,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Also:<br />
%% append(This, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Id::integer(), Item::unicode:chardata(),<br />
%% 	Option :: {'help', unicode:chardata()}<br />
%% 		 | {'kind', wx:wx_enum()}.<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec append(This, Id, Item, SubMenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(), SubMenu::wxMenu();
      (This, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.

append(This,Id,Item,SubMenu)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item),is_record(SubMenu, wx_ref) ->
  append(This,Id,Item,SubMenu, []);
append(#wx_ref{type=ThisT}=This,Id,Item, Options)
 when is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          ({kind, _kind} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Item_UC, Opts,?get_env(),?wxMenu_Append_3),
  wxe_util:rec(?wxMenu_Append_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
-spec append(This, Id, Item, SubMenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(), SubMenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
append(#wx_ref{type=ThisT}=This,Id,Item,#wx_ref{type=SubMenuT}=SubMenu, Options)
 when is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  ?CLASS(SubMenuT,wxMenu),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Item_UC,SubMenu, Opts,?get_env(),?wxMenu_Append_4),
  wxe_util:rec(?wxMenu_Append_4).

%% @equiv appendCheckItem(This,Id,Item, [])
-spec appendCheckItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

appendCheckItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  appendCheckItem(This,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendcheckitem">external documentation</a>.
-spec appendCheckItem(This, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
appendCheckItem(#wx_ref{type=ThisT}=This,Id,Item, Options)
 when is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Item_UC, Opts,?get_env(),?wxMenu_AppendCheckItem),
  wxe_util:rec(?wxMenu_AppendCheckItem).

%% @equiv appendRadioItem(This,Id,Item, [])
-spec appendRadioItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

appendRadioItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  appendRadioItem(This,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendradioitem">external documentation</a>.
-spec appendRadioItem(This, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
appendRadioItem(#wx_ref{type=ThisT}=This,Id,Item, Options)
 when is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Item_UC, Opts,?get_env(),?wxMenu_AppendRadioItem),
  wxe_util:rec(?wxMenu_AppendRadioItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendseparator">external documentation</a>.
-spec appendSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
appendSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_AppendSeparator),
  wxe_util:rec(?wxMenu_AppendSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenubreak">external documentation</a>.
-spec break(This) -> 'ok' when
	This::wxMenu().
break(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_Break).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenucheck">external documentation</a>.
-spec check(This, Id, Check) -> 'ok' when
	This::wxMenu(), Id::integer(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Id,Check)
 when is_integer(Id),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,Check,?get_env(),?wxMenu_Check).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenudelete">external documentation</a>.
%% <br /> Also:<br />
%% delete(This, Item) -> boolean() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec delete(This, Id) -> boolean() when
	This::wxMenu(), Id::integer();
      (This, Item) -> boolean() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
delete(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_Delete_1_0),
  wxe_util:rec(?wxMenu_Delete_1_0);
delete(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Delete_1_1),
  wxe_util:rec(?wxMenu_Delete_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenudestroy">external documentation</a>.
%% <br /> Also:<br />
%% 'Destroy'(This, Item) -> boolean() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec 'Destroy'(This, Id) -> boolean() when
	This::wxMenu(), Id::integer();
      (This, Item) -> boolean() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
'Destroy'(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_Destroy_1_0),
  wxe_util:rec(?wxMenu_Destroy_1_0);
'Destroy'(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Destroy_1_1),
  wxe_util:rec(?wxMenu_Destroy_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuenable">external documentation</a>.
-spec enable(This, Id, Enable) -> 'ok' when
	This::wxMenu(), Id::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Id,Enable)
 when is_integer(Id),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,Enable,?get_env(),?wxMenu_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenufinditem">external documentation</a>.
%% <br /> Also:<br />
%% findItem(This, ItemString) -> integer() when<br />
%% 	This::wxMenu(), ItemString::unicode:chardata().<br />
%% 
-spec findItem(This, Id) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer();
      (This, ItemString) -> integer() when
	This::wxMenu(), ItemString::unicode:chardata().
findItem(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_FindItem_2),
  wxe_util:rec(?wxMenu_FindItem_2);
findItem(#wx_ref{type=ThisT}=This,ItemString)
 when ?is_chardata(ItemString) ->
  ?CLASS(ThisT,wxMenu),
  ItemString_UC = unicode:characters_to_binary(ItemString),
  wxe_util:queue_cmd(This,ItemString_UC,?get_env(),?wxMenu_FindItem_1),
  wxe_util:rec(?wxMenu_FindItem_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenufinditembyposition">external documentation</a>.
-spec findItemByPosition(This, Position) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Position::integer().
findItemByPosition(#wx_ref{type=ThisT}=This,Position)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Position,?get_env(),?wxMenu_FindItemByPosition),
  wxe_util:rec(?wxMenu_FindItemByPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugethelpstring">external documentation</a>.
-spec getHelpString(This, Id) -> unicode:charlist() when
	This::wxMenu(), Id::integer().
getHelpString(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_GetHelpString),
  wxe_util:rec(?wxMenu_GetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetlabel">external documentation</a>.
-spec getLabel(This, Id) -> unicode:charlist() when
	This::wxMenu(), Id::integer().
getLabel(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_GetLabel),
  wxe_util:rec(?wxMenu_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetmenuitemcount">external documentation</a>.
-spec getMenuItemCount(This) -> integer() when
	This::wxMenu().
getMenuItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetMenuItemCount),
  wxe_util:rec(?wxMenu_GetMenuItemCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetmenuitems">external documentation</a>.
-spec getMenuItems(This) -> [wxMenuItem:wxMenuItem()] when
	This::wxMenu().
getMenuItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetMenuItems),
  wxe_util:rec(?wxMenu_GetMenuItems).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugettitle">external documentation</a>.
-spec getTitle(This) -> unicode:charlist() when
	This::wxMenu().
getTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetTitle),
  wxe_util:rec(?wxMenu_GetTitle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Pos, MenuItem) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Pos::integer(), MenuItem::wxMenuItem:wxMenuItem().<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec insert(This, Pos, Id) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer();
      (This, Pos, MenuItem) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), MenuItem::wxMenuItem:wxMenuItem().

insert(This,Pos,Id)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id) ->
  insert(This,Pos,Id, []);
insert(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=MenuItemT}=MenuItem)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(MenuItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Pos,MenuItem,?get_env(),?wxMenu_Insert_2),
  wxe_util:rec(?wxMenu_Insert_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec insert(This, Pos, Id, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(),
	Option :: {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.
insert(#wx_ref{type=ThisT}=This,Pos,Id, Options)
 when is_integer(Pos),is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  MOpts = fun({text, Text}) ->   Text_UC = unicode:characters_to_binary(Text),{text,Text_UC};
          ({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          ({kind, _kind} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,Id, Opts,?get_env(),?wxMenu_Insert_3),
  wxe_util:rec(?wxMenu_Insert_3).

%% @equiv insert(This,Pos,Id,Text,Submenu, [])
-spec insert(This, Pos, Id, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Text::unicode:chardata(), Submenu::wxMenu().

insert(This,Pos,Id,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  insert(This,Pos,Id,Text,Submenu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
-spec insert(This, Pos, Id, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
insert(#wx_ref{type=ThisT}=This,Pos,Id,Text,#wx_ref{type=SubmenuT}=Submenu, Options)
 when is_integer(Pos),is_integer(Id),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,Id,Text_UC,Submenu, Opts,?get_env(),?wxMenu_Insert_5),
  wxe_util:rec(?wxMenu_Insert_5).

%% @equiv insertCheckItem(This,Pos,Id,Item, [])
-spec insertCheckItem(This, Pos, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Item::unicode:chardata().

insertCheckItem(This,Pos,Id,Item)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id),?is_chardata(Item) ->
  insertCheckItem(This,Pos,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertcheckitem">external documentation</a>.
-spec insertCheckItem(This, Pos, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
insertCheckItem(#wx_ref{type=ThisT}=This,Pos,Id,Item, Options)
 when is_integer(Pos),is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,Id,Item_UC, Opts,?get_env(),?wxMenu_InsertCheckItem),
  wxe_util:rec(?wxMenu_InsertCheckItem).

%% @equiv insertRadioItem(This,Pos,Id,Item, [])
-spec insertRadioItem(This, Pos, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Item::unicode:chardata().

insertRadioItem(This,Pos,Id,Item)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id),?is_chardata(Item) ->
  insertRadioItem(This,Pos,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertradioitem">external documentation</a>.
-spec insertRadioItem(This, Pos, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
insertRadioItem(#wx_ref{type=ThisT}=This,Pos,Id,Item, Options)
 when is_integer(Pos),is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,Id,Item_UC, Opts,?get_env(),?wxMenu_InsertRadioItem),
  wxe_util:rec(?wxMenu_InsertRadioItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertseparator">external documentation</a>.
-spec insertSeparator(This, Pos) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenu_InsertSeparator),
  wxe_util:rec(?wxMenu_InsertSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuischecked">external documentation</a>.
-spec isChecked(This, Id) -> boolean() when
	This::wxMenu(), Id::integer().
isChecked(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_IsChecked),
  wxe_util:rec(?wxMenu_IsChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuisenabled">external documentation</a>.
-spec isEnabled(This, Id) -> boolean() when
	This::wxMenu(), Id::integer().
isEnabled(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_IsEnabled),
  wxe_util:rec(?wxMenu_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec prepend(This, Id) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer();
      (This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().

prepend(This,Id)
 when is_record(This, wx_ref),is_integer(Id) ->
  prepend(This,Id, []);
prepend(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Prepend_1),
  wxe_util:rec(?wxMenu_Prepend_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec prepend(This, Id, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(),
	Option :: {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.
prepend(#wx_ref{type=ThisT}=This,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  MOpts = fun({text, Text}) ->   Text_UC = unicode:characters_to_binary(Text),{text,Text_UC};
          ({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          ({kind, _kind} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id, Opts,?get_env(),?wxMenu_Prepend_2),
  wxe_util:rec(?wxMenu_Prepend_2).

%% @equiv prepend(This,Id,Text,Submenu, [])
-spec prepend(This, Id, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Text::unicode:chardata(), Submenu::wxMenu().

prepend(This,Id,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  prepend(This,Id,Text,Submenu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
-spec prepend(This, Id, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
prepend(#wx_ref{type=ThisT}=This,Id,Text,#wx_ref{type=SubmenuT}=Submenu, Options)
 when is_integer(Id),?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary(Text),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Text_UC,Submenu, Opts,?get_env(),?wxMenu_Prepend_4),
  wxe_util:rec(?wxMenu_Prepend_4).

%% @equiv prependCheckItem(This,Id,Item, [])
-spec prependCheckItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

prependCheckItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  prependCheckItem(This,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependcheckitem">external documentation</a>.
-spec prependCheckItem(This, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
prependCheckItem(#wx_ref{type=ThisT}=This,Id,Item, Options)
 when is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Item_UC, Opts,?get_env(),?wxMenu_PrependCheckItem),
  wxe_util:rec(?wxMenu_PrependCheckItem).

%% @equiv prependRadioItem(This,Id,Item, [])
-spec prependRadioItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

prependRadioItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  prependRadioItem(This,Id,Item, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependradioitem">external documentation</a>.
-spec prependRadioItem(This, Id, Item, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
prependRadioItem(#wx_ref{type=ThisT}=This,Id,Item, Options)
 when is_integer(Id),?is_chardata(Item),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary(Item),
  MOpts = fun({help, Help}) ->   Help_UC = unicode:characters_to_binary(Help),{help,Help_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Id,Item_UC, Opts,?get_env(),?wxMenu_PrependRadioItem),
  wxe_util:rec(?wxMenu_PrependRadioItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependseparator">external documentation</a>.
-spec prependSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
prependSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_PrependSeparator),
  wxe_util:rec(?wxMenu_PrependSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuremove">external documentation</a>.
%% <br /> Also:<br />
%% remove(This, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec remove(This, Id) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer();
      (This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
remove(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_Remove_1_0),
  wxe_util:rec(?wxMenu_Remove_1_0);
remove(#wx_ref{type=ThisT}=This,#wx_ref{type=ItemT}=Item) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:queue_cmd(This,Item,?get_env(),?wxMenu_Remove_1_1),
  wxe_util:rec(?wxMenu_Remove_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusethelpstring">external documentation</a>.
-spec setHelpString(This, Id, HelpString) -> 'ok' when
	This::wxMenu(), Id::integer(), HelpString::unicode:chardata().
setHelpString(#wx_ref{type=ThisT}=This,Id,HelpString)
 when is_integer(Id),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenu),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,Id,HelpString_UC,?get_env(),?wxMenu_SetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusetlabel">external documentation</a>.
-spec setLabel(This, Id, Label) -> 'ok' when
	This::wxMenu(), Id::integer(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Id,Label)
 when is_integer(Id),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenu),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Id,Label_UC,?get_env(),?wxMenu_SetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusettitle">external documentation</a>.
-spec setTitle(This, Title) -> 'ok' when
	This::wxMenu(), Title::unicode:chardata().
setTitle(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxMenu_SetTitle).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMenu()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenu),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
%% @hidden
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
