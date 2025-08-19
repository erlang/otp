%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxMenu).
-moduledoc """
A menu is a popup (or pull down) list of items, one of which may be selected before the
menu goes away (clicking elsewhere dismisses the menu).

Menus may be used to construct either menu bars or popup menus.

A menu item has an integer ID associated with it which can be used to identify the
selection, or to change the menu item in some way. A menu item with a special identifier `wxID_SEPARATOR`
is a separator item and doesn't have an associated command but just makes a separator
line appear in the menu.

Note: Please note that `wxID_ABOUT` and `wxID_EXIT` are predefined by wxWidgets and have
a special meaning since entries using these IDs will be taken out of the normal menus
under macOS and will be inserted into the system menu (following the appropriate macOS
interface guideline).

Menu items may be either `normal` items, `check` items or `radio` items. Normal items
don't have any special properties while the check items have a boolean flag associated to
them and they show a checkmark in the menu when the flag is set. wxWidgets automatically
toggles the flag value when the item is clicked and its value may be retrieved using
either `isChecked/2` method of `m:wxMenu` or `m:wxMenuBar` itself or by using wxEvent::IsChecked when
you get the menu notification for the item in question.

The radio items are similar to the check items except that all the other items in the
same radio group are unchecked when a radio item is checked. The radio group is formed by
a contiguous range of radio items, i.e. it starts at the first item of this kind and ends
with the first item of a different kind (or the end of the menu). Notice that because the
radio groups are defined in terms of the item positions inserting or removing the items in
the menu containing the radio items risks to not work correctly.

Allocation strategy

All menus must be created on the `heap` because all menus attached to a menubar or to
another menu will be deleted by their parent when it is deleted. The only exception to
this rule are the popup menus (i.e. menus used with `wxWindow:popupMenu/4`) as wxWidgets does not destroy them
to allow reusing the same menu more than once. But the exception applies only to the menus
themselves and not to any submenus of popup menus which are still destroyed by wxWidgets
as usual and so must be heap-allocated.

As the frame menubar is deleted by the frame itself, it means that normally all menus
used are deleted automatically.

Event handling

Event handlers for the commands generated by the menu items can be connected directly to
the menu object itself using `wxEvtHandler::Bind()` (not implemented in wx). If this menu
is a submenu of another one, the events from its items can also be processed in the parent
menu and so on, recursively.

If the menu is part of a menu bar, then events can also be handled in `m:wxMenuBar` object.

Finally, menu events can also be handled in the associated window, which is either the `m:wxFrame`
associated with the menu bar this menu belongs to or the window for which `wxWindow:popupMenu/4` was called for
the popup menus.

See overview_events_bind for how to bind event handlers to the various objects.

See:
* `m:wxMenuBar`

* `wxWindow:popupMenu/4`

* [Overview events](https://docs.wxwidgets.org/3.2/overview_events.html#overview_events)

This class is derived, and can use functions, from:

* `m:wxEvtHandler`

wxWidgets docs: [wxMenu](https://docs.wxwidgets.org/3.2/classwx_menu.html)
""".
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
-doc false.
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Constructs a `m:wxMenu` object.".
-spec new() -> wxMenu().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenu_new_0),
  wxe_util:rec(?wxMenu_new_0).

-doc "Constructs a `m:wxMenu` object.".
-spec new([Option]) -> wxMenu() when
	Option :: {'style', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxMenu_new_1),
  wxe_util:rec(?wxMenu_new_1).

-doc "Constructs a `m:wxMenu` object with a title.".
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

-doc """
Adds a menu item object.

This is the most generic variant of `append/5` method because it may be used for both items
(including separators) and submenus and because you can also specify various extra
properties of a menu item this way, such as bitmaps and fonts.

Remark: See the remarks for the other `append/5` overloads.

See:
* `appendSeparator/1`

* `appendCheckItem/4`

* `appendRadioItem/4`

* `insert/6`

* `setLabel/3`

* `getHelpString/2`

* `setHelpString/3`

* `m:wxMenuItem`
""".
-spec append(This, MenuItem) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), MenuItem::wxMenuItem:wxMenuItem().
append(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuItemT}=MenuItem) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(MenuItemT,wxMenuItem),
  wxe_util:queue_cmd(This,MenuItem,?get_env(),?wxMenu_Append_1),
  wxe_util:rec(?wxMenu_Append_1).

-doc(#{equiv => append(This,Id,Item, [])}).
-spec append(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

append(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  append(This,Id,Item, []).

-doc """
Adds a menu item.

Example: or even better for stock menu items (see `wxMenuItem:new/1`):

Remark: This command can be used after the menu has been shown, as well as on initial
creation of a menu or menubar.

See:
* `appendSeparator/1`

* `appendCheckItem/4`

* `appendRadioItem/4`

* `insert/6`

* `setLabel/3`

* `getHelpString/2`

* `setHelpString/3`

* `m:wxMenuItem`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc """
Adds a submenu.

Deprecated:

This function is deprecated, use `AppendSubMenu()` (not implemented in wx) instead.

See:
* `appendSeparator/1`

* `appendCheckItem/4`

* `appendRadioItem/4`

* `insert/6`

* `setLabel/3`

* `getHelpString/2`

* `setHelpString/3`

* `m:wxMenuItem`
""".
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

-doc(#{equiv => appendCheckItem(This,Id,Item, [])}).
-spec appendCheckItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

appendCheckItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  appendCheckItem(This,Id,Item, []).

-doc """
Adds a checkable item to the end of the menu.

See:
* `append/5`

* `insertCheckItem/5`
""".
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

-doc(#{equiv => appendRadioItem(This,Id,Item, [])}).
-spec appendRadioItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

appendRadioItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  appendRadioItem(This,Id,Item, []).

-doc """
Adds a radio item to the end of the menu.

All consequent radio items form a group and when an item in the group is checked, all the
others are automatically unchecked.

Note: Radio items are not supported under wxMotif.

See:
* `append/5`

* `insertRadioItem/5`
""".
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

-doc """
Adds a separator to the end of the menu.

See:
* `append/5`

* `insertSeparator/2`
""".
-spec appendSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
appendSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_AppendSeparator),
  wxe_util:rec(?wxMenu_AppendSeparator).

-doc """
Inserts a break in a menu, causing the next appended item to appear in a new column.

This function only actually inserts a break in wxMSW and does nothing under the other
platforms.
""".
-spec break(This) -> 'ok' when
	This::wxMenu().
break(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_Break).

-doc """
Checks or unchecks the menu item.

See: `isChecked/2`
""".
-spec check(This, Id, Check) -> 'ok' when
	This::wxMenu(), Id::integer(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Id,Check)
 when is_integer(Id),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,Check,?get_env(),?wxMenu_Check).

-doc """
Deletes the menu item from the menu.

If the item is a submenu, it will `not` be deleted. Use `'Destroy'/2` if you want to delete a submenu.

See:
* `findItem/2`

* `'Destroy'/2`

* `remove/2`
""".
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

-doc """
Deletes the menu item from the menu.

If the item is a submenu, it will be deleted. Use `remove/2` if you want to keep the submenu (for
example, to reuse it later).

See:
* `findItem/2`

* `delete/2`

* `remove/2`
""".
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

-doc """
Enables or disables (greys out) a menu item.

See: `isEnabled/2`
""".
-spec enable(This, Id, Enable) -> 'ok' when
	This::wxMenu(), Id::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Id,Enable)
 when is_integer(Id),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,Enable,?get_env(),?wxMenu_Enable).

-doc """
Finds the menu id for a menu item string.

Return: Menu item identifier, or wxNOT_FOUND if none is found.

Remark: Any special menu codes are stripped out of source and target strings before
matching.
""".
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

-doc "Returns the `m:wxMenuItem` given a position in the menu.".
-spec findItemByPosition(This, Position) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Position::integer().
findItemByPosition(#wx_ref{type=ThisT}=This,Position)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Position,?get_env(),?wxMenu_FindItemByPosition),
  wxe_util:rec(?wxMenu_FindItemByPosition).

-doc """
Returns the help string associated with a menu item.

Return: The help string, or the empty string if there is no help string or the item was
not found.

See:
* `setHelpString/3`

* `append/5`
""".
-spec getHelpString(This, Id) -> unicode:charlist() when
	This::wxMenu(), Id::integer().
getHelpString(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_GetHelpString),
  wxe_util:rec(?wxMenu_GetHelpString).

-doc """
Returns a menu item label.

Return: The item label, or the empty string if the item was not found.

See: `setLabel/3`
""".
-spec getLabel(This, Id) -> unicode:charlist() when
	This::wxMenu(), Id::integer().
getLabel(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_GetLabel),
  wxe_util:rec(?wxMenu_GetLabel).

-doc "Returns the number of items in the menu.".
-spec getMenuItemCount(This) -> integer() when
	This::wxMenu().
getMenuItemCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetMenuItemCount),
  wxe_util:rec(?wxMenu_GetMenuItemCount).

-doc "".
-spec getMenuItems(This) -> [wxMenuItem:wxMenuItem()] when
	This::wxMenu().
getMenuItems(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetMenuItems),
  wxe_util:rec(?wxMenu_GetMenuItems).

-doc """
Returns the title of the menu.

See: `setTitle/2`
""".
-spec getTitle(This) -> unicode:charlist() when
	This::wxMenu().
getTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_GetTitle),
  wxe_util:rec(?wxMenu_GetTitle).

-doc """
Inserts the given `item` before the position `pos`.

Inserting the item at position `getMenuItemCount/1` is the same as appending it.

See:
* `append/5`

* `prepend/5`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc """
Inserts the given `item` before the position `pos`.

Inserting the item at position `getMenuItemCount/1` is the same as appending it.

See:
* `append/5`

* `prepend/5`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc(#{equiv => insert(This,Pos,Id,Text,Submenu, [])}).
-spec insert(This, Pos, Id, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Text::unicode:chardata(), Submenu::wxMenu().

insert(This,Pos,Id,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  insert(This,Pos,Id,Text,Submenu, []).

-doc """
Inserts the given `submenu` before the position `pos`.

`text` is the text shown in the menu for it and `help` is the help string shown in the
status bar when the submenu item is selected.

See: `prepend/5`
""".
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

-doc(#{equiv => insertCheckItem(This,Pos,Id,Item, [])}).
-spec insertCheckItem(This, Pos, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Item::unicode:chardata().

insertCheckItem(This,Pos,Id,Item)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id),?is_chardata(Item) ->
  insertCheckItem(This,Pos,Id,Item, []).

-doc """
Inserts a checkable item at the given position.

See:
* `insert/6`

* `appendCheckItem/4`
""".
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

-doc(#{equiv => insertRadioItem(This,Pos,Id,Item, [])}).
-spec insertRadioItem(This, Pos, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Id::integer(), Item::unicode:chardata().

insertRadioItem(This,Pos,Id,Item)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Id),?is_chardata(Item) ->
  insertRadioItem(This,Pos,Id,Item, []).

-doc """
Inserts a radio item at the given position.

See:
* `insert/6`

* `appendRadioItem/4`
""".
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

-doc """
Inserts a separator at the given position.

See:
* `insert/6`

* `appendSeparator/1`
""".
-spec insertSeparator(This, Pos) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenu_InsertSeparator),
  wxe_util:rec(?wxMenu_InsertSeparator).

-doc """
Determines whether a menu item is checked.

Return: true if the menu item is checked, false otherwise.

See: `check/3`
""".
-spec isChecked(This, Id) -> boolean() when
	This::wxMenu(), Id::integer().
isChecked(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_IsChecked),
  wxe_util:rec(?wxMenu_IsChecked).

-doc """
Determines whether a menu item is enabled.

Return: true if the menu item is enabled, false otherwise.

See: `enable/3`
""".
-spec isEnabled(This, Id) -> boolean() when
	This::wxMenu(), Id::integer().
isEnabled(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenu_IsEnabled),
  wxe_util:rec(?wxMenu_IsEnabled).

-doc """
Inserts the given `item` at position 0, i.e. before all the other existing items.

See:
* `append/5`

* `insert/6`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc """
Inserts the given `item` at position 0, i.e. before all the other existing items.

See:
* `append/5`

* `insert/6`
""".
%%  Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
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

-doc(#{equiv => prepend(This,Id,Text,Submenu, [])}).
-spec prepend(This, Id, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Text::unicode:chardata(), Submenu::wxMenu().

prepend(This,Id,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Text),is_record(Submenu, wx_ref) ->
  prepend(This,Id,Text,Submenu, []).

-doc """
Inserts the given `submenu` at position 0.

See: `insert/6`
""".
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

-doc(#{equiv => prependCheckItem(This,Id,Item, [])}).
-spec prependCheckItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

prependCheckItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  prependCheckItem(This,Id,Item, []).

-doc """
Inserts a checkable item at position 0.

See:
* `prepend/5`

* `appendCheckItem/4`
""".
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

-doc(#{equiv => prependRadioItem(This,Id,Item, [])}).
-spec prependRadioItem(This, Id, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Id::integer(), Item::unicode:chardata().

prependRadioItem(This,Id,Item)
 when is_record(This, wx_ref),is_integer(Id),?is_chardata(Item) ->
  prependRadioItem(This,Id,Item, []).

-doc """
Inserts a radio item at position 0.

See:
* `prepend/5`

* `appendRadioItem/4`
""".
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

-doc """
Inserts a separator at position 0.

See:
* `prepend/5`

* `appendSeparator/1`
""".
-spec prependSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
prependSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:queue_cmd(This,?get_env(),?wxMenu_PrependSeparator),
  wxe_util:rec(?wxMenu_PrependSeparator).

-doc """
Removes the menu item from the menu but doesn't delete the associated C++ object.

This allows you to reuse the same item later by adding it back to the menu (especially
useful with submenus).

Return: A pointer to the item which was detached from the menu.
""".
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

-doc """
Sets an item's help string.

See: `getHelpString/2`
""".
-spec setHelpString(This, Id, HelpString) -> 'ok' when
	This::wxMenu(), Id::integer(), HelpString::unicode:chardata().
setHelpString(#wx_ref{type=ThisT}=This,Id,HelpString)
 when is_integer(Id),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenu),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,Id,HelpString_UC,?get_env(),?wxMenu_SetHelpString).

-doc """
Sets the label of a menu item.

See:
* `append/5`

* `getLabel/2`
""".
-spec setLabel(This, Id, Label) -> 'ok' when
	This::wxMenu(), Id::integer(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Id,Label)
 when is_integer(Id),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenu),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Id,Label_UC,?get_env(),?wxMenu_SetLabel).

-doc """
Sets the title of the menu.

Remark: Notice that you can only call this method directly for the popup menus, to change
the title of a menu that is part of a menu bar you need to use `wxMenuBar:setLabelTop/3`.

See: `getTitle/1`
""".
-spec setTitle(This, Title) -> 'ok' when
	This::wxMenu(), Title::unicode:chardata().
setTitle(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxMenu_SetTitle).

-doc "Destroys the object".
-spec destroy(This::wxMenu()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenu),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
