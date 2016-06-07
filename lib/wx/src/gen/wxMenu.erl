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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html">wxMenu</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxMenu().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

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

-export_type([wxMenu/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxMenu() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxMenu().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new([Option]) -> wxMenu() when
	Option :: {'style', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({style, Style}, Acc) -> [<<1:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxMenu_new_1,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
-spec new(Title, [Option]) -> wxMenu() when
	Title::unicode:chardata(),
	Option :: {'style', integer()}.
new(Title, Options)
 when is_list(Title),is_list(Options) ->
  Title_UC = unicode:characters_to_binary([Title,0]),
  MOpts = fun({style, Style}, Acc) -> [<<1:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxMenu_new_2,
  <<(byte_size(Title_UC)):32/?UI,(Title_UC)/binary, 0:(((8- ((4+byte_size(Title_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
-spec append(This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
append(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Append_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @equiv append(This,Itemid,Text, [])
-spec append(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

append(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  append(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Also:<br />
%% append(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),<br />
%% 	Option :: {'help', unicode:chardata()}<br />
%% 		 | {'kind', wx:wx_enum()}.<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec append(This, Itemid, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu();
      (This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.

append(This,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text),is_record(Submenu, wx_ref) ->
  append(This,Itemid,Text,Submenu, []);
append(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({kind, Kind}, Acc) -> [<<2:32/?UI,Kind:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_Append_3,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Also:<br />
%% append(This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),<br />
%% 	Option :: {'help', unicode:chardata()}.<br />
%% 
-spec append(This, Itemid, Text, Help, IsCheckable) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Help::unicode:chardata(), IsCheckable::boolean();
      (This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
append(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text,Help,IsCheckable)
 when is_integer(Itemid),is_list(Text),is_list(Help),is_boolean(IsCheckable) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  Help_UC = unicode:characters_to_binary([Help,0]),
  wxe_util:cast(?wxMenu_Append_4_0,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((4+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8,(wxe_util:from_bool(IsCheckable)):32/?UI>>);
append(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text,#wx_ref{type=SubmenuT,ref=SubmenuRef}, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_Append_4_1,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,SubmenuRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv appendCheckItem(This,Itemid,Text, [])
-spec appendCheckItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

appendCheckItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  appendCheckItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendcheckitem">external documentation</a>.
-spec appendCheckItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
appendCheckItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_AppendCheckItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv appendRadioItem(This,Itemid,Text, [])
-spec appendRadioItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

appendRadioItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  appendRadioItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendradioitem">external documentation</a>.
-spec appendRadioItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
appendRadioItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_AppendRadioItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuappendseparator">external documentation</a>.
-spec appendSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
appendSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_AppendSeparator,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenubreak">external documentation</a>.
-spec break(This) -> 'ok' when
	This::wxMenu().
break(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:cast(?wxMenu_Break,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenucheck">external documentation</a>.
-spec check(This, Itemid, Check) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Check::boolean().
check(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Check)
 when is_integer(Itemid),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:cast(?wxMenu_Check,
  <<ThisRef:32/?UI,Itemid:32/?UI,(wxe_util:from_bool(Check)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenudelete">external documentation</a>.
%% <br /> Also:<br />
%% delete(This, Item) -> boolean() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec delete(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> boolean() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
delete(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_Delete_1_0,
  <<ThisRef:32/?UI,Itemid:32/?UI>>);
delete(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Delete_1_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenudestroy">external documentation</a>.
%% <br /> Also:<br />
%% 'Destroy'(This, Item) -> boolean() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec 'Destroy'(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> boolean() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
'Destroy'(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_Destroy_1_0,
  <<ThisRef:32/?UI,Itemid:32/?UI>>);
'Destroy'(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Destroy_1_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuenable">external documentation</a>.
-spec enable(This, Itemid, Enable) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Enable)
 when is_integer(Itemid),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:cast(?wxMenu_Enable,
  <<ThisRef:32/?UI,Itemid:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenufinditem">external documentation</a>.
%% <br /> Also:<br />
%% findItem(This, Item) -> integer() when<br />
%% 	This::wxMenu(), Item::unicode:chardata().<br />
%% 
-spec findItem(This, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> integer() when
	This::wxMenu(), Item::unicode:chardata().
findItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_FindItem_2,
  <<ThisRef:32/?UI,Itemid:32/?UI>>);
findItem(#wx_ref{type=ThisT,ref=ThisRef},Item)
 when is_list(Item) ->
  ?CLASS(ThisT,wxMenu),
  Item_UC = unicode:characters_to_binary([Item,0]),
  wxe_util:call(?wxMenu_FindItem_1,
  <<ThisRef:32/?UI,(byte_size(Item_UC)):32/?UI,(Item_UC)/binary, 0:(((8- ((0+byte_size(Item_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenufinditembyposition">external documentation</a>.
-spec findItemByPosition(This, Position) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Position::integer().
findItemByPosition(#wx_ref{type=ThisT,ref=ThisRef},Position)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_FindItemByPosition,
  <<ThisRef:32/?UI,Position:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugethelpstring">external documentation</a>.
-spec getHelpString(This, Itemid) -> unicode:charlist() when
	This::wxMenu(), Itemid::integer().
getHelpString(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetHelpString,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetlabel">external documentation</a>.
-spec getLabel(This, Itemid) -> unicode:charlist() when
	This::wxMenu(), Itemid::integer().
getLabel(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetLabel,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetmenuitemcount">external documentation</a>.
-spec getMenuItemCount(This) -> integer() when
	This::wxMenu().
getMenuItemCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetMenuItemCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugetmenuitems">external documentation</a>.
-spec getMenuItems(This) -> [wxMenuItem:wxMenuItem()] when
	This::wxMenu().
getMenuItems(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetMenuItems,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenugettitle">external documentation</a>.
-spec getTitle(This) -> unicode:charlist() when
	This::wxMenu().
getTitle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetTitle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Pos, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Pos::integer(), Item::wxMenuItem:wxMenuItem().<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec insert(This, Pos, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer();
      (This, Pos, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Item::wxMenuItem:wxMenuItem().

insert(This,Pos,Itemid)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid) ->
  insert(This,Pos,Itemid, []);
insert(#wx_ref{type=ThisT,ref=ThisRef},Pos,#wx_ref{type=ItemT,ref=ItemRef})
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Insert_2,
  <<ThisRef:32/?UI,Pos:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec insert(This, Pos, Itemid, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(),
	Option :: {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.
insert(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  MOpts = fun({text, Text}, Acc) ->   Text_UC = unicode:characters_to_binary([Text,0]),[<<1:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<2:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({kind, Kind}, Acc) -> [<<3:32/?UI,Kind:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_Insert_3,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv insert(This,Pos,Itemid,Text,Submenu, [])
-spec insert(This, Pos, Itemid, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu().

insert(This,Pos,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),is_list(Text),is_record(Submenu, wx_ref) ->
  insert(This,Pos,Itemid,Text,Submenu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Also:<br />
%% insert(This, Pos, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),<br />
%% 	Option :: {'help', unicode:chardata()}.<br />
%% 
-spec insert(This, Pos, Itemid, Text, Help, IsCheckable) -> 'ok' when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Help::unicode:chardata(), IsCheckable::boolean();
      (This, Pos, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
insert(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid,Text,Help,IsCheckable)
 when is_integer(Pos),is_integer(Itemid),is_list(Text),is_list(Help),is_boolean(IsCheckable) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  Help_UC = unicode:characters_to_binary([Help,0]),
  wxe_util:cast(?wxMenu_Insert_5_0,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((4+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8,(wxe_util:from_bool(IsCheckable)):32/?UI>>);
insert(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid,Text,#wx_ref{type=SubmenuT,ref=SubmenuRef}, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_Insert_5_1,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,SubmenuRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv insertCheckItem(This,Pos,Itemid,Text, [])
-spec insertCheckItem(This, Pos, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata().

insertCheckItem(This,Pos,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),is_list(Text) ->
  insertCheckItem(This,Pos,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertcheckitem">external documentation</a>.
-spec insertCheckItem(This, Pos, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
insertCheckItem(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid,Text, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_InsertCheckItem,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv insertRadioItem(This,Pos,Itemid,Text, [])
-spec insertRadioItem(This, Pos, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata().

insertRadioItem(This,Pos,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),is_list(Text) ->
  insertRadioItem(This,Pos,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertradioitem">external documentation</a>.
-spec insertRadioItem(This, Pos, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
insertRadioItem(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid,Text, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_InsertRadioItem,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuinsertseparator">external documentation</a>.
-spec insertSeparator(This, Pos) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_InsertSeparator,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuischecked">external documentation</a>.
-spec isChecked(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer().
isChecked(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_IsChecked,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuisenabled">external documentation</a>.
-spec isEnabled(This, Itemid) -> boolean() when
	This::wxMenu(), Itemid::integer().
isEnabled(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_IsEnabled,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec prepend(This, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().

prepend(This,Itemid)
 when is_record(This, wx_ref),is_integer(Itemid) ->
  prepend(This,Itemid, []);
prepend(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Prepend_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
-spec prepend(This, Itemid, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(),
	Option :: {'text', unicode:chardata()}
		 | {'help', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.
prepend(#wx_ref{type=ThisT,ref=ThisRef},Itemid, Options)
 when is_integer(Itemid),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  MOpts = fun({text, Text}, Acc) ->   Text_UC = unicode:characters_to_binary([Text,0]),[<<1:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<2:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({kind, Kind}, Acc) -> [<<3:32/?UI,Kind:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_Prepend_2,
  <<ThisRef:32/?UI,Itemid:32/?UI, BinOpt/binary>>).

%% @equiv prepend(This,Itemid,Text,Submenu, [])
-spec prepend(This, Itemid, Text, Submenu) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu().

prepend(This,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text),is_record(Submenu, wx_ref) ->
  prepend(This,Itemid,Text,Submenu, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Also:<br />
%% prepend(This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),<br />
%% 	Option :: {'help', unicode:chardata()}.<br />
%% 
-spec prepend(This, Itemid, Text, Help, IsCheckable) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Help::unicode:chardata(), IsCheckable::boolean();
      (This, Itemid, Text, Submenu, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(), Submenu::wxMenu(),
	Option :: {'help', unicode:chardata()}.
prepend(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text,Help,IsCheckable)
 when is_integer(Itemid),is_list(Text),is_list(Help),is_boolean(IsCheckable) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  Help_UC = unicode:characters_to_binary([Help,0]),
  wxe_util:cast(?wxMenu_Prepend_4_0,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((4+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8,(wxe_util:from_bool(IsCheckable)):32/?UI>>);
prepend(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text,#wx_ref{type=SubmenuT,ref=SubmenuRef}, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  ?CLASS(SubmenuT,wxMenu),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_Prepend_4_1,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8,SubmenuRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv prependCheckItem(This,Itemid,Text, [])
-spec prependCheckItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

prependCheckItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  prependCheckItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependcheckitem">external documentation</a>.
-spec prependCheckItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
prependCheckItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_PrependCheckItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv prependRadioItem(This,Itemid,Text, [])
-spec prependRadioItem(This, Itemid, Text) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata().

prependRadioItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  prependRadioItem(This,Itemid,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependradioitem">external documentation</a>.
-spec prependRadioItem(This, Itemid, Text, [Option]) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer(), Text::unicode:chardata(),
	Option :: {'help', unicode:chardata()}.
prependRadioItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_PrependRadioItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuprependseparator">external documentation</a>.
-spec prependSeparator(This) -> wxMenuItem:wxMenuItem() when
	This::wxMenu().
prependSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_PrependSeparator,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenuremove">external documentation</a>.
%% <br /> Also:<br />
%% remove(This, Item) -> wxMenuItem:wxMenuItem() when<br />
%% 	This::wxMenu(), Item::wxMenuItem:wxMenuItem().<br />
%% 
-spec remove(This, Itemid) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Itemid::integer();
      (This, Item) -> wxMenuItem:wxMenuItem() when
	This::wxMenu(), Item::wxMenuItem:wxMenuItem().
remove(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_Remove_1_0,
  <<ThisRef:32/?UI,Itemid:32/?UI>>);
remove(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Remove_1_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusethelpstring">external documentation</a>.
-spec setHelpString(This, Itemid, HelpString) -> 'ok' when
	This::wxMenu(), Itemid::integer(), HelpString::unicode:chardata().
setHelpString(#wx_ref{type=ThisT,ref=ThisRef},Itemid,HelpString)
 when is_integer(Itemid),is_list(HelpString) ->
  ?CLASS(ThisT,wxMenu),
  HelpString_UC = unicode:characters_to_binary([HelpString,0]),
  wxe_util:cast(?wxMenu_SetHelpString,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(HelpString_UC)):32/?UI,(HelpString_UC)/binary, 0:(((8- ((4+byte_size(HelpString_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusetlabel">external documentation</a>.
-spec setLabel(This, Itemid, Label) -> 'ok' when
	This::wxMenu(), Itemid::integer(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Label)
 when is_integer(Itemid),is_list(Label) ->
  ?CLASS(ThisT,wxMenu),
  Label_UC = unicode:characters_to_binary([Label,0]),
  wxe_util:cast(?wxMenu_SetLabel,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenu.html#wxmenusettitle">external documentation</a>.
-spec setTitle(This, Title) -> 'ok' when
	This::wxMenu(), Title::unicode:chardata().
setTitle(#wx_ref{type=ThisT,ref=ThisRef},Title)
 when is_list(Title) ->
  ?CLASS(ThisT,wxMenu),
  Title_UC = unicode:characters_to_binary([Title,0]),
  wxe_util:cast(?wxMenu_SetTitle,
  <<ThisRef:32/?UI,(byte_size(Title_UC)):32/?UI,(Title_UC)/binary, 0:(((8- ((0+byte_size(Title_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMenu()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenu),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
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
