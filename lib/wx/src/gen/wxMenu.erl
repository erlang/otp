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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html">wxMenu</a>.
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

%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxMenu()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxMenu()
%% Option = {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({style, Style}, Acc) -> [<<1:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxMenu_new_1,
  <<BinOpt/binary>>).

%% @spec (Title::string(), [Option]) -> wxMenu()
%% Option = {style, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuwxmenu">external documentation</a>.
new(Title, Options)
 when is_list(Title),is_list(Options) ->
  Title_UC = unicode:characters_to_binary([Title,0]),
  MOpts = fun({style, Style}, Acc) -> [<<1:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxMenu_new_2,
  <<(byte_size(Title_UC)):32/?UI,(Title_UC)/binary, 0:(((8- ((4+byte_size(Title_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu(), Item::wxMenuItem:wxMenuItem()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuappend">external documentation</a>.
append(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Append_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv append(This,Itemid,Text, [])
append(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  append(This,Itemid,Text, []).

%% @spec (This::wxMenu(),Itemid::integer(),Text::string(),X::wxMenu()|term()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% append(This::wxMenu(), Itemid::integer(), Text::string(), Submenu::wxMenu()) -> append(This,Itemid,Text,Submenu, []) </c></p>
%% <p><c>
%% append(This::wxMenu(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem() </c>
%%<br /> Option = {help, string()} | {kind, WxItemKind}
%%<br /> WxItemKind = integer()
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
%% </p>

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

%% @spec (This::wxMenu(),Itemid::integer(),Text::string(),X::string()|wxMenu(),X::bool()|term()) -> ok|wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuappend">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% append(This::wxMenu(), Itemid::integer(), Text::string(), Help::string(), IsCheckable::bool()) -> ok </c>
%% </p>
%% <p><c>
%% append(This::wxMenu(), Itemid::integer(), Text::string(), Submenu::wxMenu(), [Option]) -> wxMenuItem:wxMenuItem() </c>
%%<br /> Option = {help, string()}
%% </p>
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

%% @spec (This::wxMenu(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv appendCheckItem(This,Itemid,Text, [])
appendCheckItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  appendCheckItem(This,Itemid,Text, []).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {help, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuappendcheckitem">external documentation</a>.
appendCheckItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_AppendCheckItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv appendRadioItem(This,Itemid,Text, [])
appendRadioItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  appendRadioItem(This,Itemid,Text, []).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {help, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuappendradioitem">external documentation</a>.
appendRadioItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_AppendRadioItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuappendseparator">external documentation</a>.
appendSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_AppendSeparator,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenu()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenubreak">external documentation</a>.
break(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:cast(?wxMenu_Break,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer(), Check::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenucheck">external documentation</a>.
check(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Check)
 when is_integer(Itemid),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:cast(?wxMenu_Check,
  <<ThisRef:32/?UI,Itemid:32/?UI,(wxe_util:from_bool(Check)):32/?UI>>).

%% @spec (This::wxMenu(),X::integer()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenudelete">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% delete(This::wxMenu(), Itemid::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% delete(This::wxMenu(), Item::wxMenuItem:wxMenuItem()) -> bool() </c>
%% </p>
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

%% @spec (This::wxMenu(),X::integer()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenudestroy">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% 'Destroy'(This::wxMenu(), Itemid::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% 'Destroy'(This::wxMenu(), Item::wxMenuItem:wxMenuItem()) -> bool() </c>
%% </p>
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

%% @spec (This::wxMenu(), Itemid::integer(), Enable::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuenable">external documentation</a>.
enable(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Enable)
 when is_integer(Itemid),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:cast(?wxMenu_Enable,
  <<ThisRef:32/?UI,Itemid:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>).

%% @spec (This::wxMenu(),X::integer()|string()) -> wxMenuItem:wxMenuItem()|integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenufinditem">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% findItem(This::wxMenu(), Itemid::integer()) -> wxMenuItem:wxMenuItem() </c>
%% </p>
%% <p><c>
%% findItem(This::wxMenu(), Item::string()) -> integer() </c>
%% </p>
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

%% @spec (This::wxMenu(), Position::integer()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenufinditembyposition">external documentation</a>.
findItemByPosition(#wx_ref{type=ThisT,ref=ThisRef},Position)
 when is_integer(Position) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_FindItemByPosition,
  <<ThisRef:32/?UI,Position:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenugethelpstring">external documentation</a>.
getHelpString(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetHelpString,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenugetlabel">external documentation</a>.
getLabel(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetLabel,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @spec (This::wxMenu()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenugetmenuitemcount">external documentation</a>.
getMenuItemCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetMenuItemCount,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenu()) -> [wxMenuItem:wxMenuItem()]
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenugetmenuitems">external documentation</a>.
getMenuItems(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetMenuItems,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenu()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenugettitle">external documentation</a>.
getTitle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_GetTitle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenu(),Pos::integer(),X::integer()|term()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% insert(This::wxMenu(), Pos::integer(), Itemid::integer()) -> insert(This,Pos,Itemid, []) </c></p>
%% <p><c>
%% insert(This::wxMenu(), Pos::integer(), Item::wxMenuItem:wxMenuItem()) -> wxMenuItem:wxMenuItem() </c>
%% </p>

insert(This,Pos,Itemid)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid) ->
  insert(This,Pos,Itemid, []);
insert(#wx_ref{type=ThisT,ref=ThisRef},Pos,#wx_ref{type=ItemT,ref=ItemRef})
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Insert_2,
  <<ThisRef:32/?UI,Pos:32/?UI,ItemRef:32/?UI>>).

%% @spec (This::wxMenu(), Pos::integer(), Itemid::integer(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {text, string()} | {help, string()} | {kind, WxItemKind}
%% WxItemKind = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
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

%% @spec (This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string(), Submenu::wxMenu()) -> wxMenuItem:wxMenuItem()
%% @equiv insert(This,Pos,Itemid,Text,Submenu, [])
insert(This,Pos,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),is_list(Text),is_record(Submenu, wx_ref) ->
  insert(This,Pos,Itemid,Text,Submenu, []).

%% @spec (This::wxMenu(),Pos::integer(),Itemid::integer(),Text::string(),X::string()|wxMenu(),X::bool()|term()) -> ok|wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuinsert">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% insert(This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string(), Help::string(), IsCheckable::bool()) -> ok </c>
%% </p>
%% <p><c>
%% insert(This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string(), Submenu::wxMenu(), [Option]) -> wxMenuItem:wxMenuItem() </c>
%%<br /> Option = {help, string()}
%% </p>
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

%% @spec (This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv insertCheckItem(This,Pos,Itemid,Text, [])
insertCheckItem(This,Pos,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),is_list(Text) ->
  insertCheckItem(This,Pos,Itemid,Text, []).

%% @spec (This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {help, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuinsertcheckitem">external documentation</a>.
insertCheckItem(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid,Text, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_InsertCheckItem,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv insertRadioItem(This,Pos,Itemid,Text, [])
insertRadioItem(This,Pos,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Itemid),is_list(Text) ->
  insertRadioItem(This,Pos,Itemid,Text, []).

%% @spec (This::wxMenu(), Pos::integer(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {help, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuinsertradioitem">external documentation</a>.
insertRadioItem(#wx_ref{type=ThisT,ref=ThisRef},Pos,Itemid,Text, Options)
 when is_integer(Pos),is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_InsertRadioItem,
  <<ThisRef:32/?UI,Pos:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((0+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu(), Pos::integer()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuinsertseparator">external documentation</a>.
insertSeparator(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_InsertSeparator,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuischecked">external documentation</a>.
isChecked(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_IsChecked,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuisenabled">external documentation</a>.
isEnabled(#wx_ref{type=ThisT,ref=ThisRef},Itemid)
 when is_integer(Itemid) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_IsEnabled,
  <<ThisRef:32/?UI,Itemid:32/?UI>>).

%% @spec (This::wxMenu(),X::integer()|term()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% prepend(This::wxMenu(), Itemid::integer()) -> prepend(This,Itemid, []) </c></p>
%% <p><c>
%% prepend(This::wxMenu(), Item::wxMenuItem:wxMenuItem()) -> wxMenuItem:wxMenuItem() </c>
%% </p>

prepend(This,Itemid)
 when is_record(This, wx_ref),is_integer(Itemid) ->
  prepend(This,Itemid, []);
prepend(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxMenu),
  ?CLASS(ItemT,wxMenuItem),
  wxe_util:call(?wxMenu_Prepend_1,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @spec (This::wxMenu(), Itemid::integer(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {text, string()} | {help, string()} | {kind, WxItemKind}
%% WxItemKind = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
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

%% @spec (This::wxMenu(), Itemid::integer(), Text::string(), Submenu::wxMenu()) -> wxMenuItem:wxMenuItem()
%% @equiv prepend(This,Itemid,Text,Submenu, [])
prepend(This,Itemid,Text,Submenu)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text),is_record(Submenu, wx_ref) ->
  prepend(This,Itemid,Text,Submenu, []).

%% @spec (This::wxMenu(),Itemid::integer(),Text::string(),X::string()|wxMenu(),X::bool()|term()) -> ok|wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuprepend">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% prepend(This::wxMenu(), Itemid::integer(), Text::string(), Help::string(), IsCheckable::bool()) -> ok </c>
%% </p>
%% <p><c>
%% prepend(This::wxMenu(), Itemid::integer(), Text::string(), Submenu::wxMenu(), [Option]) -> wxMenuItem:wxMenuItem() </c>
%%<br /> Option = {help, string()}
%% </p>
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

%% @spec (This::wxMenu(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv prependCheckItem(This,Itemid,Text, [])
prependCheckItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  prependCheckItem(This,Itemid,Text, []).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {help, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuprependcheckitem">external documentation</a>.
prependCheckItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_PrependCheckItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string()) -> wxMenuItem:wxMenuItem()
%% @equiv prependRadioItem(This,Itemid,Text, [])
prependRadioItem(This,Itemid,Text)
 when is_record(This, wx_ref),is_integer(Itemid),is_list(Text) ->
  prependRadioItem(This,Itemid,Text, []).

%% @spec (This::wxMenu(), Itemid::integer(), Text::string(), [Option]) -> wxMenuItem:wxMenuItem()
%% Option = {help, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuprependradioitem">external documentation</a>.
prependRadioItem(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Text, Options)
 when is_integer(Itemid),is_list(Text),is_list(Options) ->
  ?CLASS(ThisT,wxMenu),
  Text_UC = unicode:characters_to_binary([Text,0]),
  MOpts = fun({help, Help}, Acc) ->   Help_UC = unicode:characters_to_binary([Help,0]),[<<1:32/?UI,(byte_size(Help_UC)):32/?UI,(Help_UC)/binary, 0:(((8- ((0+byte_size(Help_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxMenu_PrependRadioItem,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxMenu()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuprependseparator">external documentation</a>.
prependSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxMenu),
  wxe_util:call(?wxMenu_PrependSeparator,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxMenu(),X::integer()|term()) -> wxMenuItem:wxMenuItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenuremove">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% remove(This::wxMenu(), Itemid::integer()) -> wxMenuItem:wxMenuItem() </c>
%% </p>
%% <p><c>
%% remove(This::wxMenu(), Item::wxMenuItem:wxMenuItem()) -> wxMenuItem:wxMenuItem() </c>
%% </p>
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

%% @spec (This::wxMenu(), Itemid::integer(), HelpString::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenusethelpstring">external documentation</a>.
setHelpString(#wx_ref{type=ThisT,ref=ThisRef},Itemid,HelpString)
 when is_integer(Itemid),is_list(HelpString) ->
  ?CLASS(ThisT,wxMenu),
  HelpString_UC = unicode:characters_to_binary([HelpString,0]),
  wxe_util:cast(?wxMenu_SetHelpString,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(HelpString_UC)):32/?UI,(HelpString_UC)/binary, 0:(((8- ((4+byte_size(HelpString_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxMenu(), Itemid::integer(), Label::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenusetlabel">external documentation</a>.
setLabel(#wx_ref{type=ThisT,ref=ThisRef},Itemid,Label)
 when is_integer(Itemid),is_list(Label) ->
  ?CLASS(ThisT,wxMenu),
  Label_UC = unicode:characters_to_binary([Label,0]),
  wxe_util:cast(?wxMenu_SetLabel,
  <<ThisRef:32/?UI,Itemid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxMenu(), Title::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmenu.html#wxmenusettitle">external documentation</a>.
setTitle(#wx_ref{type=ThisT,ref=ThisRef},Title)
 when is_list(Title) ->
  ?CLASS(ThisT,wxMenu),
  Title_UC = unicode:characters_to_binary([Title,0]),
  wxe_util:cast(?wxMenu_SetTitle,
  <<ThisRef:32/?UI,(byte_size(Title_UC)):32/?UI,(Title_UC)/binary, 0:(((8- ((0+byte_size(Title_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxMenu()) -> ok
%% @doc Destroys this object, do not use object again
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
