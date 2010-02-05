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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html">wxGridBagSizer</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxFlexGridSizer}
%% <br />{@link wxGridSizer}
%% <br />{@link wxSizer}
%% </p>
%% @type wxGridBagSizer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGridBagSizer).
-include("wxe.hrl").
-export([add/2,add/3,add/4,add/5,calcMin/1,checkForIntersection/2,checkForIntersection/3,
  checkForIntersection/4,destroy/1,findItem/2,findItemAtPoint/2,findItemAtPosition/2,
  findItemWithData/2,getCellSize/3,getEmptyCellSize/1,getItemPosition/2,
  getItemSpan/2,new/0,new/1,setEmptyCellSize/2,setItemPosition/3,setItemSpan/3]).

%% inherited exports
-export([addGrowableCol/2,addGrowableCol/3,addGrowableRow/2,addGrowableRow/3,
  addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,clear/1,clear/2,
  detach/2,fit/2,fitInside/2,getChildren/1,getCols/1,getFlexibleDirection/1,
  getHGap/1,getItem/2,getItem/3,getMinSize/1,getNonFlexibleGrowMode/1,
  getPosition/1,getRows/1,getSize/1,getVGap/1,hide/2,hide/3,insert/3,insert/4,
  insert/5,insertSpacer/3,insertStretchSpacer/2,insertStretchSpacer/3,
  isShown/2,layout/1,parent_class/1,prepend/2,prepend/3,prepend/4,prependSpacer/2,
  prependStretchSpacer/1,prependStretchSpacer/2,recalcSizes/1,remove/2,
  removeGrowableCol/2,removeGrowableRow/2,replace/3,replace/4,setCols/2,
  setDimension/5,setFlexibleDirection/2,setHGap/2,setItemMinSize/3,
  setItemMinSize/4,setMinSize/2,setMinSize/3,setNonFlexibleGrowMode/2,
  setRows/2,setSizeHints/2,setVGap/2,setVirtualSizeHints/2,show/2,show/3]).

%% @hidden
parent_class(wxFlexGridSizer) -> true;
parent_class(wxGridSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxGridBagSizer()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxGridBagSizer()
%% Option = {vgap, integer()} | {hgap, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizerwxgridbagsizer">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({vgap, Vgap}, Acc) -> [<<1:32/?UI,Vgap:32/?UI>>|Acc];
          ({hgap, Hgap}, Acc) -> [<<2:32/?UI,Hgap:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxGridBagSizer_new,
  <<BinOpt/binary>>).

%% @spec (This::wxGridBagSizer(), Item::wxSizerItem:wxSizerItem() | wxGBSizerItem:wxGBSizerItem()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizeradd">external documentation</a>.
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  ItemOP = case ?CLASS_T(ItemT,wxSizerItem) of
     true ->
       ?wxGridBagSizer_Add_1_1;
     _ -> ?CLASS(ItemT,wxGBSizerItem),
       ?wxGridBagSizer_Add_1_0
     end,
  wxe_util:call(ItemOP,
  <<ThisRef:32/?UI,ItemRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(),X::integer()|term(),X::integer()|term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizeradd">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% add(This::wxGridBagSizer(), Width::integer(), Height::integer()) -> add(This,Width,Height, []) </c></p>
%% <p><c>
%% add(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Pos::{R::integer(),C::integer()}) -> add(This,Window,Pos, []) </c></p>
%% <p><c>
%% add(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), [Option]) -> wxSizerItem:wxSizerItem() </c>
%%<br /> Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% </p>

add(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  add(This,Width,Height, []);

add(This,Window,Pos={PosR,PosC})
 when is_record(This, wx_ref),is_record(Window, wx_ref),is_integer(PosR),is_integer(PosC) ->
  add(This,Window,Pos, []);
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_Add_2_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_Add_2_0
     end,
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGridBagSizer(),X::integer()|term(),X::integer()|term(),X::term()) -> wxSizerItem:wxSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizeradd">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% add(This::wxGridBagSizer(), Width::integer(), Height::integer(), Pos::{R::integer(),C::integer()}) -> add(This,Width,Height,Pos, []) </c></p>
%% <p><c>
%% add(This::wxGridBagSizer(), Width::integer(), Height::integer(), [Option]) -> wxSizerItem:wxSizerItem() </c>
%%<br /> Option = {proportion, integer()} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% </p>
%% <p><c>
%% add(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Pos::{R::integer(),C::integer()}, [Option]) -> wxSizerItem:wxSizerItem() </c>
%%<br /> Option = {span, {RS::integer(),CS::integer()}} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% </p>

add(This,Width,Height,Pos={PosR,PosC})
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height),is_integer(PosR),is_integer(PosC) ->
  add(This,Width,Height,Pos, []);
add(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxGridBagSizer),
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGridBagSizer_Add_3_0,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>);
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},{PosR,PosC}, Options)
 when is_integer(PosR),is_integer(PosC),is_list(Options) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_Add_3_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_Add_3_1
     end,
  MOpts = fun({span, {SpanRS,SpanCS}}, Acc) -> [<<1:32/?UI,SpanRS:32/?UI,SpanCS:32/?UI,0:32>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,PosR:32/?UI,PosC:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGridBagSizer(), Width::integer(), Height::integer(), Pos::{R::integer(),C::integer()}, [Option]) -> wxSizerItem:wxSizerItem()
%% Option = {span, {RS::integer(),CS::integer()}} | {flag, integer()} | {border, integer()} | {userData, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizeradd">external documentation</a>.
add(#wx_ref{type=ThisT,ref=ThisRef},Width,Height,{PosR,PosC}, Options)
 when is_integer(Width),is_integer(Height),is_integer(PosR),is_integer(PosC),is_list(Options) ->
  ?CLASS(ThisT,wxGridBagSizer),
  MOpts = fun({span, {SpanRS,SpanCS}}, Acc) -> [<<1:32/?UI,SpanRS:32/?UI,SpanCS:32/?UI,0:32>>|Acc];
          ({flag, Flag}, Acc) -> [<<2:32/?UI,Flag:32/?UI>>|Acc];
          ({border, Border}, Acc) -> [<<3:32/?UI,Border:32/?UI>>|Acc];
          ({userData, #wx_ref{type=UserDataT,ref=UserDataRef}}, Acc) ->   ?CLASS(UserDataT,wx),[<<4:32/?UI,UserDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGridBagSizer_Add_4,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI,PosR:32/?UI,PosC:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxGridBagSizer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizercalcmin">external documentation</a>.
calcMin(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_CalcMin,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(), Item::wxGBSizerItem:wxGBSizerItem()) -> bool()
%% @equiv checkForIntersection(This,Item, [])
checkForIntersection(This,Item)
 when is_record(This, wx_ref),is_record(Item, wx_ref) ->
  checkForIntersection(This,Item, []).

%% @spec (This::wxGridBagSizer(),X::term(),X::term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizercheckforintersection">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% checkForIntersection(This::wxGridBagSizer(), Pos::{R::integer(),C::integer()}, Span::{RS::integer(),CS::integer()}) -> checkForIntersection(This,Pos,Span, []) </c></p>
%% <p><c>
%% checkForIntersection(This::wxGridBagSizer(), Item::wxGBSizerItem:wxGBSizerItem(), [Option]) -> bool() </c>
%%<br /> Option = {excludeItem, wxGBSizerItem:wxGBSizerItem()}
%% </p>

checkForIntersection(This,Pos={PosR,PosC},Span={SpanRS,SpanCS})
 when is_record(This, wx_ref),is_integer(PosR),is_integer(PosC),is_integer(SpanRS),is_integer(SpanCS) ->
  checkForIntersection(This,Pos,Span, []);
checkForIntersection(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ItemT,ref=ItemRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGridBagSizer),
  ?CLASS(ItemT,wxGBSizerItem),
  MOpts = fun({excludeItem, #wx_ref{type=ExcludeItemT,ref=ExcludeItemRef}}, Acc) ->   ?CLASS(ExcludeItemT,wxGBSizerItem),[<<1:32/?UI,ExcludeItemRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGridBagSizer_CheckForIntersection_2,
  <<ThisRef:32/?UI,ItemRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxGridBagSizer(), Pos::{R::integer(),C::integer()}, Span::{RS::integer(),CS::integer()}, [Option]) -> bool()
%% Option = {excludeItem, wxGBSizerItem:wxGBSizerItem()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizercheckforintersection">external documentation</a>.
checkForIntersection(#wx_ref{type=ThisT,ref=ThisRef},{PosR,PosC},{SpanRS,SpanCS}, Options)
 when is_integer(PosR),is_integer(PosC),is_integer(SpanRS),is_integer(SpanCS),is_list(Options) ->
  ?CLASS(ThisT,wxGridBagSizer),
  MOpts = fun({excludeItem, #wx_ref{type=ExcludeItemT,ref=ExcludeItemRef}}, Acc) ->   ?CLASS(ExcludeItemT,wxGBSizerItem),[<<1:32/?UI,ExcludeItemRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGridBagSizer_CheckForIntersection_3,
  <<ThisRef:32/?UI,PosR:32/?UI,PosC:32/?UI,SpanRS:32/?UI,SpanCS:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer()) -> wxGBSizerItem:wxGBSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizerfinditem">external documentation</a>.
findItem(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_FindItem_1_1;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_FindItem_1_0
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(), Pt::{X::integer(),Y::integer()}) -> wxGBSizerItem:wxGBSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizerfinditematpoint">external documentation</a>.
findItemAtPoint(#wx_ref{type=ThisT,ref=ThisRef},{PtX,PtY})
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_FindItemAtPoint,
  <<ThisRef:32/?UI,PtX:32/?UI,PtY:32/?UI>>).

%% @spec (This::wxGridBagSizer(), Pos::{R::integer(),C::integer()}) -> wxGBSizerItem:wxGBSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizerfinditematposition">external documentation</a>.
findItemAtPosition(#wx_ref{type=ThisT,ref=ThisRef},{PosR,PosC})
 when is_integer(PosR),is_integer(PosC) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_FindItemAtPosition,
  <<ThisRef:32/?UI,PosR:32/?UI,PosC:32/?UI>>).

%% @spec (This::wxGridBagSizer(), UserData::wx:wx()) -> wxGBSizerItem:wxGBSizerItem()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizerfinditemwithdata">external documentation</a>.
findItemWithData(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=UserDataT,ref=UserDataRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  ?CLASS(UserDataT,wx),
  wxe_util:call(?wxGridBagSizer_FindItemWithData,
  <<ThisRef:32/?UI,UserDataRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(), Row::integer(), Col::integer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizergetcellsize">external documentation</a>.
getCellSize(#wx_ref{type=ThisT,ref=ThisRef},Row,Col)
 when is_integer(Row),is_integer(Col) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_GetCellSize,
  <<ThisRef:32/?UI,Row:32/?UI,Col:32/?UI>>).

%% @spec (This::wxGridBagSizer()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizergetemptycellsize">external documentation</a>.
getEmptyCellSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_GetEmptyCellSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(),X::integer()|term()) -> {R::integer(),C::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizergetitemposition">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% getItemPosition(This::wxGridBagSizer(), Index::integer()) -> {R::integer(),C::integer()} </c>
%% </p>
%% <p><c>
%% getItemPosition(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer()) -> {R::integer(),C::integer()} </c>
%% </p>
getItemPosition(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_GetItemPosition_1_0,
  <<ThisRef:32/?UI,Index:32/?UI>>);
getItemPosition(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_GetItemPosition_1_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_GetItemPosition_1_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(),X::integer()|term()) -> {RS::integer(),CS::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizergetitemspan">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% getItemSpan(This::wxGridBagSizer(), Index::integer()) -> {RS::integer(),CS::integer()} </c>
%% </p>
%% <p><c>
%% getItemSpan(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer()) -> {RS::integer(),CS::integer()} </c>
%% </p>
getItemSpan(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_GetItemSpan_1_0,
  <<ThisRef:32/?UI,Index:32/?UI>>);
getItemSpan(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_GetItemSpan_1_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_GetItemSpan_1_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxGridBagSizer(), Sz::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizersetemptycellsize">external documentation</a>.
setEmptyCellSize(#wx_ref{type=ThisT,ref=ThisRef},{SzW,SzH})
 when is_integer(SzW),is_integer(SzH) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:cast(?wxGridBagSizer_SetEmptyCellSize,
  <<ThisRef:32/?UI,SzW:32/?UI,SzH:32/?UI>>).

%% @spec (This::wxGridBagSizer(),X::integer()|term(),Pos::{R::integer(),C::integer()}) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizersetitemposition">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setItemPosition(This::wxGridBagSizer(), Index::integer(), Pos::{R::integer(),C::integer()}) -> bool() </c>
%% </p>
%% <p><c>
%% setItemPosition(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Pos::{R::integer(),C::integer()}) -> bool() </c>
%% </p>
setItemPosition(#wx_ref{type=ThisT,ref=ThisRef},Index,{PosR,PosC})
 when is_integer(Index),is_integer(PosR),is_integer(PosC) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_SetItemPosition_2_0,
  <<ThisRef:32/?UI,Index:32/?UI,PosR:32/?UI,PosC:32/?UI>>);
setItemPosition(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},{PosR,PosC})
 when is_integer(PosR),is_integer(PosC) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_SetItemPosition_2_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_SetItemPosition_2_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,PosR:32/?UI,PosC:32/?UI>>).

%% @spec (This::wxGridBagSizer(),X::integer()|term(),Span::{RS::integer(),CS::integer()}) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgridbagsizer.html#wxgridbagsizersetitemspan">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% setItemSpan(This::wxGridBagSizer(), Index::integer(), Span::{RS::integer(),CS::integer()}) -> bool() </c>
%% </p>
%% <p><c>
%% setItemSpan(This::wxGridBagSizer(), Window::wxWindow:wxWindow() | wxSizer:wxSizer(), Span::{RS::integer(),CS::integer()}) -> bool() </c>
%% </p>
setItemSpan(#wx_ref{type=ThisT,ref=ThisRef},Index,{SpanRS,SpanCS})
 when is_integer(Index),is_integer(SpanRS),is_integer(SpanCS) ->
  ?CLASS(ThisT,wxGridBagSizer),
  wxe_util:call(?wxGridBagSizer_SetItemSpan_2_0,
  <<ThisRef:32/?UI,Index:32/?UI,SpanRS:32/?UI,SpanCS:32/?UI>>);
setItemSpan(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},{SpanRS,SpanCS})
 when is_integer(SpanRS),is_integer(SpanCS) ->
  ?CLASS(ThisT,wxGridBagSizer),
  WindowOP = case ?CLASS_T(WindowT,wxWindow) of
     true ->
       ?wxGridBagSizer_SetItemSpan_2_2;
     _ -> ?CLASS(WindowT,wxSizer),
       ?wxGridBagSizer_SetItemSpan_2_1
     end,
  wxe_util:call(WindowOP,
  <<ThisRef:32/?UI,WindowRef:32/?UI,SpanRS:32/?UI,SpanCS:32/?UI>>).

%% @spec (This::wxGridBagSizer()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridBagSizer),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxFlexGridSizer
%% @hidden
setNonFlexibleGrowMode(This,Mode) -> wxFlexGridSizer:setNonFlexibleGrowMode(This,Mode).
%% @hidden
setFlexibleDirection(This,Direction) -> wxFlexGridSizer:setFlexibleDirection(This,Direction).
%% @hidden
removeGrowableRow(This,Idx) -> wxFlexGridSizer:removeGrowableRow(This,Idx).
%% @hidden
removeGrowableCol(This,Idx) -> wxFlexGridSizer:removeGrowableCol(This,Idx).
%% @hidden
getNonFlexibleGrowMode(This) -> wxFlexGridSizer:getNonFlexibleGrowMode(This).
%% @hidden
getFlexibleDirection(This) -> wxFlexGridSizer:getFlexibleDirection(This).
%% @hidden
addGrowableRow(This,Idx, Options) -> wxFlexGridSizer:addGrowableRow(This,Idx, Options).
%% @hidden
addGrowableRow(This,Idx) -> wxFlexGridSizer:addGrowableRow(This,Idx).
%% @hidden
addGrowableCol(This,Idx, Options) -> wxFlexGridSizer:addGrowableCol(This,Idx, Options).
%% @hidden
addGrowableCol(This,Idx) -> wxFlexGridSizer:addGrowableCol(This,Idx).
 %% From wxGridSizer
%% @hidden
setVGap(This,Gap) -> wxGridSizer:setVGap(This,Gap).
%% @hidden
setRows(This,Rows) -> wxGridSizer:setRows(This,Rows).
%% @hidden
setHGap(This,Gap) -> wxGridSizer:setHGap(This,Gap).
%% @hidden
setCols(This,Cols) -> wxGridSizer:setCols(This,Cols).
%% @hidden
getVGap(This) -> wxGridSizer:getVGap(This).
%% @hidden
getRows(This) -> wxGridSizer:getRows(This).
%% @hidden
getHGap(This) -> wxGridSizer:getHGap(This).
%% @hidden
getCols(This) -> wxGridSizer:getCols(This).
 %% From wxSizer
%% @hidden
show(This,Index, Options) -> wxSizer:show(This,Index, Options).
%% @hidden
show(This,Index) -> wxSizer:show(This,Index).
%% @hidden
setVirtualSizeHints(This,Window) -> wxSizer:setVirtualSizeHints(This,Window).
%% @hidden
setSizeHints(This,Window) -> wxSizer:setSizeHints(This,Window).
%% @hidden
setItemMinSize(This,Index,Width,Height) -> wxSizer:setItemMinSize(This,Index,Width,Height).
%% @hidden
setItemMinSize(This,Index,Size) -> wxSizer:setItemMinSize(This,Index,Size).
%% @hidden
setMinSize(This,Width,Height) -> wxSizer:setMinSize(This,Width,Height).
%% @hidden
setMinSize(This,Size) -> wxSizer:setMinSize(This,Size).
%% @hidden
setDimension(This,X,Y,Width,Height) -> wxSizer:setDimension(This,X,Y,Width,Height).
%% @hidden
replace(This,Oldwin,Newwin, Options) -> wxSizer:replace(This,Oldwin,Newwin, Options).
%% @hidden
replace(This,Oldwin,Newwin) -> wxSizer:replace(This,Oldwin,Newwin).
%% @hidden
remove(This,Index) -> wxSizer:remove(This,Index).
%% @hidden
recalcSizes(This) -> wxSizer:recalcSizes(This).
%% @hidden
prependStretchSpacer(This, Options) -> wxSizer:prependStretchSpacer(This, Options).
%% @hidden
prependStretchSpacer(This) -> wxSizer:prependStretchSpacer(This).
%% @hidden
prependSpacer(This,Size) -> wxSizer:prependSpacer(This,Size).
%% @hidden
prepend(This,Width,Height, Options) -> wxSizer:prepend(This,Width,Height, Options).
%% @hidden
prepend(This,Width,Height) -> wxSizer:prepend(This,Width,Height).
%% @hidden
prepend(This,Window) -> wxSizer:prepend(This,Window).
%% @hidden
layout(This) -> wxSizer:layout(This).
%% @hidden
isShown(This,Index) -> wxSizer:isShown(This,Index).
%% @hidden
insertStretchSpacer(This,Index, Options) -> wxSizer:insertStretchSpacer(This,Index, Options).
%% @hidden
insertStretchSpacer(This,Index) -> wxSizer:insertStretchSpacer(This,Index).
%% @hidden
insertSpacer(This,Index,Size) -> wxSizer:insertSpacer(This,Index,Size).
%% @hidden
insert(This,Index,Width,Height, Options) -> wxSizer:insert(This,Index,Width,Height, Options).
%% @hidden
insert(This,Index,Width,Height) -> wxSizer:insert(This,Index,Width,Height).
%% @hidden
insert(This,Index,Window) -> wxSizer:insert(This,Index,Window).
%% @hidden
hide(This,Window, Options) -> wxSizer:hide(This,Window, Options).
%% @hidden
hide(This,Window) -> wxSizer:hide(This,Window).
%% @hidden
getMinSize(This) -> wxSizer:getMinSize(This).
%% @hidden
getPosition(This) -> wxSizer:getPosition(This).
%% @hidden
getSize(This) -> wxSizer:getSize(This).
%% @hidden
getItem(This,Window, Options) -> wxSizer:getItem(This,Window, Options).
%% @hidden
getItem(This,Window) -> wxSizer:getItem(This,Window).
%% @hidden
getChildren(This) -> wxSizer:getChildren(This).
%% @hidden
fitInside(This,Window) -> wxSizer:fitInside(This,Window).
%% @hidden
fit(This,Window) -> wxSizer:fit(This,Window).
%% @hidden
detach(This,Index) -> wxSizer:detach(This,Index).
%% @hidden
clear(This, Options) -> wxSizer:clear(This, Options).
%% @hidden
clear(This) -> wxSizer:clear(This).
%% @hidden
addStretchSpacer(This, Options) -> wxSizer:addStretchSpacer(This, Options).
%% @hidden
addStretchSpacer(This) -> wxSizer:addStretchSpacer(This).
%% @hidden
addSpacer(This,Size) -> wxSizer:addSpacer(This,Size).
