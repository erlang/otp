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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html">wxFlexGridSizer</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGridSizer}
%% <br />{@link wxSizer}
%% </p>
%% @type wxFlexGridSizer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxFlexGridSizer).
-include("wxe.hrl").
-export([addGrowableCol/2,addGrowableCol/3,addGrowableRow/2,addGrowableRow/3,
  destroy/1,getFlexibleDirection/1,getNonFlexibleGrowMode/1,new/1,new/2,
  new/4,removeGrowableCol/2,removeGrowableRow/2,setFlexibleDirection/2,
  setNonFlexibleGrowMode/2]).

%% inherited exports
-export([add/2,add/3,add/4,addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,
  calcMin/1,clear/1,clear/2,detach/2,fit/2,fitInside/2,getChildren/1,getCols/1,
  getHGap/1,getItem/2,getItem/3,getMinSize/1,getPosition/1,getRows/1,
  getSize/1,getVGap/1,hide/2,hide/3,insert/3,insert/4,insert/5,insertSpacer/3,
  insertStretchSpacer/2,insertStretchSpacer/3,isShown/2,layout/1,parent_class/1,
  prepend/2,prepend/3,prepend/4,prependSpacer/2,prependStretchSpacer/1,
  prependStretchSpacer/2,recalcSizes/1,remove/2,replace/3,replace/4,
  setCols/2,setDimension/5,setHGap/2,setItemMinSize/3,setItemMinSize/4,
  setMinSize/2,setMinSize/3,setRows/2,setSizeHints/2,setVGap/2,setVirtualSizeHints/2,
  show/2,show/3]).

%% @hidden
parent_class(wxGridSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Cols::integer()) -> wxFlexGridSizer()
%% @equiv new(Cols, [])
new(Cols)
 when is_integer(Cols) ->
  new(Cols, []).

%% @spec (Cols::integer(), [Option]) -> wxFlexGridSizer()
%% Option = {vgap, integer()} | {hgap, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizerwxflexgridsizer">external documentation</a>.
new(Cols, Options)
 when is_integer(Cols),is_list(Options) ->
  MOpts = fun({vgap, Vgap}, Acc) -> [<<1:32/?UI,Vgap:32/?UI>>|Acc];
          ({hgap, Hgap}, Acc) -> [<<2:32/?UI,Hgap:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxFlexGridSizer_new_2,
  <<Cols:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (Rows::integer(), Cols::integer(), Vgap::integer(), Hgap::integer()) -> wxFlexGridSizer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizerwxflexgridsizer">external documentation</a>.
new(Rows,Cols,Vgap,Hgap)
 when is_integer(Rows),is_integer(Cols),is_integer(Vgap),is_integer(Hgap) ->
  wxe_util:construct(?wxFlexGridSizer_new_4,
  <<Rows:32/?UI,Cols:32/?UI,Vgap:32/?UI,Hgap:32/?UI>>).

%% @spec (This::wxFlexGridSizer(), Idx::integer()) -> ok
%% @equiv addGrowableCol(This,Idx, [])
addGrowableCol(This,Idx)
 when is_record(This, wx_ref),is_integer(Idx) ->
  addGrowableCol(This,Idx, []).

%% @spec (This::wxFlexGridSizer(), Idx::integer(), [Option]) -> ok
%% Option = {proportion, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizeraddgrowablecol">external documentation</a>.
addGrowableCol(#wx_ref{type=ThisT,ref=ThisRef},Idx, Options)
 when is_integer(Idx),is_list(Options) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxFlexGridSizer_AddGrowableCol,
  <<ThisRef:32/?UI,Idx:32/?UI, BinOpt/binary>>).

%% @spec (This::wxFlexGridSizer(), Idx::integer()) -> ok
%% @equiv addGrowableRow(This,Idx, [])
addGrowableRow(This,Idx)
 when is_record(This, wx_ref),is_integer(Idx) ->
  addGrowableRow(This,Idx, []).

%% @spec (This::wxFlexGridSizer(), Idx::integer(), [Option]) -> ok
%% Option = {proportion, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizeraddgrowablerow">external documentation</a>.
addGrowableRow(#wx_ref{type=ThisT,ref=ThisRef},Idx, Options)
 when is_integer(Idx),is_list(Options) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  MOpts = fun({proportion, Proportion}, Acc) -> [<<1:32/?UI,Proportion:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxFlexGridSizer_AddGrowableRow,
  <<ThisRef:32/?UI,Idx:32/?UI, BinOpt/binary>>).

%% @spec (This::wxFlexGridSizer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizergetflexibledirection">external documentation</a>.
getFlexibleDirection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:call(?wxFlexGridSizer_GetFlexibleDirection,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFlexGridSizer()) -> WxFlexSizerGrowMode
%% WxFlexSizerGrowMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizergetnonflexiblegrowmode">external documentation</a>.
%%<br /> WxFlexSizerGrowMode is one of ?wxFLEX_GROWMODE_NONE | ?wxFLEX_GROWMODE_SPECIFIED | ?wxFLEX_GROWMODE_ALL
getNonFlexibleGrowMode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:call(?wxFlexGridSizer_GetNonFlexibleGrowMode,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFlexGridSizer(), Idx::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizerremovegrowablecol">external documentation</a>.
removeGrowableCol(#wx_ref{type=ThisT,ref=ThisRef},Idx)
 when is_integer(Idx) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:cast(?wxFlexGridSizer_RemoveGrowableCol,
  <<ThisRef:32/?UI,Idx:32/?UI>>).

%% @spec (This::wxFlexGridSizer(), Idx::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizerremovegrowablerow">external documentation</a>.
removeGrowableRow(#wx_ref{type=ThisT,ref=ThisRef},Idx)
 when is_integer(Idx) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:cast(?wxFlexGridSizer_RemoveGrowableRow,
  <<ThisRef:32/?UI,Idx:32/?UI>>).

%% @spec (This::wxFlexGridSizer(), Direction::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizersetflexibledirection">external documentation</a>.
setFlexibleDirection(#wx_ref{type=ThisT,ref=ThisRef},Direction)
 when is_integer(Direction) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:cast(?wxFlexGridSizer_SetFlexibleDirection,
  <<ThisRef:32/?UI,Direction:32/?UI>>).

%% @spec (This::wxFlexGridSizer(), Mode::WxFlexSizerGrowMode) -> ok
%% WxFlexSizerGrowMode = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxflexgridsizer.html#wxflexgridsizersetnonflexiblegrowmode">external documentation</a>.
%%<br /> WxFlexSizerGrowMode is one of ?wxFLEX_GROWMODE_NONE | ?wxFLEX_GROWMODE_SPECIFIED | ?wxFLEX_GROWMODE_ALL
setNonFlexibleGrowMode(#wx_ref{type=ThisT,ref=ThisRef},Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:cast(?wxFlexGridSizer_SetNonFlexibleGrowMode,
  <<ThisRef:32/?UI,Mode:32/?UI>>).

%% @spec (This::wxFlexGridSizer()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFlexGridSizer),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
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
calcMin(This) -> wxSizer:calcMin(This).
%% @hidden
addStretchSpacer(This, Options) -> wxSizer:addStretchSpacer(This, Options).
%% @hidden
addStretchSpacer(This) -> wxSizer:addStretchSpacer(This).
%% @hidden
addSpacer(This,Size) -> wxSizer:addSpacer(This,Size).
%% @hidden
add(This,Width,Height, Options) -> wxSizer:add(This,Width,Height, Options).
%% @hidden
add(This,Width,Height) -> wxSizer:add(This,Width,Height).
%% @hidden
add(This,Window) -> wxSizer:add(This,Window).
