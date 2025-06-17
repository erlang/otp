%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxFlexGridSizer).
-moduledoc """
A flex grid sizer is a sizer which lays out its children in a two-dimensional table with
all table fields in one row having the same height and all fields in one column having the
same width, but all rows or all columns are not necessarily the same height or width as in
the `m:wxGridSizer`.

Since wxWidgets 2.5.0, `m:wxFlexGridSizer` can also size items equally in one direction
but unequally ("flexibly") in the other. If the sizer is only flexible in one direction
(this can be changed using `setFlexibleDirection/2`), it needs to be decided how the sizer should grow in the other
("non-flexible") direction in order to fill the available space. The `setNonFlexibleGrowMode/2` method serves this purpose.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxGridSizer`

* `m:wxSizer`

wxWidgets docs: [wxFlexGridSizer](https://docs.wxwidgets.org/3.2/classwx_flex_grid_sizer.html)
""".
-include("wxe.hrl").
-export([addGrowableCol/2,addGrowableCol/3,addGrowableRow/2,addGrowableRow/3,
  destroy/1,getFlexibleDirection/1,getNonFlexibleGrowMode/1,new/1,new/2,
  new/3,new/4,removeGrowableCol/2,removeGrowableRow/2,setFlexibleDirection/2,
  setNonFlexibleGrowMode/2]).

%% inherited exports
-export([add/2,add/3,add/4,addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,
  calcMin/1,clear/1,clear/2,detach/2,fit/2,fitInside/2,getChildren/1,getCols/1,
  getHGap/1,getItem/2,getItem/3,getMinSize/1,getPosition/1,getRows/1,
  getSize/1,getVGap/1,hide/2,hide/3,insert/3,insert/4,insert/5,insertSpacer/3,
  insertStretchSpacer/2,insertStretchSpacer/3,isShown/2,layout/1,parent_class/1,
  prepend/2,prepend/3,prepend/4,prependSpacer/2,prependStretchSpacer/1,
  prependStretchSpacer/2,recalcSizes/1,remove/2,replace/3,replace/4,
  setCols/2,setDimension/3,setDimension/5,setHGap/2,setItemMinSize/3,
  setItemMinSize/4,setMinSize/2,setMinSize/3,setRows/2,setSizeHints/2,
  setVGap/2,setVirtualSizeHints/2,show/2,show/3,showItems/2]).

-type wxFlexGridSizer() :: wx:wx_object().
-export_type([wxFlexGridSizer/0]).
-doc false.
parent_class(wxGridSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Cols, [])}).
-spec new(Cols) -> wxFlexGridSizer() when
	Cols::integer().

new(Cols)
 when is_integer(Cols) ->
  new(Cols, []).

-doc "".
-spec new(Cols, [Option]) -> wxFlexGridSizer() when
	Cols::integer(),
	Option :: {'gap', {W::integer(), H::integer()}}.
new(Cols, Options)
 when is_integer(Cols),is_list(Options) ->
  MOpts = fun({gap, {_gapW,_gapH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Cols, Opts,?get_env(),?wxFlexGridSizer_new_2),
  wxe_util:rec(?wxFlexGridSizer_new_2).

-doc "".
-spec new(Cols, Vgap, Hgap) -> wxFlexGridSizer() when
	Cols::integer(), Vgap::integer(), Hgap::integer();
      (Rows, Cols, Gap) -> wxFlexGridSizer() when
	Rows::integer(), Cols::integer(), Gap::{W::integer(), H::integer()}.
new(Cols,Vgap,Hgap)
 when is_integer(Cols),is_integer(Vgap),is_integer(Hgap) ->
  wxe_util:queue_cmd(Cols,Vgap,Hgap,?get_env(),?wxFlexGridSizer_new_3_0),
  wxe_util:rec(?wxFlexGridSizer_new_3_0);
new(Rows,Cols,{GapW,GapH} = Gap)
 when is_integer(Rows),is_integer(Cols),is_integer(GapW),is_integer(GapH) ->
  wxe_util:queue_cmd(Rows,Cols,Gap,?get_env(),?wxFlexGridSizer_new_3_1),
  wxe_util:rec(?wxFlexGridSizer_new_3_1).

-doc "".
-spec new(Rows, Cols, Vgap, Hgap) -> wxFlexGridSizer() when
	Rows::integer(), Cols::integer(), Vgap::integer(), Hgap::integer().
new(Rows,Cols,Vgap,Hgap)
 when is_integer(Rows),is_integer(Cols),is_integer(Vgap),is_integer(Hgap) ->
  wxe_util:queue_cmd(Rows,Cols,Vgap,Hgap,?get_env(),?wxFlexGridSizer_new_4),
  wxe_util:rec(?wxFlexGridSizer_new_4).

-doc(#{equiv => addGrowableCol(This,Idx, [])}).
-spec addGrowableCol(This, Idx) -> 'ok' when
	This::wxFlexGridSizer(), Idx::integer().

addGrowableCol(This,Idx)
 when is_record(This, wx_ref),is_integer(Idx) ->
  addGrowableCol(This,Idx, []).

-doc """
Specifies that column `idx` (starting from zero) should be grown if there is extra space
available to the sizer.

The `proportion` parameter has the same meaning as the stretch factor for the sizers (see `m:wxBoxSizer`)
except that if all proportions are 0, then all columns are resized equally (instead of not
being resized at all).

Notice that the column must not be already growable, if you need to change the proportion
you must call `removeGrowableCol/2` first and then make it growable (with a different proportion) again. You
can use `IsColGrowable()` (not implemented in wx) to check whether a column is already
growable.
""".
-spec addGrowableCol(This, Idx, [Option]) -> 'ok' when
	This::wxFlexGridSizer(), Idx::integer(),
	Option :: {'proportion', integer()}.
addGrowableCol(#wx_ref{type=ThisT}=This,Idx, Options)
 when is_integer(Idx),is_list(Options) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Idx, Opts,?get_env(),?wxFlexGridSizer_AddGrowableCol).

-doc(#{equiv => addGrowableRow(This,Idx, [])}).
-spec addGrowableRow(This, Idx) -> 'ok' when
	This::wxFlexGridSizer(), Idx::integer().

addGrowableRow(This,Idx)
 when is_record(This, wx_ref),is_integer(Idx) ->
  addGrowableRow(This,Idx, []).

-doc """
Specifies that row idx (starting from zero) should be grown if there is extra space
available to the sizer.

This is identical to `addGrowableCol/3` except that it works with rows and not columns.
""".
-spec addGrowableRow(This, Idx, [Option]) -> 'ok' when
	This::wxFlexGridSizer(), Idx::integer(),
	Option :: {'proportion', integer()}.
addGrowableRow(#wx_ref{type=ThisT}=This,Idx, Options)
 when is_integer(Idx),is_list(Options) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  MOpts = fun({proportion, _proportion} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Idx, Opts,?get_env(),?wxFlexGridSizer_AddGrowableRow).

-doc """
Returns a ?wxOrientation value that specifies whether the sizer flexibly resizes its
columns, rows, or both (default).

Return: One of the following values:

* wxVERTICAL: Rows are flexibly sized.

* wxHORIZONTAL: Columns are flexibly sized.

* wxBOTH: Both rows and columns are flexibly sized (this is the default value).

See: `setFlexibleDirection/2`
""".
-spec getFlexibleDirection(This) -> integer() when
	This::wxFlexGridSizer().
getFlexibleDirection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxFlexGridSizer_GetFlexibleDirection),
  wxe_util:rec(?wxFlexGridSizer_GetFlexibleDirection).

-doc """
Returns the value that specifies how the sizer grows in the "non-flexible" direction if
there is one.

The behaviour of the elements in the flexible direction (i.e. both rows and columns by
default, or rows only if `getFlexibleDirection/1` is `wxVERTICAL` or columns only if it is `wxHORIZONTAL`) is
always governed by their proportion as specified in the call to `addGrowableRow/3` or `addGrowableCol/3`. What happens in the
other direction depends on the value of returned by this function as described below.

Return: One of the following values:

* wxFLEX_GROWMODE_NONE: Sizer doesn't grow its elements at all in the non-flexible direction.

* wxFLEX_GROWMODE_SPECIFIED: Sizer honors growable columns/rows set with `addGrowableCol/3` and `addGrowableRow/3` in the
non-flexible direction as well. In this case equal sizing applies to minimum sizes of
columns or rows (this is the default value).

* wxFLEX_GROWMODE_ALL: Sizer equally stretches all columns or rows in the non-flexible
direction, independently of the proportions applied in the flexible direction.

See:
* `setFlexibleDirection/2`

* `setNonFlexibleGrowMode/2`
""".
%%  Res = ?wxFLEX_GROWMODE_NONE | ?wxFLEX_GROWMODE_SPECIFIED | ?wxFLEX_GROWMODE_ALL
-spec getNonFlexibleGrowMode(This) -> wx:wx_enum() when
	This::wxFlexGridSizer().
getNonFlexibleGrowMode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxFlexGridSizer_GetNonFlexibleGrowMode),
  wxe_util:rec(?wxFlexGridSizer_GetNonFlexibleGrowMode).

-doc "Specifies that the `idx` column index is no longer growable.".
-spec removeGrowableCol(This, Idx) -> 'ok' when
	This::wxFlexGridSizer(), Idx::integer().
removeGrowableCol(#wx_ref{type=ThisT}=This,Idx)
 when is_integer(Idx) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:queue_cmd(This,Idx,?get_env(),?wxFlexGridSizer_RemoveGrowableCol).

-doc "Specifies that the `idx` row index is no longer growable.".
-spec removeGrowableRow(This, Idx) -> 'ok' when
	This::wxFlexGridSizer(), Idx::integer().
removeGrowableRow(#wx_ref{type=ThisT}=This,Idx)
 when is_integer(Idx) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:queue_cmd(This,Idx,?get_env(),?wxFlexGridSizer_RemoveGrowableRow).

-doc """
Specifies whether the sizer should flexibly resize its columns, rows, or both.

Argument `direction` can be `wxVERTICAL`, `wxHORIZONTAL` or `wxBOTH` (which is the
default value). Any other value is ignored.

See `getFlexibleDirection/1` for the explanation of these values. Note that this method does not trigger
relayout.
""".
-spec setFlexibleDirection(This, Direction) -> 'ok' when
	This::wxFlexGridSizer(), Direction::integer().
setFlexibleDirection(#wx_ref{type=ThisT}=This,Direction)
 when is_integer(Direction) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:queue_cmd(This,Direction,?get_env(),?wxFlexGridSizer_SetFlexibleDirection).

-doc """
Specifies how the sizer should grow in the non-flexible direction if there is one (so `setFlexibleDirection/2`
must have been called previously).

Argument `mode` can be one of those documented in `getNonFlexibleGrowMode/1`, please see there for their
explanation. Note that this method does not trigger relayout.
""".
%%  Mode = ?wxFLEX_GROWMODE_NONE | ?wxFLEX_GROWMODE_SPECIFIED | ?wxFLEX_GROWMODE_ALL
-spec setNonFlexibleGrowMode(This, Mode) -> 'ok' when
	This::wxFlexGridSizer(), Mode::wx:wx_enum().
setNonFlexibleGrowMode(#wx_ref{type=ThisT}=This,Mode)
 when is_integer(Mode) ->
  ?CLASS(ThisT,wxFlexGridSizer),
  wxe_util:queue_cmd(This,Mode,?get_env(),?wxFlexGridSizer_SetNonFlexibleGrowMode).

-doc "Destroys the object".
-spec destroy(This::wxFlexGridSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFlexGridSizer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxGridSizer
-doc false.
setVGap(This,Gap) -> wxGridSizer:setVGap(This,Gap).
-doc false.
setRows(This,Rows) -> wxGridSizer:setRows(This,Rows).
-doc false.
setHGap(This,Gap) -> wxGridSizer:setHGap(This,Gap).
-doc false.
setCols(This,Cols) -> wxGridSizer:setCols(This,Cols).
-doc false.
getVGap(This) -> wxGridSizer:getVGap(This).
-doc false.
getRows(This) -> wxGridSizer:getRows(This).
-doc false.
getHGap(This) -> wxGridSizer:getHGap(This).
-doc false.
getCols(This) -> wxGridSizer:getCols(This).
 %% From wxSizer
-doc false.
showItems(This,Show) -> wxSizer:showItems(This,Show).
-doc false.
show(This,Window, Options) -> wxSizer:show(This,Window, Options).
-doc false.
show(This,Window) -> wxSizer:show(This,Window).
-doc false.
setSizeHints(This,Window) -> wxSizer:setSizeHints(This,Window).
-doc false.
setItemMinSize(This,Window,Width,Height) -> wxSizer:setItemMinSize(This,Window,Width,Height).
-doc false.
setItemMinSize(This,Window,Size) -> wxSizer:setItemMinSize(This,Window,Size).
-doc false.
setMinSize(This,Width,Height) -> wxSizer:setMinSize(This,Width,Height).
-doc false.
setMinSize(This,Size) -> wxSizer:setMinSize(This,Size).
-doc false.
setDimension(This,X,Y,Width,Height) -> wxSizer:setDimension(This,X,Y,Width,Height).
-doc false.
setDimension(This,Pos,Size) -> wxSizer:setDimension(This,Pos,Size).
-doc false.
replace(This,Oldwin,Newwin, Options) -> wxSizer:replace(This,Oldwin,Newwin, Options).
-doc false.
replace(This,Oldwin,Newwin) -> wxSizer:replace(This,Oldwin,Newwin).
-doc false.
remove(This,Index) -> wxSizer:remove(This,Index).
-doc false.
prependStretchSpacer(This, Options) -> wxSizer:prependStretchSpacer(This, Options).
-doc false.
prependStretchSpacer(This) -> wxSizer:prependStretchSpacer(This).
-doc false.
prependSpacer(This,Size) -> wxSizer:prependSpacer(This,Size).
-doc false.
prepend(This,Width,Height, Options) -> wxSizer:prepend(This,Width,Height, Options).
-doc false.
prepend(This,Width,Height) -> wxSizer:prepend(This,Width,Height).
-doc false.
prepend(This,Item) -> wxSizer:prepend(This,Item).
-doc false.
layout(This) -> wxSizer:layout(This).
-doc false.
recalcSizes(This) -> wxSizer:recalcSizes(This).
-doc false.
isShown(This,Window) -> wxSizer:isShown(This,Window).
-doc false.
insertStretchSpacer(This,Index, Options) -> wxSizer:insertStretchSpacer(This,Index, Options).
-doc false.
insertStretchSpacer(This,Index) -> wxSizer:insertStretchSpacer(This,Index).
-doc false.
insertSpacer(This,Index,Size) -> wxSizer:insertSpacer(This,Index,Size).
-doc false.
insert(This,Index,Width,Height, Options) -> wxSizer:insert(This,Index,Width,Height, Options).
-doc false.
insert(This,Index,Width,Height) -> wxSizer:insert(This,Index,Width,Height).
-doc false.
insert(This,Index,Item) -> wxSizer:insert(This,Index,Item).
-doc false.
hide(This,Window, Options) -> wxSizer:hide(This,Window, Options).
-doc false.
hide(This,Window) -> wxSizer:hide(This,Window).
-doc false.
getMinSize(This) -> wxSizer:getMinSize(This).
-doc false.
getPosition(This) -> wxSizer:getPosition(This).
-doc false.
getSize(This) -> wxSizer:getSize(This).
-doc false.
getItem(This,Window, Options) -> wxSizer:getItem(This,Window, Options).
-doc false.
getItem(This,Window) -> wxSizer:getItem(This,Window).
-doc false.
getChildren(This) -> wxSizer:getChildren(This).
-doc false.
fitInside(This,Window) -> wxSizer:fitInside(This,Window).
-doc false.
setVirtualSizeHints(This,Window) -> wxSizer:setVirtualSizeHints(This,Window).
-doc false.
fit(This,Window) -> wxSizer:fit(This,Window).
-doc false.
detach(This,Window) -> wxSizer:detach(This,Window).
-doc false.
clear(This, Options) -> wxSizer:clear(This, Options).
-doc false.
clear(This) -> wxSizer:clear(This).
-doc false.
calcMin(This) -> wxSizer:calcMin(This).
-doc false.
addStretchSpacer(This, Options) -> wxSizer:addStretchSpacer(This, Options).
-doc false.
addStretchSpacer(This) -> wxSizer:addStretchSpacer(This).
-doc false.
addSpacer(This,Size) -> wxSizer:addSpacer(This,Size).
-doc false.
add(This,Width,Height, Options) -> wxSizer:add(This,Width,Height, Options).
-doc false.
add(This,Width,Height) -> wxSizer:add(This,Width,Height).
-doc false.
add(This,Window) -> wxSizer:add(This,Window).
