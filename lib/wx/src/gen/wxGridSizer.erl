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

-module(wxGridSizer).
-moduledoc """
A grid sizer is a sizer which lays out its children in a two-dimensional table with all
table fields having the same size, i.e.

the width of each field is the width of the widest child, the height of each field is the
height of the tallest child.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxSizer`

wxWidgets docs: [wxGridSizer](https://docs.wxwidgets.org/3.2/classwx_grid_sizer.html)
""".
-include("wxe.hrl").
-export([destroy/1,getCols/1,getHGap/1,getRows/1,getVGap/1,new/1,new/2,new/3,new/4,
  setCols/2,setHGap/2,setRows/2,setVGap/2]).

%% inherited exports
-export([add/2,add/3,add/4,addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,
  calcMin/1,clear/1,clear/2,detach/2,fit/2,fitInside/2,getChildren/1,getItem/2,
  getItem/3,getMinSize/1,getPosition/1,getSize/1,hide/2,hide/3,insert/3,
  insert/4,insert/5,insertSpacer/3,insertStretchSpacer/2,insertStretchSpacer/3,
  isShown/2,layout/1,parent_class/1,prepend/2,prepend/3,prepend/4,prependSpacer/2,
  prependStretchSpacer/1,prependStretchSpacer/2,recalcSizes/1,remove/2,
  replace/3,replace/4,setDimension/3,setDimension/5,setItemMinSize/3,
  setItemMinSize/4,setMinSize/2,setMinSize/3,setSizeHints/2,setVirtualSizeHints/2,
  show/2,show/3,showItems/2]).

-type wxGridSizer() :: wx:wx_object().
-export_type([wxGridSizer/0]).
-doc false.
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Cols, [])}).
-spec new(Cols) -> wxGridSizer() when
	Cols::integer().

new(Cols)
 when is_integer(Cols) ->
  new(Cols, []).

-doc "".
-spec new(Cols, [Option]) -> wxGridSizer() when
	Cols::integer(),
	Option :: {'gap', {W::integer(), H::integer()}}.
new(Cols, Options)
 when is_integer(Cols),is_list(Options) ->
  MOpts = fun({gap, {_gapW,_gapH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Cols, Opts,?get_env(),?wxGridSizer_new_2),
  wxe_util:rec(?wxGridSizer_new_2).

-doc "".
-spec new(Cols, Vgap, Hgap) -> wxGridSizer() when
	Cols::integer(), Vgap::integer(), Hgap::integer();
      (Rows, Cols, Gap) -> wxGridSizer() when
	Rows::integer(), Cols::integer(), Gap::{W::integer(), H::integer()}.
new(Cols,Vgap,Hgap)
 when is_integer(Cols),is_integer(Vgap),is_integer(Hgap) ->
  wxe_util:queue_cmd(Cols,Vgap,Hgap,?get_env(),?wxGridSizer_new_3_0),
  wxe_util:rec(?wxGridSizer_new_3_0);
new(Rows,Cols,{GapW,GapH} = Gap)
 when is_integer(Rows),is_integer(Cols),is_integer(GapW),is_integer(GapH) ->
  wxe_util:queue_cmd(Rows,Cols,Gap,?get_env(),?wxGridSizer_new_3_1),
  wxe_util:rec(?wxGridSizer_new_3_1).

-doc "".
-spec new(Rows, Cols, Vgap, Hgap) -> wxGridSizer() when
	Rows::integer(), Cols::integer(), Vgap::integer(), Hgap::integer().
new(Rows,Cols,Vgap,Hgap)
 when is_integer(Rows),is_integer(Cols),is_integer(Vgap),is_integer(Hgap) ->
  wxe_util:queue_cmd(Rows,Cols,Vgap,Hgap,?get_env(),?wxGridSizer_new_4),
  wxe_util:rec(?wxGridSizer_new_4).

-doc """
Returns the number of columns that has been specified for the sizer.

Returns zero if the sizer is automatically adjusting the number of columns depending on
number of its children. To get the effective number of columns or rows being currently
used, see `GetEffectiveColsCount()` (not implemented in wx)
""".
-spec getCols(This) -> integer() when
	This::wxGridSizer().
getCols(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetCols),
  wxe_util:rec(?wxGridSizer_GetCols).

-doc "Returns the horizontal gap (in pixels) between cells in the sizer.".
-spec getHGap(This) -> integer() when
	This::wxGridSizer().
getHGap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetHGap),
  wxe_util:rec(?wxGridSizer_GetHGap).

-doc """
Returns the number of rows that has been specified for the sizer.

Returns zero if the sizer is automatically adjusting the number of rows depending on
number of its children. To get the effective number of columns or rows being currently
used, see `GetEffectiveRowsCount()` (not implemented in wx).
""".
-spec getRows(This) -> integer() when
	This::wxGridSizer().
getRows(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetRows),
  wxe_util:rec(?wxGridSizer_GetRows).

-doc "Returns the vertical gap (in pixels) between the cells in the sizer.".
-spec getVGap(This) -> integer() when
	This::wxGridSizer().
getVGap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetVGap),
  wxe_util:rec(?wxGridSizer_GetVGap).

-doc "Sets the number of columns in the sizer.".
-spec setCols(This, Cols) -> 'ok' when
	This::wxGridSizer(), Cols::integer().
setCols(#wx_ref{type=ThisT}=This,Cols)
 when is_integer(Cols) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Cols,?get_env(),?wxGridSizer_SetCols).

-doc "Sets the horizontal gap (in pixels) between cells in the sizer.".
-spec setHGap(This, Gap) -> 'ok' when
	This::wxGridSizer(), Gap::integer().
setHGap(#wx_ref{type=ThisT}=This,Gap)
 when is_integer(Gap) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Gap,?get_env(),?wxGridSizer_SetHGap).

-doc "Sets the number of rows in the sizer.".
-spec setRows(This, Rows) -> 'ok' when
	This::wxGridSizer(), Rows::integer().
setRows(#wx_ref{type=ThisT}=This,Rows)
 when is_integer(Rows) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Rows,?get_env(),?wxGridSizer_SetRows).

-doc "Sets the vertical gap (in pixels) between the cells in the sizer.".
-spec setVGap(This, Gap) -> 'ok' when
	This::wxGridSizer(), Gap::integer().
setVGap(#wx_ref{type=ThisT}=This,Gap)
 when is_integer(Gap) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Gap,?get_env(),?wxGridSizer_SetVGap).

-doc "Destroys the object".
-spec destroy(This::wxGridSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridSizer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
