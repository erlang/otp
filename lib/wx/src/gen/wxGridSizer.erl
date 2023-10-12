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

-module(wxGridSizer).
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
%% @hidden
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Cols, [])
-spec new(Cols) -> wxGridSizer() when
	Cols::integer().

new(Cols)
 when is_integer(Cols) ->
  new(Cols, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizerwxgridsizer">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizerwxgridsizer">external documentation</a>.
%% <br /> Also:<br />
%% new(Rows, Cols, Gap) -> wxGridSizer() when<br />
%% 	Rows::integer(), Cols::integer(), Gap::{W::integer(), H::integer()}.<br />
%% 
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizerwxgridsizer">external documentation</a>.
-spec new(Rows, Cols, Vgap, Hgap) -> wxGridSizer() when
	Rows::integer(), Cols::integer(), Vgap::integer(), Hgap::integer().
new(Rows,Cols,Vgap,Hgap)
 when is_integer(Rows),is_integer(Cols),is_integer(Vgap),is_integer(Hgap) ->
  wxe_util:queue_cmd(Rows,Cols,Vgap,Hgap,?get_env(),?wxGridSizer_new_4),
  wxe_util:rec(?wxGridSizer_new_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizergetcols">external documentation</a>.
-spec getCols(This) -> integer() when
	This::wxGridSizer().
getCols(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetCols),
  wxe_util:rec(?wxGridSizer_GetCols).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizergethgap">external documentation</a>.
-spec getHGap(This) -> integer() when
	This::wxGridSizer().
getHGap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetHGap),
  wxe_util:rec(?wxGridSizer_GetHGap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizergetrows">external documentation</a>.
-spec getRows(This) -> integer() when
	This::wxGridSizer().
getRows(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetRows),
  wxe_util:rec(?wxGridSizer_GetRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizergetvgap">external documentation</a>.
-spec getVGap(This) -> integer() when
	This::wxGridSizer().
getVGap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxGridSizer_GetVGap),
  wxe_util:rec(?wxGridSizer_GetVGap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizersetcols">external documentation</a>.
-spec setCols(This, Cols) -> 'ok' when
	This::wxGridSizer(), Cols::integer().
setCols(#wx_ref{type=ThisT}=This,Cols)
 when is_integer(Cols) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Cols,?get_env(),?wxGridSizer_SetCols).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizersethgap">external documentation</a>.
-spec setHGap(This, Gap) -> 'ok' when
	This::wxGridSizer(), Gap::integer().
setHGap(#wx_ref{type=ThisT}=This,Gap)
 when is_integer(Gap) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Gap,?get_env(),?wxGridSizer_SetHGap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizersetrows">external documentation</a>.
-spec setRows(This, Rows) -> 'ok' when
	This::wxGridSizer(), Rows::integer().
setRows(#wx_ref{type=ThisT}=This,Rows)
 when is_integer(Rows) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Rows,?get_env(),?wxGridSizer_SetRows).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgridsizer.html#wxgridsizersetvgap">external documentation</a>.
-spec setVGap(This, Gap) -> 'ok' when
	This::wxGridSizer(), Gap::integer().
setVGap(#wx_ref{type=ThisT}=This,Gap)
 when is_integer(Gap) ->
  ?CLASS(ThisT,wxGridSizer),
  wxe_util:queue_cmd(This,Gap,?get_env(),?wxGridSizer_SetVGap).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGridSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGridSizer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxSizer
%% @hidden
showItems(This,Show) -> wxSizer:showItems(This,Show).
%% @hidden
show(This,Window, Options) -> wxSizer:show(This,Window, Options).
%% @hidden
show(This,Window) -> wxSizer:show(This,Window).
%% @hidden
setSizeHints(This,Window) -> wxSizer:setSizeHints(This,Window).
%% @hidden
setItemMinSize(This,Window,Width,Height) -> wxSizer:setItemMinSize(This,Window,Width,Height).
%% @hidden
setItemMinSize(This,Window,Size) -> wxSizer:setItemMinSize(This,Window,Size).
%% @hidden
setMinSize(This,Width,Height) -> wxSizer:setMinSize(This,Width,Height).
%% @hidden
setMinSize(This,Size) -> wxSizer:setMinSize(This,Size).
%% @hidden
setDimension(This,X,Y,Width,Height) -> wxSizer:setDimension(This,X,Y,Width,Height).
%% @hidden
setDimension(This,Pos,Size) -> wxSizer:setDimension(This,Pos,Size).
%% @hidden
replace(This,Oldwin,Newwin, Options) -> wxSizer:replace(This,Oldwin,Newwin, Options).
%% @hidden
replace(This,Oldwin,Newwin) -> wxSizer:replace(This,Oldwin,Newwin).
%% @hidden
remove(This,Index) -> wxSizer:remove(This,Index).
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
prepend(This,Item) -> wxSizer:prepend(This,Item).
%% @hidden
layout(This) -> wxSizer:layout(This).
%% @hidden
recalcSizes(This) -> wxSizer:recalcSizes(This).
%% @hidden
isShown(This,Window) -> wxSizer:isShown(This,Window).
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
insert(This,Index,Item) -> wxSizer:insert(This,Index,Item).
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
setVirtualSizeHints(This,Window) -> wxSizer:setVirtualSizeHints(This,Window).
%% @hidden
fit(This,Window) -> wxSizer:fit(This,Window).
%% @hidden
detach(This,Window) -> wxSizer:detach(This,Window).
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
