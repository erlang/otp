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

-module(wxStdDialogButtonSizer).
-include("wxe.hrl").
-export([addButton/2,destroy/1,new/0,realize/1,setAffirmativeButton/2,setCancelButton/2,
  setNegativeButton/2]).

%% inherited exports
-export([add/2,add/3,add/4,addSpacer/2,addStretchSpacer/1,addStretchSpacer/2,
  calcMin/1,clear/1,clear/2,detach/2,fit/2,fitInside/2,getChildren/1,getItem/2,
  getItem/3,getMinSize/1,getOrientation/1,getPosition/1,getSize/1,hide/2,
  hide/3,insert/3,insert/4,insert/5,insertSpacer/3,insertStretchSpacer/2,
  insertStretchSpacer/3,isShown/2,layout/1,parent_class/1,prepend/2,
  prepend/3,prepend/4,prependSpacer/2,prependStretchSpacer/1,prependStretchSpacer/2,
  recalcSizes/1,remove/2,replace/3,replace/4,setDimension/3,setDimension/5,
  setItemMinSize/3,setItemMinSize/4,setMinSize/2,setMinSize/3,setSizeHints/2,
  setVirtualSizeHints/2,show/2,show/3,showItems/2]).

-type wxStdDialogButtonSizer() :: wx:wx_object().
-export_type([wxStdDialogButtonSizer/0]).
%% @hidden
parent_class(wxBoxSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizerwxstddialogbuttonsizer">external documentation</a>.
-spec new() -> wxStdDialogButtonSizer().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxStdDialogButtonSizer_new),
  wxe_util:rec(?wxStdDialogButtonSizer_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizeraddbutton">external documentation</a>.
-spec addButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
addButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_AddButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizerrealize">external documentation</a>.
-spec realize(This) -> 'ok' when
	This::wxStdDialogButtonSizer().
realize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxStdDialogButtonSizer_Realize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetaffirmativebutton">external documentation</a>.
-spec setAffirmativeButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
setAffirmativeButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_SetAffirmativeButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetcancelbutton">external documentation</a>.
-spec setCancelButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
setCancelButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_SetCancelButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetnegativebutton">external documentation</a>.
-spec setNegativeButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
setNegativeButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_SetNegativeButton).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxStdDialogButtonSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxStdDialogButtonSizer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxBoxSizer
%% @hidden
getOrientation(This) -> wxBoxSizer:getOrientation(This).
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
