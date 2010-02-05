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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html">wxStdDialogButtonSizer</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxBoxSizer}
%% <br />{@link wxSizer}
%% </p>
%% @type wxStdDialogButtonSizer().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

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
  recalcSizes/1,remove/2,replace/3,replace/4,setDimension/5,setItemMinSize/3,
  setItemMinSize/4,setMinSize/2,setMinSize/3,setSizeHints/2,setVirtualSizeHints/2,
  show/2,show/3]).

%% @hidden
parent_class(wxBoxSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxStdDialogButtonSizer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizerwxstddialogbuttonsizer">external documentation</a>.
new() ->
  wxe_util:construct(?wxStdDialogButtonSizer_new,
  <<>>).

%% @spec (This::wxStdDialogButtonSizer(), Button::wxButton:wxButton()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizeraddbutton">external documentation</a>.
addButton(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ButtonT,ref=ButtonRef}) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:cast(?wxStdDialogButtonSizer_AddButton,
  <<ThisRef:32/?UI,ButtonRef:32/?UI>>).

%% @spec (This::wxStdDialogButtonSizer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizerrealize">external documentation</a>.
realize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  wxe_util:cast(?wxStdDialogButtonSizer_Realize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxStdDialogButtonSizer(), Button::wxButton:wxButton()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetaffirmativebutton">external documentation</a>.
setAffirmativeButton(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ButtonT,ref=ButtonRef}) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:cast(?wxStdDialogButtonSizer_SetAffirmativeButton,
  <<ThisRef:32/?UI,ButtonRef:32/?UI>>).

%% @spec (This::wxStdDialogButtonSizer(), Button::wxButton:wxButton()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetcancelbutton">external documentation</a>.
setCancelButton(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ButtonT,ref=ButtonRef}) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:cast(?wxStdDialogButtonSizer_SetCancelButton,
  <<ThisRef:32/?UI,ButtonRef:32/?UI>>).

%% @spec (This::wxStdDialogButtonSizer(), Button::wxButton:wxButton()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetnegativebutton">external documentation</a>.
setNegativeButton(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ButtonT,ref=ButtonRef}) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:cast(?wxStdDialogButtonSizer_SetNegativeButton,
  <<ThisRef:32/?UI,ButtonRef:32/?UI>>).

%% @spec (This::wxStdDialogButtonSizer()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxStdDialogButtonSizer),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxBoxSizer
%% @hidden
getOrientation(This) -> wxBoxSizer:getOrientation(This).
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
