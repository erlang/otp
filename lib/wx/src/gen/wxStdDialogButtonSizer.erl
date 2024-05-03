%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-moduledoc """
Functions for wxStdDialogButtonSizer class

This class creates button layouts which conform to the standard button spacing
and ordering defined by the platform or toolkit's user interface guidelines (if
such things exist). By using this class, you can ensure that all your standard
dialogs look correct on all major platforms. Currently it conforms to the
Windows, GTK+ and macOS human interface guidelines.

When there aren't interface guidelines defined for a particular platform or
toolkit, `m:wxStdDialogButtonSizer` reverts to the Windows implementation.

To use this class, first add buttons to the sizer by calling `addButton/2` (or
`setAffirmativeButton/2`, `setNegativeButton/2` or `setCancelButton/2`) and then
call Realize in order to create the actual button layout used. Other than these
special operations, this sizer works like any other sizer.

If you add a button with wxID_SAVE, on macOS the button will be renamed to
"Save" and the wxID_NO button will be renamed to "Don't Save" in accordance with
the macOS Human Interface Guidelines.

See: `m:wxSizer`,
[Overview sizer](https://docs.wxwidgets.org/3.1/overview_sizer.html#overview_sizer),
`wxDialog:createButtonSizer/2`

This class is derived (and can use functions) from: `m:wxBoxSizer` `m:wxSizer`

wxWidgets docs:
[wxStdDialogButtonSizer](https://docs.wxwidgets.org/3.1/classwx_std_dialog_button_sizer.html)
""".
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
-doc false.
parent_class(wxBoxSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizerwxstddialogbuttonsizer">external documentation</a>.
-doc "Constructor for a `m:wxStdDialogButtonSizer`.".
-spec new() -> wxStdDialogButtonSizer().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxStdDialogButtonSizer_new),
  wxe_util:rec(?wxStdDialogButtonSizer_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizeraddbutton">external documentation</a>.
-doc """
Adds a button to the `m:wxStdDialogButtonSizer`.

The `button` must have one of the following identifiers:
""".
-spec addButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
addButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_AddButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizerrealize">external documentation</a>.
-doc """
Rearranges the buttons and applies proper spacing between buttons to make them
match the platform or toolkit's interface guidelines.
""".
-spec realize(This) -> 'ok' when
	This::wxStdDialogButtonSizer().
realize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxStdDialogButtonSizer_Realize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetaffirmativebutton">external documentation</a>.
-doc """
Sets the affirmative button for the sizer.

This allows you to use identifiers other than the standard identifiers outlined
above.
""".
-spec setAffirmativeButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
setAffirmativeButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_SetAffirmativeButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetcancelbutton">external documentation</a>.
-doc """
Sets the cancel button for the sizer.

This allows you to use identifiers other than the standard identifiers outlined
above.
""".
-spec setCancelButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
setCancelButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_SetCancelButton).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstddialogbuttonsizer.html#wxstddialogbuttonsizersetnegativebutton">external documentation</a>.
-doc """
Sets the negative button for the sizer.

This allows you to use identifiers other than the standard identifiers outlined
above.
""".
-spec setNegativeButton(This, Button) -> 'ok' when
	This::wxStdDialogButtonSizer(), Button::wxButton:wxButton().
setNegativeButton(#wx_ref{type=ThisT}=This,#wx_ref{type=ButtonT}=Button) ->
  ?CLASS(ThisT,wxStdDialogButtonSizer),
  ?CLASS(ButtonT,wxButton),
  wxe_util:queue_cmd(This,Button,?get_env(),?wxStdDialogButtonSizer_SetNegativeButton).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxStdDialogButtonSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxStdDialogButtonSizer),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxBoxSizer
%% @hidden
-doc false.
getOrientation(This) -> wxBoxSizer:getOrientation(This).
 %% From wxSizer
%% @hidden
-doc false.
showItems(This,Show) -> wxSizer:showItems(This,Show).
%% @hidden
-doc false.
show(This,Window, Options) -> wxSizer:show(This,Window, Options).
%% @hidden
-doc false.
show(This,Window) -> wxSizer:show(This,Window).
%% @hidden
-doc false.
setSizeHints(This,Window) -> wxSizer:setSizeHints(This,Window).
%% @hidden
-doc false.
setItemMinSize(This,Window,Width,Height) -> wxSizer:setItemMinSize(This,Window,Width,Height).
%% @hidden
-doc false.
setItemMinSize(This,Window,Size) -> wxSizer:setItemMinSize(This,Window,Size).
%% @hidden
-doc false.
setMinSize(This,Width,Height) -> wxSizer:setMinSize(This,Width,Height).
%% @hidden
-doc false.
setMinSize(This,Size) -> wxSizer:setMinSize(This,Size).
%% @hidden
-doc false.
setDimension(This,X,Y,Width,Height) -> wxSizer:setDimension(This,X,Y,Width,Height).
%% @hidden
-doc false.
setDimension(This,Pos,Size) -> wxSizer:setDimension(This,Pos,Size).
%% @hidden
-doc false.
replace(This,Oldwin,Newwin, Options) -> wxSizer:replace(This,Oldwin,Newwin, Options).
%% @hidden
-doc false.
replace(This,Oldwin,Newwin) -> wxSizer:replace(This,Oldwin,Newwin).
%% @hidden
-doc false.
remove(This,Index) -> wxSizer:remove(This,Index).
%% @hidden
-doc false.
prependStretchSpacer(This, Options) -> wxSizer:prependStretchSpacer(This, Options).
%% @hidden
-doc false.
prependStretchSpacer(This) -> wxSizer:prependStretchSpacer(This).
%% @hidden
-doc false.
prependSpacer(This,Size) -> wxSizer:prependSpacer(This,Size).
%% @hidden
-doc false.
prepend(This,Width,Height, Options) -> wxSizer:prepend(This,Width,Height, Options).
%% @hidden
-doc false.
prepend(This,Width,Height) -> wxSizer:prepend(This,Width,Height).
%% @hidden
-doc false.
prepend(This,Item) -> wxSizer:prepend(This,Item).
%% @hidden
-doc false.
layout(This) -> wxSizer:layout(This).
%% @hidden
-doc false.
recalcSizes(This) -> wxSizer:recalcSizes(This).
%% @hidden
-doc false.
isShown(This,Window) -> wxSizer:isShown(This,Window).
%% @hidden
-doc false.
insertStretchSpacer(This,Index, Options) -> wxSizer:insertStretchSpacer(This,Index, Options).
%% @hidden
-doc false.
insertStretchSpacer(This,Index) -> wxSizer:insertStretchSpacer(This,Index).
%% @hidden
-doc false.
insertSpacer(This,Index,Size) -> wxSizer:insertSpacer(This,Index,Size).
%% @hidden
-doc false.
insert(This,Index,Width,Height, Options) -> wxSizer:insert(This,Index,Width,Height, Options).
%% @hidden
-doc false.
insert(This,Index,Width,Height) -> wxSizer:insert(This,Index,Width,Height).
%% @hidden
-doc false.
insert(This,Index,Item) -> wxSizer:insert(This,Index,Item).
%% @hidden
-doc false.
hide(This,Window, Options) -> wxSizer:hide(This,Window, Options).
%% @hidden
-doc false.
hide(This,Window) -> wxSizer:hide(This,Window).
%% @hidden
-doc false.
getMinSize(This) -> wxSizer:getMinSize(This).
%% @hidden
-doc false.
getPosition(This) -> wxSizer:getPosition(This).
%% @hidden
-doc false.
getSize(This) -> wxSizer:getSize(This).
%% @hidden
-doc false.
getItem(This,Window, Options) -> wxSizer:getItem(This,Window, Options).
%% @hidden
-doc false.
getItem(This,Window) -> wxSizer:getItem(This,Window).
%% @hidden
-doc false.
getChildren(This) -> wxSizer:getChildren(This).
%% @hidden
-doc false.
fitInside(This,Window) -> wxSizer:fitInside(This,Window).
%% @hidden
-doc false.
setVirtualSizeHints(This,Window) -> wxSizer:setVirtualSizeHints(This,Window).
%% @hidden
-doc false.
fit(This,Window) -> wxSizer:fit(This,Window).
%% @hidden
-doc false.
detach(This,Window) -> wxSizer:detach(This,Window).
%% @hidden
-doc false.
clear(This, Options) -> wxSizer:clear(This, Options).
%% @hidden
-doc false.
clear(This) -> wxSizer:clear(This).
%% @hidden
-doc false.
calcMin(This) -> wxSizer:calcMin(This).
%% @hidden
-doc false.
addStretchSpacer(This, Options) -> wxSizer:addStretchSpacer(This, Options).
%% @hidden
-doc false.
addStretchSpacer(This) -> wxSizer:addStretchSpacer(This).
%% @hidden
-doc false.
addSpacer(This,Size) -> wxSizer:addSpacer(This,Size).
%% @hidden
-doc false.
add(This,Width,Height, Options) -> wxSizer:add(This,Width,Height, Options).
%% @hidden
-doc false.
add(This,Width,Height) -> wxSizer:add(This,Width,Height).
%% @hidden
-doc false.
add(This,Window) -> wxSizer:add(This,Window).
