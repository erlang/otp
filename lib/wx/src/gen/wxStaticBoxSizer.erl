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

-module(wxStaticBoxSizer).
-moduledoc """
Functions for wxStaticBoxSizer class

`m:wxStaticBoxSizer` is a sizer derived from `m:wxBoxSizer` but adds a static
box around the sizer.

The static box may be either created independently or the sizer may create it
itself as a convenience. In any case, the sizer owns the `m:wxStaticBox` control
and will delete it in the `m:wxStaticBoxSizer` destructor.

Note that since wxWidgets 2.9.1 you are encouraged to create the windows which
are added to `m:wxStaticBoxSizer` as children of `m:wxStaticBox` itself, see
this class documentation for more details.

Example of use of this class:

See: `m:wxSizer`, `m:wxStaticBox`, `m:wxBoxSizer`,
[Overview sizer](https://docs.wxwidgets.org/3.1/overview_sizer.html#overview_sizer)

This class is derived (and can use functions) from: `m:wxBoxSizer` `m:wxSizer`

wxWidgets docs:
[wxStaticBoxSizer](https://docs.wxwidgets.org/3.1/classwx_static_box_sizer.html)
""".
-include("wxe.hrl").
-export([destroy/1,getStaticBox/1,new/2,new/3]).

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

-type wxStaticBoxSizer() :: wx:wx_object().
-export_type([wxStaticBoxSizer/0]).
%% @hidden
-doc false.
parent_class(wxBoxSizer) -> true;
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstaticboxsizer.html#wxstaticboxsizerwxstaticboxsizer">external documentation</a>.
%% <br /> Also:<br />
%% new(Box, Orient) -> wxStaticBoxSizer() when<br />
%% 	Box::wxStaticBox:wxStaticBox(), Orient::integer().<br />
%% 
-doc "This constructor uses an already existing static box.".
-spec new(Orient, Parent) -> wxStaticBoxSizer() when
	Orient::integer(), Parent::wxWindow:wxWindow();
      (Box, Orient) -> wxStaticBoxSizer() when
	Box::wxStaticBox:wxStaticBox(), Orient::integer().

new(Orient,Parent)
 when is_integer(Orient),is_record(Parent, wx_ref) ->
  new(Orient,Parent, []);
new(#wx_ref{type=BoxT}=Box,Orient)
 when is_integer(Orient) ->
  ?CLASS(BoxT,wxStaticBox),
  wxe_util:queue_cmd(Box,Orient,?get_env(),?wxStaticBoxSizer_new_2),
  wxe_util:rec(?wxStaticBoxSizer_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstaticboxsizer.html#wxstaticboxsizerwxstaticboxsizer">external documentation</a>.
-doc """
This constructor creates a new static box with the given label and parent
window.
""".
-spec new(Orient, Parent, [Option]) -> wxStaticBoxSizer() when
	Orient::integer(), Parent::wxWindow:wxWindow(),
	Option :: {'label', unicode:chardata()}.
new(Orient,#wx_ref{type=ParentT}=Parent, Options)
 when is_integer(Orient),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Orient,Parent, Opts,?get_env(),?wxStaticBoxSizer_new_3),
  wxe_util:rec(?wxStaticBoxSizer_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxstaticboxsizer.html#wxstaticboxsizergetstaticbox">external documentation</a>.
-doc "Returns the static box associated with the sizer.".
-spec getStaticBox(This) -> wxStaticBox:wxStaticBox() when
	This::wxStaticBoxSizer().
getStaticBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxStaticBoxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxStaticBoxSizer_GetStaticBox),
  wxe_util:rec(?wxStaticBoxSizer_GetStaticBox).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxStaticBoxSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxStaticBoxSizer),
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
