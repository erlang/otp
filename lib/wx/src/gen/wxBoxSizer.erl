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

-module(wxBoxSizer).
-moduledoc """
The basic idea behind a box sizer is that windows will most often be laid out in rather
simple basic geometry, typically in a row or a column or several hierarchies of either.

For more information, please see overview_sizer_box.

See:
* `m:wxSizer`

* [Overview sizer](https://docs.wxwidgets.org/3.2/overview_sizer.html#overview_sizer)

This class is derived, and can use functions, from:

* `m:wxSizer`

wxWidgets docs: [wxBoxSizer](https://docs.wxwidgets.org/3.2/classwx_box_sizer.html)
""".
-include("wxe.hrl").
-export([destroy/1,getOrientation/1,new/1]).

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

-type wxBoxSizer() :: wx:wx_object().
-export_type([wxBoxSizer/0]).
-doc false.
parent_class(wxSizer) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Constructor for a `m:wxBoxSizer`.

`orient` may be either of wxVERTICAL or wxHORIZONTAL for creating either a column sizer
or a row sizer.
""".
-spec new(Orient) -> wxBoxSizer() when
	Orient::integer().
new(Orient)
 when is_integer(Orient) ->
  wxe_util:queue_cmd(Orient,?get_env(),?wxBoxSizer_new),
  wxe_util:rec(?wxBoxSizer_new).

-doc "Returns the orientation of the box sizer, either wxVERTICAL or wxHORIZONTAL.".
-spec getOrientation(This) -> integer() when
	This::wxBoxSizer().
getOrientation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBoxSizer),
  wxe_util:queue_cmd(This,?get_env(),?wxBoxSizer_GetOrientation),
  wxe_util:rec(?wxBoxSizer_GetOrientation).

-doc "Destroys the object".
-spec destroy(This::wxBoxSizer()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBoxSizer),
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
