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

-module(wxGBSizerItem).
-moduledoc """
Functions for wxGBSizerItem class

The `m:wxGBSizerItem` class is used by the `m:wxGridBagSizer` for tracking the
items in the sizer. It adds grid position and spanning information to the normal
`m:wxSizerItem` by adding `wxGBPosition` (not implemented in wx) and `wxGBSpan`
(not implemented in wx) attributes. Most of the time you will not need to use a
`m:wxGBSizerItem` directly in your code, but there are a couple of cases where
it is handy.

This class is derived (and can use functions) from: `m:wxSizerItem`

wxWidgets docs:
[wxGBSizerItem](https://docs.wxwidgets.org/3.1/classwx_g_b_sizer_item.html)
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([assignSizer/2,assignSpacer/2,assignSpacer/3,assignWindow/2,calcMin/1,
  deleteWindows/1,detachSizer/1,getBorder/1,getFlag/1,getMinSize/1,getPosition/1,
  getProportion/1,getRatio/1,getRect/1,getSize/1,getSizer/1,getSpacer/1,
  getUserData/1,getWindow/1,isShown/1,isSizer/1,isSpacer/1,isWindow/1,
  parent_class/1,setBorder/2,setDimension/3,setFlag/2,setInitSize/3,
  setMinSize/2,setMinSize/3,setProportion/2,setRatio/2,setRatio/3,show/2]).

-type wxGBSizerItem() :: wx:wx_object().
-export_type([wxGBSizerItem/0]).
%% @hidden
-doc false.
parent_class(wxSizerItem) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

 %% From wxSizerItem
%% @hidden
-doc false.
show(This,Show) -> wxSizerItem:show(This,Show).
%% @hidden
-doc false.
assignWindow(This,Window) -> wxSizerItem:assignWindow(This,Window).
%% @hidden
-doc false.
assignSpacer(This,W,H) -> wxSizerItem:assignSpacer(This,W,H).
%% @hidden
-doc false.
assignSpacer(This,Size) -> wxSizerItem:assignSpacer(This,Size).
%% @hidden
-doc false.
assignSizer(This,Sizer) -> wxSizerItem:assignSizer(This,Sizer).
%% @hidden
-doc false.
setRatio(This,Width,Height) -> wxSizerItem:setRatio(This,Width,Height).
%% @hidden
-doc false.
setRatio(This,Ratio) -> wxSizerItem:setRatio(This,Ratio).
%% @hidden
-doc false.
setProportion(This,Proportion) -> wxSizerItem:setProportion(This,Proportion).
%% @hidden
-doc false.
setMinSize(This,X,Y) -> wxSizerItem:setMinSize(This,X,Y).
%% @hidden
-doc false.
setMinSize(This,Size) -> wxSizerItem:setMinSize(This,Size).
%% @hidden
-doc false.
setInitSize(This,X,Y) -> wxSizerItem:setInitSize(This,X,Y).
%% @hidden
-doc false.
setFlag(This,Flag) -> wxSizerItem:setFlag(This,Flag).
%% @hidden
-doc false.
setDimension(This,Pos,Size) -> wxSizerItem:setDimension(This,Pos,Size).
%% @hidden
-doc false.
setBorder(This,Border) -> wxSizerItem:setBorder(This,Border).
%% @hidden
-doc false.
isWindow(This) -> wxSizerItem:isWindow(This).
%% @hidden
-doc false.
isSpacer(This) -> wxSizerItem:isSpacer(This).
%% @hidden
-doc false.
isShown(This) -> wxSizerItem:isShown(This).
%% @hidden
-doc false.
isSizer(This) -> wxSizerItem:isSizer(This).
%% @hidden
-doc false.
getWindow(This) -> wxSizerItem:getWindow(This).
%% @hidden
-doc false.
getUserData(This) -> wxSizerItem:getUserData(This).
%% @hidden
-doc false.
getSpacer(This) -> wxSizerItem:getSpacer(This).
%% @hidden
-doc false.
getSizer(This) -> wxSizerItem:getSizer(This).
%% @hidden
-doc false.
getSize(This) -> wxSizerItem:getSize(This).
%% @hidden
-doc false.
getRect(This) -> wxSizerItem:getRect(This).
%% @hidden
-doc false.
getRatio(This) -> wxSizerItem:getRatio(This).
%% @hidden
-doc false.
getProportion(This) -> wxSizerItem:getProportion(This).
%% @hidden
-doc false.
getPosition(This) -> wxSizerItem:getPosition(This).
%% @hidden
-doc false.
getMinSize(This) -> wxSizerItem:getMinSize(This).
%% @hidden
-doc false.
getFlag(This) -> wxSizerItem:getFlag(This).
%% @hidden
-doc false.
getBorder(This) -> wxSizerItem:getBorder(This).
%% @hidden
-doc false.
detachSizer(This) -> wxSizerItem:detachSizer(This).
%% @hidden
-doc false.
deleteWindows(This) -> wxSizerItem:deleteWindows(This).
%% @hidden
-doc false.
calcMin(This) -> wxSizerItem:calcMin(This).
