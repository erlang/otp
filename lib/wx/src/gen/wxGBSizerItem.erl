%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgbsizeritem.html">wxGBSizerItem</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxSizerItem}
%% </p>
%% @type wxGBSizerItem().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGBSizerItem).
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([calcMin/1,deleteWindows/1,detachSizer/1,getBorder/1,getFlag/1,getMinSize/1,
  getPosition/1,getProportion/1,getRatio/1,getRect/1,getSize/1,getSizer/1,
  getSpacer/1,getUserData/1,getWindow/1,isShown/1,isSizer/1,isSpacer/1,
  isWindow/1,parent_class/1,setBorder/2,setDimension/3,setFlag/2,setInitSize/3,
  setMinSize/2,setMinSize/3,setProportion/2,setRatio/2,setRatio/3,setSizer/2,
  setSpacer/2,setSpacer/3,setWindow/2,show/2]).

-export_type([wxGBSizerItem/0]).
%% @hidden
parent_class(wxSizerItem) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGBSizerItem() :: wx:wx_object().
 %% From wxSizerItem
%% @hidden
show(This,Show) -> wxSizerItem:show(This,Show).
%% @hidden
setWindow(This,Window) -> wxSizerItem:setWindow(This,Window).
%% @hidden
setSpacer(This,Width,Height) -> wxSizerItem:setSpacer(This,Width,Height).
%% @hidden
setSpacer(This,Size) -> wxSizerItem:setSpacer(This,Size).
%% @hidden
setSizer(This,Sizer) -> wxSizerItem:setSizer(This,Sizer).
%% @hidden
setRatio(This,Width,Height) -> wxSizerItem:setRatio(This,Width,Height).
%% @hidden
setRatio(This,Ratio) -> wxSizerItem:setRatio(This,Ratio).
%% @hidden
setProportion(This,Proportion) -> wxSizerItem:setProportion(This,Proportion).
%% @hidden
setMinSize(This,X,Y) -> wxSizerItem:setMinSize(This,X,Y).
%% @hidden
setMinSize(This,Size) -> wxSizerItem:setMinSize(This,Size).
%% @hidden
setInitSize(This,X,Y) -> wxSizerItem:setInitSize(This,X,Y).
%% @hidden
setFlag(This,Flag) -> wxSizerItem:setFlag(This,Flag).
%% @hidden
setDimension(This,Pos,Size) -> wxSizerItem:setDimension(This,Pos,Size).
%% @hidden
setBorder(This,Border) -> wxSizerItem:setBorder(This,Border).
%% @hidden
isWindow(This) -> wxSizerItem:isWindow(This).
%% @hidden
isSpacer(This) -> wxSizerItem:isSpacer(This).
%% @hidden
isShown(This) -> wxSizerItem:isShown(This).
%% @hidden
isSizer(This) -> wxSizerItem:isSizer(This).
%% @hidden
getWindow(This) -> wxSizerItem:getWindow(This).
%% @hidden
getUserData(This) -> wxSizerItem:getUserData(This).
%% @hidden
getSpacer(This) -> wxSizerItem:getSpacer(This).
%% @hidden
getSizer(This) -> wxSizerItem:getSizer(This).
%% @hidden
getSize(This) -> wxSizerItem:getSize(This).
%% @hidden
getRect(This) -> wxSizerItem:getRect(This).
%% @hidden
getRatio(This) -> wxSizerItem:getRatio(This).
%% @hidden
getProportion(This) -> wxSizerItem:getProportion(This).
%% @hidden
getPosition(This) -> wxSizerItem:getPosition(This).
%% @hidden
getMinSize(This) -> wxSizerItem:getMinSize(This).
%% @hidden
getFlag(This) -> wxSizerItem:getFlag(This).
%% @hidden
getBorder(This) -> wxSizerItem:getBorder(This).
%% @hidden
detachSizer(This) -> wxSizerItem:detachSizer(This).
%% @hidden
deleteWindows(This) -> wxSizerItem:deleteWindows(This).
%% @hidden
calcMin(This) -> wxSizerItem:calcMin(This).
