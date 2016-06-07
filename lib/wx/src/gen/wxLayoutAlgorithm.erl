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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html">wxLayoutAlgorithm</a>.
%% @type wxLayoutAlgorithm().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxLayoutAlgorithm).
-include("wxe.hrl").
-export([destroy/1,layoutFrame/2,layoutFrame/3,layoutMDIFrame/2,layoutMDIFrame/3,
  layoutWindow/2,layoutWindow/3,new/0]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxLayoutAlgorithm/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxLayoutAlgorithm() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmwxlayoutalgorithm">external documentation</a>.
-spec new() -> wxLayoutAlgorithm().
new() ->
  wxe_util:construct(?wxLayoutAlgorithm_new,
  <<>>).

%% @equiv layoutFrame(This,Frame, [])
-spec layoutFrame(This, Frame) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxFrame:wxFrame().

layoutFrame(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutFrame(This,Frame, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutframe">external documentation</a>.
-spec layoutFrame(This, Frame, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxFrame:wxFrame(),
	Option :: {'mainWindow', wxWindow:wxWindow()}.
layoutFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxFrame),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT,ref=MainWindowRef}}, Acc) ->   ?CLASS(MainWindowT,wxWindow),[<<1:32/?UI,MainWindowRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLayoutAlgorithm_LayoutFrame,
  <<ThisRef:32/?UI,FrameRef:32/?UI, BinOpt/binary>>).

%% @equiv layoutMDIFrame(This,Frame, [])
-spec layoutMDIFrame(This, Frame) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxMDIParentFrame:wxMDIParentFrame().

layoutMDIFrame(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutMDIFrame(This,Frame, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutmdiframe">external documentation</a>.
-spec layoutMDIFrame(This, Frame, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxMDIParentFrame:wxMDIParentFrame(),
	Option :: {'rect', {X::integer(), Y::integer(), W::integer(), H::integer()}}.
layoutMDIFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxMDIParentFrame),
  MOpts = fun({rect, {RectX,RectY,RectW,RectH}}, Acc) -> [<<1:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLayoutAlgorithm_LayoutMDIFrame,
  <<ThisRef:32/?UI,FrameRef:32/?UI, BinOpt/binary>>).

%% @equiv layoutWindow(This,Frame, [])
-spec layoutWindow(This, Frame) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxWindow:wxWindow().

layoutWindow(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutWindow(This,Frame, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutwindow">external documentation</a>.
-spec layoutWindow(This, Frame, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Frame::wxWindow:wxWindow(),
	Option :: {'mainWindow', wxWindow:wxWindow()}.
layoutWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxWindow),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT,ref=MainWindowRef}}, Acc) ->   ?CLASS(MainWindowT,wxWindow),[<<1:32/?UI,MainWindowRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLayoutAlgorithm_LayoutWindow,
  <<ThisRef:32/?UI,FrameRef:32/?UI, BinOpt/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxLayoutAlgorithm()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLayoutAlgorithm),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
