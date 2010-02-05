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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlayoutalgorithm.html">wxLayoutAlgorithm</a>.
%% @type wxLayoutAlgorithm().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxLayoutAlgorithm).
-include("wxe.hrl").
-export([destroy/1,layoutFrame/2,layoutFrame/3,layoutMDIFrame/2,layoutMDIFrame/3,
  layoutWindow/2,layoutWindow/3,new/0]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxLayoutAlgorithm()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlayoutalgorithm.html#wxlayoutalgorithmwxlayoutalgorithm">external documentation</a>.
new() ->
  wxe_util:construct(?wxLayoutAlgorithm_new,
  <<>>).

%% @spec (This::wxLayoutAlgorithm(), Frame::wxFrame:wxFrame()) -> bool()
%% @equiv layoutFrame(This,Frame, [])
layoutFrame(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutFrame(This,Frame, []).

%% @spec (This::wxLayoutAlgorithm(), Frame::wxFrame:wxFrame(), [Option]) -> bool()
%% Option = {mainWindow, wxWindow:wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutframe">external documentation</a>.
layoutFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxFrame),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT,ref=MainWindowRef}}, Acc) ->   ?CLASS(MainWindowT,wxWindow),[<<1:32/?UI,MainWindowRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLayoutAlgorithm_LayoutFrame,
  <<ThisRef:32/?UI,FrameRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxLayoutAlgorithm(), Frame::wxMDIParentFrame:wxMDIParentFrame()) -> bool()
%% @equiv layoutMDIFrame(This,Frame, [])
layoutMDIFrame(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutMDIFrame(This,Frame, []).

%% @spec (This::wxLayoutAlgorithm(), Frame::wxMDIParentFrame:wxMDIParentFrame(), [Option]) -> bool()
%% Option = {rect, {X::integer(),Y::integer(),W::integer(),H::integer()}}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutmdiframe">external documentation</a>.
layoutMDIFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxMDIParentFrame),
  MOpts = fun({rect, {RectX,RectY,RectW,RectH}}, Acc) -> [<<1:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI,0:32>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLayoutAlgorithm_LayoutMDIFrame,
  <<ThisRef:32/?UI,FrameRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxLayoutAlgorithm(), Frame::wxWindow:wxWindow()) -> bool()
%% @equiv layoutWindow(This,Frame, [])
layoutWindow(This,Frame)
 when is_record(This, wx_ref),is_record(Frame, wx_ref) ->
  layoutWindow(This,Frame, []).

%% @spec (This::wxLayoutAlgorithm(), Frame::wxWindow:wxWindow(), [Option]) -> bool()
%% Option = {mainWindow, wxWindow:wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutwindow">external documentation</a>.
layoutWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxWindow),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT,ref=MainWindowRef}}, Acc) ->   ?CLASS(MainWindowT,wxWindow),[<<1:32/?UI,MainWindowRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxLayoutAlgorithm_LayoutWindow,
  <<ThisRef:32/?UI,FrameRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxLayoutAlgorithm()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLayoutAlgorithm),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
