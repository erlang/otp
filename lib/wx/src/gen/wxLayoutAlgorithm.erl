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

-module(wxLayoutAlgorithm).
-include("wxe.hrl").
-export([destroy/1,layoutFrame/2,layoutFrame/3,layoutMDIFrame/2,layoutMDIFrame/3,
  layoutWindow/2,layoutWindow/3,new/0]).

%% inherited exports
-export([parent_class/1]).

-type wxLayoutAlgorithm() :: wx:wx_object().
-export_type([wxLayoutAlgorithm/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmwxlayoutalgorithm">external documentation</a>.
-spec new() -> wxLayoutAlgorithm().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxLayoutAlgorithm_new),
  wxe_util:rec(?wxLayoutAlgorithm_new).

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
layoutFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxFrame),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT}} = Arg) ->   ?CLASS(MainWindowT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Frame, Opts,?get_env(),?wxLayoutAlgorithm_LayoutFrame),
  wxe_util:rec(?wxLayoutAlgorithm_LayoutFrame).

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
layoutMDIFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(FrameT,wxMDIParentFrame),
  MOpts = fun({rect, {_rectX,_rectY,_rectW,_rectH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Frame, Opts,?get_env(),?wxLayoutAlgorithm_LayoutMDIFrame),
  wxe_util:rec(?wxLayoutAlgorithm_LayoutMDIFrame).

%% @equiv layoutWindow(This,Parent, [])
-spec layoutWindow(This, Parent) -> boolean() when
	This::wxLayoutAlgorithm(), Parent::wxWindow:wxWindow().

layoutWindow(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  layoutWindow(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxlayoutalgorithm.html#wxlayoutalgorithmlayoutwindow">external documentation</a>.
-spec layoutWindow(This, Parent, [Option]) -> boolean() when
	This::wxLayoutAlgorithm(), Parent::wxWindow:wxWindow(),
	Option :: {'mainWindow', wxWindow:wxWindow()}.
layoutWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxLayoutAlgorithm),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({mainWindow, #wx_ref{type=MainWindowT}} = Arg) ->   ?CLASS(MainWindowT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent, Opts,?get_env(),?wxLayoutAlgorithm_LayoutWindow),
  wxe_util:rec(?wxLayoutAlgorithm_LayoutWindow).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxLayoutAlgorithm()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLayoutAlgorithm),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
