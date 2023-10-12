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

-module(wxDCOverlay).
-include("wxe.hrl").
-export([clear/1,destroy/1,new/2,new/6]).

%% inherited exports
-export([parent_class/1]).

-type wxDCOverlay() :: wx:wx_object().
-export_type([wxDCOverlay/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html#wxdcoverlaywxdcoverlay">external documentation</a>.
-spec new(Overlay, Dc) -> wxDCOverlay() when
	Overlay::wxOverlay:wxOverlay(), Dc::wxDC:wxDC().
new(#wx_ref{type=OverlayT}=Overlay,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(OverlayT,wxOverlay),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(Overlay,Dc,?get_env(),?wxDCOverlay_new_2),
  wxe_util:rec(?wxDCOverlay_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html#wxdcoverlaywxdcoverlay">external documentation</a>.
-spec new(Overlay, Dc, X, Y, Width, Height) -> wxDCOverlay() when
	Overlay::wxOverlay:wxOverlay(), Dc::wxDC:wxDC(), X::integer(), Y::integer(), Width::integer(), Height::integer().
new(#wx_ref{type=OverlayT}=Overlay,#wx_ref{type=DcT}=Dc,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(OverlayT,wxOverlay),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(Overlay,Dc,X,Y,Width,Height,?get_env(),?wxDCOverlay_new_6),
  wxe_util:rec(?wxDCOverlay_new_6).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html#wxdcoverlayclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxDCOverlay().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDCOverlay),
  wxe_util:queue_cmd(This,?get_env(),?wxDCOverlay_Clear).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxDCOverlay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDCOverlay),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxDCOverlay_destruct),
  ok.
