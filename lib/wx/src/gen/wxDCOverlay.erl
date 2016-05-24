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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html">wxDCOverlay</a>.
%% @type wxDCOverlay().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxDCOverlay).
-include("wxe.hrl").
-export([clear/1,destroy/1,new/2,new/6]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxDCOverlay/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxDCOverlay() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html#wxdcoverlaywxdcoverlay">external documentation</a>.
-spec new(Overlay, Dc) -> wxDCOverlay() when
	Overlay::wxOverlay:wxOverlay(), Dc::wxWindowDC:wxWindowDC().
new(#wx_ref{type=OverlayT,ref=OverlayRef},#wx_ref{type=DcT,ref=DcRef}) ->
  ?CLASS(OverlayT,wxOverlay),
  ?CLASS(DcT,wxWindowDC),
  wxe_util:construct(?wxDCOverlay_new_2,
  <<OverlayRef:32/?UI,DcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html#wxdcoverlaywxdcoverlay">external documentation</a>.
-spec new(Overlay, Dc, X, Y, Width, Height) -> wxDCOverlay() when
	Overlay::wxOverlay:wxOverlay(), Dc::wxWindowDC:wxWindowDC(), X::integer(), Y::integer(), Width::integer(), Height::integer().
new(#wx_ref{type=OverlayT,ref=OverlayRef},#wx_ref{type=DcT,ref=DcRef},X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(OverlayT,wxOverlay),
  ?CLASS(DcT,wxWindowDC),
  wxe_util:construct(?wxDCOverlay_new_6,
  <<OverlayRef:32/?UI,DcRef:32/?UI,X:32/?UI,Y:32/?UI,Width:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdcoverlay.html#wxdcoverlayclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxDCOverlay().
clear(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxDCOverlay),
  wxe_util:cast(?wxDCOverlay_Clear,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxDCOverlay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDCOverlay),
  wxe_util:destroy(?wxDCOverlay_destruct,Obj),
  ok.
