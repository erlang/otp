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

-module(wxGraphicsFont).
-moduledoc """
Functions for wxGraphicsFont class

A `m:wxGraphicsFont` is a native representation of a font. The contents are
specific and private to the respective renderer. Instances are ref counted and
can therefore be assigned as usual. The only way to get a valid instance is via
`wxGraphicsContext:createFont/4` or `wxGraphicsRenderer:createFont/4`.

This class is derived (and can use functions) from: `m:wxGraphicsObject`

wxWidgets docs:
[wxGraphicsFont](https://docs.wxwidgets.org/3.1/classwx_graphics_font.html)
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-type wxGraphicsFont() :: wx:wx_object().
-export_type([wxGraphicsFont/0]).
%% @hidden
-doc false.
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

 %% From wxGraphicsObject
%% @hidden
-doc false.
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
-doc false.
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
