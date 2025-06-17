%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxArtProvider).
-moduledoc """
`m:wxArtProvider` class is used to customize the look of wxWidgets application.

When wxWidgets needs to display an icon or a bitmap (e.g. in the standard file dialog),
it does not use a hard-coded resource but asks `m:wxArtProvider` for it instead. This way
users can plug in their own `m:wxArtProvider` class and easily replace standard art with
their own version.

All that is needed is to derive a class from `m:wxArtProvider`, override either its `wxArtProvider::CreateBitmap()`
(not implemented in wx) and/or its `wxArtProvider::CreateIconBundle()` (not implemented
in wx) methods and register the provider with `wxArtProvider::Push()` (not implemented in wx):

If you need bitmap images (of the same artwork) that should be displayed at different
sizes you should probably consider overriding `wxArtProvider::CreateIconBundle` (not
implemented in wx) and supplying icon bundles that contain different bitmap sizes.

There's another way of taking advantage of this class: you can use it in your code and
use platform native icons as provided by `getBitmap/2` or `getIcon/2`.

Identifying art resources

Every bitmap and icon bundle are known to `m:wxArtProvider` under an unique ID that is
used when requesting a resource from it. The ID is represented by the ?wxArtID type and
can have one of these predefined values (you can see bitmaps represented by these
constants in the page_samples_artprov):

Additionally, any string recognized by custom art providers registered using `wxArtProvider::Push`
(not implemented in wx) may be used.

Note: When running under GTK+ 2, GTK+ stock item IDs (e.g. `"gtk-cdrom"`) may be used as
well: For a list of the GTK+ stock items please refer to the [GTK+ documentation page](http://library.gnome.org/devel/gtk/stable/gtk-Stock-Items.html).
It is also possible to load icons from the current icon theme by specifying their name
(without extension and directory components). Icon themes recognized by GTK+ follow the
freedesktop.org [Icon Themes specification](http://freedesktop.org/Standards/icon-theme-spec).
Note that themes are not guaranteed to contain all icons, so `m:wxArtProvider` may return
?wxNullBitmap or ?wxNullIcon. The default theme is typically installed in `/usr/share/icons/hicolor`.

Clients

The `client` is the entity that calls `m:wxArtProvider`'s `getBitmap/2` or `getIcon/2` function. It is
represented by wxClientID type and can have one of these values:

* `wxART_TOOLBAR`

* `wxART_MENU`

* `wxART_BUTTON`

* `wxART_FRAME_ICON`

* `wxART_CMN_DIALOG`

* `wxART_HELP_BROWSER`

* `wxART_MESSAGE_BOX`

* `wxART_OTHER` (used for all requests that don't fit into any of the categories above)

Client ID serve as a hint to `m:wxArtProvider` that is supposed to help it to choose the
best looking bitmap. For example it is often desirable to use slightly different icons in
menus and toolbars even though they represent the same action (e.g. wxART_FILE_OPEN).
Remember that this is really only a hint for `m:wxArtProvider` - it is common that `getBitmap/2`
returns identical bitmap for different client values!

See:
* [Examples](https://docs.wxwidgets.org/3.2/page_samples.html#page_samples_artprov)

* `m:wxArtProvider`

wxWidgets docs: [wxArtProvider](https://docs.wxwidgets.org/3.2/classwx_art_provider.html)
""".
-include("wxe.hrl").
-export([getBitmap/1,getBitmap/2,getIcon/1,getIcon/2]).

%% inherited exports
-export([parent_class/1]).

-type wxArtProvider() :: wx:wx_object().
-export_type([wxArtProvider/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => getBitmap(Id, [])}).
-spec getBitmap(Id) -> wxBitmap:wxBitmap() when
	Id::unicode:chardata().

getBitmap(Id)
 when ?is_chardata(Id) ->
  getBitmap(Id, []).

-doc """
Query registered providers for bitmap with given ID.

Return: The bitmap if one of registered providers recognizes the ID or wxNullBitmap
otherwise.
""".
-spec getBitmap(Id, [Option]) -> wxBitmap:wxBitmap() when
	Id::unicode:chardata(),
	Option :: {'client', unicode:chardata()}
		 | {'size', {W::integer(), H::integer()}}.
getBitmap(Id, Options)
 when ?is_chardata(Id),is_list(Options) ->
  Id_UC = unicode:characters_to_binary(Id),
  MOpts = fun({client, Client}) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C]),{client,Client_UC};
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Id_UC, Opts,?get_env(),?wxArtProvider_GetBitmap),
  wxe_util:rec(?wxArtProvider_GetBitmap).

-doc(#{equiv => getIcon(Id, [])}).
-spec getIcon(Id) -> wxIcon:wxIcon() when
	Id::unicode:chardata().

getIcon(Id)
 when ?is_chardata(Id) ->
  getIcon(Id, []).

-doc "Same as `getBitmap/2`, but return a `m:wxIcon` object (or ?wxNullIcon on failure).".
-spec getIcon(Id, [Option]) -> wxIcon:wxIcon() when
	Id::unicode:chardata(),
	Option :: {'client', unicode:chardata()}
		 | {'size', {W::integer(), H::integer()}}.
getIcon(Id, Options)
 when ?is_chardata(Id),is_list(Options) ->
  Id_UC = unicode:characters_to_binary(Id),
  MOpts = fun({client, Client}) ->   Client_UC = unicode:characters_to_binary([Client, $_, $C]),{client,Client_UC};
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Id_UC, Opts,?get_env(),?wxArtProvider_GetIcon),
  wxe_util:rec(?wxArtProvider_GetIcon).

