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

-module(wxSystemOptions).
-moduledoc """
`m:wxSystemOptions` stores option/value pairs that wxWidgets itself or applications can
use to alter behaviour at run-time.

It can be used to optimize behaviour that doesn't deserve a distinct API, but is still
important to be able to configure.

System options can be set by the program itself using `setOption/2` method and they also can be set
from the program environment by defining an environment variable `wx_option` to set the
given option for all wxWidgets applications or `wx_appname_option` to set it just for the
application with the given name (as returned by `wxApp::GetAppName()` (not implemented in
wx)). Notice that any characters not allowed in the environment variables names, such as
periods and dashes, should be replaced with underscores. E.g. to define a system option
"foo-bar" you need to define the environment variable "wx_foo_bar".

The program may use system options for its own needs but they are mostly used to control
the behaviour of wxWidgets library itself.

These options are currently recognised by wxWidgets:

All platforms

* exit-on-assert: If set to non-zero value, abort the program if an assertion fails. The
default behaviour in case of assertion failure depends on the build mode and can be
changed by overriding `wxApp::OnAssertFailure()` (not implemented in wx) but setting this
option allows changing it without modifying the program code and also applies to asserts
which may happen before the `wxApp` (not implemented in wx) object creation or after its
destruction.

Windows

* no-maskblt: 1 to never use WIN32's MaskBlt function, 0 to allow it to be used where
possible. Default: 0. In some circumstances the MaskBlt function can be slower than using
the fallback code, especially if using DC caching. By default, MaskBlt will be used where
it is implemented by the operating system and driver.

* msw.remap: If 1 (the default), `m:wxToolBar` bitmap colours will be remapped to the
current theme's values. Set this to 0 to disable this functionality, for example if you're
using more than 16 colours in your tool bitmaps.

* msw.window.no-clip-children: If 1, windows will not automatically get the WS_CLIPCHILDREN
style. This restores the way windows are refreshed back to the method used in versions of
wxWidgets earlier than 2.5.4, and for some complex window hierarchies it can reduce
apparent refresh delays. You may still specify wxCLIP_CHILDREN for individual windows.

* msw.notebook.themed-background: If set to 0, globally disables themed backgrounds on
notebook pages. Note that this won't disable the theme on the actual notebook background
(noticeable only if there are no pages).

* msw.staticbox.optimized-paint: If set to 0, switches off optimized `m:wxStaticBox`
painting. Setting this to 0 causes more flicker, but allows applications to paint graphics
on the parent of a static box (the optimized refresh causes any such drawing to
disappear).

* msw.font.no-proof-quality: If set to 1, use default fonts quality instead of proof
quality when creating fonts. With proof quality the fonts have slightly better appearance
but not all fonts are available in this quality, e.g. the Terminal font in small sizes is
not and this option may be used if wider fonts selection is more important than higher
quality.

GTK+

* gtk.tlw.can-set-transparent: `wxTopLevelWindow::CanSetTransparent()` (not implemented in
wx) method normally tries to detect automatically whether transparency for top level
windows is currently supported, however this may sometimes fail and this option allows
overriding the automatic detection. Setting it to 1 makes the transparency be always
available (setting it can still fail, of course) and setting it to 0 makes it always
unavailable.

* gtk.desktop: This option can be set to override the default desktop environment
determination. Supported values are GNOME and KDE.

* gtk.window.force-background-colour: If 1, the backgrounds of windows with the
wxBG_STYLE_COLOUR background style are cleared forcibly instead of relying on the
underlying GTK+ window colour. This works around a display problem when running
applications under KDE with the gtk-qt theme installed (0.6 and below).

Mac

* mac.window-plain-transition: If 1, uses a plainer transition when showing a window. You
can also use the symbol wxMAC_WINDOW_PLAIN_TRANSITION.

* window-default-variant: The default variant used by windows (cast to integer from the
wxWindowVariant enum). Also known as wxWINDOW_DEFAULT_VARIANT.

* mac.listctrl.always_use_generic: Tells `m:wxListCtrl` to use the generic control even
when it is capable of using the native control instead. Also known as
wxMAC_ALWAYS_USE_GENERIC_LISTCTRL.

* mac.textcontrol-use-spell-checker: If 1 activates the spell checking in `m:wxTextCtrl`.

* osx.openfiledialog.always-show-types: Per default a `m:wxFileDialog` with wxFD_OPEN does
not show a types-popup on macOS but allows the selection of files from any of the
supported types. Setting this to 1 shows a `m:wxChoice` for selection (if there is more
than one supported filetype).

Motif

* motif.largebuttons: If 1, uses a bigger default size for wxButtons.

The compile-time option to include or exclude this functionality is wxUSE_SYSTEM_OPTIONS.

See: `m:wxSystemSettings`

wxWidgets docs: [wxSystemOptions](https://docs.wxwidgets.org/3.2/classwx_system_options.html)
""".
-include("wxe.hrl").
-export([getOption/1,getOptionInt/1,hasOption/1,isFalse/1,setOption/2]).

%% inherited exports
-export([parent_class/1]).

-type wxSystemOptions() :: wx:wx_object().
-export_type([wxSystemOptions/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Gets an option.

The function is case-insensitive to `name`. Returns empty string if the option hasn't
been set.

See:
* `setOption/2`

* `getOptionInt/1`

* `hasOption/1`
""".
-spec getOption(Name) -> unicode:charlist() when
	Name::unicode:chardata().
getOption(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_GetOption),
  wxe_util:rec(?wxSystemOptions_GetOption).

-doc """
Gets an option as an integer.

The function is case-insensitive to `name`. If the option hasn't been set, this function
returns 0.

See:
* `setOption/2`

* `getOption/1`

* `hasOption/1`
""".
-spec getOptionInt(Name) -> integer() when
	Name::unicode:chardata().
getOptionInt(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_GetOptionInt),
  wxe_util:rec(?wxSystemOptions_GetOptionInt).

-doc """
Returns true if the given option is present.

The function is case-insensitive to `name`.

See:
* `setOption/2`

* `getOption/1`

* `getOptionInt/1`
""".
-spec hasOption(Name) -> boolean() when
	Name::unicode:chardata().
hasOption(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_HasOption),
  wxe_util:rec(?wxSystemOptions_HasOption).

-doc """
Returns true if the option with the given `name` had been set to 0 value.

This is mostly useful for boolean options for which you can't use `GetOptionInt(name)` ==
0 as this would also be true if the option hadn't been set at all.
""".
-spec isFalse(Name) -> boolean() when
	Name::unicode:chardata().
isFalse(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_IsFalse),
  wxe_util:rec(?wxSystemOptions_IsFalse).

-doc """
Sets an option.

The function is case-insensitive to `name`.
""".
-spec setOption(Name, Value) -> 'ok' when
	Name::unicode:chardata(), Value::integer();
      (Name, Value) -> 'ok' when
	Name::unicode:chardata(), Value::unicode:chardata().
setOption(Name,Value)
 when ?is_chardata(Name),is_integer(Value) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,Value,?get_env(),?wxSystemOptions_SetOption_2_0);
setOption(Name,Value)
 when ?is_chardata(Name),?is_chardata(Value) ->
  Name_UC = unicode:characters_to_binary(Name),
  Value_UC = unicode:characters_to_binary(Value),
  wxe_util:queue_cmd(Name_UC,Value_UC,?get_env(),?wxSystemOptions_SetOption_2_1).

