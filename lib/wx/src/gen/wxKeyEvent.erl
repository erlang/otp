%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxKeyEvent).
-moduledoc """
This event class contains information about key press and release events.

The main information carried by this event is the key being pressed or released. It can
be accessed using either `getKeyCode/1` function or `getUnicodeKey/1`. For the printable characters, the latter should be
used as it works for any keys, including non-Latin-1 characters that can be entered when
using national keyboard layouts. `getKeyCode/1` should be used to handle special characters (such as
cursor arrows keys or `HOME` or `INS` and so on) which correspond to ?wxKeyCode enum
elements above the `WXK_START` constant. While `getKeyCode/1` also returns the character code for
Latin-1 keys for compatibility, it doesn't work for Unicode characters in general and will
return `WXK_NONE` for any non-Latin-1 ones. For this reason, it's recommended to always
use `getUnicodeKey/1` and only fall back to `getKeyCode/1` if `getUnicodeKey/1` returned `WXK_NONE` meaning that the event corresponds to
a non-printable special keys.

While both of these functions can be used with the events of `wxEVT_KEY_DOWN`, `wxEVT_KEY_UP`
and `wxEVT_CHAR` types, the values returned by them are different for the first two
events and the last one. For the latter, the key returned corresponds to the character
that would appear in e.g. a text zone if the user pressed the key in it. As such, its
value depends on the current state of the Shift key and, for the letters, on the state of
Caps Lock modifier. For example, if `A` key is pressed without Shift being held down, `m:wxKeyEvent`
of type `wxEVT_CHAR` generated for this key press will return (from either `getKeyCode/1` or `getUnicodeKey/1` as their
meanings coincide for ASCII characters) key code of 97 corresponding the ASCII value of `a`.
And if the same key is pressed but with Shift being held (or Caps Lock being active), then
the key could would be 65, i.e. ASCII value of capital `A`.

However for the key down and up events the returned key code will instead be `A`
independently of the state of the modifier keys i.e. it depends only on physical key being
pressed and is not translated to its logical representation using the current keyboard
state. Such untranslated key codes are defined as follows:

* For the letters they correspond to the `upper` case value of the letter.

* For the other alphanumeric keys (e.g. `7` or `+`), the untranslated key code corresponds to
the character produced by the key when it is pressed without Shift. E.g. in standard US
keyboard layout the untranslated key code for the key `=/+` in the upper right corner of
the keyboard is 61 which is the ASCII value of `=`.

* For the rest of the keys (i.e. special non-printable keys) it is the same as the normal
key code as no translation is used anyhow.

Notice that the first rule applies to all Unicode letters, not just the usual Latin-1
ones. However for non-Latin-1 letters only `getUnicodeKey/1` can be used to retrieve the key code as `getKeyCode/1` just
returns `WXK_NONE` in this case.

To summarize: you should handle `wxEVT_CHAR` if you need the translated key and `wxEVT_KEY_DOWN`
if you only need the value of the key itself, independent of the current keyboard state.

Note: Not all key down events may be generated by the user. As an example, `wxEVT_KEY_DOWN`
with `=` key code can be generated using the standard US keyboard layout but not using
the German one because the `=` key corresponds to Shift-0 key combination in this layout
and the key code for it is `0`, not `=`. Because of this you should avoid requiring your
users to type key events that might be impossible to enter on their keyboard.

Another difference between key and char events is that another kind of translation is
done for the latter ones when the Control key is pressed: char events for ASCII letters in
this case carry codes corresponding to the ASCII value of Ctrl-Latter, i.e. 1 for Ctrl-A,
2 for Ctrl-B and so on until 26 for Ctrl-Z. This is convenient for terminal-like
applications and can be completely ignored by all the other ones (if you need to handle
Ctrl-A it is probably a better idea to use the key event rather than the char one). Notice
that currently no translation is done for the presses of [, `\`, ], `^` and `_` keys which
might be mapped to ASCII values from 27 to 31. Since version 2.9.2, the enum values `WXK_CONTROL_A`
- `WXK_CONTROL_Z` can be used instead of the non-descriptive constant values 1-26.

Finally, modifier keys only generate key events but no char events at all. The modifiers
keys are `WXK_SHIFT`, `WXK_CONTROL`, `WXK_ALT` and various `WXK_WINDOWS_XXX` from
?wxKeyCode enum.

Modifier keys events are special in one additional aspect: usually the keyboard state
associated with a key press is well defined, e.g. `shiftDown/1` returns `true` only if the Shift key
was held pressed when the key that generated this event itself was pressed. There is an
ambiguity for the key press events for Shift key itself however. By convention, it is
considered to be already pressed when it is pressed and already released when it is
released. In other words, `wxEVT_KEY_DOWN` event for the Shift key itself will have `wxMOD_SHIFT`
in `getModifiers/1` and `shiftDown/1` will return true while the `wxEVT_KEY_UP` event for Shift itself will not have `wxMOD_SHIFT`
in its modifiers and `shiftDown/1` will return false.

`Tip:` You may discover the key codes and modifiers generated by all the keys on your
system interactively by running the page_samples_keyboard wxWidgets sample and pressing
some keys in it.

Note: If a key down (`EVT_KEY_DOWN`) event is caught and the event handler does not call `event.Skip()`
then the corresponding char event (`EVT_CHAR`) will not happen. This is by design and
enables the programs that handle both types of events to avoid processing the same key
twice. As a consequence, if you do not want to suppress the `wxEVT_CHAR` events for the
keys you handle, always call `event.Skip()` in your `wxEVT_KEY_DOWN` handler. Not doing
may also prevent accelerators defined using this key from working.

Note: If a key is maintained in a pressed state, you will typically get a lot of
(automatically generated) key down events but only one key up one at the end when the key
is released so it is wrong to assume that there is one up event corresponding to each down one.

Note: For Windows programmers: The key and char events in wxWidgets are similar to but
slightly different from Windows `WM_KEYDOWN` and `WM_CHAR` events. In particular, Alt-x
combination will generate a char event in wxWidgets (unless it is used as an accelerator)
and almost all keys, including ones without ASCII equivalents, generate char events too.

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxKeyEvent](https://docs.wxwidgets.org/3.2/classwx_key_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxKeyEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([altDown/1,cmdDown/1,controlDown/1,getKeyCode/1,getModifiers/1,getPosition/1,
  getRawKeyCode/1,getRawKeyFlags/1,getUnicodeKey/1,getX/1,getY/1,hasModifiers/1,
  metaDown/1,shiftDown/1]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxKeyEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxKeyEventType() :: 'char' | 'char_hook' | 'key_down' | 'key_up'.
-export_type([wxKeyEvent/0, wxKey/0, wxKeyEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns true if the Alt key is pressed.

Notice that `getModifiers/1` should usually be used instead of this one.
""".
-spec altDown(This) -> boolean() when
	This::wxKeyEvent().
altDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_AltDown),
  wxe_util:rec(?wxKeyEvent_AltDown).

-doc """
Returns true if the key used for command accelerators is pressed.

Same as `controlDown/1`. Deprecated.

Notice that `getModifiers/1` should usually be used instead of this one.
""".
-spec cmdDown(This) -> boolean() when
	This::wxKeyEvent().
cmdDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_CmdDown),
  wxe_util:rec(?wxKeyEvent_CmdDown).

-doc """
Returns true if the Control key or Apple/Command key under macOS is pressed.

This function doesn't distinguish between right and left control keys.

Notice that `getModifiers/1` should usually be used instead of this one.
""".
-spec controlDown(This) -> boolean() when
	This::wxKeyEvent().
controlDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_ControlDown),
  wxe_util:rec(?wxKeyEvent_ControlDown).

-doc """
Returns the key code of the key that generated this event.

ASCII symbols return normal ASCII values, while events from special keys such as "left
cursor arrow" (`WXK_LEFT`) return values outside of the ASCII range. See ?wxKeyCode for a
full list of the virtual key codes.

Note that this method returns a meaningful value only for special non-alphanumeric keys
or if the user entered a Latin-1 character (this includes ASCII and the accented letters
found in Western European languages but not letters of other alphabets such as e.g.
Cyrillic). Otherwise it simply method returns `WXK_NONE` and `getUnicodeKey/1` should be used to obtain the
corresponding Unicode character.

Using `getUnicodeKey/1` is in general the right thing to do if you are interested in the characters typed
by the user, `getKeyCode/1` should be only used for special keys (for which `getUnicodeKey/1` returns `WXK_NONE`). To
handle both kinds of keys you might write:
""".
-spec getKeyCode(This) -> integer() when
	This::wxKeyEvent().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetKeyCode),
  wxe_util:rec(?wxKeyEvent_GetKeyCode).

-doc """
Return the bit mask of all pressed modifier keys.

The return value is a combination of `wxMOD_ALT`, `wxMOD_CONTROL`, `wxMOD_SHIFT` and `wxMOD_META`
bit masks. Additionally, `wxMOD_NONE` is defined as 0, i.e. corresponds to no modifiers
(see `HasAnyModifiers()` (not implemented in wx)) and `wxMOD_CMD` is either `wxMOD_CONTROL`
(MSW and Unix) or `wxMOD_META` (Mac), see `cmdDown/1`. See ?wxKeyModifier for the full list of modifiers.

Notice that this function is easier to use correctly than, for example, `controlDown/1` because when
using the latter you also have to remember to test that none of the other modifiers is pressed:

and forgetting to do it can result in serious program bugs (e.g. program not working with
European keyboard layout where `AltGr` key which is seen by the program as combination of
CTRL and ALT is used). On the other hand, you can simply write:

with this function.
""".
-spec getModifiers(This) -> integer() when
	This::wxKeyEvent().
getModifiers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetModifiers),
  wxe_util:rec(?wxKeyEvent_GetModifiers).

-doc """
Obtains the position (in client coordinates) at which the key was pressed.

Notice that under most platforms this position is simply the current mouse pointer
position and has no special relationship to the key event itself.

`x` and `y` may be NULL if the corresponding coordinate is not needed.
""".
-spec getPosition(This) -> {X::integer(), Y::integer()} when
	This::wxKeyEvent().
getPosition(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetPosition),
  wxe_util:rec(?wxKeyEvent_GetPosition).

-doc """
Returns the raw key code for this event.

The flags are platform-dependent and should only be used if the functionality provided by
other `m:wxKeyEvent` methods is insufficient.

Under MSW, the raw key code is the value of `wParam` parameter of the corresponding message.

Under GTK, the raw key code is the `keyval` field of the corresponding GDK event.

Under macOS, the raw key code is the `keyCode` field of the corresponding NSEvent.

Note: Currently the raw key codes are not supported by all ports, use #ifdef
wxHAS_RAW_KEY_CODES to determine if this feature is available.
""".
-spec getRawKeyCode(This) -> integer() when
	This::wxKeyEvent().
getRawKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetRawKeyCode),
  wxe_util:rec(?wxKeyEvent_GetRawKeyCode).

-doc """
Returns the low level key flags for this event.

The flags are platform-dependent and should only be used if the functionality provided by
other `m:wxKeyEvent` methods is insufficient.

Under MSW, the raw flags are just the value of `lParam` parameter of the corresponding message.

Under GTK, the raw flags contain the `hardware_keycode` field of the corresponding GDK event.

Under macOS, the raw flags contain the modifiers state.

Note: Currently the raw key flags are not supported by all ports, use #ifdef
wxHAS_RAW_KEY_CODES to determine if this feature is available.
""".
-spec getRawKeyFlags(This) -> integer() when
	This::wxKeyEvent().
getRawKeyFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetRawKeyFlags),
  wxe_util:rec(?wxKeyEvent_GetRawKeyFlags).

-doc """
Returns the Unicode character corresponding to this key event.

If the key pressed doesn't have any character value (e.g. a cursor key) this method will
return `WXK_NONE`. In this case you should use `getKeyCode/1` to retrieve the value of the key.

This function is only available in Unicode build, i.e. when `wxUSE_UNICODE` is 1.
""".
-spec getUnicodeKey(This) -> integer() when
	This::wxKeyEvent().
getUnicodeKey(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetUnicodeKey),
  wxe_util:rec(?wxKeyEvent_GetUnicodeKey).

-doc """
Returns the X position (in client coordinates) of the event.

See: `getPosition/1`
""".
-spec getX(This) -> integer() when
	This::wxKeyEvent().
getX(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetX),
  wxe_util:rec(?wxKeyEvent_GetX).

-doc """
Returns the Y position (in client coordinates) of the event.

See: `getPosition/1`
""".
-spec getY(This) -> integer() when
	This::wxKeyEvent().
getY(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_GetY),
  wxe_util:rec(?wxKeyEvent_GetY).

-doc """
Returns true if Control or Alt are pressed.

Checks if Control, Alt or, under macOS only, Command key are pressed (notice that the
real Control key is still taken into account under OS X too).

This method returns false if only Shift is pressed for compatibility reasons and also
because pressing Shift usually doesn't change the interpretation of key events, see `HasAnyModifiers()`
(not implemented in wx) if you want to take Shift into account as well.
""".
-spec hasModifiers(This) -> boolean() when
	This::wxKeyEvent().
hasModifiers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_HasModifiers),
  wxe_util:rec(?wxKeyEvent_HasModifiers).

-doc """
Returns true if the Meta/Windows/Apple key is pressed.

This function tests the state of the key traditionally called Meta under Unix systems,
Windows keys under MSW Notice that `getModifiers/1` should usually be used instead of this one.

See: `cmdDown/1`
""".
-spec metaDown(This) -> boolean() when
	This::wxKeyEvent().
metaDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_MetaDown),
  wxe_util:rec(?wxKeyEvent_MetaDown).

-doc """
Returns true if the Shift key is pressed.

This function doesn't distinguish between right and left shift keys.

Notice that `getModifiers/1` should usually be used instead of this one.
""".
-spec shiftDown(This) -> boolean() when
	This::wxKeyEvent().
shiftDown(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxKeyEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxKeyEvent_ShiftDown),
  wxe_util:rec(?wxKeyEvent_ShiftDown).

 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
