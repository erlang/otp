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

-module(wxClipboard).
-moduledoc """
A class for manipulating the clipboard.

To use the clipboard, you call member functions of the global ?wxTheClipboard object.

See the overview_dataobject for further information.

Call `open/1` to get ownership of the clipboard. If this operation returns true, you now own the
clipboard. Call `setData/2` to put data on the clipboard, or `getData/2` to retrieve data from the clipboard.
Call `close/1` to close the clipboard and relinquish ownership. You should keep the clipboard open
only momentarily.

For example:

Note: On GTK, the clipboard behavior can vary depending on the configuration of the
end-user's machine. In order for the clipboard data to persist after the window closes, a
clipboard manager must be installed. Some clipboard managers will automatically flush the
clipboard after each new piece of data is added, while others will not. The @Flush()
function will force the clipboard manager to flush the data.

See:
* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

* [Overview dataobject](https://docs.wxwidgets.org/3.2/overview_dataobject.html#overview_dataobject)

* `m:wxDataObject`

wxWidgets docs: [wxClipboard](https://docs.wxwidgets.org/3.2/classwx_clipboard.html)
""".
-include("wxe.hrl").
-export([addData/2,clear/1,close/1,destroy/1,flush/1,get/0,getData/2,isOpened/1,
  isSupported/2,new/0,open/1,setData/2,usePrimarySelection/1,usePrimarySelection/2]).

%% inherited exports
-export([parent_class/1]).

-type wxClipboard() :: wx:wx_object().
-export_type([wxClipboard/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxClipboard().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxClipboard_new),
  wxe_util:rec(?wxClipboard_new).

-doc """
Call this function to add the data object to the clipboard.

This is an obsolete synonym for `setData/2`.
""".
-spec addData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
addData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_AddData),
  wxe_util:rec(?wxClipboard_AddData).

-doc "Clears the global clipboard object and the system's clipboard if possible.".
-spec clear(This) -> 'ok' when
	This::wxClipboard().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Clear).

-doc "Call this function to close the clipboard, having opened it with `open/1`.".
-spec close(This) -> 'ok' when
	This::wxClipboard().
close(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Close).

-doc """
Flushes the clipboard: this means that the data which is currently on clipboard will stay
available even after the application exits (possibly eating memory), otherwise the
clipboard will be emptied on exit.

Currently this method is implemented in MSW and GTK and always returns false otherwise.

Note: On GTK, only the non-primary selection can be flushed. Calling this function when
the clipboard is using the primary selection will return false and not make any data
available after the program exits.

Return: false if the operation is unsuccessful for any reason.
""".
-spec flush(This) -> boolean() when
	This::wxClipboard().
flush(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Flush),
  wxe_util:rec(?wxClipboard_Flush).

-doc """
Call this function to fill `data` with data on the clipboard, if available in the
required format.

Returns true on success.
""".
-spec getData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
getData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_GetData),
  wxe_util:rec(?wxClipboard_GetData).

-doc "Returns true if the clipboard has been opened.".
-spec isOpened(This) -> boolean() when
	This::wxClipboard().
isOpened(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_IsOpened),
  wxe_util:rec(?wxClipboard_IsOpened).

-doc """
Call this function to open the clipboard before calling `setData/2` and `getData/2`.

Call `close/1` when you have finished with the clipboard. You should keep the clipboard open for
only a very short time.

Return: true on success. This should be tested (as in the sample shown above).
""".
-spec open(This) -> boolean() when
	This::wxClipboard().
open(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Open),
  wxe_util:rec(?wxClipboard_Open).

-doc """
Call this function to set the data object to the clipboard.

The new data object replaces any previously set one, so if the application wants to
provide clipboard data in several different formats, it must use a composite data object
supporting all of the formats instead of calling this function several times with
different data objects as this would only leave data from the last one in the clipboard.

After this function has been called, the clipboard owns the data, so do not delete the
data explicitly.
""".
-spec setData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
setData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_SetData),
  wxe_util:rec(?wxClipboard_SetData).

-doc(#{equiv => usePrimarySelection(This, [])}).
-spec usePrimarySelection(This) -> 'ok' when
	This::wxClipboard().

usePrimarySelection(This)
 when is_record(This, wx_ref) ->
  usePrimarySelection(This, []).

-doc """
On platforms supporting it (all X11-based ports), `m:wxClipboard` uses the CLIPBOARD X11
selection by default.

When this function is called with true, all subsequent clipboard operations will use
PRIMARY selection until this function is called again with false.

On the other platforms, there is no PRIMARY selection and so all clipboard operations
will fail. This allows implementing the standard X11 handling of the clipboard which
consists in copying data to the CLIPBOARD selection only when the user explicitly requests
it (i.e. by selecting the "Copy" menu command) but putting the currently selected text
into the PRIMARY selection automatically, without overwriting the normal clipboard
contents with the currently selected text on the other platforms.
""".
-spec usePrimarySelection(This, [Option]) -> 'ok' when
	This::wxClipboard(),
	Option :: {'primary', boolean()}.
usePrimarySelection(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxClipboard),
  MOpts = fun({primary, _primary} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxClipboard_UsePrimarySelection).

-doc """
Returns true if there is data which matches the data format of the given data object
currently `available` on the clipboard.
""".
%%  Format = ?wxDF_INVALID | ?wxDF_TEXT | ?wxDF_BITMAP | ?wxDF_METAFILE | ?wxDF_SYLK | ?wxDF_DIF | ?wxDF_TIFF | ?wxDF_OEMTEXT | ?wxDF_DIB | ?wxDF_PALETTE | ?wxDF_PENDATA | ?wxDF_RIFF | ?wxDF_WAVE | ?wxDF_UNICODETEXT | ?wxDF_ENHMETAFILE | ?wxDF_FILENAME | ?wxDF_LOCALE | ?wxDF_PRIVATE | ?wxDF_HTML | ?wxDF_MAX
-spec isSupported(This, Format) -> boolean() when
	This::wxClipboard(), Format::wx:wx_enum().
isSupported(#wx_ref{type=ThisT}=This,Format)
 when is_integer(Format) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,Format,?get_env(),?wxClipboard_IsSupported),
  wxe_util:rec(?wxClipboard_IsSupported).

-doc "Returns the global instance (wxTheClipboard) of the clipboard object.".
-spec get() -> wxClipboard().
get() ->
  wxe_util:queue_cmd(?get_env(), ?wxClipboard_Get),
  wxe_util:rec(?wxClipboard_Get).

-doc "Destroys the object".
-spec destroy(This::wxClipboard()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxClipboard),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
