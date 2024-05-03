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

-module(wxClipboard).
-moduledoc """
Functions for wxClipboard class

A class for manipulating the clipboard.

To use the clipboard, you call member functions of the global ?wxTheClipboard
object.

See the overview_dataobject for further information.

Call `open/1` to get ownership of the clipboard. If this operation returns true,
you now own the clipboard. Call `setData/2` to put data on the clipboard, or
`getData/2` to retrieve data from the clipboard. Call `close/1` to close the
clipboard and relinquish ownership. You should keep the clipboard open only
momentarily.

For example:

Note: On GTK, the clipboard behavior can vary depending on the configuration of
the end-user's machine. In order for the clipboard data to persist after the
window closes, a clipboard manager must be installed. Some clipboard managers
will automatically flush the clipboard after each new piece of data is added,
while others will not. The @Flush() function will force the clipboard manager to
flush the data.

See:
[Overview dnd](https://docs.wxwidgets.org/3.1/overview_dnd.html#overview_dnd),
[Overview dataobject](https://docs.wxwidgets.org/3.1/overview_dataobject.html#overview_dataobject),
`m:wxDataObject`

wxWidgets docs:
[wxClipboard](https://docs.wxwidgets.org/3.1/classwx_clipboard.html)
""".
-include("wxe.hrl").
-export([addData/2,clear/1,close/1,destroy/1,flush/1,get/0,getData/2,isOpened/1,
  isSupported/2,new/0,open/1,setData/2,usePrimarySelection/1,usePrimarySelection/2]).

%% inherited exports
-export([parent_class/1]).

-type wxClipboard() :: wx:wx_object().
-export_type([wxClipboard/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardwxclipboard">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxClipboard().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxClipboard_new),
  wxe_util:rec(?wxClipboard_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardadddata">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardclear">external documentation</a>.
-doc "Clears the global clipboard object and the system's clipboard if possible.".
-spec clear(This) -> 'ok' when
	This::wxClipboard().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardclose">external documentation</a>.
-doc "Call this function to close the clipboard, having opened it with `open/1`.".
-spec close(This) -> 'ok' when
	This::wxClipboard().
close(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Close).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardflush">external documentation</a>.
-doc """
Flushes the clipboard: this means that the data which is currently on clipboard
will stay available even after the application exits (possibly eating memory),
otherwise the clipboard will be emptied on exit.

Currently this method is implemented in MSW and GTK and always returns false
otherwise.

Note: On GTK, only the non-primary selection can be flushed. Calling this
function when the clipboard is using the primary selection will return false and
not make any data available after the program exits.

Return: false if the operation is unsuccessful for any reason.
""".
-spec flush(This) -> boolean() when
	This::wxClipboard().
flush(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Flush),
  wxe_util:rec(?wxClipboard_Flush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardgetdata">external documentation</a>.
-doc """
Call this function to fill `data` with data on the clipboard, if available in
the required format.

Returns true on success.
""".
-spec getData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
getData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_GetData),
  wxe_util:rec(?wxClipboard_GetData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardisopened">external documentation</a>.
-doc "Returns true if the clipboard has been opened.".
-spec isOpened(This) -> boolean() when
	This::wxClipboard().
isOpened(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_IsOpened),
  wxe_util:rec(?wxClipboard_IsOpened).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardopen">external documentation</a>.
-doc """
Call this function to open the clipboard before calling `setData/2` and
`getData/2`.

Call `close/1` when you have finished with the clipboard. You should keep the
clipboard open for only a very short time.

Return: true on success. This should be tested (as in the sample shown above).
""".
-spec open(This) -> boolean() when
	This::wxClipboard().
open(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Open),
  wxe_util:rec(?wxClipboard_Open).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardsetdata">external documentation</a>.
-doc """
Call this function to set the data object to the clipboard.

The new data object replaces any previously set one, so if the application wants
to provide clipboard data in several different formats, it must use a composite
data object supporting all of the formats instead of calling this function
several times with different data objects as this would only leave data from the
last one in the clipboard.

After this function has been called, the clipboard owns the data, so do not
delete the data explicitly.
""".
-spec setData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
setData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_SetData),
  wxe_util:rec(?wxClipboard_SetData).

%% @equiv usePrimarySelection(This, [])
-spec usePrimarySelection(This) -> 'ok' when
	This::wxClipboard().

usePrimarySelection(This)
 when is_record(This, wx_ref) ->
  usePrimarySelection(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboarduseprimaryselection">external documentation</a>.
-doc """
On platforms supporting it (all X11-based ports), `m:wxClipboard` uses the
CLIPBOARD X11 selection by default.

When this function is called with true, all subsequent clipboard operations will
use PRIMARY selection until this function is called again with false.

On the other platforms, there is no PRIMARY selection and so all clipboard
operations will fail. This allows implementing the standard X11 handling of the
clipboard which consists in copying data to the CLIPBOARD selection only when
the user explicitly requests it (i.e. by selecting the "Copy" menu command) but
putting the currently selected text into the PRIMARY selection automatically,
without overwriting the normal clipboard contents with the currently selected
text on the other platforms.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardissupported">external documentation</a>.
%%<br /> Format = ?wxDF_INVALID | ?wxDF_TEXT | ?wxDF_BITMAP | ?wxDF_METAFILE | ?wxDF_SYLK | ?wxDF_DIF | ?wxDF_TIFF | ?wxDF_OEMTEXT | ?wxDF_DIB | ?wxDF_PALETTE | ?wxDF_PENDATA | ?wxDF_RIFF | ?wxDF_WAVE | ?wxDF_UNICODETEXT | ?wxDF_ENHMETAFILE | ?wxDF_FILENAME | ?wxDF_LOCALE | ?wxDF_PRIVATE | ?wxDF_HTML | ?wxDF_MAX
-doc """
Returns true if there is data which matches the data format of the given data
object currently `available` on the clipboard.
""".
-spec isSupported(This, Format) -> boolean() when
	This::wxClipboard(), Format::wx:wx_enum().
isSupported(#wx_ref{type=ThisT}=This,Format)
 when is_integer(Format) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,Format,?get_env(),?wxClipboard_IsSupported),
  wxe_util:rec(?wxClipboard_IsSupported).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardget">external documentation</a>.
-doc "Returns the global instance (wxTheClipboard) of the clipboard object.".
-spec get() -> wxClipboard().
get() ->
  wxe_util:queue_cmd(?get_env(), ?wxClipboard_Get),
  wxe_util:rec(?wxClipboard_Get).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxClipboard()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxClipboard),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
