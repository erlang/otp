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

-module(wxClipboard).
-include("wxe.hrl").
-export([addData/2,clear/1,close/1,destroy/1,flush/1,get/0,getData/2,isOpened/1,
  isSupported/2,new/0,open/1,setData/2,usePrimarySelection/1,usePrimarySelection/2]).

%% inherited exports
-export([parent_class/1]).

-type wxClipboard() :: wx:wx_object().
-export_type([wxClipboard/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardwxclipboard">external documentation</a>.
-spec new() -> wxClipboard().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxClipboard_new),
  wxe_util:rec(?wxClipboard_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardadddata">external documentation</a>.
-spec addData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
addData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_AddData),
  wxe_util:rec(?wxClipboard_AddData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxClipboard().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardclose">external documentation</a>.
-spec close(This) -> 'ok' when
	This::wxClipboard().
close(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Close).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardflush">external documentation</a>.
-spec flush(This) -> boolean() when
	This::wxClipboard().
flush(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Flush),
  wxe_util:rec(?wxClipboard_Flush).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardgetdata">external documentation</a>.
-spec getData(This, Data) -> boolean() when
	This::wxClipboard(), Data::wxDataObject:wxDataObject().
getData(#wx_ref{type=ThisT}=This,#wx_ref{type=DataT}=Data) ->
  ?CLASS(ThisT,wxClipboard),
  ?CLASS(DataT,wxDataObject),
  wxe_util:queue_cmd(This,Data,?get_env(),?wxClipboard_GetData),
  wxe_util:rec(?wxClipboard_GetData).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardisopened">external documentation</a>.
-spec isOpened(This) -> boolean() when
	This::wxClipboard().
isOpened(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_IsOpened),
  wxe_util:rec(?wxClipboard_IsOpened).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardopen">external documentation</a>.
-spec open(This) -> boolean() when
	This::wxClipboard().
open(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,?get_env(),?wxClipboard_Open),
  wxe_util:rec(?wxClipboard_Open).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardsetdata">external documentation</a>.
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
-spec isSupported(This, Format) -> boolean() when
	This::wxClipboard(), Format::wx:wx_enum().
isSupported(#wx_ref{type=ThisT}=This,Format)
 when is_integer(Format) ->
  ?CLASS(ThisT,wxClipboard),
  wxe_util:queue_cmd(This,Format,?get_env(),?wxClipboard_IsSupported),
  wxe_util:rec(?wxClipboard_IsSupported).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxclipboard.html#wxclipboardget">external documentation</a>.
-spec get() -> wxClipboard().
get() ->
  wxe_util:queue_cmd(?get_env(), ?wxClipboard_Get),
  wxe_util:rec(?wxClipboard_Get).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxClipboard()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxClipboard),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
