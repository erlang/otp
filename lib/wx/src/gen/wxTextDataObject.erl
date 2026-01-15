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

-module(wxTextDataObject).
-moduledoc """
`m:wxTextDataObject` is a specialization of `wxDataObjectSimple` (not implemented in wx)
for text data.

It can be used without change to paste data into the `m:wxClipboard` or a `wxDropSource`
(not implemented in wx). A user may wish to derive a new class from this class for
providing text on-demand in order to minimize memory consumption when offering data in
several formats, such as plain text and RTF because by default the text is stored in a
string in this class, but it might as well be generated when requested. For this, `getTextLength/1` and `getText/1`
will have to be overridden.

Note that if you already have the text inside a string, you will not achieve any
efficiency gain by overriding these functions because copying wxStrings is already a very
efficient operation (data is not actually copied because wxStrings are reference counted).

See:
* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

* `m:wxDataObject`

* `m:wxFileDataObject`

* `m:wxBitmapDataObject`

This class is derived, and can use functions, from:

* `m:wxDataObject`

wxWidgets docs: [wxTextDataObject](https://docs.wxwidgets.org/3.2/classwx_text_data_object.html)
""".
-include("wxe.hrl").
-export([destroy/1,getText/1,getTextLength/1,new/0,new/1,setText/2]).

%% inherited exports
-export([parent_class/1]).

-type wxTextDataObject() :: wx:wx_object().
-export_type([wxTextDataObject/0]).
-doc false.
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxTextDataObject().

new() ->
  new([]).

-doc """
Constructor, may be used to initialise the text (otherwise `setText/2` should be used
later).
""".
-spec new([Option]) -> wxTextDataObject() when
	Option :: {'text', unicode:chardata()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({text, Text}) ->   Text_UC = unicode:characters_to_binary(Text),{text,Text_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxTextDataObject_new),
  wxe_util:rec(?wxTextDataObject_new).

-doc """
Returns the data size.

By default, returns the size of the text data set in the constructor or using `setText/2`. This can
be overridden to provide text size data on-demand. It is recommended to return the text
length plus 1 for a trailing zero, but this is not strictly required.
""".
-spec getTextLength(This) -> integer() when
	This::wxTextDataObject().
getTextLength(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxTextDataObject_GetTextLength),
  wxe_util:rec(?wxTextDataObject_GetTextLength).

-doc """
Returns the text associated with the data object.

You may wish to override this method when offering data on-demand, but this is not
required by wxWidgets' internals. Use this method to get data in text form from the `m:wxClipboard`.
""".
-spec getText(This) -> unicode:charlist() when
	This::wxTextDataObject().
getText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxTextDataObject_GetText),
  wxe_util:rec(?wxTextDataObject_GetText).

-doc """
Sets the text associated with the data object.

This method is called when the data object receives the data and, by default, copies the
text into the member variable. If you want to process the text on the fly you may wish to
override this function.
""".
-spec setText(This, StrText) -> 'ok' when
	This::wxTextDataObject(), StrText::unicode:chardata().
setText(#wx_ref{type=ThisT}=This,StrText)
 when ?is_chardata(StrText) ->
  ?CLASS(ThisT,wxTextDataObject),
  StrText_UC = unicode:characters_to_binary(StrText),
  wxe_util:queue_cmd(This,StrText_UC,?get_env(),?wxTextDataObject_SetText).

-doc "Destroys the object".
-spec destroy(This::wxTextDataObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextDataObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxTextDataObject_destroy),
  ok.
 %% From wxDataObject
