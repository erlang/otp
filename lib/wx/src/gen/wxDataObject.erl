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

-module(wxDataObject).
-moduledoc """
Functions for wxDataObject class

A `m:wxDataObject` represents data that can be copied to or from the clipboard,
or dragged and dropped. The important thing about `m:wxDataObject` is that this
is a 'smart' piece of data unlike 'dumb' data containers such as memory buffers
or files. Being 'smart' here means that the data object itself should know what
data formats it supports and how to render itself in each of its supported
formats.

A supported format, incidentally, is exactly the format in which the data can be
requested from a data object or from which the data object may be set. In the
general case, an object may support different formats on 'input' and 'output',
i.e. it may be able to render itself in a given format but not be created from
data on this format or vice versa. `m:wxDataObject` defines the
`wxDataObject::Direction` (not implemented in wx) enumeration type which
distinguishes between them.

See `wxDataFormat` (not implemented in wx) documentation for more about formats.

Not surprisingly, being 'smart' comes at a price of added complexity. This is
reasonable for the situations when you really need to support multiple formats,
but may be annoying if you only want to do something simple like cut and paste
text.

To provide a solution for both cases, wxWidgets has two predefined classes which
derive from `m:wxDataObject`: `wxDataObjectSimple` (not implemented in wx) and
`wxDataObjectComposite` (not implemented in wx). `wxDataObjectSimple` (not
implemented in wx) is the simplest `m:wxDataObject` possible and only holds data
in a single format (such as HTML or text) and `wxDataObjectComposite` (not
implemented in wx) is the simplest way to implement a `m:wxDataObject` that does
support multiple formats because it achieves this by simply holding several
`wxDataObjectSimple` (not implemented in wx) objects.

So, you have several solutions when you need a `m:wxDataObject` class (and you
need one as soon as you want to transfer data via the clipboard or drag and
drop):

Please note that the easiest way to use drag and drop and the clipboard with
multiple formats is by using `wxDataObjectComposite` (not implemented in wx),
but it is not the most efficient one as each `wxDataObjectSimple` (not
implemented in wx) would contain the whole data in its respective formats. Now
imagine that you want to paste 200 pages of text in your proprietary format, as
well as Word, RTF, HTML, Unicode and plain text to the clipboard and even
today's computers are in trouble. For this case, you will have to derive from
`m:wxDataObject` directly and make it enumerate its formats and provide the data
in the requested format on demand.

Note that neither the GTK+ data transfer mechanisms for clipboard and drag and
drop, nor OLE data transfer, `copies` any data until another application
actually requests the data. This is in contrast to the 'feel' offered to the
user of a program who would normally think that the data resides in the
clipboard after having pressed 'Copy' - in reality it is only declared to be
`available`.

You may also derive your own data object classes from `wxCustomDataObject` (not
implemented in wx) for user-defined types. The format of user-defined data is
given as a mime-type string literal, such as "application/word" or "image/png".
These strings are used as they are under Unix (so far only GTK+) to identify a
format and are translated into their Windows equivalent under Win32 (using the
OLE IDataObject for data exchange to and from the clipboard and for drag and
drop). Note that the format string translation under Windows is not yet
finished.

Each class derived directly from `m:wxDataObject` must override and implement
all of its functions which are pure virtual in the base class. The data objects
which only render their data or only set it (i.e. work in only one direction),
should return 0 from `GetFormatCount()` (not implemented in wx).

See:
[Overview dnd](https://docs.wxwidgets.org/3.1/overview_dnd.html#overview_dnd),
[Examples](https://docs.wxwidgets.org/3.1/page_samples.html#page_samples_dnd),
`m:wxFileDataObject`, `m:wxTextDataObject`, `m:wxBitmapDataObject`,
`wxCustomDataObject` (not implemented in wx), `wxDropTarget` (not implemented in
wx), `wxDropSource` (not implemented in wx), `wxTextDropTarget` (not implemented
in wx), `wxFileDropTarget` (not implemented in wx)

wxWidgets docs:
[wxDataObject](https://docs.wxwidgets.org/3.1/classwx_data_object.html)
""".
-include("wxe.hrl").
-export([]).

%% inherited exports
-export([parent_class/1]).

-type wxDataObject() :: wx:wx_object().
-export_type([wxDataObject/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

