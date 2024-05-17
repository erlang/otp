<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# wx the erlang binding of wxWidgets

The _wx_ application is an erlang binding of _wxWidgets_. This document
describes the erlang mapping to wxWidgets and it's implementation. It is not a
complete users guide to wxWidgets. If you need that, you will have to read the
wxWidgets documentation instead. _wx_ tries to keep a one-to-one mapping with
the original API so that the original documentation and examples shall be as
easy as possible to use.

Wx examples and test suite can be found in the erlang src release. They
can also provide some help on how to use the API.

This is currently a very brief introduction to _wx_. The application is still
under development, which means the interface may change, and the test suite
currently have a poor coverage ratio.

## Contents

- [Introduction](chapter.md#introduction)
- [Multiple processes and memory handling](chapter.md#Multiple_processes_and_memory_handling)
- [Event Handling](chapter.md#Event_Handling)
- [Acknowledgments](chapter.md#acknowledgments)

## Introduction

The original _wxWidgets_ is an object-oriented (C++) API and that is reflected
in the erlang mapping. In most cases each class in wxWidgets is represented as a
module in erlang. This gives the _wx_ application a huge interface, spread over
several modules, and it all starts with the _wx_ module. The _wx_ module
contains functions to create and destroy the GUI, i.e. `wx:new/0`,
`wx:destroy/0`, and some other useful functions.

Objects or object references in _wx_ should be seen as erlang processes rather
than erlang terms. When you operate on them they can change state, e.g. they are
not functional objects as erlang terms are. Each object has a type or rather a
class, which is manipulated with the corresponding module or by sub-classes of
that object. Type checking is done so that a module only operates on it's
objects or inherited classes.

An object is created with _new_ and destroyed with _destroy_. Most functions in
the classes are named the same as their C++ counterpart, except that for
convenience, in erlang they start with a lowercase letter and the first argument
is the object reference. Optional arguments are last and expressed as tagged
tuples in any order.

For example the _wxWindow_ C++ class is implemented in the _wxWindow_ erlang
module and the member _wxWindow::CenterOnParent_ is thus
_wxWindow:centerOnParent_. The following C++ code:

```erlang
  wxWindow MyWin = new wxWindow();
  MyWin.CenterOnParent(wxVERTICAL);
  ...
  delete MyWin;
```

would in erlang look like:

```erlang
  MyWin = wxWindow:new(),
  wxWindow:centerOnParent(MyWin, [{dir,?wxVERTICAL}]),
  ...
  wxWindow:destroy(MyWin),
```

When you are reading wxWidgets documentation or the examples, you will notice
that some of the most basic classes are missing in _wx_, they are directly
mapped to corresponding erlang terms:

- **_wxPoint_ is represented by \{Xcoord,Ycoord\}**

- **_wxSize_ is represented by \{Width,Height\}**

- **_wxRect_ is represented by \{Xcoord,Ycoord,Width,Height\}**

- **_wxColour_ is represented by \{Red,Green,Blue\[,Alpha]\}**

- **_wxString_ is represented by
  [unicode:charlist()](`t:unicode:charlist/0`)**

- **_wxGBPosition_ is represented by \{Row,Column\}**

- **_wxGBSpan_ is represented by \{RowSpan,ColumnSPan\}**

- **_wxGridCellCoords_ is represented by \{Row,Column\}**

In the places where the erlang API differs from the original one it should be
obvious from the erlang documentation which representation has been used. E.g.
the C++ arrays and/or lists are sometimes represented as erlang lists and
sometimes as tuples.

Colours are represented with \{Red,Green,Blue\[,Alpha]\}, the Alpha value is
optional when used as an argument to functions, but it will always be returned
from _wx_ functions.

Defines, enumerations and global variables exists in `wx.hrl` as defines. Most
of these defines are constants but not all. Some are platform dependent and
therefore the global variables must be instantiated during runtime. These will
be acquired from the driver with a call, so not all defines can be used in
matching statements. Class local enumerations will be prefixed with the class
name and a underscore as in `ClassName_Enum`.

Additionally some global functions, i.e. non-class functions, exist in the
`wx_misc` module.

_Wx_ is implemented as a (threaded) driver and a rather direct interface
to the C++ API, with the drawback that if the erlang programmer does an error,
it might crash the emulator.

Since the driver is threaded it requires a _smp_ enabled emulator, that provides
a thread safe interface to the driver.

[](){: #Multiple_processes_and_memory_handling }

## Multiple processes and memory handling

The intention is that each erlang application calls wx:new() once to setup it's
GUI which creates an environment and a memory mapping. To be able to use _wx_
from several processes in your application, you must share the environment. You
can get the active environment with `wx:get_env/0` and set it in the new
processes with `wx:set_env/1`. Two processes or applications which have both
called wx:new() will not be able use each others objects.

```erlang
  wx:new(),
  MyWin = wxFrame:new(wx:null(), 42, "Example", []),
  Env = wx:get_env(),
  spawn(fun() ->
           wx:set_env(Env),
           %% Here you can do wx calls from your helper process.
           ...
        end),
  ...
```

When `wx:destroy/0` is invoked or when all processes in the application have
died, the memory is deleted and all windows created by that application are
closed.

The _wx_ application never cleans or garbage collects memory as long as the user
application is alive. Most of the objects are deleted when a window is closed,
or at least all the objects which have a parent argument that is non null. By
using `wxCLASS:destroy/1` when possible you can avoid an increasing memory
usage. This is especially important when _wxWidgets_ assumes or recommends that
you (or rather the C++ programmer) have allocated the object on the stack since
that will never be done in the erlang binding. For example `wxDC` class or its
sub-classes or `wxSizerFlags`.

Currently the dialogs show modal function freezes wxWidgets until the dialog is
closed. That is intended but in erlang where you can have several GUI
applications running at the same time it causes trouble. This will hopefully be
fixed in future _wxWidgets_ releases.

[](){: #Event_Handling }

## Event Handling

Event handling in _wx_ differs most from the original API. You must specify
every event you want to handle in _wxWidgets_, that is the same in the erlang
binding but you can choose to receive the events as messages or handle them with
callback _funs_.

Otherwise the event subscription is handled as _wxWidgets_ dynamic event-handler
connection. You subscribe to events of a certain type from objects with an _ID_
or within a range of *ID*s. The callback _fun_ is optional, if not supplied the
event will be sent to the process that called _connect/2_. Thus, a handler is a
callback _fun_ or a process which will receive an event message.

Events are handled in order from bottom to top, in the widgets hierarchy, by the
last subscribed handler first. Depending on if `wxEvent:skip()` is called the
event will be handled by the other handler(s) afterwards. Most of the events
have default event handler(s) installed.

Message events looks like
[\#wx\{id=integer(), obj=wx:wxObject(), userData=term(), event=Rec](`t:wxEvtHandler:wx/0`)
\}. The _id_ is the identifier of the object that received the event. The _obj_
field contains the object that you used _connect_ on. The _userData_ field
contains a user supplied term, this is an option to _connect_. And the _event_
field contains a record with event type dependent information. The first element
in the event record is always the type you subscribed to. For example if you
subscribed to _key_up_ events you will receive the `#wx{event=Event}` where
_Event_ will be a _wxKey_ event record where `Event#wxKey.type = key_up`.

In _wxWidgets_ the developer has to call `wxEvent:skip()` if he wants the event
to be processed by other handlers. You can do the same in _wx_ if you use
callbacks. If you want the event as messages you just don't supply a callback
and you can set the _skip_ option in _connect_ call to true or false, the
default it is false. True means that you get the message but let the subsequent
handlers also handle the event. If you want to change this behavior dynamically
you must use callbacks and call `wxEvent:skip()`.

Callback event handling is done by using the optional callback _fun/2_ when
attaching the handler. The _fun(#wx\{\},wxObject()_ must take two arguments
where the first is the same as with message events described above and the
second is an object reference to the actual event object. With the event object
you can call `wxEvent:skip()` and access all the data. When using callbacks you
must call `wxEvent:skip()` by yourself if you want any of the events to be
forwarded to the following handlers. The actual event objects are deleted after
the _fun_ returns.

The callbacks are always invoked by another process and have exclusive usage of
the GUI when invoked. This means that a callback _fun_ cannot use the process
dictionary and should not make calls to other processes. Calls to another
process inside a callback _fun_ may cause a deadlock if the other process is
waiting on completion of his call to the GUI.

## Acknowledgments

Mats-Ola Persson wrote the initial _wxWidgets_ binding as part of his master
thesis. The current version is a total re-write but many ideas have been reused.
The reason for the re-write was mostly due to the limited requirements he had
been given by us.

Also thanks to the _wxWidgets_ team that develops and supports it so we have
something to use.
