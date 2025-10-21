%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
%% SPDX-FileCopyrightText: 2024 Erlang/OTP and its contributors
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
%%
%%%-------------------------------------------------------------------
%%% File    : wx.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description :
%%%
%%% Created : 22 Feb 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------


%% @doc A port of <a href="http://www.wxwidgets.org/">wxWidgets</a>.
%%
%% This is the base api of <a href="http://www.wxwidgets.org/">wxWidgets</a>.
%% This module contains functions for
%% starting and stopping the wx-server, as well as  other utility functions.
%%
%% wxWidgets is object oriented, and not functional. Thus, in Wx a
%% module represents a class, and the object created by this class
%% has an own type, wxCLASS().  This module represents the base
%% class, and all other wxMODULE's are sub-classes of this class.
%%
%% Objects of a class are created with wxCLASS:new(...) and destroyed with
%% wxCLASS:destroy(). Member functions are called with wxCLASS:member(Object, ...)
%% instead of as in C++ Object.member(...).
%%
%% Sub class modules inherit (non static) functions from their parents.
%% The inherited functions are not documented in the sub-classes.
%%
%% This erlang port of wxWidgets tries to be a one-to-one mapping with
%% the original wxWidgets library. Some things are different though,
%% as the optional arguments use property lists and can be in any
%% order. The main difference is the event handling which is different
%% from the original library. See {@link wxEvtHandler}.
%%
%% The following classes are implemented directly as erlang types: <br />
%% wxPoint={x,y},wxSize={w,h},wxRect={x,y,w,h},wxColour={r,g,b [,a]},
%% wxString={@link //stdlib/unicode:chardata()},
%% wxGBPosition={r,c},wxGBSpan={rs,cs},wxGridCellCoords={r,c}.
%%
%% wxWidgets uses a process specific environment, which is created by
%% {@link wx:new/0}.  To be able to use the environment from other
%% processes, call {@link get_env/0} to retrieve the environment and
%% {@link set_env/1} to assign the environment in the other process.
%%
%% Global (classless) functions are located in the wx_misc module.

-module(wx).
-moduledoc """
A port of [wxWidgets](http://www.wxwidgets.org/).

This is the base api of [wxWidgets](http://www.wxwidgets.org/). This module
contains functions for starting and stopping the wx-server, as well as other
utility functions.

wxWidgets is object oriented, and not functional. Thus, in Wx a module
represents a class, and the object created by this class has an own type,
wxCLASS(). This module represents the base class, and all other wxMODULE's are
sub-classes of this class.

Objects of a class are created with wxCLASS:new(...) and destroyed with
wxCLASS:destroy(). Member functions are called with wxCLASS:member(Object, ...)
instead of as in C++ Object.member(...).

Sub class modules inherit (non static) functions from their parents. The
inherited functions are not documented in the sub-classes.

This erlang port of wxWidgets tries to be a one-to-one mapping with the original
wxWidgets library. Some things are different though, as the optional arguments
use property lists and can be in any order. The main difference is the event
handling which is different from the original library. See `m:wxEvtHandler`.

The following classes are implemented directly as erlang types:  
wxPoint=\{x,y\},wxSize=\{w,h\},wxRect=\{x,y,w,h\},wxColour=\{r,g,b \[,a]\},
wxString=[unicode:chardata()](`t:unicode:chardata/0`),
wxGBPosition=\{r,c\},wxGBSpan=\{rs,cs\},wxGridCellCoords=\{r,c\}.

wxWidgets uses a process specific environment, which is created by
[wx:new/0](`new/0`). To be able to use the environment from other processes,
call `get_env/0` to retrieve the environment and `set_env/1` to assign the
environment in the other process.

Global (classless) functions are located in the wx_misc module.

## DATA TYPES

- **[](){: #type-wx_colour } wx_colour() = \{R::byte(), G::byte(), B::byte()\} |
  [wx_colour4()](`m:wx#type-wx_colour4`)**

- **[](){: #type-wx_colour4 } wx_colour4() = \{R::byte(), G::byte(), B::byte(),
  A::byte()\}**

- **[](){: #type-wx_datetime } wx_datetime() = \{\{Year::integer(),
  Month::integer(), Day::integer()\}, \{Hour::integer(), Minute::integer(),
  Second::integer()\}\}**

  In Local Timezone

- **[](){: #type-wx_enum } wx_enum() = integer()**

  Constant defined in wx.hrl

- **[](){: #type-wx_env } wx_env() = #wx_env\{\}**

  Opaque process environment

- **[](){: #type-wx_memory } wx_memory() = binary() | #wx_mem\{\}**

  Opaque memory reference

- **[](){: #type-wx_object } wx_object() = #wx_ref\{\}**

  Opaque object reference

- **[](){: #type-wx_wxHtmlLinkInfo } wx_wxHtmlLinkInfo() =
  #wxHtmlLinkInfo\{href=[unicode:chardata()](`t:unicode:chardata/0`),
  target=[unicode:chardata()](`t:unicode:chardata/0`)\}**

- **[](){: #type-wx_wxMouseState } wx_wxMouseState() =
  #wxMouseState\{x=integer(), y=integer(), leftDown=boolean(),
  middleDown=boolean(), rightDown=boolean(), controlDown=boolean(),
  shiftDown=boolean(), altDown=boolean(), metaDown=boolean(),
  cmdDown=boolean()\}**

  See #wxMouseState\{\} defined in wx.hrl
""".

-export([parent_class/1, new/0, new/1, destroy/0,
	 get_env/0, set_env/1, subscribe_events/0, debug/1,
	 batch/1,foreach/2,map/2,foldl/3,foldr/3,
	 getObjectType/1, typeCast/2,
	 null/0, is_null/1, equal/2]).

-export([create_memory/1, get_memory_bin/1,
	 retain_memory/1, release_memory/1]).

-export([demo/0]).

-export_type([wx_object/0, wx_env/0, wx_memory/0]).
-export_type([wx_colour/0, wx_colour4/0, wx_datetime/0,
	      wx_enum/0, wx_wxMouseState/0, wx_wxHtmlLinkInfo/0]).

-include("wxe.hrl").
-include("../include/wx.hrl").

-type wx_object() :: #wx_ref{}.  %% Opaque object reference
-type wx_env() :: #wx_env{}.     %% Opaque process environment
-type wx_memory() :: binary() | #wx_mem{}.     %% Opaque memory reference

-type wx_colour4() :: {R::byte(),G::byte(),B::byte(), A::byte()}.
-type wx_colour()  :: {R::byte(),G::byte(),B::byte()}  | wx_colour4().

-type wx_datetime() :: {{Year::integer(),Month::integer(),Day::integer()},
			{Hour::integer(),Minute::integer(),Second::integer()}}. %% In Local Timezone

-type wx_wxMouseState() :: #wxMouseState{}.  %% See #wxMouseState{} defined in wx.hrl
-type wx_enum() :: integer().      %% Constant defined in wx.hrl
-type wx_wxHtmlLinkInfo() :: #wxHtmlLinkInfo{}.

-doc "".
-spec parent_class(Wx_ref :: wx_object()) -> boolean().
parent_class(_) -> true. %% Let the null pointers be sent down.

%% @doc Starts a wx server.
-doc """
Starts a wx server.
""".
-spec new() -> wx_object().
new() ->
    new([]).

%% @doc Starts a wx server.
%% Option may be {debug, Level}, see debug/1.
%% Or {silent_start, Bool}, which causes error messages at startup to
%% be suppressed. The latter can be used as a silent test of whether
%% wx is properly installed or not.
-doc """
Starts a wx server.

Option may be `{debug, Level}`, see `debug/1`. Or `{silent_start, Bool}`,
which causes error messages at startup to be suppressed.
The latter can be used as a silent test of whether wx is properly installed or
not.
""".
-spec new([Option]) -> wx_object()
         when Option :: {'debug', list() | atom()} | {'silent_start', boolean()}.
new(Options) when is_list(Options) ->
    Debug = proplists:get_value(debug, Options, 0),
    SilentStart = proplists:get_value(silent_start, Options, false),
    Level = calc_level(Debug),
    #wx_env{} = wxe_server:start(SilentStart andalso Level =:= 0),
    %% put(opengl_port, Port),
    set_debug(Level),
    null().

%% @doc Stops a wx server.
-doc """
Stops a wx server.
""".
-spec destroy() -> 'ok'.
destroy() ->
    wxe_server:stop(),
    erase(?WXE_IDENTIFIER),
    ok.

%% @doc Gets this process's current wx environment.
%% Can be sent to other processes to allow them use this process wx environment.
%% @see set_env/1
-doc """
Gets this process's current wx environment. Can be sent to other processes to
allow them use this process wx environment.

_See also:_ `set_env/1`.
""".
-spec get_env() -> wx_env().
get_env() ->
    case get(?WXE_IDENTIFIER) of
	undefined -> erlang:error({wx,unknown_env});
	Env = #wx_env{} -> Env
    end.

%% @doc Sets the process wx environment, allows this process to use
%% another process wx environment.
-doc """
Sets the process wx environment, allows this process to use another process wx
environment.
""".
-spec set_env(Wx_env :: wx_env()) -> 'ok'.
set_env(#wx_env{sv=Pid} = Env) ->
    put(?WXE_IDENTIFIER, Env),
    %%    put(opengl_port, Port),
    %%    wxe_util:cast(?REGISTER_PID, <<>>),
    wxe_server:register_me(Pid),
    ok.

%% @doc Adds the calling process to the list of of processes that are
%% listening to wx application events. At the moment these are
%% all MacOSX specific events corresponding to MacNewFile() and friends
%% from wxWidgets wxApp https://docs.wxwidgets.org/trunk/classwx_app.html
%%
%% * `{new_file, ""}`
%% * `{open_file, Filename}`
%% * `{print_file, Filename}`
%% * `{open_url, Url}`
%% * `{reopen_app, ""}`
%%
%% The call always returns ok but will have sent any already received
%% events to the calling process.
-doc """
Adds the calling process to the list of of processes that are listening to wx
application events.

At the moment these are all MacOSX specific events corresponding to
`MacNewFile()` and friends from wxWidgets
[wxApp](https://docs.wxwidgets.org/trunk/classwx_app.html):

- `{new_file, ""}`
- `{open_file, Filename}`
- `{print_file, Filename}`
- `{open_url, Url}`
- `{reopen_app, ""}`

The call always returns ok but will have sent any already received events to the
calling process.
""".
-spec subscribe_events() -> 'ok'.
subscribe_events() ->
    gen_server:call(wxe_master, subscribe_msgs, infinity).

%% @doc Returns the null object
-doc """
Returns the null object
""".
-spec null() -> wx_object().
null() ->
    #wx_ref{ref=0, type=wx}.

%% @doc Returns true if object is null, false otherwise
-doc """
Returns true if object is null, false otherwise
""".
-spec is_null(Wx_ref :: wx_object()) -> boolean().
is_null(#wx_ref{ref=NULL}) -> NULL =:= 0.

%% @doc Returns true if both arguments references the same object, false otherwise
-doc """
Returns true if both arguments references the same object, false otherwise
""".
-spec equal(Ref1 :: wx_object(), Ref2 :: wx_object()) -> boolean().
equal(#wx_ref{ref=Ref1}, #wx_ref{ref=Ref2}) -> Ref1 =:= Ref2.

%% @doc Returns the object type
-doc """
Returns the object type
""".
-spec getObjectType(Wx_ref :: wx_object()) -> atom().
getObjectType(#wx_ref{type=Type}) ->
    Type.

%% @doc Casts the object to class NewType.
%%  It is needed when using functions like wxWindow:findWindow/2, which
%%  returns a generic wxObject type.
-doc """
Casts the object to class NewType. It is needed when using functions like
wxWindow:findWindow/2, which returns a generic wxObject type.
""".
-spec typeCast(wx_object(), atom()) -> wx_object().
typeCast(Old=#wx_ref{}, NewType) when is_atom(NewType) ->
    Old#wx_ref{type=NewType}.

%% @doc Batches all <c>wx</c> commands
%% used in the fun.  Improves performance of the command processing by
%% grabbing the wxWidgets thread so that no event processing will be
%% done before the complete batch of commands is invoked.
%%
%% @see map/2
%% @see foreach/2
%% @see foldl/3
%% @see foldr/3
-doc """
Batches all `wx` commands used in the fun. Improves performance of the command
processing by grabbing the wxWidgets thread so that no event processing will be
done before the complete batch of commands is invoked.

_See also:_ `foldl/3`, `foldr/3`, `foreach/2`, `map/2`.
""".
-spec batch(function()) -> term().
batch(Fun) ->
    ok = wxe_util:queue_cmd(?BATCH_BEGIN),
    try Fun()
    catch
	error:W:S -> erlang:exit({W, S});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after
        ok = wxe_util:queue_cmd(?BATCH_END)
    end.

%% @doc Behaves like {@link //stdlib/lists:foreach/2} but batches wx commands. See {@link batch/1}.
-doc """
Behaves like `lists:foreach/2` but batches wx commands. See `batch/1`.
""".
-spec foreach(function(), list()) -> 'ok'.
foreach(Fun, List) ->
    ok = wxe_util:queue_cmd(?BATCH_BEGIN),
    try lists:foreach(Fun, List)
    catch
	error:W:S -> erlang:exit({W, S});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after
        ok = wxe_util:queue_cmd(?BATCH_END)
    end.

%% @doc Behaves like {@link //stdlib/lists:map/2} but batches wx commands. See {@link batch/1}.
-doc """
Behaves like `lists:map/2` but batches wx commands. See `batch/1`.
""".
-spec map(function(), list()) -> list().
map(Fun, List) ->
    ok = wxe_util:queue_cmd(?BATCH_BEGIN),
    try lists:map(Fun, List)
    catch
	error:W:S -> erlang:exit({W, S});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after
        ok = wxe_util:queue_cmd(?BATCH_END)
    end.

%% @doc Behaves like {@link //stdlib/lists:foldl/3} but batches wx commands. See {@link batch/1}.
-doc """
Behaves like `lists:foldl/3` but batches wx commands. See `batch/1`.
""".
-spec foldl(function(), term(), list()) -> term().
foldl(Fun, Acc, List) ->
    ok = wxe_util:queue_cmd(?BATCH_BEGIN),
    try lists:foldl(Fun, Acc, List)
    catch
	error:W:S -> erlang:exit({W, S});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after
        ok = wxe_util:queue_cmd(?BATCH_END)
    end.

%% @doc Behaves like {@link //stdlib/lists:foldr/3} but batches wx commands. See {@link batch/1}.
-doc """
Behaves like `lists:foldr/3` but batches wx commands. See `batch/1`.
""".
-spec foldr(function(), term(), list()) -> term().
foldr(Fun, Acc, List) ->
    ok = wxe_util:queue_cmd(?BATCH_BEGIN),
    try lists:foldr(Fun, Acc, List)
    catch
	error:W:S -> erlang:exit({W, S});
	throw:W -> erlang:throw(W);
	exit:W  -> erlang:exit(W)
    after
        ok = wxe_util:queue_cmd(?BATCH_END)
    end.

-define(MIN_BIN_SIZE, 64).  %% Current emulator min off heap size

%% @doc Creates a memory area (of Size in bytes) which can be used by an external library (i.e. opengl).
%% It is up to the client to keep a reference to this object so it does
%% not get garbage collected by erlang while still in use by the external
%% library.
%%
%% This is far from erlang's intentional usage and can crash the erlang emulator.
%% Use it carefully.

-doc """
Creates a memory area (of Size in bytes) which can be used by an external
library (i.e. opengl). It is up to the client to keep a reference to this object
so it does not get garbage collected by erlang while still in use by the
external library.

This is far from erlang's intentional usage and can crash the erlang emulator.
Use it carefully.
""".
-spec create_memory(Size :: integer()) -> wx_memory().
create_memory(Size) when Size > ?MIN_BIN_SIZE ->
    #wx_mem{bin = <<0:(Size*8)>>, size = Size};
create_memory(Size) ->
    #wx_mem{bin = <<0:((?MIN_BIN_SIZE+1)*8)>>, size = Size}.

%% @doc Returns the memory area as a binary.
-doc """
Returns the memory area as a binary.
""".
-spec get_memory_bin(Wx_mem :: wx_memory()) -> binary().
get_memory_bin(#wx_mem{bin=Bin, size=Size}) when Size > ?MIN_BIN_SIZE ->
    Bin;
get_memory_bin(#wx_mem{bin=Bin, size=Size}) ->
    <<WithCorrectSize:Size/binary, _/binary>> = Bin,
    WithCorrectSize.

%% @doc Saves the memory from deletion until release_memory/1 is called.
%% If release_memory/1 is not called the memory will not be garbage collected.
-doc """
Saves the memory from deletion until `release_memory/1` is called. If
`release_memory/1` is not called the memory will not be garbage collected.
""".
-spec retain_memory(Wx_mem :: wx_memory()) -> 'ok'.
retain_memory(#wx_mem{}=Mem) ->
    case get(Mem) of
        {Mem, N} -> put(Mem, N+1);
        undefined -> put(Mem, 1)
    end,
    ok;
retain_memory(Bin) when is_binary(Bin) ->
    case byte_size(Bin) > ?MIN_BIN_SIZE of
	true  -> ok;
	false -> erlang:error(small_bin)
    end,
    retain_memory(#wx_mem{bin=Bin, size=byte_size(Bin)}).

-doc "Releases the memory retained by `retain_memory/1`".
-spec release_memory(Wx_mem :: wx_memory()) -> 'ok'.
release_memory(#wx_mem{}=Mem) ->
    case erase(Mem) of
        1 -> ok;
        N -> put(Mem, N-1),
             ok
    end;
release_memory(Bin) when is_binary(Bin) ->
    release_memory(#wx_mem{bin=Bin, size=byte_size(Bin)}).

%% @doc Sets debug level. If debug level is 'verbose' or 'trace'
%% each call is printed on console. If Level is 'driver' each allocated
%% object and deletion is printed on the console.
-doc """
debug(Level)

Sets debug level. If debug level is 'verbose' or 'trace' each call is printed on
console. If Level is 'driver' each allocated object and deletion is printed on
the console.
""".
-spec debug(Level | [Level]) -> 'ok'
     when Level :: 'none' | 'verbose' | 'trace' | 'driver' | integer().

debug(Debug) ->
    Level = calc_level(Debug),
    set_debug(Level).

calc_level(none) -> calc_level(0);
calc_level(verbose) -> calc_level(1);
calc_level(trace) -> calc_level(2);
calc_level(driver) -> calc_level(16);
calc_level([]) -> calc_level(0);
calc_level(List) when is_list(List) ->
    {Drv,Erl} =
	lists:foldl(fun(verbose, {Drv,_Erl}) ->
			    {Drv,1};
		       (trace, {Drv,_Erl}) ->
			    {Drv,2};
		       (driver, {_Drv,Erl}) ->
			    {16, Erl}
		    end, {0,0}, List),
    Drv + Erl;
calc_level(Level) when is_integer(Level) ->
    Level.

set_debug(Level) when is_integer(Level) ->
    case get_env() of
	#wx_env{debug=Old} when Old =:= Level -> ok;
	Env = #wx_env{sv=Server, debug=Old} ->
	    if
		Old > 16, Level > 16 -> ok;
		Old < 16, Level < 16 -> ok;
		true -> wxe_util:debug_driver(Level bsr 4)
	    end,
	    put(?WXE_IDENTIFIER, Env#wx_env{debug=Level}),
	    wxe_server:set_debug(Server,Level),
	    ok
    end.

%% @doc Starts a Wx demo if examples directory exists and is compiled
-doc """
Starts a Wx demo if examples directory exists and is compiled
""".
-spec demo() -> 'ok' | {'error', atom()}.
demo() ->
    Priv = code:priv_dir(wx),
    Demo = filename:join([filename:dirname(Priv),examples,demo]),
    Mod  = list_to_atom("demo"), %% Fool xref tests
    case file:set_cwd(Demo) of
	ok ->
	    apply(Mod, start, []),
	    ok;
	_  ->
	    {error, no_demo_dir}
    end.

