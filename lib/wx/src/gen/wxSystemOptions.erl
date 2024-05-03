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

-module(wxSystemOptions).
-moduledoc """
Functions for wxSystemOptions class

`m:wxSystemOptions` stores option/value pairs that wxWidgets itself or
applications can use to alter behaviour at run-time. It can be used to optimize
behaviour that doesn't deserve a distinct API, but is still important to be able
to configure.

System options can be set by the program itself using `setOption/2` method and
they also can be set from the program environment by defining an environment
variable `wx_option` to set the given option for all wxWidgets applications or
`wx_appname_option` to set it just for the application with the given name (as
returned by `wxApp::GetAppName()` (not implemented in wx)). Notice that any
characters not allowed in the environment variables names, such as periods and
dashes, should be replaced with underscores. E.g. to define a system option
"foo-bar" you need to define the environment variable "wx_foo_bar".

The program may use system options for its own needs but they are mostly used to
control the behaviour of wxWidgets library itself.

These options are currently recognised by wxWidgets:

All platforms

Windows

GTK+

Mac

Motif

The compile-time option to include or exclude this functionality is
wxUSE_SYSTEM_OPTIONS.

See: `m:wxSystemSettings`

wxWidgets docs:
[wxSystemOptions](https://docs.wxwidgets.org/3.1/classwx_system_options.html)
""".
-include("wxe.hrl").
-export([getOption/1,getOptionInt/1,hasOption/1,isFalse/1,setOption/2]).

%% inherited exports
-export([parent_class/1]).

-type wxSystemOptions() :: wx:wx_object().
-export_type([wxSystemOptions/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionsgetoption">external documentation</a>.
-doc """
Gets an option.

The function is case-insensitive to `name`. Returns empty string if the option
hasn't been set.

See: `setOption/2`, `getOptionInt/1`, `hasOption/1`
""".
-spec getOption(Name) -> unicode:charlist() when
	Name::unicode:chardata().
getOption(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_GetOption),
  wxe_util:rec(?wxSystemOptions_GetOption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionsgetoptionint">external documentation</a>.
-doc """
Gets an option as an integer.

The function is case-insensitive to `name`. If the option hasn't been set, this
function returns 0.

See: `setOption/2`, `getOption/1`, `hasOption/1`
""".
-spec getOptionInt(Name) -> integer() when
	Name::unicode:chardata().
getOptionInt(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_GetOptionInt),
  wxe_util:rec(?wxSystemOptions_GetOptionInt).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionshasoption">external documentation</a>.
-doc """
Returns true if the given option is present.

The function is case-insensitive to `name`.

See: `setOption/2`, `getOption/1`, `getOptionInt/1`
""".
-spec hasOption(Name) -> boolean() when
	Name::unicode:chardata().
hasOption(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_HasOption),
  wxe_util:rec(?wxSystemOptions_HasOption).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionsisfalse">external documentation</a>.
-doc """
Returns true if the option with the given `name` had been set to 0 value.

This is mostly useful for boolean options for which you can't use
`GetOptionInt(name)` == 0 as this would also be true if the option hadn't been
set at all.
""".
-spec isFalse(Name) -> boolean() when
	Name::unicode:chardata().
isFalse(Name)
 when ?is_chardata(Name) ->
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(Name_UC,?get_env(),?wxSystemOptions_IsFalse),
  wxe_util:rec(?wxSystemOptions_IsFalse).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxsystemoptions.html#wxsystemoptionssetoption">external documentation</a>.
%% <br /> Also:<br />
%% setOption(Name, Value) -> 'ok' when<br />
%% 	Name::unicode:chardata(), Value::unicode:chardata().<br />
%% 
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

