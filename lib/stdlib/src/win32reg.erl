%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
%%
-module(win32reg).
-moduledoc """
Provides access to the registry on Windows.

This module provides read and write access to the registry on Windows. It is
essentially a port driver wrapped around the Win32 API calls for accessing the
registry.

The registry is a hierarchical database, used to store various system and
software information in Windows. It contains installation data, and is updated
by installers and system programs. The Erlang installer updates the registry by
adding data that Erlang needs.

The registry contains keys and values. Keys are like the directories in a file
system, they form a hierarchy. Values are like files, they have a name and a
value, and also a type.

Paths to keys are left to right, with subkeys to the right and backslash between
keys. (Remember that backslashes must be doubled in Erlang strings.) Case is
preserved but not significant.

For example, `"\\hkey_local_machine\\software\\Ericsson\\Erlang\\5.0"` is the
key for the installation data for the latest Erlang release.

There are six entry points in the Windows registry, top-level keys. They can be
abbreviated in this module as follows:

```text
Abbreviation     Registry key
============     ============
hkcr             HKEY_CLASSES_ROOT
current_user     HKEY_CURRENT_USER
hkcu             HKEY_CURRENT_USER
local_machine    HKEY_LOCAL_MACHINE
hklm             HKEY_LOCAL_MACHINE
users            HKEY_USERS
hku              HKEY_USERS
current_config   HKEY_CURRENT_CONFIG
hkcc             HKEY_CURRENT_CONFIG
dyn_data         HKEY_DYN_DATA
hkdd             HKEY_DYN_DATA
```

The key above can be written as `"\\hklm\\software\\ericsson\\erlang\\5.0"`.

This module uses a current key. It works much like the current directory. From
the current key, values can be fetched, subkeys can be listed, and so on.

Under a key, any number of named values can be stored. They have names, types,
and data.

`win32reg` supports storing of the following types:

- `REG_DWORD`, which is an integer
- `REG_SZ`, which is a string
- `REG_BINARY`, which is a binary

Other types can be read, and are returned as binaries.

There is also a "default" value, which has the empty string as name. It is read
and written with the atom `default` instead of the name.

Some registry values are stored as strings with references to environment
variables, for example, `%SystemRoot%Windows`. `SystemRoot` is an environment
variable, and is to be replaced with its value. Function `expand/1` is provided
so that environment variables surrounded by `%` can be expanded to their values.

For more information on the Windows registry, see consult the Win32 Programmer's
Reference.

## See Also

`erl_posix_msg`, The Windows 95 Registry (book from O'Reilly), Win32
Programmer's Reference (from Microsoft)
""".

-export([open/1, close/1,
	 current_key/1, change_key/2, change_key_create/2,
	 sub_keys/1, delete_key/1,
	 value/2, values/1, set_value/3, delete_value/2,
	 expand/1,
	 format_error/1]).

-export_type([reg_handle/0]).

%% Key handles (always open).
-define(hkey_classes_root, 16#80000000).
-define(hkey_current_user, 16#80000001).
-define(hkey_local_machine, 16#80000002).
-define(hkey_users, 16#80000003).
-define(hkey_performance_data, 16#80000004).
-define(hkey_current_config, 16#80000005).
-define(hkey_dyn_data, 16#80000006).

%% Driver commands.
-define(cmd_get_current, 0).
-define(cmd_open_key, 1).
-define(cmd_create_key, 2).
-define(cmd_get_all_subkeys, 3).
-define(cmd_get_value, 4).
-define(cmd_get_all_values, 5).
-define(cmd_set_value, 6).
-define(cmd_delete_key, 7).
-define(cmd_delete_value, 8).

%% Data types.
-define(reg_sc, 1).
-define(reg_expand_sc, 2).
-define(reg_binary, 3).
-define(reg_dword, 4).

%% Basic types internal to this file.
-doc "As returned by `open/1`.".
-opaque reg_handle() :: {'win32reg',port()}.
-type name()       :: string() | 'default'.
-type value()      :: string() | integer() | binary().

%%% Exported functions.

-doc """
Opens the registry for reading or writing. The current key is the root
(`HKEY_CLASSES_ROOT`). Flag `read` in the mode list can be omitted.

Use `change_key/2` with an absolute path after [`open`](`open/1`).
""".
-spec open(OpenModeList) -> ReturnValue when
      OpenModeList :: [OpenMode],
      OpenMode :: 'read' | 'write',
      ReturnValue :: {'ok', RegHandle} | {'error', ErrorId :: 'enotsup'},
      RegHandle :: reg_handle().

open(Modes) ->
    case os:type() of
	{win32, _} ->
	    case open_mode(Modes, []) of
		{error, Reason} ->
		    {error, Reason};
		ModeStr ->
		    P = open_port({spawn, "registry__drv__ " ++ ModeStr}, []),
		    {ok, {win32reg, P}}
	    end;
	_ ->
	    {error, enotsup}
    end.

-doc "Closes the registry. After that, the `RegHandle` cannot be used.".
-spec close(RegHandle) -> 'ok' when
      RegHandle :: reg_handle().

close({win32reg, Reg}) when is_port(Reg) ->
    unlink(Reg),
    exit(Reg, die),
    ok.

-doc """
Returns the path to the current key. This is the equivalent of `pwd`.

Notice that the current key is stored in the driver, and can be invalid (for
example, if the key has been removed).
""".
-spec current_key(RegHandle) -> ReturnValue when
      RegHandle :: reg_handle(),
      ReturnValue :: {'ok', string()}.

current_key({win32reg, Reg}) when is_port(Reg) ->
    Cmd = [?cmd_get_current],
    Reg ! {self(), {command, Cmd}},
    {state, Hkey, Name} = get_result(Reg),
    Root = hkey_to_string(Hkey),
    {ok, case Name of
	     [] -> Root;
	     _  -> Root ++ [$\\|Name]
	 end}.

-doc """
Changes the current key to another key. Works like `cd`. The key can be
specified as a relative path or as an absolute path, starting with `\.`
""".
-spec change_key(RegHandle, Key) -> ReturnValue when
      RegHandle :: reg_handle(),
      Key :: string(),
      ReturnValue :: 'ok' | {'error', ErrorId :: atom()}.

change_key({win32reg, Reg}, Key) when is_port(Reg) ->
    change_key(Reg, ?cmd_open_key, Key).

-doc """
Creates a key, or just changes to it, if it is already there. Works like a
combination of `mkdir` and `cd`. Calls the Win32 API function
`RegCreateKeyEx()`.

The registry must have been opened in write mode.
""".
-spec change_key_create(RegHandle, Key) -> ReturnValue when
      RegHandle :: reg_handle(),
      Key :: string(),
      ReturnValue :: 'ok' | {'error', ErrorId :: atom()}.

change_key_create({win32reg, Reg}, Key) when is_port(Reg) ->
    change_key(Reg, ?cmd_create_key, Key).

change_key(Reg, Cmd, Key) ->
    case parse_key(Key, Reg) of
	{ok, Hkey, Path} ->
	    Reg ! {self(), {command, [Cmd, i32(Hkey), Path, 0]}},
	    get_result(Reg);
	{error, Reason} ->
	    {error, Reason}
    end.

-doc """
Returns a list of subkeys to the current key. Calls the Win32 API function
`EnumRegKeysEx()`.

Avoid calling this on the root keys, as it can be slow.
""".
-spec sub_keys(RegHandle) -> ReturnValue when
      RegHandle :: reg_handle(),
      ReturnValue :: {'ok', [SubKey]} | {'error', ErrorId :: atom()},
      SubKey :: string().

sub_keys({win32reg, Reg}) when is_port(Reg) ->
    Cmd = [?cmd_get_all_subkeys],
    Reg ! {self(), {command, Cmd}},
    collect_keys(Reg, []).

-doc """
Deletes the current key, if it is valid. Calls the Win32 API function
`RegDeleteKey()`. Notice that this call does not change the current key (unlike
`change_key_create/2`). This means that after the call, the current key is
invalid.
""".
-spec delete_key(RegHandle) -> ReturnValue when
      RegHandle :: reg_handle(),
      ReturnValue :: 'ok' | {'error', ErrorId :: atom()}.

delete_key({win32reg, Reg}) when is_port(Reg) ->
    Cmd = [?cmd_get_current],
    Reg ! {self(), {command, Cmd}},
    case get_result(Reg) of
	{state, _Hkey, []} ->
	    {error, eperm};
	{state, _Hkey, Rest} ->
	    [Name|_] = split_key(Rest),
	    ok = change_key(Reg, ?cmd_open_key, ".."),
	    Cmd2 = [?cmd_delete_key],
	    Reg ! {self(), {command, [Cmd2, Name, 0]}},
	    get_result(Reg);
	{error, Erorr} ->
	    {error, Erorr}
    end.

-doc """
Sets the named (or default) value to `value`. Calls the Win32 API function
`RegSetValueEx()`. The value can be of three types, and the corresponding
registry type is used. The supported types are the following:

- `REG_DWORD` for integers
- `REG_SZ` for strings
- `REG_BINARY` for binaries

Other types cannot be added or changed.

The registry must have been opened in write mode.
""".
-spec set_value(RegHandle, Name, Value) -> ReturnValue when
      RegHandle :: reg_handle(),
      Name :: name(),
      Value :: value(),
      ReturnValue :: 'ok' | {'error', ErrorId :: atom()}.

set_value({win32reg, Reg}, Name0, Value) when is_port(Reg) ->
    Name =
	case Name0 of
	    default -> [];
	    _ -> Name0
	end,
    {Type, V} = term_to_value(Value),
    Cmd = [?cmd_set_value, Type, Name, 0, V],
    Reg ! {self(), {command, Cmd}},
    get_result(Reg).

-doc """
Retrieves the named value (or default) on the current key. Registry values of
type `REG_SZ` are returned as strings. Type `REG_DWORD` values are returned as
integers. All other types are returned as binaries.
""".
-spec value(RegHandle, Name) -> ReturnValue when
      RegHandle :: reg_handle(),
      Name :: name(),
      ReturnValue :: {'ok', Value :: value()} | {'error', ErrorId :: atom()}.

value({win32reg, Reg}, Name0) when is_port(Reg) ->
    Name =
	case Name0 of
	    default -> [];
	    _ -> Name0
	end,
    Cmd = [?cmd_get_value, Name, 0],
    Reg ! {self(), {command, Cmd}},
    case get_result(Reg) of
	{value, {default, Value}} when Name =:= [] ->
	    {ok, Value};
	{value, {Name, Value}} ->
	    {ok, Value};
	{error, Reason} ->
	    {error, Reason}
    end.
    
-doc """
Retrieves a list of all values on the current key. The values have types
corresponding to the registry types, see `value/2`. Calls the Win32 API function
`EnumRegValuesEx()`.
""".
-spec values(RegHandle) -> ReturnValue when
      RegHandle :: reg_handle(),
      ReturnValue :: {'ok', [ValuePair]} | {'error', ErrorId :: atom()},
      ValuePair :: {Name :: name(), Value :: value()}.

values({win32reg, Reg}) when is_port(Reg) ->
    Cmd = [?cmd_get_all_values],
    Reg ! {self(), {command, Cmd}},
    collect_values(Reg, []).

-doc """
Deletes a named value on the current key. The atom `default` is used for the
default value.

The registry must have been opened in write mode.
""".
-spec delete_value(RegHandle, Name) -> ReturnValue when
      RegHandle :: reg_handle(),
      Name :: name(),
      ReturnValue :: 'ok' | {'error', ErrorId :: atom()}.

delete_value({win32reg, Reg}, Name0) when is_port(Reg) ->
    Name =
	case Name0 of
	    default -> [];
	    _ -> Name0
	end,
    Cmd = [?cmd_delete_value, Name, 0],
    Reg ! {self(), {command, Cmd}},
    get_result(Reg).

-doc """
Expands a string containing environment variables between percent characters.
Anything between two `%` is taken for an environment variable, and is replaced
by the value. Two consecutive `%` are replaced by one `%`.

A variable name that is not in the environment results in an error.
""".
-spec expand(String) -> ExpandedString when
      String :: string(),
      ExpandedString :: string().

expand(Value) ->
    expand(Value, [], []).

expand([$%, $%|Rest], [], Result) ->
    expand(Rest, [], [$%|Result]);
expand([$%, C|Rest], [], Result) ->
    expand(Rest, [C], Result);
expand([C|Rest], [], Result) ->
    expand(Rest, [], [C|Result]);
expand([$%|Rest], Env0, Result) ->
    Env = lists:reverse(Env0),
    expand(Rest, [], lists:reverse(os:getenv(Env, ""))++Result);
expand([C|Rest], Env, Result) ->
    expand(Rest, [C|Env], Result);
expand([], [], Result) ->
    lists:reverse(Result).

-doc "Converts a POSIX error code to a string (by calling `file:format_error/1`).".
-spec format_error(ErrorId) -> ErrorString when
      ErrorId :: atom(),
      ErrorString :: string().

format_error(ErrorId) ->
    erl_posix_msg:message(ErrorId).

%%% Implementation.

-spec collect_values(port(), [{name(), value()}]) -> 
        {'ok', [{name(), value()}]} | {'error', ErrorId :: atom()}.

collect_values(P, Result) ->
    case get_result(P) of
	ok ->
	    {ok, lists:reverse(Result)};
	{value, ValueData} ->
	    collect_values(P, [ValueData|Result]);
	{error, Reason} ->
	    {error, Reason}
    end.

-spec collect_keys(port(), string()) -> {'ok', [string()]} | {'error', ErrorId :: atom()}.

collect_keys(P, Result) ->
    case get_result(P) of
	ok ->
	    {ok, lists:reverse(Result)};
	{key, KeyData} ->
	    collect_keys(P, [KeyData|Result]);
	{error, Reason} ->
	    {error, Reason}
    end.
    
get_result(P) ->
    receive
	{P, {data, Data}} ->
	    get_result1(Data)
    end.

get_result1([$e|Reason]) ->
    {error, list_to_atom(Reason)};
get_result1([$o]) ->
    ok;
get_result1([$k|Name]) ->
    {key, Name};
get_result1([$v|Rest0]) ->
    {ok, Type, Rest1} = i32_on_head(Rest0),
    {ok, Name0, Value} = get_cstring(Rest1),
    Name = 
	case Name0 of
	    [] -> default;
	    _ ->  Name0
	end,
    {value, {Name, encode_value(Type, Value)}};
get_result1([$s|Rest0]) ->
    {ok, Hkey, Name} = i32_on_head(Rest0),
    {state, Hkey, Name}.

encode_value(?reg_sc, Value) ->
    Value;
encode_value(?reg_expand_sc, Value) ->
    Value;
encode_value(?reg_dword, Value) ->
    i32(Value);
encode_value(_, Value) ->
    list_to_binary(Value).

term_to_value(Int) when is_integer(Int) ->
    {i32(?reg_dword), i32(Int)};
term_to_value(String) when is_list(String) ->
    {i32(?reg_sc), [String, 0]};
term_to_value(Bin) when is_binary(Bin) ->
    {i32(?reg_binary), Bin};
term_to_value(_) ->
    exit(badarg).
    
get_cstring(List) ->
    get_cstring(List, []).

get_cstring([0|Rest], Result) ->
    {ok, lists:reverse(Result), Rest};
get_cstring([C|Rest], Result) ->
    get_cstring(Rest, [C|Result]);
get_cstring([], Result) ->
    {ok, lists:reverse(Result), []}.
	    
i32(Int) when is_integer(Int) ->
    [(Int bsr 24) band 255,
     (Int bsr 16) band 255,
     (Int bsr  8) band 255,
     Int band 255];
i32([X1, X2, X3, X4]) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

i32_on_head([X1, X2, X3, X4 | Rest]) ->
    {ok, (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4, Rest}.

parse_key([$\\|Rest], _) ->
    parse_root(Rest, []);
parse_key(Key, Reg) ->
    parse_relative(Key, Reg).

parse_relative(Path, Reg) ->
    Cmd = [?cmd_get_current],
    Reg ! {self(), {command, Cmd}},
    {state, RootHandle, Name} = get_result(Reg),
    Original = split_key(Name),
    Relative = lists:reverse(split_key(Path)),
    case parse_relative1(Relative, Original) of
	{error,Error} ->
	    {error,Error};
	NewPath ->
	    {ok, RootHandle, NewPath}
    end.

parse_relative1([".."|_], []) ->
    {error,enoent};
parse_relative1([".."|T1], [_|T2]) ->
    parse_relative1(T1, T2);
parse_relative1([Comp|Rest], Result) ->
    parse_relative1(Rest, [Comp|Result]);
parse_relative1([], Result) ->
    reverse_and_join(Result, []).

reverse_and_join([X|Rest], []) ->
    reverse_and_join(Rest, [X]);
reverse_and_join([X|Rest], Result) ->
    reverse_and_join(Rest, [X, "\\" | Result]);
reverse_and_join([], Result) ->
    Result.

split_key(Key) ->
    split_key(Key, [], []).

split_key([$\\|Rest], Current, Result) ->
    split_key(Rest, [], [lists:reverse(Current)|Result]);
split_key([C|Rest], Current, Result) ->
    split_key(Rest, [C|Current], Result);
split_key([], [], Result) ->
    Result;
split_key([], Current, Result) ->
    [lists:reverse(Current)|Result].

parse_root([$\\|Rest], Result) ->
    Root = 
	case lists:reverse(Result) of
	    [$h, $k, $e, $y, $_|Root0] ->
		Root0;
	    Root0 ->
		Root0
	end,
    case root_to_handle(list_to_atom(Root)) of
	false ->
	    {error, enoent};
	Handle ->
	    {ok, Handle, Rest}
    end;
parse_root([C|Rest], Result) ->
    parse_root(Rest, [C|Result]);
parse_root([], Result) ->
    parse_root([$\\], Result).

root_to_handle(classes_root) -> ?hkey_classes_root;
root_to_handle(hkcr) -> ?hkey_classes_root;
root_to_handle(current_user) -> ?hkey_current_user;
root_to_handle(hkcu) -> ?hkey_current_user;
root_to_handle(local_machine) -> ?hkey_local_machine;
root_to_handle(hklm) -> ?hkey_local_machine;
root_to_handle(users) -> ?hkey_users;
root_to_handle(hku) -> ?hkey_users;
root_to_handle(current_config) -> ?hkey_current_config;
root_to_handle(hkcc) -> ?hkey_current_config;
root_to_handle(dyn_data) -> ?hkey_dyn_data;
root_to_handle(hkdd) -> ?hkey_dyn_data;
root_to_handle(performance_data) -> ?hkey_performance_data;
root_to_handle(_) -> false.

hkey_to_string(?hkey_classes_root) -> "\\hkey_classes_root";
hkey_to_string(?hkey_current_user) -> "\\hkey_current_user";
hkey_to_string(?hkey_local_machine) -> "\\hkey_local_machine";
hkey_to_string(?hkey_users) -> "\\hkey_users";
hkey_to_string(?hkey_performance_data) -> "\\hkey_performance_data";
hkey_to_string(?hkey_current_config) -> "\\hkey_current_config";
hkey_to_string(?hkey_dyn_data) -> "\\hkey_dyn_data".

open_mode([read|Rest], Result) ->
    open_mode(Rest, [$r|Result]);
open_mode([write|Rest], Result) ->
    open_mode(Rest, [$w|Result]);
open_mode([], Result) ->
    Result;
open_mode(_, _) ->
    {error, einval}.
