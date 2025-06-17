%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Maxim Fedorov
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(argparse).
-moduledoc """
Command line arguments parser.

This module implements command line parser. Parser operates with _commands_ and
_arguments_ represented as a tree. Commands are branches, and arguments are
leaves of the tree. Parser always starts with the root command, named after
`progname` (the name of the program which started Erlang).

A [`command specification`](`t:command/0`) may contain handler definition for
each command, and a number argument specifications. When parser is successful,
`argparse` calls the matching handler, passing arguments extracted from the
command line. Arguments can be positional (occupying specific position in the
command line), and optional, residing anywhere but prefixed with a specified
character.

`argparse` automatically generates help and usage messages. It will also issue
errors when users give the program invalid arguments.

## Quick start

`argparse` is designed to work with [`escript`](`e:erts:escript_cmd.md`). The
example below is a fully functioning Erlang program accepting two command line
arguments and printing their product.

```erlang
#!/usr/bin/env escript

main(Args) ->
    argparse:run(Args, cli(), #{progname => mul}).

cli() ->
    #{
        arguments => [
            #{name => left, type => integer},
            #{name => right, type => integer}
        ],
        handler =>
            fun (#{left := Left, right := Right}) ->
                io:format("~b~n", [Left * Right])
            end
    }.
```

Running this script with no arguments results in an error, accompanied by the
usage information.

The `cli` function defines a single command with embedded handler accepting a
map. Keys of the map are argument names as defined by the `argument` field of
the command, `left` and `right` in the example. Values are taken from the
command line, and converted into integers, as requested by the type
specification. Both arguments in the example above are required (and therefore
defined as positional).

## Command hierarchy

A command may contain nested commands, forming a hierarchy. Arguments defined at
the upper level command are automatically added to all nested commands. Nested
commands example (assuming `progname` is `nested`):

```erlang
cli() ->
  #{
    %% top level argument applicable to all commands
    arguments => [#{name => top}],
      commands => #{
        "first" => #{
          %% argument applicable to "first" command and
          %%  all commands nested into "first"
          arguments => [#{name => mid}],
          commands => #{
            "second" => #{
              %% argument only applicable for "second" command
              arguments => [#{name => bottom}],
              handler => fun (A) -> io:format("~p~n", [A]) end
          }
        }
      }
    }
  }.
```

In the example above, a 3-level hierarchy is defined. First is the script itself
(`nested`), accepting the only argument `top`. Since it has no associated
handler, `run/3` will not accept user input omitting nested command selection.
For this example, user has to supply 5 arguments in the command line, two being
command names, and another 3 - required positional arguments:

```text
./nested.erl one first second two three
#{top => "one",mid => "two",bottom => "three"}
```

Commands have preference over positional argument values. In the example above,
commands and positional arguments are interleaving, and `argparse` matches
command name first.

## Arguments

`argparse` supports positional and optional arguments. Optional arguments, or
options for short, must be prefixed with a special character (`-` is the default
on all operating systems). Both options and positional arguments have 1 or more
associated values. See [`argument specification`](`t:argument/0`) to find more
details about supported combinations.

In the user input, short options may be concatenated with their values. Long
options support values separated by `=`. Consider this definition:

```erlang
cli() ->
  #{
    arguments => [
      #{name => long, long => "-long"},
      #{name => short, short => $s}
    ],
    handler => fun (Args) -> io:format("~p~n", [Args]) end
  }.
```

Running `./args --long=VALUE` prints `#{long => "VALUE"}`, running
`./args -sVALUE` prints `#{short => "VALUE"}`

`argparse` supports boolean flags concatenation: it is possible to shorten
`-r -f -v` to `-rfv`.

Shortened option names are not supported: it is not possible to use `--my-argum`
instead of `--my-argument-name` even when such option can be unambiguously
found.
""".
-moduledoc(#{since => "OTP 26.0"}).
-author("maximfca@gmail.com").

%% API Exports
-export([
    run/3,
    parse/2, parse/3,
    help/1, help/2,
    format_error/1
]).

%% Internal exports for validation and error reporting.
-export([validate/1, validate/2, format_error/2]).

%%--------------------------------------------------------------------
%% API

-doc """
Defines type conversion applied to the string retrieved from the user input. If
the conversion is successful, resulting value is validated using optional
`Choices`, or minimums and maximums (for integer and floating point values
only). Strings and binary values may be validated using regular expressions.
It's possible to define custom type conversion function, accepting a string and
returning Erlang term. If this function raises error with `badarg` reason,
argument is treated as invalid.
""".
-type arg_type() ::
    boolean |
    float |
    {float, Choice :: [float()]} |
    {float, [{min, float()} | {max, float()}]} |
    integer |
    {integer, Choices :: [integer()]} |
    {integer, [{min, integer()} | {max, integer()}]} |
    string |
    {string, Choices :: [string()]} |
    {string, Re :: string()} |
    {string, Re :: string(), ReOptions :: [term()]} |
    binary |
    {binary, Choices :: [binary()]} |
    {binary, Re :: binary()} |
    {binary, Re :: binary(), ReOptions :: [term()]} |
    atom |
    {atom, Choices :: [atom()]} |
    {atom, unsafe} |
    {custom, fun((string()) -> term())}.
%% Built-in types include basic validation abilities
%% String and binary validation may use regex match (ignoring captured value).
%% For float, integer, string, binary and atom type, it is possible to specify
%%  available choices instead of regex/min/max.

-doc """
User-defined help template to print in the command usage. First element of a
tuple must be a string. It is printed as a part of the usage header. Second
element of the tuple can be either a list containing strings, `type` and
`default` atoms, or a user-defined function that must return a string. A plain
string should be wrapped as a list such as `["string is nested"]`.
""".
-type argument_help() :: {
    unicode:chardata(), %% short form, printed in command usage, e.g. "[--dir <dirname>]", developer is
                        %% responsible for proper formatting (e.g. adding <>, dots... and so on)
    [unicode:chardata() | type | default] | fun(() -> unicode:chardata())
}.
%% Help template definition for argument. Short and long forms exist for every argument.
%% Short form is printed together with command definition, e.g. "usage: rm [--force]",
%%  while long description is printed in detailed section below: "--force   forcefully remove".

-doc "Argument name is used to populate argument map.".
-type argument_name() :: atom() | string() | binary().

-doc """
Argument specification. Defines a single named argument that is returned in the
[`argument map`](`t:arg_map/0`). The only required field is `name`, all other
fields have defaults.

If either of the `short` or `long` fields is specified, the argument is treated
as optional. Optional arguments do not have specific order and may appear
anywhere in the command line. Positional arguments are ordered the same way as
they appear in the arguments list of the command specification.

By default, all positional arguments must be present in the command line. The
parser will return an error otherwise. Options, however, may be omitted, in
which case resulting argument map will either contain the default value, or not
have the key at all.

- **`name`** - Sets the argument name in the parsed argument map. If `help` is
  not defined, name is also used to generate the default usage message.

- **`short`** - Defines a short (single character) form of an optional argument.

  ```erlang
  %% Define a command accepting argument named myarg, with short form $a:
  1> Cmd = #{arguments => [#{name => myarg, short => $a}]}.
  %% Parse command line "-a str":
  2> {ok, ArgMap, _, _} = argparse:parse(["-a", "str"], Cmd), ArgMap.

  #{myarg => "str"}

  %% Option value can be concatenated with the switch: "-astr"
  3> {ok, ArgMap, _, _} = argparse:parse(["-astr"], Cmd), ArgMap.

  #{myarg => "str"}
  ```

  By default all options expect a single value following the option switch. The
  only exception is an option of a boolean type.

- **`long`** - Defines a long form of an optional argument.

  ```erlang
  1> Cmd = #{arguments => [#{name => myarg, long => "name"}]}.
  %% Parse command line "-name Erlang":
  2> {ok, ArgMap, _, _} = argparse:parse(["-name", "Erlang"], Cmd), ArgMap.

  #{myarg => "Erlang"}
  %% Or use "=" to separate the switch and the value:
  3> {ok, ArgMap, _, _} = argparse:parse(["-name=Erlang"], Cmd), ArgMap.

  #{myarg => "Erlang"}
  ```

  If neither `short` not `long` is defined, the argument is treated as
  positional.

- **`required`** - Forces the parser to expect the argument to be present in the
  command line. By default, all positional argument are required, and all
  options are not.

- **`default`** - Specifies the default value to put in the parsed argument map
  if the value is not supplied in the command line.

  ```erlang
  1> argparse:parse([], #{arguments => [#{name => myarg, short => $m}]}).

  {ok,#{}, ...
  2> argparse:parse([], #{arguments => [#{name => myarg, short => $m, default => "def"}]}).

  {ok,#{myarg => "def"}, ...
  ```

- **`type`** - Defines type conversion and validation routine. The default is
  `string`, assuming no conversion.

- **`nargs`** - Defines the number of following arguments to consume from the
  command line. By default, the parser consumes the next argument and converts
  it into an Erlang term according to the specified type.

  - **`t:pos_integer/0`** - Consume exactly this number of positional arguments,
    fail if there is not enough. Value in the argument map contains a list of
    exactly this length. Example, defining a positional argument expecting 3
    integer values:

    ```erlang
    1> Cmd = #{arguments => [#{name => ints, type => integer, nargs => 3}]},
    argparse:parse(["1", "2", "3"], Cmd).

    {ok, #{ints => [1, 2, 3]}, ...
    ```

    Another example defining an option accepted as `-env` and expecting two
    string arguments:

    ```erlang
    1> Cmd = #{arguments => [#{name => env, long => "env", nargs => 2}]},
    argparse:parse(["-env", "key", "value"], Cmd).

    {ok, #{env => ["key", "value"]}, ...
    ```

  - **`list`** - Consume all following arguments until hitting the next option
    (starting with an option prefix). May result in an empty list added to the
    arguments map.

    ```erlang
    1> Cmd = #{arguments => [
      #{name => nodes, long => "nodes", nargs => list},
      #{name => verbose, short => $v, type => boolean}
    ]},
    argparse:parse(["-nodes", "one", "two", "-v"], Cmd).

    {ok, #{nodes => ["one", "two"], verbose => true}, ...
    ```

  - **`nonempty_list`** - Same as `list`, but expects at least one argument.
    Returns an error if the following command line argument is an option switch
    (starting with the prefix).

  - **`'maybe'`** - Consumes the next argument from the command line, if it does
    not start with an option prefix. Otherwise, adds a default value to the
    arguments map.

    ```erlang
    1> Cmd = #{arguments => [
      #{name => level, short => $l, nargs => 'maybe', default => "error"},
      #{name => verbose, short => $v, type => boolean}
    ]},
    argparse:parse(["-l", "info", "-v"], Cmd).

    {ok,#{level => "info",verbose => true}, ...

    %% When "info" is omitted, argument maps receives the default "error"
    2> argparse:parse(["-l", "-v"], Cmd).

    {ok,#{level => "error",verbose => true}, ...
    ```

  - **`{'maybe', term()}`** - Consumes the next argument from the command line,
    if it does not start with an option prefix. Otherwise, adds a specified
    Erlang term to the arguments map.

  - **`all`** - Fold all remaining command line arguments into a list, ignoring
    any option prefixes or switches. Useful for proxying arguments into another
    command line utility.

    ```erlang
    1> Cmd = #{arguments => [
        #{name => verbose, short => $v, type => boolean},
        #{name => raw, long => "-", nargs => all}
    ]},
    argparse:parse(["-v", "--", "-kernel", "arg", "opt"], Cmd).

    {ok,#{raw => ["-kernel","arg","opt"],verbose => true}, ...
    ```

- **`action`** - Defines an action to take when the argument is found in the
  command line. The default action is `store`.

  - **`store`** - Store the value in the arguments map. Overwrites the value
    previously written.

    ```erlang
    1> Cmd = #{arguments => [#{name => str, short => $s}]},
    argparse:parse(["-s", "one", "-s", "two"], Cmd).

    {ok, #{str => "two"}, ...
    ```

  - **`{store, term()}`** - Stores the specified term instead of reading the
    value from the command line.

    ```erlang
    1> Cmd = #{arguments => [#{name => str, short => $s, action => {store, "two"}}]},
    argparse:parse(["-s"], Cmd).

    {ok, #{str => "two"}, ...
    ```

  - **`append`** - Appends the repeating occurrences of the argument instead of
    overwriting.

    ```erlang
    1> Cmd = #{arguments => [#{name => node, short => $n, action => append}]},
    argparse:parse(["-n", "one", "-n", "two", "-n", "three"], Cmd).

    {ok, #{node => ["one", "two", "three"]}, ...

    %% Always produces a list - even if there is one occurrence
    2> argparse:parse(["-n", "one"], Cmd).

    {ok, #{node => ["one"]}, ...
    ```

  - **`{append, term()}`** - Same as `append`, but instead of consuming the
    argument from the command line, appends a provided `t:term/0`.

  - **`count`** - Puts a counter as a value in the arguments map. Useful for
    implementing verbosity option:

    ```erlang
    1> Cmd = #{arguments => [#{name => verbose, short => $v, action => count}]},
    argparse:parse(["-v"], Cmd).

    {ok, #{verbose => 1}, ...

    2> argparse:parse(["-vvvv"], Cmd).

    {ok, #{verbose => 4}, ...
    ```

  - **`extend`** - Works as `append`, but flattens the resulting list. Valid
    only for `nargs` set to `list`, `nonempty_list`, `all` or `t:pos_integer/0`.

    ```erlang
    1> Cmd = #{arguments => [#{name => duet, short => $d, nargs => 2, action => extend}]},
    argparse:parse(["-d", "a", "b", "-d", "c", "d"], Cmd).

    {ok, #{duet => ["a", "b", "c", "d"]}, ...

    %% 'append' would result in {ok, #{duet => [["a", "b"],["c", "d"]]},
    ```

- **`help`** - Specifies help/usage text for the argument. `argparse` provides
  automatic generation based on the argument name, type and default value, but
  for better usability it is recommended to have a proper description. Setting
  this field to `hidden` suppresses usage output for this argument.
""".
-type argument() :: #{
    %% Argument name, and a destination to store value too
    %% It is allowed to have several arguments named the same, setting or appending to the same variable.
    name := argument_name(),

    %% short, single-character variant of command line option, omitting dash (example: $b, meaning -b),
    %%  when present, the argument is considered optional
    short => char(),

    %% long command line option, omitting first dash (example: "kernel" means "-kernel" in the command line)
    %% long command always wins over short abbreviation (e.g. -kernel is considered before -k -e -r -n -e -l)
    %%  when present, the argument is considered optional
    long => string(),

    %% makes parser to return an error if the argument is not present in the command line
    required => boolean(),

    %% default value, produced if the argument is not present in the command line
    %% parser also accepts a global default
    default => term(),

    %% parameter type (string by default)
    type => arg_type(),

    %% action to take when argument is matched
    action => store |       %% default: store argument consumed (last stored wins)
        {store, term()} |   %% does not consume argument, stores term() instead
        append |            %% appends consumed argument to a list
        {append, term()} |  %% does not consume an argument, appends term() to a list
        count |             %% does not consume argument, bumps counter
        extend,             %% uses when nargs is list/nonempty_list/all - appends every element to the list

    %% how many positional arguments to consume
    nargs =>
        pos_integer() |     %% consume exactly this amount, e.g. '-kernel key value' #{long => "-kernel", args => 2}
                            %%      returns #{kernel => ["key", "value"]}
        'maybe' |           %% if the next argument is positional, consume it, otherwise produce default
        {'maybe', term()} | %% if the next argument is positional, consume it, otherwise produce term()
        list |              %% consume zero or more positional arguments, until next optional
        nonempty_list |     %% consume at least one positional argument, until next optional
        all,                %% fold remaining command line into this argument

    %% help string printed in usage, hidden help is not printed at all
    help => hidden | unicode:chardata() | argument_help()
}.
%% Command line argument specification.
%% Argument can be optional - starting with - (dash), and positional.

-doc """
Arguments map is the map of argument names to the values extracted from the
command line. It is passed to the matching command handler. If an argument is
omitted, but has the default value is specified, it is added to the map. When no
default value specified, and argument is not present in the command line,
corresponding key is not present in the resulting map.
""".
-type arg_map() :: #{argument_name() => term()}.
%% Arguments map: argument name to a term, produced by parser. Supplied to the command handler

-doc """
List of command line arguments to be parsed.
""".
-type args() :: [string() | unicode:chardata()].

-doc """
Command handler specification. Called by [`run/3` ](`run/3`)upon successful
parser return.

- **`fun((arg_map()) -> term())`** - Function accepting
  [`argument map`](`t:arg_map/0`). See the basic example in the
  [Quick Start](`m:argparse#module-quick-start`) section.

- **`{Module :: module(), Function :: atom()}`** - Function named `Function`,
  exported from `Module`, accepting [`argument map`](`t:arg_map/0`).

- **`{fun(() -> term()), Default :: term()}`** - Function accepting as many
  arguments as there are in the `arguments` list for this command. Arguments
  missing from the parsed map are replaced with the `Default`. Convenient way to
  expose existing functions.

  ```erlang
  1> Cmd = #{arguments => [
          #{name => x, type => float},
          #{name => y, type => float, short => $p}],
      handler => {fun math:pow/2, 1}},
  argparse:run(["2", "-p", "3"], Cmd, #{}).

  8.0

  %% default term 1 is passed to math:pow/2
  2> argparse:run(["2"], Cmd, #{}).

  2.0
  ```

- **`{Module :: module(), Function :: atom(), Default :: term()}`** - Function
  named `Function`, exported from `Module`, accepting as many arguments as
  defined for this command. Arguments missing from the parsed map are replaced
  with the `Default`. Effectively, just a different syntax to the same
  functionality as demonstrated in the code above.
""".
-type handler() ::
    optional |                      %% valid for commands with sub-commands, suppresses parser error when no
                                    %%   sub-command is selected
    fun((arg_map()) -> term()) |    %% handler accepting arg_map
    {module(), Fn :: atom()} |      %% handler, accepting arg_map, Fn exported from module()
    {fun(() -> term()), term()} |   %% handler, positional form (term() is supplied for omitted args)
    {module(), atom(), term()}.     %% handler, positional form, exported from module()
%% Command handler. May produce some output. Can accept a map, or be
%%  arbitrary mfa() for handlers accepting positional list.
%% Special value 'optional' may be used to suppress an error that
%%  otherwise raised when command contains sub-commands, but arguments
%%  supplied via command line do not select any.

-doc """
User-defined help template. Use this option to mix custom and predefined usage
text. Help template may contain unicode strings, and following atoms:

- **usage** - Formatted command line usage text, e.g. `rm [-rf] <directory>`.

- **commands** - Expanded list of sub-commands.

- **arguments** - Detailed description of positional arguments.

- **options** - Detailed description of optional arguments.
""".
-type command_help() :: [unicode:chardata() | usage | commands | arguments | options].
%% Template for the command help/usage message.

%% Command descriptor
-doc """
Command specification. May contain nested commands, forming a hierarchy.

- **`commands`** - Maps of nested commands. Keys must be strings, matching
  command line input. Basic utilities do not need to specify any nested
  commands.

- **`arguments`** - List of arguments accepted by this command, and all nested
  commands in the hierarchy.

- **`help`** - Specifies help/usage text for this command. Pass `hidden` to
  remove this command from the usage output.

- **`handler`** - Specifies a callback function to call by `run/3` when the
  parser is successful.
""".
-type command() :: #{
    %% Sub-commands are arranged into maps. Command name must not start with <em>prefix</em>.
    commands => #{string() => command()},
    %% accepted arguments list. Order is important!
    arguments => [argument()],
    %% help line
    help => hidden | unicode:chardata() | command_help(),
    %% recommended handler function
    handler => handler()
}.

-doc """
Path to the nested command. First element is always the `progname`, subsequent
elements are nested command names.
""".
-type cmd_path() :: [string()].
%% Command path, for nested commands

-export_type([arg_type/0, argument_help/0, argument/0,
    command/0, handler/0, cmd_path/0, arg_map/0, args/0]).

-doc """
Returned from [`parse/2,3`](`parse/3`) when the user input cannot be parsed
according to the command specification.

First element is the path to the command that was considered when the parser
detected an error. Second element, `Expected`, is the argument specification
that caused an error. It could be `undefined`, meaning that `Actual` argument
had no corresponding specification in the arguments list for the current
command.

When `Actual` is set to `undefined`, it means that a required argument is
missing from the command line. If both `Expected` and `Actual` have values, it
means validation error.

Use `format_error/1` to generate a human-readable error description, unless
there is a need to provide localised error messages.
""".
-type parser_error() :: {Path :: cmd_path(),
    Expected :: argument() | undefined,
    Actual :: string() | undefined,
    Details :: unicode:chardata()}.
%% Returned from `parse/2,3' when command spec is valid, but the command line
%% cannot be parsed using the spec.
%% When `Expected' is undefined, but `Actual' is not, it means that the input contains
%% an unexpected argument which cannot be parsed according to command spec.
%% When `Expected' is an argument, and `Actual' is undefined, it means that a mandatory
%% argument is not provided in the command line.
%% When both `Expected' and `Actual' are defined, it means that the supplied argument
%% is failing validation.
%% When both are `undefined', there is some logical issue (e.g. a sub-command is required,
%% but was not selected).

-doc """
Options changing parser behaviour.

- **`prefixes`** - Changes the option prefix (the default is `-`).

- **`default`** - Specifies the default value for all optional arguments. When
  this field is set, resulting argument map will contain all argument names.
  Useful for easy pattern matching on the argument map in the handler function.

- **`progname`** - Specifies the program (root command) name. Returned as the
  first element of the command path, and printed in help/usage text. It is
  recommended to have this value set, otherwise the default one is determined
  with `init:get_argument(progname)` and is often set to `erl` instead of the
  actual script name.

- **`command`** - Specifies the path to the nested command for `help/2`. Useful
  to limit output for complex utilities with multiple commands, and used by the
  default error handling logic.

- **`columns`** - Specifies the help/usage text width (characters) for `help/2`.
  Default value is 80.
""".
-type parser_options() :: #{
    %% allowed prefixes (default is [$-]).
    prefixes => [char()],
    %% default value for all missing optional arguments
    default => term(),
    %% root command name (program name)
    progname => string() | atom(),
    %% considered by `help/2' only
    command => cmd_path(),      %% command to print the help for
    columns => pos_integer()    %% viewport width, in characters
}.
%% Parser options

-doc """
Returned from [`parse/2,3`](`parse/3`). Contains arguments extracted from the
command line, path to the nested command (if any), and a (potentially nested)
command specification that was considered when the parser finished successfully.
It is expected that the command contains a handler definition, that will be
called passing the argument map.
""".
-type parse_result() ::
    {ok, arg_map(), Path :: cmd_path(), command()} |
    {error, parser_error()}.
%% Parser result: argument map, path leading to successfully
%% matching command (contains only ["progname"] if there were
%% no subcommands matched), and a matching command.

%% @equiv validate(Command, #{})
-doc false.
-spec validate(command()) -> Progname :: string().
validate(Command) ->
    validate(Command, #{}).

%% @doc Validate command specification, taking Options into account.
%% Raises an error if the command specification is invalid.
-doc false.
-spec validate(command(), parser_options()) -> Progname :: string().
validate(Command, Options) ->
    Prog = executable(Options),
    is_list(Prog) orelse erlang:error(badarg, [Command, Options],
        [{error_info, #{cause => #{2 => <<"progname is not valid">>}}}]),
    Prefixes = maps:from_list([{P, true} || P <- maps:get(prefixes, Options, [$-])]),
    _ = validate_command([{Prog, Command}], Prefixes),
    Prog.

%% @equiv parse(Args, Command, #{})
-doc(#{equiv => parse/3}).
-doc(#{since => <<"OTP 26.0">>}).
-spec parse(args(), command()) -> parse_result().
parse(Args, Command) ->
    parse(Args, Command, #{}).

%% @doc Parses supplied arguments according to expected command specification.
%% @param Args command line arguments (e.g. `init:get_plain_arguments()')
%% @returns argument map, or argument map with deepest matched command
%%  definition.
-doc """
Parses command line arguments according to the command specification. Raises an
exception if the command specification is not valid. Use
[`erl_error:format_exception/3,4` ](`erl_error:format_exception/3`)to see a
friendlier message. Invalid command line input does not raise an exception, but
makes `parse/2,3` to return a tuple
[`{error, parser_error()}`](`t:parser_error/0`).

This function does not call command handler.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec parse(args(), command(), Options :: parser_options()) -> parse_result().
parse(Args, Command, Options) ->
    Prog = validate(Command, Options),
    %% use maps and not sets v2, because sets:is_element/2 cannot be used in guards (unlike is_map_key)
    Prefixes = maps:from_list([{P, true} || P <- maps:get(prefixes, Options, [$-])]),
    Args2 = [unicode:characters_to_list(Arg) || Arg <- Args],
    try
        parse_impl(Args2, merge_arguments(Prog, Command, init_parser(Prefixes, Command, Options)))
    catch
        %% Parser error may happen at any depth, and bubbling the error is really
        %% cumbersome. Use exceptions and catch it before returning from `parse/2,3' instead.
        throw:Reason ->
            {error, Reason}
    end.

%% @equiv help(Command, #{})
-doc(#{equiv => help/2}).
-doc(#{since => <<"OTP 26.0">>}).
-spec help(command()) -> string().
help(Command) ->
    help(Command, #{}).

%% @doc Returns help for Command formatted according to Options specified
-doc """
Generates help/usage information text for the command supplied, or any nested
command when `command` option is specified. Arguments are displayed in the same
order as specified in `Command`. Does not provide localisation. Expects
`progname` to be set, otherwise defaults to return value of
`init:get_argument(progname)`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec help(command(), parser_options()) -> unicode:chardata().
help(Command, Options) ->
    Prog = validate(Command, Options),
    format_help({Prog, Command}, Options).

%% @doc
-doc """
Parses command line arguments and calls the matching command handler. Prints
human-readable error, help/usage information for the discovered command, and
halts the emulator with code 1 if there is any error in the command
specification or user-provided command line input.

> #### Warning {: .warning }
>
> This function is designed to work as an entry point to a standalone
> [`escript`](`e:erts:escript_cmd.md`). Therefore, it halts the emulator for any
> error detected. Do not use this function through remote procedure call, or it
> may result in an unexpected shutdown of a remote node.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec run(args(), command(), parser_options()) -> term().
run(Args, Command, Options) ->
    try parse(Args, Command, Options) of
        {ok, ArgMap, Path, SubCmd} ->
            handle(Command, ArgMap, tl(Path), SubCmd);
        {error, Reason} ->
            io:format("error: ~ts~n", [argparse:format_error(Reason)]),
            io:format("~ts", [argparse:help(Command, Options#{command => tl(element(1, Reason))})]),
            erlang:halt(1)
    catch
        error:Reason:Stack ->
            io:format(erl_error:format_exception(error, Reason, Stack)),
            erlang:halt(1)
    end.

%% @doc Basic formatter for the parser error reason.
-doc """
Generates human-readable text for [`parser error`](`t:parser_error/0`). Does not
include help/usage information, and does not provide localisation.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec format_error(Reason :: parser_error()) -> unicode:chardata().
format_error({Path, undefined, undefined, Details}) ->
    io_lib:format("~ts: ~ts", [format_path(Path), Details]);
format_error({Path, undefined, Actual, Details}) ->
    io_lib:format("~ts: unknown argument: ~ts~ts", [format_path(Path), Actual, Details]);
format_error({Path, #{name := Name}, undefined, Details}) ->
    io_lib:format("~ts: required argument missing: ~ts~ts", [format_path(Path), Name, Details]);
format_error({Path, #{name := Name}, Value, Details}) ->
    io_lib:format("~ts: invalid argument for ~ts: ~ts ~ts", [format_path(Path), Name, Value, Details]).

-type validator_error() ::
    {?MODULE, command | argument, cmd_path(), Field :: atom(), Detail :: unicode:chardata()}.

%% @doc Transforms exception thrown by `validate/1,2' according to EEP54.
%% Use `erl_error:format_exception/3,4' to get the shell-like output.
-doc false.
-spec format_error(Reason :: validator_error(), erlang:stacktrace()) -> map().
format_error({?MODULE, command, Path, Field, Reason}, [{_M, _F, [Cmd], Info} | _]) ->
    #{cause := Cause} = proplists:get_value(error_info, Info, #{}),
    Cause#{general => <<"command specification is invalid">>, 1 => io_lib:format("~tp", [Cmd]),
        reason => io_lib:format("command \"~ts\": invalid field '~ts', reason: ~ts", [format_path(Path), Field, Reason])};
format_error({?MODULE, argument, Path, Field, Reason}, [{_M, _F, [Arg], Info} | _]) ->
    #{cause := Cause} = proplists:get_value(error_info, Info, #{}),
    ArgName = maps:get(name, Arg, ""),
    Cause#{general => "argument specification is invalid", 1 => io_lib:format("~tp", [Arg]),
        reason => io_lib:format("command \"~ts\", argument '~ts', invalid field '~ts': ~ts",
            [format_path(Path), ArgName, Field, Reason])}.

%%--------------------------------------------------------------------
%% Parser implementation

%% Parser state (not available via API)
-record(eos, {
    %% prefix character map, by default, only -
    prefixes :: #{char() => true},
    %% argument map to be returned
    argmap = #{} :: arg_map(),
    %% sub-commands, in reversed orders, allowing to recover the path taken
    commands = [] :: cmd_path(),
    %% command being matched
    current :: command(),
    %% unmatched positional arguments, in the expected match order
    pos = [] :: [argument()],
    %% expected optional arguments, mapping between short/long form and an argument
    short = #{} :: #{integer() => argument()},
    long = #{} :: #{string() => argument()},
    %% flag, whether there are no options that can be confused with negative numbers
    no_digits = true :: boolean(),
    %% global default for not required arguments
    default :: error | {ok, term()}
}).

init_parser(Prefixes, Cmd, Options) ->
    #eos{prefixes = Prefixes, current = Cmd, default = maps:find(default, Options)}.

%% Optional or positional argument?
-define(IS_OPTION(Arg), is_map_key(short, Arg) orelse is_map_key(long, Arg)).

%% helper function to match either a long form of "--arg=value", or just "--arg"
match_long(Arg, LongOpts) ->
    case maps:find(Arg, LongOpts) of
        {ok, Option} ->
            {ok, Option};
        error ->
            %% see if there is '=' equals sign in the Arg
            case string:split(Arg, "=") of
                [MaybeLong, Value] ->
                    case maps:find(MaybeLong, LongOpts) of
                        {ok, Option} ->
                            {ok, Option, Value};
                        error ->
                            nomatch
                    end;
                _ ->
                    nomatch
            end
    end.

%% parse_impl implements entire internal parse logic.

%% Clause: option starting with any prefix
%% No separate clause for single-character short form, because there could be a single-character
%%  long form taking precedence.
parse_impl([[Prefix | Name] | Tail], #eos{prefixes = Pref} = Eos) when is_map_key(Prefix, Pref) ->
    %% match "long" option from the list of currently known
    case match_long(Name, Eos#eos.long) of
        {ok, Option} ->
            consume(Tail, Option, Eos);
        {ok, Option, Value} ->
            consume([Value | Tail], Option, Eos);
        nomatch ->
            %% try to match single-character flag
            case Name of
                [Flag] when is_map_key(Flag, Eos#eos.short) ->
                    %% found a flag
                    consume(Tail, maps:get(Flag, Eos#eos.short), Eos);
                [Flag | Rest] when is_map_key(Flag, Eos#eos.short) ->
                    %% can be a combination of flags, or flag with value,
                    %%  but can never be a negative integer, because otherwise
                    %%  it will be reflected in no_digits
                    case abbreviated(Name, [], Eos#eos.short) of
                        false ->
                            %% short option with Rest being an argument
                            consume([Rest | Tail], maps:get(Flag, Eos#eos.short), Eos);
                        Expanded ->
                            %% expand multiple flags into actual list, adding prefix
                            parse_impl([[Prefix,E] || E <- Expanded] ++ Tail, Eos)
                    end;
                MaybeNegative when Prefix =:= $-, Eos#eos.no_digits ->
                    case is_digits(MaybeNegative) of
                        true ->
                            %% found a negative number
                            parse_positional([Prefix|Name], Tail, Eos);
                        false ->
                            catch_all_positional([[Prefix|Name] | Tail], Eos)
                    end;
                _Unknown ->
                    catch_all_positional([[Prefix|Name] | Tail], Eos)
            end
    end;

%% Arguments not starting with Prefix: attempt to match sub-command, if available
parse_impl([Positional | Tail], #eos{current = #{commands := SubCommands}} = Eos) ->
    case maps:find(Positional, SubCommands) of
        error ->
            %% sub-command not found, try positional argument
            parse_positional(Positional, Tail, Eos);
        {ok, SubCmd} ->
            %% found matching sub-command with arguments, descend into it
            parse_impl(Tail, merge_arguments(Positional, SubCmd, Eos))
    end;

%% Clause for arguments that don't have sub-commands (therefore check for
%%  positional argument).
parse_impl([Positional | Tail], Eos) ->
    parse_positional(Positional, Tail, Eos);

%% Entire command line has been matched, go over missing arguments,
%%  add defaults etc
parse_impl([], #eos{argmap = ArgMap0, commands = Commands, current = Current, pos = Pos, default = Def} = Eos) ->
    %% error if stopped at sub-command with no handler
    map_size(maps:get(commands, Current, #{})) >0 andalso
        (not is_map_key(handler, Current)) andalso
        throw({Commands, undefined, undefined, <<"subcommand expected">>}),

    %% go over remaining positional, verify they are all not required
    ArgMap1 = fold_args_map(Commands, true, ArgMap0, Pos, Def),
    %% go over optionals, and either raise an error, or set default
    ArgMap2 = fold_args_map(Commands, false, ArgMap1, maps:values(Eos#eos.short), Def),
    ArgMap3 = fold_args_map(Commands, false, ArgMap2, maps:values(Eos#eos.long), Def),

    %% return argument map, command path taken, and the deepest
    %%  last command matched (usually it contains a handler to run)
    {ok, ArgMap3, Eos#eos.commands, Eos#eos.current}.

%% Generate error for missing required argument, and supply defaults for
%%  missing optional arguments that have defaults.
fold_args_map(Commands, Req, ArgMap, Args, GlobalDefault) ->
    lists:foldl(
        fun (#{name := Name}, Acc) when is_map_key(Name, Acc) ->
                %% argument present
                Acc;
            (#{required := true} = Opt, _Acc) ->
                %% missing, and required explicitly
                throw({Commands, Opt, undefined, <<>>});
            (#{name := Name, required := false, default := Default}, Acc) ->
                %% explicitly not required argument with default
                Acc#{Name => Default};
            (#{name := Name, required := false}, Acc) ->
                %% explicitly not required with no local default, try global one
                try_global_default(Name, Acc, GlobalDefault);
            (#{name := Name, default := Default}, Acc) when Req =:= true ->
                %% positional argument with default
                Acc#{Name => Default};
            (Opt, _Acc) when Req =:= true ->
                %% missing, for positional argument, implicitly required
                throw({Commands, Opt, undefined, <<>>});
            (#{name := Name, default := Default}, Acc) ->
                %% missing, optional, and there is a default
                Acc#{Name => Default};
            (#{name := Name}, Acc) ->
                %% missing, optional, no local default, try global default
                try_global_default(Name, Acc, GlobalDefault)
        end, ArgMap, Args).

try_global_default(_Name, Acc, error) ->
    Acc;
try_global_default(Name, Acc, {ok, Term}) ->
    Acc#{Name => Term}.

%%--------------------------------------------------------------------
%% argument consumption (nargs) handling

catch_all_positional(Tail, #eos{pos = [#{nargs := all} = Opt]} = Eos) ->
    action([], Tail, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);
%% it is possible that some positional arguments are not required,
%%  and therefore it is possible to catch all skipping those
catch_all_positional(Tail, #eos{argmap = Args, pos = [#{name := Name, default := Default, required := false} | Pos]} = Eos) ->
    catch_all_positional(Tail, Eos#eos{argmap = Args#{Name => Default}, pos = Pos});
%% same as above, but no default specified
catch_all_positional(Tail, #eos{pos = [#{required := false} | Pos]} = Eos) ->
    catch_all_positional(Tail, Eos#eos{pos = Pos});
catch_all_positional([Arg | _Tail], #eos{commands = Commands}) ->
    throw({Commands, undefined, Arg, <<>>}).

parse_positional(Arg, _Tail, #eos{pos = [], commands = Commands}) ->
    throw({Commands, undefined, Arg, <<>>});
parse_positional(Arg, Tail, #eos{pos = Pos} = Eos) ->
    %% positional argument itself is a value
    consume([Arg | Tail], hd(Pos), Eos).

%% Adds CmdName to path, and includes any arguments found there
merge_arguments(CmdName, #{arguments := Args} = SubCmd, Eos) ->
    add_args(Args, Eos#eos{current = SubCmd, commands = Eos#eos.commands ++ [CmdName]});
merge_arguments(CmdName, SubCmd, Eos) ->
    Eos#eos{current = SubCmd, commands = Eos#eos.commands ++ [CmdName]}.

%% adds arguments into current set of discovered pos/opts
add_args([], Eos) ->
    Eos;
add_args([#{short := S, long := L} = Option | Tail], #eos{short = Short, long = Long} = Eos) ->
    %% remember if this option can be confused with negative number
    NoDigits = no_digits(Eos#eos.no_digits, Eos#eos.prefixes, S, L),
    add_args(Tail, Eos#eos{short = Short#{S => Option}, long = Long#{L => Option}, no_digits = NoDigits});
add_args([#{short := S} = Option | Tail], #eos{short = Short} = Eos) ->
    %% remember if this option can be confused with negative number
    NoDigits = no_digits(Eos#eos.no_digits, Eos#eos.prefixes, S, 0),
    add_args(Tail, Eos#eos{short = Short#{S => Option}, no_digits = NoDigits});
add_args([#{long := L} = Option | Tail], #eos{long = Long} = Eos) ->
    %% remember if this option can be confused with negative number
    NoDigits = no_digits(Eos#eos.no_digits, Eos#eos.prefixes, 0, L),
    add_args(Tail, Eos#eos{long = Long#{L => Option}, no_digits = NoDigits});
add_args([PosOpt | Tail], #eos{pos = Pos} = Eos) ->
    add_args(Tail, Eos#eos{pos = Pos ++ [PosOpt]}).

%% If no_digits is still true, try to find out whether it should turn false,
%%  because added options look like negative numbers, and prefixes include -
no_digits(false, _, _, _) ->
    false;
no_digits(true, Prefixes, _, _) when not is_map_key($-, Prefixes) ->
    true;
no_digits(true, _, Short, _) when Short >= $0, Short =< $9 ->
    false;
no_digits(true, _, _, Long) ->
    not is_digits(Long).

%%--------------------------------------------------------------------
%% additional functions for optional arguments processing

%% Returns true when option (!) description passed requires a positional argument,
%%  hence cannot be treated as a flag.
requires_argument(#{nargs := {'maybe', _Term}}) ->
    false;
requires_argument(#{nargs := 'maybe'}) ->
    false;
requires_argument(#{nargs := _Any}) ->
    true;
requires_argument(Opt) ->
    case maps:get(action, Opt, store) of
        store ->
            maps:get(type, Opt, string) =/= boolean;
        append ->
            maps:get(type, Opt, string) =/= boolean;
        _ ->
            false
    end.

%% Attempts to find if passed list of flags can be expanded
abbreviated([Last], Acc, AllShort) when is_map_key(Last, AllShort) ->
    lists:reverse([Last | Acc]);
abbreviated([_], _Acc, _Eos) ->
    false;
abbreviated([Flag | Tail], Acc, AllShort) ->
    case maps:find(Flag, AllShort) of
        error ->
            false;
        {ok, Opt} ->
            case requires_argument(Opt) of
                true ->
                    false;
                false ->
                    abbreviated(Tail, [Flag | Acc], AllShort)
            end
    end.

%%--------------------------------------------------------------------
%% argument consumption (nargs) handling

%% consume predefined amount (none of which can be an option?)
consume(Tail, #{nargs := Count} = Opt, Eos) when is_integer(Count) ->
    {Consumed, Remain} = split_to_option(Tail, Count, Eos, []),
    length(Consumed) < Count andalso
        throw({Eos#eos.commands, Opt, Tail,
            io_lib:format("expected ~b, found ~b argument(s)", [Count, length(Consumed)])}),
    action(Remain, Consumed, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% handle 'reminder' by just dumping everything in
consume(Tail, #{nargs := all} = Opt, Eos) ->
    action([], Tail, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% require at least one argument
consume(Tail, #{nargs := nonempty_list} = Opt, Eos) ->
    {Consumed, Remains} = split_to_option(Tail, -1, Eos, []),
    Consumed =:= [] andalso throw({Eos#eos.commands, Opt, Tail, <<"expected argument">>}),
    action(Remains, Consumed, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% consume all until next option
consume(Tail, #{nargs := list} = Opt, Eos) ->
    {Consumed, Remains} = split_to_option(Tail, -1, Eos, []),
    action(Remains, Consumed, Opt#{type => {list, maps:get(type, Opt, string)}}, Eos);

%% maybe consume one, maybe not...
%% special cases for 'boolean maybe', only consume 'true' and 'false'
consume(["true" | Tail], #{type := boolean} = Opt, Eos) ->
    action(Tail, true, Opt#{type => raw}, Eos);
consume(["false" | Tail], #{type := boolean} = Opt, Eos) ->
    action(Tail, false, Opt#{type => raw}, Eos);
consume(Tail, #{type := boolean} = Opt, Eos) ->
    %% neither true nor false means 'undefined' (with the default for boolean being true)
    action(Tail, undefined, Opt, Eos);

%% maybe behaviour, as '?'
consume(Tail, #{nargs := 'maybe'} = Opt, Eos) ->
    case split_to_option(Tail, 1, Eos, []) of
        {[], _} ->
            %% no argument given, produce default argument (if not present,
            %%  then produce default value of the specified type)
            action(Tail, default(Opt), Opt#{type => raw}, Eos);
        {[Consumed], Remains} ->
            action(Remains, Consumed, Opt, Eos)
    end;

%% maybe consume one, maybe not...
consume(Tail, #{nargs := {'maybe', Const}} = Opt, Eos) ->
    case split_to_option(Tail, 1, Eos, []) of
        {[], _} ->
            action(Tail, Const, Opt, Eos);
        {[Consumed], Remains} ->
            action(Remains, Consumed, Opt, Eos)
    end;

%% default case, which depends on action
consume(Tail, #{action := count} = Opt, Eos) ->
    action(Tail, undefined, Opt, Eos);

%% for {store, ...} and {append, ...} don't take argument out
consume(Tail, #{action := {Act, _Const}} = Opt, Eos) when Act =:= store; Act =:= append ->
    action(Tail, undefined, Opt, Eos);

%% optional: ensure not to consume another option start
consume([[Prefix | _] = ArgValue | Tail], Opt, Eos) when ?IS_OPTION(Opt), is_map_key(Prefix, Eos#eos.prefixes) ->
    case Eos#eos.no_digits andalso is_digits(ArgValue) of
        true ->
            action(Tail, ArgValue, Opt, Eos);
        false ->
            throw({Eos#eos.commands, Opt, undefined, <<"expected argument">>})
    end;

consume([ArgValue | Tail], Opt, Eos) ->
    action(Tail, ArgValue, Opt, Eos);

%% we can only be here if it's optional argument, but there is no value supplied,
%%  and type is not 'boolean' - this is an error!
consume([], Opt, Eos) ->
    throw({Eos#eos.commands, Opt, undefined, <<"expected argument">>}).

%% no more arguments for consumption, but last optional may still be action-ed
%%consume([], Current, Opt, Eos) ->
%%    action([], Current, undefined, Opt, Eos).

%% smart split: ignore arguments that can be parsed as negative numbers,
%%  unless there are arguments that look like negative numbers
split_to_option([], _, _Eos, Acc) ->
    {lists:reverse(Acc), []};
split_to_option(Tail, 0, _Eos, Acc) ->
    {lists:reverse(Acc), Tail};
split_to_option([[Prefix | _] = MaybeNumber | Tail] = All, Left,
    #eos{no_digits = true, prefixes = Prefixes} = Eos, Acc) when is_map_key(Prefix, Prefixes) ->
    case is_digits(MaybeNumber) of
        true ->
            split_to_option(Tail, Left - 1, Eos, [MaybeNumber | Acc]);
        false ->
            {lists:reverse(Acc), All}
    end;
split_to_option([[Prefix | _] | _] = All, _Left,
    #eos{no_digits = false, prefixes = Prefixes}, Acc) when is_map_key(Prefix, Prefixes) ->
    {lists:reverse(Acc), All};
split_to_option([Head | Tail], Left, Opts, Acc) ->
    split_to_option(Tail, Left - 1, Opts, [Head | Acc]).

%%--------------------------------------------------------------------
%% Action handling

action(Tail, ArgValue, #{name := ArgName, action := store} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, Opt, Eos),
    continue_parser(Tail,  Opt, Eos#eos{argmap = ArgMap#{ArgName => Value}});

action(Tail, undefined, #{name := ArgName, action := {store, Value}} = Opt, #eos{argmap = ArgMap} = Eos) ->
    continue_parser(Tail,  Opt, Eos#eos{argmap = ArgMap#{ArgName => Value}});

action(Tail, ArgValue, #{name := ArgName, action := append} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, Opt, Eos),
    continue_parser(Tail,  Opt, Eos#eos{argmap = ArgMap#{ArgName => maps:get(ArgName, ArgMap, []) ++ [Value]}});

action(Tail, undefined, #{name := ArgName, action := {append, Value}} = Opt, #eos{argmap = ArgMap} = Eos) ->
    continue_parser(Tail,  Opt, Eos#eos{argmap = ArgMap#{ArgName => maps:get(ArgName, ArgMap, []) ++ [Value]}});

action(Tail, ArgValue, #{name := ArgName, action := extend} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, Opt, Eos),
    Extended = maps:get(ArgName, ArgMap, []) ++ Value,
    continue_parser(Tail, Opt, Eos#eos{argmap = ArgMap#{ArgName => Extended}});

action(Tail, _, #{name := ArgName, action := count} = Opt, #eos{argmap = ArgMap} = Eos) ->
    continue_parser(Tail,  Opt, Eos#eos{argmap = ArgMap#{ArgName => maps:get(ArgName, ArgMap, 0) + 1}});

%% default action is `store' (important to sync the code with the first clause above)
action(Tail, ArgValue, #{name := ArgName} = Opt, #eos{argmap = ArgMap} = Eos) ->
    Value = convert_type(maps:get(type, Opt, string), ArgValue, Opt, Eos),
    continue_parser(Tail,  Opt, Eos#eos{argmap = ArgMap#{ArgName => Value}}).

%% pop last positional, unless nargs is list/nonempty_list
continue_parser(Tail, Opt, Eos) when ?IS_OPTION(Opt) ->
    parse_impl(Tail, Eos);
continue_parser(Tail, #{nargs := List}, Eos) when List =:= list; List =:= nonempty_list ->
    parse_impl(Tail, Eos);
continue_parser(Tail, _Opt, Eos) ->
    parse_impl(Tail, Eos#eos{pos = tl(Eos#eos.pos)}).

%%--------------------------------------------------------------------
%% Type conversion

%% Handle "list" variant for nargs returning list
convert_type({list, Type}, Arg, Opt, Eos) ->
    [convert_type(Type, Var, Opt, Eos) || Var <- Arg];

%% raw - no conversion applied (most likely default)
convert_type(raw, Arg, _Opt, _Eos) ->
    Arg;

%% Handle actual types
convert_type(string, Arg, _Opt, _Eos) ->
    Arg;
convert_type({string, Choices}, Arg, Opt, Eos) when is_list(Choices), is_list(hd(Choices)) ->
    lists:member(Arg, Choices) orelse
        throw({Eos#eos.commands, Opt, Arg, <<"is not one of the choices">>}),
    Arg;
convert_type({string, Re}, Arg, Opt, Eos) ->
    case re:run(Arg, Re) of
        {match, _X} -> Arg;
        _ -> throw({Eos#eos.commands, Opt, Arg, <<"does not match">>})
    end;
convert_type({string, Re, ReOpt}, Arg, Opt, Eos) ->
    case re:run(Arg, Re, ReOpt) of
        match -> Arg;
        {match, _} -> Arg;
        _ -> throw({Eos#eos.commands, Opt, Arg, <<"does not match">>})
    end;
convert_type(integer, Arg, Opt, Eos) ->
    get_int(Arg, Opt, Eos);
convert_type({integer, Opts}, Arg, Opt, Eos) ->
    minimax(get_int(Arg, Opt, Eos), Opts, Eos, Opt, Arg);
convert_type(boolean, "true", _Opt, _Eos) ->
    true;
convert_type(boolean, undefined, _Opt, _Eos) ->
    true;
convert_type(boolean, "false", _Opt, _Eos) ->
    false;
convert_type(boolean, Arg, Opt, Eos) ->
    throw({Eos#eos.commands, Opt, Arg, <<"is not a boolean">>});
convert_type(binary, Arg, _Opt, _Eos) ->
    unicode:characters_to_binary(Arg);
convert_type({binary, Choices}, Arg, Opt, Eos) when is_list(Choices), is_binary(hd(Choices)) ->
    Conv = unicode:characters_to_binary(Arg),
    lists:member(Conv, Choices) orelse
        throw({Eos#eos.commands, Opt, Arg, <<"is not one of the choices">>}),
    Conv;
convert_type({binary, Re}, Arg, Opt, Eos) ->
    case re:run(Arg, Re) of
        {match, _X} -> unicode:characters_to_binary(Arg);
        _ -> throw({Eos#eos.commands, Opt, Arg, <<"does not match">>})
    end;
convert_type({binary, Re, ReOpt}, Arg, Opt, Eos) ->
    case re:run(Arg, Re, ReOpt) of
        match -> unicode:characters_to_binary(Arg);
        {match, _} -> unicode:characters_to_binary(Arg);
        _ -> throw({Eos#eos.commands, Opt, Arg, <<"does not match">>})
    end;
convert_type(float, Arg, Opt, Eos) ->
    get_float(Arg, Opt, Eos);
convert_type({float, Opts}, Arg, Opt, Eos) ->
    minimax(get_float(Arg, Opt, Eos), Opts, Eos, Opt, Arg);
convert_type(atom, Arg, Opt, Eos) ->
    try list_to_existing_atom(Arg)
    catch error:badarg ->
        throw({Eos#eos.commands, Opt, Arg, <<"is not an existing atom">>})
    end;
convert_type({atom, unsafe}, Arg, _Opt, _Eos) ->
    list_to_atom(Arg);
convert_type({atom, Choices}, Arg, Opt, Eos) ->
    try
        Atom = list_to_existing_atom(Arg),
        lists:member(Atom, Choices) orelse throw({Eos#eos.commands, Opt, Arg, <<"is not one of the choices">>}),
        Atom
    catch error:badarg ->
        throw({Eos#eos.commands, Opt, Arg, <<"is not an existing atom">>})
    end;
convert_type({custom, Fun}, Arg, Opt, Eos) ->
    try Fun(Arg)
    catch error:badarg ->
        throw({Eos#eos.commands, Opt, Arg, <<"failed validation">>})
    end.

%% Given Var, and list of {min, X}, {max, Y}, ensure that
%%  value falls within defined limits.
minimax(Var, [], _Eos, _Opt, _Orig) ->
    Var;
minimax(Var, [{min, Min} | _], Eos, Opt, Orig) when Var < Min ->
    throw({Eos#eos.commands, Opt, Orig, <<"is less than accepted minimum">>});
minimax(Var, [{max, Max} | _], Eos, Opt, Orig) when Var > Max ->
    throw({Eos#eos.commands, Opt, Orig, <<"is greater than accepted maximum">>});
minimax(Var, [Num | Tail], Eos, Opt, Orig) when is_number(Num) ->
    lists:member(Var, [Num|Tail]) orelse
        throw({Eos#eos.commands, Opt, Orig, <<"is not one of the choices">>}),
    Var;
minimax(Var, [_ | Tail], Eos, Opt, Orig) ->
    minimax(Var, Tail, Eos, Opt, Orig).

%% returns integer from string, or errors out with debugging info
get_int(Arg, Opt, Eos) ->
    case string:to_integer(Arg) of
        {Int, []} ->
            Int;
        _ ->
            throw({Eos#eos.commands, Opt, Arg, <<"is not an integer">>})
    end.

%% returns float from string, that is floating-point, or integer
get_float(Arg, Opt, Eos) ->
    case string:to_float(Arg) of
        {Float, []} ->
            Float;
        _ ->
            %% possibly in disguise
            case string:to_integer(Arg) of
                {Int, []} ->
                    Int;
                _ ->
                    throw({Eos#eos.commands, Opt, Arg, <<"is not a number">>})
            end
    end.

%% Returns 'true' if String can be converted to a number
is_digits(String) ->
    case string:to_integer(String) of
        {_Int, []} ->
            true;
        {_, _} ->
            case string:to_float(String) of
                {_Float, []} ->
                    true;
                {_, _} ->
                    false
            end
    end.

%% 'maybe' nargs for an option that does not have default set still have
%%  to produce something, let's call it hardcoded default.
default(#{default := Default}) ->
    Default;
default(#{type := boolean}) ->
    true;
default(#{type := integer}) ->
    0;
default(#{type := float}) ->
    0.0;
default(#{type := string}) ->
    "";
default(#{type := binary}) ->
    <<"">>;
default(#{type := atom}) ->
    undefined;
%% no type given, consider it 'undefined' atom
default(_) ->
    undefined.

%% command path is now in direct order
format_path(Commands) ->
    lists:join(" ", Commands).

%%--------------------------------------------------------------------
%% Validation and preprocessing
%% Theoretically, Dialyzer should do that too.
%% Practically, so many people ignore Dialyzer and then spend hours
%%  trying to understand why things don't work, that is makes sense
%%  to provide a mini-Dialyzer here.

%% to simplify throwing errors with the right reason
-define (INVALID(Kind, Entity, Path, Field, Text),
    erlang:error({?MODULE, Kind, clean_path(Path), Field, Text}, [Entity], [{error_info, #{cause => #{}}}])).

executable(#{progname := Prog}) when is_atom(Prog) ->
    atom_to_list(Prog);
executable(#{progname := Prog}) when is_binary(Prog) ->
    binary_to_list(Prog);
executable(#{progname := Prog}) ->
    Prog;
executable(_) ->
    {ok, [[Prog]]} = init:get_argument(progname),
    Prog.

%% Recursive command validator
validate_command([{Name, Cmd} | _] = Path, Prefixes) ->
    (is_list(Name) andalso (not is_map_key(hd(Name), Prefixes))) orelse
        ?INVALID(command, Cmd, tl(Path), commands,
            <<"command name must be a string not starting with option prefix">>),
    is_map(Cmd) orelse
        ?INVALID(command, Cmd, Path, commands, <<"expected command()">>),
    is_valid_command_help(maps:get(help, Cmd, [])) orelse
        ?INVALID(command, Cmd, Path, help, <<"must be a printable unicode list, or a command help template">>),
    is_map(maps:get(commands, Cmd, #{})) orelse
        ?INVALID(command, Cmd, Path, commands, <<"expected map of #{string() => command()}">>),
    case maps:get(handler, Cmd, optional) of
        optional -> ok;
        {Mod, ModFun} when is_atom(Mod), is_atom(ModFun) -> ok; %% map form
        {Mod, ModFun, _} when is_atom(Mod), is_atom(ModFun) -> ok; %% positional form
        {Fun, _} when is_function(Fun) -> ok; %% positional form
        Fun when is_function(Fun, 1) -> ok;
        _ -> ?INVALID(command, Cmd, Path, handler, <<"handler must be a valid callback, or an atom 'optional'">>)
    end,
    Cmd1 =
        case maps:find(arguments, Cmd) of
            error ->
                Cmd;
            {ok, Opts} when not is_list(Opts) ->
                ?INVALID(command, Cmd, Path, arguments, <<"expected a list, [argument()]">>);
            {ok, Opts} ->
                Cmd#{arguments => [validate_option(Path, Opt) || Opt <- Opts]}
        end,
    %% collect all short & long option identifiers - to figure out any conflicts
    lists:foldl(
        fun ({_, #{arguments := Opts}}, Acc) ->
            lists:foldl(
                fun (#{short := Short, name := OName} = Arg, {AllS, AllL}) ->
                        is_map_key(Short, AllS) andalso
                            ?INVALID(argument, Arg, Path, short,
                                "short conflicting with previously defined short for "
                                    ++ atom_to_list(maps:get(Short, AllS))),
                        {AllS#{Short => OName}, AllL};
                    (#{long := Long, name := OName} = Arg, {AllS, AllL}) ->
                        is_map_key(Long, AllL) andalso
                            ?INVALID(argument, Arg, Path, long,
                                "long conflicting with previously defined long for "
                                    ++ atom_to_list(maps:get(Long, AllL))),
                        {AllS, AllL#{Long => OName}};
                    (_, AccIn) ->
                        AccIn
                end, Acc, Opts);
            (_, Acc) ->
                Acc
        end, {#{}, #{}}, Path),
    %% verify all sub-commands
    case maps:find(commands, Cmd1) of
        error ->
            {Name, Cmd1};
        {ok, Sub} ->
            {Name, Cmd1#{commands => maps:map(
                fun (K, V) ->
                    {K, Updated} = validate_command([{K, V} | Path], Prefixes),
                    Updated
                end, Sub)}}
    end.

%% validates option spec
validate_option(Path, #{name := Name} = Arg) when is_atom(Name); is_list(Name); is_binary(Name) ->
    %% verify specific arguments
    %% help: string, 'hidden', or a tuple of {string(), ...}
    is_valid_option_help(maps:get(help, Arg, [])) orelse
        ?INVALID(argument, Arg, Path, help, <<"must be a string or valid help template">>),
    io_lib:printable_unicode_list(maps:get(long, Arg, [])) orelse
        ?INVALID(argument, Arg, Path, long, <<"must be a printable string">>),
    is_boolean(maps:get(required, Arg, true)) orelse
        ?INVALID(argument, Arg, Path, required, <<"must be a boolean">>),
    io_lib:printable_unicode_list([maps:get(short, Arg, $a)]) orelse
        ?INVALID(argument, Arg, Path, short, <<"must be a printable character">>),
    Opt1 = maybe_validate(action, Arg, fun validate_action/3, Path),
    Opt2 = maybe_validate(type, Opt1, fun validate_type/3, Path),
    maybe_validate(nargs, Opt2, fun validate_args/3, Path);
validate_option(Path, Arg) ->
    ?INVALID(argument, Arg, Path, name, <<"argument must be a map containing 'name' field">>).

maybe_validate(Key, Map, Fun, Path) when is_map_key(Key, Map) ->
    maps:put(Key, Fun(maps:get(Key, Map), Path, Map), Map);
maybe_validate(_Key, Map, _Fun, _Path) ->
    Map.

%% validate action field
validate_action(store, _Path, _Opt) ->
    store;
validate_action({store, Term}, _Path, _Opt) ->
    {store, Term};
validate_action(append, _Path, _Opt) ->
    append;
validate_action({append, Term}, _Path, _Opt) ->
    {append, Term};
validate_action(count, _Path, _Opt) ->
    count;
validate_action(extend, _Path, #{nargs := Nargs}) when
    Nargs =:= list; Nargs =:= nonempty_list; Nargs =:= all; is_integer(Nargs) ->
    extend;
validate_action(extend, _Path, #{type := {custom, _}}) ->
    extend;
validate_action(extend, Path, Arg) ->
    ?INVALID(argument, Arg, Path, action, <<"extend action works only with lists">>);
validate_action(_Action, Path, Arg) ->
    ?INVALID(argument, Arg, Path, action, <<"unsupported">>).

%% validate type field
validate_type(Simple, _Path, _Opt) when Simple =:= boolean; Simple =:= integer; Simple =:= float;
    Simple =:= string; Simple =:= binary; Simple =:= atom; Simple =:= {atom, unsafe} ->
    Simple;
validate_type({custom, Fun}, _Path, _Opt) when is_function(Fun, 1) ->
    {custom, Fun};
validate_type({float, Opts}, Path, Arg) ->
    [?INVALID(argument, Arg, Path, type, <<"invalid validator">>)
        || {Kind, Val} <- Opts, (Kind =/= min andalso Kind =/= max) orelse (not is_float(Val))],
    {float, Opts};
validate_type({integer, Opts}, Path, Arg) ->
    [?INVALID(argument, Arg, Path, type, <<"invalid validator">>)
        || {Kind, Val} <- Opts, (Kind =/= min andalso Kind =/= max) orelse (not is_integer(Val))],
    {integer, Opts};
validate_type({atom, Choices} = Valid, Path, Arg) when is_list(Choices) ->
    [?INVALID(argument, Arg, Path, type, <<"unsupported">>) || C <- Choices, not is_atom(C)],
    Valid;
validate_type({string, Re} = Valid, _Path, _Opt) when is_list(Re) ->
    Valid;
validate_type({string, Re, L} = Valid, _Path, _Opt) when is_list(Re), is_list(L) ->
    Valid;
validate_type({binary, Re} = Valid, _Path, _Opt) when is_binary(Re) ->
    Valid;
validate_type({binary, Choices} = Valid, _Path, _Opt) when is_list(Choices), is_binary(hd(Choices)) ->
    Valid;
validate_type({binary, Re, L} = Valid, _Path, _Opt) when is_binary(Re), is_list(L) ->
    Valid;
validate_type(_Type, Path, Arg) ->
    ?INVALID(argument, Arg, Path, type, <<"unsupported">>).

validate_args(N, _Path, _Opt) when is_integer(N), N >= 1 -> N;
validate_args(Simple, _Path, _Opt) when Simple =:= all; Simple =:= list; Simple =:= 'maybe'; Simple =:= nonempty_list ->
    Simple;
validate_args({'maybe', Term}, _Path, _Opt) -> {'maybe', Term};
validate_args(_Nargs, Path, Arg) ->
    ?INVALID(argument, Arg, Path, nargs, <<"unsupported">>).

%% used to throw an error - strips command component out of path
clean_path(Path) ->
    {Cmds, _} = lists:unzip(Path),
    lists:reverse(Cmds).

is_valid_option_help(hidden) ->
    true;
is_valid_option_help(Help) when is_list(Help); is_binary(Help) ->
    true;
is_valid_option_help({Short, Desc}) when is_list(Short) orelse is_binary(Short), is_list(Desc) ->
    %% verify that Desc is a list of string/type/default
    lists:all(fun(type) -> true;
                 (default) -> true;
                 (S) when is_list(S); is_binary(S) -> true;
                 (_) -> false
              end, Desc);
is_valid_option_help({Short, Desc}) when is_list(Short) orelse is_binary(Short), is_function(Desc, 0) ->
    true;
is_valid_option_help(_) ->
    false.

is_valid_command_help(hidden) ->
    true;
is_valid_command_help(Help) when is_binary(Help) ->
    true;
is_valid_command_help(Help) when is_list(Help) ->
    %% allow printable lists
    case io_lib:printable_unicode_list(Help) of
        true ->
            true;
        false ->
            %% ... or a command help template
            lists:all(
                fun (Atom) when Atom =:= usage; Atom =:= commands; Atom =:= arguments; Atom =:= options -> true;
                    (Bin) when is_binary(Bin) -> true;
                    (Str) -> io_lib:printable_unicode_list(Str)
                end, Help)
    end;
is_valid_command_help(_) ->
    false.

%%--------------------------------------------------------------------
%% Built-in Help formatter

format_help({ProgName, Root}, Format) ->
    Prefix = hd(maps:get(prefixes, Format, [$-])),
    Nested0 = maps:get(command, Format, [ProgName]),
    %% The command path should always start with the progname, that's why it is
    %% dropped here to keep the command and sub-commands only.
    %%
    %% However, earlier versions of this function did not drop that progname.
    %% The function thus used to crash with a badkey excception if the caller
    %% passed the `CmdPath' returned by `parse/2' to this function's `command'.
    %% Therefore, to keep backward compatibility, if the command path does not
    %% start with the progname, it uses the entire list untouched.
    Nested = case Nested0 of
                 [ProgName | Tail] ->
                     Tail;
                 _ ->
                     Nested0
             end,
    %% descent into commands collecting all options on the way
    {_CmdName, Cmd, AllArgs} = collect_options(ProgName, Root, Nested, []),
    %% split arguments into Flags, Options, Positional, and create help lines
    {_, Longest, Flags, Opts, Args, OptL, PosL} = lists:foldl(fun format_opt_help/2,
        {Prefix, 0, "", [], [], [], []}, AllArgs),
    %% collect and format sub-commands
    Immediate = maps:get(commands, Cmd, #{}),
    {Long, Subs} = maps:fold(
        fun (_Name, #{help := hidden}, {Long, SubAcc}) ->
            {Long, SubAcc};
            (Name, Sub, {Long, SubAcc}) ->
            Help = maps:get(help, Sub, ""),
            {max(Long, string:length(Name)), [{Name, Help}|SubAcc]}
        end, {Longest, []}, maps:iterator(Immediate, ordered)),
    %% format sub-commands
    ShortCmd0 =
        case map_size(Immediate) of
            0 ->
                [];
            Small when Small < 4 ->
                Keys = lists:sort(maps:keys(Immediate)),
                ["{" ++ lists:append(lists:join("|", Keys)) ++ "}"];
            _Largs ->
                ["<command>"]
        end,
    %% was it nested command?
    ShortCmd = if Nested =:= [] -> ShortCmd0; true -> [lists:append(lists:join(" ", Nested)) | ShortCmd0] end,
    %% format flags
    FlagsForm = if Flags =:= [] -> [];
                    true -> [unicode:characters_to_list(io_lib:format("[~tc~ts]", [Prefix, Flags]))]
                end,
    %% format extended view
    %% usage line has hardcoded format for now
    Usage = [ProgName, ShortCmd, FlagsForm, Opts, Args],
    %% format usage according to help template
    Template0 = get_help(Root, Nested),
    %% when there is no help defined for the command, or help is a string,
    %% use the default format (original argparse behaviour)
    Template =
        case Template0 =:= "" orelse io_lib:printable_unicode_list(Template0) of
            true ->
                %% classic/compatibility format
                NL = [io_lib:nl()],
                Template1 = ["Usage:" ++ NL, usage, NL],
                Template2 = maybe_add("~n", Template0, Template0 ++ NL, Template1),
                Template3 = maybe_add("~nSubcommands:~n", Subs, commands, Template2),
                Template4 = maybe_add("~nArguments:~n", PosL, arguments, Template3),
                maybe_add("~nOptional arguments:~n", OptL, options, Template4);
            false ->
                Template0
        end,

    %% produce formatted output, taking viewport width into account
    Parts = #{usage => Usage, commands => {Long, Subs},
        arguments => {Longest, PosL}, options => {Longest, OptL}},
    Width = maps:get(columns, Format, 80), %% might also use io:columns() here
    lists:append([format_width(maps:find(Part, Parts), Part, Width) || Part <- Template]).

%% collects options on the Path, and returns found Command
collect_options(CmdName, Command, [], Args) ->
    {CmdName, Command, Args ++ maps:get(arguments, Command, [])};
collect_options(CmdName, Command, [Cmd|Tail], Args) ->
    Sub = maps:get(commands, Command),
    SubCmd = maps:get(Cmd, Sub),
    collect_options(CmdName ++ " " ++ Cmd, SubCmd, Tail, Args ++ maps:get(arguments, Command, [])).

%% gets help for sub-command
get_help(#{help := hidden}, []) ->
    "";
get_help(Command, []) ->
    case maps:get(help, Command, "") of
        Help when is_binary(Help) -> unicode:characters_to_list(Help);
        Help -> Help
    end;
get_help(Command, [Cmd|Tail]) ->
    Sub = maps:get(commands, Command),
    SubCmd = maps:get(Cmd, Sub),
    get_help(SubCmd, Tail).

%% conditionally adds text and empty lines
maybe_add(_ToAdd, [], _Element, Template) ->
    Template;
maybe_add(ToAdd, _List, Element, Template) ->
    Template ++ [io_lib:format(ToAdd, []), Element].

format_width(error, Part, Width) ->
    wrap_text(Part, 0, Width);
format_width({ok, [ProgName, ShortCmd, FlagsForm, Opts, Args]}, usage, Width) ->
    %% make every separate command/option to be a "word", and then
    %% wordwrap it indented by the ProgName length + 3
    Words = ShortCmd ++ FlagsForm ++ Opts ++ Args,
    if Words =:= [] -> io_lib:format("  ~ts", [ProgName]);
        true ->
            Indent = string:length(ProgName),
            Wrapped = wordwrap(Words, Width - Indent, 0, [], []),
            Pad = lists:append(lists:duplicate(Indent + 3, " ")),
            ArgLines = lists:join([io_lib:nl() | Pad], Wrapped),
            io_lib:format("  ~ts~ts", [ProgName, ArgLines])
    end;
format_width({ok, {Len, Texts}}, _Part, Width) ->
    SubFormat = io_lib:format("  ~~-~bts ~~ts~n", [Len]),
    [io_lib:format(SubFormat, [N, wrap_text(D, Len + 3, Width)]) || {N, D} <- lists:reverse(Texts)].

wrap_text(Text, Indent, Width) ->
    %% split text into separate lines (paragraphs)
    NL = io_lib:nl(),
    Lines = string:split(Text, NL, all),
    %% wordwrap every paragraph
    Paragraphs = lists:append([wrap_line(L, Width, Indent) || L <- Lines]),
    Pad = lists:append(lists:duplicate(Indent, " ")),
    lists:join([NL | Pad], Paragraphs).

wrap_line([], _Width, _Indent) ->
    [[]];
wrap_line(Line, Width, Indent) ->
    [First | Tail] = string:split(Line, " ", all),
    wordwrap(Tail, Width - Indent, string:length(First), First, []).

wordwrap([], _Max, _Len, [], Lines) ->
    lists:reverse(Lines);
wordwrap([], _Max, _Len, Line, Lines) ->
    lists:reverse([Line | Lines]);
wordwrap([Word | Tail], Max, Len, Line, Lines) ->
    WordLen = string:length(Word),
    case Len + 1 + WordLen > Max of
        true ->
            wordwrap(Tail, Max, WordLen, Word, [Line | Lines]);
        false ->
            wordwrap(Tail, Max, WordLen + 1 + Len, [Line, <<" ">>, Word], Lines)
    end.

%% create help line for every option, collecting together all flags, short options,
%%  long options, and positional arguments

%% format optional argument
format_opt_help(#{help := hidden}, Acc) ->
    Acc;
format_opt_help(Opt, {Prefix, Longest, Flags, Opts, Args, OptL, PosL}) when ?IS_OPTION(Opt) ->
    Desc = format_description(Opt),
    %% does it need an argument? look for nargs and action
    RequiresArg = requires_argument(Opt),
    %% long form always added to Opts
    NonOption = maps:get(required, Opt, false) =:= true,
    {Name0, MaybeOpt0} =
        case maps:find(long, Opt) of
            error ->
                {"", []};
            {ok, Long} when NonOption, RequiresArg ->
                FN = [Prefix | Long],
                {FN, [format_required(true, [FN, " "], Opt)]};
            {ok, Long} when RequiresArg ->
                FN = [Prefix | Long],
                {FN, [format_required(false, [FN, " "], Opt)]};
            {ok, Long} when NonOption ->
                FN = [Prefix | Long],
                {FN, [FN]};
            {ok, Long} ->
                FN = [Prefix | Long],
                {FN, [io_lib:format("[~ts]", [FN])]}
        end,
    %% short may go to flags, or Opts
    {Name, MaybeFlag, MaybeOpt1} =
        case maps:find(short, Opt) of
            error ->
                {Name0, [], MaybeOpt0};
            {ok, Short} when RequiresArg ->
                SN = [Prefix, Short],
                {maybe_concat(SN, Name0), [],
                    [format_required(NonOption, [SN, " "], Opt) | MaybeOpt0]};
            {ok, Short} ->
                {maybe_concat([Prefix, Short], Name0), [Short], MaybeOpt0}
        end,
    %% apply override for non-default usage (in form of {Quick, Advanced} tuple
    MaybeOpt2 =
        case maps:find(help, Opt) of
            {ok, {Str, _}} ->
                [Str];
            _ ->
                MaybeOpt1
        end,
    %% name length, capped at 24
    NameLen = string:length(Name),
    Capped = min(24, NameLen),
    {Prefix, max(Capped, Longest), Flags ++ MaybeFlag, Opts ++ MaybeOpt2, Args, [{Name, Desc} | OptL], PosL};

%% format positional argument
format_opt_help(#{name := Name} = Opt, {Prefix, Longest, Flags, Opts, Args, OptL, PosL}) ->
    Desc = format_description(Opt),
    %% positional, hence required
    LName = io_lib:format("~ts", [Name]),
    LPos = case maps:find(help, Opt) of
               {ok, {Str, _}} ->
                   Str;
               _ ->
                   format_required(maps:get(required, Opt, true), "", Opt)
           end,
    {Prefix, max(Longest, string:length(LName)), Flags, Opts, Args ++ [LPos], OptL, [{LName, Desc} | PosL]}.

%% custom format
format_description(#{help := {_Short, Fun}}) when is_function(Fun, 0) ->
    Fun();
format_description(#{help := {_Short, Desc}} = Opt) ->
    lists:map(
        fun (type) ->
                format_type(Opt);
            (default) ->
                format_default(Opt);
            (String) ->
                String
        end, Desc
    );
%% default format:
%%     "desc"
%%     "desc (type)"
%%     "desc, default: abc"
%%     "desc (type), default: abc"
format_description(#{name := Name} = Opt) ->
    NameStr = maps:get(help, Opt, io_lib:format("~ts", [Name])),
    case {NameStr, format_type(Opt), format_default(Opt)} of
        {"", "", Type} -> Type;
        {"", Default, ""} -> Default;
        {Desc, "", ""} -> Desc;
        {Desc, "", Default} -> [Desc, " , default: ", Default];
        {Desc, Type, ""} -> [Desc, " (", Type, ")"];
        {"", Type, Default} -> [Type, ", default: ", Default];
        {Desc, Type, Default} -> [Desc, " (", Type, "), default: ", Default]
    end.

%% option formatting helpers
maybe_concat(No, []) -> No;
maybe_concat(No, L) -> [No, ", ", L].

format_required(true, Extra, #{name := Name} = Opt) ->
    io_lib:format("~ts<~ts>~ts", [Extra, Name, format_nargs(Opt)]);
format_required(false, Extra, #{name := Name} = Opt) ->
    io_lib:format("[~ts<~ts>~ts]", [Extra, Name, format_nargs(Opt)]).

format_nargs(#{nargs := Dots}) when Dots =:= list; Dots =:= all; Dots =:= nonempty_list ->
    "...";
format_nargs(_) ->
    "".

format_type(#{type := {integer, Choices}}) when is_list(Choices), is_integer(hd(Choices)) ->
    io_lib:format("choice: ~s", [lists:join(", ", [integer_to_list(C) || C <- Choices])]);
format_type(#{type := {float, Choices}}) when is_list(Choices), is_number(hd(Choices)) ->
    io_lib:format("choice: ~s", [lists:join(", ", [io_lib:format("~g", [C]) || C <- Choices])]);
format_type(#{type := {Num, Valid}}) when Num =:= integer; Num =:= float ->
    case {proplists:get_value(min, Valid), proplists:get_value(max, Valid)} of
        {undefined, undefined} ->
            io_lib:format("~s", [format_type(#{type => Num})]);
        {Min, undefined} ->
            io_lib:format("~s >= ~tp", [format_type(#{type => Num}), Min]);
        {undefined, Max} ->
            io_lib:format("~s <= ~tp", [format_type(#{type => Num}), Max]);
        {Min, Max} ->
            io_lib:format("~tp <= ~s <= ~tp", [Min, format_type(#{type => Num}), Max])
    end;
format_type(#{type := {string, Re, _}}) when is_list(Re), not is_list(hd(Re)) ->
    io_lib:format("string re: ~ts", [Re]);
format_type(#{type := {string, Re}}) when is_list(Re), not is_list(hd(Re)) ->
    io_lib:format("string re: ~ts", [Re]);
format_type(#{type := {binary, Re}}) when is_binary(Re) ->
    io_lib:format("binary re: ~ts", [Re]);
format_type(#{type := {binary, Re, _}}) when is_binary(Re) ->
    io_lib:format("binary re: ~ts", [Re]);
format_type(#{type := {StrBin, Choices}}) when StrBin =:= string orelse StrBin =:= binary, is_list(Choices) ->
    io_lib:format("choice: ~ts", [lists:join(", ", Choices)]);
format_type(#{type := atom}) ->
    "existing atom";
format_type(#{type := {atom, unsafe}}) ->
    "atom";
format_type(#{type := {atom, Choices}}) ->
    io_lib:format("choice: ~ts", [lists:join(", ", [atom_to_list(C) || C <- Choices])]);
format_type(#{type := boolean}) ->
    "";
format_type(#{type := integer}) ->
    "int";
format_type(#{type := Type}) when is_atom(Type) ->
    io_lib:format("~ts", [Type]);
format_type(_Opt) ->
    "".

format_default(#{default := Def}) when is_list(Def); is_binary(Def); is_atom(Def) ->
    io_lib:format("~ts", [Def]);
format_default(#{default := Def}) ->
    io_lib:format("~tp", [Def]);
format_default(_) ->
    "".

%%--------------------------------------------------------------------
%% Basic handler execution
handle(CmdMap, ArgMap, Path, #{handler := {Mod, ModFun, Default}}) ->
    ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Mod, ModFun, ArgList);
handle(_CmdMap, ArgMap, _Path, #{handler := {Mod, ModFun}}) when is_atom(Mod), is_atom(ModFun) ->
    Mod:ModFun(ArgMap);
handle(CmdMap, ArgMap, Path, #{handler := {Fun, Default}}) when is_function(Fun) ->
    ArgList = arg_map_to_arg_list(CmdMap, Path, ArgMap, Default),
    %% if argument count may not match, better error can be produced
    erlang:apply(Fun, ArgList);
handle(_CmdMap, ArgMap, _Path, #{handler := Handler}) when is_function(Handler, 1) ->
    Handler(ArgMap).

%% Given command map, path to reach a specific command, and a parsed argument
%%  map, returns a list of arguments (effectively used to transform map-based
%%  callback handler into positional).
arg_map_to_arg_list(Command, Path, ArgMap, Default) ->
    AllArgs = collect_arguments(Command, Path, []),
    [maps:get(Arg, ArgMap, Default) || #{name := Arg} <- AllArgs].

%% recursively descend into Path, ignoring arguments with duplicate names
collect_arguments(Command, [], Acc) ->
    Acc ++ maps:get(arguments, Command, []);
collect_arguments(Command, [H|Tail], Acc) ->
    Args = maps:get(arguments, Command, []),
    Next = maps:get(H, maps:get(commands, Command, H)),
    collect_arguments(Next, Tail, Acc ++ Args).
