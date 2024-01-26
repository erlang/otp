%%
%%
%% Copyright Maxim Fedorov
%%
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

-module(argparse).
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

-type argument_help() :: {
    unicode:chardata(), %% short form, printed in command usage, e.g. "[--dir <dirname>]", developer is
                        %% responsible for proper formatting (e.g. adding <>, dots... and so on)
    [unicode:chardata() | type | default] | fun(() -> unicode:chardata())
}.
%% Help template definition for argument. Short and long forms exist for every argument.
%% Short form is printed together with command definition, e.g. "usage: rm [--force]",
%%  while long description is printed in detailed section below: "--force   forcefully remove".

-type argument_name() :: atom() | string() | binary().

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

-type arg_map() :: #{argument_name() => term()}.
%% Arguments map: argument name to a term, produced by parser. Supplied to the command handler

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

-type command_help() :: [unicode:chardata() | usage | commands | arguments | options].
%% Template for the command help/usage message.

%% Command descriptor
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

-type cmd_path() :: [string()].
%% Command path, for nested commands

-export_type([arg_type/0, argument_help/0, argument/0,
    command/0, handler/0, cmd_path/0, arg_map/0]).

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

-type parse_result() ::
    {ok, arg_map(), Path :: cmd_path(), command()} |
    {error, parser_error()}.
%% Parser result: argument map, path leading to successfully
%% matching command (contains only ["progname"] if there were
%% no subcommands matched), and a matching command.

%% @equiv validate(Command, #{})
-spec validate(command()) -> Progname :: string().
validate(Command) ->
    validate(Command, #{}).

%% @doc Validate command specification, taking Options into account.
%% Raises an error if the command specification is invalid.
-spec validate(command(), parser_options()) -> Progname :: string().
validate(Command, Options) ->
    Prog = executable(Options),
    is_list(Prog) orelse erlang:error(badarg, [Command, Options],
        [{error_info, #{cause => #{2 => <<"progname is not valid">>}}}]),
    Prefixes = maps:from_list([{P, true} || P <- maps:get(prefixes, Options, [$-])]),
    _ = validate_command([{Prog, Command}], Prefixes),
    Prog.

%% @equiv parse(Args, Command, #{})
-spec parse(Args :: [string()], command()) -> parse_result().
parse(Args, Command) ->
    parse(Args, Command, #{}).

%% @doc Parses supplied arguments according to expected command specification.
%% @param Args command line arguments (e.g. `init:get_plain_arguments()')
%% @returns argument map, or argument map with deepest matched command
%%  definition.
-spec parse(Args :: [string()], command(), Options :: parser_options()) -> parse_result().
parse(Args, Command, Options) ->
    Prog = validate(Command, Options),
    %% use maps and not sets v2, because sets:is_element/2 cannot be used in guards (unlike is_map_key)
    Prefixes = maps:from_list([{P, true} || P <- maps:get(prefixes, Options, [$-])]),
    try
        parse_impl(Args, merge_arguments(Prog, Command, init_parser(Prefixes, Command, Options)))
    catch
        %% Parser error may happen at any depth, and bubbling the error is really
        %% cumbersome. Use exceptions and catch it before returning from `parse/2,3' instead.
        throw:Reason ->
            {error, Reason}
    end.

%% @equiv help(Command, #{})
-spec help(command()) -> string().
help(Command) ->
    help(Command, #{}).

%% @doc Returns help for Command formatted according to Options specified
-spec help(command(), parser_options()) -> unicode:chardata().
help(Command, Options) ->
    Prog = validate(Command, Options),
    format_help({Prog, Command}, Options).

%% @doc
-spec run(Args :: [string()], command(), parser_options()) -> term().
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
    Nested = maps:get(command, Format, []),
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
    Template0 = maps:get(help, Root, ""),
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
