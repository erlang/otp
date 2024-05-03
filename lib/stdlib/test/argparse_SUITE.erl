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

-module(argparse_SUITE).
-author("maximfca@gmail.com").

-export([suite/0, all/0, groups/0]).

-export([
    readme/0, readme/1,
    basic/0, basic/1,
    long_form_eq/0, long_form_eq/1,
    built_in_types/0, built_in_types/1,
    type_validators/0, type_validators/1,
    invalid_arguments/0, invalid_arguments/1,
    complex_command/0, complex_command/1,
    unicode/0, unicode/1,
    parser_error/0, parser_error/1,
    nargs/0, nargs/1,
    argparse/0, argparse/1,
    negative/0, negative/1,
    nodigits/0, nodigits/1,
    pos_mixed_with_opt/0, pos_mixed_with_opt/1,
    default_for_not_required/0, default_for_not_required/1,
    global_default/0, global_default/1,
    subcommand/0, subcommand/1,
    very_short/0, very_short/1,
    multi_short/0, multi_short/1,
    proxy_arguments/0, proxy_arguments/1,

    usage/0, usage/1,
    usage_required_args/0, usage_required_args/1,
    usage_template/0, usage_template/1,
    usage_args_ordering/0, usage_args_ordering/1,
    parser_error_usage/0, parser_error_usage/1,
    command_usage/0, command_usage/1,
    usage_width/0, usage_width/1,

    validator_exception/0, validator_exception/1,
    validator_exception_format/0, validator_exception_format/1,

    run_handle/0, run_handle/1,
    run_args_ordering/0, run_args_ordering/1
]).

-include_lib("stdlib/include/assert.hrl").

suite() ->
    [{timetrap, {seconds, 30}}].

groups() ->
    [
        {parser, [parallel], [
            readme, basic, long_form_eq, built_in_types, type_validators,
            invalid_arguments, complex_command, unicode, parser_error,
            nargs, argparse, negative, nodigits, pos_mixed_with_opt,
            default_for_not_required, global_default, subcommand,
            very_short, multi_short, proxy_arguments
        ]},
        {usage, [parallel], [
            usage, usage_required_args, usage_template, usage_args_ordering,
            parser_error_usage, command_usage, usage_width
        ]},
        {validator, [parallel], [
            validator_exception, validator_exception_format
        ]},
        {run, [parallel], [
            run_handle, run_args_ordering
        ]}
    ].

all() ->
    [{group, parser}, {group, validator}, {group, usage}].

%%--------------------------------------------------------------------
%% Helpers

prog() ->
    {ok, [[ProgStr]]} = init:get_argument(progname), ProgStr.

parser_error(CmdLine, CmdMap) ->
    {error, Reason} = parse(CmdLine, CmdMap),
    unicode:characters_to_list(argparse:format_error(Reason)).

parse_opts(Args, Opts) ->
    argparse:parse(string:lexemes(Args, " "), #{arguments => Opts}).

parse(Args, Command) ->
    argparse:parse(string:lexemes(Args, " "), Command).

parse_cmd(Args, Command) ->
    argparse:parse(string:lexemes(Args, " "), #{commands => Command}).

%% ubiquitous command, containing sub-commands, and all possible option types
%% with all nargs. Not all combinations though.
ubiq_cmd() ->
    #{
        arguments => [
            #{name => r, short => $r, type => boolean, help => "recursive"},
            #{name => f, short => $f, type => boolean, long => "-force", help => "force"},
            #{name => v, short => $v, type => boolean, action => count, help => "verbosity level"},
            #{name => interval, short => $i, type => {integer, [{min, 1}]}, help => "interval set"},
            #{name => weird, long => "-req", help => "required optional, right?"},
            #{name => float, long => "-float", type => float, default => 3.14, help => "floating-point long form argument"}
        ],
        commands => #{
            "start" => #{help => "verifies configuration and starts server",
                arguments => [
                    #{name => server, help => "server to start"},
                    #{name => shard, short => $s, type => integer, nargs => nonempty_list, help => "initial shards"},
                    #{name => part, short => $p, type => integer, nargs => list, help => hidden},
                    #{name => z, short => $z, type => {integer, [{min, 1}, {max, 10}]}, help => "between"},
                    #{name => l, short => $l, type => {integer, [{max, 10}]}, nargs => 'maybe', help => "maybe lower"},
                    #{name => more, short => $m, type => {integer, [{max, 10}]}, help => "less than 10"},
                    #{name => optpos, required => false, type => {integer, []}, help => "optional positional"},
                    #{name => bin, short => $b, type => {binary, <<"m">>}, help => "binary with re"},
                    #{name => g, short => $g, type => {binary, <<"m">>, []}, help => "binary with re"},
                    #{name => t, short => $t, type => {string, "m"}, help => "string with re"},
                    #{name => e, long => "--maybe-req", required => true, type => integer, nargs => 'maybe', help => "maybe required int"},
                    #{name => y, required => true, long => "-yyy", short => $y, type => {string, "m", []}, help => "string with re"},
                    #{name => u, short => $u, type => {string, ["1", "2"]}, help => "string choices"},
                    #{name => choice, short => $c, type => {integer, [1,2,3]}, help => "tough choice"},
                    #{name => fc, short => $q, type => {float, [2.1,1.2]}, help => "floating choice"},
                    #{name => ac, short => $w, type => {atom, [one, two]}, help => "atom choice"},
                    #{name => au, long => "-unsafe", type => {atom, unsafe}, help => "unsafe atom"},
                    #{name => as, long => "-safe", type => atom, help => <<"safe atom">>},
                    #{name => name, required => false, nargs => list, help => hidden},
                    #{name => long, long => "foobar", required => false, help => [<<"foobaring option">>]}
                ], commands => #{
                    "crawler" => #{arguments => [
                        #{name => extra, long => "--extra", help => "extra option very deep"}
                    ],
                        help => "controls crawler behaviour"},
                    "doze" => #{help => "dozes a bit"}}
            },
            "stop" => #{help => <<"stops running server">>, arguments => []
            },
            "status" => #{help => "prints server status", arguments => [],
                commands => #{
                    "crawler" => #{
                        arguments => [#{name => extra, long => "--extra", help => "extra option very deep"}],
                        help => "crawler status"}}
            },
            "restart" => #{help => hidden, arguments => [
                #{name => server, help => "server to restart"},
                #{name => duo, short => $d, long => "-duo", help => "dual option"}
            ]}
    }
    }.

%%--------------------------------------------------------------------
%% Parser Test Cases

readme() ->
    [{doc, "Test cases covered in the README"}].

readme(Config) when is_list(Config) ->
    Prog = prog(),
    Rm = #{
        arguments => [
            #{name => dir},
            #{name => force, short => $f, type => boolean, default => false},
            #{name => recursive, short => $r, type => boolean}
        ]
    },
    ?assertEqual({ok, #{dir => "dir", force => true, recursive => true}, [Prog], Rm},
        argparse:parse(["-rf", "dir"], Rm)),
    %% override progname
    ?assertEqual("Usage:\n  readme\n",
        unicode:characters_to_list(argparse:help(#{}, #{progname => "readme"}))),
    ?assertEqual("Usage:\n  readme\n",
        unicode:characters_to_list(argparse:help(#{}, #{progname => readme}))),
    ?assertEqual("Usage:\n  readme\n",
        unicode:characters_to_list(argparse:help(#{}, #{progname => <<"readme">>}))),
    %% test that command has priority over just a positional argument:
    %%  - parsing "opt sub" means "find positional argument "pos", then enter subcommand
    %%  - parsing "sub opt" means "enter sub-command, and then find positional argument"
    Cmd = #{
        commands => #{"sub" => #{}},
        arguments => [#{name => pos}]
    },
    ?assertEqual(parse("opt sub", Cmd), parse("sub opt", Cmd)).

basic() ->
    [{doc, "Basic cases"}].

basic(Config) when is_list(Config) ->
    Prog = prog(),
    %% empty command, with full options path
    ?assertMatch({ok, #{}, [Prog, "cmd"], #{}},
        argparse:parse(["cmd"], #{commands => #{"cmd" => #{}}})),
    %% sub-command, with no path, but user-supplied argument
    ?assertEqual({ok, #{}, [Prog, "cmd", "sub"], #{attr => pos}},
        argparse:parse(["cmd", "sub"], #{commands => #{"cmd" => #{commands => #{"sub" => #{attr => pos}}}}})),
    %% command with positional argument
    PosCmd = #{arguments => [#{name => pos}]},
    ?assertEqual({ok, #{pos => "arg"}, [Prog, "cmd"], PosCmd},
        argparse:parse(["cmd", "arg"], #{commands => #{"cmd" => PosCmd}})),
    %% command with optional argument
    OptCmd = #{arguments => [#{name => force, short => $f, type => boolean}]},
    ?assertEqual({ok, #{force => true}, [Prog, "rm"], OptCmd},
        parse(["rm -f"], #{commands => #{"rm" => OptCmd}}), "rm -f"),
    %% command with optional and positional argument
    PosOptCmd = #{arguments => [#{name => force, short => $f, type => boolean}, #{name => dir}]},
    ?assertEqual({ok, #{force => true, dir => "dir"}, [Prog, "rm"], PosOptCmd},
        parse(["rm -f dir"], #{commands => #{"rm" => PosOptCmd}}), "rm -f dir"),
    %% no command, just argument list
    KernelCmd = #{arguments => [#{name => kernel, long => "kernel", type => atom, nargs => 2}]},
    ?assertEqual({ok, #{kernel => [port, dist]}, [Prog], KernelCmd},
        parse(["-kernel port dist"], KernelCmd)),
    %% same but positional
    ArgListCmd = #{arguments => [#{name => arg, nargs => 2, type => boolean}]},
    ?assertEqual({ok, #{arg => [true, false]}, [Prog], ArgListCmd},
        parse(["true false"], ArgListCmd)).

long_form_eq() ->
    [{doc, "Tests that long form supports --arg=value"}].

long_form_eq(Config) when is_list(Config) ->
    Prog = prog(),
    %% cmd --arg=value
    PosOptCmd = #{arguments => [#{name => arg, long => "-arg"}]},
    ?assertEqual({ok, #{arg => "value"}, [Prog, "cmd"], PosOptCmd},
        parse(["cmd --arg=value"], #{commands => #{"cmd" => PosOptCmd}})),
    %% --integer=10
    ?assertMatch({ok, #{int := 10}, _, _},
        parse(["--int=10"], #{arguments => [#{name => int, type => integer, long => "-int"}]})).

built_in_types() ->
    [{doc, "Tests all built-in types supplied as a single argument"}].

% built-in types testing
built_in_types(Config) when is_list(Config) ->
    Prog = [prog()],
    Bool = #{arguments => [#{name => meta, type => boolean, short => $b, long => "-boolean"}]},
    ?assertEqual({ok, #{}, Prog, Bool}, parse([""], Bool)),
    ?assertEqual({ok, #{meta => true}, Prog, Bool}, parse(["-b"], Bool)),
    ?assertEqual({ok, #{meta => true}, Prog, Bool}, parse(["--boolean"], Bool)),
    ?assertEqual({ok, #{meta => false}, Prog, Bool}, parse(["--boolean false"], Bool)),
    %% integer tests
    Int = #{arguments => [#{name => int, type => integer, short => $i, long => "-int"}]},
    ?assertEqual({ok, #{int => 1}, Prog, Int}, parse([" -i 1"], Int)),
    ?assertEqual({ok, #{int => 1}, Prog, Int}, parse(["--int 1"], Int)),
    ?assertEqual({ok, #{int => -1}, Prog, Int}, parse(["-i -1"], Int)),
    %% floating point
    Float = #{arguments => [#{name => f, type => float, short => $f}]},
    ?assertEqual({ok, #{f => 44.44}, Prog, Float}, parse(["-f 44.44"], Float)),
    %% atoms, existing
    Atom = #{arguments => [#{name => atom, type => atom, short => $a, long => "-atom"}]},
    ?assertEqual({ok, #{atom => atom}, Prog, Atom}, parse(["-a atom"], Atom)),
    ?assertEqual({ok, #{atom => atom}, Prog, Atom}, parse(["--atom atom"], Atom)).

type_validators() ->
    [{doc, "Test that parser return expected conversions for valid arguments"}].

type_validators(Config) when is_list(Config) ->
    %% successful string regexes
    ?assertMatch({ok, #{str := "me"}, _, _},
        parse_opts("me", [#{name => str, type => {string, "m."}}])),
    ?assertMatch({ok, #{str := "me"}, _, _},
        parse_opts("me", [#{name => str, type => {string, "m.", []}}])),
    ?assertMatch({ok, #{"str" := "me"}, _, _},
        parse_opts("me", [#{name => "str", type => {string, "m.", [{capture, none}]}}])),
    %% and binary too...
    ?assertMatch({ok, #{bin := <<"me">>}, _, _},
        parse_opts("me", [#{name => bin, type => {binary, <<"m.">>}}])),
    ?assertMatch({ok, #{<<"bin">> := <<"me">>}, _, _},
        parse_opts("me", [#{name => <<"bin">>, type => {binary, <<"m.">>, []}}])),
    ?assertMatch({ok, #{bin := <<"me">>}, _, _},
        parse_opts("me", [#{name => bin, type => {binary, <<"m.">>, [{capture, none}]}}])),
    %% successful integer with range validators
    ?assertMatch({ok, #{int := 5}, _, _},
        parse_opts("5", [#{name => int, type => {integer, [{min, 0}, {max, 10}]}}])),
    ?assertMatch({ok, #{bin := <<"5">>}, _, _},
        parse_opts("5", [#{name => bin, type => binary}])),
    ?assertMatch({ok, #{str := "011"}, _, _},
        parse_opts("11", [#{name => str, type => {custom, fun(S) -> [$0|S] end}}])),
    %% choices: valid
    ?assertMatch({ok, #{bin := <<"K">>}, _, _},
        parse_opts("K", [#{name => bin, type => {binary, [<<"M">>, <<"K">>]}}])),
    ?assertMatch({ok, #{str := "K"}, _, _},
        parse_opts("K", [#{name => str, type => {string, ["K", "N"]}}])),
    ?assertMatch({ok, #{atom := one}, _, _},
        parse_opts("one", [#{name => atom, type => {atom, [one, two]}}])),
    ?assertMatch({ok, #{int := 12}, _, _},
        parse_opts("12", [#{name => int, type => {integer, [10, 12]}}])),
    ?assertMatch({ok, #{float := 1.3}, _, _},
        parse_opts("1.3", [#{name => float, type => {float, [1.3, 1.4]}}])),
    %% test for unsafe atom
    %% ensure the atom does not exist
    ?assertException(error, badarg, list_to_existing_atom("$can_never_be")),
    {ok, ArgMap, _, _} = parse_opts("$can_never_be", [#{name => atom, type => {atom, unsafe}}]),
    argparse:validate(#{arguments => [#{name => atom, type => {atom, unsafe}}]}),
    %% now that atom exists, because argparse created it (in an unsafe way!)
    ?assertEqual(list_to_existing_atom("$can_never_be"), maps:get(atom, ArgMap)),
    %% test successful user-defined conversion
    ?assertMatch({ok, #{user := "VER"}, _, _},
        parse_opts("REV", [#{name => user, type => {custom, fun (Str) -> lists:reverse(Str) end}}])).

invalid_arguments() ->
    [{doc, "Test that parser return errors for invalid arguments"}].

invalid_arguments(Config) when is_list(Config) ->
    %% {float, [{min, float()} | {max, float()}]} |
    Prog = [prog()],
    MinFloat = #{name => float, type => {float, [{min, 1.0}]}},
    ?assertEqual({error, {Prog, MinFloat, "0.0", <<"is less than accepted minimum">>}},
        parse_opts("0.0", [MinFloat])),
    MaxFloat = #{name => float, type => {float, [{max, 1.0}]}},
    ?assertEqual({error, {Prog, MaxFloat, "2.0", <<"is greater than accepted maximum">>}},
        parse_opts("2.0", [MaxFloat])),
    %% {int, [{min, integer()} | {max, integer()}]} |
    MinInt = #{name => int, type => {integer, [{min, 20}]}},
    ?assertEqual({error, {Prog, MinInt, "10", <<"is less than accepted minimum">>}},
        parse_opts("10", [MinInt])),
    MaxInt = #{name => int, type => {integer, [{max, -10}]}},
    ?assertEqual({error, {Prog, MaxInt, "-5", <<"is greater than accepted maximum">>}},
        parse_opts("-5", [MaxInt])),
    %% string: regex & regex with options
    %% {string, string()} | {string, string(), []}
    StrRegex = #{name => str, type => {string, "me.me"}},
    ?assertEqual({error, {Prog, StrRegex, "me", <<"does not match">>}},
        parse_opts("me", [StrRegex])),
    StrRegexOpt = #{name => str, type => {string, "me.me", []}},
    ?assertEqual({error, {Prog, StrRegexOpt, "me", <<"does not match">>}},
        parse_opts("me", [StrRegexOpt])),
    %% {binary, {re, binary()} | {re, binary(), []}
    BinRegex = #{name => bin, type => {binary, <<"me.me">>}},
    ?assertEqual({error, {Prog, BinRegex, "me", <<"does not match">>}},
        parse_opts("me", [BinRegex])),
    BinRegexOpt = #{name => bin, type => {binary, <<"me.me">>, []}},
    ?assertEqual({error, {Prog, BinRegexOpt, "me", <<"does not match">>}},
        parse_opts("me", [BinRegexOpt])),
    %% invalid integer (comma , is not parsed)
    ?assertEqual({error, {Prog, MinInt, "1,", <<"is not an integer">>}},
        parse_opts(["1,"], [MinInt])),
    %% test invalid choices
    BinChoices = #{name => bin, type => {binary, [<<"M">>, <<"N">>]}},
    ?assertEqual({error, {Prog, BinChoices, "K", <<"is not one of the choices">>}},
        parse_opts("K", [BinChoices])),
    StrChoices = #{name => str, type => {string, ["M", "N"]}},
    ?assertEqual({error, {Prog, StrChoices, "K", <<"is not one of the choices">>}},
        parse_opts("K", [StrChoices])),
    AtomChoices = #{name => atom, type => {atom, [one, two]}},
    ?assertEqual({error, {Prog, AtomChoices, "K", <<"is not one of the choices">>}},
        parse_opts("K", [AtomChoices])),
    IntChoices = #{name => int, type => {integer, [10, 11]}},
    ?assertEqual({error, {Prog, IntChoices, "12", <<"is not one of the choices">>}},
        parse_opts("12", [IntChoices])),
    FloatChoices = #{name => float, type => {float, [1.2, 1.4]}},
    ?assertEqual({error, {Prog, FloatChoices, "1.3", <<"is not one of the choices">>}},
        parse_opts("1.3", [FloatChoices])),
    %% unsuccessful user-defined conversion
    ?assertMatch({error, {Prog, _, "REV", <<"failed validation">>}},
        parse_opts("REV", [#{name => user, type => {custom, fun (Str) -> integer_to_binary(Str) end}}])).

complex_command() ->
    [{doc, "Parses a complex command that has a mix of optional and positional arguments"}].

complex_command(Config) when is_list(Config) ->
    Command = #{arguments => [
        %% options
        #{name => string, short => $s, long => "-string", action => append, help => "String list option"},
        #{name => boolean, type => boolean, short => $b, action => append, help => "Boolean list option"},
        #{name => float, type => float, short => $f, long => "-float", action => append, help => "Float option"},
        %% positional args
        #{name => integer, type => integer, help => "Integer variable"},
        #{name => string, help => "alias for string option", action => extend, nargs => list}
    ]},
    CmdMap = #{commands => #{"start" => Command}},
    Parsed = argparse:parse(string:lexemes("start --float 1.04 -f 112 -b -b -s s1 42 --string s2 s3 s4", " "), CmdMap),
    Expected = #{float => [1.04, 112], boolean => [true, true], integer => 42, string => ["s1", "s2", "s3", "s4"]},
    ?assertEqual({ok, Expected, [prog(), "start"], Command}, Parsed).

unicode() ->
    [{doc, "Tests basic unicode support"}].

unicode(Config) when is_list(Config) ->
    %% test unicode short & long
    ?assertMatch({ok, #{one := true}, _, _},
        parse(["-Ф"], #{arguments => [#{name => one, short => $Ф, type => boolean}]})),
    ?assertMatch({ok, #{long := true}, _, _},
        parse(["--åäö"], #{arguments => [#{name => long, long => "-åäö", type => boolean}]})),
    %% test default, help and value in unicode
    Cmd = #{arguments => [#{name => text, type => binary, help => "åäö", default => <<"★"/utf8>>}]},
    Expected = #{text => <<"★"/utf8>>},
    Prog = [prog()],
    ?assertEqual({ok, Expected, Prog, Cmd}, argparse:parse([], Cmd)), %% default
    ?assertEqual({ok, Expected, Prog, Cmd}, argparse:parse(["★"], Cmd)), %% specified in the command line
    ?assertEqual("Usage:\n  " ++ prog() ++ " <text>\n\nArguments:\n  text åäö (binary), default: ★\n",
        unicode:characters_to_list(argparse:help(Cmd))),
    %% test command name and argument name in unicode
    Uni = #{commands => #{"åäö" => #{help => "öФ"}}, handler => optional,
        arguments => [#{name => "Ф", short => $ä, long => "åäö"}]},
    UniExpected = "Usage:\n  " ++ prog() ++
        " {åäö} [-ä <Ф>] [-åäö <Ф>]\n\nSubcommands:\n  åäö      öФ\n\nOptional arguments:\n  -ä, -åäö Ф\n",
    ?assertEqual(UniExpected, unicode:characters_to_list(argparse:help(Uni))),
    ParsedExpected = #{"Ф" => "öФ"},
    ?assertEqual({ok, ParsedExpected, Prog, Uni}, argparse:parse(["-ä", "öФ"], Uni)).

parser_error() ->
    [{doc, "Tests error tuples that the parser returns"}].

parser_error(Config) when is_list(Config) ->
    Prog = prog(),
    %% unknown option at the top of the path
    ?assertEqual({error, {[Prog], undefined, "arg", <<>>}},
        parse_cmd(["arg"], #{})),
    %% positional argument missing in a sub-command
    Opt = #{name => mode, required => true},
    ?assertMatch({error, {[Prog, "start"], _, undefined, <<>>}},
        parse_cmd(["start"], #{"start" => #{arguments => [Opt]}})),
    %% optional argument missing in a sub-command
    Opt1 = #{name => mode, short => $o, required => true},
    ?assertMatch({error, {[Prog, "start"], _, undefined, <<>>}},
        parse_cmd(["start"], #{"start" => #{arguments => [Opt1]}})),
    %% positional argument: an atom that does not exist
    Opt2 = #{name => atom, type => atom},
    ?assertEqual({error, {[Prog], Opt2, "boo-foo", <<"is not an existing atom">>}},
        parse_opts(["boo-foo"], [Opt2])),
    %% optional argument missing some items
    Opt3 = #{name => kernel, long => "kernel", type => atom, nargs => 2},
    ?assertEqual({error, {[Prog], Opt3, ["port"], "expected 2, found 1 argument(s)"}},
        parse_opts(["-kernel port"], [Opt3])),
    %% positional argument missing some items
    Opt4 = #{name => arg, type => atom, nargs => 3},
    ?assertEqual({error, {[Prog], Opt4, ["p1"], "expected 3, found 1 argument(s)"}},
        parse_opts(["p1"], [Opt4])),
    %% short option with no argument, when it's needed
    ?assertMatch({error, {_, _, undefined, <<"expected argument">>}},
        parse("-1", #{arguments => [#{name => short49, short => 49}]})).

nargs() ->
    [{doc, "Tests argument consumption option, with nargs"}].

nargs(Config) when is_list(Config) ->
    Prog = [prog()],
    %% consume optional list arguments
    Opts = [
        #{name => arg, short => $s, nargs => list, type => integer},
        #{name => bool, short => $b, type => boolean}
    ],
    ?assertMatch({ok, #{arg := [1, 2, 3], bool := true}, _, _},
        parse_opts(["-s 1 2 3 -b"], Opts)),
    %% consume one_or_more arguments in an optional list
    Opts2 = [
        #{name => arg, short => $s, nargs => nonempty_list},
        #{name => extra, short => $x}
        ],
    ?assertMatch({ok, #{extra := "X", arg := ["a","b","c"]}, _, _},
        parse_opts(["-s port -s a b c -x X"], Opts2)),
    %% error if there is no argument to consume
    ?assertMatch({error, {_, _, ["-x"], <<"expected argument">>}},
        parse_opts(["-s -x"], Opts2)),
    %% error when positional has nargs = nonempty_list or pos_integer
    ?assertMatch({error, {_, _, undefined, <<>>}},
        parse_opts([""], [#{name => req, nargs => nonempty_list}])),
    %% positional arguments consumption: one or more positional argument
    OptsPos1 = #{arguments => [
        #{name => arg, nargs => nonempty_list},
        #{name => extra, short => $x}
    ]},
    ?assertEqual({ok, #{extra => "X", arg => ["b","c"]}, Prog, OptsPos1},
        parse(["-x port -x a b c -x X"], OptsPos1)),
    %% positional arguments consumption, any number (maybe zero)
    OptsPos2 = #{arguments => [
        #{name => arg, nargs => list},
        #{name => extra, short => $x}
    ]},
    ?assertEqual({ok, #{extra => "X", arg => ["a","b","c"]}, Prog, OptsPos2},
        parse(["-x port a b c -x X"], OptsPos2)),
    %% positional: consume ALL arguments!
    OptsAll = #{arguments => [
        #{name => arg, nargs => all},
        #{name => extra, short => $x}
    ]},
    ?assertEqual({ok, #{extra => "port", arg => ["a","b","c", "-x", "X"]}, Prog, OptsAll},
        parse(["-x port a b c -x X"], OptsAll)),
    %% maybe with a specified default
    OptMaybe = [
        #{name => foo, long => "-foo", nargs => {'maybe', c}, default => d},
        #{name => bar, nargs => 'maybe', default => d}
    ],
    ?assertMatch({ok, #{foo := "YY", bar := "XX"}, Prog, _},
        parse_opts(["XX --foo YY"], OptMaybe)),
    ?assertMatch({ok, #{foo := c, bar := "XX"}, Prog, _},
        parse_opts(["XX --foo"], OptMaybe)),
    ?assertMatch({ok, #{foo := d, bar := d}, Prog, _},
        parse_opts([""], OptMaybe)),
    %% maybe with default provided by argparse
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := ok}, _, _},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, default => ok} | OptMaybe])),
    %% maybe arg - with no default given
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := 0}, _, _},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => integer} | OptMaybe])),
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := ""}, _, _},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => string} | OptMaybe])),
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := undefined}, _, _},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => atom} | OptMaybe])),
    ?assertMatch({ok, #{foo := d, bar := "XX", baz := <<"">>}, _, _},
        parse_opts(["XX -b"], [#{name => baz, nargs => 'maybe', short => $b, type => binary} | OptMaybe])),
    %% nargs: optional list, yet it still needs to be 'not required'!
    OptList = [#{name => arg, nargs => list, required => false, type => integer}],
    ?assertEqual({ok, #{}, Prog, #{arguments => OptList}}, parse_opts("", OptList)),
    %% tests that action "count" with nargs "maybe" counts two times, first time
    %% consuming an argument (for "maybe"), second time just counting
    Cmd = #{arguments => [
        #{name => short49, short => $1, long => "-force", action => count, nargs => 'maybe'}]},
    ?assertEqual({ok, #{short49 => 2}, Prog, Cmd},
        parse("-1 arg1 --force", Cmd)).

argparse() ->
    [{doc, "Tests success cases, inspired by argparse in Python"}].

argparse(Config) when is_list(Config) ->
    Prog = [prog()],
    Parser = #{arguments => [
        #{name => sum, long => "-sum", action => {store, sum}, default => max},
        #{name => integers, type => integer, nargs => nonempty_list}
        ]},
    ?assertEqual({ok, #{integers => [1, 2, 3, 4], sum => max}, Prog, Parser},
        parse("1 2 3 4", Parser)),
    ?assertEqual({ok, #{integers => [1, 2, 3, 4], sum => sum}, Prog, Parser},
        parse("1 2 3 4 --sum", Parser)),
    ?assertEqual({ok, #{integers => [7, -1, 42], sum => sum}, Prog, Parser},
        parse("--sum 7 -1 42", Parser)),
    %% name or flags
    Parser2 = #{arguments => [
        #{name => bar, required => true},
        #{name => foo, short => $f, long => "-foo"}
    ]},
    ?assertEqual({ok, #{bar => "BAR"}, Prog, Parser2}, parse("BAR", Parser2)),
    ?assertEqual({ok, #{bar => "BAR", foo => "FOO"}, Prog, Parser2}, parse("BAR --foo FOO", Parser2)),
    %PROG: error: the following arguments are required: bar
    ?assertMatch({error, {Prog, _, undefined, <<>>}}, parse("--foo FOO", Parser2)),
    %% action tests: default
    ?assertMatch({ok, #{foo := "1"}, Prog, _},
        parse("--foo 1", #{arguments => [#{name => foo, long => "-foo"}]})),
    %% action test: store
    ?assertMatch({ok, #{foo := 42}, Prog, _},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", action => {store, 42}}]})),
    %% action tests: boolean (variants)
    ?assertMatch({ok, #{foo := true}, Prog, _},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", action => {store, true}}]})),
    ?assertMatch({ok, #{foo := 42}, Prog, _},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", type => boolean, action => {store, 42}}]})),
    ?assertMatch({ok, #{foo := true}, Prog, _},
        parse("--foo", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    ?assertMatch({ok, #{foo := true}, Prog, _},
        parse("--foo true", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    ?assertMatch({ok, #{foo := false}, Prog, _},
        parse("--foo false", #{arguments => [#{name => foo, long => "-foo", type => boolean}]})),
    %% action tests: append & append_const
    ?assertMatch({ok, #{all := [1, "1"]}, Prog, _},
        parse("--x 1 -x 1", #{arguments => [
            #{name => all, long => "-x", type => integer, action => append},
            #{name => all, short => $x, action => append}]})),
    ?assertMatch({ok, #{all := ["Z", 2]}, Prog, _},
        parse("--x -x", #{arguments => [
            #{name => all, long => "-x", action => {append, "Z"}},
            #{name => all, short => $x, action => {append, 2}}]})),
    %% count:
    ?assertMatch({ok, #{v := 3}, Prog, _},
        parse("-v -v -v", #{arguments => [#{name => v, short => $v, action => count}]})).

negative() ->
    [{doc, "Test negative number parser"}].

negative(Config) when is_list(Config) ->
    Parser = #{arguments => [
        #{name => x, short => $x, type => integer, action => store},
        #{name => foo, nargs => 'maybe', required => false}
    ]},
    ?assertMatch({ok, #{x := -1}, _, _}, parse("-x -1", Parser)),
    ?assertMatch({ok, #{x := -1, foo := "-5"}, _, _}, parse("-x -1 -5", Parser)),
    %%
    Parser2 = #{arguments => [
        #{name => one, short => $1},
        #{name => foo, nargs => 'maybe', required => false}
    ]},

    %% negative number options present, so -1 is an option
    ?assertMatch({ok, #{one := "X"}, _, _}, parse("-1 X", Parser2)),
    %% negative number options present, so -2 is an option
    ?assertMatch({error, {_, undefined, "-2", _}}, parse("-2", Parser2)),

    %% negative number options present, so both -1s are options
    ?assertMatch({error, {_, _, undefined, _}}, parse("-1 -1", Parser2)),
    %% no "-" prefix, can only be an integer
    ?assertMatch({ok, #{foo := "-1"}, _, _}, argparse:parse(["-1"], Parser2, #{prefixes => "+"})),
    %% no "-" prefix, can only be an integer, but just one integer!
    ?assertMatch({error, {_, undefined, "-1", _}},
        argparse:parse(["-2", "-1"], Parser2, #{prefixes => "+"})),
    %% just in case, floats work that way too...
    ?assertMatch({error, {_, undefined, "-2", _}},
        parse("-2", #{arguments => [#{name => one, long => "1.2"}]})).

nodigits() ->
    [{doc, "Test prefixes and negative numbers together"}].

nodigits(Config) when is_list(Config) ->
    %% verify nodigits working as expected
    Parser3 = #{arguments => [
        #{name => extra, short => $3},
        #{name => arg, nargs => list}
    ]},
    %% ensure not to consume optional prefix
    ?assertEqual({ok, #{extra => "X", arg => ["a","b","3"]}, [prog()], Parser3},
        argparse:parse(string:lexemes("-3 port a b 3 +3 X", " "), Parser3, #{prefixes => "-+"})).

pos_mixed_with_opt() ->
    [{doc, "Tests that optional argument correctly consumes expected argument"
        "inspired by https://github.com/python/cpython/issues/59317"}].

pos_mixed_with_opt(Config) when is_list(Config) ->
    Parser = #{arguments => [
        #{name => pos},
        #{name => opt, default => 24, type => integer, long => "-opt"},
        #{name => vars, nargs => list}
    ]},
    ?assertEqual({ok, #{pos => "1", opt => 8, vars => ["8", "9"]}, [prog()], Parser},
        parse("1 2 --opt 8 8 9", Parser)).

default_for_not_required() ->
    [{doc, "Tests that default value is used for non-required positional argument"}].

default_for_not_required(Config) when is_list(Config) ->
    ?assertMatch({ok, #{def := 1}, _, _},
        parse("", #{arguments => [#{name => def, short => $d, required => false, default => 1}]})),
    ?assertMatch({ok, #{def := 1}, _, _},
        parse("", #{arguments => [#{name => def, required => false, default => 1}]})).

global_default() ->
    [{doc, "Tests that a global default can be enabled for all non-required arguments"}].

global_default(Config) when is_list(Config) ->
    ?assertMatch({ok, #{def := "global"}, _, _},
        argparse:parse("", #{arguments => [#{name => def, type => integer, required => false}]},
        #{default => "global"})).

subcommand() ->
    [{doc, "Tests subcommands parser"}].

subcommand(Config) when is_list(Config) ->
    TwoCmd = #{arguments => [#{name => bar}]},
    Cmd = #{
        arguments => [#{name => force, type => boolean, short => $f}],
        commands => #{"one" => #{
            arguments => [#{name => foo, type => boolean, long => "-foo"}, #{name => baz}],
            commands => #{
                "two" => TwoCmd}}}},
    ?assertEqual({ok, #{force => true, baz => "N1O1O", foo => true, bar => "bar"}, [prog(), "one", "two"], TwoCmd},
        parse("one N1O1O -f two --foo bar", Cmd)),
    %% it is an error not to choose subcommand
    ?assertEqual({error, {[prog(), "one"], undefined, undefined, <<"subcommand expected">>}},
        parse("one N1O1O -f", Cmd)).

very_short() ->
    [{doc, "Tests short option appended to the optional itself"}].

very_short(Config) when is_list(Config) ->
    ?assertMatch({ok, #{x := "V"}, _, _},
        parse("-xV", #{arguments => [#{name => x, short => $x}]})).

multi_short() ->
    [{doc, "Tests multiple short arguments blend into one"}].

multi_short(Config) when is_list(Config) ->
    %% ensure non-flammable argument does not explode, even when it's possible
    ?assertMatch({ok, #{v := "xv"}, _, _},
        parse("-vxv", #{arguments => [#{name => v, short => $v}, #{name => x, short => $x}]})),
    %% ensure 'verbosity' use-case works
    ?assertMatch({ok, #{v := 3}, _, _},
        parse("-vvv", #{arguments => [#{name => v, short => $v, action => count}]})),
    %%
    ?assertMatch({ok, #{recursive := true, force := true, path := "dir"}, _, _},
        parse("-rf dir", #{arguments => [
            #{name => recursive, short => $r, type => boolean},
            #{name => force, short => $f, type => boolean},
            #{name => path}
            ]})).

proxy_arguments() ->
    [{doc, "Tests nargs => all used to proxy arguments to another script"}].

proxy_arguments(Config) when is_list(Config) ->
    Cmd = #{
        commands => #{
            "start" => #{
                arguments => [
                    #{name => shell, short => $s, long => "-shell", type => boolean},
                    #{name => skip, short => $x, long => "-skip", type => boolean},
                    #{name => args, required => false, nargs => all}
                ]
            },
            "stop" => #{},
            "status" => #{
                arguments => [
                    #{name => skip, required => false, default => "ok"},
                    #{name => args, required => false, nargs => all}
                ]},
            "state" => #{
                arguments => [
                    #{name => skip, required => false},
                    #{name => args, required => false, nargs => all}
                ]}
        },
        arguments => [
            #{name => node}
        ],
        handler => fun (#{}) -> ok end
    },
    Prog = prog(),
    ?assertMatch({ok, #{node := "node1"}, _, _}, parse("node1", Cmd)),
    ?assertMatch({ok, #{node := "node1"}, [Prog, "stop"], #{}}, parse("node1 stop", Cmd)),
    ?assertMatch({ok, #{node := "node2.org", shell := true, skip := true}, _, _}, parse("node2.org start -x -s", Cmd)),
    ?assertMatch({ok, #{args := ["-app","key","value"],node := "node1.org"}, [Prog, "start"], _},
        parse("node1.org start -app key value", Cmd)),
    ?assertMatch({ok, #{args := ["-app","key","value", "-app2", "key2", "value2"],
        node := "node3.org", shell := true}, [Prog, "start"], _},
        parse("node3.org start -s -app key value -app2 key2 value2", Cmd)),
    %% test that any non-required positionals are skipped
    ?assertMatch({ok, #{args := ["-a","bcd"], node := "node2.org", skip := "ok"}, _, _}, parse("node2.org status -a bcd", Cmd)),
    ?assertMatch({ok, #{args := ["-app", "key"], node := "node2.org"}, _, _}, parse("node2.org state -app key", Cmd)).

%%--------------------------------------------------------------------
%% Usage Test Cases

usage() ->
    [{doc, "Basic tests for help formatter, including 'hidden' help"}].

usage(Config) when is_list(Config) ->
    Cmd = ubiq_cmd(),
    Usage = "Usage:\n"
        "  erl start {crawler|doze} [-rfvl] [--force] [-i <interval>] [--req <weird>]\n"
        "      [--float <float>] [-s <shard>...] [-z <z>] [-m <more>] [-b <bin>] [-g <g>]\n"
        "      [-t <t>] ---maybe-req -y <y> --yyy <y> [-u <u>] [-c <choice>] [-q <fc>]\n"
        "      [-w <ac>] [--unsafe <au>] [--safe <as>] [-foobar <long>] <server> [<optpos>]\n"
        "\n"
        "Subcommands:\n"
        "  crawler      controls crawler behaviour\n"
        "  doze         dozes a bit\n\n"
        "Arguments:\n"
        "  server       server to start\n"
        "  optpos       optional positional (int)\n\n"
        "Optional arguments:\n"
        "  -r           recursive\n"
        "  -f, --force  force\n"
        "  -v           verbosity level\n"
        "  -i           interval set (int >= 1)\n"
        "  --req        required optional, right?\n"
        "  --float      floating-point long form argument (float), default: 3.14\n"
        "  -s           initial shards (int)\n"
        "  -z           between (1 <= int <= 10)\n"
        "  -l           maybe lower (int <= 10)\n"
        "  -m           less than 10 (int <= 10)\n"
        "  -b           binary with re (binary re: m)\n"
        "  -g           binary with re (binary re: m)\n"
        "  -t           string with re (string re: m)\n"
        "  ---maybe-req maybe required int (int)\n"
        "  -y, --yyy    string with re (string re: m)\n"
        "  -u           string choices (choice: 1, 2)\n"
        "  -c           tough choice (choice: 1, 2, 3)\n"
        "  -q           floating choice (choice: 2.10000, 1.20000)\n"
        "  -w           atom choice (choice: one, two)\n"
        "  --unsafe     unsafe atom (atom)\n"
        "  --safe       safe atom (existing atom)\n"
        "  -foobar      foobaring option\n",
    ?assertEqual(Usage, unicode:characters_to_list(argparse:help(Cmd,
        #{progname => "erl", command => ["start"]}))),
    FullCmd = "Usage:\n  erl"
        " <command> [-rfv] [--force] [-i <interval>] [--req <weird>] [--float <float>]\n\n"
        "Subcommands:\n"
        "  start       verifies configuration and starts server\n"
        "  status      prints server status\n"
        "  stop        stops running server\n\n"
        "Optional arguments:\n"
        "  -r          recursive\n"
        "  -f, --force force\n"
        "  -v          verbosity level\n"
        "  -i          interval set (int >= 1)\n"
        "  --req       required optional, right?\n"
        "  --float     floating-point long form argument (float), default: 3.14\n",
    ?assertEqual(FullCmd, unicode:characters_to_list(argparse:help(Cmd,
        #{progname => erl}))),
    CrawlerStatus = "Usage:\n  erl status crawler [-rfv] [--force] [-i <interval>] [--req <weird>]\n"
        "      [--float <float>] [---extra <extra>]\n\n"
        "Optional arguments:\n"
        "  -r          recursive\n"
        "  -f, --force force\n"
        "  -v          verbosity level\n"
        "  -i          interval set (int >= 1)\n"
        "  --req       required optional, right?\n"
        "  --float     floating-point long form argument (float), default: 3.14\n"
        "  ---extra    extra option very deep\n",
    ?assertEqual(CrawlerStatus, unicode:characters_to_list(argparse:help(Cmd,
        #{progname => "erl", command => ["status", "crawler"]}))),
    ok.

usage_required_args() ->
    [{doc, "Verify that required args are printed as required in usage"}].

usage_required_args(Config) when is_list(Config) ->
    Cmd = #{commands => #{"test" => #{arguments => [#{name => required, required => true, long => "-req"}]}}},
    Expected = "Usage:\n  " ++ prog() ++ " test --req <required>\n\nOptional arguments:\n  --req required\n",
    ?assertEqual(Expected, unicode:characters_to_list(argparse:help(Cmd, #{command => ["test"]}))).

usage_template() ->
    [{doc, "Tests templates in help/usage"}].

usage_template(Config) when is_list(Config) ->
    %% Argument (positional)
    Cmd = #{arguments => [#{
        name => shard,
        type => integer,
        default => 0,
        help => {"[-s SHARD]", ["initial number, ", type, <<" with a default value of ">>, default]}}
    ]},
    ?assertEqual("Usage:\n  " ++ prog() ++ " [-s SHARD]\n\nArguments:\n  shard initial number, int with a default value of 0\n",
        unicode:characters_to_list(argparse:help(Cmd, #{}))),
    %% Optional
    Cmd1 = #{arguments => [#{
        name => shard,
        short => $s,
        type => integer,
        default => 0,
        help => {<<"[-s SHARD]">>, ["initial number"]}}
    ]},
    ?assertEqual("Usage:\n  " ++ prog() ++ " [-s SHARD]\n\nOptional arguments:\n  -s initial number\n",
        unicode:characters_to_list(argparse:help(Cmd1, #{}))),
    %% ISO Date example
    DefaultRange = {{2020, 1, 1}, {2020, 6, 22}},
    CmdISO = #{
        arguments => [
            #{
                name => range,
                long => "-range",
                short => $r,
                help => {"[--range RNG]", fun() ->
                    {{FY, FM, FD}, {TY, TM, TD}} = DefaultRange,
                    lists:flatten(io_lib:format("date range, ~b-~b-~b..~b-~b-~b", [FY, FM, FD, TY, TM, TD]))
                                              end},
                type => {custom, fun(S) -> [S, DefaultRange] end},
                default => DefaultRange
            }
        ]
    },
    ?assertEqual("Usage:\n  " ++ prog() ++ " [--range RNG]\n\nOptional arguments:\n  -r, --range date range, 2020-1-1..2020-6-22\n",
        unicode:characters_to_list(argparse:help(CmdISO, #{}))),
    ok.

usage_args_ordering() ->
    [{doc, "Tests the ordering of arguments in usage text"}].

usage_args_ordering(Config) when is_list(Config) ->
    Cmd = #{arguments => [
        #{name => first},
        #{name => second}
        ],
        commands => #{
            "cmd" => #{arguments => [
                #{name => third},
                #{name => fourth}
            ]}}
    },
    ?assertEqual("Usage:\n  " ++ prog() ++ " cmd <first> <second> <third> <fourth>\n"
        "\n"
        "Arguments:\n"
        "  first  first\n"
        "  second second\n"
        "  third  third\n"
        "  fourth fourth\n",
        unicode:characters_to_list(argparse:help(Cmd, #{command => ["cmd"]}))),
    ok.

parser_error_usage() ->
    [{doc, "Tests that parser errors have corresponding usage text"}].

parser_error_usage(Config) when is_list(Config) ->
    %% unknown arguments
    Prog = prog(),
    ?assertEqual(Prog ++ ": unknown argument: arg", parser_error(["arg"], #{})),
    ?assertEqual(Prog ++ ": unknown argument: -a", parser_error(["-a"], #{})),
    %% missing argument
    ?assertEqual(Prog ++ ": required argument missing: need", parser_error([""],
        #{arguments => [#{name => need}]})),
    ?assertEqual(Prog ++ ": required argument missing: need", parser_error([""],
        #{arguments => [#{name => need, short => $n, required => true}]})),
    %% invalid value
    ?assertEqual(Prog ++ ": invalid argument for need: foo is not an integer", parser_error(["foo"],
        #{arguments => [#{name => need, type => integer}]})),
    ?assertEqual(Prog ++ ": invalid argument for need: cAnNotExIsT is not an existing atom", parser_error(["cAnNotExIsT"],
        #{arguments => [#{name => need, type => atom}]})).

command_usage() ->
    [{doc, "Test command help template"}].

command_usage(Config) when is_list(Config) ->
    Cmd = #{arguments => [
        #{name => arg, help => "argument help"}, #{name => opt, short => $o, help => "option help"}],
        help => ["Options:\n", options, arguments, <<"NOTAUSAGE">>, usage, "\n"]
    },
    ?assertEqual("Options:\n  -o  option help\n  arg argument help\nNOTAUSAGE  " ++ prog() ++ " [-o <opt>] <arg>\n",
        unicode:characters_to_list(argparse:help(Cmd, #{}))).

usage_width() ->
    [{doc, "Test usage fitting in the viewport"}].

usage_width(Config) when is_list(Config) ->
    Cmd = #{arguments => [
        #{name => arg, help => "argument help that spans way over allowed viewport width, wrapping words"},
        #{name => opt, short => $o, long => "-option_long_name",
            help => "another quite long word wrapped thing spanning over several lines"},
        #{name => v, short => $v, type => boolean},
        #{name => q, short => $q, type => boolean}],
        commands => #{
            "cmd1" => #{help => "Help for command number 1, not fitting at all"},
            "cmd2" => #{help => <<"Short help">>},
            "cmd3" => #{help => "Yet another instance of a very long help message"}
        },
        help => "  Very long help line taking much more than 40 characters allowed by the test case.
Also containing a few newlines.

   Indented new lines must be honoured!"
    },

    Expected = "Usage:\n  erl {cmd1|cmd2|cmd3} [-vq] [-o <opt>]\n"
        "      [--option_long_name <opt>] <arg>\n\n"
        "  Very long help line taking much more\n"
        "than 40 characters allowed by the test\n"
        "case.\n"
        "Also containing a few newlines.\n\n"
        "   Indented new lines must be honoured!\n\n"
        "Subcommands:\n"
        "  cmd1                   Help for\n"
        "                         command number\n"
        "                         1, not fitting\n"
        "                         at all\n"
        "  cmd2                   Short help\n"
        "  cmd3                   Yet another\n"
        "                         instance of a\n"
        "                         very long help\n"
        "                         message\n\n"
        "Arguments:\n"
        "  arg                    argument help\n"
        "                         that spans way\n"
        "                         over allowed\n"
        "                         viewport width,\n"
        "                         wrapping words\n\n"
        "Optional arguments:\n"
        "  -o, --option_long_name another quite\n"
        "                         long word\n"
        "                         wrapped thing\n"
        "                         spanning over\n"
        "                         several lines\n"
        "  -v                     v\n"
        "  -q                     q\n",

    ?assertEqual(Expected, unicode:characters_to_list(argparse:help(Cmd, #{columns => 40, progname => "erl"}))).

%%--------------------------------------------------------------------
%% Validator Test Cases

validator_exception() ->
    [{doc, "Tests that the validator throws expected exceptions"}].

validator_exception(Config) when is_list(Config) ->
    Prg = [prog()],
    %% conflicting option names
    ?assertException(error, {argparse, argument, Prg, short, "short conflicting with previously defined short for one"},
        argparse:validate(#{arguments => [#{name => one, short => $$}, #{name => two, short => $$}]})),
    ?assertException(error, {argparse, argument, Prg, long, "long conflicting with previously defined long for one"},
        argparse:validate(#{arguments => [#{name => one, long => "a"}, #{name => two, long => "a"}]})),
    %% broken options
    %% long must be a string
    ?assertException(error, {argparse, argument, Prg, long, _},
        argparse:validate(#{arguments => [#{name => one, long => ok}]})),
    %% short must be a printable character
    ?assertException(error, {argparse, argument, Prg, short, _},
        argparse:validate(#{arguments => [#{name => one, short => ok}]})),
    ?assertException(error, {argparse, argument, Prg, short, _},
        argparse:validate(#{arguments => [#{name => one, short => 7}]})),
    %% required is a boolean
    ?assertException(error, {argparse, argument, Prg, required, _},
        argparse:validate(#{arguments => [#{name => one, required => ok}]})),
    ?assertException(error, {argparse, argument, Prg, help, _},
        argparse:validate(#{arguments => [#{name => one, help => ok}]})),
    %% tuple form of help must not have string as second argument
    ?assertException(error, {argparse, argument, Prg, help, _},
        argparse:validate(#{arguments => [#{name => one, help => {"one", "1"}}]})),
    %% broken commands
    try argparse:help(#{}, #{progname => 123}), ?assert(false)
    catch error:badarg:Stack ->
        [{_, _, _, Ext} | _] = Stack,
        #{cause := #{2 := Detail}} = proplists:get_value(error_info, Ext),
        ?assertEqual(<<"progname is not valid">>, Detail)
    end,
    %% not-a-list of arguments provided to a subcommand
    Prog = prog(),
    ?assertException(error, {argparse, command, [Prog, "start"], arguments, <<"expected a list, [argument()]">>},
        argparse:validate(#{commands => #{"start" => #{arguments => atom}}})),
    %% command is not a map
    ?assertException(error, {argparse, command, Prg, commands, <<"expected map of #{string() => command()}">>},
        argparse:validate(#{commands => []})),
    %% invalid commands field
    ?assertException(error, {argparse, command, Prg, commands, _},
        argparse:validate(#{commands => ok})),
    ?assertException(error, {argparse, command, _, commands, _},
        argparse:validate(#{commands => #{ok => #{}}})),
    ?assertException(error, {argparse, command, _, help,
        <<"must be a printable unicode list, or a command help template">>},
        argparse:validate(#{commands => #{"ok" => #{help => ok}}})),
    ?assertException(error, {argparse, command, _, handler, _},
        argparse:validate(#{commands => #{"ok" => #{handler => fun validator_exception/0}}})),
    %% extend + maybe: validator exception
    ?assertException(error, {argparse, argument, _, action, <<"extend action works only with lists">>},
        parse("-1 -1", #{arguments =>
        [#{action => extend, name => short49, nargs => 'maybe', short => 49}]})).

validator_exception_format() ->
    [{doc, "Tests human-readable (EEP-54) format for exceptions thrown by the validator"}].

validator_exception_format(Config) when is_list(Config) ->
    %% set up as a contract: test that EEP-54 transformation is done (but don't check strings)
    try
        argparse:validate(#{commands => #{"one" => #{commands => #{"two" => atom}}}}),
        ?assert(false)
    catch
        error:R1:S1 ->
            #{1 := Cmd, reason := RR1, general := G} = argparse:format_error(R1, S1),
            ?assertEqual("command specification is invalid", unicode:characters_to_list(G)),
            ?assertEqual("command \"" ++ prog() ++ " one two\": invalid field 'commands', reason: expected command()",
                unicode:characters_to_list(RR1)),
            ?assertEqual(["atom"], Cmd)
    end,
    %% check argument
    try
        argparse:validate(#{arguments => [#{}]}),
        ?assert(false)
    catch
        error:R2:S2 ->
            #{1 := Cmd2, reason := RR2, general := G2} = argparse:format_error(R2, S2),
            ?assertEqual("argument specification is invalid", unicode:characters_to_list(G2)),
            ?assertEqual("command \"" ++ prog() ++
                "\", argument '', invalid field 'name': argument must be a map containing 'name' field",
                unicode:characters_to_list(RR2)),
            ?assertEqual(["#{}"], Cmd2)
    end.

%%--------------------------------------------------------------------
%% Validator Test Cases

run_handle() ->
    [{doc, "Very basic tests for argparse:run/3, choice of handlers formats"}].

%% fun((arg_map()) -> term()) |    %% handler accepting arg_map
%% {module(), Fn :: atom()} |      %% handler, accepting arg_map, Fn exported from module()
%% {fun(() -> term()), term()} |   %% handler, positional form (term() is supplied for omitted args)
%% {module(), atom(), term()}

run_handle(Config) when is_list(Config) ->
    %% no subcommand, basic fun handler with argmap
    ?assertEqual(6,
        argparse:run(["-i", "3"], #{handler => fun (#{in := Val}) -> Val * 2 end,
        arguments => [#{name => in, short => $i, type => integer}]}, #{})),
    %% subcommand, positional fun() handler
    ?assertEqual(6,
        argparse:run(["mul", "2", "3"], #{commands => #{"mul" => #{
            handler => {fun (match, L, R) -> L * R end, match},
            arguments => [#{name => opt, short => $o},
                #{name => l, type => integer}, #{name => r, type => integer}]}}},
        #{})),
    %% no subcommand, positional module-based function
    ?assertEqual(6,
        argparse:run(["2", "3"], #{handler => {erlang, '*', undefined},
            arguments => [#{name => l, type => integer}, #{name => r, type => integer}]},
            #{})),
    %% subcommand, module-based function accepting argmap
    ?assertEqual([{arg, "arg"}],
        argparse:run(["map", "arg"], #{commands => #{"map" => #{
            handler => {maps, to_list},
            arguments => [#{name => arg}]}}},
            #{})).

run_args_ordering() ->
    [{doc, "Test that positional arguments are parsed in the correct order"}].

run_args_ordering(Config) when is_list(Config) ->
    ?assertEqual([{first,"1"},{second,"2"},{third,"3"},{fourth,"4"}],
        argparse:run(["cmd", "1", "2", "3", "4"],
            #{arguments => [#{name => first}, #{name => second}],
            commands => #{
                "cmd" => #{
                    handler => {maps, to_list},
                    arguments => [#{name => third}, #{name => fourth}]}
            }},
            #{progname => example})).
