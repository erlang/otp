%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
%% Yacc like LALR-1 parser generator for Erlang.
%% Ref: Aho & Johnson: "LR Parsing", ACM Computing Surveys, vol. 6:2, 1974.
%% Auxiliary files: yeccgramm.yrl, yeccparser.erl, yeccpre.hrl, yeccscan.erl.
%%

-module(yecc).

-export([compile/3, file/1, file/2, format_error/1]).

%% Kept for compatibility with R10B.
-export([yecc/2, yecc/3, yecc/4]).

-import(lists, [append/1, append/2, concat/1, delete/2, filter/2,
                flatmap/2, foldl/3, foldr/3, foreach/2, keydelete/3,
                keysort/2, last/1, map/2, member/2, reverse/1,
                sort/1, usort/1]).

-include("erl_compile.hrl").
-include("ms_transform.hrl").

-record(yecc, {
          infile,
          outfile,
          includefile,
          includefile_version,
          module,
          encoding = none,
          options = [],
          verbose = false,
          file_attrs = true,
          errors = [],
          warnings = [],
          conflicts_done = false,
          shift_reduce = [],
          reduce_reduce = [],
          n_states = 0,
          inport,
          outport,
          line,

          parse_actions,
          symbol_tab,
          inv_symbol_tab,
          state_tab,
          prec_tab,
          goto_tab,

          terminals = [],
          nonterminals = [],
          all_symbols = [],
          prec = [],
          rules_list = [],
          rules, % a tuple of rules_list
          rule_pointer2rule,
          rootsymbol = [],
          endsymbol = [],
          expect_shift_reduce = [],
          expect_n_states = [],
          header = [],
          erlang_code = none
         }).

-record(rule, {
          n,             % rule n in the grammar file
          anno,
          symbols,       % the names of symbols
          tokens,
          is_guard,      % the action is a guard (not used)
          is_well_formed % can be parsed (without macro expansion)
         }).

-record(reduce, {
          rule_nmbr,
          head,
          nmbr_of_daughters,
          prec,
          unused % assure that #reduce{} comes before #shift{} when sorting
         }).

-record(shift, {
          state,
          pos,
          prec,
          rule_nmbr
         }).

-record(user_code, {state, terminal, funname, action}).

-record(symbol, {anno = none, name}).

%% ACCEPT is neither an atom nor a non-terminal.
-define(ACCEPT, {}).

%% During the phase 'compute_states' terminals in lookahead sets are
%% coded as integers; sets of terminals are integer bit masks. This is
%% for efficiency only. '$empty' is always given the mask 1. The
%% behaviour can be turned off by un-defining SYMBOLS_AS_CODES (useful
%% when debugging).

%% Non-terminals are also given integer codes, starting with -1. The
%% absolut value of the code is used for indexing a tuple of lists of
%% rules.

-define(SYMBOLS_AS_CODES, true).

-ifdef(SYMBOLS_AS_CODES).
-define(EMPTY, 0).
-else.
-define(EMPTY, '$empty').
-endif.

%%%
%%% Exported functions
%%%

%%% Interface to erl_compile.

compile(Input0, Output0, 
        #options{warning = WarnLevel, verbose=Verbose, includes=Includes,
		 specific=Specific}) ->
    Input = shorten_filename(Input0),
    Output = shorten_filename(Output0),
    Includefile = lists:sublist(Includes, 1),
    Werror = proplists:get_bool(warnings_as_errors, Specific),
    Opts = [{parserfile,Output}, {includefile,Includefile}, {verbose,Verbose},
            {report_errors, true}, {report_warnings, WarnLevel > 0},
	    {warnings_as_errors, Werror}],
    case file(Input, Opts) of
        {ok, _OutFile} ->
            ok;
        error ->
            error
    end.

format_error(bad_declaration) ->
    io_lib:fwrite("unknown or bad declaration, ignored", []);
format_error({bad_expect, SymName}) ->
    io_lib:fwrite("argument ~ts of Expect is not an integer", 
                  [format_symbol(SymName)]);
format_error({bad_rootsymbol, SymName}) ->
    io_lib:fwrite("rootsymbol ~ts is not a nonterminal", 
                  [format_symbol(SymName)]);
format_error({bad_states, SymName}) ->
    io_lib:fwrite("argument ~ts of States is not an integer", 
                  [format_symbol(SymName)]);
format_error({conflict, Conflict}) ->
    format_conflict(Conflict);
format_error({conflicts, SR, RR}) ->
    io_lib:fwrite("conflicts: ~w shift/reduce, ~w reduce/reduce", [SR, RR]);
format_error({duplicate_declaration, Tag}) ->
    io_lib:fwrite("duplicate declaration of ~s", [atom_to_list(Tag)]);
format_error({duplicate_nonterminal, Nonterminal}) ->
    io_lib:fwrite("duplicate non-terminals ~ts", 
                  [format_symbol(Nonterminal)]);
format_error({duplicate_precedence, Op}) ->
    io_lib:fwrite("duplicate precedence operator ~ts", 
                  [format_symbol(Op)]);
format_error({duplicate_terminal, Terminal}) ->
    io_lib:fwrite("duplicate terminal ~ts", 
                  [format_symbol(Terminal)]);
format_error({endsymbol_is_nonterminal, Symbol}) ->
    io_lib:fwrite("endsymbol ~ts is a nonterminal", 
                  [format_symbol(Symbol)]);
format_error({endsymbol_is_terminal, Symbol}) ->
    io_lib:fwrite("endsymbol ~ts is a terminal", 
                  [format_symbol(Symbol)]);
format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error({file_error, Reason}) ->
    io_lib:fwrite("~ts",[file:format_error(Reason)]);
format_error(illegal_empty) ->
    io_lib:fwrite("illegal use of empty symbol", []);
format_error({internal_error, Error}) ->
    io_lib:fwrite("internal yecc error: ~w", [Error]);
format_error({missing_syntax_rule, Nonterminal}) ->
    io_lib:fwrite("no syntax rule for non-terminal symbol ~ts",
                  [format_symbol(Nonterminal)]);
format_error({n_states, Exp, N}) ->
    io_lib:fwrite("expected ~w states, but got ~p states", [Exp, N]);
format_error(no_grammar_rules) ->
    io_lib:fwrite("grammar rules are missing", []);
format_error(nonterminals_missing) ->
    io_lib:fwrite("Nonterminals is missing", []);
format_error({precedence_op_is_endsymbol, SymName}) ->
    io_lib:fwrite("precedence operator ~ts is endsymbol",
                  [format_symbol(SymName)]);
format_error({precedence_op_is_unknown, SymName}) ->
    io_lib:fwrite("unknown precedence operator ~ts",
                  [format_symbol(SymName)]);
format_error({reserved, N}) ->
    io_lib:fwrite("the use of ~w should be avoided", [N]);
format_error({symbol_terminal_and_nonterminal, SymName}) ->
    io_lib:fwrite("symbol ~ts is both a terminal and nonterminal",
                  [format_symbol(SymName)]);
format_error(rootsymbol_missing) ->
    io_lib:fwrite("Rootsymbol is missing", []);
format_error(terminals_missing) ->
    io_lib:fwrite("Terminals is missing", []);
format_error({undefined_nonterminal, Symbol}) ->
    io_lib:fwrite("undefined nonterminal: ~ts", [format_symbol(Symbol)]);
format_error({undefined_pseudo_variable, Atom}) ->
    io_lib:fwrite("undefined pseudo variable ~w", [Atom]);
format_error({undefined_symbol, SymName}) ->
    io_lib:fwrite("undefined rhs symbol ~ts", [format_symbol(SymName)]);
format_error({unused_nonterminal, Nonterminal}) ->
    io_lib:fwrite("non-terminal symbol ~ts not used", 
                  [format_symbol(Nonterminal)]);
format_error({unused_terminal, Terminal}) ->
    io_lib:fwrite("terminal symbol ~ts not used", 
                  [format_symbol(Terminal)]);
format_error({bad_symbol, String}) ->
    io_lib:fwrite("bad symbol ~ts", [String]);
format_error(cannot_parse) ->
    io_lib:fwrite("cannot parse; possibly encoding mismatch", []).

file(File) ->
    file(File, [report_errors, report_warnings]).

file(File, Options) ->
    case is_filename(File) of
        no -> erlang:error(badarg, [File, Options]);
        _ -> ok
    end,
    case options(Options) of
        badarg ->
            erlang:error(badarg, [File, Options]);
        OptionValues ->
            Self = self(),
            Flag = process_flag(trap_exit, false),
            Pid = spawn_link(fun() -> infile(Self, File, OptionValues) end),
            receive
                {Pid, Rep} -> 
                    receive after 1 -> ok end,
                    process_flag(trap_exit, Flag),
                    Rep
            end
    end.

%% Kept for backward compatibility.
yecc(Infile, Outfile) ->
    yecc(Infile, Outfile, false, []).

yecc(Infile, Outfile, Verbose) ->
    yecc(Infile, Outfile, Verbose, []).

yecc(Infilex, Outfilex, Verbose, Includefilex) ->
    _ = statistics(runtime),
    case file(Infilex, [{parserfile, Outfilex}, 
                        {verbose, Verbose}, 
                        {report, true},
                        {includefile, Includefilex}]) of
        {ok, _File} ->
            statistics(runtime);
        error ->
            exit(error)
    end.

%%%
%%% Local functions
%%%

options(Options0) when is_list(Options0) ->
    try 
        Options = flatmap(fun(return) -> short_option(return, true);
                             (report) -> short_option(report, true);
                             ({return,T}) -> short_option(return, T);
                             ({report,T}) -> short_option(report, T);
                             (T) -> [T]
                          end, Options0),
        options(Options, [file_attributes, includefile, parserfile, 
                          report_errors, report_warnings, warnings_as_errors,
                          return_errors, return_warnings, time, verbose], [])
    catch error: _ -> badarg
    end;
options(Option) ->
    options([Option]).

short_option(return, T) ->
    [{return_errors,T}, {return_warnings,T}];
short_option(report, T) ->
    [{report_errors,T}, {report_warnings,T}].

options(Options0, [Key | Keys], L) when is_list(Options0) ->
    Options = case member(Key, Options0) of
                  true -> 
                      [atom_option(Key) | delete(Key, Options0)];
                  false ->
                      Options0
              end,
    V = case lists:keyfind(Key, 1, Options) of
            {Key, Filename0} when Key =:= includefile;
                                  Key =:= parserfile ->
                case is_filename(Filename0) of
                    no -> 
                        badarg;
                    Filename -> 
                        {ok, [{Key, Filename}]}
                end;
            {Key, Bool} = KB when is_boolean(Bool) ->
                {ok, [KB]};
            {Key, _} ->
                badarg;
            false ->
                {ok, [{Key, default_option(Key)}]}
        end,
    case V of
        badarg ->
            badarg;
        {ok, KeyValueL} ->
            NewOptions = keydelete(Key, 1, Options),
            options(NewOptions, Keys, KeyValueL ++ L)
    end;
options([], [], L) ->
    foldl(fun({_,false}, A) -> A;
             ({Tag,true}, A) -> [Tag | A];
             (F, A) -> [F | A]
          end, [], L);
options(_Options, _, _L) ->
    badarg.

default_option(file_attributes) -> true;
default_option(includefile) -> [];
default_option(parserfile) -> [];
default_option(report_errors) -> true;
default_option(report_warnings) -> true;
default_option(warnings_as_errors) -> false;
default_option(return_errors) -> false;
default_option(return_warnings) -> false;
default_option(time) -> false;
default_option(verbose) -> false.

atom_option(file_attributes) -> {file_attributes, true};
atom_option(report_errors) -> {report_errors, true};
atom_option(report_warnings) -> {report_warnings, true};
atom_option(warnings_as_errors) -> {warnings_as_errors,true};
atom_option(return_errors) -> {return_errors, true};
atom_option(return_warnings) -> {return_warnings, true};
atom_option(time) -> {time, true};
atom_option(verbose) -> {verbose, true};
atom_option(Key) -> Key.

is_filename(T) ->
    try filename:flatten(T)
    catch error: _ -> no
    end.    

shorten_filename(Name0) ->
    {ok,Cwd} = file:get_cwd(),
    case string:prefix(Name0, Cwd) of
        nomatch -> Name0;
        Rest ->
            case unicode:characters_to_list(Rest) of
                "/"++N -> N;
                N -> N
            end
    end.

start(Infilex, Options) ->
    Infile = assure_extension(Infilex, ".yrl"),
    {_, Outfilex0} = lists:keyfind(parserfile, 1, Options),
    {_, Includefilex} = lists:keyfind(includefile, 1, Options),
    Outfilex = case Outfilex0 of
                   [] -> filename:rootname(Infilex, ".yrl");
                   _ -> Outfilex0
               end,
    Includefile = case Includefilex of
                      [] -> [];
                      _ -> assure_extension(Includefilex,".hrl")
                  end,
    IncludefileVersion = includefile_version(Includefile),
    Outfile = assure_extension(Outfilex, ".erl"),
    Module = list_to_atom(filename:basename(Outfile, ".erl")),
    #yecc{infile = Infile, 
          outfile = Outfile,
          includefile = Includefile,
          includefile_version = IncludefileVersion,
          module = Module,
          options = Options,
          verbose = member(verbose, Options),
          file_attrs = member(file_attributes, Options)}.

assure_extension(File, Ext) ->
    concat([strip_extension(File, Ext), Ext]).

%% Assumes File is a filename.
strip_extension(File, Ext) ->
    case filename:extension(File) of
        Ext -> filename:rootname(File);
        _Other -> File
    end.

infile(Parent, Infilex, Options) ->
    St0 = start(Infilex, Options),
    St = case file:open(St0#yecc.infile, [read, read_ahead]) of
             {ok, Inport} ->
                 try 
                     Encoding = epp:set_encoding(Inport),
                     St1 = St0#yecc{inport = Inport, encoding = Encoding},
                     outfile(St1)
                 after
                     ok = file:close(Inport)
                 end;
             {error, Reason} ->
                 add_error(St0#yecc.infile, none, {file_error, Reason}, St0)
         end,
    case {St#yecc.errors, werror(St)} of
        {[], false} -> ok;
        _ -> _ = file:delete(St#yecc.outfile), ok
    end,
    Parent ! {self(), yecc_ret(St)}.

werror(St) ->
    St#yecc.warnings =/= []
	andalso member(warnings_as_errors, St#yecc.options).

outfile(St0) ->
    case file:open(St0#yecc.outfile, [write, delayed_write]) of
        {ok, Outport} ->
            try 
                %% Set the same encoding as infile:
                set_encoding(St0, Outport),
                generate(St0#yecc{outport = Outport, line = 1})
            catch 
                throw: St1  ->
                    St1;
                exit: Reason ->
                    add_error({internal_error, Reason}, St0)
            after
               ok = file:close(Outport)
            end;
        {error, Reason} ->
            add_error(St0#yecc.outfile, none, {file_error, Reason}, St0)
    end.

os_process_size() ->
    case os:type() of
        {unix, sunos} ->
            Size = os:cmd("ps -o vsz -p " ++ os:getpid() ++ " | tail -1"),
            list_to_integer(lib:nonl(Size));
        _ ->
            0
    end.            

timeit(Name, Fun, St0) ->
    Time = runtime,
    %% Time = wall_clock,
    {Before, _} = statistics(Time),
    St = Fun(St0), 
    {After, _} = statistics(Time),
    Mem0 = erts_debug:flat_size(St)*erlang:system_info(wordsize),
    Mem = lists:flatten(io_lib:format("~.1f kB", [Mem0/1024])),
    Sz = lists:flatten(io_lib:format("~.1f MB", [os_process_size()/1024])),
    io:fwrite(" ~-30w: ~10.2f s ~12s ~10s\n", 
              [Name, (After-Before)/1000, Mem, Sz]),
    St.

-define(PASS(P), {P, fun P/1}).

generate(St0) ->
    St1 = output_encoding_comment(St0),
    Passes = [?PASS(parse_grammar), ?PASS(check_grammar),
              ?PASS(states_and_goto_table), ?PASS(parse_actions),
              ?PASS(action_conflicts), ?PASS(write_file)],
    F = case member(time, St1#yecc.options) of
            true -> 
                io:fwrite(<<"Generating parser from grammar in ~ts\n">>,
                          [format_filename(St1#yecc.infile, St1)]),
                fun timeit/3;
            false ->
                fun(_Name, Fn, St) -> Fn(St) end
        end,
    Fun = fun({Name, Fun}, St) ->
                  St2 = F(Name, Fun, St),
                  if 
                      St2#yecc.errors =:= [] -> St2;
                      true -> throw(St2)
                  end
          end,
    foldl(Fun, St1, Passes).

parse_grammar(St) ->
    parse_grammar(St#yecc.inport, 1, St).

parse_grammar(Inport, Line, St) ->
    {NextLine, Grammar} = read_grammar(Inport, St, Line),
    parse_grammar(Grammar, Inport, NextLine, St).

parse_grammar(eof, _Inport, _NextLine, St) ->
    St;
parse_grammar({#symbol{name = 'Header'}, Ss}, Inport, NextLine, St0) ->
    St1 = St0#yecc{header = [S || {string,_,S} <- Ss]},
    parse_grammar(Inport, NextLine, St1);
parse_grammar({#symbol{name = 'Erlang'}, [#symbol{name = code}]}, _Inport, 
              NextLine, St) ->
    St#yecc{erlang_code = NextLine};
parse_grammar(Grammar, Inport, NextLine, St0) ->
    St = parse_grammar(Grammar, St0),
    parse_grammar(Inport, NextLine, St).

parse_grammar({error,ErrorLine,Error}, St) ->
    add_error(erl_anno:new(ErrorLine), Error, St);
parse_grammar({rule, Rule, Tokens}, St0) ->
    NmbrOfDaughters = case Rule of
                          [_, #symbol{name = '$empty'}]  -> 0;
                          _ -> length(Rule) - 1
                      end,
    {IsGuard, IsWellFormed} = check_action(Tokens),
    {Tokens1, St} = subst_pseudo_vars(Tokens,
                                      NmbrOfDaughters,
                                      St0),
    RuleDef = #rule{symbols = Rule, 
                    tokens = Tokens1, 
                    is_guard = IsGuard, 
                    is_well_formed = IsWellFormed},
    St#yecc{rules_list = [RuleDef | St#yecc.rules_list]};
parse_grammar({prec, Prec}, St) ->
    St#yecc{prec = Prec ++ St#yecc.prec};
parse_grammar({#symbol{}, [{string,Anno,String}]}, St) ->
    add_error(Anno, {bad_symbol, String}, St);
parse_grammar({#symbol{anno = Anno, name = Name}, Symbols}, St) ->
    CF = fun(I) ->
                 case element(I, St) of
                     [] -> 
                         setelement(I, St, Symbols);
                     _ -> 
                         add_error(Anno, {duplicate_declaration, Name}, St)
                 end
         end,
    OneSymbol = length(Symbols) =:= 1,
    case Name of
        'Nonterminals' -> CF(#yecc.nonterminals);
        'Terminals' -> CF(#yecc.terminals);
        'Rootsymbol' when OneSymbol -> CF(#yecc.rootsymbol);
        'Endsymbol' when OneSymbol ->  CF(#yecc.endsymbol);
        'Expect' when OneSymbol -> CF(#yecc.expect_shift_reduce);
        'States' when OneSymbol -> CF(#yecc.expect_n_states); % undocumented
        _ -> add_warning(Anno, bad_declaration, St)
    end.

read_grammar(Inport, St, Line) ->
    case yeccscan:scan(Inport, '', Line) of
        {eof, NextLine} ->
            {NextLine, eof};
        {error, {ErrorLine, Mod, What}, NextLine} ->
            {NextLine, {error, ErrorLine, {error, Mod, What}}};
        {error, terminated} ->
            throw(St);
        {error, _} ->
            File = St#yecc.infile,
            throw(add_error(File, none, cannot_parse, St));
        {ok, Input, NextLine} ->
            {NextLine, case yeccparser:parse(Input) of
                           {error, {ErrorLine, Mod, Message}} ->
                               {error, ErrorLine, {error, Mod, Message}};
                           {ok, {rule, Rule, {erlang_code, Tokens}}} ->
                               {rule, Rule, Tokens};
                           {ok, {#symbol{name=P}, 
                                 [#symbol{name=I} | OpL]}=Ss} ->
                               A = precedence(P),
                               if
                                   A =/= unknown, 
                                   is_integer(I),
                                   OpL =/= [] ->
                                       Ps = [{Op, I , A} || Op <- OpL],
                                       {prec, Ps};
                                   true -> 
                                       Ss
                               end;
                           {ok, Ss} -> 
                               Ss
                       end}
    end.

precedence('Left') -> left;
precedence('Right') -> right;
precedence('Unary') -> unary;
precedence('Nonassoc') -> nonassoc;
precedence(_) -> unknown.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_grammar(St0) ->
    Empty = #symbol{anno = none, name = '$empty'},
    AllSymbols = St0#yecc.nonterminals ++ St0#yecc.terminals ++ [Empty],
    St1 = St0#yecc{all_symbols = AllSymbols},
    Cs = [fun check_nonterminals/1, fun check_terminals/1, 
          fun check_rootsymbol/1, fun check_endsymbol/1, 
          fun check_expect/1, fun check_states/1,
          fun check_precedences/1, fun check_rules/1],
    foldl(fun(F, St) -> F(St) end, St1, Cs).

check_nonterminals(St) ->
    case St#yecc.nonterminals of 
        [] ->
            add_error(nonterminals_missing, St);
        Nonterminals ->
            {Unique, Dups} = duplicates(names(Nonterminals)),
            St1 = add_warnings(Dups, duplicate_nonterminal, St),
            St2 = check_reserved(Unique, St1),
            St2#yecc{nonterminals = [?ACCEPT | Unique]}
    end.

check_terminals(St0) ->
    case St0#yecc.terminals of
        [] ->
            add_error(terminals_missing, St0);
        Terminals ->
            {Unique, Dups} = duplicates(names(Terminals)),
            St1 = add_warnings(Dups, duplicate_terminal, St0),
            Common = intersect(St1#yecc.nonterminals, Unique),
            St2 = add_errors(Common, symbol_terminal_and_nonterminal, St1),
            St3 = check_reserved(Unique, St2),
            St3#yecc{terminals = ['$empty' | Unique]}
    end.

check_reserved(Names, St) ->
    add_errors(intersect(Names, ['$empty', '$end', '$undefined']),
               reserved, St).

check_rootsymbol(St) ->
    case St#yecc.rootsymbol of
        [] ->
            add_error(rootsymbol_missing, St);
        [#symbol{anno = Anno, name = SymName}] ->
            case kind_of_symbol(St, SymName) of
                nonterminal ->
                    St#yecc{rootsymbol = SymName};
                _ ->
                    add_error(Anno, {bad_rootsymbol, SymName}, St)
            end
    end.

check_endsymbol(St) ->
    case St#yecc.endsymbol of
        [] ->
            St#yecc{endsymbol = '$end'};
        [#symbol{anno = Anno, name = SymName}] ->
            case kind_of_symbol(St, SymName) of
                nonterminal ->
                    add_error(Anno, {endsymbol_is_nonterminal, SymName}, St);
                terminal ->
                    add_error(Anno, {endsymbol_is_terminal, SymName}, St);
                _ ->
                    St#yecc{endsymbol = SymName}
            end
    end.

check_expect(St0) ->
    case St0#yecc.expect_shift_reduce of
        [] ->
            St0#yecc{expect_shift_reduce = 0};
        [#symbol{name = Expect}] when is_integer(Expect) ->
            St0#yecc{expect_shift_reduce = Expect};
        [#symbol{anno = Anno, name = Name}] ->
            St1 = add_error(Anno, {bad_expect, Name}, St0),
            St1#yecc{expect_shift_reduce = 0}
    end.

check_states(St) ->
    case St#yecc.expect_n_states of
        [] ->
            St;
        [#symbol{name = NStates}] when is_integer(NStates) ->
            St#yecc{expect_n_states = NStates};
        [#symbol{anno = Anno, name = Name}] ->
            add_error(Anno, {bad_states, Name}, St)
    end.

check_precedences(St0) ->
    {St1, _} = 
        foldr(fun({#symbol{anno = Anno, name = Op},_I,_A}, {St,Ps}) ->
                      case member(Op, Ps) of
                          true ->
                              {add_error(Anno, {duplicate_precedence,Op}, St),
                               Ps};
                          false ->
                              {St, [Op | Ps]}
                      end
              end, {St0,[]}, St0#yecc.prec),
    foldl(fun({#symbol{anno = Anno, name = Op},I,A}, St) ->
                  case kind_of_symbol(St, Op) of
                      endsymbol ->
                          add_error(Anno,{precedence_op_is_endsymbol,Op}, St);
                      unknown ->
                          add_error(Anno, {precedence_op_is_unknown, Op}, St);
                      _ -> 
                          St#yecc{prec = [{Op,I,A} | St#yecc.prec]}
                  end
          end, St1#yecc{prec = []}, St1#yecc.prec).

check_rule(Rule0, {St0,Rules}) ->
    Symbols = Rule0#rule.symbols,
    #symbol{anno = HeadAnno, name = Head} = hd(Symbols),
    case member(Head, St0#yecc.nonterminals) of
        false -> 
            {add_error(HeadAnno, {undefined_nonterminal, Head}, St0), Rules};
        true ->
            St = check_rhs(tl(Symbols), St0),
            Rule = Rule0#rule{anno = HeadAnno, symbols = names(Symbols)},
            {St, [Rule | Rules]}
    end.

check_rules(St0) ->
    {St,Rules0} = foldl(fun check_rule/2, {St0,[]}, St0#yecc.rules_list),
    case St#yecc.rules_list of
        [] ->
            add_error(no_grammar_rules, St);
        _ ->
            Rule = #rule{anno = none,
                         symbols = [?ACCEPT, St#yecc.rootsymbol],
                         tokens = []},
            Rules1 = [Rule | Rules0],
            Rules = map(fun({R,I}) -> R#rule{n = I} end,  count(0, Rules1)),
            St#yecc{rules_list = Rules, rules = list_to_tuple(Rules)}
    end.

duplicates(List) ->
    Unique = usort(List),
    {Unique, List -- Unique}.

names(Symbols) ->
    map(fun(Symbol) -> Symbol#symbol.name end, Symbols).

symbol_anno(Name, St) ->
    #symbol{anno = Anno} = symbol_find(Name, St#yecc.all_symbols),
    Anno.

symbol_member(Symbol, Symbols) ->
    symbol_find(Symbol#symbol.name, Symbols) =/= false.

symbol_find(Name, Symbols) ->
    lists:keyfind(Name, #symbol.name, Symbols).

states_and_goto_table(St0) ->
    St1 = create_symbol_table(St0),
    St = compute_states(St1),
    create_precedence_table(St).

parse_actions(St) ->
    _ = erase(), % the pd is used when decoding lookahead sets
    ParseActions = compute_parse_actions(St#yecc.n_states, St, []),
    _ = erase(),
    St#yecc{parse_actions = ParseActions, state_tab = []}.

action_conflicts(St0) ->
    St = find_action_conflicts(St0),
    St#yecc{conflicts_done = true}.

-record(state_info, {reduce_only, state_repr, comment}).

write_file(St0) ->
    #yecc{parse_actions = ParseActions, goto_tab = GotoTab} = St0,
    Sorted = sort_parse_actions(ParseActions),
    StateReprs = find_identical_shift_states(Sorted),
    StateInfo = collect_some_state_info(Sorted, StateReprs),
    StateJumps = find_partial_shift_states(Sorted, StateReprs),
    UserCodeActions = find_user_code(Sorted, St0),
    #yecc{infile = Infile, outfile = Outfile,
          inport = Inport, outport = Outport,
          nonterminals = Nonterminals} = St0,
    {St10, N_lines, LastErlangCodeLine} = 
        output_prelude(Outport, Inport, St0),
    St20 = St10#yecc{line = St10#yecc.line + N_lines},
    St25 = nl(St20),
    St30 = output_file_directive(St25, Outfile, St25#yecc.line),
    St40 = nl(St30),
    St50 = output_actions(St40, StateJumps, StateInfo),
    Go0 = [{Symbol,{From,To}} || {{From,Symbol},To} <- ets:tab2list(GotoTab)],
    Go = family_with_domain(Go0, Nonterminals),
    St60 = output_goto(St50, Go, StateInfo),
    St70 = output_inlined(St60, UserCodeActions, Infile),
    St = nl(St70),
    case LastErlangCodeLine of
        %% Just in case warnings or errors are emitted after the last
        %% line of the file.
        {last_erlang_code_line, Last_line} ->
            output_file_directive(St, Infile, Last_line);
        no_erlang_code ->
            St
    end.

yecc_ret(St0) ->
    St = check_expected(St0),
    report_errors(St),
    report_warnings(St),
    Es = pack_errors(St#yecc.errors),
    Ws = pack_warnings(St#yecc.warnings),
    Werror = werror(St),
    if 
        Werror ->
            do_error_return(St, Es, Ws);
        Es =:= [] -> 
            case member(return_warnings, St#yecc.options) of
                true -> {ok, St#yecc.outfile, Ws};
                false -> {ok, St#yecc.outfile}
            end;
        true -> 
            do_error_return(St, Es, Ws)
    end.

do_error_return(St, Es, Ws) ->
    case member(return_errors, St#yecc.options) of
        true -> {error, Es, Ws};
        false -> error
    end.

check_expected(St0) ->
    #yecc{shift_reduce = SR, reduce_reduce = RR, expect_shift_reduce = ExpSR,
          n_states = NStates0, expect_n_states = ExpStates,
          conflicts_done = Done} = St0,
    N_RR = length(usort(RR)),
    N_SR = length(usort(SR)),
    St1 = if
              not Done ->
                  St0;
              N_SR =:= ExpSR, N_RR =:= 0 ->
                  St0;
              true ->
                  add_warning(none, {conflicts, N_SR, N_RR}, St0)
          end,
    NStates = NStates0 + 1,
    if
        (not Done) or (ExpStates =:= []) or (NStates =:= ExpStates) ->
            St1;
        true ->
            add_warning(none, {n_states, ExpStates, NStates}, St1)
    end.

pack_errors([{File,_} | _] = Es) ->
    [{File, flatmap(fun({_,E}) -> [E] end, sort(Es))}];
pack_errors([]) ->
    [].
    
pack_warnings([{File,_} | _] = Ws) ->
    [{File, flatmap(fun({_,W}) -> [W] end, sort(Ws))}];
pack_warnings([]) ->
    [].

report_errors(St) ->
    case member(report_errors, St#yecc.options) of
        true ->
            foreach(fun({File,{none,Mod,E}}) -> 
                            io:fwrite(<<"~ts: ~ts\n">>,
                                      [File,Mod:format_error(E)]);
                       ({File,{Line,Mod,E}}) -> 
                            io:fwrite(<<"~ts:~w: ~ts\n">>,
                                      [File,Line,Mod:format_error(E)])
                    end, sort(St#yecc.errors));
        false -> 
            ok
    end.

report_warnings(St) ->
    Werror = member(warnings_as_errors, St#yecc.options),
    Prefix = case Werror of
		 true -> "";
		 false -> "Warning: "
	     end,
    ReportWerror = Werror andalso member(report_errors, St#yecc.options),
    case member(report_warnings, St#yecc.options) orelse ReportWerror of
        true ->
            foreach(fun({File,{none,Mod,W}}) -> 
                            io:fwrite(<<"~ts: ~s~ts\n">>,
                                      [File,Prefix,
				       Mod:format_error(W)]);
                       ({File,{Line,Mod,W}}) -> 
                            io:fwrite(<<"~ts:~w: ~s~ts\n">>,
                                      [File,Line,Prefix,
				       Mod:format_error(W)])
                    end, sort(St#yecc.warnings));
        false -> 
            ok
    end.

add_error(E, St) ->
    add_error(none, E, St).

add_error(Anno, E, St) ->
    add_error(St#yecc.infile, Anno, E, St).

add_error(File, Anno, E, St) ->
    Loc = location(Anno),
    St#yecc{errors = [{File,{Loc,?MODULE,E}}|St#yecc.errors]}.

add_errors(SymNames, E0, St0) ->
    foldl(fun(SymName, St) ->
                  add_error(symbol_anno(SymName, St), {E0, SymName}, St)
          end, St0, SymNames).

add_warning(Anno, W, St) ->
    Loc = location(Anno),
    St#yecc{warnings = [{St#yecc.infile,{Loc,?MODULE,W}}|St#yecc.warnings]}.

add_warnings(SymNames, W0, St0) ->
    foldl(fun(SymName, St) ->
                  add_warning(symbol_anno(SymName, St), {W0, SymName}, St)
          end, St0, SymNames).

check_rhs([#symbol{name = '$empty'}], St) ->
    St;
check_rhs(Rhs, St0) ->
    case symbol_find('$empty', Rhs) of
        #symbol{anno = Anno} ->
            add_error(Anno, illegal_empty, St0);
        false ->
            foldl(fun(Sym, St) ->
                          case symbol_member(Sym, St#yecc.all_symbols) of
                              true -> 
                                  St;
                              false -> 
                                  E = {undefined_symbol,Sym#symbol.name},
                                  add_error(Sym#symbol.anno, E, St)
                          end
                  end, St0, Rhs)
    end.

check_action(Tokens) ->
    case erl_parse:parse_exprs(add_roberts_dot(Tokens, erl_anno:new(0))) of
        {error, _Error} ->
            {false, false};
        {ok, [Expr | Exprs]} ->
            IsGuard = Exprs =:= [] andalso erl_lint:is_guard_test(Expr),
            {IsGuard, true}
    end.

add_roberts_dot([], Anno) ->
    [{'dot', Anno}];
add_roberts_dot([{'dot', Anno} | _], _) ->
    [{'dot', Anno}];
add_roberts_dot([Token | Tokens], _) ->
    [Token | add_roberts_dot(Tokens, element(2, Token))].

subst_pseudo_vars([], _, St) ->
    {[], St};
subst_pseudo_vars([H0 | T0], NmbrOfDaughters, St0) ->
    {H, St1} = subst_pseudo_vars(H0, NmbrOfDaughters, St0),
    {T, St} = subst_pseudo_vars(T0, NmbrOfDaughters, St1),
    {[H | T], St};
subst_pseudo_vars({atom, Anno, Atom}, NmbrOfDaughters, St0) ->
    case atom_to_list(Atom) of
        [$$ | Rest] ->
            try list_to_integer(Rest) of
                N when N > 0, N =< NmbrOfDaughters ->
                    {{var, Anno, list_to_atom(append("__", Rest))}, St0};
                _ ->
                    St = add_error(Anno,
                                   {undefined_pseudo_variable, Atom},
                                   St0),
                    {{atom, Anno, '$undefined'}, St}
            catch 
                error: _ -> {{atom, Anno, Atom}, St0}
            end;
        _ ->
            {{atom, Anno, Atom}, St0}
    end;
subst_pseudo_vars(Tuple, NmbrOfDaughters, St0) when is_tuple(Tuple) ->
    {L, St} = subst_pseudo_vars(tuple_to_list(Tuple), NmbrOfDaughters, St0),
    {list_to_tuple(L), St};
subst_pseudo_vars(Something_else, _, St) ->
    {Something_else, St}.

kind_of_symbol(St, SymName) ->
    case member(SymName, St#yecc.nonterminals) of
        false ->
            case member(SymName, St#yecc.terminals) of
                false ->
                    case St#yecc.endsymbol of
                        SymName ->
                            endsymbol;
                        _ ->
                            unknown
                    end;
                true ->
                    terminal
            end;
        true ->
            nonterminal
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing parse states and goto table from grammar.
% Start item: {0, [Endsymbol]} <->
% (['ACCEPT' '.', Rootsymbol], {'$'}) in Aho & Johnson
% where '$end' is the default end of input symbol of the
% scanner if no 'Endsymbol' has been declared in the syntax file.

-record(tabs, {
          symbols,      % ETS-set, keypos 1: {SymbolName, SymbolCode}
          inv_symbols,  % ETS-set, keypos 2: {SymbolName, SymbolCode}
          state_id,     % ETS-bag, keypos 1: {StateId, StateNum}
                        % StateId is not unique for a state.
          rp_rhs,       % rule pointer -> the remaining rhs symbols
          rp_info,      % rule pointer -> expanding rules and lookahead
          goto          % ETS-bag, keypos 1: first 
                        % {{FromStateNum, Symbol, ToStateNum}}, then
                        % {{FromStateNum, Symbol}, ToStateNum}
         }).

-record(item, { % what states are made of
          rule_pointer,
          look_ahead,
          rhs
         }).

compute_states(St0) ->
    SymbolTab = St0#yecc.symbol_tab,
    CodedRules = map(fun(#rule{symbols = Syms} = R) ->
                             R#rule{symbols = code_symbols(Syms, SymbolTab)}
                     end, St0#yecc.rules_list),
    CodedNonterminals = code_symbols(St0#yecc.nonterminals, SymbolTab),
    %% Only coded in this phase; StC is thrown away.
    StC = St0#yecc{rules_list = CodedRules, 
                   rules = list_to_tuple(CodedRules),
                   nonterminals = CodedNonterminals},
    {RuleIndex, RulePointer2Rule} = 
        make_rule_index(StC, St0#yecc.rules_list),
    StateTab0 = {},
    StateIdTab = ets:new(yecc_state_id, [set]),
    GotoTab = ets:new(yecc_goto, [bag]),
    RulePointerRhs = make_rhs_index(StC#yecc.rules_list),
    RulePointerInfo = make_rule_pointer_info(StC, RulePointerRhs, RuleIndex),

    Tables = #tabs{symbols = SymbolTab, 
                   state_id = StateIdTab,
                   rp_rhs = RulePointerRhs,
                   rp_info = RulePointerInfo,
                   goto = GotoTab},

    _ = erase(),
    EndsymCode = code_terminal(StC#yecc.endsymbol, StC#yecc.symbol_tab),
    {StateId, State0} = compute_state([{EndsymCode, 1}], Tables),

    StateNum0 = first_state(),
    FirstState = {StateNum0, State0},
    StateTab1 = insert_state(Tables, StateTab0, FirstState, StateId),
    {StateTab, N} = 
        compute_states1([{StateNum0, get_current_symbols(State0)}], 
                        FirstState, StateTab1, Tables),
    true = ets:delete(StateIdTab),
    St = St0#yecc{state_tab = StateTab, goto_tab = GotoTab, n_states = N,
                  rule_pointer2rule = RulePointer2Rule},
    decode_goto(GotoTab, St#yecc.inv_symbol_tab),
    check_usage(St).

first_state() ->
    0.

decode_goto(GotoTab, InvSymTab) ->
    G = ets:tab2list(GotoTab),
    ets:delete_all_objects(GotoTab),
    ets:insert(GotoTab, 
               map(fun({{From, Sym, Next}}) ->
                           {{From, decode_symbol(Sym, InvSymTab)}, Next}
                   end, G)).

check_usage(St0) ->
    SelSyms = ets:fun2ms(fun({{_,Sym},_}) -> Sym end),
    UsedSymbols = ets:select(St0#yecc.goto_tab, SelSyms),
    Syms = ordsets:from_list([?ACCEPT, '$empty' | UsedSymbols]),
    NonTerms = ordsets:from_list(St0#yecc.nonterminals),
    UnusedNonTerms = ordsets:to_list(ordsets:subtract(NonTerms, Syms)),
    St1 = add_warnings(UnusedNonTerms, unused_nonterminal, St0),
    Terms = ordsets:from_list(St0#yecc.terminals),
    St2 = add_warnings(ordsets:to_list(ordsets:subtract(Terms, Syms)),
                       unused_terminal, St1),
    DefinedNonTerminals = map(fun(#rule{symbols = [Name | _]}) -> 
                                            Name
                              end, St2#yecc.rules_list),
    DefNonTerms = ordsets:from_list(DefinedNonTerminals),
    UndefNonTerms = ordsets:subtract(NonTerms, DefNonTerms),
    add_errors(ordsets:to_list(ordsets:subtract(UndefNonTerms, 
                                                UnusedNonTerms)),
               missing_syntax_rule, St2).

%% States are sometimes big, should not be copied to ETS tables.
%% Here an "extendible" tuple is used.
lookup_state(StateTab, N) ->
    element(N+1, StateTab).

insert_state(#tabs{state_id = StateIdTab}, StateTab0, State, StateId) ->
    {N, _Items} = State,
    insert_state_id(StateIdTab, N, StateId),
    StateTab = if 
                   tuple_size(StateTab0) > N ->
                       StateTab0;
                   true ->
                       list_to_tuple(tuple_to_list(StateTab0) ++
                                     lists:duplicate(round(1 + N * 1.5), []))
               end,
    setelement(N+1, StateTab, State).

insert_state_id(StateIdTab, N, StateId) ->
    true = ets:insert(StateIdTab, {StateId, N}).

compute_states1([], {N, _}=_CurrState, StateTab0, _Tables) ->
    {StateTab0, N};
compute_states1([{N, Symbols} | Try], CurrState, StateTab, Tables) ->
    {_N, S} = lookup_state(StateTab, N),
    Seeds = state_seeds(S, Symbols),
    compute_states2(Seeds, N, Try, CurrState, StateTab, Tables).

compute_states2([], _N, Try, CurrState, StateTab, Tables) ->
    compute_states1(Try, CurrState, StateTab, Tables);
compute_states2([{Sym,Seed} | Seeds], N, Try, CurrState, StateTab, Tables) ->
    {StateId, NewState} = compute_state(Seed, Tables),
    case check_states(NewState, StateId, StateTab, Tables) of
        add ->
            {M, _} = CurrState,
            %% io:fwrite(<<"Adding state ~w\n">>, [M + 1]),
            CurrentSymbols = get_current_symbols(NewState),
            Next = M + 1,
            NextState = {Next, NewState},
            NewStateTab = insert_state(Tables, StateTab, NextState, StateId),
            insert_goto(Tables, N, Sym, Next),
            compute_states2(Seeds, N, [{Next, CurrentSymbols} | Try],
                            NextState, NewStateTab, Tables);
        {old, M} ->
            %% io:fwrite(<<"Identical to old state ~w\n">>, [M]),
            insert_goto(Tables, N, Sym, M),
            compute_states2(Seeds, N, Try, CurrState, StateTab, Tables);
        {merge, M, NewCurrent} ->
            %% io:fwrite(<<"Merging with state ~w\n">>, [M]),
            Try1 = case lists:keyfind(M, 1, Try) of
                       false ->
                           [{M, NewCurrent} | Try];
                       {_, OldCurrent} ->
                           case ordsets:is_subset(NewCurrent, OldCurrent) of
                               true ->
                                   Try;
                               false ->
                                   [{M, ordsets:union(NewCurrent, OldCurrent)}
                                    | keydelete(M, 1, Try)]
                           end
                   end,
            NewStateTab = merge_states(NewState, StateTab, Tables, M,StateId),
            insert_goto(Tables, N, Sym, M),
            compute_states2(Seeds, N, Try1, CurrState, NewStateTab, Tables)
    end.

insert_goto(Tables, From, Sym, To) ->
    true = ets:insert(Tables#tabs.goto, {{From, Sym, To}}).

%% Create an ets table for faster lookups.
create_symbol_table(St) ->
    #yecc{terminals = Terminals, endsymbol = Endsymbol} = St,
    SymbolTab = ets:new(yecc_symbols, [{keypos,1}]),
    %% '$empty' is always assigned 0
    Ts = ['$empty', Endsymbol | delete('$empty', Terminals)],
    TsC = count(0, Ts),
    NTsC = map(fun({NT,I}) -> {NT,-I} end, count(1, St#yecc.nonterminals)),
    Cs = TsC++NTsC,
    true = ets:insert(SymbolTab, Cs),

    InvSymTable = ets:new(yecc_inverted_terminals, [{keypos,2}]),
    true = ets:insert(InvSymTable, Cs),

    St#yecc{symbol_tab = SymbolTab, inv_symbol_tab = InvSymTable}.

get_current_symbols(State) ->
    usort(get_current_symbols1(State, [])).

get_current_symbols1([], Syms) ->
    Syms;
get_current_symbols1([#item{rhs = Rhs} | Items], Syms) ->
    case Rhs of
        [] ->
            get_current_symbols1(Items, Syms);
        [Symbol | _] ->
            get_current_symbols1(Items, [Symbol | Syms])
    end.

state_seeds(Items, Symbols) ->
    L = [{S,{LA,RP + 1}} || #item{rule_pointer = RP, look_ahead = LA, 
                                  rhs = [S | _]} <- Items],
    state_seeds1(keysort(1, L), Symbols).

state_seeds1(_L, []) ->
    [];
state_seeds1(L, [Symbol | Symbols]) ->
    state_seeds(L, Symbol, Symbols, []).

state_seeds([{Symbol, Item} | L], Symbol, Symbols, Is) ->
    state_seeds(L, Symbol, Symbols, [Item | Is]);
state_seeds([{S, _Item} | L], Symbol, Symbols, Is) when S < Symbol ->
    state_seeds(L, Symbol, Symbols, Is);
state_seeds(L, Symbol, Symbols, Is) ->
    [{Symbol, Is} | state_seeds1(L, Symbols)].

compute_state(Seed, Tables) ->
    RpInfo = Tables#tabs.rp_info,
    foreach(fun({LA, RulePointer}) -> put(RulePointer, LA) end, Seed),
    foreach(fun({LA, RP}) -> compute_closure(LA, RP, RpInfo) end, Seed),
    Closure = keysort(1, erase()),
    state_items(Closure, [], [], Tables#tabs.rp_rhs).

%% Collects a uniqe id for the state (all rule pointers). 
state_items([{RP, LA} | L], Is, Id, RpRhs) ->
    I = #item{rule_pointer = RP, look_ahead = LA, rhs = element(RP, RpRhs)},
    state_items(L, [I | Is], [RP | Id], RpRhs);
state_items(_, Is, Id, _RpRhs) ->
    {Id, Is}.

-compile({inline,[compute_closure/3]}).
compute_closure(Lookahead, RulePointer, RpInfo) ->
    case element(RulePointer, RpInfo) of
        []=Void -> % no followers, or terminal
            Void;
        {no_union, ExpandingRules, NewLookahead} ->
            compute_closure1(ExpandingRules, NewLookahead, RpInfo);
        {union, ExpandingRules, Lookahead0} ->
            NewLookahead = set_union(Lookahead0, Lookahead),
            compute_closure1(ExpandingRules, NewLookahead, RpInfo);
        ExpandingRules ->
            compute_closure1(ExpandingRules, Lookahead, RpInfo)
    end.
    
compute_closure1([RulePointer | Tail], NewLookahead, RpInfo) ->
    compute_closure1(Tail, NewLookahead, RpInfo),
    case get(RulePointer) of
        undefined -> % New
            put(RulePointer, NewLookahead),
            compute_closure(NewLookahead, RulePointer, RpInfo);
        Lookahead2 ->
            Lookahead = set_union(Lookahead2, NewLookahead),
            if 
                Lookahead =:= Lookahead2 -> % Old
                    Lookahead2; % void()
                true -> % Merge
                    put(RulePointer, Lookahead),
                    compute_closure(NewLookahead, RulePointer, RpInfo)
            end
    end;
compute_closure1(Nil, _, _RpInfo) ->
    Nil.

%% Check if some old state is a superset of our NewState
check_states(NewState, StateId, StateTab, #tabs{state_id = StateIdTab}) ->
    try ets:lookup_element(StateIdTab, StateId, 2) of
        N ->
            {_N, OldState} = lookup_state(StateTab, N),
            check_state1(NewState, OldState, [], N)
    catch error:_ -> add
    end.

check_state1([#item{look_ahead = Lookahead1, rhs = Rhs} | Items1],
             [#item{look_ahead = Lookahead2} | Items2], Symbols, N) ->
    case set_is_subset(Lookahead1, Lookahead2) of
        true ->
            check_state1(Items1, Items2, Symbols, N);
        false ->
            case Rhs of
                [] ->
                    check_state2(Items1, Items2, Symbols, N);
                [Symbol | _] ->
                    check_state2(Items1, Items2, [Symbol | Symbols], N)
            end
    end;
check_state1([], [], _Symbols, N) ->
    {old, N}.

check_state2([#item{look_ahead = Lookahead1, rhs = Rhs} | Items1],
             [#item{look_ahead = Lookahead2} | Items2], Symbols, N) ->
    case set_is_subset(Lookahead1, Lookahead2) of
        true ->
            check_state2(Items1, Items2, Symbols, N);
        false ->
            case Rhs of
                [] ->
                    check_state2(Items1, Items2, Symbols, N);
                [Symbol | _] ->
                    check_state2(Items1, Items2, [Symbol | Symbols], N)
            end
    end;
check_state2([], [], Symbols, N) ->
    {merge, N, usort(Symbols)}.

merge_states(NewState, StateTab, Tables, M, StateId) ->
    {_M, Old_state} = lookup_state(StateTab, M),
    MergedState = merge_states1(NewState, Old_state),
    insert_state(Tables, StateTab, {M, MergedState}, StateId).

merge_states1([Item1 | Items1], [Item2 | Items2]) ->
    LA1 = Item1#item.look_ahead,
    LA2 = Item2#item.look_ahead,
    if
        LA1 =:= LA2 ->
            [Item1 | merge_states1(Items1, Items2)];
        true ->
            [Item1#item{look_ahead = set_union(LA1, LA2)}
             | merge_states1(Items1, Items2)]
    end;
merge_states1(_, _) ->
    [].

%% RulePointer -> Rhs. Every position Rhs in has its unique "rule pointer".
make_rhs_index(RulesList) ->
    Index = flatmap(fun(#rule{symbols = [_Non | Daughters]}) ->
                            suffixes0(Daughters)
                    end, RulesList),
    list_to_tuple(Index).

suffixes0([?EMPTY]) ->
    [[], []];
suffixes0(L) ->
    suffixes(L).

suffixes([]=L) ->
    [L];
suffixes([_ | T]=L) ->
    [L | suffixes(T)].

%% Setup info about lookahead and expanding rules for each point
%% ("rule pointer") in the grammar. 
make_rule_pointer_info(StC, RpRhs, RuleIndex) ->
    SymbolTab = StC#yecc.symbol_tab,
    LcTab = make_left_corner_table(StC),
    LA_index = map(fun(Syms) ->
                           rp_info(Syms, SymbolTab, LcTab, RuleIndex)
                   end, tuple_to_list(RpRhs)),
    list_to_tuple(LA_index).

rp_info([], _SymbolTab, _LcTab, _RuleIndex) ->
    [];
rp_info([Category | Followers], SymbolTab, LcTab, RuleIndex) ->
    case dict:find(Category, RuleIndex) of
        error -> % terminal
            [];
        {ok, ExpandingRules} when Followers =:= [] ->
            ExpandingRules;
        {ok, ExpandingRules} ->
            case make_lookahead(Followers, SymbolTab, LcTab, set_empty()) of
                {empty, LA} ->
                    {union, ExpandingRules, LA};
                LA ->
                    {no_union, ExpandingRules, LA}
            end
    end.

%% Lookahead computation is complicated by the possible existence
%% of null string rewriting rules, such as A -> '$empty'.
make_lookahead([], _, _, LA) ->
    {empty, LA};
make_lookahead([Symbol | Symbols], SymbolTab, LcTab, LA) ->
    case dict:find(Symbol, LcTab) of
        {ok, LeftCorner} -> % nonterminal
            case empty_member(LeftCorner) of
                true ->
                    make_lookahead(Symbols, SymbolTab, LcTab,
                                   set_union(empty_delete(LeftCorner), LA));
                false ->
                    set_union(LeftCorner, LA)
            end;
        error -> % terminal
            set_add(Symbol, LA)
    end.

%% -> dict-of({Nonterminal, [Terminal]}).
%% The algorithm FIRST/1 from the Dragon Book.
%% Left corner table, all terminals (including '$empty') that can
%% begin strings generated by Nonterminal.
make_left_corner_table(#yecc{rules_list = RulesList} = St) ->
    SymbolTab = left_corner_symbol_table(St),
    Rules = map(fun(#rule{symbols = [Lhs | Rhs]}) ->
                        {Lhs,{Lhs, Rhs}}
                end, RulesList),
    LeftHandTab = dict:from_list(family(Rules)),
    X0 = [{S,H} || {H,{H,Rhs}} <- Rules, 
                   S <- Rhs, 
                   not is_terminal(SymbolTab, S)],
    XL = family_with_domain(X0, St#yecc.nonterminals),
    X = dict:from_list(XL),
    Xref = fun(NT) -> dict:fetch(NT, X) end,
    E = set_empty(),
    LC0 = dict:from_list([{H, E} || {H,_} <- XL]),
    %% Handle H -> a S, where a is a terminal ('$empty' inclusive).
    {Q, LC1} =
        foldl(fun({H,{H,[S | _]}}, {Q0, LC}) ->
                      case ets:lookup(SymbolTab, S) of
                          [{_,Num}=SymbolAndNum] when Num >= 0 ->
                              F = set_add_terminal(SymbolAndNum, E),
                              {[Xref(H) | Q0], upd_first(H, F, LC)};
                          _ ->
                              {Q0, LC}
                      end
              end, {[], LC0}, Rules),
    left_corners(Q, LC1, LeftHandTab, SymbolTab, Xref).

left_corners(Q0, LC0, LeftHandTab, SymbolTab, Xref) ->
    case usort(append(Q0)) of
        [] -> 
            LC0;
        Q1 -> 
            Rs = flatmap(fun(NT) -> dict:fetch(NT, LeftHandTab) end, Q1),
            {LC, Q} = left_corners2(Rs, LC0, [], SymbolTab, Xref),
            left_corners(Q, LC, LeftHandTab, SymbolTab, Xref)
    end.
    
left_corners2([], LC, Q, _SymbolTab, _Xref) ->
    {LC, Q};
left_corners2([{Head,Rhs} | Rs], LC, Q0, SymbolTab, Xref) ->
    Ts = left_corner_rhs(Rhs, Head, LC, set_empty(), SymbolTab),
    First0 = dict:fetch(Head, LC),
    case set_is_subset(Ts, First0) of
        true ->
            left_corners2(Rs, LC, Q0, SymbolTab, Xref);
        false ->
            LC1 = upd_first(Head, Ts, LC),
            left_corners2(Rs, LC1, [Xref(Head) | Q0], SymbolTab, Xref)
    end.

upd_first(NT, Ts, LC) ->
    dict:update(NT, fun(First) -> set_union(First, Ts) end, LC).

left_corner_rhs([S | Ss], Head, LC, Ts, SymbolTab) ->
    case ets:lookup(SymbolTab, S) of
        [{_,Num}=SymbolAndNum] when Num >= 0 ->
            set_add_terminal(SymbolAndNum, Ts);
        [_NonTerminalSymbol] ->
            First = dict:fetch(S, LC),
            case empty_member(First) of
                true ->
                    NTs = set_union(empty_delete(First), Ts),
                    left_corner_rhs(Ss, Head, LC, NTs, SymbolTab);
                false ->
                    set_union(First, Ts)
            end
    end;
left_corner_rhs([], _Head, _LC, Ts, _SymbolTab) ->
    set_add(?EMPTY, Ts).

%% For every non-terminal return a list of "rule pointers" for rules
%% expanding the non-terminal.
%% Also assigns a unique number to each point in the grammar, "rule pointer".
make_rule_index(#yecc{nonterminals = Nonterminals, 
                      rules_list = RulesList}, RulesListNoCodes) ->
    {RulesL, _N} = 
        lists:mapfoldl(fun(#rule{symbols = [Nonterminal | Daughters]}, I) ->
                               I1 = I + length(Daughters)+1,
                               {{Nonterminal, I}, I1}
                       end, 1, RulesList),
    IndexedTab = family_with_domain(RulesL, Nonterminals),

    Symbol2Rule = [{Foo,R} || #rule{symbols = Symbols}=R <- RulesListNoCodes,
                              Foo <- Symbols],
    Pointer2Rule = [{I, R} || {{_Foo,R},I} <- count(1, Symbol2Rule)],
    {dict:from_list(IndexedTab), dict:from_list(Pointer2Rule)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Computing parse action table from list of states and goto table:

compute_parse_actions(N, St, StateActions) ->
    case N < first_state() of
        true -> 
            StateActions;
        false ->
            {N, StateN} = lookup_state(St#yecc.state_tab, N),
            %% There can be duplicates in Actions.
            Actions = compute_parse_actions1(StateN, N, St),
            compute_parse_actions(N - 1, St, [{N, Actions} | StateActions])
    end.

compute_parse_actions1([], _, _) ->
    [];
compute_parse_actions1([#item{rule_pointer = RulePointer, 
                              look_ahead = Lookahead0, 
                              rhs = Rhs} | Items], N, St) ->
    case Rhs of
        [] ->
            Lookahead = decode_terminals(Lookahead0, St#yecc.inv_symbol_tab),
            case rule(RulePointer, St) of
                {[?ACCEPT | _], _RuleLine, _} ->
                    [{Lookahead, accept}
                     | compute_parse_actions1(Items, N, St)];
                %% Head is placed after the daughters when finding the
                %% precedence. This is how giving precedence to
                %% non-terminals takes effect.
                {[Head | Daughters0], _RuleLine, _} ->
                    Daughters = delete('$empty', Daughters0),
                    [{Lookahead,
                      #reduce{rule_nmbr = RulePointer, head = Head, 
                              nmbr_of_daughters = length(Daughters), 
                              prec = get_prec(Daughters ++ [Head], St)}}
                     | compute_parse_actions1(Items, N, St)]
            end;
        [Symbol | Daughters] ->
            case is_terminal(St#yecc.symbol_tab, Symbol) of
                true ->
                    DecSymbol = decode_symbol(Symbol, St#yecc.inv_symbol_tab),
                    {[Head | _], _RuleLine, _} = rule(RulePointer, St),
                    %% A bogus shift-shift conflict can be introduced
                    %% here if some terminal occurs in different rules
                    %% which have been given precedence "one level up".
                    Prec1 = case Daughters of
                                [] -> get_prec([DecSymbol, Head], St);
                                _ -> get_prec([DecSymbol], St)
                            end,
                    Pos = case Daughters of
                              [] -> z;
                              _ -> a
                          end,
                    [{[DecSymbol],
                      #shift{state = goto(N, DecSymbol, St), 
                             pos = Pos,
                             prec = Prec1,
                             rule_nmbr = RulePointer}}
                     | compute_parse_actions1(Items, N, St)];
                false ->
                    compute_parse_actions1(Items, N, St)
            end
    end.

get_prec(Symbols, St) ->
    get_prec1(Symbols, St#yecc.prec_tab, {0, none}).

get_prec1([], _, P) ->
    P;
get_prec1([Symbol | T], PrecTab, P) ->
    case ets:lookup(PrecTab, Symbol) of
        [] ->
            get_prec1(T, PrecTab, P);
        [{_, N, Ass}] ->
            get_prec1(T, PrecTab, {N, Ass})
    end.

create_precedence_table(St) ->
    PrecTab = ets:new(yecc_precedences, []),
    true = ets:insert(PrecTab, St#yecc.prec),
    St#yecc{prec_tab = PrecTab}.
    
-record(cxt, {terminal, state_n, yecc, res}).

%% Detects shift-reduce and reduce-reduce conflicts.
%% Also removes all but one conflicting action. As a consequence the
%% lookahead sets for a state are always disjoint.
%% Reduce/reduce conflicts are considered errors.
find_action_conflicts(St0) ->
    Cxt0 = #cxt{yecc = St0, res = []},
    {#cxt{yecc = St, res = Res}, NewParseActions0} = 
        foldl(fun({N, Actions0}, {Cxt1, StateActions}) ->
                      L = [{Terminal, Act} || {Lookahead, Act} <- Actions0,
                                              Terminal <- Lookahead],
                      {Cxt, Actions} = 
                          foldl(fun({Terminal, As}, {Cxt2,Acts0}) ->
                                        Cxt3 = Cxt2#cxt{terminal = Terminal, 
                                                        state_n = N},
                                        {Action, Cxt} = 
                                            find_action_conflicts2(As, Cxt3),
                                        {Cxt,[{Action,Terminal} | Acts0]}
                                end, {Cxt1,[]}, family(L)),
                      {Cxt,[{N,inverse(family(Actions))} | StateActions]}
              end, {Cxt0, []}, St0#yecc.parse_actions),
    if 
        length(Res) > 0, St#yecc.verbose -> 
            io:fwrite(<<"\n*** Conflicts resolved by operator "
                        "precedences:\n\n">>),
            foreach(fun({Confl, Name}) ->
                            report_conflict(Confl, St, Name, prec)
                    end, reverse(Res)),
            io:fwrite(<<"*** End of resolved conflicts\n\n">>);
        true -> 
            ok
    end,
    NewParseActions = reverse(NewParseActions0),
    St#yecc{parse_actions = NewParseActions}.

find_action_conflicts2([Action], Cxt) ->
    {Action, Cxt};
find_action_conflicts2([#shift{state = St, pos = Pos, prec = Prec},
                        #shift{state = St}=S | As], 
                       Cxt) when Pos =:= a; Prec =:= {0,none} ->
    %% This is a kludge to remove the bogus shift-shift conflict
    %% introduced in compute_parse_actions1().
    find_action_conflicts2([S | As], Cxt);
find_action_conflicts2([#shift{state = NewState, pos = z}=S1,
                        #shift{state = NewState}=S2 | _], Cxt) ->
    %% This is even worse than last clause. Give up.
    Confl = conflict(S1, S2, Cxt),
    #cxt{yecc = St0} = Cxt,
    St = conflict_error(Confl, St0),
    {S1, Cxt#cxt{yecc = St}}; % return any action
find_action_conflicts2([#shift{prec = {P1, Ass1}}=S | Rs], Cxt0) ->
    {R, Cxt1} = find_reduce_reduce(Rs, Cxt0),
    #cxt{res = Res0, yecc = St0} = Cxt1,
    #reduce{prec = {P2, Ass2}} = R,
    Confl = conflict(R, S, Cxt1),
    if
        P1 > P2 ->
            {S, Cxt1#cxt{res = [{Confl, shift} | Res0]}};
        P2 > P1 ->
            {R, Cxt1#cxt{res = [{Confl, reduce} | Res0]}};
        Ass1 =:= left, Ass2 =:= left ->
            {R, Cxt1#cxt{res = [{Confl, reduce} | Res0]}};
        Ass1 =:= right, Ass2 =:= right ->
            {S, Cxt1#cxt{res = [{Confl, shift} | Res0]}};
        Ass1 =:= nonassoc, Ass2 =:= nonassoc ->
            {nonassoc, Cxt1};
        P1 =:= 0, P2 =:= 0 ->
            report_conflict(Confl, St0, shift, default),
            St = add_conflict(Confl, St0),
            {S, Cxt1#cxt{yecc = St}};
        true ->
            St = conflict_error(Confl, St0),
            {S, Cxt1#cxt{yecc = St}} % return any action
    end;
find_action_conflicts2(Rs, Cxt0) ->
    find_reduce_reduce(Rs, Cxt0).
         
find_reduce_reduce([R], Cxt) ->
    {R, Cxt};
find_reduce_reduce([accept=A, #reduce{}=R | Rs], Cxt0) ->
    Confl = conflict(R, A, Cxt0),
    St = conflict_error(Confl, Cxt0#cxt.yecc), 
    Cxt = Cxt0#cxt{yecc = St},
    find_reduce_reduce([R | Rs], Cxt);
find_reduce_reduce([#reduce{head = Categ1, prec = {P1, _}}=R1, 
                    #reduce{head = Categ2, prec = {P2, _}}=R2 | Rs], Cxt0) ->
    #cxt{res = Res0, yecc = St0} = Cxt0,
    Confl = conflict(R1, R2, Cxt0),
    {R, Res, St} = 
        if
            P1 > P2 ->
                {R1, [{Confl, Categ1} | Res0], St0};
            P2 > P1 ->
                {R2, [{Confl, Categ2} | Res0], St0};
            true ->
                St1 = conflict_error(Confl, St0), 
                {R1, Res0, St1}
        end,
    Cxt = Cxt0#cxt{res = Res, yecc = St},
    find_reduce_reduce([R | Rs], Cxt).

%% Since the lookahead sets are disjoint (assured by
%% find_action_conflicts), the order between actions can be chosen
%% almost arbitrarily. nonassoc has to come last, though (but is later
%% discarded!). And shift has to come before reduce.
sort_parse_actions([]) ->
    [];
sort_parse_actions([{N, La_actions} | Tail]) ->
    [{N, sort_parse_actions1(La_actions)} | sort_parse_actions(Tail)].

sort_parse_actions1(LaActions) ->
    As = filter(fun({_LA, A}) -> A =:= accept end, LaActions),
    Ss = filter(fun({_LA, A}) -> is_record(A, shift) end, LaActions),
    Rs = filter(fun({_LA, A}) -> is_record(A, reduce) end, LaActions),
    Ns = filter(fun({_LA, A}) -> A =:= nonassoc end, LaActions),
    As ++ Ss ++ Rs ++ Ns.

%% -> {State, StateRepr}. StateRepr has the same set of shift actions
%% as State. No code will be output for State if State =/= StateRepr.
find_identical_shift_states(StateActions) ->
    L1 = [{Actions, State} || {State,Actions} <- StateActions],
    {SO, NotSO} = lists:partition(fun({Actions,_States}) ->
                                          shift_actions_only(Actions)
                                  end, family(L1)),
    R = [{State, hd(States)} || {_Actions, States} <- SO, State <- States]
        ++ 
        [{State, State} || {_Actions, States} <- NotSO, State <- States],
    lists:keysort(1, R).

-record(part_data, {name, eq_state, actions, n_actions, states}).

%% Replace {SStates,Actions} with {SStates,{Actions,Jump}} where
%% Jump describes which clauses that have been extracted from shift
%% states so that they can be used from other states. Some space is
%% saved.
find_partial_shift_states(StateActionsL, StateReprs) ->
    L = [{State, Actions} || {{State,Actions}, {State,State}} <-
                                 lists:zip(StateActionsL, StateReprs),
                             shift_actions_only(Actions)],
    StateActions = sofs:family(L, [{state,[action]}]),
    StateAction = sofs:family_to_relation(StateActions),

    %% Two actions are equal if they occur in the same states:
    Parts = sofs:partition(sofs:range(StateActions)),
    PartsL = sofs:to_external(Parts),
    %% Assign temporary names to the parts of the partition (of actions):
    PartNameL = lists:zip(seq1(length(PartsL)), PartsL),
    ActPartL = [{Action,PartName} || 
                   {PartName,Actions} <- PartNameL,
                   Action <- Actions],
    ActionPartName = sofs:relation(ActPartL, [{action,partname}]),
    StatePartName = sofs:relative_product(StateAction, ActionPartName),
    PartInStates = sofs:relation_to_family(sofs:converse(StatePartName)),

    %% Parts that equal all actions of a state:
    PartActions = sofs:family(PartNameL, [{partname,[action]}]),
    PartState = 
        sofs:relative_product(PartActions, sofs:converse(StateActions)),
    PartStates = sofs_family_with_domain(PartState, sofs:domain(PartActions)),

    PartDataL = [#part_data{name = Nm, eq_state = EqS, actions = P, 
                            n_actions = length(P), 
                            states = ordsets:from_list(S)} || 
                    {{Nm,P}, {Nm,S}, {Nm,EqS}} <- 
                        lists:zip3(PartNameL, 
                                   sofs:to_external(PartInStates),
                                   sofs:to_external(PartStates))],
    true = length(PartDataL) =:= length(PartNameL),
    Ps = select_parts(PartDataL),

    J1 = [{State, Actions, {jump_some,hd(States)}} ||
             {_W, #part_data{actions = Actions, eq_state = [], 
                             states = States}} <- Ps,
             State <- States],
    J2 = [{State, Actions, {jump_all,To}} ||
             {_W, #part_data{actions = Actions, eq_state = EqS, 
                             states = States}} <- Ps,
             To <- EqS,
             State <- States,
             State =/= To],
    J = lists:keysort(1, J1 ++ J2),

    JumpStates = ordsets:from_list([S || {S,_,_} <- J]),
    {JS, NJS} = 
        sofs:partition(1, sofs:relation(StateActionsL, [{state, actions}]),
                       sofs:set(JumpStates, [state])),
    R = 
        [{S, {Actions,jump_none}} || {S,Actions} <- sofs:to_external(NJS)]
        ++
        [{S, {Actions--Part, {Tag,ToS,Part}}} || 
            {{S,Actions}, {S,Part,{Tag,ToS}}} <- 
                lists:zip(sofs:to_external(JS), J)],
    true = length(StateActionsL) =:= length(R),
    lists:keysort(1, R).

%% Very greedy. By no means optimal. 
select_parts([]) ->
    [];
select_parts(PartDataL) ->
    T1 = [{score(PD), PD} || PD <- PartDataL],
    [{W, PD} | Ws] = lists:reverse(lists:keysort(1, T1)),
    #part_data{n_actions = NActions, states = S} = PD,
    if
        W < 8 -> % don't bother
            [];
        true ->
            %% Cannot extract more clauses from the chosen part's states:
            NL = [D#part_data{states = NewS} || 
                     {W1, #part_data{states = S0}=D} <- Ws,
                     W1 > 0,
                     (NewS = ordsets:subtract(S0, S)) =/= []],
            if 
                length(S) =:= 1; NActions =:= 1 ->
                    select_parts(NL);
                true -> 
                    [{W,PD} | select_parts(NL)]
            end
    end.

%% Does it pay off to extract clauses into a new function?
%% Assumptions:
%% - a call costs 8 (C = 8);
%% - a clause (per action) costs 20 plus 8 (select) (Cl = 28);
%% - a new function costs 20 (funinfo) plus 16 (select) (F = 36).
%% A is number of actions, S is number of states.
%% Special case (the part equals all actions of some state):
%% C * (S - 1) < (S - 1) * A * Cl
%% Normal case (introduce new function):
%% F + A * Cl + C * S < S * A * Cl
score(#part_data{n_actions = NActions, eq_state = [], states = S}) ->
    (length(S) * NActions * 28) - (36 + NActions * 28 + length(S) * 8);
score(#part_data{n_actions = NActions, states = S}) ->
    ((length(S) - 1) * NActions * 28) - (8 * (length(S) - 1)).

shift_actions_only(Actions) ->
    length([foo || {_Ts,{shift,_,_,_,_}} <- Actions]) =:= length(Actions).

collect_some_state_info(StateActions, StateReprs) ->
    RF = fun({_LA, A}) -> is_record(A, reduce) end,
    L = [{State, 
          begin
              RO = lists:all(RF, LaActions),
              %% C is currently always ""; identical states are all shift.
              C = [io_lib:fwrite(<<" %% ~w\n">>, [State]) || 
                      true <- [RO], Repr =/= State],
              #state_info{reduce_only = RO, state_repr = Repr, comment = C}
          end} ||
            {{State, LaActions}, {State, Repr}} <-
                lists:zip(StateActions, StateReprs)],
    list_to_tuple(L).

conflict_error(Conflict, St0) ->
    St1 = add_conflict(Conflict, St0),
    add_error({conflict, Conflict}, St1).

report_conflict(Conflict, St, ActionName, How) ->
    if
        St#yecc.verbose ->
            io:fwrite(<<"~s\n">>, [format_conflict(Conflict)]),
            Formated = format_symbol(ActionName),
            case How of 
                prec ->
                    io:fwrite(<<"Resolved in favor of ~ts.\n\n">>, [Formated]);
                default ->
                    io:fwrite(<<"Conflict resolved in favor of ~ts.\n\n">>, 
                              [Formated])
            end;
        true ->
            ok
    end.

add_conflict(Conflict, St) ->
    case Conflict of
        {Symbol, StateN, _, {reduce, _, _, _}} ->
            St#yecc{reduce_reduce = [{StateN,Symbol} |St#yecc.reduce_reduce]};
        {Symbol, StateN, _, {accept, _}} ->
            St#yecc{reduce_reduce = [{StateN,Symbol} |St#yecc.reduce_reduce]};
        {Symbol, StateN, _, {shift, _, _}} ->
            St#yecc{shift_reduce = [{StateN,Symbol} | St#yecc.shift_reduce]};
        {_Symbol, _StateN, {one_level_up, _, _}, _Confl} ->
            St
    end.

conflict(#shift{prec = Prec1, rule_nmbr = RuleNmbr1}, 
         #shift{prec = Prec2, rule_nmbr = RuleNmbr2}, Cxt) ->
    %% Conflict due to precedences "one level up". Kludge.
    #cxt{terminal = Symbol, state_n = N, yecc = St} = Cxt,    
    {_, L1, RuleN1} = rule(RuleNmbr1, St),
    {_, L2, RuleN2} = rule(RuleNmbr2, St),
    Confl = {one_level_up, {L1, RuleN1, Prec1}, {L2, RuleN2, Prec2}},
    {Symbol, N, Confl, Confl};
conflict(#reduce{rule_nmbr = RuleNmbr1}, NewAction, Cxt) ->
    #cxt{terminal = Symbol, state_n = N, yecc = St} = Cxt,
    {R1, RuleLine1, RuleN1} = rule(RuleNmbr1, St),
    Confl = case NewAction of
                accept -> 
                    {accept, St#yecc.rootsymbol};
                #reduce{rule_nmbr = RuleNmbr2} -> 
                    {R2, RuleLine2, RuleN2} = rule(RuleNmbr2, St),
                    {reduce, R2, RuleN2, RuleLine2};
                #shift{state = NewState} ->
                    {shift, NewState, last(R1)}
            end,
    {Symbol, N, {R1, RuleN1, RuleLine1}, Confl}.

format_conflict({Symbol, N, _, {one_level_up, 
                                {L1, RuleN1, {P1, Ass1}}, 
                                {L2, RuleN2, {P2, Ass2}}}}) ->
    S1 = io_lib:fwrite(<<"Conflicting precedences of symbols when "
                         "scanning ~ts in state ~w:\n">>, 
                       [format_symbol(Symbol), N]),
    S2 = io_lib:fwrite(<<"   ~s ~w (rule ~w at line ~w)\n"
                          "      vs.\n">>,
                       [format_assoc(Ass1), P1, RuleN1, L1]),
    S3 = io_lib:fwrite(<<"   ~s ~w (rule ~w at line ~w)\n">>, 
                       [format_assoc(Ass2), P2, RuleN2, L2]),
    [S1, S2, S3];
format_conflict({Symbol, N, Reduce, Confl}) ->
    S1 = io_lib:fwrite(<<"Parse action conflict scanning symbol "
                         "~ts in state ~w:\n">>, [format_symbol(Symbol), N]),
    S2 = case Reduce of
             {[HR | TR], RuleNmbr, RuleLine} ->
                 io_lib:fwrite(<<"   Reduce to ~ts from ~ts (rule ~w at "
                                 "line ~w)\n      vs.\n">>,
                               [format_symbol(HR), format_symbols(TR), 
                                RuleNmbr, RuleLine])
         end,
    S3 = case Confl of 
             {reduce, [HR2|TR2], RuleNmbr2, RuleLine2} ->
                 io_lib:fwrite(<<"   reduce to ~ts from ~ts "
                                 "(rule ~w at line ~w).">>,
                               [format_symbol(HR2), format_symbols(TR2), 
                                RuleNmbr2, RuleLine2]);
             {shift, NewState, Sym} ->
                 io_lib:fwrite(<<"   shift to state ~w, adding right "
                                 "sisters to ~ts.">>,
                               [NewState, format_symbol(Sym)]);
             {accept, Rootsymbol} ->
                 io_lib:fwrite(<<"   reduce to rootsymbol ~ts.">>,
                               [format_symbol(Rootsymbol)])
         end,
    [S1, S2, S3].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code generation:

%% The version up to and including parsetools-1.3 is called "1.0".
%%
%% "1.1", parsetools-1.4:
%% - the prologue file has been updated;
%% - nonassoc is new;
%% - different order of clauses;
%% - never more than one clause matching a given symbol in a given state;
%% - file attributes relate messages to .yrl file;
%% - actions put in inlined functions;
%% - a few other minor fixes.
%%
%% "1.2", parsetools-1.4.2:
%% - the generated code has been changed as follows:
%%   - yeccpars2() calls the functions yeccpars2_State();
%%   - several states can share yeccpars2_State(), which reduces code size;
%%   - yeccgoto() has been split on one function per nonterminal;
%%   - several minor changes have made the loaded code smaller.
%% - the include file yeccpre.hrl has been changed incompatibly.
%%
%% "1.3", parsetools-1.4.4:
%% - the generated code has been changed as follows:
%%   - yeccgoto_T() no longer returns the next state, but calls yeccpars_S();
%%   - yeccpars2() is not called when it is known which yeccpars2_S() to call;
%%   - "__Stack" has been substituted for "Stack";
%%   - several states can share yeccpars2_S_cont(), which reduces code size;
%%   - instead if calling lists:nthtail() matching code is emitted.
%%
%% "1.4", parsetools-2.0.4:
%% - yeccerror() is called when a syntax error is found (as in version 1.1).
%% - the include file yeccpre.hrl has been changed.

-define(CODE_VERSION, "1.4").
-define(YECC_BUG(M, A), 
        unicode:characters_to_binary(
          [" erlang:error({yecc_bug,\"",?CODE_VERSION,"\",",
           io_lib:fwrite(M, A), "}).\n\n"])).

%% Returns number of newlines in included files.
output_prelude(Outport, Inport, St0) when St0#yecc.includefile =:= [] ->
    St5 = output_header(St0),
    #yecc{infile = Infile, module = Module} = St5,
    St10 = fwrite(St5, <<"-module(~w).\n">>, [Module]),
    St20 = 
        fwrite(St10,
               <<"-export([parse/1, parse_and_scan/1, format_error/1]).\n">>,
               []),
    {St25, N_lines_1, LastErlangCodeLine} = 
        case St20#yecc.erlang_code of 
            none ->
                {St20, 0, no_erlang_code};
            Next_line ->
                St_10 = output_file_directive(St20, Infile, Next_line-1),
                Last_line = include1([], Inport, Outport, Infile,
                                     Next_line, St_10),
                Nmbr_of_lines = Last_line - Next_line,
                {St_10, Nmbr_of_lines, {last_erlang_code_line, Last_line}}
    end,
    St30 = nl(St25),
    IncludeFile = 
        filename:join([code:lib_dir(parsetools), "include","yeccpre.hrl"]),
    %% Maybe one could assume there are no warnings in this file.
    St = output_file_directive(St30, IncludeFile, 0),
    N_lines_2 = include(St, IncludeFile, Outport),
    {St, N_lines_1 + N_lines_2, LastErlangCodeLine};
output_prelude(Outport, Inport, St0) ->
    St5 = output_header(St0),
    #yecc{infile = Infile, module = Module, includefile = Includefile} = St5,
    St10 = fwrite(St5, <<"-module(~w).\n">>, [Module]),
    St20 = output_file_directive(St10, Includefile, 0),
    N_lines_1 = include(St20, Includefile, Outport),
    St30 = nl(St20),
    case St30#yecc.erlang_code of 
        none ->
            {St30, N_lines_1, no_erlang_code};
        Next_line ->
            St = output_file_directive(St30, Infile, Next_line-1),
            Last_line = include1([], Inport, Outport, Infile, Next_line, St),
            Nmbr_of_lines = Last_line - Next_line,
            {St, Nmbr_of_lines + N_lines_1, {last_erlang_code_line, Last_line}}
    end.

output_header(St0) ->
    lists:foldl(fun(Str, St) -> fwrite(St, <<"~ts\n">>, [Str])
                end, St0, St0#yecc.header).

output_goto(St, [{_Nonterminal, []} | Go], StateInfo) ->
    output_goto(St, Go, StateInfo);
output_goto(St0, [{Nonterminal, List} | Go], StateInfo) ->
    F = function_name(St0, yeccgoto, Nonterminal),
    St05 = fwrite(St0, <<"-dialyzer({nowarn_function, ~w/7}).\n">>, [F]),
    St10 = output_goto1(St05, List, F, StateInfo, true),
    St = output_goto_fini(F, Nonterminal, St10),
    output_goto(St, Go, StateInfo);
output_goto(St, [], _StateInfo) ->
    St.

output_goto1(St0, [{From, To} | Tail], F, StateInfo, IsFirst) ->
    St10 = delim(St0, IsFirst),
    {To, ToInfo} = lookup_state(StateInfo, To),
    #state_info{reduce_only = RO, state_repr = Repr, comment = C} = ToInfo,
    if
        RO -> 
            %% Reduce actions do not use the state, so we just pass
            %% the old (now bogus) on:
            FromS = io_lib:fwrite("~w=_S", [From]),
            ToS = "_S";
        true ->
            FromS = io_lib:fwrite("~w", [From]),
            ToS = io_lib:fwrite("~w", [To])
    end,
    St20 = fwrite(St10, <<"~w(~s, Cat, Ss, Stack, T, Ts, Tzr) ->\n">>, 
                  [F,FromS]),
    St30 = fwrite(St20, <<"~s">>, [C]),
    %% Short-circuit call to yeccpars2:
    St = fwrite(St30, <<" yeccpars2_~w(~s, Cat, Ss, Stack, T, Ts, Tzr)">>, 
                [Repr, ToS]),
    output_goto1(St, Tail, F, StateInfo, false);
output_goto1(St, [], _F, _StateInfo, _IsFirst) ->
    St.

output_goto_fini(F, NT, #yecc{includefile_version = {1,1}}=St0) ->
    %% Backward compatibility.
    St10 = delim(St0, false),
    St = fwrite(St10, <<"~w(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->\n">>,
                [F]),
    fwrite(St, 
           ?YECC_BUG(<<"{~ts, State, missing_in_goto_table}">>,
                     [quoted_atom(St0, NT)]),
           []);
output_goto_fini(_F, _NT, St) ->
    fwrite(St, <<".\n\n">>, []).

%% Find actions having user code.
find_user_code(ParseActions, St) ->
    [#user_code{state = State, 
                terminal = Terminal, 
                funname = inlined_function_name(St, State, Terminal),
                action = Action} || 
        {State, La_actions} <- ParseActions,
        {Action, Terminals, RuleNmbr, NmbrOfDaughters} 
            <- find_user_code2(La_actions),
        case tokens(RuleNmbr, St) of
            [{var, _, '__1'}] -> NmbrOfDaughters =/= 1;
            _ -> true
        end,
        Terminal <- Terminals].

find_user_code2([]) ->
    [];
find_user_code2([{_, #reduce{rule_nmbr = RuleNmbr,
                             nmbr_of_daughters = NmbrOfDaughters}
                  =Action}]) ->
    %% Same optimization as in output_state_actions.
    [{Action, ["Cat"], RuleNmbr, NmbrOfDaughters}];
find_user_code2([{La, #reduce{rule_nmbr = RuleNmbr,
                              nmbr_of_daughters = NmbrOfDaughters}
                  =Action} | T]) ->
    [{Action,La, RuleNmbr, NmbrOfDaughters} | find_user_code2(T)];
find_user_code2([_ | T]) ->
    find_user_code2(T).

output_actions(St0, StateJumps, StateInfo) ->
    %% Not all the clauses of the dispatcher function yeccpars2() can
    %% be reached. Only when shifting, that is, calling yeccpars1(),
    %% will yeccpars2() be called.
    Y2CL = [NewState || {_State,{Actions,J}} <- StateJumps,
                        {_LA, #shift{state = NewState}} <- 
                            (Actions
                             ++ [A || {_Tag,_To,Part} <- [J], A <- Part])],
    Y2CS = ordsets:from_list([0 | Y2CL]),
    Y2S = ordsets:from_list([S || {S,_} <- StateJumps]),
    NY2CS = ordsets:subtract(Y2S, Y2CS),
    Sel = [{S,true} || S <- ordsets:to_list(Y2CS)] ++
          [{S,false} || S <- ordsets:to_list(NY2CS)],

    SelS = [{State,Called} || 
               {{State,_JActions}, {State,Called}} <- 
                   lists:zip(StateJumps, lists:keysort(1, Sel))],
    St05 =
        fwrite(St0, <<"-dialyzer({nowarn_function, yeccpars2/7}).\n">>, []),
    St10 = foldl(fun({State, Called}, St_0) ->
                         {State, #state_info{state_repr = IState}} = 
                             lookup_state(StateInfo, State),
                         output_state_selection(St_0, State, IState, Called)
            end, St05, SelS),
    St20 = fwrite(St10, <<"yeccpars2(Other, _, _, _, _, _, _) ->\n">>, []),
    St = fwrite(St20,
                ?YECC_BUG(<<"{missing_state_in_action_table, Other}">>, []),
                []),
    foldl(fun({State, JActions}, St_0) ->
                  {State, #state_info{state_repr = IState}} = 
                      lookup_state(StateInfo, State),
                  output_state_actions(St_0, State, IState, 
                                       JActions, StateInfo)
          end, St, StateJumps).

output_state_selection(St0, State, IState, Called) ->
    Comment = [<<"%% ">> || false <- [Called]],
    St = fwrite(St0, <<"~syeccpars2(~w=S, Cat, Ss, Stack, T, Ts, Tzr) ->\n">>,
                [Comment, State]),
    fwrite(St, 
           <<"~s yeccpars2_~w(S, Cat, Ss, Stack, T, Ts, Tzr);\n">>, 
           [Comment, IState]).

output_state_actions(St, State, State, {Actions,jump_none}, SI) ->
    St1 = output_state_actions_begin(St, State, Actions),
    output_state_actions1(St1, State, Actions, true, normal, SI);
output_state_actions(St0, State, State, {Actions, Jump}, SI) ->
    {Tag, To, Common} = Jump,
    CS = case Tag of
             jump_some -> list_to_atom(lists:concat([cont_, To]));
             jump_all -> To
         end,
    St = output_state_actions1(St0, State, Actions, true, {to, CS}, SI),
    if 
        To =:= State ->
            St1 = output_state_actions_begin(St, State, Actions),
            output_state_actions1(St1, CS, Common, true, normal, SI);
        true ->
            St
    end;
output_state_actions(St, State, JState, _XActions, _SI) ->
    fwrite(St, <<"%% yeccpars2_~w: see yeccpars2_~w\n\n">>, [State, JState]).

output_state_actions_begin(St, State, Actions) ->
    case [yes || {_, #reduce{}} <- Actions] of
        [] ->
            fwrite(St, <<"-dialyzer({nowarn_function, yeccpars2_~w/7}).\n">>,
                   [State]); % Only when yeccerror(T) is output.
        _ -> St
    end.

output_state_actions1(St, State, [], IsFirst, normal, _SI) ->
    output_state_actions_fini(State, IsFirst, St);
output_state_actions1(St0, State, [], IsFirst, {to, ToS}, _SI) ->
    St = delim(St0, IsFirst),
    fwrite(St, 
           <<"yeccpars2_~w(S, Cat, Ss, Stack, T, Ts, Tzr) ->\n"
            " yeccpars2_~w(S, Cat, Ss, Stack, T, Ts, Tzr).\n\n">>,
           [State, ToS]);
output_state_actions1(St0, State, [{_, #reduce{}=Action}], 
                      IsFirst, _End, SI) ->
    St = output_reduce(St0, State, "Cat", Action, IsFirst, SI),
    fwrite(St, <<".\n\n">>, []);
output_state_actions1(St0, State, [{Lookahead,Action} | Tail],
                      IsFirst, End, SI) ->
    {_, St} = 
        foldl(fun(Terminal, {IsFst,St_0}) ->
                      {false,
                       output_action(St_0, State, Terminal, Action, IsFst,SI)}
              end, {IsFirst,St0}, Lookahead),
    output_state_actions1(St, State, Tail, false, End, SI).

output_action(St, State, Terminal, #reduce{}=Action, IsFirst, SI) ->
    output_reduce(St, State, Terminal, Action, IsFirst, SI);
output_action(St0, State, Terminal, #shift{state = NewState}, IsFirst, _SI) ->
    St10 = delim(St0, IsFirst),
    St = fwrite(St10, <<"yeccpars2_~w(S, ~ts, Ss, Stack, T, Ts, Tzr) ->\n">>,
                [State, quoted_atom(St10, Terminal)]),
    output_call_to_includefile(NewState, St);
output_action(St0, State, Terminal, accept, IsFirst, _SI) ->
    St10 = delim(St0, IsFirst),
    St = fwrite(St10, 
                <<"yeccpars2_~w(_S, ~ts, _Ss, Stack, _T, _Ts, _Tzr) ->\n">>,
                [State, quoted_atom(St10, Terminal)]),
    fwrite(St, <<" {ok, hd(Stack)}">>, []);
output_action(St, _State, _Terminal, nonassoc, _IsFirst, _SI) ->
    St.

output_call_to_includefile(NewState, #yecc{includefile_version = {1,1}}=St) ->
    %% Backward compatibility.
    fwrite(St, <<" yeccpars1(Ts, Tzr, ~w, [S | Ss], [T | Stack])">>, 
           [NewState]);
output_call_to_includefile(NewState, St) ->
    fwrite(St, <<" yeccpars1(S, ~w, Ss, Stack, T, Ts, Tzr)">>, 
           [NewState]).

output_state_actions_fini(State, IsFirst, St0) ->
    %% Backward compatible.
    St10 = delim(St0, IsFirst),
    St = fwrite(St10, <<"yeccpars2_~w(_, _, _, _, T, _, _) ->\n">>, [State]),
    fwrite(St, <<" yeccerror(T).\n\n">>, []).

output_reduce(St0, State, Terminal,
              #reduce{rule_nmbr = RuleNmbr, 
                      head = Head, 
                      nmbr_of_daughters = NmbrOfDaughters},
              IsFirst, StateInfo) ->
    St10 = delim(St0, IsFirst),
    QuotedTerminal = if 
                         is_atom(Terminal) -> quoted_atom(St10, Terminal);
                         true -> Terminal
                     end,
    St20 = fwrite(St10,
                  <<"yeccpars2_~w(_S, ~ts, Ss, Stack, T, Ts, Tzr) ->\n">>,
                  [State, QuotedTerminal]),
    St30 = 
        if
            NmbrOfDaughters < 2 ->
                Ns = "Ss",
                St20;
            true ->
                Ns = "Nss",
                Tmp = lists:join(",",
                                  lists:duplicate(NmbrOfDaughters - 1, "_")),
                fwrite(St20, <<" [~s|Nss] = Ss,\n">>, [Tmp])
        end,
    St40 = case tokens(RuleNmbr, St30) of
               [{var, _, '__1'}] when NmbrOfDaughters =:= 1 ->
                   NewStack = "Stack",
                   St30;
               _ ->
                   NewStack = "NewStack",
                   fwrite(St30, <<" NewStack = ~w(Stack),\n">>, 
                          [inlined_function_name(St30, State, Terminal)])
               end,
    if 
        NmbrOfDaughters =:= 0 ->
            NextState = goto(State, Head, St40),
            {NextState, I} = lookup_state(StateInfo, NextState),
            #state_info{reduce_only = RO, state_repr = Repr, comment = C} = I,
            %% Reduce actions do not use the state, so we just pass
            %% the old (now bogus) on:
            if
                RO -> NextS = "_S";
                true -> NextS = io_lib:fwrite("~w", [NextState])
            end,
            St = fwrite(St40, <<"~s">>, [C]),
            %% Short-circuit call to yeccpars2:
            fwrite(St,
                   <<" yeccpars2_~w(~s, ~ts, [~w | Ss], ~s, T, Ts, Tzr)">>,
                   [Repr, NextS, QuotedTerminal, State, NewStack]);
        true ->
            fwrite(St40, 
                   <<" ~w(hd(~s), ~ts, ~s, ~s, T, Ts, Tzr)">>,
                   [function_name(St40, yeccgoto, Head), Ns,
                    QuotedTerminal, Ns, NewStack])
    end.

delim(St, true) ->
    St;
delim(St, false) ->
    fwrite(St, <<";\n">>, []).

quoted_atom(#yecc{encoding = latin1}, Atom) when is_atom(Atom) ->
    io_lib:write_atom_as_latin1(Atom);
quoted_atom(_St, Atomic) ->
    io_lib:write(Atomic).
    
output_inlined(St, UserCodeActions, Infile) ->
    foldl(fun(#user_code{funname = InlinedFunctionName, 
                         action = Action}, St_0) ->
                  output_inlined(St_0, InlinedFunctionName, 
                                 Action, Infile)
          end, St, UserCodeActions).

%% Each action with user code is placed in a separate inlined function.
%% The purpose is to be able to pinpoint errors and warnings correctly.
output_inlined(St0, FunctionName, Reduce, Infile) ->
    #reduce{rule_nmbr = RuleNmbr, nmbr_of_daughters = N_daughters} = Reduce,
    #rule{tokens = Tokens, is_well_formed = WF} = get_rule(RuleNmbr, St0),
    Line0 = first_line(Tokens),
    NLines = last_line(Tokens) - Line0,

    St5 = if 
              WF ->
                  St0;
              not WF -> 
                  %% The compiler will generate an error message for
                  %% the inlined function (unless the reason that yecc
                  %% failed to parse the action was some macro). The
                  %% line number of the message will be correct since
                  %% we are keeping track of the current line of the
                  %% output file...
                  #yecc{outfile = Outfile, line = CurLine} = St0,
                  output_file_directive(St0, Outfile, CurLine)
          end,

    CodeStartLine = lists:max([0, Line0 - 4]),
    St10 = fwrite(St5, <<"-compile({inline,~w/1}).\n">>, [FunctionName]),
    St20 = output_file_directive(St10, Infile, CodeStartLine),
    St30 = fwrite(St20, <<"~w(__Stack0) ->\n">>, [FunctionName]),
    %% Currently the (old) inliner emits less code if matching the
    %% stack inside the body rather than in the head...
    St40 = case N_daughters of
               0 -> 
                   Stack = "__Stack0",
                   St30;
               _ -> 
                   Stack = "__Stack",
                   A = concat(flatmap(fun(I) -> [",__",I] end, 
                                      lists:seq(N_daughters, 1, -1))),
                   fwrite(St30, <<" ~s = __Stack0,\n">>, 
                          [append(["[", tl(A), " | __Stack]"])])
           end,
    St = St40#yecc{line = St40#yecc.line + NLines},
    fwrite(St, <<" [begin\n  ~ts\n  end | ~s].\n\n">>,
           [pp_tokens(Tokens, Line0, St#yecc.encoding), Stack]).

inlined_function_name(St, State, Terminal) ->
    End = case Terminal of
              "Cat" -> [];
              _ -> [quoted_atom(St, Terminal)]
          end,
    list_to_atom(concat([yeccpars2_, State, '_'] ++ End)).

-compile({nowarn_unused_function,function_name/3}).
function_name(St, Name, Suf) ->
    list_to_atom(concat([Name, '_'] ++ [quoted_atom(St, Suf)])).

rule(RulePointer, St) ->
    #rule{n = N, anno = Anno, symbols = Symbols} =
        dict:fetch(RulePointer, St#yecc.rule_pointer2rule),
    {Symbols, Anno, N}.

get_rule(RuleNmbr, St) ->
    dict:fetch(RuleNmbr, St#yecc.rule_pointer2rule).

tokens(RuleNmbr, St) ->
    Rule = dict:fetch(RuleNmbr, St#yecc.rule_pointer2rule),
    Rule#rule.tokens.

goto(From, Symbol, St) ->
    case ets:lookup(St#yecc.goto_tab, {From, Symbol}) of
        [{_, To}] ->
            To;
        [] ->
            erlang:error({error_in_goto_table, From, Symbol})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliaries:

-ifdef(SYMBOLS_AS_CODES).

%%% Bit mask operations.

-compile({inline,[set_empty/0]}).
set_empty() ->
    0.

set_add(I, BM) ->
    (1 bsl I) bor BM.

-compile({inline,[set_member/2]}).
set_member(I, BM) ->
    ((1 bsl I) band BM) =/= 0.

%% Assumes I is a member...
-compile({inline,[set_delete/2]}).
set_delete(I, BM) ->
    (1 bsl I) bxor BM.

-compile({inline,[set_union/2]}).
set_union(BM1, BM2) ->
    BM1 bor BM2.

-compile({inline,[set_is_subset/2]}).
set_is_subset(BM1, BM2) ->
    (BM1 band BM2) =:= BM1.

empty_member(BM) ->
    set_member(0, BM).

empty_delete(BM) ->
    set_delete(0, BM).

code_symbols(Ss, SymbolTable) ->
    map(fun(S) -> ets:lookup_element(SymbolTable, S, 2) end, Ss).

decode_symbol(C, InvSymbolTable) ->
    ets:lookup_element(InvSymbolTable, C, 1).

code_terminal(T, SymbolTab) ->
    set_add(ets:lookup_element(SymbolTab, T, 2), 0).

decode_terminals(BM, InvSymbolTab) ->
    case get(BM) of
        undefined -> 
            Symbols = decode_terminals(BM, 0, InvSymbolTab),
            put(BM, Symbols),
            Symbols;
        Symbols -> 
            Symbols
    end.

decode_terminals(0, _I, _InvSymbolTab) ->
    [];
decode_terminals(BM, I, InvSymbolTab) ->
    case set_member(I, BM) of
        true ->
            [ets:lookup_element(InvSymbolTab, I, 1)
             | decode_terminals(set_delete(I, BM), I+1, InvSymbolTab)];
        false ->
            decode_terminals(BM, I+1, InvSymbolTab)
    end.

set_add_terminal({_Symbol, TerminalNum}, BM) ->
    set_add(TerminalNum, BM).

-compile({inline,[is_terminal/2]}).
is_terminal(_Tab, SymbolCode) ->
    SymbolCode >= 0.

left_corner_symbol_table(St) ->
    St#yecc.inv_symbol_tab.

-else.

set_empty() ->
    [].

set_add(Symbol, L) ->
    ordsets:union([Symbol], L).

set_union(Es1, Es2) ->
    ordsets:union(Es1, Es2).

set_is_subset(Es1, Es2) ->
    ordsets:is_subset(Es1, Es2).

code_symbols(Ss, _SymbolTab) ->
    Ss.

decode_symbol(S, _InvSymbolTab) ->
    S.

code_terminal(T, _SymbolTab) ->
    [T].

decode_terminals(Ts, _InvSymbolTab) ->
    Ts.

empty_member(['$empty' | _]) ->
    true;
empty_member(_) ->
    false.

empty_delete(['$empty' | Terminals]) ->
    Terminals.

set_add_terminal({Symbol, _TerminalNum}, L) ->
    set_add(Symbol, L).

is_terminal(Tab, SymbolName) ->
   ets:lookup_element(Tab, SymbolName, 2) >= 0.

left_corner_symbol_table(St) ->
    St#yecc.symbol_tab.

-endif. % SYMBOLS_AS_CODES

intersect(L1, L2) ->
    ordsets:to_list(ordsets:intersection(ordsets:from_list(L1),
                                         ordsets:from_list(L2))).

format_symbols([Sym | Syms]) ->
    concat([format_symbol(Sym) | format_symbols1(Syms)]).

format_symbols1([]) ->
    [];
format_symbols1([H | T]) ->
    [" ", format_symbol(H) | format_symbols1(T)].

include(St, File, Outport) ->
    case file:open(File, [read]) of
        {error, Reason} ->
            throw(add_error(File, none, {file_error, Reason}, St));
        {ok, Inport} ->
            _ = epp:set_encoding(Inport),
            Line = io:get_line(Inport, ''),
            try include1(Line, Inport, Outport, File, 1, St) - 1
            after ok = file:close(Inport)
            end
    end.

include1(eof, _, _, _File, L, _St) ->
    L;
include1({error, _}=_Error, _Inport, _Outport, File, L, St) ->
    throw(add_error(File, erl_anno:new(L), cannot_parse, St));
include1(Line, Inport, Outport, File, L, St) ->
    Incr = case member($\n, Line) of
               true -> 1;
               false -> 0
           end,
    io:put_chars(Outport, Line),
    include1(io:get_line(Inport, ''), Inport, Outport, File, L + Incr, St).

includefile_version([]) ->
    {1,4};
includefile_version(Includefile) ->
    case epp:open(Includefile, []) of
        {ok, Epp} ->
            try
                parse_file(Epp)
            after
                epp:close(Epp)
            end;
        {error, _Error} ->
            {1,1}
    end.

parse_file(Epp) ->
    case epp:parse_erl_form(Epp) of
        {ok, {function,_Anno,yeccpars1,7,_Clauses}} ->
            {1,4};
        {eof,_Line} ->
            {1,1};
        _Form ->
            parse_file(Epp)
    end.

%% Keeps the line breaks of the original code.
pp_tokens(Tokens, Line0, Enc) ->
    concat(pp_tokens1(Tokens, Line0, Enc, [])).
    
pp_tokens1([], _Line0, _Enc, _T0) ->
    [];
pp_tokens1([T | Ts], Line0, Enc, T0) ->
    Line = location(anno(T)),
    [pp_sep(Line, Line0, T0), pp_symbol(T, Enc)|pp_tokens1(Ts, Line, Enc, T)].

pp_symbol({var,_,Var}, _Enc) -> Var;
pp_symbol({string,_,String}, latin1) ->
    io_lib:write_string_as_latin1(String);
pp_symbol({string,_,String}, _Enc) -> io_lib:write_string(String);
pp_symbol({_,_,Symbol}, latin1) -> io_lib:fwrite(<<"~p">>, [Symbol]);
pp_symbol({_,_,Symbol}, _Enc) -> io_lib:fwrite(<<"~tp">>, [Symbol]);
pp_symbol({Symbol, _}, _Enc) -> Symbol.

pp_sep(Line, Line0, T0) when Line > Line0 -> 
    ["\n   " | pp_sep(Line - 1, Line0, T0)];
pp_sep(_Line, _Line0, {'.',_}) -> 
    "";
pp_sep(_Line, _Line0, _T0) -> 
    " ".

set_encoding(#yecc{encoding = none}, Port) ->
    ok = io:setopts(Port, [{encoding, epp:default_encoding()}]);
set_encoding(#yecc{encoding = E}, Port) ->
    ok = io:setopts(Port, [{encoding, E}]).

output_encoding_comment(#yecc{encoding = none}=St) ->
    St;
output_encoding_comment(#yecc{encoding = Encoding}=St) ->
    fwrite(St, <<"%% ~s\n">>, [epp:encoding_to_string(Encoding)]).

output_file_directive(St, Filename, Line) when St#yecc.file_attrs ->
    fwrite(St, <<"-file(~ts, ~w).\n">>,
           [format_filename(Filename, St), Line]);
output_file_directive(St, _Filename, _Line) ->
    St.

first_line(Tokens) ->
    location(anno(hd(Tokens))).

last_line(Tokens) ->
    location(anno(lists:last(Tokens))).

location(none) -> none;
location(Anno) ->
    erl_anno:line(Anno).

anno(Token) ->
    element(2, Token).

%% Keep track of the current line in the generated file.
fwrite(#yecc{outport = Outport, line = Line}=St, Format, Args) ->
    NLines = count_nl(Format),
    io:fwrite(Outport, Format, Args),
    St#yecc{line = Line + NLines}.

%% Assumes \n is used, and never ~n.
count_nl(<<$\n,Rest/binary>>) ->
    1 + count_nl(Rest);
count_nl(<<_,Rest/binary>>) ->
    count_nl(Rest);
count_nl(<<>>) ->
    0.

nl(#yecc{outport = Outport, line = Line}=St) ->
    io:nl(Outport),
    St#yecc{line = Line + 1}.

format_filename(Filename0, St) ->
    Filename = filename:flatten(Filename0),
    case lists:keyfind(encoding, 1, io:getopts(St#yecc.outport)) of
        {encoding, unicode} -> io_lib:write_string(Filename);
        _ ->                   io_lib:write_string_as_latin1(Filename)
    end.

format_assoc(left) ->
    "Left";
format_assoc(right) ->
    "Right";
format_assoc(unary) ->
    "Unary";
format_assoc(nonassoc) ->
    "Nonassoc".

format_symbol(Symbol) ->
    String = concat([Symbol]),
    case erl_scan:string(String) of
        {ok, [{atom, _, _}], _} ->
            io_lib:fwrite(<<"~tw">>, [Symbol]);
        {ok, [{Word, _}], _} when Word =/= ':', Word =/= '->' ->
            case erl_scan:reserved_word(Word) of
                true ->
                    String;
                false ->
                    io_lib:fwrite(<<"~tw">>, [Symbol])
            end;
        {ok, [{var, _, _}], _} ->
            String;
        _ -> 
            io_lib:fwrite(<<"~tw">>, [Symbol])
    end.

inverse(L) ->
    sort([{A,B} || {B,A} <- L]).

family(L) ->
    sofs:to_external(sofs:relation_to_family(sofs:relation(L))).

seq1(To) when To < 1 ->
    [];
seq1(To) ->
    lists:seq(1, To).

count(From, L) ->
    lists:zip(L, lists:seq(From, length(L)-1+From)).

family_with_domain(L, DL) ->
    sofs:to_external(sofs_family_with_domain(sofs:relation(L), sofs:set(DL))).

sofs_family_with_domain(R0, D) ->
    R = sofs:restriction(R0, D),
    F = sofs:relation_to_family(R),
    FD = sofs:constant_function(D, sofs:from_term([])),
    sofs:family_union(F, FD).
