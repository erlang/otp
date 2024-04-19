%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2024. All Rights Reserved.
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
-module(edlin_expand).
-moduledoc """
Shell expansion and formatting of expansion suggestions.

This module provides an expand_fun for the erlang shell
[`expand/1,2`](`expand/1`). It is possible to override this expand_fun
[`io:setopts/1,2`](`io:setopts/1`).
""".
-moduledoc(#{since => "OTP 26.0"}).
%% a default expand function for edlin, expanding modules, functions
%% filepaths, variable binding, record names, function parameter values,
%% record fields and map keys and record field values.
-include_lib("kernel/include/eep48.hrl").
-export([expand/1, expand/2, expand/3, format_matches/2, number_matches/1, get_exports/1,
         shell_default_or_bif/1, bif/1, over_word/1]).
-export([is_type/3, match_arguments1/3]).
-record(shell_state,{
                     bindings = [],
                     records = [],
                     functions = []
                    }).

-doc(#{equiv => expand/2}).
-doc(#{since => <<"OTP 26.0">>}).
-spec expand(Bef0) -> {Res, Completion, Matches} when
      Bef0 :: string(), %% a line of erlang expressions in reverse
      Res :: 'yes' | 'no',
      Completion :: string(),
      Matches :: [Element] | [Section],
      Element :: {string(), [ElementOption]},
      ElementOption :: {ending, string()},
      Section :: #{title:=string(), elems:=Matches, options:=SectionOption},
      SectionOption :: {highlight_all} %% highlight the whole title
                     | {highlight, string()}  %% highlight this part of the title
                     | {highlight_param, integer()} %% highlight this parameter
                     | {hide, title}          %% hide the title
                     | {hide, result}         %% hide the results
                     | {separator, string()}. %% specify another separator between title and result
expand(Bef0) ->
    expand(Bef0, [{legacy_output, true}]).

-doc """
The standard expansion function is able to expand strings to valid erlang terms.
This includes module names:

```text
1> erla
modules
erlang:
```

function names:

```text
1> is_ato
functions
is_atom(
2> erlang:is_ato
functions
is_atom(
```

function types:

```text
1> erlang:is_atom(
typespecs
erlang:is_atom(Term)
any()
```

and automatically add , or closing parenthesis when no other valid expansion is
possible. The expand function also completes: shell bindings, record names,
record fields and map keys.

As seen below, function headers are grouped together if they've got the same
expansion suggestion, in this case all had the same suggestions, that is '\}'.
There is also limited support for filtering out function typespecs that that
does not match the types on the terms on the prompt. Only 4 suggestions are
shown below but there exists plenty more typespecs for `erlang:system_info`.

```text
1> erlang:system_info({allocator, my_allocator
typespecs
erlang:system_info(wordsize | {wordsize, ...} | {wordsize, ...})
erlang:system_info({allocator, ...})
erlang:system_info({allocator_sizes, ...})
erlang:system_info({cpu_topology, ...})
}
```

The return type of `expand` function specifies either a list of `Element` tuples
or a list of `Section` maps. The section concept was introduced to enable more
formatting options for the expansion results. For example, the shell expansion
has support to highlight text and hide suggestions. There are also a
`{highlight, Text}` that highlights all occurances of `Text` in the title, and a
`highlight_all` for simplicity which highlights the whole title, as can be seen
above for `functions` and `typespecs`.

By setting the `{hide, result}` or `{hide, title}` options you may hide
suggestions. Sometimes the title isn't useful and just produces text noise, in
the example above the `t:any/0` result is part of a section with title `Types`.
Hiding results is currently not in use, but the idea is that a section can be
selected in the expand area and all the other section entries should be
collapsed.

Its possible to set a custom separator between the title and the results. This
can be done with `{separator, Separator}`. By default its set to be `\n`, some
results display a `type_name() :: `followed by all types that define
`type_name()`.

The `{ending, Text}` ElementOption just appends Text to the `Element`.
""".
-doc(#{since => <<"OTP 26.0">>}).
-spec expand(Bef0, Opts) -> {Res, Completion, Matches} when
      Bef0 :: string(), %% a line of erlang expressions in reverse
      Opts :: [Option],
      Option :: {legacy_output, boolean()},
      Res :: 'yes' | 'no',
      Completion :: string(),
      Matches :: [Element] | [Section],
      Element :: {string(), [ElementOption]},
      ElementOption :: {ending, string()},
      Section :: #{title:=string(), elems:=Matches, options:=SectionOption},
      SectionOption :: {highlight_all} %% highlight the whole title
                     | {highlight, string()}  %% highlight this part of the title
                     | {highlight_param, integer()} %% highlight this parameter
                     | {hide, title}          %% hide the title
                     | {hide, result}         %% hide the results
                     | {separator, string()}. %% specify another separator between title and result
expand(Bef0, Opts) ->
    ShellState = try
                     shell:get_state()
                 catch
                     _:_ ->
                         %% Running on a shell that does not support get_state()
                         #shell_state{bindings=[],records=[],functions=[]}
                 end,
    expand(Bef0, Opts, ShellState).

%% Only used for testing
-doc false.
expand(Bef0, Opts, #shell_state{bindings = Bs, records = RT, functions = FT}) ->
    LegacyOutput = proplists:get_value(legacy_output, Opts, false),
    {_Bef1, Word} = over_word(Bef0),
    {Res, Expansion, Matches} = case edlin_context:get_context(Bef0) of

                 {string} -> expand_string(Bef0);

                 {binding} -> expand_binding(Word, Bs);

                 {term} -> expand_module_function(Bef0, FT);
                 {term, _, {_, Unfinished}} -> expand_module_function(lists:reverse(Unfinished), FT);
                 {error, _Column} ->
                    {no, [], []};
                 {function} -> expand_module_function(Bef0, FT);
                 {function, _Mod} -> expand_module_function(Bef0, FT);
                 {fun_} -> expand_module_function(Bef0, FT);

                 {fun_, Mod} -> expand_function_name(Mod, Word, "/", FT);

                 %% Complete with arity in a 'fun mod:fun/' expression
                 {fun_, Mod, Fun} ->
                    Arities = [integer_to_list(A) || A <- get_arities(Mod, Fun)],
                    match(Word, Arities, "");
                 {new_fun, _ArgsString} -> {no, [], []};
                 %% Suggest type of function parameter
                 %% Complete an unfinished list, tuple or map using type of function parameter
                 {function, Mod, Fun, Args, Unfinished, Nesting} ->
                    Mod2 = case Mod of
                        "user_defined" -> "shell_default";
                        _ -> Mod
                    end,
                    FunExpansion = expand_function_type(Mod2, Fun, Args, Unfinished, Nesting, FT),
                    case Word of
                        [] -> FunExpansion;
                        _ ->
                            ModuleOrBifs = expand_helper(FT, module, Word, ":"),
                            Functions = case Args =/= [] andalso lists:last(Args) of
                                {atom, MaybeMod} -> expand_function_name(MaybeMod, Word, "", FT);
                                _ -> {no, [], []}
                            end,
                            fold_results([FunExpansion] ++ ModuleOrBifs ++ [Functions])
                    end;

                 %% Complete an unfinished key or suggest valid keys of a map binding
                 {map, Binding, Keys} -> expand_map(Word, Bs, Binding, Keys);

                 {map_or_record} ->
                     {[$#|Bef2], _} = over_word(Bef0),
                     {_, Var} = over_word(Bef2),
                     case Bs of
                         [] -> expand_record(Word, RT);
                         _ ->
                             case proplists:get_value(list_to_atom(Var), Bs) of
                                 undefined ->
                                     expand_record(Word, RT);
                                 Map when is_map(Map) -> {yes, "{", []};
                                 RecordTuple when is_tuple(RecordTuple), tuple_size(RecordTuple) > 0 ->
                                     Atom = erlang:element(1, RecordTuple),
                                     case (is_atom(Atom) andalso lists:keysearch(Atom, 1, RT)) of
                                        {value, {Atom, _}} -> match(Word, [Atom], "{");
                                        _ -> {no, [], []}
                                     end;
                                 _ -> {no, [], []}
                             end
                     end;

                 {record} -> expand_record(Word, RT);

                 {record, Record, Fields, FieldToComplete, Args, Unfinished, Nestings} ->
                     RecordExpansion = expand_record_fields(FieldToComplete, Unfinished, Record, Fields, RT, Args, Nestings, FT),
                     case Word of
                         [] -> RecordExpansion;
                         _ ->
                             ModuleOrBifs = expand_helper(FT, module,Word,":"),
                             fold_results([RecordExpansion] ++ ModuleOrBifs)
                     end;
                 _ -> {no, [], []}

             end,
    Matches1 = case {Res,number_matches(Matches)} of
        {yes, 1} -> [];
        _ -> Matches
    end,
    case LegacyOutput of
        true -> {Res, Expansion, to_legacy_format(Matches1)};
        false -> {Res, Expansion, Matches1}
    end.
expand_map(_, [], _, _) ->
    {no, [], []};
expand_map(Word, Bs, Binding, Keys) ->
    case proplists:get_value(list_to_atom(Binding), Bs) of
        Map when is_map(Map) ->
            K1 = sets:from_list(maps:keys(Map)),
            K2 = sets:subtract(K1, sets:from_list([list_to_atom(K) || K <- Keys])),
            match(Word, sets:to_list(K2), "=>");
        _ -> {no, [], []}
    end.

-doc false.
over_word(Bef) ->
    {Bef1,_,_} = over_white(Bef, [], 0),
    {Bef2, Word, _} = edlin:over_word(Bef1, [], 0),
    {Bef2, Word}.


expand_binding(Prefix, Bindings) ->
    Alts = [strip_quotes(K) || {K,_} <- Bindings],
    case match(Prefix, Alts, "") of
        {_Res,_Expansion,[]}=M -> M;
        {Res, Expansion, Matches} -> {Res,Expansion,[#{title=>"bindings", elems=>Matches, options=>[highlight_all]}]}
    end.

expand_record(Prefix, RT) ->
    Alts = [Name || {Name, _} <- RT],
    case match(Prefix, Alts, "{") of
        {_Res,_Expansion,[]}=M -> M;
        {Res, Expansion, Matches} -> {Res,Expansion,[#{title=>"records", elems=>Matches, options=>[highlight_all]}]}
    end.

expand_record_fields(FieldToComplete, Word, Record, Fields, RT, _Args, Nestings, FT) ->
    Record2 = list_to_atom(Record),
    FieldSet2 = sets:from_list([list_to_atom(F) || F <- Fields]),
    FieldToComplete2 = list_to_atom(FieldToComplete),
    Word1 = case Word of
                {_, Word2} -> Word2;
                [] -> []
            end,
    case [RecordSpec || {Record3, RecordSpec} <- RT, Record2 =:= Record3] of
        [RecordType|_] ->
            case sets:is_element(FieldToComplete2, FieldSet2) of
                true ->
                    expand_record_field_content(FieldToComplete2, RecordType, Word1, Nestings, FT);
                false ->
                    expand_record_field_name(Record2, FieldSet2, RecordType, Word1)
            end;
        _ ->
            {no, [], []}
    end.

expand_record_field_name(Record, Fields, RecordType, Word) ->
    RecordFieldsList = extract_record_fields(Record, RecordType),
    RecordFieldsSet = sets:from_list(RecordFieldsList),
    RecordFields = sets:subtract(RecordFieldsSet, Fields),
    Alts = sets:to_list(RecordFields),
    case match(Word, Alts, "=") of
        {_Res,_Expansion,[]}=M -> M;
        {Res, Expansion, Matches} -> {Res,Expansion,[#{title=>"fields", elems=>Matches, options=>[highlight_all]}]}
    end.

expand_record_field_content(Field,
                            {attribute, _, record,
                             {_Record, FieldTypes}}, Word, Nestings, FT) ->
    FieldTypesFiltered = [Type1 || {typed_record_field, {record_field, _, {_,_, F}}, Type1} <- FieldTypes, F == Field] ++
        [Type1 || {typed_record_field, {record_field, _, {_,_, F}, _}, Type1} <- FieldTypes, F == Field],
    case FieldTypesFiltered of
        [] -> {no, [], []};
        [Type] ->
            T = edlin_type_suggestion:type_tree(erlang, Type, Nestings, FT),
            Types = edlin_type_suggestion:get_types([], T, Nestings),
            case Nestings of
                [] ->
                    Atoms = edlin_type_suggestion:get_atoms([], T, Nestings),
                    case {Word, match(Word, Atoms, ", ")} of
                        {[],{_Res,_Expansion,_}} -> {_Res, _Expansion, [#{title=>"types", elems=>Types, options=>[{hide, title}]}]};
                        {_,{_Res,_Expansion,[]}=M} -> M;
                        {_,{Res,Expansion,Matches}} -> {Res, Expansion, [#{title=>"matches", elems=>Matches, options=>[highlight_all]}]}
                    end;
                _ ->
                    expand_nesting_content(T, [], Nestings, #{title=>"types", elems=>Types, options=>[{hide, title}]})
            end
    end.

%% Check that the actual type on previous arguments
%% matches with the expected types
%% Since we are not doing any evaluations at this point we
%% don't know if a parenthesis, keyword, var, call or fun returns
%% a value with the wrong type.
match_arguments({function, {{parameters, Ps}, _}, Cs}, As) ->
    match_arguments1(Ps, Cs, As);
match_arguments({{parameters, Ps}, _}, As) ->
    match_arguments1(Ps, [], As).
-doc false.
match_arguments1(_,_,[]) -> true;
%% Just assume that it will evaluate to the correct type.
match_arguments1([_|Ps], Cs, [{parenthesis, _}|As]) ->
    match_arguments1(Ps, Cs, As);
match_arguments1([_|Ps], Cs, [{operation, _}|As]) ->
    match_arguments1(Ps, Cs, As);
match_arguments1([_|Ps], Cs, [{keyword, _}|As]) ->
    match_arguments1(Ps, Cs, As);
match_arguments1([_|Ps], Cs, [{var, _}|As]) ->
    match_arguments1(Ps, Cs, As);
match_arguments1([_|Ps], Cs, [{call, _}|As]) ->
    match_arguments1(Ps, Cs, As);
match_arguments1([_|Ps], Cs, [{fun_, _}|As]) ->
    match_arguments1(Ps, Cs, As);
match_arguments1([P|Ps], Cs, [{atom, [$'|_]=String}|As]) ->
    case edlin_context:odd_quotes($', lists:reverse(String)) of
        true -> false; % we know that the atom is unfinished, and thus cannot match any valid atom
        _ -> case is_type(P, Cs, String) of
                 true -> match_arguments1(Ps, Cs, As);
                 false -> false
             end
    end;
match_arguments1([P|Ps], Cs, [{_, String}|As]) ->
    case is_type(P, Cs, String) of
        true -> match_arguments1(Ps, Cs, As);
        false -> false
    end.

-doc false.
is_type(Type, Cs, String) ->
    {ok, A, _} = erl_scan:string(String++"."),
    Types = [T || T <- edlin_type_suggestion:get_types(Cs, Type, [], [no_print]) ],
    try
        {ok, Term} = erl_parse:parse_term(A),
        case Term of
            Atom when is_atom(Atom) ->
                Atoms = edlin_type_suggestion:get_atoms(Cs, Type, []),
                lists:member(to_list(Atom), Atoms) orelse 
                    lists:member(atom_to_list(Atom), Atoms) orelse
                        find_type(Types, [atom, node, module, 'fun']);
            Tuple when is_tuple(Tuple) -> find_type(Types, [tuple]);
            Map when is_map(Map) -> find_type(Types, [map]);
            Binary when is_binary(Binary) -> find_type(Types, [binary]);
            Float when is_float(Float) -> find_type(Types, [float]);
            Integer when is_integer(Integer) -> check_integer_type(Types, Integer);
            List when is_list(List), length(List) > 0 ->
                find_type(Types, [list, string, nonempty_list,maybe_improper_list, nonempty_improper_list]);
            List when is_list(List) -> find_type(Types, [list, string, maybe_improper_list])
        end
    catch
        _:_ ->
            %% Types not possible to deduce with erl_parse
            % If string contains variables, erl_parse:parse_term will fail, but we
            % consider them valid sooo.. lets replace them with the atom var
            B = [(fun({var, Anno, _}) -> {atom, Anno, var}; (Token) -> Token end)(X) || X <- A],
            try
                {ok, Term2} = erl_parse:parse_term(B),
                case Term2 of
                    Tuple2 when is_tuple(Tuple2) -> find_type(Types, [tuple]);
                    Map2 when is_map(Map2) -> find_type(Types, [map]);
                    Binary2 when is_binary(Binary2) -> find_type(Types, [binary]);
                    List2 when is_list(List2), length(List2) > 0 ->
                        find_type(Types, [list, string, nonempty_list,maybe_improper_list, nonempty_improper_list]);
                    List2 when is_list(List2) -> find_type(Types, [list, string, maybe_improper_list])
                end
            catch
                _:_ ->
                    case A of
                        [{'#',_},{var,_,'Port'},{'<',_},{float,_,_},{'>',_},{dot,_}] -> find_type(Types, [port]);
                        [{'#',_},{var,_,'Ref'},{'<',_},{float,_,_},{'.',_},{float,_,_},{'>',_},{dot,_}] -> find_type(Types, [reference]);
                        [{'fun',_},{'(',_} | _] -> find_type(Types, [parameters, function, 'fun']);
                        [{'#',_},{var,_,'Fun'},{'<',_},{atom,_,erl_eval},{'.',_},{float,_,_},{'>',_}] -> find_type(Types, [parameters, function, 'fun']);
                        [{'<', _}, {float, _, _}, {'.', _}, {integer, _, _}, {'>', _}, {dot, _}] -> find_type(Types, [pid]);
                        [{'#', _}, {atom, _, RecordName},{'{', _}| _] -> find_type(Types, [{record, RecordName}]);
                        _ -> false
                    end
            end
    end.

find_type([],_) -> false;
find_type([any|_], _) -> true; % If we find any then every type is valid
find_type([{type, any, []}|_], _) -> true;
find_type([{{parameters, _},_}|Types], ValidTypes) ->
    case lists:member(parameters, ValidTypes) of
        true -> true;
        false -> find_type(Types, ValidTypes)
    end;
find_type([{record, _}=Type|Types], ValidTypes) ->
    case lists:member(Type, ValidTypes) of
        true -> true;
        false -> find_type(Types, ValidTypes)
    end;
find_type([{Type, _}|Types], ValidTypes) ->
    case lists:member(Type, ValidTypes) of
        true -> true;
        false -> find_type(Types, ValidTypes)
    end;
find_type([{type, Type, _}|Types], ValidTypes) ->
    case lists:member(Type, ValidTypes) of
        true -> true;
        false -> find_type(Types, ValidTypes)
    end;
find_type([{type, Type, _, any}|Types], ValidTypes) ->
    case lists:member(Type, ValidTypes) of
        true -> true;
        false -> find_type(Types, ValidTypes)
    end;
find_type([_|Types], ValidTypes) -> find_type(Types, ValidTypes).

in_range(_, []) -> false;
in_range(Integer, [{type, range, [{integer, Start}, {integer, End}]}|_]) when Start =< Integer, Integer =< End -> true;
in_range(Integer, [_|Types]) -> in_range(Integer, Types).

check_integer_type(Types, Integer) when Integer == 0 -> find_type(Types, [integer, non_neg_integer, arity]) orelse in_range(Integer, Types);
check_integer_type(Types, Integer) when Integer < 0 -> find_type(Types, [integer, neg_integer]) orelse in_range(Integer, Types);
check_integer_type(Types, Integer) when Integer > 0 -> find_type(Types, [integer, non_neg_integer, pos_integer]) orelse in_range(Integer, Types).

add_to_last_nesting(Term, Nesting) ->
    Last = lists:last(Nesting),
    List = lists:droplast(Nesting),
    case Last of
        {tuple, Args, U} ->
            List ++ [{tuple, Args ++ [Term], U}];
        {list, Args, U} ->
            List ++ [{list, Args ++ [Term], U}];
        {map, F, Fs, Args, U} ->
            List ++ [{map, F, Fs, Args ++ [Term], U}]
    end.
expand_function_parameter_type(Mod, MFA, FunType, Args, Unfinished, Nestings, FT) ->
    TypeTree = edlin_type_suggestion:type_tree(Mod, FunType, Nestings, FT),

    {Parameters, Constraints1} = case TypeTree of
                                     {function, {{parameters, Parameters1},_}, Constraints} ->
                                         {Parameters1, Constraints};
                                     {{parameters, Parameters1},_}=_F ->
                                         {Parameters1, []}
                                 end,
    case match_arguments(TypeTree, Args) of
        false -> {no, [], []};
        true when Parameters == [] ->
            if Nestings == [] ->
                    {yes, ")", [#{title=>MFA, elems=>[{")",[]}], options=>[]}]};
               true ->
                    {no, [], []}
            end;
        true ->
            Parameter = lists:nth(length(Args)+1, Parameters),
            {T, _Name} = case Parameter of
                            Atom when is_atom(Atom) -> {Atom, atom_to_list(Atom)};
                            {var, Name1}=T1 -> {T1, atom_to_list(Name1)};
                            {ann_type, {var, Name1}, T1} -> {T1, atom_to_list(Name1)};
                            T1 -> {T1, edlin_type_suggestion:print_type(T1, [], [{first_only, true}])}
                        end,
            Ts = edlin_type_suggestion:get_types(Constraints1, T, Nestings),
            Types = case Ts of
                        [] -> [];
                        _  ->
                            SectionTypes = [S || #{}=S <- Ts],
                            Types1 = case [E || {_, _}=E<-Ts] of
                                [] -> SectionTypes;
                                Elems -> 
                                    case SectionTypes of
                                        [] -> Elems;
                                        ST -> [#{title=>"simple types", elems=>Elems, options=>[{hide, title}]}|ST]
                                    end
                            end,
                            
                            [#{title=>"types", elems=>(Types1), options=>[{hide, title}]}]
                    end,
            case Nestings of
                [] -> %% Expand function type
                    case Unfinished of
                        [] ->
                            case T of
                                Atom1 when is_atom(Atom1) ->
                                    CC = case length(Args)+1 < length(Parameters) of
                                             true -> ", ";
                                             false ->")"
                                         end,
                                    {Res, Expansion, Matches} = match([], [Atom1], CC),
                                    case Matches of
                                        [] -> {no, [], []};
                                        _ -> {Res, Expansion, [#{title=>MFA, elems=>[], options=>[{highlight_param, length(Args)+1}]}]}
                                    end;
                                _ when Types == [] ->
                                    {no, [], []};
                                _ ->
                                    {no, [], [#{title=>MFA, elems=>Types, options=>[{highlight_param, length(Args)+1}]}]}
                            end;
                        {_, Word} when is_atom(T) ->
                            CC = case length(Args)+1 < length(Parameters) of
                                     true -> ", ";
                                     false ->")"
                                 end,
                            {Res, Expansion, Matches} = match(Word, [T], CC),
                            case Matches of
                                [] -> {no, [], []};
                                _ -> {Res, Expansion, [#{title=>MFA, elems=>[], options=>[{highlight_param, length(Args)+1}]}]}
                            end;
                        {_, Word} ->
                            {Res, Expansion, Matches} = begin
                                                            CC = case length(Args)+1 < length(Parameters) of
                                                                     true -> ", ";
                                                                     false ->")"
                                                                 end,
                                                            Atoms1 = edlin_type_suggestion:get_atoms(Constraints1, T, Nestings),
                                                            match(Word, Atoms1, CC)
                                                        end,
                            Match1 = case Matches of
                                         [] -> [];
                                         _  -> Atoms = [#{title=>"atoms", elems=>Matches, options=>[{hide, title}]}],
                                               [#{title=>MFA, elems=>Atoms, options=>[{highlight_param, length(Args)+1}]}]
                                     end,
                            {Res, Expansion,Match1}
                    end;
                _ ->  %% Expand last nesting types
                    expand_nesting_content(T, Constraints1, Nestings, #{title=>MFA, elems=>Types, options=>[{highlight_param, length(Args)+1}]})
            end
    end.
expand_nesting_content(T, Constraints, Nestings, Section) ->
    {NestingType, UnfinishedNestingArg, _NestingArgs} = case lists:last(Nestings) of
                                              {tuple, NestingArgs1, Unfinished1} -> {tuple, Unfinished1, NestingArgs1};
                                              {list, NestingArgs1, Unfinished1} -> {list, Unfinished1, NestingArgs1};
                                              {map, _, _, NestingArgs1, Unfinished1} -> {map, Unfinished1, NestingArgs1}
                                          end,
    %% in the case of
    %% erlang:system_info({allocator, ) 
    %% we have a tuple nesting with an atom
    %% this should give us "allocator" in the nestingsargs, and empty unfinished part
    %% but we also know that we have a nesting, if we expect something other than a tuple, we shouldnt print that function
    %% lets call it NestingType
    %% now when that is fixed, how do we filter {allocator_sizes, ...} and others
    Types = [Ts || Ts <- edlin_type_suggestion:get_types(Constraints, T, lists:droplast(Nestings), [no_print]) ],
    case UnfinishedNestingArg of
        [] ->
            case find_type(Types, [NestingType]) of
                true -> 
                    %% if we know had a tuple, {allocator_sizes, } will be allowed
                    %% probably get_arity will return none
                    Nestings2 = add_to_last_nesting({var, "Var"}, Nestings),
                    NestingArities = edlin_type_suggestion:get_arity(Constraints, T, Nestings2),
                    
                    fold_results([begin
                        case NestingArity of
                            none -> {no, [], []};
                            _ -> {no, [], [Section]}
                        end
                    end || NestingArity <- NestingArities]);
                false -> {no, [], []}
            end;
        {_, Word} ->
            Atoms1 = edlin_type_suggestion:get_atoms(Constraints, T, Nestings),
            {Res1, Expansion1, Matches1} = match(Word, Atoms1, ""),
            {Res, Expansion, Matches} = case Matches1 of
                                            [] ->
                                                Nestings2 = add_to_last_nesting(UnfinishedNestingArg, Nestings),
                                                NestingArities = edlin_type_suggestion:get_arity(Constraints, T, Nestings2),
                                                fold_results([begin
                                                                  case NestingArity of
                                                                      none -> {no, [], []};
                                                                      _ -> 
                                                                        {no, [], []}
                                                                  end
                                                              end || NestingArity <- NestingArities]);
                                            [{Word2,_}] ->
                                                Nestings2 = add_to_last_nesting({atom, Word2}, Nestings),
                                                NestingArities = edlin_type_suggestion:get_arity(Constraints, T, Nestings2),
                                                fold_results([begin
                                                                  case NestingArity of
                                                                      none -> {no, [], []};
                                                                      _ -> 
                                                                        {Res1, Expansion1, Matches1}
                                                                  end
                                                              end || NestingArity <- NestingArities]);
                                            _ -> {Res1, Expansion1, Matches1}
                                        end,
            Match1 = case Matches of
                         [] -> [];
                         _  -> Atoms = [#{title=>"atoms", elems=>Matches, options=>[{hide, title}]}],
                               [Section#{elems:=Atoms}]
                     end,
            {Res, Expansion, Match1}
    end.

extract_record_fields(Record, {attribute,_,record,{Record, Fields}})->
    [X || X <- [extract_record_field(F) || F <- Fields], X /= []];
extract_record_fields(_, _)-> error.
extract_record_field({typed_record_field, {_, _,{atom, _, Field}},_})->
    Field;
extract_record_field({typed_record_field, {_, _,{atom, _, Field}, _},_})->
    Field;
extract_record_field({record_field, _,{atom, _, Field},_})->
    Field;
extract_record_field({record_field, _,{atom, _, Field}})->
    Field;
extract_record_field(_) -> [].

fold_results([]) -> {no, [], []};
fold_results([R|Results]) ->
    lists:foldl(fun fold_completion_result/2, R, Results).

fold_completion_result({yes, Cmp1, Matches1}, {yes, Cmp2, Matches2}) ->
    {_, Cmp} = longest_common_head([Cmp1,Cmp2]),
    case Cmp of
        [] -> {no, [], ordsets:union([Matches1,Matches2])};
        _ -> {yes, Cmp, ordsets:union([Matches1,Matches2])}
    end;
fold_completion_result({yes, Cmp, Matches}, {no, [], []}) ->
    {yes, Cmp, Matches};
fold_completion_result({no, [], []},{yes, Cmp, Matches}) ->
    {yes, Cmp, Matches};
fold_completion_result({_, _, Matches1}, {_, [], Matches2}) ->
    {no, [], ordsets:union([Matches1,Matches2])};
fold_completion_result(A, B) ->
    fold_completion_result(B,A).

expand_function_type(ModStr, FunStr, Args, Unfinished, Nestings, FT) ->
    maybe
        {ok, Mod} ?= to_atom(ModStr),
        {ok, Fun} ?= to_atom(FunStr),
        MinArity = if Unfinished =:= [], length(Args) =:= 0 -> 0;
                      true -> length(Args)+1
                   end,
        [_|_] = Arities ?= [A || A <- get_arities(ModStr, FunStr, FT), A >= MinArity],
        {Res, Expansion, Matches} =
            fold_results(
              [begin
                   FunTypes = edlin_type_suggestion:get_function_type(Mod, Fun, Arity, FT),
                   case FunTypes of
                       [] -> MFA = print_function_head(ModStr, FunStr, Arity),
                             case Unfinished of
                                 [] -> {no, [], [#{title=>MFA, elems=>[], options=>[]}]};
                                 _ -> {no, [], []}
                             end;
                       _ ->
                           fold_results(
                             [begin
                                  MFA = print_function_head(ModStr, FunStr, FunType, FT),
                                  expand_function_parameter_type(Mod, MFA, FunType, Args, Unfinished, Nestings, FT)
                              end || FunType <- FunTypes])
                   end
               end || Arity <- Arities]),
        case Matches of
            [] -> {Res, Expansion, Matches};
            _ -> {Res, Expansion, [#{title=>"typespecs", elems=>Matches, options=>[highlight_all]}]}
        end
    else
        _ -> {no, [], []}
    end.

%% Behaves like zsh
%% filters all files starting with . unless Word starts with .
%% outputs / on end of folders
expand_filepath(PathPrefix, Word) ->
    Path = case PathPrefix of
               [$/|_] -> PathPrefix;
               _ ->
                   {ok, Cwd} = file:get_cwd(),
                   Cwd ++ "/" ++ PathPrefix
           end,
    ShowHidden = case Word of
                     "." ++ _ -> true;
                     _ -> false
                 end,
    Entries = case file:list_dir(Path) of
                  {ok, E} -> lists:map(
                               fun(X)->
                                       case filelib:is_dir(Path ++ "/" ++ X) of
                                           true -> X ++ "/";
                                           false -> X
                                       end
                               end, [".."|E]);
                  _ -> []
              end,
    EntriesFiltered = [File || File <- Entries,
                               case File of
                                   [$.|_] -> ShowHidden;
                                   _ -> true
                               end],
    case match(Word, EntriesFiltered, []) of
        {yes, Cmp, [Match]} ->
            case filelib:is_dir(Path ++ "/" ++ Word ++ Cmp) of
                true -> {yes, Cmp, [Match]};
                false -> {yes, Cmp ++ "\"", [Match]}
            end;
        X -> X
    end.

shell(Fun) ->
    {ok, [{atom, _, Fun1}], _} = erl_scan:string(Fun),
    case shell:local_func(Fun1) of
        true -> "shell";
        false -> "user_defined"
    end.

-doc false.
shell_default_or_bif(Fun) ->
    {ok, [{atom, _, Fun1}], _} = erl_scan:string(Fun),
    case lists:member(Fun1, [E || {E,_}<-get_exports(shell_default)]) of
        true -> "shell_default";
        _ -> bif(Fun)
    end.
-doc false.
bif(Fun) ->
    {ok, [{atom, _, Fun1}], _} = erl_scan:string(Fun),
    case lists:member(Fun1, [E || {E,A}<-get_exports(erlang), erl_internal:bif(E,A)]) of
        true -> "erlang";
        _ -> shell(Fun)
    end.

expand_string(Bef0) ->
    case over_filepath(Bef0, []) of
        {_, Filepath} ->
            {Path, File} = split_at_last_slash(Filepath),
            expand_filepath(Path, File);
        _ -> {no, [], []}
    end.
%% Extract a whole filepath
%% Stops as soon as we hit a double quote (")
%% and returns everything it found before stopping.
%% assumes the string is not a filepath if it contains unescaped spaces
over_filepath([],_) -> none;
over_filepath([$", $\\|Bef1], Filepath) -> over_filepath(Bef1, [$" | Filepath]);
over_filepath([$"|Bef1], Filepath) -> {Bef1, Filepath};
over_filepath([$\ ,$\\|Bef1], Filepath) -> over_filepath(Bef1, [$\ |Filepath]);
over_filepath([$\ |_], _) -> none;
over_filepath([C|Bef1], Filepath) ->
    over_filepath(Bef1, [C|Filepath]).
split_at_last_slash(Filepath) ->
    {File, Path} = lists:splitwith(fun(X)->X/=$/ end, lists:reverse(Filepath)),
    {lists:reverse(Path), lists:reverse(File)}.

print_function_head(ModStr, FunStr, Arity) ->
    lists:flatten(ModStr ++ ":" ++ FunStr ++ "/" ++ integer_to_list(Arity)).
print_function_head(ModStr, FunStr, FunType, FT) ->
    lists:flatten(print_function_head_from_type(ModStr, FunStr, FunType, FT)).

print_function_head1(Mod, Fun, Par, _Ret) ->
    Mod++":"++Fun++"("++lists:join(", ",
                                   [case P of
                                        Atom when is_atom(Atom) -> atom_to_list(Atom);
                                        {var, V} -> atom_to_list(V);
                                        {ann_type, {var, V}, _T} -> atom_to_list(V);
                                        T -> edlin_type_suggestion:print_type(T, [], [{first_only, true}])
                                    end || {_N,P} <- lists:enumerate(Par)])++")".
print_function_head_from_type(Mod, Fun, FunType, FT) ->
    case edlin_type_suggestion:type_tree(list_to_atom(Mod), FunType, [], FT) of
        {function, {{parameters, Parameters},{return, Return}}, _} ->
            print_function_head1(Mod, Fun, Parameters, Return);
        {{parameters, Parameters},{return, Return}} ->
            print_function_head1(Mod, Fun, Parameters, Return)
    end.

%% expand_module_function(CurrentBefore, FT) -> {yes, Expansion, Matches} | {no, [], Matches}
%%  Try to expand the word before as either a module name or a function
%%  name. We can handle white space around the seperating ':' but the
%%  function name must be on the same line. CurrentBefore is reversed
%%  and over_word/3 reverses the characters it finds. In certain cases
%%  possible expansions are printed.´´´
%%
%%  The function also handles expansion with "h(" and "ht("" for module and functions.
expand_module_function(Bef0, FT) ->
    {Bef1,Word,_} = edlin:over_word(Bef0, [], 0),
    case over_white(Bef1, [], 0) of
        {[$,|Bef2],_White,_Nwh} ->
            {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
            {Bef4,Mod,_Nm} = edlin:over_word(Bef3, [], 0),
            case expand_function(Bef4) of
                help ->
                    expand_function_name(Mod, Word, ", ", FT);
                help_type ->
                    expand_type_name(Mod, Word, ", ");
                _ ->
                    fold_results(expand_helper(FT, module, Word, ":"))
            end;
        {[$:|Bef2],_White,_Nwh} ->
            {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
            {_,Mod,_Nm} = edlin:over_word(Bef3, [], 0),
            expand_function_name(Mod, Word, "(", FT);
        {[CC, N_Esc|_], _White, _Nwh} when (CC =:= $] orelse CC =:= $) orelse CC =:= $> orelse CC =:= $}
                                            orelse CC =:= $" orelse CC =:= $'),
                                           N_Esc =/= $$, N_Esc =/= $- ->
            {no, [], []};
        {[], _, _} ->
            case Word of
                [] -> {no, [], []}; %fold_results([expand_shell_default(Word), expand_user_defined_functions(FT, Word)]);
                _ -> fold_results(expand_helper(FT, all, Word, ":"))
            end;
        {_,_,_} ->
            case Word of
                [] -> {no, [], []};
                _ ->
            TypeOfExpand = expand_function(Bef1),
            CompleteChar
                = case TypeOfExpand of
                      help -> ", ";
                      help_type -> ", ";
                      _ -> ":"
                  end,
            fold_results(expand_helper(FT, TypeOfExpand, Word, CompleteChar))
        end
    end.
expand_keyword(Word) ->
    Keywords = ["begin", "case", "of", "receive", "after", "maybe", "try", "catch", "throw", "if", "fun", "when", "end"],
    {Res, Expansion, Matches} = match(Word, Keywords, ""),
    case Matches of
        [] -> {no, [], []};
        [{Word, _}] -> {no, [], []}; %% exact match
        _ -> {Res,Expansion,[#{title=>"keywords", elems=>Matches, options=>[highlight_all]}]}
    end.
expand_helper(_, help, Word, CompleteChar) ->
    [expand_module_name(Word, CompleteChar)];
expand_helper(_, help_type, Word, CompleteChar) ->
    [expand_module_name(Word, CompleteChar)];
expand_helper(FT, all, Word, CompleteChar) ->
    [expand_module_name(Word, CompleteChar), expand_bifs(Word), expand_shell_default(Word),
     expand_user_defined_functions(FT, Word), expand_keyword(Word)];
expand_helper(FT, _, Word, CompleteChar) ->
    [expand_module_name(Word, CompleteChar), expand_bifs(Word),
     expand_user_defined_functions(FT, Word), expand_keyword(Word)].
expand_function("("++Str) ->
    case edlin:over_word(Str, [], 0) of
        {_,"h",_} ->
            help;
        {_,"ht",_} ->
            help_type;
        _ ->
            module
    end;
expand_function(_) ->
    module.

expand_bifs(Prefix) ->
    Alts = [EA || {E,A}=EA <- get_exports(erlang), erl_internal:bif(E,A)],
    CC = "(",
    case match(Prefix, Alts, CC) of
        {_Res,_Expansion,[]}=M -> M;
        {Res,Expansion, Matches} -> {Res,Expansion,[#{title=>"bifs", elems=>Matches, options=>[highlight_all]}]}
    end.

expand_shell_default(Prefix) ->
    Alts = get_exports(shell_default) ++ shell:local_func(),
    CC = "(",
    case match(Prefix, Alts, CC) of
        {_Res,_Expansion,[]}=M -> M;
        {Res,Expansion, Matches} -> {Res,Expansion,[#{title=>"commands",elems=>Matches, options=>[highlight_all]}]}
    end.

expand_user_defined_functions(FT, Prefix) ->
    Alts = [{Name, Arity}||{{function, {_, Name, Arity}}, _} <- FT],
    CC = "(",
    case match(Prefix, Alts, CC) of
        {_Res,_Expansion,[]}=M -> M;
        {Res,Expansion, Matches} -> {Res,Expansion,[#{title=>"user_defined", elems=>Matches, options=>[highlight_all]}]}
    end.

expand_module_name("",_) ->
    {no, [], []};
expand_module_name(Prefix,CC) ->
    Alts = [{list_to_atom(M),""} || {M,_,_} <- code:all_available()],
    case match(Prefix, Alts, CC) of
        {_Res,_Expansion,[]}=M -> M;
        {Res,Expansion, Matches} -> {Res,Expansion,[#{title=>"modules", elems=>Matches, options=>[highlight_all]}]}
    end.

get_arities("shell_default"=ModStr, FuncStr, FT) ->
    {ok, Func} = to_atom(FuncStr),
    case [A || {{function, {_, Fun, A}}, _} <- FT, Fun =:= Func] of
        [] -> get_arities(ModStr, FuncStr);
        Arities -> Arities
    end;
get_arities(ModStr, FuncStr, _) ->
    get_arities(ModStr, FuncStr).
get_arities(ModStr, FuncStr) ->
    case to_atom(ModStr) of
        {ok, Mod} ->
            Exports = get_exports(Mod),
            lists:sort(
              [A || {H, A} <- Exports, string:equal(FuncStr, flat_write(H))]);
        error ->
            []
    end.

-doc false.
get_exports(Mod) ->
    case erlang:module_loaded(Mod) of
        true ->
            Mod:module_info(exports);
        false ->
            case beam_lib:chunks(code:which(Mod), [exports]) of
                {ok, {Mod, [{exports,E}]}} ->
                    E;
                _ ->
                    []
            end
    end.

expand_function_name(ModStr, FuncPrefix, CompleteChar, FT) ->
    case to_atom(ModStr) of
        {ok, Mod} ->
            Extra = case Mod of
                        shell_default -> [{Name, Arity}||{{function, {_, Name, Arity}}, _} <- FT];
                        _ -> []
                    end,
            Exports = get_exports(Mod) ++ Extra,
            {Res, Expansion, Matches}=Result = match(FuncPrefix, Exports, CompleteChar),
            case Matches of
                [] -> Result;
                _ -> {Res, Expansion, [#{title=>"functions", elems=>Matches, options=>[highlight_all]}]}
            end;
        error ->
            {no, [], []}
    end.

get_module_types(Mod) ->
    case code:get_doc(Mod, #{sources => [debug_info]}) of
        {ok, #docs_v1{ docs = Docs } } ->
            [{T, A} || {{type, T, A},_Anno,_Sig,_Doc,_Meta} <- Docs];
        _ -> {no, [], []}
    end.

expand_type_name(ModStr, TypePrefix, CompleteChar) ->
    case to_atom(ModStr) of
        {ok, Mod} ->
            case get_module_types(Mod) of
                {no, [], []} ->
                    {no, [], []};
                Types ->
                    {Res, Expansion, Matches}=Result = match(TypePrefix, Types, CompleteChar),
                    case Matches of
                        [] -> Result;
                        _ -> {Res, Expansion, [#{title=>"types", elems=>Matches, options=>[highlight_all]}]}
                    end
            end;
        error ->
            {no, [], []}
    end.

to_atom(Str) ->
    case erl_scan:string(Str) of
        {ok, [{atom,_,A}], _} ->
            {ok, A};
        _ ->
            error
    end.

to_list(Atom) ->
    io_lib:write_atom(Atom).

strip_quotes(Atom) ->
    [C || C<-atom_to_list(Atom), C/=$'].

match_preprocess_alt({_,_}=Alt) -> Alt;
match_preprocess_alt(X) -> {X, ""}.

match(Prefix, Alts, Extra0) ->
    Alts2 = [match_preprocess_alt(A) || A <- Alts],
    Len = string:length(Prefix),
    Matches = lists:sort(
                [{S, A} || {H, A} <- Alts2,
                           lists:prefix(Prefix, S=flat_write(H))]),
    Matches2 = lists:usort(
                 case Extra0 of
                     [] -> [{S,[]} || {S,_} <- Matches];
                     _  -> [{S,[{ending, Extra0}]} || {S,_} <- Matches]
                 end),
    case longest_common_head([N || {N, _} <- Matches]) of
        {partial, []} ->
            {no, [], Matches2};
        {partial, Str} ->
            case string:slice(Str, Len) of
                [] ->
                    {yes, [], Matches2};
                Remain ->
                    {yes, Remain, Matches2}
            end;
        {complete, Str} ->
            Extra = case {Extra0,Matches} of
                        {"/",[{Str,N}]} when is_integer(N) -> "/"++integer_to_list(N);
                        {"(",[{Str,0}]} -> "()";
                        {_,_} -> Extra0
                    end,
            {yes, string:slice(Str, Len) ++ Extra, ordsets:from_list(Matches2)};
        no ->
            {no, [], []}
    end.

flat_write(T) when is_atom(T) ->
    lists:flatten(io_lib:fwrite("~tw",[T]));
flat_write(S) ->
    S.

special_sort1([C|A], B) when C == ${ ; C == $. ; C == $# ->
    special_sort1(A, B);
special_sort1(A, [C|B]) when C == ${ ; C == $. ; C == $# ->
    special_sort1(A,B);
special_sort1(A,B) ->
    string:lowercase(A) =< string:lowercase(B).
special_sort(#{title:=A}, #{title:=B}) ->
    special_sort1(A,B);
%% Sections and elemts should not be in the same list
special_sort(#{}, {}) ->
    error;
special_sort({}, #{}) ->
    error;
special_sort({A,_},{B,_}) ->
    special_sort1(A,B);
special_sort(A,B) ->
    special_sort1(A,B).

to_legacy_format([]) -> [];
to_legacy_format([#{title:=Title}|Rest]) when Title =:= "commands"; Title =:= "bifs" ->
    to_legacy_format(Rest);
to_legacy_format([#{title:=Title, elems:=Elems}|Rest])
  when Title =:= "modules"; Title =:= "functions"; Title =:= "bindings";
       Title =:= "user_defined", Title =:= "records"; Title =:= "fields";
       Title =:= "types"; Title =:= "atoms"; Title =:= "matches";
       Title =:= "keywords"; Title =:= "typespecs" ->
    Elems1 = to_legacy_format(Elems),
    Elems1 ++ to_legacy_format(Rest);
to_legacy_format([#{title:=Title, elems:=_Elems}|Rest]) ->
    [Title] ++ to_legacy_format(Rest);
to_legacy_format([{Val, _}|Rest]) ->
    [{Val, ""}] ++ to_legacy_format(Rest).

-doc false.
format_matches([], _LineWidth) -> [];
format_matches([#{}|_]=FF, LineWidth) ->
    %% Group function head that have the exact same Type suggestion
    Groups = maps:groups_from_list(
                fun(#{title:=Title, elems:=T, options:=Opts}) ->
                    Separator = proplists:get_value(separator, Opts, "\n"),
                    case lists:last(string:split(Title++Separator, "\n", all)) of
                        [] -> format_section_matches(T, LineWidth);
                        Chars -> %% we have chars that compete with the results on the first line
                            Len = length(Chars),
                            format_section_matches(T, LineWidth, Len)
                    end
                end,
               fun(F) ->
                       format_title(F, LineWidth)
               end, FF),
    S = lists:flatten(
      [lists:join("", F)++Matches ||
          {Matches, F}<-lists:sort(fun({_,A},{_,B}) -> A =< B end, maps:to_list(Groups))]),
    lists:flatten(string:trim(S, trailing)++"\n");
format_matches(Elems, LineWidth) ->
    S = format_section_matches1(Elems, LineWidth, 0),
    lists:flatten(string:trim(S, trailing)++"\n").
format_title(#{title:=MFA, options:=Options}, _LineWidth) ->
    case proplists:get_value(hide, Options) of
        title -> "";
        _ ->
            Separator = proplists:get_value(separator, Options, "\n"),
            HighlightAll = proplists:is_defined(highlight_all, Options),
            case HighlightAll of
                true -> "\033[;1;4m"++MFA++"\033[0m"++Separator;
                _ ->
                    HighlightParam = proplists:get_value(highlight_param, Options, false),
                    
                    MFA2 = case HighlightParam of
                        false -> MFA;
                        _ -> 
                            PreviousParams = HighlightParam -1,
                            TuplePattern = "(?:\\{[^\\}]+\\})",
                            AtomVarPattern = "(?:\\w+)",
                            TypePattern="(?:(?:"++AtomVarPattern++":)?(?:"++AtomVarPattern++"\\(\\))(?:\\s[><=]+\\s\\d+)?)",
                            SimplePatterns = "(?:"++TuplePattern++"|"++TypePattern++"|"++AtomVarPattern++")",
                            UnionPattern = "(?:"++SimplePatterns++"(?:\\s\\|\\s"++SimplePatterns++")*)",
                            FunPattern="(?:fun\\(\\(" ++ UnionPattern ++ "\\)\\s*->\\s*" ++ UnionPattern ++ "\\))",
                            ArgPattern3 = "(?:"++FunPattern++"|"++UnionPattern++")",
                            PrevArgs="(?:"++ArgPattern3++",\\s){"++integer_to_list(PreviousParams) ++ "}",
                            FunctionHeadStart="^([^\\(]+\\("++PrevArgs++")", %% \\1
                            
                            HighlightArg="("++ArgPattern3++")", %\\2
                            NextArgs="(?:,\\s"++ArgPattern3++")*",
                            FunctionHeadEnd="("++NextArgs++"\\)(?:.*))$", % \\3

                            re:replace(MFA, 
                        FunctionHeadStart ++ HighlightArg ++ FunctionHeadEnd,
                                "\\1\033[;1;4m\\2\033[0m\\3",
                            [global, {return, list}, unicode])
                    end,
                    Highlight = proplists:get_value(highlight, Options, false),
                    case Highlight of
                        false -> MFA2;
                        _ -> re:replace(MFA2, "(\\Q"++Highlight++"\\E)", "\033[;1;4m\\1\033[0m", [global, {return, list}, unicode])
                    end ++ Separator
            end
    end;
format_title(_Elems, _LineWidth) ->
    %% not a section, old interface
    %% output empty list
    "".

format_section_matches(LS, LineWidth) -> format_section_matches(LS, LineWidth, 0).
format_section_matches([], _, _) -> "\n";
format_section_matches([#{}|_]=FF, LineWidth, Acc) ->
    Groups = maps:groups_from_list(
        fun(#{title:=Title, elems:=T, options:=Opts}) ->
                Separator = proplists:get_value(separator, Opts, "\n"),
                case lists:last(string:split(Title++Separator, "\n", trailing)) of
                    [] -> format_section_matches(T, LineWidth);
                    Chars -> %% we have chars that compete with the results on the first line
                        Len = string:length(Chars),
                        format_section_matches(T, LineWidth, Len+Acc)
                end
        end,
        fun(F) ->
                format_title(F, LineWidth)
        end, FF),
    lists:flatten(
        [lists:join("", F)++Matches ||
        {Matches, F}<-lists:sort(fun({_,A},{_,B}) -> A =< B end, maps:to_list(Groups))]);
format_section_matches(Elems, LineWidth, Acc) ->
    format_section_matches1(Elems, LineWidth, Acc).

format_section_matches1([], _, _) -> [];
format_section_matches1(LS, LineWidth, Len) ->
    L0 = lists:sort(fun special_sort/2, ordsets:to_list(LS)),
    L = lists:uniq(L0),
    Opt = case Len == 0 of
        true -> [];
        false -> [{title, Len}]
    end,    
    S1 = format_col(Opt ++ L, field_width(Opt ++ L, LineWidth), Len, [], LineWidth, Opt),
    S2 = lists:map(
           fun(Line) ->
                case string:length(Line) of
                    Len1 when Len1 > LineWidth ->
                        string:sub_string(Line, 1, LineWidth-4) ++ "...\n";
                    _ -> Line
                end
           end,S1),
    lists:flatten(string:trim(S2, trailing)++"\n").

format_col(X, Width, Len, Acc, LL, Opt) when Width + Len > LL ->
    format_col(X, Width, 0, ["\n" | Acc], LL, Opt);
format_col([{title,TitleLen}|T], Width, Len, Acc0, LL, Opt) ->
    Acc = [io_lib:format("~-*ts", [Width-TitleLen, ""])|Acc0],
    format_col(T, Width, Len+Width, Acc, LL, Opt);
format_col([A|T], Width, Len, Acc0, LL, _Opt) ->
    {H0, R} = format_val(A),
    Hmax = LL - string:length(R),
    {H, _} =
        case string:length(H0) > Hmax of
            true -> {io_lib:format("~-*ts", [Hmax - 3, H0]) ++ "...", true};
            false -> {H0, false}
        end,
    Acc = [io_lib:format("~-*ts", [Width, H ++ R]) | Acc0],
    format_col(T, Width, Len+Width, Acc, LL, []);
format_col([], _, _, Acc, _LL, _Opt) ->
    lists:reverse(Acc).

format_val({H, L}) when is_list(L) ->
    {H, proplists:get_value(ending, L, "")};
format_val({H, I}) when is_integer(I) ->
    {H, "/"++integer_to_list(I)};
format_val({H, _}) ->
    {H, ""};
format_val(H) ->
    {H, ""}.

field_width(L, LL) -> field_width(L, 0, LL).
field_width([{title, Len}|T], W, LL) ->
    case Len of
        L when L > W -> field_width(T, L, LL);
        _ -> field_width(T, W, LL)
    end;
field_width([H|T], W, LL) ->
    {H1, Ending} = format_val(H),
    case string:length(H1++Ending) of
        L when L > W -> field_width(T, L, LL);
        _ -> field_width(T, W, LL)
    end;
field_width([], W, LL) when W < LL ->
    W + 4;
field_width([], _, LL) ->
    LL.

-doc false.
number_matches([#{ elems := Matches }|T]) ->
    number_matches(Matches) + number_matches(T);
number_matches([_|T]) ->
    1 + number_matches(T);
number_matches([]) ->
    0.

%% Strings are handled naively, but it should be OK here.
longest_common_head([]) ->
    no;
longest_common_head(LL) ->
    longest_common_head(LL, []).

longest_common_head([[]|_], L) ->
    {partial, lists:reverse(L)};
longest_common_head(LL, L) ->
    case same_head(LL) of
        true ->
            [[H|_]|_] = LL,
            LL1 = all_tails(LL),
            case all_nil(LL1) of
                false ->
                    longest_common_head(LL1, [H|L]);
                true ->
                    {complete, lists:reverse([H|L])}
            end;
        false ->
            {partial, lists:reverse(L)}
    end.

same_head([[H|_]|T1]) -> same_head(H, T1).
same_head(H, [[H|_]|T]) -> same_head(H, T);
same_head(_, [])        -> true;
same_head(_, _)         -> false.

all_tails(LL) -> all_tails(LL, []).

all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
all_tails([], L)         -> L.

all_nil([]) -> true;
all_nil([[] | Rest]) -> all_nil(Rest);
all_nil(_) -> false.

%% over_white(Chars, InitialStack, InitialCount) ->
%%    {RemainingChars,CharStack,Count}.

over_white([$\s|Cs], Stack, N) ->
    over_white(Cs, [$\s|Stack], N+1);
over_white([$\t|Cs], Stack, N) ->
    over_white(Cs, [$\t|Stack], N+1);
over_white(Cs, Stack, N) when is_list(Cs) ->
    {Cs,Stack,N}.
