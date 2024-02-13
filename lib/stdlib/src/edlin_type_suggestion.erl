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
-module(edlin_type_suggestion).
-moduledoc false.
-include_lib("kernel/include/eep48.hrl").
-export([type_tree/4, get_arity/3, get_atoms/3, get_types/3, get_types/4, get_function_type/4, print_type/3]).


%% type_tree/4 returns a unwrapped and trimmed type specification containing
%% all the valid 'types' that are valid in each function parameter of a function or
%% each record field of a record.
%%
%% Translates the spec AST to a structure that resembles the AST but trimmed of unneeded data.
%% User types and remote types are fetched and embedded in the structure depending on requested
%% level of unnestling.
%% Unions are flattened.
%% Visited is used to prevent infinite loops when looking up a recursive / cyclic type
%% FT is a map of type {{type, Type}, {attribute,_,type,{_,TypeAST,_}}})
type_tree(Mod, FunType, Nestings, FT) ->
    %% TODO when FT is updated we would be getting incorrect results because of cache
    %%  we should probably store a "dirty bit" in the table to make this aware that
    %% a new result should be calculated. Preferably only types that depend on the table
    %% would be invalidated, but it may turn advanced.
    %% TODO look this over to make sure we don't do unnecessary work,
    case get({type_traverser, Mod, FunType, Nestings}) of
        undefined -> Res = type_traverser_cache(Mod, FunType, #{}, length(Nestings)+1, FT),
                     put({type_traverser, Mod, FunType, Nestings}, Res),
                     Res;
        Res -> Res
    end.
type_traverser_cache(Mod, T, Visited, Level, FT) ->
    case get({Mod, T, Level}) of
        undefined ->
            Res  = type_traverser(Mod, T, Visited, Level, FT),
            put({Mod, T, Level}, Res),
            Res;
        Res -> Res
    end.

type_traverser(Mod, {type, _, bounded_fun, [Fun, Constraints]}, Visited, Level, FT) ->
    Cl = [type_traverser(Mod,X,Visited, Level, FT) || X <- Constraints],
    F = type_traverser(Mod, Fun, Visited, Level, FT),
    {function, F, Cl};
type_traverser(Mod, {type, _, 'fun', [Product, Return]}, Visited, Level, FT) ->
    P = type_traverser(Mod, Product, Visited, Level, FT),
    R = type_traverser(Mod, Return, Visited, Level, FT),
    {P, {return, R}};
type_traverser(Mod, {type, _, product, Childs}, Visited, Level, FT) ->
    Cl = [type_traverser(Mod, X, Visited, Level, FT) || X <- Childs],
    {parameters, Cl};
type_traverser(Mod, {type, _, constraint, [{atom, _, is_subtype}, [Type1, Type2]]}, Visited, Level, FT) ->
    {constraint, type_traverser(Mod, Type1, Visited, Level, FT), type_traverser(Mod, Type2, Visited, Level, FT)};
type_traverser(_, {var, _, Name}, _Visited, _Level, _FT) ->
    {var, Name};
type_traverser(_Mod,{type, _, map, any}, _Visited, _Level, _FT) ->
    {type, map, []};
type_traverser(Mod, {type, _, map, Params}, Visited, Level, FT) ->
    {map, [type_traverser(Mod, X, Visited, Level-1, FT) || X <- Params]};
type_traverser(Mod, {type, _, map_field_exact, [Type1, Type2]}, Visited, Level, FT) ->
    {map_field_exact, type_traverser(Mod,Type1, Visited, Level, FT), type_traverser(Mod,Type2, Visited, Level, FT)};
type_traverser(Mod, {type, _, map_field_assoc, [Type1, Type2]}, Visited, Level, FT) ->
    {map_field_assoc, type_traverser(Mod,Type1, Visited, Level, FT), type_traverser(Mod,Type2, Visited, Level, FT)};
type_traverser(_Mod, {atom, _, Atom}, _Visited, _Level, _FT) when is_atom(Atom) ->
    Atom;
type_traverser(Mod, {op, _, Op, Type}, Visited, Level, FT) ->
    {op, Op, type_traverser(Mod, Type, Visited, Level, FT)};
type_traverser(Mod, {op, _, Op, Type1, Type2}, Visited, Level, FT) ->
    {op, Op, type_traverser(Mod, Type1, Visited, Level, FT), type_traverser(Mod, Type2, Visited, Level, FT)};
type_traverser(_Mod, {integer, _, Int}, _Visited, _Level, _FT) ->
    {integer, Int};
type_traverser(Mod, {type, _, list, [ChildType]}, Visited, Level, FT) ->
    {list, type_traverser(Mod, ChildType, Visited, Level-1, FT)};
type_traverser(_Mod, {type, _, tuple, any}, _Visited, _Level, _FT) ->
    {type, tuple, []};
type_traverser(Mod, {type, _, tuple, ChildTypes}, Visited, Level, FT) ->
    {tuple, [type_traverser(Mod, X, Visited, Level-1, FT) || X <- ChildTypes]};
type_traverser(Mod, {type, _, union, ChildTypes}, Visited, Level, FT) ->
    Childs = [type_traverser(Mod, X, Visited, Level, FT) || X <- ChildTypes],
    ChildsFiltered = [X || X <- Childs, X/=undefined],
    {UnionChilds, NonUnionChilds} = lists:partition(
                                      fun(X) ->
                                              case X of
                                                  {union, _} -> true;
                                                  _ -> false
                                              end
                                      end, ChildsFiltered),
    ChildsFlattened = lists:flatten([T || {union, T} <- UnionChilds]) ++ NonUnionChilds,
    {union, ChildsFlattened};
type_traverser(Mod, {ann_type,_,[T1,T2]}, Visited, Level, FT) ->
    {ann_type, type_traverser(Mod, T1, Visited, Level, FT), type_traverser(Mod, T2, Visited, Level, FT)};
type_traverser(Mod, {user_type,_,Name,Params}=T, Visited, Level, FT) when 0 >= Level  ->
    %% when we have level 0, do not traverse the type further, just print it
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            {type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, 0, FT) || P <- Params]};
        true -> {type, Mod, Name, []}
    end;
type_traverser(_, {remote_type,_,[{_,_,Mod},{_,_,Name}, Params]}=T, Visited, Level, FT) when 0 >= Level ->
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            {type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, 0, FT) || P <- Params]};
        true -> {type, Mod, Name, []}
    end;
type_traverser(Mod, {user_type,_,Name,Params}=T, Visited, 1=Level, FT) ->
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            case get({strip_anno(T), 1}) of
                undefined ->
                    Res = case lookup_type(Mod, Name, length(Params), FT) of
                              hidden -> {type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params]};
                              Type -> {user_type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params], type_traverser(Mod, Type, Visited#{ strip_anno(T) => true }, Level, FT)}
                          end,
                    put({strip_anno(T), 1}, Res),
                    Res;
                Res -> Res
            end;
        true -> {type, Mod, Name, []}
    end;
type_traverser(_, {remote_type,_,[{_,_,Mod},{_,_,Name}, Params]}=T, Visited, 1=Level, FT) ->
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            case get({strip_anno(T), 1}) of
                undefined ->
                    Res = case lookup_type(Mod, Name, length(Params), FT) of
                              hidden -> {type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params]};
                              Type -> {user_type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params], type_traverser(Mod, Type, Visited#{ strip_anno(T) => true }, Level, FT)}
                          end,
                    put({strip_anno(T), 1}, Res),
                    Res;
                Res -> Res
            end;
        true -> {type, Mod, Name, []}
    end;
type_traverser(Mod, {user_type,_,Name,Params}=T, Visited, Level, FT) ->
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            case get({strip_anno(T), Level}) of
                undefined ->
                    Res = case lookup_type(Mod, Name, length(Params), FT) of
                              hidden -> {type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params]};
                              Type -> type_traverser(Mod, Type, Visited#{ strip_anno(T) => true }, Level, FT)
                          end,
                    put({strip_anno(T), Level}, Res),
                    Res;
                Res -> Res
            end;
        true -> {type, Mod, Name, []}
    end;
type_traverser(_, {remote_type, _, [{_,_,Mod},{_,_,Name}, Params]}=T, Visited, Level, FT) ->
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            case get({strip_anno(T), Level}) of
                undefined ->
                    Res = case lookup_type(Mod, Name, length(Params), FT) of
                              hidden -> {type, Mod, Name, [type_traverser(Mod, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params]};
                              Type -> type_traverser(Mod, Type, Visited#{ strip_anno(T) => true }, Level, FT)
                          end,
                    put({strip_anno(T), Level}, Res),
                    Res;
                Res -> Res
            end;
        true -> {type, Mod, Name, []}
    end;
type_traverser(_, {type, _, record, [{atom, _, Record}]}, _Visited, _Level, _FT) ->
    {record, Record};
type_traverser(_, {type, _, Name, any}, _, _, _) ->
    {type, Name, []};
type_traverser(_, {type, _, term}, _, _, _) ->
    {type, any, []};
type_traverser(_, {type, _, Name}, _, _, _) ->
    {type, Name, []};
type_traverser(_, {type, _, term, _}, _, _, _) ->
    {type, any, []};
type_traverser(_, {type, _, Name, Params}=T, Visited, Level, FT) ->
    case maps:is_key(strip_anno(T), Visited) of
        false ->
            case get({strip_anno(T), 1}) of
                undefined ->
                    Res = case lookup_type(erlang, Name, length(Params), FT) of
                              hidden -> {type, Name, [type_traverser(erlang, P, Visited#{ strip_anno(T) => true }, Level, FT) || P <- Params]};
                              Type -> type_traverser(erlang, Type, Visited#{ strip_anno(T) => true}, Level, FT)
                          end,
                    put({strip_anno(T), 1}, Res),
                    Res;
                Res -> Res
            end;
        true -> {type, Name, []}
    end.

strip_anno({A, _, B}) -> {A, B};
strip_anno({A, _, B, C}) -> {A, B, C}.

simplified_type(erlang, binary, 0) -> {type, undefined, binary, []};
simplified_type(erlang, char, 0) -> {type, undefined, char, []};
simplified_type(erlang, iolist, 0) -> {type, undefined, iolist, []};
simplified_type(erlang, string, 0) -> {type, undefined, string, []};
simplified_type(unicode, chardata, 0) -> {type, erlang, string, []};
simplified_type(file, filename_all, 0) -> {type, erlang, string, []};
simplified_type(file, filename, 0) -> {type, erlang, string, []};
simplified_type(file, name_all, 0) -> {type, erlang, string, []};
simplified_type(file, name, 0) -> {type, erlang, string, []};
simplified_type(_Module, _TypeName, _Arity) -> none.

lookup_type(Mod, Type, Arity, FT) ->
    case simplified_type(Mod, Type, Arity) of
        none ->
            case code:get_doc(Mod, #{sources => [debug_info]}) of
                {ok, #docs_v1{ docs = Docs } } ->
                    FnFunctions =
                        lists:filter(fun({{type, T, A},_Anno,_Sig,_Doc,_Meta}) ->
                                             T =:= Type andalso A =:= Arity;
                                        (_) ->
                                             false
                                     end, Docs),
                    case FnFunctions of
                        [] -> 
                            case [TypeAST || {{type, Type2}, {attribute,_,type,{_,TypeAST,_}}} <- FT, Type2 =:= Type] of
                                [] -> hidden; %% can be an opaque type or missing type
                                [SingleTypeAST] -> SingleTypeAST
                            end;
                        [{_,_,_,_,#{signature := [{attribute,_,type,{_,TypeAST,_}}]}}] -> TypeAST
                    end;
                _ ->
                    case [TypeAST || {{type, Type2}, {attribute,_,type,{_,TypeAST,_}}} <- FT, Type2 =:= Type] of
                        [] -> hidden; %% can be an opaque type or missing type
                        [SingleTypeAST] -> SingleTypeAST
                    end
            end;
        T -> T
    end.
get_function_type(Mod, Fun, Arity, FT) ->
    case code:get_doc(Mod, #{sources => [debug_info]}) of
        {ok, #docs_v1{ docs = Docs } } ->
            R = lists:flatten([FunTypes ||
                              {{function, F, A},_Anno,_Sig,_Doc, #{ signature := [{attribute,_,spec,{_,FunTypes}}]}} <- Docs,
                              F =:= Fun, A =:= Arity]),
            case {Mod, R} of
                {shell_default, []} ->
                    lists:flatten([FunTypes ||
                                      {{function_type, {shell_default, F, A}},{attribute,_,spec,{_,FunTypes}}} <- FT,
                                      F =:= Fun, A =:= Arity]);
                _ -> R                                      
            end;
        _ when Mod =:= shell_default ->
            lists:flatten([FunTypes || {{function_type, {shell_default, F, A}},{attribute,_,spec,{_,FunTypes}}} <- FT,
                                        F =:= Fun, A =:= Arity]);
        _ -> []
    end.
get_arity(Constraints, Type, Nestings) ->
    case get_arity1(Type, Constraints, Nestings) of
        List when is_list(List) -> List;
        Val -> [Val]
    end.
get_arity1({var, _Var}=C, Constraints, Nestings) ->
    case get_constraint(C, Constraints) of
        {constraint, _, T} -> get_arity1(T, Constraints, Nestings);
        _ -> none
    end;
get_arity1({list, _T}, _Constraints, [{'list', _, _}]) ->
    99; %% Can be higher, but probably do not need completion for that
get_arity1({list, T}, Constraints, [{'list', _, _}|Nestings]) ->
    get_arity1(T, Constraints, Nestings);
get_arity1({tuple, LT}, Constraints, [{'tuple', Args, _}]) when length(LT) >= length(Args) ->
    case edlin_expand:match_arguments1(LT, Constraints, Args) of
        true -> length(LT);
        false ->
            none
    end;
get_arity1({tuple, LT}, Constraints, [{'tuple', Args, _}|Nestings]) when length(LT) >= length(Args)+1 ->
    case edlin_expand:match_arguments1(LT, Constraints, Args) of
        true -> get_arity1(lists:nth(length(Args)+1, LT), Constraints, Nestings);
        false -> none
    end;
get_arity1({map, Types}, _Constraints, [{'map', _Keys, [], _, _}]) ->
    length(Types);
get_arity1({map, Types}, _Constraints, [{'map', _Keys, _Key, _, _}]) ->
    length(Types);
get_arity1({map, Types}, Constraints, [{'map', Keys, [], _, _}|Nestings]) ->
    lists:flatten([get_arity1(T, Constraints, Nestings) || {_, Key, _}=T <- Types, not lists:member(atom_to_list(Key), Keys)]);
get_arity1({map, Types}, Constraints, [{'map', _Keys, Key, _, _}|Nestings]) ->
    case [V || {_, K, V} <- Types, K =:= list_to_atom(Key)] of
        [] -> none;
        [Type] -> get_arity1(Type, Constraints, Nestings)
    end;
get_arity1({map_field_assoc, K, _V}, C, Nestings) ->
    get_arity1(K, C, Nestings);
get_arity1({map_field_exact, K, _V}, C, Nestings) ->
    get_arity1(K, C, Nestings);
get_arity1({union, Types}, Constraints, Nestings) ->
    Arities = [get_arity1(T, Constraints, Nestings) || T <- Types],
    [X || X <- lists:flatten(Arities), X/=none];
get_arity1({ann_type, _Var, Type}, Constraints, Nestings) ->
    get_arity1(Type, Constraints, Nestings);
get_arity1({user_type, _, _, _, Type}, Constraints, Nestings) ->
    get_arity1(Type, Constraints, Nestings);
get_arity1(_, _, _) ->
    none.

%% get_atoms returns the valid atoms in the current context as a list
get_atoms(Constraints, Type, Nestings) ->
    case get_atoms1(Type, Constraints, Nestings) of
        List when is_list(List) -> [io_lib:write_atom(Atom) || Atom <- List];
        Atom when is_atom(Atom) -> [io_lib:write_atom(Atom)]
    end.
get_atoms1({var, _Var}=C, Constraints, Nestings) ->
    case get_constraint(C, Constraints) of
        {constraint, _, T} -> get_atoms1(T, Constraints, Nestings);
        _ -> []
    end;
get_atoms1({list, T}, Constraints, [{'list', _, _}|Nestings]) ->
    get_atoms1(T, Constraints, Nestings);
get_atoms1({tuple, LT}, Constraints, [{'tuple', Args, _}|Nestings]) when length(LT) >= length(Args)+1 ->
    case edlin_expand:match_arguments1(LT, Constraints, Args) of
        true -> get_atoms1(lists:nth(length(Args)+1, LT), Constraints, Nestings);
        false -> []
    end;
get_atoms1({map, Types}, Constraints, [{'map', Keys, [], _, _}|Nestings]) ->
    lists:flatten([get_atoms1(T, Constraints, Nestings) || {_, Key, _}=T <- Types, not lists:member(atom_to_list(Key), Keys)]);
get_atoms1({map, Types}, Constraints, [{'map', _Keys, Key, _, _}|Nestings]) ->
    case [V || {_, K, V} <- Types, K =:= list_to_atom(Key)] of
        [] -> [];
        [Type] -> get_atoms1(Type, Constraints, Nestings)
    end;
get_atoms1({map_field_assoc, K, _V}, C, Nestings) ->
    get_atoms1(K, C, Nestings);
get_atoms1({map_field_exact, K, _V}, C, Nestings) ->
    get_atoms1(K, C, Nestings);
get_atoms1( {union, Types}, Constraints, Nestings) ->
    Atoms = [get_atoms1(T, Constraints, Nestings) || T <- Types],
    [X || X <- lists:flatten(Atoms), X/=[]];
get_atoms1(Atom, _Constraints, []) when is_atom(Atom) ->
    Atom;
get_atoms1({user_type, _, _, _, Type}, Constraints, Nestings) ->
    get_atoms1(Type, Constraints, Nestings);
get_atoms1(_, _, _) ->
    [].

get_types(Constraints, T, Nestings) ->
    get_types(Constraints, T, Nestings,[]).
get_types(Constraints, T, Nestings, Options) ->
    MaxUserTypeExpansions = 1,
    case get_types1(T, Constraints, Nestings, MaxUserTypeExpansions, Options) of
        [] -> [];
        [_|_]=Types -> [Type || Type <- Types, Type /= []];
        Type -> [Type]
    end.
get_types1({var, _Var}=C, Constraints, Nestings, MaxUserTypeExpansions, Options) ->
    case get_constraint(C, Constraints) of
        {constraint, _, T} -> get_types1(T, Constraints, Nestings, MaxUserTypeExpansions, Options);
        _ -> []
    end;
get_types1({union, Types}, Cs, Nestings, MaxUserTypeExpansions, Options) ->
    lists:flatten([get_types1(T, Cs, Nestings, MaxUserTypeExpansions, Options) || T <- Types]);

get_types1({list, T}, Cs, [{list, _Args, _}|Nestings], MaxUserTypeExpansions, Options) ->
    get_types1(T, Cs, Nestings, MaxUserTypeExpansions, Options);
get_types1({tuple, LT}, Cs, [{tuple, Args, _}|Nestings], MaxUserTypeExpansions, Options) when length(LT) >= length(Args)+1 ->
    case edlin_expand:match_arguments1(LT, Cs, Args) of
        true -> get_types1(lists:nth(length(Args)+1, LT), Cs, Nestings, MaxUserTypeExpansions, Options);
        false -> []
    end;
get_types1({'map', Types}, Cs, [{'map', Keys, [], _Args, _}|Nestings], MaxUserTypeExpansions, Options) ->
    lists:flatten([get_types1(T, Cs, Nestings, MaxUserTypeExpansions, Options) || {_, Key, _}=T <- Types, not lists:member(atom_to_list(Key), Keys)]);
get_types1({'map', Types}, Cs, [{'map', _, Key, _Args, _}|Nestings], MaxUserTypeExpansions, Options) ->
    case [V || {_, K, V} <- Types, K =:= list_to_atom(Key)] of
        [] -> [];
        [Type] -> get_types1(Type, Cs, Nestings, MaxUserTypeExpansions, Options)
    end;
get_types1({user_type, _Mod, _Name, _Params, Type}, Cs, Nestings, MaxUserTypeExpansions, [no_print]=Options) when MaxUserTypeExpansions > 0 ->
    lists:flatten([get_types1(Type, Cs, Nestings, MaxUserTypeExpansions-1, Options)]);
get_types1({user_type, _, _, _, Type}, Cs, Nestings, 0, [no_print]=Options) ->
    get_types1(Type, Cs, Nestings, 0, Options);
get_types1({ann_type, _Var, T}, Cs, Nestings, MaxUserTypeExpansions, [no_print]) ->
    get_types1(T, Cs, Nestings, MaxUserTypeExpansions, [no_print]);
get_types1({ann_type, _Var, _T}=Type, Cs, [], _MaxUserTypeExpansions, []) ->
    {print_type(Type, Cs), ""};
get_types1({ann_type, _Var, T}, Cs, Nestings, MaxUserTypeExpansions, []) ->
    get_types1(T, Cs, Nestings, MaxUserTypeExpansions, []);
get_types1(Type, _Cs, [], _, [no_print]) ->
    Type;
get_types1({user_type, Mod, Name, Params, Type}, Cs, Nestings, MaxUserTypeExpansions, []) when MaxUserTypeExpansions > 0 ->
    Title = print_type({type, Mod, Name, Params}, Cs, []),
    Elems = lists:flatten([get_types1(Type, Cs, Nestings, MaxUserTypeExpansions-1, [])]),
    #{title=>Title, elems=>Elems, options=>[{separator, " :: "}, {highlight_all}]};
get_types1({user_type, _, _, _, Type}, Cs, Nestings, 0, []) ->
    get_types1(Type, Cs, Nestings, 0, []);
get_types1(Type, Cs, [],_, []) ->
    {print_type(Type, Cs), ""};
get_types1(_, _, _, _, _) -> [].

get_constraint(Type, Constraints) ->
    case [ X || {constraint, T, _}=X <- Constraints, T == Type] of
        [C|_] -> C;
        [] -> []
    end.

print_type(Type, Constraints) ->
    lists:flatten(print_type(Type, Constraints, [], [])).
print_type(Type, Constraints, Options) ->
    lists:flatten(print_type(Type, Constraints, [], Options)).
print_type({var, Name}=Var, Constraints, Visited, Options) ->
    case lists:member(Var, Visited) of
        true -> atom_to_list(Name);
        false ->
            case get_constraint(Var, Constraints) of
                {constraint, _, T2} -> print_type(T2, Constraints, [Var| Visited], Options);
                _ -> atom_to_list(Name)
            end
    end;
print_type(Atom, _Cs, _V, _) when is_atom(Atom) -> io_lib:write_atom(Atom);
print_type({{parameters, Ps}, {return, R}}, Cs, V, Options) ->
    "fun(("++lists:join(", ", [print_type(X, Cs, V, Options) || X <- Ps]) ++ ") -> " ++ print_type(R, Cs, V, Options) ++ ")";
print_type({list, Type}, Cs, V, Options)->
    "[" ++ print_type(Type, Cs, V, Options) ++  "]";
print_type({tuple, Types}, Cs, V, Options) when is_list(Types) ->
    Types1 = [print_type(X, Cs, V, Options) || X <- Types],
    case Types1 of
        [] -> "{}";
        _ -> "{"++ lists:nth(1, Types1) ++ ", ...}"
    end;
print_type({ann_type, Var, Type}, Cs, V, Options) ->
    print_type(Var, Cs, V, Options) ++ " :: " ++ print_type(Type, Cs, V, Options);
print_type({map, Types}, Cs, V, Options) ->
    Types1 = [print_type(X, Cs, V, Options) || X <- Types],
    "#{"++lists:join(", ", Types1) ++ "}";
print_type({map_field_assoc, Type1, Type2}, Cs, V, Options) ->
    print_type(Type1, Cs, V, Options) ++ "=>" ++ print_type(Type2, Cs, V, Options);
print_type({map_field_exact, Type1, Type2}, Cs, V, Options) ->
    print_type(Type1, Cs, V, Options) ++ ":=" ++ print_type(Type2, Cs, V, Options);
print_type({integer, Int}, _Cs, _V, _) ->
    integer_to_list(Int);
print_type({op, Op, Type}, Cs, V, Options) ->
    "op ("++atom_to_list(Op)++" "++print_type(Type, Cs, V, Options)++")";
print_type({op, Op, Type1, Type2}, Cs, V, Options) ->
    "op ("++print_type(Type1, Cs, V, Options)++" "++atom_to_list(Op)++" "++print_type(Type2, Cs, V, Options)++")";
print_type({record, Record}, _Cs, _V, _) ->
    "#" ++ atom_to_list(Record);
print_type({type, range, [{integer, Int1},{integer, Int2}]}, _Cs, _V, _) ->
    integer_to_list(Int1) ++ ".." ++ integer_to_list(Int2);
print_type({type, non_neg_integer, []}, _Cs, _V, _) ->
    "integer() >= 0";
print_type({type, neg_integer, []}, _Cs, _V, _) ->
    "integer() < 0";
print_type({type, pos_integer, []}, _Cs, _V, _) ->
    "integer() > 0";
print_type({type, Name, []}, _Cs, _V, _) ->
    atom_to_list(Name)++"()";
print_type({type, Name, Params}, _Cs, _V, _) ->
    atom_to_list(Name) ++ "(" ++ lists:join(", ",[ extract_param(P) || P <- Params]) ++ ")";
print_type({union, Types}, Cs, V, Options) ->
    lists:join(" | ", [print_type(X, Cs, V, Options) || X <- Types]);
print_type({type, Mod, Name, Params}, _Cs, _V, _) ->
    atom_to_list(Mod) ++ ":" ++ atom_to_list(Name) ++
        "(" ++ lists:join(", ", [extract_param(P) || P <- Params]) ++ ")";
print_type({user_type, Mod, Name, Params, Type}, Cs, V, Options) ->
    First = proplists:get_value(first_only, Options, false),
    case First of
        true -> print_type({type, Mod, Name, Params}, Cs, V, Options);
        _ -> print_type({type, Mod, Name, Params}, Cs, V, Options) ++ " :: " ++ print_type(Type, Cs, V, Options)
    end;
print_type(_,_,_,_) -> atom_to_list(unknown).


extract_param({var, Var}) ->
    atom_to_list(Var);
extract_param({integer, Value}) ->
    io_lib:format("~p",[Value]);
extract_param({type, Type,_}) ->
    io_lib:format("~p", [Type]);
extract_param(T)->
    print_type(T, []).
