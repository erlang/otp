%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2018. All Rights Reserved.
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
%% Purpose : Replace values by aliases from patterns optimisation for Core

%% Replace expressions by aliases from patterns. For example:
%%
%%    example({ok, Val}) ->
%%        {ok, Val}.
%%
%% will become:
%%
%%    example({ok, Val} = Tuple) ->
%%        Tuple.
%%
%% Currently this pass aliases tuple and cons nodes made of literals,
%% variables and other cons. The tuple/cons may appear anywhere in the
%% pattern and it will be aliased if used later on.
%%
%% Notice a tuple/cons made only of literals is not aliased as it may
%% be part of the literal pool.

-module(sys_core_alias).

-export([module/2]).

-include("core_parse.hrl").

-define(NOTSET, 0).

-record(sub, {p=#{} :: #{term() => ?NOTSET | atom()},              %% Found pattern substitutions
              v=cerl_sets:new() :: cerl_sets:set(cerl:var_name()), %% Variables used by patterns
              t=undefined :: term()}).                             %% Temporary information from pre to post

-type sub() :: #sub{}.

-spec module(cerl:c_module(), [compile:option()]) ->
        {'ok',cerl:c_module(),[]}.

module(#c_module{defs=Ds0}=Mod, _Opts) ->
    Ds1 = [def(D) || D <- Ds0],
    {ok,Mod#c_module{defs=Ds1},[]}.

def({#c_var{name={F,Arity}}=Name,B0}) ->
    try
        put(new_var_num, 0),
        {B1,_} = cerl_trees:mapfold(fun pre/2, fun post/2, sub_new(undefined), B0),
        erase(new_var_num),
        {Name,B1}
    catch
        Class:Error:Stack ->
            io:fwrite("Function: ~w/~w\n", [F,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

pre(#c_let{vars=Vars}=Node, Sub) ->
    {Node,sub_fold(get_variables(Vars), Sub)};

pre(#c_fun{vars=Vars}=Node, Sub) ->
    {Node,sub_fold(get_variables(Vars), Sub)};

pre(#c_clause{pats=Pats}=Node, Sub0) ->
    VarNames = get_variables(Pats),
    Sub1 = sub_fold(VarNames, Sub0),
    Keys = get_pattern_keys(Pats),
    Sub2 = sub_add_keys(Keys, Sub1),

    #sub{v=SubNames,t=Temp} = Sub2,
    Sub3 = Sub2#sub{v=merge_variables(VarNames, SubNames),
                    t={clause,Pats,Keys,SubNames,Temp}},

    {Node#c_clause{pats=[]},Sub3};

pre(Node, Sub0) ->
    %% We cache only tuples and cons.
    case cerl:is_data(Node) andalso not cerl:is_literal(Node) of
        false ->
            {Node,Sub0};
        true ->
            Kind = cerl:data_type(Node),
            Es = cerl:data_es(Node),
            case sub_cache_nodes(Kind, Es, Sub0) of
                {Name,Sub1} ->
                    {cerl:ann_c_var(cerl:get_ann(Node), Name),Sub1};
                error ->
                    {Node,Sub0}
            end
    end.

post(#c_let{}=Node, Sub) ->
    {Node,sub_unfold(Sub)};

post(#c_fun{}=Node, Sub) ->
    {Node,sub_unfold(Sub)};

post(#c_clause{}=Node, #sub{t={clause,Pats0,Keys,V,T}}=Sub0) ->
    {Sub1,PostKeys} = sub_take_keys(Keys, Sub0),
    Pats1 = put_pattern_keys(Pats0, PostKeys),
    Sub2 = sub_unfold(Sub1#sub{v=V,t=T}),
    {Node#c_clause{pats=Pats1},Sub2};

post(Node, Sub) ->
    {Node,Sub}.

%% sub_new/1
%% sub_add_keys/2
%% sub_take_keys/3
%% sub_cache_nodes/3
%%
%% Manages the substitutions record.

%% Builds a new sub.
-spec sub_new(term()) -> sub().
sub_new(Temp) ->
    #sub{t=Temp}.

%% Folds the sub into a new one if the variables in nodes are not disjoint
sub_fold(VarNames, #sub{v=SubNames}=Sub) ->
    case is_disjoint_variables(VarNames, SubNames) of
        true  -> Sub#sub{t={temp,Sub#sub.t}};
        false -> sub_new({sub,Sub})
    end.

%% Unfolds the sub in case one was folded in the previous step
sub_unfold(#sub{t={temp,Temp}}=Sub) ->
    Sub#sub{t=Temp};
sub_unfold(#sub{t={sub,Sub}}) ->
    Sub.

%% Adds the keys extracted from patterns to the state.
-spec sub_add_keys([term()], sub()) -> sub().
sub_add_keys(Keys, #sub{p=Pat0}=Sub) ->
    Pat1 =
        lists:foldl(fun(Key, Acc) ->
            false = maps:is_key(Key, Acc),         %Assertion.
            maps:put(Key, ?NOTSET, Acc)
        end, Pat0, Keys),
    Sub#sub{p=Pat1}.

%% Take the keys from the map taking into account the keys
%% that have changed as those must become aliases in the pattern.
-spec sub_take_keys([term()], sub()) -> {sub(), [{term(), atom()}]}.
sub_take_keys(Keys, #sub{p=Pat0}=Sub) ->
    {Pat1,Acc} = sub_take_keys(Keys, Pat0, []),
    {Sub#sub{p=Pat1},Acc}.

sub_take_keys([K|T], Sub0, Acc) ->
    case maps:take(K, Sub0) of
        {?NOTSET,Sub1} ->
            sub_take_keys(T, Sub1, Acc);
        {Name,Sub1} ->
            sub_take_keys(T, Sub1, [{K,Name}|Acc])
    end;
sub_take_keys([], Sub, Acc) ->
    {Sub,Acc}.

%% Check if the node can be cached based on the state information.
%% If it can be cached and it does not have an alias for it, we
%% build one.
-spec sub_cache_nodes(atom(), [cerl:cerl()], sub()) -> {atom(), sub()} | error.
sub_cache_nodes(Kind, Nodes, #sub{p=Pat}=Sub) ->
    case nodes_to_key(Kind, Nodes) of
        {ok, Key} ->
            case Pat of
                #{Key := ?NOTSET} ->
                    new_var_name(Key, Sub);
                #{Key := Name} ->
                    {Name,Sub};
                #{} ->
                    error
            end;
        error ->
            error
    end.

new_var_name(Key, #sub{p=Pat}=Sub) ->
    Counter = get(new_var_num),
    Name = list_to_atom("@r" ++ integer_to_list(Counter)),
    put(new_var_num, Counter + 1),
    {Name,Sub#sub{p=maps:put(Key, Name, Pat)}}.

%% get_variables/1
%% is_disjoint_variables/2
%% merge_variables/2

get_variables(NodesList) ->
    cerl_sets:from_list([Var || Node <- NodesList, Var <- cerl_trees:variables(Node)]).

is_disjoint_variables(Vars1, Vars2) ->
    cerl_sets:is_disjoint(Vars1, Vars2).

merge_variables(Vars1, Vars2) ->
    cerl_sets:union(Vars1, Vars2).

%% get_pattern_keys/2
%% put_pattern_keys/2
%%
%% Gets keys from patterns or add them as aliases.

get_pattern_keys(Patterns) ->
    lists:foldl(fun get_pattern_keys/2, [], Patterns).

get_pattern_keys(#c_tuple{es=Es}, Acc0) ->
    Acc1 = accumulate_pattern_keys(tuple, Es, Acc0),
    lists:foldl(fun get_pattern_keys/2, Acc1, Es);
get_pattern_keys(#c_cons{hd=Hd,tl=Tl}, Acc0) ->
    Acc1 = accumulate_pattern_keys(cons, [Hd, Tl], Acc0),
    get_pattern_keys(Tl, get_pattern_keys(Hd, Acc1));
get_pattern_keys(#c_alias{pat=Pat}, Acc0) ->
    get_pattern_keys(Pat, Acc0);
get_pattern_keys(#c_map{es=Es}, Acc0) ->
    lists:foldl(fun get_pattern_keys/2, Acc0, Es);
get_pattern_keys(#c_map_pair{val=Val}, Acc0) ->
    get_pattern_keys(Val, Acc0);
get_pattern_keys(_, Acc) ->
    Acc.

accumulate_pattern_keys(Kind, Nodes, Acc) ->
    case nodes_to_key(Kind, Nodes) of
        {ok,Key} -> [Key|Acc];
        error -> Acc
    end.

put_pattern_keys(Patterns, []) ->
    Patterns;
put_pattern_keys(Patterns, Keys) ->
    {NewPatterns,Map} =
        lists:mapfoldl(fun alias_pattern_keys/2, maps:from_list(Keys), Patterns),
    %% Check all aliases have been consumed from the map.
    0 = map_size(Map),
    NewPatterns.

alias_pattern_keys(#c_tuple{anno=Anno,es=Es0}=Node, Acc0) ->
    {Es1,Acc1} = lists:mapfoldl(fun alias_pattern_keys/2, Acc0, Es0),
    nodes_to_alias(tuple, Es0, Anno, Node#c_tuple{es=Es1}, Acc1);
alias_pattern_keys(#c_cons{anno=Anno,hd=Hd0,tl=Tl0}=Node, Acc0) ->
    {Hd1,Acc1} = alias_pattern_keys(Hd0, Acc0),
    {Tl1,Acc2} = alias_pattern_keys(Tl0, Acc1),
    nodes_to_alias(cons, [Hd0, Tl0], Anno, Node#c_cons{hd=Hd1,tl=Tl1}, Acc2);
alias_pattern_keys(#c_alias{pat=Pat0}=Node, Acc0) ->
    {Pat1,Acc1} = alias_pattern_keys(Pat0, Acc0),
    {Node#c_alias{pat=Pat1}, Acc1};
alias_pattern_keys(#c_map{es=Es0}=Node, Acc0) ->
    {Es1,Acc1} = lists:mapfoldl(fun alias_pattern_keys/2, Acc0, Es0),
    {Node#c_map{es=Es1}, Acc1};
alias_pattern_keys(#c_map_pair{val=Val0}=Node, Acc0) ->
    {Val1,Acc1} = alias_pattern_keys(Val0, Acc0),
    {Node#c_map_pair{val=Val1}, Acc1};
alias_pattern_keys(Pattern, Acc) ->
    {Pattern,Acc}.

%% Check if a node must become an alias because
%% its pattern was used later on as an expression.
nodes_to_alias(Kind, Inner, Anno, Node, Keys0) ->
    case nodes_to_key(Kind, Inner) of
        {ok,Key} ->
            case maps:take(Key, Keys0) of
                {Name,Keys1} ->
                    Var = cerl:ann_c_var(Anno, Name),
                    {cerl:ann_c_alias(Anno, Var, Node), Keys1};
                error ->
                    {Node,Keys0}
            end;
        error ->
            {Node,Keys0}
     end.

%% Builds the key used to check if a value can be
%% replaced by an alias. It considers literals,
%% aliases, variables, tuples and cons recursively.
nodes_to_key(Kind, Nodes) ->
    nodes_to_key(Nodes, [], Kind).

nodes_to_key([#c_alias{var=Var}|T], Acc, Kind) ->
    nodes_to_key([Var|T], Acc, Kind);
nodes_to_key([#c_var{name=Name}|T], Acc, Kind) ->
    nodes_to_key(T, [[var,Name]|Acc], Kind);
nodes_to_key([Node|T], Acc0, Kind) ->
    case cerl:is_data(Node) of
        false ->
            error;
        true ->
            case nodes_to_key(cerl:data_es(Node), [], cerl:data_type(Node)) of
                {ok,Key} ->
                    nodes_to_key(T, [Key|Acc0], Kind);
                error ->
                    error
            end
    end;
nodes_to_key([], Acc, Kind) ->
    {ok,[Kind|Acc]}.
