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

%% @doc EDoc interface to Erlang specifications and types.

-module(edoc_specs).

-export([type/2, spec/2, dummy_spec/1, docs/2]).

-export([add_data/4, tag/1, is_tag/1]).

-include("edoc.hrl").
-include("edoc_types.hrl").

-type syntaxTree() :: erl_syntax:syntaxTree().

-define(TOP_TYPE, term).

%%
%%  Exported functions
%%

-spec type(Form::syntaxTree(), TypeDocs::dict:dict()) -> #tag{}.

%% @doc Convert an Erlang type to EDoc representation.
%% TypeDocs is a dict of {Name, Doc}.
%% Note: #t_typedef.name is set to {record, R} for record types.
type(Form, TypeDocs) ->
    {Name, Data0} = analyze_type_attribute(Form),
    {TypeName, Type, Args, Doc} =
        case Data0 of
            {R, Fs} ->
                record = Name,
                L = erl_syntax:get_pos(Form),
                {{record, R}, {type, L, record, [{atom,L,R} | Fs]}, [], ""};
            {N,T,As} ->
                type = tag(Name),
                Doc0 =
                    case dict:find({N, length(As)}, TypeDocs) of
                        {ok, Doc1} ->
                            Doc1;
                        error ->
                            ""
                    end,
                {#t_name{name = N}, T, As, Doc0}
        end,
    #tag{name = type, line = get_line(element(2, Type)),
         origin = code,
         data = {#t_typedef{name = TypeName,
                            args = d2e(Args),
                            type = d2e(opaque2abstr(Name, Type))},
                 Doc}}.

-spec spec(Form::syntaxTree(), ClauseN::pos_integer()) -> #tag{}.

%% @doc Convert an Erlang spec to EDoc representation.
spec(Form, Clause) ->
    {Name, _Arity, TypeSpecs} = get_spec(Form),
    TypeSpec = lists:nth(Clause, TypeSpecs),
    #tag{name = spec, line = get_line(element(2, TypeSpec)),
         origin = code,
         data = aspec(d2e(TypeSpec), Name)}.

-spec dummy_spec(Form::syntaxTree()) -> #tag{}.

%% @doc Create a #tag{} record where data is a string with the name of
%% the given Erlang spec and an empty list of arguments.
dummy_spec(Form) ->
    {#t_name{name = Name}, Arity, TypeSpecs} = get_spec(Form),
    As = lists:join(",", lists:duplicate(Arity, "_X")),
    S = lists:flatten(io_lib:format("~p(~s) -> true\n", [Name, As])),
    #tag{name = spec, line = get_line(element(2, hd(TypeSpecs))),
         origin = code, data = S}.

-spec docs(Forms::[syntaxTree()],
           CommentFun :: fun( ([syntaxTree()], Line :: term()) -> #tag{} ))
          -> dict:dict().

%% @doc Find comments after -type/-opaque declarations.
%% Postcomments "inside" the type are skipped.
docs(Forms, CommentFun) ->
    find_type_docs(Forms, [], CommentFun).

-type entry() :: #entry{}.
-type module_info() :: #module{}.
-type entries() :: [entry()].
-spec add_data(Entries::entries(), Options::proplists:proplist(),
               File::file:filename(), Module::module_info()) -> entries().

%% @doc Create tags a la EDoc for Erlang specifications and types.
%% Exported types and types used (indirectly) by Erlang specs are
%% added to the entries.
add_data(Entries, Opts, File, Module) ->
    TypeDefs0 = espec_types(Entries),
    TypeTable = ets:new(etypes, [ordered_set]),
    Es1 = expand_records(Entries, TypeDefs0, TypeTable, Opts, File, Module),
    Es = [use_tags(E, TypeTable) || E <- Es1],
    true = ets:delete(TypeTable),
    Es.

%%
%%  Local functions
%%

aspec(#t_spec{}=Spec, Name) ->
    Spec#t_spec{name = Name};
aspec(Type, Name) ->
    #t_spec{name = Name, type = Type}.

get_spec(Form) ->
    {spec, Data0} = erl_syntax_lib:analyze_wild_attribute(Form),
    case Data0 of
        {{F,A}, D} ->
            {#t_name{name = F}, A, D};
        {{M,F,A}, D} ->
            {#t_name{module = M, name = F}, A, D}
    end.

find_type_docs([], Cs, _Fun) ->
    dict:from_list(Cs);
find_type_docs([F | Fs], Cs, Fun) ->
    try get_name_and_last_line(F) of
        {Name, LastTypeLine} ->
            C0 = erl_syntax:comment(["% @type f(). "]),
            C1 = erl_syntax:set_pos(C0, LastTypeLine),
            %% Postcomments before the dot after the typespec are ignored.
            C2 = [C1 | [C ||
                           C <- erl_syntax:get_postcomments(F),
                           erl_syntax:get_pos(C) >= LastTypeLine]],
            C3 = collect_comments(Fs, LastTypeLine),
            #tag{data = Doc0} = Fun(lists:reverse(C2 ++ C3), LastTypeLine),
            case strip(Doc0) of % Strip away "f(). \n"
                "" ->
                    find_type_docs(Fs, Cs, Fun);
                Doc ->
                    W = edoc_wiki:parse_xml(Doc, LastTypeLine),
                    find_type_docs(Fs, [{Name, W}|Cs], Fun)
            end
    catch _:_ ->
            find_type_docs(Fs, Cs, Fun)
    end.

collect_comments([], _Line) ->
    [];
collect_comments([F | Fs], Line) ->
    L1 = erl_syntax:get_pos(F),
    if
        L1 =:= Line + 1;
        L1 =:= Line -> % a separate postcomment
            case is_comment(F) of
                true ->
                    [F | collect_comments(Fs, L1)];
                false ->
                    []
            end;
        true ->
            []
    end.
%% Note: there is a creepy bug concerning an include file terminated
%% by a -type attribute and the include statement is followed by a
%% comment (which is not meant to be documentation of the type).

is_comment(F) ->
    erl_syntax_lib:analyze_form(F) =:= comment.

strip("") ->
    "";
strip([$\n | S]) ->
    S;
strip([_ | S]) ->
    strip(S).

%% Find the type name and the greatest line number of a type spec.
%% Should use syntax_tools but this has to do for now.
get_name_and_last_line(F) ->
    {Name, Data} = analyze_type_attribute(F),
    type = edoc_specs:tag(Name),
    Attr = {attribute, erl_syntax:get_pos(F), Name, Data},
    Fun = fun(A) ->
                  Line = get_line(A),
                  case get('$max_line') of
                      Max when Max < Line ->
                          _ = put('$max_line', Line);
                      _ ->
                          ok
                  end
          end,
    undefined = put('$max_line', 0),
    _ = erl_parse:map_anno(Fun, Attr),
    Line = erase('$max_line'),
    TypeName = case Data of
                   {N, _T, As} when is_atom(N) -> % skip records
                       {N, length(As)}
               end,
    {TypeName, Line}.

get_line(Anno) ->
    erl_anno:line(Anno).

%% Collect all Erlang types. Types in comments (@type) shadow Erlang
%% types (-spec/-opaque).
espec_types(Entries) ->
    Tags = get_all_tags(Entries),
    CommTs = [type_name(T) ||
                 #tag{name = type, origin = comment}=T <- Tags],
    CT = sets:from_list(CommTs),
    [T || #tag{name = Name, origin = code}=T <- Tags,
          tag(Name) =:= type,
          not sets:is_element(type_name(T), CT)].

get_all_tags(Es) ->
    lists:flatmap(fun (#entry{data = Ts}) -> Ts end, Es).

%% Turns an opaque type into an abstract datatype.
%% Note: top level annotation is ignored.
opaque2abstr(opaque, _T) -> undefined;
opaque2abstr(record, T) -> T;
opaque2abstr(type, T) -> T.

%% Replaces the parameters extracted from the source (by
%% edoc_extract:parameters/1) by annotations and variable names, using
%% the source parameters as default values
%% Selects seen types (exported types, types used by specs),
%% skips records and unused types.
use_tags(#entry{data = Ts}=E, TypeTable) ->
    use_tags(Ts, E, TypeTable, []).

use_tags([], E, _TypeTable, NTs) ->
    E#entry{data = lists:reverse(NTs)};
use_tags([#tag{origin = code}=T | Ts], E, TypeTable, NTs) ->
    case tag(T#tag.name) of
        spec ->
            Args = params(T, E#entry.args),
            use_tags(Ts, E#entry{args = Args}, TypeTable, [T | NTs]);
        type ->
            TypeName = type_name(T),
            case ets:lookup(TypeTable, TypeName) of
                [{{{record,_},_},_,_}] ->
                    use_tags(Ts, E, TypeTable, NTs);
                [{_,_,not_seen}] ->
                    use_tags(Ts, E, TypeTable, NTs);
                [] ->
                    use_tags(Ts, E, TypeTable, NTs);
                [{TypeName, Tag, seen}] ->
                    use_tags(Ts, E, TypeTable, [Tag | NTs])
            end
    end;
use_tags([T | Ts], E, TypeTable, NTs) ->
    use_tags(Ts, E, TypeTable, [T | NTs]).

params(#tag{name = spec, data=#t_spec{type = #t_fun{args = As}}}, Default) ->
    parms(As, Default).

parms([], []) ->
    [];
parms([A | As], [D | Ds]) ->
    [param(A, D) | parms(As, Ds)].

param(#t_paren{type = Type}, Default) ->
    param(Type, Default);
param(#t_record{name = #t_atom{val = Name}}=T, Default) ->
    AtomList = atom_to_list(Name),
    case AtomList =:= lists:flatten(io_lib:write_atom(Name)) of
        true ->
            list_to_atom(capitalize(AtomList));
        false ->
            arg_name(?t_ann(T), Default)
    end;
param(T, Default) ->
    arg_name(?t_ann(T), Default).

capitalize([C | Cs]) when C >= $a, C =< $z -> [C - 32 | Cs];
capitalize([C | Cs]) when C >= $\340, C =< $\376, C /= $\367 -> [C - 32 | Cs];
capitalize(Cs) -> Cs.

%% Like edoc_types:arg_name/1
arg_name([], Default) ->
    Default;
arg_name([A | As], Default) ->
    case is_name(A) of
        true -> A;
        false -> arg_name(As, Default)
    end.

is_name(A) ->
    is_atom(A).

d2e(T) ->
    d2e(T, 0).

d2e({ann_type,_,[V, T0]}, Prec) ->
    %% Note: the -spec/-type syntax allows annotations everywhere, but
    %% EDoc does not. The fact that the annotation is added to the
    %% type here does not necessarily mean that it will be used by the
    %% layout module.
    {_L,P,R} = erl_parse:type_inop_prec('::'),
    T1 = d2e(T0, R),
    T = ?add_t_ann(T1, element(3, V)),
    maybe_paren(P, Prec, T); % the only necessary call to maybe_paren()
d2e({remote_type,_,[{atom,_,M},{atom,_,F},Ts0]}, _Prec) ->
    Ts = d2e(Ts0),
    typevar_anno(#t_type{name = #t_name{module = M, name = F}, args = Ts}, Ts);
d2e({type,_,'fun',[{type,_,product,As0},Ran0]}, _Prec) ->
    Ts = [Ran|As] = d2e([Ran0|As0]),
    %% Assume that the linter has checked type variables.
    typevar_anno(#t_fun{args = As, range = Ran}, Ts);
d2e({type,_,'fun',[A0={type,_,any},Ran0]}, _Prec) ->
    Ts = [A, Ran] = d2e([A0, Ran0]),
    typevar_anno(#t_fun{args = [A], range = Ran}, Ts);
d2e({type,_,'fun',[]}, _Prec) ->
    #t_type{name = #t_name{name = function}, args = []};
d2e({type,_,any}, _Prec) ->
    #t_var{name = '...'}; % Kludge... not a type variable!
d2e({type,_,nil,[]}, _Prec) ->
    #t_nil{};
d2e({paren_type,_,[T]}, Prec) ->
    d2e(T, Prec);
d2e({type,_,list,[T0]}, _Prec) ->
    T = d2e(T0),
    typevar_anno(#t_list{type = T}, [T]);
d2e({type,_,nonempty_list,[T0]}, _Prec) ->
    T = d2e(T0),
    typevar_anno(#t_nonempty_list{type = T}, [T]);
d2e({type,_,bounded_fun,[T,Gs]}, _Prec) ->
    [F0|Defs] = d2e([T|Gs]),
    F = ?set_t_ann(F0, lists:keydelete(type_variables, 1, ?t_ann(F0))),
    %% Assume that the linter has checked type variables.
    #t_spec{type = typevar_anno(F, [F0]), defs = Defs};
d2e({type,_,range,[V1,V2]}, Prec) ->
    {_L,P,_R} = erl_parse:type_inop_prec('..'),
    {integer,_,I1} = erl_eval:partial_eval(V1),
    {integer,_,I2} = erl_eval:partial_eval(V2),
    T0 = #t_integer_range{from = I1, to = I2},
    maybe_paren(P, Prec, T0);
d2e({type,_,constraint,[Sub,Ts0]}, _Prec) ->
    case {Sub,Ts0} of
        {{atom,_,is_subtype},[{var,_,N},T0]} ->
            Ts = [T] = d2e([T0]),
            #t_def{name = #t_var{name = N}, type = typevar_anno(T, Ts)};
        {{atom,_,is_subtype},[ST0,T0]} ->
            %% Should not happen.
            Ts = [ST,T] = d2e([ST0,T0]),
            #t_def{name = ST, type = typevar_anno(T, Ts)};
        _ ->
            throw_error(get_line(element(2, Sub)), "cannot handle guard", [])
    end;
d2e({type,_,union,Ts0}, Prec) ->
    {_L,P,R} = erl_parse:type_inop_prec('|'),
    Ts = d2e(Ts0, R),
    T = maybe_paren(P, Prec, #t_union{types = Ts}),
    typevar_anno(T, Ts);
d2e({type,_,tuple,any}, _Prec) ->
    #t_type{name = #t_name{name = tuple}, args = []};
d2e({type,_,binary,[Base,Unit]}, _Prec) ->
    {integer,_,B} = erl_eval:partial_eval(Base),
    {integer,_,U} = erl_eval:partial_eval(Unit),
    #t_binary{base_size = B, unit_size = U};
d2e({type,_,map,any}, _Prec) ->
    #t_type{name = #t_name{name = map}, args = []};
d2e({type,_,map,Es}, _Prec) ->
    #t_map{types = d2e(Es) };
d2e({type,_,map_field_assoc,[K,V]}, Prec) ->
    T = #t_map_field{assoc_type = assoc, k_type = d2e(K), v_type=d2e(V) },
    {P,_R} = erl_parse:type_preop_prec('#'),
    maybe_paren(P, Prec, T);
d2e({type,_,map_field_exact,[K,V]}, Prec) ->
    T = #t_map_field{assoc_type = exact, k_type = d2e(K), v_type=d2e(V) },
    {P,_R} = erl_parse:type_preop_prec('#'),
    maybe_paren(P, Prec, T);
d2e({type,_,tuple,Ts0}, _Prec) ->
    Ts = d2e(Ts0),
    typevar_anno(#t_tuple{types = Ts}, Ts);
d2e({type,_,record,[Name|Fs0]}, Prec) ->
    Atom = #t_atom{val = element(3, Name)},
    Fs = d2e(Fs0),
    {P,_R} = erl_parse:type_preop_prec('#'),
    T = maybe_paren(P, Prec, #t_record{name = Atom, fields = Fs}),
    typevar_anno(T, Fs);
d2e({type,_,field_type,[Name,Type0]}, Prec) ->
    {_L,P,R} = erl_parse:type_inop_prec('::'),
    Type = maybe_paren(P, Prec, d2e(Type0, R)),
    T = #t_field{name = #t_atom{val = element(3, Name)}, type = Type},
    typevar_anno(T, [Type]);
d2e({typed_record_field,{record_field,L,Name},Type}, Prec) ->
    d2e({type,L,field_type,[Name,Type]}, Prec);
d2e({typed_record_field,{record_field,L,Name,_E},Type}, Prec) ->
    d2e({type,L,field_type,[Name,Type]}, Prec);
d2e({record_field,L,_Name,_E}=F, Prec) ->
    d2e({typed_record_field,F,{type,L,any,[]}}, Prec); % Maybe skip...
d2e({record_field,L,_Name}=F, Prec) ->
    d2e({typed_record_field,F,{type,L,any,[]}}, Prec); % Maybe skip...
d2e({type,_,Name,Types0}, _Prec) ->
    Types = d2e(Types0),
    typevar_anno(#t_type{name = #t_name{name = Name}, args = Types}, Types);
d2e({user_type,_,Name,Types0}, _Prec) ->
    Types = d2e(Types0),
    typevar_anno(#t_type{name = #t_name{name = Name}, args = Types}, Types);
d2e({var,_,'_'}, _Prec) ->
    #t_type{name = #t_name{name = ?TOP_TYPE}};
d2e({var,_,TypeName}, _Prec) ->
    TypeVar = ordsets:from_list([TypeName]),
    T = #t_var{name = TypeName},
    %% Annotate type variables with the name of the variable.
    %% Doing so will stop edoc_layout (and possibly other layout modules)
    %% from using the argument name from the source or to invent a new name.
    T1 = ?add_t_ann(T, {type_variables, TypeVar}),
    ?add_t_ann(T1, TypeName);
d2e(L, Prec) when is_list(L) ->
    [d2e(T, Prec) || T <- L];
d2e({atom,_,A}, _Prec) ->
    #t_atom{val = A};
d2e(undefined = U, _Prec) -> % opaque
    U;
d2e(Expr, _Prec) ->
    {integer,_,I} = erl_eval:partial_eval(Expr),
    #t_integer{val = I}.

%% A type annotation (a tuple; neither an atom nor a list).
typevar_anno(Type, Ts) ->
    Vs = typevars(Ts),
    case ordsets:to_list(Vs) of
        [] -> Type;
        _ -> ?add_t_ann(Type, {type_variables, Vs})
    end.

typevars(Ts) ->
    ordsets:union(get_typevars(Ts)).

get_typevars(Ts) ->
    [Vs || T <- Ts, T =/= undefined, {type_variables, Vs} <- ?t_ann(T)].

maybe_paren(P, Prec, T)  when P < Prec ->
    #t_paren{type = T};
maybe_paren(_P, _Prec, T) ->
    T.

-record(parms, {tab, warn, file, line}).

%% Expands record references. Explicitly given record fields are kept,
%% but otherwise the fields from the record definition are substituted
%% for the reference. The reason is that there are no record types.
%% It is recommended to introduce types like "r() :: r{}" and then use
%% r() everywhere. The right hand side, r{}, is expanded in order to
%% show all fields.
%% Returns updated types in the ETS table DT.
expand_records(Entries, TypeDefs, DT, Opts, File, Module) ->
    TypeList = [{type_name(T), T, not_seen} || T <- TypeDefs],
    true = ets:insert(DT, TypeList),
    Warn = proplists:get_value(report_missing_types, Opts,
                               ?REPORT_MISSING_TYPES) =:= true,
    P = #parms{tab = DT, warn = Warn, file = File, line = 0},
    ExportedTypes = [Name ||
                        {export_type,Ts} <- Module#module.attributes,
                        is_list(Ts),
                        {N,I} <- Ts,
                        ets:member(DT, Name = {#t_name{name = N}, I})],
    _ = lists:foreach(fun({N,A}) -> true = seen_type(N, A, P)
                      end, ExportedTypes),
    entries(Entries, P, Opts).

entries([E0 | Es], P, Opts) ->
    E = case edoc_data:hidden_filter([E0], Opts) of
            [] ->
                E0;
            [_] ->
                E0#entry{data = specs(E0#entry.data, P)}
        end,
    [E | entries(Es, P, Opts)];
entries([], _P, _Opts) ->
    [].

specs([#tag{line = L, name = spec, origin = code, data = Spec}=Tag0 | Tags],
      P0) ->
    #t_spec{type = Type0, defs = Defs0} = Spec,
    P = P0#parms{line = L},
    Type = xrecs(Type0, P),
    Defs = xrecs(Defs0, P),
    Tag = Tag0#tag{data = Spec#t_spec{type = Type, defs = Defs}},
    [Tag | specs(Tags, P)];
specs([Tag | Tags], P) ->
    [Tag | specs(Tags, P)];
specs([], _P) ->
    [].

xrecs(#t_def{type = Type0}=T, P) ->
    Type = xrecs(Type0, P),
    T#t_def{type = Type};
xrecs(#t_type{name = Name, args = Args0}=T, P) ->
    Args = xrecs(Args0, P),
    NArgs = length(Args),
    true = seen_type(Name, NArgs, P),
    T#t_type{args = Args};
xrecs(#t_var{}=T, _P) ->
    T;
xrecs(#t_fun{args = Args0, range = Range0}=T, P) ->
    Args = xrecs(Args0, P),
    Range = xrecs(Range0, P),
    T#t_fun{args = Args, range = Range};
xrecs(#t_map{types = Ts0 }=T,P) ->
    Ts = xrecs(Ts0, P),
    T#t_map{types = Ts };
xrecs(#t_map_field{k_type=Kt, v_type=Vt}=T, P) ->
    T#t_map_field{k_type=xrecs(Kt,P), v_type=xrecs(Vt,P)};
xrecs(#t_tuple{types = Types0}=T, P) ->
    Types = xrecs(Types0, P),
    T#t_tuple{types = Types};
xrecs(#t_list{type = Type0}=T, P) ->
    Type = xrecs(Type0, P),
    T#t_list{type = Type};
xrecs(#t_nil{}=T, _P) ->
    T;
xrecs(#t_paren{type = Type0}=T, P) ->
    Type = xrecs(Type0, P),
    T#t_paren{type = Type};
xrecs(#t_nonempty_list{type = Type0}=T, P) ->
    Type = xrecs(Type0, P),
    T#t_nonempty_list{type = Type};
xrecs(#t_atom{}=T, _P) ->
    T;
xrecs(#t_integer{}=T, _P) ->
    T;
xrecs(#t_integer_range{}=T, _P) ->
    T;
xrecs(#t_binary{}=T, _P) ->
    T;
xrecs(#t_float{}=T, _P) ->
    T;
xrecs(#t_union{types = Types0}=T, P) ->
    Types = xrecs(Types0, P),
    T#t_union{types = Types};
xrecs(#t_record{fields = Fields0}=T, P) ->
    Fields1 = xrecs(Fields0, P),
    #t_record{name = #t_atom{val = Name}} = T,
    RName = {record, Name},
    true = seen_type(RName, 0, P),
    Fields = select_fields(Fields1, RName, P#parms.tab),
    T#t_record{fields = Fields};
xrecs(#t_field{type = Type0}=T, P) ->
    Type = xrecs(Type0, P),
    T#t_field{type = Type};
xrecs(undefined=T, _P) -> % opaque
    T;
xrecs([]=T, _P) ->
    T;
xrecs([E0 | Es0], P) ->
    [xrecs(E0, P) | xrecs(Es0, P)].

seen_type(N, NArgs, P) ->
    TypeName = {N, NArgs},
    #parms{tab = DT} = P,
    case {ets:lookup(DT, TypeName), N} of
        {[{TypeName, _, seen}], _} ->
            true;
        {[{TypeName, TagType, not_seen}], _} when N#t_name.module =:= [] ->
            expand_datatype(TagType, proper_type, DT, P);
        {[{TypeName, TagType, not_seen}], {record, _}} ->
            expand_datatype(TagType, record_type, DT, P);
        {[], {record, R}} ->
            #parms{warn = W, line = L, file = File} = P,
            [edoc_report:warning(L, File, "reference to untyped record ~w",
                                 [R]) || W],
            ets:insert(DT, {TypeName, fake, seen});
        {[], _} -> % External type or missing type.
            true
    end.

expand_datatype(Tag0, Kind, DT, P0) ->
    #tag{line = L, data = {T0, Doc}} = Tag0,
    #t_typedef{type = Type0, defs = []} = T0,
    TypeName = type_name(Tag0),
    true = ets:update_element(DT, TypeName, {3, seen}),
    P = P0#parms{line = L},
    Type = case Kind of
               record_type ->
                   #t_record{fields = Fields0} = Type0,
                   Fields = xrecs(Fields0, P),
                   Type0#t_record{fields = Fields};
               proper_type ->
                   xrecs(Type0, P)
           end,
    Tag = Tag0#tag{data={T0#t_typedef{type=Type}, Doc}},
    ets:insert(DT, {TypeName, Tag, seen}).

select_fields(Fields, Name, DT) ->
    RecordName = {Name, 0},
    case ets:lookup(DT, RecordName) of
        [{RecordName, fake, seen}] ->
            Fields;
        [{RecordName, #tag{data = {T, _Doc}}, seen}] ->
            #t_typedef{args = [], type = #t_record{fields = Fs}, defs = []}=T,
            [find_field(F, Fields) || F <- Fs]
    end.

find_field(F, Fs) ->
    case lists:keyfind(F#t_field.name, #t_field.name, Fs) of
        false -> F;
        NF -> NF
    end.

type_name(#tag{name = type,
               data = {#t_typedef{name = Name, args = As},_}}) ->
    {Name, length(As)}.

analyze_type_attribute(Form) ->
    Name = erl_syntax:atom_value(erl_syntax:attribute_name(Form)),
    case tag(Name) of
        type ->
            erl_syntax_lib:analyze_wild_attribute(Form);
        _ when Name =:= record ->
            {attribute, _, record, {N, Fields}} = erl_syntax:revert(Form),
            {record, {N, Fields}}
    end.

%% @doc Return `true' if `Tag' is one of the specification and type
%% attribute tags recognized by the Erlang compiler.

-spec is_tag(Tag::atom()) -> boolean().

is_tag(opaque) -> true;
is_tag(spec) -> true;
is_tag(type) -> true;
is_tag(_) -> false.

%% @doc Return the kind of the attribute tag.

-type tag_kind() :: 'type' | 'spec' | 'unknown'.
-spec tag(Tag::atom()) -> tag_kind().

tag(opaque) -> type;
tag(spec) -> spec;
tag(type) -> type;
tag(_) -> unknown.

throw_error(Line, S, A) ->
    edoc_report:error(Line, "", io_lib:format(S, A)),
    throw(error).
