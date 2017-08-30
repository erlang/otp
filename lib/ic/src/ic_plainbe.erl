%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%
-module(ic_plainbe).


-export([do_gen/3]).
%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-import(ic_util, [mk_var/1, mk_oe_name/2, to_atom/1, to_list/1]).
-import(ic_forms, [get_id/1, get_id2/1, get_body/1]).
-import(ic_codegen, [emit/3, nl/1]).

-import(lists, [foreach/2, map/2]).

-include("icforms.hrl").
-include("ic.hrl").

%%------------------------------------------------------------
%%
%% Generate the client side Erlang stubs.
%%
%% Each module is generated to a separate file.
%%
%% Export declarations for all interface functions must be
%% generated. Each function then needs to generate a function head and
%% a body. IDL parameters must be converted into Erlang parameters
%% (variables, capitalised) and a type signature list must be
%% generated (for later encode/decode).
%%
%%------------------------------------------------------------ 


do_gen(G, File, Form) -> 
    G2 = ic_file:filename_push(G, [], mk_oe_name(G, 
					       ic_file:remove_ext(to_list(File))),
			     erlang),
    gen_head(G2, [], Form),
    exportDependency(G2),
    gen(G2, [], Form),
    genDependency(G2),
    ic_file:filename_pop(G2, erlang),
    ok.


gen(G, N, [X|Xs]) when is_record(X, preproc) ->
    NewG = ic:handle_preproc(G, N, X#preproc.cat, X),
    gen(NewG, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, module) ->
    CD = ic_code:codeDirective(G,X),
    G2 = ic_file:filename_push(G, N, X, CD),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    G3 = ic_file:filename_pop(G2, CD),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, interface) ->
    %% Add inheritence data to pragmatab
    ic_pragma:add_inh_data(G,N,X),
    G2 = ic_file:filename_push(G, N, X, erlang),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    foreach(fun({_Name, Body}) -> gen(G2, N2, Body) end, 
	    X#interface.inherit_body), 
    G3 = ic_file:filename_pop(G2, erlang),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, const) ->
%    N2 = [get_id2(X) | N],
    emit_constant_func(G, X#const.id, X#const.val),
    gen(G, N, Xs); %% N or N2?

gen(G, N, [X|Xs]) when is_record(X, op) ->
    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
    emit_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
    gen(G, N, Xs);
	

gen(G, N, [X|Xs]) when is_record(X, attr) ->
    emit_attr(G, N, X, fun emit_func/7),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, except) ->
    icstruct:except_gen(G, N, X, erlang),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) ->
    case may_contain_structs(X) of
	true -> icstruct:struct_gen(G, N, X, erlang);
	false -> ok
    end,
    gen(G, N, Xs);

gen(_G, _N, []) -> ok.


may_contain_structs(X) when is_record(X, typedef) -> true;
may_contain_structs(X) when is_record(X, struct) -> true;
may_contain_structs(X) when is_record(X, union) -> true;
may_contain_structs(_X) -> false.


%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%


gen_head_special(G, N, X) when is_record(X, interface) ->
    Fd = ic_genobj:stubfiled(G),

    foreach(fun({Name, Body}) ->
		    ic_codegen:comment(Fd, "Exports from ~p", 
				  [ic_util:to_colon(Name)]),
		    ic_codegen:export(Fd, exp_top(G, N, Body, [])),
		    nl(Fd)
	    end, X#interface.inherit_body),
    Fd;
gen_head_special(_G, _N, _X) -> ok.

    

%% Shall generate all export declarations
gen_head(G, N, X) -> 
    case ic_genobj:is_stubfile_open(G) of
	true -> 
	    F = ic_genobj:stubfiled(G),
	    ic_codegen:comment(F, "Interface functions"),
	    ic_codegen:export(F, exp_top(G, N, X, [])), 
	    nl(F),
	    gen_head_special(G, N, X);
	false -> ok
    end.

exp_top(_G, _N, X, Acc)  when element(1, X) == preproc -> 
    Acc;
exp_top(G, N, L, Acc)  when is_list(L) ->
    exp_list(G, N, L, Acc);
exp_top(G, N, M, Acc)  when is_record(M, module) ->
    exp_list(G, N, get_body(M), Acc);
exp_top(G, N, I, Acc)  when is_record(I, interface) ->
    exp_list(G, N, get_body(I), Acc);
exp_top(G, N, X, Acc) ->
    exp3(G, N, X, Acc).

exp3(_G, _N, C, Acc)  when is_record(C, const) -> 
    [{get_id(C#const.id), 0} | Acc];

exp3(_G, _N, Op, Acc)  when is_record(Op, op) ->
    FuncName = get_id(Op#op.id),
    Arity = length(ic:filter_params([in, inout], Op#op.params)),
    [{FuncName, Arity} | Acc];

exp3(_G, _N, A, Acc)  when is_record(A, attr) ->
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> [{Get, 1} | Acc2];
			    _ ->             [{Get, 1}, {Set, 2} | Acc2]
			end end, Acc, ic_forms:get_idlist(A));

exp3(_G, _N, _X, Acc) -> Acc.

exp_list(G, N, L, OrigAcc) -> 
    lists:foldr(fun(X, Acc) -> exp3(G, N, X, Acc) end, OrigAcc, L).




%%------------------------------------------------------------
%%
%% Emit stuff
%%
%%	Low level generation primitives
%%


emit_func(G, _N, X, Name, ArgNames, _TypeList, OutArgs) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = ic_genobj:stubfiled(G),
	    OpName = list_to_atom(Name),
	    ArgList = mk_list(ArgNames),
	    emit_op_comment(G, Fd, X, OpName, ArgNames, OutArgs),
	    emit(Fd, "~p(~s) ->\n", [OpName,ArgList]),
	    emit(Fd, "    ~p:~p(~s).\n\n", [to_atom(ic_genobj:impl(G)), OpName, ArgList])
    end.

emit_attr(G, N, X, F) ->
    XX = #id_of{type=X},
    {GetType, SetType} = mk_attr_func_types(N, X),
    lists:foreach(fun(Id) ->
			  X2 = XX#id_of{id=Id},
			  {Get, Set} = mk_attr_func_names(N, get_id(Id)),
			  F(G, N, X2, Get, [], GetType, []),
			  case X#attr.readonly of
			      {readonly, _} -> ok;
			      _ -> 
				  F(G, N, X2, Set, [ic_util:mk_name(G, "Value")], 
				    SetType, [])
			  end end, ic_forms:get_idlist(X)).

emit_constant_func(G, Id, Val) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = ic_genobj:stubfiled(G),
	    N = list_to_atom(get_id(Id)),
	    emit_const_comment(G, Fd, Id, N),
	    emit(Fd, "~p() -> ~p.\n\n", [N, Val])
    end.


emit_const_comment(_G, F, _X, Name) ->
    ic_codegen:mcomment_light(F,
			 [io_lib:format("Constant: ~p", [Name])]).


emit_op_comment(G, F, X, Name, InP, OutP) ->
    ic_codegen:mcomment_light(F,
			 [io_lib:format("~s: ~p", [get_title(X), Name]),
			  "",
			  get_returns(G, X, InP, OutP) |
			  get_raises(X)]).

get_title(X) when is_record(X, attr) -> "Attribute Operation";
get_title(_X) -> "Operation".

get_raises(X) when is_record(X, op) ->
    if  X#op.raises == [] -> [];
	true ->
	    ["  Raises:  " ++ 
	     mk_list(lists:map(fun(E) -> ic_util:to_colon(E) end, X#op.raises))]
    end;
get_raises(_X) -> [].

get_returns(_G, _X, _InP, []) ->
    "  Returns: RetVal";
get_returns(G, _X, _InP, OutP) ->
    "  Returns: "++mk_list(["RetVal" | mk_erl_vars(G, OutP)]).




%%------------------------------------------------------------
%%
%% Utilities
%%
%% Convenient little go-get functions
%%
%%------------------------------------------------------------

%% The automaticly generated get and set operation names for an
%% attribute.
mk_attr_func_names(_Scope, Name) ->
    {"_get_" ++ Name, "_set_" ++ Name}.

%% Returns TK of the Get and Set attribute functions.
mk_attr_func_types(_N, X) ->
    TK = ic_forms:get_tk(X),
    {{TK, [], []}, {tk_void, [TK], []}}.
        


%%------------------------------------------------------------
%%
%% Generation utilities and common stuff
%%
%% Convenient stuff for generation
%%
%%------------------------------------------------------------


%% Input is a list of parameters (in parse form) and output is a list
%% of capitalised variable names. mk_var is in icgen
mk_erl_vars(_G, Params) ->
    map(fun(P) -> mk_var(get_id(P#param.id)) end, Params).


%% mk_list produces a nice comma separated string of variable names
mk_list([]) -> [];
mk_list([Arg | Args]) ->
    Arg ++ mk_list2(Args).
mk_list2([Arg | Args]) ->
    ", " ++ Arg ++ mk_list2(Args);
mk_list2([]) -> [].


%%------------------------------------------------------------
%%
%% Parser utilities
%%
%% Called from the yecc parser. Expands the identifier list of an
%% attribute so that the attribute generator never has to handle
%% lists.
%%
%%------------------------------------------------------------




%% Export code produce for dependency function
exportDependency(G) ->
    Fd = ic_genobj:stubfiled(G),
    ic_codegen:export(Fd, [{oe_dependency, 0}]),
    nl(Fd).

%% Code produce for dependency function
genDependency(G) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd),nl(Fd),
    ic_codegen:comment(Fd, "Idl file dependency list function"), 
    emit(Fd, "oe_dependency() ->\n", []),
    emit(Fd, "    ~p.\n\n", [ic_pragma:get_dependencies(G)]).   




extract_info(G, _N, X) when is_record(X, op) ->
    Name	= get_id2(X),
    InArgs	= ic:filter_params([in,inout], X#op.params),
    OutArgs	= ic:filter_params([out,inout], X#op.params),
    ArgNames	= mk_erl_vars(G, InArgs),
    TypeList	= {ic_forms:get_tk(X),
		   map(fun(Y) -> ic_forms:get_tk(Y) end, InArgs),
		   map(fun(Y) -> ic_forms:get_tk(Y) end, OutArgs)
		  },
    {Name, ArgNames, TypeList, OutArgs}.
