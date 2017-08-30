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
-module(ic_noc).


-export([do_gen/3]).
%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-export([unfold/1, mk_attr_func_names/2]).


-import(ic_util, [mk_name/2, mk_var/1, mk_oe_name/2, to_atom/1, to_list/1]).
-import(ic_forms, [get_id/1, get_id2/1, get_body/1, is_oneway/1]).
-import(ic_codegen, [emit/2, emit/3, nl/1]).
-import(ic_options, [get_opt/2]).


-import(lists, [foreach/2, foldr/3, map/2]).
 

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
    %% Loop through form and adds inheritence data
    ic_pragma:preproc(G2, [], Form),
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
    G2 = ic_file:filename_push(G, N, X, erlang),
    N2 = [get_id2(X) | N],
    gen_head(G2, N2, X),
    gen(G2, N2, get_body(X)),
    foreach(fun({_Name, Body}) -> gen(G2, N2, Body) end, 
	    X#interface.inherit_body),
    gen_serv(G2, N, X), 
    G3 = ic_file:filename_pop(G2, erlang),
    gen(G3, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, const) ->
%    N2 = [get_id2(X) | N],
    emit_constant_func(G, X#const.id, X#const.val),
    gen(G, N, Xs); %% N2 or N?

gen(G, N, [X|Xs]) when is_record(X, op) ->
    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),

    case getNocType(G,X,N) of
	transparent ->
	    emit_transparent_func(G, N, X, Name, ArgNames, TypeList, OutArgs);
	multiple ->
	    mark_not_transparent(G,N),
	    emit_transparent_func(G, N, X, Name, ArgNames, TypeList, OutArgs);
	_XTuple ->
	    mark_not_transparent(G,N),
	    emit_stub_func(G, N, X, Name, ArgNames, TypeList, OutArgs)
    end,

    gen(G, N, Xs);
	

gen(G, N, [X|Xs]) when is_record(X, attr) ->
    emit_attr(G, N, X, fun emit_stub_func/7),
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



%%--------------------------------------------------------------------
%%
%% Generate the server side (handle_call and handle_cast)
%%

gen_serv(G, N, X) ->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    emit_serv_std(G, N, X),
	    N2 = [get_id2(X) | N], 
	    gen_calls(G, N2, get_body(X)),
	    lists:foreach(fun({_Name, Body}) ->
				  gen_calls(G, N2, Body) end,
			  X#interface.inherit_body),
	    get_if_gen(G, N2, X),
	    gen_end_of_call(G, N, X),		% Note N instead of N2

	    gen_casts(G, N2, get_body(X)),
	    lists:foreach(fun({_Name, Body}) ->
				  gen_casts(G, N2, Body) end,
			  X#interface.inherit_body),
	    gen_end_of_cast(G, N, X),		% Note N instead of N2
	    emit_skel_footer(G, N, X);		% Note N instead of N2
	false ->
	    ok
    end.

gen_calls(G, N, [X|Xs]) when is_record(X, op) ->
    case is_oneway(X) of
	false ->
	    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
	    emit_skel_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
	    gen_calls(G, N, Xs);
	true ->
	    gen_calls(G, N, Xs)
    end;

gen_calls(G, N, [X|Xs]) when is_record(X, attr) ->
    emit_attr(G, N, X, fun emit_skel_func/7),
    gen_calls(G, N, Xs);

gen_calls(G, N, [_X|Xs]) -> gen_calls(G, N, Xs);
gen_calls(_G, _N, []) -> ok.

gen_casts(G, N, [X|Xs]) when is_record(X, op) ->
    case is_oneway(X) of
	true ->
	    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
	    emit_skel_func(G, N, X, Name, ArgNames, TypeList, OutArgs),
	    gen_casts(G, N, Xs);
	false ->
	    gen_casts(G, N, Xs)
    end;

gen_casts(G, N, [_X|Xs]) -> gen_casts(G, N, Xs);
gen_casts(_G, _N, []) -> ok.

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
				  F(G, N, X2, Set, [mk_name(G, "Value")], 
				    SetType, [])
			  end end, ic_forms:get_idlist(X)).


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




emit_serv_std(G, N, X) ->
    Fd = ic_genobj:stubfiled(G),
    case transparent(G) of
	true ->
	    true;
	_XTupleORMultiple ->
	    Impl	= getImplMod(G,X,[get_id2(X)|N]),
	    TypeID = ictk:get_IR_ID(G, N, X),
	    
	    nl(Fd), nl(Fd), nl(Fd),
	    ic_codegen:mcomment(Fd, ["Server implementation."]),
	    nl(Fd), nl(Fd),
	    ic_codegen:mcomment(Fd, ["Function for fetching the interface type ID."]),
	    nl(Fd), 
	    emit(Fd, "typeID() ->\n"),
	    emit(Fd, "    \"~s\".\n", [TypeID]),
	    nl(Fd), nl(Fd),
	    ic_codegen:mcomment(Fd, ["Server creation functions."]),
	    nl(Fd), 
	    emit(Fd, "oe_create() ->\n"),
	    emit(Fd, "    start([], []).\n", []),
	    nl(Fd),
	    emit(Fd, "oe_create_link() ->\n"),
	    emit(Fd, "    start_link([], []).\n", []),
	    nl(Fd),
	    emit(Fd, "oe_create(Env) ->\n"),
	    emit(Fd, "    start(Env, []).\n", []),
	    nl(Fd),
	    emit(Fd, "oe_create_link(Env) ->\n"),
	    emit(Fd, "    start_link(Env, []).\n", []),
	    nl(Fd),
	    emit(Fd, "oe_create(Env, RegName) ->\n"),
	    emit(Fd, "    start(RegName, Env, []).\n", []),
	    nl(Fd),
	    emit(Fd, "oe_create_link(Env, RegName) ->\n"),
	    emit(Fd, "    start_link(RegName, Env, []).\n", []),
	    nl(Fd),
	    ic_codegen:mcomment(Fd, ["Start functions."]),
	    nl(Fd), 
	    emit(Fd, "start(Env, Opt) ->\n"),
	    emit(Fd, "    gen_server:start(?MODULE, Env, Opt).\n"),
	    nl(Fd),
	    emit(Fd, "start_link(Env, Opt) ->\n"),
	    emit(Fd, "    gen_server:start_link(?MODULE, Env, Opt).\n"),
	    nl(Fd),
	    emit(Fd, "start(RegName, Env, Opt) ->\n"),
	    emit(Fd, "    gen_server:start(RegName, ?MODULE, Env, Opt).\n"),
	    nl(Fd),
	    emit(Fd, "start_link(RegName, Env, Opt) ->\n"),
	    emit(Fd, "    gen_server:start_link(RegName, ?MODULE, Env, Opt).\n"),
	    nl(Fd),
	    ic_codegen:comment(Fd, "Call to implementation init"),
	    emit(Fd, "init(Env) ->\n"),
	    emit(Fd, "    ~p:~p(Env).\n", [Impl, init]),
	    nl(Fd),
	    emit(Fd, "terminate(Reason, State) ->\n"),
	    emit(Fd, "    ~p:~p(Reason, State).\n", 
		 [Impl, terminate]),
            nl(Fd),
	    emit(Fd, "code_change(_OldVsn, State, _Extra) ->\n"),
	    emit(Fd, "    {ok, State}.\n"),
	    nl(Fd), nl(Fd)
    end,
    Fd.




gen_end_of_call(G, _N, _X) ->
    case transparent(G) of
	true ->
	    true;
	_XTuple ->
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd),
	    ic_codegen:mcomment_light(Fd, ["Standard gen_server call handle"]),
	    emit(Fd, "handle_call(stop, _From, State) ->\n"),
	    emit(Fd, "    {stop, normal, ok, State}"),
	    case get_opt(G, serv_last_call) of
		exception ->
		    emit(Fd, ";\n"),
		    nl(Fd),
		    emit(Fd, "handle_call(_Req, _From, State) ->\n"),
		    emit(Fd, "    {reply, ~p, State}.\n",[getCallErr()]);
		exit ->
		    emit(Fd, ".\n"),
		    nl(Fd),
		    nl(Fd)
	    end
    end,
    ok.


gen_end_of_cast(G, _N, _X) ->
    case transparent(G) of
	true ->
	    true;
	_XTuple ->
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd),
	    ic_codegen:mcomment_light(Fd, ["Standard gen_server cast handle"]),
	    emit(Fd, "handle_cast(stop, State) ->\n"),
	    emit(Fd, "    {stop, normal, State}"),
	    case get_opt(G, serv_last_call) of
		exception ->
		    emit(Fd, ";\n"),
		    nl(Fd),
		    emit(Fd, "handle_cast(_Req, State) ->\n"),
		    emit(Fd, "    {reply, ~p, State}.\n",[getCastErr()]);
		exit ->
		    emit(Fd, ".\n"),
		    nl(Fd), nl(Fd)
	    end
    end,
    ok.


emit_skel_footer(G, N, X) ->
    case transparent(G) of
	true ->
	    true;
	_XTuple ->
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd),
	    ic_codegen:mcomment_light(Fd, ["Standard gen_server handles"]),
	    case use_impl_handle_info(G, N, X) of
		true ->
                    emit(Fd, "handle_info(X, State) ->\n"),
		    emit(Fd, "    ~p:handle_info(X, State).\n\n", 
			 [list_to_atom(ic_genobj:impl(G))]);
		false ->
                    emit(Fd, "handle_info(_X, State) ->\n"),
		    emit(Fd, "    {reply, ~p, State}.\n\n",[getInfoErr()])
	    end
    end,
    ok.


use_impl_handle_info(G, N, X) ->
    FullName = ic_util:to_colon([get_id2(X) | N]),
    case {get_opt(G, {handle_info, true}), get_opt(G, {handle_info, FullName})} of
	{_, force_false} -> false;
	{false, false} -> false;
	_ -> true
    end.


use_timeout(G, N, _X) ->
    FullName = ic_util:to_colon(N),
    case {get_opt(G, {timeout, true}), get_opt(G, {timeout, FullName})} of
	{_, force_false} -> false;
	{false, false} -> false;
	_ -> true
    end.


get_if_name(G) -> mk_oe_name(G, "get_interface").


%% Generates the get_interface function (for Lars)
get_if_gen(G, N, X) ->
    case transparent(G) of
	true ->
	    ok;
	_XTuple ->
	    case ic_genobj:is_stubfile_open(G) of
		true ->
		    IFC_TKS = tk_interface_data(G,N,X),
		    Fd = ic_genobj:stubfiled(G),
		    Name = to_atom(get_if_name(G)),

		    ic_codegen:mcomment_light(Fd, 
					 [io_lib:format("Standard Operation: ~p",
							[Name])]),

		    emit(Fd, "handle_call({_~s, ~p, []}, _From, State) ->~n",
			 [mk_name(G, "Ref"), Name]),
		    emit(Fd, "    {reply, ~p, State};~n", [IFC_TKS]),
		    nl(Fd),
		    ok;

		false -> ok
	    end
    end.


get_if(G,N,[X|Rest]) when is_record(X, op) ->
    R = ic_forms:get_tk(X),
    IN = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		   ic:filter_params([in, inout], X#op.params)),
    OUT = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		    ic:filter_params([out, inout], X#op.params)),
    case print_tk(G,N,X) of
	true ->
	    [{get_id2(X), {R, IN, OUT}} | get_if(G,N,Rest)];
	false ->
	    get_if(G,N,Rest)
    end;

get_if(G,N,[X|Rest]) when is_record(X, attr) -> %% Attributes not handled so far <<<<<<<<<<<<<<<<<<<<<<<<
    {GetT, SetT} = mk_attr_func_types([], X),
    AList = lists:map(fun(Id) -> 
			      {Get, Set} = mk_attr_func_names([], get_id(Id)),
			      case X#attr.readonly of
				  {readonly, _} -> 
				      {Get, GetT};
				  _ -> 
				      [{Set, SetT}, {Get, GetT}]
			      end end, ic_forms:get_idlist(X)),
    lists:flatten(AList) ++ get_if(G,N,Rest);

get_if(G,N,[_X|Rest]) -> get_if(G,N,Rest);
get_if(_,_,[]) -> [].




%%------------------------------------------------------------
%%
%% Export stuff
%%
%%	Gathering of all names that should be exported from a stub
%%	file.
%%


gen_head_special(G, N, X) when is_record(X, interface) ->
    Fd = ic_genobj:stubfiled(G),
    NocType = getNocType(G,X,N),

    foreach(fun({Name, Body}) ->
		    ic_codegen:comment(Fd, "Exports from ~p", 
				  [ic_util:to_colon(Name)]),
		    ic_codegen:export(Fd, exp_top(G, N, Body, NocType, [])),
		    nl(Fd)
	    end, X#interface.inherit_body),

    case transparent(G) of
	true ->
	    nl(Fd), nl(Fd);
	_XTuple ->
	    ic_codegen:comment(Fd, "Type identification function"),
	    ic_codegen:export(Fd, [{typeID, 0}]), 
	    nl(Fd),
	    ic_codegen:comment(Fd, "Used to start server"),
	    ic_codegen:export(Fd, [{start, 2},{start_link, 3}]),
	    ic_codegen:export(Fd, [{oe_create, 0}, {oe_create_link, 0}, {oe_create, 1}, 
			      {oe_create_link, 1},{oe_create, 2}, {oe_create_link, 2}]),
	    nl(Fd),
	    ic_codegen:comment(Fd, "gen server export stuff"),
	    emit(Fd, "-behaviour(gen_server).\n"),
	    ic_codegen:export(Fd, [{init, 1}, {terminate, 2}, {code_change, 3},
                                   {handle_call, 3}, {handle_cast, 2}, {handle_info, 2}]),
	    nl(Fd), nl(Fd),
	    ic_codegen:mcomment(Fd, ["Object interface functions."]),
	    nl(Fd), nl(Fd), nl(Fd)
    end,
    Fd;
	

gen_head_special(_G, _N, _X) -> ok.

    

%% Shall generate all export declarations
gen_head(G, N, X) -> 
    case ic_genobj:is_stubfile_open(G) of
	true -> 
	    F = ic_genobj:stubfiled(G),
	    ic_codegen:comment(F, "Interface functions"),
	    ic_codegen:export(F, exp_top(G, N, X, getNocType(G,X,N), [])), 
	    nl(F),
	    gen_head_special(G, N, X);
	false -> ok
    end.

exp_top(_G, _N, X, _NT, Acc)  when element(1, X) == preproc -> 
    Acc;
exp_top(G, N, L, NT, Acc)  when is_list(L) ->
    exp_list(G, N, L, NT, Acc);
exp_top(G, N, M, NT, Acc)  when is_record(M, module) ->
    exp_list(G, N, get_body(M), NT, Acc);
exp_top(G, N, I, NT, Acc)  when is_record(I, interface) ->
    exp_list(G, N, get_body(I), NT, Acc);
exp_top(G, N, X, NT, Acc) ->
    exp3(G, N, X, NT, Acc).

exp3(_G, _N, C, _NT, Acc)  when is_record(C, const) -> 
    [{get_id(C#const.id), 0} | Acc];

exp3(G, N, Op, NocType, Acc)  when is_record(Op, op) ->
    FuncName = get_id(Op#op.id),
    
    TA = case use_timeout(G,N,Op) of
	     true ->
		 1;
	     false ->
		 0
	 end,

    case NocType of
	transparent ->
	    Arity = length(ic:filter_params([in, inout], Op#op.params)) + TA + 1,
	    [{FuncName, Arity} | Acc];
	multiple ->
	    case getModType(G, Op, N) of
		dt ->
		    Arity = length(ic:filter_params([in, inout], Op#op.params)) + TA + 1,
		    [{FuncName, Arity} | Acc];
		do ->
		    Arity = length(ic:filter_params([in, inout], Op#op.params)) + TA + 1,
		    [{FuncName, Arity} | Acc];
		spt ->
		    Arity = length(ic:filter_params([in, inout], Op#op.params)) + TA + 1,
		    [{FuncName, Arity} | Acc];
		spo ->
		    Arity = length(ic:filter_params([in, inout], Op#op.params)) + TA + 1,
		    [{FuncName, Arity} | Acc]
	    end;
	_ ->
	    Arity = length(ic:filter_params([in, inout], Op#op.params)) + TA + 1,
	    [{FuncName, Arity} | Acc]
    end;
exp3(_G, _N, A, _NT, Acc)  when is_record(A, attr) ->
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> [{Get, 1} | Acc2];
			    _ ->             [{Get, 1}, {Set, 2} | Acc2]
			end end, Acc, ic_forms:get_idlist(A));

exp3(_G, _N, _X, _NT, Acc) -> Acc.

exp_list(G, N, L, NT, OrigAcc) -> 
    lists:foldr(fun(X, Acc) -> exp3(G, N, X, NT, Acc) end, OrigAcc, L).




%%------------------------------------------------------------
%%
%% Emit stuff
%%
%%	Low level generation primitives
%%

emit_stub_func(G, N, X, Name, ArgNames, TypeList, _OutArgs) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = ic_genobj:stubfiled(G),
	    StubName = list_to_atom(Name),
	    This = mk_name(G, "Ref"),
	    XTuple = getNocType(G,X,N),
	    CallOrCast = 
		case is_oneway(X) of 
		    true -> ?CAST; 
		    _ -> ?CALL
		end,

	    %% Type expand operation on comments
	    ic_code:type_expand_op(G,N,X,Fd),

	    case use_timeout(G,N,X) of
		true ->
		    Timeout = mk_name(G,"Timeout"),
		    emit(Fd, "~p(~s) ->\n", 
			 [StubName, mk_list([This, Timeout| ArgNames])]),
		    emit(Fd, "    ~p:~s(~s, ~s, ?MODULE, ~p, ~p, [~s], ~p).\n\n", 
			 [getImplMod(G,X,N), 
			  CallOrCast, 
			  This, 
			  Timeout, 
			  XTuple, 
			  StubName, 
			  mk_list(ArgNames), 
			  tk_operation_data(G, N, X, TypeList)]);
		false ->
		    emit(Fd, "~p(~s) ->\n", 
			 [StubName, mk_list([This | ArgNames])]),
		    
		    emit(Fd, "    ~p:~s(~s, ~p, ?MODULE, ~p, [~s], ~p).\n\n", 
			 [getImplMod(G,X,N), 
			  CallOrCast, 
			  This, 
			  XTuple, 
			  StubName, 
			  mk_list(ArgNames), 
			  tk_operation_data(G, N, X, TypeList)])
	    end
    end.


emit_transparent_func(G, N, X, Name, ArgNames, _TypeList, _OutArgs) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> ok;
	true ->
	    Fd = ic_genobj:stubfiled(G),
	    OpName = list_to_atom(Name),
	    
	    ArgList = case use_timeout(G,N,X) of
			  true ->
			      mk_list([mk_name(G,"Ref"),mk_name(G,"Timeout")|ArgNames]);
			  false ->
			       mk_list([mk_name(G,"Ref")|ArgNames])
		      end,

	    %% Type expand operation on comments
	    ic_code:type_expand_op(G,N,X,Fd),

	    emit(Fd, "~p(~s) ->\n", [OpName,ArgList]),
	    emit(Fd, "    ~p:~s(~s).\n\n", [getImplMod(G,X,N), OpName, ArgList])
    end.






emit_skel_func(G, N, X, OpName, ArgNames, _TypeList, _OutArgs) ->
    case getNocType(G,X,N) of
	transparent ->
	    true;
	multiple ->
	    true;
	XTuple ->
	    case ic_genobj:is_stubfile_open(G) of
		false -> ok;
		true ->
		    Fd = ic_genobj:stubfiled(G),
		    Name	= list_to_atom(OpName),
		    This	= mk_name(G, "Ref"),
		    From	= mk_name(G, "From"),
		    State	= mk_name(G, "State"),
		    
		    %% Type expand handle operation on comments
		    ic_code:type_expand_handle_op(G,N,X,Fd),

		    case is_oneway(X) of
			true ->
			    emit(Fd, "handle_cast({~s, ~p, OE_Module, ~p, [~s]}, ~s) ->\n",
				 [This, XTuple, Name, mk_list(ArgNames), State]),
			    emit(Fd, "    ~p:handle_cast({~s, ~p, OE_Module, ~p, [~s]}, ~s);\n\n", 
				 [getImplMod(G,X,N), This, XTuple, Name, mk_list(ArgNames), State]);
			false ->
			    emit(Fd, "handle_call({~s, ~p, OE_Module, ~p, [~s]}, ~s, ~s) ->\n",
				 [This, XTuple, Name, mk_list(ArgNames), From, State]),
			    emit(Fd, "    ~p:handle_call({~s, ~p, OE_Module, ~p, [~s]}, ~s, ~s);\n\n", 
				 [getImplMod(G,X,N), This, XTuple, Name, mk_list(ArgNames), From, State])
		    end
	    end
    end.



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


%% Unfold identifier lists or nested lists. Note that many records
%% contain an entry named id that is a list before unfold and a single
%% id afterwards.
unfold(L) when is_list(L) ->
    lists:flatten(map(fun(X) -> unfold2(X) end, L));
unfold(X) -> unfold2(X).
    
unfold2(A) when is_record(A, attr) ->
    map(fun(Id) -> A#attr{id=Id} end, A#attr.id);
unfold2(M) when is_record(M, member) ->
    map(fun(Id) -> M#member{id=Id} end, M#member.id);
unfold2(M) when is_record(M, case_dcl) ->
    map(fun(Id) -> M#case_dcl{label=Id} end, M#case_dcl.label);
unfold2(T) when is_record(T, typedef) ->
    map(fun(Id) -> T#typedef{id=Id} end, T#typedef.id   ).






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





%%%%%%


getImplMod(G,X,Scope) -> %% to_atom(ic_genobj:impl(G)) | ChoicedModuleName

    %% Get actual pragma appliance scope
    SpecScope = getActualScope(G,X,Scope),

    %% The "broker" option is passed 
    %% only by pragmas, seek for module.
    case ic_pragma:getBrokerData(G,X,SpecScope) of
	{Module,_Type} ->
	    Module;
	_List ->
	    element(1,ic_pragma:defaultBrokerData(G))
    end.


getNocType(G,X,Scope) when is_record(X, interface) -> %% default | specified 
    OpList = getAllOperationScopes(G,Scope),
    getNocType2(G,X,OpList);
getNocType(G,X,Scope) -> %% transparent | {extraarg1,....,extraargN}
    getNocType3(G,X,Scope).

getNocType2(G,X,List) ->
    getNocType2(G,X,List,[]).

getNocType2(_,_,[],Found) ->
    selectTypeFromList(Found);
getNocType2(G,X,[OpScope|OpScopes],Found) ->
    getNocType2(G,X,OpScopes,[getNocType3(G,X,OpScope)|Found]).

getNocType3(G,X,Scope) -> %% transparent | {extraarg1,....,extraargN}

    %% Get actual pragma appliance scope
    SpecScope = getActualScope(G,X,Scope),

    %% The "broker" option is passed 
    %% only by pragmas, seek for type.
    case ic_pragma:getBrokerData(G,X,SpecScope) of
	{_Module,Type} ->
	    Type;
	List ->
	    selectTypeFromList(List) %%transparent/multiple
    end.


getModType(G,X,Scope) -> %% default | specified 

    %% Get actual pragma appliance scope
    SpecScope = getActualScope(G,X,Scope),

    %% The "broker" option is passed 
    %% only by pragmas, seek for brokerdata.
    case ic_pragma:getBrokerData(G,X,SpecScope) of
	{Module,Type} ->
	    case Module == ic_genobj:impl(G) of
		true ->
		    case Type of 
			transparent ->
			    dt; %% default + transparent
			_ ->
			    do  %% default + opaque
		    end;
		false ->
		    case Type of 
			transparent ->
			    spt; %% specified + transparent
			_ ->
			    spo  %% specified + opaque
		    end
	    end;
	_List ->
	    dt
    end.



%%%%
%%
%% Returns a list of ALL operation full 
%% scoped names local and inherited
%% from other interfaces
%%

getAllOperationScopes(G,Scope) ->
    getOperationScopes(G,Scope) ++ 
	getInhOperationScopes(G,Scope).
	

getOperationScopes(G,Scope) ->
    getOpScopes(G,
		Scope,
		ets:match(ic_genobj:pragmatab(G),{op,'$0',Scope,'_','_'}),
		[]).

getOpScopes(_,_,[],OpScopes) ->
    OpScopes;
getOpScopes(G,Scope,[[Name]|Names],Found) ->
    getOpScopes(G,Scope,Names,[[Name|Scope]|Found]).


getInhOperationScopes(G,Scope) ->
    getInhOpScopes1(G,
		   Scope,
		   ets:match(ic_genobj:pragmatab(G),{inherits,Scope,'$1'}),
		   []).

getInhOpScopes1(G,_Scope,[],OpScopes) ->
    getInhOpScopes2(G,OpScopes);
getInhOpScopes1(G,Scope,[[SC]|SCs],Found) ->
    getInhOpScopes1(G,Scope,SCs,[SC|Found]).


getInhOpScopes2(G,Scopes) ->
    getInhOpScopes2(G,Scopes,[]).

getInhOpScopes2(_G,[],Found) ->
    Found;
getInhOpScopes2(G,[SC|SCs],Found) ->
   getOperationScopes(G,SC) ++ getInhOpScopes2(G,SCs,Found).

%%
%%
%%%%



%%%%
%%
%%
%% Seek the actual operation scope :
%%
%%   * if the operation is inherited, get the real scope for it
%%
%%   * if the operation has a specific pragma, apply the real
%%     scope, otherwise return the including scope 
%%
getActualScope(G, X, Scope) when is_record(X, op) ->
    OpScope = getRealOpScope(G,X,Scope),
    case ets:match(ic_genobj:pragmatab(G),{codeopt_specific,OpScope}) of
	[[]] ->
	    OpScope;
	_ ->
	    Scope
    end;
getActualScope(_G, _X, N) ->
    N.

%%
%%  Just seek and return the scope for the operation
%%  where it were originaly defined
%%
getRealOpScope(G,X,N) when is_record(X, op) ->
    Ptab = ic_genobj:pragmatab(G),
    Id = get_id2(X),
    
    case ets:match(Ptab,{op,Id,N,'_','_'}) of
	[[]] ->
	    [Id|N];
	_ ->
	    getRealOpScope(G, Ptab, X, N, Id,  ets:match(Ptab,{inherits,N,'$1'}))
    end;
getRealOpScope(_G,_X,N) ->
    N.

getRealOpScope(_G, _S, _X, N, Id, []) ->
    [Id|N];
getRealOpScope(G, S, X, N, Id, [[OS]|OSs]) -> 
    case ets:match(S,{op,Id,OS,'_','_'}) of
	[[]] ->
	    [Id|OS];
	_ ->
	    getRealOpScope(G, S, X, N, Id, OSs)
    end.
    
selectTypeFromList([]) ->
    transparent;
selectTypeFromList([{_,transparent}|Rest]) ->
    selectTypeFromList(Rest);
selectTypeFromList([transparent|Rest]) ->
    selectTypeFromList(Rest);
selectTypeFromList([_|_Rest]) ->
    multiple.



getCallErr() ->    
    {'ERROR' ,"Bad Operation -- handle call"}.

getCastErr() ->
    {'ERROR' ,"Bad Operation -- handle cast"}.

getInfoErr() ->
    {'ERROR' ,"Bad Operation -- handle info"}.






%%
%% Type code access utilities
%%

tk_operation_data(G, N, X, TL) ->
    case print_tk(G,N,X) of
	true ->
	    TL;
	false ->
	    no_tk
    end.

tk_interface_data(G, N, X) ->
    InfoList = 
	foldr(fun({_Name, Body}, Acc) ->
		      get_if(G,N,Body)++Acc end,
	      get_if(G,N,get_body(X)),
	      X#interface.inherit_body),
    case InfoList of
	[] ->
	    no_tk;  %%%%%%%% Should be changed to [] <<<<<<<<<<<<<<<<<<<<<<<<<<< Warning !
	_ ->
	    InfoList
    end.


print_tk(G, N, X) when is_record(X, op)-> %% operation
    case getNocType(G,X,N) of
	transparent ->
	    false;
	multiple ->
	    false;
	_XTuple -> %%check if there are any USETK pragmas
	    operation_usetk(G,N,X)
    end; 
print_tk(_G, _N, _X) -> %% error
    false.


operation_usetk(G,N,X) ->
    PTab = ic_genobj:pragmatab(G),
    OTab = ic_genobj:optiontab(G),
    OpName = get_id2(X),
%    SID = ic_util:to_colon(N),
    Res = case use_tk(OTab,[N]) of
	      {ok,N} ->
		  true;
	      false ->
		  %% Look if there is an operation with that name
                  %% which can be found in an included file.
		  case ets:match(PTab,{file_data_included,'_','_',op,'$3',OpName,'_','_','_'}) of
		      [] ->
			  false;
		      ScopeList ->
			  case use_tk(OTab,ScopeList) of
			      %% There is an operation with that name, 
			      %% look if it is inherited by interface "N"
			      {ok,FoundScope} ->
				  ic_pragma:is_inherited_by(FoundScope,N,PTab);
			      false ->
				  false
			  end
		  end
	  end,
    Res. 


use_tk(_,[]) ->
    false;
use_tk(OTab,[[Scope]|Scopes]) ->
    SID = ic_util:to_colon(Scope),
    case ets:match(OTab,{{option,{use_tk,SID}},true}) of
	[] ->
	    case ets:match(OTab,{{option,{use_tk,"::"++SID}},true}) of
		[] ->
		    use_tk(OTab,Scopes);
		_ ->
		    {ok,Scope}
	    end;
	_ ->
	    {ok,Scope}
    end;
use_tk(OTab,[Scope|Scopes]) ->
    SID = ic_util:to_colon(Scope),
    case ets:match(OTab,{{option,{use_tk,SID}},true}) of
	[] ->
	    case ets:match(OTab,{{option,{use_tk,"::"++SID}},true}) of
		[] ->
		    use_tk(OTab,Scopes);
		_ ->
		    {ok,Scope}
	    end;
	_ ->
	    {ok,Scope}
    end.





mark_not_transparent(G,N) ->

    %% Mark that there are multiple 
    %% functions in interface 
    S = ic_genobj:pragmatab(G),
    ets:insert(S,{no_transparent,N}).


transparent(G) ->
    
    S = ic_genobj:pragmatab(G),
    case ets:match_object(S,{no_transparent,'$0'}) of
	[] ->
	    true;
	_ ->
	    false
    end.

