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
-module(ic_erlbe).


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

-include_lib("stdlib/include/erl_compile.hrl").


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
    GT = get_opt(G, be),
    G2 = ic_file:filename_push(G, [], mk_oe_name(G, 
					       ic_file:remove_ext(to_list(File))),
			     erlang),
    Light = ic_options:get_opt(G, light_ifr),
    R = if
	    GT == erl_corba, Light == false ->
		case ic_genobj:is_stubfile_open(G2) of
		    true ->
			emit(ic_genobj:stubfiled(G2), "-include_lib(\"~s/include/~s\").\n\n", 
			     [?ORBNAME, ?IFRTYPESHRL]);
		    false -> ok
		end,
		gen_head(G2, [], Form),
		ic_codegen:export(ic_genobj:stubfiled(G2), 
				  [{ictk:register_name(G2), 0},
				   {ictk:unregister_name(G2), 0},
				   {oe_get_module,5}, 
				   {oe_dependency,0}]),
		R0= gen(G2, [], Form),
		ictk:reg_gen(G2, [], Form),
		ictk:unreg_gen(G2, [], Form), % "new" unreg_gen/3
		genDependency(G2), % creates code for dependency list
		R0;
	    GT == erl_corba, Light == true ->
		case ic_genobj:is_stubfile_open(G2) of
		    true ->
			emit(ic_genobj:stubfiled(G2), "-include_lib(\"~s/include/~s\").\n\n", 
			     [?ORBNAME, ?IFRTYPESHRL]);
		    false -> ok
		end,
		gen_head(G2, [], Form),
		ic_codegen:export(ic_genobj:stubfiled(G2), 
				  [{ictk:register_name(G2), 0},
				   {ictk:register_name(G2), 1},
				   {ictk:unregister_name(G2), 0},
				   {ictk:unregister_name(G2), 1}]),
		R0= gen(G2, [], Form),
		ictk:reg_gen(G2, [], Form),
		ictk:unreg_gen(G2, [], Form), % "new" unreg_gen/3
		R0;
	    true ->
		gen_head(G2, [], Form),
		gen(G2, [], Form)
	end,
    ic_file:filename_pop(G2, erlang),
    R.


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
    emit_stub_func(G, N, X, Name, ArgNames, TypeList, OutArgs,
		   is_oneway(X), get_opt(G, be)),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, attr) ->
    emit_attr(G, N, X, fun emit_stub_func/9),
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
	    GT = get_opt(G, be),
	    gen_oe_is_a(G, N, X, GT),
	    N2 = [get_id2(X) | N], 
	    gen_oe_tc(G, N2, X, GT),

	    emit_serv_std(GT, G, N, X),

	    gen_calls(G, N2, get_body(X)),
	    lists:foreach(fun({_Name, Body}) ->
				  gen_calls(G, N2, Body) end,
			  X#interface.inherit_body),
	    gen_end_of_call(GT, G),
	    
	    gen_casts(G, N2, get_body(X)),
	    lists:foreach(fun({_Name, Body}) ->
				  gen_casts(G, N2, Body) end,
			  X#interface.inherit_body),
	    gen_end_of_cast(GT, G),
	    emit_skel_footer(GT, G, N, X); % Note N instead of N2
	false ->
	    ok
    end.

gen_oe_is_a(G, N, X, erl_corba) when is_record(X, interface) ->
    Fd = ic_genobj:stubfiled(G),
    ic_codegen:mcomment(Fd, ["Inherited Interfaces"]),
    emit(Fd, "oe_is_a(~p) -> true;\n", [ictk:get_IR_ID(G, N, X)]),
    lists:foreach(fun(ScopedName) ->
			  emit(Fd, "oe_is_a(~p) -> true;\n", 
			       [ic_pragma:scope2id(G, ScopedName)])
		  end, X#interface.inherit),
    emit(Fd, "oe_is_a(_) -> false.\n"),
    nl(Fd),
    ok;
gen_oe_is_a(_G, _N, _X, _BE) -> ok.


%% Generates the oe_tc function 
gen_oe_tc(G, N, X, erl_corba) ->
    Fd = ic_genobj:stubfiled(G),
    ic_codegen:mcomment(Fd, ["Interface TypeCode"]),
    LocalInterface = gen_oe_tc2(G, N, get_body(X), Fd, []),
    CompleteInterface = 
	lists:foldl(fun({Name, Body}, FunAcc) ->
			    AName = ic_util:to_atom(ic_util:to_undersc(Name)),
			    gen_oe_tc3(G, AName, Body, Fd, FunAcc)
		    end, LocalInterface, X#interface.inherit_body),
    emit(Fd, "oe_tc(_) -> undefined.\n"),
    nl(Fd),
    emit(Fd, "oe_get_interface() -> \n\t["),
    emit_oe_get_interface(Fd, CompleteInterface),
    nl(Fd),
    ok;
gen_oe_tc(_, _, _, _) ->
    ok.

emit_oe_get_interface(Fd, []) ->
    emit(Fd, "].\n");
emit_oe_get_interface(Fd, [Item]) ->
    emit(Fd, "~s].\n", [lists:flatten(Item)]);
emit_oe_get_interface(Fd, [H|T]) ->
    emit(Fd, "~s,\n\t", [lists:flatten(H)]),
    emit_oe_get_interface(Fd, T).

gen_oe_tc2(_,_,[],_, Acc) -> 
    Acc;
gen_oe_tc2(G, N, [X|Rest], Fd, Acc) when is_record(X, op) ->
    R = ic_forms:get_tk(X),
    IN = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		   ic:filter_params([in, inout], X#op.params)),
    OUT = lists:map(fun(P) -> ic_forms:get_tk(P) end,
		    ic:filter_params([out, inout], X#op.params)),
    Function = get_id2(X),
    FunctionAtom = ic_util:to_atom(Function),
    emit(Fd, "oe_tc(~p) -> \n\t~p;\n",[FunctionAtom, {R, IN, OUT}]),
    GI = io_lib:format("{~p, oe_tc(~p)}",[Function, FunctionAtom]),
    gen_oe_tc2(G, N, Rest, Fd, [GI|Acc]);

gen_oe_tc2(G, N, [X|Rest], Fd, Acc) when is_record(X, attr) ->
    {GetT, SetT} = mk_attr_func_types([], X),
    NewAcc = 
	lists:foldl(fun(Id, FunAcc) -> 
			    {Get, Set} = mk_attr_func_names([], get_id(Id)),
			    GetAttrAtom = ic_util:to_atom(Get),
			    emit(Fd, "oe_tc(~p) -> \n\t~p;\n",
				 [GetAttrAtom, GetT]),
			    case X#attr.readonly of
				{readonly, _} ->
				    GI = io_lib:format("{~p, oe_tc(~p)}",
						       [Get, GetAttrAtom]),
				    [GI|FunAcc];
				_ -> 
				    SetAttrAtom = ic_util:to_atom(Set),
				    
				    emit(Fd, "oe_tc(~p) -> \n\t~p;\n",
					 [SetAttrAtom, SetT]),
				    GetGI = io_lib:format("{~p, oe_tc(~p)}", 
						       [Get, GetAttrAtom]),
				    SetGI = io_lib:format("{~p, oe_tc(~p)}", 
						       [Set, SetAttrAtom]),
				    [GetGI, SetGI|FunAcc]
			    end 
		    end, Acc, ic_forms:get_idlist(X)),
    gen_oe_tc2(G, N, Rest, Fd, NewAcc);

gen_oe_tc2(G,N,[_X|Rest], Fd, Acc) -> 
    gen_oe_tc2(G,N,Rest, Fd, Acc).

                  
gen_oe_tc3(_,_,[],_, Acc) -> 
    Acc;
gen_oe_tc3(G, N, [X|Rest], Fd, Acc) when is_record(X, op) ->
    Function = get_id2(X),
    FunctionAtom = ic_util:to_atom(get_id2(X)),
    GI = io_lib:format("{~p, ~p:oe_tc(~p)}",[Function, N, FunctionAtom]),
    emit(Fd, "oe_tc(~p) -> ~p:oe_tc(~p);\n",
	 [FunctionAtom, N, FunctionAtom]),
    gen_oe_tc3(G, N, Rest, Fd, [GI|Acc]);

gen_oe_tc3(G, N, [X|Rest], Fd, Acc) when is_record(X, attr) ->
    NewAcc = lists:foldl(fun(Id, FunAcc) -> 
				 {Get, Set} = mk_attr_func_names([], get_id(Id)),
				 GetAttrAtom = ic_util:to_atom(Get),
				 emit(Fd, "oe_tc(~p) -> ~p:oe_tc(~p);\n",
				      [GetAttrAtom, N, GetAttrAtom]),
				 case X#attr.readonly of
				     {readonly, _} ->
					 [io_lib:format("{~p, ~p:oe_tc(~p)}",
							[Get, N, GetAttrAtom])|FunAcc];
				     _ -> 
					 SetAttrAtom = ic_util:to_atom(Set),
					 emit(Fd, "oe_tc(~p) -> ~p:oe_tc(~p);\n",
					      [SetAttrAtom, N, SetAttrAtom]),
					 [io_lib:format("{~p, ~p:oe_tc(~p)}",
							[Get, N, GetAttrAtom]),
					  io_lib:format("{~p, ~p:oe_tc(~p)}",
							[Set, N, SetAttrAtom])|FunAcc]
				 end 
			 end, Acc, ic_forms:get_idlist(X)),
    gen_oe_tc3(G, N, Rest, Fd, NewAcc);

gen_oe_tc3(G,N,[_X|Rest], Fd, Acc) -> 
    gen_oe_tc3(G,N,Rest, Fd, Acc).

gen_calls(G, N, [X|Xs]) when is_record(X, op) ->
    case is_oneway(X) of
	false ->
	    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
	    emit_skel_func(G, N, X, Name, ArgNames, TypeList, OutArgs, false,
			   get_opt(G, be)),
	    gen_calls(G, N, Xs);
	true ->
	    gen_calls(G, N, Xs)
    end;

gen_calls(G, N, [X|Xs]) when is_record(X, attr) ->
    emit_attr(G, N, X, fun emit_skel_func/9),
    gen_calls(G, N, Xs);

gen_calls(G, N, [_X|Xs]) -> gen_calls(G, N, Xs);
gen_calls(_G, _N, []) -> ok.

gen_casts(G, N, [X|Xs]) when is_record(X, op) ->
    case is_oneway(X) of
	true ->
	    {Name, ArgNames, TypeList, OutArgs} = extract_info(G, N, X),
	    emit_skel_func(G, N, X, Name, ArgNames, TypeList, OutArgs, true,
			   get_opt(G, be)),
	    gen_casts(G, N, Xs);
	false ->
	    gen_casts(G, N, Xs)
    end;

gen_casts(G, N, [_X|Xs]) -> gen_casts(G, N, Xs);
gen_casts(_G, _N, []) -> ok.

emit_attr(G, N, X, F) ->
    XX = #id_of{type=X},
    BE = get_opt(G, be),
    {GetType, SetType} = mk_attr_func_types(N, X),
    lists:foreach(fun(Id) ->
			  X2 = XX#id_of{id=Id},
			  {Get, Set} = mk_attr_func_names(N, get_id(Id)),
			  F(G, N, X2, Get, [], GetType, [],
			    is_oneway(X2), BE),
			  case X#attr.readonly of
			      {readonly, _} -> ok;
			      _ -> 
				  F(G, N, X2, Set, [mk_name(G, "Value")], 
				    SetType, [],
				    is_oneway(X2), BE)
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



%% This function generates the standard functions of an object
%% gen_server
emit_serv_std(erl_corba, G, N, X) ->
    Fd		= ic_genobj:stubfiled(G),
    Impl	= ic_genobj:impl(G),
    TypeID = ictk:get_IR_ID(G, N, X),

    nl(Fd), nl(Fd), nl(Fd),
    ic_codegen:mcomment(Fd, ["Object server implementation."]),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment(Fd, ["Function for fetching the interface type ID."]),
    nl(Fd), 
    emit(Fd, "typeID() ->\n"),
    emit(Fd, "    \"~s\".\n", [TypeID]),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment(Fd, ["Object creation functions."]),
    nl(Fd), 
    emit(Fd, "oe_create() ->\n"),
    emit(Fd, "    corba:create(?MODULE, \"~s\").\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create_link() ->\n"),
    emit(Fd, "    corba:create_link(?MODULE, \"~s\").\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create(Env) ->\n"),
    emit(Fd, "    corba:create(?MODULE, \"~s\", Env).\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create_link(Env) ->\n"),
    emit(Fd, "    corba:create_link(?MODULE, \"~s\", Env).\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create(Env, RegName) ->\n"),
    emit(Fd, "    corba:create(?MODULE, \"~s\", Env, RegName).\n", [TypeID]),
    nl(Fd),
    emit(Fd, "oe_create_link(Env, RegName) ->\n"),
    emit(Fd, "    corba:create_link(?MODULE, \"~s\", Env, RegName).\n", [TypeID]),
    nl(Fd),
    ic_codegen:mcomment(Fd, ["Init & terminate functions."]),
    nl(Fd), 
    emit(Fd, "init(Env) ->\n"),
    ic_codegen:comment(Fd, "Call to implementation init"),
    emit(Fd, "    corba:handle_init(~p, Env).\n", [to_atom(Impl)]),
    nl(Fd),
    emit(Fd, "terminate(Reason, State) ->\n"),
    emit(Fd, "    corba:handle_terminate(~p, Reason, State).\n", 
	 [to_atom(Impl)]),
    nl(Fd), nl(Fd),
    Fd;
emit_serv_std(erl_genserv, G, N, X) ->
    Fd		= ic_genobj:stubfiled(G),
    Impl	= ic_genobj:impl(G),
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
    ic_codegen:comment(Fd, "Standard gen_server termination"),
    emit(Fd, "stop(OE_THIS) ->\n"),
    emit(Fd, "    gen_server:cast(OE_THIS,stop).\n"),
    nl(Fd),
    ic_codegen:comment(Fd, "Call to implementation init"),
    emit(Fd, "init(Env) ->\n"),
    emit(Fd, "    ~p:~p(Env).\n", [to_atom(Impl), init]),
    nl(Fd),
    emit(Fd, "terminate(Reason, State) ->\n"),
    emit(Fd, "    ~p:~p(Reason, State).\n", 
	 [to_atom(Impl), terminate]),
    nl(Fd), nl(Fd),
    Fd.

gen_end_of_call(erl_corba, G) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment_light(Fd, ["Standard gen_server call handle"]),
    emit(Fd, "handle_call(stop, _, State) ->\n"),
    emit(Fd, "    {stop, normal, ok, State}"),
    case get_opt(G, serv_last_call) of
	exception ->
	    emit(Fd, ";\n"),
	    nl(Fd),
	    emit(Fd, "handle_call(_, _, State) ->\n"),
	    emit(Fd, "    {reply, catch corba:raise(#'BAD_OPERATION'{minor=1163001857, completion_status='COMPLETED_NO'}), State}.\n");
	exit ->
	    emit(Fd, ".\n"),
	    nl(Fd),
	    nl(Fd)
    end,
    ok;
gen_end_of_call(erl_genserv, G) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment_light(Fd, ["Standard gen_server call handle"]),
    emit(Fd, "handle_call(stop, _, State) ->\n"),
    emit(Fd, "    {stop, normal, ok, State}"),
    emit(Fd, ".\n"),
    nl(Fd), nl(Fd),
    ok.

gen_end_of_cast(erl_corba, G) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment_light(Fd, ["Standard gen_server cast handle"]),
    emit(Fd, "handle_cast(stop, State) ->\n"),
    emit(Fd, "    {stop, normal, State}"),
    case get_opt(G, serv_last_call) of
	exception ->
	    emit(Fd, ";\n"),
	    nl(Fd),
	    emit(Fd, "handle_cast(_, State) ->\n"),
	    emit(Fd, "    {noreply, State}.\n");
	exit ->
	    emit(Fd, ".\n"),
	    nl(Fd), nl(Fd)
    end,
    ok;
gen_end_of_cast(erl_genserv, G) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment_light(Fd, ["Standard gen_server cast handle"]),
    emit(Fd, "handle_cast(stop, State) ->\n"),
    emit(Fd, "    {stop, normal, State}"),
    emit(Fd, ".\n"),
    nl(Fd), nl(Fd),   
    ok.

emit_skel_footer(erl_corba, G, N, X) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment_light(Fd, ["Standard gen_server handles"]),
    case use_impl_handle_info(G, N, X) of
	true ->
	    emit(Fd, "handle_info(Info, State) ->\n"),
	    emit(Fd, "    corba:handle_info(~p, Info, State).\n\n", 
		 [list_to_atom(ic_genobj:impl(G))]);
	false ->
	    emit(Fd, "handle_info(_, State) ->\n"),
	    emit(Fd, "    {noreply, State}.\n\n")
    end,
    nl(Fd),
    case get_opt(G, no_codechange) of
	false ->
	    emit(Fd, "code_change(OldVsn, State, Extra) ->\n"),    
	    emit(Fd, "    corba:handle_code_change(~p, OldVsn, State, Extra).\n\n", 
		 [list_to_atom(ic_genobj:impl(G))]);
	true ->
	    emit(Fd, "code_change(_, State, _) ->\n"),    
	    emit(Fd, "    {ok, State}.\n\n")
    end,
    ok;
emit_skel_footer(erl_genserv, G, N, X) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd), nl(Fd),
    ic_codegen:mcomment_light(Fd, ["Standard gen_server handles"]),
    case use_impl_handle_info(G, N, X) of
	true ->
	    emit(Fd, "handle_info(Info, State) ->\n"),
	    emit(Fd, "    ~p:handle_info(Info, State).\n\n", 
		 [list_to_atom(ic_genobj:impl(G))]);
	false ->
	    emit(Fd, "handle_info(_, State) ->\n"),
	    emit(Fd, "    {noreply, State}.\n\n")
    end,
    nl(Fd), nl(Fd),
    case get_opt(G, no_codechange) of
	false ->
	    emit(Fd, "code_change(OldVsn, State, Extra) ->\n"),    
	    emit(Fd, "    ~p:code_change(OldVsn, State, Extra).\n\n", 
		 [list_to_atom(ic_genobj:impl(G))]);
	true ->
	    emit(Fd, "code_change(_, State, _) ->\n"),    
	    emit(Fd, "    {ok, State}.\n\n")
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

use_precond(G, N, X) ->
    FullName = ic_util:to_colon([get_id2(X) | N]),
    case get_opt(G, {precond, FullName}) of
	false -> 
	    InterfaceName = ic_util:to_colon(N),
	    case get_opt(G, {precond, InterfaceName}) of
		false ->
		    case get_opt(G, precond) of
			false -> false;
			V2 -> V2
		    end;
		V2 -> V2
	    end;
	V1 -> V1
    end.

use_postcond(G, N, X) ->
    FullName = ic_util:to_colon([get_id2(X) | N]),
    case get_opt(G, {postcond, FullName}) of
	false -> 
	    InterfaceName = ic_util:to_colon(N),
	    case get_opt(G, {postcond, InterfaceName}) of
		false ->
		    case get_opt(G, postcond) of
			false -> false;
			V3 -> V3
		    end;
		V2 -> V2
	    end;
	V1 -> V1
    end.


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
		    ic_codegen:export(Fd, exp_top(G, N, Body, [], get_opt(G, be))),
		    nl(Fd)
	    end, X#interface.inherit_body),

    ic_codegen:comment(Fd, "Type identification function"),
    ic_codegen:export(Fd, [{typeID, 0}]), 
    nl(Fd),
    ic_codegen:comment(Fd, "Used to start server"),
    ic_codegen:export(Fd, [{oe_create, 0}, {oe_create_link, 0}, {oe_create, 1}, {oe_create_link, 1},
			   {oe_create, 2}, {oe_create_link, 2}]),
    nl(Fd),
    case get_opt(G, be) of
	erl_corba ->
	    ic_codegen:comment(Fd, "TypeCode Functions and inheritance"),
	    ic_codegen:export(Fd, [{oe_tc, 1}, {oe_is_a, 1}, {oe_get_interface, 0}]);
	_ ->
	    ic_codegen:export(Fd, [{start, 2}, {start_link, 3}])
    end,
    nl(Fd),
    ic_codegen:comment(Fd, "gen server export stuff"),
    emit(Fd, "-behaviour(gen_server).\n"),

    case get_opt(G, be) of
	erl_genserv -> %% stop/1 is only for erl_genserv backend
	    ic_codegen:export(Fd, [{stop, 1}, {init, 1}, {terminate, 2}, {handle_call, 3}, 
			      {handle_cast, 2}, {handle_info, 2}, {code_change, 3}]);
	_ ->
	     ic_codegen:export(Fd, [{init, 1}, {terminate, 2}, {handle_call, 3}, 
			      {handle_cast, 2}, {handle_info, 2}, {code_change, 3}])
    end,

    case get_opt(G, be) of
	erl_corba ->
	    nl(Fd),
	    emit(Fd, "-include_lib(\"~s/include/~s\").\n", [?ORBNAME, ?CORBAHRL]);
	_ ->
	    ok
    end,
    nl(Fd), nl(Fd),
    ic_codegen:mcomment(Fd, ["Object interface functions."]),
    nl(Fd), nl(Fd), nl(Fd),
    Fd;
gen_head_special(_G, _N, _X) -> ok.

    

%% Shall generate all export declarations
gen_head(G, N, X) -> 
    case ic_genobj:is_stubfile_open(G) of
	true -> 
	    F = ic_genobj:stubfiled(G),
	    ic_codegen:comment(F, "Interface functions"),
	    ic_codegen:export(F, exp_top(G, N, X, [], get_opt(G, be))),
	    nl(F),
	    gen_head_special(G, N, X);
	false -> ok
    end.

exp_top(_G, _N, X, Acc, _)  when element(1, X) == preproc -> 
    Acc;
exp_top(G, N, L, Acc, BE)  when is_list(L) ->
    exp_list(G, N, L, Acc, BE);
exp_top(G, N, M, Acc, BE)  when is_record(M, module) ->
    exp_list(G, N, get_body(M), Acc, BE);
exp_top(G, N, I, Acc, BE)  when is_record(I, interface) ->
    exp_list(G, N, get_body(I), Acc, BE);
exp_top(G, N, X, Acc, BE) ->
    exp3(G, N, X, Acc, BE).

exp3(_G, _N, C, Acc, _BE)  when is_record(C, const) ->
    [{get_id(C#const.id), 0} | Acc];
exp3(_G, _N, Op, Acc, erl_corba)  when is_record(Op, op) ->
    FuncName = get_id(Op#op.id),
    Arity = length(ic:filter_params([in, inout], Op#op.params)) + 1,
    [{FuncName, Arity}, {FuncName, Arity+1} | Acc];
exp3(G, N, Op, Acc, _BE)  when is_record(Op, op) ->
    FuncName = get_id(Op#op.id),
    Arity = 
	case use_timeout(G,N,Op) of
	    true ->
		 %% NO TimeOut on ONEWAYS here !!!!
		case is_oneway(Op) of 
		    true ->
			length(ic:filter_params([in, inout], Op#op.params)) + 1;
		    false ->
			length(ic:filter_params([in, inout], Op#op.params)) + 2
		end;
	    false ->
		length(ic:filter_params([in, inout], Op#op.params)) + 1
	end,
    [{FuncName, Arity} | Acc];

exp3(_G, _N, A, Acc, erl_corba)  when is_record(A, attr) ->
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> [{Get, 1}, {Get, 2} | Acc2];
			    _ ->             [{Get, 1}, {Get, 2}, 
					      {Set, 2}, {Set, 3} | Acc2]
			end end, Acc, ic_forms:get_idlist(A));
exp3(_G, _N, A, Acc, _BE)  when is_record(A, attr) ->
    lists:foldr(fun(Id, Acc2) ->
			{Get, Set} = mk_attr_func_names([], get_id(Id)),
			case A#attr.readonly of
			    {readonly, _} -> [{Get, 1} | Acc2];
			    _ ->             [{Get, 1}, {Set, 2} | Acc2]
			end end, Acc, ic_forms:get_idlist(A));

exp3(_G, _N, _X, Acc, _BE) -> Acc.

exp_list(G, N, L, OrigAcc, BE) -> 
    lists:foldr(fun(X, Acc) -> exp3(G, N, X, Acc, BE) end, OrigAcc, L).




%%------------------------------------------------------------
%%
%% Emit stuff
%%
%%	Low level generation primitives
%%

emit_stub_func(G, N, X, Name, ArgNames, _TypeList, OutArgs, Oneway, Backend) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> 
	    ok;
	true ->
	    Fd = ic_genobj:stubfiled(G),
	    StubName = list_to_atom(Name),
	    UsingTimeout = use_timeout(G, N, X),
	    Timeout = case UsingTimeout of
			  true ->
			      mk_name(G, "Timeout");
			  false ->
			      "infinity"
		      end,
	    Options = mk_name(G, "Options"),
	    This = mk_name(G, "THIS"),
	    CallOrCast = 
		case is_oneway(X) of 
		    true -> ?CAST; 
		    _ -> ?CALL
		end,
	    emit_op_comment(G, Fd, X, StubName, ArgNames, OutArgs),
	    case Backend of
		erl_corba ->
		    emit(Fd, "~p(~s) ->\n", 
			 [StubName, mk_list([This | ArgNames])]),
		    emit(Fd, "    ~s:~s(~s, ~p, [~s], ?MODULE).\n\n", 
			 [?CORBAMOD, CallOrCast, This, StubName, mk_list(ArgNames)]),
		    emit(Fd, "~p(~s) ->\n", 
			 [StubName, mk_list([This, Options| ArgNames])]),
		    emit(Fd, "    ~s:~s(~s, ~p, [~s], ?MODULE, ~s).\n\n", 
			 [?CORBAMOD, CallOrCast, This, StubName, mk_list(ArgNames),
			  Options]);
		_  ->
		    FunName = case ic_options:get_opt(G, scoped_op_calls) of 
				  true -> 
				      list_to_atom(ic_util:to_undersc([Name | N]));
				  false ->
				      StubName
			      end,
		    %% NO TimeOut on ONEWAYS here !!!!
		    case Oneway of 
			true ->
			    emit(Fd, "~p(~s) ->\n", 
				 [StubName, mk_list([This | ArgNames])]);
			false ->
			    case UsingTimeout of
				true ->
				    emit(Fd, "~p(~s) ->\n",
					 [StubName, mk_list([This, Timeout| ArgNames])]);
				false ->
				    emit(Fd, "~p(~s) ->\n", 
					 [StubName, mk_list([This | ArgNames])])
			    end
		    end,
		    
		    %% NO TimeOut on ONEWAYS here !!!!
		    if 
			length(ArgNames) == 0 ->
			    case is_oneway(X) of 
				true ->
				    emit(Fd, "    ~s:~s(~s, ~p).\n\n",
					 [?GENSERVMOD, CallOrCast, This, FunName]);
				false ->
				    emit(Fd, "    ~s:~s(~s, ~p, ~s).\n\n",
					 [?GENSERVMOD, CallOrCast, This, FunName, Timeout])
			    end;
			true ->
			    case is_oneway(X) of 
				true ->
				    emit(Fd, "    ~s:~s(~s, {~p, ~s}).\n\n", 
					 [?GENSERVMOD, CallOrCast, This, FunName,
					  mk_list(ArgNames)]);
				false ->
				    emit(Fd, "    ~s:~s(~s, {~p, ~s}, ~s).\n\n", 
					 [?GENSERVMOD, CallOrCast, This, FunName,
					  mk_list(ArgNames), Timeout])
			    end
		    end
	    end
    end.

emit_skel_func(G, N, X, OpName, ArgNames, TypeList, OutArgs, Oneway, Backend) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> 
	    ok;
	true ->
	    emit_skel_func_helper(G, N, X, OpName, ArgNames, TypeList, OutArgs, 
				  Oneway, Backend)
    end.

emit_skel_func_helper(G, N, X, OpName, ArgNames, _TypeList, OutArgs, Oneway, 
		      erl_corba) ->
    Fd = ic_genobj:stubfiled(G),
    Name	= list_to_atom(OpName),
    ImplF	= Name,
    ImplM	= list_to_atom(ic_genobj:impl(G)),
    ThisStr	= mk_name(G, "THIS"),
    FromStr	= mk_name(G, "From"),
    State	= mk_name(G, "State"),
    Context	= mk_name(G, "Context"),

    {UseFrom, From} =
	case Oneway of
	    false ->
		case use_from(G, N, OpName) of
		    true ->
			{FromStr, FromStr};
		    false ->
			{"false", "_"}
		end;
	    true ->
		{"false", "_"}
	end,
    {UseThis, This} =
	case use_this(G, N, OpName) of
	    true ->
		{ThisStr, ThisStr};
	    false ->
		{"false", "_"}
	end,
    %% Create argument list string
    CallArgs = mk_list(ArgNames),
    emit_op_comment(G, Fd, X, Name, ArgNames, OutArgs),
    
    %% Check if pre and post conditions are specified for this operation
    Precond = use_precond(G, N, X),
    Postcond = use_postcond(G, N, X),
    
    case Oneway of
	true ->
	    emit(Fd, "handle_cast({~s, ~s, ~p, [~s]}, ~s) ->\n",
		 [This, Context, Name, CallArgs, State]),
	    case {Precond, Postcond} of
		{false, false} ->
		    emit(Fd, "    corba:handle_cast(~p, ~p, [~s], ~s, ~s, ~s);\n\n", 
			 [ImplM, ImplF, CallArgs, State, Context, UseThis]);
		_ ->
		    emit(Fd, "    corba:handle_cast(~p, ~p, [~s], ~s, ~s, ~s, ~p, ~p, ?MODULE);\n\n", 
			 [ImplM, ImplF, CallArgs, State, Context, UseThis, 
			  Precond, Precond])
	    end;
	false ->
	    emit(Fd, "handle_call({~s, ~s, ~p, [~s]}, ~s, ~s) ->\n",
		 [This, Context, Name, CallArgs, From, State]),
	    case {Precond, Postcond} of
		{false, false} ->
		    emit(Fd, "  corba:handle_call(~p, ~p, [~s], ~s, ~s, ~s, ~s);\n\n", 
			 [ImplM, ImplF, CallArgs, State, Context, UseThis, UseFrom]);
		_->
		    emit(Fd, "  corba:handle_call(~p, ~p, [~s], ~s, ~s, ~s, ~s, ~p, ~p, ?MODULE);\n\n", 
			 [ImplM, ImplF, CallArgs, State, Context, UseThis, UseFrom,
			  Precond, Postcond])
	    end
    end;
emit_skel_func_helper(G, N, X, OpName, ArgNames, _TypeList, OutArgs, Oneway,
		      _Backend) ->
    Fd = ic_genobj:stubfiled(G),
    Name	= list_to_atom(OpName),
    ImplF	= Name,
    ImplM	= list_to_atom(ic_genobj:impl(G)),
    FromStr	= mk_name(G, "From"),
    State	= mk_name(G, "State"),

    %% Create argument list
    CallArgs1 = [State | ArgNames],
    {CallArgs2, From} =
	case is_oneway(X) of
	    false ->
		case use_from(G, N, OpName) of
		    true ->
			{[FromStr | CallArgs1], FromStr};
		    false ->
			{CallArgs1, "_"}
		end;
	    true ->
		{CallArgs1, "_"}
	end,
    %% Create argument list string
    CallArgs = mk_list(CallArgs2),
    emit_op_comment(G, Fd, X, Name, ArgNames, OutArgs),
    FunName = case ic_options:get_opt(G, scoped_op_calls) of 
		  true -> 
		      list_to_atom(ic_util:to_undersc([OpName | N]));
		  false ->
		      list_to_atom(OpName)
	      end,
    case Oneway of
	true ->
	    if
		length(ArgNames) == 0 ->
		    emit(Fd, "handle_cast(~p, ~s) ->\n", [FunName, State]);
		true ->
		    emit(Fd, "handle_cast({~p, ~s}, ~s) ->\n",
			 [FunName, mk_list(ArgNames), State])
	    end,
	    emit(Fd, "    ~p:~p(~s);\n\n", [ImplM, ImplF, CallArgs]);
	false ->
	    if
		length(ArgNames) == 0 ->
		    emit(Fd, "handle_call(~p, ~s, ~s) ->\n",
			 [FunName, From, State]);
		true ->
		    emit(Fd, "handle_call({~p, ~s}, ~s, ~s) ->\n",  
			 [FunName, mk_list(ArgNames), From, State])
	    end,
	    emit(Fd, "    ~p:~p(~s);\n\n", [ImplM, ImplF, CallArgs])
    end.

use_this(G, N, OpName) ->
    FullOp = ic_util:to_colon([OpName|N]),
    FullIntf = ic_util:to_colon(N),
    case {get_opt(G, {this, FullIntf}), get_opt(G, {this, FullOp}), 
	  get_opt(G, {this, true})} of
	{_, force_false, _} -> false;
	{force_false, false, _} -> false;
	{false, false, false} -> false;
	_ -> true
    end.

use_from(G, N, OpName) ->
    FullOp = ic_util:to_colon([OpName|N]),
    FullIntf = ic_util:to_colon(N),
    case {get_opt(G, {from, FullIntf}), get_opt(G, {from, FullOp}), 
	  get_opt(G, {from, true})} of
	{_, force_false, _} -> false;
	{force_false, false, _} -> false;
	{false, false, false} -> false;
	_ -> true
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
	     mk_list(lists:map(fun(E) -> ic_util:to_colon(E) end, 
			       X#op.raises))]
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
%%    {scoped_name(Scope, "_get_"++Name), scoped_name(Scope, "_set_"++Name)}.

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
    map(fun(Id) -> T#typedef{id=Id} end, T#typedef.id).




%% Code produce for dependency function
genDependency(G) ->
    Fd = ic_genobj:stubfiled(G),
    nl(Fd),nl(Fd),
    ic_codegen:comment(Fd, "Idl file dependency list function"), 
    emit(Fd, "oe_dependency() ->\n\n", []),
    emit(Fd, "    ~p.\n\n", [ic_pragma:get_dependencies(G)]).   
