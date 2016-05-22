%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(ictk).


%% Toplevel generation functions
-export([reg_gen/3, unreg_gen/3]).


%% Utilities
-export([get_IR_ID/3, get_IR_VSN/3, register_name/1, unregister_name/1]).

-import(ic_forms, [get_id2/1, get_body/1, get_idlist/1]).
-import(ic_util, [mk_name/2, mk_oe_name/2, to_atom/1, to_list/1]).
-import(ic_codegen, [emit/2, emit/3, nl/1]).

-include("icforms.hrl").
-include("ic.hrl").

%%--------------------------------------------------------------------
%%
%% IFR Registration Generation
%%
%%
%%--------------------------------------------------------------------

-define(IFRID(G), mk_name(G, "IFR")).
-define(VARID(G), mk_name(G, "VAR")).
-define(IFRMOD, orber_ifr).

reg_gen(G, N, X) ->
    S = ic_genobj:tktab(G),
    Light = ic_options:get_opt(G, light_ifr),
    init_var(),
    case ic_genobj:is_stubfile_open(G) of
	true when Light == false ->
	    Var = ?IFRID(G),
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd), nl(Fd),
	    emit(Fd, "~p() ->\n", [to_atom(register_name(G))]),
	    emit(Fd, "    ~s = ~p:find_repository(),\n",
		 [Var, ?IFRMOD]),
	    nl(Fd),
	    
            %% Write call function that checks if included
            %% modules and interfaces are created.
	    emit(Fd, "    register_tests(~s),\n",[?IFRID(G)]),
	    
	    reg2(G, S, N, Var, X),
	    nl(Fd),
	    emit(Fd, "    ok.\n"),

	    %% Write general register test function.
	    register_tests(Fd,G),
	    
	    %% Write functopn that registers modules only if  
            %% they are not registered.
	    register_if_unregistered(Fd);
	true when Light == true ->
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd), nl(Fd),
	    Regname = to_atom(register_name(G)),
	    emit(Fd, "~p() ->\n\t~p([]).\n\n", [Regname, Regname]),
	    emit(Fd, "~p(OE_Options) ->\n\t~p:add_items(?MODULE, OE_Options,\n\t[", 
		 [Regname, ?IFRMOD]),
	    reg_light(G, N, X),
	    emit(Fd, "ok]),\n\tok.\n");
	false -> 
	    ok
    end.

reg_light(G, N, X)  when is_list(X) -> 
    reg_light_list(G, N, X);
reg_light(G, N, X) when is_record(X, module) ->
    reg_light_list(G, [get_id2(X) | N], get_body(X));
reg_light(G, N, X) when is_record(X, struct) ->
    emit(ic_genobj:stubfiled(G), "{~p, ~p, struct},\n\t", 
	 [get_IR_ID(G, N, X), get_module(X, N)]);
reg_light(G, N, X) when is_record(X, except) ->
    emit(ic_genobj:stubfiled(G), "{~p, ~p, except},\n\t", 
	 [get_IR_ID(G, N, X), get_module(X, N)]);
reg_light(G, N, X) when is_record(X, union) ->
    emit(ic_genobj:stubfiled(G), "{~p, ~p, union},\n\t", 
	 [get_IR_ID(G, N, X), get_module(X, N)]);
reg_light(G, N, X) when is_record(X, interface) ->
    emit(ic_genobj:stubfiled(G), "{~p, ~p, interface},\n\t", 
	 [get_IR_ID(G, N, X), get_module(X, N)]),
    reg_light_list(G, [get_id2(X)|N], get_body(X));
reg_light(_G, _N, _X) ->  
    ok.

get_module(X, N) ->
    List = [get_id2(X) | N],
    list_to_atom(lists:foldl(fun(E, Acc) -> E++"_"++Acc end, 
			     hd(List), tl(List))).

%% This function filters off all "#include <FileName>.idl" code that 
%% come along from preprocessor and scanner. Produces code ONLY for
%% the actuall file. See ticket OTP-2133
reg_light_list(_G, _N, []) -> [];
reg_light_list(G, N, List ) ->
    CurrentFileName = ic_genobj:idlfile(G), 
    reg_light_list(G, N, {CurrentFileName,true}, List).

%% The filter function + loop 
reg_light_list(_G, _N, {_CFN, _Status}, []) -> [];
reg_light_list(G, N, {CFN,Status}, [X | Xs]) ->
    case Status of 
	true ->
	    case X of
		{preproc,_,{_,_,_FileName},[{_,_,"1"}]} ->
		    reg_light_list(G, N, {CFN,false}, Xs);
		_ ->
		    reg_light(G, N, X),
		    reg_light_list(G, N, {CFN,Status}, Xs)
	    end;
	false ->
	    case X of
		{preproc,_,{_,_,CFN},[{_,_,"2"}]} ->
		    reg_light(G, N, X),
		    reg_light_list(G, N, {CFN,true}, Xs);
		_ ->
		    reg_light_list(G, N, {CFN,Status}, Xs)
	    end
    end.


%% reg2 is top level registration 

reg2(G, S, N, Var, X) ->
    reg2(G, S, N, "Repository_create_", Var, X).

reg2(G, S, N, C, V, X)  when is_list(X) -> reg2_list(G, S, N, C, V, X);

reg2(G, S, N, C, V, X) when is_record(X, module) ->
    NewV = r_emit2(G, S, N, C, V, X, "", []),
    reg2_list(G, S, [get_id2(X) | N], "ModuleDef_create_", NewV, get_body(X));

reg2(G, S, N, C, V, X) when is_record(X, const) ->
    r_emit2(G, S, N, C, V, X, ", ~s, ~p", 
	    [get_idltype(G, S, N, X), {X#const.tk, X#const.val}]);

reg2(G, S, N, C, V, X) when is_record(X, struct) ->
    do_struct(G, S, N, C, V, X, ic_forms:get_tk(X));

reg2(G, S, N, C, V, X) when is_record(X, except) ->
    do_except(G, S, N, C, V, X, ic_forms:get_tk(X));

reg2(G, S, N, C, V, X) when is_record(X, union) ->
    do_union(G, S, N, C, V, X, ic_forms:get_tk(X));

reg2(G, S, N, C, V, X) when is_record(X, enum) ->
    r_emit2(G, S, N, C, V, X, ", ~p", 
	    [get_enum_member_list(G, S, N, get_body(X))]);

reg2(G, S, N, C, V, X) when is_record(X, typedef) ->
    do_typedef(G, S, N, C, V, X),
    look_for_types(G, S, N, C, V, get_body(X));

reg2(G, S, N, C, V, X) when is_record(X, attr) ->
    XX = #id_of{type=X},
    lists:foreach(fun(Id) -> r_emit2(G, S, N, C, V, XX#id_of{id=Id}, ", ~s, ~p",
				     [get_idltype(G, S, N, X), get_mode(G, N, X)])
		  end,
		  get_idlist(X));

reg2(G, S, N, C, V, X) when is_record(X, interface) ->
    N2 = [get_id2(X) | N],
    Body = get_body(X), 
    BIs = get_base_interfaces(G,X), %% produce code for the interface inheritance
    NewV = r_emit2(G, S, N, C, V, X, ", " ++ BIs,[]),
    reg2_list(G, S, N2, "InterfaceDef_create_", NewV, Body);


reg2(G, S, N, C, V, X) when is_record(X, op) ->
    r_emit2(G, S, N, C, V, X, ", ~s, ~p, [~s], [~s], ~p",
	    [get_idltype(G, S, N, X), get_mode(G, N, X), 
	     get_params(G, S, N, X#op.params), get_exceptions(G, S, N, X),
	     get_context(G, S, N, X)]);

reg2(_G, _S, _N, _C, _V, X)  when is_record(X, preproc) -> ok;

reg2(_G, _S, _N, _C, _V, X)  when is_record(X, pragma) -> ok;

reg2(_G, _S, _N, _C, _V, _X) ->  ok.


%% This function filters off all "#include <FileName>.idl" code that 
%% come along from preprocessor and scanner. Produces code ONLY for
%% the actuall file. See ticket OTP-2133
reg2_list(_G, _S, _N, _C, _V, []) -> [];
reg2_list(G, S, N, C, V, List ) ->
    CurrentFileName = ic_genobj:idlfile(G), 
    reg2_list(G, S, N, C, V, {CurrentFileName,true}, List).

%% The filter function + loop 
reg2_list(_G, _S, _N, _C, _V, {_CFN, _Status}, []) -> [];
reg2_list(G, S, N, C, V, {CFN,Status}, [X | Xs]) ->
    case Status of 
	true ->
	    case X of
		{preproc,_,{_,_,_FileName},[{_,_,"1"}]} ->
		    reg2_list(G, S, N, C, V, {CFN,false}, Xs);
		_ ->
		    F = reg2(G, S, N, C, V, X),
		    [F | reg2_list(G, S, N, C, V, {CFN,Status}, Xs)]
	    end;
	false ->
	    case X of
		{preproc,_,{_,_,CFN},[{_,_,"2"}]} ->
		    F = reg2(G, S, N, C, V, X),
		    [F | reg2_list(G, S, N, C, V, {CFN,true}, Xs)];
		_ ->
		    reg2_list(G, S, N, C, V, {CFN,Status}, Xs)
	    end
    end.





%% General registration tests
register_tests(Fd,G) ->
    IfrId = ?IFRID(G),
    emit(Fd,"\n\n%% General IFR registration checks.\n", []),
    emit(Fd,"register_tests(~s)->\n",[IfrId]),
    emit(Fd,"  re_register_test(~s),\n",[IfrId]),
    emit(Fd,"  include_reg_test(~s).\n\n",[IfrId]),

    emit(Fd,"\n%% IFR type Re-registration checks.\n", []),
    case ic_pragma:fetchRandomLocalType(G) of
	{ok,TypeId} ->
	    emit(Fd,"re_register_test(~s)->\n",[IfrId]),
	    emit(Fd,"  case orber_ifr:'Repository_lookup_id'(~s,~p) of\n", [IfrId,TypeId]),
	    emit(Fd,"    []  ->\n      true;\n",[]),
	    emit(Fd,"    _ ->\n      exit({allready_registered,~p})\n end.\n\n", [TypeId]);
	false ->
	    emit(Fd,"re_register_test(_)-> true.\n",[])
    end,

    emit(Fd,"~s",[check_include_regs(G)]).




%% This function produces code for existance check over
%% top level included modules and interfaces
check_include_regs(G) ->
    IfrId = ?IFRID(G),
    case ic_pragma:get_incl_refs(G) of
	none ->
	    io_lib:format("\n%% No included idl-files detected.\n", []) ++
	    io_lib:format("include_reg_test(_~s) -> true.\n",[IfrId]);
	IMs ->
	    io_lib:format("\n%% IFR registration checks for included idl files.\n", []) ++
	    io_lib:format("include_reg_test(~s) ->\n",[IfrId]) ++
		check_incl_refs(G,IfrId,IMs)
    end.



check_incl_refs(_,_,[]) ->
    io_lib:format("  true.\n",[]);
check_incl_refs(G,IfrId,[[First]|Rest]) ->
    ModId = ic_pragma:scope2id(G,First),
    io_lib:format("  case orber_ifr:'Repository_lookup_id'(~s,~p) of~n", [IfrId,ModId]) ++
	io_lib:format("    [] ->~n      exit({unregistered,~p});~n", [ModId]) ++
	io_lib:format("    _  ->~n      true~n  end,~n",[]) ++
	check_incl_refs(G,IfrId,Rest).



%% This function will return module ref, it will
%% also register module if not registered.
register_if_unregistered(Fd) ->
    emit(Fd, "\n\n%% Fetch top module reference, register if unregistered.\n"),
    emit(Fd, "oe_get_top_module(OE_IFR, ID, Name, Version) ->\n"),
    emit(Fd, "  case orber_ifr:'Repository_lookup_id'(OE_IFR, ID) of\n"),
    emit(Fd, "    [] ->\n"),
    emit(Fd, "      orber_ifr:'Repository_create_module'(OE_IFR, ID, Name, Version);\n"),
    emit(Fd, "    Mod  ->\n"),
    emit(Fd, "      Mod\n",[]),
    emit(Fd, "   end.\n\n"),
    emit(Fd, "%% Fetch module reference, register if unregistered.\n"),
    emit(Fd, "oe_get_module(OE_IFR, OE_Parent, ID, Name, Version) ->\n"),
    emit(Fd, "  case orber_ifr:'Repository_lookup_id'(OE_IFR, ID) of\n"),
    emit(Fd, "    [] ->\n"),
    emit(Fd, "      orber_ifr:'ModuleDef_create_module'(OE_Parent, ID, Name, Version);\n"),
    emit(Fd, "    Mod  ->\n"),
    emit(Fd, "      Mod\n",[]),
    emit(Fd, "   end.\n").



do_typedef(G, S, N, C, V, X) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> ok;
	true -> 
	    Fd = ic_genobj:stubfiled(G),
	    Thing = get_thing_name(X),
	    IR_VSN = get_IR_VSN(G, N, X),
	    TK = ic_forms:get_tk(X),

	    lists:foreach(
	      fun(Id) ->
		      r_emit_raw(G, X, Fd, "", C, Thing, V, 
				 get_IR_ID(G, N, Id), get_id2(Id),
				 IR_VSN, ", ~s", 
				 [get_idltype_tk(G, S, N, 
						 ictype:maybe_array(G, S, N, 
							     Id, TK))])
	      end, get_idlist(X))
    end.


do_union(G, S, N, C, V, X, {tk_union, _IFRID, _Name, DiscrTK, _DefNr, L}) ->
    N2 = [get_id2(X) | N],
    r_emit2(G, S, N, C, V, X, ", ~s, [~s]", 
	    [get_idltype_tk(G, S, N, DiscrTK),
	     get_union_member_def(G, S, N2, L)]),
    look_for_types(G, S, N2, C, V, get_body(X)).

do_struct(G, S, N, C, V, X, {tk_struct, _IFRID, _Name, ElemList}) ->
    N2 = [get_id2(X) | N],
    r_emit2(G, S, N, C, V, X, ", [~s]", 
	    [get_member_def(G, S, N, ElemList)]),
    look_for_types(G, S, N2, C, V, get_body(X)).

do_except(G, S, N, C, V, X, {tk_except, _IFRID, _Name, ElemList}) ->
    N2 = [get_id2(X) | N],
    r_emit2(G, S, N, C, V, X, ", [~s]", 
	    [get_member_def(G, S, N, ElemList)]),
    look_for_types(G, S, N2, C, V, get_body(X)).


%% new_var finds an unused Erlang variable name by increasing a
%% counter.
new_var(_G) ->
    lists:flatten(["_OE_", integer_to_list(put(var_count, get(var_count) + 1))]).
init_var() ->
    put(var_count, 1).

%% Public interface. The name of the register function.
register_name(G) ->
    mk_oe_name(G, "register").
unregister_name(G) ->
    mk_oe_name(G, "unregister").



look_for_types(G, S, N, C, V, L) when is_list(L) ->
    lists:foreach(fun(X) -> look_for_types(G, S, N, C, V, X) end, L);
look_for_types(G, S, N, C, V, {_Name, TK}) ->	% member
    look_for_types(G, S, N, C, V, TK);
look_for_types(_G, _S, _N, _C, _V, {tk_union, _IFRID, _Name, _DT, _Def, _L}) ->
    ok;
look_for_types(G, S, N, C, V, {_Label, _Name, TK}) ->	% case_dcl
    look_for_types(G, S, N, C, V, TK);
look_for_types(_G, _S, _N, _C, _V, {tk_struct, _IFRID, _Name, _L}) ->
    ok;
look_for_types(_G, _S, _N, _C, _V, _X) ->
    ok.

    


%% This function produces code for the interface inheritance registration.
%% It produces a string that represents a list of function calls.
%% This list becomes a list of object references when the main function
%% "orber_ifr:ModuleDef_create_interface" is called.

get_base_interfaces(G,X) ->
    case element(3,X) of
	[] ->
	    "[]";
	L ->
	    "[" ++ 
		lists:flatten(
		  lists:foldl( 
		    fun(E, Acc) -> [call_fun_str(G,E), ", " | Acc] end, 
		    call_fun_str(G,hd(L)), 
		    tl(L)
		   )
		 ) ++ "]"
    end.

call_fun_str(G,S) ->
    lists:flatten( 
	io_lib:format("orber_ifr:lookup_id(~s,\"~s\")",
		      [ ?IFRID(G),
			ic_pragma:scope2id(G,S)] )).





%%--------------------------------------------------------------------
%%
%% r_emit emits an IFR register function call. It returns a new
%% variable (if further defs should be added to that one)
%%
%%	G is genobj
%%
%%	S is symbol table (ets)
%%
%%	N is list of ids describing scope
%%
%%	C is create stub (eg. "Repository_create_")
%%
%%	V is variable name where current def should be added,
%%
%%	X is the current def item,
%%
%%	F and A is auxillary format and args that will be io_lib
%%	formatted and inserted as a string (don't forget to start with
%%	", ")
%%
r_emit2(G, _S, N, C, V, X, F, A) ->
    case ic_genobj:is_stubfile_open(G) of
	false -> ok;
	true ->
	    {NewV, Str} = get_assign(G, V, X),
	    r_emit_raw(G, X, ic_genobj:stubfiled(G), Str, 
		       C, get_thing_name(X), V, 
		       get_IR_ID(G, N, X), get_id2(X), get_IR_VSN(G, N, X), 
		       F, A),
	    NewV
    end.


%%--------------------------------------------------------------------
%%
%% An IFR register line registers an entity (Thing) into the IFR. The
%% thing is registered INTO something, an type is registered into a
%% module for instance, and this is reflected in the Var parameter
%% below. The var parameter is the name of the parent IFR object. The
%% Thing parameter is the name of the thing we're trying to register,
%% a typdef is called an alias and an interface is called an
%% interface. Sometimes we need to store the thing we're registering
%% into a variable because we're going to add other things to it
%% later, modules and interfaces are such containers, so we must
%% remember that variable for later use.
%%
%% All parameters shall be strings unless otherwise noted
%%
%% Fd		- File descriptor
%% AssignStr	- Assign or not, empty except for interfaces and modules
%% Create	- Create has diff. names dep. on into what we register
%% Thing	- WHAT is registered, interface
%% Var		- The name of the variable we register into
%% IR_ID	- The IFR identifier (may be "")
%% Id		- The identifier (name) of the object
%% IR_VSN	- The IFR version as a string
%% AuxStr	- An auxillary string
%%
%%r_emit_raw(Fd, AssignStr, Create, Thing, Var, IR_ID, Id, IR_VSN) ->
%%    r_emit_raw(Fd, AssignStr, Create, Thing, Var, IR_ID, Id, IR_VSN, "", []).
r_emit_raw(_G, X, Fd, AssignStr, "Repository_create_", Thing, Var, IR_ID, Id, IR_VSN, F, A) 
  when is_record(X, module) ->
    emit(Fd, "~n    ~s~p(~s, \"~s\", \"~s\", \"~s\"~s),~n", 
	 [AssignStr, to_atom("oe_get_top_"++Thing), Var, IR_ID, Id, 
	  IR_VSN, io_lib:format(F, A)]);
r_emit_raw(G, X, Fd, AssignStr, "ModuleDef_create_", Thing, Var, IR_ID, Id, IR_VSN, F, A) 
  when is_record(X, module) ->
    emit(Fd, "~n    ~s~p(~s, ~s, \"~s\", \"~s\", \"~s\"~s),~n", 
	 [AssignStr, to_atom("oe_get_"++Thing), ?IFRID(G), Var, IR_ID, Id, 
	  IR_VSN, io_lib:format(F, A)]);
r_emit_raw(_G, _X, Fd, AssignStr, Create, Thing, Var, IR_ID, Id, IR_VSN, F, A) ->
    emit(Fd, "~n    ~s~p:~p(~s, \"~s\", \"~s\", \"~s\"~s),~n", 
	 [AssignStr, ?IFRMOD, to_atom(Create++Thing), Var, IR_ID, Id, 
	  IR_VSN, io_lib:format(F, A)]).

			 


%% Used by r_emit. Returns tuple {Var, Str} where Var is the resulting
%% output var (if any, otherwise same as input arg) and Str is a
%% string of the assignment if any ("" or "Var = ")
get_assign(G, _V, X) when is_record(X, module) ->
    mk_assign(G);
get_assign(G, _V, X) when is_record(X, interface) ->
    mk_assign(G);
get_assign(_G, V, _X) -> {V, ""}.
mk_assign(G) ->
    V = new_var(G),
    {V, io_lib:format("~s = ", [V])}.

%% Returns a list of strings of all enum members (suitable for ~p)
get_enum_member_list(_G, _S, _N, L) ->
    lists:map(fun(M) -> get_id2(M) end, L).

%% Will output a string of the union members.
get_union_member_def(_G, _S, _N, []) -> [];
get_union_member_def(G, S, N, L) ->
    [union_member2str(G, S, N, hd(L)) | 
     lists:map(fun(M) -> [", ", union_member2str(G, S, N, M)] end, tl(L))].
%%    lists:foldl(fun(M, Acc) -> 
%%			[union_member2str(G, S, N, M),", " | Acc] end,
%%		union_member2str(G, S, N, hd(L)), tl(L)).

union_member2str(G, S, N, {Label, Name, TK}) ->
    io_lib:format("~s{name=~p, label=~p, type=~p, type_def=~s}",
		  ["#unionmember", Name, Label, TK, 
		   get_idltype_tk(G, S, N, TK)]).


%% Will output a string of the struct members. Works for exceptions
%% and structs
%%
get_member_def(_G, _S, _N, []) -> [];
get_member_def(G, S, N, L) ->
    [member2str(G, S, N, hd(L)) | 
     lists:map(fun(M) -> [", ", member2str(G, S, N, M)] end, tl(L))].

member2str(G, S, N, {Id, TK}) ->
    io_lib:format("~s{name=~p, type=~p, type_def=~s}",
		  ["#structmember", Id, TK, get_idltype_tk(G, S, N, TK)]).

%% Translates between record names and create operation names. 
get_thing_name(X) when is_record(X, op) -> "operation";
get_thing_name(X) when is_record(X, const) -> "constant";
get_thing_name(X) when is_record(X, typedef) -> "alias";
get_thing_name(X) when is_record(X, attr) -> "attribute";
get_thing_name(X) when is_record(X, except) -> "exception";
get_thing_name(X) when is_record(X, id_of) -> get_thing_name(X#id_of.type);
get_thing_name(X) -> to_list(element(1,X)).


%% Returns the mode (in, out, oneway etc) of ops and params. Return
%% value is an atom.
get_mode(_G, _N, X) when is_record(X, op) ->
    case X#op.oneway of
	{oneway, _} -> 'OP_ONEWAY';
	_ -> 'OP_NORMAL'
    end;
get_mode(_G, _N, X) when is_record(X, attr) ->
    case X#attr.readonly of
	{readonly, _} -> 'ATTR_READONLY';
	_ -> 'ATTR_NORMAL'
    end;
get_mode(_G, _N, X) when is_record(X, param) ->
    case X#param.inout of
	{in, _} -> 'PARAM_IN';
	{inout, _} -> 'PARAM_INOUT';
	{out, _} -> 'PARAM_OUT'
    end.


%% Returns a string form of idltype creation.
%%get_idltype_id(G, S, N, X, Id) ->
%%    TK = ictype:tk_lookup(G, S, N, Id),
%%    get_idltype_tk(G, S, N, TK).
get_idltype(G, S, N, X) ->
    get_idltype_tk(G, S, N, ic_forms:get_tk(X)).
get_idltype_tk(G, _S, _N, TK) ->
    io_lib:format("~p:~p(~s, ~p)", [orber_ifr, 'Repository_create_idltype',
				    ?IFRID(G), TK]).

%% Returns a string form of typecode creation. This shall be found in
%% the type code symbol table.
%%get_typecode(G, S, N, X) -> typecode.
%%get_typecode(G, S, N, X) -> tk(G, S, N, get_type(X)).


%% Returns the string form of a list of parameters.
get_params(_G, _S, _N, []) ->  "";
get_params(G, S, N, L) -> 
    lists:foldl(fun(X, Acc) -> param2str(G, S, N, X)++", "++Acc end,
		param2str(G, S, N, hd(L)), tl(L)).


%% Converts a parameter to a string.
param2str(G, S, N, X) ->
    io_lib:format("~s{name=~p, type=~p, type_def=~s, mode=~p}~n", 
		  ["#parameterdescription", get_id2(X),
		   ic_forms:get_tk(X),
		   %%tk_lookup(G, S, N, get_type(X)),
		   get_idltype(G, S, N, X),
		   get_mode(G, N, X)]).




%% Public interface. Returns the IFR ID of an object. This
%% is updated to comply with CORBA 2.0 pragma directives.
get_IR_ID(G, N, X) ->
    ScopedId = [get_id2(X) | N],
    case ic_pragma:get_alias(G,ScopedId) of
	none ->
	    case ic_pragma:pragma_id(G, N, X) of
		none ->
		    case ic_pragma:pragma_prefix(G, N, X) of
			none ->
			    IR_ID = lists:flatten(
				      io_lib:format("IDL:~s:~s", 
						    [slashify(ScopedId), 
						     get_IR_VSN(G, N, X)])),
			    ic_pragma:mk_alias(G,IR_ID,ScopedId),
			    IR_ID;
			PF ->
			    IR_ID = lists:flatten(
				      io_lib:format("IDL:~s:~s", 
						    [ PF ++ "/" ++
						      get_id2(X),
						      get_IR_VSN(G, N, X)])),
			    ic_pragma:mk_alias(G,IR_ID,ScopedId),
			    IR_ID
		    end;
		PI ->
		    ic_pragma:mk_alias(G,PI,ScopedId),
		    PI
	    end;
	Alias ->
	    Alias
    end.

    
%% Public interface. Returns the IFR Version of an object. This
%% is updated to comply with CORBA 2.0 pragma directives.
get_IR_VSN(G, N, X) ->
    ic_pragma:pragma_version(G,N,X).
    




%% Returns a slashified name, [I1, M1] becomes "M1/I1"
%slashify(List) -> lists:foldl(fun(X, Acc) -> get_id2(X)++"/"++Acc end, 
%			      hd(List), tl(List)).

%% Returns a slashified name, [I1, M1] becomes "M1/I1"
slashify(List) -> lists:foldl(fun(X, Acc) -> X++"/"++Acc end, 
			      hd(List), tl(List)).


%% Returns the context literals of an op
get_context(_G, _S, _N, X) -> 
    lists:map(fun(C) -> element(3, C) end, X#op.ctx).



%% Returns the list of the exceptions of an operation
get_exceptions(G, S, N, X) -> 
    case X#op.raises of
	[] ->
	    "";
	L ->
	    lists:flatten(
	      lists:foldl( 
		fun(E, Acc) -> [excdef(G, S, N, X, E), ", " | Acc] end, 
		excdef(G, S, N, X, hd(L)), 
		tl(L)
	       )
	     )
    end.


%% Returns the definition of an exception of an operation
excdef(G, S, N, X, L) ->
    io_lib:format("orber_ifr:lookup_id(~s,\"~s\")",
		  [ ?IFRID(G),
		   get_EXC_ID(G, S, N, X, L) ] ).






%% This function produces code for the exception registration.
%% It produces a string that represents a list of function calls.
%% This list becomes a list of object references when the main function
%% "orber_ifr:InterfaceDef_create_operation" is called.

get_EXC_ID(G, _S, N, X, ScopedId) ->
    case ic_pragma:get_alias(G,ScopedId) of
	none ->
	    case ic_pragma:pragma_id(G, N, X) of
		none ->
		    case ic_pragma:pragma_prefix(G, N, X) of
			none ->
			    EXC_ID = lists:flatten(
				       io_lib:format("IDL:~s:~s", [slashify(ScopedId), 
								   get_IR_VSN(G, N, X)])),
			    ic_pragma:mk_alias(G,EXC_ID,ScopedId),
			    EXC_ID;
			PF ->
			    EXC_ID = lists:flatten(
				       io_lib:format("IDL:~s:~s", [ PF ++ "/" ++ 
								    hd(ScopedId),
								    get_IR_VSN(G, N, X)])),
			    ic_pragma:mk_alias(G,EXC_ID,ScopedId),
			    EXC_ID
		    end;
		PI ->
		    ic_pragma:mk_alias(G,PI,ScopedId),
		    PI
	    end;
	Alias ->
	    Alias
    end.





%% unreg_gen/1 uses the information stored in pragma table
%% to decide which modules are to be unregistered
unreg_gen(G, N, X) ->
    Light = ic_options:get_opt(G, light_ifr),
    case ic_genobj:is_stubfile_open(G) of
	true when Light == false ->
	    Var = ?IFRID(G),
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd), nl(Fd),
	    emit(Fd, "~p() ->\n", [to_atom(unregister_name(G))]),
	    emit(Fd, "    ~s = ~p:find_repository(),\n",
		 [Var, ?IFRMOD]),
	    nl(Fd),

	    unreg2(G, N, X),
	    emit(Fd, "    ok.\n\n"),
	    destroy(Fd);
	true ->
	    Fd = ic_genobj:stubfiled(G),
	    nl(Fd), nl(Fd),
	    Unregname = to_atom(unregister_name(G)),
	    emit(Fd, "~p() ->\n\t~p([]).\n\n~p(OE_Options) ->\n", 
		 [Unregname, Unregname, Unregname]),
	    emit(Fd, "\t~p:remove(?MODULE, OE_Options),\n\tok.\n\n", [?IFRMOD]);
	false -> ok
    end.


destroy(Fd) ->
emit(Fd,"
oe_destroy_if_empty(OE_IFR,IFR_ID) ->
    case orber_ifr:'Repository_lookup_id'(OE_IFR, IFR_ID) of
	[] ->
	    ok;
	Ref ->
	    case orber_ifr:contents(Ref, \'dk_All\', \'true\') of
		[] ->
		    orber_ifr:destroy(Ref),
		    ok;
		_ ->
		    ok
	    end
    end.

oe_destroy(OE_IFR,IFR_ID) ->
    case orber_ifr:'Repository_lookup_id'(OE_IFR, IFR_ID) of
	[] ->
	    ok;
	Ref ->
	    orber_ifr:destroy(Ref),
	    ok
    end.

",[]).










%% unreg2 is top level registration 

unreg2(G, N, X) ->
    emit(ic_genobj:stubfiled(G),"~s",[lists:flatten(unreg3(G, N, X))]).

unreg3(G, N, X)  when is_list(X) -> 
    unreg3_list(G, N, X, []);

unreg3(G, N, X) when is_record(X, module) ->
    unreg3_list(G, [get_id2(X) | N], get_body(X), [unreg_collect(G, N, X)]);

unreg3(G, N, X) when is_record(X, const) ->
    unreg_collect(G, N, X);

unreg3(G, N, X) when is_record(X, struct) ->
    unreg_collect(G, N, X);

unreg3(G, N, X) when is_record(X, except) ->
    unreg_collect(G, N, X);

unreg3(G, N, X) when is_record(X, union) ->
    unreg_collect(G, N, X);

unreg3(G, N, X) when is_record(X, enum) ->
    unreg_collect(G, N, X);

unreg3(G, N, X) when is_record(X, typedef) ->
    unreg_collect(G, N, X);

unreg3(G, N, X) when is_record(X, interface) ->
    unreg_collect(G, N, X);

unreg3(_G, _N, X) when is_record(X, op) -> [];

unreg3(_G, _N, X) when is_record(X, attr) -> [];

unreg3(_G, _N, X)  when is_record(X, preproc) -> [];

unreg3(_G, _N, X)  when is_record(X, pragma) -> [];

unreg3(_G, _N, _X) ->  [].


unreg3_list(_G, _N, [], Found) -> 
    Found;
unreg3_list(G, N, List, Found) ->
    CurrentFileName = ic_genobj:idlfile(G), 
    unreg3_list(G, N, {CurrentFileName,true}, List, Found).

%% The filter function + loop 
unreg3_list(_G, _N, {_CFN, _Status}, [], Found) -> 
    Found;
unreg3_list(G, N, {CFN,Status}, [X | Xs], Found) ->
    case Status of 
	true ->
	    case X of
		{preproc,_,{_,_,_FileName},[{_,_,"1"}]} ->
		    unreg3_list(G, N, {CFN,false}, Xs, Found);
		_ ->
		    unreg3_list(G, N, {CFN,Status}, Xs, [unreg3(G, N, X) | Found])
	    end;
	false ->
	    case X of
		{preproc,_,{_,_,CFN},[{_,_,"2"}]} ->
		    unreg3_list(G, N, {CFN,true}, Xs,[unreg3(G, N, X) | Found]);
		_ ->
		    unreg3_list(G, N, {CFN,Status}, Xs, Found)
	    end
    end.



unreg_collect(G, N, X) when is_record(X, module) ->
    io_lib:format("    oe_destroy_if_empty(OE_IFR, ~p),\n", 
	      [get_IR_ID(G, N, X)]);
unreg_collect(G, N, X) when is_record(X, typedef) ->
    lists:map(fun(Id) ->
		      io_lib:format("    oe_destroy(OE_IFR, ~p),\n", 
				    [get_IR_ID(G, N, Id)])
		  end,
	      ic_forms:get_idlist(X));
unreg_collect(G, N, X) ->
    io_lib:format("    oe_destroy(OE_IFR, ~p),\n", 
	      [get_IR_ID(G, N, X)]).



