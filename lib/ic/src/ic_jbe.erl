%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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


-module(ic_jbe).


-export([do_gen/3, gen/3, emit_type_function/4]).



-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
-include_lib("stdlib/include/erl_compile.hrl").



%%------------------------------------------------------------
%%
%% Entry point
%%
%%------------------------------------------------------------

do_gen(G, _File, Form) -> 
    gen(G, [], Form).


%%------------------------------------------------------------
%%
%% Generate the client side C stubs.
%%
%% Each module is generated to a separate file.
%%
%% Each function needs to generate a function head and
%% a body. IDL parameters must be converted into C parameters.
%%
%%------------------------------------------------------------

gen(G, N, [X|Xs]) when is_record(X, preproc) ->
    NewG = handle_preproc(G, N, X#preproc.cat, X),
    gen(NewG, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, module) ->
    gen_module(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, interface) ->
    gen_interface(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, const) ->
    ic_constant_java:gen(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, op) ->
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, attr) ->
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, except) ->
    gen_exception(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, enum) ->
    ic_enum_java:gen(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, struct) ->
    ic_struct_java:gen(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, union) ->
    ic_union_java:gen(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, typedef) ->
    gen_typedef(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, member) ->
    %%?PRINTDEBUG2("gen member: ~p\n",[ic_forms:get_type(X)]),
    gen_member(G, N, X),
    gen(G, N, Xs);

gen(G, N, [X|Xs]) when is_record(X, case_dcl) ->
    %%?PRINTDEBUG2("gen case decl: ~p\n",[ic_forms:get_type(X)]),
    gen(G, N, [ic_forms:get_type(X)]),
    gen(G, N, Xs);

gen(G, N, [_|Xs]) ->
    gen(G, N, Xs);

gen(_G, _N, []) -> 
    ok.


%%%--------------------------------------------
%%%
%%%   Just generates the directory to host 
%%%   the module files
%%%
%%%--------------------------------------------

gen_module(G, N, X) ->
    case ic_genobj:do_gen(G) of

	true -> %% Generate & register
	    N1 = [ic_forms:get_id2(X) | N],
	    %% Create directory
	    ic_file:createJavaDirectory(G, N1),
	    gen(G, N1, ic_forms:get_body(X));

	false -> %% Register only
	    N1 = [ic_forms:get_id2(X) | N],
	    reg(G, N1, ic_forms:get_body(X))
    end.

reg(G, N, [X|_Xs]) when is_record(X, module) ->
    reg(G, [ic_forms:get_id2(X) | N], ic_forms:get_body(X));

reg(G, N, [X|_Xs]) when is_record(X, interface) ->
    reg(G, [ic_forms:get_id2(X) | N], ic_forms:get_body(X));

reg(G, N, [X|Xs]) when is_record(X, typedef) ->
    Name = ic_util:to_dot(G,[ic_forms:get_java_id(X) | N]),
    case X#typedef.type of 
	{scoped_id,_,_,_} ->
	    {FullScopedName, _, _, _} = 
		ic_symtab:get_full_scoped_name(G, N, X#typedef.type),
	    Type = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
	    ic_code:insert_typedef(G, Name, Type);
	_ ->
	    ok
    end,
    reg(G, N, Xs);

reg(G, N, [_|Xs]) ->
    reg(G, N, Xs);

reg(_G, _N, []) -> 
    ok.



    
%%%----------------------------------------------
%%%
%%%   Generates the interface code
%%%
%%%----------------------------------------------

gen_interface(G, N, X) ->
    case ic_genobj:do_gen(G) of
	true ->
 	    G1 = ic_file:javaInterfaceFilePush(G, N, X),

	    %% Generate Interface file
	    InterfaceFd = ic_genobj:interfacefiled(G1),
	    emit_interface(G1, N, X, InterfaceFd),

	    %% Generate Helper file
	    HelperFd = ic_genobj:helperfiled(G1),
	    emit_helper(G1, N, X, HelperFd),

	    %% Generate Holder file
	    HolderFd = ic_genobj:holderfiled(G1),
	    emit_holder(G1, N, X, HolderFd),

	    %% Generate Stub file
	    StubFd = ic_genobj:stubfiled(G1),
	    emit_stub(G1,N,X,StubFd),  %<--------------------------------------------------- 1

	    %% Generate Skeleton file
	    SkelFd = ic_genobj:skelfiled(G1),
	    emit_skel(G1, N, X, SkelFd),

	    ic_file:javaInterfaceFilePop(G1);
	false ->
	    ok
    end.
	



%%%--------------------------------------------
%%%
%%%   Typedef redirection
%%%
%%%--------------------------------------------

gen_typedef(G, N, X) ->
    Name = ic_util:to_dot(G,[ic_forms:get_java_id(X) | N]),
    case X#typedef.type of 
	{scoped_id,_,_,_} ->
	    {FullScopedName, _, _, _} = 
		ic_symtab:get_full_scoped_name(G, N, X#typedef.type),
	    Type = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
	    ic_code:insert_typedef(G, Name, Type);
	_ ->
	    ok
    end,
    gen_typedef_1(G, N, X, ic_forms:get_body(X)).

gen_typedef_1(G, N, X, Type) when is_record(Type, sequence) ->
    ic_sequence_java:gen(G, N, Type, ic_forms:get_java_id(X));
gen_typedef_1(G, N, X, Type) when is_record(Type, array) ->
    ic_array_java:gen(G, N, X, Type);
gen_typedef_1(G, N, X, _Type) ->
    gen_typedef_2(G, N, X, X#typedef.id), 
    ok.

gen_typedef_2(G, N, X, Type) when is_record(Type, array) ->
    gen_typedef_1(G, N, X, Type);
gen_typedef_2(G, N, X, Type) when is_list(Type) ->
    case Type of
	[] ->
	    ok;
	_ ->
	    gen_typedef_2(G, N, X, hd(Type)),
	    gen_typedef_2(G, N, X, tl(Type))
    end;
%gen_typedef_2(G, N, X, Type) -> %% Generating Helpers for typedef
%                                %% Stoped due to compatibility problems
%                                %% with erl_genserv backend
%    case ic_java_type:isBasicType(G,N,X#typedef.type) of
%	true ->
%	    ok;
%	false ->
%	    case ic_forms:get_type_code(G,N,X#typedef.type) of
%		{'tk_struct', _, _, _} ->
%		    ic_struct_java:gen(G, N, X);
%		{'tk_sequence',_,_} ->
%		    ic_sequence_java:gen(G, N, X, ic_forms:get_java_id(X)),
%		    ok;
%		_ ->
%		    ok
%	    end
%    end;
gen_typedef_2(_G, _N, _X, _Type) ->
    ok.



%%%--------------------------------------------
%%%
%%%   Member redirection
%%%
%%%--------------------------------------------

gen_member(G, N, X) ->
    gen_member_1(G, N, X, [X#member.type]),
    gen_member_2(G, N, X, X#member.id).


gen_member_1(_G, _N, _X, []) ->
    ok;

gen_member_1(G, N, X, [T|Ts]) when is_record(T, sequence) ->
    ic_sequence_java:gen(G, N, T, ic_forms:get_java_id(X)),
    gen_member_1(G, N, X, Ts);

gen_member_1(G, N, X, [T|Ts]) ->
    gen(G,N,[T]),
    gen_member_1(G,N,X,Ts).


gen_member_2(_G, _N, _X, []) ->
    ok;

gen_member_2(G, N, X, [T|Ts]) when is_record(T, array) -> %% BUG !
    ic_array_java:gen(G, N, X, T),
    gen_member_2(G, N, X, Ts);

gen_member_2(G, N, X, [_T|Ts]) ->
    gen_member_2(G, N, X, Ts).



gen_exception(_G, N, X) ->
    io:format("Warning : Exceptions not supported for java mapping, ~p ignored\n",
	     [ic_util:to_colon([ic_forms:get_java_id(X)|N])]),
    ok.



%%%-----------------------------------------------------
%%%
%%%  Interface file generation
%%%
%%%-----------------------------------------------------

emit_interface(G, N, X, Fd) ->
    Interface = ic_forms:get_java_id(X), %% Java Interface Name
    IFCName = ic_forms:get_id2(X), %% Internal Interface Name 

    ic_codegen:emit(Fd, "public interface ~s {\n\n",[Interface]),
    Body = ic_forms:get_body(X),

    %% Generate type declarations inside interface
    gen(G, [IFCName |N], Body),
    
    lists:foreach(fun({_Name, Body1}) ->
			  emit_interface_prototypes(G, [IFCName|N], Body1, Fd) end,
		  [{x, Body} | X#interface.inherit_body]),
	    
    ic_codegen:emit(Fd, "}\n\n").


emit_interface_prototypes(G, N, [X |Xs], Fd) when is_record(X, op) ->

    {_, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParameterTypes, _} = TypeList,

    OpName = ic_forms:get_java_id(X),
    RT = ic_java_type:getParamType(G,N,R,ret),
    PL = ic_util:mk_list(gen_par_list(G, N, X, ParameterTypes,ArgNames)),

    ic_codegen:emit(Fd, "/*\n"),
    ic_codegen:emit(Fd, " * Operation ~p interface functions \n", [ic_util:to_colon([OpName|N])]),
    ic_codegen:emit(Fd, " */\n\n"),

    ic_codegen:emit(Fd, "~s ~s(~s)\n",[RT, OpName, PL]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception;\n\n\n"),

    emit_interface_prototypes(G, N, Xs, Fd);
emit_interface_prototypes(G, N, [X |Xs], Fd) when is_record(X, attr) ->
    ic_attribute_java:emit_attribute_prototype(G, N, X, Fd),
    emit_interface_prototypes(G, N, Xs, Fd);
emit_interface_prototypes(G, N, [_X|Xs], Fd) ->
    emit_interface_prototypes(G, N, Xs, Fd);
emit_interface_prototypes(_G, _N, [], _Fd) -> ok.




%%%-----------------------------------------------------
%%%
%%%  Holder file generation
%%%
%%%-----------------------------------------------------

emit_holder(_G, N, X, Fd) ->
    InterfaceName = ic_forms:get_java_id(X),
    FullInterfaceName = ic_util:to_dot([InterfaceName|N]),

    ic_codegen:emit(Fd, "public final class ~sHolder {\n\n",[InterfaceName]),
    
    ic_codegen:emit(Fd, "    // Instance variable\n"),
    ic_codegen:emit(Fd, "    public ~s value;\n\n",[FullInterfaceName]),

    ic_codegen:emit(Fd, "    // Constructors\n"),
    ic_codegen:emit(Fd, "    public ~sHolder() {\n",[InterfaceName]),
    ic_codegen:emit(Fd, "        this(null);\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public ~sHolder(~s _arg) {\n",[InterfaceName, FullInterfaceName]),
    ic_codegen:emit(Fd, "        value = _arg;\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public void _marshal() {\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public void _unmarshal() {\n"),
    ic_codegen:emit(Fd, "    }\n\n"),
	    
    ic_codegen:emit(Fd, "}\n\n").




%%%-----------------------------------------------------
%%%
%%%  Helper file generation
%%%
%%%-----------------------------------------------------
emit_helper(G, N, X, Fd) ->
    InterfaceName = ic_forms:get_java_id(X),
    FullInterfaceName = ic_util:to_dot([InterfaceName|N]),

    ic_codegen:emit(Fd, "public final class ~sHelper {\n\n",[InterfaceName]),

    ic_codegen:emit(Fd, "    // Constructor\n"),
    ic_codegen:emit(Fd, "    public ~sHelper() {\n",[InterfaceName]),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public static void _marshal() {\n"),
    ic_codegen:emit(Fd, "        // Writing the object to the message\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public static ~s _unmarshal() {\n",[FullInterfaceName]),
    ic_codegen:emit(Fd, "        // Reading the object from the message\n"),
    ic_codegen:emit(Fd, "        return null;\n"),
    ic_codegen:emit(Fd, "    }\n\n"),
    
    ic_codegen:emit(Fd, "    public static java.lang.String id() {\n"),
    ic_codegen:emit(Fd, "        return ~p;\n",[ictk:get_IR_ID(G, N, X)]),
    ic_codegen:emit(Fd, "    }\n\n"),
	    
    ic_codegen:emit(Fd, "}\n\n").




%%%-----------------------------------------------------
%%%
%%%  Stub file generation
%%%
%%%-----------------------------------------------------

emit_stub(G, N, X, Fd) ->
    InterfaceName = ic_forms:get_java_id(X), %% Java Interface Name
    IFCName = ic_forms:get_id2(X),           %% Internal Interface Name
 
    FullInterfaceName = ic_util:to_dot([InterfaceName|N]),
    Body = ic_forms:get_body(X),

    ic_codegen:emit(Fd, "public class _~sStub implements ~s {\n\n",
		    [InterfaceName,FullInterfaceName]),

    ic_codegen:emit(Fd, "    // Client data\n"),
    ic_codegen:emit(Fd, "    public ~sEnvironment _env;\n\n",[?ICPACKAGE]),

    ic_codegen:emit(Fd, "    // Constructors\n"),
    ic_codegen:emit(Fd, "    public _~sStub(~sOtpSelf _self,\n",[InterfaceName,?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "                       ~sOtpPeer _peer,\n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "                       java.lang.Object _server) throws java.lang.Exception {\n\n"),
    
    ic_codegen:emit(Fd, "      _env =\n"),
    ic_codegen:emit(Fd, "         new ~sEnvironment(_self, _peer, _server);\n",[?ICPACKAGE]),
    ic_codegen:emit(Fd, "      _env.connect();\n"),
    ic_codegen:emit(Fd, "    }\n\n"),
    
    ic_codegen:emit(Fd, "    public _~sStub(java.lang.String _selfN,\n",[InterfaceName]),
    ic_codegen:emit(Fd, "                       java.lang.String _peerN,\n"),
    ic_codegen:emit(Fd, "                       java.lang.String _cookie,\n"),
    ic_codegen:emit(Fd, "                       java.lang.Object _server) throws java.lang.Exception {\n\n"),
    ic_codegen:emit(Fd, "      _env =\n"),
    ic_codegen:emit(Fd, "         new ~sEnvironment(_selfN, _peerN, _cookie, _server);\n",[?ICPACKAGE]),
    ic_codegen:emit(Fd, "      _env.connect();\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public _~sStub(~sOtpConnection _connection,\n",[InterfaceName, ?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "                       java.lang.Object _server) throws java.lang.Exception {\n\n"),
    ic_codegen:emit(Fd, "      _env =\n"),
    ic_codegen:emit(Fd, "         new ~sEnvironment(_connection, _server);\n",[?ICPACKAGE]),
    ic_codegen:emit(Fd, "      _env.connect();\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    emit_message_reference_extraction(Fd),

    emit_servers_object_access(Fd),

    emit_client_connection_close(Fd),

    emit_client_connection_reconnect(Fd),

    emit_client_destroy(Fd),

    lists:foreach(fun({_Name, Body1}) ->
			  emit_op_implementation(G, [IFCName|N], Body1, Fd) end,
		  [{x, Body} | X#interface.inherit_body]),
    
    ic_codegen:emit(Fd, "}\n\n").


emit_op_implementation(G, N, [X |Xs], Fd) when is_record(X, op) ->

    WireOpName = ic_forms:get_id2(X),
    OpName = ic_forms:get_java_id(WireOpName),
    {_, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParamTypes, _} = TypeList,
    
    RT = ic_java_type:getParamType(G,N,R,ret),
    PL = ic_util:mk_list(gen_par_list(G, N, X, ParamTypes, ArgNames)),
    CMCPL = ic_util:mk_list(gen_client_marshal_call_par_list(ArgNames)),

    ic_codegen:emit(Fd, "    // Operation ~p implementation\n", [ic_util:to_colon([WireOpName|N])]),
    ic_codegen:emit(Fd, "    public ~s ~s(~s)\n", [RT, OpName, PL]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),

    %% Function marshal call
    ic_codegen:emit(Fd, "      // Calling the marshal function\n"),

    case CMCPL of
	"" ->
	    ic_codegen:emit(Fd, "      _~s_marshal(_env);\n\n",[OpName]);
	_ ->
	    ic_codegen:emit(Fd, "      _~s_marshal(_env, ~s);\n\n",[OpName, CMCPL])
    end,

    %% Sending call
    ic_codegen:emit(Fd, "      // Message send\n"),
    ic_codegen:emit(Fd, "      _env.send();\n\n"),

    case ic_forms:is_oneway(X) of
	true ->
	    ok;
	false ->
	    %% Receiving return values
	    ic_codegen:emit(Fd, "      // Message receive\n"),
	    ic_codegen:emit(Fd, "      _env.receive();\n\n"),
	    
	    %% Function unmarshal call
	    case RT of
		"void" ->
		    case ic_util:mk_list(gen_client_unmarshal_call_par_list(ArgNames)) of
			"" ->
			    ic_codegen:emit(Fd, "      // Calling the unmarshal function\n"),
			    ic_codegen:emit(Fd, "      _~s_unmarshal(_env);\n",
					    [OpName]);
			UMCPL ->
			    ic_codegen:emit(Fd, "      // Calling the unmarshal function\n"),
			    ic_codegen:emit(Fd, "      _~s_unmarshal(_env, ~s);\n",
					    [OpName,UMCPL])
		    end;
		_ ->
		    ic_codegen:emit(Fd, "      // Calling the unmarshal function\n"),
		    case ic_util:mk_list(gen_client_unmarshal_call_par_list(ArgNames)) of
			"" ->
			    ic_codegen:emit(Fd, "      return _~s_unmarshal(_env);\n",
					    [OpName]);
			UMCPL ->
			    ic_codegen:emit(Fd, "      return _~s_unmarshal(_env, ~s);\n",
					    [OpName,UMCPL])
		    end
	    end
    end,
    ic_codegen:emit(Fd, "    }\n\n"),

    %% Marshalling
    emit_op_marshal(G, N, X, Fd),

    %% UnMarshalling
    emit_op_unmarshal(G, N, X, Fd),
    ic_codegen:emit(Fd, "\n"),

    emit_op_implementation(G, N, Xs, Fd);
emit_op_implementation(G, N, [X |Xs], Fd) when is_record(X, attr) ->
    ic_attribute_java:emit_attribute_stub_code(G, N, X, Fd),
    emit_op_implementation(G, N, Xs, Fd);
emit_op_implementation(G, N, [_X|Xs], Fd) ->
    emit_op_implementation(G, N, Xs, Fd);
emit_op_implementation(_G, _N, [], _Fd) -> ok.





%%---------------------------------------
%%
%%  Marshal operation generation
%%
%%---------------------------------------

emit_op_marshal(G, N, X, Fd) ->
    WireOpName = ic_forms:get_id2(X),
    OpName = ic_forms:get_java_id(WireOpName),
    {_, ArgNames, TypeList} = extract_info(G, N, X),
    {_R, ParamTypes, _} = TypeList,
    
    PL = ic_util:mk_list(gen_marshal_par_list(G, N, X, ParamTypes, ArgNames)),
    
    ic_codegen:emit(Fd, "    // Marshal operation for ~p\n", [OpName]),
    case PL of
	"" ->
	    ic_codegen:emit(Fd, "    public static void _~s_marshal(~sEnvironment __env)\n", 
			    [OpName, ?ICPACKAGE]),
	    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"); 
	_ ->
	    ic_codegen:emit(Fd, "    public static void _~s_marshal(~sEnvironment __env, ~s)\n", 
			    [OpName, ?ICPACKAGE, PL]),
	    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n")
    end,
    %% Message encoding
    emit_op_encode(G, N, X, OpName, WireOpName, ParamTypes, ArgNames, Fd),

    ic_codegen:emit(Fd, "    }\n\n").


emit_op_encode(G, N, X, _OpN, WOpN, ParamTypes, ArgNames, Fd) ->

    OpCallName = case ic_options:get_opt(G, scoped_op_calls) of 
		     true -> 
			 ic_util:to_undersc([WOpN|N]);
		     false ->
			 WOpN
		 end,
    
    SendParamNr = count_client_send(ArgNames),

    ic_codegen:emit(Fd, "      ~sOtpOutputStream __os = __env.getOs();\n\n",
		    [?ERLANGPACKAGE]),

    case ic_forms:is_oneway(X) of
	true ->
	    %% Initiating call tuple
	    ic_codegen:emit(Fd, "      // Message header assembly\n"),
	    ic_codegen:emit(Fd, "      __os.reset();\n"),
	    ic_codegen:emit(Fd, "      __os.write_tuple_head(2);\n"),
	    ic_codegen:emit(Fd, "      __os.write_atom(\"$gen_cast\");\n\n");
	false ->
	    %% Initiating call tuple
	    ic_codegen:emit(Fd, "      // Message header assembly\n"),
	    ic_codegen:emit(Fd, "      __os.reset();\n"),
	    ic_codegen:emit(Fd, "      __os.write_tuple_head(3);\n"),
	    ic_codegen:emit(Fd, "      __os.write_atom(\"$gen_call\");\n\n"),

	    %% Initiating call identity tuple
	    ic_codegen:emit(Fd, "      // Message identity part creation\n"),
	    ic_codegen:emit(Fd, "      __os.write_tuple_head(2);\n"),
	    ic_codegen:emit(Fd, "      __env.write_client_pid();\n"),
	    ic_codegen:emit(Fd, "      __env.write_client_ref();\n\n")
    end,
    
    %% Operation part initializations
    case SendParamNr > 0 of
	true ->
	    ic_codegen:emit(Fd, "      // Operation attribute creation\n"),
	    ic_codegen:emit(Fd, "      __os.write_tuple_head(~p);\n", [SendParamNr+1]),
	    ic_codegen:emit(Fd, "      __os.write_atom(~p);\n", [OpCallName]),
	    emit_op_encode_loop(G, N, X, ParamTypes, ArgNames, 1, Fd);
	false -> %% No in/inout paramaters
	    ic_codegen:emit(Fd, "      __os.write_atom(~p);\n", [OpCallName])
    end.



emit_op_encode_loop(_,_,_,_,[],_,_Fd) ->
    ok;
emit_op_encode_loop(G, N, X, [_Type|Types],[{out, _Arg}|Args], Counter, Fd) ->
    emit_op_encode_loop(G, N, X, Types, Args, Counter, Fd);
emit_op_encode_loop(G, N, X, [Type|Types], [{inout, Arg}|Args], Counter, Fd) ->
    case ic_java_type:isBasicType(G, N, Type) of
	true ->
	    ic_codegen:emit(Fd, "      __os~s(~s.value);\n",
			    [ic_java_type:marshalFun(G, N, X, Type),Arg]);
	false ->
	    ic_codegen:emit(Fd, "      ~s(__os, ~s.value);\n",
			    [ic_java_type:marshalFun(G, N, X, Type),Arg])
    end,
    emit_op_encode_loop(G, N, X, Types, Args, Counter+1, Fd);
emit_op_encode_loop(G, N, X, [Type|Types], [{in, Arg}|Args], Counter, Fd) ->
    case ic_java_type:isBasicType(G, N, Type) of
	true ->
	    ic_codegen:emit(Fd, "      __os~s(~s);\n",
			    [ic_java_type:marshalFun(G, N, X, Type),Arg]);
	false ->
	    ic_codegen:emit(Fd, "      ~s(__os, ~s);\n",
			    [ic_java_type:marshalFun(G, N, X, Type),Arg])
    end,
    emit_op_encode_loop(G, N, X, Types, Args, Counter+1, Fd).






%%-------------------------------------
%%
%%  UnMarshal operation generation
%%
%%-------------------------------------

emit_op_unmarshal(G, N, X, Fd) ->
    case ic_forms:is_oneway(X) of
	true ->
	    ok;
	false ->
	    OpName = ic_forms:get_java_id(X),
	    {_, ArgNames, TypeList} = extract_info(G, N, X),
	    {R, ParamTypes, _} = TypeList,
    
	    RT = ic_java_type:getParamType(G,N,R,ret),
	    PL = ic_util:mk_list(gen_unmarshal_par_list(G, N, X, ParamTypes, ArgNames)),

	    case PL of
		"" ->
		    case RT of
			"void" ->
			    ic_codegen:emit(Fd, "    // Unmarshal operation for ~p\n", [OpName]),
			    ic_codegen:emit(Fd, "    public static void _~s_unmarshal(~sEnvironment __env)\n", 
					    [OpName, ?ICPACKAGE]),
			    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),
			    ic_codegen:emit(Fd, "        __env.getIs().read_atom();\n"),
			    ic_codegen:emit(Fd, "    }\n\n");
			_ ->
			    ic_codegen:emit(Fd, "    // Unmarshal operation for ~p\n", [OpName]),
			    ic_codegen:emit(Fd, "    public static ~s _~s_unmarshal(~sEnvironment __env)\n", 
					    [RT, OpName, ?ICPACKAGE]),
			    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),

			    ic_codegen:emit(Fd, "      // Get input stream\n"),
			    ic_codegen:emit(Fd, "      ~sOtpInputStream __is = __env.getIs();\n\n",
					    [?ERLANGPACKAGE]),

			    emit_op_decode(G, N, X, R, RT, ParamTypes, ArgNames, Fd),
			    ic_codegen:emit(Fd, "    }\n\n")
		    end;
		_ ->     
		    ic_codegen:emit(Fd, "    // Unmarshal operation for ~p\n", [OpName]),
		    ic_codegen:emit(Fd, "    public static ~s _~s_unmarshal(~sEnvironment __env, ~s)\n", 
				    [RT, OpName, ?ICPACKAGE, PL]),
		    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),
		    
		    ic_codegen:emit(Fd, "      // Get input stream\n"),
		    ic_codegen:emit(Fd, "      ~sOtpInputStream __is = __env.getIs();\n\n",
				    [?ERLANGPACKAGE]),

		    emit_op_decode(G, N, X, R, RT, ParamTypes, ArgNames, Fd),
		    ic_codegen:emit(Fd, "    }\n\n")
	    end
    end.


emit_op_decode(G, N, X, R, RT, ParamTypes, ArgNames, Fd) ->
    ReceiveNr = count_client_receive(ArgNames),
    
    case RT of 
	"void" ->
	    case ReceiveNr > 0 of
		true ->		    
		    ic_codegen:emit(Fd, "      // Extracting output values\n"),
		    ic_codegen:emit(Fd, "      __is.read_tuple_head();\n"),
		    ic_codegen:emit(Fd, "      __is.read_atom();\n"),
		    emit_op_decode_loop(G, N, X, ParamTypes, ArgNames, 1, Fd);		
		false ->
		    ic_codegen:emit(Fd, "      __is.read_atom();\n")
	    end;
	_ ->
	    case ReceiveNr > 0 of
		true ->
		    ic_codegen:emit(Fd, "      // Extracting return/output values\n"),
		    ic_codegen:emit(Fd, "      __is.read_tuple_head();\n"),
		    case ic_java_type:isBasicType(G,N,R) of
			true ->
			    ic_codegen:emit(Fd, "      ~s _result = __is~s;\n",
					    [RT,ic_java_type:unMarshalFun(G, N, X, R)]);
			false ->
			    ic_codegen:emit(Fd, "      ~s _result = ~s.unmarshal(__is);\n",
					    [RT, ic_java_type:getUnmarshalType(G,N,X,R)])
		    end,
		    emit_op_decode_loop(G, N, X, ParamTypes, ArgNames, 1, Fd),

		    ic_codegen:nl(Fd),
		    ic_codegen:emit(Fd, "      return _result;\n");
		false ->
		    ic_codegen:emit(Fd, "      // Extracting return value\n"),
		    case ic_java_type:isBasicType(G,N,R) of
			true ->
			    ic_codegen:emit(Fd, "      return __is~s;\n",
					    [ic_java_type:unMarshalFun(G, N, X, R)]);
			false ->
			    ic_codegen:emit(Fd, "      return ~s.unmarshal(__is);\n",
					    [ic_java_type:getUnmarshalType(G,N,X,R)])
		    end
	    end
    end.

emit_op_decode_loop(_,_,_,_,[],_,_Fd) ->
    ok;
emit_op_decode_loop(G, N, X, [_Type|Types], [{in, _Arg}|Args], Counter, Fd) ->
    emit_op_decode_loop(G, N, X, Types, Args, Counter, Fd);
emit_op_decode_loop(G, N, X, [Type|Types], [{_, Arg}|Args], Counter, Fd) ->
    case ic_java_type:isBasicType(G,N,Type) of
	true ->
	    ic_codegen:emit(Fd, "      ~s.value = __is~s;\n",
			    [Arg,
			     ic_java_type:unMarshalFun(G, N, X, Type)]);
	false ->
	    ic_codegen:emit(Fd, "      ~s.value = ~s.unmarshal(__is);\n",
			    [Arg,
			     ic_java_type:getUnmarshalType(G, N, X, Type)])
    end,
    emit_op_decode_loop(G, N, X, Types, Args, Counter+1, Fd).



emit_message_reference_extraction(Fd) ->
    ic_codegen:emit(Fd, "    // Returns call reference\n"),
    ic_codegen:emit(Fd, "    public ~sOtpErlangRef __getRef()\n",
		   [?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n"),        
    ic_codegen:emit(Fd, "        return _env.received_ref();\n"),
    ic_codegen:emit(Fd, "    }\n\n").

emit_servers_object_access(Fd) ->
    ic_codegen:emit(Fd, "    // Returns the server\n"),
    ic_codegen:emit(Fd, "    public java.lang.Object __server() {\n"),
    ic_codegen:emit(Fd, "      return _env.server();\n"),
    ic_codegen:emit(Fd, "    }\n\n").

emit_client_connection_close(Fd) ->
    ic_codegen:emit(Fd, "    // Closes connection\n"),
    ic_codegen:emit(Fd, "    public void __disconnect() {\n"),
    ic_codegen:emit(Fd, "      _env.disconnect();\n"),
    ic_codegen:emit(Fd, "    }\n\n").

emit_client_connection_reconnect(Fd) ->
    ic_codegen:emit(Fd, "    // Reconnects client\n"),
    ic_codegen:emit(Fd, "    public void __reconnect()\n"),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n"),
    ic_codegen:emit(Fd, "      _env.reconnect();\n"),
    ic_codegen:emit(Fd, "    }\n\n").

emit_client_destroy(Fd) ->
    ic_codegen:emit(Fd, "    // Destroy server\n"),
    ic_codegen:emit(Fd, "    public void __stop()\n"),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n"),
    ic_codegen:emit(Fd, "      _env.client_stop_server();\n"),
    ic_codegen:emit(Fd, "    }\n\n").



%%%----------------------------------------------------
%%%
%%%   Generates the server code
%%%
%%%----------------------------------------------------

emit_skel(G, N, X, Fd) ->
    InterfaceName = ic_forms:get_java_id(X),
    FullInterfaceName = ic_util:to_dot([InterfaceName|N]),

    ic_codegen:emit(Fd, "public abstract class _~sImplBase implements ~s {\n\n",
		    [InterfaceName,FullInterfaceName]),

    ic_codegen:emit(Fd, "    // Server data\n"),
    ic_codegen:emit(Fd, "    protected ~sEnvironment _env = null;\n\n",[?ICPACKAGE]),

    ic_codegen:emit(Fd, "    // Constructors\n"),
    ic_codegen:emit(Fd, "    public _~sImplBase() {\n",[InterfaceName]),
    ic_codegen:emit(Fd, "    }\n\n"),

    emit_caller_pid(G, N, X, Fd), 

    %% Emit operation dictionary
    emit_dictionary(G, N, X, Fd),

    %% Emit server switch
    emit_server_switch(G, N, X, Fd), 
    
    ic_codegen:emit(Fd, "}\n").


emit_server_switch(G, N, X, Fd) ->
    
    IFCName = ic_forms:get_id2(X),           %% Internal Interface Name 
    Body = ic_forms:get_body(X),
    Counter = 0,

    ic_codegen:emit(Fd, "    // Operation invokation\n"),
    ic_codegen:emit(Fd, "    public ~sOtpOutputStream invoke(~sOtpInputStream _in)\n",
		    [?ERLANGPACKAGE,?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),

    ic_codegen:emit(Fd, "       // Create a new environment if needed\n"),
    ic_codegen:emit(Fd, "      	if (_env == null)\n"),
    ic_codegen:emit(Fd, "      	  _env = new com.ericsson.otp.ic.Environment();\n\n"),

    ic_codegen:emit(Fd, "       // Unmarshal head\n"),
    ic_codegen:emit(Fd, "      	_env.uHead(_in);\n\n"),
    
    ic_codegen:emit(Fd, "      	// Switch over operation\n"),
    ic_codegen:emit(Fd, "      	return __switch(_env);\n"),

    ic_codegen:emit(Fd, "    }\n\n"),
    

    ic_codegen:emit(Fd, "     // Operation switch\n"),  
    ic_codegen:emit(Fd, "     public ~sOtpOutputStream __switch(~sEnvironment __env)\n", [?ERLANGPACKAGE,?ICPACKAGE]),     
    ic_codegen:emit(Fd, "       throws java.lang.Exception {\n\n"),
 
    ic_codegen:emit(Fd, "       // Setup streams and operation label\n"), 
    ic_codegen:emit(Fd, "       ~sOtpOutputStream __os = __env.getOs();\n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "       __os.reset();\n"),
    ic_codegen:emit(Fd, "       int __label = __env.uLabel(__operations);\n\n"),
    
    ic_codegen:emit(Fd, "       // Switch over operation\n"),     
    ic_codegen:emit(Fd, "       switch(__label) {\n\n"),

    OpNr = emit_server_op_switch_loop(G,                
				      [IFCName|N], 
				      [{x, Body} | X#interface.inherit_body], 
				      Counter, 
				      Fd),
    
    ic_codegen:emit(Fd, "       case ~p: { // Standard stop operation\n\n",[OpNr]),
    ic_codegen:emit(Fd, "         __env.server_stop_server();\n\n"),
    ic_codegen:emit(Fd, "       } break;\n\n"),
    
    ic_codegen:emit(Fd, "       default: // It will never come down here \n"),
    ic_codegen:emit(Fd, "         throw new java.lang.Exception(\"BAD OPERATION\");\n\n", []),
   
    ic_codegen:emit(Fd, "      }\n\n"),

    ic_codegen:emit(Fd, "      if(__os.count() > 0)\n"),
    ic_codegen:emit(Fd, "        return __os;\n\n"),
    
    ic_codegen:emit(Fd, "      return null;\n"),
    ic_codegen:emit(Fd, "    }\n\n").



emit_server_op_switch_loop(_G, _N, [], C, _Fd) ->
    C;
emit_server_op_switch_loop(G, N, [{_,X}|Xs], C, Fd) ->
    C1 = emit_server_op_switch(G, N, X, C, Fd),
    emit_server_op_switch_loop(G, N, Xs, C1, Fd).


emit_server_op_switch(G, N, [X|Xs], C, Fd) when is_record(X, op) ->

    OpName = ic_forms:get_java_id(X),

    ic_codegen:emit(Fd, "       case ~p:  {  // Operation ~s\n\n",[C,ic_util:to_dot([OpName|N])]),
    
    emit_invoke(G, N, X, Fd),  
    
    ic_codegen:emit(Fd, "       } break;\n\n"),

    emit_server_op_switch(G, N, Xs, C+1, Fd);
emit_server_op_switch(G, N, [X |Xs], C, Fd) when is_record(X, attr) -> 
    C1 = ic_attribute_java:emit_attribute_switch_case(G,N,X,Fd,C),
    emit_server_op_switch(G, N, Xs, C1, Fd);
emit_server_op_switch(G, N, [_X|Xs], C, Fd) ->
    emit_server_op_switch(G, N, Xs, C, Fd);
emit_server_op_switch(_G, _N, [], C, _Fd) -> 
    C.


emit_caller_pid(_G, _N, _X, Fd) ->
    ic_codegen:emit(Fd, "    // Extracts caller identity\n"),
    ic_codegen:emit(Fd, "    public ~sOtpErlangPid __getCallerPid() {\n", [?ERLANGPACKAGE]),    
    ic_codegen:emit(Fd, "      return _env.getScaller();\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public ~sOtpErlangPid __getCallerPid(~sEnvironment __env) {\n", 
		    [?ERLANGPACKAGE, ?ICPACKAGE]),    
    ic_codegen:emit(Fd, "      return __env.getScaller();\n"),
    ic_codegen:emit(Fd, "    }\n\n"),
    
    ic_codegen:emit(Fd, "    public boolean __isStopped() {\n"),    
    ic_codegen:emit(Fd, "      return _env.isStopped();\n"),
    ic_codegen:emit(Fd, "    }\n\n"),

    ic_codegen:emit(Fd, "    public boolean __isStopped(~sEnvironment __env) {\n",
		    [?ICPACKAGE]),    
    ic_codegen:emit(Fd, "      return __env.isStopped();\n"),
    ic_codegen:emit(Fd, "    }\n\n").
    


%% Creates an operation dictionary
emit_dictionary(G, N, X, Fd) ->

    Counter = 0,
    Body = ic_forms:get_body(X),

    ic_codegen:emit(Fd, "    // Operation dictionary\n"),
    ic_codegen:emit(Fd, "    private static java.util.Dictionary __operations = new java.util.Hashtable();\n"),
    ic_codegen:emit(Fd, "    static {\n"),

    emit_dictionary_loop(G, 
			 [ic_forms:get_id2(X)|N],
			 [{x, Body} | X#interface.inherit_body], 
			 Counter, 
			 Fd),
        
    ic_codegen:emit(Fd, "    }\n\n"),
    
    ic_codegen:emit(Fd, "    // Operation dictionary access\n"),
    ic_codegen:emit(Fd, "    public static java.util.Dictionary __operations() {\n"),
    ic_codegen:emit(Fd, "      return __operations;\n"),
    ic_codegen:emit(Fd, "    }\n\n").




emit_dictionary_loop(_G, _N, [], C, Fd) ->
    ic_codegen:emit(Fd, "      __operations.put(~p, new java.lang.Integer(~p));\n",
		    ["stop",C]);
emit_dictionary_loop(G, N, [{_,X}|Xs], C, Fd) ->
    C1 = emit_dictionary(G, N, X, C, Fd),
    emit_dictionary_loop(G, N, Xs, C1, Fd).


emit_dictionary(G, N, [X|Xs], C, Fd) when is_record(X, op) ->

    OpName = case ic_options:get_opt(G, scoped_op_calls) of 
		 true -> 
		     ic_util:to_undersc([ic_forms:get_id2(X)|N]);
		 false ->
		     ic_forms:get_id2(X)
	     end,
    
    ic_codegen:emit(Fd, "      __operations.put(~p, new java.lang.Integer(~p));\n",
		    [OpName,C]),
    emit_dictionary(G, N, Xs, C+1, Fd);

emit_dictionary(G, N, [X |Xs], C, Fd) when is_record(X, attr) ->
    C1 = ic_attribute_java:emit_atrribute_on_dictionary(G, N, X, Fd, C),
    emit_dictionary(G, N, Xs, C1, Fd);

emit_dictionary(G, N, [_X|Xs], C, Fd) ->
    emit_dictionary(G, N, Xs, C, Fd);

emit_dictionary(_G, _N, [], C, _Fd) -> 
    C.



emit_invoke(G, N, X, Fd) ->

    {_, ArgNames, TypeList} = extract_info(G, N, X),
    {R, ParamTypes, _} = TypeList,
    OpName = ic_forms:get_java_id(X),
    RT = ic_java_type:getParamType(G,N,R,ret),
    PL = ic_util:mk_list(gen_cb_arg_list(ArgNames)),
    OutParamNr = count_server_send(ArgNames),

    case count_server_receive(ArgNames) of
	0 ->
	    ok;
	_C ->
	    ic_codegen:emit(Fd, "          // Preparing input\n"),
	    ic_codegen:emit(Fd, "          ~sOtpInputStream __is = __env.getIs();\n",
			    [?ERLANGPACKAGE]),
	    emit_server_unmarshal_loop(G, N, X, ParamTypes, ArgNames, 1, Fd)  
    end,
    
    ic_codegen:emit(Fd, "          // Calling implementation function\n"),
    case RT of
	"void" ->
	    ic_codegen:emit(Fd, "          this.~s(~s);\n\n", 
			    [OpName,PL]);
	_ ->
	    ic_codegen:emit(Fd, "          ~s _result = this.~s(~s);\n\n", 
			    [RT, OpName, PL])
    end,
    
    case ic_forms:is_oneway(X) of
	true ->
	    ok;
	false ->
	    ic_codegen:emit(Fd, "          // Marshaling output\n"), 
	    ic_codegen:emit(Fd, "          ~sOtpErlangRef __ref = __env.getSref();\n",[?ERLANGPACKAGE]),
 
	    case RT of 
		"void" ->
		    case OutParamNr > 0 of
			true ->
			    ic_codegen:emit(Fd, "          __os.write_tuple_head(2);\n"),
			    ic_codegen:emit(Fd, "          __os.write_ref(__ref.node(),__ref.ids(),__ref.creation());  // Call reference\n"),
			    ic_codegen:emit(Fd, "          __os.write_tuple_head(~p);\n",[OutParamNr+1]),
			    ic_codegen:emit(Fd, "          __os.write_atom(\"ok\");\n"),
			    emit_server_marshal_loop(G, N, X, ParamTypes,ArgNames,1,Fd);
			false ->
			    ic_codegen:emit(Fd, "          __os.write_tuple_head(2);\n"),
			    ic_codegen:emit(Fd, "          __os.write_ref(__ref.node(),__ref.ids(),__ref.creation());  // Call reference\n"),
			    ic_codegen:emit(Fd, "          __os.write_atom(\"ok\");\n\n")
		    end;
		_ ->
		    case OutParamNr > 0 of
			true ->
			    ic_codegen:emit(Fd, "          __os.write_tuple_head(2);\n"),
			    ic_codegen:emit(Fd, "          __os.write_ref(__ref.node(),__ref.ids(),__ref.creation());  // Call reference\n"),
			    ic_codegen:emit(Fd, "          __os.write_tuple_head(~p);\n",[OutParamNr+1]),
	     
			    case ic_java_type:isBasicType(G,N,R) of
				true ->
				    ic_codegen:emit(Fd, "          __os~s(_result);  // Return value\n", 
						    [ic_java_type:marshalFun(G,N,X,R)]);
				false ->
				    ic_codegen:emit(Fd, "          ~s(__os,_result);  // Return value\n", 
						    [ic_java_type:marshalFun(G,N,X,R)])
			    end,
			    emit_server_marshal_loop(G, N, X, ParamTypes,ArgNames,1,Fd);
			false ->
			    ic_codegen:emit(Fd, "          __os.write_tuple_head(2);\n"),
			    ic_codegen:emit(Fd, "          __os.write_ref(__ref.node(),__ref.ids(),__ref.creation());  // Call reference\n"),

			    case ic_java_type:isBasicType(G,N,R) of
				true ->
				    ic_codegen:emit(Fd, "          __os~s(_result);  // Return value\n\n", 
						    [ic_java_type:marshalFun(G,N,X,R)]);
				false ->
				    ic_codegen:emit(Fd, "          ~s(__os,_result);  // Return value\n\n", 
						    [ic_java_type:marshalFun(G,N,X,R)])
			    end
		    end
	    end,
	    ic_codegen:nl(Fd)
    end.


emit_server_unmarshal_loop(_,_,_,_,[],_,Fd) ->
    ic_codegen:nl(Fd);
emit_server_unmarshal_loop(G, N, X, [Type|Types], [{in, Arg}|Args], Counter, Fd) ->
    case ic_java_type:isBasicType(G,N,Type) of
	true ->
	    ic_codegen:emit(Fd, "          ~s ~s = __is~s;  // In value\n",
			    [ic_java_type:getType(G,N,Type),
			     Arg,
			     ic_java_type:unMarshalFun(G,N,X,Type)]); 
	false -> 
	    ic_codegen:emit(Fd, "          ~s ~s = ~s.unmarshal(__is);  // In value\n",
			    [ic_java_type:getType(G,N,Type), 
			     Arg, 
			     ic_java_type:getUnmarshalType(G,N,X,Type)])
    end,
    emit_server_unmarshal_loop(G, N, X, Types, Args, Counter+1, Fd);
emit_server_unmarshal_loop(G, N, X, [Type|Types],[{inout, Arg}|Args], Counter, Fd) -> 
    Holder = ic_java_type:getHolderType(G,N,Type),
    case ic_java_type:isBasicType(G,N,Type) of
	true ->
%	    OtpEncVar = ic_java_type:getUnmarshalType(G,N,X,Type),
	    ic_codegen:emit(Fd, "          ~s _~s = __is~s;\n",
			    [ic_java_type:getType(G,N,Type),
			     Arg,
			     ic_java_type:unMarshalFun(G,N,X,Type)]),
	    ic_codegen:emit(Fd, "          ~s ~s = new ~s(_~s);  // InOut value\n",
			    [Holder, 
			     Arg, 
			     Holder,
			     Arg]);
	false ->
	    ic_codegen:emit(Fd, "          ~s ~s = new ~s();  // InOut value\n",
			    [Holder, 
			     Arg, 
			     Holder]),
	    ic_codegen:emit(Fd, "          ~s._unmarshal(__is);\n",
			    [Arg])
    end,
    emit_server_unmarshal_loop(G, N, X, Types, Args, Counter+1, Fd);
emit_server_unmarshal_loop(G, N, X, [Type|Types],[{out, Arg}|Args], Counter, Fd) ->
    Holder = ic_java_type:getHolderType(G,N,Type),
    ic_codegen:emit(Fd, "          ~s ~s = new ~s();  // Out value\n", [Holder, Arg, Holder]),
    emit_server_unmarshal_loop(G, N, X, Types, Args, Counter, Fd).


emit_server_marshal_loop(_,_,_,_,[],_,_Fd) ->
    ok;
emit_server_marshal_loop(G, N, X, [_Type|Types],[{in, _Arg}|Args], Counter, Fd) ->
    emit_server_marshal_loop(G, N, X, Types, Args, Counter, Fd);
emit_server_marshal_loop(G, N, X, [Type|Types],[{_, Arg}|Args], Counter, Fd) -> 
%    Holder = ic_java_type:getHolderType(G,N,Type),
    case ic_java_type:isBasicType(G,N,Type) of
	true ->
	    ic_codegen:emit(Fd, "          __os~s(~s.value);  // Out/InOut value\n", 
			    [ic_java_type:marshalFun(G,N,X,Type),Arg]);
	false ->
	    ic_codegen:emit(Fd, "          ~s._marshal(__os);  // Out/InOut value\n", 
			    [Arg])
    end,
    emit_server_marshal_loop(G, N, X, Types, Args, Counter+1, Fd).





%%%----------------------------------------------------
%%%
%%%   Utilities
%%%
%%%----------------------------------------------------

extract_info(_G, N, X) when is_record(X, op) ->
    Name	=  ic_util:to_undersc([ic_forms:get_id2(X) | N]),
    Args	= X#op.params,
    ArgNames	= mk_c_vars(Args),
    TypeList	= {ic_forms:get_type(X),
		   lists:map(fun(Y) -> ic_forms:get_type(Y) end, Args),
		   []
		  },
    {Name, ArgNames, TypeList};
extract_info(_G, N, X) ->
    Name	=  ic_util:to_undersc([ic_forms:get_id2(X) | N]),
    {Name, [], []}.

%% Input is a list of parameters (in parse form) and output is a list
%% of parameter attribute and variable names.
mk_c_vars(Params) ->
    lists:map(fun(P) -> {A, _} = P#param.inout,
			{A, ic_forms:get_id(P#param.id)}
	      end,
	      Params).

%%
handle_preproc(G, _N, line_nr, X) ->
    Id = ic_forms:get_java_id(X),
    Flags = X#preproc.aux,
    case Flags of
	[] -> ic_genobj:push_file(G, Id);
	_ ->
	    lists:foldr(fun({_, _, "1"}, Gprim) -> ic_genobj:push_file(Gprim, Id);
			   ({_, _, "2"}, Gprim) -> ic_genobj:pop_file(Gprim, Id);
			   ({_, _, "3"}, Gprim) -> ic_genobj:sys_file(Gprim, Id) end,
			G, Flags)
    end;
handle_preproc(G, _N, _Other, _X) ->
    G.


%%
gen_par_list(_, _, _, [], []) ->
    [];
gen_par_list(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    JType = ic_java_type:getParamType(G, N, Type, Attr),
    [JType ++ " " ++ Arg | 
     gen_par_list(G, N, X, Types, Args)].


gen_marshal_par_list(_, _, _, [], []) ->
    [];
gen_marshal_par_list(G, N, X, [_Type |Types], [{out, _Arg}|Args]) ->
    gen_marshal_par_list(G, N, X, Types, Args);
gen_marshal_par_list(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    JType = ic_java_type:getParamType(G, N, Type, Attr),
    [JType ++ " " ++ Arg | 
     gen_marshal_par_list(G, N, X, Types, Args)].


gen_unmarshal_par_list(_, _, _, [], []) ->
    [];
gen_unmarshal_par_list(G, N, X, [_Type |Types], [{in, _Arg}|Args]) ->
    gen_unmarshal_par_list(G, N, X, Types, Args);
gen_unmarshal_par_list(G, N, X, [Type |Types], [{Attr, Arg}|Args]) ->
    JType = ic_java_type:getParamType(G, N, Type, Attr),
    [JType ++ " " ++ Arg | 
     gen_unmarshal_par_list(G, N, X, Types, Args)].


%%
gen_client_marshal_call_par_list([]) ->
    [];
gen_client_marshal_call_par_list([{out, _Arg}|Args]) ->
    gen_client_marshal_call_par_list(Args);
gen_client_marshal_call_par_list([{_Attr, Arg}|Args]) ->
    [Arg | gen_client_marshal_call_par_list(Args)].


gen_client_unmarshal_call_par_list([]) ->
    [];
gen_client_unmarshal_call_par_list([{in, _Arg}|Args]) ->
    gen_client_unmarshal_call_par_list(Args);
gen_client_unmarshal_call_par_list([{_Attr, Arg}|Args]) ->
    [Arg | gen_client_unmarshal_call_par_list(Args)].



count_client_receive(ArgNames) ->
    count_client_receive(ArgNames,0).

count_client_receive([],C) ->
    C;
count_client_receive([{in, _Arg}|Args],C) ->
    count_client_receive(Args,C);
count_client_receive([_|Args],C) ->
    count_client_receive(Args,C+1).



count_client_send(ArgNames) ->
    count_client_send(ArgNames,0).

count_client_send([],C) ->
    C;
count_client_send([{out, _Arg}|Args],C) ->
    count_client_send(Args,C);
count_client_send([_|Args],C) ->
    count_client_send(Args,C+1).


gen_cb_arg_list([]) ->
    [];
gen_cb_arg_list([{_Attr, Arg}|Args]) ->
    [Arg | gen_cb_arg_list(Args)].


count_server_receive(ArgNames) ->
    count_server_receive(ArgNames,0).

count_server_receive([],C) ->
    C;
count_server_receive([_|Args],C) ->
    count_server_receive(Args,C+1).


count_server_send(ArgNames) ->
    count_server_send(ArgNames,0).

count_server_send([],C) ->
    C;
count_server_send([{in, _Arg}|Args],C) ->
    count_server_send(Args,C);
count_server_send([_|Args],C) ->
    count_server_send(Args,C+1).





%%%-------------------------------------------------------


emit_type_function(G, N, X, Fd) ->

    TC = ic_forms:get_type_code(G, N, X),

    %%io:format("X = ~p\nTC = ~p\n",[X,TC]),

    ic_codegen:emit(Fd, "   private static ~sTypeCode _tc;\n",[?ICPACKAGE]),
    ic_codegen:emit(Fd, "   synchronized public static ~sTypeCode type() {\n\n",[?ICPACKAGE]),

    ic_codegen:emit(Fd, "     if (_tc != null)\n"),
    ic_codegen:emit(Fd, "       return _tc;\n\n"),

    emit_type_function(TC, 0, Fd),
    
    ic_codegen:emit(Fd, "\n     _tc = _tc0;\n"),

    ic_codegen:emit(Fd, "\n     return _tc0;\n"),
    ic_codegen:emit(Fd, "   }\n\n").
    

	
emit_type_function({tk_struct, ID, Name, ML}, C, Fd) -> %% struct
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_struct);\n", [C,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.id(~p);\n", [C,ID]),
    ic_codegen:emit(Fd, "     _tc~p.name(~p);\n", [C,Name]),
    ic_codegen:emit(Fd, "     _tc~p.member_count(~p);\n", [C,length(ML)]),
    emit_struct_members(ML, C, C+1, 0, Fd);

emit_type_function({tk_enum, ID, Name, MNames}, C, Fd) -> %% enum
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_enum);\n", [C,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.id(~p);\n", [C,ID]),
    ic_codegen:emit(Fd, "     _tc~p.name(~p);\n", [C,Name]),
    ic_codegen:emit(Fd, "     _tc~p.member_count(~p);\n", [C,length(MNames)]),
    emit_enum_members(MNames, C, 0, Fd),
    C+1;

emit_type_function({tk_array, ET, L}, C, Fd) -> %% array
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_array);\n", [C,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.id(id());\n",[C]),
    ic_codegen:emit(Fd, "     _tc~p.length(~p);\n", [C,L]),
    C1 = C+1,
    C2 = emit_type_function(ET, C1, Fd),
    ic_codegen:emit(Fd, "     _tc~p.content_type(_tc~p);\n", [C,C1]),
    C2;

emit_type_function({tk_sequence, ET, L}, C, Fd) -> %% sequence 
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_sequence);\n", [C,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.id(id());\n",[C]),
    ic_codegen:emit(Fd, "     _tc~p.length(~p);\n", [C,L]),
    C1 = C+1,
    C2 = emit_type_function(ET, C1, Fd),
    ic_codegen:emit(Fd, "     _tc~p.content_type(_tc~p);\n", [C,C1]),
    C2;

emit_type_function({tk_string, L}, C, Fd) -> %% string
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_string);\n", [C,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.length(~p);\n", [C,L]),
    C+1;	     

emit_type_function({tk_union, ID, Name, DT, DI, LL}, C, Fd) -> %% union
    
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_union);\n", [C,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.id(~p);\n", [C,ID]),
    ic_codegen:emit(Fd, "     _tc~p.name(~p);\n", [C,Name]),
    
    C1 = C+1,
    C2 = emit_type_function(DT, C1, Fd),

    ic_codegen:emit(Fd, "     _tc~p.discriminator_type(_tc~p);\n", [C,C1]),
    ic_codegen:emit(Fd, "     _tc~p.default_index(~p);\n", [C,DI]),
    ic_codegen:emit(Fd, "     _tc~p.member_count(~p);\n", [C,length(LL)]),

    emit_union_labels(LL, C, DT, C2, 0, Fd);

emit_type_function(tk_term, C, Fd) -> %% term, must change it to tk_any
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.tk_any);\n", [C,?ICPACKAGE]),
    C+1;

emit_type_function(TC, C, Fd) -> %% other 
    ic_codegen:emit(Fd, "     ~sTypeCode _tc~p =\n",[?ICPACKAGE,C]),
    ic_codegen:emit(Fd, "       new ~sTypeCode();\n", [?ICPACKAGE]),
    ic_codegen:emit(Fd, "     _tc~p.kind(~sTCKind.~p);\n", [C,?ICPACKAGE,TC]),
    C+1.



emit_struct_members([], _, TCtr, _, _Fd) -> 
    TCtr;
emit_struct_members([{Name,MT}|Rest], BTCtr, TCtr, I, Fd) ->
    ic_codegen:emit(Fd, "     _tc~p.member_name(~p,~p);\n", [BTCtr,I,Name]),
    TCtr2 = emit_type_function(MT, TCtr, Fd),
    ic_codegen:emit(Fd, "     _tc~p.member_type(~p,_tc~p);\n", [BTCtr,I,TCtr]),
    emit_struct_members(Rest, BTCtr, TCtr2, I+1, Fd).

emit_enum_members([], _, _, _Fd) -> 
    ok;
emit_enum_members([Name|Names], BTCtr, I, Fd) ->
    ic_codegen:emit(Fd, "     _tc~p.member_name(~p,~p);\n", [BTCtr,I,Name]),
    emit_enum_members(Names, BTCtr, I+1, Fd).


emit_union_labels([], _, _, TCtr, _, _) -> 
    TCtr;
emit_union_labels([{L, LN, LT}|Rest], BTCtr, DT, TCtr, I, Fd) ->
    ic_codegen:emit(Fd, "     ~sAny _any~p =\n",[?ICPACKAGE,TCtr]),
    ic_codegen:emit(Fd, "       new ~sAny();\n", [?ICPACKAGE]),
    TCtr1 = TCtr+1,
    TCtr2 = emit_type_function(LT, TCtr1,Fd),
    ic_codegen:emit(Fd, "     _any~p.type(_tc~p);\n",[TCtr,TCtr1]),

    case L of
	default ->
	    ic_codegen:emit(Fd, "     _any~p.insert_atom(\"default\");\n", [TCtr]);
	_ ->
	    case DT of
		tk_boolean ->
		    ic_codegen:emit(Fd, "     _any~p.insert_boolean(~p);\n",[TCtr,L]);
		tk_char ->
		    Default = if is_integer(L) ->
				      [L];
				 true ->
				      L
			      end,
		    ic_codegen:emit(Fd, "     _any~p.insert_char('~s');\n",[TCtr,Default]);
		tk_ushort ->
		    ic_codegen:emit(Fd, "     _any~p.insert_ushort(~p);\n",[TCtr,L]);
		tk_ulong ->
		    ic_codegen:emit(Fd, "     _any~p.insert_ulong(~p);\n",[TCtr,L]);
		tk_short ->
		    ic_codegen:emit(Fd, "     _any~p.insert_short(~p);\n",[TCtr,L]);
		tk_long ->
		    ic_codegen:emit(Fd, "     _any~p.insert_long(~p);\n",[TCtr,L]);
		_ ->
		    ic_codegen:emit(Fd, "     _any~p.insert_string(~p);\n", [TCtr,L])
	    end
    end,
    ic_codegen:emit(Fd, "     _tc~p.member_label(~p,_any~p);\n", [BTCtr,I,TCtr]),
    ic_codegen:emit(Fd, "     _tc~p.member_name(~p,~p);\n", [BTCtr,I,LN]),
    TCtr3 = emit_type_function(LT, TCtr2, Fd),
    ic_codegen:emit(Fd, "     _tc~p.member_type(~p,_tc~p);\n", [BTCtr,I,TCtr2]),
    emit_union_labels(Rest, BTCtr, DT, TCtr3, I+1, Fd).








