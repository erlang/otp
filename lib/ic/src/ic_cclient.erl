%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
-module(ic_cclient).

%% This module implements generation of C client code, where the
%% client acts as an Erlang C-node, and where the communication thus
%% is according to the Erlang distribution protocol.
%%

-export([do_gen/3]).

%%------------------------------------------------------------
%% IMPLEMENTATION CONVENTIONS
%%------------------------------------------------------------
%% Functions:
%%
%% mk_*       returns things to be used. No side effects.
%% emit_*     Writes to file. Has Fd in arguments.
%% gen_*      Same, but has no Fd. Usually for larger things.
%%
%% Terminology for generating C:
%%
%% par_list   list of identifiers with types, types only, or with 
%%            parameters (arguments) only. 
%% arg_list   list of identifiers only (for function calls)
%%

%%------------------------------------------------------------
%% Internal stuff
%%------------------------------------------------------------

-import(lists, [foreach/2, foldl/3, foldr/3]).
-import(ic_codegen, [emit/2, emit/3, emit/4, emit_c_enc_rpt/4, emit_c_dec_rpt/4]).

-include("icforms.hrl").
-include("ic.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(IC_HEADER, "ic.h").
-define(ERL_INTERFACEHEADER, "erl_interface.h").
-define(EICONVHEADER, "ei.h").
-define(ERLANGATOMLENGTH, "256").


%%------------------------------------------------------------
%% ENTRY POINT
%%------------------------------------------------------------
do_gen(G, File, Form) -> 
    OeName = ic_util:mk_oe_name(G, remove_ext(ic_util:to_list(File))), 
    G2 = ic_file:filename_push(G, [], OeName, c), 
    gen_headers(G2, [], Form), 
    R = gen(G2, [], Form), 
    ic_file:filename_pop(G2, c), 
    R.

remove_ext(File) ->
    filename:rootname(filename:basename(File)).

%%------------------------------------------------------------
%%
%% Generate client side C stubs. 
%%
%% - each module definition results in a separate file.
%% - each interface definition results in a separate file.
%%
%%  G = record(genobj) (see ic.hrl)
%%  N = scoped names in reverse
%%  X = current form to consider. 
%%------------------------------------------------------------

gen(G, N, [X| Xs]) when is_record(X, preproc) ->
    G1 = change_file_stack(G, N, X), 
    gen(G1, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, module) ->
    CD = ic_code:codeDirective(G, X), 
    G2 = ic_file:filename_push(G, N, X, CD), 
    N2 = [ic_forms:get_id2(X)| N], 
    gen_headers(G2, N2, X), 
    gen(G2, N2, ic_forms:get_body(X)), 
    G3 = ic_file:filename_pop(G2, CD), 
    gen(G3, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, interface) ->

    G2 = ic_file:filename_push(G, N, X, c), 
    N2 = [ic_forms:get_id2(X)| N], 

    %% Sets the temporary variable counter.
    put(op_variable_count, 0), 
    put(tmp_declarations, []), 

    gen_headers(G2, N2, X), 

    gen(G2, N2, ic_forms:get_body(X)), 

    lists:foreach(
      fun({_Name, Body}) -> 
	      gen(G2, N2, Body) end, 
      X#interface.inherit_body), 

    %% Generate Prototypes
    gen_prototypes(G2, N2, X), 

    %% Generate generic preparation for decoding
    gen_receive_info(G2, N2, X), 

    G3 = ic_file:filename_pop(G2, c), 

    gen(G3, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, const) ->
    emit_constant(G, N, X), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, op) ->
    {OpName, ArgNames, RetParTypes} = ic_cbe:extract_info(G, N, X), 
    %% XXX Note: N is the list of scoped ids of the *interface*.
    gen_operation(G, N, X, OpName, ArgNames, RetParTypes), 
    gen_encoder(G, N, X, OpName, ArgNames, RetParTypes), 
    gen_decoder(G, N, X, OpName, ArgNames, RetParTypes), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, attr) ->
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, except) ->
    icstruct:except_gen(G, N, X, c), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, enum) ->
    icenum:enum_gen(G, N, X, c), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, typedef) ->
    icstruct:struct_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, struct) ->
    icstruct:struct_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, union) ->
    icstruct:struct_gen(G, N, X, c),
    gen(G, N, Xs);

gen(G, N, [_X| Xs]) ->
    %% XXX Should have debug message here.
    gen(G, N, Xs);

gen(_G, _N, []) -> 
    ok.

%%------------------------------------------------------------
%% Change file stack
%%------------------------------------------------------------

change_file_stack(G, _N, X) when X#preproc.cat == line_nr ->
    Id = ic_forms:get_id2(X), 
    Flags = X#preproc.aux, 
    case Flags of
	[] -> 
	    ic_genobj:push_file(G, Id);
	_ ->
	    foldr(
	      fun({_, _, "1"}, G1) -> 
		      ic_genobj:push_file(G1, Id);
		 ({_, _, "2"}, G1) -> 
		      ic_genobj:pop_file(G1, Id);
		 ({_, _, "3"}, G1) -> 
		      ic_genobj:sys_file(G1, Id) 
	      end, G, Flags)
    end;
change_file_stack(G, _N, _X) ->
    G.

%%------------------------------------------------------------
%% Generate headers in stubfiles and header files 
%%------------------------------------------------------------

gen_headers(G, N, X) when is_record(X, interface) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    %% Set the temporary variable counter
	    put(op_variable_count, 0), 
	    put(tmp_declarations, []), 
	    HFd = ic_genobj:hrlfiled(G), 
	    IncludeFileStack = ic_genobj:include_file_stack(G), 
	    L = length(N), 
	    Filename =
		if
		    L < 2 ->
			lists:nth(L + 1, IncludeFileStack);
		    true ->
			lists:nth(2, IncludeFileStack)
		end, 
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]), 
	    ic_code:gen_includes(HFd, G, X, c_client), 

	    IfName = ic_util:to_undersc(N), 
	    IfNameUC = ic_util:to_uppercase(IfName), 
	    emit(HFd, "\n#ifndef __~s__\n", [IfNameUC]), 
	    emit(HFd, "#define __~s__\n", [IfNameUC]), 
	    LCmt = io_lib:format("Interface object definition: ~s", [IfName]), 
	    ic_codegen:mcomment_light(HFd, [LCmt], c), 
	    case get_c_timeout(G, "") of
		"" ->
		    ok;
		{SendTmo, RecvTmo} ->
		    emit(HFd, "#define OE_~s_SEND_TIMEOUT  ~s\n", 
			 [IfNameUC, SendTmo]), 
		    emit(HFd, "#define OE_~s_RECV_TIMEOUT  ~s\n", 
			 [IfNameUC, RecvTmo]), 
		    emit(HFd, "#ifndef EI_HAVE_TIMEOUT\n"),
		    emit(HFd, "#error Functions for send and receive with "
			 "timeout not defined in erl_interface\n"),
		    emit(HFd, "#endif\n\n")
	    end,

	    emit(HFd, "typedef CORBA_Object ~s;\n", [IfName]), 
	    emit(HFd, "#endif\n\n");

	false -> ok
    end, 
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 
	    ic_codegen:nl(Fd), 
	    emit(Fd, "#include <stdlib.h>\n"), 
	    emit(Fd, "#include <string.h>\n"), 
	    case ic_options:get_opt(G, c_report) of 
		true ->
		    emit(Fd, "#ifndef OE_C_REPORT\n"), 
		    emit(Fd, "#define OE_C_REPORT\n"), 
		    emit(Fd, "#include <stdio.h>\n"), 
		    emit(Fd, "#endif\n");
		_  ->
		    ok
	    end,
	    emit(Fd, "#include \"~s\"\n", [?IC_HEADER]), 
	    emit(Fd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]), 
	    emit(Fd, "#include \"~s\"\n", [?EICONVHEADER]), 
	    emit(Fd, "#include \"~s\"\n", 
		 [filename:basename(ic_genobj:include_file(G))]), 
	    ic_codegen:nl(Fd), ic_codegen:nl(Fd), 
	    Fd;					% XXX ??
	false ->
	    ok
    end;

%% Some items have extra includes
gen_headers(G, N, X) when is_record(X, module) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    IncludeFileStack = ic_genobj:include_file_stack(G), 
	    Filename = lists:nth(length(N) + 1, IncludeFileStack), 
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]), 
	    ic_code:gen_includes(HFd, G, X, c_client);
	false -> ok
    end;
gen_headers(G, [], _X) -> 
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    case ic_options:get_opt(G, c_report) of 
		true ->
		    emit(HFd, "#ifndef OE_C_REPORT\n"), 
		    emit(HFd, "#define OE_C_REPORT\n"), 
		    emit(HFd, "#include <stdio.h>\n"), 
		    emit(HFd, "#endif\n");
		_  ->
		    ok
	    end,
	    emit(HFd, "#include \"~s\"\n", [?IC_HEADER]), 
	    emit(HFd, "#include \"~s\"\n", [?ERL_INTERFACEHEADER]), 
	    emit(HFd, "#include \"~s\"\n", [?EICONVHEADER]), 
	    ic_code:gen_includes(HFd, G, c_client);
	false -> ok
    end;
gen_headers(_G, _N, _X) -> 
    ok.


%%------------------------------------------------------------
%% Generate all prototypes (for interface)
%%------------------------------------------------------------
gen_prototypes(G, N, X) ->
    case ic_genobj:is_hrlfile_open(G) of
	false -> 
	    ok;
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    IfName = ic_util:to_undersc(N), 

	    %% Emit generated function prototypes
	    emit(HFd, "\n/* Operation functions  */\n"), 
	    lists:foreach(fun({_Name, Body}) ->
				  emit_operation_prototypes(G, HFd, N, Body)
			  end, [{x, ic_forms:get_body(X)}| 
				X#interface.inherit_body]), 

	    UserProto = get_user_proto(G, false),
	    %% Emit generic function prototypes
	    case UserProto of
		false ->
		    ok;
		UserProto ->
		    emit(HFd, 
			 "\n/* Generic user defined encoders */\n"), 
		    emit(HFd, 
			 "int ~s_prepare_notification_encoding("
			 "CORBA_Environment*);"
			 "\n", [UserProto]), 
		    emit(HFd, 
			 "int ~s_prepare_request_encoding(CORBA_Environment*);"
			 "\n", [UserProto])
	    end,
	    %% Emit encoding function prototypes
	    emit(HFd, "\n/* Input encoders */\n"), 
	    lists:foreach(fun({_Name, Body}) ->
				  emit_encoder_prototypes(G, HFd, N, Body) 
			  end, 
			  [{x, ic_forms:get_body(X)}| 
			   X#interface.inherit_body]), 

	    %% Emit generic function prototypes
	    emit(HFd, "\n/* Generic decoders */\n"), 
	    emit(HFd, "int ~s__receive_info(~s, CORBA_Environment*);\n", 
		 [IfName, IfName]), 

	    case UserProto of
		false ->
		    ok;
		UserProto ->
		    emit(HFd, "\n/* Generic user defined decoders */\n"), 
		    emit(HFd, 
			 "int ~s_prepare_reply_decoding(CORBA_Environment*);"
			 "\n", [UserProto]) 
	    end,
	    %% Emit decode function prototypes
	    emit(HFd, "\n/* Result decoders */\n"), 
	    lists:foreach(fun({_Name, Body}) ->
				  emit_decoder_prototypes(G, HFd, N, Body) 
			  end, [{x, ic_forms:get_body(X)}| 
				X#interface.inherit_body]), 
	    case UserProto of
		false ->
		    ok;
		UserProto ->
		    %% Emit generic send and receive_prototypes
		    {Sfx, TmoType} = case get_c_timeout(G, "") of
				     "" ->
					 {"", ""};
				     _ ->
					 {"_tmo", ", unsigned int"} 
			     end,
		    emit(HFd, 
			 "\n/* Generic user defined send and receive "
			 "functions */\n"), 
		    emit(HFd, 
			 "int ~s_send_notification~s(CORBA_Environment*~s);\n",
			 [UserProto, Sfx, TmoType]), 
		    emit(HFd, 
			 "int ~s_send_request_and_receive_reply~s("
			 "CORBA_Environment*~s~s);\n", 
			 [UserProto, Sfx, TmoType, TmoType])
	    end
    end.

%%------------------------------------------------------------
%% Generate receive_info() (generic part for message reception) 
%% (for interface). For backward compatibility only.
%%------------------------------------------------------------

gen_receive_info(G, N, _X) ->
    case ic_genobj:is_stubfile_open(G) of
	false ->
	    ok;
	true ->
	    Fd = ic_genobj:stubfiled(G), 
	    IfName = ic_util:to_undersc(N), 
	    UserProto = get_user_proto(G, oe),
	    Code = 
		"
/*
 *  Generic function, used to return received message information.
 *  Not used by oneways. Always generated. For backward compatibility only.
 */

int ~s__receive_info(~s oe_obj, CORBA_Environment *oe_env)
{
  return  ~s_prepare_reply_decoding(oe_env);
}\n", 
        emit(Fd, Code, [IfName, IfName, UserProto])
end.

%%------------------------------------------------------------
%% Emit constant
%%------------------------------------------------------------

emit_constant(G, N, ConstRecord) ->
    case ic_genobj:is_hrlfile_open(G) of
	false -> ok;
	true ->
	    Fd = ic_genobj:hrlfiled(G), 
	    CName = ic_util:to_undersc(
		      [ic_forms:get_id(ConstRecord#const.id)| N]), 
	    UCName = ic_util:to_uppercase(CName), 

	    emit(Fd, "\n#ifndef __~s__\n", [UCName]), 
	    emit(Fd, "#define __~s__\n", [UCName]), 

	    emit(Fd, "/* Constant: ~s */\n", [CName]), 

	    if is_record(ConstRecord#const.type, wstring) -> 
		    %% If wstring, add 'L' 
		    emit(Fd, "#define ~s L~p\n", 
			 [CName, ConstRecord#const.val]);
	       true ->
		    emit(Fd, "#define ~s ~p\n", 
			 [CName, ConstRecord#const.val])
	    end, 
	    emit(Fd, "#endif\n\n")
    end.

%%------------------------------------------------------------
%% Generate operation (for interface)
%%------------------------------------------------------------

%% N is the list of scoped ids of the *interface*. 
%% X is the operation
gen_operation(G, N, X, OpName, ArgNames, RetParTypes) ->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    do_gen_operation(G, N, X, OpName, ArgNames, RetParTypes);
	false ->
	    ok
    end.

do_gen_operation(G, N, X, OpName, ArgNames, RetParTypes) ->
    Fd = ic_genobj:stubfiled(G), 
    IfName = ic_util:to_undersc(N), 
    IfNameUC = ic_util:to_uppercase(IfName), 

    {R, ParTypes, _} = RetParTypes, 

    IsOneway = ic_forms:is_oneway(X),

    emit(Fd, "\n"
	 "/***\n"
	 " ***  Operation function \"~s\" ~s\n"
	 " ***/\n\n", 
	 [OpName, ifelse(IsOneway, "(oneway)", "")]),

    RV = element(1, R), 
    Ret = case IsOneway of
	      false ->
		  if RV /= void -> 
			  mk_ret_type(G, N, R);
		     true -> 
			  "void"
		  end;
	      true ->
		  "void"
	  end, 
    ParListStr = ic_util:chain(mk_par_type_list(G, N, X, [in, out], 
						[types, args], 
						ParTypes, ArgNames), ", "),
    emit(Fd, 
	 "~s ~s(~s, ~sCORBA_Environment *oe_env)\n{\n",
	 [Ret, OpName, [IfName, " ", "oe_obj"], ParListStr]), 

    case IsOneway of
	true ->
	    ok;
	false ->
	    case ictype:isArray(G, N, R) of
		true ->
		    emit(Fd, "  ~s oe_return = NULL;\n\n", 
			 [mk_ret_type(G, N, R)]);
		false ->
		    if RV /= void ->
			    emit(Fd, "  ~s oe_return;\n\n", 
				 [Ret]);
		       true ->
			    ok
		    end
	    end, 
	    emit(Fd, 
		 "  /* Initiating the message reference */\n" 
		 "  ic_init_ref(oe_env, &oe_env->_unique);\n")
    end,

    emit(Fd, 
	 "  /* Initiating exception indicator */ \n"
	 "  oe_env->_major = CORBA_NO_EXCEPTION;\n"),

    %% XXX Add pointer checks: checks of in-parameter
    %% pointers, and non-variable out-parameter pointers.

    emit(Fd,"  /* Creating ~s message */ \n", 
	 [ifelse(IsOneway, "cast", "call")]),

    EncParListStr = ic_util:chain(mk_arg_list_for_encoder(G, N, X, 
							  ParTypes, ArgNames),
				  ", "),
    emit(Fd, 
	 "  if (~s__client_enc(oe_obj, ~s""oe_env) < 0) {\n", 
	 [OpName, EncParListStr]),
    emit(Fd, 
	 "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
	 "DATA_CONVERSION, \"Cannot encode message\");\n"), 

    RetVar = ifelse(RV /= void, " oe_return", ""),
    emit_c_enc_rpt(Fd, "    ", "client operation ~s\\n====\\n", [OpName]),

    emit(Fd, "    return~s;\n  }\n", [RetVar]),

    emit(Fd,"  /* Sending ~s message */ \n", 
	 [ifelse(IsOneway, "cast", "call")]),

    UserProto = get_user_proto(G, oe),
    {Sfx, SendTmo, RecvTmo} = case get_c_timeout(G, "") of
				  "" ->
				      {"", "", ""};
				  _ ->
				      {"_tmo", 
				       [", OE_", IfNameUC, "_SEND_TIMEOUT"], 
				       [", OE_", IfNameUC, "_RECV_TIMEOUT"]} 
			      end,

    case IsOneway of
	true ->
	    emit(Fd, 
		 "  if (~s_send_notification~s(oe_env~s) < 0)\n"
		 "    return~s;\n", [UserProto, Sfx, SendTmo, RetVar]);
	false ->
	    emit(Fd, 
		 "  if (~s_send_request_and_receive_reply~s(oe_env~s~s) < 0)\n"
		 "    return~s;\n", 
		 [UserProto, Sfx, SendTmo, RecvTmo, RetVar]),

	    DecParList0 = mk_arg_list_for_decoder(G, N, X, 
						  ParTypes, ArgNames), 
	    DecParList1 = case mk_ret_type(G, N, R) of
			      "void" ->
				  DecParList0;
			      _ ->
				  ["&oe_return"| DecParList0]
		end, 

	    DecParListStr = ic_util:chain(DecParList1, ", "),
	    %% YYY Extracting results
	    emit(Fd, 
		 "  /* Extracting result value(s) */ \n" 
		 "  if (~s__client_dec(oe_obj, ~s""oe_env) < 0) {\n", 
		 [OpName, DecParListStr]), 
	    emit(Fd, 
		 "    CORBA_exc_set(oe_env, "
		 "CORBA_SYSTEM_EXCEPTION, DATA_CONVERSION, "
		 "\"Bad result value(s)\");\n"),
	    emit_c_dec_rpt(Fd, "    ", "client operation ~s\\n=====\\n", [OpName]),
	    emit(Fd, 
		 "    return~s;\n"
		 "  }\n", [RetVar])
    end,
    emit(Fd, "  return~s;\n", [RetVar]),
    emit(Fd, "}\n\n\n").

%%------------------------------------------------------------
%% Generate encoder 
%%------------------------------------------------------------
%% N is the list of scoped ids of the *interface*. 
%% X is the operation
gen_encoder(G, N, X, OpName, ArgNames, RetParTypes)->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 
	    IfName = ic_util:to_undersc(N), 
	    {_R, ParTypes, _} = RetParTypes, 
	    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames), 
	    emit(Fd, "/*\n *  Encode operation input for \"~s\"\n */\n\n", 
		 [OpName]), 
	    ParList = ic_util:chain(
			mk_par_type_list(G, N, X, [in], [types, args], 
					 ParTypes, ArgNames), ", "), 
	    emit(Fd, 
		 "int ~s__client_enc(~s oe_obj, ~s"
		 "CORBA_Environment *oe_env)\n{\n", 
		 [OpName, IfName, ParList]),

	    InTypeAttrArgs = lists:filter(fun({_, in, _}) -> true;
					     ({_, _, _}) -> false
					  end, TypeAttrArgs), 
	    case InTypeAttrArgs of 
		[] ->
		    ok;
		_ ->
		    emit(Fd, 
			 "  int oe_error_code = 0;\n\n")
	    end, 

	    emit_encodings(G, N, Fd, X, InTypeAttrArgs, 
			   ic_forms:is_oneway(X)), 
 	    emit(Fd, "  return 0;\n}\n\n"), 
	    ok;
	
	false -> 
	    ok
    end.

%%------------------------------------------------------------
%% Generate decoder
%%------------------------------------------------------------
%% N is the list of scoped ids of the *interface*. 
%% X is the operation
gen_decoder(G, N, X, OpName, ArgNames, RetParTypes)->
    case ic_forms:is_oneway(X) of
	true ->
	    ok;
	false ->
	    case ic_genobj:is_stubfile_open(G) of
		true ->
		    Fd = ic_genobj:stubfiled(G), 
		    IfName = ic_util:to_undersc(N), 
		    {R, ParTypes, _} = RetParTypes, 
		    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames), 
		    emit(Fd, "/*\n *  Decode operation results for "
			 "\"~s\"\n */\n\n", [OpName]), 
		    ParList0 = mk_par_type_list(G, N, X, [out],
						[types, args], 
						ParTypes, ArgNames),
		    PARLIST = case mk_ret_type(G, N, R) of
				  "void" ->
				      ParList0;
				  Else ->    
				      [Else ++ "* oe_return"| ParList0]
			      end, 
		    PLFCD = ic_util:chain(PARLIST, ", "), 
		    emit(Fd, 
			 "int ~s__client_dec(~s oe_obj, ~s"
			 "CORBA_Environment *oe_env)\n{\n", 
			 [OpName, IfName, PLFCD]),
		    emit(Fd, "  int oe_error_code = 0;\n"), 
		    OutTypeAttrArgs = lists:filter(fun({_, out, _}) -> true;
						      ({_, _, _}) -> false
						   end, TypeAttrArgs), 
		    emit_decodings(G, N, Fd, R, OutTypeAttrArgs),
		    emit(Fd, "  return 0;\n}\n\n"), 
		    ok;
		
		false -> 
		    ok
	    end
    end.

%%------------------------------------------------------------
%% EMIT ENCODINGS/DECODINGS
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Emit encodings
%%------------------------------------------------------------
%% N is the list of scoped ids of the *interface*. 
%% X is the operation
%% emit_encodings(G, N, Fd, X, TypeAttrArgs, IsOneWay) 
%%
emit_encodings(G, N, Fd, X, TypeAttrArgs, true) ->
    %% Cast
    UserProto = get_user_proto(G, oe),
    emit(Fd, 
	 "  if (~s_prepare_notification_encoding(oe_env) < 0)\n"
	 "    return -1;\n", [UserProto]),
    emit_encodings_1(G, N, Fd, X, TypeAttrArgs);
emit_encodings(G, N, Fd, X, TypeAttrArgs, false) ->
    %% Call
    UserProto = get_user_proto(G, oe),
    emit(Fd, 
	 "  if (~s_prepare_request_encoding(oe_env) < 0)\n"
	 "    return -1;\n", [UserProto]),
    emit_encodings_1(G, N, Fd, X, TypeAttrArgs).

emit_encodings_1(G, N, Fd, X, TypeAttrArgs) ->
    {ScopedName, _, _} = ic_cbe:extract_info(G, N, X), 
    Name = case ic_options:get_opt(G, scoped_op_calls) of 
	       true -> 
		   ScopedName;
	       false ->
		   ic_forms:get_id2(X)
	   end, 
    if 
	TypeAttrArgs /= [] -> 
	    emit(Fd, "  if (oe_ei_encode_tuple_header(oe_env, ~p) < 0) {\n", 
		 [length(TypeAttrArgs) + 1]), 
	    emit_c_enc_rpt(Fd, "    ", "ei_encode_tuple_header", []),
	    emit(Fd, "    return -1;\n  }\n");
	true ->
	    ok
    end,
    emit(Fd, "  if (oe_ei_encode_atom(oe_env, ~p) < 0) {\n", [Name]), 
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_atom", []),
    emit(Fd, "    return -1;\n  }\n"),

    foreach(fun({{'void', _}, _, _}) ->
		    ok;
		({T1, A1, N1}) ->
		    IndOp  = mk_ind_op(A1), 
		    emit_coding_comment(G, N, Fd, "Encode", IndOp, 
					  T1, N1), 
		    ic_cbe:emit_encoding_stmt(G, N, X, Fd, T1, IndOp ++ N1,
					      "oe_env->_outbuf")
	    end, TypeAttrArgs), 
    ok.

%%------------------------------------------------------------
%% Emit dedodings
%%------------------------------------------------------------
%% XXX Unfortunately we have to retain the silly `oe_first' variable,
%% since its name is hardcoded in other modules (icstruct, icunion,
%% etc).
%% N is the list of scoped ids of the *interface*. 
%% X is the operation
emit_decodings(G, N, Fd, RetType, TypeAttrArgs) ->
    if 
	TypeAttrArgs /= [] ->
	    %% Only if there are out parameters
	    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header("
		 "oe_env->_inbuf, &oe_env->_iin, "
		 "&oe_env->_received)) < 0) {\n"), 
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
	    emit(Fd, "    return oe_error_code;\n    }\n"), 
	    Len = length(TypeAttrArgs) + 1, 
	    emit(Fd, "  if (oe_env->_received != ~p) {\n", [Len]),
	    emit_c_dec_rpt(Fd, "    ", "tuple header size != ~p", [Len]),
	    emit(Fd, "    return -1;\n    }\n"); 
	true  ->
	    ok
    end,

    %% Fetch the return value
    emit_coding_comment(G, N, Fd, "Decode return value", "*", RetType, "oe_return"), 
    APars =
	case ic_cbe:is_variable_size(G, N, RetType) of
	    true ->
		emit(Fd, 
		     "  {\n"
		     "    int oe_size_count_index = oe_env->_iin;\n"
		     "    int oe_malloc_size = 0;\n"
		     "    void *oe_first = NULL;\n"),
		ic_cbe:emit_malloc_size_stmt(G, N, Fd, RetType, 
					     "oe_env->_inbuf", 
					     1, caller), 
		%% XXX Add malloc prefix from option
		emit(Fd, 
		     "    OE_MALLOC_SIZE_CHECK(oe_env, oe_malloc_size);\n" 
		     "    if ((*oe_return = oe_first = "
		     "malloc(oe_malloc_size)) == NULL) {\n"
		     "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		     "NO_MEMORY, \"Cannot malloc\");\n" 
		     "      return -1;\n"
		     "    }\n"),
		Pars = ["*oe_return"],
		DecType = case ictype:isArray(G, N, RetType) of
			      true -> array_dyn;
			      false -> caller_dyn
			  end,
		ic_cbe:emit_decoding_stmt(G, N, Fd, RetType, 
					  "(*oe_return)", 
					  "", "oe_env->_inbuf", 1, 
					  "&oe_outindex", DecType,
					  Pars), 
		emit(Fd, "  }\n"),
		Pars;
	    false ->
		case ictype:isArray(G, N, RetType) of
		    true ->
			Pars = ["*oe_return"],
			emit(Fd, 
			     "  {\n"
			     "    int oe_size_count_index = oe_env->_iin;\n"
			     "    int oe_malloc_size = 0;\n"
			     "    void *oe_first = NULL;\n"),
			ic_cbe:emit_malloc_size_stmt(G, N, Fd, RetType, 
						     "oe_env->_inbuf", 
						     1, caller), 
			%% XXX Add malloc prefix from option
			emit(Fd, 
			     "    OE_MALLOC_SIZE_CHECK(oe_env, "
			     "oe_malloc_size);\n" 
			     "    if ((*oe_return = oe_first = "
			     "malloc(oe_malloc_size)) == NULL) {\n"
			     "      CORBA_exc_set(oe_env, "
			     "CORBA_SYSTEM_EXCEPTION, NO_MEMORY, "
			     "\"Cannot malloc\");\n" 
			     "        return -1;"
			     "    }\n"),
			ic_cbe:emit_decoding_stmt(G, N, Fd, RetType, 
						  "oe_return", "", 
						  "oe_env->_inbuf", 1, 
						  "&oe_outindex", 
						  array_fix_ret, 
						  Pars), 
			emit(Fd, "  }\n"),
			Pars;
		    false ->
			Pars = [],
			%% The last parameter "oe_outindex" is not interesting 
			%% in the static case.
			ic_cbe:emit_decoding_stmt(G, N, Fd, RetType, 
						  "oe_return", "", 
						  "oe_env->_inbuf", 1, 
						  "&oe_outindex", 
						  caller, Pars), 
			ic_codegen:nl(Fd),
			Pars
		end
	end, 

    foldl(fun({{'void', _}, _, _}, Acc) ->
		  Acc;
	     ({T, A, N1}, Acc) ->
		  emit_one_decoding(G, N, Fd, T, A, N1, Acc)
	  end, APars, TypeAttrArgs), 
    ok.

emit_one_decoding(G, N, Fd, T, A, N1, Acc) ->
    IndOp = mk_ind_op(A), 
    case ic_cbe:is_variable_size(G, N, T) of
	true ->
	    emit_coding_comment(G, N, Fd, "Decode", IndOp, 
				  T, N1), 
	    emit(Fd, 
		 "  {\n"
		 "    int oe_size_count_index = oe_env->_iin;\n"
		 "    int oe_malloc_size = 0;\n"
		 "    void *oe_first = NULL;\n"),
	    ic_cbe:emit_malloc_size_stmt(G, N, Fd, T, 
					 "oe_env->_inbuf", 
					 1, caller), 
	    %% XXX Add malloc prefix from option
	    emit(Fd, 
		 "    OE_MALLOC_SIZE_CHECK(oe_env, oe_malloc_size);\n" 
		 "    if ((~s~s = oe_first = "
		 "malloc(oe_malloc_size)) == NULL) {\n", [IndOp, N1]),
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", Acc),
	    emit(Fd,
		 "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "NO_MEMORY, \"Cannot malloc\");\n" 
		 "      return -1;\n"
		 "    }\n"),
	    NAcc = [IndOp ++ N1| Acc],  
	    DecType = case ictype:isArray(G, N, T) of
			  true ->
			      array_dyn;
			  false ->
			      caller_dyn
		      end,
	    ic_cbe:emit_decoding_stmt(G, N, Fd, T,  
				      "(" ++ IndOp
				      ++ N1 ++ ")", "", 
				      "oe_env->_inbuf", 1, 
				      "&oe_outindex", 
				      DecType, NAcc), 
	    emit(Fd, "  }\n"),
	    NAcc;
	false ->
	    case ictype:isArray(G, N, T) of
		true ->
		    emit_coding_comment(G, N, Fd, "Decode", "", 
					  T, N1), 
		    ic_cbe:emit_decoding_stmt(G, N, Fd, T, N1, 
					      "", "oe_env->_inbuf", 
					      1, "&oe_outindex", 
					      array_fix_out, Acc), 
		    ic_codegen:nl(Fd),
		    [N1| Acc]; 
		false ->
		    %% The last parameter "oe_outindex" is
		    %% not interesting in the static case, but
		    %% must be present anyhow.
		    emit_coding_comment(G, N, Fd, "Decode", 
					  IndOp, T, N1), 
		    ic_cbe:emit_decoding_stmt(G, N, Fd, T,  N1, 
					      "", "oe_env->_inbuf", 
					      1, "&oe_outindex", 
					      caller, Acc), 
		    ic_codegen:nl(Fd),
		    Acc
	    end
    end.

%%------------------------------------------------------------
%% GENERATE PROTOTYPES
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Generate operation prototypes
%%------------------------------------------------------------
emit_operation_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when is_record(X, op) ->
	      {ScopedName, ArgNames, RetParTypes} = 
		  ic_cbe:extract_info(G, N, X), 
	      {R, ParTypes, _} = RetParTypes, 
	      IfName = ic_util:to_undersc(N), 
	      RT = mk_ret_type(G, N, R), 
	      ParList = 
		  ic_util:chain(
		    mk_par_type_list(G, N, X, [in, out], [types], 
				     ParTypes, ArgNames), 
		    ", "), 
	      emit(Fd, "~s ~s(~s, ~sCORBA_Environment*);\n", 
		   [RT, ScopedName, IfName, ParList]);
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Generate encoder prototypes
%%------------------------------------------------------------
emit_encoder_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when is_record(X, op) ->
	      {ScopedName, ArgNames, RetParTypes} = 
		  ic_cbe:extract_info(G, N, X), 
	      {_R, ParTypes, _} = RetParTypes, 
	      IfName = ic_util:to_undersc(N), 
	      ParList = ic_util:chain(
			  mk_par_type_list(G, N, X, [in], [types], 
					   ParTypes, ArgNames), 
			  ", "),
	    emit(Fd, "int ~s__client_enc(~s, ~sCORBA_Environment*);\n", 
		 [ScopedName, IfName, ParList]);
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Generate decoder prototypes
%%------------------------------------------------------------
emit_decoder_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when is_record(X, op) ->
	      case ic_forms:is_oneway(X) of
		  true ->
		      true;
		  false ->
		      IfName = ic_util:to_undersc(N), 
		      {ScopedName, ArgNames, RetParTypes} = 
			  ic_cbe:extract_info(G, N, X), 
		      {R, ParTypes, _} = RetParTypes, 
		      ParList0 = 
			  mk_par_type_list(G, N, X, [out], [types], 
					   ParTypes, ArgNames), 
		      PARLIST = case mk_ret_type(G, N, R) of
				    "void" ->
					ParList0;
				    Else ->
					[Else ++ "*"| ParList0]
				end, 
		      ParList = ic_util:chain(PARLIST, ", "),
		      emit(Fd, "int ~s__client_dec(~s, ~s"
			   "CORBA_Environment*);\n", 
			   [ScopedName, IfName, ParList])
	      end;
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% PARAMETER TYPE LISTS
%%------------------------------------------------------------
%%------------------------------------------------------------
%%  Make parameter type list
%%
%%  InOrOut = in | out | [in | out]
%%  TypesOrArgs = types | args | [types | args]
%%------------------------------------------------------------
mk_par_type_list(G, N, X, InOrOut, TypesOrArgs, Types, Args) ->
    TypeAttrArgs = 
	filterzip(
	  fun(_, {inout, Arg}) ->
		  ic_error:error(G, {inout_spec_for_c, X, Arg}), 
		  false;
	     (Type, {Attr, Arg}) ->
		  case lists:member(Attr, InOrOut) of
		      true ->
			  {true, {Type, Attr, Arg}}; 
		      false ->
			  false
		  end
	  end, Types, Args),
    lists:map(
      fun({Type, Attr, Arg}) ->
	      Ctype = ic_cbe:mk_c_type(G, N, Type), 
	      IsArray = ictype:isArray(G, N, Type), 
	      IsStruct = ictype:isStruct(G, N, Type), 
	      IsUnion = ictype:isUnion(G, N, Type), 
	      Dyn = 
		  case ic_cbe:is_variable_size(G, N, Type) of
		      true ->
			  if 
			      is_record(Type, string) ->		"";
			      Ctype == "CORBA_char *" -> 	"";
			      is_record(Type, wstring) ->		"";
			      Ctype == "CORBA_wchar *" ->	"";
			      true ->
				  case IsArray of
				      true ->
					  "_slice*";
				      false ->
					  "*"
				  end
			  end;
		      false ->
			  if 
			      Attr == in, Ctype == "erlang_pid" ->
				  "*";
			      Attr == in, Ctype == "erlang_port" ->
				  "*";
			      Attr == in, Ctype == "erlang_ref" ->
				  "*";
			      Attr == in, IsStruct == true ->
				  "*";
			      Attr == in, IsUnion == true ->
				  "*";
			      Attr == in, IsArray == true ->
				  "_slice*";
			      Attr == out, IsArray == true ->
				  "_slice";
			      true ->
				  ""
			  end
		  end, 
	      IndOp = mk_ind_op(Attr),
	      case {lists:member(types, TypesOrArgs), 
		    lists:member(args, TypesOrArgs)} of
		  {true, true} ->
		      Ctype ++ Dyn ++ IndOp ++ " " ++ Arg; 
		  {true, false} ->
		      Ctype ++ Dyn ++ IndOp;
		  {false, true} ->
		      Arg;
		  {false, false} ->
		      ""
	      end
      end, TypeAttrArgs).

%%------------------------------------------------------------
%% ENCODER ARG LIST
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Make encoder argument list XXX
%%------------------------------------------------------------
mk_arg_list_for_encoder(G, _N, X, Types, Args) ->
    filterzip(
      fun(_, {out, _}) ->
	      false;
	 (_, {inout, Arg}) ->
	      ic_error:error(G, {inout_spec_for_c, X, Arg}), 
	      false;
	 (_Type, {in, Arg}) ->
	      {true, Arg}
      end, Types, Args).

%%------------------------------------------------------------
%% DECODER ARG LIST
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Make decoder argument list XXX
%%------------------------------------------------------------
mk_arg_list_for_decoder(G, _N, X, Types, Args) ->
    filterzip(fun(_, {in, _}) ->
		      false;
		 (_, {inout, Arg}) -> 
		      ic_error:error(G, {inout_spec_for_c, X, Arg}), 
		      false;
		 (_, {out, Arg}) ->
		      {true, Arg}
	      end, Types, Args).

%%------------------------------------------------------------
%% MISC
%%------------------------------------------------------------
%%------------------------------------------------------------
%% Make list of {Type, Attr, Arg}
%%------------------------------------------------------------
mk_type_attr_arg_list(Types, Args) ->
    filterzip(fun(Type, {Attr, Arg}) ->
		      {true, {Type, Attr, Arg}}
	      end, Types, Args).

%%------------------------------------------------------------
%% Make return type
%%------------------------------------------------------------
mk_ret_type(G, N, Type) ->
    Ctype = ic_cbe:mk_c_type(G, N, Type), 
    Dyn = case ic_cbe:is_variable_size(G, N, Type) of
	      true ->
		  if 
		      is_record(Type, string) ->
			  "";
		      Ctype == "CORBA_char *" ->
			  "";
		      is_record(Type, wstring) ->  
			  "";
		      Ctype == "CORBA_wchar *" ->  
			  "";
		      true ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "_slice*";
			      false ->
				  "*"
			  end
		  end;
	      false ->
		  case ictype:isArray(G, N, Type) of
		      true ->
			  "_slice*";
		      false ->
			  ""
		  end
	  end, 
    Ctype ++ Dyn.


%%------------------------------------------------------------
%% Make indirection operator (to "*" or not to "*").
%%------------------------------------------------------------
mk_ind_op(in) ->
    "";
mk_ind_op(inout) ->
    error;
mk_ind_op(out) ->
    "*".

%%------------------------------------------------------------
%% Emit encoding/decoding comment
%%------------------------------------------------------------
emit_coding_comment(G, N, Fd, String, RefOrVal, Type, Name) ->
    emit(Fd, "  /* ~s parameter: ~s~s ~s */\n", 
	 [String, ic_cbe:mk_c_type(G, N, Type), RefOrVal, Name]).

%%------------------------------------------------------------
%% User protocol prefix for generic functions
%%------------------------------------------------------------
get_user_proto(G, Default) ->
    case ic_options:get_opt(G, user_protocol) of
	false ->
	    Default;
	Pfx ->
	    Pfx
     end.

%%------------------------------------------------------------
%% Timeout. Returns a string (or Default).
%%------------------------------------------------------------
get_c_timeout(G, Default) ->
    case ic_options:get_opt(G, c_timeout) of
	Tmo when is_integer(Tmo) ->
	    TmoStr = integer_to_list(Tmo),
	    {TmoStr, TmoStr};
	{SendTmo, RecvTmo}  when is_integer(SendTmo) andalso is_integer(RecvTmo) ->
	    {integer_to_list(SendTmo), integer_to_list(RecvTmo)};
	false ->
	    Default
    end.

%%------------------------------------------------------------
%% ZIPPERS (merging of successive elements of two lists).
%%------------------------------------------------------------

%% zip([H1| T1], [H2| T2]) ->
%%     [{H1, H2}| zip(T1, T2)];
%% zip([], []) ->
%%     [].

filterzip(F, [H1| T1], [H2| T2]) ->
    case F(H1, H2) of
	false ->
	    filterzip(F, T1, T2);
	{true, Val} ->
	    [Val| filterzip(F, T1, T2)]
    end;
filterzip(_, [], []) ->
    [].
    

ifelse(true, A, _) ->
    A;
ifelse(false, _, B) ->
    B.
