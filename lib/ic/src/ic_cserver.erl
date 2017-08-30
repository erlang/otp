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
-module(ic_cserver).

%% This module implements generation of C server code, where the
%% server acts as an Erlang C-node, where the functionality is that of
%% a gen_server (in C), and where the communication thus is according
%% to the Erlang distribution protocol.
%%

-export([do_gen/3]).

%% Silly dialyzer.
-export([filterzip/3]).

%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-import(lists, [foreach/2, foldl/3, foldr/3, map/2]).
-import(ic_codegen, [emit/2, emit/3, emit/4, emit_c_enc_rpt/4, emit_c_dec_rpt/4]).

-include("icforms.hrl").
-include("ic.hrl").
-include_lib("stdlib/include/erl_compile.hrl").

-define(IC_HEADER, "ic.h").
-define(ERL_INTERFACEHEADER, "erl_interface.h").
-define(EICONVHEADER, "ei.h").
-define(OE_MSGBUFSIZE, "OE_MSGBUFSIZE").
-define(ERLANGATOMLENGTH, "256").

%%------------------------------------------------------------
%%
%% Entry point
%%
%%------------------------------------------------------------
do_gen(G, File, Form) -> 
    OeName = ic_util:mk_oe_name(G, remove_ext(ic_util:to_list(File))), 
    G2 = ic_file:filename_push(G, [], OeName, c_server), 
    gen_headers(G2, [], Form), 
    R = gen(G2, [], Form), 
    ic_file:filename_pop(G2, c), 
    R.

remove_ext(File) ->
    filename:rootname(filename:basename(File)).

%%------------------------------------------------------------
%%
%% Generate the server side C stub and header files.
%%
%% For each module a separate file is generated.
%%
%% 
%%------------------------------------------------------------

gen(G, N, [X| Xs]) when is_record(X, preproc) ->
    NewG = change_file_stack(G, N, X#preproc.cat, X), 
    gen(NewG, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, module) ->
    CD = ic_code:codeDirective(G, X), 
    G2 = ic_file:filename_push(G, N, X, CD), 
    N2 = [ic_forms:get_id2(X)| N], 
    gen_headers(G2, N2, X), 
    gen(G2, N2, ic_forms:get_body(X)), 
    G3 = ic_file:filename_pop(G2, CD), 
    gen(G3, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, interface) ->
    G2 = ic_file:filename_push(G, N, X, c_server), 
    N2 = [ic_forms:get_id2(X)| N], 
    gen_prototypes(G2, N2, X), 
    gen_serv(G2, N2, X), 
    G3 = ic_file:filename_pop(G2, c), 
    gen(G3, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, const) ->
    emit_constant(G, N, X), 
    gen(G, N, Xs);

gen(G, N, [X| Xs]) when is_record(X, op) ->
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

gen(G, N, [_| Xs]) ->
    gen(G, N, Xs);

gen(_G, _N, []) -> 
    ok.

%%------------------------------------------------------------
%% Change file stack
%%------------------------------------------------------------

change_file_stack(G, _N, line_nr, X) ->
    Id = ic_forms:get_id2(X), 
    Flags = X#preproc.aux, 
    case Flags of
	[] -> ic_genobj:push_file(G, Id);
	_ ->
	    foldr(
	      fun({_, _, "1"}, G1) -> ic_genobj:push_file(G1, Id);
		 ({_, _, "2"}, G1) -> ic_genobj:pop_file(G1, Id);
		 ({_, _, "3"}, G1) -> ic_genobj:sys_file(G1, Id) 
	      end, G, Flags)
    end;
change_file_stack(G, _N, _Other, _X) ->
    G.

%%------------------------------------------------------------
%% Generate headers
%%------------------------------------------------------------

%% Some items have extra includes
gen_headers(G, N, X) when is_record(X, module) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    IncludeFileStack = ic_genobj:include_file_stack(G), 
	    Filename = lists:nth(length(N) + 1, IncludeFileStack), 
	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]), 
	    ic_code:gen_includes(HFd, G, X, c_server);
	false -> ok
    end;
gen_headers(G, [], _X) -> 
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    HFd = ic_genobj:hrlfiled(G), 
	    emit(HFd, "#include <stdlib.h>\n"), 
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
	    ic_code:gen_includes(HFd, G, c_server);
	false -> ok
    end;
gen_headers(_G, _N, _X) -> 
    ok.

%%------------------------------------------------------------
%% Generate prototypes
%%------------------------------------------------------------

gen_prototypes(G, N, X) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
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

	    IName = ic_util:to_undersc(N), 
	    INameUC = ic_util:to_uppercase(IName), 

	    emit(HFd, "#include \"~s\"\n", [filename:basename(Filename)]), 
	    ic_code:gen_includes(HFd, G, X, c_server), 
	    ic_codegen:nl(HFd), 

	    emit(HFd, "\n#ifndef __~s__\n", [ic_util:to_uppercase(IName)]), 
	    emit(HFd, "#define __~s__\n", [ic_util:to_uppercase(IName)]), 
	    ic_codegen:mcomment_light(HFd, 
				      [io_lib:format("Interface "
						     "object "
						     "definition: ~s", 
						     [IName])], c), 
	    case get_c_timeout(G, "") of
		"" ->
		    ok;
		{SendTmo, RecvTmo} ->
		    emit(HFd, "#define OE_~s_SEND_TIMEOUT  ~s\n", 
			 [INameUC, SendTmo]), 
		    emit(HFd, "#define OE_~s_RECV_TIMEOUT  ~s\n", 
			 [INameUC, RecvTmo]), 
		    emit(HFd, "#ifndef EI_HAVE_TIMEOUT\n"),
		    emit(HFd, "#error Functions for send and receive with "
			 "timeout not defined in erl_interface\n"),
		    emit(HFd, "#endif\n\n")
	    end,

	    emit(HFd, "typedef CORBA_Object ~s;\n\n", [IName]), 	
	    emit(HFd, "#endif\n\n"), 
	    
	    Bodies = [{N, ic_forms:get_body(X)}| X#interface.inherit_body],

	    emit(HFd, "\n/* Structure definitions  */\n", []), 
	    foreach(fun({N2, Body}) ->
			    emit_structs_inside_module(G, HFd, N2, Body) end, 
		    Bodies),

	    emit(HFd, "\n/* Switch and exec functions  */\n", []), 
	    emit(HFd, "int ~s__switch(~s oe_obj, CORBA_Environment "
		 "*oe_env);\n", [IName, IName]), 
	    foreach(fun({_N2, Body}) ->
			    emit_exec_prototypes(G, HFd, N, Body) end, 
		    Bodies),

	    emit(HFd, "\n/* Generic decoder */\n", []), 
	    emit(HFd, "int ~s__call_info(~s oe_obj, CORBA_Environment "
		 "*oe_env);\n", [IName, IName]), 

	    emit(HFd, "\n/* Restore function typedefs */\n", []), 
	    foreach(fun({_N2, Body}) ->
			    emit_restore_typedefs(G, HFd, N, Body) end, 
		    Bodies),

	    emit(HFd, "\n/* Callback functions */\n", []), 
	    foreach(fun({_N2, Body}) ->
			    emit_callback_prototypes(G, HFd, N, Body) end, 
		    Bodies),

	    emit(HFd, "\n/* Parameter decoders */\n", []), 
	    foreach(fun({_N2, Body}) ->
			    emit_decoder_prototypes(G, HFd, N, Body) end, 
		    Bodies),

	    emit(HFd, "\n/* Message encoders */\n", []), 
	    foreach(fun({_N2, Body}) ->
			    emit_encoder_prototypes(G, HFd, N, Body) end, 
		    Bodies),

	    %% Emit operation mapping structures
	    emit_operation_mapping_declaration(G, HFd, N, Bodies), 

	    ok;

	false -> 
	    ok
    end.

%%------------------------------------------------------------
%% Generate the server encoding/decoding function
%%------------------------------------------------------------


gen_serv(G, N, X) ->
    case ic_genobj:is_stubfile_open(G) of
	true ->
	    Fd = ic_genobj:stubfiled(G), 

	    emit_switch(G, Fd, N, X), 
	    emit_server_generic_decoding(G, Fd, N), 

	    %% Sets the temporary variable counter.
	    put(op_variable_count, 0), 
	    put(tmp_declarations, []), 

	    %% Generate exec, decode and encoding functions, and
	    %% table of exec functions.
	    Bodies = [{N, ic_forms:get_body(X)}| 
		      X#interface.inherit_body],

	    foreach(fun({_N2, Body}) ->
			    emit_dispatch(G, Fd, N, Body) end, 
		    Bodies), 
	    emit_operation_mapping(G, Fd, N, Bodies);
	false ->
	    ok
    end.

%%------------------------------------------------------------
%% Emit structs inside module
%%------------------------------------------------------------

emit_structs_inside_module(G, _Fd, N, Xs)->
    lists:foreach(
      fun(X) when is_record(X, enum) ->
	      icenum:enum_gen(G, N, X, c);
	 (X) when is_record(X, typedef) ->
	      icstruct:struct_gen(G, N, X, c);
	 (X) when is_record(X, struct) ->
	      icstruct:struct_gen(G, N, X, c);
	 (X) when is_record(X, union) ->
	      icstruct:struct_gen(G, N, X, c);
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Emit exec prototypes
%%------------------------------------------------------------

emit_exec_prototypes(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when is_record(X, op) ->
	      {ScopedName, _, _} = ic_cbe:extract_info(G, N, X), 
	      emit(Fd, 
		   "int ~s__exec(~s oe_obj, CORBA_Environment *oe_env);\n", 
		   [ScopedName, ic_util:to_undersc(N)]);
	 (X) when is_record(X, const) ->
	      emit_constant(G, N, X); 
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Emit restore typedefs
%%------------------------------------------------------------

emit_restore_typedefs(G, Fd, N, [X| Xs]) when is_record(X, op) ->
    %% Check if to use scoped call names
    {ScopedName, ArgNames, Types} = ic_cbe:extract_info(G, N, X), 
    {RetType, ParTypes, _} = Types, 
    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames),
    RT = mk_c_ret_type(G, N, RetType), 

    PL = ic_util:mk_list(mk_par_list_for_callback_prototypes(G, N, X, 
							     TypeAttrArgs)), 
    RPL = case PL of
	      "" ->
		  "";
	      _PL ->
		  ", " ++ PL
	  end, 

    case RT of
	"void" ->
	    case PL of 
		"" ->
		    emit(Fd, "typedef void (*~s__rs(~s oe_obj, "
			 "CORBA_Environment *oe_env));\n", 
			 [ScopedName, ic_util:to_undersc(N)]);
		_ ->
		    emit(Fd, "typedef void (*~s__rs(~s oe_obj, ~s, "
			 "CORBA_Environment *oe_env));\n", 
			 [ScopedName, ic_util:to_undersc(N), PL])
	    end;

	"erlang_port*" ->
	    emit(Fd, "typedef void (*~s__rs(~s oe_obj, ~s~s, "
		 "CORBA_Environment *oe_env));\n", 
		 [ScopedName, ic_util:to_undersc(N), RT, RPL]);

	"erlang_pid*" ->
	    emit(Fd, "typedef void (*~s__rs(~s oe_obj, ~s~s, "
		 "CORBA_Environment *oe_env));\n", 
		 [ScopedName, ic_util:to_undersc(N), RT, RPL]);

	"erlang_ref*" ->
	    emit(Fd, "typedef void (*~s__rs(~s oe_obj, ~s~s, "
		 "CORBA_Environment *oe_env));\n", 
		 [ScopedName, ic_util:to_undersc(N), RT, RPL]);

	_ ->
	    case ictype:isArray(G, N, RetType) of
		true ->
		    emit(Fd, "typedef void (*~s__rs(~s oe_obj, ~s~s, "
			 "CORBA_Environment *oe_env));\n", 
			 [ScopedName, ic_util:to_undersc(N), RT, RPL]);
		false ->
		    emit(Fd, "typedef void (*~s__rs(~s oe_obj, ~s*~s, "
			 "CORBA_Environment *oe_env));\n", 
			 [ScopedName, ic_util:to_undersc(N), RT, RPL])
	    end
    end, 
    emit_restore_typedefs(G, Fd, N, Xs);
emit_restore_typedefs(G, Fd, N, [X| Xs]) when is_record(X, attr) ->
    emit_restore_typedefs(G, Fd, N, Xs);
emit_restore_typedefs(G, Fd, N, [_X| Xs]) ->
    emit_restore_typedefs(G, Fd, N, Xs);
emit_restore_typedefs(_G, _Fd, _N, []) -> ok.


%%------------------------------------------------------------
%% Emit call-back prototypes
%%------------------------------------------------------------

emit_callback_prototypes(G, Fd, N, [X| Xs]) when is_record(X, op) ->
    %% Check scoped names XXX
    {ScopedName, ArgNames, Types} = ic_cbe:extract_info(G, N, X), 
    {RetType, ParTypes, _} = Types, 
    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames),
    RT = mk_c_ret_type(G, N, RetType), 

    PL = ic_util:mk_list(mk_par_list_for_callback_prototypes(G, N, X, 
							     TypeAttrArgs)), 
    CBPL = case PL of
	       "" ->
		   "";
	       _PL ->
		   ", " ++ PL
	   end, 
    case RT of
	"void" ->
	    case PL of
		"" ->
		    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, "
			 "CORBA_Environment *oe_env);\n", 
			 [ScopedName, ScopedName, ic_util:to_undersc(N)]);
		_ ->
		    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, ~s, "
			 "CORBA_Environment *oe_env);\n", 
			 [ScopedName, ScopedName, ic_util:to_undersc(N), PL])
	    end;
	"erlang_port*" ->
	    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, ~s~s, "
		 "CORBA_Environment *oe_env);\n", 
		 [ScopedName, ScopedName, ic_util:to_undersc(N), RT, CBPL]);

	"erlang_pid*" ->
	    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, ~s~s, "
		 "CORBA_Environment *oe_env);\n", 
		 [ScopedName, ScopedName, ic_util:to_undersc(N), RT, CBPL]);

	"erlang_ref*" ->
	    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, ~s~s, "
		 "CORBA_Environment *oe_env);\n", 
		 [ScopedName, ScopedName, ic_util:to_undersc(N), RT, CBPL]);

	_ ->
	    case ictype:isArray(G, N, RetType) of
		true ->
		    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, ~s~s, "
			 "CORBA_Environment *oe_env);\n", 
			 [ScopedName, ScopedName, ic_util:to_undersc(N), RT, 
			  CBPL]);
		false ->
		    emit(Fd, "~s__rs* ~s__cb(~s oe_obj, ~s*~s, "
			 "CORBA_Environment *oe_env);\n", 
			 [ScopedName, ScopedName, ic_util:to_undersc(N), RT, 
			  CBPL])
	    end
    end, 
    emit_callback_prototypes(G, Fd, N, Xs);
emit_callback_prototypes(G, Fd, N, [X| Xs]) when is_record(X, attr) ->
    emit_callback_prototypes(G, Fd, N, Xs);
emit_callback_prototypes(G, Fd, N, [_X| Xs]) ->
    emit_callback_prototypes(G, Fd, N, Xs);
emit_callback_prototypes(_G, _Fd, _N, []) -> ok.

%%------------------------------------------------------------
%% Emit decoder prototypes
%%------------------------------------------------------------

emit_decoder_prototypes(G, Fd, N, [X| Xs]) when is_record(X, op) ->
    %% Check if to use scoped call names
    {ScopedName, ArgNames, Types} = ic_cbe:extract_info(G, N, X), 
    {_RetType, ParTypes, _} = Types, 
    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames),
    case ic_util:mk_list(mk_par_list_for_decoder_prototypes(G, N, X, 
							    TypeAttrArgs)) of
	"" ->
	    ok;
	PLFDP ->
	    emit(Fd, "int ~s__dec(~s oe_obj, ~s, CORBA_Environment "
		 "*oe_env);\n", 
		 [ScopedName, ic_util:to_undersc(N), PLFDP])
    end, 
    emit_decoder_prototypes(G, Fd, N, Xs);
emit_decoder_prototypes(G, Fd, N, [X| Xs]) when is_record(X, attr) ->
    emit_decoder_prototypes(G, Fd, N, Xs);
emit_decoder_prototypes(G, Fd, N, [_X| Xs]) ->
    emit_decoder_prototypes(G, Fd, N, Xs);
emit_decoder_prototypes(_G, _Fd, _N, []) -> ok.


%%------------------------------------------------------------
%% Emit encoder prototypes
%%------------------------------------------------------------

emit_encoder_prototypes(G, Fd, N, [X| Xs]) when is_record(X, op) ->
    case ic_forms:is_oneway(X) of
	true ->
	    emit_encoder_prototypes(G, Fd, N, Xs);
	false ->
	    %% Check if to use scoped call names
	    {ScopedName, ArgNames, Types} = ic_cbe:extract_info(G, N, X), 
	    {RetType, ParTypes, _} = Types, 
	    TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames),
	    RType = mk_c_ret_type(G, N, RetType), 
	    case ic_util:mk_list(mk_par_list_for_encoder_prototypes(
				   G, N, X, TypeAttrArgs)) of
		"" ->
		    case RType of
			"void" ->
			    emit(Fd, "int ~s__enc(~s oe_obj, "
				 "CORBA_Environment *oe_env);\n", 
				 [ScopedName, ic_util:to_undersc(N)]);
			_ ->
			    emit(Fd, "int ~s__enc(~s oe_obj, ~s, "
				 "CORBA_Environment *oe_env);\n", 
				 [ScopedName, ic_util:to_undersc(N), RType])
		    end;
		PLFEP ->
		    case RType of
			"void" ->
			    emit(Fd, "int ~s__enc(~s oe_obj, ~s, "
				 "CORBA_Environment *oe_env);\n", 
				 [ScopedName, ic_util:to_undersc(N), PLFEP]);
			_ ->
			    emit(Fd, "int ~s__enc(~s oe_obj, ~s, ~s, "
				 "CORBA_Environment *oe_env);\n", 
				 [ScopedName, ic_util:to_undersc(N), RType, 
				  PLFEP])
		    end
	    end, 
	    emit_encoder_prototypes(G, Fd, N, Xs)
    end;
emit_encoder_prototypes(G, Fd, N, [X| Xs]) when is_record(X, attr) ->
    emit_encoder_prototypes(G, Fd, N, Xs);
emit_encoder_prototypes(G, Fd, N, [_X| Xs]) ->
    emit_encoder_prototypes(G, Fd, N, Xs);
emit_encoder_prototypes(_G, _Fd, _N, []) -> ok.

%%------------------------------------------------------------
%% Emit operation mapping declaration
%%------------------------------------------------------------

emit_operation_mapping_declaration(G, Fd, N, Bodies) ->
    Interface = ic_util:to_undersc(N), 
    Length = erlang:length(get_all_opnames(G, N, Bodies)),
    emit(Fd, "\n/* Operation mapping */\n", []), 
    emit(Fd, "extern oe_map_t oe_~s_map;\n", [Interface]), 
    emit(Fd, "/* For backward compatibility */\n"),
    emit(Fd, "#define ___~s_map___ oe_~s_map\n", 
		 [Interface, Interface]),
    case Length of 
	0 ->
	    ok;
	_ ->
	    emit(Fd, "extern oe_operation_t oe_~s_operations[];\n", 
		 [Interface]), 
	    emit(Fd, "/* For backward compatibility */\n"),
	    emit(Fd, "#define ___~s_operations___ oe_~s_operations\n", 
		 [Interface, Interface])
    end.


%% Returns a list of {OpName, ScopedOpName} for all operations, where
%% OpName == ScopedOpName in case the `scoped_op_calls' option has
%% been set.
%%
get_all_opnames(G, N, Bodies) ->
    ScNF = fun(X) ->
		  {ScName, _, _} = ic_cbe:extract_info(G, N, X),
		  ScName
	  end, 
    NF = case ic_options:get_opt(G, scoped_op_calls) of
	    true ->
		ScNF;
	    false  ->
		fun(X) -> ic_forms:get_id2(X) end
	end,
    Filter = fun(X) when is_record(X, op) -> 
		     {true, {NF(X), ScNF(X)}};
		(_) ->
		     false
	     end,
    %% zf == filtermap
    lists:flatmap(fun({_, Xs}) -> lists:zf(Filter, Xs) end, Bodies).

%%------------------------------------------------------------
%% Emit switch 
%%------------------------------------------------------------

emit_switch(G, Fd, N, _X) ->
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
    StartCode =
	"#include \"ic.h\"\n"
	"#include \"erl_interface.h\"\n"
	"#include \"ei.h\"\n"
	"#include \"~s__s.h\"\n\n"
	"/*\n"
	" * Main switch\n"
	" */\n\n"
	"int ~s__switch(~s oe_obj, CORBA_Environment *oe_env)\n"
	"{\n"
	"   return oe_exec_switch(oe_obj, oe_env, &oe_~s_map);\n"
	"}\n\n", 
    ScopedName = ic_util:to_undersc(N), 
    emit(Fd, StartCode, [ScopedName, ScopedName, ScopedName, ScopedName]).

%%------------------------------------------------------------
%% Emit server generic decoding. 
%%------------------------------------------------------------

emit_server_generic_decoding(G, Fd, N) ->
    UserProto = get_user_proto(G, oe),
    Code = 
	"/*\n"
	" * Returns call identity (left only for backward compatibility)\n"
	" */\n\n"
	"int ~s__call_info(~s oe_obj, CORBA_Environment *oe_env)\n"
	"{\n"
	"   return ~s_prepare_request_decoding(oe_env);\n"
	"}\n\n", 
    IName = ic_util:to_undersc(N), 
    emit(Fd, Code, [IName, IName, UserProto]).

%%------------------------------------------------------------
%% Emit dispatch
%%------------------------------------------------------------

emit_dispatch(G, Fd, N, Xs) ->
    lists:foreach(
      fun(X) when is_record(X, op) ->
	      {Name, ArgNames, Types} = ic_cbe:extract_info(G, N, X), 
	      {RetType, ParTypes, _} = Types, 
	      TypeAttrArgs = mk_type_attr_arg_list(ParTypes, ArgNames),
	      emit_exec_function(G, Fd, N, X, Name, RetType, TypeAttrArgs), 
	      emit_parameter_decoder(G, Fd, N, X, Name, RetType, TypeAttrArgs),
	      emit_message_encoder(G, Fd, N, X, Name, RetType, TypeAttrArgs);
	 (_) ->
	      ok
      end, Xs).

%%------------------------------------------------------------
%% Emit operation mapping
%%------------------------------------------------------------

emit_operation_mapping(G, Fd, N, Bodies) ->
    OpNames = get_all_opnames(G, N, Bodies),
    Interface = ic_util:to_undersc(N), 
    Length = erlang:length(OpNames), 
    emit(Fd, "\n/* Operation mapping */\n\n", []), 
    case Length of 
	0 ->
	    emit(Fd, "oe_map_t oe_~s_map = { 0, NULL };\n\n", [Interface]);
	_ ->
	    emit(Fd, "\noe_operation_t oe_~s_operations[~p]  =  {\n",
		 [Interface, Length]), 
	    Members = lists:map(
			fun({OpN, ScOpN}) ->
				Name = ic_util:to_undersc([OpN]), 
				ScName = ic_util:to_undersc([ScOpN]), 
				io_lib:fwrite("  {~p, ~p, ~s__exec}", 
					      [Interface, Name, ScName])
			end, OpNames),
	    emit(Fd, ic_util:join(Members, ",\n")),
	    emit(Fd, "};\n\n", []), 
	    emit(Fd, "oe_map_t oe_~s_map = "
		 "{~p, oe_~s_operations};\n\n", 
		 [Interface, Length, Interface])
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
	    emit(Fd, "#define __~s__\n\n", [UCName]), 

	    emit(Fd, "/* Constant: ~s */\n", [CName]), 

	    if is_record(ConstRecord#const.type, wstring) -> 
		    %% If wstring, add 'L' 
		    emit(Fd, "#define ~s L~p\n\n", [CName, 
						    ConstRecord#const.val]);
	       true ->
		    emit(Fd, "#define ~s ~p\n\n", [CName, 
						   ConstRecord#const.val])
	    end, 

	    emit(Fd, "#endif\n\n")
    end.

%%------------------------------------------------------------
%% Emit exec function
%%------------------------------------------------------------

emit_exec_function(G, Fd, N, X, Name, RetType, TypeAttrArgs) ->
    %% Decoding operation specific part
    InTypeAttrArgs = lists:filter(fun({_, in, _}) -> true;
				({_, _, _}) -> false
			     end, TypeAttrArgs), 
    ic_codegen:nl(Fd), 

    emit(Fd, 
	 "int ~s__exec(~s oe_obj, CORBA_Environment *oe_env)\n"
	 "{\n", 
	 [Name, ic_util:to_undersc(N)]), 

    emit(Fd, "  if (oe_env->_received != ~p) {\n", [length(InTypeAttrArgs)]), 
    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, BAD_PARAM, "
	 "\"Wrong number of operation parameters\");\n"), 
    emit_c_dec_rpt(Fd, "    ", "wrong number of parameters", []),
    emit_c_dec_rpt(Fd, "    ", "server exec ~s\\n====\\n", [Name]),
    emit(Fd, "    return -1;\n", []), 
    emit(Fd, "  }\n"), 
    emit(Fd, "  else {\n", []), 

    case InTypeAttrArgs of
	[] ->
	    true;
	_ ->
	    emit(Fd, "    int oe_error_code = 0;\n")
    end, 

    %% Callback variable definition
    emit_variable_defs(G, Fd, N, X, Name, RetType, TypeAttrArgs),  

    %% Call to parameter decoder
    emit_parameter_decoder_call(G, Fd, N, X, Name, RetType, TypeAttrArgs),

    %% Callback to user code
    emit_callback(G, Fd, N, X, Name, RetType, TypeAttrArgs), 

    %% Call to return message encoder 
    case ic_forms:is_oneway(X) of 
	true ->
	    true;
        false ->
	    emit_message_encoder_call(G, Fd, N, X, Name, RetType, TypeAttrArgs)
    end, 

    %% Restore function call
    emit_restore(G, Fd, N, X, Name, RetType, TypeAttrArgs), 

    emit(Fd, "  }\n  return 0;\n}\n\n").

%%------------------------------------------------------------
%% Emit parameter decoder
%%------------------------------------------------------------

emit_parameter_decoder(G, Fd, N, X, Name, _RetType, TypeAttrArgs) ->
    %% Decoding operation specific part
    InTypeAttrArgs = 
	lists:filter(fun({_, in, _}) -> true;
			({_, _, _}) -> false
		     end, TypeAttrArgs), 
    case InTypeAttrArgs of
	[] ->
	    ok;
	_ ->
	    case ic_util:mk_list(mk_par_list_for_decoder(G, N, X, 
							 TypeAttrArgs)) of
		"" ->
		    emit(Fd, "int ~s__dec(~s oe_obj, CORBA_Environment "
			 "*oe_env)\n{\n  int oe_error_code;\n\n", 
			 [Name, ic_util:to_undersc(N)]);
		PLFD ->
		    emit(Fd, "int ~s__dec(~s oe_obj, ~s, CORBA_Environment "
			 "*oe_env)\n{\n", 
			 [Name, ic_util:to_undersc(N), PLFD]), 
		    emit(Fd, "  int oe_error_code;\n\n")
	    end, 
	    
	    APars = [],				% XXX Alloced parameters
	    foldl(
	      fun({{'void', _}, _, _}, _Acc) ->
		      ok;
		 ({T1, A1, N1}, Acc) ->
		      emit_one_decoding(G, N, Fd, T1, A1, N1, Acc)
	      end, APars, InTypeAttrArgs),
	    
	    emit(Fd, "  return 0;\n}\n\n") 
    end.
	
%%------------------------------------------------------------
%% Emit one decoding
%%------------------------------------------------------------

emit_one_decoding(G, N, Fd, T1, A1, N1, AllocedPars) ->
    IndOp = mk_ind_op(A1), 
    case ic_cbe:is_variable_size(G, N, T1) of
	false ->
	    %% The last parameter "oe_outindex" is not used in 
	    %% the static case but must be there anyhow. 
	    emit_decoding_stmt(G, N, Fd, T1,  
			       N1, "", "oe_env->_inbuf", 1, "&oe_outindex", 
			       caller, AllocedPars), 
	    ic_codegen:nl(Fd),
	    AllocedPars;
	true ->
	    emit_encoding_comment(G, N, Fd, "Decode", IndOp, T1, N1), 
	    emit(Fd, "  {\n"), 
	    emit(Fd, "    int oe_size_count_index = oe_env->_iin;\n"), 
	    emit(Fd, "    int oe_malloc_size = 0;\n"), 
	    emit(Fd, "    void *oe_first = NULL;\n"), 
	    ic_cbe:emit_malloc_size_stmt(G, N, Fd, T1, 
					 "oe_env->_inbuf", 1, caller), 
	    %% This is the only malloc call in this file
	    emit(Fd, 
		 "    OE_MALLOC_SIZE_CHECK(oe_env, oe_malloc_size);\n" 
		 "    if ((*~s = oe_first = "
		 "malloc(oe_malloc_size)) == NULL) {\n", [N1]),
	    ic_cbe:emit_dealloc_stmts(Fd, "        ", AllocedPars),
	    emit(Fd,
		 "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "NO_MEMORY, \"Cannot malloc\");\n" 
		 "      return -1;\n"	
		 "    }\n"),
	    ParName = "*" ++ N1,		% XXX Why not IndOp?
	    NAllocedPars = [ParName| AllocedPars],
	    case ictype:isArray(G, N, T1) of
		true ->
		    emit_decoding_stmt(G, N, Fd, T1,  
				       "(*" ++ IndOp ++ N1 ++ ")", "", 
				       "oe_env->_inbuf", 1, "&oe_outindex", 
				       array_dyn, NAllocedPars);
		false ->
		    emit_decoding_stmt(G, N, Fd, T1,  
				       "(*" ++ IndOp ++ N1 ++ ")", "", 
				       "oe_env->_inbuf", 1, "&oe_outindex", 
				       caller_dyn, NAllocedPars)
	    end, 
	    emit(Fd, "  }\n\n"),
	    NAllocedPars
    end.

%%------------------------------------------------------------
%% Emit message encoder
%%------------------------------------------------------------

emit_message_encoder(G, Fd, N, X, Name, RetType, TypeAttrArgs) ->
    case ic_forms:is_oneway(X) of
	false ->
	    %% Encoding operation specific part
	    emit(Fd, 
		 "\nint ~s__enc(~s oe_obj", 
		 [Name, ic_util:to_undersc(N)]), 
	    RType = mk_c_ret_type(G, N, RetType), 
	    ParList = mk_par_list_for_encoder(G, N, X, TypeAttrArgs), 
	    case ic_util:mk_list(ParList) of 
		"" ->
		    case RType of
			"void" ->
			    emit(Fd, ", CORBA_Environment *oe_env)\n{");
			_ ->
			    emit(Fd, ", ~s oe_return, CORBA_Environment "
				 "*oe_env)\n{", [RType])
		    end;
		PLFD ->
		    case RType of
			"void" ->
			    emit(Fd, ", ~s, CORBA_Environment "
				 "*oe_env)\n{", [PLFD]);
			_ ->
			    emit(Fd, ", ~s oe_return~s, CORBA_Environment "
				 "*oe_env)\n{", [RType, ", " ++ PLFD])
		    end
	    end, 


	    emit(Fd, "\n"),
	    emit(Fd, "  int oe_error_code;\n\n"),
	    UserProto = get_user_proto(G, oe),
	    emit(Fd, "  ~s_prepare_reply_encoding(oe_env);\n", [UserProto]),

	    OutTypeAttrArgs = 
		lists:filter(fun({_, out, _}) -> true;
				({_, _, _}) -> false
			     end, TypeAttrArgs),

	    OutLength = length(OutTypeAttrArgs), 
	    case OutLength > 0 of
		false ->
		    ic_codegen:nl(Fd);
		true ->
		    emit(Fd, "  oe_ei_encode_tuple_header(oe_env, ~p);\n\n", 
			 [OutLength+1])

	    end, 

	    emit_encoding_comment(G, N, Fd, "Encode", "", RetType, 
				  "oe_return"), 
	    emit_encoding_stmt(G, N, X, Fd, RetType, "oe_return"), 

	    foreach(fun({T1, _A1, N1}) ->
			    case T1 of
				{'void', _} ->
				    ok;
				_ ->
				    emit_encoding_comment(G, N, Fd, "Encode", 
							  "", T1, N1), 
				    emit_encoding_stmt(G, N, X, Fd, T1, N1) 
			    end
		    end, OutTypeAttrArgs), 
	    emit(Fd, "  return 0;\n}\n\n");
	_ ->
	    %% Oneway
	    ok
    end.

%%------------------------------------------------------------
%% Emit message encoder call
%%------------------------------------------------------------

emit_message_encoder_call(G, Fd, N, X, Name, RetType, TypeAttrArgs) ->
    emit(Fd, "    /* Encoding reply message */\n"), 
    RType =  mk_c_ret_type(G, N, RetType), 
    case  ic_util:mk_list(mk_enc_par_list(G, N, X, TypeAttrArgs)) of
	"" ->
	    case RType of
		"void" ->
		    emit(Fd, "    ~s(oe_obj, oe_env);\n", 
			 [Name ++ "__enc"]);
		"erlang_pid*" ->
		    emit(Fd, "    ~s(oe_obj, &oe_return, oe_env);\n", 
			 [Name ++ "__enc"]);
		"erlang_port*" ->
		    emit(Fd, "    ~s(oe_obj, &oe_return, oe_env);\n", 
			 [Name ++ "__enc"]);
		"erlang_ref*" ->
		    emit(Fd, "    ~s(oe_obj, &oe_return, oe_env);\n", 
			 [Name ++ "__enc"]);
		_ ->
		    emit(Fd, "    ~s(oe_obj, oe_return, oe_env);\n", 
			 [Name ++ "__enc"])
	    end;

	PLFE ->
	    case RType of
		"void" ->
		    emit(Fd, "    ~s(oe_obj, ~s, oe_env);\n", 
			 [Name ++ "__enc", PLFE]);
		"erlang_pid*" ->
		    emit(Fd, "    ~s(oe_obj, &oe_return, ~s, oe_env);\n", 
			 [Name ++ "__enc", PLFE]);
		"erlang_port*" ->
		    emit(Fd, "    ~s(oe_obj, &oe_return, ~s, oe_env);\n", 
			 [Name ++ "__enc", PLFE]);
		"erlang_ref*" ->
		    emit(Fd, "    ~s(oe_obj, &oe_return, ~s, oe_env);\n", 
			 [Name ++ "__enc", PLFE]);
		_ ->
		    emit(Fd, "    ~s(oe_obj, oe_return, ~s, oe_env);\n", 
			 [Name ++ "__enc", PLFE])
	    end
    end, 
    ic_codegen:nl(Fd).

%%------------------------------------------------------------
%% Emit parameter decoding call
%%------------------------------------------------------------

emit_parameter_decoder_call(G, Fd, N, X, Name, _R, TypeAttrArgs) ->
    case ic_util:mk_list(mk_dec_par_list(G, N, X, TypeAttrArgs)) of
	"" -> %% No parameters ! skip it !
	    ok;
	PLFDC ->
	    ParDecName = Name ++ "__dec", 
	    emit(Fd, 
		 "    /* Decode parameters */\n" 
		 "    if((oe_error_code = ~s(oe_obj, ~s, oe_env)) < 0) {\n", 
		 [ParDecName, PLFDC]), 
	    emit_c_dec_rpt(Fd, "    ", "parmeters", []),
	    emit(Fd, 
		 "      if(oe_env->_major == CORBA_NO_EXCEPTION)\n"
		 "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad parameter on decode\");\n" 
		 "      return oe_error_code;\n  }\n\n")
    end.

%%------------------------------------------------------------
%% Emit call-back
%%------------------------------------------------------------

emit_callback(G, Fd, N, X, Name, RetType, TypeAttrArgs) ->
    CallBackName = Name ++ "__cb", 
    emit(Fd, "    /* Callback function call */\n"), 
    PL = ic_util:mk_list(mk_cb_par_list(G, N, X, TypeAttrArgs)), 
    case ic_forms:is_oneway(X) of
	true ->
	    case PL of
		"" ->
		    emit(Fd, "    oe_restore = ~s(oe_obj, oe_env);\n\n", 
			 [CallBackName]);
		_ ->
		    emit(Fd, "    oe_restore = ~s(oe_obj, ~s, oe_env);\n\n", 
			 [CallBackName, PL])
	    end;
	false ->
	    CBPL = case PL of
		       "" ->
			   "";
		       _PL ->
			   ", " ++ PL
		   end, 
	    case mk_c_ret_type(G, N, RetType) of
		"void" ->
		    case PL of 
			"" ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, oe_env);"
				 "\n\n", [CallBackName]);
			_ ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, ~s, oe_env);"
				 "\n\n", [CallBackName, PL])
		    end;
		_ ->
		    case ictype:isArray(G, N, RetType) of
			true ->
			    emit(Fd, 
				 "    oe_restore = ~s(oe_obj, oe_return~s, "
				 " oe_env);\n\n", [CallBackName, CBPL]);
			false ->
			    emit(Fd, "    oe_restore = ~s(oe_obj, "
				 "&oe_return~s, oe_env);\n\n", 
				 [CallBackName, CBPL])
		    end
	    end
    end.

%%------------------------------------------------------------
%% Emit restore
%%------------------------------------------------------------

emit_restore(G, Fd, N, X, _Name, RetType, TypeAttrArgs) ->
    emit(Fd, "    /* Restore function call */\n"), 
    emit(Fd, "    if (oe_restore != NULL)\n"), 
    PL = ic_util:mk_list(mk_cb_par_list(G, N, X, TypeAttrArgs)), 
    case ic_forms:is_oneway(X) of
	true ->
	    case PL of
		"" ->
		    emit(Fd, "      (*oe_restore)(oe_obj, oe_env);\n\n");
		_ ->
		    emit(Fd, "      (*oe_restore)(oe_obj, ~s, oe_env);\n\n", 
			 [PL])
	    end;
	false ->
	    RPL = case PL of
		      "" ->
			  "";
		      _PL ->
			  ", " ++ PL
		  end, 
	    case mk_c_ret_type(G, N, RetType) of
		"void" ->
		    case PL of
			"" ->
			    emit(Fd, "      (*oe_restore)(oe_obj, oe_env);"
				 "\n\n");
			_ ->
			    emit(Fd, "      (*oe_restore)(oe_obj, ~s, oe_env);"
				 "\n\n", [PL])
		    end;
		_ ->
		    case ictype:isArray(G, N, RetType) of
			true ->
			    emit(Fd, 
				 "      (*oe_restore)(oe_obj, oe_return~s, "
				 " oe_env);\n\n", [RPL]);
			false ->
			    emit(Fd, "      (*oe_restore)(oe_obj, "
				 "&oe_return~s, oe_env);\n\n", [RPL])
		    end
	    end
    end.

%%------------------------------------------------------------
%% Emit variable defs
%%------------------------------------------------------------

emit_variable_defs(G, Fd, N, X, _Name, RetType, TypeAttrArgs) ->
    {ScopedName, _, _} = ic_cbe:extract_info(G, N, X), 
    emit(Fd, "    ~s__rs* oe_restore = NULL;\n", [ScopedName]), 
    RestVars = mk_var_list(mk_var_decl_list(G, N, X, TypeAttrArgs)), 
    case ic_forms:is_oneway(X) of
	true ->
	    emit(Fd, "~s\n\n", [RestVars]);
	false ->
	    RType =  mk_c_ret_type(G, N, RetType), 
	    case RType of
		"void" ->
		    emit(Fd, "~s\n\n", [RestVars]);
		"CORBA_unsigned_long" -> 
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_unsigned_long_long" -> 
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_unsigned_short" -> 
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_short" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_long" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_long_long" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_float" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_double" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_char" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_wchar" ->  %% WCHAR
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_boolean" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		"CORBA_octet" ->
		    emit(Fd, "~s    ~s oe_return = 0;\n\n", [RestVars, RType]);
		_ ->
		    case ic_cbe:is_variable_size(G, N, RetType) of
			true ->
			    emit(Fd, "~s    ~s oe_return;\n\n", 
				 [RestVars, RType]);
			false ->
			    TK = ic_forms:get_tk(X), 
			    case TK of
				{tk_enum, _, _, _List} ->
				    emit(Fd, "~s    ~s oe_return;\n\n", 
					 [RestVars, RType]);
				_ ->
				    case RType of
					"erlang_binary*" ->
					    emit(Fd, "~s    erlang_binary "
						 "oe_return;\n\n", [RestVars]);
					"erlang_pid*" ->
					    emit(Fd, "~s    erlang_pid "
						 "oe_return;\n\n", [RestVars]);
					"erlang_port*" ->
					    emit(Fd, "~s    erlang_port "
						 "oe_return;\n\n", [RestVars]);
					"erlang_ref*" ->
					    emit(Fd, "~s    erlang_ref "
						 "oe_return;\n\n", [RestVars]);
					_ ->
					    %% Structures are
					    %% initiated by memset
					    emit(Fd, "~s    ~s "
						 "oe_return;\n\n", 
						 [RestVars, RType])
				    end, 
				    emit(Fd, "    memset(&oe_return, 0, "
					 "sizeof(oe_return));\n\n")
			    end
		    end
	    end
    end.

%%------------------------------------------------------------
%% Make variable list
%%------------------------------------------------------------

%% XXX Modify
mk_var_list([]) -> 
    "";
mk_var_list([Arg| Args]) ->
    "    " ++ Arg ++ ";\n" ++ mk_var_list(Args).

%%------------------------------------------------------------
%% Make return type
%%------------------------------------------------------------

mk_c_ret_type(G, N, Type) ->
    Ctype = mk_c_type(G, N, Type), 
    Dyn = case ic_cbe:is_variable_size(G, N, Type) of
	      true ->
		  if 
		      is_record(Type, string) ->
			  "*";
		      Ctype == "CORBA_char *" ->
			  "";
		      is_record(Type, wstring) ->  %% WSTRING
			  "*";
		      Ctype == "CORBA_wchar *" ->  %% WSTRING
			  "";
		      true ->
			  case ictype:isArray(G, N, Type) of
			      true ->
				  "";
			      _ ->
				  "*"
			  end
		  end;
	      false ->
		  if 
		      Ctype == "erlang_pid" ->
			  "*";
		      Ctype == "erlang_port" ->
			  "*";
		      Ctype == "erlang_ref" ->
			  "*";
		      true ->
			  ""
		  end
	  end, 
    Ctype ++ Dyn.

%%------------------------------------------------------------
%% Make call-back parameter list
%%------------------------------------------------------------

mk_cb_par_list(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [in, out], TypeAttrArgs0),
    lists:map(
      fun({Type, Attr, Arg}) ->
	      case ic_cbe:is_variable_size(G, N, Type) of
		  true ->
		      case Attr of
			  in ->
			      Arg;
			  out ->
			      case ictype:isArray(G, N, Type) of
				  true ->
				      Arg;
				  _ ->
				      "&" ++ Arg
			      end
		      end;
		  false ->
		      case ictype:isArray(G, N, Type) of
			  true ->
			      Arg;
			  _ ->
			      "&" ++ Arg
		      end
	      end
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make decoder parameter list
%%------------------------------------------------------------

mk_dec_par_list(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [in], 
					      TypeAttrArgs0),
    lists:map(
      fun({Type, _Attr, Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      case ic_cbe:is_variable_size(G, N, Type) of
		  true ->
		      if 
			  is_record(Type, string) ->
			      "&" ++ Arg;
			  Ctype == "CORBA_char *" ->
			      Arg;
			  is_record(Type, wstring) ->
			      "&" ++ Arg;
			  Ctype == "CORBA_wchar *" ->
			      Arg;
			  true ->
			      "&" ++ Arg
		      end;
		  false ->
		      case ictype:isArray(G, N, Type) of
			  true ->
			      Arg;
			  _ ->
			      "&" ++ Arg
		      end
	      end
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make encoder parameter list
%%------------------------------------------------------------

mk_enc_par_list(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [out], 
					      TypeAttrArgs0),
    lists:map(
      fun({Type, _Attr, Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      case Ctype of
		  "erlang_pid" ->
		      "&" ++ Arg;
		  "erlang_port" ->
		      "&" ++ Arg;
		  "erlang_ref" ->
		      "&" ++ Arg;
		  _ ->
		      Arg
	      end
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make type argument list
%%------------------------------------------------------------

mk_type_attr_arg_list(Types, Args) ->
    filterzip(
      fun(Type, {Attr, Arg}) ->
	      {true, {Type, Attr, Arg}}
      end, Types, Args).

%%------------------------------------------------------------
%% Filter type argument list
%%------------------------------------------------------------

filter_type_attr_arg_list(G, X, InOrOut, TypeAttrArgs) ->
    lists:filter(

      fun({_Type, inout, Arg}) ->
	      ic_error:error(G, {inout_spec_for_c, X, Arg}),
	      false;
	 ({_Type, Attr, _Arg}) ->
	      lists:member(Attr, InOrOut)
      end, TypeAttrArgs).

%%------------------------------------------------------------
%% Make indirection operator
%%------------------------------------------------------------

mk_ind_op(in) ->
    "";
mk_ind_op(inout) ->
    error;
mk_ind_op(_) ->
    "*".

%%------------------------------------------------------------
%% Make parameter list for decoder
%%------------------------------------------------------------

mk_par_list_for_decoder(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [in], TypeAttrArgs0),
    lists:map(
      fun({Type, Attr, Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      Dyn = case ic_cbe:is_variable_size(G, N, Type) of
			true ->
			    if 
				is_record(Type, string) ->
				    "**";
				Ctype == "CORBA_char *" ->
				    "";
				is_record(Type, wstring) ->  %% WSTRING
				    "**";
				Ctype == "CORBA_wchar *" ->  %% WSTRING
				    "";
				true ->
				    case ictype:isArray(G, N, Type) of
					true ->
					    slice(Attr) ++ "*";
					_ ->
					    "**"
				    end
			    end;
			false ->
			    case ictype:isArray(G, N, Type) of
				true ->
				    "";
				_ ->
				    "*"
			    end
		    end, 
	      Ctype ++ Dyn ++ " " ++ Arg
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make parameter list for encoder
%%------------------------------------------------------------

mk_par_list_for_encoder(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [out], TypeAttrArgs0),
    lists:map(
      fun({Type, _Attr, Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      Dyn = case ic_cbe:is_variable_size(G, N, Type) of
			true ->
			    if 
				is_record(Type, string) ->
				    "*";
				Ctype == "CORBA_char *" ->
				    "";
				is_record(Type, wstring) ->  %% WSTRING
				    "*";
				Ctype == "CORBA_wchar *" ->  %% WSTRING
				    "";
				true ->
				    case ictype:isArray(G, N, Type) of
					true ->
					    "";
					_ ->
					    "*"
				    end
			    end;
			false ->
			    if 
				Ctype == "erlang_pid" ->
				    "*";
				Ctype == "erlang_port" ->
				    "*";
				Ctype == "erlang_ref" ->
				    "*";
				true ->
				    ""
			    end
		    end, 
	      Ctype ++ " " ++ Dyn ++ Arg
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make parameter list for decoder prototypes
%%------------------------------------------------------------

mk_par_list_for_decoder_prototypes(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [in], TypeAttrArgs0),
    lists:map(
      fun({Type, Attr, _Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      Dyn = case ic_cbe:is_variable_size(G, N, Type) of
			true ->
			    if 
				is_record(Type, string) ->
				    "**";
				Ctype == "CORBA_char *" ->
				    "";
				is_record(Type, wstring) ->  %% WSTRING
				    "**";
				Ctype == "CORBA_wchar *" ->  %% WSTRING
				    "";
				true ->
				    case ictype:isArray(G, N, Type) of
					true ->
					    slice(Attr) ++ "*";
					_ ->
					    "**"
				    end
			    end;
			false ->
			    case ictype:isArray(G, N, Type) of
				true ->
				    "";
				_ ->
				    "*"
			    end
		    end, 
	      Ctype ++ Dyn
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make parameter list for encoder prototypes
%%------------------------------------------------------------

mk_par_list_for_encoder_prototypes(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [out], TypeAttrArgs0),
    lists:map(
      fun({Type, _Attr, _Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      Dyn = case ic_cbe:is_variable_size(G, N, Type) of
			true ->
			    if 
				is_record(Type, string) ->
				    "*";
				Ctype == "CORBA_char *" ->
				    "";
				is_record(Type, wstring) ->  %% WSTRING
				    "*";
				Ctype == "CORBA_wchar *" ->  %% WSTRING
				    "";
				true ->
				    case ictype:isArray(G, N, Type) of
					true ->
					    "";
					_ ->
					    "*"
				    end
			    end;
			false ->
			    if 
				Ctype == "erlang_pid" ->
				    "*";
				Ctype == "erlang_port" ->
				    "*";
				Ctype == "erlang_ref" ->
				    "*";
				true ->
				    ""
			    end
		    end, 
	      Ctype ++ Dyn
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make parameter list for call-back prototypes
%%------------------------------------------------------------

mk_par_list_for_callback_prototypes(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [in, out], 
					      TypeAttrArgs0),
    lists:map(
      fun({Type, Attr, _Arg}) ->
	      IndOp = mk_ind_op(Attr),
	      Ctype = mk_c_type(G, N, Type), 
	      Dyn = case ic_cbe:is_variable_size(G, N, Type) of
			true ->
			    if 
				is_record(Type, string) ->
				    "*" ++ IndOp;
				Ctype == "CORBA_char *" ->
				    "" ++ IndOp;
				is_record(Type, wstring) ->  %% WSTRING
				    "*" ++ IndOp;
				Ctype == "CORBA_wchar *" ->  %% WSTRING
				    "" ++ IndOp;
				true ->
				    case ictype:isArray(G, N, Type) of
					true ->
					    "";
					_ ->
					    "*" ++ IndOp
				    end
			    end;
			false ->
			    case ictype:isArray(G, N, Type) of
				true ->
				    "";
				_ ->
				    case Attr of  %% Should just be IndOp
					in ->
					    "*" ++ IndOp;
					out ->
					    IndOp
				    end
			    end
		    end, 
	      Ctype ++ Dyn
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Make variable declaration list 
%%------------------------------------------------------------
 
mk_var_decl_list(G, N, X, TypeAttrArgs0) ->
    TypeAttrArgs1 = filter_type_attr_arg_list(G, X, [in, out], 
					      TypeAttrArgs0),
    lists:map(
      fun({Type, Attr, Arg}) ->
	      Ctype = mk_c_type(G, N, Type), 
	      VarDecl = case ic_cbe:is_variable_size(G, N, Type) of
			    true ->
				if 
				    is_record(Type, string) ->
					Ctype ++ "* " ++ Arg ++ " = NULL";
				    Ctype == "CORBA_char *" ->
					Ctype ++ " " ++ Arg ++ " = NULL";
				    is_record(Type, wstring) ->  %% WSTRING
					Ctype ++ "* " ++ Arg ++ " = NULL";
				    Ctype == "CORBA_wchar *" ->  %% WSTRING
					Ctype ++ " " ++ Arg ++ " = NULL";
				    true ->
					case ictype:isArray(G, N, Type) of
					    true ->
						Ctype ++ slice(Attr) ++ " " ++ 
						    Arg;
					    _ ->
						Ctype ++ "* " ++ Arg
					end
				end;
			    false ->
				Ctype ++ " " ++ Arg
			end, 

	      VarDecl
      end, TypeAttrArgs1).

%%------------------------------------------------------------
%% Slice
%%------------------------------------------------------------

slice(in) ->
    "_slice*";
slice(_) ->
    "".

%%------------------------------------------------------------
%%    Special comment functions
%%------------------------------------------------------------

emit_encoding_comment(G, N, F, String, RefOrVal, Type, Name) ->
    emit(F, [io_lib:format("  /* ~s parameter: ~s~s ~s */\n", 
			   [String, mk_c_type(G, N, Type), 
			    RefOrVal, Name])]).


%%------------------------------------------------------------
%% Make C type
%%------------------------------------------------------------

%%
%% Warning this is NOT identical to mk_c_type in ic_cbe.erl
%%
mk_c_type(G, N, S) ->
    mk_c_type(G, N, S, evaluate).

mk_c_type(G, N, S, evaluate) when element(1, S) == scoped_id ->
    {FullScopedName, _T, _TK, _} = ic_symtab:get_full_scoped_name(G, N, S), 
    BT = ic_code:get_basetype(G, ic_util:to_undersc(FullScopedName)), 
    case BT of
	"erlang_binary" ->
	    "erlang_binary";
	"erlang_pid" ->
	    "erlang_pid";
	"erlang_port" ->
	    "erlang_port";
	"erlang_ref" ->
	    "erlang_ref";
	"erlang_term" ->
	    "ETERM*";
	{enum, Type} ->
	    mk_c_type(G, N, Type, evaluate);
	Type ->
	    mk_c_type(G, N, Type, evaluate)
    end;
mk_c_type(G, N, S, evaluate_not) when element(1, S) == scoped_id ->
    {FullScopedName, _T, _TK, _} = ic_symtab:get_full_scoped_name(G, N, S), 
    BT = ic_code:get_basetype(G, ic_util:to_undersc(FullScopedName)), 
    case BT of
	"erlang_binary" ->
	    "erlang_binary";
	"erlang_pid" ->
	    "erlang_pid";
	"erlang_port" ->
	    "erlang_port";
	"erlang_ref" ->
	    "erlang_ref";
	"erlang_term" ->
	    "ETERM*";
	Type ->
	    Type
    end;
mk_c_type(_G, _N, S, _) when is_list(S) ->
    S;
mk_c_type(_G, _N, S, _) when is_record(S, string) ->
    "CORBA_char";
mk_c_type(_G, _N, S, _) when is_record(S, wstring) ->  %% WSTRING
    "CORBA_wchar";
mk_c_type(_G, _N, {boolean, _}, _) ->
    "CORBA_boolean";
mk_c_type(_G, _N, {octet, _}, _) ->
    "CORBA_octet";
mk_c_type(_G, _N, {void, _}, _) ->
    "void";
mk_c_type(_G, _N, {unsigned, U}, _) ->
    case U of
	{short, _} ->
	    "CORBA_unsigned_short";
	{long, _} ->
	    "CORBA_unsigned_long";
	{'long long', _} ->
	    "CORBA_unsigned_long_long"
    end;
mk_c_type(_G, _N, {'long long', _}, _) ->
    "CORBA_long_long";
mk_c_type(_G, _N, {'any', _}, _) ->  %% Fix for any type
    "CORBA_long";
mk_c_type(_G, _N, {T, _}, _) ->
    "CORBA_" ++ atom_to_list(T).

%%------------------------------------------------------------
%% Emit encoding statement
%%------------------------------------------------------------

%% emit_encoding_stmt(G, N, X, Fd, T, LName)
%% 
%%
emit_encoding_stmt(G, N, X, Fd, T, LName) when element(1, T) == scoped_id ->
    case mk_c_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_pid(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_pid", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_port" ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_port(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_port", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_ref" ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_ref(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_ref", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"ETERM*" ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_term(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_term", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{enum, FSN} ->
	    emit_encoding_stmt(G, N, X, Fd, FSN, LName);
	FSN ->
	    emit_encoding_stmt(G, N, X, Fd, FSN, LName)
    end;
emit_encoding_stmt(G, N, X, Fd, T, LName) when is_list(T) -> 
    %% Already a fullscoped name
    case get_param_tk(LName, X) of
	error ->
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0) {\n", 
		 [ic_util:mk_oe_name(G, "encode_"), T, LName]);
	ParamTK ->
	    case ic_cbe:is_variable_size(ParamTK) of
		true ->
		    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s)) < 0)"
			 " {\n", 
			 [ic_util:mk_oe_name(G, "encode_"), T, LName]), 
		    emit(Fd, "    CORBA_exc_set(oe_env, "
			 "CORBA_SYSTEM_EXCEPTION, "
			 "BAD_PARAM, \"Bad operation parameter on encode\");"
			 "\n"), 
		    ?emit_c_enc_rpt(Fd, "    ", "", []),
		    emit(Fd, "    return oe_error_code;\n  }\n\n");
		false ->
		    if is_atom(ParamTK) ->
			    case ParamTK of
				tk_ushort -> 
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_ulong(oe_env, "
					 "(unsigned long) ~s)) < 0) {\n", 
					 [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "ushort", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_ulong -> 
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_ulong(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "ulong", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_ulonglong -> 
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_ulonglong(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "ulonglong", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_short ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_long(oe_env, "
					 "(long) ~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "short", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_long ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_long(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "long", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_longlong ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_longlong(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "longlong", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_float ->
				    emit(Fd, "    if ((oe_error_code = "
					 "oe_ei_encode_double(oe_env, "
					 "(double) ~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "float", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_double ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_double(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "double", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_boolean ->
				    emit(Fd, "  switch(~s) {\n", [LName]), 
				    emit(Fd, "    case 0 :\n"), 
				    emit(Fd, "      if ((oe_error_code = "
					 "oe_ei_encode_atom(oe_env, "
					 "\"false\")) < 0) {\n"), 
				    emit(Fd, "        CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "boolean", []),
				    emit(Fd, "        return "
					 "oe_error_code;\n      }\n"), 
				    emit(Fd, "      break;\n"), 
				    emit(Fd, "    case 1 :\n"), 
				    emit(Fd, "      if ((oe_error_code = "
					 "oe_ei_encode_atom(oe_env, "
					 "\"true\")) < 0) {\n"), 
				    emit(Fd, "        CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "boolean", []),
				    emit(Fd, "        return "
					 "oe_error_code;\n      }\n"), 
				    emit(Fd, "      break;\n"), 
				    emit(Fd, "    default :\n"), 
				    emit(Fd, "      CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "boolean", []),
				    emit(Fd, "      return -1;\n"), 
				    emit(Fd, "  }\n\n");
				tk_char ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_char(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "char", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_wchar ->  %% WCHAR
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_wchar(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "wchar", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_octet ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_char(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "octet", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				tk_any ->
				    emit(Fd, "  if ((oe_error_code = "
					 "oe_ei_encode_long(oe_env, "
					 "~s)) < 0) {\n", [LName]), 
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "any", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n");
				_ ->
				    emit(Fd, "    CORBA_exc_set(oe_env, "
					 "CORBA_SYSTEM_EXCEPTION, "
					 "BAD_PARAM, \"Bad operation "
					 "parameter on encode\");\n"), 
				    ?emit_c_enc_rpt(Fd, "    ", "tk_unknown", []),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n\n"), 
				    ok
			    end;
		       true ->
			    case element(1, ParamTK) of
				tk_enum ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n", 
					 [ic_util:mk_oe_name(G, "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "enum", []);
				tk_array ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n", 
					 [ic_util:mk_oe_name(G, "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "array", []);
				_ ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, &~s)) < 0) {\n", 
					 [ic_util:mk_oe_name(G, "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "", [])
			    end, 
			    emit(Fd, "    CORBA_exc_set(oe_env, "
				 "CORBA_SYSTEM_EXCEPTION, "
				 "BAD_PARAM, \"Bad operation "
				 "parameter on encode\");\n"), 
			    emit(Fd, "    return oe_error_code;\n  }\n\n")
		    end
	    end
    end;
emit_encoding_stmt(G, N, _X, Fd, T, LName)  when is_record(T, string) ->
    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_string(oe_env, (const char*) ~s)) < 0) {\n", 
	 [LName]), 
    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
	 "BAD_PARAM, \"Cannot encode string\");\n"), 
    ?emit_c_enc_rpt(Fd, "    ", "string", []),
    emit(Fd, "    return oe_error_code;\n  }\n\n");
emit_encoding_stmt(G, N, _X, Fd, T, LName) when is_record(T, wstring) ->
    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_wstring(oe_env, ~s)) < 0) {\n", 
	 [LName]), 
    ?emit_c_enc_rpt(Fd, "    ", "wstring", []),
    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
	 "BAD_PARAM, \"Cannot encode string\");\n"), 
    emit(Fd, "    return oe_error_code;\n  }\n\n");
emit_encoding_stmt(G, N, _X, Fd, T, LName) ->
    case T of
	{unsigned, {short, _}} -> 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_ulong(oe_env, (unsigned long) ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "ushort", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{unsigned, {long, _}} -> 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_ulong(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "ulong", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{unsigned, {'long long', _}} -> 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_ulonglong(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "ulonglong", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{short, _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_long(oe_env, (long) ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "short", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{long, _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_long(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "long", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{'long long', _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_longlong(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "longlong", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{float, _} ->
	    emit(Fd, "    if ((oe_error_code = "
		 "oe_ei_encode_double(oe_env, (double) ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "float", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{double, _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_double(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "double", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{boolean, _} ->
	    emit(Fd, "  switch(~s) {\n", [LName]), 
	    emit(Fd, "    case 0 :\n"), 
	    emit(Fd, "      if ((oe_error_code = "
		 "oe_ei_encode_atom(oe_env, \"false\")) < 0) {\n"), 
	    ?emit_c_enc_rpt(Fd, "    ", "boolean", []),
	    emit(Fd, "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "        return oe_error_code;\n      }\n"), 
	    emit(Fd, "      break;\n"), 
	    emit(Fd, "    case 1 :\n"), 
	    emit(Fd, "      if ((oe_error_code = "
		 "oe_ei_encode_atom(oe_env, \"true\")) < 0) {\n"), 
	    ?emit_c_enc_rpt(Fd, "    ", "boolean", []),
	    emit(Fd, "        CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "        return oe_error_code;\n      }\n"), 
	    emit(Fd, "      break;\n"), 
	    emit(Fd, "    default :\n"), 
	    ?emit_c_enc_rpt(Fd, "    ", "boolean", []),
	    emit(Fd, "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "      return -1;\n"), 
	    emit(Fd, "  }\n\n");
	{char, _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_char(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "char", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{wchar, _} ->  %% WCHAR
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_wchar(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "wchar", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{octet, _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_char(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "octet", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{void, _} ->
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_atom(oe_env, \"void\")) < 0) {\n"), 
	    ?emit_c_enc_rpt(Fd, "    ", "void", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{sequence, _, _} ->
	    ?emit_c_enc_rpt(Fd, "    ", "sequence", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	{any, _} -> %% Fix for any type
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_long(oe_env, ~s)) < 0) {\n", 
		 [LName]), 
	    ?emit_c_enc_rpt(Fd, "    ", "any", []),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "BAD_PARAM, \"Bad operation parameter on encode\");\n"), 
	    emit(Fd, "    return oe_error_code;\n  }\n\n");
	_ ->
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.

%%------------------------------------------------------------
%% Get type kind parameter
%%------------------------------------------------------------

%% Useful functions
get_param_tk("oe_return", Op) ->
    ic_forms:get_tk(Op);
get_param_tk(Name, Op) ->
    case get_param(Name, Op) of
	error ->
	    error;
	Param ->
	    ic_forms:get_tk(Param)
    end.

%%------------------------------------------------------------
%% Get parameter (for what? XXX)
%%------------------------------------------------------------

get_param(Name, Op) when is_record(Op, op) ->
    get_param_loop(Name, Op#op.params);
get_param(_Name, _Op) ->
    error.

get_param_loop(_Name, []) ->
    error;
get_param_loop(Name, [Param| Params]) ->
    case ic_forms:get_id2(Param) of
	Name ->
	    Param;
	_ ->
	    get_param_loop(Name, Params)
    end.

%%------------------------------------------------------------
%% Emit decoding statement
%%------------------------------------------------------------

emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, Align, NextPos, 
		   DecType, AllocedPars) when element(1, T) == scoped_id ->
    case mk_c_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_pid(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "", []),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n\n");
	"erlang_port" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_port(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "", []),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n\n");
	"erlang_ref" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_ref(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "", []),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n\n");
	"ETERM*" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_term(~s, "
		 "&oe_env->_iin, (void**)~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "", []),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n\n");
	{enum, FSN} ->
	    emit_decoding_stmt(G, N, Fd, FSN, LName, IndOp, 
			       InBuffer, Align, NextPos, DecType, AllocedPars);
	FSN ->
	    emit_decoding_stmt(G, N, Fd, FSN, LName, IndOp, 
			       InBuffer, Align, NextPos, DecType, AllocedPars) 
    end;
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, NextPos, 
		   DecType, AllocedPars)  when is_list(T) ->
    %% Already a fullscoped name
    Type = ictype:name2type(G, T), 
    case ictype:isBasicType(Type) of
	true ->
	    emit_decoding_stmt_for_basic_type(Fd, Type, InBuffer, IndOp, 
					      LName, AllocedPars);
	false ->
	    emit(Fd, "    {\n"), 
	    case DecType of
		caller -> %% No malloc used, define oe_first anyhow.  
		    emit(Fd, "      void *oe_first = NULL;\n"), 
		    emit(Fd, "      int oe_outindex = 0;\n\n");
		array_dyn ->  %% Malloc used 
		    emit(Fd, "      int oe_outindex = 0;\n\n");
		%% [ic_util:mk_align(io_lib:format("sizeof(~s)", [T]))]);
		caller_dyn ->  %% Malloc used 
		    emit(Fd, "      int oe_outindex = 0;\n\n")
	    end, 
	    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, oe_first, "
		 "~s, ~s)) < 0) {\n", 
		 [ic_util:mk_oe_name(G, "decode_"), 
		  T, NextPos, LName]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "        ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "", []),
	    emit(Fd, "        return oe_error_code;\n"), 
	    emit(Fd, "      }\n"),
	    emit(Fd, "    }\n")
    end;
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, _NextPos, 
		   _DecType, AllocedPars)  when is_record(T, string) ->
    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, "
	 "&oe_env->_iin, ~s~s)) < 0) {\n", 
	 [InBuffer, IndOp, LName]), 
    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
    ?emit_c_dec_rpt(Fd, "    ", "", []),
    emit(Fd, "      return oe_error_code;\n"),
    emit(Fd, "    }\n");
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, _NextPos, 
		   _DecType, AllocedPars)  when is_record(T, wstring) ->  
    %% WSTRING
    emit(Fd, "    if ((oe_error_code = "
	 "oe_ei_decode_wstring(~s, "
	 "&oe_env->_iin, ~s~s)) < 0) {\n", 
	 [InBuffer, IndOp, LName]), 
    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
    ?emit_c_dec_rpt(Fd, "    ", "", []),
    emit(Fd, "      return oe_error_code;\n\n"),
    emit(Fd, "    }\n");
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, _NextPos, 
		   _DecType, AllocedPars) ->
    case ic_cbe:normalize_type(T) of
	{basic, Type} ->
	    emit_decoding_stmt_for_basic_type(Fd, Type, InBuffer, IndOp, 
					      LName, AllocedPars);
	_ ->
	    case T of
		{void, _} ->
		    emit(Fd, 
			 "  if ((oe_error_code = ei_decode_atom(~s, "
			 "&oe_env->_iin, 0)) < 0) {\n", 
			 [InBuffer]), 
		    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "    ", "", []),
		    emit(Fd, "    return oe_error_code;\n"),
		    emit(Fd, "  }\n");
		{sequence, _, _} ->
		    %% XXX XXX Why?
		    ?emit_c_dec_rpt(Fd, "    ", "", []),
		    emit(Fd, "    return oe_error_code;\n\n");
		{any, _} -> %% Fix for any type
		    emit(Fd, 
			 "  if ((oe_error_code = ei_decode_long(~s, "
			 "&oe_env->_iin, ~s~s)) < 0) {\n", 
			 [InBuffer, IndOp, LName]), 
		    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "    ", "", []),
		    emit(Fd, "    return oe_error_code;\n\n"),
		    emit(Fd, "  }\n");
		_ ->
		    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.

emit_decoding_stmt_for_basic_type(Fd, Type, InBuffer, IndOp, 
				  LName, AllocedPars) ->
    Fmt = 
	"  if ((oe_error_code = ~sei_decode_~s(~s, &oe_env->_iin, "
	"~s~s)) < 0) {\n", 
    Ret = 
	"    return oe_error_code;\n"
	"}\n",

    {Pre, DecType} = 
	case Type of
	    ushort ->		{"", "ulong"};
	    ulong ->		{"", "ulong"};
	    ulonglong ->	{"oe_", "ulonglong"};
	    short ->		{"", "long"};
	    long ->		{"", "long"};
	    longlong ->		{"oe_", "longlong"};
	    float ->		{"", "double"};
	    double ->		{"", "double"};
	    boolean ->		{"", "atom"};
	    char ->		{"", "char"};
	    wchar ->		{"oe_", "wchar"};
	    octet ->		{"", "char"};
	    any ->		{"", "long"}
	end,
    case Type of
	ushort ->
	    emit(Fd, "  {\n"), 
	    emit(Fd, "    unsigned long oe_ulong;\n"), 
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(~s, "
		 "&oe_env->_iin, &oe_ulong)) < 0) {\n", [InBuffer]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    emit_c_dec_rpt(Fd, "    ", "ushort", []),
	    emit(Fd, "      return oe_error_code;\n"), 
	    emit(Fd, "    }\n"), 
	    emit(Fd, "    *~s = (unsigned short) oe_ulong;\n", [LName]), 
	    emit(Fd, "  }\n\n");
	short ->
	    emit(Fd, "  {\n"), 
	    emit(Fd, "    long oe_long;\n"), 
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, "
		 "&oe_env->_iin, &oe_long)) < 0) {\n", [InBuffer]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    emit_c_dec_rpt(Fd, "    ", "short", []),
	    emit(Fd, "      return oe_error_code;\n"), 
	    emit(Fd, "    }\n"), 
	    emit(Fd, "    *~s = (short) oe_long;\n", [LName]), 
	    emit(Fd, "  }\n\n");
	float ->
	    emit(Fd, "  {\n"), 
	    emit(Fd, "    double oe_double;\n"), 
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, "
		 "&oe_env->_iin, &oe_double)) < 0) {\n", [InBuffer]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    emit_c_dec_rpt(Fd, "    ", "float", []),
	    emit(Fd, "      return oe_error_code;\n"), 
	    emit(Fd, "    }\n"), 
	    emit(Fd, "    *~s = (float) oe_double;\n", [LName]), 
	    emit(Fd, "  }\n\n");
	boolean ->
	    emit(Fd, "  {\n"), 
	    emit(Fd, "    char oe_bool[25];\n\n"), 
	    emit(Fd, "    if ((oe_error_code = ei_decode_atom(~s, "
		 "&oe_env->_iin, oe_bool)) < 0) {\n", [InBuffer]), 
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    emit_c_dec_rpt(Fd, "    ", "boolean", []),
	    emit(Fd, "      return oe_error_code;\n"), 
	    emit(Fd, "    }\n"), 
	    emit(Fd, "    if (strcmp(oe_bool, \"false\") == 0) {\n"), 
	    emit(Fd, "      *(~s) = 0;\n", [LName]), 
	    emit(Fd, "    }\n"), 
	    emit(Fd, "    else if (strcmp(oe_bool, \"true\") == 0) {\n"), 
	    emit(Fd, "      *(~s) = 1;\n", [LName]), 
	    emit(Fd, "    } else {\n"), 
	    ic_cbe:emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    emit_c_dec_rpt(Fd, "    ", "boolean", []),
	    emit(Fd, "      return -1;\n"), 
	    emit(Fd, "    }\n"), 
	    emit(Fd, "  }\n\n");
	_ ->
	    emit(Fd, Fmt, [Pre, DecType, InBuffer, IndOp, LName]),
	    ic_cbe:emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    emit(Fd, Ret)
    end.


%%------------------------------------------------------------
%% Prefix for generic functions
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
    

