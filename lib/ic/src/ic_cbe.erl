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

%%------------------------------------------------------------ 
%%
%% This module is a main module for generation of C code, both
%% for ic_cclient and ic_cserver.
%%
%% The former role of this module (ic_cbe) was to generate client
%% code only.
%%
-module(ic_cbe).

-export([emit_malloc_size_stmt/7, emit_encoding_stmt/6,
	 emit_encoding_stmt/7, emit_decoding_stmt/10,
	 emit_decoding_stmt/11, emit_dealloc_stmts/3,
	 mk_variable_name/1, mk_c_type/3, mk_c_type/4, mk_c_type2/3,
	 is_variable_size/1, is_variable_size/3, mk_dim/1,
	 mk_slice_dim/1, emit_tmp_variables/1, store_tmp_decl/2,
	 extract_info/3, normalize_type/1]).

%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------

-import(ic_codegen, [emit/2, emit/3, emit/4, emit_c_enc_rpt/4, emit_c_dec_rpt/4]).

-include("icforms.hrl").
-include ("ic.hrl").

%%------------------------------------------------------------
%%    ENCODING
%%------------------------------------------------------------

emit_encoding_stmt(G, N, Fd, T, LName, OutBuffer) when element(1, T) == scoped_id ->
    case mk_c_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_pid(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_port" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_port(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n}  \n");
	"erlang_ref" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_ref(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"ETERM*" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_term(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{enum, FSN} ->
	    emit_encoding_stmt(G, N, Fd, FSN, LName, OutBuffer);
	FSN ->
	    emit_encoding_stmt(G, N, Fd, FSN, LName, OutBuffer)
    end;

%% XXX T is a string
emit_encoding_stmt(G, N, Fd, T, LName, _OutBuffer)  when is_list(T) -> 
    %% Already a fullscoped name
    Type = ictype:name2type(G,T),
    case ictype:isBasicType(Type) of
	true ->
	    emit_encoding_stmt_for_basic_type(G, N, T, Fd, Type, LName);
	false ->
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, ~s))"
		 " < 0) {\n",
		 [ic_util:mk_oe_name(G, "encode_"), T, LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]), % XXX list
	    emit(Fd, "    return oe_error_code;\n  }\n")
    end;
emit_encoding_stmt(G, N, Fd, T, LName, _OutBuffer)  when is_record(T, string) ->
    %% Note prefix: oe_ei 
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_string(oe_env, "
	 " ~s)) < 0) {\n", 
	 [LName]),
    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
    emit(Fd, "    return oe_error_code;\n  }\n");
emit_encoding_stmt(G, N, Fd, T, LName, _OutBuffer) when is_record(T, wstring) ->
    %% Note prefix: oe_ei 
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_wstring(oe_env, "
	 "~s)) < 0) {\n", 
	 [LName]),
    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
    emit(Fd, "    return oe_error_code;\n  }\n");
emit_encoding_stmt(G, N, Fd, T, LName, _OutBuffer) ->
    case normalize_type(T) of
	{basic, Type} ->
	    emit_encoding_stmt_for_basic_type(G, N, T, Fd, Type, LName);
	%% XXX Why only returns?
	{void, _} ->
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{sequence, _, _} ->
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{_ArrayType, {array, _, _}} ->
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{union, _, _, _, _} -> 
	    %% Union as a member in struct !  
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{struct, _, _, _} -> 
	    %% Struct as a member in struct !  
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	_ ->  
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.

%% Arity = 7. 
%%
emit_encoding_stmt(G, N, X, Fd, T, LName, OutBuffer) when element(1, T) == scoped_id ->
    case mk_c_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_pid(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_port" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_port(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_ref" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_ref(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"ETERM*" ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = "
		 "oe_ei_encode_term(oe_env, ~s)) < 0) {\n",
		 [LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{enum, FSN} ->
	    emit_encoding_stmt(G, N, X, Fd, FSN, LName, OutBuffer);
	FSN ->
	    emit_encoding_stmt(G, N, X, Fd, FSN, LName, OutBuffer)
    end;

%% XXX T is a string
emit_encoding_stmt(G, N, X, Fd, T, LName, _OutBuffer) when is_list(T) -> 
    %% Already a fullscoped name
    case get_param_tk(LName,X) of
	error ->
	    emit(Fd, "  if ((oe_error_code = "
		 "~s~s(oe_env, ~s)) < 0) {\n",
		 [ic_util:mk_oe_name(G, "encode_"), T, LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	ParamTK ->
	    case is_variable_size(ParamTK) of
		true ->
		    if is_tuple(ParamTK) ->
			    case element(1,ParamTK) of
				tk_array ->
				    %% Array of dynamic data
				    emit(Fd, 
					 "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, 
							     "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, 
					 "    return "
					 "oe_error_code;\n  }\n");
				_ ->
				    emit(Fd, 
					 "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, 
							     "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, "    return "
					 "oe_error_code;\n  }\n")
			    end;
		       true ->
			    emit(Fd, 
				 "  if ((oe_error_code = "
				 "~s~s(oe_env, ~s)) < 0) {\n",
				 [ic_util:mk_oe_name(G, "encode_"),
				  T, LName]),
			    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
			    emit(Fd, "    return oe_error_code;\n  }\n")
		    end;
		false ->
		    if is_atom(ParamTK) ->
			    case normalize_type(ParamTK) of
				{basic, Type} ->
				    emit_encoding_stmt_for_basic_type(G, N, T, Fd,
								      Type, 
								      LName);
				_ ->
				    %% Why only return?
				    ?emit_c_enc_rpt(Fd, "    ", "~/slist/~s", [T, LName]),
				    emit(Fd, "    return oe_error_code;\n  }\n"),
				    ok
			    end;
		       true ->
			    case element(1,ParamTK) of
				tk_enum ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, "    return oe_error_code;\n  }\n");
				tk_array ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, "encode_"), 
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, "    return oe_error_code;\n  }\n");
				tk_struct ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, "encode_"),
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, "    return oe_error_code;\n  }\n");
				tk_union ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, ~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, "encode_"),
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, "    return oe_error_code;\n  }\n");
				_ ->
				    emit(Fd, "  if ((oe_error_code = "
					 "~s~s(oe_env, &~s)) < 0) {\n",
					 [ic_util:mk_oe_name(G, "encode_"),
					  T, LName]),
				    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
				    emit(Fd, "    return oe_error_code;\n  }\n")
			    end
		    end
	    end
    end;
emit_encoding_stmt(G, N, _X, Fd, T, LName, _OutBuffer)  when is_record(T, string) ->
    %% Note prefix: oe_ei 
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_string(oe_env, ~s)) < 0) {\n", 
	 [LName]),
    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
    emit(Fd, "    return oe_error_code;\n  }\n");
emit_encoding_stmt(G, N, _X, Fd, T, LName, _OutBuffer) when is_record(T, wstring) ->
    %% Note prefix: oe_ei 
    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_wstring(oe_env, ~s)) < 0) {\n", 
	 [LName]),
    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
    emit(Fd, "    return oe_error_code;\n  }\n");
emit_encoding_stmt(G, N, _X, Fd, T, LName, _OutBuffer) ->
    case normalize_type(T) of
	{basic, Type} ->
	    emit_encoding_stmt_for_basic_type(G, N, T, Fd, Type, LName);
	{void, _} ->
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n"),
	    ok;
	{sequence, _, _} ->
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n"),
	    ok;
	{_ArrayType, {array, _, _}} ->
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n"),
	    ok;
	{struct, _, _, _} -> %% Struct as a member in struct !  
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n"),
	    ok;
	_ ->
	    %%io:format("2 ------------> ~p~n", [T]),
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.

%%------------------------------------------------------------
emit_encoding_stmt_for_basic_type(G, N, T, Fd, Type, LName) ->
    {Cast, DecType} = 
	case Type of
 	    ushort ->		{"(unsigned long) ", "ulong"};
 	    ulong ->		{"", "ulong"};
 	    ulonglong ->	{"", "ulonglong"};
 	    short ->		{"(long) ", "long"};
 	    long ->		{"", "long"};
 	    longlong ->		{"", "longlong"};
 	    float ->		{"(double) ", "double"};
 	    double ->		{"", "double"};
 	    boolean ->		{"", "atom"};
 	    char ->		{"", "char"};
 	    wchar ->		{"", "wchar"};
 	    octet ->		{"", "char"};
 	    any ->		{"", "long"}	% Fix for any
	end,
    case Type of	
	boolean ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  switch(~s) {\n",[LName]),
	    emit(Fd, "    case 0 :\n"),
	    emit(Fd, "      if ((oe_error_code = "
		 "oe_ei_encode_atom(oe_env, "
		 "\"false\")) < 0) {\n"),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n    }\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    case 1 :\n"),
	    emit(Fd, "      if ((oe_error_code = "
		 "oe_ei_encode_atom(oe_env, "
		 "\"true\")) < 0) {\n"),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n    }\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    default :\n"),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "  }\n\n");
	_ ->
	    Fmt =
		"  if ((oe_error_code = oe_ei_encode_~s(oe_env, ~s~s)) < 0) {\n",
	    emit(Fd, Fmt, [DecType, Cast, LName]),
	    ?emit_c_enc_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n  }\n")
    end.


%%------------------------------------------------------------ 
%% MALLOC SIZE (for Decode)
%%------------------------------------------------------------

emit_malloc_size_stmt(G, N, Fd, T, InBuffer, 
		      Align, CalcType) when element(1, T) == scoped_id ->
    case mk_c_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    emit(Fd, "  oe_malloc_size += sizeof(erlang_pid);\n\n"),
	    emit(Fd, "  if ((oe_error_code = ei_decode_pid(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n", [InBuffer]),
	    ?emit_c_dec_rpt(Fd, "    ", "erlang_pid", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_port" ->
	    emit(Fd, "  oe_malloc_size += sizeof(erlang_port);\n\n"),
	    emit(Fd, "  if ((oe_error_code = ei_decode_port(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n", [InBuffer]),
	    ?emit_c_dec_rpt(Fd, "    ", "erlang_port", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"erlang_ref" ->
	    emit(Fd, "  oe_malloc_size += sizeof(erlang_ref);\n\n"),
	    emit(Fd, "  if ((oe_error_code = ei_decode_ref(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n", [InBuffer]),
	    ?emit_c_dec_rpt(Fd, "    ", "erlang_ref", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"ETERM*" ->
	    emit(Fd, "  oe_malloc_size += sizeof(char*);\n\n"),
	    emit(Fd, "  if ((oe_error_code = ei_decode_term(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n", [InBuffer]),
	    ?emit_c_dec_rpt(Fd, "    ", "ETERM*", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	{enum, FSN} ->
	    emit_malloc_size_stmt(G, N, Fd, FSN, InBuffer, Align, CalcType);
	FSN ->
	    %% io:format("emit_malloc_size_stmt: ~p ~p~n",[FSN, 
	    %% CalcType]),
	    emit_malloc_size_stmt(G, N, Fd, FSN, InBuffer, Align, CalcType)
    end;

%% XXX T is a string
emit_malloc_size_stmt(G, N, Fd, T, InBuffer, 
		      _Align, CalcType)  when is_list(T) -> 
    %% Already a fullscoped name
    Type = ictype:name2type(G,T),
    case ictype:isBasicType(Type) of
	true ->
	    emit_malloc_size_stmt_for_basic_type(G, N, T, Fd, Type, InBuffer);
	false ->
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), T]),
		    ?emit_c_dec_rpt(Fd, "    ", "~s", [T]),
		    emit(Fd, "    return oe_error_code;\n    }\n");
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "&oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), T]),
		    ?emit_c_dec_rpt(Fd, "    ", "~s", [T]),
		    emit(Fd, "    return oe_error_code;\n    }\n")
	    end
    end;
emit_malloc_size_stmt(G, N, Fd, T, InBuffer, _Align, 
		      CalcType) when is_record(T, string) ->
    Tname = mk_variable_name(op_variable_count),
    store_tmp_decl("    int ~s = 0;\n",[Tname]),
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, "
		 "oe_size_count_index, &oe_type, &~s)) < 0) {\n",
		 [InBuffer, Tname]);
	_ ->
	    emit(Fd, "    int oe_type = 0;\n"),
	    emit(Fd, "    int oe_temp = 0;\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, "
		 "&oe_size_count_index, &oe_type, &oe_temp)) < 0) {\n",
		 [InBuffer])
    end,
    ?emit_c_dec_rpt(Fd, "      ", "ei_get_type", []),
    emit(Fd, "      return oe_error_code;\n    }\n"),
    if
	T#string.length == 0 ->
	    ok;
	true ->
	    Length = ic_util:eval_c(G, N, T#string.length), 
	    case CalcType of
		generator ->
		    emit(Fd, "  if (~s > ~s)\n",[Tname, Length]),
		    emit(Fd, "    return -1;\n\n");
		_ ->
		    emit(Fd, "  if (oe_temp > ~s)\n",[Length]),
		    emit(Fd, "    return -1;\n\n")
	    end		    
    end,
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n", [InBuffer]);
	_ ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, "
		 "&oe_size_count_index, NULL)) < 0) {\n", [InBuffer])
    end,
    ?emit_c_dec_rpt(Fd, "      ", "ei_decode_string", []),
    emit(Fd, "      return oe_error_code;\n    }\n"),
    case CalcType of
	generator ->
	    emit(Fd, "    oe_malloc_size = ~s;\n\n", 
		 [ic_util:mk_align("oe_malloc_size + " ++ Tname ++"+1")]);
	_ ->
	    emit(Fd, "    oe_malloc_size = ~s;\n\n", 
		 [ic_util:mk_align("oe_malloc_size + oe_temp+1")])
    end;
emit_malloc_size_stmt(G, N, Fd, T, InBuffer, _Align, 
		      CalcType) when is_record(T, wstring) ->
    Tname = mk_variable_name(op_variable_count),
    store_tmp_decl("    int ~s = 0;\n",[Tname]),
    case CalcType of
	generator ->
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, "
		 "oe_size_count_index, &oe_type, &~s)) < 0) {\n",
		 [InBuffer, Tname]);
	_ ->
	    emit(Fd, "    int oe_type = 0;\n"),
	    emit(Fd, "    int oe_temp = 0;\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_get_type(~s, "
		 "&oe_size_count_index, &oe_type, &oe_temp)) < 0) {\n",
		 [InBuffer])
    end,
    ?emit_c_dec_rpt(Fd, "    ", "ei_get_type", []),
    emit(Fd, "    return oe_error_code;\n    }\n"),
    if
	T#wstring.length == 0 ->
	    ok;
	true ->
	    Length = ic_util:eval_c(G, N, T#wstring.length), 
	    case CalcType of
		generator ->
		    emit(Fd, "  if (~s > ~s)\n",[Tname, Length]),
		    emit(Fd, "    return -1;\n\n");
		_ ->
		    emit(Fd, "  if (oe_temp > ~s)\n",[Length]),
		    emit(Fd, "    return -1;\n\n")
	    end		    
    end,
    case CalcType of
	generator ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n", [InBuffer]);
	_ ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, "
		 "&oe_size_count_index, NULL)) < 0) {\n", [InBuffer])
    end,
    ?emit_c_dec_rpt(Fd, "    ", "oe_ei_decode_wstring", []),
    emit(Fd, "    return oe_error_code;\n    }\n"),
    case CalcType of
	generator ->
	    emit(Fd, "    oe_malloc_size =\n      ~s;\n\n", 
		 [ic_util:mk_align("oe_malloc_size + ((" 
				   ++ Tname 
				   ++"+ 1) * __OE_WCHAR_SIZE_OF__)")]);
	_ ->
	    emit(Fd, "    oe_malloc_size =\n      ~s;\n\n", 
		 [ic_util:mk_align("oe_malloc_size + (("
				   "oe_temp + 1) * __OE_WCHAR_SIZE_OF__)")])
    end;
emit_malloc_size_stmt(G, N, Fd, T, InBuffer, Align, CalcType) ->
    case Align of
	0 ->
	    emit(Fd, "  oe_malloc_size += sizeof(~s);\n\n", 
		 [mk_c_type(G, N, T)]);
	_ -> 
	    ok
    end,
    case normalize_type(T) of
	{basic, Type} ->
	    emit_malloc_size_stmt_for_basic_type(G, N, T, Fd, Type, InBuffer);
	{void, _} ->
	    ok;
	{sequence, _, _} ->
	    ok;
	{_, {array, SId, _}} ->
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), 
			  ic_forms:get_id2(SId)]),
		    ?emit_c_dec_rpt(Fd, "    ", "array1", []),
		    emit(Fd, "    return oe_error_code;\n\n");
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "&oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), 
			  ic_forms:get_id2(SId)]),
		    ?emit_c_dec_rpt(Fd, "    ", "array2", []),
		    emit(Fd, "    return oe_error_code;\n\n")
	    end;
	{union, UId, _, _, _} ->
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), 
			  ic_forms:get_id2(UId)]),
		    ?emit_c_dec_rpt(Fd, "    ", "union1", []),
		    emit(Fd, "    return oe_error_code;\n\n");
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "&oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), 
			  ic_forms:get_id2(UId)]),
		    ?emit_c_dec_rpt(Fd, "    ", "union2", []),
		    emit(Fd, "    return oe_error_code;\n\n")
	    end;
	{struct, UId, _, _} -> %% Struct as a member in struct !
	    case CalcType of
		generator ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), 
			  ic_forms:get_id2(UId)]),
		    ?emit_c_dec_rpt(Fd, "    ", "struct1", []),
		    emit(Fd, "    return oe_error_code;\n\n");
		_ ->
		    emit(Fd, "    if ((oe_error_code = ~s~s(oe_env, "
			 "&oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), 
			  ic_forms:get_id2(UId)]),
		    ?emit_c_dec_rpt(Fd, "    ", "struct2", []),
		    emit(Fd, "    return oe_error_code;\n\n")
	    end;
	{any, _} ->   %% Fix for any type
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, "
		 "oe_size_count_index, NULL)) < 0) {\n",
		 [InBuffer]),
	    ?emit_c_dec_rpt(Fd, "    ", "any", []),
	    emit(Fd, "    return oe_error_code;\n    }\n");
	_ ->
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.

%%------------------------------------------------------------

emit_malloc_size_stmt_for_basic_type(G, N, T, Fd, Type, InBuffer) ->
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
    Fmt = 
	"    if ((oe_error_code = ~sei_decode_~s(~s, oe_size_count_index, "
	"NULL)) < 0) {\n",
    emit(Fd, Fmt, [Pre, DecType, InBuffer]),
    ?emit_c_dec_rpt(Fd, "      ", "~s", [DecType]),
    emit(Fd, "      return oe_error_code;\n    }\n").

%%------------------------------------------------------------
%%    DECODING
%%------------------------------------------------------------

emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, Align, 
		   NextPos, DecType) ->
    emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, Align, 
		       NextPos, DecType, []).

emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, Align, NextPos,
		   DecType, AllocedPars) when element(1, T) == scoped_id ->
    Fmt = 
	"  if ((oe_error_code = ei_decode_~s(~s, &oe_env->_iin, ~s~s)) < 0)"
	" {\n",
    Emit = fun(Type) ->
		   emit(Fd, Fmt, [Type, InBuffer, IndOp, LName]),
		   emit_dealloc_stmts(Fd, "    ", AllocedPars),
		   ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
		   emit(Fd, "    return oe_error_code;\n"),
		   emit(Fd, "  }\n")
	   end,
    case mk_c_type(G, N, T, evaluate_not) of
	"erlang_pid" ->
	    Emit("pid");
	"erlang_port" ->
	    Emit("port");
	"erlang_ref" ->
	    Emit("ref");
	"ETERM*" ->
	    Emit("term");
	{enum, FSN} ->
	    emit_decoding_stmt(G, N, Fd, FSN, LName, IndOp, InBuffer,
			       Align, NextPos, DecType, AllocedPars);
	FSN ->
	    emit_decoding_stmt(G, N, Fd, FSN, LName, IndOp, InBuffer,
			       Align, NextPos, DecType, AllocedPars) 
    end;

%% XXX T is a string
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, NextPos,
		   DecType, AllocedPars)  when is_list(T) -> 
    %% Already a fullscoped name
    Type = ictype:name2type(G,T),
    case ictype:isBasicType(Type) of
	true ->
	    emit_decoding_stmt_for_basic_type(G, N, T, Fd, Type, InBuffer, IndOp, 
					      LName, AllocedPars);
	false ->
	    case DecType of
		generator ->
		    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, oe_first, "
			 "~s, ~s)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit_dealloc_stmts(Fd, "    ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
		    emit(Fd, "    return oe_error_code;\n"),
		    emit(Fd, "  }\n");
		caller -> %% No malloc used, define oe_first
		    emit(Fd, "    {\n"),
		    emit(Fd, "      void *oe_first = NULL;\n"), 
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, "
			 "oe_first, ~s, ~s)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit_dealloc_stmts(Fd, "      ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "         ", "~s", [LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      }\n"),
		    emit(Fd, "    }\n");
		caller_dyn ->  %% Malloc used
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, "
			 "oe_first, ~s, ~s)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit_dealloc_stmts(Fd, "        ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "        ", "~s", [LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      }\n"),
		    emit(Fd, "    }\n");
		array_dyn ->  %% Malloc used
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, "
			 "oe_first, ~s, ~s)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit_dealloc_stmts(Fd, "    ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "        ", "~s", [LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      }\n"),
		    emit(Fd, "    }\n");
		array_fix_ret ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, "
			 "oe_first, ~s,*~s)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit_dealloc_stmts(Fd, "        ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "         ", "~s", [LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      }\n"),
		    emit(Fd, "    }\n");
		array_fix_out -> %% No malloc used, define oe_first
		    emit(Fd, "    {\n"),
		    emit(Fd, "      void *oe_first = NULL;\n"), 
		    emit(Fd, "      int oe_outindex = 0;\n\n"),
		    emit(Fd, "      if ((oe_error_code = ~s~s(oe_env, "
			 "oe_first, ~s, ~s)) < 0) {\n",
			 [ic_util:mk_oe_name(G, "decode_"),
			  T, NextPos, LName]),
		    emit_dealloc_stmts(Fd, "        ", AllocedPars),
		    ?emit_c_dec_rpt(Fd, "        ", "~s", [LName]),
		    emit(Fd, "        return oe_error_code;\n"),
		    emit(Fd, "      }\n"),
		    emit(Fd, "    }\n")
	    end
    end;
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, _NextPos,
		   DecType, AllocedPars)  when is_record(T, string) ->
    case DecType of
	caller_dyn ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_string(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n");
	_ ->
	    emit(Fd, "  ~s~s = oe_first + *oe_outindex;\n\n", 
		 [IndOp, LName]),
	    emit(Fd, "  {\n"),
	    emit(Fd, "    int oe_type=0;\n"),
	    emit(Fd, "    int oe_string_ctr=0;\n\n"),

	    emit(Fd, "    (int) ei_get_type(~s, "
		 "&oe_env->_iin, &oe_type, &oe_string_ctr);\n\n",
		 [InBuffer]),

	    emit(Fd, "    if ((oe_error_code = ei_decode_string(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return oe_error_code;\n"),
	    emit(Fd, "    }\n"),
	    emit(Fd, "  *oe_outindex = ~s;\n",
		 [ic_util:mk_align("*oe_outindex+oe_string_ctr+1")]),
	    emit(Fd, "  }\n\n")	
    end;
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, _NextPos,
		   DecType, AllocedPars)  when is_record(T, wstring) ->  
    case DecType of
	caller_dyn ->
	    %% Note prefix: oe_ei 
	    emit(Fd, "  if ((oe_error_code = oe_ei_decode_wstring(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }/* --- */\n");		% XXX
	_ ->
	    emit(Fd, "  ~s~s = oe_first + *oe_outindex;\n\n", 
		 [IndOp, LName]),

	    emit(Fd, "  {\n"),
	    emit(Fd, "    int oe_type=0;\n"),
	    emit(Fd, "    int oe_string_ctr=0;\n\n"),
	    emit(Fd, "    (int) ei_get_type(~s, "
		 "&oe_env->_iin, &oe_type, &oe_string_ctr);\n\n",
		 [InBuffer]),
	    %% Note prefix: oe_ei 
	    emit(Fd, "    if ((oe_error_code = oe_ei_decode_wstring(~s, "
		 "&oe_env->_iin, ~s~s)) < 0) {\n", 
		 [InBuffer, IndOp, LName]),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return oe_error_code;\n"),
	    emit(Fd, "    }\n"),
	    emit(Fd, "  *oe_outindex = ~s;\n",
		 [ic_util:mk_align("*oe_outindex+oe_string_ctr+1")]),
	    emit(Fd, "  }\n")	
    end;
emit_decoding_stmt(G, N, Fd, T, LName, IndOp, InBuffer, _Align, NextPos,
		   _DecType, AllocedPars) ->
    case normalize_type(T) of
	{basic, Type} ->
	    emit_decoding_stmt_for_basic_type(G, N, T, Fd, Type, InBuffer, IndOp, 
					      LName, AllocedPars);
	{void, _} ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_atom(~s, "
		 "&oe_env->_iin, NULL)) < 0) {\n", 
		 [InBuffer]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n");
	{sequence, _, _} ->
	    ok;
	{_, {array, SId, Dims}} ->
	    AName = ic_forms:get_id2({array, SId, Dims}),
	    Ptr = "oe_out->"++AName,
	    emit(Fd, "  if ((oe_error_code = ~s~s(oe_env, "
		 "oe_first, ~s, ~s)) < 0) {\n",
		 [ic_util:mk_oe_name(G, "decode_"),
		  ic_forms:get_id2(SId), 
		  NextPos, Ptr]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
	    emit(Fd, "    return oe_error_code;\n"),
	    emit(Fd, "  }\n");
	{struct, _, _, _} -> %% Struct as a member in struct !  
	    ok;
	_ ->
	    %%io:format("3 ------------> ~p~n", [T]),
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end.

%% XXX DecType used in two senses in this file. 
emit_decoding_stmt_for_basic_type(G, N, T, Fd, Type, InBuffer, IndOp, 
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
		 "&oe_env->_iin, &oe_ulong)) < 0) {\n",
		 [InBuffer]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return oe_error_code;\n"),
	    emit(Fd, "}\n"),
	    emit(Fd, "    *(~s) = (unsigned short) oe_ulong;\n\n",
		 [LName]),
	    emit(Fd, "    if (*(~s) !=  oe_ulong){\n",
		 [LName]),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "    }\n"),
	    emit(Fd, "  }\n\n");
	short ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    long oe_long;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(~s, "
		 "&oe_env->_iin, &oe_long)) < 0){\n",
		 [InBuffer]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd, "}\n"),
	    emit(Fd, "    *(~s) = (short) oe_long;\n\n",[LName]),
	    emit(Fd, "    if (*(~s) !=  oe_long){\n", [LName]),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "    }\n"),
	    emit(Fd, "  }\n");
	float ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    double oe_double;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(~s, "
		 "&oe_env->_iin, &oe_double)) < 0){\n",
		 [InBuffer]),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return oe_error_code;\n\n"),
	    emit(Fd,      "}\n"),
	    emit(Fd, "    *(~s) = (float) oe_double;\n",[LName]),
	    emit(Fd, "  }\n");
	boolean ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    char oe_bool[25];\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_atom(~s, "
		 "&oe_env->_iin, oe_bool)) < 0){\n",[InBuffer]),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return oe_error_code;\n"),
	    emit(Fd,      "}\n"),
	    emit(Fd, "    if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    emit(Fd, "      *(~s) = 0;\n",[LName]), 
	    emit(Fd, "    }\n"),
	    emit(Fd, "    else if (strcmp(oe_bool, \"true\") == 0)"
		 " {\n"),
	    emit(Fd, "      *(~s) = 1;\n",[LName]), 
	    emit(Fd, "    }\n"),
	    emit(Fd, "    else {\n"),
	    emit_dealloc_stmts(Fd, "      ", AllocedPars),
	    ?emit_c_dec_rpt(Fd, "      ", "~s", [LName]),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "    }\n"),
	    emit(Fd, "  }\n");
	_ ->
	    emit(Fd, Fmt, [Pre, DecType, InBuffer, IndOp, LName]),
	    ?emit_c_dec_rpt(Fd, "    ", "~s", [LName]),
	    emit_dealloc_stmts(Fd, "    ", AllocedPars),
	    emit(Fd, Ret)
    end.

%%------------------------------------------------------------
%%
%%------------------------------------------------------------
emit_dealloc_stmts(Fd, Prefix, AllocedPars) ->
    Fmt = Prefix ++ "CORBA_free(~s);\n",
    lists:foreach(
      fun(Par) -> emit(Fd, Fmt, [Par]) end, 
      AllocedPars).


%%------------------------------------------------------------
%%
%%------------------------------------------------------------

mk_variable_name(Var) ->
    Nr = get(Var),
    put(Var, Nr + 1),
    "oe_tmp" ++ integer_to_list(Nr).

%%    IDL to C type conversion
%%------------------------------------------------------------
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
    "CORBA_char *";
mk_c_type(_G, _N, S, _) when is_record(S, wstring) -> 
    "CORBA_wchar *";
mk_c_type(_G, _N, {boolean, _}, _) ->
    "CORBA_boolean";
mk_c_type(_G, _N, {octet, _}, _) ->
    "CORBA_octet";
mk_c_type(_G, _N, {void, _}, _) ->
    "void";
mk_c_type(_G, _N, {unsigned, U}, _) ->
    case U of
	{short,_} ->
	    "CORBA_unsigned_short";
	{long,_} ->
	    "CORBA_unsigned_long";
	{'long long',_} ->
	    "CORBA_unsigned_long_long"
    end;

mk_c_type(_G, _N, {'long long', _}, _) ->
    "CORBA_long_long";

mk_c_type(_G, _N, S, _) when is_record(S, union)->
    ic_forms:get_id2(S);

mk_c_type(_G, N, S, _) when is_record(S, struct) -> %% Locally defined member
    Fullname = [ic_forms:get_id2(S) | N],
    ic_util:to_undersc(Fullname);

mk_c_type(_G, _N, {'any', _}, _) ->  %% Fix for any type
    "CORBA_long";

mk_c_type(_G, _N, {T, _}, _) ->
    "CORBA_" ++ atom_to_list(T).

%%-------------------------------------------------------------------
%%    IDL to C type conversion used by the emit_c_*_rpt macros.
%%-------------------------------------------------------------------
mk_c_type2(G, N, S) when element(1, S) == scoped_id ->
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
	    mk_c_type2(G, N, Type);
	Type ->
	    mk_c_type2(G, N, Type)
    end;

mk_c_type2(_G, _N, S) when is_list(S) ->
    S;
mk_c_type2(_G, _N, S) when is_record(S, string) ->
    "CORBA_char *";
mk_c_type2(_G, _N, S) when is_record(S, wstring) -> 
    "CORBA_wchar *";
mk_c_type2(_G, _N, {boolean, _}) ->
    "CORBA_boolean";
mk_c_type2(_G, _N, {octet, _}) ->
    "CORBA_octet";
mk_c_type2(_G, _N, {void, _}) ->
    "void";
mk_c_type2(_G, _N, {unsigned, U}) ->
    case U of
	{short,_} ->
	    "CORBA_unsigned_short";
	{long,_} ->
	    "CORBA_unsigned_long";
	{'long long',_} ->
	    "CORBA_unsigned_long_long"
    end;

mk_c_type2(_G, _N, {'long long', _}) ->
    "CORBA_long_long";

mk_c_type2(_G, _N, S) when is_record(S, union)->
    ic_forms:get_id2(S);

mk_c_type2(_G, N, S) when is_record(S, struct) ->
    Fullname = [ic_forms:get_id2(S) | N],
    ic_util:to_undersc(Fullname);

mk_c_type2(_G, _N, S) when is_record(S, sequence) ->
    mk_c_type2(_G, _N, S#sequence.type);

mk_c_type2(_G, _N, {'any', _}) ->  %% Fix for any type
    "CORBA_long";

mk_c_type2(_G, _N, {T, _}) ->
    "CORBA_" ++ atom_to_list(T).

%%-----

is_variable_size_rec(Es) ->
    lists:any(
      fun({_N, T}) -> is_variable_size(T);
	 ({_, _N, T}) -> is_variable_size(T)
      end, Es).

is_variable_size({'tk_struct', _IFRId, "port", _ElementList}) ->
    false;
is_variable_size({'tk_struct', _IFRId, "pid", _ElementList}) ->
    false;
is_variable_size({'tk_struct', _IFRId, "ref", _ElementList}) ->
    false;
is_variable_size({'tk_struct', _IFRId, "term", _ElementList}) ->
    false;
is_variable_size({'tk_struct', _IFRId, _Name, ElementList}) ->
    is_variable_size_rec(ElementList);
is_variable_size({'tk_array', ElemTC, _Length}) ->
    is_variable_size(ElemTC);
is_variable_size({'tk_string', _}) -> 
    true;
is_variable_size({'tk_wstring', _}) ->
    true;
is_variable_size({'tk_sequence', _ElemTC, _MaxLsextractength}) ->
    true;
is_variable_size({'tk_union', _IFRId, _Name, _, _, ElementList}) ->
    is_variable_size_rec(ElementList);
is_variable_size(_Other) ->
    false.


is_variable_size(_G, _N, T)  when is_record(T, string) ->
    true;
is_variable_size(_G, _N, T)  when is_record(T, wstring) ->
    true;
is_variable_size(_G, _N, T)  when is_record(T, sequence) ->
    true;
is_variable_size(G, N, T)  when is_record(T, union) ->
    %%io:format("~n~p = ~p~n",[ic_forms:get_id2(T),ictype:fetchTk(G, N, T)]),
    is_variable_size(ictype:fetchTk(G, N, T));
is_variable_size(G, N, T)  when is_record(T, struct) ->
    is_variable_size(ictype:fetchTk(G, N, T));
is_variable_size(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
	{_FullScopedName, _, TK, _} ->
	    is_variable_size(TK);
	_ ->
	    ic_error:fatal_error(G, {name_not_found, T})
    end;
is_variable_size(_G, _N, _Other) ->
    false.    

%% mk_dim produces 
mk_dim([Arg | Args]) ->
    "[" ++ Arg ++ "]" ++ mk_dim(Args);
mk_dim([]) -> [].

mk_slice_dim(Args) ->
    mk_dim(tl(Args)).


emit_tmp_variables(Fd) ->
    DeclList = get(tmp_declarations),
    emit_tmp_variables(Fd, DeclList),
    ok.

emit_tmp_variables(Fd, [Decl |Rest]) ->
    emit_tmp_variables(Fd, Rest),
    emit(Fd, "~s", [Decl]);
emit_tmp_variables(_Fd, []) ->
    ok.

store_tmp_decl(Format, Args) ->
    Decl = io_lib:format(Format, Args),
    DeclList = get(tmp_declarations),
    put(tmp_declarations, [Decl |DeclList]).

%%------------------------------------------------------------
%%
%% Parser utilities
%%
%% Called from the yecc parser. Expands the identifier list of an
%% attribute so that the attribute generator never has to handle
%% lists.
%%
%%------------------------------------------------------------

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



%% Usefull functions
get_param_tk(Name, Op) ->
    case get_param(Name, Op) of
	error ->
	    error;
	Param ->
	    ic_forms:get_tk(Param)
    end.

get_param(Name, Op) when is_record(Op, op) ->
    get_param_loop(Name, Op#op.params);
get_param(_Name, _Op) ->
    error.

get_param_loop(Name,[Param|Params]) ->
    case ic_forms:get_id2(Param) of
	Name ->
	    Param;
	_ ->
	    get_param_loop(Name,Params)
    end;
get_param_loop(_Name, []) ->
    error.


%% Input is a list of parameters (in parse form) and output is a list
%% of parameter attribute and variable names.
mk_c_vars(Params) ->
    lists:map(fun(P) -> {A, _} = P#param.inout,
			{A, ic_forms:get_id(P#param.id)}
	      end,
	      Params).

normalize_type({unsigned, {short, _}}) ->	{basic, ushort};
normalize_type({unsigned, {long, _}}) -> 	{basic, ulong};
normalize_type({unsigned, {'long long', _}}) ->	{basic, ulonglong};
normalize_type({short,_}) ->			{basic, short};
normalize_type({long, _}) ->			{basic, long};
normalize_type({'long long', _}) ->		{basic, longlong};
normalize_type({float,_}) ->			{basic, float};
normalize_type({double, _}) ->			{basic, double};
normalize_type({boolean, _}) ->			{basic, boolean};
normalize_type({char, _}) ->			{basic, char};
normalize_type({wchar, _}) ->			{basic, wchar};
normalize_type({octet, _}) ->			{basic, octet};
normalize_type({any, _}) ->			{basic, any};
normalize_type(tk_ushort) -> 			{basic, ushort};
normalize_type(tk_ulong) -> 			{basic, ulong};
normalize_type(tk_ulonglong) -> 		{basic, ulonglong};
normalize_type(tk_short) -> 			{basic, short};
normalize_type(tk_long) -> 			{basic, long};
normalize_type(tk_longlong) -> 			{basic, longlong};
normalize_type(tk_float) -> 			{basic, float};
normalize_type(tk_double) -> 			{basic, double};
normalize_type(tk_boolean) -> 			{basic, boolean};
normalize_type(tk_char) -> 			{basic, char};
normalize_type(tk_wchar) -> 			{basic, wchar};
normalize_type(tk_octet) -> 			{basic, octet}; 
normalize_type(tk_any) -> 			{basic, any};
normalize_type(ushort) -> 			{basic, ushort};
normalize_type(ulong) -> 			{basic, ulong};
normalize_type(ulonglong) ->	 		{basic, ulonglong};
normalize_type(short) -> 			{basic, short};
normalize_type(long) -> 			{basic, long};
normalize_type(longlong) -> 			{basic, longlong};
normalize_type(float) -> 			{basic, float};
normalize_type(double) -> 			{basic, double};
normalize_type(boolean) -> 			{basic, boolean};
normalize_type(char) -> 			{basic, char};
normalize_type(wchar) -> 			{basic, wchar};
normalize_type(octet) -> 			{basic, octet}; 
normalize_type(any) -> 				{basic, any};
normalize_type(Type) ->	Type.

