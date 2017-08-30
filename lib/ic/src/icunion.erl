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

-module(icunion).

-import(ic_codegen, [emit/2, emit/3, emit/4, emit_c_enc_rpt/4, emit_c_dec_rpt/4]).
-import(ic_cbe, [mk_c_type/3, mk_c_type/4]).

-include("icforms.hrl").
-include("ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([union_gen/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

union_gen(G, N, X, c) when is_record(X, union) ->
    emit_c_union(G, N, X);
union_gen(_G, _N, _X, _L) ->
    ok.


%% Emits the union
emit_c_union(G, N, X) ->
    %%io:format("Rec = ~p\n",[X]),
    case ic_genobj:is_hrlfile_open(G) of
	true ->

	    %% Sort Union Default = put it last in case list
	    NewX = #union{ id = X#union.id,
			   type = X#union.type,
			   body = mvDefaultToTail(X#union.body),
			   tk = X#union.tk },

	    UnionScope = [ic_forms:get_id2(NewX) | N],

	    case ic_pragma:is_local(G,UnionScope) of

		true ->

		    HFd = ic_genobj:hrlfiled(G),
		    emit_c_union_values(G, N, NewX, HFd),
		    UnionName = ic_util:to_undersc(UnionScope),

		    emit(HFd, "\n#ifndef __~s__\n",[ictype:to_uppercase(UnionName)]),	
		    emit(HFd, "#define __~s__\n",[ictype:to_uppercase(UnionName)]),
		    ic_codegen:mcomment_light(HFd,
					      [io_lib:format("Union definition: ~s",
							     [UnionName])],
					      c),
		    emit(HFd, "typedef struct {\n"),
		    emit(HFd, "  ~s _d;\n", [get_c_union_discriminator(G, N, NewX)]),
		    emit(HFd, "  union {\n"),
		    emit_c_union_values_decl(G, N, NewX, HFd),
		    emit(HFd, "  } _u;\n"),
		    emit(HFd, "} ~s;\n\n", [UnionName]),

		    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
			 [ic_util:mk_oe_name(G, "sizecalc_"), UnionName]),
		    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s*);\n",
			 [ic_util:mk_oe_name(G, "encode_"), UnionName, UnionName]),
		    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s*);\n",
			 [ic_util:mk_oe_name(G, "decode_"), UnionName, UnionName]),
		    emit(HFd, "\n#endif\n\n"),
		    create_c_union_file(G, N, NewX, UnionName);

		false -> %% Do not generate included types att all.
		    ok
	    end;
	false ->
	    ok
    end.



%% Loops over union members and creates members typedefs
emit_c_union_values(G, N, X, Fd) ->
    emit_c_union_values_loop(G, N, X, Fd, X#union.body).

emit_c_union_values_loop(G, N, X, Fd, [CU]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array, _AID, _SZ} -> % Check for arrays 	
		    mk_array_file(G,N,X,Id,Type,Fd);
		_ ->              % Elementary types or seq/struct
		    ok
	    end;
	_ ->
	    error
    end;
emit_c_union_values_loop(G, N, X, Fd, [CU |CUs]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array, _AID, _SZ} -> % Check for arrays	
		    mk_array_file(G,N,X,Id,Type,Fd);
		_ ->              % Elementary types or seq/struct
		    emit_c_union_values_loop(G, N, X, Fd, CUs)
	    end;
	_ ->
	    error
    end.


%% Loops over union members and declares members inside union structure
emit_c_union_values_decl(G, N, X, Fd) ->
    emit_c_union_values_decl_loop(G, N, X, Fd, X#union.body).

emit_c_union_values_decl_loop(G, N, X, Fd, [CU]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array, _AID, _SZ} -> % Check for arrays 	
		    mk_array_decl(G,N,X,Id,Type,Fd);
		_ ->              % Elementary types or seq/struct
		    mk_union_member_decl(G,N,X,Id,Type,Fd),
		    ok
	    end;
	_ ->
	    error
    end;
emit_c_union_values_decl_loop(G, N, X, Fd, [CU |CUs]) ->
    case CU of
	{case_dcl,_,Id,Type} ->
	    case Id of
		{array, _AID, _SZ} -> % Check for arrays	
		    mk_array_decl(G,N,X,Id,Type,Fd),
		    emit_c_union_values_decl_loop(G, N, X, Fd, CUs);
		_ ->              % Elementary types or seq/struct
		    mk_union_member_decl(G,N,X,Id,Type,Fd),
		    emit_c_union_values_decl_loop(G, N, X, Fd, CUs)
	    end;
	_ ->
	    error
    end.


%% Makes the declaration for the array in union
mk_array_decl(G,N,X,Id,Type,Fd) ->
    emit(Fd, "    ~s ~s;\n", 
	 [getCaseTypeStr(G,N,X,Id,Type), 
	  mk_array_name(Id)]).

mk_array_name({array,Id,D}) ->
    ic_forms:get_id2(Id) ++ mk_array_dim(D).

mk_array_dim([]) ->
    "";
mk_array_dim([{_,_,Dim}|Dims]) ->
    "[" ++ Dim ++ "]" ++ mk_array_dim(Dims).


%% Creates the array file 
mk_array_file(G,N,X,{array,AID,SZ},Type,HFd) ->
    ArrayName = ic_util:to_undersc([ic_forms:get_id2(AID),ic_forms:get_id2(X) | N]),
    ArrayDim =  extract_array_dim(SZ),
    emit(HFd, "\n#ifndef __~s__\n",[ictype:to_uppercase(ArrayName)]),	
    emit(HFd, "#define __~s__\n\n",[ictype:to_uppercase(ArrayName)]),
    icstruct:create_c_array_coding_file(G,
					N,
					{ArrayName,ArrayDim},
					Type,
					no_typedef),
    emit(HFd, "\n#endif\n\n").

extract_array_dim([{_,_,Dim}]) ->
    [Dim];
extract_array_dim([{_,_,Dim}|Dims]) ->
    [Dim | extract_array_dim(Dims)].


%% Makes the declaration for the member in union
mk_union_member_decl(G,N,X,Id,Type,Fd) ->
    emit(Fd, "    ~s ~s;\n", 
	 [getCaseTypeStr(G,N,X,Id,Type), 
	  ic_forms:get_id2(Id)]).




%% File utilities
create_c_union_file(G, N, X, UnionName) ->

    {Fd , SName} = open_c_coding_file(G, UnionName),
    _HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    HrlFName = filename:basename(ic_genobj:include_file(G)),
    ic_codegen:emit_stub_head(G, Fd, SName, c),
    emit(Fd, "#include \"~s\"\n\n",[HrlFName]),

    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Fd = ic_genobj:stubfiled(G), %% Write on stubfile
    %%  HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    %%  HrlFName = filename:basename(ic_genobj:include_file(G)),
    %%  emit(Fd, "#include \"~s\"\n\n",[HrlFName]),
    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    %% Write generated code on file
    emit_union_sizecount(G, N, X, Fd, UnionName),
    emit_union_encode(G, N, X, Fd, UnionName),
    emit_union_decode(G, N, X, Fd, UnionName),
    file:close(Fd).

open_c_coding_file(G, Name) ->
    SName = string:concat(ic_util:mk_oe_name(G, "code_"), Name),
    FName =  
        ic_file:join(ic_options:get_opt(G, stubdir),ic_file:add_dot_c(SName)),
    case file:open(FName, [write]) of
        {ok, Fd} ->
            {Fd, SName};
        Other ->
            exit(Other)
    end.




get_c_union_discriminator(G, N, X) ->
    case getDiscrStr(G, N, X#union.type) of
	error ->
	    ic_error:fatal_error(G, {illegal_typecode_for_c, X#union.type, N});
	DiscrStr ->
	    case ic_code:get_basetype(G, DiscrStr) of
		{short, _} ->
		    "CORBA_short";
		{unsigned,{short, _}} ->
		    "CORBA_unsigned_short";
		{long, _} ->
		    "CORBA_long";
		{unsigned,{long, _}} ->
		    "CORBA_unsigned_long";
		{boolean,_} ->
		    "CORBA_boolean";
		{char,_} ->
		    "CORBA_char";
		{enum, EnumType} ->
		    EnumType;
		_ ->
		    DiscrStr
	    end
    end.

getDiscrStr(G, N, S) when element(1, S) == scoped_id -> 
    case ic_symtab:get_full_scoped_name(G, N, S) of
	{FSN, _, tk_short, _} ->
	    ic_util:to_undersc(FSN);
	{FSN, _, tk_ushort, _} ->
	    ic_util:to_undersc(FSN);
	{FSN, _, tk_long, _} ->
	    ic_util:to_undersc(FSN);
	{FSN, _, tk_ulong, _} ->
	    ic_util:to_undersc(FSN);
	{FSN, _, tk_boolean, _} ->
	    ic_util:to_undersc(FSN);
	{FSN, _, tk_char, _} ->
	    ic_util:to_undersc(FSN);
	{FSN, _, {tk_enum,_,_,_}, _} ->
	    ic_util:to_undersc(FSN);
	_ ->
	    error
    end;
getDiscrStr(_G, N, X) ->
    case X of
	{short,_} ->
	    "CORBA_short";
	{unsigned,{short,_}} ->
	    "CORBA_unsigned_short";
	{long, _} ->
	    "CORBA_long";
	{unsigned,{long,_}} ->
	    "CORBA_unsigned_long";
	{boolean,_} ->
	    "CORBA_boolean";
	{char,_} ->
	    "CORBA_char";
	{enum,TID,_,_} ->
	    ic_util:to_undersc([ic_forms:get_id2(TID) | N]);
	_ ->
	    error
    end.




getCaseTypeStr(G, N, X, I, T) when element(1, T) == scoped_id ->
    case catch ic_symtab:get_full_scoped_name(G, N, T) of
	{FSN, _, _, _} ->
	    BT = ic_code:get_basetype(G, ic_util:to_undersc(FSN)),
	    case isList(BT) of
		true ->
		    BT;
		false ->
		    case BT of
			{short,_} ->
			    "CORBA_short";
			{unsigned,{short,_}} ->
			    "CORBA_unsigned_short";
			{long, _} ->
			    "CORBA_long";
			{unsigned,{long,_}} ->
			    "CORBA_unsigned_long";
			{float,_} ->
			    "CORBA_float";
			{double,_} ->
			    "CORBA_double";
			{boolean,_} ->
			    "CORBA_boolean";
			{char,_} ->
			    "CORBA_char";
			{wchar,_} ->
			    "CORBA_wchar";
			{octet,_} ->
			    "CORBA_octet";
			{string,_} ->
			    "CORBA_char*";
			{wstring,_} ->
			    "CORBA_wchar*";
			{sequence,_,_} ->
			    ic_util:to_undersc([ic_forms:get_id2(I), ic_forms:get_id2(X) | N]);
			{struct,SID,_,_} ->
			    ic_util:to_undersc([ic_forms:get_id2(SID), ic_forms:get_id2(X) | N]);
			{enum,EID} ->
			    EID;
			{any, _} -> %% Fix for any type
			    "CORBA_long";
			_ ->
			    %%io:format("BT = ~p~n",[BT]),
			    error
		    end
	    end
    end;
getCaseTypeStr(_G, N, X, I, T) ->
    case T of
	{short,_} ->
	    "CORBA_short";
	{unsigned,{short,_}} ->
	    "CORBA_unsigned_short";
	{long, _} ->
	    "CORBA_long";
	{unsigned,{long,_}} ->
	    "CORBA_unsigned_long";
	{float,_} ->
	    "CORBA_float";
	{double,_} ->
	    "CORBA_double";
	{boolean,_} ->
	    "CORBA_boolean";
	{char,_} ->
	    "CORBA_char";
	{wchar,_} ->
	    "CORBA_wchar";
	{octet,_} ->
	    "CORBA_octet";
	{string,_} ->
	    "CORBA_char*";
	{wstring,_} ->
	    "CORBA_wchar*";
	{sequence,_,_} ->
	    ic_util:to_undersc([ic_forms:get_id2(I), ic_forms:get_id2(X) | N]);
	{struct,SID,_,_} ->
	    ic_util:to_undersc([ic_forms:get_id2(SID), ic_forms:get_id2(X) | N]);
	{union,UID,_,_,_} ->
	    ic_util:to_undersc([ic_forms:get_id2(UID), ic_forms:get_id2(X) | N]);
	{any, _} -> %% Fix for any type
	    "CORBA_long";
	_ ->
	    error
    end.

isList(L) when is_list(L) ->
    true;
isList(_) ->
    false.

%%
%%  Sizecount facilities
%%
emit_union_sizecount(G, N, X, Fd, UnionName) ->
    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, int* oe_size) {\n\n",
	 [ic_util:mk_oe_name(G, "sizecalc_"), UnionName]),

    emit(Fd, "  int oe_malloc_size = 0;\n"),
    emit(Fd, "  int oe_error_code = 0;\n"),
    emit(Fd, "  int oe_type = 0;\n"),
    emit(Fd, "  int oe_tmp = 0;\n"),
    emit_union_discr_var_decl(G, N, X, Fd),

    ic_codegen:nl(Fd),
    emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + sizeof(",UnionName,")"]),
    emit(Fd, "    *oe_size = ~s;\n\n", [ic_util:mk_align(AlignName)]),

    emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &oe_tmp)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "    ", "ei_get_type", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    %%emit(Fd, "  if (oe_tmp != 3)\n"),
    %%emit(Fd, "    return -1;\n\n"),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),
    emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n", []),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit_c_union_discr_sizecount(G, N, X, Fd),
    emit(Fd, "  /* Calculate union size */\n"),
    emit(Fd, "  switch(oe_discr) {\n"),

    emit_c_union_loop(G, N, X, Fd, X#union.body, sizecalc),
    emit(Fd, "  }\n\n"),

    emit(Fd, "  *oe_size = ~s;\n",[ic_util:mk_align("*oe_size+oe_malloc_size")]),
    emit(Fd, "  return 0;\n"),
    emit(Fd, "}\n\n\n").


emit_union_discr_var_decl(G, N, X, Fd) ->
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    emit(Fd, "  long oe_discr = 0;\n");
	"CORBA_unsigned_short" ->
	    emit(Fd, "  unsigned long oe_discr = 0;\n");
	"CORBA_long" ->
	    emit(Fd, "  long oe_discr = 0;\n");
	"CORBA_unsigned_long" ->
	    emit(Fd, "  unsigned long oe_discr = 0;\n");
	"CORBA_boolean" ->
	    emit(Fd, "  int oe_discr = 0;\n"),
	    emit(Fd, "  char oe_bool[256];\n");
	"CORBA_char" ->
	    emit(Fd, "  char oe_discr = 0;\n");
	_T ->
	    emit(Fd, "  int oe_dummy = 0;\n"),
	    emit(Fd, "  ~s oe_discr = 0;\n",[UD])
    end.


emit_c_union_discr_sizecount(G, N, X, Fd) ->
    emit(Fd, "  /* Calculate discriminator size */\n"),
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_long", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_unsigned_short" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_ulong", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_long" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_long", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_unsigned_long" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_ulong", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_boolean" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, oe_bool)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_atom", []),
	    emit(Fd, "    return oe_error_code;\n  }\n"),
	    emit(Fd, "  if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    emit(Fd, "    oe_discr = 0;\n"), 
	    emit(Fd, "  }\n"),
	    emit(Fd, "  else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    emit(Fd, "    oe_discr = 1;\n"), 
	    emit(Fd, "  }\n"),
	    emit(Fd, "  else {\n"),
	    emit_c_dec_rpt(Fd, "    ", "not boolean", []),
	    emit(Fd, "    return -1;\n  }\n");

	"CORBA_char" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, &oe_discr)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ei_decode_char", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	T ->
	    emit(Fd, "  oe_tmp = *oe_size_count_index;\n"),
	    emit(Fd, "  if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0) {\n", [T]),
	    ?emit_c_dec_rpt(Fd, "    ", "oe_size_calc_~s", [T]),
	    emit(Fd, "    return oe_error_code;\n  }\n"),

	    emit(Fd, "  *oe_size_count_index = oe_tmp;\n"),
	    emit(Fd, "  oe_tmp = oe_env->_iin;\n"),
	    emit(Fd, "  oe_env->_iin = *oe_size_count_index;\n"),
	    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, NULL, &oe_dummy, &oe_discr)) < 0) {\n", [T]),
	    ?emit_c_dec_rpt(Fd, "    ", "oe_decode_~s", [T]),
	    emit(Fd, "    return oe_error_code;\n  }\n"),

	    emit(Fd, "  *oe_size_count_index = oe_env->_iin;\n"),
	    emit(Fd, "  oe_env->_iin = oe_tmp;\n\n")
    end.



emit_c_union_loop(G, N, X, Fd, CaseList, Case) ->
    emit_c_union_loop(G, N, X, Fd, CaseList, false, Case).

emit_c_union_loop(G, N, X, Fd, [], GotDefaultCase, Case) ->
    case GotDefaultCase of
	false ->
	    emit_c_union_valueless_discriminator(G, N, X, Fd, Case)
    end;
emit_c_union_loop(G, N, X, Fd, [CU|CUs], GotDefaultCase, Case) ->
    case CU of
	{case_dcl,CaseList,I,T} ->
	    GotDefaultCase = emit_c_union_case(G, N, X, Fd, I, T, CaseList, Case),
	    emit_c_union_loop(G, N, X, Fd, CUs, GotDefaultCase, Case);
	_ ->
	    error
    end.

emit_c_union_valueless_discriminator(_G, _N, _X, Fd, Case) ->  
    emit(Fd, "  default:\n"),
    case Case of
	sizecalc ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      char oe_undefined[15];\n\n"),
	    emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, "
		 "oe_size_count_index, oe_undefined)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "        ", "ei_decode_atom", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "    }\n");
	encode ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_atom(oe_env, \"undefined\")) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "      ", "oe_ei_encode_atom", []),
	    emit(Fd, "      return oe_error_code;\n      }\n"),
	    emit(Fd, "    break;\n");
	decode ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      char oe_undefined[15];\n\n"),
	    emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, "
		 "oe_undefined)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "        ", "ei_decode_atom", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      if (strcmp(oe_undefined, \"undefined\") != 0) {\n"),
	    emit_c_dec_rpt(Fd, "        ", "undefined", []),
	    emit(Fd, "        return -1;\n      }\n"),
	    emit(Fd, "    }\n")
    end.


emit_c_union_case(G, N, X, Fd, I, T, [{default,_}], Case) -> 
    emit(Fd, "  default:\n"),
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end, 
    true;
emit_c_union_case(G, N, X, Fd, I, T, [{Bool,_}], Case) -> %% Boolean discriminator
    case Bool of
	'TRUE' ->
	    emit(Fd, "  case 1:\n");
	'FALSE' ->
	    emit(Fd, "  case 0:\n")
    end,
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end,
    emit(Fd, "    break;\n\n"),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{Bool,_}|Rest], Case) -> %% Boolean discriminator
    case Bool of
	'TRUE' ->
	    emit(Fd, "  case 1:\n");
	'FALSE' ->
	    emit(Fd, "  case 0:\n")
    end,
    emit_c_union_case(G, N, X, Fd, I, T, Rest, Case),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{_,_,NrStr}], Case) -> %% Integer type discriminator
    case get_c_union_discriminator(G, N, X) of
	"CORBA_char" ->
	    emit(Fd, "  case \'~s\':\n",[NrStr]);
	_ ->
	    emit(Fd, "  case ~s:\n",[NrStr])
    end,
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end,
    emit(Fd, "    break;\n\n"),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{_,_,NrStr}|Rest], Case) -> %% Integer type discriminator
    emit(Fd, "  case ~s:\n",[NrStr]),
    emit_c_union_case(G, N, X, Fd, I, T, Rest, Case),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{scoped_id,_,_,[EID]}], Case) -> %% Enumerant type discriminator
    SID = ic_util:to_undersc([EID|get_c_union_discriminator_scope(G, N, X)]),
    %%io:format("SID = ~p~n",[SID]),
    emit(Fd, "  case ~s:\n",[SID]),
    case Case of
	sizecalc ->
	    getCaseTypeSizecalc(G, N, X, Fd, I, T);
	encode ->
	    getCaseTypeEncode(G, N, X, Fd, I, T);
	decode ->
	    getCaseTypeDecode(G, N, X, Fd, I, T)
    end,
    emit(Fd, "    break;\n\n"),
    false;
emit_c_union_case(G, N, X, Fd, I, T, [{scoped_id,_,_,[EID]}|Rest], Case) -> %% Enumerant type discriminator
    SID = ic_util:to_undersc([EID|get_c_union_discriminator_scope(G, N, X)]),
    %%io:format("SID = ~p~n",[SID]),
    emit(Fd, "  case ~s:\n",[SID]),
    emit_c_union_case(G, N, X, Fd, I, T, Rest, Case),
    false.


%%
%% Returns the enumerant discriminator scope
%%
get_c_union_discriminator_scope(G, N, X) ->
    {FullScopedName, _, _TK, _} = ic_symtab:get_full_scoped_name(G, N, X#union.type),
    BT = case ic_code:get_basetype(G, ic_util:to_undersc(FullScopedName)) of
	     {enum,ST} ->
		 ST;
	     Other ->
		 Other
	 end,
    tl(lists:reverse(string:tokens(BT,"_"))). %% Ugly work arround





getCaseTypeSizecalc(G, N, X, Fd, I, T) when element(1, T) == scoped_id ->
    case ic_fetch:member2type(G,X,I) of
	ushort ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "ushort:ei_decode_ulong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	ulong -> 
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "ulong:ei_decode_ulong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	short ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "short:ei_decode_long", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"); 	
	long ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "long:ei_decode_long", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	float ->
	    emit(Fd, "   if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "float:ei_decode_double", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");     
	double ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "double:ei_decode_double", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");       
	boolean ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "boolean:ei_decode_atom", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	char ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "char:ei_decode_char", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	octet ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "octet:ei_decode_char", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	string ->
	    emit(Fd, "    if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &oe_tmp)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "ei_get_type", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "ei_decode_string", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    oe_malloc_size = ~s;\n",[ic_util:mk_align("oe_malloc_size+oe_tmp+1")]);
	any -> %% Fix for any type
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "      ", "ei_decode_long", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");

	_ ->
	    case getCaseTypeStr(G, N, X, I, T) of
		"erlang_pid" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_pid(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n",
			 []),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_pid", []),
		    emit(Fd, "    return oe_error_code;\n    }\n");
		"erlang_port" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_port(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n",
			 []),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_port", []),
		    emit(Fd, "    return oe_error_code;\n    }\n");
		"erlang_ref" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n",
			 []),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_ref", []),
		    emit(Fd, "    return oe_error_code;\n    }\n");
		"erlang_term" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_term(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n",
			 []),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_deoce_term", []),
		    emit(Fd, "    return oe_error_code;\n    }\n");

		Other ->

		    emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [Other]),
		    ?emit_c_dec_rpt(Fd, "      ", "oe_sizecalc_~s", [Other]),
		    emit(Fd, "      return oe_error_code;\n    }\n")
	    end
    end;
getCaseTypeSizecalc(G, N, X, Fd, I, T) ->
    case I of 
	{array,_,_}  ->
	    ArrayName = ic_util:to_undersc([ic_forms:get_id2(I),ic_forms:get_id2(X) | N]),
	    emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0) {\n",
		 [ArrayName]),
	    ?emit_c_dec_rpt(Fd, "      ", "oe_sizecalc_~s", [ArrayName]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	_ ->
	    case T of
		{short,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "short:ei_decode_long", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{unsigned,{short,_}} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "ushort:ei_decode_ulong", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{long, _} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "long:ei_decode_long", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{unsigned,{long,_}} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "ulong:ei_decode_ulong", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{float,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "float:ei_decode_double", []),
		    emit(Fd, "      return oe_error_code;\n    }");
		{double,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "double:ei_decode_double", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{boolean,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "boolean:ei_decode_atom", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{char,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "char:ei_decode_char", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{octet,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "octet:ei_decode_char", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{string,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, &oe_tmp)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "ei_get_type", []),
		    emit(Fd, "      return oe_error_code;\n    }\n"),
		    emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "ei_decode_string", []),
		    emit(Fd, "      return oe_error_code;\n    }\n"),
		    emit(Fd, "    oe_malloc_size = ~s;\n",[ic_util:mk_align("oe_malloc_size+oe_tmp+1")]);
		{sequence,_,_} ->
		    SeqName = ic_util:to_undersc([ic_forms:get_id2(I), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [SeqName]),
		    ?emit_c_dec_rpt(Fd, "      ", "sequence:oe_sizecalc_~s", [SeqName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{struct,SID,_,_} ->
		    StructName = ic_util:to_undersc([ic_forms:get_id2(SID), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [StructName]),
		    ?emit_c_dec_rpt(Fd, "      ", "struct:oe_sizecalc_~s", [StructName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{union,UID,_,_,_} ->
		    UnionName = ic_util:to_undersc([ic_forms:get_id2(UID), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_sizecalc_~s(oe_env, oe_size_count_index, &oe_malloc_size)) < 0) {\n",
			 [UnionName]),
		    ?emit_c_dec_rpt(Fd, "      ", "union:oe_sizecalce_~s", [UnionName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{any, _} -> %% Fix for any type
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "      ", "any:ei_decode_long", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		_ ->
		    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.





%%
%% Encode facilities
%%
emit_union_encode(G, N, X, Fd, UnionName) ->
    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s* oe_rec) {\n\n",
	 [ic_util:mk_oe_name(G, "encode_"), UnionName, UnionName]),

    emit(Fd, "  int oe_error_code = 0;\n\n"),

    emit(Fd, "  if ((oe_error_code = oe_ei_encode_tuple_header(oe_env, 3)) < 0) {\n"),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  if ((oe_error_code = oe_ei_encode_atom(oe_env, \"~s\")) < 0) {\n", 
	 [UnionName]),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit_c_union_discr_encode(G, N, X, Fd),  
    emit(Fd, "  /* Encode union */\n"),
    emit(Fd, "  switch(oe_rec->_d) {\n"),
    emit_c_union_loop(G, N, X, Fd, X#union.body, encode),
    emit(Fd, "  }\n\n"),
    emit(Fd, "  return 0;\n"),
    emit(Fd, "}\n\n\n").


emit_c_union_discr_encode(G, N, X, Fd) ->
    emit(Fd, "  /* Encode descriminator */\n"),
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_d)) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_long", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_unsigned_short" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_d)) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_ulong", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_long" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_d)) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_long", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_unsigned_long" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_d)) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_ulong", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_boolean" ->
	    emit(Fd, "  switch(oe_rec->_d) {\n"),
	    emit(Fd, "  case 0:\n"),
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "      ", "oe_ei_encode_atom", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    break;\n"),
	    emit(Fd, "  case 1:\n"),
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "      ", "oe_ei_encode_atom", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    break;\n"),
	    emit(Fd, "  default:\n"),
	    emit_c_enc_rpt(Fd, "    ", "boolean failure", []),
	    emit(Fd, "    return -1;\n"),
	    emit(Fd, "  }\n\n");
	"CORBA_char" ->
	    emit(Fd, "  if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_d)) < 0) {\n"),
	    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_char", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	T ->
	    emit(Fd, "  if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_d)) < 0) {\n", [T]),
	    ?emit_c_enc_rpt(Fd, "    ", "oe_encode_~s", [T]),
	    emit(Fd, "    return oe_error_code;\n  }\n")
    end.


getCaseTypeEncode(G, N, X, Fd, I, T) when element(1, T) == scoped_id -> 
    case ic_fetch:member2type(G,X,I) of
	ushort ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "ushort:oe_ei_encode_ulong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	ulong -> 
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "ulong:oe_ei_encode_ulong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	short ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "short:oe_ei_encode_long", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"); 	
	long ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "long:oe_ei_encode_long", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	float ->
	    emit(Fd, "   if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "float:oe_ei_encode_double", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");     
	double ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "double:oe_ei_encode_double", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");       
	boolean ->
	    emit(Fd, "    switch(oe_rec->_u.~s) {\n",[ic_forms:get_id2(I)]),
	    emit(Fd, "    case 0:\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0) {\n"),
	    ?emit_c_enc_rpt(Fd, "        ", "boolean:oe_ei_encode_atom", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    case 1:\n"),
	    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0) {\n"),
	    ?emit_c_enc_rpt(Fd, "        ", "boolean:oe_ei_encode_atom", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      break;\n"),
	    emit(Fd, "    default:\n"),
	    ?emit_c_enc_rpt(Fd, "      ", "boolean failure", []),
	    emit(Fd, "      return -1;\n"),
	    emit(Fd, "    }\n");
	char ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "char:oe_ei_encode_char", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	octet ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "octet:oe_ei_encode_char", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	string ->
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_string(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "oe_ei_encode_string", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	struct ->
	    case ic_cbe:mk_c_type(G, N, T, evaluate_not) of
		"erlang_pid" ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_pid(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_pid", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		"erlang_port" ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_port(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_port", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		"erlang_ref" ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_ref(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_ref", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		"ETERM*" ->
		    emit(Fd, "  if ((oe_error_code = oe_ei_encode_term(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_term", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		_ ->
		    emit(Fd, "  if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [getCaseTypeStr(G, N, X, I, T), ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "    ", "oe_encode_~s", 
				    [getCaseTypeStr(G, N, X, I, T)]),
		    emit(Fd, "    return oe_error_code;\n  }\n")
	    end;
	sequence ->
	    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "sequence:oe_encode_~s", 
			    [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	array ->
	    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "array:oe_encode_~s", 
			    [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	union ->
	    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "union:oe_encode_~s", 
			    [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	enum ->
	    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "enum:oe_encode_~s", 
			    [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	any -> %% Fix for any type
	    emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_enc_rpt(Fd, "      ", "enum:oe_ei_encodelong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	_ ->
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end;
getCaseTypeEncode(G, N, X, Fd, I, T) ->
    case I of
	{array,AID,_} ->
	    ArrayName = ic_util:to_undersc([ic_forms:get_id2(AID),ic_forms:get_id2(X) | N]),
	    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, oe_rec->_u.~s)) < 0) {\n",
		 [ArrayName,ic_forms:get_id2(AID)]),
	    ?emit_c_enc_rpt(Fd, "      ", "array:oe_encode_~s", [ArrayName]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	_ ->
	    case T of
		{short,_} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "short:oe_ei_encode_long", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{unsigned,{short,_}} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "ushort:oe_ei_encode_ulong", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{long, _} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_long(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "long:oe_ei_encode_long", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{unsigned,{long,_}} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_ulong(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "ulong:oe_ei_encode_ulong", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{float,_} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "float:oe_ei_encode_double", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{double,_} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_double(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "double:oe_ei_encode_double", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{boolean,_} ->
		    emit(Fd, "    switch(oe_rec->_u.~s) {\n",[ic_forms:get_id2(I)]),
		    emit(Fd, "    case 0:\n"),
		    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"false\")) < 0) {\n"),
		    ?emit_c_enc_rpt(Fd, "        ", "boolean:oe_ei_encode_atom", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),
		    emit(Fd, "      break;\n"),
		    emit(Fd, "    case 1:\n"),
		    emit(Fd, "      if ((oe_error_code = oe_ei_encode_atom(oe_env, \"true\")) < 0) {\n"),
		    ?emit_c_enc_rpt(Fd, "        ", "boolean:oe_ei_encode_atom", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),
		    emit(Fd, "      break;\n"),
		    emit(Fd, "    default:\n"),
		    ?emit_c_enc_rpt(Fd, "      ", "boolean failure", []),
		    emit(Fd, "      return -1;\n"),
		    emit(Fd, "    }\n");
		{char,_} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "char:oe_ei_encode_char", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{octet,_} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_char(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "octet:oe_ei_encode_char", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{string,_} ->
		    emit(Fd, "    if ((oe_error_code = oe_ei_encode_string(oe_env, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "oe_ei_encode_string", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{sequence,_,_} ->
		    SeqName = ic_util:to_undersc([ic_forms:get_id2(I), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [SeqName,ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "sequence:oe_encode_~s", [SeqName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{struct,SID,_,_} ->
		    StructName = ic_util:to_undersc([ic_forms:get_id2(SID), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [StructName,ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "struct:oe_encode_~s", [StructName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{union,UID,_,_,_} ->
		    UnionName = ic_util:to_undersc([ic_forms:get_id2(UID), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_encode_~s(oe_env, &oe_rec->_u.~s)) < 0) {\n",
			 [UnionName,ic_forms:get_id2(I)]),
		    ?emit_c_enc_rpt(Fd, "      ", "union:oe_encode_~s", [UnionName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		_ ->
		    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.




%%
%% Decode facilities
%%
emit_union_decode(G, N, X, Fd, UnionName) ->
    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_index, ~s* oe_rec) {\n\n",
	 [ic_util:mk_oe_name(G, "decode_"), UnionName, UnionName]),

    emit(Fd, "  int oe_error_code = 0;\n"),
    emit(Fd, "  int oe_tmp = 0;\n"),
    emit(Fd, "  char oe_union_name[256];\n\n"),

    emit(Fd, "  if((char*) oe_rec == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_index + sizeof(",UnionName,")"]),
    emit(Fd, "    *oe_index = ~s;\n\n", [ic_util:mk_align(AlignName)]),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, &oe_env->_iin, &oe_tmp)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_union_name)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit_c_union_discr_decode(G, N, X, Fd),
    emit(Fd, "  /* Decode union */\n"),
    emit(Fd, "  switch(oe_rec->_d) {\n"),
    emit_c_union_loop(G, N, X, Fd, X#union.body, decode),
    emit(Fd, "  }\n\n"),

    emit(Fd, "  *oe_index = ~s;\n", [ic_util:mk_align("*oe_index")]),
    emit(Fd, "  return 0;\n"),
    emit(Fd, "}\n\n\n").


emit_c_union_discr_decode(G, N, X, Fd) ->
    emit(Fd, "  /* Decode descriminator */\n"),
    UD = get_c_union_discriminator(G, N, X),
    case UD of
	"CORBA_short" ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    long oe_long;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_long)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "      ", "short:ei_decode_long", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    oe_rec->_d = (short) oe_long;\n\n"),
	    emit(Fd, "    if (oe_rec->_d !=  oe_long)\n      return -1;\n"),
	    emit(Fd, "  }\n\n");
	"CORBA_unsigned_short" ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    unsigned long oe_ulong;\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_ulong)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "      ", "unshort:ei_decode_ulong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    oe_rec->_d = (unsigned short) oe_ulong;\n\n"),
	    emit(Fd, "    if (oe_rec->_d !=  oe_ulong)\n      return -1;\n"),
	    emit(Fd, "  }\n\n");
	"CORBA_long" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_d)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "long:ei_decode_long", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_unsigned_long" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_d)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "ulong:ei_decode_ulong", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	"CORBA_boolean" ->
	    emit(Fd, "  {\n"),
	    emit(Fd, "    char oe_bool[25];\n\n"),
	    emit(Fd, "    if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_bool)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "      ", "boolean:ei_decode_atom", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    emit(Fd, "      oe_rec->_d = 0;\n"), 
	    emit(Fd, "    }else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    emit(Fd, "      oe_rec->_d = 1;\n"), 
	    emit(Fd, "    } else {\n"),
	    emit_c_dec_rpt(Fd, "      ", "boolean failure", []),
	    emit(Fd, "      return -1;\n    }\n"),
	    emit(Fd, "  }\n\n");
	"CORBA_char" ->
	    emit(Fd, "  if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_d)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "    ", "char:ei_decode_char", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	T ->
	    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_d)) < 0) {\n",
		 [T]),
	    ?emit_c_dec_rpt(Fd, "    ", "oe_decode_~s", [T]),
	    emit(Fd, "    return oe_error_code;\n  }\n")
    end.



getCaseTypeDecode(G, N, X, Fd, I, T) when element(1, T) == scoped_id -> 
    case ic_fetch:member2type(G,X,I) of
	ushort ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      unsigned long oe_ulong;\n"),
	    emit(Fd, "      if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_ulong)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "        ", "ushort:ei_decode_ulong", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      oe_rec->_u.~s = (unsigned short) oe_ulong;\n\n",[ic_forms:get_id2(I)]),
	    emit(Fd, "      if (oe_rec->_u.~s !=  oe_ulong)\n        return -1;\n",[ic_forms:get_id2(I)]),
	    emit(Fd, "    }\n");
	ulong -> 
	    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "      ", "ulong:ei_decode_ulong", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	short ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      long oe_long;\n"),
	    emit(Fd, "      if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_long)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "        ", "short:ei_decode_long", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      oe_rec->_u.~s = (short) oe_long;\n\n",[ic_forms:get_id2(I)]),
	    emit(Fd, "      if (oe_rec->_u.~s !=  oe_long)\n        return -1;\n",[ic_forms:get_id2(I)]),
	    emit(Fd, "    }\n");
	long ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "    ", "long:ei_decode_long", []),
	    emit(Fd, "    return oe_error_code;\n    }\n");
	float ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      double oe_double;\n"),
	    emit(Fd, "      if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_double)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "        ", "float:ei_decode_double", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      oe_rec->_u.~s = (float) oe_double;\n",[ic_forms:get_id2(I)]),
	    emit(Fd, "    }\n");
	double ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "      ", "double:ei_decode_double", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	boolean ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      char oe_bool[25];\n\n"),
	    emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_bool)) < 0) {\n"),
	    ?emit_c_dec_rpt(Fd, "        ", "boolean:ei_decode_atom", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),
	    emit(Fd, "      if (strcmp(oe_bool, \"false\") == 0) {\n"),
	    emit(Fd, "        oe_rec->_u.~s = 0;\n",[ic_forms:get_id2(I)]), 
	    emit(Fd, "      } else if (strcmp(oe_bool, \"true\") == 0) {\n"),
	    emit(Fd, "        oe_rec->_u.~s = 1;\n",[ic_forms:get_id2(I)]), 
	    emit(Fd, "      } else {\n"),
	    ?emit_c_dec_rpt(Fd, "        ", "boolean failure", []),
	    emit(Fd, "        return -1;\n        }\n"),
	    emit(Fd, "    }\n");
	char ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "      ", "char:ei_decode_char", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	octet ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "      ", "octet:ei_decode_char", []),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	string ->
	    emit(Fd, "    {\n"),
	    emit(Fd, "      int oe_type = 0;\n"),
	    emit(Fd, "      int oe_string_ctr = 0;\n\n"),

	    emit(Fd, "      (int) ei_get_type(oe_env->_inbuf, &oe_env->_iin, &oe_type, &oe_string_ctr);\n\n"),

	    emit(Fd, "      oe_rec->_u.~s = (void *) (oe_first + *oe_index);\n\n",[ic_forms:get_id2(I)]),

	    emit(Fd, "      if ((oe_error_code = ei_decode_string(oe_env->_inbuf, &oe_env->_iin, oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "        ", "ei_decode_string", []),
	    emit(Fd, "        return oe_error_code;\n      }\n"),

	    emit(Fd, "      *oe_index = ~s;\n",[ic_util:mk_align("*oe_index+oe_string_ctr+1")]),
	    emit(Fd, "    }\n");
	struct ->
	    case ic_cbe:mk_c_type(G, N, T, evaluate_not) of
		"erlang_pid" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_pid(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_pid", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		"erlang_port" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_port(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_port", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		"erlang_ref" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_ref(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_ref", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");
		"ETERM*" ->
		    emit(Fd, "  if ((oe_error_code = ei_decode_term(oe_env->_inbuf, &oe_env->_iin, (void **)&oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "    ", "ei_decode_term", []),
		    emit(Fd, "    return oe_error_code;\n  }\n");

		_ ->
		    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
			 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "    ", "oe_decode_~s", 
				    [getCaseTypeStr(G, N, X, I, T)]),
		    emit(Fd, "    return oe_error_code;\n  }\n")
	    end;
	sequence ->
	    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "    ", "sequence:oe_decode_~s", 
			    [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	array ->
	    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "    ", "array:oe_decode_~s", [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	union ->
	    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "    ", "union:oe_decode_~s", [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	enum ->
	    emit(Fd, "  if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
		 [getCaseTypeStr(G, N, X, I, T),ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "    ", "enum:oe_decode_~s", [getCaseTypeStr(G, N, X, I, T)]),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	any -> %% Fix for any type
	    emit(Fd, "  if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
		 [ic_forms:get_id2(I)]),
	    ?emit_c_dec_rpt(Fd, "    ", "any:ei_decodelong", []),
	    emit(Fd, "    return oe_error_code;\n  }\n");
	_ ->
	    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
    end;
getCaseTypeDecode(G, N, X, Fd, I, T) ->
    case I of
	{array,AID,_} ->
	    ArrayName = ic_util:to_undersc([ic_forms:get_id2(AID),ic_forms:get_id2(X) | N]),
	    emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, oe_rec->_u.~s)) < 0) {\n",
		 [ArrayName,ic_forms:get_id2(AID)]),
	    ?emit_c_dec_rpt(Fd, "      ", "array:oe_decode_~s", [ArrayName]),
	    emit(Fd, "      return oe_error_code;\n    }\n");
	_ ->
	    case T of
		{short,_} ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      long oe_long;\n"),
		    emit(Fd, "      if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_long)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "        ", "short:ei_decode_long", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),
		    emit(Fd, "      oe_rec->_u.~s = (short) oe_long;\n\n",[ic_forms:get_id2(I)]),
		    emit(Fd, "      if (oe_rec->_u.~s !=  oe_long)\n        return -1;\n",[ic_forms:get_id2(I)]),
		    emit(Fd, "    }\n");
		{unsigned,{short,_}} ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      unsigned long oe_ulong;\n"),
		    emit(Fd, "      if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_ulong)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "        ", "ushort:ei_decode_ulong", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),
		    emit(Fd, "      oe_rec->_u.~s = (unsigned short) oe_ulong;\n\n",[ic_forms:get_id2(I)]),
		    emit(Fd, "      if (oe_rec->_u.~s !=  oe_ulong)\n        return -1;\n",[ic_forms:get_id2(I)]),
		    emit(Fd, "    }\n");
		{long, _} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_long(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "long:ei_decode_long", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{unsigned,{long,_}} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_ulong(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "ulong:ei_decode_ulong", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{float,_} ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      double oe_double;\n"),
		    emit(Fd, "      if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_double)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "        ", "float:ei_decode_double", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),
		    emit(Fd, "      oe_rec->_u.~s = (float) oe_double;\n",[ic_forms:get_id2(I)]),
		    emit(Fd, "    }\n");
		{double,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_double(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "dobule:ei_decode_double", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{boolean,_} ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      char oe_bool[25];\n\n"),
		    emit(Fd, "      if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_bool)) < 0) {\n"),
		    ?emit_c_dec_rpt(Fd, "        ", "boolean:ei_decode_atom", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),
		    emit(Fd, "      if (strcmp(oe_bool, \"false\") == 0) {\n"),
		    emit(Fd, "        oe_rec->_u.~s = 0;\n",[ic_forms:get_id2(I)]), 
		    emit(Fd, "      } else if (strcmp(oe_bool, \"true\") == 0) {\n"),
		    emit(Fd, "        oe_rec->_u.~s = 1;\n",[ic_forms:get_id2(I)]), 
		    emit(Fd, "      } else {\n"),
		    ?emit_c_dec_rpt(Fd, "        ", "boolean failure", []),
		    emit(Fd, "        return -1;\n  }\n"),
		    emit(Fd, "    }\n");
		{char,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "char:ei_decode_char", []),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{octet,_} ->
		    emit(Fd, "    if ((oe_error_code = ei_decode_char(oe_env->_inbuf, &oe_env->_iin, &oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{string,_} ->
		    emit(Fd, "    {\n"),
		    emit(Fd, "      int oe_type = 0;\n"),
		    emit(Fd, "      int oe_string_ctr = 0;\n\n"),

		    emit(Fd, "      (int) ei_get_type(oe_env->_inbuf, &oe_env->_iin, &oe_type, &oe_string_ctr);\n\n"),

		    emit(Fd, "      oe_rec->_u.~s = (void *) (oe_first + *oe_index);\n\n",[ic_forms:get_id2(I)]),

		    emit(Fd, "      if ((oe_error_code = ei_decode_string(oe_env->_inbuf, &oe_env->_iin, oe_rec->_u.~s)) < 0) {\n",
			 [ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "        ", "ei_decode_string", []),
		    emit(Fd, "        return oe_error_code;\n      }\n"),

		    emit(Fd, "      *oe_index = ~s;\n",[ic_util:mk_align("*oe_index+oe_string_ctr+1")]),
		    emit(Fd, "    }\n");
		{sequence,_,_} ->
		    SeqName = ic_util:to_undersc([ic_forms:get_id2(I), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
			 [SeqName,ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "sequence:oe_decode_~s", [SeqName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{struct,SID,_,_} ->
		    StructName = ic_util:to_undersc([ic_forms:get_id2(SID), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
			 [StructName,ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "struct:oe_decode_~s", [StructName]),
		    emit(Fd, "      return oe_error_code;\n    }\n");
		{union,UID,_,_,_} ->
		    UnionName = ic_util:to_undersc([ic_forms:get_id2(UID), ic_forms:get_id2(X) | N]),
		    emit(Fd, "    if ((oe_error_code = oe_decode_~s(oe_env, oe_first, oe_index, &oe_rec->_u.~s)) < 0) {\n",
			 [UnionName,ic_forms:get_id2(I)]),
		    ?emit_c_dec_rpt(Fd, "      ", "union:oe_decode_~s", [UnionName]),
		    emit(Fd, "      return oe_error_code;\n    }");
		_ ->
		    ic_error:fatal_error(G, {illegal_typecode_for_c, T, N})
	    end
    end.

mvDefaultToTail(CDclL) ->
    mvDefaultToTail(CDclL,[],[]).


mvDefaultToTail([], F, FD) ->
    lists:reverse(F) ++ FD;
mvDefaultToTail([{case_dcl,CaseList,I,T}|Rest], Found, FoundDefault) ->
    case lists:keysearch(default, 1, CaseList) of
	{value,Default} ->
	    NewCaseList = lists:delete(Default, CaseList) ++ [Default],
	    mvDefaultToTail(Rest, Found, [{case_dcl,NewCaseList,I,T}|FoundDefault]);
	false ->
	    mvDefaultToTail(Rest, [{case_dcl,CaseList,I,T}|Found], FoundDefault)
    end.


