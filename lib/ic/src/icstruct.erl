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
-module(icstruct).


-export([struct_gen/4, except_gen/4, create_c_array_coding_file/5]).

%%------------------------------------------------------------
%%
%% Internal stuff
%%
%%------------------------------------------------------------
-import(ic_codegen, [emit/2, emit/3, emit/4, emit_c_enc_rpt/4, emit_c_dec_rpt/4]).

-include("icforms.hrl").
-include("ic.hrl").



%%------------------------------------------------------------

%%------------------------------------------------------------
%%
%% File handling stuff
%%
%%------------------------------------------------------------



%%------------------------------------------------------------
%%
%% Generation loop
%%
%%	The idea is to traverse everything and find every struct that
%%	may be hiding down in nested types. All structs that are found
%%	are generated to a hrl file.
%%
%%	struct_gen is entry point for structs and types, except_gen is
%%	for exceptions
%%
%%------------------------------------------------------------


except_gen(G, N, X, L) when is_record(X, except) ->
    N2 = [ic_forms:get_id2(X) | N],
    if
	L == c ->
	    io:format("Warning : Exception not defined for c mapping\n", []);
	true ->
	    emit_struct(G, N, X, L)
    end,
    struct_gen_list(G, N2, ic_forms:get_body(X), L).

struct_gen(G, N, X, L) when is_record(X, struct) ->
    N2 = [ic_forms:get_id2(X) | N],
    struct_gen_list(G, N2, ic_forms:get_body(X), L),
    emit_struct(G, N, X, L);
struct_gen(G, N, X, L) when is_record(X, union) ->
    N2 = [ic_forms:get_id2(X) | N],
    if
	L == c ->
	    %% Produce the "body" first
	    struct_gen_list(G, N2, ic_forms:get_body(X), L), 
	    icunion:union_gen(G, N, X, c);
	true ->
	    struct_gen(G, N, ic_forms:get_type(X), L),
	    struct_gen_list(G, N2, ic_forms:get_body(X), L)
    end,
    emit_union(G, N, X, L);
struct_gen(G, N, X, L) when is_record(X, member) ->
    struct_gen(G, N, ic_forms:get_type(X), L);
struct_gen(G, N, X, L) when is_record(X, typedef) ->
    struct_gen(G, N, ic_forms:get_body(X), L),
    emit_typedef(G, N, X, L);
struct_gen(G, N, X, L) when is_record(X, type_dcl) ->
    struct_gen_list(G, N, ic_forms:get_type(X), L);
struct_gen(G, N, X, L) when is_record(X, case_dcl) ->
    struct_gen(G, N, ic_forms:get_type(X), L);
struct_gen(G, N, X, L) when is_record(X, sequence) ->
    struct_gen(G, N, ic_forms:get_type(X), L),
    X;
struct_gen(G, N, X, L) when is_record(X, enum) -> 
    icenum:enum_gen(G, N, X, L);
struct_gen(_G, _N, _X, _L) -> 
    ok.

%% List clause for struct_gen
struct_gen_list(G, N, Xs, L) -> 
    lists:foreach(
      fun(X) ->
	      R = struct_gen(G, N, X, L),
	      if
		  L == c ->
		      if
			  is_record(R,sequence) ->
			      emit_sequence_head_def(G,N,X,R,L);
			  true ->
			      ok
		      end;
		  true ->
		      ok
	      end
      end, Xs).


%% emit primitive for structs.
emit_struct(G, N, X, erlang) ->
    case ic_genobj:is_hrlfile_open(G) of
        true ->
            %% Make a straight list of all member ids (this is a
            %% variant of flatten)
            EList = lists:map(
		      fun(XX) -> 
			      lists:map(
				fun(XXX) ->
					ic_util:to_atom(ic_forms:get_id2(XXX))
				end,
				ic_forms:get_idlist(XX))
		      end,
		      ic_forms:get_body(X)),
            ic_codegen:record(G, X, 
			      ic_util:to_undersc([ic_forms:get_id2(X) | N]), 
			      ictk:get_IR_ID(G, N, X), lists:flatten(EList)),
	    mkFileRecObj(G,N,X,erlang);
	false -> 
	    ok
    end;
emit_struct(G, N, X, c) ->

    N1 = [ic_forms:get_id2(X) | N],
    case ic_pragma:is_local(G,N1) of
	true ->
	    emit_c_struct(G, N, X,local);
	false ->
	    emit_c_struct(G, N, X,included)
    end.


emit_c_struct(_G, _N, _X, included) -> 
    %% Do not generate included types att all.
    ok;
emit_c_struct(G, N, X, local) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),

	    N1 = [ic_forms:get_id2(X) | N],
	    StructName = ic_util:to_undersc(N1),

	    %% Make a straight list of all member ids (this is a
	    %% variant of flatten)
	    M = lists:map(
		  fun(XX) -> 
			  lists:map(
			    fun(XXX) ->
				    if 
					is_record(XXX, array) ->
					    Type = ic_forms:get_type(XX),
					    Name = element(3,element(2,XXX)),
					    {_, _, StructTK, _} =
						ic_symtab:get_full_scoped_name(
						  G, 
						  N, 
						  ic_symtab:scoped_id_new(
						    ic_forms:get_id2(X))),
					    ArrayTK = 
						get_structelement_tk(StructTK,
								     Name),
					    Dim = extract_dim(ArrayTK),
					    %% emit array file
					    emit(Fd, "\n#ifndef __~s__\n",
						 [ic_util:to_uppercase(
						    StructName ++ "_" 
						    ++ Name)]),	
					    emit(Fd, "#define __~s__\n\n",
						 [ic_util:to_uppercase(
						    StructName ++ "_" 
						    ++ Name)]),
					    create_c_array_coding_file(
					      G, 
					      N,
					      {StructName ++ "_" ++ Name, Dim},
					      Type,
					      no_typedef),
					    emit(Fd, "\n#endif\n\n"),
					    {{Type, XXX}, 
					     ic_forms:get_id2(XXX)};
				       true ->
					    %% Ugly work around to fix the ETO
					    %% return patch problem
					    Name = 
						case ic_forms:get_id2(XXX) of
						    "return" ->
							"return1";
						    Other ->
							Other
						end,
					    {ic_forms:get_type(XX), Name}
				    end
			    end,
			    ic_forms:get_idlist(XX))
		  end,
		  ic_forms:get_body(X)),
	    EList = lists:flatten(M),
	    %%io:format("Elist = ~p~n",[EList]),

	    emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(StructName)]),
	    emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(StructName)]),
	    ic_codegen:mcomment_light(Fd,
				      [io_lib:format("Struct definition: ~s",
						     [StructName])],
				      c),
	    emit(Fd, "typedef struct {\n"),
	    lists:foreach(
	      fun({Type, Name}) ->
		      emit_struct_member(Fd, G, N1, X, Name, Type)
	      end,
	      EList),
	    emit(Fd, "} ~s;\n\n", [StructName]),	
	    create_c_struct_coding_file(G, N, X, nil, StructName, 
					EList, struct),
	    emit(Fd, "\n#endif\n\n");
	false -> 
	    ok
    end.

%% Extracts array dimention(s)

get_structelement_tk({tk_struct, _, _, EList}, EN) ->
    {value, {EN, ArrayTK}} = lists:keysearch(EN, 1, EList),
    ArrayTK.

extract_dim({tk_array, {tk_array, T, D1}, D}) ->
    [integer_to_list(D) | extract_dim({tk_array, T, D1})];
extract_dim({tk_array, _, D}) ->
    [integer_to_list(D)].

%% Makes the array name
mk_array_name(Name,Dim) ->
    Name ++ mk_array_name(Dim).

mk_array_name([]) ->
    "";
mk_array_name([Dim|Dims]) ->
    "[" ++ Dim ++ "]" ++ mk_array_name(Dims).


emit_struct_member(Fd, G, N, X, Name,{Type,Array}) when is_record(Array, array)->
    {_, _, StructTK, _} = 
	ic_symtab:get_full_scoped_name(
	  G, 
	  N, 
	  ic_symtab:scoped_id_new(ic_forms:get_id2(X))),
    ArrayTK = get_structelement_tk(StructTK, Name),
    Dim = extract_dim(ArrayTK),
    emit(Fd, "   ~s ~s;\n",
	 [ic_cbe:mk_c_type(G, N, Type),mk_array_name(Name,Dim)]);
emit_struct_member(Fd, _G, N, _X, Name, Union) when is_record(Union, union)->
    emit(Fd, "   ~s ~s;\n",
	 [ic_util:to_undersc([ic_forms:get_id2(Union) | N]),Name]);
emit_struct_member(Fd, _G, _N, _X, Name, {string, _}) ->
    emit(Fd, "   CORBA_char *~s;\n",
	 [Name]);
emit_struct_member(Fd, _G, N, _X, Name, {sequence, _Type, _Length}) ->
    %% Sequence used as struct
    emit(Fd, "   ~s ~s;\n",
	 [ic_util:to_undersc([Name | N]), Name]);
emit_struct_member(Fd, G, N, X, Name, Type) 
  when element(1, Type) == scoped_id ->
    CType = ic_cbe:mk_c_type(G, N, Type, evaluate_not),
    emit_struct_member(Fd, G, N, X, Name, CType);
emit_struct_member(Fd, G, N, _X, Name, {enum, Type}) ->
    emit(Fd, "   ~s ~s;\n",
	 [ic_cbe:mk_c_type(G, N, Type),
	  Name]);
emit_struct_member(Fd, _G, _N, _X, Name, "ETERM*") ->
    emit(Fd, "   ETERM* ~s;\n",
	 [Name]);
emit_struct_member(Fd, _G, _N, _X, Name, Type) when is_list(Type) ->  
    emit(Fd, "   ~s ~s;\n",
	 [Type, Name]);
emit_struct_member(Fd, G, N, _X, Name, Type) ->
    emit(Fd, "   ~s ~s;\n",
	 [ic_cbe:mk_c_type(G, N, Type),
	  Name]).


emit_typedef(G, N, X, erlang) ->
    case X of
	{typedef,_,[{array,_,_}],_} -> %% Array but not a typedef of
	    %% an array definition
	    case ic_options:get_opt(G, be) of
		noc ->
		    mkFileArrObj(G,N,X,erlang);
		_ ->
		    %% Search the table to see if the type is local or
		    %% inherited.
		    PTab = ic_genobj:pragmatab(G),
		    Id = ic_forms:get_id2(X),
		    case ets:match(PTab,{file_data_local,'_','_',
					 typedef,N,Id,
					 ic_util:to_undersc([Id | N]),
					 '_','_'}) of
			[[]] ->
			    %% Local, create erlang file for the array
			    mkFileArrObj(G,N,X,erlang);
			_ ->
			    %% Inherited, do nothing
			    ok
		    end
	    end;

	{typedef,{sequence,_,_},_,{tk_sequence,_,_}} -> 
	    %% Sequence but not a typedef of
	    %% a typedef of a sequence definition
	    case ic_options:get_opt(G, be) of
		noc ->
		    mkFileRecObj(G,N,X,erlang);
		_ ->
		    %% Search the table to see if the type is local or
		    %% inherited.
		    PTab = ic_genobj:pragmatab(G),
		    Id = ic_forms:get_id2(X),
		    case ets:match(PTab,{file_data_local,'_','_',typedef,
					 N,Id,
					 ic_util:to_undersc([Id | N]),
					 '_','_'}) of
			[[]] ->
			    %% Local, create erlang file for the sequence
			    mkFileRecObj(G,N,X,erlang);
			_ ->
			    %% Inherited, do nothing
			    ok
		    end
	    end;
	_ ->
	    ok
    end;
emit_typedef(G, N, X, c) ->
    B = ic_forms:get_body(X),
    if
	is_record(B, sequence) ->
	    emit_sequence_head_def(G, N, X, B, c);
	true ->
	    lists:foreach(fun(D) ->
				  emit_typedef(G, N, D, B, c)
			  end, 
			  ic_forms:get_idlist(X))
    end.

emit_typedef(G, N, D, Type, c) when is_record(D, array) ->
    emit_array(G, N, D, Type);
emit_typedef(G, N, D, Type, c)  ->
    Name = ic_util:to_undersc([ic_forms:get_id2(D) | N]),
    CType = ic_cbe:mk_c_type(G, N, Type),
    TDType = mk_base_type(G, N, Type),
    ic_code:insert_typedef(G, Name, TDType),
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(Name)]),
	    emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(Name)]),
	    ic_codegen:mcomment_light(Fd,
				      [io_lib:format("Type definition ~s "
						     "for type  ~s",
						     [Name, CType])],
				      c),
	    emit(Fd, "typedef ~s ~s;\n",
		 [CType, Name]),
	    emit(Fd, "\n#endif\n\n"),
	    ic_codegen:nl(Fd);
	false ->
	    ok
    end.


mk_base_type(G, N, S) when element(1, S) == scoped_id ->
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
mk_base_type(_G, _N, S) ->
    S.

emit_array(G, N, D, Type) -> 
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    Name = ic_util:to_undersc([ic_forms:get_id2(D) | N]),
	    {_, _, ArrayTK, _} = 
		ic_symtab:get_full_scoped_name(G, N, 
					       ic_symtab:scoped_id_new(
						 ic_forms:get_id(D))),
	    Dim = extract_dim(ArrayTK),
	    CType = ic_cbe:mk_c_type(G, N, Type),
	    emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(Name)]),
	    emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(Name)]),
	    ic_codegen:mcomment_light(Fd,
				      [io_lib:format("Array definition ~s "
						     "for type  ~s",
						     [Name, CType])],
				      c),
	    emit(Fd, "typedef ~s ~s~s;\n",
		 [CType, Name, ic_cbe:mk_dim(Dim)]),
	    emit(Fd, "typedef ~s ~s_slice~s;\n",
		 [CType, Name, ic_cbe:mk_slice_dim(Dim)]),
	    ic_codegen:nl(Fd),
	    create_c_array_coding_file(G, N, {Name, Dim}, Type, typedef),
	    emit(Fd, "\n#endif\n\n");
	false ->
	    ok
    end.

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



create_c_array_coding_file(G, N, {Name, Dim}, Type, TypeDefFlag) ->

    {Fd , SName} = open_c_coding_file(G, Name), 
    HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    HrlFName = filename:basename(ic_genobj:include_file(G)),
    ic_codegen:emit_stub_head(G, Fd, SName, c),
    emit(Fd, "#include \"~s\"\n\n",[HrlFName]),

    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%  Fd = ic_genobj:stubfiled(G), %% Write on stubfile
    %%  HFd = ic_genobj:hrlfiled(G), %% Write on stubfile header
    %%  HrlFName = filename:basename(ic_genobj:include_file(G)),
    %%  emit(Fd, "#include \"~s\"\n\n",[HrlFName]),
    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
	 [ic_util:mk_oe_name(G, "sizecalc_"), Name]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, "
	 "int* oe_size) {\n", [ic_util:mk_oe_name(G, "sizecalc_"), Name]),

    emit(Fd, "  int oe_malloc_size = 0;\n",[]),
    emit(Fd, "  int oe_error_code = 0;\n",[]),
    emit(Fd, "  int oe_type = 0;\n",[]),
    emit(Fd, "  int oe_array_size = 0;\n",[]),

    {ok, RamFd} = ram_file:open([], [binary, write]),

    emit_sizecount(array, G, N, nil, RamFd, {Name, Dim}, Type),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data} = ram_file:get_file(RamFd),
    emit(Fd, Data),
    ram_file:close(RamFd),

    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n",[]),

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    RefStr = get_refStr(Dim),

    case TypeDefFlag of
	typedef ->
	    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s);\n",
		 [ic_util:mk_oe_name(G, "encode_"), Name, Name]),

	    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec) {\n",
		 [ic_util:mk_oe_name(G, "encode_"), Name, Name]);
	no_typedef ->

	    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec~s);\n",
		 [ic_util:mk_oe_name(G, "encode_"), 
		  Name,  
		  ic_cbe:mk_c_type(G, N, Type),
		  RefStr]),

	    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec~s) {\n",
		 [ic_util:mk_oe_name(G, "encode_"), 
		  Name,
		  ic_cbe:mk_c_type(G, N, Type),
		  RefStr])
    end,

    emit(Fd, "  int oe_error_code = 0;\n",[]),

    {ok, RamFd1} = ram_file:open([], [binary, write]),

    case TypeDefFlag of
	typedef ->
	    emit_encode(array, G, N, nil, RamFd1, {Name, Dim}, Type);
	no_typedef ->
	    emit_encode(array_no_typedef, G, N, nil, RamFd1, {Name, Dim}, Type)
    end,

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data1} = ram_file:get_file(RamFd1),
    emit(Fd, Data1),
    ram_file:close(RamFd1),

    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n",[]),

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    case TypeDefFlag of
	typedef ->
	    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, "
		 "int*, ~s);\n",
		 [ic_util:mk_oe_name(G, "decode_"), Name, Name]), 

	    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, "
		 "int* oe_outindex, ~s oe_out) {\n",
		 [ic_util:mk_oe_name(G, "decode_"), Name, Name]);
	no_typedef ->
	    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, "
		 "~s oe_rec~s);\n",
		 [ic_util:mk_oe_name(G, "decode_"), 
		  Name, 
		  ic_cbe:mk_c_type(G, N, Type),
		  RefStr]), 

	    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, "
		 "int* oe_outindex, ~s oe_out~s) {\n",
		 [ic_util:mk_oe_name(G, "decode_"), 
		  Name, 
		  ic_cbe:mk_c_type(G, N, Type),
		  RefStr])
    end,

    emit(Fd, "  int oe_error_code = 0;\n",[]),
    emit(Fd, "  int oe_array_size = 0;\n",[]),

    {ok, RamFd2} = ram_file:open([], [binary, write]),

    case TypeDefFlag of
	typedef ->
	    emit_decode(array, G, N, nil, RamFd2, {Name, Dim}, Type);
	no_typedef ->
	    emit_decode(array_no_typedef, G, N, nil, RamFd2, {Name, Dim}, Type)
    end,


    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data2} = ram_file:get_file(RamFd2),
    emit(Fd, Data2),
    ram_file:close(RamFd2),

    emit(Fd, "  *oe_outindex = ~s;\n\n",[align("*oe_outindex")]),

    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n",[]),
    file:close(Fd).


get_refStr([]) ->
    "";
get_refStr([X|Xs]) ->
    "[" ++ X ++ "]" ++ get_refStr(Xs).


emit_sequence_head_def(G, N, X, T, c) ->
    %% T is the sequence 
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    Fd = ic_genobj:hrlfiled(G),
	    SeqName = ic_util:to_undersc([ic_forms:get_id2(X) | N]),
	    emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(SeqName)]),
	    emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(SeqName)]),
	    ic_codegen:mcomment_light(Fd,
				      [io_lib:format("Struct definition:  ~s",
						     [SeqName])],
				      c),
	    emit(Fd, "typedef struct {\n"),
	    emit(Fd, "  CORBA_unsigned_long _maximum;\n"),
	    emit(Fd, "  CORBA_unsigned_long _length;\n"),
	    emit_seq_buffer(Fd, G, N, T#sequence.type),
	    emit(Fd, "} ~s;\n\n", [SeqName]),
	    create_c_struct_coding_file(G, N, X, T, SeqName, 
					T#sequence.type, sequence_head),
	    emit(Fd, "\n#endif\n\n");

	false ->
	    ok
    end.

emit_seq_buffer(Fd, G, N, Type) ->
    emit(Fd, "  ~s* _buffer;\n",
	 [ic_cbe:mk_c_type(G, N, Type)]).

%%------------------------------------------------------------
%%
%% Emit decode bodies for functions in C for array, sequences and
%% structs.
%%
%%------------------------------------------------------------
emit_decode(array, G, N, _T, Fd, {_Name, Dim}, Type) ->
    emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = 
	lists:concat(["*oe_outindex + ", dim_multiplication(Dim),
		      " * sizeof(", ic_cbe:mk_c_type(G, N, Type),")"]),
    emit(Fd, "    *oe_outindex = ~s;\n\n",[align(AlignName)]),
    array_decode_dimension_loop(G, N, Fd, Dim, "", Type, array);
emit_decode(array_no_typedef, G, N, _T, Fd, {_Name, Dim}, Type) ->
    emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = 
	lists:concat(["*oe_outindex + ", dim_multiplication(Dim),
		      " * sizeof(", ic_cbe:mk_c_type(G, N, Type),")"]),
    emit(Fd, "    *oe_outindex = ~s;\n\n",[align(AlignName)]),
    array_decode_dimension_loop(G, N, Fd, Dim, "", Type, array_no_typedef);
emit_decode(sequence_head, G, N, T, Fd, SeqName, ElType) ->
    ic_cbe:store_tmp_decl("  int oe_seq_len = 0;\n", []),
    ic_cbe:store_tmp_decl("  int oe_seq_count = 0;\n", []),
    ic_cbe:store_tmp_decl("  int oe_seq_dummy = 0;\n", []),

    TmpBuf = 
	case ictype:isBasicTypeOrEterm(G, N, ElType) of
	    true ->
		Tmp = "oe_seq_tmpbuf",
		ic_cbe:store_tmp_decl("  char* ~s = 0;\n", [Tmp]),
		Tmp;
	    false ->
		"NOT USED"
	end,

    MaxSize = get_seq_max(T),
    emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    emit(Fd, "    *oe_outindex = ~s;\n\n",
	 [align(["*oe_outindex + sizeof(", SeqName, ")"])]),

    Ctype = ic_cbe:mk_c_type(G, N, ElType),
    emit(Fd, "  if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, "
	 "&oe_env->_iin, &oe_seq_len)) < 0) {\n"),
    case ictype:isBasicTypeOrEterm(G, N, ElType) of
	true ->
	    emit(Fd, "    int oe_type = 0;\n"),
	    emit(Fd, "    (int) ei_get_type(oe_env->_inbuf, &oe_env->_iin, "
		 "&oe_type, &oe_seq_len);\n\n"),

	    if 
		MaxSize == infinity ->
		    ok;
		true ->
		    emit(Fd, "  if (oe_seq_len > ~w) {\n", [MaxSize]),
		    emit(Fd, "    CORBA_exc_set(oe_env, "
			 "CORBA_SYSTEM_EXCEPTION, DATA_CONVERSION, "
			 "\"Length of sequence `~s' out of bound\");\n"
			 "    return -1;\n  }\n", [SeqName])
	    end,
	    emit(Fd, "    oe_out->_maximum = oe_seq_len;\n"),
	    emit(Fd, "    oe_out->_length = oe_seq_len;\n"),
	    emit(Fd, "    oe_out->_buffer = (void *) (oe_first + "
		 "*oe_outindex);\n"),
	    emit(Fd, "    *oe_outindex = ~s;\n",
		 [align(["*oe_outindex + (sizeof(", Ctype, ") * "
			 "oe_out->_length)"])]),
	    emit(Fd, 
		 "    if ((~s = malloc(oe_seq_len + 1)) == NULL) {\n" 
		 "      CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "NO_MEMORY, \"Cannot malloc\");\n" 
		 "      return -1;\n"
		 "    }\n", [TmpBuf]),
	    emit(Fd, "    if ((oe_error_code = ei_decode_string("
		 "oe_env->_inbuf, &oe_env->_iin, ~s)) < 0) {\n", [TmpBuf]),
	    emit(Fd, "      CORBA_free(~s);\n\n", [TmpBuf]),
	    emit_c_dec_rpt(Fd, "      ", "string1", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),
	    emit(Fd, "    for (oe_seq_count = 0; "
		 "oe_seq_count < oe_out->_length; oe_seq_count++)\n"), 
	    case ictype:isBasicType(G, N, ElType) of
		true -> 
		    emit(Fd, "      oe_out->_buffer[oe_seq_count] = (unsigned char) "
			 "~s[oe_seq_count];\n\n", [TmpBuf]);
		false -> %% Term
		    emit(Fd, "      oe_out->_buffer[oe_seq_count] = "
			 "erl_mk_int(~s[oe_seq_count]);\n\n",[TmpBuf]) % XXXX What?
	    end,
	    emit(Fd, "    CORBA_free(~s);\n\n", [TmpBuf]);
	false ->
	    emit(Fd, "    return oe_error_code;\n")
    end,

    emit(Fd, "  } else {\n"),

    if 
	MaxSize == infinity ->
	    ok;
	true ->
	    emit(Fd, "    if (oe_seq_len > ~w) {\n", [MaxSize]),
	    emit(Fd, "      CORBA_exc_set(oe_env, "
		 "CORBA_SYSTEM_EXCEPTION, DATA_CONVERSION, "
		 "\"Length of sequence `~s' out of bound\");\n"
		 "      return -1;\n  }\n", [SeqName])
    end,

    emit(Fd, "    oe_out->_maximum = oe_seq_len;\n"),
    emit(Fd, "    oe_out->_length = oe_seq_len;\n"),
    emit(Fd, "    oe_out->_buffer = (void *) (oe_first + *oe_outindex);\n"),
    emit(Fd, "    *oe_outindex = ~s;\n\n",
	 [align(["*oe_outindex + (sizeof(", Ctype, ") * oe_out->_length)"])]),

    if
	Ctype == "CORBA_char *" ->
	    emit(Fd, "    for (oe_seq_count = 0; "
		 "oe_seq_count < oe_out->_length; oe_seq_count++) {\n"),
	    emit(Fd, "      oe_out->_buffer[oe_seq_count] = "
		 "(void*) (oe_first + *oe_outindex);\n\n"),
	    ic_cbe:emit_decoding_stmt(G, N, Fd, ElType, 
				      "oe_out->_buffer[oe_seq_count]", 
				      "", 
				      "oe_env->_inbuf", 0, "", caller_dyn),
            emit(Fd, "      *oe_outindex = ~s;",
		 [align(["*oe_outindex + strlen(oe_out->_buffer["
			 "oe_seq_count]) + 1"])]);
	true ->
	    emit(Fd, "    for (oe_seq_count = 0; "
		 "oe_seq_count < oe_out->_length; oe_seq_count++) {\n"), 
	    case ictype:isArray(G, N, ElType) of
		%% XXX Silly. There is no real difference between the
		%% C statements produced by the following calls.  
		true ->
		    ic_cbe:emit_decoding_stmt(G, N, Fd, ElType, 
					      "oe_out->_buffer[oe_seq_count]",
					      "", 
					      "oe_env->_inbuf", 
					      0, "oe_outindex", generator);
		false ->
		    ic_cbe:emit_decoding_stmt(G, N, Fd, ElType, 
					      "oe_out->_buffer + oe_seq_count",
					      "", 
					      "oe_env->_inbuf", 
					      0, "oe_outindex", generator)
	    end
    end,
    emit(Fd, "    }\n"),
    emit(Fd, "    if (oe_out->_length != 0) {\n"),
    emit(Fd, "      if ((oe_error_code = ei_decode_list_header("
	 "oe_env->_inbuf, &oe_env->_iin, &oe_seq_dummy)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "        ", "ei_decode_list_header", []),
    emit(Fd, "        return oe_error_code;\n      }\n"),
    emit(Fd, "    } else\n"),
    emit(Fd, "        oe_out->_buffer = NULL;\n"),
    emit(Fd, "  }\n");

emit_decode(struct, G, N, _T, Fd, StructName, ElTypes) ->
    Length = length(ElTypes) + 1,
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    Tname1 = ic_cbe:mk_variable_name(op_variable_count),

    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),
    ic_cbe:store_tmp_decl("  char ~s[256];\n\n",[Tname1]),

    emit(Fd, "  if((char*) oe_out == oe_first)\n",[]),
    AlignName = lists:concat(["*oe_outindex + sizeof(",StructName,")"]),
    emit(Fd, "    *oe_outindex = ~s;\n\n", [align(AlignName)]),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	 "&oe_env->_iin, &~s)) < 0) {\n", [Tname]),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  if (~s != ~p) {\n",[Tname, Length]),
    emit_c_dec_rpt(Fd, "      ", "tuple header size != ~p", [Length]),
    emit(Fd, "    return -1;\n  }\n"),

    emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, "
	 "&oe_env->_iin, ~s)) < 0) {\n", [Tname1]),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),
    emit(Fd, "  if (strcmp(~s, ~p) != 0)\n",[Tname1, StructName]),
    emit(Fd, "    return -1;\n\n"),
    lists:foreach(
      fun({ET, EN}) ->
	      case ic_cbe:is_variable_size(G, N, ET) of
		  true ->
		      case ET of

			  {struct, _, _, _} ->
			      %% Sequence member = a struct
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							ic_forms:get_id2(ET), 
							"&oe_out->" ++ EN,
							"", "oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);

			  {sequence, _, _} ->
			      %% Sequence member = a struct XXX ??
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							EN, 
							"&oe_out->" ++ EN,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);
			  {_,{array, _, _}} ->
			      emit(Fd, "  oe_out->~s = (void *) "
				   "(oe_first+*oe_outindex);\n\n",[EN]),
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							EN, "oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);

			  {union, _, _, _, _} ->
			      %% Sequence member = a union
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							ic_forms:get_id2(ET), 
							"&oe_out->" ++ EN,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);

			  {string,_} -> 
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							ET, 
							"oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator_malloc);

			  {scoped_id,_,_,_} ->
			      case ictype:member2type(G,StructName,EN) of
				  array ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"oe_out->" ++ 
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);
				  struct ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"&oe_out->" ++
								EN ,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);
				  sequence ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"&oe_out->" ++
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);
				  union ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"&oe_out->" ++
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);
				  _ ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"oe_out->" ++ 
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator)
			      end;

			  _ ->
			      emit(Fd, "  oe_out->~s = (void *) "
				   "(oe_first+*oe_outindex);\n\n",[EN]),
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							ET, 
							"oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, "oe_outindex", 
							generator)
		      end;
		  false ->
		      case ET of

			  {struct, _, _, _} ->
			      %% A struct member
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							ic_forms:get_id2(ET), 
							"&oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);

			  {_,{array, _, _}} ->
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							EN, 
							"oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);

			  {union, _, _, _, _} ->
			      %% Sequence member = a union
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							StructName ++ "_" ++ 
							ic_forms:get_id2(ET), 
							"&oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);

			  {_,_} ->
			      ic_cbe:emit_decoding_stmt(G, N, Fd, 
							ET, 
							"&oe_out->" ++ EN ,
							"", 
							"oe_env->_inbuf", 
							0, 
							"oe_outindex", 
							generator);
			  {scoped_id,_,_,_} ->
			      case ic_symtab:get_full_scoped_name(G, N, ET) of
				  {_FullScopedName, _, {tk_array,_,_}, _} ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"oe_out->" ++
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);
				  {_FullScopedName, _, {tk_string,_}, _} ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"oe_out->" ++ 
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);
				  {_FullScopedName, _, {tk_struct,_,_,_}, _} ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"&oe_out->" ++
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);

				  {_FullScopedName, _, 
				   {tk_union,_,_,_,_,_}, _} ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"&oe_out->" ++
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator);

				  _ ->
				      ic_cbe:emit_decoding_stmt(G, N, Fd, 
								ET, 
								"&oe_out->" ++
								EN,
								"", 
								"oe_env->"
								"_inbuf", 
								0, 
								"oe_outindex",
								generator)
			      end
		      end
	      end
      end,
      ElTypes).


ref_array_static_dec(array, true) -> 
    %% Typedef, Static, Basic Type
    "&(oe_out)";
ref_array_static_dec(array, false) -> 
    %% Typedef, Static, Constr Type
    "&(oe_out)";
ref_array_static_dec(array_no_typedef, true) -> 
    %% No Typedef, Static, Basic Type
    "&oe_out";
ref_array_static_dec(array_no_typedef, false) -> 
    %% No Typedef, Static, Constr Type
    "&oe_out".


ref_array_dynamic_dec(G, N, T, array) ->  
    case ictype:isString(G, N, T) of 
	true ->   % Typedef, Dynamic, String
	    "oe_out";
	false ->  % Typedef, Dynamic, No String
	    "&(oe_out)"
    end;
ref_array_dynamic_dec(G, N, T, array_no_typedef) -> 
    case ictype:isString(G, N, T) of
	true ->   % No Typedef, Dynamic, String
	    "oe_out";
	false ->  % No Typedef, Dynamic, No String
	    "&oe_out"
    end.



array_decode_dimension_loop(G, N, Fd, [Dim], Dimstr, Type, TDFlag) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	 "&oe_env->_iin, &oe_array_size)) < 0) {\n",
	 []),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    %% This is disabled due to a bug in erl_interface :
    %% tuples inside tuples hae no correct data about the size 
    %% of the tuple........( allways = 0 )
    %%emit(Fd, "  if (oe_array_size != ~s)\n",[Dim]),
    %%emit(Fd, "    return -1;\n\n"),

    emit(Fd, "  for (~s = 0; ~s < ~s; ~s++) {\n",
	 [Tname, Tname, Dim, Tname]),


    ArrAccess = 
	case ic_cbe:is_variable_size(G, N, Type) of
	    true ->
		ref_array_dynamic_dec(G, N, Type, TDFlag) ++ 
		    Dimstr ++ "[" ++ Tname ++ "]";
	    false ->
		ref_array_static_dec(TDFlag, ictype:isBasicType(G,N,Type)) ++
		    Dimstr ++ "[" ++ Tname ++ "]"
	end,

    ic_cbe:emit_decoding_stmt(G, N, Fd, Type,
			      ArrAccess,
			      "", "oe_env->_inbuf", 0,
			      "oe_outindex", generator),

    %%  emit(Fd, "\n *oe_outindex +=
    %%  sizeof(~s);\n",[ic_cbe:mk_c_type(G, N, Type)]),
    emit(Fd, "  }\n");
array_decode_dimension_loop(G, N, Fd, [Dim | Ds], _Dimstr, Type, TDFlag) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	 "&oe_env->_iin, &oe_array_size)) < 0) {\n",
	 []),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    %% This is disabled due to a bug in erl_interface :
    %% tuples inside tuples hae no correct data about the size 
    %% of the tuple........( allways = 0 )
    %%emit(Fd, "  if (oe_array_size != ~s)\n",[Dim]),
    %%emit(Fd, "    return -1;\n\n"),

    emit(Fd, "  for (~s = 0; ~s < ~s; ~s++) {\n",
	 [Tname, Tname, Dim, Tname]),
    array_decode_dimension_loop(G, N, Fd, Ds, "[" ++ Tname ++ "]" , Type, 
				TDFlag),

    emit(Fd, "  }\n").

dim_multiplication([D]) ->
    D;
dim_multiplication([D |Ds]) ->
    D ++ "*" ++ dim_multiplication(Ds).

emit_encode(array, G, N, _T, Fd, {_Name, Dim}, Type) ->
    array_encode_dimension_loop(G, N, Fd, Dim, {"",""}, Type, array);
emit_encode(array_no_typedef, G, N, _T, Fd, {_Name, Dim}, Type) ->
    array_encode_dimension_loop(G, N, Fd, Dim, {"",""}, Type, 
				array_no_typedef);
emit_encode(sequence_head, G, N, T, Fd, SeqName, ElType) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n\n",[Tname]),

    MaxSize = get_seq_max(T),
    if 
	MaxSize == infinity ->
	    ok;
	true ->
	    emit(Fd, "  if (oe_rec->_length > ~w) {\n", [MaxSize]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "DATA_CONVERSION, \"Length of sequence `~s' "
		 "out of bound\");\n"
		 "    return -1;\n  }\n", [SeqName])
    end,

    emit(Fd, "  if (oe_rec->_length != 0) {\n"),

    emit(Fd, "    if ((oe_error_code = oe_ei_encode_list_header(oe_env, "
	 "oe_rec->_length)) < 0) {\n",
	 []),
    emit_c_enc_rpt(Fd, "      ", "oi_ei_encode_list_header", []),
    emit(Fd, "      return oe_error_code;\n    }\n"),

    emit(Fd, "    for (~s = 0; ~s < oe_rec->_length; ~s++) {\n",
	 [Tname, Tname, Tname]),
    case ElType of 
	{_,_} -> %% ElType = elementary type or pointer type
	    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType, "oe_rec->_buffer[" ++
				      Tname ++ "]", "oe_env->_outbuf");

	{scoped_id,local,_,["term","erlang"]} ->
	    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType, "oe_rec->_buffer[" ++
				      Tname ++ "]", "oe_env->_outbuf");

	{scoped_id,_,_,_} ->
	    case ic_symtab:get_full_scoped_name(G, N, ElType) of
		{_, typedef, TDef, _} ->
		    case TDef of
			{tk_struct,_,_,_} ->
			    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
						      "&oe_rec->_buffer[" ++ 
						      Tname ++ "]", 
						      "oe_env->_outbuf");
			{tk_sequence,_,_} ->
			    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
						      "&oe_rec->_buffer[" ++
						      Tname ++ "]", 
						      "oe_env->_outbuf");
			{tk_union,_,_,_,_,_} ->
			    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
						      "&oe_rec->_buffer[" ++
						      Tname ++ "]", 
						      "oe_env->_outbuf");
			_ ->
			    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
						      "oe_rec->_buffer[" ++ 
						      Tname ++ "]", 
						      "oe_env->_outbuf") 
		    end;
		{_,enum,_,_} ->
		    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
					      "oe_rec->_buffer[" ++ 
					      Tname ++ "]", 
					      "oe_env->_outbuf");
		_ ->
		    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
					      "&oe_rec->_buffer[" ++ 
					      Tname ++ "]", 
					      "oe_env->_outbuf")
	    end;

	_ ->     %% ElType = structure 
	    ic_cbe:emit_encoding_stmt(G, N, Fd, ElType,
				      "&oe_rec->_buffer[" ++ Tname ++ "]", 
				      "oe_env->_outbuf")
    end,
    emit(Fd, "    }\n"),
    emit(Fd, "  }\n"),
    emit(Fd, "  if ((oe_error_code = oe_ei_encode_empty_list(oe_env)) < 0) {\n"),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_empty_list", []),
    emit(Fd, "    return oe_error_code;\n  }\n");
emit_encode(struct, G, N, _T, Fd, StructName, ElTypes) ->
    Length = length(ElTypes) + 1,
    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_tuple_header(oe_env, ~p)) < 0) {\n", [Length]),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),
    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_atom(oe_env, ~p)) < 0) {\n", [StructName]),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_atom", []),
    emit(Fd, "    return oe_error_code;\n    }\n"),
    lists:foreach(
      fun({ET, EN}) -> 
	      case ET of
		  {sequence, _, _} ->
		      %% Sequence = struct
		      ic_cbe:emit_encoding_stmt(G, N, Fd, 
						StructName ++ "_" ++ EN, 
						"&oe_rec->" ++ EN,
						"oe_env->_outbuf");
		  {_,{array, _, _Dims}} ->
		      ic_cbe:emit_encoding_stmt(G, N, Fd, 
						StructName ++ "_" ++ EN, 
						"oe_rec->" ++ EN,
						"oe_env->_outbuf");

		  {union,_,_,_,_} ->
		      ic_cbe:emit_encoding_stmt(G, N, Fd, 
						StructName ++ "_" ++ 
						ic_forms:get_id2(ET), 
						"&oe_rec->" ++ EN,
						"oe_env->_outbuf");

		  {struct,_,_,_} ->
		      ic_cbe:emit_encoding_stmt(G, N, Fd, 
						StructName ++ "_" ++ 
						ic_forms:get_id2(ET), 
						"&oe_rec->" ++ EN,
						"oe_env->_outbuf");

		  {scoped_id,_,_,_} ->
		      case ictype:member2type(G,StructName,EN) of
			  struct ->
			      ic_cbe:emit_encoding_stmt(G, N, Fd, 
							ET, 
							"&oe_rec->" ++ EN, 
							"oe_env->_outbuf");
			  sequence ->
			      ic_cbe:emit_encoding_stmt(G, N, Fd, 
							ET, 
							"&oe_rec->" ++ EN, 
							"oe_env->_outbuf");
			  union ->
			      ic_cbe:emit_encoding_stmt(G, N, Fd, 
							ET, 
							"&oe_rec->" ++ EN, 
							"oe_env->_outbuf");
			  array ->
			      ic_cbe:emit_encoding_stmt(G, N, Fd, 
							ET, 
							"oe_rec->" ++ EN, 
							"oe_env->_outbuf");
			  _ ->
			      ic_cbe:emit_encoding_stmt(G, N, Fd, 
							ET, 
							"oe_rec->" ++ EN, 
							"oe_env->_outbuf")
		      end;
		  _ ->
		      ic_cbe:emit_encoding_stmt(G, N, Fd, 
						ET, 
						"oe_rec->" ++ EN, 
						"oe_env->_outbuf")
	      end
      end,
      ElTypes).

ref_array_static_enc(array, true) -> 
    %% Typedef, Static, Basic Type
    "oe_rec";
ref_array_static_enc(array, false) -> 
    %% Typedef, Static, Constr Type
    "&(oe_rec)"; 
ref_array_static_enc(array_no_typedef, true) -> 
    %% No Typedef, Static, Basic Type
    "oe_rec";
ref_array_static_enc(array_no_typedef, false) -> 
    %% No Typedef, Static, Constr Type
    "&oe_rec".


ref_array_dynamic_enc(G, N, T, array) -> 
    case ictype:isString(G, N, T) of
	true ->    % Typedef, Dynamic, String
	    "oe_rec";
	false ->   % Typedef, Dynamic, No String
	    "&(oe_rec)"
    end;
ref_array_dynamic_enc(G, N, T, array_no_typedef) -> 
    case ictype:isString(G, N, T) of
	true ->    % No Typedef, Dynamic, String
	    "oe_rec";
	false ->   % No Typedef, Dynamic, No String
	    "&oe_rec"
    end.



array_encode_dimension_loop(G, N, Fd, [Dim], {Str1,_Str2}, Type, TDFlag) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_tuple_header(oe_env, ~s)) < 0) {\n", [Dim]),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  for (~s = 0; ~s < ~s; ~s++) {\n",
	 [Tname, Tname, Dim, Tname]),

    ArrAccess = 
	case ic_cbe:is_variable_size(G, N, Type) of
	    true ->
		ref_array_dynamic_enc(G, N, Type, TDFlag) ++
		    Str1 ++ "[" ++ Tname ++ "]";
	    false ->
		ref_array_static_enc(TDFlag, ictype:isBasicType(G,N,Type)) ++
		    Str1 ++ "[" ++ Tname ++ "]"
	end,

    ic_cbe:emit_encoding_stmt(G, N, Fd, Type, ArrAccess, "oe_env->_outbuf"),
    emit(Fd, "  }\n");
array_encode_dimension_loop(G, N, Fd, [Dim | Ds],{Str1,Str2}, Type, TDFlag) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),

    emit(Fd, "  if ((oe_error_code = "
	 "oe_ei_encode_tuple_header(oe_env, ~s)) < 0) {\n", [Dim]),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  for (~s = 0; ~s < ~s; ~s++) {\n",
	 [Tname, Tname, Dim, Tname]),
    array_encode_dimension_loop(G, N, Fd, Ds,
				{Str1 ++ "[" ++ Tname ++ "]", Str2},
				Type, TDFlag),
    emit(Fd, "  }\n").


emit_sizecount(array, G, N, _T, Fd, {_Name, Dim}, Type) ->
    emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + ", dim_multiplication(Dim),
			      " * sizeof(", ic_cbe:mk_c_type(G, N, Type),")"]),
    emit(Fd, "    *oe_size = ~s;\n\n",[align(AlignName)]),
    array_size_dimension_loop(G, N, Fd, Dim, Type),
    emit(Fd, "  *oe_size = ~s;\n\n", 
	 [align("*oe_size + oe_malloc_size")]),
    ic_codegen:nl(Fd);

emit_sizecount(sequence_head, G, N, T, Fd, SeqName, ElType) ->
    ic_cbe:store_tmp_decl("  int oe_seq_len = 0;\n", []),
    ic_cbe:store_tmp_decl("  int oe_seq_count = 0;\n", []),

    emit(Fd, "  if(*oe_size == 0)\n",[]),
    emit(Fd, "    *oe_size = ~s;\n\n", 
	 [align(["*oe_size + sizeof(", SeqName, ")"])]),

    MaxSize = get_seq_max(T),

    emit(Fd, "  if ((oe_error_code = ei_get_type(oe_env->_inbuf, "
	 "oe_size_count_index, &oe_type, &oe_seq_len)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "    ", "ei_get_type", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    if 
	MaxSize == infinity ->
	    ok;
	true ->
	    emit(Fd, "  if (oe_seq_len > ~w) {\n", [MaxSize]),
	    emit(Fd, "    CORBA_exc_set(oe_env, CORBA_SYSTEM_EXCEPTION, "
		 "DATA_CONVERSION, \"Length of sequence `~s' "
		 "out of bound\");\n"
		 "    return -1;\n  }\n", [SeqName])
    end,

    CType = ic_cbe:mk_c_type(G, N, ElType),

    emit(Fd, "  if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf, "
	 "oe_size_count_index, NULL)) < 0) {\n"),

    case ictype:isBasicTypeOrEterm(G, N, ElType) of
	true ->
	    emit(Fd, "    if ((oe_error_code = ei_decode_string(oe_env->"
		 "_inbuf, oe_size_count_index, NULL)) < 0) {\n"),
	    emit_c_dec_rpt(Fd, "      ", "ei_decode_string", []),
	    emit(Fd, "      return oe_error_code;\n    }\n"),

	    emit(Fd, "    oe_malloc_size = ~s;\n\n",
		 [align(["sizeof(", CType, ") * oe_seq_len"])]);
	false ->
	    emit_c_dec_rpt(Fd, "    ", "non mea culpa", []),
	    emit(Fd, "    return oe_error_code;\n\n")
    end,

    emit(Fd, "  } else {\n"),

    emit(Fd, "    oe_malloc_size = ~s;\n\n",
	 [align(["sizeof(", CType, ") * oe_seq_len"])]),

    emit(Fd, "    for (oe_seq_count = 0; oe_seq_count < oe_seq_len; "
	 "oe_seq_count++) {\n"), 
    ic_cbe:emit_malloc_size_stmt(G, N, Fd, ElType,
				 "oe_env->_inbuf", 0, generator),
    emit(Fd, "    }\n"),

    emit(Fd, "    if (oe_seq_len != 0) \n"),
    emit(Fd, "      if ((oe_error_code = ei_decode_list_header(oe_env->_inbuf,"
	 "oe_size_count_index, NULL)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "      ", "ei_decode_list_header", []),
    emit(Fd, "        return oe_error_code;\n    }\n"),
    emit(Fd, "  }\n"),
    emit(Fd, "  *oe_size = ~s;\n\n", [align("*oe_size + oe_malloc_size")]);

emit_sizecount(struct, G, N, _T, Fd, StructName, ElTypes) ->
    Length = length(ElTypes) + 1,
    Tname = ic_cbe:mk_variable_name(op_variable_count),
    ic_cbe:store_tmp_decl("  int ~s = 0;\n\n",[Tname]),

    emit(Fd, "  if(*oe_size == 0)\n",[]),
    AlignName = lists:concat(["*oe_size + sizeof(",StructName,")"]),
    emit(Fd, "    *oe_size = ~s;\n\n", [align(AlignName)]),
    ic_codegen:nl(Fd),

    emit(Fd, "  if ((oe_error_code = "
	 "ei_get_type(oe_env->_inbuf, oe_size_count_index, &oe_type, "
	 "&~s)) < 0) {\n", [Tname]),
    emit_c_dec_rpt(Fd, "    ", "ei_get_type", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  if (~s != ~p) {\n",[Tname, Length]),
    emit_c_dec_rpt(Fd, "    ", "~s != ~p", [Tname, Length]),
    emit(Fd, "    return -1;\n  }\n"),


    emit(Fd, "  if ((oe_error_code = "
	 "ei_decode_tuple_header(oe_env->_inbuf, "
	 "oe_size_count_index, 0)) < 0) {\n"),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),
    emit(Fd, "  if ((oe_error_code = "
	 "ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n", []),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),
    lists:foreach(
      fun({ET, EN}) ->
	      case ic_cbe:is_variable_size(G, N, ET) of
		  true ->
		      case ET of
			  {sequence, _, _} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ EN,
				"oe_env->_inbuf", 
				0, 
				generator);
			  {_,{array, _, _}} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ EN,
				"oe_env->_inbuf", 
				0, 
				generator);
			  {union,_,_,_,_} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ ic_forms:get_id2(ET),
				"oe_env->_inbuf", 
				0, 
				generator);

			  {struct,_,_,_} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ ic_forms:get_id2(ET),
				"oe_env->_inbuf", 
				0, 
				generator);

			  _  ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				ET, 
				"oe_env->_inbuf", 
				0, 
				generator)
		      end;
		  false ->
		      case ET of
			  {_,{array, _, _}} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ EN,
				"oe_env->_inbuf", 
				0, 
				generator);

			  {union,_,_,_,_} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ ic_forms:get_id2(ET),
				"oe_env->_inbuf", 
				0, 
				generator);

			  {struct,_,_,_} ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				StructName ++ "_" ++ ic_forms:get_id2(ET),
				"oe_env->_inbuf", 
				0, 
				generator);
			  _  ->
			      ic_cbe:emit_malloc_size_stmt(
				G, N, Fd, 
				ET, 
				"oe_env->_inbuf", 
				1, 
				generator)
		      end
	      end
      end,
      ElTypes),

    emit(Fd, "  *oe_size = ~s;\n\n",
	 [align("*oe_size + oe_malloc_size")]).


array_size_dimension_loop(G, N, Fd, [Dim], Type) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),

    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),
    emit(Fd, "  if ((oe_error_code = "
	 "ei_get_type(oe_env->_inbuf, oe_size_count_index, "
	 "&oe_type, &oe_array_size)) < 0) {\n",
	 []),
    emit_c_dec_rpt(Fd, "    ", "ei_get_type", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  if (oe_array_size != ~s) {\n",[Dim]),
    emit_c_dec_rpt(Fd, "    ", "array size != ~s", [Dim]),
    emit(Fd, "    return -1;\n  }\n"),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	 "oe_size_count_index, 0)) < 0) {\n", []),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  for (~s = 0; ~s < ~s; ~s++) {\n",
	 [Tname, Tname, Dim, Tname]),
    ic_cbe:emit_malloc_size_stmt(G, N, Fd, 
				 Type, "oe_env->_inbuf", 0, generator),
    emit(Fd, "  }\n");
array_size_dimension_loop(G, N, Fd, [Dim | Ds], Type) ->
    Tname = ic_cbe:mk_variable_name(op_variable_count),

    ic_cbe:store_tmp_decl("  int ~s = 0;\n",[Tname]),
    emit(Fd, "  if ((oe_error_code = "
	 "ei_get_type(oe_env->_inbuf, oe_size_count_index, "
	 "&oe_type, &oe_array_size)) < 0) {\n", []),
    emit_c_dec_rpt(Fd, "    ", "ei_get_type", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  if (oe_array_size != ~s) {\n",[Dim]),
    emit_c_dec_rpt(Fd, "    ", "array size != ~s", [Dim]),
    emit(Fd, "    return -1;\n  }\n"),

    emit(Fd, "  if ((oe_error_code = ei_decode_tuple_header(oe_env->_inbuf, "
	 "oe_size_count_index, 0)) < 0) {\n",
	 []),
    emit_c_dec_rpt(Fd, "    ", "ei_decode_tuple_header", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  for (~s = 0; ~s < ~s; ~s++) {\n",
	 [Tname, Tname, Dim, Tname]),
    array_size_dimension_loop(G, N, Fd, Ds, Type),
    emit(Fd, "  }\n").


create_c_struct_coding_file(G, N, _X, T, StructName, ElTypes, StructType) ->
    
    {Fd , SName} = open_c_coding_file(G,  StructName), % stub file
    HFd = ic_genobj:hrlfiled(G),		% stub header file
    HrlFName = filename:basename(ic_genobj:include_file(G)),

    ic_codegen:emit_stub_head(G, Fd, SName, c),
    HrlFName = filename:basename(ic_genobj:include_file(G)),
    emit(Fd, "#include \"~s\"\n\n",[HrlFName]),

    %% Size count

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
	 [ic_util:mk_oe_name(G, "sizecalc_"), StructName]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, "
	 "int* oe_size_count_index, int* oe_size)\n{\n",
	 [ic_util:mk_oe_name(G, "sizecalc_"), StructName]),

    emit(Fd, "  int oe_malloc_size = 0;\n",[]),
    emit(Fd, "  int oe_error_code = 0;\n",[]),
    emit(Fd, "  int oe_type = 0;\n",[]), 

    {ok, RamFd} = ram_file:open([], [binary, write]),

    emit_sizecount(StructType, G, N, T, RamFd, StructName, ElTypes),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data} = ram_file:get_file(RamFd),
    emit(Fd, Data),
    ram_file:close(RamFd),

    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n\n",[]),

    %% Encode 

    put(op_variable_count, 0),
    put(tmp_declarations, []),


    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s*);\n",
	 [ic_util:mk_oe_name(G, "encode_"), StructName, StructName]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s* oe_rec)\n{\n",
	 [ic_util:mk_oe_name(G, "encode_"), StructName, StructName]),

    emit(Fd, "  int oe_error_code = 0;\n",[]),

    {ok, RamFd1} = ram_file:open([], [binary, write]),

    emit_encode(StructType, G, N, T, RamFd1, StructName, ElTypes),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data1} = ram_file:get_file(RamFd1),
    emit(Fd, Data1),
    ram_file:close(RamFd1),

    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n\n",[]),

    %% Decode

    put(op_variable_count, 0),
    put(tmp_declarations, []),

    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s *);\n",
	 [ic_util:mk_oe_name(G, "decode_"), StructName, StructName]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, "
	 "int* oe_outindex, "
	 "~s *oe_out)\n{\n",
	 [ic_util:mk_oe_name(G, "decode_"), StructName, StructName]),

    emit(Fd, "  int oe_error_code = 0;\n",[]),

    {ok, RamFd2} = ram_file:open([], [binary, write]),

    emit_decode(StructType, G, N, T, RamFd2, StructName, ElTypes),

    ic_cbe:emit_tmp_variables(Fd),
    ic_codegen:nl(Fd),
    %% Move data from ram file to output file.
    {ok, Data2} = ram_file:get_file(RamFd2),
    emit(Fd, Data2),
    ram_file:close(RamFd2),

    emit(Fd, "  *oe_outindex = ~s;\n",[align("*oe_outindex")]),
    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n\n",[]),
    file:close(Fd).


%%------------------------------------------------------------
%%
%% emit primitive for unions.
%%
%%------------------------------------------------------------
emit_union(G, N, X, erlang) ->
    case ic_genobj:is_hrlfile_open(G) of
        true ->
            ic_codegen:record(G, X, 
			      ic_util:to_undersc([ic_forms:get_id2(X) | N]),
			      nil,nil),
	    mkFileRecObj(G,N,X,erlang);
	false -> ok
    end;
emit_union(_G, _N, _X, c) -> %% Not supported in c backend
    true.


%%------------------------------------------------------------
%%
%% emit erlang modules for objects with record definitions
%% (such as unions or structs), or sequences 
%%
%% The record files, other than headers are only generated
%% for CORBA...... If wished an option could allows even
%% for other backends ( not necessary anyway )
%%
%%------------------------------------------------------------
mkFileRecObj(G,N,X,erlang) ->
    case ic_options:get_opt(G, be) of
	erl_corba ->
	    SName = 
		ic_util:to_undersc([ic_forms:get_id2(X) | N]),
	    FName =  
		ic_file:join(ic_options:get_opt(G, stubdir),
			     ic_file:add_dot_erl(SName)),

	    case file:open(FName, [write]) of
		{ok, Fd} ->
		    HrlFName = filename:basename(ic_genobj:include_file(G)),

		    ic_codegen:emit_stub_head(G, Fd, SName, erlang),
		    emit(Fd, "-include(~p).\n\n",[HrlFName]),
		    emit_exports(G,Fd),
		    emit_rec_methods(G,N,X,SName,Fd),  
		    ic_codegen:nl(Fd),
		    ic_codegen:nl(Fd),
		    file:close(Fd);
		Other -> 
		    exit(Other)
	    end;
	_ ->
	    true
    end.


%%------------------------------------------------------------
%%
%% emit erlang modules for objects with array definitions..
%%
%%------------------------------------------------------------
mkFileArrObj(G,N,X,erlang) ->
    SName = 
	ic_util:to_undersc([ic_forms:get_id2(X) | N]),
    FName =  
	ic_file:join(ic_options:get_opt(G, stubdir),
		     ic_file:add_dot_erl(SName)),

    case file:open(FName, [write]) of
	{ok, Fd} ->
	    HrlFName = filename:basename(ic_genobj:include_file(G)),

	    ic_codegen:emit_stub_head(G, Fd, SName, erlang),
	    emit(Fd, "-include(~p).\n\n",[HrlFName]),
	    emit_exports(G,Fd),
	    emit_arr_methods(G,N,X,SName,Fd),  
	    ic_codegen:nl(Fd),
	    ic_codegen:nl(Fd),
	    file:close(Fd);
	Other -> 
	    exit(Other)
    end.




%%------------------------------------------------------------
%%
%% emit exports for erlang modules which represent records.
%%
%%------------------------------------------------------------
emit_exports(G,Fd) ->
    case ic_options:get_opt(G, be) of
	erl_corba ->
	    emit(Fd, "-export([tc/0,id/0,name/0]).\n\n\n\n",[]);
	_ ->
	    emit(Fd, "-export([id/0,name/0]).\n\n\n\n",[])
    end.


%%------------------------------------------------------------
%%
%% emit erlang module functions which represent records, yields
%% record information such as type code, identity and name.
%%
%%------------------------------------------------------------
emit_rec_methods(G,N,X,Name,Fd) ->

    IR_ID = ictk:get_IR_ID(G, N, X),

    case ic_options:get_opt(G, be) of

	erl_corba ->
	    TK = ic_forms:get_tk(X),

	    case TK of
		undefined ->
		    STK = ic_forms:search_tk(G,ictk:get_IR_ID(G, N, X)),
		    emit(Fd, "%% returns type code\n",[]),
		    emit(Fd, "tc() -> ~p.\n\n",[STK]),
		    emit(Fd, "%% returns id\n",[]),
		    emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
		    emit(Fd, "%% returns name\n",[]),
		    emit(Fd, "name() -> ~p.\n\n",[Name]);
		_ ->
		    emit(Fd, "%% returns type code\n",[]),
		    emit(Fd, "tc() -> ~p.\n\n",[TK]),
		    emit(Fd, "%% returns id\n",[]),
		    emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
		    emit(Fd, "%% returns name\n",[]),
		    emit(Fd, "name() -> ~p.\n\n",[Name])
	    end;

	_ ->
	    emit(Fd, "%% returns id\n",[]),
	    emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
	    emit(Fd, "%% returns name\n",[]),
	    emit(Fd, "name() -> ~p.\n\n",[Name])
    end.



%%------------------------------------------------------------
%%
%% emit erlang module functions which represent arrays, yields
%% record information such as type code, identity and name.
%%
%%------------------------------------------------------------
emit_arr_methods(G,N,X,Name,Fd) ->

    IR_ID = ictk:get_IR_ID(G, N, X),

    case ic_options:get_opt(G, be) of

	erl_corba ->

	    TK = ic_forms:get_type_code(G, N, X),

	    emit(Fd, "%% returns type code\n",[]),
	    emit(Fd, "tc() -> ~p.\n\n",[TK]),
	    emit(Fd, "%% returns id\n",[]),
	    emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
	    emit(Fd, "%% returns name\n",[]),
	    emit(Fd, "name() -> ~p.\n\n",[Name]);

	_ ->

	    emit(Fd, "%% returns id\n",[]),
	    emit(Fd, "id() -> ~p.\n\n",[IR_ID]),
	    emit(Fd, "%% returns name\n",[]),
	    emit(Fd, "name() -> ~p.\n\n",[Name])
    end. 

get_seq_max(T) when is_record(T, sequence) andalso T#sequence.length == 0 ->
    infinity;
get_seq_max(T) when is_record(T, sequence) andalso is_tuple(T#sequence.length) ->
    list_to_integer(element(3, T#sequence.length)).


align(Cs) ->
    ic_util:mk_align(Cs).

