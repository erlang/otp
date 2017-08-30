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
%%-----------------------------------------------------------------
%% File: icenum.erl
%% 
%%
%%-----------------------------------------------------------------
%%
%% Code generation for enum's.
%%-----------------------------------------------------------------
-module(icenum).

-import(ic_codegen, [emit/2, emit/3, emit/4, emit_c_enc_rpt/4, emit_c_dec_rpt/4]).

-include("icforms.hrl").
-include("ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([enum_gen/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

enum_gen(G, N, X, c) when is_record(X, enum) ->
    emit_c_enum(G, N, X);
enum_gen(_G, _N, _X, _L) ->
    ok.


emit_c_enum(G, N, X) ->
    case ic_genobj:is_hrlfile_open(G) of
	true ->
	    EnumName = [ic_forms:get_id2(X) | N],

	    case ic_pragma:is_local(G,EnumName) of
		true ->

		    Fd = ic_genobj:hrlfiled(G),
		    EnumNameStr = ic_util:to_undersc(EnumName),
		    ic_code:insert_typedef(G, EnumNameStr, {enum, EnumNameStr}),
		    {tk_enum,_,_,EList} = ic_forms:get_tk(X),
		    emit(Fd, "\n#ifndef __~s__\n",[ic_util:to_uppercase(EnumNameStr)]),	
		    emit(Fd, "#define __~s__\n",[ic_util:to_uppercase(EnumNameStr)]),
		    ic_codegen:mcomment_light(Fd,
					      [io_lib:format("Enum definition: ~s",
							     [EnumNameStr])],
					      c),
		    emit(Fd, "typedef CORBA_enum {", []),
		    emit_c_enum_values(G, N, Fd, EList),
		    emit(Fd, "} ~s ;\n\n", [EnumNameStr]),
		    create_c_enum_file(G, N, EnumNameStr, EList),
		    emit(Fd, "\n#endif\n\n");

		false ->   %% Do not generate included types att all.
		    ok
	    end;

	false ->
	    ok
    end.


emit_c_enum_values(_G, N, Fd, [E]) ->
    emit(Fd, "~s", [ic_util:to_undersc([E| N])]);
emit_c_enum_values(G, N, Fd, [E |Es]) ->
    emit(Fd, "~s, ", [ic_util:to_undersc([E| N])]),
    emit_c_enum_values(G, N, Fd, Es).


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


create_c_enum_file(G, N, Name, Elist) ->

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
    %%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    emit(Fd, "char* ~s[~p] = {\n", [ic_util:mk_oe_name(G, Name),
				    length(Elist)]),
    emit_c_enum_array_values(Fd, Elist),
    emit(Fd, "};\n\n",[]),
    emit_sizecount(G, N, Fd, HFd, Name, Elist),
    emit_encode(G, N, Fd, HFd, Name, Elist),
    emit_decode(G, N, Fd, HFd, Name, Elist),
    file:close(Fd).

emit_c_enum_array_values(Fd, [E]) ->
    emit(Fd, "  ~p\n", [E]);
emit_c_enum_array_values(Fd, [E |Es]) ->
    emit(Fd, "  ~p,\n", [E]),
    emit_c_enum_array_values(Fd, Es).


emit_sizecount(G, _N, Fd, HFd, Name, _Elist) ->

    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, int*, int*);\n",
	 [ic_util:mk_oe_name(G, "sizecalc_"), Name]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, int* oe_size_count_index, int* oe_size)\n"
	 "{\n",
	 [ic_util:mk_oe_name(G, "sizecalc_"), Name]),
    emit(Fd, "  int oe_error_code = 0;\n\n",[]),

    AlignName = lists:concat(["*oe_size + sizeof(",Name,")"]),
    emit(Fd, "  *oe_size = ~s;\n\n",[ic_util:mk_align(AlignName)]),

    emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, oe_size_count_index, 0)) < 0) {\n"),
    emit_c_enc_rpt(Fd, "    ", "ei_decode_atom", []),
    emit(Fd, "    return oe_error_code;\n   }\n"),
    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n\n",[]).


emit_encode(G, _N, Fd, HFd, Name, _Elist) ->

    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, ~s);\n",
	 [ic_util:mk_oe_name(G, "encode_"), Name, Name]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, ~s oe_rec) {\n",
	 [ic_util:mk_oe_name(G, "encode_"), Name, Name]),
    emit(Fd, "  int oe_error_code = 0;\n\n",[]),

    emit(Fd, "  if ((oe_error_code = oe_ei_encode_atom(oe_env, ~s[oe_rec])) < 0) {\n", 
	 [ic_util:mk_oe_name(G, Name)]),
    emit_c_enc_rpt(Fd, "    ", "oe_ei_encode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  return 0;\n\n",[]),
    emit(Fd, "}\n\n",[]).

emit_decode(G, _N, Fd, HFd, Name, Elist) ->

    emit(HFd, "int ~s~s(CORBA_Environment *oe_env, char *, int*, ~s *);\n",
	 [ic_util:mk_oe_name(G, "decode_"), Name, Name]),

    emit(Fd, "int ~s~s(CORBA_Environment *oe_env, char *oe_first, int* oe_outindex, "
	 "~s *oe_out) {\n\n",
	 [ic_util:mk_oe_name(G, "decode_"), Name, Name]),
    emit(Fd, "  int oe_error_code = 0;\n",[]),
    emit(Fd, "  int oe_i;\n",[]),
    emit(Fd, "  char oe_atom[256];\n\n",[]),

    AlignName = lists:concat(["*oe_outindex + sizeof(",Name,")"]),
    emit(Fd, "  *oe_outindex = ~s;\n\n",[ic_util:mk_align(AlignName)]),

    emit(Fd, "  if ((oe_error_code = ei_decode_atom(oe_env->_inbuf, &oe_env->_iin, oe_atom)) < 0) {\n"),
    emit_c_enc_rpt(Fd, "    ", "ei_decode_atom", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    Len = length(Elist),
    emit(Fd, "  for(oe_i = 0; oe_i < ~p && strcmp(oe_atom, ~s[oe_i]); oe_i++);\n",
	 [Len, ic_util:mk_oe_name(G, Name)]),
    emit(Fd, "    *oe_out = oe_i;\n\n", []),

    emit(Fd, "  if (oe_i == ~p) {\n",[Len]),
    emit_c_enc_rpt(Fd, "    ", "decode atom failure", []),
    emit(Fd, "    return oe_error_code;\n  }\n"),

    emit(Fd, "  return 0;\n",[]),
    emit(Fd, "}\n\n",[]).





