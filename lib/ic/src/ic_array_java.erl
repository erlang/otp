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

-module(ic_array_java).

-export([gen/4]).

-include("ic.hrl").
-include("icforms.hrl").


gen(G, N, X, Array) when is_record(X, member) ->
    ArrayName =  ic_forms:get_java_id(Array),
    ArrayElement = ic_forms:get_type(X),
    emit_holder_class(G, N, X, Array, ArrayName, ArrayElement),
    emit_helper_class(G, N, X, Array, ArrayName, ArrayElement);
gen(G, N, X, Array) when is_record(X, case_dcl) ->
    ArrayName =  ic_forms:get_java_id(Array),
    ArrayElement = ic_forms:get_type(X),
    emit_holder_class(G, N, X, Array, ArrayName, ArrayElement),
    emit_helper_class(G, N, X, Array, ArrayName, ArrayElement);
gen(G, N, X, Array)  ->
    ArrayName =  ic_forms:get_java_id(Array),
    ArrayElement = ic_forms:get_body(X),
    emit_holder_class(G, N, X, Array, ArrayName, ArrayElement),
    emit_helper_class(G, N, X, Array, ArrayName, ArrayElement).



%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, _X, Array, ArrayName, ArrayElement) ->
    SName = string:concat(ArrayName, "Holder"),
    {Fd, _}= ic_file:open_java_file(G, N, SName), 

    ArrayElementName = ic_java_type:getType(G, N, ArrayElement),
    EmptyDim = arrayEmptyDim(Array),

    ic_codegen:emit(Fd, "final public class ~sHolder {\n",[ArrayName]),

    ic_codegen:emit(Fd, "   // instance variables\n", []),
    ic_codegen:emit(Fd, "   public ~s~s value;\n\n", 
		    [ArrayElementName,EmptyDim]),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   public ~sHolder() {}\n", [ArrayName]),
    ic_codegen:emit(Fd, "   public ~sHolder(~s~s initial) {\n", 
		    [ArrayName,ArrayElementName,EmptyDim]),
    ic_codegen:emit(Fd, "      value = initial;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
    
    ic_codegen:emit(Fd, "   // methods\n", []),
    
    ic_codegen:emit(Fd, "   public void _marshal(~sOtpOutputStream out)\n", [?ERLANGPACKAGE]),  
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n"),
    ic_codegen:emit(Fd, "      ~sHelper.marshal(out, value);\n", [ArrayName]),
    ic_codegen:emit(Fd, "   }\n"),
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   public void _unmarshal(~sOtpInputStream in)\n", [?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n"),
    ic_codegen:emit(Fd, "      value = ~sHelper.unmarshal(in);\n", [ArrayName]),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
    
    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_helper_class/4
%%-----------------------------------------------------------------
emit_helper_class(G, N, X, Array, ArrayName, ArrayElement) ->
    SName = string:concat(ArrayName, "Helper"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),
 
    ArrayElementName = ic_java_type:getType(G, N, ArrayElement),
    EmptyDim = arrayEmptyDim(Array),
%    Dim = arrayDim(G,N,Array),

    ic_codegen:emit(Fd, "public class ~sHelper {\n",[ArrayName]),
    
    ic_codegen:emit(Fd, "   // constructors\n"),
    ic_codegen:emit(Fd, "   private ~sHelper() {}\n\n", [ArrayName]),

    ic_codegen:emit(Fd, "   // methods\n"),

    ic_codegen:emit(Fd, "   public static void marshal(~sOtpOutputStream _out, ~s~s _value)\n", 
		    [?ERLANGPACKAGE,ArrayElementName,EmptyDim]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
    emit_array_marshal_loop(G,N,X,Array,ArrayElement,Fd),
    ic_codegen:emit(Fd, "   }\n"),
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   public static ~s~s unmarshal(~sOtpInputStream _in)\n",
		    [ArrayElementName,EmptyDim,?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
    ic_codegen:emit(Fd, "     ~s~s _value = new ~s;\n\n", 
		    [ArrayElementName,EmptyDim,ic_java_type:getFullType(G, N, X, Array)]),
    emit_array_unmarshal_loop(G,N,X,Array,ArrayElement,Fd),
    ic_codegen:emit(Fd, "     return _value;\n"),
    ic_codegen:emit(Fd, "   }\n\n"),
    
    ic_codegen:emit(Fd, "   public static String id() {\n", []), 
    ic_codegen:emit(Fd, "     return ~p;\n",[ictk:get_IR_ID(G, N, Array)]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static String name() {\n", []), 
    ic_codegen:emit(Fd, "     return ~p;\n",[ArrayName]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_jbe:emit_type_function(G, N, X, Fd),

    ic_codegen:emit(Fd, "   public static void insert(~sAny _any, ~s~s _this)\n",
		    [?ICPACKAGE,ArrayElementName,EmptyDim]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
   
    ic_codegen:emit(Fd, "     ~sOtpOutputStream _os = \n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "       new ~sOtpOutputStream();\n\n",[?ERLANGPACKAGE]), 
    
    ic_codegen:emit(Fd, "     _any.type(type());\n"),     
    ic_codegen:emit(Fd, "     marshal(_os, _this);\n"),
    ic_codegen:emit(Fd, "     _any.insert_Streamable(_os);\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static ~s~s extract(~sAny _any)\n",
		    [ArrayElementName,EmptyDim,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
  
    ic_codegen:emit(Fd, "     return unmarshal(_any.extract_Streamable());\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "}\n"),
    file:close(Fd).




emit_array_marshal_loop(G,N,X,Array,AEl,Fd) ->
    DimList = mk_array_dim_list(G,N,Array),
    emit_array_marshal_loop_1(G,N,X,Array,AEl,DimList,0,Fd).


emit_array_marshal_loop_1(G,N,X,Array,AEl,[D],C,Fd) ->

    DimList = mk_array_dim_list(G,N,Array),

    ic_codegen:emit(Fd, "     _out.write_tuple_head(~s);\n\n",[D]),

    ic_codegen:emit(Fd, "     for(int _tmp~p = 0; _tmp~p < ~s; _tmp~p++)\n",[C,C,D,C]),

    case ic_java_type:isBasicType(G, N, AEl) of 
	true ->
	    ic_codegen:emit(Fd, "       _out~s(_value",
			    [ic_java_type:marshalFun(G, N, X, AEl)]);
	false ->
	    ic_codegen:emit(Fd, "       ~s(_out, _value",
			    [ic_java_type:marshalFun(G, N, X, AEl)])
    end,

    emit_array_dimensions(DimList,0,Fd),

    ic_codegen:emit(Fd, ");\n\n");

emit_array_marshal_loop_1(G,N,X,Array,AEl,[D|Ds],C,Fd) ->
%    DimList = mk_array_dim_list(G,N,Array),

    ic_codegen:emit(Fd, "     _out.write_tuple_head(~s);\n\n",[D]),

    ic_codegen:emit(Fd, "     for(int _tmp~p = 0; _tmp~p < ~s; _tmp~p++) {\n",[C,C,D,C]),

    emit_array_marshal_loop_1(G,N,X,Array,AEl,Ds,C+1,Fd),

    ic_codegen:emit(Fd, "     }\n\n").





emit_array_unmarshal_loop(G,N,X,Array,AEl,Fd) ->
    DimList = mk_array_dim_list(G,N,Array),
    case length(DimList) > 0 of
	true ->
	    ic_codegen:emit(Fd, "     _in.read_tuple_head();\n\n"),

	    ic_codegen:emit(Fd, "     for(int _tmp0 = 0; _tmp0 < ~s; _tmp0++) {\n\n",[hd(DimList)]),
	    emit_array_unmarshal_loop_1(G,N,X,Array,AEl,tl(DimList),1,Fd),
	    ic_codegen:emit(Fd, "     }\n\n");
	false ->
	    emit_array_unmarshal_loop_1(G,N,X,Array,AEl,DimList,0,Fd)
    end.

emit_array_unmarshal_loop_1(G,N,X,_Array,AEl,[],1,Fd) -> %% One dimensional array
    case ic_java_type:isBasicType(G, N, AEl) of 
	true ->
	    ic_codegen:emit(Fd, "       _value[_tmp0] = _in~s;\n",
			    [ic_java_type:unMarshalFun(G, N, X, AEl)]);
	false ->
	    ic_codegen:emit(Fd, "       _value[_tmp0] = ~s.unmarshal(_in);\n\n",
			    [ic_java_type:getUnmarshalType(G, N, X, AEl)])
    end;
emit_array_unmarshal_loop_1(G,N,X,Array,AEl,[],_C,Fd) ->
    DimList = mk_array_dim_list(G,N,Array),
    ic_codegen:emit(Fd, "       _value"),
    emit_array_dimensions(DimList,0,Fd),
    case ic_java_type:isBasicType(G,N,AEl) of
	true ->
	    ic_codegen:emit(Fd, " = _in~s;\n",
			    [ic_java_type:unMarshalFun(G, N, X, AEl)]);
	false ->
	    ic_codegen:emit(Fd, " = ~s.unmarshal(_in);\n",
			    [ic_java_type:getUnmarshalType(G, N, X, AEl)])
	end;
emit_array_unmarshal_loop_1(G,N,X,Array,AEl,[D|Ds],C,Fd) ->
    ic_codegen:emit(Fd, "     _in.read_tuple_head();\n\n"),

    ic_codegen:emit(Fd, "     for(int _tmp~p = 0; _tmp~p < ~s; _tmp~p++) {\n\n",[C,C,D,C]),
    emit_array_unmarshal_loop_1(G,N,X,Array,AEl,Ds,C+1,Fd),
    ic_codegen:emit(Fd, "     }\n").





%%---------------------------------------------------
%%  Utilities
%%---------------------------------------------------

mk_array_dim_list(G,N,Array) ->
    mk_array_dim_list2(G,N,Array#array.size).


mk_array_dim_list2(_G,_N,[]) ->
    [];

mk_array_dim_list2(G,N,[D |Ds]) when is_record(D,scoped_id) ->
    {FSN, _, _, _} = ic_symtab:get_full_scoped_name(G, N, D),
    [ ic_util:to_dot(G,FSN) | mk_array_dim_list2(G,N,Ds)];

mk_array_dim_list2(G,N,[D |Ds]) ->
    [ic_util:eval_java(G,N,D) | mk_array_dim_list2(G,N,Ds)].



%% Array dimension string
%arrayDim(G,N,X) ->
%    arrayDim2(G,N,X#array.size).

%arrayDim2(_G,_N,[]) ->
%    "";
%arrayDim2(G,N,[D|Ds]) when record(D,scoped_id) ->
%    {FSN, _, _, _} = ic_symtab:get_full_scoped_name(G, N, D),
%    "[" ++ ic_util:to_dot(G,FSN) ++ "]" ++ arrayDim2(G,N,Ds);
%arrayDim2(G,N,[D|Ds]) ->
%    "[" ++ ic_util:eval_java(G,N,D) ++ "]" ++ arrayDim2(G,N,Ds).


%% Array Empty dimension string
arrayEmptyDim(X) ->
    arrayEmptyDim2(X#array.size).

arrayEmptyDim2([_D]) ->
    "[]";
arrayEmptyDim2([_D |Ds]) ->
    "[]" ++ arrayEmptyDim2(Ds).


emit_array_dimensions([_D],C,Fd) ->
    ic_codegen:emit(Fd, "[_tmp~p]",[C]);
emit_array_dimensions([_D|Ds],C,Fd) ->
    ic_codegen:emit(Fd, "[_tmp~p]",[C]),
    emit_array_dimensions(Ds,C+1,Fd).






