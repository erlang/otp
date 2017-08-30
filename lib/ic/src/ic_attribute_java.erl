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

-module(ic_attribute_java).

-include("icforms.hrl").
-include("ic.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([emit_attribute_prototype/4,
	 emit_attribute_stub_code/4,
	 emit_atrribute_on_dictionary/5,
	 emit_attribute_switch_case/5]).





%%%-----------------------------------------------------
%%%
%%%   Generates operation in interface
%%%
%%%-----------------------------------------------------
emit_attribute_prototype(G, N, X, Fd) ->
    emit_attribute_prototype(G, N, X, Fd, ic_forms:get_idlist(X)).

emit_attribute_prototype(_G, _N, _X, _Fd, []) ->
    ok;
emit_attribute_prototype(G, N, X, Fd, [V|Vs]) ->
    WireAttrName = ic_forms:get_id(V),
    AttrName = ic_forms:get_java_id(WireAttrName),
    emit_attr_prototype(G, N, X, Fd, AttrName,WireAttrName),
    emit_attribute_prototype(G, N, X, Fd, Vs).


emit_attr_prototype(G, N, X, Fd, OpName, WireOpName) ->

    ic_codegen:emit(Fd, "/****\n"),
    ic_codegen:emit(Fd, " * Attribute ~p interface functions \n", [ic_util:to_colon([WireOpName|N])]),
    ic_codegen:emit(Fd, " *\n"),
    ic_codegen:emit(Fd, " */\n\n"),
    
    AT = ic_forms:get_type(X),
    Type = ic_java_type:getType(G, N, AT),
%    HolderType = ic_java_type:getHolderType(G, N, AT),

    ic_codegen:emit(Fd, "    ~s ~s() throws java.lang.Exception;\n\n",[Type, OpName]),
   
    case X#attr.readonly of
	{readonly, _} ->
	    ok;
	_ ->
	    ic_codegen:emit(Fd, "    void ~s(~s _value) throws java.lang.Exception;\n\n",[OpName, Type])
    end.



%%%-----------------------------------------------------
%%%
%%%   Generates attribute insertion in dictionary
%%%
%%%-----------------------------------------------------
emit_atrribute_on_dictionary(G, N, X, Fd, C) ->
    emit_atrribute_on_dictionary(G, N, X, Fd, C, ic_forms:get_idlist(X)).

emit_atrribute_on_dictionary(_G, _N, _X, _Fd, C, []) ->
    C;
emit_atrribute_on_dictionary(G, N, X, Fd, C, [V|Vs]) ->

    WireAttrName = ic_forms:get_id(V),

    ic_codegen:emit(Fd, "      _operations.put(\"_get_~s\", new java.lang.Integer(~p));\n",
		    [WireAttrName,C]),

    case X#attr.readonly of
	{readonly, _} ->
	    
	    emit_atrribute_on_dictionary(G, N, X, Fd, C+1, Vs);

	_ ->

	    ic_codegen:emit(Fd, "      _operations.put(\"_set_~s\", new java.lang.Integer(~p));\n",
			    [WireAttrName,C+1]),

	    emit_atrribute_on_dictionary(G, N, X, Fd, C+2, Vs)
    end.



%%%-----------------------------------------------------
%%%
%%%   Generates attribute case in server switch
%%%
%%%-----------------------------------------------------
emit_attribute_switch_case(G, N, X, Fd, C) ->
    Tk = ic_forms:get_tk(X),
    emit_attribute_switch_case(G, N, X, Fd, Tk, C, ic_forms:get_idlist(X)).

emit_attribute_switch_case(_G, _N, _X, _Fd, _Tk, C, []) ->
    C;
emit_attribute_switch_case(G, N, X, Fd, Tk, C, [V|Vs]) ->
    AttrName = ic_forms:get_java_id(V),

    emit_attribute_switch_case1(G,N,X,Fd,"_get_",AttrName,Tk,C),

    case X#attr.readonly of
	{readonly, _} ->
	    emit_attribute_switch_case(G, N, X, Fd, Tk, C+1, Vs);

	_ ->
	    emit_attribute_switch_case1(G,N,X,Fd,"_set_",AttrName,Tk,C+1),
	    emit_attribute_switch_case(G, N, X, Fd, Tk, C+2, Vs)
    end.


emit_attribute_switch_case1(G, N, X, Fd, "_get_", Name, _Tk, C) ->

    R = ic_forms:get_type(X),
    RT = ic_java_type:getParamType(G,N,R,ret),

    ic_codegen:emit(Fd, "        case ~p:  {  // Get operation for attribute ~s\n\n",[C,ic_util:to_dot([Name|N])]),
    
    ic_codegen:emit(Fd, "          // Calling implementation function\n"),
    ic_codegen:emit(Fd, "          ~s _result = this.~s();\n\n", [RT, Name]),

    ic_codegen:emit(Fd, "          // Marshalling output\n"),
    ic_codegen:emit(Fd, "          ~sOtpErlangRef __ref = __env.getSref();\n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "          __os.write_tuple_head(2);\n"),
    ic_codegen:emit(Fd, "          __os.write_ref(__ref.node(),__ref.id(),__ref.creation());  // Call reference\n"),

    case ic_java_type:isBasicType(G,N,R) of
	true ->
	    ic_codegen:emit(Fd, "          __os~s(_result);  // Return value\n\n", 
			    [ic_java_type:marshalFun(G,N,X,R)]);
	false ->
	    ic_codegen:emit(Fd, "          ~s(__os,_result);  // Return value\n\n", 
			    [ic_java_type:marshalFun(G,N,X,R)])
    end,

    ic_codegen:emit(Fd, "        } break;\n\n");


emit_attribute_switch_case1(G, N, X, Fd, "_set_", Name, _Tk, C) ->
    ic_codegen:emit(Fd, "        case ~p:  {  // Set operation for attribute ~s\n\n",[C,ic_util:to_dot([Name|N])]),
    
    Type = ic_forms:get_type(X),
    
    ic_codegen:emit(Fd, "          // Preparing input\n"),
    ic_codegen:emit(Fd, "          ~sOtpInputStream __is = __env.getIs();\n",[?ERLANGPACKAGE]),

    case ic_java_type:isBasicType(G,N,Type) of
	true ->
	    ic_codegen:emit(Fd, "          ~s _value = __is~s;  // In value\n\n",
			    [ic_java_type:getParamType(G,N,Type,in),
			     ic_java_type:unMarshalFun(G,N,X,Type)]); 
	false -> 
	    ic_codegen:emit(Fd, "          ~s _value = ~s.unmarshal(__is);  // In value\n\n",
			    [ic_java_type:getParamType(G,N,Type,in),
			     ic_java_type:getUnmarshalType(G,N,X,Type)])
    end,
 
    
    ic_codegen:emit(Fd, "          // Calling implementation function\n"),
    ic_codegen:emit(Fd, "          this.~s(_value);\n\n", [Name]),

    ic_codegen:emit(Fd, "          // Marshalling output\n"),
    ic_codegen:emit(Fd, "          ~sOtpErlangRef __ref = __env.getSref();\n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "          __os.write_tuple_head(2);\n"),
    ic_codegen:emit(Fd, "          __os.write_ref(__ref.node(),__ref.id(),__ref.creation());  // Call reference\n"),
    ic_codegen:emit(Fd, "          __os.write_atom(\"ok\");\n\n"),
    
    ic_codegen:emit(Fd, "        } break;\n\n").



    



%%%-----------------------------------------------------
%%%
%%%   Generates attribute function in stub
%%%
%%%-----------------------------------------------------
emit_attribute_stub_code(G, N, X, Fd) ->
    emit_attribute_stub_code(G, N, X, Fd, ic_forms:get_idlist(X)).

emit_attribute_stub_code(_G, _N, _X, _Fd, []) ->
    ok;
emit_attribute_stub_code(G, N, X, Fd, [V|Vs]) ->
    WireAttrName = ic_forms:get_id(V),
    AttrName = ic_forms:get_java_id(WireAttrName),

    emit_attribute_stub_code1(G,N,X,Fd,"_get_",AttrName,WireAttrName),

    case X#attr.readonly of
	{readonly, _} ->
	    emit_attribute_stub_code(G, N, X, Fd, Vs);

	_ ->
	    emit_attribute_stub_code1(G,N,X,Fd,"_set_",AttrName,WireAttrName),
	    emit_attribute_stub_code(G, N, X, Fd, Vs)
    end.


emit_attribute_stub_code1(G,N,X,Fd,"_get_",Name,WireName) ->

    Type = ic_forms:get_type(X),
    RT = ic_java_type:getType(G,N,Type),

    %%
    %%  Main get operation
    %%
    ic_codegen:emit(Fd, "    // Attribute ~p get operation implementation\n", [ic_util:to_colon([WireName|N])]),
    ic_codegen:emit(Fd, "    public ~s ~s() throws java.lang.Exception {\n\n", [RT, Name]),
    
    %% Function marshal call
    ic_codegen:emit(Fd, "      // Calling the marshal function\n"),
    ic_codegen:emit(Fd, "      _~s_marshal(_env);\n\n", [Name]),
    
    %% Sending call
    ic_codegen:emit(Fd, "      // Message send\n"),
    ic_codegen:emit(Fd, "      _env.send();\n\n"),

    %% Receiving return value
    ic_codegen:emit(Fd, "      // Message receive\n"),
    ic_codegen:emit(Fd, "      _env.receive();\n\n"),

    ic_codegen:emit(Fd, "      // Calling the unmarshal function\n"),
    ic_codegen:emit(Fd, "      return _~s_get_unmarshal(_env);\n", [Name]),
    ic_codegen:emit(Fd, "    }\n\n"),


    %%
    %%  Marshal get operation
    %%
    ic_codegen:emit(Fd, "    // Marshal operation for get attribute ~p\n", [Name]),
    ic_codegen:emit(Fd, "    public static void _~s_marshal(~sEnvironment __env)\n", 
		    [Name, ?ICPACKAGE]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),
    
    ic_codegen:emit(Fd, "      // Get output stream\n"),
    ic_codegen:emit(Fd, "      ~sOtpOutputStream __os = __env.getOs();\n\n",[?ERLANGPACKAGE]),

    %% Initiating Message header
    ic_codegen:emit(Fd, "      // Message header assembly\n"),
    ic_codegen:emit(Fd, "      __os.reset();\n"),
    ic_codegen:emit(Fd, "      __os.write_tuple_head(3);\n"),
    ic_codegen:emit(Fd, "      __os.write_atom(\"$gen_call\");\n\n"),
    

    %% Creating call identity tuple
    ic_codegen:emit(Fd, "      // Message identity part creation\n"),
    ic_codegen:emit(Fd, "      __os.write_tuple_head(2);\n"),
    ic_codegen:emit(Fd, "      __env.write_client_pid();\n"),
    ic_codegen:emit(Fd, "      __env.write_client_ref();\n\n"),

    OpCallName = case ic_options:get_opt(G, scoped_op_calls) of 
		     true -> 
			 ic_util:to_undersc(["_get_"++WireName|N]);
		     false ->
			 "_get_"++WireName
		 end,

    %% Creating operation identity
    ic_codegen:emit(Fd, "      // Message operation part creation\n"),
    ic_codegen:emit(Fd, "      __os.write_atom(~p);\n\n",[OpCallName]),

    ic_codegen:emit(Fd, "    }\n\n"),


    %%
    %%  Unmarshal get operation
    %%
    MRT = ic_java_type:getParamType(G,N,Type,ret),

    ic_codegen:emit(Fd, "    // Unmarshal operation for get attribute ~p\n", [Name]),
    ic_codegen:emit(Fd, "    public static ~s _~s_get_unmarshal(~sEnvironment __env)\n", 
		    [MRT, Name, ?ICPACKAGE]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),

    
    ic_codegen:emit(Fd, "        // Get input stream\n"),
    ic_codegen:emit(Fd, "        ~sOtpInputStream __is = __env.getIs();\n\n",[?ERLANGPACKAGE]),

    ic_codegen:emit(Fd, "      // Extracting return value\n"),
    case ic_java_type:isBasicType(G, N, Type) of
	true ->
	    ic_codegen:emit(Fd, "      return __is~s;\n",
			    [ic_java_type:unMarshalFun(G, N, X, Type)]);
	false ->
	    ic_codegen:emit(Fd, "      return ~s.unmarshal(__is);\n",
			    [ic_java_type:getUnmarshalType(G, N, X, Type)])
    end,
    
    ic_codegen:emit(Fd, "    }\n\n");


emit_attribute_stub_code1(G,N,X,Fd,"_set_",Name,WireName) ->

    Type = ic_forms:get_type(X),

    %%
    %%  Main set operation
    %%
    IT = ic_java_type:getType(G,N,Type),

    ic_codegen:emit(Fd, "    // Attribute ~p set operation implementation\n", [ic_util:to_colon([WireName|N])]),
    ic_codegen:emit(Fd, "    public void ~s(~s _value) throws java.lang.Exception {\n\n", [Name,IT]),

    %% Function marshal call
    ic_codegen:emit(Fd, "      // Calling the marshal function\n"),
    ic_codegen:emit(Fd, "      _~s_marshal(_env, _value);\n\n", [Name]),
    
    %% Sending call
    ic_codegen:emit(Fd, "      // Message send\n"),
    ic_codegen:emit(Fd, "      _env.send();\n\n"),

    %% Receiving return value
    ic_codegen:emit(Fd, "      // Message receive\n"),
    ic_codegen:emit(Fd, "      _env.receive();\n\n"),

    ic_codegen:emit(Fd, "      // Calling the unmarshal function\n"),
    ic_codegen:emit(Fd, "      _~s_set_unmarshal(_env);\n", [Name]),
    
    ic_codegen:emit(Fd, "    }\n\n"),
    
    
    %%
    %%  Marshal set operation
    %%
    IP = ic_java_type:getParamType(G, N, Type, in),
    OpCallName = case ic_options:get_opt(G, scoped_op_calls) of 
		     true -> 
			 ic_util:to_undersc(["_set_"++WireName|N]);
		     false ->
			 "_set_"++WireName
		 end,

    ic_codegen:emit(Fd, "    // Marshal operation for set attribute ~p\n", [Name]),
    ic_codegen:emit(Fd, "    public static void _~s_marshal(~sEnvironment __env, ~s _value)\n", 
		    [Name, ?ICPACKAGE, IP]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),
    
    ic_codegen:emit(Fd, "      // Get output stream\n"),
    ic_codegen:emit(Fd, "      ~sOtpOutputStream __os = __env.getOs();\n\n",[?ERLANGPACKAGE]),

    %% Initiating Message header
    ic_codegen:emit(Fd, "      // Message header assembly\n"),
    ic_codegen:emit(Fd, "      __os.reset();\n"),
    ic_codegen:emit(Fd, "      __os.write_tuple_head(3);\n"),
    ic_codegen:emit(Fd, "      __os.write_atom(\"$gen_call\");\n\n"),
    
    
    %% Creating call identity tuple
    ic_codegen:emit(Fd, "      // Message identity part creation\n"),
    ic_codegen:emit(Fd, "      __os.write_tuple_head(2);\n"),
    ic_codegen:emit(Fd, "      __env.write_client_pid();\n"),
    ic_codegen:emit(Fd, "      __env.write_client_ref();\n\n"),
    

    %% Creating operation identity
    ic_codegen:emit(Fd, "      // Message operation part creation\n"),
    ic_codegen:emit(Fd, "      __os.write_tuple_head(2);\n"),
    ic_codegen:emit(Fd, "      __os.write_atom(~p);\n",[OpCallName]),
    
    case ic_java_type:isBasicType(G, N, Type) of
	true ->
	    ic_codegen:emit(Fd, "      __os~s(_value);\n\n",
			    [ic_java_type:marshalFun(G, N, X, Type)]);
	false ->
	    ic_codegen:emit(Fd, "      ~s(__os, _value);\n\n",
			    [ic_java_type:marshalFun(G, N, X, Type)])
    end,
    ic_codegen:emit(Fd, "    }\n\n"),
    
    
    ic_codegen:emit(Fd, "    // Unmarshal operation for set attribute ~p\n", [Name]),
    ic_codegen:emit(Fd, "    public static void _~s_set_unmarshal(~sEnvironment __env)\n", 
		    [Name, ?ICPACKAGE]),
    ic_codegen:emit(Fd, "      throws java.lang.Exception {\n\n"),

    ic_codegen:emit(Fd, "        // Get input stream\n"),
    ic_codegen:emit(Fd, "        ~sOtpInputStream __is = __env.getIs();\n\n",[?ERLANGPACKAGE]),

    ic_codegen:emit(Fd, "        __is.read_atom();\n"),
    ic_codegen:emit(Fd, "    }\n\n").






