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

-module(ic_sequence_java).


-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([gen/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: gen/4
%%-----------------------------------------------------------------
gen(G, N, X, SequenceName) when is_record(X, sequence) ->
    emit_holder_class(G, N, X, SequenceName),
    emit_helper_class(G, N, X, SequenceName);
gen(_G, _N, _X, _SequenceName) -> 
    ok.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------


%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, X, SequenceName) ->
    SName = string:concat(SequenceName, "Holder"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),

    SequenceType = ic_java_type:getType(G, N, X),

    ic_codegen:emit(Fd, ["final public class ",SequenceName,"Holder {\n"
			 "   // instance variables\n"
			 "   public ",SequenceType," value;\n\n"
			 "   // constructors\n"
			 "   public ",SequenceName,"Holder() {}\n"
			 "   public ",SequenceName,"Holder(",SequenceType," initial) {\n"
			 "      value = initial;\n"
			 "   }\n\n"

			 "   // methods\n"

			 "   public void _marshal(",?ERLANGPACKAGE,"OtpOutputStream out) throws java.lang.Exception{\n"  
			 "      ",SequenceName,"Helper.marshal(out, value);\n"
			 "   }\n\n"

			 "   public void _unmarshal(",?ERLANGPACKAGE,"OtpInputStream in) throws java.lang.Exception {\n" 
			 "      value = ",SequenceName,"Helper.unmarshal(in);\n"
			 "   }\n\n"
			 "}\n"]),
    file:close(Fd).



emit_helper_class(G, N, X, SequenceName) ->
    SName = string:concat(SequenceName, "Helper"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),
    
    SequenceType = ic_java_type:getType(G, N, X),
    ElementType = ic_forms:get_type(X),

    ic_codegen:emit(Fd, ["public class ",SequenceName,"Helper {\n"
    
			 "   // constructors\n"
			 "   private ",SequenceName,"Helper() {}\n\n"

			 "   // methods\n"
			 "   public static void marshal(",?ERLANGPACKAGE,"OtpOutputStream _out, ",SequenceType," _value) \n"
			 "     throws java.lang.Exception {\n\n"]), 

    emit_sequence_marshal_function(G, N, X, Fd, SequenceName, ElementType),

    ic_codegen:emit(Fd, ["   }\n\n"

			 "   public static ",SequenceType," unmarshal(",?ERLANGPACKAGE,"OtpInputStream _in) \n"
			 "     throws java.lang.Exception {\n\n"]),

    emit_sequence_unmarshal_function(G, N, X, Fd, SequenceName, ElementType),

    ic_codegen:emit(Fd, ["   }\n\n"
    
			 "   public static String id() {\n" 
			 "      return \"",ic_pragma:scope2id(G, [SequenceName | N]),"\";\n"
			 "   }\n\n"

			 "   public static String name() {\n" 
			 "      return \"",SequenceName,"\";\n"
			 "   }\n\n"]),

    ic_jbe:emit_type_function(G, N, X, Fd),
    
    ic_codegen:emit(Fd, ["   public static void insert(",?ICPACKAGE,"Any _any, ",SequenceType," _this)\n"
			 "     throws java.lang.Exception {\n\n"
   
			 "     ",?ERLANGPACKAGE,"OtpOutputStream _os = \n"
			 "       new ",?ERLANGPACKAGE,"OtpOutputStream();\n\n" 
    
			 "     _any.type(type());\n"    
			 "     marshal(_os, _this);\n"
			 "     _any.insert_Streamable(_os);\n"
			 "   }\n\n"
			 
			 "   public static ",SequenceType," extract(",?ICPACKAGE,"Any _any)\n"
			 "     throws java.lang.Exception {\n\n"
			 
			 "     return unmarshal(_any.extract_Streamable());\n"
			 "   }\n\n"
    

    %% In corba mapping there is also a _type function here.
			 "}\n\n"]),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func: emit_sequence_marshal_function/6
%%-----------------------------------------------------------------
emit_sequence_marshal_function(G, N, X, Fd, _SequenceName, ElementType) ->
    ic_codegen:emit(Fd, ["    int _length = _value.length;\n\n"

			 "    _out.write_list_head(_length);\n\n"

			 "    if (_length > 0) {\n"
			 "      for(int _tmp = 0; _tmp < _length; _tmp++)\n"]),

    case ic_java_type:isBasicType(G, N, ElementType) of 
	true ->
	    ic_codegen:emit(Fd, ["        _out",ic_java_type:marshalFun(G, N, X, ElementType),"(_value[_tmp]);\n\n"]);
	false ->
	    ic_codegen:emit(Fd, ["        ",ic_java_type:marshalFun(G, N, X, ElementType),"(_out, _value[_tmp]);\n\n"])
    end,
    
    ic_codegen:emit(Fd, ["      _out.write_nil();\n"
			 "    }\n\n"]).




%%-----------------------------------------------------------------
%% Func: emit_sequence_unmarshal_function/6
%%-----------------------------------------------------------------
emit_sequence_unmarshal_function(G, N, X, Fd, _SequenceName, ElementType) ->

    SequenceElementType = ic_java_type:getType(G, N, ElementType),

    ic_codegen:emit(Fd, ["    int _tag,_length;\n"
			 "    ",SequenceElementType," _sequence[];\n"
			 "    _tag = _in.peek();\n\n"]),

    case ic_java_type:isIntegerType(G, N, ElementType) of
	true ->
	    ic_codegen:emit(Fd, ["    switch(_tag) {\n"
				 "    case ",?ERLANGPACKAGE,"OtpExternal.stringTag:\n"
				 "      byte _compressed[] = (_in.read_string()).getBytes();\n"
				 "      _length = _compressed.length;\n"
				 "      _sequence = new ",ic_java_type:getFullType(G,N,X),";\n\n"
				 
				 "      for(int _tmp = 0; _tmp < _length; _tmp++)\n"
				 "        _sequence[_tmp] = (",ic_java_type:getType(G, N, ElementType),")(_compressed[_tmp] & 0xff);\n\n"
				 
				 "      break;\n"
				 "    default:\n"
				 "      _length = _in.read_list_head();\n"
				 "      _sequence = new ",ic_java_type:getFullType(G,N,X),";\n\n"
				 
				 "      if(_length > 0) {\n"
				 "        for(int _tmp = 0; _tmp < _length; _tmp++)\n"
				 "          _sequence[_tmp] = _in",ic_java_type:unMarshalFun(G, N, X, ElementType),";\n\n"
				 
				 "        _in.read_nil();\n"
				 "      }\n"
				 "    }\n"]);
	false ->
	    ic_codegen:emit(Fd, ["    _length = _in.read_list_head();\n"
				 "    _sequence = new ",ic_java_type:getFullType(G,N,X),";\n\n"
				 
				 "    if(_length > 0) {\n"
				 "      for(int _tmp = 0; _tmp < _length; _tmp++)\n"]),
	    case ic_java_type:isBasicType(G, N, ElementType) of 
		true ->
		    ic_codegen:emit(Fd, ["        _sequence[_tmp] = _in",ic_java_type:unMarshalFun(G, N, X, ElementType),";\n\n"]);
		_ ->
		    ic_codegen:emit(Fd, ["        _sequence[_tmp] = ",ic_java_type:getUnmarshalType(G, N, X, ElementType),".unmarshal(_in);\n\n"])
	    end,
	    
	    ic_codegen:emit(Fd, ["      _in.read_nil();\n"
				 "    }\n\n"])
    end,
	    
    ic_codegen:emit(Fd, "    return _sequence;\n").




%%---------------------------------------------------
%%  Utilities
%%---------------------------------------------------









