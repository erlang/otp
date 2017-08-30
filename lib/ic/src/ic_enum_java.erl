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

-module(ic_enum_java).

-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([gen/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: gen/3
%%-----------------------------------------------------------------
gen(G, N, X) when is_record(X, enum) ->
    %%?PRINTDEBUG2("enum: ~p", [X]),
    EnumName = ic_forms:get_java_id(X),
    N2 = ["_" ++ EnumName |N],  
    ic_jbe:gen(G, N2, ic_forms:get_body(X)),

    emit_enum_class(G, N, X, EnumName),
    emit_holder_class(G, N, X, EnumName),
    emit_helper_class(G, N, X, EnumName);
gen(_G, _N, _X) -> 
    ok.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: emit_enum_class/4
%%-----------------------------------------------------------------
emit_enum_class(G, N, X, EnumName) ->
    {Fd, _} = ic_file:open_java_file(G, N, EnumName), 

    EList = enum_member_name_list(G, N, X),
    %%?PRINTDEBUG2("EList: ~p", [EList]),
    ic_codegen:emit(Fd, ["final public class ",EnumName," {\n\n"

			 "   // instance variables\n"]),

    emit_enum_member_int_values_initialization(G, N, X, Fd, EList),
    emit_enum_public_instance_variables(G, N, X, Fd, EnumName, EList),

    ic_codegen:emit(Fd, ["   private int _value;\n\n"

			 "   // constructors\n"
			 "   private ",EnumName,"(int __value) {\n"
			 "      _value = __value;\n"
			 "   }\n\n"
			 
			 "   // methods\n"
			 "   public int value() {\n"
			 "      return _value;\n"
			 "   }\n"]),
 
    emit_enum_from_int_function(G, N, X, Fd, EnumName, EList),
 
    ic_codegen:emit(Fd, "\n}\n"),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, _X, EnumName) ->
    EName = string:concat(EnumName, "Holder"),
    {Fd, _} = ic_file:open_java_file(G, N, EName), 
    
    ic_codegen:emit(Fd, ["final public class ",EnumName,"Holder {\n\n"

			 "   // instance variables\n"
			 "   public ",EnumName," value;\n\n"

			 "   // constructors\n"
			 "   public ",EnumName,"Holder() {}\n\n"

			 "   public ",EnumName,"Holder(",EnumName," initial) {\n"
			 "      value = initial;\n"
			 "   }\n\n"

			 "   // methods\n"
			 "   public void _marshal(",?ERLANGPACKAGE,"OtpOutputStream out) throws java.lang.Exception {\n"
			 "      ",EnumName,"Helper.marshal(out, value);\n"
			 "   }\n\n"
			 
			 "   public void _unmarshal(",?ERLANGPACKAGE,"OtpInputStream in) throws java.lang.Exception {\n"
			 "      value = ",EnumName,"Helper.unmarshal(in);\n"
			 "   }\n\n"
			 "}\n"]),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_helper_class/4
%%-----------------------------------------------------------------
emit_helper_class(G, N, X, EnumName) ->
    EName = string:concat(EnumName, "Helper"),
    WEList = enum_member_atom_list(G, N, X),
    {Fd, _} = ic_file:open_java_file(G, N, EName), 
    
    ic_codegen:emit(Fd, ["public class ",EnumName,"Helper {\n\n"

			 "   // constructors\n"
			 "   private ",EnumName,"Helper() {}\n\n"
    
			 "   // methods\n"

			 "   public static void marshal(",?ERLANGPACKAGE,"OtpOutputStream _out, ",EnumName," _value)\n"
			 "     throws java.lang.Exception {\n\n"]),
    
    emit_enum_write_function(G, N, X, Fd, EnumName),
    
    ic_codegen:emit(Fd, ["   }\n\n"

			 "   public static ",EnumName," unmarshal(",?ERLANGPACKAGE,"OtpInputStream _in)\n"
			 "     throws java.lang.Exception {\n\n"]),

    emit_enum_read_function(G, N, X, Fd, EnumName),

    ic_codegen:emit(Fd, "\n   }\n\n"),

    emit_enum_private_member_variables(Fd, WEList),

    ic_codegen:emit(Fd, ["\n   // Get integer value of enum from string\n"
			 "   private static int _getIntFromName(String name) throws java.lang.Exception {\n" 
			 "      for(int i = 0; i < _memberCount; i++) {\n"
			 "         if (name.equals(_members[i]))\n"
			 "            return i;\n"
			 "      }\n"
			 "      throw new java.lang.Exception(\"\");\n"
			 "   }\n\n"
			 
			 "   public static String id() {\n"
			 "      return \"",ictk:get_IR_ID(G, N, X),"\";\n"
			 "   }\n\n"
			 
			 "   public static String name() {\n" 
			 "      return \"",EnumName,"\";\n"
			 "   }\n\n"]),
    
    ic_jbe:emit_type_function(G, N, X, Fd),

    ic_codegen:emit(Fd, ["   public static void insert(",?ICPACKAGE,"Any _any, ",EnumName," _this)\n"
			 "     throws java.lang.Exception {\n\n"
			 
			 "     ",?ERLANGPACKAGE,"OtpOutputStream _os = \n"
			 "       new ",?ERLANGPACKAGE,"OtpOutputStream();\n\n"
			 
			 "     _any.type(type());\n"
			 "     marshal(_os, _this);\n"
			 "     _any.insert_Streamable(_os);\n"
			 "   }\n\n"
			 
			 "   public static ",EnumName," extract(",?ICPACKAGE,"Any _any)\n"
			 "     throws java.lang.Exception {\n\n"
			 
			 "     return unmarshal(_any.extract_Streamable());\n"
			 "   }\n\n"
			 
			 "}\n"]),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_enum_public_instance_variables/6
%%-----------------------------------------------------------------
emit_enum_public_instance_variables(_G, _N, _X, _Fd, _EnumName, []) ->
    ok;
emit_enum_public_instance_variables(G, N, X, Fd, EnumName, [Enumerator |EList]) ->
    ic_codegen:emit(Fd, ["   public static final ",EnumName," ",Enumerator," = new ",EnumName,"(_",Enumerator,");\n"]),
    emit_enum_public_instance_variables(G, N, X, Fd, EnumName, EList).

%%-----------------------------------------------------------------
%% Func:  emit_enum_member_int_values_initialization/5
%%-----------------------------------------------------------------
emit_enum_member_int_values_initialization(G, N, X, Fd, EList) ->
    InitString = emit_enum_member_int_values_initialization_1(G, N, X, Fd, EList, 0),
    ic_codegen:emit(Fd, ["   public static final int ",InitString,";\n"]).


%%-----------------------------------------------------------------
%% Func:  emit_enum_member_int_values_initialization_1/6
%%-----------------------------------------------------------------
emit_enum_member_int_values_initialization_1(_G, _N, _X, _Fd, [Enumerator], Num) ->
    "                           _" ++ Enumerator ++ " = " ++ ic_util:to_list(Num);
emit_enum_member_int_values_initialization_1(G, N, X, Fd, [Enumerator |EList], Num) ->
    Spaces = if
	Num == 0 ->
	    "";
	true ->
	    "                           "
    end,
    Spaces ++ "_" ++ Enumerator ++ " = " ++ ic_util:to_list(Num) ++ ",\n" ++
	emit_enum_member_int_values_initialization_1(G, N, X, Fd, EList, Num + 1).

%%-----------------------------------------------------------------
%% Func:  emit_enum_from_int_function/6
%%-----------------------------------------------------------------
emit_enum_from_int_function(_G, _N, _X, Fd, EnumName, EList) ->
    ic_codegen:emit(Fd, 
		    ["   public static final ",EnumName," from_int(int __value)  throws java.lang.Exception {\n"
		     "      switch (__value) {\n"]),
    emit_enum_from_int_function_switchbody(Fd, EList),
    ic_codegen:emit(Fd, ["      }\n"
			 "   }\n"]).
    
%%-----------------------------------------------------------------
%% Func:  emit_enum_from_int_function_switchbody/2
%%-----------------------------------------------------------------
emit_enum_from_int_function_switchbody(Fd, []) ->
    ic_codegen:emit(Fd, ["         default:\n"
			 "            throw new java.lang.Exception(\"\");\n"]);
emit_enum_from_int_function_switchbody(Fd, [Enumerator |EList]) ->
    ic_codegen:emit(Fd, ["         case _",Enumerator,":\n"
			 "            return ",Enumerator,";\n"]),    
    emit_enum_from_int_function_switchbody(Fd, EList).

%%-----------------------------------------------------------------
%% Func:  emit_enum_private_member_variables/2
%%-----------------------------------------------------------------
emit_enum_private_member_variables(Fd, EList) ->
    ic_codegen:emit(Fd, ["   private static final int _memberCount = ",integer_to_list(length(EList)),";\n"
			 "   private static String[] _members  = {\n"]),
    emit_enum_private_member_variables_1(Fd, EList),
    ic_codegen:emit(Fd, "   };\n").

%%-----------------------------------------------------------------
%% Func:  emit_enum_private_member_variables_1/2
%%-----------------------------------------------------------------
emit_enum_private_member_variables_1(Fd, [Enumerator]) ->
    ic_codegen:emit(Fd, ["      \"",Enumerator,"\"\n"]);
emit_enum_private_member_variables_1(Fd, [Enumerator |EList]) ->
    ic_codegen:emit(Fd, ["      \"",Enumerator,"\",\n"]),
    emit_enum_private_member_variables_1(Fd, EList).

%%-----------------------------------------------------------------
%% Func:  emit_enum_read_function/5
%%-----------------------------------------------------------------
emit_enum_read_function(_G, _N, _X, Fd, EnumName) ->
    ic_codegen:emit(Fd, ["     return ",EnumName,".from_int(_getIntFromName(_in.read_atom()));"]).

%%-----------------------------------------------------------------
%% Func:  emit_enum_write_function/5
%%-----------------------------------------------------------------
emit_enum_write_function(_G, _N, _X, Fd, _EnumName) ->
    ic_codegen:emit(Fd, "     _out.write_atom(_members[_value.value()]);\n").


%%-----------------------------------------------------------------
%% Func:  enum_member_name_list/3
%%
%% Note: The names generated are checked for name coalition 
%%       with java keywords. If so the name is always prefixed
%%       by "_"
%%-----------------------------------------------------------------
enum_member_name_list(_G, _N, X) ->
    lists:map(
      fun(Enumerator) -> 
	      ic_forms:get_java_id(Enumerator)
      end,
      ic_forms:get_body(X)).

%%-----------------------------------------------------------------
%% Func:  enum_member_atom_list/3
%%
%% Note : Similar to the emit_member_list/3 but does not
%%        solves name coalitions with java keywords.
%%        Used for wire encoding only 
%%-----------------------------------------------------------------
enum_member_atom_list(_G, _N, X) ->
    lists:map(
      fun(Enumerator) -> 
	      ic_forms:get_id2(Enumerator)
      end,
      ic_forms:get_body(X)).








