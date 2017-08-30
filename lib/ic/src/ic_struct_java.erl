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

-module(ic_struct_java).

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
gen(G, N, X) when is_record(X, struct) ->
    StructName = ic_forms:get_java_id(X),
    WireStructName = ic_forms:get_id2(X),
    emit_struct_class(G, N, X, StructName),
    emit_holder_class(G, N, X, StructName),
    emit_helper_class(G, N, X, StructName, WireStructName),
    N2 = [StructName ++ "Package" |N],  
    ic_jbe:gen(G, N2, ic_forms:get_body(X));
gen(_G, _N, _X) -> 
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func:  emit_struct_class/4
%%-----------------------------------------------------------------
emit_struct_class(G, N, X, StructName) ->
    {Fd, _}= ic_file:open_java_file(G, N, StructName), 
    
    MList = struct_member_list(G, N, X),
    ArgList = gen_parameter_list(G, [ StructName ++ "Package" |N], X, MList), 

    ic_codegen:emit(Fd, ["final public class ",StructName," {\n"
			 "   // instance variables\n"]),

    emit_struct_members_declarations(G, [StructName ++ "Package" |N],
				     X, Fd, MList),

    ic_codegen:emit(Fd, ["\n   // constructors\n"
			 "   public ",StructName,"() {}\n\n"

			 "   public ",StructName,"(",ArgList,") {\n"]),
    
    emit_struct_members_initialisation(G, N, X, Fd, MList),
    
    ic_codegen:emit(Fd, ["   }\n\n"
			 
			 "}\n\n"]),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, _X, StructName) ->
    SName = string:concat(StructName, "Holder"),
    {Fd, _}= ic_file:open_java_file(G, N, SName), 
    
    ic_codegen:emit(Fd, ["final public class ",StructName,"Holder {\n"

			 "   // instance variables\n"
			 "   public ",StructName," value;\n\n"

			 "   // constructors\n"
			 "   public ",StructName,"Holder() {}\n\n"

			 "   public ",StructName,"Holder(",StructName," initial) {\n"
			 "      value = initial;\n"
			 "   }\n\n"

			 "   // methods\n"]),

    ic_codegen:emit(Fd, ["   public void _marshal(",?ERLANGPACKAGE,"OtpOutputStream out) throws java.lang.Exception {\n"
			 "      ",StructName,"Helper.marshal(out, value);\n"
			 "   }\n\n"
			 
			 "   public void _unmarshal(",?ERLANGPACKAGE,"OtpInputStream in) throws java.lang.Exception {\n"
			 "      value = ",StructName,"Helper.unmarshal(in);\n"
			 "   }\n"
			 
			 "}\n\n"]),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_helper_class/5
%%-----------------------------------------------------------------
emit_helper_class(G, N, X, StructName, WireStructName) ->
    SName = string:concat(StructName, "Helper"),
    {Fd, _}= ic_file:open_java_file(G, N, SName),
 
    ic_codegen:emit(Fd, ["public class ",StructName,"Helper {\n"
    
			 "   // constructors\n"
			 "   private ",StructName,"Helper() {}\n\n"

			 "   // methods\n"]),

    MList = struct_member_list(G, N, X),

    ic_codegen:emit(Fd, ["   public static void marshal(",?ERLANGPACKAGE,"OtpOutputStream _out, ",StructName," _value)\n"
			 "     throws java.lang.Exception {\n\n"]),

    emit_struct_marshal_function(G, N, X, Fd, StructName, WireStructName, MList),
    
    ic_codegen:emit(Fd, ["   }\n\n"

			 "   public static ",StructName," unmarshal(",?ERLANGPACKAGE,"OtpInputStream _in)\n"
			 "     throws java.lang.Exception {\n\n"]),

    emit_struct_unmarshal_function(G, N, X, Fd, StructName, WireStructName, MList),

    ic_codegen:emit(Fd, ["   }\n\n"

			 "   public static String id() {\n" 
			 "      return \"",ictk:get_IR_ID(G, N, X),"\";\n"
			 "   }\n\n"
			 
			 "   public static String name() {\n"
			 "      return \"",StructName,"\";\n"
			 "   }\n\n"]),
    
    ic_jbe:emit_type_function(G, N, X, Fd),

    ic_codegen:emit(Fd, ["   public static void insert(",?ICPACKAGE,"Any _any, ",StructName," _this)\n"
			 "     throws java.lang.Exception {\n\n"
   
			 "     ",?ERLANGPACKAGE,"OtpOutputStream _os = \n"
			 "       new ",?ERLANGPACKAGE,"OtpOutputStream();\n\n" 
			 
			 "     _any.type(type());\n"
			 "     marshal(_os, _this);\n"
			 "     _any.insert_Streamable(_os);\n"
			 "   }\n\n"
			 
			 "   public static ",StructName," extract(",?ICPACKAGE,"Any _any)\n"
			 "     throws java.lang.Exception {\n\n"
			 
			 "     return unmarshal(_any.extract_Streamable());\n"
			 "   }\n\n"
			 
			 
			 %% In corba mapping there is also a _type function here.
			 "}\n"]),
    file:close(Fd).

    
%%-----------------------------------------------------------------
%% Func: emit_struct_members_declarations/
%%-----------------------------------------------------------------
emit_struct_members_declarations(_, _, _, _, []) ->
    ok;
emit_struct_members_declarations(G, N, X, Fd, [{Member, _Type, Id} | MList]) ->
    ic_codegen:emit(Fd, ["   public ",ic_java_type:getType(G, N, Member)," ",Id,";\n"]),
    emit_struct_members_declarations(G, N, X, Fd, MList).



%%-----------------------------------------------------------------
%% Func: emit_struct_members_initialisation/5
%%-----------------------------------------------------------------
emit_struct_members_initialisation(_, _, _, _, []) ->
    ok;
emit_struct_members_initialisation(G, N, X, Fd, [{_Member, _Type, Id} | MList]) ->
    ic_codegen:emit(Fd, ["     ",Id," = _",Id,";\n"]),
    emit_struct_members_initialisation(G, N, X, Fd, MList).



			       
%%-----------------------------------------------------------------
%% Func: emit_struct_marshal_function/7
%%-----------------------------------------------------------------
emit_struct_marshal_function(G, N, X, Fd, StructName, WireStructName, MList) ->

    ic_codegen:emit(Fd, ["     _out.write_tuple_head(",integer_to_list(length(MList) + 1),");\n"
			 "     _out.write_atom(\"",ic_util:to_undersc([WireStructName|N]),"\");\n\n"]),

    emit_struct_marshal_function_loop(G, [StructName ++ "Package" |N],
				      X, Fd, MList, 1).

%%-----------------------------------------------------------------
%% Func: emit_struct_marshal_function_loop/6
%%-----------------------------------------------------------------
emit_struct_marshal_function_loop(_, _, _, Fd, [], _) ->
    ic_codegen:nl(Fd);
emit_struct_marshal_function_loop(G, N, X, Fd, [{Member, Type, Id} |MList], Num) ->
    
    case ic_java_type:isBasicType(G, N, Member) of
	true ->
	    ic_codegen:emit(Fd, ["     _out",ic_java_type:marshalFun(G, N, Member, Type),"(_value.",Id,");\n"]);
	_ ->
	    if (element(1,hd(element(3,Member))) == array) ->
		    ic_codegen:emit(Fd, 
				    ["     ",
				     ic_util:to_dot(G,[ic_forms:get_id2(Member)|N]),
				     "Helper.marshal(_out, _value.",Id,");\n"]);
	       true ->
		    ic_codegen:emit(Fd, ["     ",
					 ic_java_type:marshalFun(G, N, Member, Type),
					 "(_out, _value.",Id,");\n"])
	    end
    end,
    
    emit_struct_marshal_function_loop(G, N, X, Fd, MList, Num+1).




%%-----------------------------------------------------------------
%% Func: emit_struct_unmarshal_function/7
%%-----------------------------------------------------------------
emit_struct_unmarshal_function(G, N, X, Fd, StructName, WireStructName, MList) ->

    ic_codegen:emit(Fd, ["     _in.read_tuple_head();\n\n"
    
			 "     if ((_in.read_atom()).compareTo(\"",
			 ic_util:to_undersc([WireStructName|N]),
			 "\") != 0)\n"
			 "       throw new java.lang.Exception(\"\");\n\n"

			 "     ",StructName," _value = new ",StructName,"();\n"]),
    
    emit_struct_unmarshal_function_loop(G, [StructName ++ "Package"|N],
					X, Fd, MList, 1),
    
    ic_codegen:emit(Fd, "     return _value;\n").

%%-----------------------------------------------------------------
%% Func:  emit_union_unmarshal_function_loop/6
%%-----------------------------------------------------------------
emit_struct_unmarshal_function_loop(_, _, _, Fd, [], _) ->
    ic_codegen:nl(Fd);
emit_struct_unmarshal_function_loop(G, N, X, Fd, [{Member, Type, Id} |MList], Num) ->

    case ic_java_type:isBasicType(G, N, Member) of
	true ->
	    ic_codegen:emit(Fd, ["     _value.",Id," = _in",ic_java_type:unMarshalFun(G, N, Member, Type),";\n"]);
	_ ->
	    if (element(1,hd(element(3,Member))) == array) ->
		    ic_codegen:emit(Fd, 
				    ["     _value.",Id," = ",ic_util:to_dot(G,[ic_forms:get_id2(Member)|N]),"Helper.unmarshal(_in);\n"]);
	       true ->
		    ic_codegen:emit(Fd, 
				    ["     _value.",Id," = ",ic_java_type:getUnmarshalType(G, N, Member, Type),".unmarshal(_in);\n"])
	    end
    end,

    emit_struct_unmarshal_function_loop(G, N, X, Fd, MList, Num +1).



%%-----------------------------------------------------------------
%% Func: gen_parameter_list/4
%%-----------------------------------------------------------------
gen_parameter_list(G, N, _X, [{Member, _Type, Id}]) ->
    ic_java_type:getType(G,N,Member) ++
	" _" ++ 
	ic_util:to_list(Id);
gen_parameter_list(G, N, X, [{Member, _Type, Id} | MList]) ->
    ic_java_type:getType(G,N,Member) ++ 
	" _" ++
	ic_util:to_list(Id) ++ 
	", " ++
	gen_parameter_list(G, N, X, MList).


%%-----------------------------------------------------------------
%% Func: struct_member_list/3
%%-----------------------------------------------------------------
struct_member_list(_G, _N, X) ->
    M = lists:map(
	  fun(Member) -> 
		  lists:map(
		    fun(Id) ->
			    Type = ic_forms:get_type(Member),
			    { Member, Type, ic_forms:get_java_id(Id)}
		    end, 
		    ic_forms:get_idlist(Member))
	  end,
	  ic_forms:get_body(X)),
    lists:flatten(M).



