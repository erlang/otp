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


-module(ic_union_java).

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
gen(G, N, X) when is_record(X, union) ->

    %% Create a TK value if not existed
    %% Should be integrated in fetchTk
    %% instead
    NewX = case ic_forms:get_tk(X) of
	       undefined ->
		   S = ic_genobj:tktab(G),
		   Tk = ictype:tk(G, S, N, X),
		   #union{ id = X#union.id,
			   type = X#union.type,
			   body = X#union.body,
			   tk = Tk };
	       _Tk ->
		   X
	   end,
    
    UnionName = ic_forms:get_java_id(NewX),
    WiredUnionName = ic_forms:get_id2(NewX),
    N2 = [UnionName ++ "Package"|N],  
    %%?PRINTDEBUG2("Recursive call over type ~p",
    %%		 [[ic_forms:get_type(NewX)]]),
    ic_jbe:gen(G, N, [ic_forms:get_type(NewX)]),
    %%?PRINTDEBUG2("Recursive call over body: ~p", 
    %%		 [ic_forms:get_body(NewX)]),
    ic_jbe:gen(G, N2, ic_forms:get_body(NewX)),

    emit_union_class(G, N, NewX, UnionName),
    emit_holder_class(G, N, NewX, UnionName),
    emit_helper_class(G, N, NewX, UnionName, WiredUnionName);
gen(_G, _N, _X) -> 
    ok.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: emit_union_class/4
%%-----------------------------------------------------------------
emit_union_class(G, N, X, UnionName) ->
    {Fd, _} = ic_file:open_java_file(G, N, UnionName), 

    DiscrType = ic_java_type:getType(G, [UnionName ++ "Package"|N],
				  ic_forms:get_type(X)),
    
    MList = union_member_list(G, N, X, DiscrType),

    ic_codegen:emit(Fd, "final public class ~s {\n",[UnionName]),

    ic_codegen:emit(Fd, "   // instance variables\n", []),
    ic_codegen:emit(Fd, "   private boolean _initialized;\n", []),
    ic_codegen:emit(Fd, "   private ~s _discriminator;\n", [DiscrType]),
    ic_codegen:emit(Fd, "   private java.lang.Object _value;\n", []),

    {tk_union,_, _,DiscrTk, _, _} = ic_forms:get_tk(X),

    DV = get_default_val(G, [UnionName |N], DiscrType, DiscrTk, MList),

    case DV of
	none -> %% all values in case
	    ok;
	_ ->
	    ic_codegen:emit(Fd, "   private ~s _default =  ~s;\n",
			    [DiscrType, DV])
    end,
	    
    ic_codegen:nl(Fd),
    ic_codegen:emit(Fd, "   // constructors\n", []),

    ic_codegen:emit(Fd, "   public ~s() {\n", [UnionName]),
    ic_codegen:emit(Fd, "      _initialized = false;\n", []),
    ic_codegen:emit(Fd, "      _value = null;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // discriminator access\n", []),

    ic_codegen:emit(Fd, "   public ~s discriminator() "
		    "throws java.lang.Exception {\n", [DiscrType]),
    ic_codegen:emit(Fd, "      if (!_initialized) {\n", []),
    ic_codegen:emit(Fd, "         throw new java.lang.Exception(\"\");\n",[]),
    ic_codegen:emit(Fd, "      }\n", []),
    ic_codegen:emit(Fd, "      return _discriminator;\n", []),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
 
    emit_union_members_functions(G, [UnionName ++ "Package"|N], X,
				 Fd, UnionName, DiscrType, MList, MList),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "}\n", []),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_holder_class/4
%%-----------------------------------------------------------------
emit_holder_class(G, N, _X, UnionName) ->
    UName = string:concat(UnionName, "Holder"),
    {Fd, _} = ic_file:open_java_file(G, N, UName), 
    
    ic_codegen:emit(Fd, "final public class ~sHolder {\n",[UnionName]),

    ic_codegen:emit(Fd, "   // instance variables\n"),
    ic_codegen:emit(Fd, "   public ~s value;\n", [UnionName]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // constructors\n"),
    ic_codegen:emit(Fd, "   public ~sHolder() {}\n", [UnionName]),
    ic_codegen:emit(Fd, "   public ~sHolder(~s initial) {\n", 
		    [UnionName, UnionName]),
    ic_codegen:emit(Fd, "      value = initial;\n"),
    ic_codegen:emit(Fd, "   }\n"),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // methods\n"),

    ic_codegen:emit(Fd, "   public void _marshal(~sOtpOutputStream out) throws java.lang.Exception {\n",
		    [?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "      ~sHelper.marshal(out, value);\n", [UnionName]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public void _unmarshal(~sOtpInputStream in) throws java.lang.Exception {\n",
		    [?ERLANGPACKAGE]),  
    ic_codegen:emit(Fd, "      value = ~sHelper.unmarshal(in);\n", [UnionName]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "}\n"),
    file:close(Fd).


%%-----------------------------------------------------------------
%% Func:  emit_helper_class/4
%%-----------------------------------------------------------------
emit_helper_class(G, N, X, UnionName, WiredUnionName) ->
    UName = string:concat(UnionName, "Helper"),
    {Fd, _} = ic_file:open_java_file(G, N, UName), 

    DiscrType = ic_java_type:getType(G, [ UnionName ++ "Package" |N],
				  ic_forms:get_type(X)),

    ic_codegen:emit(Fd, "public class ~sHelper {\n",[UnionName]),

    ic_codegen:emit(Fd, "   // constructors\n", []),
    ic_codegen:emit(Fd, "   private ~sHelper() {}\n", [UnionName]),
    ic_codegen:nl(Fd),

    ic_codegen:emit(Fd, "   // methods\n", []),
    MList = union_member_list(G, N, X, DiscrType),

    ic_codegen:emit(Fd, "   public static void marshal(~sOtpOutputStream _out, ~s _value)\n",
		    [?ERLANGPACKAGE, UnionName]), 
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
    emit_union_marshal_function(G, N, X, Fd, UnionName, WiredUnionName, MList),
    ic_codegen:emit(Fd, "   }\n\n"),
    
    ic_codegen:emit(Fd, "   public static ~s unmarshal(~sOtpInputStream _in)\n",
		    [UnionName, ?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
    emit_union_unmarshal_function(G, N, X, Fd, UnionName, WiredUnionName, MList),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static String id() {\n"), 
    ic_codegen:emit(Fd, "      return ~p;\n",[ictk:get_IR_ID(G, N, X)]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static String name() {\n"), 
    ic_codegen:emit(Fd, "      return ~p;\n",[UnionName]),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_jbe:emit_type_function(G, N, X, Fd),


    ic_codegen:emit(Fd, "   public static void insert(~sAny _any, ~s _this)\n",
		    [?ICPACKAGE,UnionName]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
   
    ic_codegen:emit(Fd, "     ~sOtpOutputStream _os = \n",[?ERLANGPACKAGE]),
    ic_codegen:emit(Fd, "       new ~sOtpOutputStream();\n\n",[?ERLANGPACKAGE]), 
    
    ic_codegen:emit(Fd, "     _any.type(type());\n"),     
    ic_codegen:emit(Fd, "     marshal(_os, _this);\n"),
    ic_codegen:emit(Fd, "     _any.insert_Streamable(_os);\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static ~s extract(~sAny _any)\n",
		    [UnionName,?ICPACKAGE]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n\n"),
  
    ic_codegen:emit(Fd, "     return unmarshal(_any.extract_Streamable());\n"),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "   public static int discriminatorAsInt(~s _discriminator)\n",
		    [DiscrType]),
    ic_codegen:emit(Fd, "     throws java.lang.Exception {\n"),
    emit_discriminator_as_int(G, N, ic_forms:get_type(X), Fd),
    ic_codegen:emit(Fd, "   }\n\n"),

    ic_codegen:emit(Fd, "}\n"),
    file:close(Fd).

%%-----------------------------------------------------------------
%% Func:  emit_union_members_functions/7
%%-----------------------------------------------------------------
emit_union_members_functions(_, _, _, _, _, _, [], _) ->
    ok;
emit_union_members_functions(G, N, X, Fd, UnionName, DiscrType,
			     [{Label, Case, TypeDef, Id, Ls} | MList], MListTot) ->

    CaseId = Case#case_dcl.id, %% Maybe Array
    CaseType = Case#case_dcl.type, %% Maybe Sequence

    Type = if element(1,CaseId) == array ->
		   ic_java_type:getType(G, N, TypeDef) ++
		       ic_java_type:getdim(CaseId#array.size);
	      true ->
		   ic_java_type:getType(G, N, TypeDef)
	   end,

    HolderType = 
	if element(1,CaseId) == array ->
		ic_java_type:getHolderType(G, N, CaseId);
	   true ->
		if element(1,CaseType) == sequence ->
			ic_util:to_dot(G,[Id|N]) ++"Holder";
		   true ->
			ic_java_type:getHolderType(G, N, TypeDef)
		end
	end,

    %%
    %% Set method
    %%	    
    ic_codegen:emit(Fd, "   // ~s access and set functions\n",[Id]),
    ic_codegen:emit(Fd, "   public void ~s(~s value) "
		    "throws java.lang.Exception {\n",
		    [Id, Type]),
    ic_codegen:emit(Fd, "      _initialized = true;\n", []),
    case Label of
	"default" ->
	    ic_codegen:emit(Fd, "      _discriminator = (~s) _default;\n",
			    [DiscrType]);
	_ ->
	    case ic_java_type:isBasicType(G, N, ic_forms:get_type(X)) of
		true ->
		    ic_codegen:emit(Fd, "      _discriminator = (~s) "
				    "~s;\n",
				    [DiscrType, Label]);
		_ ->
		    ic_codegen:emit(Fd, "      _discriminator = (~s) "
				    "~s.~s;\n",
				    [DiscrType, DiscrType, Label])
	    end
    end,
    ic_codegen:emit(Fd, "      _value = new ~s(value);\n",
		    [HolderType]),
    ic_codegen:emit(Fd, "   }\n", []),

    %%
    %% Check this entry has more than one label and the generate an extra set method.
    %%
    case Ls of
	[] ->
	    ok;
	_ ->
	    ic_codegen:emit(Fd, "   public void ~s(~s discriminator, ~s value) "
			    "throws java.lang.Exception {\n",
			    [Id, DiscrType, Type]),
	    ic_codegen:emit(Fd, "      _initialized = true;\n", []),
	    ic_codegen:emit(Fd, "      _discriminator = (~s) discriminator;\n",
				    [DiscrType]),
	    ic_codegen:emit(Fd, "      _value = new ~s(value);\n",
			    [HolderType]),
	    ic_codegen:emit(Fd, "   }\n", [])
    end,
    
    %%
    %% Get method
    %%	    
    ic_codegen:emit(Fd, "   public ~s ~s() throws java.lang.Exception {\n",
		    [Type, Id]),
    ic_codegen:emit(Fd, "      if (!_initialized) {\n", []),
    ic_codegen:emit(Fd, "         throw new java.lang.Exception(\"\");\n",[]),
    ic_codegen:emit(Fd, "      }\n", []),
    ic_codegen:emit(Fd, "      switch (~sHelper.discriminatorAsInt"
		    "(discriminator())) {\n",
		    [UnionName]),
    if
	Label == "default" ->
	    ic_codegen:emit(Fd, "         default:\n", []),
	    ic_codegen:emit(Fd, "            break;\n", []),
	    emit_default_access_fun_switch_cases(G, N, X, Fd, DiscrType,
						 MListTot),
	    ic_codegen:emit(Fd, "            throw new java.lang.Exception(\"\");\n", []);
	true ->
	    ic_codegen:emit(Fd, "         case ~s:\n",
			    [get_case_as_int(G, N, ic_forms:get_type(X),
					     DiscrType, Label)]),
	    ic_codegen:emit(Fd, "            break;\n", []),
	    ic_codegen:emit(Fd, "         default:\n", []),
	    ic_codegen:emit(Fd, "            throw new java.lang.Exception(\"\");\n", [])
    end,
    ic_codegen:emit(Fd, "      }\n", []),
	    
    ic_codegen:emit(Fd, "      return ((~s) _value).value;\n",
		    [HolderType]),
    ic_codegen:emit(Fd, "   }\n", []),
    ic_codegen:nl(Fd),
    emit_union_members_functions(G, N, X, Fd, UnionName, DiscrType, MList,
				 MListTot).


%%-----------------------------------------------------------------
%% Func:  emit_default_access_fun_switch_cases/6
%%-----------------------------------------------------------------
emit_default_access_fun_switch_cases(_G, _N, _X, _Fd, _DiscrType, []) ->
    ok;
emit_default_access_fun_switch_cases(G, N, X, Fd, DiscrType,
				     [{"default", _, _, _, _} |MList]) ->
    emit_default_access_fun_switch_cases(G, N, X, Fd, DiscrType, MList);
emit_default_access_fun_switch_cases(G, N, X, Fd, DiscrType,
				     [{Label, _Case, _TypeDef, _Id, _} | MList]) ->
    ic_codegen:emit(Fd, "         case ~s:\n",
		    [get_case_as_int(G, N, ic_forms:get_type(X),
				     DiscrType, Label)]),
    emit_default_access_fun_switch_cases(G, N, X, Fd, DiscrType, MList).



%%-----------------------------------------------------------------
%% Func:  emit_union_unmarshal_function/5
%%-----------------------------------------------------------------
emit_union_unmarshal_function(G, N, X, Fd, UnionName, WiredUnionName, MList) ->
    DiscrTypeForm = ic_forms:get_type(X),
    DiscrType = ic_java_type:getType(G, [UnionName ++ "Package"|N],
				  DiscrTypeForm),

    ic_codegen:emit(Fd, "     _in.read_tuple_head();\n\n"),

    ic_codegen:emit(Fd, "     if ((_in.read_atom()).compareTo(~p) != 0)\n",
	 	    [ic_util:to_undersc([WiredUnionName|N])]),
    ic_codegen:emit(Fd, "       throw new java.lang.Exception(\"\");\n\n",[]),

    ic_codegen:emit(Fd, "     ~s _value = new ~s();\n", [UnionName, UnionName]),
    
    %% Decode discriminator
    case ic_java_type:isBasicType(G, N, DiscrTypeForm) of
	true ->
	    ic_codegen:emit(Fd, "     ~s _discriminator = _in~s;\n\n",
			    [DiscrType,
			     ic_java_type:unMarshalFun(G, N, X, DiscrTypeForm)]);
	_ ->
	    ic_codegen:emit(Fd, "     ~s _discriminator = ~s.unmarshal(_in);\n\n",
			    [DiscrType,ic_java_type:getUnmarshalType(G, N, X, DiscrTypeForm)])
    end,

    ic_codegen:emit(Fd, "     switch (~sHelper.discriminatorAsInt(_discriminator)) {\n",
		    [UnionName]),

    emit_union_unmarshal_function_loop(G, [UnionName ++ "Package"|N], X,
				       Fd, DiscrType, MList),

    ic_codegen:emit(Fd, "     }\n\n"),

    ic_codegen:emit(Fd, "     return _value;\n").

%%-----------------------------------------------------------------
%% Func:  emit_union_unmarshal_function_loop/6
%%-----------------------------------------------------------------
emit_union_unmarshal_function_loop(_, _, _, _, _, []) ->
    ok;
emit_union_unmarshal_function_loop(G, N, X, Fd, DiscrType,
			       [{Label, Case, Type, Id, Ls} |MList]) ->
    case Label of
	"default" ->
	    ic_codegen:emit(Fd, "     default:\n");	  
	_ ->
	    ic_codegen:emit(Fd, "     case ~s:\n",
			    [get_case_as_int(G, N, ic_forms:get_type(X),
					     DiscrType, Label)])
    end,
    
    gen_multiple_cases(G, N, X, Fd, DiscrType, Ls),

    CaseId = Case#case_dcl.id, %% Maybe Array
    CaseType = Case#case_dcl.type, %% Maybe Sequence

    case element(1,CaseId) of
	array ->
	    ic_codegen:emit(Fd, "       _value.~s(~s.unmarshal(_in));\n",
			    [Id,
			     ic_java_type:getUnmarshalType(G, N, Case, CaseId)]);
	
	_ ->
	    case element(1, CaseType) of
		sequence ->
		    ic_codegen:emit(Fd, "       _value.~s(~s.unmarshal(_in));\n",
				    [Id,
				     ic_java_type:getUnmarshalType(G, N, Case, CaseType)]);
		_ ->
		    case ic_java_type:isBasicType(G, N, CaseType) of
			true ->
			    ic_codegen:emit(Fd, "       _value.~s(_in~s);\n",
					    [Id,
					     ic_java_type:unMarshalFun(G, N, X, Type)]);
			false ->
			    ic_codegen:emit(Fd, "       _value.~s(~s.unmarshal(_in));\n",
					    [Id,
					     ic_java_type:getUnmarshalType(G, N, X, Type)])
		    end
	    end
    end,

    ic_codegen:emit(Fd, "       break;\n", []),
    emit_union_unmarshal_function_loop(G, N, X, Fd, DiscrType, MList).
			       




%%-----------------------------------------------------------------
%% Func:  emit_union_marshal_function/6
%%-----------------------------------------------------------------
emit_union_marshal_function(G, N, X, Fd, UnionName, WiredUnionName, MList) ->

    DiscrTypeForm = ic_forms:get_type(X),
    DiscrType = ic_java_type:getType(G, [UnionName ++ "Package" |N],
				  DiscrTypeForm),

    ic_codegen:emit(Fd, "     _out.write_tuple_head(3);\n"),
    ic_codegen:emit(Fd, "     _out.write_atom(~p);\n", 
		    [ic_util:to_undersc([WiredUnionName|N])]),

    case ic_java_type:isBasicType(G, N, DiscrTypeForm) of
	true ->
	    ic_codegen:emit(Fd, "     _out~s(_value.discriminator());\n\n",
			    [ic_java_type:marshalFun(G, N, X, DiscrTypeForm)]);
	false ->
	    ic_codegen:emit(Fd, "     ~s(_out, _value.discriminator());\n\n",
			    [ic_java_type:marshalFun(G, N, X, DiscrTypeForm)])
    end,

    ic_codegen:emit(Fd, "     switch(~sHelper.discriminatorAsInt(_value.discriminator())) {\n",
		    [UnionName]),

    emit_union_marshal_function_loop(G, 
				     [ UnionName ++ "Package"|N], 
				     X,
				     Fd, 
				     DiscrType, 
				     MList),
    
    ic_codegen:emit(Fd, "     }\n\n", []).


%%-----------------------------------------------------------------
%% Func:  emit_union_marshal_function_loop/
%%-----------------------------------------------------------------
emit_union_marshal_function_loop(_, _, _, _, _, []) ->
    ok;
emit_union_marshal_function_loop(G, N, X, Fd, DiscrType,
			       [{Label, Case, Type, Id, Ls} |MList]) ->
    case Label of
	"default" ->
	    ic_codegen:emit(Fd, "     default:\n",
			    []);
	_ ->
	    ic_codegen:emit(Fd, "     case ~s:\n",
			    [get_case_as_int(G, N, ic_forms:get_type(X),
					     DiscrType, Label)])
    end,

    gen_multiple_cases(G, N, X, Fd, DiscrType, Ls),


    CaseId = Case#case_dcl.id, %% Maybe Array
    CaseType = Case#case_dcl.type, %% Maybe Sequence

    case element(1,CaseId) of
	array ->
	    ic_codegen:emit(Fd, "       ~s(_out, _value.~s());\n",
			    [ic_java_type:marshalFun(G, N, Case, CaseId),
			     Id]);
	_ ->
	    case element(1, CaseType) of
		sequence ->
		    ic_codegen:emit(Fd, "       ~s.marshal(_out, _value.~s());\n",
				    [ic_util:to_dot(G,[Id|N]) ++ "Helper",
				     Id]);
		_ ->
		    case ic_java_type:isBasicType(G, N, CaseType) of
			true ->
			    ic_codegen:emit(Fd, "       _out~s(_value.~s());\n",
					    [ic_java_type:marshalFun(G, N, X, Type),
					     Id]);
			false ->
			    ic_codegen:emit(Fd, "       ~s(_out, _value.~s());\n",
					    [ic_java_type:marshalFun(G, N, X, Type),
					     Id])
		    end
	    end
    end,
    
    ic_codegen:emit(Fd, "       break;\n", []),
    emit_union_marshal_function_loop(G, N, X, Fd, DiscrType, MList).
			       
  

gen_multiple_cases(_G, _N, _X, _Fd, _DiscrType, []) ->
    ok;
gen_multiple_cases(G, N, X, Fd, DiscrType, [Label |Ls]) ->
    ic_codegen:emit(Fd, "        case ~s:\n",
		    [get_case_as_int(G, N, ic_forms:get_type(X),
				     DiscrType, getLabel(DiscrType, Label))]),
    gen_multiple_cases(G, N, X, Fd, DiscrType, Ls).


%%-----------------------------------------------------------------
%% Func:  union_member_list/3
%%-----------------------------------------------------------------
union_member_list(G, N, X, DiscrType) ->
    M = lists:map(
	  fun(Case) -> 
		  {Label, LabelList} = case  check_default(ic_forms:get_idlist(Case)) of
					   {{default, C}, List}  ->
					       {{default, C}, List};
					   {L, []} ->
					       {L, []};
					   {_, [L |Ls]} ->
					       {L, Ls}
				       end,

		  CName = ic_forms:get_java_id(Case),
		  CId = Case#case_dcl.id,
		  CType = Case#case_dcl.type,

		  if element(1,CId) == array ->
			  N2 = [ic_forms:get_id2(X) ++ "Package" |N],  
			  ic_array_java:gen(G, N2, Case, CId);
		     true ->
			  if element(1,Case#case_dcl.type) == sequence ->
				  N2 = [ic_forms:get_id2(X) ++ "Package" |N],  
				  ic_sequence_java:gen(G, N2, CType, CName);
			     true ->
				  ok
			  end
		  end,
   
		  {getLabel(DiscrType, Label),
		   Case,
		   ic_forms:get_type(Case),
		   CName,
		   LabelList}
	  end,
	  ic_forms:get_body(X)),
    lists:flatten(M).

check_default([]) ->
    {false, []};
check_default([{default, X} |Ls]) ->
    {{default, X}, Ls};
check_default([L]) ->
    {false, [L]};
check_default([L |Ls]) ->
    {X, Y} = check_default(Ls),
    {X, [L | Y]}.

getLabel(_, {'<integer_literal>', _, N}) ->
    N;
getLabel(_, {'<character_literal>', _, N}) ->
    "'" ++ N ++ "'";
getLabel(_, {'<wcharacter_literal>', _, N}) ->
    "'" ++ N ++ "'";
getLabel(_, {'TRUE',_}) ->
    "true";
getLabel(_, {'FALSE',_}) ->
    "true";
getLabel(_, {default, _}) ->
    "default";
getLabel(_DiscrType, X) -> %%DiscrType ++ "." ++ 
    ic_util:to_dot(ic_forms:get_id(X)).

get_default_val(G, N, _, tk_short, MList) ->
    integer_default_val(G, N, 1, lists:map(fun({V, _, _, _, _}) -> V end, MList));
get_default_val(G, N, _, tk_long, MList) ->
    integer_default_val(G, N, 1, lists:map(fun({V, _, _, _, _}) -> V end, MList));
get_default_val(G, N, _, tk_ushort, MList) ->
    integer_default_val(G, N, 1, lists:map(fun({V, _, _, _, _}) -> V end, MList));
get_default_val(G, N, _, tk_ulong, MList) ->
    integer_default_val(G, N, 1, lists:map(fun({V, _, _, _, _}) -> V end, MList));
get_default_val(G, N, _, tk_char, MList) ->
    char_default_val(G, N, $a, lists:map(fun({V, _, _, _, _}) -> V end, MList));
get_default_val(G, N, _, tk_boolean, MList) ->
    boolean_default_val(G, N, lists:map(fun({V, _, _, _, _}) -> V end, MList));
get_default_val(G, N, DiscrType, {tk_enum, _, _, Values}, MList) ->
    enum_default_val(G, N, DiscrType, Values, MList).

integer_default_val(G, N, Num, MList) ->
    Num2 = integer_to_list(Num),
    case lists:member(Num2, MList) of
	true ->
	    integer_default_val(G, N, Num + 1, MList);
	false ->
	    Num2
    end.

char_default_val(G, N, CharNum, MList) ->
    Str = "'",
    CharNum2 = Str ++ [CharNum | Str],
    case lists:member(CharNum2, MList) of
	true ->
	    char_default_val(G, N, CharNum + 1, MList);
	false ->
	    CharNum2
    end.

boolean_default_val(G, N, MList) ->
    if
	length(MList) > 2 ->
	    ic_error:error(G, {plain_error_string,
			       lists:flatten(
				 io_lib:format("Default value found while all values have label on ~s",
					       [ic_util:to_colon(N)]))}),
	    none;
	true ->
	    case MList of
		["true"] ->
		    "false";
		["false"] ->
		    "true";
		["default","true"] ->
		    "false";
		["true","default"] ->
		    "false";
		["default","false"] ->
		    "true";
		["false","default"] ->
		    "true";
		_ ->
		    none
	    end
    end.




enum_default_val(G, N, DiscrType, Values, Mlist) ->

    VLen = length(Values),
    MLen = length(Mlist),

    case MLen > VLen of
	true ->
	    ic_error:error(G, {plain_error_string,
			       lists:flatten(
				 io_lib:format("Default value found while all values have label on ~s",
					       [ic_util:to_colon(N)]))}),
	    none;
	false ->
	    enum_default_val_loop(G, N, DiscrType, Values, Mlist)
    end.

enum_default_val_loop(_G, _N, _, [], []) ->
    none;
enum_default_val_loop(_G, _N, DiscrType, [Value| _], []) ->
    DiscrType ++ "." ++ Value;
enum_default_val_loop(G, N, DiscrType, Values, [Case | MList]) when is_tuple(Case) ->
    NewValues = lists:delete(element(1,Case), Values),
    enum_default_val_loop(G, N, DiscrType, NewValues, MList).



emit_discriminator_as_int(G, N, T, Fd) ->
    case ictype:isBoolean(G,N,T) of
	true ->
	    ic_codegen:emit(Fd, "      if(_discriminator)\n", []),
	    ic_codegen:emit(Fd, "         return 1;\n", []),
	    ic_codegen:emit(Fd, "      else\n", []),
	    ic_codegen:emit(Fd, "         return 0;\n", []);
	false ->
	    case ictype:isEnum(G, N, T) of
		true ->
		    ic_codegen:emit(Fd, "      return _discriminator.value();\n",
				    []);
		false ->
		    ic_codegen:emit(Fd, "      return _discriminator;\n", [])
	    end
    end.


get_case_as_int(G, N, T, DiscrJavaTypeName, Label) ->
  case ictype:isBoolean(G,N,T) of
      true ->
	  case Label of 
	      "true" ->
		  "1";
	      "false" ->
		  "0"
	  end;
      false ->
	  case ictype:isEnum(G, N, T) of
	      true ->
		  DiscrJavaTypeName ++ "._" ++ Label;
	      false ->
		  "(" ++ DiscrJavaTypeName ++ ") " ++ Label
	  end
  end.
  


