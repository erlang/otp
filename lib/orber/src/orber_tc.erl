%%--------------------------------------------------------------------
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
%% File: orber_tc.erl
%% Description:
%%    This file contains utility functions to create TypeCodes
%%
%%-----------------------------------------------------------------
-module(orber_tc).

-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([null/0, void/0, short/0, unsigned_short/0, 
	 long/0, longdouble/0, unsigned_long/0, long_long/0,
	 unsigned_long_long/0, float/0, double/0,
	 boolean/0, char/0, wchar/0, octet/0, any/0,
	 typecode/0, principal/0,
	 object_reference/2, struct/3, 
	 union/5, enum/3,
	 string/1, wstring/1, sequence/2, array/2, alias/3,
	 exception/3, fixed/2, value/5, value_box/3, native/2, abstract_interface/2,
	 get_tc/1, check_tc/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% A number of function which can be used to create TypeCodes
null() ->
    tk_null.
void() ->
    tk_void.
short() ->
    tk_short.
unsigned_short() ->
    tk_ushort.
long() ->
    tk_long.
unsigned_long() ->
    tk_ulong.
long_long() ->
    tk_longlong.
unsigned_long_long() ->
    tk_ulonglong.
float() ->
    tk_float.
double() ->
    tk_double.
longdouble() ->
    tk_longdouble.

boolean() ->
    tk_boolean.
char() ->
    tk_char.
wchar() ->
    tk_wchar.
octet() ->
    tk_octet.
any() ->
    tk_any.
typecode() ->
    tk_TypeCode.
principal() ->
    tk_Principal.

object_reference(Id, Name) ->
    {tk_objref, Id, Name}.

struct(Id, Name, ElementList) ->
    {tk_struct, Id, Name, ElementList}.

union(Id, Name, DiscrTC, Default, ElementList) ->
    {tk_union, Id, Name, DiscrTC, Default, ElementList}.

enum(Id, Name, ElementList) ->
    {tk_enum, Id, Name, ElementList}.

string(Length) ->
    {tk_string, Length}.

wstring(Length) ->
    {tk_wstring, Length}.

sequence(ElemTC, Length) ->
    {tk_sequence, ElemTC, Length}.

array(ElemTC, Length) ->
    {tk_array, ElemTC, Length}.

alias(Id, Name, TC) ->
    {tk_alias, Id, Name, TC}.

exception(Id, Name, ElementList) ->
    {tk_except, Id, Name, ElementList}.

fixed(Digits, Scale) ->
    {tk_fixed, Digits, Scale}.

value(RepId, Name, ValueModifier, TC, ElementList) ->
    {tk_value, RepId, Name, ValueModifier, TC, ElementList}.

value_box(RepId, Name, TC) ->
    {tk_value_box, RepId, Name, TC}.

native(RepId, Name) ->
    {tk_native, RepId, Name}.

abstract_interface(RepId, Name) ->
    {tk_abstract_interface, RepId, Name}.


%%-----------------------------------------------------------------
%% Get TypeCode (can be used for constructed types like structs, 
%% unions and exceptions)
%%
get_tc(T) when is_tuple(T) ->
    Type = element(1, T),
    case catch Type:tc() of
	{'EXIT', R} ->
	    orber:dbg("[~p] ~p:get_tc(~p); Exit: ~p",
		      [?LINE, ?MODULE, T, R], ?DEBUG_LEVEL),
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
	X ->
	    X
    end;
%% This call can be used if one have the IFR id and wants a typecode.
get_tc(IFRId) when is_list(IFRId) ->
    Rep = orber_ifr:find_repository(),
    Def = orber_ifr:lookup_id(Rep, IFRId),
    Descr = orber_ifr:describe(Def),
    TypeDescr = Descr#contained_description.value,
    TypeDescr#typedescription.type.
    

%%-----------------------------------------------------------------
%% Check TypeCode format
%%
check_tc('tk_null') -> true;
check_tc('tk_void') -> true;
check_tc('tk_short') -> true;
check_tc('tk_ushort') -> true;
check_tc('tk_long') -> true;
check_tc('tk_ulong') -> true;
check_tc('tk_longlong') -> true;
check_tc('tk_ulonglong') -> true;
check_tc('tk_float') -> true;
check_tc('tk_double') -> true;
check_tc('tk_longdouble') -> true;
check_tc('tk_boolean') -> true;
check_tc('tk_char') -> true;
check_tc('tk_wchar') -> true;
check_tc('tk_octet') -> true;
check_tc('tk_any') -> true;
check_tc('tk_TypeCode') -> true;
check_tc('tk_Principal') -> true;
check_tc({'tk_objref', RepId, Name}) when is_list(RepId) andalso
					  is_list(Name) -> true;
check_tc({'tk_struct', RepId, Name, ElementList}) when is_list(RepId) andalso
						       is_list(Name) -> 
    Fun = fun(X) -> 			  
		  case X of
		      {MemberName, MemberTC} when is_list(MemberName) ->
			  check_tc(MemberTC);
		      _ ->
			  false
		  end
	  end,
    lists:all(Fun, ElementList);
check_tc({'tk_union', RepId, Name, DiscrTC, 
	  Default, ElementList}) when is_list(RepId) andalso
				      is_list(Name) andalso
				      is_integer(Default) -> 
    case check_tc(DiscrTC) of
	false ->
	    false;
	true ->
	    Fun = fun(X) -> 			  
			  case X of
			      {_, MemberName, MemberTC} when
				    is_list(MemberName) ->
				  check_tc(MemberTC);
			      _ ->
				  false
			  end
		  end,
	    lists:all(Fun, ElementList)
    end;
check_tc({'tk_enum', RepId, Name, ElementList}) when is_list(RepId) andalso
						     is_list(Name) -> 
    Fun = fun(X) -> 
		  if
		      is_list(X) ->
			  true;
		      true ->
			  false
		  end
	  end,
    lists:all(Fun, ElementList);
check_tc({'tk_string', MaxLength}) when is_integer(MaxLength) -> true;
check_tc({'tk_wstring', MaxLength}) when is_integer(MaxLength) -> true;
check_tc({'tk_fixed', Digits, Scale}) when is_integer(Digits) andalso
					   is_integer(Scale) -> true;
check_tc({'tk_sequence', ElemTC, MaxLength}) when is_integer(MaxLength) -> 
    check_tc(ElemTC);
check_tc({'tk_array', ElemTC, Length})  when is_integer(Length) -> 
    check_tc(ElemTC);
check_tc({'tk_alias', RepId, Name, TC}) when is_list(RepId) andalso
					     is_list(Name) -> 
    check_tc(TC);
check_tc({'tk_except', RepId, Name, ElementList}) when is_list(RepId) andalso
						       is_list(Name) -> 
    Fun = fun(X) -> 
		  case X of
		      {MemberName, TC} when is_list(MemberName) ->
			  check_tc(TC);
		      _ ->
			  false
		  end
	  end,
    lists:all(Fun, ElementList);
check_tc({'tk_value', RepId, Name, ValueModifier, 
	  TC, ElementList}) when is_list(RepId) andalso
				 is_list(Name) andalso
				 is_integer(ValueModifier) -> 
    case check_tc(TC) of
	false ->
	    false;
	true ->
	    Fun = fun(X) -> 
			  case X of
			      {MemberName, MemberTC, Visibility} when 
				    is_list(MemberName) andalso is_integer(Visibility) ->
				  check_tc(MemberTC);
			      _ ->
				  false
			  end
		  end,
	    lists:all(Fun, ElementList)
    end;
check_tc({'tk_value_box', RepId, Name, TC}) when is_list(RepId) andalso
						 is_list(Name) -> 
    check_tc(TC);
check_tc({'tk_native', RepId, Name}) when is_list(RepId) andalso
					  is_list(Name) -> true;
check_tc({'tk_abstract_interface', RepId, Name}) when is_list(RepId) andalso
						      is_list(Name) -> true;
check_tc({'none', Indirection}) when is_integer(Indirection) -> true;
check_tc(_) -> false.
    
