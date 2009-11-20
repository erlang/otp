%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
%%----------------------------------------------------------------------
%% File    : orber_ifr_utils.erl
%% Purpose : Common function for the Interface Repository
%%----------------------------------------------------------------------

-module(orber_ifr_utils).

-export([
	 select/2,
	 index/2,
	 construct/3,
	 get_object/1,
	 set_object/1,
	 get_field/2,
	 set_field/3,
	 write_result/1,
	 read_result/1,
	 ifr_transaction_read/1,
	 ifr_transaction_write/1,
	 ifr_transaction_read_write/1,
	 makeref/1,
	 unique/0,
	 existence_check/2,
	 existence_check/3,
	 create_repository/0,
	 init_DB/2, init_DB/3
	]).

-include_lib("orber/include/corba.hrl").
-include("orber_ifr.hrl").
-include("ifr_objects.hrl").


%%======================================================================
%% Internal stuff

%%----------------------------------------------------------------------
%% Make a record selection.
%%
%% This code *must* be amended whenever a new record is added in the
%% files ifr_objects.hrl or ../include/ifr_types.hrl

select(Record,Field) when is_record(Record,ir_IRObject) ->
    select(Record,record_info(fields,ir_IRObject),Field);
select(Record,Field) when is_record(Record,ir_Contained) ->
    select(Record,record_info(fields,ir_Contained),Field);
select(Record,Field) when is_record(Record,ir_Container) ->
    select(Record,record_info(fields,ir_Container),Field);
select(Record,Field) when is_record(Record,ir_IDLType) ->
    select(Record,record_info(fields,ir_IDLType),Field);
select(Record,Field) when is_record(Record,ir_Repository) ->
    select(Record,record_info(fields,ir_Repository),Field);
select(Record,Field) when is_record(Record,ir_ModuleDef) ->
    select(Record,record_info(fields,ir_ModuleDef),Field);
select(Record,Field) when is_record(Record,ir_ConstantDef) ->
    select(Record,record_info(fields,ir_ConstantDef),Field);
select(Record,Field) when is_record(Record,ir_TypedefDef) ->
    select(Record,record_info(fields,ir_TypedefDef),Field);
select(Record,Field) when is_record(Record,ir_StructDef) ->
    select(Record,record_info(fields,ir_StructDef),Field);
select(Record,Field) when is_record(Record,ir_UnionDef) ->
    select(Record,record_info(fields,ir_UnionDef),Field);
select(Record,Field) when is_record(Record,ir_EnumDef) ->
    select(Record,record_info(fields,ir_EnumDef),Field);
select(Record,Field) when is_record(Record,ir_AliasDef) ->
    select(Record,record_info(fields,ir_AliasDef),Field);
select(Record,Field) when is_record(Record,ir_PrimitiveDef) ->
    select(Record,record_info(fields,ir_PrimitiveDef),Field);
select(Record,Field) when is_record(Record,ir_StringDef) ->
    select(Record,record_info(fields,ir_StringDef),Field);
select(Record,Field) when is_record(Record,ir_WstringDef) ->
    select(Record,record_info(fields,ir_WstringDef),Field);
select(Record,Field) when is_record(Record,ir_SequenceDef) ->
    select(Record,record_info(fields,ir_SequenceDef),Field);
select(Record,Field) when is_record(Record,ir_ArrayDef) ->
    select(Record,record_info(fields,ir_ArrayDef),Field);
select(Record,Field) when is_record(Record,ir_ExceptionDef) ->
    select(Record,record_info(fields,ir_ExceptionDef),Field);
select(Record,Field) when is_record(Record,ir_AttributeDef) ->
    select(Record,record_info(fields,ir_AttributeDef),Field);
select(Record,Field) when is_record(Record,ir_OperationDef) ->
    select(Record,record_info(fields,ir_OperationDef),Field);
select(Record,Field) when is_record(Record,ir_InterfaceDef) ->
    select(Record,record_info(fields,ir_InterfaceDef),Field);
select(Record,Field) when is_record(Record,ir_FixedDef) ->
    select(Record,record_info(fields,ir_FixedDef),Field);
select([],_) -> [];
select(Record,Field) ->
    orber:dbg("[~p] orber_ifr_utils:select(~p, ~p);~n"
	      "Unknown Record Type~n", [?LINE, Record,Field], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

-define(ELEMENT_OFFSET, 2).

select(Record,Fields,Field) ->
    Index = index(Fields,Field),
    element(?ELEMENT_OFFSET + Index, Record).

index(List,Element) ->
    index(List,Element,0).

index([H|_T],Element,Index) when H == Element ->
    Index;
index([_H|T],Element,Index) ->
    index(T,Element,Index+1);
index([],Element,Index) ->
    orber:dbg("[~p] orber_ifr_utils:index(~p, ~p);~n"
	      "Index error.~n", [?LINE, Element, Index], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

%%%----------------------------------------------------------------------
%%% Construct a record.
%%%
%%% This code *must* be amended whenever a new record is added in the
%%% files ifr_objects.hrl or ../include/ifr_types.hrl

construct(Record,Field,Value) when is_record(Record,ir_IRObject) ->
    construct(Record,record_info(fields,ir_IRObject),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_Contained) ->
    construct(Record,record_info(fields,ir_Contained),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_Container) ->
    construct(Record,record_info(fields,ir_Container),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_IDLType) ->
    construct(Record,record_info(fields,ir_IDLType),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_Repository) ->
    construct(Record,record_info(fields,ir_Repository),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_ModuleDef) ->
    construct(Record,record_info(fields,ir_ModuleDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_ConstantDef) ->
    construct(Record,record_info(fields,ir_ConstantDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_TypedefDef) ->
    construct(Record,record_info(fields,ir_TypedefDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_StructDef) ->
    construct(Record,record_info(fields,ir_StructDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_UnionDef) ->
    construct(Record,record_info(fields,ir_UnionDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_EnumDef) ->
    construct(Record,record_info(fields,ir_EnumDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_AliasDef) ->
    construct(Record,record_info(fields,ir_AliasDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_PrimitiveDef) ->
    construct(Record,record_info(fields,ir_PrimitiveDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_StringDef) ->
    construct(Record,record_info(fields,ir_StringDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_WstringDef) ->
    construct(Record,record_info(fields,ir_WstringDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_SequenceDef) ->
    construct(Record,record_info(fields,ir_SequenceDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_ArrayDef) ->
    construct(Record,record_info(fields,ir_ArrayDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_ExceptionDef) ->
    construct(Record,record_info(fields,ir_ExceptionDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_AttributeDef) ->
    construct(Record,record_info(fields,ir_AttributeDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_OperationDef) ->
    construct(Record,record_info(fields,ir_OperationDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_InterfaceDef) ->
    construct(Record,record_info(fields,ir_InterfaceDef),Field,Value);
construct(Record,Field,Value) when is_record(Record,ir_FixedDef) ->
    construct(Record,record_info(fields,ir_FixedDef),Field,Value);
construct(Record,Field,Value) ->
    orber:dbg("[~p] orber_ifr_utils:construct(~p, ~p, ~p);~n"
	      "Unknown Record Type~n", 
	      [?LINE, Record,Field,Value], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

construct(Record,Fields,Field,Value) ->
    Index = index(Fields,Field),
    setelement(?ELEMENT_OFFSET + Index,Record,Value).

%%%----------------------------------------------------------------------
%%% Read an object from the database

get_object(Objref) ->
%%% Use mnesia:dirty_read/1. It is much faster than doing a transaction.
    case mnesia:dirty_read(Objref) of
	[Res] ->
	    Res;
	[] ->
	    [];
	Other ->
	    orber:dbg("[~p] orber_ifr_utils:get_object(~p);~n", 
		      [?LINE, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})
    end.
%%% This is the old code, with a transaction. We might have to revert back
%%% to this at some future time...
%%    _F = ?read_function(Objref),
%%    read_result(ifr_transaction_read(_F)).

%%%----------------------------------------------------------------------
%%% Write an object to the database

set_object(Object) ->
    _F = fun() -> mnesia:write(Object) end,
    write_result(ifr_transaction_write(_F)).

%%%----------------------------------------------------------------------
%%% Get the value of a field in a record in the DB

get_field(Objref,FieldName) ->
    Object = get_object(Objref),
    select(Object,FieldName).

%%%----------------------------------------------------------------------
%%% Atomically set the value of a field in a record in the DB

set_field(Objref,FieldName,Value) ->
    _F = fun() -> Object = get_object(Objref),
		  New_object = construct(Object,FieldName,Value),
		  mnesia:write(New_object)
	 end,
    write_result(ifr_transaction_write(_F)).


%%%----------------------------------------------------------------------
%%% Check a write transaction

write_result({atomic,ok}) -> ok;
write_result(Wres) ->
    orber:dbg("[~p] orber_ifr_utils:write_result(~p);~n", 
	      [?LINE, Wres], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

%%%----------------------------------------------------------------------
%%% Extract the data from a read

read_result({atomic,[Qres]}) -> Qres;
read_result({atomic,[]}) -> [];
read_result(Qres) ->
    orber:dbg("[~p] orber_ifr_utils:read_result(~p);~n", 
	      [?LINE, Qres], ?DEBUG_LEVEL),
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO}).

%%%----------------------------------------------------------------------
%%% Execute a transaction or a dirty read/write.
%%%
%%% Since nested transctions will upgrade the inner activity to the
%%% same kind as the outer, we cannot use the check the result in the
%%% above simplistic manner. Therefore we will not mix transaction
%%% with async_dirty (or any of the other transaction-like
%%% activities). A rather extensive rewrite of the query extraction
%%% code must be done first.

ifr_transaction_read(Fun) ->			% read synchronously
    Tr = mnesia:transaction(Fun),
    {atomic, _} = Tr,
    Tr.
ifr_transaction_write(Fun) ->			% write synchronously
    Tr = mnesia:transaction(Fun),
    {atomic, _} = Tr,
    Tr.
ifr_transaction_read_write(Fun) ->		% write synchronously
    Tr = mnesia:transaction(Fun),
    {atomic, _} = Tr,
    Tr.

%%%----------------------------------------------------------------------
%%% Make an object reference from an object

makeref(Obj) ->
    [ObjType, ObjID | _] = tuple_to_list(Obj),
    {ObjType, ObjID}.

%%%----------------------------------------------------------------------
%%% Make a unique tag.
%%%
%%% The call to term_to_binary is made to hide the representation of the
%%% unique tag. I do this because the tuple generated takes a lot of space
%%% when I dump the database. A binary is simply printed as #Bin, which
%%% is much less obtrusive.
%%% The code has been moved to a macro defined in orber_ifr.hrl, so we
%%% can use a simpler uniqification code when debugging.

unique() -> term_to_binary({node(), now()}).

%%%----------------------------------------------------------------------
%%% Check for an existing object with the Id of the object which is
%%% about to be created.

existence_check({ObjType, ObjID}, Id) ->
    Rep = case ObjType of
	      ir_Repository ->
		  {ObjType, ObjID};
	      _ ->
		  orber_ifr_contained:'_get_containing_repository'({ObjType,
								    ObjID})
	  end,
    case orber_ifr_repository:lookup_id(Rep, Id) of
	[] ->
	    ok;
	What ->
	    orber:dbg("[~p] orber_ifr_utils:existence_check(~p, ~p, ~p);~n"
		      "Name clash(?): ~p", 
		      [?LINE, ObjType, ObjID, Id, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})
    end.

existence_check(Id, Tab, FieldNum) ->
    case mnesia:dirty_index_read(Tab, Id, FieldNum) of
	[] ->
	    ok;
	What ->
	    orber:dbg("[~p] orber_ifr_utils:existence_check(~p, ~p, ~p);~n"
		      "Name clash(?): ~p", 
		      [?LINE, Id, Tab, FieldNum, What], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})
    end.

%%======================================================================
%% Database initialization

init_DB(Timeout, Options) ->
    init_DB(Timeout, Options, false).

init_DB(Timeout, Options, LightIFR) ->
    Func = case Options of
	       {localCopy, IFR_storage_type} when LightIFR == true ->
		   ?ifr_light_record_tuple_list_local(IFR_storage_type);
	       {localCopy, IFR_storage_type} ->
		   ?ifr_record_tuple_list_local(IFR_storage_type);
	       _ when LightIFR == true ->
		   ?ifr_light_record_tuple_list(Options);
	       _ ->
		   ?ifr_record_tuple_list(Options)
    end,
    create_tables(Func), 
    Wait = wait_for_tables(LightIFR, Timeout),
    db_error_check([Wait],"Database table waiting failed.").

wait_for_tables(true, Timeout) ->
    mnesia:wait_for_tables(?ifr_light_object_list, Timeout);
wait_for_tables(_, Timeout) ->
    mnesia:wait_for_tables(?ifr_object_list, Timeout).

db_error_check(Checkval,_Message) ->
    case lists:any(fun(X) -> X/= ok end, Checkval) of
	true ->
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
	false ->
	    ok
    end.   

create_tables([{T,F}|Rest]) -> 
    case F() of
	ok ->
	    create_tables2(Rest);
	{aborted,{already_exists,_}} ->
	    exit({error, "Orber Mnesia Table(s) already exist. Cannot install Orber."});
	Reason ->
	    orber:dbg("[~p] orber_ifr_utils:create_tables(~p);~n"
		      "Failed to create the Mnesia table.~n"
		      "Reason: ~p", [?LINE, T, Reason], ?DEBUG_LEVEL),
	    exit({error, "Unable to create Mnesia Table"})
    end.

create_tables2([]) -> 
    ok;
create_tables2([{T,F}|Rest]) -> 
    case F() of
	ok ->
	    create_tables2(Rest);
	Reason ->
	    orber:dbg("[~p] orber_ifr_utils:create_tables2(~p);~n"
		      "Failed to create the Mnesia table.~n"
		      "Reason: ~p", [?LINE, T, Reason], ?DEBUG_LEVEL),
	    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})
    end.


%%%----------------------------------------------------------------------
%%% Create an interface repository. This function should only be called
%%% once, after the database has been set up and initialized.

create_repository() ->
    case orber:light_ifr() of
	true ->
	    #orber_light_ifr_ref{data = #lightdata{scope = "",
						   id = ""}}; 
	false ->
	    _R = fun() ->
			 Pat = mnesia:table_info(ir_Repository, wild_pattern),
			 case [X#ir_Repository.ir_Internal_ID ||
				  X <- mnesia:match_object(Pat)] of
			     [] ->
				 PrimitiveDefs = create_primitivedefs(),
				 New = #ir_Repository{ir_Internal_ID = unique(),
						      def_kind = dk_Repository,
						      contents = [],
						      primitivedefs = PrimitiveDefs},
				 mnesia:write(New), 
				 {ir_Repository,New#ir_Repository.ir_Internal_ID};
			     [Rep_ID] ->
				 {ir_Repository,Rep_ID};
			     Error ->
				 mnesia:abort(Error)
			 end
		 end,
	    case mnesia:transaction(_R) of
		{atomic, RepRef} ->
		    RepRef;
		{aborted, Error} ->
		    orber:dbg("[~p] orber_ifr_utils:create_repository() failed;~n"
			      "Reason: ~p", [?LINE, Error], ?DEBUG_LEVEL),
		    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO})
	    end
    end.

create_primitivedefs() ->
    lists:map(fun(Pk) ->
		      orber_ifr_repository:create_primitivedef(Pk, false)
	      end,
	      [pk_void,pk_short,pk_long,pk_longlong,pk_ulonglong,pk_ushort,pk_ulong,
	       pk_float,pk_double,pk_boolean,pk_char,pk_wchar,pk_octet,pk_any,
	       pk_TypeCode,pk_Principal,pk_string,pk_wstring,pk_objref]).


