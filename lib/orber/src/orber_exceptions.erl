%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
%% File: orber_exceptions.erl
%% 
%% Description:
%%
%%-----------------------------------------------------------------

-module(orber_exceptions).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([dissect/1,
	 get_def/1,
	 get_name/2,
	 type/1,
	 is_system_exception/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export(['UNKNOWN'/1,
	 'BAD_PARAM'/1,
	 'NO_MEMORY'/1,
	 'IMP_LIMIT'/1,
	 'COMM_FAILURE'/1,
	 'INV_OBJREF'/1,
	 'NO_PERMISSION'/1,
	 'INTERNAL'/1,
	 'MARSHAL'/1,
	 'INITIALIZE'/1,
	 'NO_IMPLEMENT'/1,
	 'BAD_TYPECODE'/1,
	 'BAD_OPERATION'/1,
	 'NO_RESOURCES'/1,
	 'NO_RESPONSE'/1,
	 'PERSIST_STORE'/1,
	 'BAD_INV_ORDER'/1,
	 'TRANSIENT'/1,
	 'FREE_MEM'/1,
	 'INV_IDENT'/1,
	 'INV_FLAG'/1,
	 'INTF_REPOS'/1,
	 'BAD_CONTEXT'/1,
	 'OBJ_ADAPTER'/1,
	 'DATA_CONVERSION'/1,
	 'OBJECT_NOT_EXIST'/1,
	 'TRANSACTION_REQUIRED'/1,
	 'TRANSACTION_ROLLEDBACK'/1,
	 'INVALID_TRANSACTION'/1,
	 'INV_POLICY'/1,
	 'CODESET_INCOMPATIBLE'/1,
	 'REBIND'/1,
	 'TIMEOUT'/1,
	 'TRANSACTION_UNAVAILABLE'/1,
	 'TRANSACTION_MODE'/1,
	 'BAD_QOS'/1]).


-define(DEBUG_LEVEL, 5).

%%-----------------------------------------------------------------
%% Function   : is_system_exception
%% Arguments  : Exception - record()
%% Returns    : true | false
%% Raises     : 
%% Description: Check if CORBA system exception or user defined
%%-----------------------------------------------------------------
is_system_exception({'EXCEPTION', E}) ->
    is_system_exception(E);
is_system_exception(E) when is_tuple(E) ->
    ?SYSTEM_EXCEPTION == type(element(1, E));
is_system_exception(_E) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------------
%% Function   : type
%% Arguments  : ExceptionName - atom()
%% Returns    : ?SYSTEM_EXCEPTION | ?USER_EXCEPTION
%% Raises     : 
%% Description: Check if CORBA system exception or user defined
%%-----------------------------------------------------------------
type('UNKNOWN') ->                 ?SYSTEM_EXCEPTION;
type('BAD_PARAM') ->               ?SYSTEM_EXCEPTION;
type('NO_MEMORY') ->               ?SYSTEM_EXCEPTION;
type('IMP_LIMIT') ->               ?SYSTEM_EXCEPTION;
type('COMM_FAILURE') ->            ?SYSTEM_EXCEPTION;
type('INV_OBJREF') ->              ?SYSTEM_EXCEPTION;
type('NO_PERMISSION') ->           ?SYSTEM_EXCEPTION;
type('INTERNAL') ->                ?SYSTEM_EXCEPTION;
type('MARSHAL') ->                 ?SYSTEM_EXCEPTION;
type('INITIALIZE') ->              ?SYSTEM_EXCEPTION;
type('NO_IMPLEMENT') ->            ?SYSTEM_EXCEPTION;
type('BAD_TYPECODE') ->            ?SYSTEM_EXCEPTION;
type('BAD_OPERATION') ->           ?SYSTEM_EXCEPTION;
type('NO_RESOURCES') ->            ?SYSTEM_EXCEPTION;
type('NO_RESPONSE') ->             ?SYSTEM_EXCEPTION;
type('PERSIST_STORE') ->           ?SYSTEM_EXCEPTION;
type('BAD_INV_ORDER') ->           ?SYSTEM_EXCEPTION;
type('TRANSIENT') ->               ?SYSTEM_EXCEPTION;
type('FREE_MEM') ->                ?SYSTEM_EXCEPTION;
type('INV_IDENT') ->               ?SYSTEM_EXCEPTION;
type('INV_FLAG') ->                ?SYSTEM_EXCEPTION;
type('INTF_REPOS') ->              ?SYSTEM_EXCEPTION;
type('BAD_CONTEXT') ->             ?SYSTEM_EXCEPTION;
type('OBJ_ADAPTER') ->             ?SYSTEM_EXCEPTION;
type('DATA_CONVERSION') ->         ?SYSTEM_EXCEPTION;
type('OBJECT_NOT_EXIST') ->        ?SYSTEM_EXCEPTION;
type('TRANSACTION_REQUIRED') ->    ?SYSTEM_EXCEPTION;
type('TRANSACTION_ROLLEDBACK') ->  ?SYSTEM_EXCEPTION;
type('INVALID_TRANSACTION') ->     ?SYSTEM_EXCEPTION;
type('INV_POLICY') ->              ?SYSTEM_EXCEPTION;
type('CODESET_INCOMPATIBLE') ->    ?SYSTEM_EXCEPTION;
type('REBIND') ->                  ?SYSTEM_EXCEPTION;
type('TIMEOUT') ->                 ?SYSTEM_EXCEPTION;
type('TRANSACTION_UNAVAILABLE') -> ?SYSTEM_EXCEPTION;
type('TRANSACTION_MODE') ->        ?SYSTEM_EXCEPTION;
type('BAD_QOS') ->                 ?SYSTEM_EXCEPTION;
type(_) ->                         ?USER_EXCEPTION.

%%-----------------------------------------------------------------
%% Function   : get_def
%% Arguments  : Exception - record()
%% Returns    : {Type, TypeCode, Exc}
%% Raises     : 
%% Description: Returns the TC for the supplied exception
%%-----------------------------------------------------------------
get_def(Exception) ->
    [Exc, TypeId | _] = tuple_to_list(Exception),
    case type(Exc) of
	?SYSTEM_EXCEPTION ->
	    {?SYSTEM_EXCEPTION, get_system_exception_def(Exc), Exception};
	?USER_EXCEPTION ->
	    case orber:light_ifr() of
		true ->
		    case catch orber_ifr:get_tc(TypeId, ?IFR_ExceptionDef) of
			{'EXCEPTION', NewExc} ->
			    {?SYSTEM_EXCEPTION,
			     get_system_exception_def(NewExc),
			     NewExc};
			TC ->
			    {?USER_EXCEPTION, TC, Exception}
		    end;
		false ->
		    case mnesia:dirty_index_read(ir_ExceptionDef, TypeId,
						 #ir_ExceptionDef.id) of
			[ExcDef] when is_record(ExcDef, ir_ExceptionDef) ->  
			    {?USER_EXCEPTION, 
			     ExcDef#ir_ExceptionDef.type,
			     Exception};
			Other ->
			    orber:dbg("[~p] ~p:get_user_exception_type(~p).~n"
				      "IFR Id not found: ~p", 
				      [?LINE, ?MODULE, TypeId, Other], ?DEBUG_LEVEL),
			    NewExc = #'UNKNOWN'{minor=(?CORBA_OMGVMCID bor 1), 
						completion_status=?COMPLETED_MAYBE},
			    {?SYSTEM_EXCEPTION,
			     get_system_exception_def(NewExc),
			     NewExc}
		   end
	    end
    end.

%%-----------------------------------------------------------------
%% Function   : get_name
%% Arguments  : TypeId - string()
%%              Type - ?SYSTEM_EXCEPTION ( | ?USER_EXCEPTION)
%% Returns    : ExceptionName - atom()
%% Raises     : #'UNKNOWN'{}
%% Description: Extract exception name
%%-----------------------------------------------------------------
get_name(TypeId, ?SYSTEM_EXCEPTION) ->
    ExcName = 
	case string:tokens(TypeId, ":/") of
	    [_IDL, _OMGORG, _CORBA, Name, _Version] when is_list(Name) ->
		list_to_atom(Name);
	    [_IDL, _CORBA, Name, _Version] when is_list(Name) ->
		%% We should remove this case but we keep it for now due to backward 
		%% compatible reasons.
		list_to_atom(Name);
	    Other ->
		%% The CORBA-spec states that this exception should be raised if
		%% it's a system exception we do not support.
		orber:dbg("[~p] ~p:get_system_exception_name(~p).~n"
			  "Unknown System Exception: ~p", 
			  [?LINE, ?MODULE, TypeId, Other], ?DEBUG_LEVEL),
		corba:raise(#'UNKNOWN'{minor=(?CORBA_OMGVMCID bor 2), 
				       completion_status=?COMPLETED_MAYBE})
	end,
    case type(ExcName) of
	?SYSTEM_EXCEPTION ->
	    ExcName;
	What ->
	    orber:dbg("[~p] ~p:get_system_exception_name(~p).~n"
		      "Unknown System Exception: ~p", 
		      [?LINE, ?MODULE, TypeId, What], ?DEBUG_LEVEL),
	    corba:raise(#'UNKNOWN'{minor=(?CORBA_OMGVMCID bor 2), 
				   completion_status=?COMPLETED_MAYBE})
    end.
    

%%-----------------------------------------------------------------
%% Generate system exception TypeCode
%%-----------------------------------------------------------------
get_system_exception_def(ExcName) when is_atom(ExcName) ->
    Name = atom_to_list(ExcName),
    {'tk_except', "IDL:omg.org/CORBA/" ++ Name ++ ":1.0", Name,
     [{"minor",'tk_ulong'},
      {"completed",
       {'tk_enum', "", "completion_status",
	["COMPLETED_YES", "COMPLETED_NO",
	 "COMPLETED_MAYBE"]}}]};
get_system_exception_def(Exc) ->
    get_system_exception_def(element(1, Exc)).


%%-----------------------------------------------------------------
%% Mapping minor codes to a printable string.
%%-----------------------------------------------------------------
dissect({'EXCEPTION', Exc}) ->
    dissect(Exc);
dissect(Exception) when is_tuple(Exception) ->
    [Exc, TypeId | _] = tuple_to_list(Exception),
    case type(Exc) of
	?USER_EXCEPTION ->
	    {ok, lists:flatten(io_lib:format("~n------------- EXCEPTION INFO --------------
User Defined Exception.: ~p
IFR Id.................: ~s
-------------------------------------------~n", [Exc, TypeId]))};
	?SYSTEM_EXCEPTION ->
            case map_exc(Exception) of
                {ok, String} ->
		    {ok, lists:flatten(String)};
		{error, Reason} ->
		    {error, Reason}
	    end
    end;
dissect(_What) ->
    {error, "Not a correct exception supplied to orber_exceptions:dissect/1"}.
	    
map_exc({Name, _, Minor, Status}) when is_integer(Minor) ->
    case lookup_vendor(Minor) of
	{true, Vendor, VMCID} ->
	    case catch ?MODULE:Name(Minor) of
		MinorInfo when is_list(MinorInfo) ->
		    {ok, io_lib:format("~n------------- EXCEPTION INFO --------------
Vendor.....: ~s
VMCID......: ~s
Exception..: ~p
Status.....: ~p
Minor Code.: ~p
Info.......: ~s
-------------------------------------------~n", 
				  [Vendor, VMCID, Name, Status, (Minor band 16#fff), MinorInfo])};
		_ ->
		    {ok, io_lib:format("~n------------- EXCEPTION INFO --------------
Vendor.....: ~s
VMCID......: ~s
Exception..: ~p
Status.....: ~p
Minor Code.: ~p
Info.......: -
------------------------------------~n", [Vendor, VMCID, Name, Status, (Minor band 16#fff)])}
	    end;
	{false, Vendor, VMCID} ->
	    {ok, io_lib:format("~n------------- EXCEPTION INFO --------------
Vendor.....: ~s
VMCID......: ~s
Exception..: ~p
Status.....: ~p
Minor Code.: ~p
Info.......: -
-------------------------------------------~n", [Vendor, VMCID, Name, Status, (Minor band 16#fff)])}
    end;
map_exc(_) ->
    {error, "Not a correct exception supplied to orber_exceptions:map_exc/1"}. 

lookup_vendor(Minor) when (?ORBER_VMCID bxor Minor) < 16#0fff ->
    {true, "Orber", "0x45520000"};
lookup_vendor(Minor) when (?CORBA_OMGVMCID bxor Minor) < 16#0fff ->
    {true, "OMG", "0x4f4d0000"};
lookup_vendor(Minor) when (?IONA_VMCID_1 bxor Minor) < 16#0fff ->
    {false, "IONA", "0x4f4f0000"};
lookup_vendor(Minor) when (?IONA_VMCID_2 bxor Minor) < 16#0fff ->
    {false, "IONA", "0x49540000"};
lookup_vendor(Minor) when (?SUN_VMCID bxor Minor) < 16#0fff ->
    {false, "SUN", "0x53550000"};
lookup_vendor(Minor) when (?BORLAND_VMCID bxor Minor) < 16#0fff ->
    {false, "Borland", "0x56420000"};
lookup_vendor(Minor) when (?TAO_VMCID bxor Minor) < 16#0fff ->
    {false, "TAO", "0x54410000"};
lookup_vendor(Minor) when (?PRISMTECH_VMCID bxor Minor) < 16#0fff ->
    {false, "PrismTech", "0x50540000"};
lookup_vendor(Minor) when is_integer(Minor), Minor =< ?ULONGMAX ->
    {false, "undefined", extract_VMCID(Minor)};
lookup_vendor(Minor) when is_integer(Minor), Minor =< ?ULONGMAX ->
    {false, "Unknown", "Unable to extract it"}.

extract_VMCID(Int) ->
    int_to_hex_str(3, ((Int bsr 8) band 16#fffff0), ["00"]).

int_to_hex_str(0, _, Acc) ->
    lists:flatten(["0x" | Acc]);
int_to_hex_str(N, Int, Acc) ->
    int_to_hex_str(N-1, (Int bsr 8), [int_to_hex((16#ff band Int))|Acc]).
    
int_to_hex(B) when B < 256, B >= 0 ->
    N1 = B div 16,
    N2 = B rem 16,
    [code_character(N1),
     code_character(N2)].
code_character(N) when N < 10 ->
    $0 + N;
code_character(N) ->
    $a + (N - 10).


%% The following functions all maps to a system exception. 
%% UNKNOWN - OMG
'UNKNOWN'(?CORBA_OMGVMCID bor 1) -> "Unlisted user exception received 
by client";
'UNKNOWN'(?CORBA_OMGVMCID bor 2) -> "Non-standard System Exception
not supported";
%% UNKNOWN - Orber
'UNKNOWN'(?ORBER_VMCID bor 1) -> "Missing beam-file. Unable to extract TC.";
'UNKNOWN'(_) -> "-".


%% BAD_PARAM - OMG
'BAD_PARAM'(?CORBA_OMGVMCID bor 1) -> "Failure to register, unregister, or 
lookup value factory";
'BAD_PARAM'(?CORBA_OMGVMCID bor 2) -> "RID already defined in IFR";
'BAD_PARAM'(?CORBA_OMGVMCID bor 3) -> "Name already used in the context in IFR";
'BAD_PARAM'(?CORBA_OMGVMCID bor 4) -> "Target is not a valid container";
'BAD_PARAM'(?CORBA_OMGVMCID bor 5) -> "Name clash in inherited context";
'BAD_PARAM'(?CORBA_OMGVMCID bor 6) -> "Incorrect type for abstract interface";
'BAD_PARAM'(?CORBA_OMGVMCID bor 7) -> "string_to_object conversion failed
due to bad scheme name";
'BAD_PARAM'(?CORBA_OMGVMCID bor 8) -> "string_to_object conversion failed
due to bad address";
'BAD_PARAM'(?CORBA_OMGVMCID bor 9) -> "string_to_object conversion failed
due to bad bad schema specific part";
'BAD_PARAM'(?CORBA_OMGVMCID bor 10) -> "string_to_object conversion failed
due to non specific reason";
'BAD_PARAM'(?CORBA_OMGVMCID bor 11) -> "Attempt to derive abstract interface
from non-abstract base interface 
in the Interface Repository";
'BAD_PARAM'(?CORBA_OMGVMCID bor 12) -> "Attempt to let a ValueDef support 
more than one non-abstract interface 
in the Interface Repository";
'BAD_PARAM'(?CORBA_OMGVMCID bor 13) -> "Attempt to use an incomplete 
TypeCode as a parameter";
'BAD_PARAM'(?CORBA_OMGVMCID bor 14) -> "Invalid object id passed to
POA::create_reference_by_id";
'BAD_PARAM'(?CORBA_OMGVMCID bor 15) -> "Bad name argument in TypeCode operation";
'BAD_PARAM'(?CORBA_OMGVMCID bor 16) -> "Bad RepositoryId argument in TypeCode 
operation";
'BAD_PARAM'(?CORBA_OMGVMCID bor 17) -> "Invalid member name in TypeCode operation";
'BAD_PARAM'(?CORBA_OMGVMCID bor 18) -> "Duplicate label value in create_union_tc";
'BAD_PARAM'(?CORBA_OMGVMCID bor 19) -> "Incompatible TypeCode of label and 
discriminator in create_union_tc";
'BAD_PARAM'(?CORBA_OMGVMCID bor 20) -> "Supplied discriminator type illegitimate 
in create_union_tc";
'BAD_PARAM'(?CORBA_OMGVMCID bor 21) -> "Any passed to ServerRequest::set_exception
does not contain an exception";
'BAD_PARAM'(?CORBA_OMGVMCID bor 22) -> "Unlisted user exception passed to
ServerRequest::set_exception";
'BAD_PARAM'(?CORBA_OMGVMCID bor 23) -> "wchar transmission code set not 
in service context";
'BAD_PARAM'(?CORBA_OMGVMCID bor 24) -> "Service context is not in OMG-defined range";
'BAD_PARAM'(?CORBA_OMGVMCID bor 25) -> "Enum value out of range";
'BAD_PARAM'(?CORBA_OMGVMCID bor 26) -> "Invalid service context Id in portable
interceptor";
'BAD_PARAM'(?CORBA_OMGVMCID bor 27) -> "Attempt to call register_initial_reference
with a null Object";
'BAD_PARAM'(?CORBA_OMGVMCID bor 28) -> "Invalid component Id in 
portable interceptor";
'BAD_PARAM'(?CORBA_OMGVMCID bor 29) -> "Invalid profile Id in portable 
interceptor";
'BAD_PARAM'(?CORBA_OMGVMCID bor 30) -> "Two or more Policy objects with the
same PolicyType value supplied to
Object::set_policy_overrides or 
PolicyManager::set_policy_overrides";
'BAD_PARAM'(?CORBA_OMGVMCID bor 31) -> "Attempt to define a oneway
operation with non-void result, 
out or inout parameters or user 
exceptions";
'BAD_PARAM'(?CORBA_OMGVMCID bor 32) -> "DII asked to create request
for an implicit operation";
%% BAD_PARAM - Orber
'BAD_PARAM'(?ORBER_VMCID bor 1) -> "Bad return value from the objects
init-function (create phase) or invalid
options suuplied";
'BAD_PARAM'(_) -> "-".

%% NO_MEMORY - OMG
'NO_MEMORY'(_) -> "-".

%% IMP_LIMIT - OMG
'IMP_LIMIT'(?CORBA_OMGVMCID bor 1) -> "Unable to use any profile in IOR";
%% IMP_LIMIT - Orber
'IMP_LIMIT'(?ORBER_VMCID bor 1) -> "All ports assigned to the configuration
parameter 'iiop_out_ports' are in use";
'IMP_LIMIT'(_) -> "-".

%% COMM_FAILURE - OMG
%% COMM_FAILURE - Orber
'COMM_FAILURE'(?ORBER_VMCID bor 1) -> "Unable to connect to another ORB -
probably inactive";
'COMM_FAILURE'(?ORBER_VMCID bor 2) -> "Unable to connect to another ORB -
interceptor(s) rejected it or behaves 
badly";
'COMM_FAILURE'(?ORBER_VMCID bor 3) -> "Request terminated by another process";
'COMM_FAILURE'(?ORBER_VMCID bor 4) -> "Unable to connect to another ORB - timed out";
'COMM_FAILURE'(_) -> "-".

%% INV_OBJREF - OMG
'INV_OBJREF'(?CORBA_OMGVMCID bor 1) -> "wchar Code Set support not specified";
'INV_OBJREF'(?CORBA_OMGVMCID bor 2) -> "Codeset component required for type using wchar or wstring data";
'INV_OBJREF'(_) -> "-".

%% NO_PERMISSION - OMG
'NO_PERMISSION'(_) -> "-".

%% INTERNAL - OMG
%% INTERNAL - Orber
'INTERNAL'(?ORBER_VMCID bor 1) -> "Unable to connect to an Orber installation";
'INTERNAL'(?ORBER_VMCID bor 2) -> "Failed to register internal objectkey in the database";
'INTERNAL'(_) -> "-".

%% MARSHAL - OMG
'MARSHAL'(?CORBA_OMGVMCID bor 1) -> "Unable to locate value factory";
'MARSHAL'(?CORBA_OMGVMCID bor 2) -> "ServerRequest::set_result called 
before ServerRequest::ctx when the
operation IDL contains a context 
clause";
'MARSHAL'(?CORBA_OMGVMCID bor 3) -> "NVList passed to 
ServerRequest::arguments does not
describe all parameters passed
by client";
'MARSHAL'(?CORBA_OMGVMCID bor 4) -> "Attempt to marshal Local object";
'MARSHAL'(?CORBA_OMGVMCID bor 5) -> "wchar or wstring data erroneosly
sent by client over GIOP 1.0
connection";
'MARSHAL'(?CORBA_OMGVMCID bor 6) -> "wchar or wstring data erroneously
returned by server over GIOP 1.0
connection";
%% MARSHAL - Orber
'MARSHAL'(?ORBER_VMCID bor 1) -> "Integer overflow";
'MARSHAL'(?ORBER_VMCID bor 2) -> "Passed a non-integer,
when it must be an integer";
'MARSHAL'(?ORBER_VMCID bor 3) -> "Incorrect boolean";
'MARSHAL'(?ORBER_VMCID bor 4) -> "Passed a non-number, 
when it must be a float, double
or long double";
'MARSHAL'(?ORBER_VMCID bor 5) -> "Bad enumerant - does not exist";
'MARSHAL'(?ORBER_VMCID bor 6) -> "Passed something else but character
or octet";
'MARSHAL'(?ORBER_VMCID bor 7) -> "Unable to marshal the supplied 
typecode";
'MARSHAL'(?ORBER_VMCID bor 8) -> "Unable to un-marshal the supplied 
typecode";
'MARSHAL'(?ORBER_VMCID bor 9) -> "Union IFR-id does not exist";
'MARSHAL'(?ORBER_VMCID bor 10) -> "Struct IFR-id does not exist";
'MARSHAL'(?ORBER_VMCID bor 11) -> "Empty string supplied as IFR-id";
'MARSHAL'(?ORBER_VMCID bor 12) -> "Unable to decode target address";
'MARSHAL'(?ORBER_VMCID bor 13) -> "Incorrect TypeCode or unsupported
data type";
'MARSHAL'(?ORBER_VMCID bor 14) -> "The Fixed type does not match the
defined digits/scale parameters";
'MARSHAL'(?ORBER_VMCID bor 15) -> "The supplied array is to long or to short";
'MARSHAL'(?ORBER_VMCID bor 16) -> "String/Wstring exceeds maximum length";
'MARSHAL'(?ORBER_VMCID bor 17) -> "To few or to many parameters supplied";
'MARSHAL'(?ORBER_VMCID bor 18) -> "Unable to decode message header";
'MARSHAL'(?ORBER_VMCID bor 19) -> "Sequnce exceeds maximum length";
'MARSHAL'(_) -> "-".

%% INITIALIZE - OMG
'INITIALIZE'(_) -> "-".

%% NO_IMPLEMENT - OMG
'NO_IMPLEMENT'(?CORBA_OMGVMCID bor 1) -> "Missing local value implementation";
'NO_IMPLEMENT'(?CORBA_OMGVMCID bor 2) -> "Incompatible value implementation version";
'NO_IMPLEMENT'(?CORBA_OMGVMCID bor 3) -> "Unable to use any profile in IOR";
'NO_IMPLEMENT'(?CORBA_OMGVMCID bor 4) -> "Attempt to use DII on Local object";
'NO_IMPLEMENT'(_) -> "-".


%% BAD_TYPECODE - OMG
'BAD_TYPECODE'(?CORBA_OMGVMCID bor 1) -> "Attempt to marshal incomplete
TypeCode";
'BAD_TYPECODE'(?CORBA_OMGVMCID bor 2) -> "Member type code illegitimate
in TypeCode operation";
'BAD_TYPECODE'(_) -> "-".

%% BAD_OPERATION - OMG
'BAD_OPERATION'(?CORBA_OMGVMCID bor 1) -> "ServantManager returned wrong
servant type";
%% BAD_OPERATION - Orber
'BAD_OPERATION'(?ORBER_VMCID bor 1) -> "Incorrect instance type for this
operation";
'BAD_OPERATION'(?ORBER_VMCID bor 2) -> "Incorrect instance type for this
operation (one-way)";
'BAD_OPERATION'(?ORBER_VMCID bor 3) -> "The IC option 'handle_info' was
not used when compiling the stub";
'BAD_OPERATION'(?ORBER_VMCID bor 4) -> "Incorrect instance type for the
invoked operation (two- or one-way)";
'BAD_OPERATION'(_) -> "-".

%% NO_RESOURCES - OMG
'NO_RESOURCES'(?CORBA_OMGVMCID bor 1) -> "Portable Interceptor operation
not supported in this binding";
'NO_RESOURCES'(_) -> "-".

%% NO_RESPONSE - OMG
'NO_RESPONSE'(_) -> "-".

%% PERSIST_STORE - OMG
'PERSIST_STORE'(_) -> "-".

%% BAD_INV_ORDER - OMG
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 1) -> "Dependency exists in IFR preventing
destruction of this object";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 2) -> "Attempt to destroy indestructible
objects in IFR";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 3) -> "Operation would deadlock";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 4) -> "ORB has shutdown";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 5) -> "Attempt to invoke send or invoke 
operation of the same Request object
more than once";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 6) -> "Attempt to set a servant manager 
after one has already been set";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 7) -> "ServerRequest::arguments called more
than once or after a call to
ServerRequest:: set_exception";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 8) -> "ServerRequest::ctx called more than
once or before ServerRequest::arguments or 
after ServerRequest::ctx, ServerRequest::set_result
or ServerRequest::set_exception";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 9) -> "ServerRequest::set_result called more
than once or before ServerRequest::arguments
or after ServerRequest::set_result or 
ServerRequest::set_exception";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 10) -> "Attempt to send a DII request after
it was sent previously";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 11) -> "Attempt to poll a DII request or to
retrieve its result before the request
was sent";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 12) -> "Attempt to poll a DII request or to
retrieve its result after the result
was retrieved previously";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 13) -> "Attempt to poll a synchronous DII
request or to retrieve results from
a synchronous DII request";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 14) -> "Invalid portable interceptor call";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 15) -> "Service context add failed in portable 
interceptor because a service context 
with the given id already exists";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 16) -> "Registration of PolicyFactory failed 
because a factory already exists for
the given PolicyType";
'BAD_INV_ORDER'(?CORBA_OMGVMCID bor 17) -> "POA cannot create POAs while undergoing
destruction";
'BAD_INV_ORDER'(_) -> "-".

%% TRANSIENT - OMG
'TRANSIENT'(?CORBA_OMGVMCID bor 1) -> "Request discarded because of resource 
exhaustion in POA, or because POA 
is in discarding state";
'TRANSIENT'(?CORBA_OMGVMCID bor 2) -> "No usable profile in IOR";
'TRANSIENT'(?CORBA_OMGVMCID bor 3) -> "Request cancelled";
'TRANSIENT'(?CORBA_OMGVMCID bor 4) -> "POA destroyed";
%% TRANSIENT - Orber
'TRANSIENT'(?ORBER_VMCID bor 1) -> "Orber is being restarted, or should be,
on one node in the multi-node Orber
installation";
'TRANSIENT'(?ORBER_VMCID bor 2) -> "The node the target object resides on 
is down (multi-node Orber installation)";
'TRANSIENT'(?ORBER_VMCID bor 3) -> "Received EXIT when invoking an operation
on an object residing on another
node in a multi-node Orber installation";
'TRANSIENT'(?ORBER_VMCID bor 4) -> "Received EXIT when invoking an operation
on a local object";
'TRANSIENT'(?ORBER_VMCID bor 5) -> "Received unknown reply when invoking an 
operation on an object residing on
another node in a multi-node Orber 
installation";
'TRANSIENT'(?ORBER_VMCID bor 6) -> "Received unknown reply when invoking an 
operation on a local object";
'TRANSIENT'(?ORBER_VMCID bor 7) -> "Either the stub/skeleton does not exist or an
incorrect IC-version was used, which does not generate
the oe_tc/1 or oe_get_interface/1 functions";
'TRANSIENT'(_) -> "-".

%% FREE_MEM - OMG
'FREE_MEM'(_) -> "-".

%% INV_IDENT - OMG
'INV_IDENT'(_) -> "-".

%% INV_FLAG - OMG
'INV_FLAG'(_) -> "-".

%% INTF_REPOS - OMG
'INTF_REPOS'(?CORBA_OMGVMCID bor 1) -> "Interface Repository not available";
'INTF_REPOS'(?CORBA_OMGVMCID bor 2) -> "No entry for requested interface in
Interface Repository";
'INTF_REPOS'(_) -> "-".

%% BAD_CONTEXT - OMG
'BAD_CONTEXT'(_) -> "-".

%% OBJ_ADAPTER - OMG
'OBJ_ADAPTER'(?CORBA_OMGVMCID bor 1) -> "System exception in 
AdapterActivator::unknown_adapter";
'OBJ_ADAPTER'(?CORBA_OMGVMCID bor 2) -> "Servant not found [ServantManager]";
'OBJ_ADAPTER'(?CORBA_OMGVMCID bor 3) -> "No default servant available [POA policy]";
'OBJ_ADAPTER'(?CORBA_OMGVMCID bor 4) -> "No servant manager available [POA Policy]";
'OBJ_ADAPTER'(?CORBA_OMGVMCID bor 5) -> "Violation of POA policy by
ServantActivator::incarnate";
'OBJ_ADAPTER'(?CORBA_OMGVMCID bor 6) -> "Exception in 
PortableInterceptor::IORInterceptor.components_established";
%% OBJ_ADAPTER - Orber
'OBJ_ADAPTER'(?ORBER_VMCID bor 1) -> "Call-back module does not exist";
'OBJ_ADAPTER'(?ORBER_VMCID bor 2) -> "Missing function or incorrect arity in
call-back module";
'OBJ_ADAPTER'(?ORBER_VMCID bor 3) -> "Function exported but arity incorrect";
'OBJ_ADAPTER'(?ORBER_VMCID bor 4) -> "Unknown error. Call-back module generated
EXIT";
'OBJ_ADAPTER'(?ORBER_VMCID bor 5) -> "Call-back module invoked operation on a
non-existing module";
'OBJ_ADAPTER'(?ORBER_VMCID bor 6) -> "Missing function or incorrect arity in
a module invoked via the call-back module";
'OBJ_ADAPTER'(?ORBER_VMCID bor 7) -> "Function exported but arity incorrect in
a module invoked via the call-back module";
'OBJ_ADAPTER'(?ORBER_VMCID bor 8) -> "Call-back module contains a function_clause,
case_clause or badarith error";
'OBJ_ADAPTER'(?ORBER_VMCID bor 9) -> "Call-back module invoked operation exported
by another module which contains a function_clause, case_clause or badarith error";
'OBJ_ADAPTER'(?ORBER_VMCID bor 10) -> "Unknown EXIT returned by call-back module";
'OBJ_ADAPTER'(_) -> "-".

%% DATA_CONVERSION - OMG
'DATA_CONVERSION'(?CORBA_OMGVMCID bor 1) -> "Character does not map to negotiated
transmission code set";
'DATA_CONVERSION'(_) -> "-".

%% OBJECT_NOT_EXIST - OMG
'OBJECT_NOT_EXIST'(?CORBA_OMGVMCID bor 1) -> "Attempt to pass an unactivated
(unregistered) value as an object reference";
'OBJECT_NOT_EXIST'(?CORBA_OMGVMCID bor 2) -> "Failed to create or locate Object
Adapter";
'OBJECT_NOT_EXIST'(?CORBA_OMGVMCID bor 3) -> "Biomolecular Sequence Analysis
Service is no longer available";
'OBJECT_NOT_EXIST'(?CORBA_OMGVMCID bor 4) -> "Object Adapter inactive";
'OBJECT_NOT_EXIST'(_) -> "-".

%% TRANSACTION_REQUIRED - OMG
'TRANSACTION_REQUIRED'(_) -> "-".

%% TRANSACTION_ROLLEDBACK - OMG
'TRANSACTION_ROLLEDBACK'(_) -> "-".

%% INVALID_TRANSACTION - OMG
'INVALID_TRANSACTION'(_) -> "-".

%% INV_POLICY - OMG
'INV_POLICY'(?CORBA_OMGVMCID bor 1) -> "Unable to reconcile IOR specified
policy with effective policy override";
'INV_POLICY'(?CORBA_OMGVMCID bor 2) -> "Invalid PolicyType";
'INV_POLICY'(?CORBA_OMGVMCID bor 3) -> "No PolicyFactory has been registered
for the given PolicyType";
'INV_POLICY'(_) -> "-".

%% CODESET_INCOMPATIBLE - OMG
'CODESET_INCOMPATIBLE'(_) -> "-".

%% REBIND - OMG
'REBIND'(_) -> "-".

%% TIMEOUT - OMG
'TIMEOUT'(_) -> "-".

%% TRANSACTION_UNAVAILABLE - OMG
'TRANSACTION_UNAVAILABLE'(_) -> "-".

%% TRANSACTION_MODE - OMG
'TRANSACTION_MODE'(_) -> "-".

%% BAD_QOS - OMG
'BAD_QOS'(_) -> "-".
    
