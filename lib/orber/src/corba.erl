%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%%--------------------------------------------------------------------
%% File: corba.erl
%% 
%% Description:
%%    This file contains the CORBA::ORB interface plus some 
%%    Orber specific functions.
%%-----------------------------------------------------------------
-module(corba).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% Standard interface CORBA
%%-----------------------------------------------------------------
-export([orb_init/1, orb_init/2]).
%%-----------------------------------------------------------------
%% Standard interface CORBA::ORB
%%-----------------------------------------------------------------
-export([%create_list/2,
	 %create_operation_list/2,
	 %% get_default_context/1,
	 %% 'BOA_init/2,
	 resolve_initial_references/1,
	 resolve_initial_references/2,
	 resolve_initial_references_local/1,
	 list_initial_services/0,
	 add_initial_service/2,
	 remove_initial_service/1,
	 resolve_initial_references_remote/2,
	 resolve_initial_references_remote/3,
	 list_initial_services_remote/1,
	 list_initial_services_remote/2,
	 object_to_string/1, object_to_string/2, 
	 object_to_string/3, object_to_string/4,
	 string_to_object/1,
	 string_to_object/2]).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([create/2,
	 create/3,
	 create/4,
	 create_link/2,
	 create_link/3,
	 create_link/4,
	 create_remote/3,
	 create_remote/5,
	 create_link_remote/3,
	 create_link_remote/5,
	 create_nil_objref/0,
	 dispose/1,
	 create_subobject_key/2,
	 get_subobject_key/1,
	 get_pid/1,
	 raise/1, raise_with_state/2,
	 print_object/1,
	 print_object/2,
	 add_alternate_iiop_address/3,
	 add_FTGroup_component/4,
	 add_FTPrimary_component/1,
	 call_internal/10]).

%%-----------------------------------------------------------------
%% Internal (inside orber implementation) exports
%%-----------------------------------------------------------------
-export([call/4, call/5, reply/2,
	 cast/4, cast/5, locate/1, locate/2, locate/3,
	 request_from_iiop/6,
	 common_create/5,
	 mk_objkey/4,
	 mk_light_objkey/2,
	 objkey_to_string/1,
	 string_to_objkey/1,
	 string_to_objkey_local/1,
	 call_relay/3,
	 cast_relay/2, 
	 handle_init/2,
	 handle_terminate/3,
	 handle_info/3,
	 handle_code_change/4,
	 handle_call/7, 
	 handle_call/10, 
	 handle_cast/9,
	 handle_cast/6,
	 get_implicit_context/1]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 5).

-record(is, {flags = 0}).

%% Defines possible configuration parameters a user can add when
%% creating new CORBA objects.
-record(options, {sup_child = false, 
		  persistent = false, 
		  regname = [], 
		  pseudo = false,
		  object_flags = ?ORB_INIT_FLAGS,
		  object_flags_set = ?ORB_INIT_FLAGS,
		  create_options = [],
		  passive = false,
		  group_id = 0,
		  internal_state}).

-record(extra, {timeout = infinity,
		context = []}).


%%--------------------------------------------------------------------
%% FT stuff
%%--------------------------------------------------------------------
-define(IDL_MODULES, [oe_TimeBase, 
		      oe_CosEventComm,
		      oe_CosEventChannelAdmin,
		      oe_CosNotification,
		      oe_CosNotifyComm,
		      oe_CosNotifyFilter,
		      oe_GIOP]).

-define(groupid_to_table(Integer), 
	list_to_atom("ft_" ++ integer_to_list(Integer))).

-define(RM_TABLE_SPEC, 
	[{attributes, record_info(fields, ft_replication_manager)}]).
-define(RO_TABLE_SPEC,
	[{attributes, record_info(fields, ft_replicated_object)}]).
-define(RR_TABLE_SPEC,
	[{attributes, record_info(fields, ft_reply_retention)}]).

%% how long we're allowed to wait for database tables to be available.
-define(TABLE_TIMEOUT, infinite).

%-record(rm_state, {default_options, type_options, node_port_ips}).

%-record(node_port_ip, {node, port, ip}).

-record(ft_replication_manager, {object_group_id,
				 type_id,
				 primary,
				 iogr, 
				 ref_version,
				 options}).

-record(ft_replicated_object, {group_id, state}).
-record(ft_reply_retention, {retention_id, reply}).

%-record(ft_properties, {replications_style,
%			membership_style,
%			consistency_style,
%			initial_number_replicas,
%			minimum_number_replicas}).

% one should change things work with stdlib:proplist and clean up the mess.
%-record(ft_criteria, {ft_properties, 
%		      object_location,
%		      object_init,
%		      object_impl}).

%%------------------------------------------------------------
%%
%% Implementation of CORBA CORBA::ORB interfaces
%%
%%------------------------------------------------------------

%%create_list(Count) ->
%%    corba_nvlist:create_list(Count).

%%create_operation_list(OpDef) ->
%%    corba_nvlist:create_operation_list(OpDef).

orb_init(KeyValueList) ->
    orb_init(KeyValueList, "ORBER").

orb_init([], _Name) ->
    ok;
orb_init(KeyValueList, _Name) ->
    orber:multi_configure(KeyValueList).

%%-----------------------------------------------------------------
%% Initial reference handling
%%-----------------------------------------------------------------
resolve_initial_references(ObjectId) ->   
    resolve_initial_references(ObjectId, []).
resolve_initial_references(ObjectId, Ctx) ->   
    case use_local_host(ObjectId) of
	true ->
	    orber_initial_references:get(ObjectId);
	Ref ->
	    string_to_object(Ref, Ctx)
    end.

resolve_initial_references_local(ObjectId) ->   
    orber_initial_references:get(ObjectId).

list_initial_services() ->  
    Local = orber_initial_references:list(),
    case orber:get_ORBInitRef() of
	undefined ->
	    Local;
	InitRef ->
	    orber_tb:unique(Local ++ get_prefixes(InitRef, []))
    end.

get_prefixes([], Acc) ->
    Acc;
%% A list of ORBInitRef's
get_prefixes([H|T], Acc) when is_list(H) ->
    [Key|_] = string:tokens(H, "="),
    get_prefixes(T, [Key|Acc]);
%% A single ORBInitRef
get_prefixes(InitRef, _Acc) when is_list(InitRef) ->
    [Key|_] = string:tokens(InitRef, "="),
    [Key];
get_prefixes(What, _) ->
    orber:dbg("[~p] corba:get_prefixes(~p);~nMalformed argument?", 
	      [?LINE, What], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


use_local_host(ObjectId) ->
    case orber:get_ORBInitRef() of
	undefined ->
	    case orber:get_ORBDefaultInitRef() of
		undefined ->
		    true;
		DefRef ->
		    DefRef++"/"++ObjectId
	    end;
	InitRef ->
	    case check_prefixes(InitRef, ObjectId) of
		false ->
		    case orber:get_ORBDefaultInitRef() of
			undefined ->
			    true;
			DefRef ->
			    DefRef++"/"++ObjectId
		    end;
		UseRef ->
		    strip_junk(UseRef)
	    end
    end.


check_prefixes([], _) ->
    false;
%% A list of ORBInitRef's
check_prefixes([H|T], ObjectId) when is_list(H) ->
    case prefix(ObjectId, H) of
	false ->
	    check_prefixes(T, ObjectId);
	UseRef ->
	    UseRef
    end;
%% A single ORBInitRef
check_prefixes(InitRef, ObjectId) when is_list(InitRef) ->
    case prefix(ObjectId, InitRef) of
	false ->
	    false;
	UseRef ->
	    UseRef
    end;
check_prefixes(What,_) -> 
    orber:dbg("[~p] corba:check_prefixes(~p);~nMalformed argument?", 
	      [?LINE, What], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


%% Valid is, for example, "NameService = corbaloc::host/NameService". 
%% Hence, we must remove ' ' and '='.
strip_junk([32|T]) ->
    strip_junk(T);
strip_junk([$=|T]) ->
    strip_junk(T);
strip_junk(Ref) ->
    Ref.

add_initial_service(ObjectId, ObjectRef) ->   
    orber_initial_references:add(ObjectId, ObjectRef).

remove_initial_service(ObjectId) ->  
    orber_initial_references:remove(ObjectId).

resolve_initial_references_remote(ObjectId, Address) ->
    resolve_initial_references_remote(ObjectId, Address, []).

resolve_initial_references_remote(_ObjectId, [], _Ctx) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
resolve_initial_references_remote(ObjectId, [RemoteModifier| Rest], Ctx) 
  when is_list(RemoteModifier) ->
    case parse_remote_modifier(RemoteModifier) of
	{error, _} -> 
	    resolve_initial_references_remote(ObjectId, Rest, Ctx);
	{ok, Host, Port} ->
	    IOR = iop_ior:create_external(orber:giop_version(), "", 
					  Host, list_to_integer(Port), "INIT"),
	    %% We know it's an external referens. Hence, no need to check.
	    {_, Key} = iop_ior:get_key(IOR),
	    orber_iiop:request(Key, 'get', [ObjectId], 
			       {{'tk_objref', 12, "object"},
				[{'tk_string', 0}],
				[]}, 'true', infinity, IOR, Ctx)
    end.

list_initial_services_remote(Address) ->
    list_initial_services_remote(Address, []).

list_initial_services_remote([], _Ctx) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
list_initial_services_remote([RemoteModifier| Rest], Ctx) when is_list(RemoteModifier) ->
    case parse_remote_modifier(RemoteModifier) of
	{error, _} -> 
	    resolve_initial_references_remote(Rest, Ctx);
	{ok, Host, Port} ->
	    IOR = iop_ior:create_external(orber:giop_version(), "", 
					  Host, list_to_integer(Port), "INIT"),
	    %% We know it's an external referens. Hence, no need to check.
	    {_, Key} = iop_ior:get_key(IOR),
	    orber_iiop:request(Key, 'list', [],
			       {{'tk_sequence', {'tk_string',0},0},
				[], []}, 'true', infinity, IOR, Ctx)
    end;
list_initial_services_remote(_, _) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


parse_remote_modifier("iiop://" ++ Rest) ->
    parse_host_version(Rest);
parse_remote_modifier(_RemoteModifier) ->
    {error, not_supported}.

parse_host_version("[" ++ Rest) ->
    parse_ipv6(Rest, []);
parse_host_version(Rest) ->
    parse_ipv4_or_dnsname(Rest, []).


parse_ipv4_or_dnsname([$: |Rest], Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_ipv4_or_dnsname([C |Rest], Acc) ->
    parse_ipv4_or_dnsname(Rest, [C |Acc]).

parse_ipv6("]:" ++ Rest, Acc) ->
    {ok, lists:reverse(Acc), Rest};
parse_ipv6([C |Rest], Acc) ->
    parse_ipv6(Rest, [C |Acc]).

    
%%-----------------------------------------------------------------
%% Objectreference convertions
%%-----------------------------------------------------------------
object_to_string(Object) ->
    iop_ior:string_code(Object).

object_to_string(Object, [H|_] = Hosts) when is_list(H) ->
    iop_ior:string_code(Object, Hosts);
object_to_string(_Object, _Hosts) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

object_to_string(Object, [H|_] = Hosts, Port) when is_list(H) andalso
						   is_integer(Port) ->
    iop_ior:string_code(Object, Hosts, Port);
object_to_string(_Object, _Hosts, _Port) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

object_to_string(Object, [H|_] = Hosts, Port, SSLPort) when is_list(H) andalso 
							    is_integer(Port) andalso
							    is_integer(SSLPort)->
    iop_ior:string_code(Object, Hosts, Port, SSLPort);
object_to_string(_Object, _Hosts, _Port, _SSLPort) ->
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


string_to_object(IORString) ->
    string_to_object(IORString, []).

string_to_object(IORString, Ctx) when is_list(Ctx) ->
    case lists:prefix("IOR", IORString) of
	true ->
	    {ObjRef, _, _} = iop_ior:string_decode(IORString),
	    ObjRef;
	_ ->
	    %% CORBA-2.4 allows both IOR and ior prefix.
	    case lists:prefix("ior", IORString) of
		true ->
		    {ObjRef, _, _} = iop_ior:string_decode(IORString),
		    ObjRef;
		_ ->
		    Data = orber_cosnaming_utils:select_type(IORString),
		    case orber_cosnaming_utils:lookup(Data, Ctx) of
			String when is_list(String) ->
			    {Obj, _, _} = iop_ior:string_decode(String),
			    Obj;
			ObjRef ->
			    ObjRef
		    end
	    end
    end;
string_to_object(IORString, Ctx) ->
    orber:dbg("[~p] corba:string_to_object(~p, ~p);~n"
	      "Failed to supply a context list.", 
	      [?LINE, IORString, Ctx], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%------------------------------------------------------------
%%
%% Implementation of NON-standard functions
%%
%%------------------------------------------------------------
create(Module, TypeID) ->
    create(Module, TypeID, []).

create(Module, TypeID, Env) ->
    common_create(Module, TypeID, Env, [], 'start').

create(Module, TypeID, Env, {Type, RegName}) ->
    common_create(Module, TypeID, Env, [{regname, {Type, RegName}}], 'start');
create(Module, TypeID, Env, Options) ->
    common_create(Module, TypeID, Env, Options, 'start').


create_link(Module, TypeID) ->
    create_link(Module, TypeID, []).

create_link(Module, TypeID, Env) ->
    common_create(Module, TypeID, Env, [], 'start_link').

create_link(Module, TypeID, Env, {Type, RegName}) ->
    common_create(Module, TypeID, Env, [{regname, {Type, RegName}}], 'start_link');
create_link(Module, TypeID, Env, Options) ->
    common_create(Module, TypeID, Env, Options, 'start_link').


create_remote(Node, Module, TypeID) ->
    create_remote(Node, Module, TypeID, []).

create_remote(Node, Module, TypeID, Env) ->
    common_create_remote(Node, Module, TypeID, Env, [], 'start').

create_remote(Node, Module, TypeID, Env, {Type, RegName}) ->
    common_create_remote(Node, Module, TypeID, Env, [{regname, {Type, RegName}}], 'start');
create_remote(Node, Module, TypeID, Env, Options) ->
    common_create_remote(Node, Module, TypeID, Env, Options, 'start').


create_link_remote(Node, Module, TypeID) ->
    create_link_remote(Node, Module, TypeID, []).

create_link_remote(Node, Module, TypeID, Env) ->
    common_create_remote(Node, Module, TypeID, Env, [], 'start_link').

create_link_remote(Node, Module, TypeID, Env, {Type, RegName}) ->
    common_create_remote(Node, Module, TypeID, Env, [{regname, {Type, RegName}}], 'start_link');
create_link_remote(Node, Module, TypeID, Env, Options) ->
    common_create_remote(Node, Module, TypeID, Env, Options, 'start_link').

common_create_remote(Node, Module, TypeID, Env, {Type, RegName}, StartMethod) ->
    common_create_remote(Node, Module, TypeID, Env, [{regname, {Type, RegName}}], StartMethod);
common_create_remote(Node, Module, TypeID, Env, Options, StartMethod) ->
    case node_check(Node) of
	true ->
	    rpc:call(Node, corba, common_create, [Module, TypeID, Env, Options, StartMethod]);
	_ ->
	    orber:dbg("[~p] corba:common_create_remote(~p);~n"
		      "Node not in current domain.", [?LINE, Node], ?DEBUG_LEVEL),
	    raise(#'OBJ_ADAPTER'{completion_status=?COMPLETED_NO})
    end.

node_check(Node) ->
    lists:member(Node,orber:orber_nodes()).

common_create(Module, _TypeID, Env, Options, StartMethod) when is_list(Options) ->
    Opt = evaluate_options(Options, #options{}),
    case Opt#options.regname of
	[] ->
	    ok;
	{'local', Atom} when is_atom(Atom) andalso Opt#options.persistent == false ->
	    ok;
	{'global', _} ->
	    ok;
	Why ->
	    orber:dbg("[~p] corba:common_create(~p, ~p);~n"
		      "Bad name type or combination(~p).", 
		      [?LINE, Module, Options, Why], ?DEBUG_LEVEL),
	    raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				     completion_status=?COMPLETED_NO})   
    end,
    case Opt of
        #options{pseudo = false, passive = false} ->
	    case gen_server:StartMethod(Module, {Opt#options.object_flags, Env}, 
                                        Opt#options.create_options) of
		{ok, Pid} ->
		    case catch mk_objkey(Module, Pid, Opt#options.regname, 
                                         Opt#options.persistent, 
                                         Opt#options.object_flags) of
			{'EXCEPTION', E} ->
			    %% This branch is only used if we couldn't register 
			    %% our new objectkey due to an internal error in orber.
			    gen_server:call(Pid, stop),
			    raise(E);
			{'EXIT', _} ->
			    %% This branch takes care of exit values
			    %% which aren't expected (due to bug).
			    gen_server:call(Pid, stop),
	                    raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				                     completion_status=?COMPLETED_NO});
			Objkey when Opt#options.sup_child == true ->
		            {ok, Pid, Objkey};
                        Objkey ->
			    Objkey
		    end;
		X ->
		    X
	    end;
        #options{pseudo = true, passive = false} ->
	    ModuleImpl = list_to_atom(lists:concat([Module, '_impl'])),
	    case ModuleImpl:init(Env) of
		{ok, State} ->
		    create_subobject_key(mk_pseudo_objkey(Module, ModuleImpl, 
                                                          Opt#options.object_flags),
                                                          State);
		{ok, State,_} ->
		    create_subobject_key(mk_pseudo_objkey(Module, ModuleImpl,
                                                          Opt#options.object_flags),
                                                          State);
		Reason ->
		    orber:dbg("[~p] corba:common_create(~p);~n"
			      "'init' function incorrect(~p).", 
			      [?LINE, ModuleImpl, Reason], ?DEBUG_LEVEL),
	            raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				             completion_status=?COMPLETED_NO})   
	    end;
        #options{pseudo = false, passive = true} ->
            ModuleImpl = list_to_atom(lists:concat([Module, '_impl'])),
            create_subobject_key(mk_passive_objkey(Module, ModuleImpl,
                                                   Opt#options.object_flags),
                                 ?groupid_to_table(Opt#options.group_id));
	What ->
	    orber:dbg("[~p] corba:common_create(~p, ~p);~n"
		      "not a boolean(~p).", 
		      [?LINE, Module, Options, What], ?DEBUG_LEVEL),
	    raise(#'BAD_PARAM'{minor=(?ORBER_VMCID bor 1), 
				     completion_status=?COMPLETED_NO})   
    end.

%%----------------------------------------------------------------------
%% Function   : dispose
%% Arguments  : Object
%% Returns    : 
%% Description: Terminate the object represented by the supplied reference.
%%----------------------------------------------------------------------
dispose(?ORBER_NIL_OBJREF) ->
    ok;
dispose(Obj) ->
    corba_boa:dispose(Obj).

%%----------------------------------------------------------------------
%% Function   : create_nil_objref
%% Arguments  : -
%% Returns    : A NIL object reference
%% Description: 
%%----------------------------------------------------------------------
create_nil_objref() ->
    ?ORBER_NIL_OBJREF.

%%----------------------------------------------------------------------
%% Function   : create_subobject_key
%% Arguments  : A local object reference and an Erlang term().
%% Returns    : A new instance of the supplied reference with the 
%%              sub-object field changed to the given value.
%% Description: Initially, this field is set to 'undefined'
%%----------------------------------------------------------------------
create_subobject_key(Objkey, B) when is_binary(B) ->
    iop_ior:set_privfield(Objkey, B);
create_subobject_key(Objkey, T) ->
    create_subobject_key(Objkey, term_to_binary(T)).

%%----------------------------------------------------------------------
%% Function   : get_subobject_key
%% Arguments  : A local object reference
%% Returns    : Erlang term().
%% Description: Return the value set by using create_subobject_key/2
%%----------------------------------------------------------------------
get_subobject_key(Objkey) ->
    iop_ior:get_privfield(Objkey).

%%----------------------------------------------------------------------
%% Function   : get_pid
%% Arguments  : A local object reference
%% Returns    : If the object is local and is associated with a pid, this
%%              pid is returned. Otherwise, external- or pseudo-object,
%%              an exception is raised.
%% Description: 
%%----------------------------------------------------------------------
get_pid(Objkey) ->
    case iop_ior:get_key(Objkey) of
	{'internal', Key, _, _, _} ->
	    orber_objectkeys:get_pid(Key);
	{'internal_registered', Key, _, _, _} when is_atom(Key) ->
	    case whereis(Key) of
		undefined ->
		    raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
		Pid ->
		    Pid
	    end;
	 R ->
	    orber:dbg("[~p] corba:get_pid(~p);~n"
		      "Probably a pseudo- or external object(~p).", 
		      [?LINE, Objkey, R], ?DEBUG_LEVEL),
	    raise(#'INV_OBJREF'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------------------
%% Function   : raise
%% Arguments  : Local exception representation.
%% Returns    : Throws the exception.
%% Description: 
%%----------------------------------------------------------------------
%% To avoid dialyzer warnings due to the use of exit/throw.
-spec raise(term()) -> no_return().
raise(E) ->
    throw({'EXCEPTION', E}).

%%----------------------------------------------------------------------
%% Function   : raise_with_state
%% Arguments  : Local exception representation.
%% Returns    : Throws the exception.
%% Description: 
%%----------------------------------------------------------------------
%% To avoid dialyzer warnings due to the use of exit/throw.
-spec raise_with_state(term(), term()) -> no_return().
raise_with_state(E, State) ->
    throw({reply, {'EXCEPTION', E}, State}).

%%----------------------------------------------------------------------
%% Function   : reply
%% Arguments  : To - pid
%%              Reply - Erlang term().
%% Returns    : 
%% Description: Used to reply to the invoker but still be able
%%              to do some more work in the callback module.
%%----------------------------------------------------------------------
reply(To, Reply) ->
    gen_server:reply(To, Reply).

%%----------------------------------------------------------------------
%% Function   : print_object
%% Arguments  : An object represented as one of the following:
%%               - local (tuple)
%%               - IOR
%%               - stringified IOR
%%               - corbaloc- or corbaname-schema
%%              IoDevice - the same as the io-module defines.
%% Returns    : 
%% Description: Prints the object's components and profiles.
%%----------------------------------------------------------------------
print_object(Object) ->
    iop_ior:print(Object).
print_object(Object, IoDevice) ->
    iop_ior:print(IoDevice, Object).

%%----------------------------------------------------------------------
%% Function   : add_alternate_iiop_address
%% Arguments  : Local object (tuple or IOR).
%%              IP - IP-string
%%              Port - integer().
%% Returns    : A local IOR with a TAG_ALTERNATE_IIOP_ADDRESS component.
%% Description: 
%%----------------------------------------------------------------------
add_alternate_iiop_address(Obj, Host, Port) when is_list(Host) andalso is_integer(Port) ->
    TC = #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS, 
				component_data = #'ALTERNATE_IIOP_ADDRESS'{
				  'HostID' = Host, 
				  'Port' = Port}},
    iop_ior:add_component(Obj, TC);
add_alternate_iiop_address(_, Host, Port) ->
    orber:dbg("[~p] corba:add_alternate_iiop_address(~p, ~p);~n"
	      "Incorrect argument(s). Host must be IP-string and Port an integer.", 
	      [?LINE, Host, Port], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


%%----------------------------------------------------------------------
%% Function   : add_FTGroup_component
%% Arguments  : Local object (tuple or IOR).
%%              FTDomain - FT Domain. String().
%%              GroupID - Replicated object group's id. Integer(). (ulonglong)
%%              GroupVer - Object group's version number. Integer(). (ulong)
%% Returns    : A local IOR with one TAG_FT_GROUP component.
%% Description: 
%%----------------------------------------------------------------------
add_FTGroup_component(Obj, FTDomain, GroupID, GroupVer) 
  when is_list(FTDomain) andalso is_integer(GroupID) andalso is_integer(GroupVer) andalso
       GroupID >= ?ULONGLONGMIN andalso GroupID =< ?ULONGLONGMAX andalso
       GroupVer >= ?ULONGMIN andalso GroupVer =< ?ULONGMAX ->
    TC = #'IOP_TaggedComponent'{tag = ?TAG_FT_GROUP,
				component_data = #'FT_TagFTGroupTaggedComponent'{
				  version = #'GIOP_Version'{major = 1, minor = 0},
				  ft_domain_id = FTDomain,
				  object_group_id = GroupID,
				  object_group_ref_version = GroupVer}},
    iop_ior:add_component(Obj, TC);
add_FTGroup_component(_Obj, FTDomain, GroupID, GroupVer) ->
    orber:dbg("[~p] corba:add_FTGroup_component(~p, ~p, ~p);~n"
	      "Incorrect argument(s).",
	      [?LINE, FTDomain, GroupID, GroupVer], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status = ?COMPLETED_NO}).


%%----------------------------------------------------------------------
%% Function   : add_FTPrimary_component
%% Arguments  : Local object (tuple or IOR).
%% Returns    : A local IOR with one TAG_FT_PRIMARY component.
%% Description: 
%%----------------------------------------------------------------------
add_FTPrimary_component(Obj) ->
    TC = #'IOP_TaggedComponent'{
      tag=?TAG_FT_PRIMARY,
      component_data=#'FT_TagFTPrimaryTaggedComponent'{primary = true}},
    iop_ior:add_component(Obj, TC).


%%-----------------------------------------------------------------
%% Generic functions for accessing the call-back modules (i.e. X_impl.erl).
%% These functions are invoked by the generated stubs.
%%-----------------------------------------------------------------
handle_init(M, {Flags, Env}) ->
    case M:init(Env) of
	{ok, State} ->
	    {ok, {#is{flags = Flags}, State}};
	{ok,State,Timeout} ->
	    {ok, {#is{flags = Flags}, State}, Timeout};
	Other ->
	    %% E.g. ignore | {stop, Reason}
	    Other
    end.


handle_terminate(M, Reason, {_InternalState, State}) ->
    catch (M:terminate(Reason, State)).

handle_info(M, Info, {InternalState, State}) ->
    case catch M:handle_info(Info, State) of
	{noreply,NewState} ->
	    {noreply, {InternalState, NewState}};
	{noreply, NewState, Timeout} ->
	    {noreply, {InternalState, NewState}, Timeout};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{'EXIT', Why} ->
	    handle_exit(InternalState, State, Why, true, 
			{M, handle_info}, [Info, State])
    end.

handle_code_change(M, OldVsn, {InternalState, State}, Extra) ->
    {ok, NewState} = M:code_change(OldVsn, State, Extra),
    {ok, {InternalState, NewState}}.


%% This function handles call Pre- & Post-conditions.
handle_call(M, F, A, {InternalState, State}, Ctx, This, From,
	    PreData, PostData, Stub) ->
    CArgs = call_state(A, State, This, From),
    case catch invoke_precond(PreData, Stub, F, CArgs) of
	{'EXIT', Why} ->
	    handle_exit(InternalState, State, Why, false, PreData, [Stub, F, CArgs]);
	{'EXCEPTION', E} ->
	    {reply, {'EXCEPTION', E}, {InternalState, State}};
	ok ->
	    Result = handle_call2(M, F, CArgs, InternalState, State, Ctx),
	    case catch invoke_postcond(PostData, Stub, F, CArgs, Result) of
		{'EXIT', Why} ->
		    handle_exit(InternalState, State, Why, false, PostData, A);
		{'EXCEPTION', E} ->
		    {reply, {'EXCEPTION', E}, {InternalState, State}};
		ok ->
		    Result
	    end
    end.


invoke_precond(false, _, _, _) ->
    ok;
invoke_precond({CondM, CondF}, Stub, F, CArgs) ->
    CondM:CondF(Stub, F, CArgs).

%% We must remove the Internal State before invoking post-cond.
invoke_postcond(false, _, _, _, _) ->
    ok;
invoke_postcond({CondM, CondF}, Stub, F, CArgs,	{reply, Reply, {_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {reply, Reply, NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {reply, Reply, {_, NS}, Timeout}) ->
    CondM:CondF(Stub, F, CArgs, {reply, Reply, NS, Timeout});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {stop, Reason, Reply, {_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {stop, Reason, Reply, NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {stop, Reason, {_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {stop, Reason, NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {noreply,{_, NS}}) ->
    CondM:CondF(Stub, F, CArgs, {noreply,NS});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, {noreply,{_, NS}, Timeout}) ->
    CondM:CondF(Stub, F, CArgs, {noreply, NS, Timeout});
invoke_postcond({CondM, CondF}, Stub, F, CArgs, Result) ->
    CondM:CondF(Stub, F, CArgs, Result).


handle_call(M, F, A, {InternalState, State}, Ctx, This, From) ->
    handle_call2(M, F, call_state(A, State, This, From), InternalState, State, Ctx).

handle_call2(M, F, A, InternalState, State, []) ->
    case catch apply(M, F, A) of
	{reply, Reply, NewState} ->
	    {reply, add_context(Reply), {InternalState, NewState}};
	{reply, Reply, NewState, Timeout} ->
	    {reply, add_context(Reply), {InternalState, NewState}, Timeout};
	{stop, Reason, Reply, NewState} ->
	    {stop, Reason, add_context(Reply), {InternalState, NewState}};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{noreply,NewState} ->
	    {noreply,{InternalState, NewState}};
	{noreply,NewState,Timeout} ->
	    {noreply,{InternalState, NewState},Timeout};
	{'EXIT', Reason} ->
	    handle_exit(InternalState, State, Reason, false, {M, F}, A);
	{'EXCEPTION', E} ->
	    {reply, add_context({'EXCEPTION', E}), {InternalState, State}};
	{Reply, NewState} -> 
	    {reply, add_context(Reply), {InternalState, NewState}}
    end;
handle_call2(M, F, A, InternalState, State, Ctx) ->
    %% Set the new Context.
    put(oe_server_in_context, Ctx),
    case catch apply(M, F, A) of
	{reply, Reply, NewState} ->
	    put(oe_server_in_context, undefined),
	    {reply, add_context(Reply), {InternalState, NewState}};
	{reply, Reply, NewState, Timeout} ->
	    put(oe_server_in_context, undefined),
	    {reply, add_context(Reply), {InternalState, NewState}, Timeout};
	{stop, Reason, Reply, NewState} ->
	    {stop, Reason, add_context(Reply), {InternalState, NewState}};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{noreply,NewState} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}};
	{noreply, {InternalState, NewState}, Timeout} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState},Timeout};
	{'EXIT', Reason} ->
		handle_exit(InternalState, State, Reason, false, {M, F}, A);
	{'EXCEPTION', E} ->
	    put(oe_server_in_context, undefined),
	    {reply, add_context({'EXCEPTION', E}), {InternalState, State}};
	{Reply, NewState} -> 
	    put(oe_server_in_context, undefined),
	    {reply, add_context(Reply), {InternalState, NewState}}
    end.

call_state(A, State, false, false) ->
    [State|A];
call_state(A, State, false, From) ->
    [From, State|A];
call_state(A, State, This, false) ->
    [This, State|A];
call_state(A, State, This, From) ->
    [This, From, State|A].

cast_state(A, State, false) ->
    [State|A];
cast_state(A, State, This) ->
    [This, State|A].

add_context(Reply) ->
    %% Reset oe_server_out_context
    case put(oe_server_out_context, undefined) of
	undefined ->
	    Reply;
	 _OutCtx ->
	    %% The previous value wasn't 'undefined', which means that
	    %% the server supplied a return context.
	    Reply
    end.


%% This function handles call Pre- & Post-conditions.
handle_cast(M, F, A, {InternalState, State}, Ctx, This, PreData, PostData, Stub) ->
    CArgs = cast_state(A, State, This),
    case catch invoke_precond(PreData, Stub, F, CArgs) of
	{'EXIT', Why} ->
	    handle_exit(InternalState, State, Why, true, PreData, [Stub, F, CArgs]);
	{'EXCEPTION', _} ->
	    {noreply, {InternalState, State}};
	ok ->
	    Result = handle_cast2(M, F, CArgs, InternalState, State, Ctx),
	    case catch invoke_postcond(PostData, Stub, F, CArgs, Result) of
		{'EXIT', Why} ->
		    handle_exit(InternalState, State, Why, true, PostData, A);
		{'EXCEPTION', _} ->
		    {noreply, {InternalState, State}};
		ok ->
		    Result
	    end
    end.


handle_cast(M, F, A, {InternalState, State}, Ctx, This) ->
    handle_cast2(M, F, cast_state(A, State, This), InternalState, State, Ctx).

handle_cast2(M, F, A, InternalState, State, []) ->
    case catch apply(M, F, A) of
	{noreply, NewState} ->
	    {noreply, {InternalState, NewState}};
	{noreply, NewState, Timeout} ->
	    {noreply, {InternalState, NewState}, Timeout};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{'EXCEPTION', _} ->
	    {noreply, {InternalState, State}};
	{'EXIT', Reason} -> 
	    handle_exit(InternalState, State, Reason, true, {M, F}, A);
	NewState -> 
	    {noreply, {InternalState, NewState}}
    end;
handle_cast2(M, F, A, InternalState, State, Ctx) ->
    put(oe_server_in_context, Ctx),
    case catch apply(M, F, A) of
	{noreply, NewState} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}};
	{noreply, NewState, Timeout} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}, Timeout};
	{stop, Reason, NewState} ->
	    {stop, Reason, {InternalState, NewState}};
	{'EXCEPTION', _} ->
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, State}};
	{'EXIT', Reason} -> 
	    handle_exit(InternalState, State, Reason, true, {M, F}, A);
	NewState -> 
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, NewState}}
    end.

handle_exit(InternalState, State, {undef, [{M, F, _, _}|_]} = Reason, 
	    OnewayOp, {M, F}, A) ->
    case catch check_exports(M:module_info(exports), F) of
	{'EXIT',{undef,_}} ->
	    %% No such module.
	    orber:dbg("~p.beam doesn't exist.~n"
		      "Check IC compile options (e.g. 'impl') and that the~n"
		      "beam-file is load-able.",
		      [M], ?DEBUG_LEVEL),
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 1),
					    completion_status=?COMPLETED_MAYBE});
	"" ->
	    orber:dbg("~p:~p/~p doesn't exist.~n"
		      "Check spelling, export-attributes etc",
		      [M, F, length(A)], ?DEBUG_LEVEL),
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 2),
					    completion_status=?COMPLETED_MAYBE});
	Exports when is_list(Exports) ->
	    orber:dbg("~p:~p/~p doesn't exist.~n"
		      "~p:~p~s do exists.~nCheck export-attributes etc",
		      [M, F, length(A), M, F, Exports], ?DEBUG_LEVEL),
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 3),
					    completion_status=?COMPLETED_MAYBE});
	_ ->
	    %% Should never happen
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 4),
					    completion_status=?COMPLETED_MAYBE})
    end;
handle_exit(InternalState, State, {undef, [{M2, F2, A2, _}|_]} = Reason, 
	    OnewayOp, {M, F}, A) ->
    case catch check_exports(M2:module_info(exports), F2) of
	{'EXIT',{undef,_}} ->
	    %% No such module.
	    orber:dbg("~p.beam doesn't exist.~n"
		      "~p:~p/~p invoked an operation on the module above.~n"
		      "Check IC compile options and that the beam-file is load-able.",
		      [M2, M, F, length(A)], ?DEBUG_LEVEL),
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 5),
					    completion_status=?COMPLETED_MAYBE});
	"" ->
	    orber:dbg("~p:~p/~p doesn't exist.~n"
		      "~p:~p/~p invoked the operation above~n"
		      "Check spelling, export-attributes etc",
		      [M2, F2, length(A2), M, F, length(A)], ?DEBUG_LEVEL),
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 6),
					    completion_status=?COMPLETED_MAYBE});
	Exports when is_list(Exports) ->
	    orber:dbg("~p:~p/~p doesn't exist.~n"
		      "~p:~p~s do exist(s).~nCheck export-attributes etc~n"
		      "~p:~p/~p invoked the operation above~n",
		      [M2, F2, length(A2), M2, F2, Exports, M, F, length(A)], ?DEBUG_LEVEL),
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 7),
					    completion_status=?COMPLETED_MAYBE});
	_ ->
	    %% Should never happen
	    reply_after_exit(InternalState, State, Reason, OnewayOp,
			     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 4),
					    completion_status=?COMPLETED_MAYBE})
    end;
%% Misc errors. We separate between direct and in-direct errors. Due to different
%% notation we must separate between different cases.
handle_exit(InternalState, State, {{case_clause,_}, [{M, F, _}|_]} = Reason, 
	    OnewayOp, {M, F}, A) ->
    orber:dbg("~p:~p/~p contains a 'case_clause' error.",
	      [M, F, length(A)], ?DEBUG_LEVEL),
    reply_after_exit(InternalState, State, Reason, OnewayOp,
		     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 8),
				    completion_status=?COMPLETED_MAYBE});
handle_exit(InternalState, State, {Reason, [{M, F, _}|_]}, OnewayOp, {M, F}, A) ->
    orber:dbg("~p:~p/~p contains a '~p' error.",
	      [M, F, length(A), Reason], ?DEBUG_LEVEL),
    reply_after_exit(InternalState, State, Reason, OnewayOp,
		     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 8),
				    completion_status=?COMPLETED_MAYBE});
handle_exit(InternalState, State, {function_clause, [{M2, F2, A2}|_]} = Reason, 
	    OnewayOp, {M, F}, A) ->
    orber:dbg("~p:~p/~p contains a 'function_clause' error.~n"
	      "Invoked via the operation:~n"
	      "~p:~p/~p",
	      [M2, F2, length(A2), M, F, length(A)], ?DEBUG_LEVEL),
    reply_after_exit(InternalState, State, Reason, OnewayOp,
		     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 9),
				    completion_status=?COMPLETED_MAYBE});
handle_exit(InternalState, State, {{case_clause,_}, [{M2, F2, A2}|_]} = Reason, 
	    OnewayOp, {M, F}, A) ->
    orber:dbg("~p:~p/~p contains a 'case_clause' error.~n"
	      "Invoked via the operation:~n"
	      "~p:~p/~p",
	      [M2, F2, A2, M, F, length(A)], ?DEBUG_LEVEL),
    reply_after_exit(InternalState, State, Reason, OnewayOp,
		     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 9),
				    completion_status=?COMPLETED_MAYBE});
handle_exit(InternalState, State, {Reason, [{M2, F2, A2}|_]} = Reason,
	    OnewayOp, {M, F}, A) ->
    orber:dbg("~p:~p/~p contains a '~p' error.~n"
	      "Invoked via the operation:~n"
	      "~p:~p/~p",
	      [M2, F2, A2, Reason, M, F, length(A)], ?DEBUG_LEVEL),
    reply_after_exit(InternalState, State, Reason, OnewayOp,
		     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 9),
				    completion_status=?COMPLETED_MAYBE});
handle_exit(InternalState, State, Reason, OnewayOp, {M, F}, A) ->
    orber:dbg("~p:~p(~p) ->~n"
	      "  {EXIT, ~p}~n",
	      [M, F, A, Reason], ?DEBUG_LEVEL),
    reply_after_exit(InternalState, State, Reason, OnewayOp,
		     #'OBJ_ADAPTER'{minor=(?ORBER_VMCID bor 10),
				    completion_status=?COMPLETED_MAYBE}).


reply_after_exit(#is{flags = Flags} = InternalState, State, 
		 Reason, OnewayOp, Exc) ->
    case ?ORB_FLAG_TEST(Flags, ?ORB_SURVIVE_EXIT) of
	false ->
	    exit(Reason);
	true when OnewayOp == false ->
	    put(oe_server_in_context, undefined),
	    {reply, {'EXCEPTION', Exc}, {InternalState, State}};
	true ->
	    %% One-way operation. Cannot return exception.
	    put(oe_server_in_context, undefined),
	    {noreply, {InternalState, State}}
    end.
	    

check_exports(Exports, Op) ->
    check_exports(Exports, Op, []).

check_exports([], _, Acc) ->
    Acc;
check_exports([{Op, Arity}|Rest], Op, Acc)  ->
    check_exports(Rest, Op, Acc ++ "/" ++ integer_to_list(Arity));
check_exports([_|Rest], Op, Acc)  ->
    check_exports(Rest, Op, Acc).


%%-----------------------------------------------------------------
%% Corba:call - the function for reqests
%%-----------------------------------------------------------------
call(Obj, Func, Args, TypesOrMod) ->
    call_helper(Obj, Func, Args, TypesOrMod, infinity, []).

call(Obj, Func, Args, TypesOrMod, [{context, Ctx}]) ->
    call_helper(Obj, Func, Args, TypesOrMod, infinity, Ctx);
call(Obj, Func, Args, TypesOrMod, [{timeout, Timeout}]) ->
    call_helper(Obj, Func, Args, TypesOrMod, Timeout, []);
call(Obj, Func, Args, TypesOrMod, Extra) when is_list(Extra) ->
    ExtraData = extract_extra_data(Extra, #extra{}),
    call_helper(Obj, Func, Args, TypesOrMod, ExtraData#extra.timeout, 
		ExtraData#extra.context);
call(Obj, Func, Args, TypesOrMod, Timeout) ->
    call_helper(Obj, Func, Args, TypesOrMod, Timeout, []).

call_helper(Obj, Func, Args, TypesOrMod, Timeout, InCtx) ->
    Ctx = get_implicit_context(InCtx),
    case iop_ior:get_key(Obj) of
	{'internal', Key, _, Flags, Mod} ->
	    Pid = orber_objectkeys:get_pid(Key),
	    call_internal(Pid, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), 
			  ?ORB_FLAG_TEST(Flags, ?ORB_USE_PI), Mod, Timeout, Ctx);
	{'internal_registered', Key, _, Flags, Mod} ->
	    call_internal(Key, Obj, Func, Args, TypesOrMod,
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), 
			  ?ORB_FLAG_TEST(Flags, ?ORB_USE_PI), Mod, Timeout, Ctx);
	{'external', Key} when is_atom(TypesOrMod) ->		   
	    case catch TypesOrMod:oe_tc(Func) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:call_helper(~p);~n"
			      "The call-back module does not exist or"
			      " incorrect IC-version used.~nReason: ~p", 
			      [?LINE, TypesOrMod, What], ?DEBUG_LEVEL),
		    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO});
		undefined ->
		    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
					   completion_status=?COMPLETED_NO});
		Types ->
		    orber_iiop:request(Key, Func, Args, Types, 'true', Timeout, Obj, Ctx)
	    end;
	{'external', Key} ->		   
	    orber_iiop:request(Key, Func, Args, TypesOrMod, 'true', Timeout, Obj, Ctx)
    end.

get_implicit_context([]) ->
    case get(oe_server_in_context) of
	undefined ->
	    [];
	ImplCtx ->
	    ImplCtx
    end;
get_implicit_context(Ctx) ->
    case get(oe_server_in_context) of
	undefined ->
	    Ctx;
	ImplCtx ->
	    %% Both defined. An explicit interface context overrides
	    %% an implicit.
	    case check_for_interface_ctx(Ctx) of
		false ->
		    ImplCtx;
		true ->
		    remove_interface_ctx(ImplCtx, Ctx)
	    end
    end.

check_for_interface_ctx([]) ->
    false;
check_for_interface_ctx([#'IOP_ServiceContext'
			 {context_id=?ORBER_GENERIC_CTX_ID, 
			  context_data = {interface, _I}}|_]) ->
    true;
check_for_interface_ctx([_|T]) ->
    check_for_interface_ctx(T).

remove_interface_ctx([], Acc) ->
    Acc;
remove_interface_ctx([#'IOP_ServiceContext'
		      {context_id=?ORBER_GENERIC_CTX_ID, 
		       context_data = {interface, _I}}|T], Acc) ->
    remove_interface_ctx(T, Acc);
remove_interface_ctx([H|T], Acc) ->
    remove_interface_ctx(T, [H|Acc]).


extract_extra_data([], ED) ->
    ED;
extract_extra_data([{context, Ctx}|T], ED) ->
    extract_extra_data(T, ED#extra{context = Ctx});
extract_extra_data([{timeout, Timeout}|T], ED) ->
    extract_extra_data(T, ED#extra{timeout = Timeout}).

call_internal(Pid, Obj, Func, Args, Types, Check, PI, Mod, Timeout, Ctx) 
  when is_pid(Pid) andalso node(Pid) == node() ->
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    case catch gen_server:call(Pid, {Obj, Ctx, Func, Args}, Timeout) of
	{'EXCEPTION', E} ->
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', E}),
            typecheck_reply(Check, {'EXCEPTION', E}, Mod, Func),
	    raise(E);
	{'EXIT',{timeout, _}} ->
	    Exc = #'TIMEOUT'{completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	{'EXIT',R} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p);~ncall exit(~p).", 
		      [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 4), 
			       completion_status=?COMPLETED_NO},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	Res ->
	    invoke_pi_reply(PI, Obj, Ctx, Func, Res),
            typecheck_reply(Check, Res, Types, Func),
	    Res
    end;
call_internal(Pid, Obj, Func, Args, Types, Check, PI, 
	      _Mod, Timeout, Ctx) when is_pid(Pid) ->
    typecheck_request(Check, Args, Types, Func),
    case catch rpc:call(node(Pid), corba, call_relay, 
			[Pid, {Obj, Ctx, Func, Args}, Timeout]) of
	{'EXCEPTION', E} ->
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', E}),
            typecheck_reply(Check, {'EXCEPTION', E}, Types, Func),
	    raise(E);
	{badrpc, {'EXIT',R}} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p);~ncall exit(~p).", 
		      [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 3), 
			       completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	{badrpc,nodedown} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p);~nNode ~p down.", 
		      [?LINE, Func, Args, Types, node(Pid)], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 2), 
			       completion_status=?COMPLETED_NO},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	{badrpc, Reason} ->
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p);~n"
		      "Unable to invoke operation due to: ~p", 
		      [?LINE, Func, Args, Types, Reason], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 5), 
			       completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	Res ->
	    invoke_pi_reply(PI, Obj, Ctx, Func, Res),
            typecheck_reply(Check, Res, Types, Func),
	    Res
    end;

%% This case handles if the reference is created as a Pseudo object. 
%% Just call apply/3.
call_internal({pseudo, Module}, Obj, Func, Args, Types, Check, PI,
	      _Mod, _Timeout, Ctx) ->
    OldCtx = put(oe_server_in_context, Ctx),
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    State = binary_to_term(get_subobject_key(Obj)),
    case catch apply(Module, Func, [Obj, State|Args]) of
	{noreply, _} ->
	    put(oe_server_in_context, OldCtx),
	    ok;
	{noreply, _, _} ->
	    put(oe_server_in_context, OldCtx),
	    ok;
	{reply, Reply, _} ->
	    put(oe_server_in_context, OldCtx),
	    invoke_pi_reply(PI, Obj, Ctx, Func, Reply),
            typecheck_reply(Check, Reply, Types, Func),
	    Reply;
	{reply, Reply, _, _} ->
	    put(oe_server_in_context, OldCtx),
	    invoke_pi_reply(PI, Obj, Ctx, Func, Reply),
            typecheck_reply(Check, Reply, Types, Func),
	    Reply;
	{stop, _, Reply, _} ->
	    put(oe_server_in_context, OldCtx),
	    invoke_pi_reply(PI, Obj, Ctx, Func, Reply),
            typecheck_reply(Check, Reply, Types, Func),
	    Reply;
	{stop, _, _} ->
	    put(oe_server_in_context, OldCtx),
	    ok;
	{'EXCEPTION', E} ->
	    put(oe_server_in_context, OldCtx),
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', E}),
            typecheck_reply(Check, {'EXCEPTION', E}, Types, Func),
	    raise(E);
	{'EXIT', What} ->
	    put(oe_server_in_context, OldCtx),
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p);~n"
		      "Pseudo object exit(~p).", 
		      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
			       completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	Unknown ->
	    put(oe_server_in_context, OldCtx),
	    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p);~n"
		      "Pseudo object failed due to bad return value (~p).", 
		      [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 6),
			       completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc)
    end;
call_internal({passive, Module}, Obj, Func, Args, Types, Check, PI, 
	      Mod, Timeout, Ctx) ->
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    GroupID = binary_to_term(get_subobject_key(Obj)),
    Transaction = 
	fun() ->
		ObjectGroup = read_object_group(GroupID),
		call_primary_protected(ObjectGroup, Module, Obj,
				       Func, Args, GroupID,
				       get_FTRequestCtx(Ctx))
	end,
    case mnesia:transaction(Transaction) of
	{atomic, Reply} ->
	    %% this check should be inside transaction so that
	    %% failing typecheck_reply would result in transaction 
	    %% abortion. Or not. call_internal(Registered...) does not
	    %% cancel the state transition even if the result isn't type compliant. 
	    %% So, we do likewise.
	    typecheck_reply(Check, Reply, Mod, Func),
	    Reply;
	{aborted, {not_primary, Primary, _}} ->
	    FTRequestCtx = mk_FTRequestCtx(10000000),
	    case rpc:call(Primary, corba, call_internal, 
			  [{passive, Module}, Obj, Func, Args, 
			   Types, Check, PI, Mod, Timeout, 
			   [FTRequestCtx|Ctx]]) of
		{badrpc, Reason} ->
		    orber:dbg("[~p] corba:call_passive(~p, ~p, ~p); ~n"
			      " badrpc(~p).",
			      [?LINE, Func, Args, Types, Reason],?DEBUG_LEVEL),
		    raise(#'TRANSIENT'{minor=0,
				       completion_status=?COMPLETED_MAYBE});
		%% one should keep trying request_duration_policy_value -time.
		{'EXCEPTION', E} ->
		    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', E}),
		    raise(E);
		Reply ->
		    %% is this typecheck_reply neccessary? The check is made
		    %% on the remote node...
		    invoke_pi_reply(PI, Obj, Ctx, Func, Reply),	    
		    typecheck_reply(Check, Reply, Mod, Func),
		    Reply
		    %% generate RetentionID's and call Primary node with flag that tells
		    %% the node not to escalate rpc call's to next node if the primary
		    %% has changed again.
		    %% raise({not_primary, Primary});
	    end;
	{aborted, {throw, {'EXCEPTION', E}}} ->
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', E}),
	    typecheck_reply(Check, {'EXCEPTION', E}, Mod, Func),
	    raise(E);
	{aborted, {'EXIT', What}} ->
	    orber:dbg("[~p] corba:call_passive(~p, ~p, ~p); " ++
		      "Passive object exit(~p).", 
		      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
			       completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),	    
	    raise(Exc);
	{aborted, Unknown} ->
	    orber:dbg("[~p] corba:call_passive(~p, ~p, ~p); " ++
		      "Passive object failed due to bad return value (~p).", 
		      [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
	    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 6),
			       completion_status=?COMPLETED_MAYBE},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),	    
	    raise(Exc)
    end;
call_internal(Registered, Obj, Func, Args, Types, Check, PI,
	      _Mod, Timeout, Ctx) when is_atom(Registered)->
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    case whereis(Registered) of
	undefined ->
	    Exc = #'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO},
	    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
	    raise(Exc);
	P ->
	    case catch gen_server:call(P, {Obj, Ctx, Func, Args}, Timeout) of
		{'EXCEPTION', E} ->
		    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', E}),
		    typecheck_reply(Check, {'EXCEPTION', E}, Types, Func),
		    raise(E);
		{'EXIT',{timeout, _}} ->
		    Exc = #'TIMEOUT'{completion_status=?COMPLETED_MAYBE},
		    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
		    raise(Exc);	
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p, ~p, ~p).~n"
			      "call exit(~p).", 
			      [?LINE, Func, Args, Types, R], ?DEBUG_LEVEL),
		    Exc = #'TRANSIENT'{minor=(?ORBER_VMCID bor 5), 
				       completion_status=?COMPLETED_MAYBE},
		    invoke_pi_reply(PI, Obj, Ctx, Func, {'EXCEPTION', Exc}),
		    raise(Exc);
		Res ->
		    invoke_pi_reply(PI, Obj, Ctx, Func, Res),
                    typecheck_reply(Check, Res, Types, Func),
		    Res
	    end
    end.

invoke_pi_request(false, _Obj, _Ctx, _Func, _Args) ->
    ok;
invoke_pi_request(_, Obj, Ctx, Func, Args) ->
    case orber:get_cached_interceptors() of
	{native, PIs} ->
	    orber_pi:out_request(PIs, Obj, Ctx, Func, "localhost", Args);
	_ ->
	    ok
    end.

invoke_pi_reply(false, _Obj, _Ctx, _Func, _Res) ->
    ok;
invoke_pi_reply(_, Obj, Ctx, Func, Res) ->
    case orber:get_cached_interceptors() of
	{native, PIs} ->
	    orber_pi:in_reply(PIs, Obj, Ctx, Func, "localhost", Res);
	_ ->
	    ok
    end.

typecheck_request(false, _, _, _) ->
    ok;
typecheck_request(true, Args, Mod, Func) when is_atom(Mod) ->
    case catch Mod:oe_tc(Func) of
	undefined ->
	    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
				   completion_status=?COMPLETED_NO});
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:typecheck_request(~p, ~p, ~p);~n"
		      "The call-back module does not exist or incorrect"
		      "IC-version used.~nReason: ~p", 
		      [?LINE, Mod, Func, Args, What], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
			       completion_status=?COMPLETED_NO});
	Types ->
	    typecheck_request_helper(Types, Args, Mod, Func)
    end;
typecheck_request(true, Args, Types, Func) ->
    typecheck_request_helper(Types, Args, Types, Func).

typecheck_request_helper(Types, Args, Mod, Func) ->
    case catch cdr_encode:validate_request_body(
		 #giop_env{version = {1,2}, tc = Types, parameters = Args, 
			   host = orber:host(), iiop_port = orber:iiop_port(),
			   iiop_ssl_port = orber:iiop_ssl_port(),
			   domain = orber:domain(),
			   partial_security = orber:partial_security(),
			   flags = orber:get_flags()}) of
	{'EXCEPTION', E} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========= Orber Typecheck Request =========~n"
				   "Invoked......: ~p:~p/~p~n"
				   "Typecode.....: ~p~n"
				   "Arguments....: ~p~n"
				   "Result.......: ~p~n"
				   "===========================================~n", 
				   [Mod, Func, length(TC), TC, Args, {'EXCEPTION', E}]),
	    raise(E);
	{'EXIT',R} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========= Orber Typecheck Request =========~n"
				   "Invoked......: ~p:~p/~p~n"
				   "Typecode.....: ~p~n"
				   "Arguments....: ~p~n"
				   "Result.......: ~p~n"
				   "===========================================~n", 
				   [Mod, Func, length(TC), TC, Args, {'EXIT',R}]),
	    raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	_ ->
	    ok
    end.

typecheck_reply(true, Args, Mod, Func) when is_atom(Mod) ->
    case catch Mod:oe_tc(Func) of
	undefined ->
	    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
				   completion_status=?COMPLETED_NO});
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:typecheck_reply(~p, ~p, ~p);~n"
		      "The call-back module does not exist or incorrect"
		      " IC-version used.~nReason: ~p", 
		      [?LINE, Mod, Func, Args, What], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO});
	Types ->
	    typecheck_reply_helper(Types, Args, Mod, Func)
    end;
typecheck_reply(true, Args, Types, Func) ->
    typecheck_reply_helper(Types, Args, Types, Func);
typecheck_reply(_, _, _, _) ->
    ok.

typecheck_reply_helper(Types, Args, Mod, Func) ->
    case catch cdr_encode:validate_reply_body(
		 #giop_env{version = {1,2}, tc = Types, 
			   host = orber:host(), iiop_port = orber:iiop_port(),
			   iiop_ssl_port = orber:iiop_ssl_port(),
			   domain = orber:domain(),
			   partial_security = orber:partial_security(),
			   flags = orber:get_flags()}, Args) of
	{'tk_except', ExcType, ExcTC, {'EXCEPTION', E}} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========~n"
				   "Invoked........: ~p:~p/~p~n"
				   "Exception Type.: ~p~n"
				   "Typecode.......: ~p~n"
				   "Raised.........: ~p~n"
				   "Result.........: ~p~n"
				   "===========================================~n", 
				   [Mod, Func, length(TC), ExcType, ExcTC, Args, {'EXCEPTION', E}]),
	    raise(E);
	{'EXCEPTION', E} ->
	    {RetType, TC, OutParams} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========~n"
				   "Invoked......: ~p:~p/~p~n"
				   "Typecode.....: ~p~n"
				   "Reply........: ~p~n"
				   "Result.......: ~p~n"
				   "===========================================~n",
				   [Mod, Func, length(TC), [RetType | OutParams], Args, {'EXCEPTION', E}]),
	    raise(E);
	{'tk_except', ExcType, ExcTC, {'EXIT',R}} ->
	    {_, TC, _} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========~n"
				   "Invoked........: ~p:~p/~p~n"
				   "Exception Type.: ~p~n"
				   "Typecode.......: ~p~n"
				   "Raised.........: ~p~n"
				   "Result.........: ~p~n"
				   "===========================================~n",
				   [Mod, Func, length(TC), ExcType, ExcTC, Args, {'EXIT',R}]),
	    raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	{'EXIT',R} ->
	    {RetType, TC, OutParams} = Types, 
	    error_logger:error_msg("========== Orber Typecheck Reply ==========~n"
				   "Invoked........: ~p:~p/~p~n"
				   "Typecode.......: ~p~n"
				   "Reply..........: ~p~n"
				   "Result.........: ~p~n"
				   "===========================================~n", 
				   [Mod, Func, length(TC), [RetType | OutParams], Args, {'EXIT',R}]),
	    raise(#'MARSHAL'{completion_status=?COMPLETED_MAYBE});
	_ ->
	    ok
    end.

call_relay(Pid, Data, Timeout) ->
    case whereis(orber_objkeyserver) of
	undefined ->
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_MAYBE});
	_ ->
	    case catch gen_server:call(Pid, Data, Timeout) of
		{'EXCEPTION', E} ->
		    raise(E);
		{'EXIT',{timeout, _}} ->
		    raise(#'TIMEOUT'{completion_status=?COMPLETED_MAYBE});
		{'EXIT',R} ->
		    orber:dbg("[~p] corba:call_internal(~p);~n"
			      "call exit(~p).", [?LINE, Data, R], ?DEBUG_LEVEL),
		    exit(R);
		Res ->
		    Res
	    end
    end.
	
%%-----------------------------------------------------------------
%% Corba:cast - the function for ONEWAY requests
%%-----------------------------------------------------------------
cast(Obj, Func, Args, TypesOrMod) ->
    cast_helper(Obj, Func, Args, TypesOrMod, []).

cast(Obj, Func, Args, TypesOrMod, [{context, Ctx}]) ->
    cast_helper(Obj, Func, Args, TypesOrMod, Ctx).

cast_helper(Obj, Func, Args, TypesOrMod, InCtx) ->
    Ctx = get_implicit_context(InCtx),
    case iop_ior:get_key(Obj) of
	{'internal', Key, _, Flags, Mod} ->
	    Pid = orber_objectkeys:get_pid(Key),
	    cast_internal(Pid, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), 
			  ?ORB_FLAG_TEST(Flags, ?ORB_USE_PI), Mod, Ctx);
	{'internal_registered', Key, _, Flags, Mod} ->
	    cast_internal(Key, Obj, Func, Args, TypesOrMod, 
			  ?ORB_FLAG_TEST(Flags, ?ORB_TYPECHECK), 
			  ?ORB_FLAG_TEST(Flags, ?ORB_USE_PI), Mod, Ctx);
	{'external', Key} when is_atom(TypesOrMod) ->
	    case catch TypesOrMod:oe_tc(Func) of
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:cast_helper(~p);~n"
			      "The call-back module does not exist or incorrect"
			      " IC-version used.~nReason: ~p", 
			      [?LINE, TypesOrMod, What], ?DEBUG_LEVEL),
		    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO});
		undefined ->
		    raise(#'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
					   completion_status=?COMPLETED_NO});
		Types ->
		    orber_iiop:request(Key, Func, Args, Types, 'false', infinity, 
				       Obj, Ctx)
	    end;
	{'external', Key} ->
	    orber_iiop:request(Key, Func, Args, TypesOrMod, 'false', infinity, 
			       Obj, Ctx)
    end.

cast_internal(Pid, Obj, Func, Args, Types, Check, PI, _Mod, Ctx)
  when is_pid(Pid) andalso node(Pid) == node() ->
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    catch gen_server:cast(Pid, {Obj, Ctx, Func, Args}),
    ok;
cast_internal(Pid, Obj, Func, Args, Types, Check, PI, Mod, Ctx) when is_pid(Pid) ->
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    case catch rpc:call(node(Pid), corba, cast_relay, [Pid, {Obj, Ctx, Func, Args}]) of
	{'EXCEPTION', E} ->
            typecheck_reply(Check, {'EXCEPTION', E}, Mod, Func),
	    raise(E);
	{badrpc, {'EXIT', _R}} ->
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 3), 
			       completion_status=?COMPLETED_MAYBE});
	{badrpc,nodedown} ->
	    orber:dbg("[~p] corba:cast_internal(~p, ~p, ~p);~nNode ~p down.",
		      [?LINE, Func, Args, Types, node(Pid)], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 2), 
			       completion_status=?COMPLETED_NO});
	Other ->
	    orber:dbg("[~p] corba:cast_internal(~p, ~p, ~p);~n"
		      "Communication with node: ~p failed with reason: ~p.", 
		      [?LINE, Func, Args, Types, node(Pid), Other], ?DEBUG_LEVEL),
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 5), 
			       completion_status=?COMPLETED_MAYBE})
    end;

%% This case handles if the reference is created as a Pseudo object.
%% Just call apply/3.
cast_internal({pseudo, Module}, Obj, Func, Args, Types, Check, PI, _Mod, Ctx) ->
    OldCtx = put(oe_server_in_context, Ctx),
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    State = binary_to_term(get_subobject_key(Obj)),
    catch apply(Module, Func, [Obj, State|Args]),
    put(oe_server_in_context, OldCtx),
    ok;
cast_internal(Registered, Obj, Func, Args, Types, Check, PI, _Mod, Ctx) ->
    invoke_pi_request(PI, Obj, Ctx, Func, Args),
    typecheck_request(Check, Args, Types, Func),
    case whereis(Registered) of
	undefined ->
	    raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
	P ->
	    gen_server:cast(P, {Obj, Ctx, Func, Args})
    end.
	
cast_relay(Pid, Data) ->
    case whereis(orber_objkeyserver) of
	undefined ->
	    raise(#'TRANSIENT'{minor=(?ORBER_VMCID bor 1), 
				     completion_status=?COMPLETED_NO});
	_ ->
	    gen_server:cast(Pid, Data)
    end.

%%-----------------------------------------------------------------
%% Corba:locate - this function is for the moment just used for tests
%%-----------------------------------------------------------------
locate(Obj) ->
    locate(Obj, infinity, []).

locate(Obj, Timeout) ->
    locate(Obj, Timeout, []).

locate(Obj, Timeout, Ctx) ->
    case iop_ior:get_key(Obj) of
	{'external', Key} ->
	    orber_iiop:locate(Key, Timeout, Obj, Ctx);
	_ ->
	    orber_objectkeys:check(iop_ior:get_objkey(Obj))
    end.

%%-----------------------------------------------------------------
%% Incomming request from iiop
%%-----------------------------------------------------------------
%% Operations which do not allow object invokation.
request_from_iiop(Obj, '_is_a', [Args], _, _, _) ->
    catch corba_object:is_a(Obj, Args);
%% First the OMG specified this operation to be '_not_existent' and then
%% changed it to '_non_existent' without suggesting that both must be supported.
%% See CORBA2.3.1 page 15-34, Minor revision 2.3.1: October 1999
request_from_iiop(Obj, '_not_existent', _, _, _, _) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(Obj, '_non_existent', _, _, _, _) ->
    catch corba_object:non_existent(Obj);
request_from_iiop(_, '_FT_HB', _, _, _, _) ->
    ok;

%% "Ordinary" operations.
request_from_iiop({Mod, _, _, _, _, _}, oe_get_interface, 
		  _, _, _, _ServiceCtx) when is_atom(Mod) ->
    case catch Mod:oe_get_interface() of
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:request_from_iiop(~p);~n"
		      "The call-back module does not exist or"
		      " incorrect IC-version used.~nReason: ~p", 
		      [?LINE, Mod, What], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 7),
				       completion_status=?COMPLETED_NO}};
	undefined ->
	    {'EXCEPTION', #'BAD_OPERATION'{minor = (?ORBER_VMCID bor 4),
					   completion_status='COMPLETED_NO'}};
	Interface ->
	    Interface
    end;
request_from_iiop({_Mod, pseudo, Module, _UserDef, _OrberDef, _Flags} = ObjRef, 
		  Func, Args, Types, ResponseExpected, _ServiceCtx) ->
    State = binary_to_term(get_subobject_key(ObjRef)),
    case ResponseExpected of
	true ->
	    case catch apply(Module, Func, [ObjRef, State|Args]) of
		{noreply, _} ->
		    ok;
		{noreply, _, _} ->
		    ok;
		{reply, Reply, _} ->
		    Reply;
		{reply, Reply, _, _} ->
		    Reply;
		{stop, _, Reply, _} ->
		    Reply;
		{stop, _, _} ->
		    ok;
		{'EXCEPTION', E} ->
		    {'EXCEPTION', E};
		{'EXIT', {undef, _}} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p);~n"
			      "The call-back module does not exist.", 
			      [?LINE, Func, Args, Types], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_NO}};
		{'EXIT', What} ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p);~n"
			      "Pseudo object exit(~p).~n"
			      "The call-back module probably contain an error.",
			      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
					       completion_status=?COMPLETED_MAYBE}};
		Unknown ->
		    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p);~n"
			      "Pseudo object failed(~p);~n"
			      "Confirm that the return value is correct"
			      " (e.g. {reply, Reply, State})", 
			      [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 6),
					       completion_status=?COMPLETED_MAYBE}}
	    end;
	false ->
	    catch apply(Module, Func, [ObjRef, State|Args]),
	    ok;
	true_oneway ->
	    catch apply(Module, Func, [ObjRef, State|Args]),
	    ok
    end;
% FOR PASSIVE REPLICATION! (Response IS expected --- one way semantics doesn't
% really mix with intentions to be consistent & fault tolerant.)
request_from_iiop({_Mod, passive, Module, _UserDef, _OrberDef, _Flags} = ObjRef,
		  Func, Args, Types, true, Ctx) ->
    GroupID = binary_to_term(get_subobject_key(ObjRef)),
    FTGroupVersionCtx = get_FTGroupVersionCtx(Ctx),
    Transaction =
	fun() ->
		ObjectGroup = read_object_group(GroupID),
		check_version_context(ObjectGroup,
				      FTGroupVersionCtx),
		call_primary_protected(ObjectGroup,
				       Module,
				       ObjRef,
				       Func,
				       Args,
				       GroupID,
				       get_FTRequestCtx(Ctx))
	end,
    case mnesia:transaction(Transaction) of
	{atomic, Reply} ->
	    Reply;
	{aborted, {too_old_reference, IOGR}} ->
	    {oe_location_forward_perm, IOGR};
	{aborted, {not_primary, _Primary, IOGR}} ->
	    case FTGroupVersionCtx of
		[] ->
		    {oe_location_forward_perm, IOGR};
		_ ->
		    {'EXCEPTION', #'TRANSIENT'{minor = 0,
					       completion_status = ?COMPLETED_NO}}
	    end;
	{aborted, {throw, {'EXCEPTION', E}}} ->
	    {'EXCEPTION', E};
	{aborted, {'EXIT', What}} ->
	    orber:dbg("[~p] corba:call_passive(~p, ~p, ~p);~n"
		      "Passive object exit(~p).", 
		      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'TRANSIENT'{minor = 0,
				       completion_status=?COMPLETED_MAYBE}};
	{aborted, Unknown} ->
	    orber:dbg("[~p] corba:call_passive(~p, ~p, ~p);~n"
		      "Passive object failed due to bad return value (~p).", 
		      [?LINE, Func, Args, Types, Unknown], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'TRANSIENT'{minor = 0,
				       completion_status=?COMPLETED_MAYBE}}
    end;
request_from_iiop({_Mod, _Type, Key, _UserDef, _OrberDef, _Flags} = ObjRef, 
		  Func, Args, Types, true, _ServiceCtx) ->
    case catch gen_server:call(convert_key_to_pid(Key), 
			       {ObjRef, [], Func, Args}, infinity) of
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p);~n"
		      "gen_server:call exit: ~p", 
		      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
				       completion_status=?COMPLETED_MAYBE}};
	Result ->
	    Result
    end;
request_from_iiop({_Mod, _Type, Key, _UserDef, _OrberDef, _Flags} = ObjRef, 
		  Func, Args, Types, _, _ServiceCtx) ->
    case catch gen_server:cast(convert_key_to_pid(Key), 
			       {ObjRef, [], Func, Args}) of
	{'EXIT', What} ->
	    orber:dbg("[~p] corba:request_from_iiop(~p, ~p, ~p);~n"
		      "gen_server:cast exit: ~p", 
		      [?LINE, Func, Args, Types, What], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'TRANSIENT'{minor=(?ORBER_VMCID bor 4),
				       completion_status=?COMPLETED_MAYBE}};
	Result ->
	    Result
    end.

%%------------------------------------------------------------
%% Internal stuff
%%------------------------------------------------------------

convert_key_to_pid(Key) when is_binary(Key) ->
    orber_objectkeys:get_pid(Key);
convert_key_to_pid(Name) when is_atom(Name) ->
    Name.

mk_objkey(Mod, Pid, RegName, Persistent) ->
    mk_objkey(Mod, Pid, RegName, Persistent, 0).

mk_objkey(Mod, Pid, [], _, Flags) when is_pid(Pid) ->
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, false) of
	ok ->
	    {Mod, 'key', Key, term_to_binary(undefined), 0, Flags};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p);~n"
		      "unable to store key(~p).", [?LINE, Mod, R], ?DEBUG_LEVEL),
	    raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO})
    end;
mk_objkey(Mod, Pid, {'global', RegName}, Persitent, Flags) when is_pid(Pid) ->
    Key = term_to_binary(RegName),
    case orber_objectkeys:register(Key, Pid, Persitent) of
	ok ->
	    {Mod, 'key', Key, term_to_binary(undefined), 0, Flags};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p, ~p);~n"
		      "unable to store key(~p).", 
		      [?LINE, Mod, RegName, R], ?DEBUG_LEVEL),
	    raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO})
    end;
mk_objkey(Mod, Pid, {'local', RegName}, Persistent, Flags) when is_pid(Pid) andalso is_atom(RegName) ->
    register(RegName, Pid),
    Key = make_objkey(),
    case orber_objectkeys:register(Key, Pid, Persistent) of
	ok ->
	    {Mod, 'registered', RegName, term_to_binary(undefined), 0, Flags};
	R ->
	    orber:dbg("[~p] corba:mk_objkey(~p, ~p);~n"
		      "unable to store key(~p).", 
		      [?LINE, Mod, RegName, R], ?DEBUG_LEVEL),
	    raise(#'INTERNAL'{minor=(?ORBER_VMCID bor 2), completion_status=?COMPLETED_NO})
    end.


mk_light_objkey(Mod, RegName) ->
    {Mod, 'registered', RegName, term_to_binary(undefined), 0, 0}.

mk_pseudo_objkey(Mod, Module, Flags) ->
    {Mod, 'pseudo', Module, term_to_binary(undefined), 0, Flags}.

mk_passive_objkey(Mod, Module, Flags) ->
    {Mod, 'passive', Module, term_to_binary(undefined), 0, Flags}.

make_objkey() ->
    term_to_binary({{erlang:system_time(), 
		     erlang:unique_integer()}, 
		    node()}).

objkey_to_string({_Mod, 'registered', 'orber_init', _UserDef, _OrberDef, _Flags}) ->
    "INIT";
objkey_to_string({Mod, Type, Key, UserDef, OrberDef, Flags}) ->
    orber:domain() ++ [ 7 | binary_to_list(term_to_binary({Mod, Type, Key, UserDef, OrberDef, Flags}))];
objkey_to_string(External_object_key) ->
    External_object_key.

string_to_objkey("INIT") ->
    {orber_initial_references, 'registered', 'orber_init', term_to_binary(undefined), 0, 0};
string_to_objkey(String) -> 
    case prefix(orber:domain(), String) of
	[7 | Rest] ->
	    binary_to_term(list_to_binary(Rest));
	_ ->
	    String
    end.
%% This function may only be used when we know it's a local reference (i.e. target
%% key in a request; IOR's passed as argument or reply doesn't qualify)!
string_to_objkey_local("INIT") ->
    {orber_initial_references, 'registered', 'orber_init', term_to_binary(undefined), 0, 0};
string_to_objkey_local(String) -> 
    case prefix(orber:domain(), String) of
	[7 | Rest] ->
	    binary_to_term(list_to_binary(Rest));
	_ ->
	    case resolve_initial_references(String) of
		?ORBER_NIL_OBJREF ->
		    orber:dbg("[~p] corba:string_to_objkey_local(~p);~n"
			      "Invalid ObjektKey.", [?LINE, String], ?DEBUG_LEVEL),
		    ?ORBER_NIL_OBJREF;
		Object ->
		    {location_forward, Object}
	    end
    end.

prefix([], L2) ->
    L2;
prefix([E |L1], [E | L2]) ->
    prefix(L1, L2);
prefix(_, _) ->
    false.


evaluate_options([], Options) ->
    GlobalFlags = orber:get_flags(),
    Options2 = check_flag(Options, ?ORB_TYPECHECK, 
			  ?ORB_ENV_LOCAL_TYPECHECKING, GlobalFlags),
    Options3 = check_flag(Options2, ?ORB_USE_PI, ?ORB_ENV_USE_PI, GlobalFlags),
    check_flag(Options3, ?ORB_SURVIVE_EXIT, ?ORB_ENV_SURVIVE_EXIT, GlobalFlags);
%% Pseudo or not.
evaluate_options([{pseudo, false}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{pseudo, true}|Rest], #options{passive = false} = Options) ->
    evaluate_options(Rest, Options#options{pseudo = true});
%% FT stuff
evaluate_options([{passive, true}|Rest], #options{pseudo = false} = Options) ->
    evaluate_options(Rest, Options#options{passive = true});
evaluate_options([{group_id, ID}|Rest], Options) when is_integer(ID) ->
    evaluate_options(Rest, Options#options{group_id = ID});
%% Options accepted by gen_server (e.g. dbg).
evaluate_options([{create_options, COpt}|Rest], Options) when is_list(COpt) ->
    evaluate_options(Rest, Options#options{create_options = COpt});
%% When starting object as supervisor child.
evaluate_options([{sup_child, false}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{sup_child, true}|Rest], Options) ->
    evaluate_options(Rest, Options#options{sup_child = true});
%% Persistent object-key
evaluate_options([{persistent, false}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{persistent, true}|Rest], Options) ->
    evaluate_options(Rest, Options#options{persistent = true});
evaluate_options([{regname, []}|Rest], Options) ->
    evaluate_options(Rest, Options);
evaluate_options([{regname, Name}|Rest], Options) ->
    evaluate_options(Rest, Options#options{regname = Name});
evaluate_options([{survive_exit, false}|Rest], 
		 #options{object_flags_set = FlagsSet} = Options) ->
    %% This option overrides a global setting.
    evaluate_options(Rest, Options#options{object_flags_set = 
					   (?ORB_SURVIVE_EXIT bor FlagsSet)});
evaluate_options([{survive_exit, true}|Rest], 
		 #options{object_flags = Flags,
			  object_flags_set = FlagsSet} = Options) ->
    evaluate_options(Rest, Options#options{object_flags = 
					   (?ORB_SURVIVE_EXIT bor Flags),
					   object_flags_set = 
					   (?ORB_SURVIVE_EXIT bor FlagsSet)});
evaluate_options([{local_typecheck, false}|Rest], 
		 #options{object_flags_set = FlagsSet} = Options) ->
    %% This option overrides a global setting.
    evaluate_options(Rest, Options#options{object_flags_set = 
					   (?ORB_TYPECHECK bor FlagsSet)});
evaluate_options([{local_typecheck, true}|Rest], 
		 #options{object_flags = Flags,
			  object_flags_set = FlagsSet} = Options) ->
    evaluate_options(Rest, Options#options{object_flags = (?ORB_TYPECHECK bor Flags),
					   object_flags_set = 
					   (?ORB_TYPECHECK bor FlagsSet)});
evaluate_options([{local_interceptors, false}|Rest], 
		 #options{object_flags_set = FlagsSet} = Options) ->
    %% This option overrides a global setting.
    evaluate_options(Rest, Options#options{object_flags_set = 
					   (?ORB_USE_PI bor FlagsSet)});
evaluate_options([{local_interceptors, true}|Rest], 
		 #options{object_flags = Flags,
			  object_flags_set = FlagsSet} = Options) ->
    evaluate_options(Rest, Options#options{object_flags = (?ORB_USE_PI bor Flags),
					   object_flags_set = 
					   (?ORB_USE_PI bor FlagsSet)});
%% Temporary option.
evaluate_options([{no_security, true}|Rest], 
		 #options{object_flags = Flags} = Options) ->
    %% We do not allow this option to be set globally.
    evaluate_options(Rest, Options#options{object_flags = (?ORB_NO_SECURITY bor Flags)});
evaluate_options([{no_security, false}|Rest], Options) ->
    %% We do not allow this option to be set globally.
    evaluate_options(Rest, Options);
evaluate_options([{Key, Value}|_], _) ->
    orber:dbg("[~p] corba:evaluate_options(~p, ~p);~n"
	      "Option not recognized, illegal value or combination.~n"
	      "Allowed settings:~n"
	      "survive_exit.......: boolean()~n"
	      "sup_child..........: boolean()~n"
	      "persistent.........: boolean()~n"
	      "pseudo.............: boolean()~n"
	      "local_typecheck....: boolean()~n"
	      "local_interceptors.: boolean()~n"
	      "regname............: {local, atom()} | {global, term()}", 
	      [?LINE, Key, Value], ?DEBUG_LEVEL),
    raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

check_flag(#options{object_flags = Flags, 
		    object_flags_set = FlagsSet} = Options, Flag, 
	   FlagConstant, GlobalFlags) ->
    %% Option activated/deactived by a supplied option.
    case ?ORB_FLAG_TEST(FlagsSet, Flag) of
	true ->
	    Options;
	false ->
	    %% Not the above. Globally defined?
	    case ?ORB_FLAG_TEST(GlobalFlags, FlagConstant) of
		true ->
		    Options#options{object_flags = (Flag bor Flags)};
		false ->
		    Options
	    end
    end.

%%%%%%%%%%%%%%%%% FOR PASSIVE REPLICATION! 
% Note should be called inside transaction. Does not catch exceptions.
% let's not allow corba:reply from transaction... (no {noreply, ...} messages)
% should the object be able to stop itself by returning {stop, ...}?
% how about corba:dispose then? Deleting table representing object group and
% corresponding entry in ft_replication_manager -table might just do the job?
% No {stop, ...} messages for now
% Exceptions falls through. They are expected to be caught by transaction in a
% form of {aborted, {throw, {'EXCEPTION', ...}}}
call_passive(Module, Obj, Func, Args, GroupID) ->
    [Record] = mnesia:read(ft_replicated_object, GroupID, sticky_write),
    State = Record#ft_replicated_object.state,

    case apply(Module, Func, [Obj, State|Args]) of
	{reply, Reply, NewState} ->
	    {Reply, NewState};
	{reply, Reply, NewState, _} ->
	    {Reply, NewState}
    end,
    mnesia:write(ft_replicated_object,
		 #ft_replicated_object{group_id = GroupID, state = NewState},
		 sticky_write),
    Reply.



% FTRequestCtx protected object call
% One should protect agains aged reply. If expirations_time is reached and
% request is retransmitted, one might return BAD_CONTEXT -exception!
call_RQprotected(Module, Obj, Func, Args, GroupID, RQCtx) ->
    case mnesia:read(ft_reply_retention, RQCtx, sticky_write) of
	% fresh request
	[] ->
	    Reply = call_passive(Module, Obj, Func, Args, GroupID),
	    mnesia:write(ft_reply_retention,
			 #ft_reply_retention{retention_id= RQCtx,reply= Reply},
			 sticky_write),
	    Reply;
	% retransmitted request
	[#ft_reply_retention{reply = Reply}] ->
	    Reply
    end.



% call_primary_protected. Protects agains calling non-primary node.
% normal case, without FTRequest Service Context
call_primary_protected(#ft_replication_manager{primary = Primary}, 
		       Module,
		       Obj, 
		       Func,
		       Args,
		       GroupID,
		       []) when Primary == node() ->
    call_passive(Module, Obj, Func, Args, GroupID);
% normal case, with FTRequest Service Context
call_primary_protected(#ft_replication_manager{primary = Primary},
		       Module,
		       Obj,
		       Func,
		       Args,
		       GroupID,
		       RetentionID) when Primary == node() ->
    call_RQprotected(Module, Obj, Func, Args, GroupID, RetentionID);
% case where primary resides in another node
call_primary_protected(#ft_replication_manager{primary = Primary,
					       iogr = IOGR},
		      _Module, _Obj, _Func, _Args, _GroupID, _) ->
    mnesia:abort({not_primary, Primary, IOGR}).



% no context
check_version_context(_, []) ->
    ok;
% client's IOGR is current.
check_version_context(#ft_replication_manager{ref_version = CurrentVer},
		      GroupVer) when CurrentVer == GroupVer ->
    ok;
% client's IOGR is old.
check_version_context(#ft_replication_manager{ref_version = CurrentVer,
					      iogr = IOGR},
		      GroupVer) when CurrentVer > GroupVer ->
    mnesia:abort({too_old_reference, IOGR});
% client's IOGR is too new!
check_version_context(#ft_replication_manager{ref_version = CurrentVer},
		      GroupVer) when CurrentVer < GroupVer ->
    raise(#'INV_OBJREF'{completion_status = ?COMPLETED_NO}).



read_object_group(GroupID) ->
    case mnesia:read({ft_replication_manager, GroupID}) of
	[] ->
	    raise(#'OBJECT_NOT_EXIST'{completion_status = ?COMPLETED_NO});
	[ObjectGroup] ->
	    ObjectGroup
    end.



mk_FTRequestCtx(Expiration_time) ->
    #'FT_FTRequestServiceContext'{ 
		 client_id = atom_to_list(node()), 
		 retention_id = orber_request_number:get(), 
		 expiration_time = Expiration_time}.



get_FTRequestCtx([#'FT_FTRequestServiceContext'
		  {client_id = Client_ID, retention_id = Retention_ID, 
		   expiration_time = Expiration_time}|_Ctxs]) ->
    {Client_ID, Retention_ID, Expiration_time};
get_FTRequestCtx([]) ->
    [];
get_FTRequestCtx([_Ctx|Ctxs]) ->
    get_FTRequestCtx(Ctxs).



get_FTGroupVersionCtx([#'FT_FTGroupVersionServiceContext'
		       {object_group_ref_version = Version}|_Ctxs]) ->
    Version;
get_FTGroupVersionCtx([]) ->
    [];
get_FTGroupVersionCtx([_Ctx|Ctxs]) ->
    get_FTGroupVersionCtx(Ctxs).

