%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
%% Description:
%% Test suite for corba/boa/object/orber API functions
%%
%%-----------------------------------------------------------------
-module(corba_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").


-define(default_timeout, test_server:minutes(5)).

-define(match(ExpectedRes,Expr),
	fun() ->
	       AcTuAlReS = (catch (Expr)),
	       case AcTuAlReS of
		   ExpectedRes ->
		       io:format("------ CORRECT RESULT ------~n~p~n",
				 [AcTuAlReS]),
		       AcTuAlReS;
		   _ ->
		       io:format("###### ERROR ERROR ######~nRESULT:  ~p~n",
				 [AcTuAlReS]),
		       exit(AcTuAlReS)
	       end
       end()).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([pseudo_calls/2, pseudo_casts/2]).
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [exception_info_api, corba_api, object_api, orber_api,
     orber_objectkeys_api, orber_pseudo_objects,
     callback_ok_api, callback_arity_api,
     callback_module_api, callback_function_api,
     callback_precond_api, callback_postcond_api,
     callback_exit_api, callback_badarith_api,
     callback_case_clause_api, callback_function_clause_api]. 

%% boa_api, request, locate_request, locate_reply].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    corba:orb_init([{orber_debug_level, 10}, {giop_version, {1,2}},
		    {iiop_port, 0}]),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    orber:install([node()]),
    application:start(mnesia),
    application:start(orber),
    if
	is_list(Config) ->
	    Config;
	true ->
	    exit("Config not a list")
    end.

end_per_suite(Config) ->
    application:stop(orber),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    Config.

%%-----------------------------------------------------------------
%%  API tests for pseudo interface CORBA
%%-----------------------------------------------------------------
corba_api(_) ->
    NIL = corba:create_nil_objref(),
    ok = corba:dispose(NIL),
    NS = corba:resolve_initial_references("NameService"),
    List = corba:list_initial_services(),
    ["NameService"] = List, 
    NSstring = corba:object_to_string(NS),
    NS1 = corba:string_to_object(NSstring),
    NSstring = corba:object_to_string(NS1),    
    true = corba:add_initial_service("MyData", NS),
    NS = corba:resolve_initial_references("MyData"),
    [_,_] = corba:list_initial_services(),
    false = corba:remove_initial_service("Wrong"),
    NIL = corba:resolve_initial_references("Wrong"),
    NS = corba:string_to_object("corbaloc:rir:/MyData"),
    true = corba:remove_initial_service("MyData"),
    ["NameService"] = corba:list_initial_services(),

    %% This is a collection of different stringified IOR:s (correct & incorrect)
    %% which we use to test IOR encode/decode.
    IOR1 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e3000030303000000040000000000000100000102010000000a3132372e302e302e31009d610000002dabacab3131303432343836383731005f526f6f74504f4100414c4c5f504f410000cafebabe3e2316570000000003030300000002000000210000007800010202000000010040020200000022000000080003030300000000004000400000000806066781020101010000001b0401000806066781020101010000000b40616469726f6e2e636f6d010400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f000000010000002c00030303000100010000000400010020000101090001010005010001000101090000000200010100050100010000000000000184000102010000000a3132372e302e302e310000000000002dabacab3131303432343836383731005f526f6f74504f4100414c4c5f504f410000cafebabe3e231657000000000303030000000300000021000000ec000102020000000200060202000000240000001c0001006600060202000000010000000a3132372e302e302e31009d600000000000000000000000000400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f00460202000000240000001c0001006600060202000000010000000a3132372e302e302e31009d62004000400000000806066781020101010000001b0401000806066781020101010000000b40616469726f6e2e636f6d010400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f00000014000000080001006600069d5e000000010000002c000303030001000100000004000100200001010900010100050100010001010900000002000101000501000100000000000000dc000102010000000a3132372e302e302e31009d5f0000002dabacab3131303432343836383731005f526f6f74504f4100414c4c5f504f410000cafebabe3e23165700000000030303000000020000002100000054000102020000000100000202000000220000000800030303000000000000000000000000000000000400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f000000010000002c00030303000100010000000400010020000101090001010005010001000101090000000200010100050100010000000000000080000102010000000a3132372e302e302e31009d5d0000002dabacab3131303432343836383731005f526f6f74504f4100414c4c5f504f410000cafebabe3e2316570000000003030300000001000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR2 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e30000303030000000100000000000000e0000102010000000a3132372e302e302e31009d5f00000034abacab3131303432343836383731005f526f6f74504f410049494f505f43534976325f504f410000cafebabe3e23165700000000000000020000002100000054000102020000000100000202000000220000000800030303000000000000000000000000000000000400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR3 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e3000030303000000010000000000000108000102010000000a3132372e302e302e31009d6100000037abacab3131303432343836383731005f526f6f74504f410049494f505f43534976325f55505f504f410000cafebabe3e231657000000000100000002000000210000007800010202000000010040020200000022000000080003030300000000004000400000000806066781020101010000001b0401000806066781020101010000000b40616469726f6e2e636f6d010400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR4 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e3000030303000000010000000000000080000102010000000a3132372e302e302e31009d5d0000002eabacab3131303432343836383731005f526f6f74504f410049494f505f504f410000cafebabe3e23165700000000020200000001000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR5 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e30000303030000000100000000000000fc000102010000000a3132372e302e302e3100000000000033abacab3131303432343836383731005f526f6f74504f4100544c535f43534976325f504f410000cafebabe3e231657000000000100000002000000210000007000010202000000010006020200000024000000220001006600060202000000010000000f3132382e3233302e3230382e353500019d6000000000020200000000000000000400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR6 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e3000030303000000010000000000000124000102010000000a3132372e302e302e3100000000000036abacab3131303432343836383731005f526f6f74504f4100544c535f43534976325f55505f504f410000cafebabe3e23165700000000020200000002000000210000009400010202000000010046020200000024000000220001006600060202000000010000000f3132382e3233302e3230382e353500019d620040004002020000000806066781020101010000001b0401000806066781020101010000000b40616469726f6e2e636f6d010400000000000000000000020000000806066781020101010000000b06092a864886f712010202010000000f000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR7 = ?match({'IOP_IOR',_,_}, corba:string_to_object("IOR:000303030000000d49444c3a746573743a312e3000030303000000010000000000000090000102010000000a3132372e302e302e310000000000002dabacab3131303432343836383731005f526f6f74504f4100544c535f504f410000cafebabe3e231657000000000303030000000200000014000000080001006600069d5e000000010000002c0003030300010001000000040001002000010109000101000501000100010109000000020001010005010001")),
    IOR1 = corba:string_to_object(corba:object_to_string(IOR1)),
    IOR2 = corba:string_to_object(corba:object_to_string(IOR2)),
    IOR3 = corba:string_to_object(corba:object_to_string(IOR3)),
    IOR4 = corba:string_to_object(corba:object_to_string(IOR4)),
    IOR5 = corba:string_to_object(corba:object_to_string(IOR5)),
    IOR6 = corba:string_to_object(corba:object_to_string(IOR6)),
    IOR7 = corba:string_to_object(corba:object_to_string(IOR7)),
    ?match(ok, corba:print_object(IOR1)),
    ?match(ok, corba:print_object(IOR2)),
    ?match(ok, corba:print_object(IOR3)),
    ?match(ok, corba:print_object(IOR4)),
    ?match(ok, corba:print_object(IOR5)),
    ?match(ok, corba:print_object(IOR6)),
    ?match(ok, corba:print_object(IOR7)),
    ?match(ok, corba:print_object("IOR:000303030000000d49444c3a746573743a312e300003030300000002000000000000003000010001000000136d792e686f73742e65726c616e672e6f72670001801a02020000000c424f410a00000a0000070a010000000100000024000303030000000100000001000000140003030300010001000000000001010900000000")),
    [IP] = ?match([_], orber:host()),
    ?match(#'IOP_IOR'{profiles=[#'IOP_TaggedProfile'
				{tag=?TAG_INTERNET_IOP, 
				 profile_data=#'IIOP_ProfileBody_1_1'
				 {host = IP}}]},
	   corba:string_to_object(corba:object_to_string(NS))),
    ?match(#'IOP_IOR'{profiles=[#'IOP_TaggedProfile'
				{tag=?TAG_INTERNET_IOP, 
				 profile_data=#'IIOP_ProfileBody_1_1'
				 {host = "127.0.0.1"}}]},
	   corba:string_to_object(corba:object_to_string(NS, ["127.0.0.1"]))),
    ?match(#'IOP_IOR'{profiles=[#'IOP_TaggedProfile'
				{tag=?TAG_INTERNET_IOP, 
				 profile_data=#'IIOP_ProfileBody_1_1'
				 {host = "127.0.0.1", port = 5468}}]},
	   corba:string_to_object(corba:object_to_string(NS, ["127.0.0.1"],
							 5468))),
    ok.

%%-----------------------------------------------------------------
%% API tests for interface BOA
%%-----------------------------------------------------------------
boa_api(_) ->
    ok.

%%-----------------------------------------------------------------
%% API tests for interface OBJECT
%%-----------------------------------------------------------------
object_api(_) ->
    oe_orber_test_server:oe_register(),
    EC = orber_test_server:oe_create(), 
    NS = corba:resolve_initial_references("NameService"),
    %% testing corba_object:is_a(Obj, IFRID) locally.
    orber_test_lib:corba_object_tests(EC, NS),

    ?match(false, corba_object:non_existent(NS)),

    corba:dispose(EC),
    oe_orber_test_server:oe_unregister(),
    ok.

%%-----------------------------------------------------------------
%% API tests for orbers main module
%%-----------------------------------------------------------------
orber_api(_) ->
    ok = orber:uninstall(),
    orber:install([node()]),
    application:start(orber),
    NodeList = orber:orber_nodes(),
    NL = node(),
    [NL] = NodeList,
    ok.

%%-----------------------------------------------------------------
%% API tests for exception mapping
%%-----------------------------------------------------------------
exception_info_api(_) ->
    {ok, S1} = orber:exception_info({'EXCEPTION',{'MARSHAL',[],1163001858,'COMPLETED_NO'}}),
    {ok, S2} = orber:exception_info({'EXCEPTION',{'MARSHAL',[],1330446337,'COMPLETED_NO'}}),
    {ok, S3} = orber:exception_info({'EXCEPTION',{'MARSHAL',[],1398079490,'COMPLETED_NO'}}),
    {ok, S4} = orber:exception_info({'EXCEPTION',{'MARSHAL',[],1347813377,'COMPLETED_NO'}}),
    {ok, S5} = orber:exception_info({'EXCEPTION', {'CosNaming_NamingContext_InvalidName',"IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0"}}),
    error_logger:info_msg("~s", [S1]),
    error_logger:info_msg("~s", [S2]),
    error_logger:info_msg("~s", [S3]),
    error_logger:info_msg("~s", [S4]),
    error_logger:info_msg("~s", [S5]),
    ok.

%%-----------------------------------------------------------------
%% API tests for orbers pseudo objects.
%%-----------------------------------------------------------------
orber_pseudo_objects(_) ->
    oe_orber_test_server:oe_register(),
    Obj1=(catch orber_test_server:oe_create(state,[{pseudo,true},
						   {local_typecheck, true}])),
    ?match({_,pseudo,orber_test_server_impl, _,_, _}, Obj1),
    Obj2=(catch orber_test_server:oe_create([],[{pseudo, truce}])),
    ?match({'EXCEPTION',{'BAD_PARAM',[],_,'COMPLETED_NO'}}, Obj2),
    spawn(?MODULE, pseudo_calls, [20, Obj1]),
    ?match({ok, 10000}, orber_test_server:pseudo_call_delay(Obj1, 10000)),
    spawn(?MODULE, pseudo_casts, [20, Obj1]),
    ?match(ok, orber_test_server:pseudo_cast_delay(Obj1, 10000)),

    ?match('object_here', corba:locate(Obj1)),

    NS = corba:resolve_initial_references("NameService"),

    orber_test_lib:corba_object_tests(Obj1, NS),

    ?match("IDL:omg.org/orber_test/server:1.0",orber_test_server:typeID()),

    %% Test if exceptions are handled properly.
    ?match({'EXCEPTION',{'BAD_QOS',_,_,_}}, 
	   orber_test_server:pseudo_call_raise_exc(Obj1, 1)),
    ?match({'EXCEPTION',{'BAD_QOS',_,_,_}}, 
	   orber_test_server:pseudo_call_raise_exc(Obj1, 2)),

    %% Test if exit is handled properly.
    ?match({'EXCEPTION',{'TRANSIENT',_,_,_}},
	   orber_test_server:stop_brutal(Obj1)),

    orber_test_lib:test_coding(Obj1, true),

    %% possible to use subobject key?
    ?match(state, binary_to_term(corba:get_subobject_key(Obj1))),
    
    ?match({'EXCEPTION',{'INV_OBJREF',[],_,'COMPLETED_NO'}}, 
		 corba:get_pid(Obj1)),
    ?match(false, corba_object:non_existent(Obj1)),

    ?match(ok, corba:dispose(Obj1)),

    ?match(false, corba_object:non_existent(Obj1)),

    %% Try if it's possible to stringify and recover the object reference.
    IOR_string = (catch corba:object_to_string(Obj1)),
    Obj3 =(catch corba:string_to_object(IOR_string)),
    ?match(IOR_string, corba:object_to_string(Obj3)),

    Obj4=(catch orber_test_server:oe_create(undefined,[{pseudo,true}])),
    ?match(ok, corba:dispose(Obj4)),
    oe_orber_test_server:oe_unregister(),
    ok.

%%-----------------------------------------------------------------
%% API tests for orbers objectkeys server.
%%-----------------------------------------------------------------
orber_objectkeys_api(_) ->
    Obj0=(catch orber_test_server:oe_create([], [{sup_child, true}])),
    Obj1=(catch orber_test_server:oe_create([], [{persistent, true}, 
						 {regname, {local,obj1}}])),
    Obj2=(catch orber_test_server:oe_create([], [{persistent, true},
						 {regname, {global,{obj2, 12345}}}])),

    %% Obj0 is supposed to be a child started by a supervisor (r6) which 
    %% handles not only {ok, Pid} but also {ok,Pid, Returnvalue}. In our
    %% case the Returnvalue is an ObjectRef.
    ?match({ok,_,{_,key,_, _,_, _}}, Obj0),
    {ok,_,Obj0Ref} = Obj0,
    corba:dispose(Obj0Ref),

    %% Only 'global' servers are at the moment allowed to be persistent.
    ?match({'EXCEPTION',{'BAD_PARAM',[],_,'COMPLETED_NO'}}, Obj1),

    %% We created a persistent object successfully.
    ?match({_,key,_,_,_, _}, Obj2),

    %% Get key and Pid
    {_,_,Key,_,_, _} = Obj2,
    PID=(catch orber_objectkeys:get_pid(Key)),

    %% Use the two different ways to look up if the server is persistent.
    ?match(true, orber_objectkeys:is_persistent(Key)),
    ?match(true, orber_objectkeys:is_persistent(PID)),

    %% Create servers using every possible way.
    O1=(catch orber_test_server:oe_create()),
    O2=(catch orber_test_server:oe_create_link()),
    O3=(catch orber_test_server:oe_create([])),
    O4=(catch orber_test_server:oe_create_link([])),
    %% NOTE!!! Next four lines requires that we still support RegName instead of
    %% only OptionList as the second argument to oe_create*/2. Remove these when that
    %% is no longer the case.
    O5=(catch orber_test_server:oe_create([], {'local', o5})),
    O6=(catch orber_test_server:oe_create([], {'global', {o6, obj}})),
    O7=(catch orber_test_server:oe_create_link([], {'local', o7})),
    O8=(catch orber_test_server:oe_create_link([], {'global', {o8, obj}})),

    %% Test if all the object references are correct. 
    ?match({_,key,_,_,_, _}, O1),
    ?match({_,key,_,_,_, _}, O2),
    ?match({_,key,_,_,_, _}, O3),
    ?match({_,key,_,_,_, _}, O4),
    ?match({_, registered, o5, _,_, _}, O5), 
    ?match({_,key,_,_,_, _}, O6),
    ?match({_, registered, o7, _,_, _}, O7), 
    ?match({_,key,_,_,_, _}, O8),

    %% Test if persistent.
    {_,_,Key1,_,_, _} = O1,
    PID1=(catch orber_objectkeys:get_pid(Key1)),
    ?match(false, orber_objectkeys:is_persistent(Key1)),
    ?match(false, orber_objectkeys:is_persistent(PID1)),

    %% all the servers are alive(?!).
    ?match(false, corba_object:non_existent(O1)),
    ?match(false, corba_object:non_existent(O2)),
    ?match(false, corba_object:non_existent(O3)),
    ?match(false, corba_object:non_existent(O4)),
    ?match(false, corba_object:non_existent(O5)),
    ?match(false, corba_object:non_existent(O6)),
    ?match(false, corba_object:non_existent(O7)),
    ?match(false, corba_object:non_existent(O8)),
    ?match(false, corba_object:non_existent(Obj2)),

    %% Does locate work?
    ?match('object_here', corba:locate(O1)),
    ?match('object_here', corba:locate(O2)),
    ?match('object_here', corba:locate(O3)),
    ?match('object_here', corba:locate(O4)),
    ?match('object_here', corba:locate(O5)),
    ?match('object_here', corba:locate(O6)),
    ?match('object_here', corba:locate(O7)),
    ?match('object_here', corba:locate(O8)),
    ?match('object_here', corba:locate(Obj2)),
    
    %% Terminate all servers with reason 'normal'.
    catch corba:dispose(O1),
    catch corba:dispose(O2),
    catch corba:dispose(O3),
    catch corba:dispose(O4),
    catch corba:dispose(O5),
    catch corba:dispose(O6),
    catch corba:dispose(O7),
    catch corba:dispose(O8),
    catch corba:dispose(Obj2),


    %% To make sure that orber_objectkeys-server is able to
    %% clean up we wait.
    timer:sleep(2000),

    %% all the servers are dead(?!). If one of these test-cases
    %% fails the only error can be that we didn't sleep long enough, i.e.,
    %% try a longer timeout. If still fails something is wrong.
    ?match(true, corba_object:non_existent(O1)),
    ?match(true, corba_object:non_existent(O2)),
    ?match(true, corba_object:non_existent(O3)),
    ?match(true, corba_object:non_existent(O4)),
    ?match(true, corba_object:non_existent(O5)),
    ?match(true, corba_object:non_existent(O6)),
    ?match(true, corba_object:non_existent(O7)),
    ?match(true, corba_object:non_existent(O8)),
    ?match(true, corba_object:non_existent(Obj2)),

    %% Create a new persistent server.
    Obj3=(catch orber_test_server:oe_create([], 
					    [{persistent, true},
					     {regname, {global,{obj2, 12345}}}])),

    %% OK?!
    ?match({_,key,_,_,_, _}, Obj3),

    %% Try to create a server with the same name (naturally it fails).
    ?match({'EXCEPTION',{'INTERNAL',[],_,'COMPLETED_NO'}},
		 orber_test_server:oe_create([], 
					     [{persistent, true},
					      {regname, {global,{obj2, 12345}}}])),
    %% Try to remove all 'dead' servers. No server should be removed. 
    orber_objectkeys:gc(0),

    %% Kill object brutal, i.e., not with reason 'normal' or 'shutdown'.
    P3 = corba:get_pid(Obj3),
    exit(P3, kill),

    {_,_,Key3,_,_, _} = Obj3,

    %% Give time to clean up.
    timer:sleep(2000),
    ?match({'EXCEPTION',{'TRANSIENT',[],_,'COMPLETED_NO'}},
		 gen_server:call(orber_objkeyserver,
				 {get_pid, Key3}, 
				 infinity)),

    ?match(false,corba_object:non_existent(Obj3)),

    %% Run gc wit a "huge" time-limit. Will not erase the dead object.
    orber_objectkeys:gc(10000),
    ?match(false,corba_object:non_existent(Obj3)),

    %% Run gc with minimum time-limit. Will erase the dead object.
    orber_objectkeys:gc(0),
    ?match(true,corba_object:non_existent(Obj3)),

    %% Create a new persistent server.
    Obj4=(catch orber_test_server:oe_create([], 
					    [{persistent, true},
					     {regname, {global,{obj2, 12345}}}])),

    %% OK?!
    ?match({_,key,_,_,_, _}, Obj4),
    %% Kill object brutal, i.e., not with reason 'normal' or 'shutdown'.
    P4 = corba:get_pid(Obj4),
    exit(P4, kill),

    %% Give time to clean up.
    timer:sleep(2000),
%    ?match({'EXCEPTION',{'COMM_FAILURE',[],0,'COMPLETED_NO'}}, 
    ?match({error, _},
		 corba:get_pid(Obj4)),

    ?match(false,corba_object:non_existent(Obj4)),

    %% Restart the object.
    Obj5=(catch orber_test_server:oe_create([], 
					    [{persistent, true},
					     {regname, {global,{obj2, 12345}}}])),
    %% OK?!
    ?match({_,key,_,_,_, _}, Obj5),

    %% Run gc with minimum time-limit.
    orber_objectkeys:gc(0),
    ?match(false,corba_object:non_existent(Obj5)),
    corba:dispose(Obj5),
    ok.


%%-----------------------------------------------------------------
%% API tests for callback functions
%%-----------------------------------------------------------------
-define(DO_EXIT_FLAG, 0).
-define(NO_EXIT_FLAG, 16#10).

-define(DO_EXIT, {is, 0}).
-define(NO_EXIT, {is, 16#10}).



%% Successful callback API tests
callback_ok_api(_) ->
    %% Init
    ?match({ok, {?DO_EXIT, state}}, corba:handle_init(?MODULE, {?DO_EXIT_FLAG, state})),
    %% Terminate
    ?match(ok, corba:handle_terminate(?MODULE, "reason", {?DO_EXIT, state})),
    %% Handle_call
    ?match({reply,ok,{?DO_EXIT,state}}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, state}, [], false, false)),
    %% Handle_cast
    ?match({noreply, {?DO_EXIT,state}}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, state}, [], false)),
    %% Handle_call precond/postcond
    ?match({reply, ok, {?DO_EXIT, state}}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, state}, [], false, false, {?MODULE, precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_cast precond/postcond
    ?match({noreply, {?DO_EXIT, state}}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, state}, [], false, {?MODULE, precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_info
    ?match({noreply, {?DO_EXIT, state}}, 
		 corba:handle_info(?MODULE, "info", {?DO_EXIT, state})),
    ok.

%% Callback arity API tests
callback_arity_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', {undef,_}}, 
		 corba:handle_call(?MODULE, foo, [to, many, arguments],
				   {?DO_EXIT, state}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _},
		 corba:handle_call(?MODULE, foo, [to, many, arguments],
				   {?NO_EXIT, state}, [], false, false)),
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, arity}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _},
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, arity}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', {undef,_}}, 
		 corba:handle_cast(?MODULE, foo_1w, [to, many, arguments],
				   {?DO_EXIT, state}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}},
		 corba:handle_cast(?MODULE, foo_1w, [to, many, arguments],
				   {?NO_EXIT, state}, [], false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, arity}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, arity}},
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, arity}, [], false)),
    %% Handle_info - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_info(?MODULE, "info", {?DO_EXIT, arity})),
    
    %% Handle_info - stay-alive == true
    ?match({noreply, {?NO_EXIT, arity}}, 
		 corba:handle_info(?MODULE, "info", {?NO_EXIT, arity})),
    ok.

%% Module callback API tests
callback_module_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', {undef,_}}, 
		 corba:handle_call(wrong_mod, foo, [],
				   {?DO_EXIT, state}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _}, 
		 corba:handle_call(wrong_mod, foo, [],
				   {?NO_EXIT, state}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', {undef,_}}, 
		 corba:handle_cast(wrong_mod, foo_1w, [],
				   {?DO_EXIT, state}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}},
		 corba:handle_cast(wrong_mod, foo_1w, [],
				   {?NO_EXIT, state}, [], false)),
    %% Handle_info - stay-alive == false.
    ?match({'EXIT', _}, 
		 corba:handle_info(wrong_mod, "info", {?DO_EXIT, state})),
    
    %% Handle_info - stay-alive == true.
    ?match({noreply, {?NO_EXIT, state}}, 
		 corba:handle_info(wrong_mod, "info", {?NO_EXIT, state})),
    ok.

%% Function callback API tests
callback_function_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', {undef,_}}, 
		 corba:handle_call(?MODULE, bad_function, [],
				   {?DO_EXIT, state}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _},
		 corba:handle_call(?MODULE, bad_function, [],
				   {?NO_EXIT, state}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', {undef,_}}, 
		 corba:handle_cast(?MODULE, bad_function, [],
				   {?DO_EXIT, state}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}},
		 corba:handle_cast(?MODULE, bad_function, [],
				   {?NO_EXIT, state}, [], false)),
    %% Handle_info - stay-alive == false. Note, we cannot use ?MODULE here.
    ?match({'EXIT', _}, 
		 corba:handle_info(corba, "info", {?DO_EXIT, state})),
    
    %% Handle_info - stay-alive == true. Note, we cannot use ?MODULE here.
    ?match({noreply, {?NO_EXIT, state}}, 
		 corba:handle_info(corba, "info", {?NO_EXIT, state})),
    ok.

%% Precond callback API tests
callback_precond_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, state}, [], false, false, {wrong_mod, precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, state}, [], false, false, {?MODULE, bad_precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}},_}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, state}, [], false, false, {wrong_mod, precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}},_}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, state}, [], false, false, {?MODULE, bad_precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, state}, [], false, {wrong_mod, precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, state}, [], false, {?MODULE, bad_precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, state}, [], false, {wrong_mod, precond},
				   {?MODULE, postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, state}, [], false, {?MODULE, bad_precond},
				  {?MODULE, postcond}, ?MODULE)),
    ok.


%% Postcond callback API tests
callback_postcond_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, state}, [], false, false, {?MODULE, precond},
				   {wrong_mod, postcond}, ?MODULE)),
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, state}, [], false, false, {?MODULE, precond},
				  {?MODULE, bad_postcond}, ?MODULE)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}},_}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, state}, [], false, false, {?MODULE, precond},
				   {wrong_mod, postcond}, ?MODULE)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}},_}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, state}, [], false, false, {?MODULE, precond},
				  {?MODULE, bad_postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, state}, [], false, {?MODULE, precond},
				  {wrong_mod, postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, state}, [], false, {?MODULE, precond},
				  {?MODULE, bad_postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, state}, [], false, {?MODULE, precond},
				   {wrong_mod, postcond}, ?MODULE)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, state}}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, state}, [], false, {?MODULE, precond},
				   {?MODULE, bad_postcond}, ?MODULE)),
    ok.

    
%% Callback exit API tests
callback_exit_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, exit}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _},
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, exit}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, exit}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, exit}},
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, exit}, [], false)),
    %% Handle_info - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_info(?MODULE, "info", {?DO_EXIT, exit})),
    
    %% Handle_info - stay-alive == true
    ?match({noreply, {?NO_EXIT, exit}}, 
		 corba:handle_info(?MODULE, "info", {?NO_EXIT, exit})),
    ok.


%% Callback badarith API tests
callback_badarith_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, badarith}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}},_}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, badarith}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, badarith}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, badarith}},
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, badarith}, [], false)),
    %% Handle_info - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_info(?MODULE, "info", {?DO_EXIT, badarith})),
    
    %% Handle_info - stay-alive == true
    ?match({noreply, {?NO_EXIT, badarith}}, 
		 corba:handle_info(?MODULE, "info", {?NO_EXIT, badarith})),
    ok.
    
%% Callback case_clause API tests
callback_case_clause_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, case_clause}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _},
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, case_clause}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, case_clause}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, case_clause}},
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, case_clause}, [], false)),
    %% Handle_info - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_info(?MODULE, "info", {?DO_EXIT, case_clause})),
    
    %% Handle_info - stay-alive == true
    ?match({noreply, {?NO_EXIT, case_clause}}, 
		 corba:handle_info(?MODULE, "info", {?NO_EXIT, case_clause})),
    ok.
    
%% Callback function_clause API tests
callback_function_clause_api(_) ->
    %% Handle_call - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_call(?MODULE, foo, [],
				   {?DO_EXIT, function_clause}, [], false, false)),
    %% Handle_call - stay-alive == true
    ?match({reply, {'EXCEPTION', #'OBJ_ADAPTER'{}}, _},
		 corba:handle_call(?MODULE, foo, [],
				   {?NO_EXIT, function_clause}, [], false, false)),
    %% Handle_cast - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?DO_EXIT, function_clause}, [], false)),
    %% Handle_cast - stay-alive == true
    ?match({noreply, {?NO_EXIT, function_clause}},
		 corba:handle_cast(?MODULE, foo_1w, [],
				   {?NO_EXIT, function_clause}, [], false)),
    %% Handle_info - stay-alive == false
    ?match({'EXIT', _}, 
		 corba:handle_info(?MODULE, "info", {?DO_EXIT, function_clause})),
    %% Handle_info - stay-alive == true
    ?match({noreply, {?NO_EXIT, function_clause}}, 
		 corba:handle_info(?MODULE, "info", {?NO_EXIT, function_clause})),
    ok.
    
%% Faked mandatory operations
init(State) ->
    evaluate_state(State),
    {ok, State}.
terminate(_Reason, State) ->
    evaluate_state(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    evaluate_state(State),
    {ok, State}.
handle_call(_,_, State) ->
    evaluate_state(State),
    {noreply, State}.
handle_cast(_, State) ->
    evaluate_state(State),
    {noreply, State}.
handle_info(_Info, State) ->
    evaluate_state(State),
    {noreply, State}.

foo(State) ->
    evaluate_state(State),
    {reply, ok, State}.
foo(State, _Arg) ->
    evaluate_state(State),
    {reply, ok, State}.

foo_1w(State) ->
    evaluate_state(State),
    {noreply, State}.
foo_1w(State, _Arg) ->
    evaluate_state(State),
    {noreply, State}.

precond(_Module, _Function, _Args) ->
    ok.

postcond(_Module, _Function, _Args, _Result) ->
    ok.

evaluate_state(exit) ->
    exit("exit on purpose");
evaluate_state(badarith) ->
    10 * atom;
evaluate_state(case_clause) ->
    case 10 of
	false ->
	    ok
    end;
evaluate_state(module) ->
    non_existing_module:bar();
evaluate_state(function) ->
    ?MODULE:non_existing_function();
evaluate_state(arity) ->
    ?MODULE:foo(to, many, arguments);
evaluate_state(function_clause) ->
    evaluate_state(incorrect_state);
evaluate_state(state) ->
    ok.

%%-----------------------------------------------------------------
%% Local functions.
%%-----------------------------------------------------------------

pseudo_calls(0, _) ->
    ok;
pseudo_calls(Times, Obj) ->
    orber_test_server:pseudo_call(Obj),
    New = Times - 1,
    pseudo_calls(New, Obj). 
pseudo_casts(0, _) ->
    ok;
pseudo_casts(Times, Obj) ->
    orber_test_server:pseudo_cast(Obj),
    New = Times - 1,
    pseudo_casts(New, Obj). 
