%%-----------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%% File    : orber_web_SUITE.erl
%% Purpose : 
%%-----------------------------------------------------------------

-module(orber_web_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

-define(default_timeout, test_server:minutes(3)).

-define(match(ExpectedRes, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
			io:format("------ CORRECT RESULT ------~n~p~n",
				  [AcTuAlReS]),
			AcTuAlReS;
		    _ ->
			io:format("###### ERROR ERROR ######~n~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS)
		end
	end()).

-define(nomatch(Not, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    Not ->
			io:format("###### ERROR ERROR ######~n~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS);
		    _ ->
			io:format("------ CORRECT RESULT ------~n~p~n",
				  [AcTuAlReS]),
			AcTuAlReS
		end
	end()).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [menu, configure, info, nameservice, ifr_select,
     ifr_data, create, delete_ctx, add_ctx, delete_obj,
     server].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    orber:jump_start(2875),
    oe_orber_test_server:oe_register(),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    oe_orber_test_server:oe_unregister(),
    orber:jump_stop(),
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: menu
%% Description: 
%%-----------------------------------------------------------------
menu(_) ->
    Node = atom_to_list(node()),
    OK = orber_web:menu(env, [{"node", Node}]),
    ?match(OK, orber_web:menu(env, [])),
    ?match(OK, orber_web:menu(env, [42, {"node", Node}, "wrong"])),
    ?match({'EXIT', _E}, orber_web:menu(env, [{"node", localhost}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: configure
%% Description: 
%%-----------------------------------------------------------------
configure(_) ->
    Node = atom_to_list(node()),
    ?match({'EXIT', _}, orber_web:configure(env, [])),
    ?match({'EXIT', _}, orber_web:configure(env, [{"node", localhost},
						  {"data", atom}])),
    ?match([_H|_T], orber_web:configure(env, [{"node", Node}, {"data", ""}])),
    ?match([_H|_T], orber_web:configure(env, [{"node", Node},
					      {"data", "[{orber_debug_level, 9}]"}])),
    ?match({ok, 9}, application:get_env(orber, orber_debug_level)),
    ?match([_H|_T], orber_web:configure(env, [{"node", "bad_node"},
					      {"data", "[{orber_debug_level, 9}]"}])),
    ?match({error, _}, orber_web:configure(env, [{"node", Node},
						 {"data", "{orber_debug_level 9}"}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: info
%% Description: 
%%-----------------------------------------------------------------
info(_) ->
    ?match({'EXIT', _}, orber_web:info(env, [])),
    ?match({'EXIT', _}, orber_web:info(env, [{"node", localhost}])),
    ?match([_H|_T], orber_web:info(env, [{"node", atom_to_list(node())}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: nameservice
%% Description: 
%%-----------------------------------------------------------------
nameservice(_) ->
    NodeStr = atom_to_list(node()),
    ?match({'EXIT', _}, orber_web:nameservice(env, [{"node", localhost}, 
						    {"context", "root"}])),
    ?match({'EXIT', _}, orber_web:nameservice(env, [{"node", localhost}, 
						    {"context", "id1"}])),
    ?match([_H|_T], orber_web:nameservice(env, [{"node", "bad_node"}, 
						{"context", "root"}])),
    ?match([_,_,_,NodeStr|_], orber_web:nameservice(env, [{"node", NodeStr}, 
							  {"context", "root"}])),
    ?match({ok,_}, orber_web:nameservice(env, [{"node", NodeStr}, 
					       {"context", "id1"}])),
    ?match([_H|_T], orber_web:add_ctx(env, [{"node", NodeStr},
					    {"context", "root"},
					    {"id", "id1"}])),
    ?match([_H|_T], orber_web:add_ctx(env, [{"node", NodeStr},
					    {"context", "id1"},
					    {"id", "id2"}])),
    [_,_,_,IOR] = 
	?match([_,_,_,_], orber_web:create(env, [{"node", NodeStr},
						 {"module", "orber_test_server"},
						 {"arguments", "[]"},
						 {"options", "[{pseudo, true}]"},
						 {"namestr", "id1/id2/id3"},
						 {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR)),

    ?match([_,"id1"|_], orber_web:nameservice(env, [{"node", NodeStr}, 
						    {"context", "id1"}])),
    ?nomatch({error, _}, orber_web:nameservice(env, [{"node", NodeStr}, 
						     {"context", "id1/id2"},
						     {"object", "id3"}])),
    
    ok.

%%-----------------------------------------------------------------
%% Test Case: ifr_select
%% Description: 
%%-----------------------------------------------------------------
ifr_select(_) ->
    ?match({'EXIT', _}, orber_web:ifr_select(env, [])),
    ?match({'EXIT', _}, orber_web:ifr_select(env, [{"node", localhost}])),
    ?match([_H|_T], orber_web:ifr_select(env, [{"node", "bad_node"}])),
    ?match([_H|_T], orber_web:ifr_select(env, [{"node", atom_to_list(node())}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: ifr_data
%% Description: 
%%-----------------------------------------------------------------
ifr_data(_) ->
    ?match({'EXIT', _}, orber_web:ifr_data(env, [])),
    ?match({'EXIT', _}, orber_web:ifr_data(env, [{"node", localhost},
						 {"table", "ir_ModuleDef"}])),
    ?match({error, _}, orber_web:ifr_data(env, [{"node", "bad_host"},
						{"table", "ir_ModuleDef"}])),
    ?match({'EXIT', _}, orber_web:ifr_data(env, [{"node", atom_to_list(node())},
						 {"table", "bad_table"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_ModuleDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_InterfaceDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_StructDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_ExceptionDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_ConstantDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_EnumDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_AliasDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_AttributeDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_OperationDef"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_Contained"}])),
    ?match([_H|_T], orber_web:ifr_data(env, [{"node", atom_to_list(node())},
					     {"table", "ir_TypedefDef"}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: create
%% Description: 
%%-----------------------------------------------------------------
create(_) ->
    NodeStr = atom_to_list(node()),
    ?match({'EXIT', _}, orber_web:create(env, [])),
    ?match({'EXIT', _}, orber_web:create(env, [{"node", localhost}])),
    ?match({error, _}, orber_web:create(env, [{"node", NodeStr},
					      {"module", "orber_test_server"},
					      {"arguments", "[]"},
					      {"options", "[bad_option 42]"},
					      {"namestr", "[]"},
					      {"bind", "rebind"}])),
    ?match({error, _}, orber_web:create(env, [{"node", NodeStr},
					      {"module", "orber_test_server"},
					      {"arguments", "[bad_argument 42]"},
					      {"options", "[]"},
					      {"namestr", "[]"},
					      {"bind", "rebind"}])),

    ?match([_, NodeStr|_T], orber_web:create(env, [{"node", NodeStr}])),
    
    [_,IOR] = ?match([_,_], orber_web:create(env, [{"node", NodeStr},
						   {"module", "orber_test_server"},
						   {"arguments", "[]"},
						   {"options", "[]"},
						   {"namestr", ""},
						   {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR)),

    [_,_,_,_,_,IOR2] =
	?match([_,_,_,_,_,_], orber_web:create(env, [{"node", NodeStr},
						     {"module", "orber_test_server"},
						     {"arguments", "[]"},
						     {"options", "[]"},
						     {"namestr", "id1"},
						     {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR2)),

    [_,_,_,IOR3] =
	?match([_,_,_,_], orber_web:create(env, [{"node", NodeStr},
						 {"module", "orber_test_server"},
						 {"arguments", "[]"},
						 {"options", "[{pseudo, true}]"},
						 {"namestr", "id2"},
						 {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR3)),

    [_,IOR4] =?match([_,_], orber_web:create(env, [{"node", NodeStr},
						   {"module", "orber_test_server"},
						   {"arguments", "[]"},
						   {"options", "[{pseudo, true}]"},
						   {"namestr", ""},
						   {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR4)),

    ?match([_], orber_web:create(env, [{"node", NodeStr},
				       {"module", "orber_test_server"},
				       {"arguments", "[]"},
				       {"options", "[{unknown, option}]"},
				       {"namestr", "id1"},
				       {"bind", "rebind"}])),
    
    ?match([_, "id1"], orber_web:create(env, [{"node", NodeStr},
					      {"module", "orber_test_server"},
					      {"arguments", "[]"},
					      {"options", "[]"},
					      {"namestr", "id1"},
					      {"bind", "bind"}])),
    
    ?match([_, "id2"], orber_web:create(env, [{"node", NodeStr},
					      {"module", "orber_test_server"},
					      {"arguments", "[]"},
					      {"options", "[{pseudo, true}]"},
					      {"namestr", "id2"},
					      {"bind", "bind"}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: delete_ctx
%% Description: 
%%-----------------------------------------------------------------
delete_ctx(_) ->
    ?match({ok, _}, orber_web:delete_ctx(env, [{"node", atom_to_list(node())},
					       {"context", "id1"}])),
    ?match([_H|_T], orber_web:add_ctx(env, [{"node", atom_to_list(node())},
					    {"context", "root"},
					    {"id", "id1"}])),
    ?match([_H|_T], orber_web:delete_ctx(env, [{"node", atom_to_list(node())},
					       {"context", "id1"}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: add_ctx
%% Description: 
%%-----------------------------------------------------------------
add_ctx(_) ->
    ?match({error, _}, orber_web:add_ctx(env, [{"node", "bad_node"},
 					       {"context", "root"},
					       {"id", "id1"}])),
    ?match([_H|_T], orber_web:add_ctx(env, [{"node", atom_to_list(node())},
					    {"context", "root"},
					    {"id", ""}])),
    ?match([_H|_T], orber_web:add_ctx(env, [{"node", atom_to_list(node())},
					    {"context", "root"},
					    {"id", "id1"}])),
    ?match([_H|_T], orber_web:add_ctx(env, [{"node", atom_to_list(node())},
					    {"context", "id1"},
					    {"id", "id2"}])),
    ok.

%%-----------------------------------------------------------------
%% Test Case: delete_obj
%% Description: 
%%-----------------------------------------------------------------
delete_obj(_) ->
    NodeStr = atom_to_list(node()),
    ?match({error, _}, orber_web:delete_obj(env, [{"node", "bad_node"},
						  {"context", "id1"},
						  {"action", "unbind"}])),
    ?match({error, _}, orber_web:delete_obj(env, [{"node", "bad_node"},
						  {"context", "id1"},
						  {"action", "both"}])),
    ?match({'EXIT', _}, orber_web:delete_obj(env, [{"node", bad_node},
						   {"context", "id1"},
						   {"action", "both"}])),
    ?match({error, _}, orber_web:delete_obj(env, [{"node", NodeStr},
						  {"context", "non/existing"},
						  {"action", "unbind"}])),
    [_,_,_,_,_,IOR2] =
	?match([_,_,_,_,_,_], orber_web:create(env, [{"node", NodeStr},
						     {"module", "orber_test_server"},
						     {"arguments", "[]"},
						     {"options", "[]"},
						     {"namestr", "id1"},
						     {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR2)),
    
    ?match({error, _}, orber_web:delete_obj(env, [{"node", NodeStr},
						  {"context", "bad/INS./id"},
						  {"action", "unbind"}])),

    [_,_,_,IOR3] =
	?match([_,_,_,_], orber_web:create(env, [{"node", NodeStr},
						 {"module", "orber_test_server"},
						 {"arguments", "[]"},
						 {"options", "[{pseudo, true}]"},
						 {"namestr", "id2"},
						 {"bind", "rebind"}])),
    ?match(#'IOP_IOR'{}, corba:string_to_object(IOR3)),

    ?match([_, "id1"|_], orber_web:delete_obj(env, [{"node", NodeStr},
						    {"context", "id1"},
						    {"action", "unbind"}])),
    ?match([_, "id2"|_], orber_web:delete_obj(env, [{"node", NodeStr},
						    {"context", "id2"},
						    {"action", "unbind"}])),


    ok.

%%-----------------------------------------------------------------
%% Test Case: server
%% Description: 
%%-----------------------------------------------------------------
server(_) ->
    NodeStr = "node=" ++ atom_to_list(node()),
    {ok, Pid} = ?match({ok,_}, orber_web_server:start()),
    ?match({error,{already_started, Pid}}, orber_web_server:start_link()),
    ?match({error,{already_started,Pid}}, orber_web_server:start()),
    ?match({orber, _}, orber_web_server:config_data()),
    ?match([_H|_T], orber_web_server:ifr_select(env, "node=badnode")),
    ?match([_H|_T], orber_web_server:ifr_select(env, "node=" ++ NodeStr)),
    ?match([_H|_T], orber_web_server:menu(env, NodeStr)),
    ?match([_H|_T], orber_web_server:configure(env, NodeStr ++ "&data=[{orber_debug_level, 9}]")), 
    ?match([_H|_T], orber_web_server:nameservice(env, NodeStr ++ "&context=root")),
    ?match([_H|_T], orber_web_server:info(env, NodeStr)),
    ?match([_H|_T], orber_web_server:ifr_data(env, NodeStr ++ "&table=ir_ModuleDef")),
    ?match([_H|_T], orber_web_server:create(env, NodeStr)),
    ?match([_H|_T], orber_web_server:add_ctx(env, NodeStr ++ "&context=root&id=id1")),
    ?match([_H|_T], orber_web_server:delete_ctx(env, NodeStr++"&context=id1")),
    ?match([_H|_T], orber_web_server:delete_obj(env, NodeStr++"&context=id1&action=unbind")),
    ?match([_H|_T], orber_web_server:default_selection(env, NodeStr)),
    ?match(ok, orber_web_server:stop()),
    ok.


