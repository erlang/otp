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
%% File    : interceptors_SUITE.erl
%% Purpose : 
%%-----------------------------------------------------------------

-module(interceptors_SUITE).

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
-export([in_reply/6, out_request/6]).
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [local_pseudo, local_default, local_local, local_global].

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
    corba:orb_init([{flags, (?ORB_ENV_USE_PI bor ?ORB_ENV_LOCAL_TYPECHECKING)}, 
		    {local_interceptors, {native, [?MODULE]}}]),
    orber:jump_start(2945),
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
%% Test Case: local_pseudo
%% Description: 
%%-----------------------------------------------------------------
local_pseudo(_) ->
    ?match({native, [?MODULE]}, orber:get_local_interceptors()),
    %% Global settings
    Obj1 = orber_test_server:oe_create(state,[{pseudo,true}]),
    Result11 = orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result11, put(in_reply, undefined)),
    
    Result12 = ?match({'EXCEPTION',_}, 
		      orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX+1)),
    ?match([(?USHORTMAX+1)], put(out_request, undefined)),
    ?nomatch(Result12, put(in_reply, undefined)),

    Result13 = orber_test_server:testing_iiop_oneway_delay(Obj1, 0),
    ?match([0], put(out_request, undefined)),
    ?nomatch(Result13, put(in_reply, undefined)),
    
    Result14 = ?match({'EXCEPTION', _}, 
		      orber_test_server:raise_local_exception(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result14, put(in_reply, undefined)),

    Result15 = ?match({'EXCEPTION',_}, orber_test_server:stop_brutal(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result15, put(in_reply, undefined)),
    
    %% Per-object
    Obj2 = orber_test_server:oe_create(state,[{pseudo,true},
					      {local_interceptors, false}]),
    
    Result21 = orber_test_server:testing_iiop_ushort(Obj2, ?USHORTMAX),
    ?nomatch([?USHORTMAX], put(out_request, undefined)),
    ?nomatch(Result21, put(in_reply, undefined)),

    Obj3 = orber_test_server:oe_create(state,[{pseudo,true},
					      {local_interceptors, true}]),
    
    Result31 = orber_test_server:testing_iiop_ushort(Obj3, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result31, put(in_reply, undefined)),

    ok.

%%-----------------------------------------------------------------
%% Test Case: local_default
%% Description: 
%%-----------------------------------------------------------------
local_default(_) ->
    ?match({native, [?MODULE]}, orber:get_local_interceptors()),
    %% Global settings
    Obj1 = orber_test_server:oe_create(state, []),
    Result11 = orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result11, put(in_reply, undefined)),
    
    Result12 = ?match({'EXCEPTION',_}, 
		      orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX+1)),
    ?match([(?USHORTMAX+1)], put(out_request, undefined)),
    ?nomatch(Result12, put(in_reply, undefined)),

    Result13 = orber_test_server:testing_iiop_oneway_delay(Obj1, 0),
    ?match([0], put(out_request, undefined)),
    ?nomatch(Result13, put(in_reply, undefined)),
    
    Result14 = ?match({'EXCEPTION', _}, 
		      orber_test_server:raise_local_exception(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result14, put(in_reply, undefined)),

    Result15 = ?match({'EXCEPTION',_}, orber_test_server:stop_brutal(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result15, put(in_reply, undefined)),
    
    
    %% Per-object
    Obj2 = orber_test_server:oe_create(state,[{local_interceptors, false}]),
    
    Result21 = orber_test_server:testing_iiop_ushort(Obj2, ?USHORTMAX),
    ?nomatch([?USHORTMAX], put(out_request, undefined)),
    ?nomatch(Result21, put(in_reply, undefined)),
    corba:dispose(Obj2),

    Obj3 = orber_test_server:oe_create(state,[{local_interceptors, true}]),
    
    Result31 = orber_test_server:testing_iiop_ushort(Obj3, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result31, put(in_reply, undefined)),
    corba:dispose(Obj3),
    ok.

%%-----------------------------------------------------------------
%% Test Case: local_local
%% Description: 
%%-----------------------------------------------------------------
local_local(_) ->
    ?match({native, [?MODULE]}, orber:get_local_interceptors()),
    %% Global settings
    Obj1 = orber_test_server:oe_create(state, [{regname, {local, regname}}]),
    Result11 = orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result11, put(in_reply, undefined)),
    
    Result12 = ?match({'EXCEPTION',_}, 
		      orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX+1)),
    ?match([(?USHORTMAX+1)], put(out_request, undefined)),
    ?nomatch(Result12, put(in_reply, undefined)),

    Result13 = orber_test_server:testing_iiop_oneway_delay(Obj1, 0),
    ?match([0], put(out_request, undefined)),
    ?nomatch(Result13, put(in_reply, undefined)),
    
    Result14 = ?match({'EXCEPTION', _}, 
		      orber_test_server:raise_local_exception(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result14, put(in_reply, undefined)),

    Result15 = ?match({'EXCEPTION',_}, orber_test_server:stop_brutal(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result15, put(in_reply, undefined)),
    
    %% Per-object
    Obj2 = orber_test_server:oe_create(state,[{regname, {local, regname}},
					      {local_interceptors, false}]),
    
    Result21 = orber_test_server:testing_iiop_ushort(Obj2, ?USHORTMAX),
    ?nomatch([?USHORTMAX], put(out_request, undefined)),
    ?nomatch(Result21, put(in_reply, undefined)),
    corba:dispose(Obj2),

    Obj3 = orber_test_server:oe_create(state,[{regname, {local, regname}},
					      {local_interceptors, true}]),
    
    Result31 = orber_test_server:testing_iiop_ushort(Obj3, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result31, put(in_reply, undefined)),
    corba:dispose(Obj3),
    ok.

%%-----------------------------------------------------------------
%% Test Case: local_global
%% Description: 
%%-----------------------------------------------------------------
local_global(_) ->
    ?match({native, [?MODULE]}, orber:get_local_interceptors()),
    %% Global settings
    Obj1 = orber_test_server:oe_create(state, [{regname, {global, regname}}]),
    Result11 = orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result11, put(in_reply, undefined)),
    
    Result12 = ?match({'EXCEPTION',_}, 
		      orber_test_server:testing_iiop_ushort(Obj1, ?USHORTMAX+1)),
    ?match([(?USHORTMAX+1)], put(out_request, undefined)),
    ?nomatch(Result12, put(in_reply, undefined)),

    Result13 = orber_test_server:testing_iiop_oneway_delay(Obj1, 0),
    ?match([0], put(out_request, undefined)),
    ?nomatch(Result13, put(in_reply, undefined)),
    
    Result14 = ?match({'EXCEPTION', _}, 
		      orber_test_server:raise_local_exception(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result14, put(in_reply, undefined)),

    Result15 = ?match({'EXCEPTION',_}, orber_test_server:stop_brutal(Obj1)),
    ?match([], put(out_request, undefined)),
    ?match(Result15, put(in_reply, undefined)),
    
    %% Per-object
    Obj2 = orber_test_server:oe_create(state,[{regname, {global, regname}},
					      {local_interceptors, false}]),
    
    Result21 = orber_test_server:testing_iiop_ushort(Obj2, ?USHORTMAX),
    ?nomatch([?USHORTMAX], put(out_request, undefined)),
    ?nomatch(Result21, put(in_reply, undefined)),
    corba:dispose(Obj2),

    Obj3 = orber_test_server:oe_create(state,[{regname, {global, regname}},
					      {local_interceptors, true}]),
    
    Result31 = orber_test_server:testing_iiop_ushort(Obj3, ?USHORTMAX),
    ?match([?USHORTMAX], put(out_request, undefined)),
    ?match(Result31, put(in_reply, undefined)),
    corba:dispose(Obj3),
    ok.




%%-----------------------------------------------------------------
%% Local functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% function : in_reply
%%-----------------------------------------------------------------
in_reply(Ref, _ObjKey, Ctx, Op, Reply, _Args) ->
    error_logger:info_msg("=============== in_reply =================
Connection: ~p
Operation : ~p
Reply     : ~p
Context   : ~p
==========================================~n", 
                          [Ref, Op, Reply, Ctx]),
    put(in_reply, Reply),
    {Reply, "NewArgs"}.

%%-----------------------------------------------------------------
%% function : out_request
%%-----------------------------------------------------------------
out_request(Ref, _ObjKey, Ctx, Op, Params, _Args) ->
    error_logger:info_msg("=============== out_request ==============
Connection: ~p
Operation : ~p
Parameters: ~p
Context   : ~p
==========================================~n", 
                          [Ref, Op, Params, Ctx]),
    put(out_request, Params),
    {Params, "NewArgs"}.
