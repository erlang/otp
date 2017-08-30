%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% File    : grammar_SUITE.erl
%% Purpose : Testing the CosNotification BNF grammar.
%%--------------------------------------------------------------------

-module(grammar_SUITE).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% cosEvent files.
-include_lib("cosEvent/include/CosEventChannelAdmin.hrl").
%% Application files
-include_lib("cosNotification/include/CosNotification.hrl").
-include_lib("cosNotification/include/CosNotifyChannelAdmin.hrl").
-include_lib("cosNotification/include/CosNotifyComm.hrl").
-include_lib("cosNotification/include/CosNotifyFilter.hrl").

-include_lib("cosNotification/src/CosNotification_Definitions.hrl").

-include("idl_output/notify_test.hrl").

-include_lib("common_test/include/ct.hrl").

%%--------------- DEFINES ------------------------------------
-define(default_timeout, test_server:minutes(20)).
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
 
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 cases/0, init_per_suite/1, end_per_suite/1, 
	 union_api/1, enum_api/1, simple_types_api/1,
	 components_api/1, positional_api/1, variable_api/1,
	 init_per_testcase/2, end_per_testcase/2]).

-import(cosNotification_Filter, [create_filter/1, eval/2]).

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
    [variable_api, union_api, enum_api, simple_types_api,
     components_api, positional_api].

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
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    if
        is_list(Config) ->
	    Config;
        true ->
            exit("Config not a list")
    end.
 
end_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Config.


%%-----------------------------------------------------------------
%%  simple types grammar tests
%%-----------------------------------------------------------------
simple_types_api(_Config) ->
    %% Will always be true, no matter what kind of event we receive.
    {ok,T1}  = ?match({ok, _}, create_filter("2==2 and 3<4")),
    ?match(true, eval(T1, ?not_CreateSE("DomainName","TypeName","EventName",
					[],[], any:create(orber_tc:null(), null)))),
    
    %% Will always be true, no matter what kind of event we receive.
    {ok,T2}  = ?match({ok, _}, create_filter("")),
    ?match(true, eval(T2, ?not_CreateSE("DomainName","TypeName","EventName",
					[],[], any:create(orber_tc:null(), null)))),

    %% Check if $variable works
    {ok,T3}  = ?match({ok, _}, create_filter("$city == \'Berlin\'")),
    ?match(true, eval(T3, ?not_CreateSE("DomainName","TypeName","EventName",
					[#'CosNotification_Property'{name="city", 
								     value=any:create(orber_tc:string(0), "Berlin")}],
					[], any:create(orber_tc:null(), null)))),
    ?match(false, eval(T3, ?not_CreateSE("DomainName","TypeName", "EventName",
					 [#'CosNotification_Property'{name="city", 
								      value=any:create(orber_tc:string(0), "Dallas")}],
					 
					[], any:create(orber_tc:null(), null)))),
    
    
    {ok,T4}  = ?match({ok, _}, create_filter("$zip == 44")),
    ?match(true, eval(T4, ?not_CreateSE("DomainName","TypeName", "EventName",
					[#'CosNotification_Property'{name="zip", 
								     value=any:create(orber_tc:short(), 44)}],
					
					[], any:create(orber_tc:null(), null)))),
    ?match(true, eval(T4, ?not_CreateSE("DomainName","TypeName", "EventName",
					[],[], 
					any:create('CosNotification_Property':tc(), 
						   #'CosNotification_Property'
						   {name="zip", 
						    value=any:create(orber_tc:short(), 
								     44)})))),
    ?match(false, eval(T4, ?not_CreateSE("DomainName","TypeName", "EventName",
					 [#'CosNotification_Property'{name="zip", 
								      value=any:create(orber_tc:short(), 33)}],
					 
					[], any:create(orber_tc:null(), null)))),
    
    %% Will always be true, no matter what kind of event we receive.
    {ok,T5}  = ?match({ok, _}, create_filter("\'oo'\~\'foobar\'")),
    ?match(true, eval(T5, ?not_CreateSE("DomainName","TypeName","EventName",
					[],[], any:create(orber_tc:null(), null)))),
    %% Will always be false, no matter what kind of event we receive.
    {ok,T6} = ?match({ok, _}, create_filter("\'o1'\~\'foobar\'")),
    ?match(false, eval(T6, ?not_CreateSE("DomainName","TypeName","EventName",
					 [],[], any:create(orber_tc:null(), null)))),

    %% Can we apply the ~ operation as above using a variable
    {ok,T7} = ?match({ok, _}, create_filter("$str~\'foobar\'")),
    ?match(true, eval(T7, ?not_CreateSE("DomainName","TypeName","EventName",
					[#'CosNotification_Property'{name="str", 
								     value=any:create(orber_tc:string(0), "oo")}],
					[], any:create(orber_tc:null(), null)))),
    ?match(false, eval(T7, ?not_CreateSE("DomainName","TypeName","EventName",
					 [#'CosNotification_Property'{name="str", 
								      value=any:create(orber_tc:string(0), "ok")}],
					 [], any:create(orber_tc:null(), null)))),
    
    

    {ok,_T8} = ?match({ok, _}, create_filter("$\\zip == 44444")),

    ok.

%%-----------------------------------------------------------------
%%  enum grammar tests
%%-----------------------------------------------------------------
enum_api(_Config) ->
    %% Accept events whose 'in' enum is set to the value 'HOUSE' or 'CAR'. 
    {ok,T1} = ?match({ok, _}, create_filter("$.\\in == HOUSE or $.\\in == CAR")),

    ?match(true, eval(T1, any:create(orber_tc:alias("IFRId","in",tk_any),
				     any:create({tk_enum, "IFRId", "in", ["HOUSE", "CAR"]},
						'HOUSE')))),
    ?match(false, eval(T1, any:create(orber_tc:alias("IFRId","in",tk_any),
				      any:create({tk_enum, "IFRId", "in", ["HOUSE", "CAR"]},
						 'GARAGE')))),
    ok.
  

%%-----------------------------------------------------------------
%%  Union grammar tests
%%-----------------------------------------------------------------
union_api(_Config) ->
    {ok,T1} = ?match({ok, _}, create_filter("exist $.uni1._d and $.uni1._d == 1 and $.uni1.(1) == 10")),
    {ok,T2} = ?match({ok, _}, create_filter("default $.uni1._d and $.uni1.() == 10")),
    {ok,T3} = ?match({ok, _}, create_filter("default $.uni1._d and $.uni1.(999) == 10")),
    ?match(true, eval(T1, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(orber_tc:alias("IDL:notify_test/namedAny:1.0",
								  "uni1",
								  tk_any),
						    any:create(notify_test_uni1:tc(),
							       #notify_test_uni1{label= 1, 
										 value=10}))))),
    ?match(true, eval(T2, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(orber_tc:alias("IDL:notify_test/namedAny:1.0",
								   "uni1",
								  tk_any),
						    any:create(notify_test_uni1:tc(),
							       #notify_test_uni1{label= 100, 
										 value=10}))))),
    ?match(true, eval(T3, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(orber_tc:alias("IDL:notify_test/namedAny:1.0",
								  "uni1",
								  tk_any),
						    any:create(notify_test_uni1:tc(),
							       #notify_test_uni1{label= 100, 
										 value=10}))))),
    ?match(true, eval(T1, any:create(orber_tc:alias("IDL:notify_test/namedAny:1.0",
						    "uni1",
						    tk_any),
				     any:create(notify_test_uni1:tc(),
						#notify_test_uni1{label= 1, 
								   value=10})))),
    ?match(false, eval(T2, any:create(orber_tc:alias("IDL:notify_test/namedAny:1.0",
						     "uni1",
						     tk_any),
				      any:create(notify_test_uni1:tc(),
						 #notify_test_uni1{label= 1, 
								   value=10})))),
    ?match(false, eval(T3, any:create(orber_tc:alias("IDL:notify_test/namedAny:1.0",
						     "uni1",
						     tk_any),
				      any:create(notify_test_uni1:tc(),
						 #notify_test_uni1{label= 1, 
								   value=10})))),
    ?match(true, eval(T1, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(notify_test_studies:tc(), #notify_test_studies
						    {uni1 = #notify_test_uni1{label= 1, value=10},
						     gpa = 90,
						     tests = [#'CosNotification_Property'
							      {name="midterm", value=any:create(orber_tc:short(), 70)},
							      #'CosNotification_Property'
							      {name="final", value=any:create(orber_tc:short(), 60)}],
						     monthly_attendance = {0,1,2,10}})))),
    ?match(false, eval(T2, ?not_CreateSE("DomainName","CommunicationsAlarm",
					 "EventName",[],[],
					 any:create(notify_test_studies:tc(), #notify_test_studies
						    {uni1 = #notify_test_uni1{label= 1, value=10},
						     gpa = 90,
						     tests = [#'CosNotification_Property'
							      {name="midterm", value=any:create(orber_tc:short(), 70)},
							      #'CosNotification_Property'
							      {name="final", value=any:create(orber_tc:short(), 60)}],
						     monthly_attendance = {0,1,2,10}})))),
    ?match(false, eval(T3, ?not_CreateSE("DomainName","CommunicationsAlarm",
					 "EventName",[],[],
					 any:create(notify_test_studies:tc(), #notify_test_studies
						    {uni1 = #notify_test_uni1{label= 1, value=10},
						     gpa = 90,
						     tests = [#'CosNotification_Property'
							      {name="midterm", value=any:create(orber_tc:short(), 70)},
							      #'CosNotification_Property'
							      {name="final", value=any:create(orber_tc:short(), 60)}],
						     monthly_attendance = {0,1,2,10}})))),
    ?match(true, eval(T1, any:create(notify_test_studies:tc(), #notify_test_studies
				     {uni1 = #notify_test_uni1{label= 1, value=10},
				      gpa = 90,
				      tests = [#'CosNotification_Property'
					       {name="midterm", value=any:create(orber_tc:short(), 70)},
					       #'CosNotification_Property'
					       {name="final", value=any:create(orber_tc:short(), 60)}],
				      monthly_attendance = {0,1,2,10}}))),
    ?match(false, eval(T2, any:create(notify_test_studies:tc(), #notify_test_studies
				      {uni1 = #notify_test_uni1{label= 1, value=10},
				       gpa = 90,
				       tests = [#'CosNotification_Property'
						{name="midterm", value=any:create(orber_tc:short(), 70)},
						#'CosNotification_Property'
						{name="final", value=any:create(orber_tc:short(), 60)}],
				       monthly_attendance = {0,1,2,10}}))),
    ?match(false, eval(T3, any:create(notify_test_studies:tc(), #notify_test_studies
				      {uni1 = #notify_test_uni1{label= 1, value=10},
				       gpa = 90,
				       tests = [#'CosNotification_Property'
						{name="midterm", value=any:create(orber_tc:short(), 70)},
						#'CosNotification_Property'
						{name="final", value=any:create(orber_tc:short(), 60)}],
				       monthly_attendance = {0,1,2,10}}))),
    
    {ok,T4} = ?match({ok, _}, create_filter("exist $.alias.uni1._d and $.alias.uni1._d == 1 and $.alias.uni1.(1) == 10")),
    ?match(true, eval(T4, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(orber_tc:alias(notify_test_studies:id(),
								  "alias",
								  notify_test_studies:tc()), 
						   #notify_test_studies
						   {uni1 = #notify_test_uni1{label= 1, value=10},
						    gpa = 90, tests = [],
						    monthly_attendance = {0,1,2,10}})))),
    ?match(true, eval(T4, any:create(orber_tc:alias(notify_test_studies:id(),
						    "alias",
						    notify_test_studies:tc()), 
				     #notify_test_studies
				     {uni1 = #notify_test_uni1{label= 1, value=10},
				      gpa = 90, tests = [],
				      monthly_attendance = {0,1,2,10}}))),
    %% Accept events with a default union discriminator set to the value 2. 
    {ok,T5} = ?match({ok, _}, create_filter("default $._d and $.defvalue == 2")),
    ?match(true, eval(T5, any:create(notify_test_uni1:tc(), 
				     #notify_test_uni1{label= 100, value=2}))),
    %% label not default.
    ?match(false, eval(T5, any:create(notify_test_uni1:tc(), 
				      #notify_test_uni1{label= 2, value=2}))),
    %% Default does not exist (nor the component defvalue)
    ?match(false, eval(T5, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 100, value=2}))),
    %% Both wrong
    ?match(false, eval(T5, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 2, value=2}))),
   
    {ok,T6} = ?match({ok, _}, create_filter("default $._d and $.(-8) == 2")),
    ?match(true, eval(T6, any:create(notify_test_uni1:tc(), 
				     #notify_test_uni1{label= 100, value=2}))),
    %% label not default.
    ?match(false, eval(T6, any:create(notify_test_uni1:tc(), 
				      #notify_test_uni1{label= 2, value=2}))),
    %% Default does not exist (nor the component defvalue)
    ?match(false, eval(T6, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 100, value=2}))),
    %% Both wrong
    ?match(false, eval(T6, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 2, value=2}))),
    %% the same as the above, but we try to access a label that is not default
    {ok,T7} = ?match({ok, _}, create_filter("default $._d and $.(2) == 2")),
    ?match({error, _}, eval(T7, any:create(notify_test_uni1:tc(), 
					   #notify_test_uni1{label= 100, value=2}))),

    %% Must be a default-union with its 'defvalue' set to 2.
    {ok,T8} = ?match({ok, _}, create_filter("default $._d and $.('defvalue') == 2")),
    ?match(true, eval(T8, any:create(notify_test_uni1:tc(), 
				     #notify_test_uni1{label= 100, value=2}))),
    %% label not default.
    ?match(false, eval(T8, any:create(notify_test_uni1:tc(), 
				      #notify_test_uni1{label= 2, value=2}))),
    %% Default does not exist (nor the component defvalue)
    ?match(false, eval(T8, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 100, value=2}))),
    %% Both wrong
    ?match(false, eval(T8, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 2, value=2}))),

    %% Must be a default-union with its value set to 2.
    {ok,T9} = ?match({ok, _}, create_filter("default $._d and $.(+100) == 2")),
    ?match(true, eval(T9, any:create(notify_test_uni1:tc(), 
				     #notify_test_uni1{label= 100, value=2}))),
    %% label not default.
    ?match(false, eval(T9, any:create(notify_test_uni1:tc(), 
				      #notify_test_uni1{label= 2, value=2}))),
    %% Default does not exist (nor the component defvalue)
    ?match(false, eval(T9, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 100, value=2}))),
    %% Both wrong
    ?match(false, eval(T9, any:create(notify_test_uni2:tc(), 
				      #notify_test_uni2{label= 2, value=2}))),

    %% So far, we have only tested to access the union itself. No will use more
    %% complex union members.
    %% T10 and T11 is "equal"
    {ok,T10} = ?match({ok, _}, create_filter("$.M < 54")),
    {ok,T11} = ?match({ok, _}, create_filter("$.(5) < 54")),
    ?match(false, eval(T10, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value=54}))),
    ?match(false, eval(T11, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value=54}))),
    ?match(true, eval(T10, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 5, value=50}))),
    ?match(true, eval(T11, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 5, value=50}))),
    ?match({error,_}, eval(T10, any:create(notify_test_K:tc(), 
					   #notify_test_K{label= -1, value=50}))),
    ?match({error,_}, eval(T11, any:create(notify_test_K:tc(), 
					   #notify_test_K{label= -1, value=50}))),
    
    %% T12 and T13 is "equal"
    {ok,T12} = ?match({ok, _}, create_filter("$.L.C < 128")),
    {ok,T13} = ?match({ok, _}, create_filter("$.(3).2 < 128")),
    ?match(true, eval(T12, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "string",
								    'C' = 120}}))),
    ?match(true, eval(T13, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "string",
								    'C' = 120}}))),
    ?match(false, eval(T12, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "string",
								    'C' = 200}}))),
    ?match(false, eval(T13, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "string",
								    'C' = 200}}))),

    %% Test if 'putty' is a substring of K
    {ok,T15} = ?match({ok, _}, create_filter("'putty' ~ $.(2)")),
    {ok,T16} = ?match({ok, _}, create_filter("'putty' ~ $.K")),
    ?match(true, eval(T15, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "isputtyok"}))),
    ?match(true, eval(T16, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "isputtyok"}))),
    ?match(false, eval(T15, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 2, value= "notputtok"}))),
    ?match(false, eval(T16, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 2, value= "notputtok"}))),
    
    {ok,_T17} = ?match({ok, _}, create_filter("'putty' ~ $.(3).1")),
    {ok,_T18} = ?match({ok, _}, create_filter("'putty' ~ $.L.B")),
    ?match(true, eval(T12, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "isputtyok",
								    'C' = 120}}))),
    ?match(true, eval(T13, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "isputtyok",
								    'C' = 120}}))),
    ?match(false, eval(T12, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "notputtok",
								    'C' = 200}}))),
    ?match(false, eval(T13, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 3, value=
						     #notify_test_X{'A' = 1,
								    'B' = "notputtok",
								    'C' = 200}}))),

    %% Please observe that the switch 0 and 2 is defined to be equivalent.
    {ok,T19} = ?match({ok, _}, create_filter("$._d == 2 and $.(0) != 'hoob'")),
    {ok,T20} = ?match({ok, _}, create_filter("$._d == 2 and $.(2) != 'hoob'")),
    ?match(true, eval(T19, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "nothoob"}))),
    ?match(true, eval(T20, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "nothoob"}))),
    ?match(false, eval(T19, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 2, value= "hoob"}))),
    ?match(false, eval(T20, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 2, value= "hoob"}))),

    ?match(false, eval(T19, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value= 55}))),
    ?match(false, eval(T20, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value= 55}))),

    ?match(false, eval(T19, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 100, value= "nothoob"}))),
    ?match(false, eval(T20, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 100, value= "nothoob"}))),

    {ok,T21} = ?match({ok, _}, create_filter("exist $.K")),
    {ok,T22} = ?match({ok, _}, create_filter("exist $.(0) or exist $.(2)")),
    ?match(true, eval(T21, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 0, value= "hoob"}))),
    ?match(true, eval(T22, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 0, value= "hoob"}))),
    ?match(true, eval(T21, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "hoob"}))),
    ?match(true, eval(T22, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "hoob"}))),
    ?match(false, eval(T21, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value= 55}))),
    ?match(false, eval(T22, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value= 55}))),


    %% Please observe that the switch 0 and 2 is defined to be equivalent.
    {ok,T23} = ?match({ok, _}, create_filter("exist $.(2)")),
    {ok,T24} = ?match({ok, _}, create_filter("exist $.(0)")),
    ?match(true, eval(T23, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 2, value= "hoob"}))),
    ?match(false, eval(T24, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 2, value= "hoob"}))),
    ?match(false, eval(T23, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 0, value= "hoob"}))),
    ?match(true, eval(T24, any:create(notify_test_K:tc(), 
				      #notify_test_K{label= 0, value= "hoob"}))),
    ?match(false, eval(T23, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value= 55}))),
    ?match(false, eval(T24, any:create(notify_test_K:tc(), 
				       #notify_test_K{label= 5, value= 55}))),

    ok.

%%-----------------------------------------------------------------
%%  Variables grammar tests
%%-----------------------------------------------------------------
variable_api(_Config) ->
    %% Accept all "CommunicationsAlarm" events 
    {ok,T0} = ?match({ok, _}, create_filter("$type_name == 'CommunicationsAlarm'")),

    ?match(true, eval(T0, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(orber_tc:null(), null)))),
    ?match(false, eval(T0, ?not_CreateSE("DomainName","CommunicationsOK",
					 "EventName", [],[],
					 any:create(orber_tc:null(), null)))),
    ?match(true, eval(T0, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName", [],[],
					any:create(orber_tc:alias("IFRId", "type_name",
						    orber_tc:string(0)),
						    "CommunicationsOK")))),

    ?match(true, eval(T0, any:create(orber_tc:alias("IFRId", "type_name",
						    orber_tc:string(0)),
						    "CommunicationsAlarm"))), 
    ?match(false, eval(T0, any:create(orber_tc:alias("IFRId", "type_name",
						    orber_tc:string(0)),
						    "CommunicationsOK"))), 


    %% Accept all "CommunicationsAlarm" events but no "lost_packet" messages. 
    {ok,T1} = ?match({ok, _}, create_filter("$type_name == 'CommunicationsAlarm' and not ($event_name == 'lost_packet')")),

    ?match(true, eval(T1, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",[],[],
					any:create(orber_tc:null(), null)))),
    ?match(false, eval(T1, ?not_CreateSE("DomainName","CommunicationsAlarm",
					 "lost_packet", [],[],
					 any:create(orber_tc:null(), null)))),
    ?match(true, 
	   eval(T1, any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
			       [#'CosNotification_Property'{name="type_name",
					    value=any:create(orber_tc:string(0), "CommunicationsAlarm")},
				#'CosNotification_Property'{name="event_name",
					    value=any:create(orber_tc:string(0), "EventName")}]))),
    ?match(false, 
	   eval(T1, any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
			       [#'CosNotification_Property'{name="type_name",
					    value=any:create(orber_tc:string(0), "CommunicationsAlarm")},
				#'CosNotification_Property'{name="event_name",
					    value=any:create(orber_tc:string(0), "lost_packet")}]))),


    %% Accept "CommunicationsAlarm" events with priorities ranging from 1 to 5. 
    {ok,T2} = ?match({ok, _}, create_filter("$type_name == 'CommunicationsAlarm' and $priority >= 1 and $priority <= 5")),
    ?match(true, eval(T2, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName", 
					[#'CosNotification_Property'{name="priority", 
								     value=any:create(orber_tc:short(), 2)}],
					[], any:create(orber_tc:null(), null)))),
    ?match(false, eval(T2, ?not_CreateSE("DomainName","CommunicationsAlarm",
					 "EventName", 
					 [#'CosNotification_Property'{name="priority", 
								      value=any:create(orber_tc:short(), 20)}],
					 [], any:create(orber_tc:null(), null)))),
    
    %% Select "MOVIE" events featuring at least 3 of the Marx Brothers. 
    {ok,T3} = ?match({ok, _}, create_filter("$type_name == 'MOVIE' and (('groucho' in $starlist) + ('chico' in $starlist) + ('harpo' in $starlist) + ('zeppo' in $starlist) + ('gummo' in $starlist)) > 2")),
    ?match(true, eval(T3, ?not_CreateSE("DomainName","MOVIE",
					"EventName", 
					[#'CosNotification_Property'{name="starlist", 
								     value=any:create(orber_tc:sequence(orber_tc:string(0),0),
										      ["groucho", "harpo", "sam", "gummo"])}],
					[], any:create(orber_tc:null(), null)))),
    ?match(false, eval(T3, ?not_CreateSE("DomainName","MOVIE",
					 "EventName", 
					 [#'CosNotification_Property'{name="starlist", 
								      value=any:create(orber_tc:sequence(orber_tc:string(0),0),
										       ["frodo", "bilbo", "sam", "gummo"])}],
					 [], any:create(orber_tc:null(), null)))),
    %% Accept students that took all 3 tests and had an average score of at least 80%. 
    {ok,T4} = ?match({ok, _}, create_filter("$test._length == 3 and ($test[0].score + $test[1].score + $test[2].score)/3 >=80")),
    ?match(true, eval(T4, ?not_CreateSE("DomainName","TypeName",
					"EventName", [],
					[#'CosNotification_Property'{name="test", 
								     value=any:create(orber_tc:array(notify_test_data:tc(),0),
										      {#notify_test_data{score=75},
										       #notify_test_data{score=80},
										       #notify_test_data{score=85}})}],
					any:create(orber_tc:null(), null)))),
    ?match(false, eval(T4, ?not_CreateSE("DomainName","TypeName",
					 "EventName", [],
					 [#'CosNotification_Property'{name="test", 
								      value=any:create(orber_tc:array(notify_test_data:tc(),0),
										       {#notify_test_data{score=75},
											#notify_test_data{score=80},
											#notify_test_data{score=80}})}],
					 any:create(orber_tc:null(), null)))),
    ?match(false, eval(T4, ?not_CreateSE("DomainName","TypeName",
					 "EventName", [],
					 [#'CosNotification_Property'{name="test", 
								      value=any:create(orber_tc:array(notify_test_data:tc(),0),
										       {#notify_test_data{score=75},
											#notify_test_data{score=85}})}],
					 any:create(orber_tc:null(), null)))),
    %% Select processes that exceed a certain usage threshold. 
    {ok,T5} = ?match({ok, _}, create_filter("$memsize / 5.5 + $cputime * 1275.0 + $filesize * 1.25 > 500000.0")),
    ?match(true, eval(T5, ?not_CreateSE("DomainName","TypeName",
					"EventName", [],
					[#'CosNotification_Property'{name="memsize", 
								     value=any:create(orber_tc:float(), 5.5)},
					 #'CosNotification_Property'{name="cputime", 
								     value=any:create(orber_tc:float(), 0.00078431137)},
					 #'CosNotification_Property'{name="filesize", 
								     value=any:create(orber_tc:float(), 500000)}],
					any:create(orber_tc:null(), null)))),
    ?match(false, eval(T5, ?not_CreateSE("DomainName","TypeName",
					 "EventName", [],
					 [#'CosNotification_Property'{name="memsize", 
								      value=any:create(orber_tc:float(), 5.5)},
					  #'CosNotification_Property'{name="cputime", 
								      value=any:create(orber_tc:float(), 0.00078431137)},
					  #'CosNotification_Property'{name="filesize", 
								      value=any:create(orber_tc:float(), 500)}],
					 any:create(orber_tc:null(), null)))),
    ?match({error, _}, eval(T5, ?not_CreateSE("DomainName","TypeName",
					      "EventName", [],
					      [#'CosNotification_Property'{name="memsize", 
									   value=any:create(orber_tc:float(), 5.5)},
					       #'CosNotification_Property'{name="filesize", 
									   value=any:create(orber_tc:float(), 500)}],
					      any:create(orber_tc:null(), null)))),

    ?match(true, eval(T5, ?not_CreateSE("DomainName","TypeName",
					"EventName", [], [],
					any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
					[#'CosNotification_Property'{name="memsize", 
								     value=any:create(orber_tc:float(), 5.5)},
					 #'CosNotification_Property'{name="cputime", 
								     value=any:create(orber_tc:float(), 0.00078431137)},
					 #'CosNotification_Property'{name="filesize", 
								     value=any:create(orber_tc:float(), 500000)}])))),
    ?match(false, eval(T5, ?not_CreateSE("DomainName","TypeName",
					"EventName", [], [],
					any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
					[#'CosNotification_Property'{name="memsize", 
								     value=any:create(orber_tc:float(), 5.5)},
					 #'CosNotification_Property'{name="cputime", 
								     value=any:create(orber_tc:float(), 0.00078431137)},
					 #'CosNotification_Property'{name="filesize", 
								     value=any:create(orber_tc:float(), 500)}])))),
    ?match({error, _}, eval(T5, ?not_CreateSE("DomainName","TypeName",
					"EventName", [], [],
					any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
					[#'CosNotification_Property'{name="memsize", 
								     value=any:create(orber_tc:float(), 5.5)},
					 #'CosNotification_Property'{name="filesize", 
								     value=any:create(orber_tc:float(), 500)}])))),

    ?match(true, eval(T5, any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
				     [#'CosNotification_Property'{name="memsize", 
								  value=any:create(orber_tc:float(), 5.5)},
				      #'CosNotification_Property'{name="cputime", 
								  value=any:create(orber_tc:float(), 0.00078431137)},
				      #'CosNotification_Property'{name="filesize", 
								  value=any:create(orber_tc:float(), 500000)}]))),
    ?match(false, eval(T5, any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
					[#'CosNotification_Property'{name="memsize", 
								     value=any:create(orber_tc:float(), 5.5)},
					 #'CosNotification_Property'{name="cputime", 
								     value=any:create(orber_tc:float(), 0.00078431137)},
					 #'CosNotification_Property'{name="filesize", 
								     value=any:create(orber_tc:float(), 500)}]))),
    ?match({error, _}, eval(T5, any:create(orber_tc:sequence('CosNotification_Property':tc(),0),
					   [#'CosNotification_Property'{name="memsize", 
									value=any:create(orber_tc:float(), 5.5)},
					    #'CosNotification_Property'{name="filesize", 
									value=any:create(orber_tc:float(), 500)}]))),

    %% Accept events where a threshold has the unscoped type name 'data'. 
    {ok,T6} = ?match({ok, _}, create_filter("exist $threshold._type_id and $threshold._type_id == 'data'")),
    ?match(true, eval(T6, any:create(orber_tc:alias(notify_test_data:id(),
						    "threshold",
						    notify_test_data:tc()), 
				     #notify_test_data{score = 10, name = "Erlang"}))),
    


    ?match(true, eval(T6, ?not_CreateSE("DomainName","TypeName",
					"EventName", [],
					[#'CosNotification_Property'
					 {name="threshold", 
					  value=any:create(notify_test_data:tc(),
							   #notify_test_data
							   {score = 10, 
							    name  = "Erlang"})}],
					any:create(orber_tc:null(), null)))),


    ?match(true, eval(T6, ?not_CreateSE("DomainName","TypeName",
					"EventName", [],
					[#'CosNotification_Property'
					 {name="NotThreshold", 
					  value=any:create(notify_test_data:tc(),
							   #notify_test_data
							   {score = 10, 
							    name  = "Erlang"})}],
					any:create(orber_tc:alias(notify_test_data:id(),
								  "threshold",
								  notify_test_data:tc()), 
						   #notify_test_data{score = 10, name = "Erlang"})))),



    %% Accept events with a serviceUser property of the correct standard type. 
    {ok,T7} = ?match({ok, _}, create_filter("$violation(TestData)._repos_id == 'IDL:notify_test/data:1.0'")),
    ?match(true, eval(T7, ?not_CreateSE("DomainName","TypeName",
					"EventName", [],
					[#'CosNotification_Property'
					 {name="violation", 
					  value=any:create(orber_tc:array('CosNotification_Property':tc(),0),
							   [#'CosNotification_Property'
							    {name="TestData",
							     value=any:create(notify_test_data:tc(),
									      #notify_test_data
									      {score=100,
									       name="perfect score"})}])}],
					any:create(orber_tc:null(), null)))),
    
    {ok,T8} = ?match({ok, _}, create_filter("$type_name == 'CommunicationsAlarm' and $event_name == 'lost_packet' and $priority < 2")),
    %% All correct
    Event1 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			   "lost_packet",
			   [#'CosNotification_Property'{name="priority", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    %% Priority to high
    Event2 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packet",
			   [#'CosNotification_Property'{name="priority", 
							value=any:create(orber_tc:short(), 2)}],
			   [], any:create(orber_tc:null(), null)),
    %% Misspell event_name, i.e., lost_packets instead of lost_packet
    Event3 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packets",
			   [#'CosNotification_Property'{name="priority", 
							value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),
    %% Another type_name
    Event4 = ?not_CreateSE("DomainName","TemperatureAlarm",
			   "lost_packets",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    
    ?match(true,  eval(T8, Event1)),
    ?match(false, eval(T8, Event2)),
    ?match(false, eval(T8, Event3)),
    ?match(false, eval(T8, Event4)),
    
    {ok,T9} = ?match({ok, _}, create_filter("$gpa < 80 or $tests(midterm) > $tests(final) or $monthly_attendance[3] < 10")),

    %% midterm > final yields true, the others false
    Event5 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [],
			   [#'CosNotification_Property'
			    {name="tests",
			     value=any:create(orber_tc:array('CosNotification_Property':tc(),0),
						 [#'CosNotification_Property'{name="midterm",
							value=any:create(orber_tc:short(), 70)},
						  #'CosNotification_Property'{name="final",
									      value=any:create(orber_tc:short(), 60)}])},
			    #'CosNotification_Property'{name="monthly_attendance", 
							value=any:create(orber_tc:array(orber_tc:short(), 0),
									 {0,1,2,10})},
			    #'CosNotification_Property'{name="gpa", 
							value=any:create(orber_tc:short(), 90)}],
			   any:create(orber_tc:null(), null)),
    
    %% monthly_attendance[3] < 10 yields true, the others false
    Event6 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [],
			   [#'CosNotification_Property'{name="tests",
					value=any:create(orber_tc:array('CosNotification_Property':tc(),0),
						 [#'CosNotification_Property'{name="midterm",
							value=any:create(orber_tc:short(), 70)},
						  #'CosNotification_Property'{name="final",
									      value=any:create(orber_tc:short(), 80)}])},
			    #'CosNotification_Property'{name="monthly_attendance", 
							value=any:create(orber_tc:array(orber_tc:short(), 0),
									 {0,1,2,9})},
			    #'CosNotification_Property'{name="gpa", 
							value=any:create(orber_tc:short(), 90)}],
			   any:create(orber_tc:null(), null)),

    %% gpa < 80 true, rest false.
    Event7 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [],
			   [#'CosNotification_Property'{name="tests",
					value=any:create(orber_tc:array('CosNotification_Property':tc(),0),
						 [#'CosNotification_Property'{name="midterm",
							value=any:create(orber_tc:short(), 70)},
						  #'CosNotification_Property'{name="final",
									      value=any:create(orber_tc:short(), 80)}])},
			    #'CosNotification_Property'{name="monthly_attendance", 
							value=any:create(orber_tc:array(orber_tc:short(), 0),
									 {0,1,2,10})},
			    #'CosNotification_Property'{name="gpa", 
							value=any:create(orber_tc:short(), 70)}],
			   any:create(orber_tc:null(), null)),

    %% All false
    Event8 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [],
			   [#'CosNotification_Property'{name="tests",
					value=any:create(orber_tc:array('CosNotification_Property':tc(),0),
						 [#'CosNotification_Property'{name="midterm",
							value=any:create(orber_tc:short(), 70)},
						  #'CosNotification_Property'{name="final",
									      value=any:create(orber_tc:short(), 80)}])},
			    #'CosNotification_Property'{name="monthly_attendance", 
							value=any:create(orber_tc:array(orber_tc:short(), 0),
									 {0,1,2,10})},
			    #'CosNotification_Property'{name="gpa", 
							value=any:create(orber_tc:short(), 80)}],
			   any:create(orber_tc:null(), null)),

    ?match(true,  eval(T9, Event5)),
    ?match(true,  eval(T9, Event6)),
    ?match(true,  eval(T9, Event7)),
    ?match(false, eval(T9, Event8)),
    ok.

%%-----------------------------------------------------------------
%%  Misc grammar tests
%%-----------------------------------------------------------------
positional_api(_Config) ->
    {ok,T1} = ?match({ok, _}, create_filter("$.3 < 80 or $.1(midterm) > $.1(final) or $.2[3] < 10")),

    %% midterm > final yields true, the others false
    Event1 = any:create(notify_test_studies:tc(), #notify_test_studies
			{gpa = 90,
			 tests = [#'CosNotification_Property'
				  {name="midterm", value=any:create(orber_tc:short(), 70)},
				  #'CosNotification_Property'
				  {name="final", value=any:create(orber_tc:short(), 60)}],
			 monthly_attendance = {0,1,2,10}}),
    %% monthly_attendance[3] < 10 yields true, the others false
    Event2 = any:create(notify_test_studies:tc(), #notify_test_studies
			{gpa = 90,
			 tests = [#'CosNotification_Property'
				  {name="midterm", value=any:create(orber_tc:short(), 70)},
				  #'CosNotification_Property'
				  {name="final", value=any:create(orber_tc:short(), 80)}],
			 monthly_attendance = {0,1,2,9}}),
    %% gpa < 80 true, rest false.
    Event3 = any:create(notify_test_studies:tc(), #notify_test_studies
			{gpa = 70,
			 tests = [#'CosNotification_Property'
				  {name="midterm", value=any:create(orber_tc:short(), 70)},
				  #'CosNotification_Property'
				  {name="final", value=any:create(orber_tc:short(), 80)}],
			 monthly_attendance = {0,1,2,10}}),
    %% All false
    Event4 = any:create(notify_test_studies:tc(), #notify_test_studies
			{gpa = 80,
			 tests = [#'CosNotification_Property'
				  {name="midterm", value=any:create(orber_tc:short(), 70)},
				  #'CosNotification_Property'
				  {name="final", value=any:create(orber_tc:short(), 80)}],
			 monthly_attendance = {0,1,2,10}}),

    ?match(true,  eval(T1, Event1)),
    ?match(true,  eval(T1, Event2)),
    ?match(true,  eval(T1, Event3)),
    ?match(false, eval(T1, Event4)),

    {ok,T2} = ?match({ok, _}, create_filter("$.0.0.0.1 == 'CommunicationsAlarm'")),

    Event5 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			   "lost_packet", [], [], 
			   any:create(orber_tc:null(), null)),

    ?match(true,  eval(T2, Event5)),
    
    ok.

%%-----------------------------------------------------------------
%%  Components grammar tests
%%-----------------------------------------------------------------
components_api(_Config) ->
    {ok,T1}  = ?match({ok, _}, create_filter("$ == 2")),
    ?match(true, eval(T1, ?not_CreateSE("DomainName","TypeName","EventName",
					[],[], any:create(orber_tc:short(), 2)))),
    ?match(true, eval(T1, any:create(orber_tc:short(), 2))),
    ?match(false, eval(T1, ?not_CreateSE("DomainName","TypeName","EventName",
					 [],[], any:create(orber_tc:short(), 3)))),
    ?match(false, eval(T1, any:create(orber_tc:short(), 3))),

    %% Select "MOVIE" events featuring at least 3 of the Marx Brothers. 
    {ok,T2} = ?match({ok, _}, create_filter("$type_name == 'MOVIE' and (('groucho' in $.starlist) + ('chico' in $.starlist) + ('harpo' in $.starlist) + ('zeppo' in $.starlist) + ('gummo' in $.starlist)) > 2")),
    ?match(true, eval(T2, ?not_CreateSE("DomainName","MOVIE", "EventName", [], [],
					any:create(orber_tc:alias("IFRId","starlist",tk_any),
						   any:create(orber_tc:sequence(orber_tc:string(0),0),
							      ["groucho", "harpo", "sam", "gummo"]))))),
    ?match(false, eval(T2, ?not_CreateSE("DomainName","MOVIE", "EventName", [], [], 
					any:create(orber_tc:alias("IFRId","starlist",tk_any),
						   any:create(orber_tc:sequence(orber_tc:string(0),0),
							      ["frodo", "bilbo", "sam", "gummo"]))))),

    %% Accept only recent events (e.g., generated within the last 15 minutes or so). 
    {ok,_T3} = ?match({ok, _}, create_filter("$origination_timestamp.high + 2 < $curtime.high")),


    %% Accept students that took all 3 tests and had an average score of at least 80%. 
    {ok,T4} = ?match({ok, _}, create_filter("$.test._length == 3 and ($.test[0].score + $.test[1].score + $.test[2].score)/3 >=80")),
    ?match(true, eval(T4, ?not_CreateSE("DomainName","TypeName", "EventName", [], [],
					any:create(orber_tc:alias("IFRId","test",tk_any),
						   any:create(orber_tc:array(notify_test_data:tc(),0),
							      {#notify_test_data{score=75},
							       #notify_test_data{score=80},
							       #notify_test_data{score=85}}))))),
    ?match(false, eval(T4, ?not_CreateSE("DomainName","TypeName", "EventName", [], [],
					 any:create(orber_tc:alias("IFRId","test",tk_any),
						    any:create(orber_tc:array(notify_test_data:tc(),0),
							       {#notify_test_data{score=75},
								#notify_test_data{score=80},
								#notify_test_data{score=80}}))))),
    ?match(false, eval(T4, ?not_CreateSE("DomainName","TypeName", "EventName", [], [],
					 any:create(orber_tc:alias("IFRId","test",tk_any),
						    any:create(orber_tc:array(notify_test_data:tc(),0),
							       {#notify_test_data{score=75},
								#notify_test_data{score=80}}))))),

    %% Select processes that exceed a certain usage threshold. 
    {ok,T5} = ?match({ok, _}, create_filter("$.memsize / 5.5 + $.cputime * 1275.0 + $.filesize * 1.25 > 500000.0")),
    ?match(true, eval(T5, ?not_CreateSE("DomainName","TypeName", "EventName", [], [],
					any:create(notify_test_computer:tc(),
						   #notify_test_computer
						   {memsize=5.5,
						    cputime = 0.00078431137, 
						    filesize = 500000})))),
    ?match(false, eval(T5, ?not_CreateSE("DomainName","TypeName", "EventName", [], [],
					 any:create(notify_test_computer:tc(),
						    #notify_test_computer
						    {memsize=5.5,
						     cputime = 0.00078431137, 
						     filesize = 500})))),
    ?match({error,_}, eval(T5, ?not_CreateSE("DomainName","TypeName", "EventName", [], [],
					     any:create(notify_test_computer:tc(),
							#notify_test_computer
							{memsize=5.5,
							 cputime = 0.00078431137})))),

    %% Accept only Notification Service structured events. 
    {ok,T6} = ?match({ok, _}, create_filter("$._repos_id == 'IDL:omg.org/CosNotification/StructuredEvent:1.0'")),
    ?match(true, eval(T6, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"EventName",
					[], [], any:create(orber_tc:null(), null)))),

   

    %% Accept only those events that have a specified security "rights list". 
    {ok,T7} = ?match({ok, _}, create_filter("exist $.header.variable_header(required_rights)")),
    ?match(false, eval(T7, ?not_CreateSE("DomainName","CommunicationsAlarm",
					 "lost_packet",
					 [#'CosNotification_Property'{name="priority", 
								      value=any:create(orber_tc:short(), 1)}],
					 [], any:create(orber_tc:null(), null)))),
    ?match(true, eval(T7, ?not_CreateSE("DomainName","CommunicationsAlarm",
					"lost_packet",
					[#'CosNotification_Property'{name="required_rights", 
								     value=any:create(orber_tc:short(), 1)}],
					[], any:create(orber_tc:null(), null)))),


    {ok,T8} = ?match({ok, _}, create_filter("$.header.fixed_header.event_type.type_name == 'CommunicationsAlarm' and $.header.fixed_header.event_name == 'lost_packet' and $.header.variable_header(priority) < 2")),
    %% All correct
    Event1 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packet",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),
    %% Priority to high
    Event2 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packet",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 2)}],
			  [], any:create(orber_tc:null(), null)),
    %% Misspell event_name, i.e., lost_packets instead of lost_packet
    Event3 = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packets",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),
    %% Another type_name
    Event4 = ?not_CreateSE("DomainName","TemperatureAlarm",
			  "lost_packets",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),

    ?match(true,  eval(T8, Event1)),
    ?match(false, eval(T8, Event2)),
    ?match(false, eval(T8, Event3)),
    ?match(false, eval(T8, Event4)),
   

    {ok,T9} = ?match({ok, _}, create_filter("$.gpa < 80 or $.tests(midterm) > $.tests(final) or $.monthly_attendance[3] < 10")),

    %% midterm > final yields true, the others false
    Event5 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [], [],
			    any:create(notify_test_studies:tc(), #notify_test_studies
				       {gpa = 90,
					tests = [#'CosNotification_Property'
						 {name="midterm", value=any:create(orber_tc:short(), 70)},
						 #'CosNotification_Property'
						 {name="final", value=any:create(orber_tc:short(), 60)}],
					monthly_attendance = {0,1,2,10}})),
    %% monthly_attendance[3] < 10 yields true, the others false
    Event6 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [], [],
			    any:create(notify_test_studies:tc(), #notify_test_studies
				       {gpa = 90,
					tests = [#'CosNotification_Property'
						 {name="midterm", value=any:create(orber_tc:short(), 70)},
						 #'CosNotification_Property'
						 {name="final", value=any:create(orber_tc:short(), 80)}],
					monthly_attendance = {0,1,2,9}})),
    %% gpa < 80 true, rest false.
    Event7 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [], [],
			    any:create(notify_test_studies:tc(), #notify_test_studies
				       {gpa = 70,
					tests = [#'CosNotification_Property'
						 {name="midterm", value=any:create(orber_tc:short(), 70)},
						 #'CosNotification_Property'
						 {name="final", value=any:create(orber_tc:short(), 80)}],
					monthly_attendance = {0,1,2,10}})),
    %% All false
    Event8 = ?not_CreateSE("DomainName","TypeName",
			   "EventName", [], [],
			    any:create(notify_test_studies:tc(), #notify_test_studies
				       {gpa = 80,
					tests = [#'CosNotification_Property'
						 {name="midterm", value=any:create(orber_tc:short(), 70)},
						 #'CosNotification_Property'
						 {name="final", value=any:create(orber_tc:short(), 80)}],
					monthly_attendance = {0,1,2,10}})),

    ?match(true,  eval(T9, Event5)),
    ?match(true,  eval(T9, Event6)),
    ?match(true,  eval(T9, Event7)),
    ?match(false, eval(T9, Event8)),
    ok.

 
%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-------------------- End of Module ------------------------------
