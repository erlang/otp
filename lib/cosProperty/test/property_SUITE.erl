%%----------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File        : property_SUITE.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module(property_SUITE).

 
%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("cosProperty/src/cosProperty.hrl").
-include_lib("cosProperty/include/CosPropertyService.hrl").

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
 
-define(match_inverse(NotExpectedRes, Expr),
        fun() ->
                AcTuAlReS = (catch (Expr)),
               case AcTuAlReS of
                   NotExpectedRes ->
                       io:format("###### ERROR ERROR ######~n ~p~n",
                                 [AcTuAlReS]),
                       exit(AcTuAlReS);
                   _ ->
                       io:format("------ CORRECT RESULT ------~n~p~n",
                                 [AcTuAlReS]),
                       AcTuAlReS
               end
       end()).
 

-define(val1, #any{typecode=tk_short, value=1}).
-define(val2, #any{typecode=tk_short, value=2}).
-define(val3, #any{typecode=tk_short, value=3}).
-define(val4, #any{typecode=tk_short, value=4}).
-define(val5, #any{typecode=tk_long, value=5}).
-define(badval, #any{typecode=tk_shirt, value=5}).

-define(id1, "id1").
-define(id2, "id2").
-define(id3, "id3").
-define(id4, "id4").
-define(id5, "id5").
-define(badid, "").

 
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
%% Fixed exports
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, cases/0, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([create_setdef_api/1, create_set_api/1, define_with_mode_api/1,
	 define_api/1, names_iterator_api/1, properties_iterator_api/1,
	 app_test/1]).
 
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
    [create_setdef_api, create_set_api,
     define_with_mode_api, define_api, names_iterator_api,
     properties_iterator_api, app_test].
 
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
    orber:jump_start(),
    cosProperty:install(),
    cosProperty:install_db(),
    ?match(ok, application:start(cosProperty)),
    if
        is_list(Config) ->
            Config;
        true ->
            exit("Config not a list")
    end.
 
end_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    application:stop(cosProperty),
    cosProperty:uninstall_db(),
    cosProperty:uninstall(),
    orber:jump_stop(),
    Config.

%%-----------------------------------------------------------------
%%  Tests app file
%%-----------------------------------------------------------------
app_test(_Config) ->
    ok=test_server:app_test(cosProperty),
    ok.


%%-----------------------------------------------------------------
%%  CosPropertyService_PropertySetDefFactory API tests 
%%-----------------------------------------------------------------
create_setdef_api(_Config) ->

    ValidDefs = [#'CosPropertyService_PropertyDef'
		 {property_name = ?id1,
		  property_value = ?val1,
		  property_mode = normal},
		 #'CosPropertyService_PropertyDef'
		 {property_name = ?id2,
		  property_value = ?val2,
		  property_mode = normal}],
    InvalidDefs = [#'CosPropertyService_PropertyDef'
		   {property_name = ?id1,
		    property_value = ?val1,
		    property_mode = normal},
		   #'CosPropertyService_PropertyDef'
		   {property_name = ?badid,
		    property_value = ?badval,
		    property_mode = normal}],
    
    Fac = ?match({_,pseudo,_,_,_,_}, cosProperty:start_SetDefFactory()),

    Obj1 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetDefFactory':
		  create_propertysetdef(Fac)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    corba:dispose(Obj1),


    Obj2 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetDefFactory':
		  create_constrained_propertysetdef(Fac, [tk_short], ValidDefs)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    corba:dispose(Obj2),

    %% Both arguments correct but 'ValidDefs' contain other TC:s than
    %% tk_null.
    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetDefFactory':
	   create_constrained_propertysetdef(Fac, [tk_null], ValidDefs)),
    'CosPropertyService_PropertySetDef_impl':dump(),

    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetDefFactory':
	   create_constrained_propertysetdef(Fac, [tk_null], InvalidDefs)),
    'CosPropertyService_PropertySetDef_impl':dump(),

    %% The allowed TC not supported.
    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetDefFactory':
	   create_constrained_propertysetdef(Fac, [tk_noll], ValidDefs)),
    'CosPropertyService_PropertySetDef_impl':dump(),

    Obj4 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetDefFactory':
		  create_initial_propertysetdef(Fac, ValidDefs)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    corba:dispose(Obj4),

    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetDefFactory':
	   create_initial_propertysetdef(Fac, InvalidDefs)),

    ?match(ok, cosProperty:stop_SetDefFactory(Fac)),

    ok.


%%-----------------------------------------------------------------
%%  CosPropertyService_PropertySetFactory API tests 
%%-----------------------------------------------------------------
create_set_api(_Config) ->
    Valid = [#'CosPropertyService_Property'
	     {property_name = ?id1,
	      property_value = ?val1},
	     #'CosPropertyService_Property'
	     {property_name = ?id2,
	      property_value = ?val2}],
    Invalid = [#'CosPropertyService_Property'
	       {property_name = ?id1,
		property_value = ?val1},
	       #'CosPropertyService_Property'
	       {property_name = ?badid,
		property_value = ?badval}],
    
    Fac = ?match({_,pseudo,_,_,_,_}, cosProperty:start_SetFactory()),
    Obj1 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		  create_propertyset(Fac)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    corba:dispose(Obj1),
    

    Obj2 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		  create_constrained_propertyset(Fac, [tk_short], Valid)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    corba:dispose(Obj2),

    %% Both arguments correct but 'Valid' contain other TC:s than
    %% tk_null.
    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetFactory':
	   create_constrained_propertyset(Fac, [tk_null], Valid)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    
    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetFactory':
	   create_constrained_propertyset(Fac, [tk_null], Invalid)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    
    %% The allowed TC not supported.
    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetFactory':
	   create_constrained_propertyset(Fac, [tk_noll], Valid)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    
    Obj4 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		  create_initial_propertyset(Fac, Valid)),
    'CosPropertyService_PropertySetDef_impl':dump(),
    corba:dispose(Obj4),

    ?match({'EXCEPTION', _}, 'CosPropertyService_PropertySetFactory':
	   create_initial_propertyset(Fac, Invalid)),
    ?match(ok, cosProperty:stop_SetFactory(Fac)),
    ok.

%%-----------------------------------------------------------------
%%  CosPropertyService_PropertySetDef API tests 
%%-----------------------------------------------------------------
define_api(_Config) ->
    ValidDefs = [#'CosPropertyService_Property'
		 {property_name = ?id1,
		  property_value = ?val1},
		 #'CosPropertyService_Property'
		 {property_name = ?id2,
		  property_value = ?val2},
		 #'CosPropertyService_Property'
		 {property_name = ?id3,
		  property_value = ?val3}],

    Fac = ?match({_,pseudo,_,_,_,_}, cosProperty:start_SetFactory()),

    io:format("@@@@ Testing PropertySet returned by the factory operation create_propertyset/1 @@@@", []),
    Obj = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		 create_propertyset(Fac)),
    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj, ?id1, ?val1)),
    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj, ?id1, ?val1)),
    
    ?match(1, 'CosPropertyService_PropertySet':get_number_of_properties(Obj)),
    ?match(ok, 'CosPropertyService_PropertySet':
	   define_properties(Obj, [#'CosPropertyService_Property'{property_name = ?id2, 
								  property_value = ?val2},
				   #'CosPropertyService_Property'{property_name = ?id3, 
								  property_value = ?val3}])),

    ?match(3, 'CosPropertyService_PropertySet':get_number_of_properties(Obj)),

    ?match({true, [_]}, 'CosPropertyService_PropertySet':get_properties(Obj, [?id1])),
    ?match({true, [_, _, _]}, 'CosPropertyService_PropertySet':get_properties(Obj, [?id1, ?id2, ?id3])),
    ?match({false,[_, _, _]}, 'CosPropertyService_PropertySet':get_properties(Obj, [?id1, "wrong", ?id3])),

    ?match(?val2, 'CosPropertyService_PropertySet':get_property_value(Obj, ?id2)),
    ?match(ok, 'CosPropertyService_PropertySet':delete_property(Obj, ?id1)),
    
    ?match(2, 'CosPropertyService_PropertySet':get_number_of_properties(Obj)),

    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj, ?id1, ?val1)),
    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj, ?id2, ?val2)),
    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj, ?id3, ?val3)),

    ?match(true, 'CosPropertyService_PropertySet':delete_all_properties(Obj)),
    ?match(0, 'CosPropertyService_PropertySet':get_number_of_properties(Obj)),
    
    ?match(ok, 'CosPropertyService_PropertySet':
	   define_properties(Obj, [#'CosPropertyService_Property'{property_name = ?id1, 
								  property_value = ?val1},
				   #'CosPropertyService_Property'{property_name = ?id2, 
								  property_value = ?val2},
				   #'CosPropertyService_Property'{property_name = ?id3, 
								  property_value = ?val3}])),

    ?match(3, 'CosPropertyService_PropertySet':get_number_of_properties(Obj)),
    ?match(?val2, 'CosPropertyService_PropertySet':get_property_value(Obj, ?id2)),

    ?match({'EXCEPTION',{'CosPropertyService_PropertyNotFound',_}}, 
	   'CosPropertyService_PropertySet':get_property_value(Obj, "wrongID")),
    ?match({'EXCEPTION',{'CosPropertyService_InvalidPropertyName',_}}, 
	   'CosPropertyService_PropertySet':get_property_value(Obj, "")),
    ?match({'EXCEPTION',{'CosPropertyService_InvalidPropertyName',_}}, 
	   'CosPropertyService_PropertySet':is_property_defined(Obj, "")),
    ?match(false, 'CosPropertyService_PropertySet':is_property_defined(Obj, "wrongID")),
    ?match(true, 'CosPropertyService_PropertySet':is_property_defined(Obj, ?id1)),

    %% This function is not supported by PropertySet.
    ?match({'EXCEPTION',{'NO_IMPLEMENT',_,_,_}}, 
	   'CosPropertyService_PropertySetDef':get_property_modes(Obj, [?id1, ?id2, ?id3])),

    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySet':delete_properties(Obj, [?id1, ?id2, ?id3, "wrongID"])),
    ?match(0, 'CosPropertyService_PropertySet':get_number_of_properties(Obj)),
    corba:dispose(Obj),

   io:format("@@@@ Testing PropertySet returned by the factory operation create_constrained_propertyset/3 @@@@", []),
    Obj2 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		 create_constrained_propertyset(Fac, [tk_short], ValidDefs)),

    ?match(0, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),
    ?match({'EXCEPTION', {'CosPropertyService_UnsupportedProperty',_}}, 
	   'CosPropertyService_PropertySet':define_property(Obj2, ?id4, ?val4)),
    ?match({'EXCEPTION', {'CosPropertyService_UnsupportedTypeCode',_}}, 
	   'CosPropertyService_PropertySet':define_property(Obj2, ?id1, ?val5)),
    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj2, ?id1, ?val1)),
    ?match(1, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),
    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}},
	   'CosPropertyService_PropertySet':
	   define_properties(Obj2, [#'CosPropertyService_Property'{property_name = ?id2, 
								   property_value = ?val2},
				    #'CosPropertyService_Property'{property_name = ?id3, 
								   property_value = ?val3},
				    #'CosPropertyService_Property'{property_name = "wrongId", 
								   property_value = ?val2}])),
    ?match(ok,'CosPropertyService_PropertySet':
	   define_properties(Obj2, [#'CosPropertyService_Property'{property_name = ?id2, 
								   property_value = ?val2},
				    #'CosPropertyService_Property'{property_name = ?id3, 
								   property_value = ?val3}])),
    ?match(3, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),
    ?match({'EXCEPTION',{'CosPropertyService_PropertyNotFound',_}}, 
	   'CosPropertyService_PropertySet':get_property_value(Obj2, "wrongID")),
    ?match(?val2, 'CosPropertyService_PropertySet':get_property_value(Obj2, ?id2)),
    ?match({'EXCEPTION',{'CosPropertyService_InvalidPropertyName',_}}, 
	   'CosPropertyService_PropertySet':get_property_value(Obj2, "")),
    ?match({'EXCEPTION',{'CosPropertyService_InvalidPropertyName',_}}, 
	   'CosPropertyService_PropertySet':is_property_defined(Obj2, "")),
    ?match(false, 'CosPropertyService_PropertySet':is_property_defined(Obj2, "wrongID")),
    ?match(true, 'CosPropertyService_PropertySet':is_property_defined(Obj2, ?id1)),

    
    ?match({'EXCEPTION',{'CosPropertyService_PropertyNotFound',_}}, 
	   'CosPropertyService_PropertySet':delete_property(Obj2, "wrongID")),
    ?match(3, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),
    ?match(ok, 'CosPropertyService_PropertySet':delete_property(Obj2, ?id1)),
    ?match(2, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),

    ?match(ok, 'CosPropertyService_PropertySet':delete_properties(Obj2, [?id2])),
    ?match(1, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),

    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySet':delete_properties(Obj2, [?id3, "wrongID"])),
    ?match(0, 'CosPropertyService_PropertySet':get_number_of_properties(Obj2)),
    corba:dispose(Obj2),
    
    io:format("@@@@ Testing PropertySet returned by the factory operation create_initial_propertyset/2 @@@@", []),
    Obj3 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		 create_initial_propertyset(Fac, ValidDefs)),
    ?match(3, 'CosPropertyService_PropertySet':get_number_of_properties(Obj3)),

    ?match(ok, 'CosPropertyService_PropertySet':define_property(Obj3, ?id4, ?val4)),
    ?match(4, 'CosPropertyService_PropertySet':get_number_of_properties(Obj3)),

    ?match(ok,'CosPropertyService_PropertySet':
	   define_properties(Obj3, [#'CosPropertyService_Property'{property_name = ?id5, 
								   property_value = ?val5}])),

    ?match(5, 'CosPropertyService_PropertySet':get_number_of_properties(Obj3)),

    ?match({'EXCEPTION',{'CosPropertyService_PropertyNotFound',_}}, 
	   'CosPropertyService_PropertySet':get_property_value(Obj3, "wrongID")),
    ?match(?val2, 'CosPropertyService_PropertySet':get_property_value(Obj3, ?id2)),
    ?match({'EXCEPTION',{'CosPropertyService_InvalidPropertyName',_}}, 
	   'CosPropertyService_PropertySet':get_property_value(Obj3, "")),
    ?match({'EXCEPTION',{'CosPropertyService_InvalidPropertyName',_}}, 
	   'CosPropertyService_PropertySet':is_property_defined(Obj3, "")),
    ?match(false, 'CosPropertyService_PropertySet':is_property_defined(Obj3, "wrongID")),
    ?match(true, 'CosPropertyService_PropertySet':is_property_defined(Obj3, ?id1)),

    ?match({'EXCEPTION',{'CosPropertyService_PropertyNotFound',_}}, 
	   'CosPropertyService_PropertySet':delete_property(Obj3, "wrongId")),
    ?match(ok, 'CosPropertyService_PropertySet':delete_property(Obj3, ?id5)),
    ?match(4, 'CosPropertyService_PropertySet':get_number_of_properties(Obj3)),

    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySet':delete_properties(Obj3, [?id1, ?id2, ?id3, "wrongID"])),
    ?match(1, 'CosPropertyService_PropertySet':get_number_of_properties(Obj3)),

    ?match(true, 'CosPropertyService_PropertySet':delete_all_properties(Obj3)),
    ?match(0, 'CosPropertyService_PropertySet':get_number_of_properties(Obj3)),

    corba:dispose(Obj3),
    ?match(ok, cosProperty:stop_SetFactory(Fac)),

    ok.

%%-----------------------------------------------------------------
%%  CosPropertyService_PropertySetDef API tests 
%%-----------------------------------------------------------------
define_with_mode_api(_Config) ->
    ValidDefs = [#'CosPropertyService_PropertyDef'
		 {property_name = ?id1,
		  property_value = ?val1,
		  property_mode = normal},
		 #'CosPropertyService_PropertyDef'
		 {property_name = ?id2,
		  property_value = ?val2,
		  property_mode = normal},
		 #'CosPropertyService_PropertyDef'
		 {property_name = ?id3,
		  property_value = ?val3,
		  property_mode = normal}],

    Fac = ?match({_,pseudo,_,_,_,_}, cosProperty:start_SetDefFactory()),

    io:format("@@@@ Testing PropertySetDef returned by the factory operation create_propertysetdef/1 @@@@", []),
    Obj = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetDefFactory':
		 create_propertysetdef(Fac)),

    %% Initally no prop's created and no restrictions at all
    ?match(0, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj)),
    ?match({ok, []}, 'CosPropertyService_PropertySetDef':get_allowed_property_types(Obj)),
    ?match({ok, []}, 'CosPropertyService_PropertySetDef':get_allowed_properties(Obj)),

    %% Add two properties.
    ?match(ok, 'CosPropertyService_PropertySetDef':define_property_with_mode(Obj, ?id4, ?val4, read_only)),
    ?match(ok, 'CosPropertyService_PropertySetDef':define_property_with_mode(Obj, ?id5, ?val5, normal)),
    %% Try to add the same property again (shouldn't add another since using the sam Id).
    ?match(ok, 'CosPropertyService_PropertySetDef':define_property_with_mode(Obj, ?id5, ?val5, normal)),

    %% Try to add another identical proprty with wrong TC.
    ?match({'EXCEPTION',{'CosPropertyService_ConflictingProperty',_}}, 
	   'CosPropertyService_PropertySetDef':define_property_with_mode(Obj, ?id5, ?val4, normal)),


    %% Should be two now.
    ?match(2, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj)),

    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj, ?id4)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj, ?id5)),
    ?match(ok, 'CosPropertyService_PropertySetDef':
	   define_properties_with_modes(Obj, 
					[#'CosPropertyService_PropertyDef'{property_name = ?id1, 
									   property_value = ?val1,
									   property_mode = normal},
					 #'CosPropertyService_PropertyDef'{property_name = ?id2, 
									   property_value = ?val2,
									   property_mode = normal},
					 #'CosPropertyService_PropertyDef'{property_name = ?id3, 
									   property_value = ?val3,
									  property_mode = normal}])),
    %% Should be five now.
    ?match(5, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj)),
    ?match({true, [_,_]}, 'CosPropertyService_PropertySetDef':get_property_modes(Obj, [?id1, ?id3])),
    ?match({false, [_,_,_]}, 'CosPropertyService_PropertySetDef':get_property_modes(Obj, [?id1, ?id3, "wrongID"])),

    ?match(ok, 'CosPropertyService_PropertySetDef':set_property_mode(Obj, ?id1, read_only)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj, ?id1)),

    ?match({'EXCEPTION',{'CosPropertyService_PropertyNotFound',_}}, 
	   'CosPropertyService_PropertySetDef':set_property_mode(Obj, "wrongID", read_only)),

    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySetDef':
	   set_property_modes(Obj, 
			      [#'CosPropertyService_PropertyMode'{property_name = ?id2, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = ?id3, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = "wrongID", 
								  property_mode = read_only}])),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj, ?id2)),
    ?match(ok, 
	   'CosPropertyService_PropertySetDef':
	   set_property_modes(Obj, 
			      [#'CosPropertyService_PropertyMode'{property_name = ?id2, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = ?id3, 
								  property_mode = read_only}])),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj, ?id2)),

    corba:dispose(Obj),


    io:format("@@@@ Testing PropertySetDef returned by the factory operation create_constrained_propertysetdef/3 @@@@", []),
    Obj2 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetDefFactory':
		  create_constrained_propertysetdef(Fac, [tk_short], ValidDefs)),

    %% Initally no prop's created and the restrictions that only Properties eq. to ValidDefs
    %% may be handled.
    ?match(0, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj2)),
    ?match({ok, [tk_short]}, 'CosPropertyService_PropertySetDef':get_allowed_property_types(Obj2)),
    %% We cannot be sure in which order it's returned. Hmm, that's not really true but it
    %% may change in the future.
    ?match({ok, [_,_,_]}, 'CosPropertyService_PropertySetDef':get_allowed_properties(Obj2)),
    %% Try to add a Property with and Id not eq. to ?id1, ?id2 or ?id3; must fail.
    ?match({'EXCEPTION', {'CosPropertyService_UnsupportedProperty',_}}, 
	   'CosPropertyService_PropertySetDef':define_property_with_mode(Obj2, ?id4, ?val4, read_only)),
    %% To be sure that nothing was updated.
    ?match(0, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj2)),
    %% Add a valid Property.
    ?match(ok, 'CosPropertyService_PropertySetDef':define_property_with_mode(Obj2, ?id1, ?val1, normal)),
    ?match(1, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj2)),
    %% Add a sequence of 1 valid and one invalid Prop's
    ?match({'EXCEPTION', {'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySetDef':
	   define_properties_with_modes(Obj2, 
					[#'CosPropertyService_PropertyDef'{property_name = ?id2, 
									   property_value = ?val2,
									   property_mode = normal},
					 #'CosPropertyService_PropertyDef'{property_name = "wrongID", 
									   property_value = ?val2,
									  property_mode = normal}])),
    %% One should be added.
    ?match(1, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj2)),
    ?match(ok, 'CosPropertyService_PropertySetDef':
	   define_properties_with_modes(Obj2, 
					[#'CosPropertyService_PropertyDef'{property_name = ?id3, 
									   property_value = ?val3,
									   property_mode = normal}])),
    %% Add a sequence of 1 valid Prop.
    ?match(2, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj2)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id1)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id3)),


    ?match({true, [_,_]}, 'CosPropertyService_PropertySetDef':get_property_modes(Obj2, [?id1, ?id3])),
    ?match({false, [_,_,_]}, 'CosPropertyService_PropertySetDef':get_property_modes(Obj2, [?id1, ?id3, "wrongID"])),

    ?match(ok, 'CosPropertyService_PropertySetDef':set_property_mode(Obj2, ?id1, read_only)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id1)),
    ?match(ok, 'CosPropertyService_PropertySetDef':
	   set_property_modes(Obj2, 
			      [#'CosPropertyService_PropertyMode'{property_name = ?id1, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = ?id3, 
								  property_mode = read_only}])),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id1)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id3)),
    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySetDef':
	   set_property_modes(Obj2, 
			      [#'CosPropertyService_PropertyMode'{property_name = ?id1, 
								  property_mode = normal},
			       #'CosPropertyService_PropertyMode'{property_name = ?id3, 
								  property_mode = normal},
			       #'CosPropertyService_PropertyMode'{property_name = "wrongID", 
								  property_mode = normal}])),

    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id1)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj2, ?id3)),
    corba:dispose(Obj2),

    io:format("@@@@ Testing PropertySetDef returned by the factory operation create_initial_propertysetdef/2 @@@@", []),
    Obj3 = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetDefFactory':
		  create_initial_propertysetdef(Fac, ValidDefs)),

    %% Initally the supplied prop's are created and no restrictions.
    ?match(3, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj3)),
    ?match({ok, []}, 'CosPropertyService_PropertySetDef':get_allowed_property_types(Obj3)),
    ?match({ok, []}, 'CosPropertyService_PropertySetDef':get_allowed_properties(Obj3)),

    %% Add a new properties an test if they have been inserted.
    ?match(ok, 'CosPropertyService_PropertySetDef':define_property_with_mode(Obj3, ?id4, ?val4, read_only)),
    ?match(4, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj3)),
    ?match(ok, 'CosPropertyService_PropertySetDef':define_property_with_mode(Obj3, ?id5, ?val5, read_only)),
    ?match(5, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj3)),

    %% Lookup each Property's mode.
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id1)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id2)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id3)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id4)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id5)),

    ?match({true, [_,_,_,_,_]}, 
	   'CosPropertyService_PropertySetDef':get_property_modes(Obj3, [?id1, ?id2, ?id3, ?id4, ?id5])),
    ?match({false, [_,_]}, 
	   'CosPropertyService_PropertySetDef':get_property_modes(Obj3, [?id1, "wrongID"])),
    ?match(ok, 'CosPropertyService_PropertySetDef':set_property_mode(Obj3, ?id4, normal)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id4)),

    ?match(ok, 'CosPropertyService_PropertySetDef':
	   set_property_modes(Obj3, 
			      [#'CosPropertyService_PropertyMode'{property_name = ?id1, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = ?id2, 
								  property_mode = read_only}])),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id1)),
    ?match(read_only, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id2)),
    ?match({'EXCEPTION',{'CosPropertyService_MultipleExceptions',_,_}}, 
	   'CosPropertyService_PropertySetDef':
	   set_property_modes(Obj3, 
			      [#'CosPropertyService_PropertyMode'{property_name = ?id3, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = ?id4, 
								  property_mode = read_only},
			       #'CosPropertyService_PropertyMode'{property_name = "wrongID", 
								  property_mode = read_only}])),

    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id3)),
    ?match(normal, 'CosPropertyService_PropertySetDef':get_property_mode(Obj3, ?id4)),
    
    corba:dispose(Obj3),

    ?match(ok, cosProperty:stop_SetDefFactory(Fac)),

    ok.



%%-----------------------------------------------------------------
%%  CosPropertyService_PropertyNamesIterator API tests 
%%-----------------------------------------------------------------
names_iterator_api(_Config) ->
    Fac = ?match({_,pseudo,_,_,_,_}, cosProperty:start_SetFactory()),
    Obj = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		  create_propertyset(Fac)),
    ?match(ok, 'CosPropertyService_PropertySet':
	   define_properties(Obj, [#'CosPropertyService_Property'{property_name = ?id1, 
								  property_value = ?val1},
				   #'CosPropertyService_Property'{property_name = ?id2, 
								  property_value = ?val2},
				   #'CosPropertyService_Property'{property_name = ?id3, 
								  property_value = ?val3}])),

    ?match(3, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj)),
    {_, _,ItObj} = ?match({ok, [], _}, 'CosPropertyService_PropertySetDef':get_all_property_names(Obj, 0)),
    ?match({false, [_,_,_]}, 'CosPropertyService_PropertyNamesIterator':next_n(ItObj,3)),
    ?match(ok, 'CosPropertyService_PropertyNamesIterator':reset(ItObj)),
    ?match({false, [_,_,_]}, 'CosPropertyService_PropertyNamesIterator':next_n(ItObj,4)),
    ?match(ok, 'CosPropertyService_PropertyNamesIterator':reset(ItObj)),
    ?match({true, [_]}, 'CosPropertyService_PropertyNamesIterator':next_n(ItObj,1)),
    ?match({true, _}, 'CosPropertyService_PropertyNamesIterator':next_one(ItObj)),
    ?match({true, _}, 'CosPropertyService_PropertyNamesIterator':next_one(ItObj)),
    ?match({false, _}, 'CosPropertyService_PropertyNamesIterator':next_one(ItObj)),
    ?match(ok, 'CosPropertyService_PropertyNamesIterator':destroy(ItObj)),

    corba:dispose(Obj),
    ok.

%%-----------------------------------------------------------------
%%  CosPropertyService_PropertiesIterator API tests 
%%-----------------------------------------------------------------
properties_iterator_api(_Config) ->
    Fac = ?match({_,pseudo,_,_,_,_}, cosProperty:start_SetFactory()),
    Obj = ?match({_,pseudo,_,_,_,_}, 'CosPropertyService_PropertySetFactory':
		  create_propertyset(Fac)),

    ?match(ok, 'CosPropertyService_PropertySet':
	   define_properties(Obj, [#'CosPropertyService_Property'{property_name = ?id1, 
								  property_value = ?val1},
				   #'CosPropertyService_Property'{property_name = ?id2, 
								  property_value = ?val2},
				   #'CosPropertyService_Property'{property_name = ?id3, 
								  property_value = ?val3}])),

    ?match(3, 'CosPropertyService_PropertySetDef':get_number_of_properties(Obj)),
    {_, _,ItObj} = ?match({ok, [], _}, 
		       'CosPropertyService_PropertySetDef':get_all_properties(Obj, 0)),
    ?match({false, [_,_,_]}, 'CosPropertyService_PropertiesIterator':next_n(ItObj,3)),
    ?match(ok, 'CosPropertyService_PropertiesIterator':reset(ItObj)),
    ?match({false, [_,_,_]}, 'CosPropertyService_PropertiesIterator':next_n(ItObj,4)),
    ?match(ok, 'CosPropertyService_PropertiesIterator':reset(ItObj)),
    ?match({true, [_]}, 'CosPropertyService_PropertiesIterator':next_n(ItObj,1)),
    ?match({true, {'CosPropertyService_Property',_,_}}, 
	   'CosPropertyService_PropertiesIterator':next_one(ItObj)),
    ?match({true, {'CosPropertyService_Property',_,_}}, 
	   'CosPropertyService_PropertiesIterator':next_one(ItObj)),
    ?match({false, {'CosPropertyService_Property',_,_}}, 
	   'CosPropertyService_PropertiesIterator':next_one(ItObj)),
    ?match(ok, 'CosPropertyService_PropertiesIterator':destroy(ItObj)),
    corba:dispose(Obj),
    ok.


%%-----------------------------------------------------------------
%%  END OF MODULE
%%-----------------------------------------------------------------
