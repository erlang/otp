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
%% File    : generated_SUITE.erl
%% Purpose : 
%%-----------------------------------------------------------------

-module(generated_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").

-define(default_timeout, test_server:minutes(3)).

-define(match(ExpectedRes, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
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
			AcTuAlReS
		end
	end()).


-define(checktc(_Op),
        fun(TC) ->
		case orber_tc:check_tc(TC) of
		    false ->
			io:format("###### ERROR ERROR ######~n~p - ~p~n", [Op, TC]),
			exit(TC);
		    true ->
			true
		end
	end).

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
    ['CosPropertyService_ConflictingProperty',
     'CosPropertyService_ConstraintNotSupported',
     'CosPropertyService_FixedProperty',
     'CosPropertyService_InvalidPropertyName',
     'CosPropertyService_MultipleExceptions',
     'CosPropertyService_Properties',
     'CosPropertyService_Property',
     'CosPropertyService_PropertyDef',
     'CosPropertyService_PropertyDefs',
     'CosPropertyService_PropertyException',
     'CosPropertyService_PropertyExceptions',
     'CosPropertyService_PropertyMode',
     'CosPropertyService_PropertyModes',
     'CosPropertyService_PropertyNames',
     'CosPropertyService_PropertyNotFound',
     'CosPropertyService_PropertyTypes',
     'CosPropertyService_ReadOnlyProperty',
     'CosPropertyService_UnsupportedMode',
     'CosPropertyService_UnsupportedProperty',
     'CosPropertyService_UnsupportedTypeCode',
     'CosPropertyService_PropertyNamesIterator',
     'CosPropertyService_PropertiesIterator',
     'CosPropertyService_PropertySet',
     'CosPropertyService_PropertySetDef',
     'CosPropertyService_PropertySetDefFactory',
     'CosPropertyService_PropertySetFactory'].

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
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_ConflictingProperty'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_ConflictingProperty'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_ConflictingProperty':tc())),
    ?match("IDL:omg.org/CosPropertyService/ConflictingProperty:1.0", 
	   'CosPropertyService_ConflictingProperty':id()),
    ?match("CosPropertyService_ConflictingProperty", 
	   'CosPropertyService_ConflictingProperty':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_ConstraintNotSupported'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_ConstraintNotSupported'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_ConstraintNotSupported':tc())),
    ?match("IDL:omg.org/CosPropertyService/ConstraintNotSupported:1.0", 
	   'CosPropertyService_ConstraintNotSupported':id()),
    ?match("CosPropertyService_ConstraintNotSupported", 
	   'CosPropertyService_ConstraintNotSupported':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_FixedProperty'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_FixedProperty'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_FixedProperty':tc())),
    ?match("IDL:omg.org/CosPropertyService/FixedProperty:1.0", 
	   'CosPropertyService_FixedProperty':id()),
    ?match("CosPropertyService_FixedProperty", 
	   'CosPropertyService_FixedProperty':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_InvalidPropertyName'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_InvalidPropertyName'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_InvalidPropertyName':tc())),
    ?match("IDL:omg.org/CosPropertyService/InvalidPropertyName:1.0", 
	   'CosPropertyService_InvalidPropertyName':id()),
    ?match("CosPropertyService_InvalidPropertyName", 
	   'CosPropertyService_InvalidPropertyName':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_MultipleExceptions'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_MultipleExceptions'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_MultipleExceptions':tc())),
    ?match("IDL:omg.org/CosPropertyService/MultipleExceptions:1.0", 
	   'CosPropertyService_MultipleExceptions':id()),
    ?match("CosPropertyService_MultipleExceptions", 
	   'CosPropertyService_MultipleExceptions':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_Properties'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_Properties'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_Properties':tc())),
    ?match("IDL:omg.org/CosPropertyService/Properties:1.0", 
	   'CosPropertyService_Properties':id()),
    ?match("CosPropertyService_Properties", 
	   'CosPropertyService_Properties':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_Property'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_Property'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_Property':tc())),
    ?match("IDL:omg.org/CosPropertyService/Property:1.0", 
	   'CosPropertyService_Property':id()),
    ?match("CosPropertyService_Property", 
	   'CosPropertyService_Property':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyDef'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyDef'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyDef':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyDef:1.0", 
	   'CosPropertyService_PropertyDef':id()),
    ?match("CosPropertyService_PropertyDef", 
	   'CosPropertyService_PropertyDef':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyDefs'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyDefs'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyDefs':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyDefs:1.0", 
	   'CosPropertyService_PropertyDefs':id()),
    ?match("CosPropertyService_PropertyDefs", 
	   'CosPropertyService_PropertyDefs':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyException'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyException'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyException':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyException:1.0", 
	   'CosPropertyService_PropertyException':id()),
    ?match("CosPropertyService_PropertyException", 
	   'CosPropertyService_PropertyException':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyExceptions'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyExceptions'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyExceptions':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyExceptions:1.0", 
	   'CosPropertyService_PropertyExceptions':id()),
    ?match("CosPropertyService_PropertyExceptions", 
	   'CosPropertyService_PropertyExceptions':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyMode'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyMode'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyMode':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyMode:1.0", 
	   'CosPropertyService_PropertyMode':id()),
    ?match("CosPropertyService_PropertyMode", 
	   'CosPropertyService_PropertyMode':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyModes'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyModes'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyModes':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyModes:1.0", 
	   'CosPropertyService_PropertyModes':id()),
    ?match("CosPropertyService_PropertyModes", 
	   'CosPropertyService_PropertyModes':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyNames'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyNames'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyNames':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyNames:1.0", 
	   'CosPropertyService_PropertyNames':id()),
    ?match("CosPropertyService_PropertyNames", 
	   'CosPropertyService_PropertyNames':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyNotFound':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyNotFound:1.0", 
	   'CosPropertyService_PropertyNotFound':id()),
    ?match("CosPropertyService_PropertyNotFound", 
	   'CosPropertyService_PropertyNotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyTypes'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyTypes'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_PropertyTypes':tc())),
    ?match("IDL:omg.org/CosPropertyService/PropertyTypes:1.0", 
	   'CosPropertyService_PropertyTypes':id()),
    ?match("CosPropertyService_PropertyTypes", 
	   'CosPropertyService_PropertyTypes':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_ReadOnlyProperty'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_ReadOnlyProperty'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_ReadOnlyProperty':tc())),
    ?match("IDL:omg.org/CosPropertyService/ReadOnlyProperty:1.0", 
	   'CosPropertyService_ReadOnlyProperty':id()),
    ?match("CosPropertyService_ReadOnlyProperty", 
	   'CosPropertyService_ReadOnlyProperty':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_UnsupportedMode'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_UnsupportedMode'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_UnsupportedMode':tc())),
    ?match("IDL:omg.org/CosPropertyService/UnsupportedMode:1.0", 
	   'CosPropertyService_UnsupportedMode':id()),
    ?match("CosPropertyService_UnsupportedMode", 
	   'CosPropertyService_UnsupportedMode':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_UnsupportedProperty'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_UnsupportedProperty'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_UnsupportedProperty':tc())),
    ?match("IDL:omg.org/CosPropertyService/UnsupportedProperty:1.0", 
	   'CosPropertyService_UnsupportedProperty':id()),
    ?match("CosPropertyService_UnsupportedProperty", 
	   'CosPropertyService_UnsupportedProperty':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_UnsupportedTypeCode'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_UnsupportedTypeCode'(_) ->
    ?match(true, orber_tc:check_tc('CosPropertyService_UnsupportedTypeCode':tc())),
    ?match("IDL:omg.org/CosPropertyService/UnsupportedTypeCode:1.0", 
	   'CosPropertyService_UnsupportedTypeCode':id()),
    ?match("CosPropertyService_UnsupportedTypeCode", 
	   'CosPropertyService_UnsupportedTypeCode':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertyNamesIterator'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertyNamesIterator'(_) ->
    ?nomatch(undefined, 'CosPropertyService_PropertyNamesIterator':oe_tc(reset)),
    ?nomatch(undefined, 'CosPropertyService_PropertyNamesIterator':oe_tc(next_one)),
    ?nomatch(undefined, 'CosPropertyService_PropertyNamesIterator':oe_tc(next_n)),
    ?nomatch(undefined, 'CosPropertyService_PropertyNamesIterator':oe_tc(destroy)),
    ?match(undefined, 'CosPropertyService_PropertyNamesIterator':oe_tc(undefined)),
    ?match([_|_], 'CosPropertyService_PropertyNamesIterator':oe_get_interface()),
    ?match("IDL:omg.org/CosPropertyService/PropertyNamesIterator:1.0", 
	   'CosPropertyService_PropertyNamesIterator':typeID()),
    check_tc('CosPropertyService_PropertyNamesIterator':oe_get_interface()),
    ?match(true, 'CosPropertyService_PropertyNamesIterator':oe_is_a('CosPropertyService_PropertyNamesIterator':typeID())),
    ?match(false, 'CosPropertyService_PropertyNamesIterator':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertiesIterator'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertiesIterator'(_) ->
    ?nomatch(undefined, 'CosPropertyService_PropertiesIterator':oe_tc(reset)),
    ?nomatch(undefined, 'CosPropertyService_PropertiesIterator':oe_tc(next_one)),
    ?nomatch(undefined, 'CosPropertyService_PropertiesIterator':oe_tc(next_n)),
    ?nomatch(undefined, 'CosPropertyService_PropertiesIterator':oe_tc(destroy)),
    ?match(undefined, 'CosPropertyService_PropertiesIterator':oe_tc(undefined)),
    ?match([_|_], 'CosPropertyService_PropertiesIterator':oe_get_interface()),
    ?match("IDL:omg.org/CosPropertyService/PropertiesIterator:1.0", 
	   'CosPropertyService_PropertiesIterator':typeID()),
    check_tc('CosPropertyService_PropertiesIterator':oe_get_interface()),
    ?match(true, 'CosPropertyService_PropertiesIterator':oe_is_a('CosPropertyService_PropertiesIterator':typeID())),
    ?match(false, 'CosPropertyService_PropertiesIterator':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertySet'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertySet'(_) ->
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(define_property)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(define_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(get_number_of_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(get_all_property_names)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(get_property_value)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(get_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(get_all_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(delete_property)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(delete_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(delete_all_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySet':oe_tc(is_property_defined)),
    ?match(undefined, 'CosPropertyService_PropertySet':oe_tc(undefined)),
    ?match([_|_], 'CosPropertyService_PropertySet':oe_get_interface()),
    ?match("IDL:omg.org/CosPropertyService/PropertySet:1.0", 
	   'CosPropertyService_PropertySet':typeID()),
    check_tc('CosPropertyService_PropertySet':oe_get_interface()),
    ?match(true, 'CosPropertyService_PropertySet':oe_is_a('CosPropertyService_PropertySet':typeID())),
    ?match(false, 'CosPropertyService_PropertySet':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertySetDef'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertySetDef'(_) ->
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_allowed_property_types)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_allowed_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(define_property_with_mode)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(define_properties_with_modes)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_property_mode)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_property_modes)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(set_property_mode)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(set_property_modes)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(define_property)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(define_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_number_of_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_all_property_names)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_property_value)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(get_all_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(delete_property)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(delete_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(delete_all_properties)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDef':oe_tc(is_property_defined)),
    ?match(undefined, 'CosPropertyService_PropertySetDef':oe_tc(undefined)),
    ?match([_|_], 'CosPropertyService_PropertySetDef':oe_get_interface()),
    ?match("IDL:omg.org/CosPropertyService/PropertySetDef:1.0", 
	   'CosPropertyService_PropertySetDef':typeID()),
    check_tc('CosPropertyService_PropertySetDef':oe_get_interface()),
    ?match(true, 'CosPropertyService_PropertySetDef':oe_is_a('CosPropertyService_PropertySetDef':typeID())),
    ?match(true, 'CosPropertyService_PropertySetDef':oe_is_a('CosPropertyService_PropertySet':typeID())),
    ?match(false, 'CosPropertyService_PropertySetDef':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertySetDefFactory'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertySetDefFactory'(_) ->
    ?nomatch(undefined, 'CosPropertyService_PropertySetDefFactory':oe_tc(create_propertysetdef)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDefFactory':oe_tc(create_constrained_propertysetdef)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetDefFactory':oe_tc(create_initial_propertysetdef)),
    ?match(undefined, 'CosPropertyService_PropertySetDefFactory':oe_tc(undefined)),
    ?match([_|_], 'CosPropertyService_PropertySetDefFactory':oe_get_interface()),
    ?match("IDL:omg.org/CosPropertyService/PropertySetDefFactory:1.0", 
	   'CosPropertyService_PropertySetDefFactory':typeID()),
    check_tc('CosPropertyService_PropertySetDefFactory':oe_get_interface()),
    ?match(true, 'CosPropertyService_PropertySetDefFactory':oe_is_a('CosPropertyService_PropertySetDefFactory':typeID())),
    ?match(false, 'CosPropertyService_PropertySetDefFactory':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosPropertyService_PropertySetFactory'
%% Description: 
%%-----------------------------------------------------------------
'CosPropertyService_PropertySetFactory'(_) ->
    ?nomatch(undefined, 'CosPropertyService_PropertySetFactory':oe_tc(create_propertyset)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetFactory':oe_tc(create_constrained_propertyset)),
    ?nomatch(undefined, 'CosPropertyService_PropertySetFactory':oe_tc(create_initial_propertyset)),
    ?match(undefined, 'CosPropertyService_PropertySetFactory':oe_tc(undefined)),
    ?match([_|_], 'CosPropertyService_PropertySetFactory':oe_get_interface()),
    ?match("IDL:omg.org/CosPropertyService/PropertySetFactory:1.0", 
	   'CosPropertyService_PropertySetFactory':typeID()),
    check_tc('CosPropertyService_PropertySetFactory':oe_get_interface()),
    ?match(true, 'CosPropertyService_PropertySetFactory':oe_is_a('CosPropertyService_PropertySetFactory':typeID())),
    ?match(false, 'CosPropertyService_PropertySetFactory':oe_is_a("wrong")),
    ok.



%%-----------------------------------------------------------------
%% MISC functions
%%-----------------------------------------------------------------
check_tc([]) ->
    ok;
check_tc([{Op, {RetType, InParameters, OutParameters}}|T]) ->
    io:format("checked - ~s~n", [Op]),
    lists:all(?checktc(Op), [RetType|InParameters]),
    lists:all(?checktc(Op), OutParameters),
    check_tc(T).
    
    
