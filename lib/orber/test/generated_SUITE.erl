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
-export([]).
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    ['OrberApp_IFR', erlang_binary, erlang_pid, erlang_port,
     erlang_ref, 'CosNaming_Binding',
     'CosNaming_BindingList', 'CosNaming_Name',
     'CosNaming_NameComponent',
     'CosNaming_NamingContextExt_InvalidAddress',
     'CosNaming_NamingContext_AlreadyBound',
     'CosNaming_NamingContext_CannotProceed',
     'CosNaming_NamingContext_InvalidName',
     'CosNaming_NamingContext_NotEmpty',
     'CosNaming_NamingContext_NotFound',
     'CosNaming_BindingIterator', 'CosNaming_NamingContext',
     'CosNaming_NamingContextExt'].

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
%% Test Case:'OrberApp_IFR'
%% Description: 
%%-----------------------------------------------------------------
'OrberApp_IFR'(_) ->
    ?nomatch(undefined, 'OrberApp_IFR':oe_tc(get_absolute_name)),
    ?nomatch(undefined, 'OrberApp_IFR':oe_tc(get_user_exception_type)),
    ?match(undefined, 'OrberApp_IFR':oe_tc(undefined)),
    ?match([_|_], 'OrberApp_IFR':oe_get_interface()),
    ?match("IDL:OrberApp/IFR:1.0", 'OrberApp_IFR':typeID()),
    check_tc('OrberApp_IFR':oe_get_interface()),
    ?match(true, 'OrberApp_IFR':oe_is_a('OrberApp_IFR':typeID())),
    ?match(false, 'OrberApp_IFR':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: erlang_binary
%% Description: 
%%-----------------------------------------------------------------
erlang_binary(_) ->
    ?match(true, orber_tc:check_tc(erlang_binary:tc())),
    ?match("IDL:erlang/binary:1.0", erlang_binary:id()),
    ?match("erlang_binary", erlang_binary:name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: erlang_pid
%% Description: 
%%-----------------------------------------------------------------
erlang_pid(_) ->
    ?match(true, orber_tc:check_tc(erlang_pid:tc())),
    ?match("IDL:erlang/pid:1.0", erlang_pid:id()),
    ?match("erlang_pid", erlang_pid:name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: erlang_port
%% Description: 
%%-----------------------------------------------------------------
erlang_port(_) ->
    ?match(true, orber_tc:check_tc(erlang_port:tc())),
    ?match("IDL:erlang/port:1.0", erlang_port:id()),
    ?match("erlang_port", erlang_port:name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: erlang_ref
%% Description: 
%%-----------------------------------------------------------------
erlang_ref(_) ->
    ?match(true, orber_tc:check_tc(erlang_ref:tc())),
    ?match("IDL:erlang/ref:1.0", erlang_ref:id()),
    ?match("erlang_ref", erlang_ref:name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_Binding'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_Binding'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_Binding':tc())),
    ?match("IDL:omg.org/CosNaming/Binding:1.0", 'CosNaming_Binding':id()),
    ?match("CosNaming_Binding", 'CosNaming_Binding':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_BindingList'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_BindingList'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_BindingList':tc())),
    ?match("IDL:omg.org/CosNaming/BindingList:1.0", 'CosNaming_BindingList':id()),
    ?match("CosNaming_BindingList", 'CosNaming_BindingList':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_Name'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_Name'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_Name':tc())),
    ?match("IDL:omg.org/CosNaming/Name:1.0", 'CosNaming_Name':id()),
    ?match("CosNaming_Name", 'CosNaming_Name':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NameComponent'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NameComponent'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NameComponent':tc())),
    ?match("IDL:omg.org/CosNaming/NameComponent:1.0", 'CosNaming_NameComponent':id()),
    ?match("CosNaming_NameComponent", 'CosNaming_NameComponent':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContextExt_InvalidAddress'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContextExt_InvalidAddress'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NamingContextExt_InvalidAddress':tc())),
    ?match("IDL:omg.org/CosNaming/NamingContextExt/InvalidAddress:1.0", 'CosNaming_NamingContextExt_InvalidAddress':id()),
    ?match("CosNaming_NamingContextExt_InvalidAddress", 'CosNaming_NamingContextExt_InvalidAddress':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContext_AlreadyBound'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContext_AlreadyBound'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NamingContext_AlreadyBound':tc())),
    ?match("IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0", 'CosNaming_NamingContext_AlreadyBound':id()),
    ?match("CosNaming_NamingContext_AlreadyBound", 'CosNaming_NamingContext_AlreadyBound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContext_CannotProceed'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContext_CannotProceed'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NamingContext_CannotProceed':tc())),
    ?match("IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0", 'CosNaming_NamingContext_CannotProceed':id()),
    ?match("CosNaming_NamingContext_CannotProceed", 'CosNaming_NamingContext_CannotProceed':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContext_InvalidName'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContext_InvalidName'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NamingContext_InvalidName':tc())),
    ?match("IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0", 'CosNaming_NamingContext_InvalidName':id()),
    ?match("CosNaming_NamingContext_InvalidName", 'CosNaming_NamingContext_InvalidName':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContext_NotEmpty'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContext_NotEmpty'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NamingContext_NotEmpty':tc())),
    ?match("IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0", 'CosNaming_NamingContext_NotEmpty':id()),
    ?match("CosNaming_NamingContext_NotEmpty", 'CosNaming_NamingContext_NotEmpty':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContext_NotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContext_NotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNaming_NamingContext_NotFound':tc())),
    ?match("IDL:omg.org/CosNaming/NamingContext/NotFound:1.0", 'CosNaming_NamingContext_NotFound':id()),
    ?match("CosNaming_NamingContext_NotFound", 'CosNaming_NamingContext_NotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_BindingIterator'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_BindingIterator'(_) ->
    ?nomatch(undefined, 'CosNaming_BindingIterator':oe_tc(next_one)),
    ?nomatch(undefined, 'CosNaming_BindingIterator':oe_tc(next_n)),
    ?nomatch(undefined, 'CosNaming_BindingIterator':oe_tc(destroy)),
    ?match(undefined, 'CosNaming_BindingIterator':oe_tc(undefined)),
    ?match([_|_], 'CosNaming_BindingIterator':oe_get_interface()),
    ?match("IDL:omg.org/CosNaming/BindingIterator:1.0", 
	   'CosNaming_BindingIterator':typeID()),
    check_tc('CosNaming_BindingIterator':oe_get_interface()),
    ?match(true, 'CosNaming_BindingIterator':oe_is_a('CosNaming_BindingIterator':typeID())),
    ?match(false, 'CosNaming_BindingIterator':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContext'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContext'(_) ->
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(bind)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(rebind)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(bind_context)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(rebind_context)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(resolve)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(unbind)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(new_context)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(bind_new_context)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNaming_NamingContext':oe_tc(list)),
    ?match(undefined, 'CosNaming_NamingContext':oe_tc(undefined)),
    ?match([_|_], 'CosNaming_NamingContext':oe_get_interface()),
    ?match("IDL:omg.org/CosNaming/NamingContext:1.0", 
	   'CosNaming_NamingContext':typeID()),
    check_tc('CosNaming_NamingContext':oe_get_interface()),
    ?match(true, 'CosNaming_NamingContext':oe_is_a('CosNaming_NamingContext':typeID())),
    ?match(false, 'CosNaming_NamingContext':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNaming_NamingContexExt'
%% Description: 
%%-----------------------------------------------------------------
'CosNaming_NamingContextExt'(_) ->
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(to_string)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(to_name)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(to_url)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(resolve_str)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(bind)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(rebind)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(bind_context)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(rebind_context)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(new_context)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(bind_new_context)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNaming_NamingContextExt':oe_tc(list)),
    ?match(undefined, 'CosNaming_NamingContextExt':oe_tc(undefined)),
    ?match([_|_], 'CosNaming_NamingContextExt':oe_get_interface()),
    ?match("IDL:omg.org/CosNaming/NamingContextExt:1.0", 
	   'CosNaming_NamingContextExt':typeID()),
    check_tc('CosNaming_NamingContextExt':oe_get_interface()),
    ?match(true, 'CosNaming_NamingContextExt':oe_is_a('CosNaming_NamingContextExt':typeID())),
    ?match(true, 'CosNaming_NamingContextExt':oe_is_a('CosNaming_NamingContext':typeID())),
    ?match(false, 'CosNaming_NamingContextExt':oe_is_a("wrong")),
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
    
    
