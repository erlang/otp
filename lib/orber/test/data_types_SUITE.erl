%%-----------------------------------------------------------------
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
%% File    : data_types_SUITE.erl
%% Purpose : 
%%-----------------------------------------------------------------

-module(data_types_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").

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
    [fixed_type, any_type].

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


%%-----------------------------------------------------------------
%% Test Case: Fixed Point Datatype
%% Description: 
%%-----------------------------------------------------------------
fixed_type(_) ->
    Val1 = ?match({fixed,3,2,314}, orber_test_server:val1()),
    _Val2 = ?match({fixed,3,2,314}, orber_test_server:val2()),
    _Val3 = ?match({fixed,3,2,314}, orber_test_server:val3()),
    Val4 = ?match({fixed,3,2,314}, orber_test_server:val4()),
    Val5 = ?match({fixed,2,2,14}, orber_test_server:val5()),
    _Val6 = ?match({fixed,1,0,3}, orber_test_server:val6()),
    Val7 = ?match({fixed,2,2,-14}, orber_test_server:val7()),
    _Val8 = ?match({fixed,1,0,-3}, orber_test_server:val8()),
    Val9 = ?match({fixed,3,2,328}, orber_test_server:val9()),
    Val10 = ?match({fixed,4,4,4396}, orber_test_server:val10()),
    Val11 = ?match({fixed,31,29,2242857142857142857142857142857}, orber_test_server:val11()),
    Val12 = ?match({fixed,9,6,123140001}, orber_test_server:val12()),
    Val13 = ?match({fixed,9,1,123140001}, orber_test_server:val13()),
    Val14 = ?match({fixed,14,6,-12313876959999}, orber_test_server:val14()),
    Val15 = ?match({fixed,14,6,12314123240001}, orber_test_server:val15()),
    Val16 = ?match({fixed,17,7,15163459846280001}, orber_test_server:val16()),
    _Val17 = ?match({fixed,3,2,402}, orber_test_server:val17()),
    _Val18 = ?match({fixed,5,4,40401}, orber_test_server:val18()),
    _Val19 = ?match({fixed,3,0,200}, orber_test_server:val19()),
    Val20 = ?match({fixed,31,0,1999999999999999999999999999999}, orber_test_server:val20()),
    Val21 = ?match({fixed,1,0,0}, orber_test_server:val21()),
    Val22 = ?match({fixed,31,0,9999999999999999999999999999998}, orber_test_server:val22()),
    Val23 = ?match({fixed,1,0,1}, orber_test_server:val23()),
    _Val24 = ?match({fixed,5,0,19998}, orber_test_server:val24()),
    _Val25 = ?match({fixed,2,0,40}, orber_test_server:val25()),
    Val26 = ?match({fixed,31,0,9999999999999999999999999999999}, orber_test_server:val26()),

    ?match(Val1, fixed:create(3,2,314)),
    Val27 = ?match({fixed,6,2,314}, fixed:create(6,2,314)),

    ?match({tk_fixed,3,2}, fixed:get_typecode(Val1)),
    ?match({tk_fixed,6,2}, fixed:get_typecode(Val27)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}}, fixed:create(3,2,3140)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}}, fixed:create(5,6,314)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}}, fixed:create(32,2,314)),
    ?match(Val10, fixed:multiply(Val4, Val5)),
    ?match(Val16, fixed:multiply(Val12, Val13)),
    ?match(Val22, fixed:multiply(Val26, Val26)),

    ?match(Val9, fixed:add(Val4, Val5)),
    ?match(Val15, fixed:add(Val12, Val13)),
    ?match(Val20, fixed:add(Val26, Val26)),

    ?match(Val11, fixed:divide(Val4, Val5)),
    ?match(Val23, fixed:divide(Val26, Val26)),
    
    ?match(Val14, fixed:subtract(Val12, Val13)),
    ?match(Val21, fixed:subtract(Val26, Val26)),

    ?match(Val7, fixed:unary_minus(Val5)),
    ?match(Val5, fixed:unary_minus(Val7)),



    ok.

%%-----------------------------------------------------------------
%% Test Case: Any type
%% Description: 
%%-----------------------------------------------------------------
any_type(_) ->
    ?match(#any{typecode=undefined, value=undefined}, 
	   any:create()),
    ?match(#any{typecode=tk_short, value=undefined}, 
	   any:set_typecode(any:create(), tk_short)),
    ?match({'EXCEPTION', #'BAD_TYPECODE'{}}, 
	   any:set_typecode(any:create(), "wrong")),
    ?match({'EXCEPTION', #'BAD_TYPECODE'{}},
	   any:create("wrong", 1)),
    ?match(#any{typecode=tk_short, value = 1},
	   any:create(tk_short, 1)),
    ?match(tk_short,
	   any:get_typecode(any:create(tk_short, 1))),
    ?match(1,
	   any:get_value(any:create(tk_short, 1))),
    ?match(#any{typecode=tk_short, value=2},
	   any:set_value(any:create(tk_short, 1), 2)),

    ok.
