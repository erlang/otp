%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
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

-module(nested_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 case_in_case/1, case_in_after/1, catch_in_catch/1, bif_in_bif/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [case_in_case, case_in_after, catch_in_catch,
     bif_in_bif].

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


case_in_case(suite) -> [];
case_in_case(Config) when is_list(Config) ->
    ?line done = search_any([a], [{a, 1}]),
    ?line done = search_any([x], [{a, 1}]),
    ok.

search_any([Key|Rest], List) ->
    ?line case case lists:keysearch(Key, 1, List) of
		   {value, _} -> 
		       true;
		   _ ->
		       false
	       end of
	      true ->
		  ok;
	      false ->
		  error;
	      Other ->
		  test_server:fail({other_result, Other})
	  end,
    ?line search_any(Rest, List);
search_any([], _) ->
    done.

case_in_after(suite) -> [];
case_in_after(Config) when is_list(Config) ->
    receive
	after case {x, y, z} of
		  {x, y, z} -> 0
	      end ->
		ok
	end,
    ok.

catch_in_catch(doc) -> "Test a catch within a catch in the same function.";
catch_in_catch(suite) -> [];
catch_in_catch(Config) when is_list(Config) ->
    ?line {outer, inner_exit} = catcher(),
    ok.

catcher() ->
    case (catch 
	  case (catch ?MODULE:non_existing()) of    % bogus function
	      {'EXIT', _} ->
		  inner_exit;
	      Res1 ->
		  {inner, Res1}
	  end) of
	{'EXIT', _} ->
	    outer_exit;
	Res2 ->
	    {outer, Res2}
    end.

bif_in_bif(doc) -> "Test a BIF call within a BIF call.";
bif_in_bif(suite) -> [];
bif_in_bif(Config) when is_list(Config) ->
    Self = self(),
    put(pid, Self),
    Self = register_me(),
    ok.

register_me() ->
    register(?MODULE, Pid = get(pid)),
    Pid.
