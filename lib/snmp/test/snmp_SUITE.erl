%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2019. All Rights Reserved.
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

-module(snmp_SUITE).

-include("snmp_test_lib.hrl").

-export([all/0, 
	 suite/0,
	 groups/0, 
	 init_per_suite/1,    end_per_suite/1, 
	 init_per_group/2,    end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2
	]).


%%
%% -----
%%

init_per_testcase(_Case, Config) when is_list(Config) ->
    Config.

end_per_testcase(_Case, Config) when is_list(Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->

    ?DBG("init_per_suite -> entry with"
	 "~n   Config: ~p", [Config]),

    %% We have some crap machines that causes random test case failures
    %% for no obvious reason. So, attempt to identify those without actually
    %% checking for the host name...
    %% We have two "machines" we are checking for. Both are old installations
    %% running on really slow VMs (the host machines are old and tired).
    LinuxVersionVerify =
        fun(V) when (V > {3,6,11}) ->
                false; % OK - No skip
           (V) when (V =:= {3,6,11}) ->
                case string:trim(os:cmd("cat /etc/issue")) of
                    "Fedora release 16 " ++ _ -> % Stone age Fedora => Skip
                        true;
                    _ ->
                        false
                end;
           (V) when (V > {2,6,24}) ->
                false; % OK - No skip
           (_) ->
                %% We are specifically checking for
                %% a *really* old gento...
                case string:find(string:strip(os:cmd("uname -a")), "gentoo") of
                    nomatch ->
                        false;
                    _ -> % Stone age gentoo => Skip
                        true
                end
        end,
    COND = [{unix, [{linux, LinuxVersionVerify}]}],
    case ?OS_BASED_SKIP(COND) of
        true ->
            {skip, "Unstable host and/or os (or combo thererof)"};
        false ->
            Config
    end.

end_per_suite(Config) when is_list(Config) ->

    ?DBG("end_per_suite -> entry with"
	 "~n   Config: ~p", [Config]),

    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

suite() -> 
    [{ct_hooks, [ts_install_cth]}].

all() -> 
    [
     {group, manager}
    ].

groups() -> 
    [
     {manager, [], manager_cases()},

     {manager_user_test,   [], [{snmp_manager_user_test,   all}]},
     {manager_test,        [], [{snmp_manager_test,        all}]}
    ].


manager_cases() ->
    [
     {group, manager_user_test},
     {group, manager_test}
    ].


init_per_group(GroupName, Config0) ->

    ?DBG("init_per_group -> entry with"
	 "~n   GroupName: ~p"
	 "~n   Config0:   ~p", [GroupName, Config0]),

    %% Group name is not really the suite name
    %% (but it is a good enough approximation),
    %% but it does not matter since we only need
    %% it to be unique.
    snmp_test_lib:init_suite_top_dir(GroupName, Config0).

end_per_group(_GroupName, Config) ->
    lists:keydelete(snmp_suite_top_dir, 1, Config).
