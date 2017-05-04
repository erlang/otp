%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-module(skip_case_SUITE).

-compile(export_all).

-include("ct.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

init_per_group(_,Config) ->
    Config.

end_per_group(_,_) ->
    ok.

init_per_testcase(skip_in_init,Config) ->
    {skip,"Skipped in init_per_testcase/2"};
init_per_testcase(fail_in_init,Config) ->
    ct:fail("Failed in init_per_testcase/2");
init_per_testcase(exit_in_init,Config) ->
    exit(self(),"Exit in init_per_testcase/2");
init_per_testcase(_,Config) ->
    Config.

end_per_testcase(fail_in_end,_) ->
    ct:fail("Failed in end_per_testcase/2");
end_per_testcase(exit_in_end,_) ->
    exit(self(),"Exit in end_per_testcase/2");
end_per_testcase(_,_) ->
    ok.

all() ->
    [skip_in_spec,
     skip_in_init,
     fail_in_init,
     exit_in_init,
     fail_in_end,
     exit_in_end,
     skip_in_case,
     req_auto_skip,
     fail_auto_skip
    ].

%% Test cases starts here.
skip_in_spec(Config) ->
    ct:fail("This test shall never be run. "
            "It shall be skipped in the test spec.").

skip_in_init(Config) ->
    ct:fail("This test shall never be run. "
            "It shall be skipped in init_per_testcase/2.").

fail_in_init(Config) ->
    ct:fail("This test shall never be run. "
            "It shall fail in init_per_testcase/2.").

exit_in_init(Config) ->
    ct:fail("This test shall never be run. "
            "It shall exit in init_per_testcase/2.").

fail_in_end(Config) ->
    ok.

exit_in_end(Config) ->
    ok.

skip_in_case(Config) ->
    {skip,"Skipped in test case function"}.

req_auto_skip() ->
    [{require,whatever}].
req_auto_skip(Config) ->
    ct:fail("This test shall never be run due to "
            "failed require").

fail_auto_skip() ->
    faulty_return_value.
fail_auto_skip(Config) ->
    ct:fail("This test shall never be run due to "
            "faulty return from info function").
