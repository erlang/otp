%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(versions_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).

%% Test cases
-export([check_misc/1, compare/1, branch_base/1, branch/1, doctests/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [check_misc, compare, branch_base, branch, doctests].

check_misc(Config) when is_list(Config) ->
    lists:foreach(fun (V) ->
                          check_misc_version(V)
                  end, valid_versions(Config)),
    check_bad_vstr("27"),
    check_bad_vlist([27]),
    check_bad_vstr(""),
    check_bad_vlist([]),
    check_bad_vstr("30.0-rc1"),
    check_bad_vstr("31.2.7.4.05"),
    check_bad_vstr("031.2.7.4.5"),
    check_bad_vstr("31.2.007.4.5"),
    check_bad_vstr("31.2.007"),
    check_bad_vstr("31.02"),
    check_bad_vstr("0031.2"),
    ok.

compare(Config) when is_list(Config) ->
    ancestor = check_cmp(~"0.0", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"0.3", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"0.3.1", ~"35.3.0.2.2"),
    undefined = check_cmp(~"0.3.1.1", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"17.0", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"35.2.7", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"35.3", ~"35.3.0.2.2"),
    undefined = check_cmp(~"35.3.1", ~"35.3.0.2.2"),
    undefined = check_cmp(~"35.3.1.2", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"35.3.0.1", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"35.3.0.2", ~"35.3.0.2.2"),
    undefined = check_cmp(~"35.3.0.3", ~"35.3.0.2.2"),
    undefined = check_cmp(~"35.3.0.4", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"35.3.0.2.1", ~"35.3.0.2.2"),
    same = check_cmp(~"35.3.0.2.2", ~"35.3.0.2.2"),
    descendant = check_cmp(~"35.3.0.2.3", ~"35.3.0.2.2"),
    descendant = check_cmp(~"35.3.0.2.3.1", ~"35.3.0.2.2"),
    descendant = check_cmp(~"35.3.0.2.4", ~"35.3.0.2.2"),
    descendant = check_cmp(~"35.3.0.2.65.114.256", ~"35.3.0.2.2"),
    undefined = check_cmp(~"40.0", ~"35.3.0.2.2"),
    undefined = check_cmp(~"40.0.1", ~"35.3.0.2.2"),
    undefined = check_cmp(~"40.0.1.2", ~"35.3.0.2.2"),
    ancestor = check_cmp(~"18.2.4", ~"18.2.4.1"),
    ancestor = check_cmp(~"18.2.4", ~"18.2.4.0.1"),
    undefined = check_cmp(~"18.2.4.1", ~"18.2.4.0.1"),
    undefined = check_cmp(~"18.2.4.1", ~"18.3"),
    undefined = check_cmp(~"18.2.4.0.1", ~"18.3"),
    ok.

branch_base(Config) when is_list(Config) ->
    ~"0.0" = versions:branch_base(~"0."),
    ~"35.3.1" = versions:branch_base(~"35.3.1."),
    ~"35.3" = versions:branch_base(~"35.3.0."),
    ~"35.3.0.2" = versions:branch_base(~"35.3.0.2."),
    ~"35.3.0.2.3" = versions:branch_base(~"35.3.0.2.3."),
    ~"40.0" = versions:branch_base(~"40.0.0."),
    ~"40.0" = versions:branch_base(~"40.0.0.0."),
    ~"40.0" = versions:branch_base(~"40.0.0.0.0."),
    ~"18.2.4" = versions:branch_base(~"18.2.4."),
    ~"18.2.4" = versions:branch_base(~"18.2.4.0."),
    ~"18.2.4" = versions:branch_base(~"18.2.4.0.0."),
    ~"0.0" = versions:branch_base(versions:branch(~"0.0")),
    ok.

branch(Config) when is_list(Config) ->
    ~"0." = versions:branch(~"0.0"),
    ~"0." = versions:branch(~"0.3"),
    ~"0." = versions:branch(~"0.3.1"),
    ~"0." = versions:branch(~"17.0"),
    ~"0." = versions:branch(~"35.2.7"),
    ~"0." = versions:branch(~"35.3"),
    ~"35.3.1." = versions:branch(~"35.3.1.1"),
    ~"35.3.1." = versions:branch(~"35.3.1.2"),
    ~"35.3.0." = versions:branch(~"35.3.0.1"),
    ~"35.3.0." = versions:branch(~"35.3.0.2"),
    ~"35.3.0.2." = versions:branch(~"35.3.0.2.3"),
    ~"35.3.0.2." = versions:branch(~"35.3.0.2.2"),
    ~"35.3.0.2.3." = versions:branch(~"35.3.0.2.3.1"),
    ~"35.3.0.2.3." = versions:branch(~"35.3.0.2.3.2"),
    ~"40.0.0." = versions:branch(~"40.0.0.1"),
    ~"40.0.0.0." = versions:branch(~"40.0.0.0.1"),
    ~"40.0.0.0.0." = versions:branch(~"40.0.0.0.0.1"),
    ~"18.2.4." = versions:branch(~"18.2.4.1"),
    ~"18.2.4.0." = versions:branch(~"18.2.4.0.1"),
    ~"18.2.4.0.0." = versions:branch(~"18.2.4.0.0.1"),
    ok.

doctests(Config) when is_list(Config) ->
    ct_doctest:module(versions, [{missing_tests,
                                  [{list_check, 1},
                                   {list_to_string, 1},
                                   {string_to_list, 1}]}]).

check_misc_version(V) ->
    true = versions:check(V),
    VL = versions:string_to_list(V),
    VL = versions:string_to_list(c2l(V)),
    true = versions:list_check(VL),
    V = versions:list_to_string(VL),
    same = check_cmp(V, c2l(V)),
    same = check_cmp(c2l(V), V),
    check_cmp(V, VL, ~"24.0", [24,0]),
    check_cmp(V, VL, ~"17.0.1", [17,0,1]),
    check_cmp(V, VL, ~"22.3.4.12", [22,3,4,12]),
    check_cmp(V, VL, ~"23.2.7.2", [23,2,7,2]),
    B = versions:branch(V),
    try
        _ = versions:branch_base(V),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    try
        _ = versions:branch(B),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    false = versions:check(B),
    BV = versions:branch_base(B),
    if V == ~"0.0" -> same = check_cmp(BV, V);
       V == "0.0" -> same = check_cmp(BV, V);
       true -> ancestor = check_cmp(BV, V)
    end,
    true = versions:check(BV),
    check_bad_vstr(c2b([~".", V])),
    check_bad_vstr(c2b([~"-2.", V])),
    check_bad_vstr(c2b([~"a.", V])),
    check_bad_vstr(c2b([V, ~"."])),
    check_bad_vstr(c2b([V, ~".0"])),
    check_bad_vstr(c2b([V, ~".-1.3"])),
    check_bad_vstr(c2b([V, ~".a.8"])),
    check_bad_vlist(['.' | VL]),
    check_bad_vlist([-2 | VL]),
    check_bad_vlist([a | VL]),
    check_bad_vlist(VL ++ ['.']),
    check_bad_vlist(VL ++ [0]),
    check_bad_vlist(VL ++ [-1, 3]),
    check_bad_vlist(VL ++ [a, 8]),
    ok.

check_cmp(V1, V2) ->
    check_cmp(V1, versions:string_to_list(V1),
              V2, versions:string_to_list(V2)).

check_cmp(V1, VL1, V2, VL2) ->
    Res1 = versions:compare(V1, V2),
    Res1 = versions:list_compare(VL1, VL2),
    Res2 = versions:compare(V2, V1),
    Res2 = versions:list_compare(VL2, VL1),
    case Res1 of
        same -> same = Res2;
        ancestor -> descendant = Res2;
        descendant -> ancestor = Res2;
        undefined -> undefined = Res2
    end,
    Res1.

check_bad_vstr(BV) ->
    false = versions:check(BV),
    try
        _ = versions:compare(~"17.0", BV),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    try
        _ = versions:compare(BV, ~"17.0"),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    try
        _ = versions:branch(BV),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    try
        _ = versions:string_to_list(BV),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end.

check_bad_vlist(BV) ->
    false = versions:list_check(BV),
    try
        _ = versions:list_compare([17,0], BV),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    try
        _ = versions:list_compare(BV, [17,0]),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end,
    try
        _ = versions:list_to_string(BV),
        error(unexpected_success)
    catch
        error:badarg ->
            ok
    end.


valid_versions(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    {ok, Content} = file:read_file(filename:join([DataDir, "otp_versions.table"])),
    lists:foldl(fun (~":", Acc) ->
                        Acc;
                    (~"#", Acc) ->
                        Acc;
                    (~":\n", Acc) ->
                        Acc;
                    (AppVer, Acc) ->
                        [_, Ver] = binary:split(AppVer, ~"-"),
                        [Ver | Acc]
                end, [], binary:split(Content, <<" ">>, [global,trim])).


c2b(Cs) ->
    unicode:characters_to_binary(Cs).

c2l(Cs) ->
    unicode:characters_to_list(Cs).
