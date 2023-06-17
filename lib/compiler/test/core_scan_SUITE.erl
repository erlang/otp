%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
-module(core_scan_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 triple_quoted_string/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [triple_quoted_string].

groups() ->
    [].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

triple_quoted_string(Config) when is_list(Config) ->
    {ok,[{string,1,""}],2} =
        core_scan:string("\"\"\"\n"
                         "\"\"\""),

    {ok,[{string,1,"\\\"\\\"\\\""}],3} =
        core_scan:string("\"\"\"\n"
                         "\\\"\\\"\\\"\n"
                         "\"\"\""),

    {ok,[{string,1,"\n"}],4} =
        core_scan:string("\"\"\"\n"
                         "\n\n"
                         "\"\"\""),

    {ok,[{string,1,"this is a\nvery long\nstring"}],5} =
        core_scan:string("\"\"\"\n"
                         "this is a\n"
                         "very long\n"
                         "string\n"
                         "\"\"\""),

    {ok,[{string,1,"this is a\r\nvery long\r\nstring"}],5} =
        core_scan:string("\"\"\"\r\n"
                         "this is a\r\n"
                         "very long\r\n"
                         "string\r\n"
                         "\"\"\""),

    {ok,[{string,1,"  this is a\n    very long\n  string"}],5} =
        core_scan:string("\"\"\"\n"
                         "  this is a\n"
                         "    very long\n"
                         "  string\n"
                         "\"\"\""),

    {ok,[{string,1,"this contains \"quotes\"\n"
                   "and \"\"\"triple quotes\"\"\" and\n"
                   "ends here"}],5} =
        core_scan:string("\"\"\"\n"
                         "this contains \"quotes\"\n"
                         "and \"\"\"triple quotes\"\"\" and\n"
                         "ends here\n"
                         "\"\"\""),

    {ok,[{string,1,"```erlang\n"
                   "foo() ->\n"
                   "    \"\"\"\n"
                   "    foo\n"
                   "    bar\n"
                   "    \"\"\".\n"
                   "```"}],9} =
        core_scan:string("\"\"\"\"\n"
                         "```erlang\n"
                         "foo() ->\n"
                         "    \"\"\"\n"
                         "    foo\n"
                         "    bar\n"
                         "    \"\"\".\n"
                         "```\n"
                         "\"\"\"\""),

    {error,{1,core_scan,{triple_quoted_string,syntax}},2} =
        core_scan:string("\"\"\"foo\n"
                         "\"\"\""),

    {error,{1,core_scan,{triple_quoted_string,outdented}},3} =
        core_scan:string("\"\"\"\n"
                         "foo\n"
                         "  \"\"\""),

    {error,{1,core_scan,{triple_quoted_string,eof}},2} =
        core_scan:string("\"\"\"\n"
                         "foo\""),

    ok.
