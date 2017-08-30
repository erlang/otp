
%%----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : xmerl_sax_std_SUITE.erl
%% Created : 2009-06-01
%%----------------------------------------------------------------------
-module(xmerl_sax_std_SUITE).
-compile(export_all).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Initializations
%%----------------------------------------------------------------------

init_per_suite(Config) ->
    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
    ok=erl_tar:extract("ibm.tgz",[compressed]),
    ok=erl_tar:extract("japanese.tgz",[compressed]),
    ok=erl_tar:extract("oasis.tgz",[compressed]),
    ok=erl_tar:extract("sun.tgz",[compressed]),
    ok=erl_tar:extract("xmltest.tgz",[compressed]),
    ok = change_mode(["ibm","japanese","oasis",
                      "sun","xmltest"]),
    Config.

-ifndef(dont_rm_test_dirs).

end_per_suite(Config) ->
    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
    ok=rm_files(["ibm","japanese","oasis","sun","xmltest"]),
    Config.

-else.

end_per_suite(Config) ->
    Config.

-endif.

%% initialization before each testcase
init_per_testcase(_TestCase,Config) ->
    io:format("Config:\n~p\n",[Config]),
    {ok, _} = file:read_file_info(filename:join([privdir(Config)])),
    code:add_patha(privdir(Config)),
    Config.
 
%% clean up after each testcase
end_per_testcase(_Func,_Config) ->
    ok.

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/001.xml
%% ID: not-wf-sa-001
%% Type: not-wf
%% Sections: 3.1 [41]
'not-wf-sa-001'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/001.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/002.xml
%% ID: not-wf-sa-002
%% Type: not-wf
%% Sections: 2.3 [4]
'not-wf-sa-002'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/002.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/003.xml
%% ID: not-wf-sa-003
%% Type: not-wf
%% Sections: 2.6 [16]
'not-wf-sa-003'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/003.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/004.xml
%% ID: not-wf-sa-004
%% Type: not-wf
%% Sections: 2.6 [16]
'not-wf-sa-004'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/004.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/005.xml
%% ID: not-wf-sa-005
%% Type: not-wf
%% Sections: 2.6 [16]
'not-wf-sa-005'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/005.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/006.xml
%% ID: not-wf-sa-006
%% Type: not-wf
%% Sections: 2.5 [16]
'not-wf-sa-006'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/006.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/007.xml
%% ID: not-wf-sa-007
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-007'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/007.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/008.xml
%% ID: not-wf-sa-008
%% Type: not-wf
%% Sections: 2.3 [5]
'not-wf-sa-008'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/008.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/009.xml
%% ID: not-wf-sa-009
%% Type: not-wf
%% Sections: 4.1 [66]
'not-wf-sa-009'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/009.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/010.xml
%% ID: not-wf-sa-010
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-010'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/010.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/011.xml
%% ID: not-wf-sa-011
%% Type: not-wf
%% Sections: 3.1 [41]
'not-wf-sa-011'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/011.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/012.xml
%% ID: not-wf-sa-012
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-012'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/012.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/013.xml
%% ID: not-wf-sa-013
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-013'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/013.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/014.xml
%% ID: not-wf-sa-014
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-014'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/014.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/015.xml
%% ID: not-wf-sa-015
%% Type: not-wf
%% Sections: 3.1 [41]
'not-wf-sa-015'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/015.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/016.xml
%% ID: not-wf-sa-016
%% Type: not-wf
%% Sections: 3.1 [41]
'not-wf-sa-016'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/016.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/017.xml
%% ID: not-wf-sa-017
%% Type: not-wf
%% Sections: 2.7 [18]
'not-wf-sa-017'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/017.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/018.xml
%% ID: not-wf-sa-018
%% Type: not-wf
%% Sections: 2.7 [19]
'not-wf-sa-018'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/018.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/019.xml
%% ID: not-wf-sa-019
%% Type: not-wf
%% Sections: 3.1 [42]
'not-wf-sa-019'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/019.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/020.xml
%% ID: not-wf-sa-020
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-020'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/020.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/021.xml
%% ID: not-wf-sa-021
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-021'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/021.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/022.xml
%% ID: not-wf-sa-022
%% Type: not-wf
%% Sections: 4.1 [66]
'not-wf-sa-022'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/022.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/023.xml
%% ID: not-wf-sa-023
%% Type: not-wf
%% Sections: 2.3 [5]
'not-wf-sa-023'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/023.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/024.xml
%% ID: not-wf-sa-024
%% Type: not-wf
%% Sections: 2.3 [5]
'not-wf-sa-024'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/024.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/025.xml
%% ID: not-wf-sa-025
%% Type: not-wf
%% Sections: 2.4 [14]
'not-wf-sa-025'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/025.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/026.xml
%% ID: not-wf-sa-026
%% Type: not-wf
%% Sections: 2.4 [14]
'not-wf-sa-026'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/026.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/027.xml
%% ID: not-wf-sa-027
%% Type: not-wf
%% Sections: 2.5 [15]
'not-wf-sa-027'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/027.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/028.xml
%% ID: not-wf-sa-028
%% Type: not-wf
%% Sections: 2.6 [16]
'not-wf-sa-028'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/028.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/029.xml
%% ID: not-wf-sa-029
%% Type: not-wf
%% Sections: 2.4 [14]
'not-wf-sa-029'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/029.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/030.xml
%% ID: not-wf-sa-030
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-030'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/030.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/031.xml
%% ID: not-wf-sa-031
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-031'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/031.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/032.xml
%% ID: not-wf-sa-032
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-032'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/032.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/033.xml
%% ID: not-wf-sa-033
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-033'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/033.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/034.xml
%% ID: not-wf-sa-034
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-034'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/034.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/035.xml
%% ID: not-wf-sa-035
%% Type: not-wf
%% Sections: 3.1 [43]
'not-wf-sa-035'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/035.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/036.xml
%% ID: not-wf-sa-036
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-036'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/036.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"Illegal data\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/037.xml
%% ID: not-wf-sa-037
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-037'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/037.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"&#32;\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/038.xml
%% ID: not-wf-sa-038
%% Type: not-wf
%% Sections: 3.1
'not-wf-sa-038'(Config) -> 
    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/038.xml"]),
    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/039.xml
%% ID: not-wf-sa-039
%% Type: not-wf
%% Sections: 3
'not-wf-sa-039'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/039.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/040.xml
%% ID: not-wf-sa-040
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-040'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/040.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"<doc></doc>\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/041.xml
%% ID: not-wf-sa-041
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-041'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/041.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"<doc></doc>\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/042.xml
%% ID: not-wf-sa-042
%% Type: not-wf
%% Sections: 3.1 [42]
'not-wf-sa-042'(Config) ->  {skip, "Fix 1"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/042.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/043.xml
%% ID: not-wf-sa-043
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-043'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/043.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"Illegal data\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/044.xml
%% ID: not-wf-sa-044
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-044'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/044.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"<doc/>\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/045.xml
%% ID: not-wf-sa-045
%% Type: not-wf
%% Sections: 3.1 [44]
'not-wf-sa-045'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/045.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/046.xml
%% ID: not-wf-sa-046
%% Type: not-wf
%% Sections: 3.1 [40]
'not-wf-sa-046'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/046.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/047.xml
%% ID: not-wf-sa-047
%% Type: not-wf
%% Sections: 3.1 [44]
'not-wf-sa-047'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/047.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/048.xml
%% ID: not-wf-sa-048
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-048'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/048.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"<![CDATA[]]>\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/049.xml
%% ID: not-wf-sa-049
%% Type: not-wf
%% Sections: 3.1 [40]
'not-wf-sa-049'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/049.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/050.xml
%% ID: not-wf-sa-050
%% Type: not-wf
%% Sections: 2.1 [1]
'not-wf-sa-050'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/050.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/051.xml
%% ID: not-wf-sa-051
%% Type: not-wf
%% Sections: 2.7 [18]
'not-wf-sa-051'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/051.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/052.xml
%% ID: not-wf-sa-052
%% Type: not-wf
%% Sections: 4.1 [66]
'not-wf-sa-052'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/052.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/053.xml
%% ID: not-wf-sa-053
%% Type: not-wf
%% Sections: 3.1 [42]
'not-wf-sa-053'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/053.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/054.xml
%% ID: not-wf-sa-054
%% Type: not-wf
%% Sections: 4.2.2 [75]
'not-wf-sa-054'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/054.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/055.xml
%% ID: not-wf-sa-055
%% Type: not-wf
%% Sections: 2.8 [28]
'not-wf-sa-055'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/055.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/056.xml
%% ID: not-wf-sa-056
%% Type: not-wf
%% Sections: 2.8 [28]
'not-wf-sa-056'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/056.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/057.xml
%% ID: not-wf-sa-057
%% Type: not-wf
%% Sections: 3.2 [45]
'not-wf-sa-057'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/057.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/058.xml
%% ID: not-wf-sa-058
%% Type: not-wf
%% Sections: 3.3.1 [54]
'not-wf-sa-058'(_Config) -> {skip, "Attlist Notation parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/058.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/059.xml
%% ID: not-wf-sa-059
%% Type: not-wf
%% Sections: 3.3.1 [59]
'not-wf-sa-059'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/059.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/060.xml
%% ID: not-wf-sa-060
%% Type: not-wf
%% Sections: 3.3.1 [56]
'not-wf-sa-060'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/060.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/061.xml
%% ID: not-wf-sa-061
%% Type: not-wf
%% Sections: 4.2.2 [75]
'not-wf-sa-061'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/061.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/062.xml
%% ID: not-wf-sa-062
%% Type: not-wf
%% Sections: 4.2 [71]
'not-wf-sa-062'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/062.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/063.xml
%% ID: not-wf-sa-063
%% Type: not-wf
%% Sections: 2.8 [29]
'not-wf-sa-063'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/063.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/064.xml
%% ID: not-wf-sa-064
%% Type: not-wf
%% Sections: 3.3 [53]
'not-wf-sa-064'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/064.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/065.xml
%% ID: not-wf-sa-065
%% Type: not-wf
%% Sections: 3.3 [53]
'not-wf-sa-065'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/065.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/066.xml
%% ID: not-wf-sa-066
%% Type: not-wf
%% Sections: 3.3 [52]
'not-wf-sa-066'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/066.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/067.xml
%% ID: not-wf-sa-067
%% Type: not-wf
%% Sections: 3.3 [53]
'not-wf-sa-067'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/067.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/068.xml
%% ID: not-wf-sa-068
%% Type: not-wf
%% Sections: 3.3.1 [58]
'not-wf-sa-068'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/068.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/069.xml
%% ID: not-wf-sa-069
%% Type: not-wf
%% Sections: 4.2.2 [76]
'not-wf-sa-069'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/069.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/070.xml
%% ID: not-wf-sa-070
%% Type: not-wf
%% Sections: 2.5 [16]
'not-wf-sa-070'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/070.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/071.xml
%% ID: not-wf-sa-071
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-071'(_Config) -> {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/071.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/072.xml
%% ID: not-wf-sa-072
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-072'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/072.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/073.xml
%% ID: not-wf-sa-073
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-073'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/073.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/074.xml
%% ID: not-wf-sa-074
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-074'(_Config) -> {skip, "Entity not correct tag pair NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/074.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/075.xml
%% ID: not-wf-sa-075
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-075'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/075.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/076.xml
%% ID: not-wf-sa-076
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-076'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/076.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/077.xml
%% ID: not-wf-sa-077
%% Type: not-wf
%% Sections: 41. [68]
'not-wf-sa-077'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/077.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/078.xml
%% ID: not-wf-sa-078
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-078'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/078.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/079.xml
%% ID: not-wf-sa-079
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-079'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/079.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/080.xml
%% ID: not-wf-sa-080
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-080'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/080.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/081.xml
%% ID: not-wf-sa-081
%% Type: not-wf
%% Sections: 3.1
'not-wf-sa-081'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/081.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/082.xml
%% ID: not-wf-sa-082
%% Type: not-wf
%% Sections: 3.1
'not-wf-sa-082'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/082.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/083.xml
%% ID: not-wf-sa-083
%% Type: not-wf
%% Sections: 4.2.2 [76]
'not-wf-sa-083'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/083.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/084.xml
%% ID: not-wf-sa-084
%% Type: not-wf
%% Sections: 4.1
'not-wf-sa-084'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/084.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/085.xml
%% ID: not-wf-sa-085
%% Type: not-wf
%% Sections: 2.3 [13]
'not-wf-sa-085'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/085.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/086.xml
%% ID: not-wf-sa-086
%% Type: not-wf
%% Sections: 2.3 [13]
'not-wf-sa-086'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/086.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/087.xml
%% ID: not-wf-sa-087
%% Type: not-wf
%% Sections: 2.3 [13]
'not-wf-sa-087'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/087.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/088.xml
%% ID: not-wf-sa-088
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-088'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/088.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/089.xml
%% ID: not-wf-sa-089
%% Type: not-wf
%% Sections: 4.2 [74]
'not-wf-sa-089'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/089.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/090.xml
%% ID: not-wf-sa-090
%% Type: not-wf
%% Sections: 2.3 [10]
'not-wf-sa-090'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/090.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/091.xml
%% ID: not-wf-sa-091
%% Type: not-wf
%% Sections: 4.2 [74]
'not-wf-sa-091'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/091.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/092.xml
%% ID: not-wf-sa-092
%% Type: not-wf
%% Sections: 4.5
'not-wf-sa-092'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/092.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/093.xml
%% ID: not-wf-sa-093
%% Type: not-wf
%% Sections: 4.1 [66]
'not-wf-sa-093'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/093.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/094.xml
%% ID: not-wf-sa-094
%% Type: not-wf
%% Sections: 2.8 [24]
'not-wf-sa-094'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/094.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/095.xml
%% ID: not-wf-sa-095
%% Type: not-wf
%% Sections: 2.8 [23]
'not-wf-sa-095'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/095.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/096.xml
%% ID: not-wf-sa-096
%% Type: not-wf
%% Sections: 2.9 [32]
'not-wf-sa-096'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/096.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/097.xml
%% ID: not-wf-sa-097
%% Type: not-wf
%% Sections: 2.8 [24]
'not-wf-sa-097'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/097.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/098.xml
%% ID: not-wf-sa-098
%% Type: not-wf
%% Sections: 2.8 [23]
'not-wf-sa-098'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/098.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/099.xml
%% ID: not-wf-sa-099
%% Type: not-wf
%% Sections: 2.8 [23]
'not-wf-sa-099'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/099.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/100.xml
%% ID: not-wf-sa-100
%% Type: not-wf
%% Sections: 2.9 [32]
'not-wf-sa-100'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/100.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/101.xml
%% ID: not-wf-sa-101
%% Type: not-wf
%% Sections: 4.3.3 [81]
'not-wf-sa-101'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/101.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/102.xml
%% ID: not-wf-sa-102
%% Type: not-wf
%% Sections: 2.8 [26]
'not-wf-sa-102'(Config) ->  {skip, "Fix 2"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/102.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/103.xml
%% ID: not-wf-sa-103
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-103'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/103.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/104.xml
%% ID: not-wf-sa-104
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-104'(_Config) -> {skip, "Entity not correct tag pair NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/104.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/105.xml
%% ID: not-wf-sa-105
%% Type: not-wf
%% Sections: 2.7 
'not-wf-sa-105'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/105.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/106.xml
%% ID: not-wf-sa-106
%% Type: not-wf
%% Sections: 4.2
'not-wf-sa-106'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/106.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/107.xml
%% ID: not-wf-sa-107
%% Type: not-wf
%% Sections: 2.8 [28]
'not-wf-sa-107'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/107.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/108.xml
%% ID: not-wf-sa-108
%% Type: not-wf
%% Sections: 2.7 [19]
'not-wf-sa-108'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/108.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/109.xml
%% ID: not-wf-sa-109
%% Type: not-wf
%% Sections: 4.2 [70]
'not-wf-sa-109'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/109.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/110.xml
%% ID: not-wf-sa-110
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-110'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/110.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"&e;\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/111.xml
%% ID: not-wf-sa-111
%% Type: not-wf
%% Sections: 3.1 [43]
'not-wf-sa-111'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/111.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/112.xml
%% ID: not-wf-sa-112
%% Type: not-wf
%% Sections: 2.7 [19]
'not-wf-sa-112'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/112.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/113.xml
%% ID: not-wf-sa-113
%% Type: not-wf
%% Sections: 2.3 [9]
'not-wf-sa-113'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/113.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/114.xml
%% ID: not-wf-sa-114
%% Type: not-wf
%% Sections: 2.3 [9]
'not-wf-sa-114'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/114.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/115.xml
%% ID: not-wf-sa-115
%% Type: not-wf
%% Sections: 4.5
'not-wf-sa-115'(_Config) -> {skip, "& expansion not correct"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/115.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/116.xml
%% ID: not-wf-sa-116
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-116'(_Config) -> {skip, "& expansion not correct"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/116.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/117.xml
%% ID: not-wf-sa-117
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-117'(_Config) -> {skip, "& expansion not correct"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/117.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/118.xml
%% ID: not-wf-sa-118
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-118'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/118.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/119.xml
%% ID: not-wf-sa-119
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-119'(_Config) -> {skip, "& expansion not correct"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/119.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/120.xml
%% ID: not-wf-sa-120
%% Type: not-wf
%% Sections: 4.5
'not-wf-sa-120'(_Config) -> {skip, "& expansion not correct"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/120.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/121.xml
%% ID: not-wf-sa-121
%% Type: not-wf
%% Sections: 4.1 [68]
'not-wf-sa-121'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/121.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/122.xml
%% ID: not-wf-sa-122
%% Type: not-wf
%% Sections: 3.2.1 [47]
'not-wf-sa-122'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/122.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/123.xml
%% ID: not-wf-sa-123
%% Type: not-wf
%% Sections: 3.2.1 [48]
'not-wf-sa-123'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/123.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/124.xml
%% ID: not-wf-sa-124
%% Type: not-wf
%% Sections: 3.2.2 [51]
'not-wf-sa-124'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/124.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/125.xml
%% ID: not-wf-sa-125
%% Type: not-wf
%% Sections: 3.2.2 [51]
'not-wf-sa-125'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/125.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/126.xml
%% ID: not-wf-sa-126
%% Type: not-wf
%% Sections: 3.2.2 [51]
'not-wf-sa-126'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/126.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/127.xml
%% ID: not-wf-sa-127
%% Type: not-wf
%% Sections: 3.2.2 [51]
'not-wf-sa-127'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/127.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/128.xml
%% ID: not-wf-sa-128
%% Type: not-wf
%% Sections: 2.7 [18]
'not-wf-sa-128'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/128.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/129.xml
%% ID: not-wf-sa-129
%% Type: not-wf
%% Sections: 3.2 [45]
'not-wf-sa-129'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/129.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/130.xml
%% ID: not-wf-sa-130
%% Type: not-wf
%% Sections: 3.2 [45]
'not-wf-sa-130'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/130.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/131.xml
%% ID: not-wf-sa-131
%% Type: not-wf
%% Sections: 3.2 [45]
'not-wf-sa-131'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/131.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/132.xml
%% ID: not-wf-sa-132
%% Type: not-wf
%% Sections: 3.2.1 [50]
'not-wf-sa-132'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/132.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/133.xml
%% ID: not-wf-sa-133
%% Type: not-wf
%% Sections: 3.2.1
'not-wf-sa-133'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/133.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/134.xml
%% ID: not-wf-sa-134
%% Type: not-wf
%% Sections: 3.2.1
'not-wf-sa-134'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/134.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/135.xml
%% ID: not-wf-sa-135
%% Type: not-wf
%% Sections: 3.2.1 [47]
'not-wf-sa-135'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/135.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/136.xml
%% ID: not-wf-sa-136
%% Type: not-wf
%% Sections: 3.2 [45]
'not-wf-sa-136'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/136.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/137.xml
%% ID: not-wf-sa-137
%% Type: not-wf
%% Sections: 3.2 [45]
'not-wf-sa-137'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/137.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/138.xml
%% ID: not-wf-sa-138
%% Type: not-wf
%% Sections: 3.2.1 [48]
'not-wf-sa-138'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/138.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/139.xml
%% ID: not-wf-sa-139
%% Type: not-wf
%% Sections: 3.2.1 [46]
'not-wf-sa-139'(_Config) ->  {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/139.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/140.xml
%% ID: not-wf-sa-140
%% Type: not-wf
%% Sections: 2.3 [4]
'not-wf-sa-140'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/140.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/141.xml
%% ID: not-wf-sa-141
%% Type: not-wf
%% Sections: 2.3 [5]
'not-wf-sa-141'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/141.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/142.xml
%% ID: not-wf-sa-142
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-142'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/142.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/143.xml
%% ID: not-wf-sa-143
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-143'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/143.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/144.xml
%% ID: not-wf-sa-144
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-144'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/144.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/145.xml
%% ID: not-wf-sa-145
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-145'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/145.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/146.xml
%% ID: not-wf-sa-146
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-146'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/146.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/147.xml
%% ID: not-wf-sa-147
%% Type: not-wf
%% Sections: 2.8 [22]
'not-wf-sa-147'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/147.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/148.xml
%% ID: not-wf-sa-148
%% Type: not-wf
%% Sections: 2.8 [22]
'not-wf-sa-148'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/148.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/149.xml
%% ID: not-wf-sa-149
%% Type: not-wf
%% Sections: 2.8 [28]
'not-wf-sa-149'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/149.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/150.xml
%% ID: not-wf-sa-150
%% Type: not-wf
%% Sections: 3.1 [43]
'not-wf-sa-150'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/150.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/151.xml
%% ID: not-wf-sa-151
%% Type: not-wf
%% Sections: 2.8 [27]
'not-wf-sa-151'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/151.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"<?xml version=\"1.0\"?>\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   % R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   % check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/152.xml
%% ID: not-wf-sa-152
%% Type: not-wf
%% Sections: 2.8 [22]
'not-wf-sa-152'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/152.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/153.xml
%% ID: not-wf-sa-153
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-153'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/153.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/154.xml
%% ID: not-wf-sa-154
%% Type: not-wf
%% Sections: 2.8 2.6 [23, 17]
'not-wf-sa-154'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/154.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/155.xml
%% ID: not-wf-sa-155
%% Type: not-wf
%% Sections: 2.8 2.6 [23, 17]
'not-wf-sa-155'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/155.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/156.xml
%% ID: not-wf-sa-156
%% Type: not-wf
%% Sections: 2.8 2.6 [23, 17]
'not-wf-sa-156'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/156.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/157.xml
%% ID: not-wf-sa-157
%% Type: not-wf
%% Sections: 2.6 [17]
'not-wf-sa-157'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/157.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/158.xml
%% ID: not-wf-sa-158
%% Type: not-wf
%% Sections: 3.3 [52]
'not-wf-sa-158'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/158.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/159.xml
%% ID: not-wf-sa-159
%% Type: not-wf
%% Sections: 2.3 [9]
'not-wf-sa-159'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/159.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/160.xml
%% ID: not-wf-sa-160
%% Type: not-wf
%% Sections: 2.8
'not-wf-sa-160'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/160.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/161.xml
%% ID: not-wf-sa-161
%% Type: not-wf
%% Sections: 2.8
'not-wf-sa-161'(_Config) -> {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/161.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/162.xml
%% ID: not-wf-sa-162
%% Type: not-wf
%% Sections: 2.8
'not-wf-sa-162'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/162.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/163.xml
%% ID: not-wf-sa-163
%% Type: not-wf
%% Sections: 4.1 [69]
'not-wf-sa-163'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/163.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/164.xml
%% ID: not-wf-sa-164
%% Type: not-wf
%% Sections: 4.1 [69]
'not-wf-sa-164'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/164.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/165.xml
%% ID: not-wf-sa-165
%% Type: not-wf
%% Sections: 4.2 [72]
'not-wf-sa-165'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/165.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/166.xml
%% ID: not-wf-sa-166
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-166'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/166.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/167.xml
%% ID: not-wf-sa-167
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-167'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/167.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/168.xml
%% ID: not-wf-sa-168
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-168'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/168.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/169.xml
%% ID: not-wf-sa-169
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-169'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/169.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/170.xml
%% ID: not-wf-sa-170
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-170'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/170.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/171.xml
%% ID: not-wf-sa-171
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-171'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/171.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/172.xml
%% ID: not-wf-sa-172
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-172'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/172.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/173.xml
%% ID: not-wf-sa-173
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-173'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/173.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/174.xml
%% ID: not-wf-sa-174
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-174'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/174.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/175.xml
%% ID: not-wf-sa-175
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-175'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/175.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/176.xml
%% ID: not-wf-sa-176
%% Type: not-wf
%% Sections: 3 [39]
'not-wf-sa-176'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/176.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/177.xml
%% ID: not-wf-sa-177
%% Type: not-wf
%% Sections: 2.2 [2]
'not-wf-sa-177'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/177.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/178.xml
%% ID: not-wf-sa-178
%% Type: not-wf
%% Sections: 3.1 [41]
'not-wf-sa-178'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/178.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/179.xml
%% ID: not-wf-sa-179
%% Type: not-wf
%% Sections: 4.1 [66]
'not-wf-sa-179'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/179.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/180.xml
%% ID: not-wf-sa-180
%% Type: not-wf
%% Sections: 4.1
'not-wf-sa-180'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/180.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/181.xml
%% ID: not-wf-sa-181
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-181'(_Config) -> {skip, "Entity not tag pair NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/181.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/182.xml
%% ID: not-wf-sa-182
%% Type: not-wf
%% Sections: 4.3.2
'not-wf-sa-182'(_Config) -> {skip, "Entity not tag pair NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/182.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/183.xml
%% ID: not-wf-sa-183
%% Type: not-wf
%% Sections: 3.2.2 [51]
'not-wf-sa-183'(_Config) -> {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/183.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/184.xml
%% ID: not-wf-sa-184
%% Type: not-wf
%% Sections: 3.2.2 [51]
'not-wf-sa-184'(_Config) -> {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/184.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/185.xml
%% ID: not-wf-sa-185
%% Type: not-wf
%% Sections: 4.1
'not-wf-sa-185'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/185.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sa/186.xml
%% ID: not-wf-sa-186
%% Type: not-wf
%% Sections: 3.1 [44]
'not-wf-sa-186'(Config) ->  {skip, "Fix 2"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/sa/186.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/001.xml
%% ID: not-wf-not-sa-001
%% Type: not-wf
%% Sections: 3.4 [62]
'not-wf-not-sa-001'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/001.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/002.xml
%% ID: not-wf-not-sa-002
%% Type: not-wf
%% Sections: 2.6 [17]
'not-wf-not-sa-002'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/002.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/003.xml
%% ID: not-wf-not-sa-003
%% Type: not-wf
%% Sections: 3.4 [62]
'not-wf-not-sa-003'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/003.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/004.xml
%% ID: not-wf-not-sa-004
%% Type: not-wf
%% Sections: 3.4 [62]
'not-wf-not-sa-004'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/004.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/005.xml
%% ID: not-wf-not-sa-005
%% Type: error
%% Sections: 4.1
'not-wf-not-sa-005'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/005.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/006.xml
%% ID: not-wf-not-sa-006
%% Type: not-wf
%% Sections: 3.4 [62]
'not-wf-not-sa-006'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/006.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/007.xml
%% ID: not-wf-not-sa-007
%% Type: not-wf
%% Sections: 4.3.2 [79]
'not-wf-not-sa-007'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/007.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/008.xml
%% ID: not-wf-not-sa-008
%% Type: not-wf
%% Sections: 4.1 [69]
'not-wf-not-sa-008'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/008.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa/009.xml
%% ID: not-wf-not-sa-009
%% Type: not-wf
%% Sections: 2.8
'not-wf-not-sa-009'(_Config) -> {skip, "not a complete content in PE NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/not-sa/009.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/ext-sa/001.xml
%% ID: not-wf-ext-sa-001
%% Type: not-wf
%% Sections: 4.1
'not-wf-ext-sa-001'(Config) ->  {skip, "Fix 1"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/ext-sa/001.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/ext-sa/002.xml
%% ID: not-wf-ext-sa-002
%% Type: not-wf
%% Sections: 4.3.1 4.3.2 [77, 78]
'not-wf-ext-sa-002'(Config) -> {skip, "Fix 1"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/ext-sa/002.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/ext-sa/003.xml
%% ID: not-wf-ext-sa-003
%% Type: not-wf
%% Sections: 2.6 [17]
'not-wf-ext-sa-003'(Config) -> {skip, "Fix 1"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","not-wf/ext-sa/003.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/002.xml
%% ID: invalid--002
%% Type: invalid
%% Sections: 3.2.1
'invalid--002'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","invalid/002.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/005.xml
%% ID: invalid--005
%% Type: invalid
%% Sections: 2.8
'invalid--005'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","invalid/005.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/006.xml
%% ID: invalid--006
%% Type: invalid
%% Sections: 2.8
'invalid--006'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","invalid/006.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa/022.xml
%% ID: invalid-not-sa-022
%% Type: invalid
%% Sections: 3.4 [62]
'invalid-not-sa-022'(_Config) -> {skip, "DTD element content parsing NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","invalid/not-sa/022.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/001.xml
%% ID: valid-sa-001
%% Type: valid
%% Sections: 3.2.2 [51]
'valid-sa-001'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/001.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/002.xml
%% ID: valid-sa-002
%% Type: valid
%% Sections: 3.1 [40]
'valid-sa-002'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/002.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/003.xml
%% ID: valid-sa-003
%% Type: valid
%% Sections: 3.1 [42]
'valid-sa-003'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/003.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/004.xml
%% ID: valid-sa-004
%% Type: valid
%% Sections: 3.1 [41]
'valid-sa-004'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/004.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/005.xml
%% ID: valid-sa-005
%% Type: valid
%% Sections: 3.1 [40]
'valid-sa-005'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/005.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/006.xml
%% ID: valid-sa-006
%% Type: valid
%% Sections: 3.1 [41]
'valid-sa-006'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/006.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/007.xml
%% ID: valid-sa-007
%% Type: valid
%% Sections: 3.1 4.6 [43]
'valid-sa-007'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/007.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/008.xml
%% ID: valid-sa-008
%% Type: valid
%% Sections: 2.4 3.1 [43]
'valid-sa-008'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/008.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/009.xml
%% ID: valid-sa-009
%% Type: valid
%% Sections: 2.3 3.1 [43]
'valid-sa-009'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/009.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/010.xml
%% ID: valid-sa-010
%% Type: valid
%% Sections: 3.1 [40]
'valid-sa-010'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/010.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/011.xml
%% ID: valid-sa-011
%% Type: valid
%% Sections: 3.1 [40]
'valid-sa-011'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/011.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/012.xml
%% ID: valid-sa-012
%% Type: valid
%% Sections: 2.3 [4]
'valid-sa-012'(Config) ->  {skip, "Fix 1"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/012.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/013.xml
%% ID: valid-sa-013
%% Type: valid
%% Sections: 2.3 3.1 [13] [40]
'valid-sa-013'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/013.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/014.xml
%% ID: valid-sa-014
%% Type: valid
%% Sections: 2.3 3.1 [13] [40]
'valid-sa-014'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/014.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/015.xml
%% ID: valid-sa-015
%% Type: valid
%% Sections: 2.3 3.1 [13] [40]
'valid-sa-015'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/015.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/016.xml
%% ID: valid-sa-016
%% Type: valid
%% Sections: 2.6 3.1 [16] [43]
'valid-sa-016'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/016.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/017.xml
%% ID: valid-sa-017
%% Type: valid
%% Sections: 2.6 3.1 [16] [43]
'valid-sa-017'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/017.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/018.xml
%% ID: valid-sa-018
%% Type: valid
%% Sections: 2.7 3.1 [18] [43]
'valid-sa-018'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/018.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/019.xml
%% ID: valid-sa-019
%% Type: valid
%% Sections: 2.7 3.1 [18] [43]
'valid-sa-019'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/019.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/020.xml
%% ID: valid-sa-020
%% Type: valid
%% Sections: 2.7 3.1 [18] [43]
'valid-sa-020'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/020.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/021.xml
%% ID: valid-sa-021
%% Type: valid
%% Sections: 2.5 3.1 [15] [43]
'valid-sa-021'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/021.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/022.xml
%% ID: valid-sa-022
%% Type: valid
%% Sections: 2.5 3.1 [15] [43]
'valid-sa-022'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/022.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/023.xml
%% ID: valid-sa-023
%% Type: valid
%% Sections: 3.1 [43]
'valid-sa-023'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/023.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/024.xml
%% ID: valid-sa-024
%% Type: valid
%% Sections: 3.1 4.1 [43] [66]
'valid-sa-024'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/024.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/025.xml
%% ID: valid-sa-025
%% Type: valid
%% Sections: 3.2 [46]
'valid-sa-025'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/025.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/026.xml
%% ID: valid-sa-026
%% Type: valid
%% Sections: 3.2 [46]
'valid-sa-026'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/026.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/027.xml
%% ID: valid-sa-027
%% Type: valid
%% Sections: 3.2 [46]
'valid-sa-027'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/027.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/028.xml
%% ID: valid-sa-028
%% Type: valid
%% Sections: 2.8 [24]
'valid-sa-028'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/028.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/029.xml
%% ID: valid-sa-029
%% Type: valid
%% Sections: 2.8 [24]
'valid-sa-029'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/029.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/030.xml
%% ID: valid-sa-030
%% Type: valid
%% Sections: 2.8 [25]
'valid-sa-030'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/030.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/031.xml
%% ID: valid-sa-031
%% Type: valid
%% Sections: 4.3.3 [80]
'valid-sa-031'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/031.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/032.xml
%% ID: valid-sa-032
%% Type: valid
%% Sections: 2.9 [32]
'valid-sa-032'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/032.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/033.xml
%% ID: valid-sa-033
%% Type: valid
%% Sections: 2.8 [23]
'valid-sa-033'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/033.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/034.xml
%% ID: valid-sa-034
%% Type: valid
%% Sections: 3.1 [44]
'valid-sa-034'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/034.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/035.xml
%% ID: valid-sa-035
%% Type: valid
%% Sections: 3.1 [44]
'valid-sa-035'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/035.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/036.xml
%% ID: valid-sa-036
%% Type: valid
%% Sections: 2.6 [16]
'valid-sa-036'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/036.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/037.xml
%% ID: valid-sa-037
%% Type: valid
%% Sections: 2.6 [15]
'valid-sa-037'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/037.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/038.xml
%% ID: valid-sa-038
%% Type: valid
%% Sections: 2.6 [15]
'valid-sa-038'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/038.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/039.xml
%% ID: valid-sa-039
%% Type: valid
%% Sections: 2.6 [16]
'valid-sa-039'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/039.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/040.xml
%% ID: valid-sa-040
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [54]
'valid-sa-040'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/040.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/041.xml
%% ID: valid-sa-041
%% Type: valid
%% Sections: 3.3.1 4.1 [54] [66]
'valid-sa-041'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/041.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/042.xml
%% ID: valid-sa-042
%% Type: valid
%% Sections: 3.3.1 4.1 [54] [66]
'valid-sa-042'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/042.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/043.xml
%% ID: valid-sa-043
%% Type: valid
%% Sections: 3.3
'valid-sa-043'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/043.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/044.xml
%% ID: valid-sa-044
%% Type: valid
%% Sections: 3.1 [44]
'valid-sa-044'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/044.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/045.xml
%% ID: valid-sa-045
%% Type: valid
%% Sections: 3.3 [52]
'valid-sa-045'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/045.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/046.xml
%% ID: valid-sa-046
%% Type: valid
%% Sections: 3.3 [52]
'valid-sa-046'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/046.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/047.xml
%% ID: valid-sa-047
%% Type: valid
%% Sections: 3.1 [43]
'valid-sa-047'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/047.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/048.xml
%% ID: valid-sa-048
%% Type: valid
%% Sections: 2.4 3.1 [14] [43]
'valid-sa-048'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/048.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/049.xml
%% ID: valid-sa-049
%% Type: valid
%% Sections: 2.2 [2]
'valid-sa-049'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/049.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/050.xml
%% ID: valid-sa-050
%% Type: valid
%% Sections: 2.2 [2]
'valid-sa-050'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/050.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/051.xml
%% ID: valid-sa-051
%% Type: valid
%% Sections: 2.2 [2]
'valid-sa-051'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/051.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/052.xml
%% ID: valid-sa-052
%% Type: valid
%% Sections: 2.2 [2]
'valid-sa-052'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/052.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/053.xml
%% ID: valid-sa-053
%% Type: valid
%% Sections: 4.4.2
'valid-sa-053'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/053.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/054.xml
%% ID: valid-sa-054
%% Type: valid
%% Sections: 3.1 [40] [42]
'valid-sa-054'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/054.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/055.xml
%% ID: valid-sa-055
%% Type: valid
%% Sections: 2.6 2.10 [16]
'valid-sa-055'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/055.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/056.xml
%% ID: valid-sa-056
%% Type: valid
%% Sections: 3.3.1 4.1 [54] [66]
'valid-sa-056'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/056.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/057.xml
%% ID: valid-sa-057
%% Type: valid
%% Sections: 3.2.1 [47]
'valid-sa-057'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/057.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/058.xml
%% ID: valid-sa-058
%% Type: valid
%% Sections: 3.3.3
'valid-sa-058'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/058.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/059.xml
%% ID: valid-sa-059
%% Type: valid
%% Sections: 3.2 3.3 [46] [53]
'valid-sa-059'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/059.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/060.xml
%% ID: valid-sa-060
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-060'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/060.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/061.xml
%% ID: valid-sa-061
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-061'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/061.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/062.xml
%% ID: valid-sa-062
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-062'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/062.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/063.xml
%% ID: valid-sa-063
%% Type: valid
%% Sections: 2.3 [5]
'valid-sa-063'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/063.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/064.xml
%% ID: valid-sa-064
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-064'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/064.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/065.xml
%% ID: valid-sa-065
%% Type: valid
%% Sections: 4.5
'valid-sa-065'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/065.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/066.xml
%% ID: valid-sa-066
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-066'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/066.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/067.xml
%% ID: valid-sa-067
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-067'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/067.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/068.xml
%% ID: valid-sa-068
%% Type: valid
%% Sections: 2.11, 4.5
'valid-sa-068'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/068.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/069.xml
%% ID: valid-sa-069
%% Type: valid
%% Sections: 4.7
'valid-sa-069'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/069.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/070.xml
%% ID: valid-sa-070
%% Type: valid
%% Sections: 4.4.8
'valid-sa-070'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/070.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/071.xml
%% ID: valid-sa-071
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [56]
'valid-sa-071'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/071.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/072.xml
%% ID: valid-sa-072
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [56]
'valid-sa-072'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/072.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/073.xml
%% ID: valid-sa-073
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [56]
'valid-sa-073'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/073.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/074.xml
%% ID: valid-sa-074
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [56]
'valid-sa-074'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/074.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/075.xml
%% ID: valid-sa-075
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [56]
'valid-sa-075'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/075.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/076.xml
%% ID: valid-sa-076
%% Type: valid
%% Sections: 3.3.1
'valid-sa-076'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/076.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/077.xml
%% ID: valid-sa-077
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [54]
'valid-sa-077'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/077.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/078.xml
%% ID: valid-sa-078
%% Type: valid
%% Sections: 3.3 3.3.1 [52] [54]
'valid-sa-078'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/078.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/079.xml
%% ID: valid-sa-079
%% Type: valid
%% Sections: 3.3 3.3.2 [52] [60]
'valid-sa-079'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/079.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/080.xml
%% ID: valid-sa-080
%% Type: valid
%% Sections: 3.3 3.3.2 [52] [60]
'valid-sa-080'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/080.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/081.xml
%% ID: valid-sa-081
%% Type: valid
%% Sections: 3.2.1 [50]
'valid-sa-081'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/081.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/082.xml
%% ID: valid-sa-082
%% Type: valid
%% Sections: 4.2 [72]
'valid-sa-082'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/082.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/083.xml
%% ID: valid-sa-083
%% Type: valid
%% Sections: 4.2 [72]
'valid-sa-083'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/083.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/084.xml
%% ID: valid-sa-084
%% Type: valid
%% Sections: 2.10
'valid-sa-084'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/084.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/085.xml
%% ID: valid-sa-085
%% Type: valid
%% Sections: 4
'valid-sa-085'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/085.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/086.xml
%% ID: valid-sa-086
%% Type: valid
%% Sections: 4.2
'valid-sa-086'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/086.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/087.xml
%% ID: valid-sa-087
%% Type: valid
%% Sections: 4.5
'valid-sa-087'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/087.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/088.xml
%% ID: valid-sa-088
%% Type: valid
%% Sections: 4.5
'valid-sa-088'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/088.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/089.xml
%% ID: valid-sa-089
%% Type: valid
%% Sections: 4.1 [66]
'valid-sa-089'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/089.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/090.xml
%% ID: valid-sa-090
%% Type: valid
%% Sections: 3.3.1
'valid-sa-090'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/090.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/091.xml
%% ID: valid-sa-091
%% Type: valid
%% Sections: 3.3.1
'valid-sa-091'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/091.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/092.xml
%% ID: valid-sa-092
%% Type: valid
%% Sections: 2.3 2.10
'valid-sa-092'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/092.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/093.xml
%% ID: valid-sa-093
%% Type: valid
%% Sections: 2.10
'valid-sa-093'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/093.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/094.xml
%% ID: valid-sa-094
%% Type: valid
%% Sections: 2.8
'valid-sa-094'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/094.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/095.xml
%% ID: valid-sa-095
%% Type: valid
%% Sections: 3.3.3
'valid-sa-095'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/095.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/096.xml
%% ID: valid-sa-096
%% Type: valid
%% Sections: 3.3.3
'valid-sa-096'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/096.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/097.xml
%% ID: valid-sa-097
%% Type: valid
%% Sections: 3.3
'valid-sa-097'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/097.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/098.xml
%% ID: valid-sa-098
%% Type: valid
%% Sections: 2.6 2.10 [16]
'valid-sa-098'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/098.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/099.xml
%% ID: valid-sa-099
%% Type: valid
%% Sections: 4.3.3 [81]
'valid-sa-099'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/099.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/100.xml
%% ID: valid-sa-100
%% Type: valid
%% Sections: 2.3 [12]
'valid-sa-100'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/100.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/101.xml
%% ID: valid-sa-101
%% Type: valid
%% Sections: 4.5
'valid-sa-101'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/101.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/102.xml
%% ID: valid-sa-102
%% Type: valid
%% Sections: 3.3.3
'valid-sa-102'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/102.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/103.xml
%% ID: valid-sa-103
%% Type: valid
%% Sections: 3.3.3
'valid-sa-103'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/103.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/104.xml
%% ID: valid-sa-104
%% Type: valid
%% Sections: 3.1 [40]
'valid-sa-104'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/104.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/105.xml
%% ID: valid-sa-105
%% Type: valid
%% Sections: 3.3.3
'valid-sa-105'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/105.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/106.xml
%% ID: valid-sa-106
%% Type: valid
%% Sections: 3.3.3
'valid-sa-106'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/106.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/107.xml
%% ID: valid-sa-107
%% Type: valid
%% Sections: 3.3.3
'valid-sa-107'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/107.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/108.xml
%% ID: valid-sa-108
%% Type: valid
%% Sections: 2.11, 3.3.3
'valid-sa-108'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/108.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/109.xml
%% ID: valid-sa-109
%% Type: valid
%% Sections: 2.3 3.1 [10][40][41]
'valid-sa-109'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/109.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/110.xml
%% ID: valid-sa-110
%% Type: valid
%% Sections: 3.3.3
'valid-sa-110'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/110.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/111.xml
%% ID: valid-sa-111
%% Type: valid
%% Sections: 3.3.3
'valid-sa-111'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/111.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/112.xml
%% ID: valid-sa-112
%% Type: valid
%% Sections: 3.2.1 [48][49]
'valid-sa-112'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/112.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/113.xml
%% ID: valid-sa-113
%% Type: valid
%% Sections: 3.3 [52][53]
'valid-sa-113'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/113.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/114.xml
%% ID: valid-sa-114
%% Type: valid
%% Sections: 2.7 [20]
'valid-sa-114'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/114.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/115.xml
%% ID: valid-sa-115
%% Type: valid
%% Sections: 3.3.3
'valid-sa-115'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/115.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/116.xml
%% ID: valid-sa-116
%% Type: valid
%% Sections: 2.11
'valid-sa-116'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/116.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/117.xml
%% ID: valid-sa-117
%% Type: valid
%% Sections: 4.5
'valid-sa-117'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/117.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/118.xml
%% ID: valid-sa-118
%% Type: valid
%% Sections: 4.5
'valid-sa-118'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/118.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa/119.xml
%% ID: valid-sa-119
%% Type: valid
%% Sections: 2.5
'valid-sa-119'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/sa/119.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/001.xml
%% ID: valid-not-sa-001
%% Type: valid
%% Sections: 4.2.2 [75]
'valid-not-sa-001'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/001.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/002.xml
%% ID: valid-not-sa-002
%% Type: valid
%% Sections: 4.2.2 [75]
'valid-not-sa-002'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/002.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/003.xml
%% ID: valid-not-sa-003
%% Type: valid
%% Sections: 4.1 [69]
'valid-not-sa-003'(_Config) -> {skip, "external entity NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/003.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/004.xml
%% ID: valid-not-sa-004
%% Type: valid
%% Sections: 4.1 [69]
'valid-not-sa-004'(_Config) -> {skip, "external entity NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/004.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/005.xml
%% ID: valid-not-sa-005
%% Type: valid
%% Sections: 4.1 [69]
'valid-not-sa-005'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/005.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/006.xml
%% ID: valid-not-sa-006
%% Type: valid
%% Sections: 3.3 [52]
'valid-not-sa-006'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/006.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/007.xml
%% ID: valid-not-sa-007
%% Type: valid
%% Sections: 3.3 [52]
'valid-not-sa-007'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/007.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/008.xml
%% ID: valid-not-sa-008
%% Type: valid
%% Sections: 4.2.2 [75]
'valid-not-sa-008'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/008.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/009.xml
%% ID: valid-not-sa-009
%% Type: valid
%% Sections: 4.2.2 [75]
'valid-not-sa-009'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/009.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/010.xml
%% ID: valid-not-sa-010
%% Type: valid
%% Sections: 3.3 [52]
'valid-not-sa-010'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/010.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/011.xml
%% ID: valid-not-sa-011
%% Type: valid
%% Sections: 4.2 4.2.1 [72] [75]
'valid-not-sa-011'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/011.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/012.xml
%% ID: valid-not-sa-012
%% Type: valid
%% Sections: 4.3.1 [77]
'valid-not-sa-012'(Config) -> {skip, "Fix 3"}.
  %%  file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %% Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/012.xml"]),
   %% R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %% check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/013.xml
%% ID: valid-not-sa-013
%% Type: valid
%% Sections: 3.4 [62]
'valid-not-sa-013'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/013.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/014.xml
%% ID: valid-not-sa-014
%% Type: valid
%% Sections: 3.4 [62]
'valid-not-sa-014'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/014.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/015.xml
%% ID: valid-not-sa-015
%% Type: valid
%% Sections: 3.4 [63]
'valid-not-sa-015'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/015.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/016.xml
%% ID: valid-not-sa-016
%% Type: valid
%% Sections: 3.4 [62]
'valid-not-sa-016'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/016.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/017.xml
%% ID: valid-not-sa-017
%% Type: valid
%% Sections: 4.2 [72]
'valid-not-sa-017'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/017.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/018.xml
%% ID: valid-not-sa-018
%% Type: valid
%% Sections: 4.2.2 [75]
'valid-not-sa-018'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/018.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/019.xml
%% ID: valid-not-sa-019
%% Type: valid
%% Sections: 4.4.8
'valid-not-sa-019'(_Config) -> {skip, "partly replacement of markupdecls"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/019.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/020.xml
%% ID: valid-not-sa-020
%% Type: valid
%% Sections: 4.4.8
'valid-not-sa-020'(_Config) -> {skip, "partly replacement of markupdecls"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/020.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/021.xml
%% ID: valid-not-sa-021
%% Type: valid
%% Sections: 4.2 [72]
'valid-not-sa-021'(_Config) -> {skip, "partly replacement of markupdecls"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/021.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/023.xml
%% ID: valid-not-sa-023
%% Type: valid
%% Sections: 2.3 4.1 [10] [69]
'valid-not-sa-023'(_Config) -> {skip, "partly replacement of markupdecls"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/023.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/024.xml
%% ID: valid-not-sa-024
%% Type: valid
%% Sections: 2.8, 4.1 [69]
'valid-not-sa-024'(_Config) -> {skip, "partly replacement of markupdecls"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/024.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/025.xml
%% ID: valid-not-sa-025
%% Type: valid
%% Sections: 4.2
'valid-not-sa-025'(_Config) -> {skip, "partly replacement of markupdecls"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/025.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/026.xml
%% ID: valid-not-sa-026
%% Type: valid
%% Sections: 3.3 [52]
'valid-not-sa-026'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/026.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/027.xml
%% ID: valid-not-sa-027
%% Type: valid
%% Sections: 4.1 [69]
'valid-not-sa-027'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/027.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/028.xml
%% ID: valid-not-sa-028
%% Type: valid
%% Sections: 3.4 [62]
'valid-not-sa-028'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/028.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/029.xml
%% ID: valid-not-sa-029
%% Type: valid
%% Sections: 3.4 [62]
'valid-not-sa-029'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/029.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/030.xml
%% ID: valid-not-sa-030
%% Type: valid
%% Sections: 3.4 [62]
'valid-not-sa-030'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/030.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa/031.xml
%% ID: valid-not-sa-031
%% Type: valid
%% Sections: 2.7
'valid-not-sa-031'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/not-sa/031.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/001.xml
%% ID: valid-ext-sa-001
%% Type: valid
%% Sections: 2.11
'valid-ext-sa-001'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/001.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/002.xml
%% ID: valid-ext-sa-002
%% Type: valid
%% Sections: 2.11
'valid-ext-sa-002'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/002.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/003.xml
%% ID: valid-ext-sa-003
%% Type: valid
%% Sections: 3.1 4.1 [43] [68]
'valid-ext-sa-003'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/003.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/004.xml
%% ID: valid-ext-sa-004
%% Type: valid
%% Sections: 2.11
'valid-ext-sa-004'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/004.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/005.xml
%% ID: valid-ext-sa-005
%% Type: valid
%% Sections: 3.2.1 4.2.2 [48] [75]
'valid-ext-sa-005'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/005.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/006.xml
%% ID: valid-ext-sa-006
%% Type: valid
%% Sections: 2.11 3.2.1 3.2.2 4.2.2 [48] [51] [75]
'valid-ext-sa-006'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/006.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/007.xml
%% ID: valid-ext-sa-007
%% Type: valid
%% Sections: 4.2.2 4.4.3 [75]
'valid-ext-sa-007'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/007.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/008.xml
%% ID: valid-ext-sa-008
%% Type: valid
%% Sections: 4.2.2 4.3.3. 4.4.3 [75] [80]
'valid-ext-sa-008'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/008.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/009.xml
%% ID: valid-ext-sa-009
%% Type: valid
%% Sections: 2.11
'valid-ext-sa-009'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/009.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/011.xml
%% ID: valid-ext-sa-011
%% Type: valid
%% Sections: 2.11 4.2.2 [75]
'valid-ext-sa-011'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/011.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/012.xml
%% ID: valid-ext-sa-012
%% Type: valid
%% Sections: 4.2.1 4.2.2
'valid-ext-sa-012'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/012.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/013.xml
%% ID: valid-ext-sa-013
%% Type: valid
%% Sections: 3.3.3
'valid-ext-sa-013'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/013.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext-sa/014.xml
%% ID: valid-ext-sa-014
%% Type: valid
%% Sections: 4.1 4.4.3 [68]
'valid-ext-sa-014'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"xmltest","valid/ext-sa/014.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: James Clark XMLTEST cases, 18-Nov-1998


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: James Clark  XML 1.0 Tests


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: pr-xml-euc-jp.xml
%% ID: pr-xml-euc-jp
%% Type: error
%% Sections: 4.3.3 [4,84]
'pr-xml-euc-jp'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","pr-xml-euc-jp.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: pr-xml-iso-2022-jp.xml
%% ID: pr-xml-iso-2022-jp
%% Type: error
%% Sections: 4.3.3 [4,84]
'pr-xml-iso-2022-jp'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","pr-xml-iso-2022-jp.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: pr-xml-little-endian.xml
%% ID: pr-xml-little
%% Type: valid
%% Sections: 4.3.3 [4,84]
'pr-xml-little'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","pr-xml-little-endian.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: pr-xml-shift_jis.xml
%% ID: pr-xml-shift_jis
%% Type: error
%% Sections: 4.3.3 [4,84]
'pr-xml-shift_jis'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","pr-xml-shift_jis.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: pr-xml-utf-16.xml
%% ID: pr-xml-utf-16
%% Type: valid
%% Sections: 4.3.3 [4,84]
'pr-xml-utf-16'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","pr-xml-utf-16.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: pr-xml-utf-8.xml
%% ID: pr-xml-utf-8
%% Type: valid
%% Sections: 4.3.3 [4,84]
'pr-xml-utf-8'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","pr-xml-utf-8.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: weekly-euc-jp.xml
%% ID: weekly-euc-jp
%% Type: error
%% Sections: 4.3.3 [4,84]
'weekly-euc-jp'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","weekly-euc-jp.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: weekly-iso-2022-jp.xml
%% ID: weekly-iso-2022-jp
%% Type: error
%% Sections: 4.3.3 [4,84]
'weekly-iso-2022-jp'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","weekly-iso-2022-jp.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: weekly-little-endian.xml
%% ID: weekly-little
%% Type: valid
%% Sections: 4.3.3 [4,84]
'weekly-little'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","weekly-little-endian.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: weekly-shift_jis.xml
%% ID: weekly-shift_jis
%% Type: error
%% Sections: 4.3.3 [4,84]
'weekly-shift_jis'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","weekly-shift_jis.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: weekly-utf-16.xml
%% ID: weekly-utf-16
%% Type: valid
%% Sections: 4.3.3 [4,84]
'weekly-utf-16'(Config) -> {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","weekly-utf-16.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: weekly-utf-8.xml
%% ID: weekly-utf-8
%% Type: valid
%% Sections: 4.3.3 [4,84]
'weekly-utf-8'(Config) -> {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"japanese","weekly-utf-8.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: Fuji Xerox Japanese Text Tests


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: Fuji Xerox Japanese Text Tests XML 1.0 Tests


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/pe01.xml
%% ID: pe01
%% Type: valid
%% Sections: 2.8
'pe01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/pe01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/dtd00.xml
%% ID: dtd00
%% Type: valid
%% Sections: 3.2.2 [51]
'dtd00'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/dtd00.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/dtd01.xml
%% ID: dtd01
%% Type: valid
%% Sections: 2.5 [15]
'dtd01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/dtd01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/element.xml
%% ID: element
%% Type: valid
%% Sections: 3
'element'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/element.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext01.xml
%% ID: ext01
%% Type: valid
%% Sections: 4.3.1 4.3.2 [77] [78]
'ext01'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/ext01.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/ext02.xml
%% ID: ext02
%% Type: valid
%% Sections: 4.3.2 [78]
'ext02'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/ext02.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa01.xml
%% ID: not-sa01
%% Type: valid
%% Sections: 2.9
'not-sa01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/not-sa01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa02.xml
%% ID: not-sa02
%% Type: valid
%% Sections: 2.9
'not-sa02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/not-sa02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa03.xml
%% ID: not-sa03
%% Type: valid
%% Sections: 2.9
'not-sa03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/not-sa03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/not-sa04.xml
%% ID: not-sa04
%% Type: valid
%% Sections: 2.9
'not-sa04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/not-sa04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/notation01.xml
%% ID: notation01
%% Type: valid
%% Sections: 4.7 [82]
'notation01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/notation01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/optional.xml
%% ID: optional
%% Type: valid
%% Sections: 3 3.2.1 [47]
'optional'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/optional.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/required00.xml
%% ID: required00
%% Type: valid
%% Sections: 3.3.2 [60]
'required00'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/required00.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa01.xml
%% ID: sa01
%% Type: valid
%% Sections: 2.9 [32]
'sa01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/sa01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa02.xml
%% ID: sa02
%% Type: valid
%% Sections: 2.9 [32]
'sa02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/sa02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa03.xml
%% ID: sa03
%% Type: valid
%% Sections: 2.9 [32]
'sa03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/sa03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa04.xml
%% ID: sa04
%% Type: valid
%% Sections: 2.9 [32]
'sa04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/sa04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sa05.xml
%% ID: sa05
%% Type: valid
%% Sections: 2.9 [32]
'sa05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/sa05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/sgml01.xml
%% ID: v-sgml01
%% Type: valid
%% Sections: 3.3.1 [59]
'v-sgml01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/sgml01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/v-lang01.xml
%% ID: v-lang01
%% Type: valid
%% Sections: 2.12 [35]
'v-lang01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/v-lang01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/v-lang02.xml
%% ID: v-lang02
%% Type: valid
%% Sections: 2.12 [35]
'v-lang02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/v-lang02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/v-lang03.xml
%% ID: v-lang03
%% Type: valid
%% Sections: 2.12 [36]
'v-lang03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/v-lang03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/v-lang04.xml
%% ID: v-lang04
%% Type: valid
%% Sections: 2.12 [37]
'v-lang04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/v-lang04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/v-lang05.xml
%% ID: v-lang05
%% Type: valid
%% Sections: 2.12 [35]
'v-lang05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/v-lang05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/v-lang06.xml
%% ID: v-lang06
%% Type: valid
%% Sections: 2.12 [37]
'v-lang06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/v-lang06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/pe00.xml
%% ID: v-pe00
%% Type: valid
%% Sections: 4.5
'v-pe00'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/pe00.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/pe03.xml
%% ID: v-pe03
%% Type: valid
%% Sections: 4.5
'v-pe03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/pe03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/pe02.xml
%% ID: v-pe02
%% Type: valid
%% Sections: 4.5
'v-pe02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","valid/pe02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/dtd01.xml
%% ID: inv-dtd01
%% Type: invalid
%% Sections: 3.2.2
'inv-dtd01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/dtd01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/dtd02.xml
%% ID: inv-dtd02
%% Type: invalid
%% Sections: 4.2.2
'inv-dtd02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/dtd02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/dtd03.xml
%% ID: inv-dtd03
%% Type: invalid
%% Sections: 3
'inv-dtd03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/dtd03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/el01.xml
%% ID: el01
%% Type: invalid
%% Sections: 3
'el01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/el01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/el02.xml
%% ID: el02
%% Type: invalid
%% Sections: 3
'el02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/el02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/el03.xml
%% ID: el03
%% Type: invalid
%% Sections: 3
'el03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/el03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/el04.xml
%% ID: el04
%% Type: invalid
%% Sections: 3.2
'el04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/el04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/el05.xml
%% ID: el05
%% Type: invalid
%% Sections: 3.2.2
'el05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/el05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/el06.xml
%% ID: el06
%% Type: invalid
%% Sections: 3
'el06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/el06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id01.xml
%% ID: id01
%% Type: invalid
%% Sections: 3.3.1
'id01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id02.xml
%% ID: id02
%% Type: invalid
%% Sections: 3.3.1
'id02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id03.xml
%% ID: id03
%% Type: invalid
%% Sections: 3.3.1
'id03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id04.xml
%% ID: id04
%% Type: invalid
%% Sections: 3.3.1
'id04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id05.xml
%% ID: id05
%% Type: invalid
%% Sections: 3.3.1
'id05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id06.xml
%% ID: id06
%% Type: invalid
%% Sections: 3.3.1
'id06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id07.xml
%% ID: id07
%% Type: invalid
%% Sections: 3.3.1
'id07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id08.xml
%% ID: id08
%% Type: invalid
%% Sections: 3.3.1
'id08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/id09.xml
%% ID: id09
%% Type: invalid
%% Sections: 3.3.1
'id09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/id09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa01.xml
%% ID: inv-not-sa01
%% Type: invalid
%% Sections: 2.9
'inv-not-sa01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa02.xml
%% ID: inv-not-sa02
%% Type: invalid
%% Sections: 2.9
'inv-not-sa02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa04.xml
%% ID: inv-not-sa04
%% Type: invalid
%% Sections: 2.9
'inv-not-sa04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa05.xml
%% ID: inv-not-sa05
%% Type: invalid
%% Sections: 2.9
'inv-not-sa05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa06.xml
%% ID: inv-not-sa06
%% Type: invalid
%% Sections: 2.9
'inv-not-sa06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa07.xml
%% ID: inv-not-sa07
%% Type: invalid
%% Sections: 2.9
'inv-not-sa07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa08.xml
%% ID: inv-not-sa08
%% Type: invalid
%% Sections: 2.9
'inv-not-sa08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa09.xml
%% ID: inv-not-sa09
%% Type: invalid
%% Sections: 2.9
'inv-not-sa09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa10.xml
%% ID: inv-not-sa10
%% Type: invalid
%% Sections: 2.9
'inv-not-sa10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa11.xml
%% ID: inv-not-sa11
%% Type: invalid
%% Sections: 2.9
'inv-not-sa11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa12.xml
%% ID: inv-not-sa12
%% Type: invalid
%% Sections: 2.9
'inv-not-sa12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa13.xml
%% ID: inv-not-sa13
%% Type: invalid
%% Sections: 2.9
'inv-not-sa13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/not-sa14.xml
%% ID: inv-not-sa14
%% Type: invalid
%% Sections: 3
'inv-not-sa14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/not-sa14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional01.xml
%% ID: optional01
%% Type: invalid
%% Sections: 3
'optional01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional02.xml
%% ID: optional02
%% Type: invalid
%% Sections: 3
'optional02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional03.xml
%% ID: optional03
%% Type: invalid
%% Sections: 3
'optional03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional04.xml
%% ID: optional04
%% Type: invalid
%% Sections: 3
'optional04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional05.xml
%% ID: optional05
%% Type: invalid
%% Sections: 3
'optional05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional06.xml
%% ID: optional06
%% Type: invalid
%% Sections: 3
'optional06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional07.xml
%% ID: optional07
%% Type: invalid
%% Sections: 3
'optional07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional08.xml
%% ID: optional08
%% Type: invalid
%% Sections: 3
'optional08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional09.xml
%% ID: optional09
%% Type: invalid
%% Sections: 3
'optional09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional10.xml
%% ID: optional10
%% Type: invalid
%% Sections: 3
'optional10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional11.xml
%% ID: optional11
%% Type: invalid
%% Sections: 3
'optional11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional12.xml
%% ID: optional12
%% Type: invalid
%% Sections: 3
'optional12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional13.xml
%% ID: optional13
%% Type: invalid
%% Sections: 3
'optional13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional14.xml
%% ID: optional14
%% Type: invalid
%% Sections: 3
'optional14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional20.xml
%% ID: optional20
%% Type: invalid
%% Sections: 3
'optional20'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional20.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional21.xml
%% ID: optional21
%% Type: invalid
%% Sections: 3
'optional21'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional21.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional22.xml
%% ID: optional22
%% Type: invalid
%% Sections: 3
'optional22'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional22.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional23.xml
%% ID: optional23
%% Type: invalid
%% Sections: 3
'optional23'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional23.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional24.xml
%% ID: optional24
%% Type: invalid
%% Sections: 3
'optional24'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional24.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/optional25.xml
%% ID: optional25
%% Type: invalid
%% Sections: 3
'optional25'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/optional25.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/required00.xml
%% ID: inv-required00
%% Type: invalid
%% Sections: 3.3.2
'inv-required00'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/required00.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/required01.xml
%% ID: inv-required01
%% Type: invalid
%% Sections: 3.1 2.10
'inv-required01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/required01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/required02.xml
%% ID: inv-required02
%% Type: invalid
%% Sections: 3.1 2.12
'inv-required02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/required02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/root.xml
%% ID: root
%% Type: invalid
%% Sections: 2.8
'root'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/root.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr01.xml
%% ID: attr01
%% Type: invalid
%% Sections: 3.3.1
'attr01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr02.xml
%% ID: attr02
%% Type: invalid
%% Sections: 3.3.1
'attr02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr03.xml
%% ID: attr03
%% Type: invalid
%% Sections: 3.3.1
'attr03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr04.xml
%% ID: attr04
%% Type: invalid
%% Sections: 3.3.1
'attr04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr05.xml
%% ID: attr05
%% Type: invalid
%% Sections: 3.3.1
'attr05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr06.xml
%% ID: attr06
%% Type: invalid
%% Sections: 3.3.1
'attr06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr07.xml
%% ID: attr07
%% Type: invalid
%% Sections: 3.3.1
'attr07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr08.xml
%% ID: attr08
%% Type: invalid
%% Sections: 3.3.2
'attr08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr09.xml
%% ID: attr09
%% Type: invalid
%% Sections: 3.3.2
'attr09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr10.xml
%% ID: attr10
%% Type: invalid
%% Sections: 3.3.2
'attr10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr11.xml
%% ID: attr11
%% Type: invalid
%% Sections: 3.3.2
'attr11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr12.xml
%% ID: attr12
%% Type: invalid
%% Sections: 3.3.2
'attr12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr13.xml
%% ID: attr13
%% Type: invalid
%% Sections: 3.3.2
'attr13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr14.xml
%% ID: attr14
%% Type: invalid
%% Sections: 3.3.2
'attr14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr15.xml
%% ID: attr15
%% Type: invalid
%% Sections: 3.3.2
'attr15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/attr16.xml
%% ID: attr16
%% Type: invalid
%% Sections: 3.3.2
'attr16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/attr16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/utf16b.xml
%% ID: utf16b
%% Type: invalid
%% Sections: 4.3.3 2.8
'utf16b'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/utf16b.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/utf16l.xml
%% ID: utf16l
%% Type: invalid
%% Sections: 4.3.3 2.8
'utf16l'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/utf16l.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/empty.xml
%% ID: empty
%% Type: invalid
%% Sections: 2.4 2.7 [18] 3
'empty'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","invalid/empty.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/not-sa03.xml
%% ID: not-wf-sa03
%% Type: not-wf
%% Sections: 2.9
'not-wf-sa03'(Config) -> {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/not-sa03.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist01.xml
%% ID: attlist01
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist02.xml
%% ID: attlist02
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist03.xml
%% ID: attlist03
%% Type: not-wf
%% Sections: 3.3.1 [59]
'attlist03'(_Config) ->  {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist04.xml
%% ID: attlist04
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist05.xml
%% ID: attlist05
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist06.xml
%% ID: attlist06
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist07.xml
%% ID: attlist07
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist08.xml
%% ID: attlist08
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist09.xml
%% ID: attlist09
%% Type: not-wf
%% Sections: 3.3.1 [56]
'attlist09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist10.xml
%% ID: attlist10
%% Type: not-wf
%% Sections: 3.1 [40]
'attlist10'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist10.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/attlist11.xml
%% ID: attlist11
%% Type: not-wf
%% Sections: 3.1 [44]
'attlist11'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/attlist11.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/cond01.xml
%% ID: cond01
%% Type: not-wf
%% Sections: 3.4 [61]
'cond01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/cond01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/cond02.xml
%% ID: cond02
%% Type: not-wf
%% Sections: 3.4 [61]
'cond02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/cond02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/content01.xml
%% ID: content01
%% Type: not-wf
%% Sections: 3.2.1 [48]
'content01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/content01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/content02.xml
%% ID: content02
%% Type: not-wf
%% Sections: 3.2.1 [48]
'content02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/content02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/content03.xml
%% ID: content03
%% Type: not-wf
%% Sections: 3.2.1 [48]
'content03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/content03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/decl01.xml
%% ID: decl01
%% Type: not-wf
%% Sections: 4.3.1 [77]
'decl01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/decl01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd00.xml
%% ID: nwf-dtd00
%% Type: not-wf
%% Sections: 3.2.1 [55]
'nwf-dtd00'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd00.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd01.xml
%% ID: nwf-dtd01
%% Type: not-wf
%% Sections: 3.2.1 [55]
'nwf-dtd01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd02.xml
%% ID: dtd02
%% Type: not-wf
%% Sections: 4.1 [69]
'dtd02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd03.xml
%% ID: dtd03
%% Type: not-wf
%% Sections: 4.1 [69]
'dtd03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd04.xml
%% ID: dtd04
%% Type: not-wf
%% Sections: 4.2.2 [75]
'dtd04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd05.xml
%% ID: dtd05
%% Type: not-wf
%% Sections: 4.2.2 [75]
'dtd05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/dtd07.xml
%% ID: dtd07
%% Type: not-wf
%% Sections: 4.3.1 [77]
'dtd07'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/dtd07.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/element00.xml
%% ID: element00
%% Type: not-wf
%% Sections: 3.1 [42]
'element00'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/element00.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/element01.xml
%% ID: element01
%% Type: not-wf
%% Sections: 3.1 [42]
'element01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/element01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/element02.xml
%% ID: element02
%% Type: not-wf
%% Sections: 3.1 [43]
'element02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/element02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/element03.xml
%% ID: element03
%% Type: not-wf
%% Sections: 3.1 [43]
'element03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/element03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/element04.xml
%% ID: element04
%% Type: not-wf
%% Sections: 3.1 [43]
'element04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/element04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding01.xml
%% ID: encoding01
%% Type: not-wf
%% Sections: 4.3.3 [81]
'encoding01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding02.xml
%% ID: encoding02
%% Type: not-wf
%% Sections: 4.3.3 [81]
'encoding02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding03.xml
%% ID: encoding03
%% Type: not-wf
%% Sections: 4.3.3 [81]
'encoding03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding04.xml
%% ID: encoding04
%% Type: not-wf
%% Sections: 4.3.3 [81]
'encoding04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding05.xml
%% ID: encoding05
%% Type: not-wf
%% Sections: 4.3.3 [81]
'encoding05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding06.xml
%% ID: encoding06
%% Type: not-wf
%% Sections: 4.3.3 [81]
'encoding06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/encoding07.xml
%% ID: encoding07
%% Type: not-wf
%% Sections: 4.3.1 [77]
'encoding07'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/encoding07.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/pi.xml
%% ID: pi
%% Type: not-wf
%% Sections: 2.6 [16]
'pi'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/pi.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/pubid01.xml
%% ID: pubid01
%% Type: not-wf
%% Sections: 2.3 [12]
'pubid01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/pubid01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/pubid02.xml
%% ID: pubid02
%% Type: not-wf
%% Sections: 2.3 [12]
'pubid02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/pubid02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/pubid03.xml
%% ID: pubid03
%% Type: not-wf
%% Sections: 2.3 [12]
'pubid03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/pubid03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/pubid04.xml
%% ID: pubid04
%% Type: not-wf
%% Sections: 2.3 [12]
'pubid04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/pubid04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/pubid05.xml
%% ID: pubid05
%% Type: not-wf
%% Sections: 2.3 [12]
'pubid05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/pubid05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml01.xml
%% ID: sgml01
%% Type: not-wf
%% Sections: 3 [39]
'sgml01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml02.xml
%% ID: sgml02
%% Type: not-wf
%% Sections: 2.8 
'sgml02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml03.xml
%% ID: sgml03
%% Type: not-wf
%% Sections: 2.5 [15]
'sgml03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml04.xml
%% ID: sgml04
%% Type: not-wf
%% Sections: 3.3 [52]
'sgml04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml05.xml
%% ID: sgml05
%% Type: not-wf
%% Sections: 3.2 [45]
'sgml05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml06.xml
%% ID: sgml06
%% Type: not-wf
%% Sections: 3.3 [52]
'sgml06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml07.xml
%% ID: sgml07
%% Type: not-wf
%% Sections: 3.2 [45]
'sgml07'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml07.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml08.xml
%% ID: sgml08
%% Type: not-wf
%% Sections: 3.2 [45]
'sgml08'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml08.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml09.xml
%% ID: sgml09
%% Type: not-wf
%% Sections: 3.2 [45]
'sgml09'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml09.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml10.xml
%% ID: sgml10
%% Type: not-wf
%% Sections: 3.2 [45]
'sgml10'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml10.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml11.xml
%% ID: sgml11
%% Type: not-wf
%% Sections: 3.2 [46]
'sgml11'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml11.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml12.xml
%% ID: sgml12
%% Type: not-wf
%% Sections: 3.2 [46]
'sgml12'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml12.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/sgml13.xml
%% ID: sgml13
%% Type: not-wf
%% Sections: 3.2.1 [47]
'sgml13'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/sgml13.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/uri01.xml
%% ID: uri01
%% Type: error
%% Sections: 4.2.2 [75]
'uri01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"sun","not-wf/uri01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: Sun Microsystems XML Tests


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01pass2.xml
%% ID: o-p01pass2
%% Type: valid
%% Sections: 2.2 [1]
'o-p01pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p06pass1.xml
%% ID: o-p06pass1
%% Type: valid
%% Sections: 2.3 [6]
'o-p06pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p06pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p07pass1.xml
%% ID: o-p07pass1
%% Type: valid
%% Sections: 2.3 [7]
'o-p07pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p07pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p08pass1.xml
%% ID: o-p08pass1
%% Type: valid
%% Sections: 2.3 [8]
'o-p08pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p08pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p09pass1.xml
%% ID: o-p09pass1
%% Type: valid
%% Sections: 2.3 [9]
'o-p09pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p09pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12pass1.xml
%% ID: o-p12pass1
%% Type: valid
%% Sections: 2.3 [12]
'o-p12pass1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12pass1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22pass4.xml
%% ID: o-p22pass4
%% Type: valid
%% Sections: 2.8 [22]
'o-p22pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22pass5.xml
%% ID: o-p22pass5
%% Type: valid
%% Sections: 2.8 [22]
'o-p22pass5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22pass5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22pass6.xml
%% ID: o-p22pass6
%% Type: valid
%% Sections: 2.8 [22]
'o-p22pass6'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22pass6.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p28pass1.xml
%% ID: o-p28pass1
%% Type: valid
%% Sections: 3.1 [43] [44]
'o-p28pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p28pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p28pass3.xml
%% ID: o-p28pass3
%% Type: valid
%% Sections: 2.8 4.1 [28] [69]
'o-p28pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p28pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p28pass4.xml
%% ID: o-p28pass4
%% Type: valid
%% Sections: 2.8 4.2.2 [28] [75]
'o-p28pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p28pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p28pass5.xml
%% ID: o-p28pass5
%% Type: valid
%% Sections: 2.8 4.1 [28] [69]
'o-p28pass5'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p28pass5.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p29pass1.xml
%% ID: o-p29pass1
%% Type: valid
%% Sections: 2.8 [29]
'o-p29pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p29pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p30pass1.xml
%% ID: o-p30pass1
%% Type: valid
%% Sections: 2.8 4.2.2 [30] [75]
'o-p30pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p30pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p30pass2.xml
%% ID: o-p30pass2
%% Type: valid
%% Sections: 2.8 4.2.2 4.3.1 [30] [75] [77]
'o-p30pass2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p30pass2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p31pass1.xml
%% ID: o-p31pass1
%% Type: valid
%% Sections: 2.8 [31]
'o-p31pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p31pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p31pass2.xml
%% ID: o-p31pass2
%% Type: valid
%% Sections: 2.8 3.4 4.2.2 [31] [62] [63] [75]
'o-p31pass2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p31pass2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p43pass1.xml
%% ID: o-p43pass1
%% Type: valid
%% Sections: 2.4 2.5 2.6 2.7 [15] [16] [18]
'o-p43pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p43pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p45pass1.xml
%% ID: o-p45pass1
%% Type: valid
%% Sections: 3.2 [45]
'o-p45pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p45pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46pass1.xml
%% ID: o-p46pass1
%% Type: valid
%% Sections: 3.2 3.2.1 3.2.2 [45] [46] [47] [51]
'o-p46pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p47pass1.xml
%% ID: o-p47pass1
%% Type: valid
%% Sections: 3.2 3.2.1 [45] [46] [47] 
'o-p47pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p47pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p48pass1.xml
%% ID: o-p48pass1
%% Type: valid
%% Sections: 3.2 3.2.1 [45] [46] [47]
'o-p48pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p48pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p49pass1.xml
%% ID: o-p49pass1
%% Type: valid
%% Sections: 3.2 3.2.1 [45] [46] [47]
'o-p49pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p49pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p50pass1.xml
%% ID: o-p50pass1
%% Type: valid
%% Sections: 3.2 3.2.1 [45] [46] [47]
'o-p50pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p50pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51pass1.xml
%% ID: o-p51pass1
%% Type: valid
%% Sections: 3.2.2 [51]
'o-p51pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p52pass1.xml
%% ID: o-p52pass1
%% Type: valid
%% Sections: 3.3 [52]
'o-p52pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p52pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p53pass1.xml
%% ID: o-p53pass1
%% Type: valid
%% Sections: 3.3 [53]
'o-p53pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p53pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p54pass1.xml
%% ID: o-p54pass1
%% Type: valid
%% Sections: 3.3.1 [54]
'o-p54pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p54pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p55pass1.xml
%% ID: o-p55pass1
%% Type: valid
%% Sections: 3.3.1 [55]
'o-p55pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p55pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p56pass1.xml
%% ID: o-p56pass1
%% Type: valid
%% Sections: 3.3.1 [56]
'o-p56pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p56pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p57pass1.xml
%% ID: o-p57pass1
%% Type: valid
%% Sections: 3.3.1 [57]
'o-p57pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p57pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58pass1.xml
%% ID: o-p58pass1
%% Type: valid
%% Sections: 3.3.1 [58]
'o-p58pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p59pass1.xml
%% ID: o-p59pass1
%% Type: valid
%% Sections: 3.3.1 [59]
'o-p59pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p59pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p60pass1.xml
%% ID: o-p60pass1
%% Type: valid
%% Sections: 3.3.2 [60]
'o-p60pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p60pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p61pass1.xml
%% ID: o-p61pass1
%% Type: valid
%% Sections: 3.4 [61]
'o-p61pass1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p61pass1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p62pass1.xml
%% ID: o-p62pass1
%% Type: valid
%% Sections: 3.4 [62]
'o-p62pass1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p62pass1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p63pass1.xml
%% ID: o-p63pass1
%% Type: valid
%% Sections: 3.4 [63]
'o-p63pass1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p63pass1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p64pass1.xml
%% ID: o-p64pass1
%% Type: valid
%% Sections: 3.4 [64]
'o-p64pass1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p64pass1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p68pass1.xml
%% ID: o-p68pass1
%% Type: valid
%% Sections: 4.1 [68]
'o-p68pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p68pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p69pass1.xml
%% ID: o-p69pass1
%% Type: valid
%% Sections: 4.1 [69]
'o-p69pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p69pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p70pass1.xml
%% ID: o-p70pass1
%% Type: valid
%% Sections: 4.2 [70]
'o-p70pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p70pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p71pass1.xml
%% ID: o-p71pass1
%% Type: valid
%% Sections: 4.2 [71]
'o-p71pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p71pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p72pass1.xml
%% ID: o-p72pass1
%% Type: valid
%% Sections: 4.2 [72]
'o-p72pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p72pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p73pass1.xml
%% ID: o-p73pass1
%% Type: valid
%% Sections: 4.2 [73]
'o-p73pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p73pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p76pass1.xml
%% ID: o-p76pass1
%% Type: valid
%% Sections: 4.2.2 [76]
'o-p76pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p76pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01pass1.xml
%% ID: o-p01pass1
%% Type: invalid
%% Sections: 2.1 [1]
'o-p01pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01pass3.xml
%% ID: o-p01pass3
%% Type: invalid
%% Sections: 2.1 [1]
'o-p01pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03pass1.xml
%% ID: o-p03pass1
%% Type: invalid
%% Sections: 2.3 [3]
'o-p03pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p04pass1.xml
%% ID: o-p04pass1
%% Type: invalid
%% Sections: 2.3 [4]
'o-p04pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p04pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p05pass1.xml
%% ID: o-p05pass1
%% Type: invalid
%% Sections: 2.3 [5]
'o-p05pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p05pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p06fail1.xml
%% ID: o-p06fail1
%% Type: invalid
%% Sections: 2.3 [6]
'o-p06fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p06fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p08fail1.xml
%% ID: o-p08fail1
%% Type: invalid
%% Sections: 2.3 [8]
'o-p08fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p08fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p08fail2.xml
%% ID: o-p08fail2
%% Type: invalid
%% Sections: 2.3 [8]
'o-p08fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p08fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p10pass1.xml
%% ID: o-p10pass1
%% Type: invalid
%% Sections: 2.3 [10]
'o-p10pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p10pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p14pass1.xml
%% ID: o-p14pass1
%% Type: invalid
%% Sections: 2.4 [14]
'o-p14pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p14pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p15pass1.xml
%% ID: o-p15pass1
%% Type: invalid
%% Sections: 2.5 [15]
'o-p15pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p15pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p16pass1.xml
%% ID: o-p16pass1
%% Type: invalid
%% Sections: 2.6 [16] [17]
'o-p16pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p16pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p16pass2.xml
%% ID: o-p16pass2
%% Type: invalid
%% Sections: 2.6 [16]
'o-p16pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p16pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p16pass3.xml
%% ID: o-p16pass3
%% Type: invalid
%% Sections: 2.6 [16]
'o-p16pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p16pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p18pass1.xml
%% ID: o-p18pass1
%% Type: invalid
%% Sections: 2.7 [18]
'o-p18pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p18pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22pass1.xml
%% ID: o-p22pass1
%% Type: invalid
%% Sections: 2.8 [22]
'o-p22pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22pass2.xml
%% ID: o-p22pass2
%% Type: invalid
%% Sections: 2.8 [22]
'o-p22pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22pass3.xml
%% ID: o-p22pass3
%% Type: invalid
%% Sections: 2.8 [22]
'o-p22pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23pass1.xml
%% ID: o-p23pass1
%% Type: invalid
%% Sections: 2.8 [23]
'o-p23pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23pass2.xml
%% ID: o-p23pass2
%% Type: invalid
%% Sections: 2.8 [23]
'o-p23pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23pass3.xml
%% ID: o-p23pass3
%% Type: invalid
%% Sections: 2.8 [23]
'o-p23pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23pass4.xml
%% ID: o-p23pass4
%% Type: invalid
%% Sections: 2.8 [23]
'o-p23pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p24pass1.xml
%% ID: o-p24pass1
%% Type: invalid
%% Sections: 2.8 [24]
'o-p24pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p24pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p24pass2.xml
%% ID: o-p24pass2
%% Type: invalid
%% Sections: 2.8 [24]
'o-p24pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p24pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p24pass3.xml
%% ID: o-p24pass3
%% Type: invalid
%% Sections: 2.8 [24]
'o-p24pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p24pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p24pass4.xml
%% ID: o-p24pass4
%% Type: invalid
%% Sections: 2.8 [24]
'o-p24pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p24pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p25pass1.xml
%% ID: o-p25pass1
%% Type: invalid
%% Sections: 2.8 [25]
'o-p25pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p25pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p25pass2.xml
%% ID: o-p25pass2
%% Type: invalid
%% Sections: 2.8 [25]
'o-p25pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p25pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p26pass1.xml
%% ID: o-p26pass1
%% Type: invalid
%% Sections: 2.8 [26]
'o-p26pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p26pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p27pass1.xml
%% ID: o-p27pass1
%% Type: invalid
%% Sections: 2.8 [27]
'o-p27pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p27pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p27pass2.xml
%% ID: o-p27pass2
%% Type: invalid
%% Sections: 2.8 [27]
'o-p27pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p27pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p27pass3.xml
%% ID: o-p27pass3
%% Type: invalid
%% Sections: 2.8 [27]
'o-p27pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p27pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p27pass4.xml
%% ID: o-p27pass4
%% Type: invalid
%% Sections: 2.8 [27]
'o-p27pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p27pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32pass1.xml
%% ID: o-p32pass1
%% Type: invalid
%% Sections: 2.9 [32]
'o-p32pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32pass2.xml
%% ID: o-p32pass2
%% Type: invalid
%% Sections: 2.9 [32]
'o-p32pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39pass1.xml
%% ID: o-p39pass1
%% Type: invalid
%% Sections: 3 3.1 [39] [44]
'o-p39pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39pass2.xml
%% ID: o-p39pass2
%% Type: invalid
%% Sections: 3 3.1 [39] [43]
'o-p39pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40pass1.xml
%% ID: o-p40pass1
%% Type: invalid
%% Sections: 3.1 [40]
'o-p40pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40pass2.xml
%% ID: o-p40pass2
%% Type: invalid
%% Sections: 3.1 [40]
'o-p40pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40pass3.xml
%% ID: o-p40pass3
%% Type: invalid
%% Sections: 3.1 [40] [41]
'o-p40pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40pass4.xml
%% ID: o-p40pass4
%% Type: invalid
%% Sections: 3.1 [40]
'o-p40pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p41pass1.xml
%% ID: o-p41pass1
%% Type: invalid
%% Sections: 3.1 [41]
'o-p41pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p41pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p41pass2.xml
%% ID: o-p41pass2
%% Type: invalid
%% Sections: 3.1 [41]
'o-p41pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p41pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p42pass1.xml
%% ID: o-p42pass1
%% Type: invalid
%% Sections: 3.1 [42]
'o-p42pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p42pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p42pass2.xml
%% ID: o-p42pass2
%% Type: invalid
%% Sections: 3.1 [42]
'o-p42pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p42pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44pass1.xml
%% ID: o-p44pass1
%% Type: invalid
%% Sections: 3.1 [44]
'o-p44pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44pass2.xml
%% ID: o-p44pass2
%% Type: invalid
%% Sections: 3.1 [44]
'o-p44pass2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44pass2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44pass3.xml
%% ID: o-p44pass3
%% Type: invalid
%% Sections: 3.1 [44]
'o-p44pass3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44pass3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44pass4.xml
%% ID: o-p44pass4
%% Type: invalid
%% Sections: 3.1 [44]
'o-p44pass4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44pass4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44pass5.xml
%% ID: o-p44pass5
%% Type: invalid
%% Sections: 3.1 [44]
'o-p44pass5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44pass5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66pass1.xml
%% ID: o-p66pass1
%% Type: invalid
%% Sections: 4.1 [66]
'o-p66pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p74pass1.xml
%% ID: o-p74pass1
%% Type: invalid
%% Sections: 4.2 [74]
'o-p74pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p74pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75pass1.xml
%% ID: o-p75pass1
%% Type: invalid
%% Sections: 4.2.2 [75]
'o-p75pass1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75pass1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: e2.xml
%% ID: o-e2
%% Type: invalid
%% Sections: 3.3.1 [58] [59] Errata [E2]
'o-e2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","e2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01fail1.xml
%% ID: o-p01fail1
%% Type: not-wf
%% Sections: 2.1 [1]
'o-p01fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01fail2.xml
%% ID: o-p01fail2
%% Type: not-wf
%% Sections: 2.1 [1]
'o-p01fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01fail3.xml
%% ID: o-p01fail3
%% Type: not-wf
%% Sections: 2.1 [1]
'o-p01fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01fail3.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_, <<"<bad/>", _/binary>>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p01fail4.xml
%% ID: o-p01fail4
%% Type: not-wf
%% Sections: 2.1 [1]
'o-p01fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p01fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail1.xml
%% ID: o-p02fail1
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail10.xml
%% ID: o-p02fail10
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail11.xml
%% ID: o-p02fail11
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail12.xml
%% ID: o-p02fail12
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail13.xml
%% ID: o-p02fail13
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail14.xml
%% ID: o-p02fail14
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail15.xml
%% ID: o-p02fail15
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail16.xml
%% ID: o-p02fail16
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail17.xml
%% ID: o-p02fail17
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail18.xml
%% ID: o-p02fail18
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail19.xml
%% ID: o-p02fail19
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail19'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail19.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail2.xml
%% ID: o-p02fail2
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail20.xml
%% ID: o-p02fail20
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail20'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail20.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail21.xml
%% ID: o-p02fail21
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail21'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail21.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail22.xml
%% ID: o-p02fail22
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail22'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail22.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail23.xml
%% ID: o-p02fail23
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail23'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail23.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail24.xml
%% ID: o-p02fail24
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail24'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail24.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail25.xml
%% ID: o-p02fail25
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail25'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail25.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail26.xml
%% ID: o-p02fail26
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail26'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail26.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail27.xml
%% ID: o-p02fail27
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail27'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail27.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail28.xml
%% ID: o-p02fail28
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail28'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail28.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail29.xml
%% ID: o-p02fail29
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail29'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail29.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail3.xml
%% ID: o-p02fail3
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail30.xml
%% ID: o-p02fail30
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail30'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail30.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail31.xml
%% ID: o-p02fail31
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail31'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail31.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail4.xml
%% ID: o-p02fail4
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail5.xml
%% ID: o-p02fail5
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail6.xml
%% ID: o-p02fail6
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail6'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail6.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail7.xml
%% ID: o-p02fail7
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail7'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail7.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail8.xml
%% ID: o-p02fail8
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail8'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail8.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p02fail9.xml
%% ID: o-p02fail9
%% Type: not-wf
%% Sections: 2.2 [2]
'o-p02fail9'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p02fail9.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail1.xml
%% ID: o-p03fail1
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail10.xml
%% ID: o-p03fail10
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail11.xml
%% ID: o-p03fail11
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail12.xml
%% ID: o-p03fail12
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail13.xml
%% ID: o-p03fail13
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail14.xml
%% ID: o-p03fail14
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail15.xml
%% ID: o-p03fail15
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail16.xml
%% ID: o-p03fail16
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail17.xml
%% ID: o-p03fail17
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail18.xml
%% ID: o-p03fail18
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail19.xml
%% ID: o-p03fail19
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail19'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail19.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail2.xml
%% ID: o-p03fail2
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail20.xml
%% ID: o-p03fail20
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail20'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail20.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail21.xml
%% ID: o-p03fail21
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail21'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail21.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail22.xml
%% ID: o-p03fail22
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail22'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail22.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail23.xml
%% ID: o-p03fail23
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail23'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail23.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail24.xml
%% ID: o-p03fail24
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail24'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail24.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail25.xml
%% ID: o-p03fail25
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail25'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail25.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail26.xml
%% ID: o-p03fail26
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail26'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail26.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail27.xml
%% ID: o-p03fail27
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail27'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail27.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail28.xml
%% ID: o-p03fail28
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail28'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail28.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail29.xml
%% ID: o-p03fail29
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail29'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail29.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail3.xml
%% ID: o-p03fail3
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail4.xml
%% ID: o-p03fail4
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail5.xml
%% ID: o-p03fail5
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail7.xml
%% ID: o-p03fail7
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail7'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail7.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail8.xml
%% ID: o-p03fail8
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail8'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail8.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p03fail9.xml
%% ID: o-p03fail9
%% Type: not-wf
%% Sections: 2.3 [3]
'o-p03fail9'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p03fail9.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p04fail1.xml
%% ID: o-p04fail1
%% Type: not-wf
%% Sections: 2.3 [4]
'o-p04fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p04fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p04fail2.xml
%% ID: o-p04fail2
%% Type: not-wf
%% Sections: 2.3 [4]
'o-p04fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p04fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p04fail3.xml
%% ID: o-p04fail3
%% Type: not-wf
%% Sections: 2.3 [4]
'o-p04fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p04fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p05fail1.xml
%% ID: o-p05fail1
%% Type: not-wf
%% Sections: 2.3 [5]
'o-p05fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p05fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p05fail2.xml
%% ID: o-p05fail2
%% Type: not-wf
%% Sections: 2.3 [5]
'o-p05fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p05fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p05fail3.xml
%% ID: o-p05fail3
%% Type: not-wf
%% Sections: 2.3 [5]
'o-p05fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p05fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p05fail4.xml
%% ID: o-p05fail4
%% Type: not-wf
%% Sections: 2.3 [5]
'o-p05fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p05fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p05fail5.xml
%% ID: o-p05fail5
%% Type: not-wf
%% Sections: 2.3 [5]
'o-p05fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p05fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p09fail1.xml
%% ID: o-p09fail1
%% Type: not-wf
%% Sections: 2.3 [9]
'o-p09fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p09fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p09fail2.xml
%% ID: o-p09fail2
%% Type: not-wf
%% Sections: 2.3 [9]
'o-p09fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p09fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p09fail3.xml
%% ID: o-p09fail3
%% Type: not-wf
%% Sections: 2.3 [9]
'o-p09fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p09fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p09fail4.xml
%% ID: o-p09fail4
%% Type: not-wf
%% Sections: 2.3 [9]
'o-p09fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p09fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p09fail5.xml
%% ID: o-p09fail5
%% Type: not-wf
%% Sections: 2.3 [9]
'o-p09fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p09fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p10fail1.xml
%% ID: o-p10fail1
%% Type: not-wf
%% Sections: 2.3 [10]
'o-p10fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p10fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p10fail2.xml
%% ID: o-p10fail2
%% Type: not-wf
%% Sections: 2.3 [10]
'o-p10fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p10fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p10fail3.xml
%% ID: o-p10fail3
%% Type: not-wf
%% Sections: 2.3 [10]
'o-p10fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p10fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p11fail1.xml
%% ID: o-p11fail1
%% Type: not-wf
%% Sections: 2.3 [11]
'o-p11fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p11fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p11fail2.xml
%% ID: o-p11fail2
%% Type: not-wf
%% Sections: 2.3 [11]
'o-p11fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p11fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail1.xml
%% ID: o-p12fail1
%% Type: not-wf
%% Sections: 2.3 [12]
'o-p12fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail2.xml
%% ID: o-p12fail2
%% Type: not-wf
%% Sections: 2.3 [12]
'o-p12fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail3.xml
%% ID: o-p12fail3
%% Type: not-wf
%% Sections: 2.3 [12]
'o-p12fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail4.xml
%% ID: o-p12fail4
%% Type: not-wf
%% Sections: 2.3 [12]
'o-p12fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail5.xml
%% ID: o-p12fail5
%% Type: not-wf
%% Sections: 2.3 [12]
'o-p12fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail6.xml
%% ID: o-p12fail6
%% Type: not-wf
%% Sections: 2.3 [12]
'o-p12fail6'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail6.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p12fail7.xml
%% ID: o-p12fail7
%% Type: not-wf
%% Sections: 2.3 [13]
'o-p12fail7'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p12fail7.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p14fail1.xml
%% ID: o-p14fail1
%% Type: not-wf
%% Sections: 2.4 [14]
'o-p14fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p14fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p14fail2.xml
%% ID: o-p14fail2
%% Type: not-wf
%% Sections: 2.4 [14]
'o-p14fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p14fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p14fail3.xml
%% ID: o-p14fail3
%% Type: not-wf
%% Sections: 2.4 [14]
'o-p14fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p14fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p15fail1.xml
%% ID: o-p15fail1
%% Type: not-wf
%% Sections: 2.5 [15]
'o-p15fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p15fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p15fail2.xml
%% ID: o-p15fail2
%% Type: not-wf
%% Sections: 2.5 [15]
'o-p15fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p15fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p15fail3.xml
%% ID: o-p15fail3
%% Type: not-wf
%% Sections: 2.5 [15]
'o-p15fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p15fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p16fail1.xml
%% ID: o-p16fail1
%% Type: not-wf
%% Sections: 2.6 [16]
'o-p16fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p16fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p16fail2.xml
%% ID: o-p16fail2
%% Type: not-wf
%% Sections: 2.6 [16]
'o-p16fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p16fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p16fail3.xml
%% ID: o-p16fail3
%% Type: not-wf
%% Sections: 2.6 [16]
'o-p16fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p16fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p18fail1.xml
%% ID: o-p18fail1
%% Type: not-wf
%% Sections: 2.7 [18]
'o-p18fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p18fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p18fail2.xml
%% ID: o-p18fail2
%% Type: not-wf
%% Sections: 2.7 [18]
'o-p18fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p18fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p18fail3.xml
%% ID: o-p18fail3
%% Type: not-wf
%% Sections: 2.7 [18]
'o-p18fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p18fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22fail1.xml
%% ID: o-p22fail1
%% Type: not-wf
%% Sections: 2.8 [22]
'o-p22fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p22fail2.xml
%% ID: o-p22fail2
%% Type: not-wf
%% Sections: 2.8 [22]
'o-p22fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p22fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23fail1.xml
%% ID: o-p23fail1
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p23fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23fail2.xml
%% ID: o-p23fail2
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p23fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23fail3.xml
%% ID: o-p23fail3
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p23fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23fail4.xml
%% ID: o-p23fail4
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p23fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p23fail5.xml
%% ID: o-p23fail5
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p23fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p23fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p24fail1.xml
%% ID: o-p24fail1
%% Type: not-wf
%% Sections: 2.8 [24]
'o-p24fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p24fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p24fail2.xml
%% ID: o-p24fail2
%% Type: not-wf
%% Sections: 2.8 [24]
'o-p24fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p24fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p25fail1.xml
%% ID: o-p25fail1
%% Type: not-wf
%% Sections: 2.8 [25]
'o-p25fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p25fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p26fail1.xml
%% ID: o-p26fail1
%% Type: not-wf
%% Sections: 2.8 [26]
'o-p26fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p26fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p26fail2.xml
%% ID: o-p26fail2
%% Type: not-wf
%% Sections: 2.8 [26]
'o-p26fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p26fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p27fail1.xml
%% ID: o-p27fail1
%% Type: not-wf
%% Sections: 2.8 [27]
'o-p27fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p27fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p28fail1.xml
%% ID: o-p28fail1
%% Type: not-wf
%% Sections: 2.8 [28]
'o-p28fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p28fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p29fail1.xml
%% ID: o-p29fail1
%% Type: not-wf
%% Sections: 2.8 [29]
'o-p29fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p29fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p30fail1.xml
%% ID: o-p30fail1
%% Type: not-wf
%% Sections: 2.8 [30]
'o-p30fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p30fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p31fail1.xml
%% ID: o-p31fail1
%% Type: not-wf
%% Sections: 2.8 [31]
'o-p31fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p31fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32fail1.xml
%% ID: o-p32fail1
%% Type: not-wf
%% Sections: 2.9 [32]
'o-p32fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32fail2.xml
%% ID: o-p32fail2
%% Type: not-wf
%% Sections: 2.9 [32]
'o-p32fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32fail3.xml
%% ID: o-p32fail3
%% Type: not-wf
%% Sections: 2.9 [32]
'o-p32fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32fail4.xml
%% ID: o-p32fail4
%% Type: not-wf
%% Sections: 2.9 [32]
'o-p32fail4'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32fail4.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p32fail5.xml
%% ID: o-p32fail5
%% Type: not-wf
%% Sections: 2.9 [32]
'o-p32fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p32fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39fail1.xml
%% ID: o-p39fail1
%% Type: not-wf
%% Sections: 3 [39]
'o-p39fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39fail2.xml
%% ID: o-p39fail2
%% Type: not-wf
%% Sections: 3 [39]
'o-p39fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39fail3.xml
%% ID: o-p39fail3
%% Type: not-wf
%% Sections: 3 [39]
'o-p39fail3'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39fail3.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39fail4.xml
%% ID: o-p39fail4
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p39fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p39fail5.xml
%% ID: o-p39fail5
%% Type: not-wf
%% Sections: 2.8 [23]
'o-p39fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p39fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40fail1.xml
%% ID: o-p40fail1
%% Type: not-wf
%% Sections: 3.1 [40]
'o-p40fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40fail2.xml
%% ID: o-p40fail2
%% Type: not-wf
%% Sections: 3.1 [40]
'o-p40fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40fail3.xml
%% ID: o-p40fail3
%% Type: not-wf
%% Sections: 3.1 [40]
'o-p40fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p40fail4.xml
%% ID: o-p40fail4
%% Type: not-wf
%% Sections: 3.1 [40]
'o-p40fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p40fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p41fail1.xml
%% ID: o-p41fail1
%% Type: not-wf
%% Sections: 3.1 [41]
'o-p41fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p41fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p41fail2.xml
%% ID: o-p41fail2
%% Type: not-wf
%% Sections: 3.1 [41]
'o-p41fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p41fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p41fail3.xml
%% ID: o-p41fail3
%% Type: not-wf
%% Sections: 3.1 [41]
'o-p41fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p41fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p42fail1.xml
%% ID: o-p42fail1
%% Type: not-wf
%% Sections: 3.1 [42]
'o-p42fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p42fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p42fail2.xml
%% ID: o-p42fail2
%% Type: not-wf
%% Sections: 3.1 [42]
'o-p42fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p42fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p42fail3.xml
%% ID: o-p42fail3
%% Type: not-wf
%% Sections: 3.1 [42]
'o-p42fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p42fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p43fail1.xml
%% ID: o-p43fail1
%% Type: not-wf
%% Sections: 3.1 [43]
'o-p43fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p43fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p43fail2.xml
%% ID: o-p43fail2
%% Type: not-wf
%% Sections: 3.1 [43]
'o-p43fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p43fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p43fail3.xml
%% ID: o-p43fail3
%% Type: not-wf
%% Sections: 3.1 [43]
'o-p43fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p43fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44fail1.xml
%% ID: o-p44fail1
%% Type: not-wf
%% Sections: 3.1 [44]
'o-p44fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44fail2.xml
%% ID: o-p44fail2
%% Type: not-wf
%% Sections: 3.1 [44]
'o-p44fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44fail3.xml
%% ID: o-p44fail3
%% Type: not-wf
%% Sections: 3.1 [44]
'o-p44fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44fail4.xml
%% ID: o-p44fail4
%% Type: not-wf
%% Sections: 3.1 [44]
'o-p44fail4'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44fail4.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p44fail5.xml
%% ID: o-p44fail5
%% Type: not-wf
%% Sections: 3.1 [44]
'o-p44fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p44fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p45fail1.xml
%% ID: o-p45fail1
%% Type: not-wf
%% Sections: 3.2 [45]
'o-p45fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p45fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p45fail2.xml
%% ID: o-p45fail2
%% Type: not-wf
%% Sections: 3.2 [45]
'o-p45fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p45fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p45fail3.xml
%% ID: o-p45fail3
%% Type: not-wf
%% Sections: 3.2 [45]
'o-p45fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p45fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p45fail4.xml
%% ID: o-p45fail4
%% Type: not-wf
%% Sections: 3.2 [45]
'o-p45fail4'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p45fail4.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46fail1.xml
%% ID: o-p46fail1
%% Type: not-wf
%% Sections: 3.2 [46]
'o-p46fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46fail2.xml
%% ID: o-p46fail2
%% Type: not-wf
%% Sections: 3.2 [46]
'o-p46fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46fail3.xml
%% ID: o-p46fail3
%% Type: not-wf
%% Sections: 3.2 [46]
'o-p46fail3'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46fail3.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46fail4.xml
%% ID: o-p46fail4
%% Type: not-wf
%% Sections: 3.2 [46]
'o-p46fail4'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46fail4.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46fail5.xml
%% ID: o-p46fail5
%% Type: not-wf
%% Sections: 3.2 [46]
'o-p46fail5'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46fail5.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p46fail6.xml
%% ID: o-p46fail6
%% Type: not-wf
%% Sections: 3.2 [46]
'o-p46fail6'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p46fail6.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p47fail1.xml
%% ID: o-p47fail1
%% Type: not-wf
%% Sections: 3.2.1 [47]
'o-p47fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p47fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p47fail2.xml
%% ID: o-p47fail2
%% Type: not-wf
%% Sections: 3.2.1 [47]
'o-p47fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p47fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p47fail3.xml
%% ID: o-p47fail3
%% Type: not-wf
%% Sections: 3.2.1 [47]
'o-p47fail3'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p47fail3.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p47fail4.xml
%% ID: o-p47fail4
%% Type: not-wf
%% Sections: 3.2.1 [47]
'o-p47fail4'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p47fail4.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p48fail1.xml
%% ID: o-p48fail1
%% Type: not-wf
%% Sections: 3.2.1 [48]
'o-p48fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p48fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p48fail2.xml
%% ID: o-p48fail2
%% Type: not-wf
%% Sections: 3.2.1 [48]
'o-p48fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p48fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p49fail1.xml
%% ID: o-p49fail1
%% Type: not-wf
%% Sections: 3.2.1 [49]
'o-p49fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p49fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p50fail1.xml
%% ID: o-p50fail1
%% Type: not-wf
%% Sections: 3.2.1 [50]
'o-p50fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p50fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail1.xml
%% ID: o-p51fail1
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail2.xml
%% ID: o-p51fail2
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail3.xml
%% ID: o-p51fail3
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail3'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail3.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail4.xml
%% ID: o-p51fail4
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail4'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail4.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail5.xml
%% ID: o-p51fail5
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail5'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail5.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail6.xml
%% ID: o-p51fail6
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail6'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail6.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p51fail7.xml
%% ID: o-p51fail7
%% Type: not-wf
%% Sections: 3.2.2 [51]
'o-p51fail7'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p51fail7.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p52fail1.xml
%% ID: o-p52fail1
%% Type: not-wf
%% Sections: 3.3 [52]
'o-p52fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p52fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p52fail2.xml
%% ID: o-p52fail2
%% Type: not-wf
%% Sections: 3.3 [52]
'o-p52fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p52fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p53fail1.xml
%% ID: o-p53fail1
%% Type: not-wf
%% Sections: 3.3 [53]
'o-p53fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p53fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p53fail2.xml
%% ID: o-p53fail2
%% Type: not-wf
%% Sections: 3.3 [53]
'o-p53fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p53fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p53fail3.xml
%% ID: o-p53fail3
%% Type: not-wf
%% Sections: 3.3 [53]
'o-p53fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p53fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p53fail4.xml
%% ID: o-p53fail4
%% Type: not-wf
%% Sections: 3.3 [53]
'o-p53fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p53fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p53fail5.xml
%% ID: o-p53fail5
%% Type: not-wf
%% Sections: 3.3 [53]
'o-p53fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p53fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p54fail1.xml
%% ID: o-p54fail1
%% Type: not-wf
%% Sections: 3.3.1 [54]
'o-p54fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p54fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p55fail1.xml
%% ID: o-p55fail1
%% Type: not-wf
%% Sections: 3.3.1 [55]
'o-p55fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p55fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p56fail1.xml
%% ID: o-p56fail1
%% Type: not-wf
%% Sections: 3.3.1 [56]
'o-p56fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p56fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p56fail2.xml
%% ID: o-p56fail2
%% Type: not-wf
%% Sections: 3.3.1 [56]
'o-p56fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p56fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p56fail3.xml
%% ID: o-p56fail3
%% Type: not-wf
%% Sections: 3.3.1 [56]
'o-p56fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p56fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p56fail4.xml
%% ID: o-p56fail4
%% Type: not-wf
%% Sections: 3.3.1 [56]
'o-p56fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p56fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p56fail5.xml
%% ID: o-p56fail5
%% Type: not-wf
%% Sections: 3.3.1 [56]
'o-p56fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p56fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p57fail1.xml
%% ID: o-p57fail1
%% Type: not-wf
%% Sections: 3.3.1 [57]
'o-p57fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p57fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail1.xml
%% ID: o-p58fail1
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail2.xml
%% ID: o-p58fail2
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail3.xml
%% ID: o-p58fail3
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail4.xml
%% ID: o-p58fail4
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail5.xml
%% ID: o-p58fail5
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail6.xml
%% ID: o-p58fail6
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail6'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail6.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail7.xml
%% ID: o-p58fail7
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail7'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail7.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p58fail8.xml
%% ID: o-p58fail8
%% Type: not-wf
%% Sections: 3.3.1 [58]
'o-p58fail8'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p58fail8.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p59fail1.xml
%% ID: o-p59fail1
%% Type: not-wf
%% Sections: 3.3.1 [59]
'o-p59fail1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p59fail1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p59fail2.xml
%% ID: o-p59fail2
%% Type: not-wf
%% Sections: 3.3.1 [59]
'o-p59fail2'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p59fail2.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p59fail3.xml
%% ID: o-p59fail3
%% Type: not-wf
%% Sections: 3.3.1 [59]
'o-p59fail3'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p59fail3.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p60fail1.xml
%% ID: o-p60fail1
%% Type: not-wf
%% Sections: 3.3.2 [60]
'o-p60fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p60fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p60fail2.xml
%% ID: o-p60fail2
%% Type: not-wf
%% Sections: 3.3.2 [60]
'o-p60fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p60fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p60fail3.xml
%% ID: o-p60fail3
%% Type: not-wf
%% Sections: 3.3.2 [60]
'o-p60fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p60fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p60fail4.xml
%% ID: o-p60fail4
%% Type: not-wf
%% Sections: 3.3.2 [60]
'o-p60fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p60fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p60fail5.xml
%% ID: o-p60fail5
%% Type: not-wf
%% Sections: 3.3.2 [60]
'o-p60fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p60fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p61fail1.xml
%% ID: o-p61fail1
%% Type: not-wf
%% Sections: 3.4 [61]
'o-p61fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p61fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p62fail1.xml
%% ID: o-p62fail1
%% Type: not-wf
%% Sections: 3.4 [62]
'o-p62fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p62fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p62fail2.xml
%% ID: o-p62fail2
%% Type: not-wf
%% Sections: 3.4 [62]
'o-p62fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p62fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p63fail1.xml
%% ID: o-p63fail1
%% Type: not-wf
%% Sections: 3.4 [63]
'o-p63fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p63fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p63fail2.xml
%% ID: o-p63fail2
%% Type: not-wf
%% Sections: 3.4 [63]
'o-p63fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p63fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p64fail1.xml
%% ID: o-p64fail1
%% Type: not-wf
%% Sections: 3.4 [64]
'o-p64fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p64fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p64fail2.xml
%% ID: o-p64fail2
%% Type: not-wf
%% Sections: 3.4 [64]
'o-p64fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p64fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66fail1.xml
%% ID: o-p66fail1
%% Type: not-wf
%% Sections: 4.1 [66]
'o-p66fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66fail2.xml
%% ID: o-p66fail2
%% Type: not-wf
%% Sections: 4.1 [66]
'o-p66fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66fail3.xml
%% ID: o-p66fail3
%% Type: not-wf
%% Sections: 4.1 [66]
'o-p66fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66fail4.xml
%% ID: o-p66fail4
%% Type: not-wf
%% Sections: 4.1 [66]
'o-p66fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66fail5.xml
%% ID: o-p66fail5
%% Type: not-wf
%% Sections: 4.1 [66]
'o-p66fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p66fail6.xml
%% ID: o-p66fail6
%% Type: not-wf
%% Sections: 4.1 [66]
'o-p66fail6'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p66fail6.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p68fail1.xml
%% ID: o-p68fail1
%% Type: not-wf
%% Sections: 4.1 [68]
'o-p68fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p68fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p68fail2.xml
%% ID: o-p68fail2
%% Type: not-wf
%% Sections: 4.1 [68]
'o-p68fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p68fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p68fail3.xml
%% ID: o-p68fail3
%% Type: not-wf
%% Sections: 4.1 [68]
'o-p68fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p68fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p69fail1.xml
%% ID: o-p69fail1
%% Type: not-wf
%% Sections: 4.1 [69]
'o-p69fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p69fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p69fail2.xml
%% ID: o-p69fail2
%% Type: not-wf
%% Sections: 4.1 [69]
'o-p69fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p69fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p69fail3.xml
%% ID: o-p69fail3
%% Type: not-wf
%% Sections: 4.1 [69]
'o-p69fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p69fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p70fail1.xml
%% ID: o-p70fail1
%% Type: not-wf
%% Sections: 4.2 [70]
'o-p70fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p70fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p71fail1.xml
%% ID: o-p71fail1
%% Type: not-wf
%% Sections: 4.2 [71]
'o-p71fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p71fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p71fail2.xml
%% ID: o-p71fail2
%% Type: not-wf
%% Sections: 4.2 [71]
'o-p71fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p71fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p71fail3.xml
%% ID: o-p71fail3
%% Type: not-wf
%% Sections: 4.2 [71]
'o-p71fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p71fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p71fail4.xml
%% ID: o-p71fail4
%% Type: not-wf
%% Sections: 4.2 [71]
'o-p71fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p71fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p72fail1.xml
%% ID: o-p72fail1
%% Type: not-wf
%% Sections: 4.2 [72]
'o-p72fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p72fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p72fail2.xml
%% ID: o-p72fail2
%% Type: not-wf
%% Sections: 4.2 [72]
'o-p72fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p72fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p72fail3.xml
%% ID: o-p72fail3
%% Type: not-wf
%% Sections: 4.2 [72]
'o-p72fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p72fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p72fail4.xml
%% ID: o-p72fail4
%% Type: not-wf
%% Sections: 4.2 [72]
'o-p72fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p72fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p73fail1.xml
%% ID: o-p73fail1
%% Type: not-wf
%% Sections: 4.2 [73]
'o-p73fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p73fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p73fail2.xml
%% ID: o-p73fail2
%% Type: not-wf
%% Sections: 4.2 [73]
'o-p73fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p73fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p73fail3.xml
%% ID: o-p73fail3
%% Type: not-wf
%% Sections: 4.2 [73]
'o-p73fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p73fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p73fail4.xml
%% ID: o-p73fail4
%% Type: not-wf
%% Sections: 4.2 [73]
'o-p73fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p73fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p73fail5.xml
%% ID: o-p73fail5
%% Type: not-wf
%% Sections: 4.2 [73]
'o-p73fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p73fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p74fail1.xml
%% ID: o-p74fail1
%% Type: not-wf
%% Sections: 4.2 [74]
'o-p74fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p74fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p74fail2.xml
%% ID: o-p74fail2
%% Type: not-wf
%% Sections: 4.2 [74]
'o-p74fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p74fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p74fail3.xml
%% ID: o-p74fail3
%% Type: not-wf
%% Sections: 4.2 [74]
'o-p74fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p74fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75fail1.xml
%% ID: o-p75fail1
%% Type: not-wf
%% Sections: 4.2.2 [75]
'o-p75fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75fail2.xml
%% ID: o-p75fail2
%% Type: not-wf
%% Sections: 4.2.2 [75]
'o-p75fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75fail3.xml
%% ID: o-p75fail3
%% Type: not-wf
%% Sections: 4.2.2 [75]
'o-p75fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75fail4.xml
%% ID: o-p75fail4
%% Type: not-wf
%% Sections: 4.2.2 [75]
'o-p75fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75fail5.xml
%% ID: o-p75fail5
%% Type: not-wf
%% Sections: 4.2.2 [75]
'o-p75fail5'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75fail5.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p75fail6.xml
%% ID: o-p75fail6
%% Type: not-wf
%% Sections: 4.2.2 [75]
'o-p75fail6'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p75fail6.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p76fail1.xml
%% ID: o-p76fail1
%% Type: not-wf
%% Sections: 4.2.2 [76]
'o-p76fail1'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p76fail1.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p76fail2.xml
%% ID: o-p76fail2
%% Type: not-wf
%% Sections: 4.2.2 [76]
'o-p76fail2'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p76fail2.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p76fail3.xml
%% ID: o-p76fail3
%% Type: not-wf
%% Sections: 4.2.2 [76]
'o-p76fail3'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p76fail3.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p76fail4.xml
%% ID: o-p76fail4
%% Type: not-wf
%% Sections: 4.2.2 [76]
'o-p76fail4'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p76fail4.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: p11pass1.xml
%% ID: o-p11pass1
%% Type: error
%% Sections: 2.3, 4.2.2 [11]
'o-p11pass1'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"oasis","p11pass1.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: OASIS/NIST TESTS, 1-Nov-1998


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: OASIS/NIST XML 1.0 Tests


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P28/ibm28i01.xml
%% ID: ibm-invalid-P28-ibm28i01.xml
%% Type: invalid
%% Sections: 2.8
'ibm-invalid-P28-ibm28i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P28/ibm28i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 28


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P32/ibm32i01.xml
%% ID: ibm-invalid-P32-ibm32i01.xml
%% Type: invalid
%% Sections: 2.9
'ibm-invalid-P32-ibm32i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P32/ibm32i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P32/ibm32i03.xml
%% ID: ibm-invalid-P32-ibm32i03.xml
%% Type: invalid
%% Sections: 2.9
'ibm-invalid-P32-ibm32i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P32/ibm32i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P32/ibm32i04.xml
%% ID: ibm-invalid-P32-ibm32i04.xml
%% Type: invalid
%% Sections: 2.9
'ibm-invalid-P32-ibm32i04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P32/ibm32i04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 32


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P39/ibm39i01.xml
%% ID: ibm-invalid-P39-ibm39i01.xml
%% Type: invalid
%% Sections: 3
'ibm-invalid-P39-ibm39i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P39/ibm39i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P39/ibm39i02.xml
%% ID: ibm-invalid-P39-ibm39i02.xml
%% Type: invalid
%% Sections: 3
'ibm-invalid-P39-ibm39i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P39/ibm39i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P39/ibm39i03.xml
%% ID: ibm-invalid-P39-ibm39i03.xml
%% Type: invalid
%% Sections: 3
'ibm-invalid-P39-ibm39i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P39/ibm39i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P39/ibm39i04.xml
%% ID: ibm-invalid-P39-ibm39i04.xml
%% Type: invalid
%% Sections: 3
'ibm-invalid-P39-ibm39i04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P39/ibm39i04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 39


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P41/ibm41i01.xml
%% ID: ibm-invalid-P41-ibm41i01.xml
%% Type: invalid
%% Sections: 3.1
'ibm-invalid-P41-ibm41i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P41/ibm41i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P41/ibm41i02.xml
%% ID: ibm-invalid-P41-ibm41i02.xml
%% Type: invalid
%% Sections: 3.1
'ibm-invalid-P41-ibm41i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P41/ibm41i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 41


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P45/ibm45i01.xml
%% ID: ibm-invalid-P45-ibm45i01.xml
%% Type: invalid
%% Sections: 3.2
'ibm-invalid-P45-ibm45i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P45/ibm45i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 45


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P49/ibm49i01.xml
%% ID: ibm-invalid-P49-ibm49i01.xml
%% Type: invalid
%% Sections: 3.2.1
'ibm-invalid-P49-ibm49i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P49/ibm49i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 49


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P50/ibm50i01.xml
%% ID: ibm-invalid-P50-ibm50i01.xml
%% Type: invalid
%% Sections: 3.2.1
'ibm-invalid-P50-ibm50i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P50/ibm50i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 50


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P51/ibm51i01.xml
%% ID: ibm-invalid-P51-ibm51i01.xml
%% Type: invalid
%% Sections: 3.2.2
'ibm-invalid-P51-ibm51i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P51/ibm51i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P51/ibm51i03.xml
%% ID: ibm-invalid-P51-ibm51i03.xml
%% Type: invalid
%% Sections: 3.2.2
'ibm-invalid-P51-ibm51i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P51/ibm51i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 51


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i01.xml
%% ID: ibm-invalid-P56-ibm56i01.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i02.xml
%% ID: ibm-invalid-P56-ibm56i02.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i03.xml
%% ID: ibm-invalid-P56-ibm56i03.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i05.xml
%% ID: ibm-invalid-P56-ibm56i05.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i06.xml
%% ID: ibm-invalid-P56-ibm56i06.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i07.xml
%% ID: ibm-invalid-P56-ibm56i07.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i08.xml
%% ID: ibm-invalid-P56-ibm56i08.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i09.xml
%% ID: ibm-invalid-P56-ibm56i09.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i10.xml
%% ID: ibm-invalid-P56-ibm56i10.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i11.xml
%% ID: ibm-invalid-P56-ibm56i11.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i12.xml
%% ID: ibm-invalid-P56-ibm56i12.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i13.xml
%% ID: ibm-invalid-P56-ibm56i13.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i14.xml
%% ID: ibm-invalid-P56-ibm56i14.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i15.xml
%% ID: ibm-invalid-P56-ibm56i15.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i16.xml
%% ID: ibm-invalid-P56-ibm56i16.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i17.xml
%% ID: ibm-invalid-P56-ibm56i17.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P56/ibm56i18.xml
%% ID: ibm-invalid-P56-ibm56i18.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P56-ibm56i18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P56/ibm56i18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 56


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P58/ibm58i01.xml
%% ID: ibm-invalid-P58-ibm58i01.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P58-ibm58i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P58/ibm58i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P58/ibm58i02.xml
%% ID: ibm-invalid-P58-ibm58i02.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P58-ibm58i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P58/ibm58i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 58


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P59/ibm59i01.xml
%% ID: ibm-invalid-P59-ibm59i01.xml
%% Type: invalid
%% Sections: 3.3.1
'ibm-invalid-P59-ibm59i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P59/ibm59i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 59


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P60/ibm60i01.xml
%% ID: ibm-invalid-P60-ibm60i01.xml
%% Type: invalid
%% Sections: 3.3.2
'ibm-invalid-P60-ibm60i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P60/ibm60i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P60/ibm60i02.xml
%% ID: ibm-invalid-P60-ibm60i02.xml
%% Type: invalid
%% Sections: 3.3.2
'ibm-invalid-P60-ibm60i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P60/ibm60i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P60/ibm60i03.xml
%% ID: ibm-invalid-P60-ibm60i03.xml
%% Type: invalid
%% Sections: 3.3.2
'ibm-invalid-P60-ibm60i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P60/ibm60i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P60/ibm60i04.xml
%% ID: ibm-invalid-P60-ibm60i04.xml
%% Type: invalid
%% Sections: 3.3.2
'ibm-invalid-P60-ibm60i04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P60/ibm60i04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 60


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P68/ibm68i01.xml
%% ID: ibm-invalid-P68-ibm68i01.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P68-ibm68i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P68/ibm68i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P68/ibm68i02.xml
%% ID: ibm-invalid-P68-ibm68i02.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P68-ibm68i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P68/ibm68i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P68/ibm68i03.xml
%% ID: ibm-invalid-P68-ibm68i03.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P68-ibm68i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P68/ibm68i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P68/ibm68i04.xml
%% ID: ibm-invalid-P68-ibm68i04.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P68-ibm68i04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P68/ibm68i04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 68


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P69/ibm69i01.xml
%% ID: ibm-invalid-P69-ibm69i01.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P69-ibm69i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P69/ibm69i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P69/ibm69i02.xml
%% ID: ibm-invalid-P69-ibm69i02.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P69-ibm69i02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P69/ibm69i02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P69/ibm69i03.xml
%% ID: ibm-invalid-P69-ibm69i03.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P69-ibm69i03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P69/ibm69i03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P69/ibm69i04.xml
%% ID: ibm-invalid-P69-ibm69i04.xml
%% Type: error
%% Sections: 4.1
'ibm-invalid-P69-ibm69i04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P69/ibm69i04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 69


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: invalid/P76/ibm76i01.xml
%% ID: ibm-invalid-P76-ibm76i01.xml
%% Type: invalid
%% Sections: 4.2.2
'ibm-invalid-P76-ibm76i01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","invalid/P76/ibm76i01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "invalid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 76


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - invalid tests


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P01/ibm01n01.xml
%% ID: ibm-not-wf-P01-ibm01n01.xml
%% Type: not-wf
%% Sections: 2.1
'ibm-not-wf-P01-ibm01n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P01/ibm01n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P01/ibm01n02.xml
%% ID: ibm-not-wf-P01-ibm01n02.xml
%% Type: not-wf
%% Sections: 2.1
'ibm-not-wf-P01-ibm01n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P01/ibm01n02.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_, <<"<?xml version=\"1.0\"?>", _/binary>>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   % R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   % check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P01/ibm01n03.xml
%% ID: ibm-not-wf-P01-ibm01n03.xml
%% Type: not-wf
%% Sections: 2.1
'ibm-not-wf-P01-ibm01n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P01/ibm01n03.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_, <<"<title>Wrong combination!</title>", _/binary>>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 1


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n01.xml
%% ID: ibm-not-wf-P02-ibm02n01.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n02.xml
%% ID: ibm-not-wf-P02-ibm02n02.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n03.xml
%% ID: ibm-not-wf-P02-ibm02n03.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n04.xml
%% ID: ibm-not-wf-P02-ibm02n04.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n05.xml
%% ID: ibm-not-wf-P02-ibm02n05.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n06.xml
%% ID: ibm-not-wf-P02-ibm02n06.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n07.xml
%% ID: ibm-not-wf-P02-ibm02n07.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n08.xml
%% ID: ibm-not-wf-P02-ibm02n08.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n09.xml
%% ID: ibm-not-wf-P02-ibm02n09.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n10.xml
%% ID: ibm-not-wf-P02-ibm02n10.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n11.xml
%% ID: ibm-not-wf-P02-ibm02n11.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n12.xml
%% ID: ibm-not-wf-P02-ibm02n12.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n13.xml
%% ID: ibm-not-wf-P02-ibm02n13.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n14.xml
%% ID: ibm-not-wf-P02-ibm02n14.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n15.xml
%% ID: ibm-not-wf-P02-ibm02n15.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n16.xml
%% ID: ibm-not-wf-P02-ibm02n16.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n17.xml
%% ID: ibm-not-wf-P02-ibm02n17.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n18.xml
%% ID: ibm-not-wf-P02-ibm02n18.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n19.xml
%% ID: ibm-not-wf-P02-ibm02n19.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n19'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n19.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n20.xml
%% ID: ibm-not-wf-P02-ibm02n20.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n20'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n20.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n21.xml
%% ID: ibm-not-wf-P02-ibm02n21.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n21'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n21.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n22.xml
%% ID: ibm-not-wf-P02-ibm02n22.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n22'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n22.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n23.xml
%% ID: ibm-not-wf-P02-ibm02n23.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n23'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n23.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n24.xml
%% ID: ibm-not-wf-P02-ibm02n24.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n24'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n24.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n25.xml
%% ID: ibm-not-wf-P02-ibm02n25.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n25'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n25.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n26.xml
%% ID: ibm-not-wf-P02-ibm02n26.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n26'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n26.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n27.xml
%% ID: ibm-not-wf-P02-ibm02n27.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n27'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n27.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n28.xml
%% ID: ibm-not-wf-P02-ibm02n28.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n28'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n28.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n29.xml
%% ID: ibm-not-wf-P02-ibm02n29.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n29'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n29.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n30.xml
%% ID: ibm-not-wf-P02-ibm02n30.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n30'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n30.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n31.xml
%% ID: ibm-not-wf-P02-ibm02n31.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n31'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n31.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n32.xml
%% ID: ibm-not-wf-P02-ibm02n32.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n32'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n32.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P02/ibm02n33.xml
%% ID: ibm-not-wf-P02-ibm02n33.xml
%% Type: not-wf
%% Sections: 2.2
'ibm-not-wf-P02-ibm02n33'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P02/ibm02n33.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 2


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P03/ibm03n01.xml
%% ID: ibm-not-wf-P03-ibm03n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P03-ibm03n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P03/ibm03n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 3


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n01.xml
%% ID: ibm-not-wf-P04-ibm04n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n02.xml
%% ID: ibm-not-wf-P04-ibm04n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n03.xml
%% ID: ibm-not-wf-P04-ibm04n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n04.xml
%% ID: ibm-not-wf-P04-ibm04n04.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n05.xml
%% ID: ibm-not-wf-P04-ibm04n05.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n06.xml
%% ID: ibm-not-wf-P04-ibm04n06.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n07.xml
%% ID: ibm-not-wf-P04-ibm04n07.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n08.xml
%% ID: ibm-not-wf-P04-ibm04n08.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n09.xml
%% ID: ibm-not-wf-P04-ibm04n09.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n10.xml
%% ID: ibm-not-wf-P04-ibm04n10.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n11.xml
%% ID: ibm-not-wf-P04-ibm04n11.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n12.xml
%% ID: ibm-not-wf-P04-ibm04n12.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n13.xml
%% ID: ibm-not-wf-P04-ibm04n13.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n14.xml
%% ID: ibm-not-wf-P04-ibm04n14.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n15.xml
%% ID: ibm-not-wf-P04-ibm04n15.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n16.xml
%% ID: ibm-not-wf-P04-ibm04n16.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n17.xml
%% ID: ibm-not-wf-P04-ibm04n17.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P04/ibm04n18.xml
%% ID: ibm-not-wf-P04-ibm04n18.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P04-ibm04n18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P04/ibm04n18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 4


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P05/ibm05n01.xml
%% ID: ibm-not-wf-P05-ibm05n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P05-ibm05n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P05/ibm05n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P05/ibm05n02.xml
%% ID: ibm-not-wf-P05-ibm05n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P05-ibm05n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P05/ibm05n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P05/ibm05n03.xml
%% ID: ibm-not-wf-P05-ibm05n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P05-ibm05n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P05/ibm05n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 5


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P09/ibm09n01.xml
%% ID: ibm-not-wf-P09-ibm09n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P09-ibm09n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P09/ibm09n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P09/ibm09n02.xml
%% ID: ibm-not-wf-P09-ibm09n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P09-ibm09n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P09/ibm09n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P09/ibm09n03.xml
%% ID: ibm-not-wf-P09-ibm09n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P09-ibm09n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P09/ibm09n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P09/ibm09n04.xml
%% ID: ibm-not-wf-P09-ibm09n04.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P09-ibm09n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P09/ibm09n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 9


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n01.xml
%% ID: ibm-not-wf-P10-ibm10n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n02.xml
%% ID: ibm-not-wf-P10-ibm10n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n03.xml
%% ID: ibm-not-wf-P10-ibm10n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n04.xml
%% ID: ibm-not-wf-P10-ibm10n04.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n05.xml
%% ID: ibm-not-wf-P10-ibm10n05.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n06.xml
%% ID: ibm-not-wf-P10-ibm10n06.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n07.xml
%% ID: ibm-not-wf-P10-ibm10n07.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P10/ibm10n08.xml
%% ID: ibm-not-wf-P10-ibm10n08.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P10-ibm10n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P10/ibm10n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 10


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P11/ibm11n01.xml
%% ID: ibm-not-wf-P11-ibm11n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P11-ibm11n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P11/ibm11n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P11/ibm11n02.xml
%% ID: ibm-not-wf-P11-ibm11n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P11-ibm11n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P11/ibm11n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P11/ibm11n03.xml
%% ID: ibm-not-wf-P11-ibm11n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P11-ibm11n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P11/ibm11n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P11/ibm11n04.xml
%% ID: ibm-not-wf-P11-ibm11n04.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P11-ibm11n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P11/ibm11n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 11


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P12/ibm12n01.xml
%% ID: ibm-not-wf-P12-ibm12n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P12-ibm12n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P12/ibm12n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P12/ibm12n02.xml
%% ID: ibm-not-wf-P12-ibm12n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P12-ibm12n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P12/ibm12n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P12/ibm12n03.xml
%% ID: ibm-not-wf-P12-ibm12n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P12-ibm12n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P12/ibm12n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 12


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P13/ibm13n01.xml
%% ID: ibm-not-wf-P13-ibm13n01.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P13-ibm13n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P13/ibm13n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P13/ibm13n02.xml
%% ID: ibm-not-wf-P13-ibm13n02.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P13-ibm13n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P13/ibm13n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P13/ibm13n03.xml
%% ID: ibm-not-wf-P13-ibm13n03.xml
%% Type: not-wf
%% Sections: 2.3
'ibm-not-wf-P13-ibm13n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P13/ibm13n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 13


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P14/ibm14n01.xml
%% ID: ibm-not-wf-P14-ibm14n01.xml
%% Type: not-wf
%% Sections: 2.4
'ibm-not-wf-P14-ibm14n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P14/ibm14n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P14/ibm14n02.xml
%% ID: ibm-not-wf-P14-ibm14n02.xml
%% Type: not-wf
%% Sections: 2.4
'ibm-not-wf-P14-ibm14n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P14/ibm14n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P14/ibm14n03.xml
%% ID: ibm-not-wf-P14-ibm14n03.xml
%% Type: not-wf
%% Sections: 2.4
'ibm-not-wf-P14-ibm14n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P14/ibm14n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 14


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P15/ibm15n01.xml
%% ID: ibm-not-wf-P15-ibm15n01.xml
%% Type: not-wf
%% Sections: 2.5
'ibm-not-wf-P15-ibm15n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P15/ibm15n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P15/ibm15n02.xml
%% ID: ibm-not-wf-P15-ibm15n02.xml
%% Type: not-wf
%% Sections: 2.5
'ibm-not-wf-P15-ibm15n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P15/ibm15n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P15/ibm15n03.xml
%% ID: ibm-not-wf-P15-ibm15n03.xml
%% Type: not-wf
%% Sections: 2.5
'ibm-not-wf-P15-ibm15n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P15/ibm15n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P15/ibm15n04.xml
%% ID: ibm-not-wf-P15-ibm15n04.xml
%% Type: not-wf
%% Sections: 2.5
'ibm-not-wf-P15-ibm15n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P15/ibm15n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 15


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P16/ibm16n01.xml
%% ID: ibm-not-wf-P16-ibm16n01.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P16-ibm16n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P16/ibm16n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P16/ibm16n02.xml
%% ID: ibm-not-wf-P16-ibm16n02.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P16-ibm16n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P16/ibm16n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P16/ibm16n03.xml
%% ID: ibm-not-wf-P16-ibm16n03.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P16-ibm16n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P16/ibm16n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P16/ibm16n04.xml
%% ID: ibm-not-wf-P16-ibm16n04.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P16-ibm16n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P16/ibm16n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 16


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P17/ibm17n01.xml
%% ID: ibm-not-wf-P17-ibm17n01.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P17-ibm17n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P17/ibm17n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P17/ibm17n02.xml
%% ID: ibm-not-wf-P17-ibm17n02.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P17-ibm17n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P17/ibm17n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P17/ibm17n03.xml
%% ID: ibm-not-wf-P17-ibm17n03.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P17-ibm17n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P17/ibm17n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P17/ibm17n04.xml
%% ID: ibm-not-wf-P17-ibm17n04.xml
%% Type: not-wf
%% Sections: 2.6
'ibm-not-wf-P17-ibm17n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P17/ibm17n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 17


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P18/ibm18n01.xml
%% ID: ibm-not-wf-P18-ibm18n01.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P18-ibm18n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P18/ibm18n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P18/ibm18n02.xml
%% ID: ibm-not-wf-P18-ibm18n02.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P18-ibm18n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P18/ibm18n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 18


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P19/ibm19n01.xml
%% ID: ibm-not-wf-P19-ibm19n01.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P19-ibm19n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P19/ibm19n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P19/ibm19n02.xml
%% ID: ibm-not-wf-P19-ibm19n02.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P19-ibm19n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P19/ibm19n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P19/ibm19n03.xml
%% ID: ibm-not-wf-P19-ibm19n03.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P19-ibm19n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P19/ibm19n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 19


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P20/ibm20n01.xml
%% ID: ibm-not-wf-P20-ibm20n01.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P20-ibm20n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P20/ibm20n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 20


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P21/ibm21n01.xml
%% ID: ibm-not-wf-P21-ibm21n01.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P21-ibm21n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P21/ibm21n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P21/ibm21n02.xml
%% ID: ibm-not-wf-P21-ibm21n02.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P21-ibm21n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P21/ibm21n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P21/ibm21n03.xml
%% ID: ibm-not-wf-P21-ibm21n03.xml
%% Type: not-wf
%% Sections: 2.7
'ibm-not-wf-P21-ibm21n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P21/ibm21n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 21


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P22/ibm22n01.xml
%% ID: ibm-not-wf-P22-ibm22n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P22-ibm22n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P22/ibm22n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P22/ibm22n02.xml
%% ID: ibm-not-wf-P22-ibm22n02.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P22-ibm22n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P22/ibm22n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P22/ibm22n03.xml
%% ID: ibm-not-wf-P22-ibm22n03.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P22-ibm22n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P22/ibm22n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 22


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P23/ibm23n01.xml
%% ID: ibm-not-wf-P23-ibm23n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P23-ibm23n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P23/ibm23n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P23/ibm23n02.xml
%% ID: ibm-not-wf-P23-ibm23n02.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P23-ibm23n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P23/ibm23n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P23/ibm23n03.xml
%% ID: ibm-not-wf-P23-ibm23n03.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P23-ibm23n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P23/ibm23n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P23/ibm23n04.xml
%% ID: ibm-not-wf-P23-ibm23n04.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P23-ibm23n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P23/ibm23n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P23/ibm23n05.xml
%% ID: ibm-not-wf-P23-ibm23n05.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P23-ibm23n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P23/ibm23n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P23/ibm23n06.xml
%% ID: ibm-not-wf-P23-ibm23n06.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P23-ibm23n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P23/ibm23n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 23


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n01.xml
%% ID: ibm-not-wf-P24-ibm24n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n02.xml
%% ID: ibm-not-wf-P24-ibm24n02.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n03.xml
%% ID: ibm-not-wf-P24-ibm24n03.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n04.xml
%% ID: ibm-not-wf-P24-ibm24n04.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n05.xml
%% ID: ibm-not-wf-P24-ibm24n05.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n05'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n05.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n06.xml
%% ID: ibm-not-wf-P24-ibm24n06.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n07.xml
%% ID: ibm-not-wf-P24-ibm24n07.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n07'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n07.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n08.xml
%% ID: ibm-not-wf-P24-ibm24n08.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n08'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n08.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P24/ibm24n09.xml
%% ID: ibm-not-wf-P24-ibm24n09.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P24-ibm24n09'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P24/ibm24n09.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 24


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P25/ibm25n01.xml
%% ID: ibm-not-wf-P25-ibm25n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P25-ibm25n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P25/ibm25n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P25/ibm25n02.xml
%% ID: ibm-not-wf-P25-ibm25n02.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P25-ibm25n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P25/ibm25n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 25


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P26/ibm26n01.xml
%% ID: ibm-not-wf-P26-ibm26n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P26-ibm26n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P26/ibm26n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 26


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P27/ibm27n01.xml
%% ID: ibm-not-wf-P27-ibm27n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P27-ibm27n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P27/ibm27n01.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_, <<"<!ELEMENT cat EMPTY>">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 27


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n01.xml
%% ID: ibm-not-wf-P28-ibm28n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n02.xml
%% ID: ibm-not-wf-P28-ibm28n02.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n03.xml
%% ID: ibm-not-wf-P28-ibm28n03.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n04.xml
%% ID: ibm-not-wf-P28-ibm28n04.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n05.xml
%% ID: ibm-not-wf-P28-ibm28n05.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n06.xml
%% ID: ibm-not-wf-P28-ibm28n06.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n07.xml
%% ID: ibm-not-wf-P28-ibm28n07.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P28/ibm28n08.xml
%% ID: ibm-not-wf-P28-ibm28n08.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P28-ibm28n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P28/ibm28n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 28


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/p28a/ibm28an01.xml
%% ID: ibm-not-wf-p28a-ibm28an01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-p28a-ibm28an01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/p28a/ibm28an01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 28a


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n01.xml
%% ID: ibm-not-wf-P29-ibm29n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n02.xml
%% ID: ibm-not-wf-P29-ibm29n02.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n03.xml
%% ID: ibm-not-wf-P29-ibm29n03.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n04.xml
%% ID: ibm-not-wf-P29-ibm29n04.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n05.xml
%% ID: ibm-not-wf-P29-ibm29n05.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n06.xml
%% ID: ibm-not-wf-P29-ibm29n06.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P29/ibm29n07.xml
%% ID: ibm-not-wf-P29-ibm29n07.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P29-ibm29n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P29/ibm29n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 29


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P30/ibm30n01.xml
%% ID: ibm-not-wf-P30-ibm30n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P30-ibm30n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P30/ibm30n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 30


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P31/ibm31n01.xml
%% ID: ibm-not-wf-P31-ibm31n01.xml
%% Type: not-wf
%% Sections: 2.8
'ibm-not-wf-P31-ibm31n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P31/ibm31n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 31


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n01.xml
%% ID: ibm-not-wf-P32-ibm32n01.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n02.xml
%% ID: ibm-not-wf-P32-ibm32n02.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n03.xml
%% ID: ibm-not-wf-P32-ibm32n03.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n04.xml
%% ID: ibm-not-wf-P32-ibm32n04.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n05.xml
%% ID: ibm-not-wf-P32-ibm32n05.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n06.xml
%% ID: ibm-not-wf-P32-ibm32n06.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n07.xml
%% ID: ibm-not-wf-P32-ibm32n07.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n08.xml
%% ID: ibm-not-wf-P32-ibm32n08.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n08'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n08.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P32/ibm32n09.xml
%% ID: ibm-not-wf-P32-ibm32n09.xml
%% Type: not-wf
%% Sections: 2.9
'ibm-not-wf-P32-ibm32n09'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P32/ibm32n09.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 32


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P39/ibm39n01.xml
%% ID: ibm-not-wf-P39-ibm39n01.xml
%% Type: not-wf
%% Sections: 3
'ibm-not-wf-P39-ibm39n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P39/ibm39n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P39/ibm39n02.xml
%% ID: ibm-not-wf-P39-ibm39n02.xml
%% Type: not-wf
%% Sections: 3
'ibm-not-wf-P39-ibm39n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P39/ibm39n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P39/ibm39n03.xml
%% ID: ibm-not-wf-P39-ibm39n03.xml
%% Type: not-wf
%% Sections: 3
'ibm-not-wf-P39-ibm39n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P39/ibm39n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P39/ibm39n04.xml
%% ID: ibm-not-wf-P39-ibm39n04.xml
%% Type: not-wf
%% Sections: 3
'ibm-not-wf-P39-ibm39n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P39/ibm39n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P39/ibm39n05.xml
%% ID: ibm-not-wf-P39-ibm39n05.xml
%% Type: not-wf
%% Sections: 3
'ibm-not-wf-P39-ibm39n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P39/ibm39n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P39/ibm39n06.xml
%% ID: ibm-not-wf-P39-ibm39n06.xml
%% Type: not-wf
%% Sections: 3
'ibm-not-wf-P39-ibm39n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P39/ibm39n06.xml"]),
   %% Special case becase we returns everything after a legal document 
   %% as an rest instead of giving and error to let the user handle 
   %% multipple docs on a stream.
   {ok,_,<<"content after end tag\r\n">>} = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]).
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 39


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P40/ibm40n01.xml
%% ID: ibm-not-wf-P40-ibm40n01.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P40-ibm40n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P40/ibm40n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P40/ibm40n02.xml
%% ID: ibm-not-wf-P40-ibm40n02.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P40-ibm40n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P40/ibm40n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P40/ibm40n03.xml
%% ID: ibm-not-wf-P40-ibm40n03.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P40-ibm40n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P40/ibm40n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P40/ibm40n04.xml
%% ID: ibm-not-wf-P40-ibm40n04.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P40-ibm40n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P40/ibm40n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P40/ibm40n05.xml
%% ID: ibm-not-wf-P40-ibm40n05.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P40-ibm40n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P40/ibm40n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 40


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n01.xml
%% ID: ibm-not-wf-P41-ibm41n01.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n02.xml
%% ID: ibm-not-wf-P41-ibm41n02.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n03.xml
%% ID: ibm-not-wf-P41-ibm41n03.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n04.xml
%% ID: ibm-not-wf-P41-ibm41n04.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n05.xml
%% ID: ibm-not-wf-P41-ibm41n05.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n06.xml
%% ID: ibm-not-wf-P41-ibm41n06.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n07.xml
%% ID: ibm-not-wf-P41-ibm41n07.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n08.xml
%% ID: ibm-not-wf-P41-ibm41n08.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n09.xml
%% ID: ibm-not-wf-P41-ibm41n09.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n10.xml
%% ID: ibm-not-wf-P41-ibm41n10.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n11.xml
%% ID: ibm-not-wf-P41-ibm41n11.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n12.xml
%% ID: ibm-not-wf-P41-ibm41n12.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n13.xml
%% ID: ibm-not-wf-P41-ibm41n13.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P41/ibm41n14.xml
%% ID: ibm-not-wf-P41-ibm41n14.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P41-ibm41n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P41/ibm41n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 41


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P42/ibm42n01.xml
%% ID: ibm-not-wf-P42-ibm42n01.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P42-ibm42n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P42/ibm42n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P42/ibm42n02.xml
%% ID: ibm-not-wf-P42-ibm42n02.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P42-ibm42n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P42/ibm42n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P42/ibm42n03.xml
%% ID: ibm-not-wf-P42-ibm42n03.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P42-ibm42n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P42/ibm42n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P42/ibm42n04.xml
%% ID: ibm-not-wf-P42-ibm42n04.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P42-ibm42n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P42/ibm42n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P42/ibm42n05.xml
%% ID: ibm-not-wf-P42-ibm42n05.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P42-ibm42n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P42/ibm42n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 42


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P43/ibm43n01.xml
%% ID: ibm-not-wf-P43-ibm43n01.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P43-ibm43n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P43/ibm43n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P43/ibm43n02.xml
%% ID: ibm-not-wf-P43-ibm43n02.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P43-ibm43n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P43/ibm43n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P43/ibm43n04.xml
%% ID: ibm-not-wf-P43-ibm43n04.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P43-ibm43n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P43/ibm43n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P43/ibm43n05.xml
%% ID: ibm-not-wf-P43-ibm43n05.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P43-ibm43n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P43/ibm43n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 43


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P44/ibm44n01.xml
%% ID: ibm-not-wf-P44-ibm44n01.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P44-ibm44n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P44/ibm44n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P44/ibm44n02.xml
%% ID: ibm-not-wf-P44-ibm44n02.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P44-ibm44n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P44/ibm44n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P44/ibm44n03.xml
%% ID: ibm-not-wf-P44-ibm44n03.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P44-ibm44n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P44/ibm44n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P44/ibm44n04.xml
%% ID: ibm-not-wf-P44-ibm44n04.xml
%% Type: not-wf
%% Sections: 3.1
'ibm-not-wf-P44-ibm44n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P44/ibm44n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 44


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n01.xml
%% ID: ibm-not-wf-P45-ibm45n01.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n02.xml
%% ID: ibm-not-wf-P45-ibm45n02.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n03.xml
%% ID: ibm-not-wf-P45-ibm45n03.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n04.xml
%% ID: ibm-not-wf-P45-ibm45n04.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n05.xml
%% ID: ibm-not-wf-P45-ibm45n05.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n06.xml
%% ID: ibm-not-wf-P45-ibm45n06.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n07.xml
%% ID: ibm-not-wf-P45-ibm45n07.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n08.xml
%% ID: ibm-not-wf-P45-ibm45n08.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P45/ibm45n09.xml
%% ID: ibm-not-wf-P45-ibm45n09.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P45-ibm45n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P45/ibm45n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 45


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P46/ibm46n01.xml
%% ID: ibm-not-wf-P46-ibm46n01.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P46-ibm46n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P46/ibm46n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P46/ibm46n02.xml
%% ID: ibm-not-wf-P46-ibm46n02.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P46-ibm46n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P46/ibm46n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P46/ibm46n03.xml
%% ID: ibm-not-wf-P46-ibm46n03.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P46-ibm46n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P46/ibm46n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P46/ibm46n04.xml
%% ID: ibm-not-wf-P46-ibm46n04.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P46-ibm46n04'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P46/ibm46n04.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P46/ibm46n05.xml
%% ID: ibm-not-wf-P46-ibm46n05.xml
%% Type: not-wf
%% Sections: 3.2
'ibm-not-wf-P46-ibm46n05'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P46/ibm46n05.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 46


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P47/ibm47n01.xml
%% ID: ibm-not-wf-P47-ibm47n01.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P47-ibm47n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P47/ibm47n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P47/ibm47n02.xml
%% ID: ibm-not-wf-P47-ibm47n02.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P47-ibm47n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P47/ibm47n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P47/ibm47n03.xml
%% ID: ibm-not-wf-P47-ibm47n03.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P47-ibm47n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P47/ibm47n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P47/ibm47n04.xml
%% ID: ibm-not-wf-P47-ibm47n04.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P47-ibm47n04'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P47/ibm47n04.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P47/ibm47n05.xml
%% ID: ibm-not-wf-P47-ibm47n05.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P47-ibm47n05'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P47/ibm47n05.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P47/ibm47n06.xml
%% ID: ibm-not-wf-P47-ibm47n06.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P47-ibm47n06'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P47/ibm47n06.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 47


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n01.xml
%% ID: ibm-not-wf-P48-ibm48n01.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n02.xml
%% ID: ibm-not-wf-P48-ibm48n02.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n03.xml
%% ID: ibm-not-wf-P48-ibm48n03.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n04.xml
%% ID: ibm-not-wf-P48-ibm48n04.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n04'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n04.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n05.xml
%% ID: ibm-not-wf-P48-ibm48n05.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n05'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n05.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n06.xml
%% ID: ibm-not-wf-P48-ibm48n06.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n06'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n06.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P48/ibm48n07.xml
%% ID: ibm-not-wf-P48-ibm48n07.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P48-ibm48n07'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P48/ibm48n07.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 48


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P49/ibm49n01.xml
%% ID: ibm-not-wf-P49-ibm49n01.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P49-ibm49n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P49/ibm49n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P49/ibm49n02.xml
%% ID: ibm-not-wf-P49-ibm49n02.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P49-ibm49n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P49/ibm49n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P49/ibm49n03.xml
%% ID: ibm-not-wf-P49-ibm49n03.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P49-ibm49n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P49/ibm49n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P49/ibm49n04.xml
%% ID: ibm-not-wf-P49-ibm49n04.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P49-ibm49n04'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P49/ibm49n04.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P49/ibm49n05.xml
%% ID: ibm-not-wf-P49-ibm49n05.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P49-ibm49n05'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P49/ibm49n05.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P49/ibm49n06.xml
%% ID: ibm-not-wf-P49-ibm49n06.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P49-ibm49n06'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P49/ibm49n06.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 49


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n01.xml
%% ID: ibm-not-wf-P50-ibm50n01.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n02.xml
%% ID: ibm-not-wf-P50-ibm50n02.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n03.xml
%% ID: ibm-not-wf-P50-ibm50n03.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n04.xml
%% ID: ibm-not-wf-P50-ibm50n04.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n05.xml
%% ID: ibm-not-wf-P50-ibm50n05.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n06.xml
%% ID: ibm-not-wf-P50-ibm50n06.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P50/ibm50n07.xml
%% ID: ibm-not-wf-P50-ibm50n07.xml
%% Type: not-wf
%% Sections: 3.2.1
'ibm-not-wf-P50-ibm50n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P50/ibm50n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 50

%%   ['ibm-not-wf-P50-ibm50n01','ibm-not-wf-P50-ibm50n02','ibm-not-wf-P50-ibm50n03','ibm-not-wf-P50-ibm50n04','ibm-not-wf-P50-ibm50n05','ibm-not-wf-P50-ibm50n06','ibm-not-wf-P50-ibm50n07'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n01.xml
%% ID: ibm-not-wf-P51-ibm51n01.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n02.xml
%% ID: ibm-not-wf-P51-ibm51n02.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n03.xml
%% ID: ibm-not-wf-P51-ibm51n03.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n04.xml
%% ID: ibm-not-wf-P51-ibm51n04.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n05.xml
%% ID: ibm-not-wf-P51-ibm51n05.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n06.xml
%% ID: ibm-not-wf-P51-ibm51n06.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P51/ibm51n07.xml
%% ID: ibm-not-wf-P51-ibm51n07.xml
%% Type: not-wf
%% Sections: 3.2.2
'ibm-not-wf-P51-ibm51n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P51/ibm51n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 51

%%   ['ibm-not-wf-P51-ibm51n01','ibm-not-wf-P51-ibm51n02','ibm-not-wf-P51-ibm51n03','ibm-not-wf-P51-ibm51n04','ibm-not-wf-P51-ibm51n05','ibm-not-wf-P51-ibm51n06','ibm-not-wf-P51-ibm51n07'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P52/ibm52n01.xml
%% ID: ibm-not-wf-P52-ibm52n01.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P52-ibm52n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P52/ibm52n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P52/ibm52n02.xml
%% ID: ibm-not-wf-P52-ibm52n02.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P52-ibm52n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P52/ibm52n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P52/ibm52n03.xml
%% ID: ibm-not-wf-P52-ibm52n03.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P52-ibm52n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P52/ibm52n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P52/ibm52n04.xml
%% ID: ibm-not-wf-P52-ibm52n04.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P52-ibm52n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P52/ibm52n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P52/ibm52n05.xml
%% ID: ibm-not-wf-P52-ibm52n05.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P52-ibm52n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P52/ibm52n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P52/ibm52n06.xml
%% ID: ibm-not-wf-P52-ibm52n06.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P52-ibm52n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P52/ibm52n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 52


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n01.xml
%% ID: ibm-not-wf-P53-ibm53n01.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n02.xml
%% ID: ibm-not-wf-P53-ibm53n02.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n03.xml
%% ID: ibm-not-wf-P53-ibm53n03.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n04.xml
%% ID: ibm-not-wf-P53-ibm53n04.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n05.xml
%% ID: ibm-not-wf-P53-ibm53n05.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n06.xml
%% ID: ibm-not-wf-P53-ibm53n06.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n07.xml
%% ID: ibm-not-wf-P53-ibm53n07.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P53/ibm53n08.xml
%% ID: ibm-not-wf-P53-ibm53n08.xml
%% Type: not-wf
%% Sections: 3.3
'ibm-not-wf-P53-ibm53n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P53/ibm53n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 53


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P54/ibm54n01.xml
%% ID: ibm-not-wf-P54-ibm54n01.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P54-ibm54n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P54/ibm54n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P54/ibm54n02.xml
%% ID: ibm-not-wf-P54-ibm54n02.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P54-ibm54n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P54/ibm54n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 54


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P55/ibm55n01.xml
%% ID: ibm-not-wf-P55-ibm55n01.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P55-ibm55n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P55/ibm55n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P55/ibm55n02.xml
%% ID: ibm-not-wf-P55-ibm55n02.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P55-ibm55n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P55/ibm55n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P55/ibm55n03.xml
%% ID: ibm-not-wf-P55-ibm55n03.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P55-ibm55n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P55/ibm55n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 55


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n01.xml
%% ID: ibm-not-wf-P56-ibm56n01.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n02.xml
%% ID: ibm-not-wf-P56-ibm56n02.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n03.xml
%% ID: ibm-not-wf-P56-ibm56n03.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n04.xml
%% ID: ibm-not-wf-P56-ibm56n04.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n05.xml
%% ID: ibm-not-wf-P56-ibm56n05.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n06.xml
%% ID: ibm-not-wf-P56-ibm56n06.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P56/ibm56n07.xml
%% ID: ibm-not-wf-P56-ibm56n07.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P56-ibm56n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P56/ibm56n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 56


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P57/ibm57n01.xml
%% ID: ibm-not-wf-P57-ibm57n01.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P57-ibm57n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P57/ibm57n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 57


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n01.xml
%% ID: ibm-not-wf-P58-ibm58n01.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n02.xml
%% ID: ibm-not-wf-P58-ibm58n02.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n03.xml
%% ID: ibm-not-wf-P58-ibm58n03.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n04.xml
%% ID: ibm-not-wf-P58-ibm58n04.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n05.xml
%% ID: ibm-not-wf-P58-ibm58n05.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n06.xml
%% ID: ibm-not-wf-P58-ibm58n06.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n06'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n06.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n07.xml
%% ID: ibm-not-wf-P58-ibm58n07.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P58/ibm58n08.xml
%% ID: ibm-not-wf-P58-ibm58n08.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P58-ibm58n08'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P58/ibm58n08.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 58


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P59/ibm59n01.xml
%% ID: ibm-not-wf-P59-ibm59n01.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P59-ibm59n01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P59/ibm59n01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P59/ibm59n02.xml
%% ID: ibm-not-wf-P59-ibm59n02.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P59-ibm59n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P59/ibm59n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P59/ibm59n03.xml
%% ID: ibm-not-wf-P59-ibm59n03.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P59-ibm59n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P59/ibm59n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P59/ibm59n04.xml
%% ID: ibm-not-wf-P59-ibm59n04.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P59-ibm59n04'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P59/ibm59n04.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P59/ibm59n05.xml
%% ID: ibm-not-wf-P59-ibm59n05.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P59-ibm59n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P59/ibm59n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P59/ibm59n06.xml
%% ID: ibm-not-wf-P59-ibm59n06.xml
%% Type: not-wf
%% Sections: 3.3.1
'ibm-not-wf-P59-ibm59n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P59/ibm59n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 59


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n01.xml
%% ID: ibm-not-wf-P60-ibm60n01.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n02.xml
%% ID: ibm-not-wf-P60-ibm60n02.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n03.xml
%% ID: ibm-not-wf-P60-ibm60n03.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n04.xml
%% ID: ibm-not-wf-P60-ibm60n04.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n05.xml
%% ID: ibm-not-wf-P60-ibm60n05.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n06.xml
%% ID: ibm-not-wf-P60-ibm60n06.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n07.xml
%% ID: ibm-not-wf-P60-ibm60n07.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P60/ibm60n08.xml
%% ID: ibm-not-wf-P60-ibm60n08.xml
%% Type: not-wf
%% Sections: 3.3.2
'ibm-not-wf-P60-ibm60n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P60/ibm60n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 60


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P61/ibm61n01.xml
%% ID: ibm-not-wf-P61-ibm61n01.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P61-ibm61n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P61/ibm61n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 61


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n01.xml
%% ID: ibm-not-wf-P62-ibm62n01.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n02.xml
%% ID: ibm-not-wf-P62-ibm62n02.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n03.xml
%% ID: ibm-not-wf-P62-ibm62n03.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n04.xml
%% ID: ibm-not-wf-P62-ibm62n04.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n05.xml
%% ID: ibm-not-wf-P62-ibm62n05.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n06.xml
%% ID: ibm-not-wf-P62-ibm62n06.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n07.xml
%% ID: ibm-not-wf-P62-ibm62n07.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P62/ibm62n08.xml
%% ID: ibm-not-wf-P62-ibm62n08.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P62-ibm62n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P62/ibm62n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 62


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n01.xml
%% ID: ibm-not-wf-P63-ibm63n01.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n02.xml
%% ID: ibm-not-wf-P63-ibm63n02.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n03.xml
%% ID: ibm-not-wf-P63-ibm63n03.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n04.xml
%% ID: ibm-not-wf-P63-ibm63n04.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n05.xml
%% ID: ibm-not-wf-P63-ibm63n05.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n06.xml
%% ID: ibm-not-wf-P63-ibm63n06.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P63/ibm63n07.xml
%% ID: ibm-not-wf-P63-ibm63n07.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P63-ibm63n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P63/ibm63n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 63


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P64/ibm64n01.xml
%% ID: ibm-not-wf-P64-ibm64n01.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P64-ibm64n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P64/ibm64n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P64/ibm64n02.xml
%% ID: ibm-not-wf-P64-ibm64n02.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P64-ibm64n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P64/ibm64n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P64/ibm64n03.xml
%% ID: ibm-not-wf-P64-ibm64n03.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P64-ibm64n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P64/ibm64n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 64


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P65/ibm65n01.xml
%% ID: ibm-not-wf-P65-ibm65n01.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P65-ibm65n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P65/ibm65n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P65/ibm65n02.xml
%% ID: ibm-not-wf-P65-ibm65n02.xml
%% Type: not-wf
%% Sections: 3.4
'ibm-not-wf-P65-ibm65n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P65/ibm65n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 65


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n01.xml
%% ID: ibm-not-wf-P66-ibm66n01.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n02.xml
%% ID: ibm-not-wf-P66-ibm66n02.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n03.xml
%% ID: ibm-not-wf-P66-ibm66n03.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n04.xml
%% ID: ibm-not-wf-P66-ibm66n04.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n05.xml
%% ID: ibm-not-wf-P66-ibm66n05.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n06.xml
%% ID: ibm-not-wf-P66-ibm66n06.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n07.xml
%% ID: ibm-not-wf-P66-ibm66n07.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n08.xml
%% ID: ibm-not-wf-P66-ibm66n08.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n09.xml
%% ID: ibm-not-wf-P66-ibm66n09.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n10.xml
%% ID: ibm-not-wf-P66-ibm66n10.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n11.xml
%% ID: ibm-not-wf-P66-ibm66n11.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n12.xml
%% ID: ibm-not-wf-P66-ibm66n12.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n13.xml
%% ID: ibm-not-wf-P66-ibm66n13.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n14.xml
%% ID: ibm-not-wf-P66-ibm66n14.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P66/ibm66n15.xml
%% ID: ibm-not-wf-P66-ibm66n15.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P66-ibm66n15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P66/ibm66n15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 66


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n01.xml
%% ID: ibm-not-wf-P68-ibm68n01.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n02.xml
%% ID: ibm-not-wf-P68-ibm68n02.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n03.xml
%% ID: ibm-not-wf-P68-ibm68n03.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n04.xml
%% ID: ibm-not-wf-P68-ibm68n04.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n05.xml
%% ID: ibm-not-wf-P68-ibm68n05.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n06.xml
%% ID: ibm-not-wf-P68-ibm68n06.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n06'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n06.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n07.xml
%% ID: ibm-not-wf-P68-ibm68n07.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n08.xml
%% ID: ibm-not-wf-P68-ibm68n08.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n09.xml
%% ID: ibm-not-wf-P68-ibm68n09.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n09'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n09.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P68/ibm68n10.xml
%% ID: ibm-not-wf-P68-ibm68n10.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P68-ibm68n10'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P68/ibm68n10.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 68


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n01.xml
%% ID: ibm-not-wf-P69-ibm69n01.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n02.xml
%% ID: ibm-not-wf-P69-ibm69n02.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n03.xml
%% ID: ibm-not-wf-P69-ibm69n03.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n04.xml
%% ID: ibm-not-wf-P69-ibm69n04.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n05.xml
%% ID: ibm-not-wf-P69-ibm69n05.xml
%% Type: error
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "error").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n06.xml
%% ID: ibm-not-wf-P69-ibm69n06.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n06'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n06.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P69/ibm69n07.xml
%% ID: ibm-not-wf-P69-ibm69n07.xml
%% Type: not-wf
%% Sections: 4.1
'ibm-not-wf-P69-ibm69n07'(_Config) ->  {skip, "No loop detection yet"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P69/ibm69n07.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 69


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm70n01.xml
%% ID: ibm-not-wf-P71-ibm70n01.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm70n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm70n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n01.xml
%% ID: ibm-not-wf-P71-ibm71n01.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n02.xml
%% ID: ibm-not-wf-P71-ibm71n02.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n03.xml
%% ID: ibm-not-wf-P71-ibm71n03.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n04.xml
%% ID: ibm-not-wf-P71-ibm71n04.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n05.xml
%% ID: ibm-not-wf-P71-ibm71n05.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n06.xml
%% ID: ibm-not-wf-P71-ibm71n06.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n07.xml
%% ID: ibm-not-wf-P71-ibm71n07.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P71/ibm71n08.xml
%% ID: ibm-not-wf-P71-ibm71n08.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P71-ibm71n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P71/ibm71n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 71


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n01.xml
%% ID: ibm-not-wf-P72-ibm72n01.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n02.xml
%% ID: ibm-not-wf-P72-ibm72n02.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n03.xml
%% ID: ibm-not-wf-P72-ibm72n03.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n04.xml
%% ID: ibm-not-wf-P72-ibm72n04.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n05.xml
%% ID: ibm-not-wf-P72-ibm72n05.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n06.xml
%% ID: ibm-not-wf-P72-ibm72n06.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n07.xml
%% ID: ibm-not-wf-P72-ibm72n07.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n08.xml
%% ID: ibm-not-wf-P72-ibm72n08.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P72/ibm72n09.xml
%% ID: ibm-not-wf-P72-ibm72n09.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P72-ibm72n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P72/ibm72n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 72


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P73/ibm73n01.xml
%% ID: ibm-not-wf-P73-ibm73n01.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P73-ibm73n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P73/ibm73n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P73/ibm73n03.xml
%% ID: ibm-not-wf-P73-ibm73n03.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P73-ibm73n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P73/ibm73n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 73


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P74/ibm74n01.xml
%% ID: ibm-not-wf-P74-ibm74n01.xml
%% Type: not-wf
%% Sections: 4.2
'ibm-not-wf-P74-ibm74n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P74/ibm74n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 74


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n01.xml
%% ID: ibm-not-wf-P75-ibm75n01.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n02.xml
%% ID: ibm-not-wf-P75-ibm75n02.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n03.xml
%% ID: ibm-not-wf-P75-ibm75n03.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n04.xml
%% ID: ibm-not-wf-P75-ibm75n04.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n05.xml
%% ID: ibm-not-wf-P75-ibm75n05.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n06.xml
%% ID: ibm-not-wf-P75-ibm75n06.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n07.xml
%% ID: ibm-not-wf-P75-ibm75n07.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n08.xml
%% ID: ibm-not-wf-P75-ibm75n08.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n09.xml
%% ID: ibm-not-wf-P75-ibm75n09.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n10.xml
%% ID: ibm-not-wf-P75-ibm75n10.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n11.xml
%% ID: ibm-not-wf-P75-ibm75n11.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n12.xml
%% ID: ibm-not-wf-P75-ibm75n12.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P75/ibm75n13.xml
%% ID: ibm-not-wf-P75-ibm75n13.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P75-ibm75n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P75/ibm75n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 75


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n01.xml
%% ID: ibm-not-wf-P76-ibm76n01.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n02.xml
%% ID: ibm-not-wf-P76-ibm76n02.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n03.xml
%% ID: ibm-not-wf-P76-ibm76n03.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n04.xml
%% ID: ibm-not-wf-P76-ibm76n04.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n05.xml
%% ID: ibm-not-wf-P76-ibm76n05.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n06.xml
%% ID: ibm-not-wf-P76-ibm76n06.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P76/ibm76n07.xml
%% ID: ibm-not-wf-P76-ibm76n07.xml
%% Type: not-wf
%% Sections: 4.2.2
'ibm-not-wf-P76-ibm76n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P76/ibm76n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 76


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P77/ibm77n01.xml
%% ID: ibm-not-wf-P77-ibm77n01.xml
%% Type: not-wf
%% Sections: 4.3.1
'ibm-not-wf-P77-ibm77n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P77/ibm77n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P77/ibm77n02.xml
%% ID: ibm-not-wf-P77-ibm77n02.xml
%% Type: not-wf
%% Sections: 4.3.1
'ibm-not-wf-P77-ibm77n02'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P77/ibm77n02.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P77/ibm77n03.xml
%% ID: ibm-not-wf-P77-ibm77n03.xml
%% Type: not-wf
%% Sections: 4.3.1
'ibm-not-wf-P77-ibm77n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P77/ibm77n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P77/ibm77n04.xml
%% ID: ibm-not-wf-P77-ibm77n04.xml
%% Type: not-wf
%% Sections: 4.3.1
'ibm-not-wf-P77-ibm77n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P77/ibm77n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 77


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P78/ibm78n01.xml
%% ID: ibm-not-wf-P78-ibm78n01.xml
%% Type: not-wf
%% Sections: 4.3.2
'ibm-not-wf-P78-ibm78n01'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P78/ibm78n01.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P78/ibm78n02.xml
%% ID: ibm-not-wf-P78-ibm78n02.xml
%% Type: not-wf
%% Sections: 4.3.2
'ibm-not-wf-P78-ibm78n02'(Config) ->  {skip, "Fix 3"}.
   %%file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   %%Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P78/ibm78n02.xml"]),
   %%R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   %%check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 78


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P79/ibm79n01.xml
%% ID: ibm-not-wf-P79-ibm79n01.xml
%% Type: not-wf
%% Sections: 4.3.2
'ibm-not-wf-P79-ibm79n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P79/ibm79n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P79/ibm79n02.xml
%% ID: ibm-not-wf-P79-ibm79n02.xml
%% Type: not-wf
%% Sections: 4.3.2
'ibm-not-wf-P79-ibm79n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P79/ibm79n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 79


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P80/ibm80n01.xml
%% ID: ibm-not-wf-P80-ibm80n01.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P80-ibm80n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P80/ibm80n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P80/ibm80n02.xml
%% ID: ibm-not-wf-P80-ibm80n02.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P80-ibm80n02'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P80/ibm80n02.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P80/ibm80n03.xml
%% ID: ibm-not-wf-P80-ibm80n03.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P80-ibm80n03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P80/ibm80n03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P80/ibm80n04.xml
%% ID: ibm-not-wf-P80-ibm80n04.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P80-ibm80n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P80/ibm80n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P80/ibm80n05.xml
%% ID: ibm-not-wf-P80-ibm80n05.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P80-ibm80n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P80/ibm80n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P80/ibm80n06.xml
%% ID: ibm-not-wf-P80-ibm80n06.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P80-ibm80n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P80/ibm80n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 80


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n01.xml
%% ID: ibm-not-wf-P81-ibm81n01.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n02.xml
%% ID: ibm-not-wf-P81-ibm81n02.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n03.xml
%% ID: ibm-not-wf-P81-ibm81n03.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n04.xml
%% ID: ibm-not-wf-P81-ibm81n04.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n05.xml
%% ID: ibm-not-wf-P81-ibm81n05.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n06.xml
%% ID: ibm-not-wf-P81-ibm81n06.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n07.xml
%% ID: ibm-not-wf-P81-ibm81n07.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n08.xml
%% ID: ibm-not-wf-P81-ibm81n08.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P81/ibm81n09.xml
%% ID: ibm-not-wf-P81-ibm81n09.xml
%% Type: not-wf
%% Sections: 4.3.3
'ibm-not-wf-P81-ibm81n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P81/ibm81n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 81


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n01.xml
%% ID: ibm-not-wf-P82-ibm82n01.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n02.xml
%% ID: ibm-not-wf-P82-ibm82n02.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n03.xml
%% ID: ibm-not-wf-P82-ibm82n03.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n04.xml
%% ID: ibm-not-wf-P82-ibm82n04.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n05.xml
%% ID: ibm-not-wf-P82-ibm82n05.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n06.xml
%% ID: ibm-not-wf-P82-ibm82n06.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n07.xml
%% ID: ibm-not-wf-P82-ibm82n07.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P82/ibm82n08.xml
%% ID: ibm-not-wf-P82-ibm82n08.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P82-ibm82n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P82/ibm82n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 82


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P83/ibm83n01.xml
%% ID: ibm-not-wf-P83-ibm83n01.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P83-ibm83n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P83/ibm83n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P83/ibm83n02.xml
%% ID: ibm-not-wf-P83-ibm83n02.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P83-ibm83n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P83/ibm83n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P83/ibm83n03.xml
%% ID: ibm-not-wf-P83-ibm83n03.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P83-ibm83n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P83/ibm83n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P83/ibm83n04.xml
%% ID: ibm-not-wf-P83-ibm83n04.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P83-ibm83n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P83/ibm83n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P83/ibm83n05.xml
%% ID: ibm-not-wf-P83-ibm83n05.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P83-ibm83n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P83/ibm83n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P83/ibm83n06.xml
%% ID: ibm-not-wf-P83-ibm83n06.xml
%% Type: not-wf
%% Sections: 4.7
'ibm-not-wf-P83-ibm83n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P83/ibm83n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 83


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n01.xml
%% ID: ibm-not-wf-P85-ibm85n01.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n02.xml
%% ID: ibm-not-wf-P85-ibm85n02.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n03.xml
%% ID: ibm-not-wf-P85-ibm85n03.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n04.xml
%% ID: ibm-not-wf-P85-ibm85n04.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n05.xml
%% ID: ibm-not-wf-P85-ibm85n05.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n06.xml
%% ID: ibm-not-wf-P85-ibm85n06.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n07.xml
%% ID: ibm-not-wf-P85-ibm85n07.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n08.xml
%% ID: ibm-not-wf-P85-ibm85n08.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n09.xml
%% ID: ibm-not-wf-P85-ibm85n09.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n10.xml
%% ID: ibm-not-wf-P85-ibm85n10.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n100.xml
%% ID: ibm-not-wf-P85-ibm85n100.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n100'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n100.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n101.xml
%% ID: ibm-not-wf-P85-ibm85n101.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n101'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n101.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n102.xml
%% ID: ibm-not-wf-P85-ibm85n102.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n102'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n102.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n103.xml
%% ID: ibm-not-wf-P85-ibm85n103.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n103'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n103.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n104.xml
%% ID: ibm-not-wf-P85-ibm85n104.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n104'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n104.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n105.xml
%% ID: ibm-not-wf-P85-ibm85n105.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n105'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n105.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n106.xml
%% ID: ibm-not-wf-P85-ibm85n106.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n106'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n106.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n107.xml
%% ID: ibm-not-wf-P85-ibm85n107.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n107'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n107.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n108.xml
%% ID: ibm-not-wf-P85-ibm85n108.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n108'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n108.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n109.xml
%% ID: ibm-not-wf-P85-ibm85n109.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n109'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n109.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n11.xml
%% ID: ibm-not-wf-P85-ibm85n11.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n110.xml
%% ID: ibm-not-wf-P85-ibm85n110.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n110'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n110.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n111.xml
%% ID: ibm-not-wf-P85-ibm85n111.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n111'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n111.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n112.xml
%% ID: ibm-not-wf-P85-ibm85n112.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n112'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n112.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n113.xml
%% ID: ibm-not-wf-P85-ibm85n113.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n113'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n113.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n114.xml
%% ID: ibm-not-wf-P85-ibm85n114.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n114'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n114.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n115.xml
%% ID: ibm-not-wf-P85-ibm85n115.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n115'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n115.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n116.xml
%% ID: ibm-not-wf-P85-ibm85n116.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n116'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n116.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n117.xml
%% ID: ibm-not-wf-P85-ibm85n117.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n117'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n117.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n118.xml
%% ID: ibm-not-wf-P85-ibm85n118.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n118'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n118.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n119.xml
%% ID: ibm-not-wf-P85-ibm85n119.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n119'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n119.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n12.xml
%% ID: ibm-not-wf-P85-ibm85n12.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n120.xml
%% ID: ibm-not-wf-P85-ibm85n120.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n120'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n120.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n121.xml
%% ID: ibm-not-wf-P85-ibm85n121.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n121'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n121.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n122.xml
%% ID: ibm-not-wf-P85-ibm85n122.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n122'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n122.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n123.xml
%% ID: ibm-not-wf-P85-ibm85n123.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n123'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n123.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n124.xml
%% ID: ibm-not-wf-P85-ibm85n124.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n124'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n124.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n125.xml
%% ID: ibm-not-wf-P85-ibm85n125.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n125'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n125.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n126.xml
%% ID: ibm-not-wf-P85-ibm85n126.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n126'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n126.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n127.xml
%% ID: ibm-not-wf-P85-ibm85n127.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n127'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n127.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n128.xml
%% ID: ibm-not-wf-P85-ibm85n128.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n128'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n128.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n129.xml
%% ID: ibm-not-wf-P85-ibm85n129.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n129'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n129.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n13.xml
%% ID: ibm-not-wf-P85-ibm85n13.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n130.xml
%% ID: ibm-not-wf-P85-ibm85n130.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n130'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n130.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n131.xml
%% ID: ibm-not-wf-P85-ibm85n131.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n131'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n131.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n132.xml
%% ID: ibm-not-wf-P85-ibm85n132.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n132'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n132.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n133.xml
%% ID: ibm-not-wf-P85-ibm85n133.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n133'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n133.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n134.xml
%% ID: ibm-not-wf-P85-ibm85n134.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n134'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n134.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n135.xml
%% ID: ibm-not-wf-P85-ibm85n135.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n135'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n135.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n136.xml
%% ID: ibm-not-wf-P85-ibm85n136.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n136'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n136.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n137.xml
%% ID: ibm-not-wf-P85-ibm85n137.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n137'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n137.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n138.xml
%% ID: ibm-not-wf-P85-ibm85n138.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n138'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n138.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n139.xml
%% ID: ibm-not-wf-P85-ibm85n139.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n139'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n139.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n14.xml
%% ID: ibm-not-wf-P85-ibm85n14.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n140.xml
%% ID: ibm-not-wf-P85-ibm85n140.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n140'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n140.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n141.xml
%% ID: ibm-not-wf-P85-ibm85n141.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n141'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n141.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n142.xml
%% ID: ibm-not-wf-P85-ibm85n142.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n142'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n142.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n143.xml
%% ID: ibm-not-wf-P85-ibm85n143.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n143'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n143.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n144.xml
%% ID: ibm-not-wf-P85-ibm85n144.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n144'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n144.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n145.xml
%% ID: ibm-not-wf-P85-ibm85n145.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n145'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n145.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n146.xml
%% ID: ibm-not-wf-P85-ibm85n146.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n146'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n146.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n147.xml
%% ID: ibm-not-wf-P85-ibm85n147.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n147'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n147.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n148.xml
%% ID: ibm-not-wf-P85-ibm85n148.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n148'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n148.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n149.xml
%% ID: ibm-not-wf-P85-ibm85n149.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n149'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n149.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n15.xml
%% ID: ibm-not-wf-P85-ibm85n15.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n150.xml
%% ID: ibm-not-wf-P85-ibm85n150.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n150'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n150.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n151.xml
%% ID: ibm-not-wf-P85-ibm85n151.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n151'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n151.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n152.xml
%% ID: ibm-not-wf-P85-ibm85n152.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n152'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n152.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n153.xml
%% ID: ibm-not-wf-P85-ibm85n153.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n153'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n153.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n154.xml
%% ID: ibm-not-wf-P85-ibm85n154.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n154'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n154.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n155.xml
%% ID: ibm-not-wf-P85-ibm85n155.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n155'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n155.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n156.xml
%% ID: ibm-not-wf-P85-ibm85n156.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n156'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n156.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n157.xml
%% ID: ibm-not-wf-P85-ibm85n157.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n157'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n157.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n158.xml
%% ID: ibm-not-wf-P85-ibm85n158.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n158'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n158.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n159.xml
%% ID: ibm-not-wf-P85-ibm85n159.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n159'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n159.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n16.xml
%% ID: ibm-not-wf-P85-ibm85n16.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n160.xml
%% ID: ibm-not-wf-P85-ibm85n160.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n160'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n160.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n161.xml
%% ID: ibm-not-wf-P85-ibm85n161.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n161'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n161.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n162.xml
%% ID: ibm-not-wf-P85-ibm85n162.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n162'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n162.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n163.xml
%% ID: ibm-not-wf-P85-ibm85n163.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n163'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n163.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n164.xml
%% ID: ibm-not-wf-P85-ibm85n164.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n164'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n164.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n165.xml
%% ID: ibm-not-wf-P85-ibm85n165.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n165'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n165.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n166.xml
%% ID: ibm-not-wf-P85-ibm85n166.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n166'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n166.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n167.xml
%% ID: ibm-not-wf-P85-ibm85n167.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n167'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n167.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n168.xml
%% ID: ibm-not-wf-P85-ibm85n168.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n168'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n168.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n169.xml
%% ID: ibm-not-wf-P85-ibm85n169.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n169'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n169.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n17.xml
%% ID: ibm-not-wf-P85-ibm85n17.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n170.xml
%% ID: ibm-not-wf-P85-ibm85n170.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n170'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n170.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n171.xml
%% ID: ibm-not-wf-P85-ibm85n171.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n171'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n171.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n172.xml
%% ID: ibm-not-wf-P85-ibm85n172.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n172'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n172.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n173.xml
%% ID: ibm-not-wf-P85-ibm85n173.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n173'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n173.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n174.xml
%% ID: ibm-not-wf-P85-ibm85n174.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n174'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n174.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n175.xml
%% ID: ibm-not-wf-P85-ibm85n175.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n175'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n175.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n176.xml
%% ID: ibm-not-wf-P85-ibm85n176.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n176'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n176.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n177.xml
%% ID: ibm-not-wf-P85-ibm85n177.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n177'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n177.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n178.xml
%% ID: ibm-not-wf-P85-ibm85n178.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n178'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n178.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n179.xml
%% ID: ibm-not-wf-P85-ibm85n179.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n179'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n179.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n18.xml
%% ID: ibm-not-wf-P85-ibm85n18.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n180.xml
%% ID: ibm-not-wf-P85-ibm85n180.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n180'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n180.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n181.xml
%% ID: ibm-not-wf-P85-ibm85n181.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n181'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n181.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n182.xml
%% ID: ibm-not-wf-P85-ibm85n182.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n182'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n182.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n183.xml
%% ID: ibm-not-wf-P85-ibm85n183.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n183'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n183.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n184.xml
%% ID: ibm-not-wf-P85-ibm85n184.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n184'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n184.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n185.xml
%% ID: ibm-not-wf-P85-ibm85n185.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n185'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n185.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n186.xml
%% ID: ibm-not-wf-P85-ibm85n186.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n186'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n186.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n187.xml
%% ID: ibm-not-wf-P85-ibm85n187.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n187'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n187.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n188.xml
%% ID: ibm-not-wf-P85-ibm85n188.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n188'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n188.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n189.xml
%% ID: ibm-not-wf-P85-ibm85n189.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n189'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n189.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n19.xml
%% ID: ibm-not-wf-P85-ibm85n19.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n19'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n19.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n190.xml
%% ID: ibm-not-wf-P85-ibm85n190.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n190'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n190.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n191.xml
%% ID: ibm-not-wf-P85-ibm85n191.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n191'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n191.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n192.xml
%% ID: ibm-not-wf-P85-ibm85n192.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n192'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n192.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n193.xml
%% ID: ibm-not-wf-P85-ibm85n193.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n193'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n193.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n194.xml
%% ID: ibm-not-wf-P85-ibm85n194.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n194'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n194.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n195.xml
%% ID: ibm-not-wf-P85-ibm85n195.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n195'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n195.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n196.xml
%% ID: ibm-not-wf-P85-ibm85n196.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n196'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n196.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n197.xml
%% ID: ibm-not-wf-P85-ibm85n197.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n197'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n197.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n198.xml
%% ID: ibm-not-wf-P85-ibm85n198.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n198'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n198.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n20.xml
%% ID: ibm-not-wf-P85-ibm85n20.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n20'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n20.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n21.xml
%% ID: ibm-not-wf-P85-ibm85n21.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n21'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n21.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n22.xml
%% ID: ibm-not-wf-P85-ibm85n22.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n22'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n22.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n23.xml
%% ID: ibm-not-wf-P85-ibm85n23.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n23'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n23.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n24.xml
%% ID: ibm-not-wf-P85-ibm85n24.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n24'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n24.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n25.xml
%% ID: ibm-not-wf-P85-ibm85n25.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n25'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n25.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n26.xml
%% ID: ibm-not-wf-P85-ibm85n26.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n26'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n26.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n27.xml
%% ID: ibm-not-wf-P85-ibm85n27.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n27'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n27.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n28.xml
%% ID: ibm-not-wf-P85-ibm85n28.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n28'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n28.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n29.xml
%% ID: ibm-not-wf-P85-ibm85n29.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n29'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n29.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n30.xml
%% ID: ibm-not-wf-P85-ibm85n30.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n30'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n30.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n31.xml
%% ID: ibm-not-wf-P85-ibm85n31.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n31'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n31.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n32.xml
%% ID: ibm-not-wf-P85-ibm85n32.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n32'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n32.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n33.xml
%% ID: ibm-not-wf-P85-ibm85n33.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n33'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n33.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n34.xml
%% ID: ibm-not-wf-P85-ibm85n34.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n34'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n34.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n35.xml
%% ID: ibm-not-wf-P85-ibm85n35.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n35'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n35.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n36.xml
%% ID: ibm-not-wf-P85-ibm85n36.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n36'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n36.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n37.xml
%% ID: ibm-not-wf-P85-ibm85n37.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n37'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n37.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n38.xml
%% ID: ibm-not-wf-P85-ibm85n38.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n38'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n38.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n39.xml
%% ID: ibm-not-wf-P85-ibm85n39.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n39'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n39.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n40.xml
%% ID: ibm-not-wf-P85-ibm85n40.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n40'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n40.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n41.xml
%% ID: ibm-not-wf-P85-ibm85n41.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n41'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n41.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n42.xml
%% ID: ibm-not-wf-P85-ibm85n42.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n42'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n42.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n43.xml
%% ID: ibm-not-wf-P85-ibm85n43.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n43'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n43.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n44.xml
%% ID: ibm-not-wf-P85-ibm85n44.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n44'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n44.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n45.xml
%% ID: ibm-not-wf-P85-ibm85n45.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n45'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n45.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n46.xml
%% ID: ibm-not-wf-P85-ibm85n46.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n46'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n46.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n47.xml
%% ID: ibm-not-wf-P85-ibm85n47.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n47'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n47.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n48.xml
%% ID: ibm-not-wf-P85-ibm85n48.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n48'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n48.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n49.xml
%% ID: ibm-not-wf-P85-ibm85n49.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n49'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n49.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n50.xml
%% ID: ibm-not-wf-P85-ibm85n50.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n50'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n50.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n51.xml
%% ID: ibm-not-wf-P85-ibm85n51.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n51'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n51.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n52.xml
%% ID: ibm-not-wf-P85-ibm85n52.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n52'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n52.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n53.xml
%% ID: ibm-not-wf-P85-ibm85n53.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n53'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n53.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n54.xml
%% ID: ibm-not-wf-P85-ibm85n54.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n54'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n54.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n55.xml
%% ID: ibm-not-wf-P85-ibm85n55.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n55'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n55.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n56.xml
%% ID: ibm-not-wf-P85-ibm85n56.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n56'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n56.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n57.xml
%% ID: ibm-not-wf-P85-ibm85n57.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n57'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n57.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n58.xml
%% ID: ibm-not-wf-P85-ibm85n58.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n58'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n58.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n59.xml
%% ID: ibm-not-wf-P85-ibm85n59.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n59'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n59.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n60.xml
%% ID: ibm-not-wf-P85-ibm85n60.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n60'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n60.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n61.xml
%% ID: ibm-not-wf-P85-ibm85n61.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n61'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n61.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n62.xml
%% ID: ibm-not-wf-P85-ibm85n62.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n62'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n62.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n63.xml
%% ID: ibm-not-wf-P85-ibm85n63.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n63'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n63.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n64.xml
%% ID: ibm-not-wf-P85-ibm85n64.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n64'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n64.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n65.xml
%% ID: ibm-not-wf-P85-ibm85n65.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n65'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n65.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n66.xml
%% ID: ibm-not-wf-P85-ibm85n66.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n66'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n66.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n67.xml
%% ID: ibm-not-wf-P85-ibm85n67.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n67'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n67.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n68.xml
%% ID: ibm-not-wf-P85-ibm85n68.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n68'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n68.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n69.xml
%% ID: ibm-not-wf-P85-ibm85n69.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n69'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n69.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n70.xml
%% ID: ibm-not-wf-P85-ibm85n70.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n70'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n70.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n71.xml
%% ID: ibm-not-wf-P85-ibm85n71.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n71'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n71.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n72.xml
%% ID: ibm-not-wf-P85-ibm85n72.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n72'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n72.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n73.xml
%% ID: ibm-not-wf-P85-ibm85n73.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n73'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n73.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n74.xml
%% ID: ibm-not-wf-P85-ibm85n74.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n74'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n74.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n75.xml
%% ID: ibm-not-wf-P85-ibm85n75.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n75'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n75.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n76.xml
%% ID: ibm-not-wf-P85-ibm85n76.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n76'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n76.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n77.xml
%% ID: ibm-not-wf-P85-ibm85n77.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n77'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n77.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n78.xml
%% ID: ibm-not-wf-P85-ibm85n78.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n78'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n78.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n79.xml
%% ID: ibm-not-wf-P85-ibm85n79.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n79'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n79.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n80.xml
%% ID: ibm-not-wf-P85-ibm85n80.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n80'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n80.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n81.xml
%% ID: ibm-not-wf-P85-ibm85n81.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n81'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n81.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n82.xml
%% ID: ibm-not-wf-P85-ibm85n82.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n82'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n82.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n83.xml
%% ID: ibm-not-wf-P85-ibm85n83.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n83'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n83.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n84.xml
%% ID: ibm-not-wf-P85-ibm85n84.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n84'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n84.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n85.xml
%% ID: ibm-not-wf-P85-ibm85n85.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n85'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n85.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n86.xml
%% ID: ibm-not-wf-P85-ibm85n86.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n86'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n86.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n87.xml
%% ID: ibm-not-wf-P85-ibm85n87.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n87'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n87.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n88.xml
%% ID: ibm-not-wf-P85-ibm85n88.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n88'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n88.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n89.xml
%% ID: ibm-not-wf-P85-ibm85n89.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n89'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n89.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n90.xml
%% ID: ibm-not-wf-P85-ibm85n90.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n90'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n90.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n91.xml
%% ID: ibm-not-wf-P85-ibm85n91.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n91'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n91.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n92.xml
%% ID: ibm-not-wf-P85-ibm85n92.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n92'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n92.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n93.xml
%% ID: ibm-not-wf-P85-ibm85n93.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n93'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n93.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n94.xml
%% ID: ibm-not-wf-P85-ibm85n94.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n94'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n94.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n95.xml
%% ID: ibm-not-wf-P85-ibm85n95.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n95'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n95.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n96.xml
%% ID: ibm-not-wf-P85-ibm85n96.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n96'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n96.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n97.xml
%% ID: ibm-not-wf-P85-ibm85n97.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n97'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n97.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n98.xml
%% ID: ibm-not-wf-P85-ibm85n98.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n98'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n98.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P85/ibm85n99.xml
%% ID: ibm-not-wf-P85-ibm85n99.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P85-ibm85n99'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P85/ibm85n99.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 85


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P86/ibm86n01.xml
%% ID: ibm-not-wf-P86-ibm86n01.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P86-ibm86n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P86/ibm86n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P86/ibm86n02.xml
%% ID: ibm-not-wf-P86-ibm86n02.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P86-ibm86n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P86/ibm86n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P86/ibm86n03.xml
%% ID: ibm-not-wf-P86-ibm86n03.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P86-ibm86n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P86/ibm86n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P86/ibm86n04.xml
%% ID: ibm-not-wf-P86-ibm86n04.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P86-ibm86n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P86/ibm86n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 86


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n01.xml
%% ID: ibm-not-wf-P87-ibm87n01.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n02.xml
%% ID: ibm-not-wf-P87-ibm87n02.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n03.xml
%% ID: ibm-not-wf-P87-ibm87n03.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n04.xml
%% ID: ibm-not-wf-P87-ibm87n04.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n05.xml
%% ID: ibm-not-wf-P87-ibm87n05.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n06.xml
%% ID: ibm-not-wf-P87-ibm87n06.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n07.xml
%% ID: ibm-not-wf-P87-ibm87n07.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n08.xml
%% ID: ibm-not-wf-P87-ibm87n08.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n09.xml
%% ID: ibm-not-wf-P87-ibm87n09.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n10.xml
%% ID: ibm-not-wf-P87-ibm87n10.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n11.xml
%% ID: ibm-not-wf-P87-ibm87n11.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n12.xml
%% ID: ibm-not-wf-P87-ibm87n12.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n13.xml
%% ID: ibm-not-wf-P87-ibm87n13.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n14.xml
%% ID: ibm-not-wf-P87-ibm87n14.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n15.xml
%% ID: ibm-not-wf-P87-ibm87n15.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n16.xml
%% ID: ibm-not-wf-P87-ibm87n16.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n17.xml
%% ID: ibm-not-wf-P87-ibm87n17.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n17'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n17.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n18.xml
%% ID: ibm-not-wf-P87-ibm87n18.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n18'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n18.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n19.xml
%% ID: ibm-not-wf-P87-ibm87n19.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n19'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n19.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n20.xml
%% ID: ibm-not-wf-P87-ibm87n20.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n20'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n20.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n21.xml
%% ID: ibm-not-wf-P87-ibm87n21.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n21'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n21.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n22.xml
%% ID: ibm-not-wf-P87-ibm87n22.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n22'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n22.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n23.xml
%% ID: ibm-not-wf-P87-ibm87n23.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n23'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n23.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n24.xml
%% ID: ibm-not-wf-P87-ibm87n24.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n24'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n24.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n25.xml
%% ID: ibm-not-wf-P87-ibm87n25.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n25'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n25.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n26.xml
%% ID: ibm-not-wf-P87-ibm87n26.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n26'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n26.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n27.xml
%% ID: ibm-not-wf-P87-ibm87n27.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n27'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n27.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n28.xml
%% ID: ibm-not-wf-P87-ibm87n28.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n28'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n28.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n29.xml
%% ID: ibm-not-wf-P87-ibm87n29.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n29'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n29.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n30.xml
%% ID: ibm-not-wf-P87-ibm87n30.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n30'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n30.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n31.xml
%% ID: ibm-not-wf-P87-ibm87n31.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n31'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n31.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n32.xml
%% ID: ibm-not-wf-P87-ibm87n32.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n32'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n32.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n33.xml
%% ID: ibm-not-wf-P87-ibm87n33.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n33'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n33.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n34.xml
%% ID: ibm-not-wf-P87-ibm87n34.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n34'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n34.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n35.xml
%% ID: ibm-not-wf-P87-ibm87n35.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n35'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n35.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n36.xml
%% ID: ibm-not-wf-P87-ibm87n36.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n36'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n36.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n37.xml
%% ID: ibm-not-wf-P87-ibm87n37.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n37'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n37.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n38.xml
%% ID: ibm-not-wf-P87-ibm87n38.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n38'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n38.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n39.xml
%% ID: ibm-not-wf-P87-ibm87n39.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n39'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n39.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n40.xml
%% ID: ibm-not-wf-P87-ibm87n40.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n40'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n40.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n41.xml
%% ID: ibm-not-wf-P87-ibm87n41.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n41'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n41.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n42.xml
%% ID: ibm-not-wf-P87-ibm87n42.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n42'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n42.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n43.xml
%% ID: ibm-not-wf-P87-ibm87n43.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n43'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n43.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n44.xml
%% ID: ibm-not-wf-P87-ibm87n44.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n44'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n44.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n45.xml
%% ID: ibm-not-wf-P87-ibm87n45.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n45'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n45.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n46.xml
%% ID: ibm-not-wf-P87-ibm87n46.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n46'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n46.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n47.xml
%% ID: ibm-not-wf-P87-ibm87n47.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n47'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n47.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n48.xml
%% ID: ibm-not-wf-P87-ibm87n48.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n48'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n48.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n49.xml
%% ID: ibm-not-wf-P87-ibm87n49.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n49'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n49.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n50.xml
%% ID: ibm-not-wf-P87-ibm87n50.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n50'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n50.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n51.xml
%% ID: ibm-not-wf-P87-ibm87n51.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n51'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n51.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n52.xml
%% ID: ibm-not-wf-P87-ibm87n52.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n52'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n52.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n53.xml
%% ID: ibm-not-wf-P87-ibm87n53.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n53'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n53.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n54.xml
%% ID: ibm-not-wf-P87-ibm87n54.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n54'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n54.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n55.xml
%% ID: ibm-not-wf-P87-ibm87n55.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n55'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n55.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n56.xml
%% ID: ibm-not-wf-P87-ibm87n56.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n56'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n56.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n57.xml
%% ID: ibm-not-wf-P87-ibm87n57.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n57'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n57.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n58.xml
%% ID: ibm-not-wf-P87-ibm87n58.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n58'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n58.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n59.xml
%% ID: ibm-not-wf-P87-ibm87n59.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n59'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n59.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n60.xml
%% ID: ibm-not-wf-P87-ibm87n60.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n60'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n60.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n61.xml
%% ID: ibm-not-wf-P87-ibm87n61.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n61'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n61.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n62.xml
%% ID: ibm-not-wf-P87-ibm87n62.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n62'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n62.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n63.xml
%% ID: ibm-not-wf-P87-ibm87n63.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n63'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n63.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n64.xml
%% ID: ibm-not-wf-P87-ibm87n64.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n64'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n64.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n66.xml
%% ID: ibm-not-wf-P87-ibm87n66.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n66'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n66.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n67.xml
%% ID: ibm-not-wf-P87-ibm87n67.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n67'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n67.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n68.xml
%% ID: ibm-not-wf-P87-ibm87n68.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n68'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n68.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n69.xml
%% ID: ibm-not-wf-P87-ibm87n69.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n69'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n69.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n70.xml
%% ID: ibm-not-wf-P87-ibm87n70.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n70'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n70.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n71.xml
%% ID: ibm-not-wf-P87-ibm87n71.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n71'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n71.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n72.xml
%% ID: ibm-not-wf-P87-ibm87n72.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n72'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n72.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n73.xml
%% ID: ibm-not-wf-P87-ibm87n73.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n73'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n73.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n74.xml
%% ID: ibm-not-wf-P87-ibm87n74.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n74'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n74.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n75.xml
%% ID: ibm-not-wf-P87-ibm87n75.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n75'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n75.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n76.xml
%% ID: ibm-not-wf-P87-ibm87n76.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n76'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n76.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n77.xml
%% ID: ibm-not-wf-P87-ibm87n77.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n77'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n77.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n78.xml
%% ID: ibm-not-wf-P87-ibm87n78.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n78'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n78.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n79.xml
%% ID: ibm-not-wf-P87-ibm87n79.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n79'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n79.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n80.xml
%% ID: ibm-not-wf-P87-ibm87n80.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n80'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n80.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n81.xml
%% ID: ibm-not-wf-P87-ibm87n81.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n81'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n81.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n82.xml
%% ID: ibm-not-wf-P87-ibm87n82.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n82'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n82.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n83.xml
%% ID: ibm-not-wf-P87-ibm87n83.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n83'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n83.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n84.xml
%% ID: ibm-not-wf-P87-ibm87n84.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n84'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n84.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P87/ibm87n85.xml
%% ID: ibm-not-wf-P87-ibm87n85.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P87-ibm87n85'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P87/ibm87n85.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 87


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n01.xml
%% ID: ibm-not-wf-P88-ibm88n01.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n02.xml
%% ID: ibm-not-wf-P88-ibm88n02.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n03.xml
%% ID: ibm-not-wf-P88-ibm88n03.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n04.xml
%% ID: ibm-not-wf-P88-ibm88n04.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n05.xml
%% ID: ibm-not-wf-P88-ibm88n05.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n06.xml
%% ID: ibm-not-wf-P88-ibm88n06.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n08.xml
%% ID: ibm-not-wf-P88-ibm88n08.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n09.xml
%% ID: ibm-not-wf-P88-ibm88n09.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n10.xml
%% ID: ibm-not-wf-P88-ibm88n10.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n11.xml
%% ID: ibm-not-wf-P88-ibm88n11.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n12.xml
%% ID: ibm-not-wf-P88-ibm88n12.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n13.xml
%% ID: ibm-not-wf-P88-ibm88n13.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n13'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n13.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n14.xml
%% ID: ibm-not-wf-P88-ibm88n14.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n14'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n14.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n15.xml
%% ID: ibm-not-wf-P88-ibm88n15.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n15'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n15.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P88/ibm88n16.xml
%% ID: ibm-not-wf-P88-ibm88n16.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P88-ibm88n16'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P88/ibm88n16.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 88


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n01.xml
%% ID: ibm-not-wf-P89-ibm89n01.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n02.xml
%% ID: ibm-not-wf-P89-ibm89n02.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n03.xml
%% ID: ibm-not-wf-P89-ibm89n03.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n04.xml
%% ID: ibm-not-wf-P89-ibm89n04.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n05.xml
%% ID: ibm-not-wf-P89-ibm89n05.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n06.xml
%% ID: ibm-not-wf-P89-ibm89n06.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n07.xml
%% ID: ibm-not-wf-P89-ibm89n07.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n08.xml
%% ID: ibm-not-wf-P89-ibm89n08.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n09.xml
%% ID: ibm-not-wf-P89-ibm89n09.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n10.xml
%% ID: ibm-not-wf-P89-ibm89n10.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n11.xml
%% ID: ibm-not-wf-P89-ibm89n11.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n11'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n11.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: not-wf/P89/ibm89n12.xml
%% ID: ibm-not-wf-P89-ibm89n12.xml
%% Type: not-wf
%% Sections: B.
'ibm-not-wf-P89-ibm89n12'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","not-wf/P89/ibm89n12.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "not-wf").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 89


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - not-wf tests


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P01/ibm01v01.xml
%% ID: ibm-valid-P01-ibm01v01.xml
%% Type: valid
%% Sections: 2.1
'ibm-valid-P01-ibm01v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P01/ibm01v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 1


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P02/ibm02v01.xml
%% ID: ibm-valid-P02-ibm02v01.xml
%% Type: valid
%% Sections: 2.2
'ibm-valid-P02-ibm02v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P02/ibm02v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 2


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P03/ibm03v01.xml
%% ID: ibm-valid-P03-ibm03v01.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P03-ibm03v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P03/ibm03v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 3


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P09/ibm09v01.xml
%% ID: ibm-valid-P09-ibm09v01.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P09-ibm09v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P09/ibm09v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P09/ibm09v02.xml
%% ID: ibm-valid-P09-ibm09v02.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P09-ibm09v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P09/ibm09v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P09/ibm09v03.xml
%% ID: ibm-valid-P09-ibm09v03.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P09-ibm09v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P09/ibm09v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P09/ibm09v04.xml
%% ID: ibm-valid-P09-ibm09v04.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P09-ibm09v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P09/ibm09v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P09/ibm09v05.xml
%% ID: ibm-valid-P09-ibm09v05.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P09-ibm09v05'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P09/ibm09v05.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 9


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v01.xml
%% ID: ibm-valid-P10-ibm10v01.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v02.xml
%% ID: ibm-valid-P10-ibm10v02.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v03.xml
%% ID: ibm-valid-P10-ibm10v03.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v04.xml
%% ID: ibm-valid-P10-ibm10v04.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v05.xml
%% ID: ibm-valid-P10-ibm10v05.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v06.xml
%% ID: ibm-valid-P10-ibm10v06.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v07.xml
%% ID: ibm-valid-P10-ibm10v07.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P10/ibm10v08.xml
%% ID: ibm-valid-P10-ibm10v08.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P10-ibm10v08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P10/ibm10v08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 10


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P11/ibm11v01.xml
%% ID: ibm-valid-P11-ibm11v01.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P11-ibm11v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P11/ibm11v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P11/ibm11v02.xml
%% ID: ibm-valid-P11-ibm11v02.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P11-ibm11v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P11/ibm11v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P11/ibm11v03.xml
%% ID: ibm-valid-P11-ibm11v03.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P11-ibm11v03'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P11/ibm11v03.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P11/ibm11v04.xml
%% ID: ibm-valid-P11-ibm11v04.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P11-ibm11v04'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P11/ibm11v04.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 11


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P12/ibm12v01.xml
%% ID: ibm-valid-P12-ibm12v01.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P12-ibm12v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P12/ibm12v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P12/ibm12v02.xml
%% ID: ibm-valid-P12-ibm12v02.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P12-ibm12v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P12/ibm12v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P12/ibm12v03.xml
%% ID: ibm-valid-P12-ibm12v03.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P12-ibm12v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P12/ibm12v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P12/ibm12v04.xml
%% ID: ibm-valid-P12-ibm12v04.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P12-ibm12v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P12/ibm12v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 12

%%   ['ibm-valid-P12-ibm12v01','ibm-valid-P12-ibm12v02','ibm-valid-P12-ibm12v03','ibm-valid-P12-ibm12v04'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P13/ibm13v01.xml
%% ID: ibm-valid-P13-ibm13v01.xml
%% Type: valid
%% Sections: 2.3
'ibm-valid-P13-ibm13v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P13/ibm13v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 13

%%   ['ibm-valid-P13-ibm13v01'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P14/ibm14v01.xml
%% ID: ibm-valid-P14-ibm14v01.xml
%% Type: valid
%% Sections: 2.4
'ibm-valid-P14-ibm14v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P14/ibm14v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P14/ibm14v02.xml
%% ID: ibm-valid-P14-ibm14v02.xml
%% Type: valid
%% Sections: 2.4
'ibm-valid-P14-ibm14v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P14/ibm14v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P14/ibm14v03.xml
%% ID: ibm-valid-P14-ibm14v03.xml
%% Type: valid
%% Sections: 2.4
'ibm-valid-P14-ibm14v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P14/ibm14v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 14


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P15/ibm15v01.xml
%% ID: ibm-valid-P15-ibm15v01.xml
%% Type: valid
%% Sections: 2.5
'ibm-valid-P15-ibm15v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P15/ibm15v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P15/ibm15v02.xml
%% ID: ibm-valid-P15-ibm15v02.xml
%% Type: valid
%% Sections: 2.5
'ibm-valid-P15-ibm15v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P15/ibm15v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P15/ibm15v03.xml
%% ID: ibm-valid-P15-ibm15v03.xml
%% Type: valid
%% Sections: 2.5
'ibm-valid-P15-ibm15v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P15/ibm15v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P15/ibm15v04.xml
%% ID: ibm-valid-P15-ibm15v04.xml
%% Type: valid
%% Sections: 2.5
'ibm-valid-P15-ibm15v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P15/ibm15v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 15


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P16/ibm16v01.xml
%% ID: ibm-valid-P16-ibm16v01.xml
%% Type: valid
%% Sections: 2.6
'ibm-valid-P16-ibm16v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P16/ibm16v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P16/ibm16v02.xml
%% ID: ibm-valid-P16-ibm16v02.xml
%% Type: valid
%% Sections: 2.6
'ibm-valid-P16-ibm16v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P16/ibm16v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P16/ibm16v03.xml
%% ID: ibm-valid-P16-ibm16v03.xml
%% Type: valid
%% Sections: 2.6
'ibm-valid-P16-ibm16v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P16/ibm16v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 16


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P17/ibm17v01.xml
%% ID: ibm-valid-P17-ibm17v01.xml
%% Type: valid
%% Sections: 2.6
'ibm-valid-P17-ibm17v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P17/ibm17v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 17


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P18/ibm18v01.xml
%% ID: ibm-valid-P18-ibm18v01.xml
%% Type: valid
%% Sections: 2.7
'ibm-valid-P18-ibm18v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P18/ibm18v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 18


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P19/ibm19v01.xml
%% ID: ibm-valid-P19-ibm19v01.xml
%% Type: valid
%% Sections: 2.7
'ibm-valid-P19-ibm19v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P19/ibm19v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 19


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P20/ibm20v01.xml
%% ID: ibm-valid-P20-ibm20v01.xml
%% Type: valid
%% Sections: 2.7
'ibm-valid-P20-ibm20v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P20/ibm20v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P20/ibm20v02.xml
%% ID: ibm-valid-P20-ibm20v02.xml
%% Type: valid
%% Sections: 2.7
'ibm-valid-P20-ibm20v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P20/ibm20v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 20


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P21/ibm21v01.xml
%% ID: ibm-valid-P21-ibm21v01.xml
%% Type: valid
%% Sections: 2.7
'ibm-valid-P21-ibm21v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P21/ibm21v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 21


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v01.xml
%% ID: ibm-valid-P22-ibm22v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v02.xml
%% ID: ibm-valid-P22-ibm22v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v03.xml
%% ID: ibm-valid-P22-ibm22v03.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v04.xml
%% ID: ibm-valid-P22-ibm22v04.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v05.xml
%% ID: ibm-valid-P22-ibm22v05.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v06.xml
%% ID: ibm-valid-P22-ibm22v06.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P22/ibm22v07.xml
%% ID: ibm-valid-P22-ibm22v07.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P22-ibm22v07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P22/ibm22v07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 22


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P23/ibm23v01.xml
%% ID: ibm-valid-P23-ibm23v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P23-ibm23v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P23/ibm23v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P23/ibm23v02.xml
%% ID: ibm-valid-P23-ibm23v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P23-ibm23v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P23/ibm23v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P23/ibm23v03.xml
%% ID: ibm-valid-P23-ibm23v03.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P23-ibm23v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P23/ibm23v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P23/ibm23v04.xml
%% ID: ibm-valid-P23-ibm23v04.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P23-ibm23v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P23/ibm23v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P23/ibm23v05.xml
%% ID: ibm-valid-P23-ibm23v05.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P23-ibm23v05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P23/ibm23v05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P23/ibm23v06.xml
%% ID: ibm-valid-P23-ibm23v06.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P23-ibm23v06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P23/ibm23v06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 23


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P24/ibm24v01.xml
%% ID: ibm-valid-P24-ibm24v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P24-ibm24v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P24/ibm24v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P24/ibm24v02.xml
%% ID: ibm-valid-P24-ibm24v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P24-ibm24v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P24/ibm24v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 24


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P25/ibm25v01.xml
%% ID: ibm-valid-P25-ibm25v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P25-ibm25v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P25/ibm25v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P25/ibm25v02.xml
%% ID: ibm-valid-P25-ibm25v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P25-ibm25v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P25/ibm25v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P25/ibm25v03.xml
%% ID: ibm-valid-P25-ibm25v03.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P25-ibm25v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P25/ibm25v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P25/ibm25v04.xml
%% ID: ibm-valid-P25-ibm25v04.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P25-ibm25v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P25/ibm25v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 25


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P26/ibm26v01.xml
%% ID: ibm-valid-P26-ibm26v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P26-ibm26v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P26/ibm26v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 26


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P27/ibm27v01.xml
%% ID: ibm-valid-P27-ibm27v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P27-ibm27v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P27/ibm27v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P27/ibm27v02.xml
%% ID: ibm-valid-P27-ibm27v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P27-ibm27v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P27/ibm27v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P27/ibm27v03.xml
%% ID: ibm-valid-P27-ibm27v03.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P27-ibm27v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P27/ibm27v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 27


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P28/ibm28v01.xml
%% ID: ibm-valid-P28-ibm28v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P28-ibm28v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P28/ibm28v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P28/ibm28v02.xml
%% ID: ibm-valid-P28-ibm28v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P28-ibm28v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P28/ibm28v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 28


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P29/ibm29v01.xml
%% ID: ibm-valid-P29-ibm29v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P29-ibm29v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P29/ibm29v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P29/ibm29v02.xml
%% ID: ibm-valid-P29-ibm29v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P29-ibm29v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P29/ibm29v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 29


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P30/ibm30v01.xml
%% ID: ibm-valid-P30-ibm30v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P30-ibm30v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P30/ibm30v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P30/ibm30v02.xml
%% ID: ibm-valid-P30-ibm30v02.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P30-ibm30v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P30/ibm30v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 30


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P31/ibm31v01.xml
%% ID: ibm-valid-P31-ibm31v01.xml
%% Type: valid
%% Sections: 2.8
'ibm-valid-P31-ibm31v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P31/ibm31v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 31

%%   ['ibm-valid-P31-ibm31v01'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P32/ibm32v01.xml
%% ID: ibm-valid-P32-ibm32v01.xml
%% Type: valid
%% Sections: 2.9
'ibm-valid-P32-ibm32v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P32/ibm32v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P32/ibm32v02.xml
%% ID: ibm-valid-P32-ibm32v02.xml
%% Type: valid
%% Sections: 2.9
'ibm-valid-P32-ibm32v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P32/ibm32v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P32/ibm32v03.xml
%% ID: ibm-valid-P32-ibm32v03.xml
%% Type: valid
%% Sections: 2.9
'ibm-valid-P32-ibm32v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P32/ibm32v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P32/ibm32v04.xml
%% ID: ibm-valid-P32-ibm32v04.xml
%% Type: valid
%% Sections: 2.9
'ibm-valid-P32-ibm32v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P32/ibm32v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 32


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P33/ibm33v01.xml
%% ID: ibm-valid-P33-ibm33v01.xml
%% Type: valid
%% Sections: 2.12
'ibm-valid-P33-ibm33v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P33/ibm33v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 33


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P34/ibm34v01.xml
%% ID: ibm-valid-P34-ibm34v01.xml
%% Type: valid
%% Sections: 2.12
'ibm-valid-P34-ibm34v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P34/ibm34v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 34


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P35/ibm35v01.xml
%% ID: ibm-valid-P35-ibm35v01.xml
%% Type: valid
%% Sections: 2.12
'ibm-valid-P35-ibm35v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P35/ibm35v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 35


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P36/ibm36v01.xml
%% ID: ibm-valid-P36-ibm36v01.xml
%% Type: valid
%% Sections: 2.12
'ibm-valid-P36-ibm36v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P36/ibm36v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 36


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P37/ibm37v01.xml
%% ID: ibm-valid-P37-ibm37v01.xml
%% Type: valid
%% Sections: 2.12
'ibm-valid-P37-ibm37v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P37/ibm37v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 37


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P38/ibm38v01.xml
%% ID: ibm-valid-P38-ibm38v01.xml
%% Type: valid
%% Sections: 2.12
'ibm-valid-P38-ibm38v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P38/ibm38v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 38


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P39/ibm39v01.xml
%% ID: ibm-valid-P39-ibm39v01.xml
%% Type: valid
%% Sections: 3
'ibm-valid-P39-ibm39v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P39/ibm39v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 39


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P40/ibm40v01.xml
%% ID: ibm-valid-P40-ibm40v01.xml
%% Type: valid
%% Sections: 3.1
'ibm-valid-P40-ibm40v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P40/ibm40v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 40


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P41/ibm41v01.xml
%% ID: ibm-valid-P41-ibm41v01.xml
%% Type: valid
%% Sections: 3.1
'ibm-valid-P41-ibm41v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P41/ibm41v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 41


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P42/ibm42v01.xml
%% ID: ibm-valid-P42-ibm42v01.xml
%% Type: valid
%% Sections: 3.1
'ibm-valid-P42-ibm42v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P42/ibm42v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 42


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P43/ibm43v01.xml
%% ID: ibm-valid-P43-ibm43v01.xml
%% Type: valid
%% Sections: 3.1
'ibm-valid-P43-ibm43v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P43/ibm43v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 43


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P44/ibm44v01.xml
%% ID: ibm-valid-P44-ibm44v01.xml
%% Type: valid
%% Sections: 3.1
'ibm-valid-P44-ibm44v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P44/ibm44v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 44


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P45/ibm45v01.xml
%% ID: ibm-valid-P45-ibm45v01.xml
%% Type: valid
%% Sections: 3.2
'ibm-valid-P45-ibm45v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P45/ibm45v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 45


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P47/ibm47v01.xml
%% ID: ibm-valid-P47-ibm47v01.xml
%% Type: valid
%% Sections: 3.2.1
'ibm-valid-P47-ibm47v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P47/ibm47v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 47


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P49/ibm49v01.xml
%% ID: ibm-valid-P49-ibm49v01.xml
%% Type: valid
%% Sections: 3.2.1
'ibm-valid-P49-ibm49v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P49/ibm49v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 49


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P50/ibm50v01.xml
%% ID: ibm-valid-P50-ibm50v01.xml
%% Type: valid
%% Sections: 3.2.1
'ibm-valid-P50-ibm50v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P50/ibm50v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 50


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P51/ibm51v01.xml
%% ID: ibm-valid-P51-ibm51v01.xml
%% Type: valid
%% Sections: 3.2.2
'ibm-valid-P51-ibm51v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P51/ibm51v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P51/ibm51v02.xml
%% ID: ibm-valid-P51-ibm51v02.xml
%% Type: valid
%% Sections: 3.2.2
'ibm-valid-P51-ibm51v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P51/ibm51v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 51


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P52/ibm52v01.xml
%% ID: ibm-valid-P52-ibm52v01.xml
%% Type: valid
%% Sections: 3.3
'ibm-valid-P52-ibm52v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P52/ibm52v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 52


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P54/ibm54v01.xml
%% ID: ibm-valid-P54-ibm54v01.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P54-ibm54v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P54/ibm54v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P54/ibm54v02.xml
%% ID: ibm-valid-P54-ibm54v02.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P54-ibm54v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P54/ibm54v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P54/ibm54v03.xml
%% ID: ibm-valid-P54-ibm54v03.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P54-ibm54v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P54/ibm54v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 54


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P55/ibm55v01.xml
%% ID: ibm-valid-P55-ibm55v01.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P55-ibm55v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P55/ibm55v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 55


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v01.xml
%% ID: ibm-valid-P56-ibm56v01.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v02.xml
%% ID: ibm-valid-P56-ibm56v02.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v03.xml
%% ID: ibm-valid-P56-ibm56v03.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v04.xml
%% ID: ibm-valid-P56-ibm56v04.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v05.xml
%% ID: ibm-valid-P56-ibm56v05.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v06.xml
%% ID: ibm-valid-P56-ibm56v06.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v06'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v06.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v07.xml
%% ID: ibm-valid-P56-ibm56v07.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v07'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v07.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v08.xml
%% ID: ibm-valid-P56-ibm56v08.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v08'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v08.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v09.xml
%% ID: ibm-valid-P56-ibm56v09.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v09'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v09.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P56/ibm56v10.xml
%% ID: ibm-valid-P56-ibm56v10.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P56-ibm56v10'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P56/ibm56v10.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 56


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P57/ibm57v01.xml
%% ID: ibm-valid-P57-ibm57v01.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P57-ibm57v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P57/ibm57v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 57


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P58/ibm58v01.xml
%% ID: ibm-valid-P58-ibm58v01.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P58-ibm58v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P58/ibm58v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P58/ibm58v02.xml
%% ID: ibm-valid-P58-ibm58v02.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P58-ibm58v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P58/ibm58v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 58


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P59/ibm59v01.xml
%% ID: ibm-valid-P59-ibm59v01.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P59-ibm59v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P59/ibm59v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P59/ibm59v02.xml
%% ID: ibm-valid-P59-ibm59v02.xml
%% Type: valid
%% Sections: 3.3.1
'ibm-valid-P59-ibm59v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P59/ibm59v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 59


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P60/ibm60v01.xml
%% ID: ibm-valid-P60-ibm60v01.xml
%% Type: valid
%% Sections: 3.3.2
'ibm-valid-P60-ibm60v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P60/ibm60v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P60/ibm60v02.xml
%% ID: ibm-valid-P60-ibm60v02.xml
%% Type: valid
%% Sections: 3.3.2
'ibm-valid-P60-ibm60v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P60/ibm60v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P60/ibm60v03.xml
%% ID: ibm-valid-P60-ibm60v03.xml
%% Type: valid
%% Sections: 3.3.2
'ibm-valid-P60-ibm60v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P60/ibm60v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P60/ibm60v04.xml
%% ID: ibm-valid-P60-ibm60v04.xml
%% Type: valid
%% Sections: 3.3.2
'ibm-valid-P60-ibm60v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P60/ibm60v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 60


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P61/ibm61v01.xml
%% ID: ibm-valid-P61-ibm61v01.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P61-ibm61v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P61/ibm61v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P61/ibm61v02.xml
%% ID: ibm-valid-P61-ibm61v02.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P61-ibm61v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P61/ibm61v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 61

%%   ['ibm-valid-P61-ibm61v01','ibm-valid-P61-ibm61v02'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P62/ibm62v01.xml
%% ID: ibm-valid-P62-ibm62v01.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P62-ibm62v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P62/ibm62v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P62/ibm62v02.xml
%% ID: ibm-valid-P62-ibm62v02.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P62-ibm62v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P62/ibm62v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P62/ibm62v03.xml
%% ID: ibm-valid-P62-ibm62v03.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P62-ibm62v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P62/ibm62v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P62/ibm62v04.xml
%% ID: ibm-valid-P62-ibm62v04.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P62-ibm62v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P62/ibm62v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P62/ibm62v05.xml
%% ID: ibm-valid-P62-ibm62v05.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P62-ibm62v05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P62/ibm62v05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 62

%%   ['ibm-valid-P62-ibm62v01','ibm-valid-P62-ibm62v02','ibm-valid-P62-ibm62v03','ibm-valid-P62-ibm62v04','ibm-valid-P62-ibm62v05'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P63/ibm63v01.xml
%% ID: ibm-valid-P63-ibm63v01.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P63-ibm63v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P63/ibm63v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P63/ibm63v02.xml
%% ID: ibm-valid-P63-ibm63v02.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P63-ibm63v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P63/ibm63v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P63/ibm63v03.xml
%% ID: ibm-valid-P63-ibm63v03.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P63-ibm63v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P63/ibm63v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P63/ibm63v04.xml
%% ID: ibm-valid-P63-ibm63v04.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P63-ibm63v04'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P63/ibm63v04.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P63/ibm63v05.xml
%% ID: ibm-valid-P63-ibm63v05.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P63-ibm63v05'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P63/ibm63v05.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 63

%%   ['ibm-valid-P63-ibm63v01','ibm-valid-P63-ibm63v02','ibm-valid-P63-ibm63v03','ibm-valid-P63-ibm63v04','ibm-valid-P63-ibm63v05'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P64/ibm64v01.xml
%% ID: ibm-valid-P64-ibm64v01.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P64-ibm64v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P64/ibm64v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P64/ibm64v02.xml
%% ID: ibm-valid-P64-ibm64v02.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P64-ibm64v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P64/ibm64v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P64/ibm64v03.xml
%% ID: ibm-valid-P64-ibm64v03.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P64-ibm64v03'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P64/ibm64v03.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 64

%%   ['ibm-valid-P64-ibm64v01','ibm-valid-P64-ibm64v02','ibm-valid-P64-ibm64v03'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P65/ibm65v01.xml
%% ID: ibm-valid-P65-ibm65v01.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P65-ibm65v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P65/ibm65v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P65/ibm65v02.xml
%% ID: ibm-valid-P65-ibm65v02.xml
%% Type: valid
%% Sections: 3.4
'ibm-valid-P65-ibm65v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P65/ibm65v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 65

%%   ['ibm-valid-P65-ibm65v01','ibm-valid-P65-ibm65v02'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P66/ibm66v01.xml
%% ID: ibm-valid-P66-ibm66v01.xml
%% Type: valid
%% Sections: 4.1
'ibm-valid-P66-ibm66v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P66/ibm66v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 66


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P67/ibm67v01.xml
%% ID: ibm-valid-P67-ibm67v01.xml
%% Type: valid
%% Sections: 4.1
'ibm-valid-P67-ibm67v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P67/ibm67v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 67


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P68/ibm68v01.xml
%% ID: ibm-valid-P68-ibm68v01.xml
%% Type: valid
%% Sections: 4.1
'ibm-valid-P68-ibm68v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P68/ibm68v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P68/ibm68v02.xml
%% ID: ibm-valid-P68-ibm68v02.xml
%% Type: valid
%% Sections: 4.1
'ibm-valid-P68-ibm68v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P68/ibm68v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 68


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P69/ibm69v01.xml
%% ID: ibm-valid-P69-ibm69v01.xml
%% Type: valid
%% Sections: 4.1
'ibm-valid-P69-ibm69v01'(_Config) -> {skip, "NYI"}.
%%    file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
%%    Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P69/ibm69v01.xml"]),
%%    R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
%%    check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P69/ibm69v02.xml
%% ID: ibm-valid-P69-ibm69v02.xml
%% Type: valid
%% Sections: 4.1
'ibm-valid-P69-ibm69v02'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P69/ibm69v02.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 69


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P70/ibm70v01.xml
%% ID: ibm-valid-P70-ibm70v01.xml
%% Type: valid
%% Sections: 4.2
'ibm-valid-P70-ibm70v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P70/ibm70v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 70


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P78/ibm78v01.xml
%% ID: ibm-valid-P78-ibm78v01.xml
%% Type: valid
%% Sections: 4.3.2
'ibm-valid-P78-ibm78v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P78/ibm78v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 78

%%   ['ibm-valid-P78-ibm78v01'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P79/ibm79v01.xml
%% ID: ibm-valid-P79-ibm79v01.xml
%% Type: valid
%% Sections: 4.3.2
'ibm-valid-P79-ibm79v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P79/ibm79v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 79


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P82/ibm82v01.xml
%% ID: ibm-valid-P82-ibm82v01.xml
%% Type: valid
%% Sections: 4.7
'ibm-valid-P82-ibm82v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P82/ibm82v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 82


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P85/ibm85v01.xml
%% ID: ibm-valid-P85-ibm85v01.xml
%% Type: valid
%% Sections: B.
'ibm-valid-P85-ibm85v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P85/ibm85v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 85

%%   ['ibm-valid-P85-ibm85v01'].

%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P86/ibm86v01.xml
%% ID: ibm-valid-P86-ibm86v01.xml
%% Type: valid
%% Sections: B.
'ibm-valid-P86-ibm86v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P86/ibm86v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 86


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P87/ibm87v01.xml
%% ID: ibm-valid-P87-ibm87v01.xml
%% Type: valid
%% Sections: B.
'ibm-valid-P87-ibm87v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P87/ibm87v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 87


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P88/ibm88v01.xml
%% ID: ibm-valid-P88-ibm88v01.xml
%% Type: valid
%% Sections: B.
'ibm-valid-P88-ibm88v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P88/ibm88v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 88


%%----------------------------------------------------------------------
%% Test Case 
%% Uri: valid/P89/ibm89v01.xml
%% ID: ibm-valid-P89-ibm89v01.xml
%% Type: valid
%% Sections: B.
'ibm-valid-P89-ibm89v01'(Config) -> 
   file:set_cwd(xmerl_test_lib:get_data_dir(Config)),
   Path = filename:join([xmerl_test_lib:get_data_dir(Config),"ibm","valid/P89/ibm89v01.xml"]),
   R = xmerl_sax_parser:file(Path, [{event_fun, fun(_,_,S) -> S end}]),
   check_result(R, "valid").

%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - Production 89


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML Conformance Test Suite - valid tests


%%----------------------------------------------------------------------
%% Test Cases 
%% Profile: IBM XML 1.0 Tests


%%----------------------------------------------------------------------
%% Test Suite 
%% Profile: XML 1.0 (2nd edition) W3C Conformance Test Suite, 6 October 2000

all() -> 
    [{group, testcases1}, {group, testcases3},
     {group, testcases5}, {group, testcases6},
     {group, testcases8}].

groups() -> 
    [{testcases2, [],
      ['not-wf-sa-001', 'not-wf-sa-002', 'not-wf-sa-003',
       'not-wf-sa-004', 'not-wf-sa-005', 'not-wf-sa-006',
       'not-wf-sa-007', 'not-wf-sa-008', 'not-wf-sa-009',
       'not-wf-sa-010', 'not-wf-sa-011', 'not-wf-sa-012',
       'not-wf-sa-013', 'not-wf-sa-014', 'not-wf-sa-015',
       'not-wf-sa-016', 'not-wf-sa-017', 'not-wf-sa-018',
       'not-wf-sa-019', 'not-wf-sa-020', 'not-wf-sa-021',
       'not-wf-sa-022', 'not-wf-sa-023', 'not-wf-sa-024',
       'not-wf-sa-025', 'not-wf-sa-026', 'not-wf-sa-027',
       'not-wf-sa-028', 'not-wf-sa-029', 'not-wf-sa-030',
       'not-wf-sa-031', 'not-wf-sa-032', 'not-wf-sa-033',
       'not-wf-sa-034', 'not-wf-sa-035', 'not-wf-sa-036',
       'not-wf-sa-037', 'not-wf-sa-038', 'not-wf-sa-039',
       'not-wf-sa-040', 'not-wf-sa-041', 'not-wf-sa-042',
       'not-wf-sa-043', 'not-wf-sa-044', 'not-wf-sa-045',
       'not-wf-sa-046', 'not-wf-sa-047', 'not-wf-sa-048',
       'not-wf-sa-049', 'not-wf-sa-050', 'not-wf-sa-051',
       'not-wf-sa-052', 'not-wf-sa-053', 'not-wf-sa-054',
       'not-wf-sa-055', 'not-wf-sa-056', 'not-wf-sa-057',
       'not-wf-sa-058', 'not-wf-sa-059', 'not-wf-sa-060',
       'not-wf-sa-061', 'not-wf-sa-062', 'not-wf-sa-063',
       'not-wf-sa-064', 'not-wf-sa-065', 'not-wf-sa-066',
       'not-wf-sa-067', 'not-wf-sa-068', 'not-wf-sa-069',
       'not-wf-sa-070', 'not-wf-sa-071', 'not-wf-sa-072',
       'not-wf-sa-073', 'not-wf-sa-074', 'not-wf-sa-075',
       'not-wf-sa-076', 'not-wf-sa-077', 'not-wf-sa-078',
       'not-wf-sa-079', 'not-wf-sa-080', 'not-wf-sa-081',
       'not-wf-sa-082', 'not-wf-sa-083', 'not-wf-sa-084',
       'not-wf-sa-085', 'not-wf-sa-086', 'not-wf-sa-087',
       'not-wf-sa-088', 'not-wf-sa-089', 'not-wf-sa-090',
       'not-wf-sa-091', 'not-wf-sa-092', 'not-wf-sa-093',
       'not-wf-sa-094', 'not-wf-sa-095', 'not-wf-sa-096',
       'not-wf-sa-097', 'not-wf-sa-098', 'not-wf-sa-099',
       'not-wf-sa-100', 'not-wf-sa-101', 'not-wf-sa-102',
       'not-wf-sa-103', 'not-wf-sa-104', 'not-wf-sa-105',
       'not-wf-sa-106', 'not-wf-sa-107', 'not-wf-sa-108',
       'not-wf-sa-109', 'not-wf-sa-110', 'not-wf-sa-111',
       'not-wf-sa-112', 'not-wf-sa-113', 'not-wf-sa-114',
       'not-wf-sa-115', 'not-wf-sa-116', 'not-wf-sa-117',
       'not-wf-sa-118', 'not-wf-sa-119', 'not-wf-sa-120',
       'not-wf-sa-121', 'not-wf-sa-122', 'not-wf-sa-123',
       'not-wf-sa-124', 'not-wf-sa-125', 'not-wf-sa-126',
       'not-wf-sa-127', 'not-wf-sa-128', 'not-wf-sa-129',
       'not-wf-sa-130', 'not-wf-sa-131', 'not-wf-sa-132',
       'not-wf-sa-133', 'not-wf-sa-134', 'not-wf-sa-135',
       'not-wf-sa-136', 'not-wf-sa-137', 'not-wf-sa-138',
       'not-wf-sa-139', 'not-wf-sa-140', 'not-wf-sa-141',
       'not-wf-sa-142', 'not-wf-sa-143', 'not-wf-sa-144',
       'not-wf-sa-145', 'not-wf-sa-146', %'not-wf-sa-147', LATH: Check this later
       'not-wf-sa-148', 'not-wf-sa-149', 'not-wf-sa-150',
       'not-wf-sa-151', 'not-wf-sa-152', 'not-wf-sa-153',
       'not-wf-sa-154', 'not-wf-sa-155', 'not-wf-sa-156',
       'not-wf-sa-157', 'not-wf-sa-158', 'not-wf-sa-159',
       'not-wf-sa-160', 'not-wf-sa-161', 'not-wf-sa-162',
       'not-wf-sa-163', 'not-wf-sa-164', 'not-wf-sa-165',
       'not-wf-sa-166', 'not-wf-sa-167', 'not-wf-sa-168',
       'not-wf-sa-169', 'not-wf-sa-170', 'not-wf-sa-171',
       'not-wf-sa-172', 'not-wf-sa-173', 'not-wf-sa-174',
       'not-wf-sa-175', 'not-wf-sa-176', 'not-wf-sa-177',
       'not-wf-sa-178', 'not-wf-sa-179', 'not-wf-sa-180',
       'not-wf-sa-181', 'not-wf-sa-182', 'not-wf-sa-183',
       'not-wf-sa-184', 'not-wf-sa-185', 'not-wf-sa-186',
       'not-wf-not-sa-001', 'not-wf-not-sa-002',
       'not-wf-not-sa-003', 'not-wf-not-sa-004',
       'not-wf-not-sa-005', 'not-wf-not-sa-006',
       'not-wf-not-sa-007', 'not-wf-not-sa-008',
       'not-wf-not-sa-009', 'not-wf-ext-sa-001',
       'not-wf-ext-sa-002', 'not-wf-ext-sa-003',
       'invalid--002', 'invalid--005', 'invalid--006',
       'invalid-not-sa-022', 'valid-sa-001', 'valid-sa-002',
       'valid-sa-003', 'valid-sa-004', 'valid-sa-005',
       'valid-sa-006', 'valid-sa-007', 'valid-sa-008',
       'valid-sa-009', 'valid-sa-010', 'valid-sa-011',
       'valid-sa-012', 'valid-sa-013', 'valid-sa-014',
       'valid-sa-015', 'valid-sa-016', 'valid-sa-017',
       'valid-sa-018', 'valid-sa-019', 'valid-sa-020',
       'valid-sa-021', 'valid-sa-022', 'valid-sa-023',
       'valid-sa-024', 'valid-sa-025', 'valid-sa-026',
       'valid-sa-027', 'valid-sa-028', 'valid-sa-029',
       'valid-sa-030', 'valid-sa-031', 'valid-sa-032',
       'valid-sa-033', 'valid-sa-034', 'valid-sa-035',
       'valid-sa-036', 'valid-sa-037', 'valid-sa-038',
       'valid-sa-039', 'valid-sa-040', 'valid-sa-041',
       'valid-sa-042', 'valid-sa-043', 'valid-sa-044',
       'valid-sa-045', 'valid-sa-046', 'valid-sa-047',
       'valid-sa-048', 'valid-sa-049', 'valid-sa-050',
       'valid-sa-051', 'valid-sa-052', 'valid-sa-053',
       'valid-sa-054', 'valid-sa-055', 'valid-sa-056',
       'valid-sa-057', 'valid-sa-058', 'valid-sa-059',
       'valid-sa-060', 'valid-sa-061', 'valid-sa-062',
       'valid-sa-063', 'valid-sa-064', 'valid-sa-065',
       'valid-sa-066', 'valid-sa-067', 'valid-sa-068',
       'valid-sa-069', 'valid-sa-070', 'valid-sa-071',
       'valid-sa-072', 'valid-sa-073', 'valid-sa-074',
       'valid-sa-075', 'valid-sa-076', 'valid-sa-077',
       'valid-sa-078', 'valid-sa-079', 'valid-sa-080',
       'valid-sa-081', 'valid-sa-082', 'valid-sa-083',
       'valid-sa-084', 'valid-sa-085', 'valid-sa-086',
       'valid-sa-087', 'valid-sa-088', 'valid-sa-089',
       'valid-sa-090', 'valid-sa-091', 'valid-sa-092',
       'valid-sa-093', 'valid-sa-094', 'valid-sa-095',
       'valid-sa-096', 'valid-sa-097', 'valid-sa-098',
       'valid-sa-099', 'valid-sa-100', 'valid-sa-101',
       'valid-sa-102', 'valid-sa-103', 'valid-sa-104',
       'valid-sa-105', 'valid-sa-106', 'valid-sa-107',
       'valid-sa-108', 'valid-sa-109', 'valid-sa-110',
       'valid-sa-111', 'valid-sa-112', 'valid-sa-113',
       'valid-sa-114', 'valid-sa-115', 'valid-sa-116',
       'valid-sa-117', 'valid-sa-118', 'valid-sa-119',
       'valid-not-sa-001', 'valid-not-sa-002',
       'valid-not-sa-003', 'valid-not-sa-004',
       'valid-not-sa-005', 'valid-not-sa-006',
       'valid-not-sa-007', 'valid-not-sa-008',
       'valid-not-sa-009', 'valid-not-sa-010',
       'valid-not-sa-011', 'valid-not-sa-012',
       'valid-not-sa-013', 'valid-not-sa-014',
       'valid-not-sa-015', 'valid-not-sa-016',
       'valid-not-sa-017', 'valid-not-sa-018',
       'valid-not-sa-019', 'valid-not-sa-020',
       'valid-not-sa-021', 'valid-not-sa-023',
       'valid-not-sa-024', 'valid-not-sa-025',
       'valid-not-sa-026', 'valid-not-sa-027',
       'valid-not-sa-028', 'valid-not-sa-029',
       'valid-not-sa-030', 'valid-not-sa-031',
       'valid-ext-sa-001', 'valid-ext-sa-002',
       'valid-ext-sa-003', 'valid-ext-sa-004',
       'valid-ext-sa-005', 'valid-ext-sa-006',
       'valid-ext-sa-007', 'valid-ext-sa-008',
       'valid-ext-sa-009', 'valid-ext-sa-011',
       'valid-ext-sa-012', 'valid-ext-sa-013',
       'valid-ext-sa-014']},
     {testcases1, [], [{group, testcases2}]},
     {testcases4, [],
      ['pr-xml-euc-jp', 'pr-xml-iso-2022-jp', 'pr-xml-little',
       'pr-xml-shift_jis', 'pr-xml-utf-16', 'pr-xml-utf-8',
       'weekly-euc-jp', 'weekly-iso-2022-jp', 'weekly-little',
       'weekly-shift_jis', 'weekly-utf-16', 'weekly-utf-8']},
     {testcases3, [], [{group, testcases4}]},
     {testcases5, [],
      [pe01, dtd00, dtd01, element, ext01, ext02, 'not-sa01',
       'not-sa02', 'not-sa03', 'not-sa04', notation01,
       optional, required00, sa01, sa02, sa03, sa04, sa05,
       'v-sgml01', 'v-lang01', 'v-lang02', 'v-lang03',
       'v-lang04', 'v-lang05', 'v-lang06', 'v-pe00', 'v-pe03',
       'v-pe02', 'inv-dtd01', 'inv-dtd02', 'inv-dtd03', el01,
       el02, el03, el04, el05, el06, id01, id02, id03, id04,
       id05, id06, id07, id08, id09, 'inv-not-sa01',
       'inv-not-sa02', 'inv-not-sa04', 'inv-not-sa05',
       'inv-not-sa06', 'inv-not-sa07', 'inv-not-sa08',
       'inv-not-sa09', 'inv-not-sa10', 'inv-not-sa11',
       'inv-not-sa12', 'inv-not-sa13', 'inv-not-sa14',
       optional01, optional02, optional03, optional04,
       optional05, optional06, optional07, optional08,
       optional09, optional10, optional11, optional12,
       optional13, optional14, optional20, optional21,
       optional22, optional23, optional24, optional25,
       'inv-required00', 'inv-required01', 'inv-required02',
       root, attr01, attr02, attr03, attr04, attr05, attr06,
       attr07, attr08, attr09, attr10, attr11, attr12, attr13,
       attr14, attr15, attr16, utf16b, utf16l, empty,
       'not-wf-sa03', attlist01, attlist02, attlist03,
       attlist04, attlist05, attlist06, attlist07, attlist08,
       attlist09, attlist10, attlist11, cond01, cond02,
       content01, content02, content03, decl01, 'nwf-dtd00',
       'nwf-dtd01', dtd02, dtd03, dtd04, dtd05, dtd07,
       element00, element01, element02, element03, element04,
       encoding01, encoding02, encoding03, encoding04,
       encoding05, encoding06, encoding07, pi, pubid01,
       pubid02, pubid03, pubid04, pubid05, sgml01, sgml02,
       sgml03, sgml04, sgml05, sgml06, sgml07, sgml08, sgml09,
       sgml10, sgml11, sgml12, sgml13, uri01]},
     {testcases7, [],
      ['o-p01pass2', 'o-p06pass1', 'o-p07pass1', 'o-p08pass1',
       'o-p09pass1', 'o-p12pass1', 'o-p22pass4', 'o-p22pass5',
       'o-p22pass6', 'o-p28pass1', 'o-p28pass3', 'o-p28pass4',
       'o-p28pass5', 'o-p29pass1', 'o-p30pass1', 'o-p30pass2',
       'o-p31pass1', 'o-p31pass2', 'o-p43pass1', 'o-p45pass1',
       'o-p46pass1', 'o-p47pass1', 'o-p48pass1', 'o-p49pass1',
       'o-p50pass1', 'o-p51pass1', 'o-p52pass1', 'o-p53pass1',
       'o-p54pass1', 'o-p55pass1', 'o-p56pass1', 'o-p57pass1',
       'o-p58pass1', 'o-p59pass1', 'o-p60pass1', 'o-p61pass1',
       'o-p62pass1', 'o-p63pass1', 'o-p64pass1', 'o-p68pass1',
       'o-p69pass1', 'o-p70pass1', 'o-p71pass1', 'o-p72pass1',
       'o-p73pass1', 'o-p76pass1', 'o-p01pass1', 'o-p01pass3',
       'o-p03pass1', 'o-p04pass1', 'o-p05pass1', 'o-p06fail1',
       'o-p08fail1', 'o-p08fail2', 'o-p10pass1', 'o-p14pass1',
       'o-p15pass1', 'o-p16pass1', 'o-p16pass2', 'o-p16pass3',
       'o-p18pass1', 'o-p22pass1', 'o-p22pass2', 'o-p22pass3',
       'o-p23pass1', 'o-p23pass2', 'o-p23pass3', 'o-p23pass4',
       'o-p24pass1', 'o-p24pass2', 'o-p24pass3', 'o-p24pass4',
       'o-p25pass1', 'o-p25pass2', 'o-p26pass1', 'o-p27pass1',
       'o-p27pass2', 'o-p27pass3', 'o-p27pass4', 'o-p32pass1',
       'o-p32pass2', 'o-p39pass1', 'o-p39pass2', 'o-p40pass1',
       'o-p40pass2', 'o-p40pass3', 'o-p40pass4', 'o-p41pass1',
       'o-p41pass2', 'o-p42pass1', 'o-p42pass2', 'o-p44pass1',
       'o-p44pass2', 'o-p44pass3', 'o-p44pass4', 'o-p44pass5',
       'o-p66pass1', 'o-p74pass1', 'o-p75pass1', 'o-e2',
       'o-p01fail1', 'o-p01fail2', 'o-p01fail3', 'o-p01fail4',
       'o-p02fail1', 'o-p02fail10', 'o-p02fail11',
       'o-p02fail12', 'o-p02fail13', 'o-p02fail14',
       'o-p02fail15', 'o-p02fail16', 'o-p02fail17',
       'o-p02fail18', 'o-p02fail19', 'o-p02fail2',
       'o-p02fail20', 'o-p02fail21', 'o-p02fail22',
       'o-p02fail23', 'o-p02fail24', 'o-p02fail25',
       'o-p02fail26', 'o-p02fail27', 'o-p02fail28',
       'o-p02fail29', 'o-p02fail3', 'o-p02fail30',
       'o-p02fail31', 'o-p02fail4', 'o-p02fail5', 'o-p02fail6',
       'o-p02fail7', 'o-p02fail8', 'o-p02fail9', 'o-p03fail1',
       'o-p03fail10', 'o-p03fail11', 'o-p03fail12',
       'o-p03fail13', 'o-p03fail14', 'o-p03fail15',
       'o-p03fail16', 'o-p03fail17', 'o-p03fail18',
       'o-p03fail19', 'o-p03fail2', 'o-p03fail20',
       'o-p03fail21', 'o-p03fail22', 'o-p03fail23',
       'o-p03fail24', 'o-p03fail25', 'o-p03fail26',
       'o-p03fail27', 'o-p03fail28', 'o-p03fail29',
       'o-p03fail3', 'o-p03fail4', 'o-p03fail5', 'o-p03fail7',
       'o-p03fail8', 'o-p03fail9', 'o-p04fail1', 'o-p04fail2',
       'o-p04fail3', 'o-p05fail1', 'o-p05fail2', 'o-p05fail3',
       'o-p05fail4', 'o-p05fail5', 'o-p09fail1', 'o-p09fail2',
       'o-p09fail3', 'o-p09fail4', 'o-p09fail5', 'o-p10fail1',
       'o-p10fail2', 'o-p10fail3', 'o-p11fail1', 'o-p11fail2',
       'o-p12fail1', 'o-p12fail2', 'o-p12fail3', 'o-p12fail4',
       'o-p12fail5', 'o-p12fail6', 'o-p12fail7', 'o-p14fail1',
       'o-p14fail2', 'o-p14fail3', 'o-p15fail1', 'o-p15fail2',
       'o-p15fail3', 'o-p16fail1', 'o-p16fail2', 'o-p16fail3',
       'o-p18fail1', 'o-p18fail2', 'o-p18fail3', 'o-p22fail1',
       'o-p22fail2', 'o-p23fail1', 'o-p23fail2', 'o-p23fail3',
       'o-p23fail4', 'o-p23fail5', 'o-p24fail1', 'o-p24fail2',
       'o-p25fail1', 'o-p26fail1', 'o-p26fail2', 'o-p27fail1',
       'o-p28fail1', 'o-p29fail1', 'o-p30fail1', 'o-p31fail1',
       'o-p32fail1', 'o-p32fail2', 'o-p32fail3', 'o-p32fail4',
       'o-p32fail5', 'o-p39fail1', 'o-p39fail2', 'o-p39fail3',
       'o-p39fail4', 'o-p39fail5', 'o-p40fail1', 'o-p40fail2',
       'o-p40fail3', 'o-p40fail4', 'o-p41fail1', 'o-p41fail2',
       'o-p41fail3', 'o-p42fail1', 'o-p42fail2', 'o-p42fail3',
       'o-p43fail1', 'o-p43fail2', 'o-p43fail3', 'o-p44fail1',
       'o-p44fail2', 'o-p44fail3', 'o-p44fail4', 'o-p44fail5',
       'o-p45fail1', 'o-p45fail2', 'o-p45fail3', 'o-p45fail4',
       'o-p46fail1', 'o-p46fail2', 'o-p46fail3', 'o-p46fail4',
       'o-p46fail5', 'o-p46fail6', 'o-p47fail1', 'o-p47fail2',
       'o-p47fail3', 'o-p47fail4', 'o-p48fail1', 'o-p48fail2',
       'o-p49fail1', 'o-p50fail1', 'o-p51fail1', 'o-p51fail2',
       'o-p51fail3', 'o-p51fail4', 'o-p51fail5', 'o-p51fail6',
       'o-p51fail7', 'o-p52fail1', 'o-p52fail2', 'o-p53fail1',
       'o-p53fail2', 'o-p53fail3', 'o-p53fail4', 'o-p53fail5',
       'o-p54fail1', 'o-p55fail1', 'o-p56fail1', 'o-p56fail2',
       'o-p56fail3', 'o-p56fail4', 'o-p56fail5', 'o-p57fail1',
       'o-p58fail1', 'o-p58fail2', 'o-p58fail3', 'o-p58fail4',
       'o-p58fail5', 'o-p58fail6', 'o-p58fail7', 'o-p58fail8',
       'o-p59fail1', 'o-p59fail2', 'o-p59fail3', 'o-p60fail1',
       'o-p60fail2', 'o-p60fail3', 'o-p60fail4', 'o-p60fail5',
       'o-p61fail1', 'o-p62fail1', 'o-p62fail2', 'o-p63fail1',
       'o-p63fail2', 'o-p64fail1', 'o-p64fail2', 'o-p66fail1',
       'o-p66fail2', 'o-p66fail3', 'o-p66fail4', 'o-p66fail5',
       'o-p66fail6', 'o-p68fail1', 'o-p68fail2', 'o-p68fail3',
       'o-p69fail1', 'o-p69fail2', 'o-p69fail3', 'o-p70fail1',
       'o-p71fail1', 'o-p71fail2', 'o-p71fail3', 'o-p71fail4',
       'o-p72fail1', 'o-p72fail2', 'o-p72fail3', 'o-p72fail4',
       'o-p73fail1', 'o-p73fail2', 'o-p73fail3', 'o-p73fail4',
       'o-p73fail5', 'o-p74fail1', 'o-p74fail2', 'o-p74fail3',
       'o-p75fail1', 'o-p75fail2', 'o-p75fail3', 'o-p75fail4',
       'o-p75fail5', 'o-p75fail6', 'o-p76fail1', 'o-p76fail2',
       'o-p76fail3', 'o-p76fail4', 'o-p11pass1']},
     {testcases6, [], [{group, testcases7}]},
     {testcases10, [], ['ibm-invalid-P28-ibm28i01']},
     {testcases11, [],
      ['ibm-invalid-P32-ibm32i01', 'ibm-invalid-P32-ibm32i03',
       'ibm-invalid-P32-ibm32i04']},
     {testcases12, [],
      ['ibm-invalid-P39-ibm39i01', 'ibm-invalid-P39-ibm39i02',
       'ibm-invalid-P39-ibm39i03',
       'ibm-invalid-P39-ibm39i04']},
     {testcases13, [],
      ['ibm-invalid-P41-ibm41i01',
       'ibm-invalid-P41-ibm41i02']},
     {testcases14, [], ['ibm-invalid-P45-ibm45i01']},
     {testcases15, [], ['ibm-invalid-P49-ibm49i01']},
     {testcases16, [], ['ibm-invalid-P50-ibm50i01']},
     {testcases17, [],
      ['ibm-invalid-P51-ibm51i01',
       'ibm-invalid-P51-ibm51i03']},
     {testcases18, [],
      ['ibm-invalid-P56-ibm56i01', 'ibm-invalid-P56-ibm56i02',
       'ibm-invalid-P56-ibm56i03', 'ibm-invalid-P56-ibm56i05',
       'ibm-invalid-P56-ibm56i06', 'ibm-invalid-P56-ibm56i07',
       'ibm-invalid-P56-ibm56i08', 'ibm-invalid-P56-ibm56i09',
       'ibm-invalid-P56-ibm56i10', 'ibm-invalid-P56-ibm56i11',
       'ibm-invalid-P56-ibm56i12', 'ibm-invalid-P56-ibm56i13',
       'ibm-invalid-P56-ibm56i14', 'ibm-invalid-P56-ibm56i15',
       'ibm-invalid-P56-ibm56i16', 'ibm-invalid-P56-ibm56i17',
       'ibm-invalid-P56-ibm56i18']},
     {testcases19, [],
      ['ibm-invalid-P58-ibm58i01',
       'ibm-invalid-P58-ibm58i02']},
     {testcases20, [], ['ibm-invalid-P59-ibm59i01']},
     {testcases21, [],
      ['ibm-invalid-P60-ibm60i01', 'ibm-invalid-P60-ibm60i02',
       'ibm-invalid-P60-ibm60i03',
       'ibm-invalid-P60-ibm60i04']},
     {testcases22, [],
      ['ibm-invalid-P68-ibm68i01', 'ibm-invalid-P68-ibm68i02',
       'ibm-invalid-P68-ibm68i03',
       'ibm-invalid-P68-ibm68i04']},
     {testcases23, [],
      ['ibm-invalid-P69-ibm69i01', 'ibm-invalid-P69-ibm69i02',
       'ibm-invalid-P69-ibm69i03',
       'ibm-invalid-P69-ibm69i04']},
     {testcases24, [], ['ibm-invalid-P76-ibm76i01']},
     {testcases9, [],
      [{group, testcases10}, {group, testcases11},
       {group, testcases12}, {group, testcases13},
       {group, testcases14}, {group, testcases15},
       {group, testcases16}, {group, testcases17},
       {group, testcases18}, {group, testcases19},
       {group, testcases20}, {group, testcases21},
       {group, testcases22}, {group, testcases23},
       {group, testcases24}]},
     {testcases26, [],
      ['ibm-not-wf-P01-ibm01n01', 'ibm-not-wf-P01-ibm01n02',
       'ibm-not-wf-P01-ibm01n03']},
     {testcases27, [],
      ['ibm-not-wf-P02-ibm02n01', 'ibm-not-wf-P02-ibm02n02',
       'ibm-not-wf-P02-ibm02n03', 'ibm-not-wf-P02-ibm02n04',
       'ibm-not-wf-P02-ibm02n05', 'ibm-not-wf-P02-ibm02n06',
       'ibm-not-wf-P02-ibm02n07', 'ibm-not-wf-P02-ibm02n08',
       'ibm-not-wf-P02-ibm02n09', 'ibm-not-wf-P02-ibm02n10',
       'ibm-not-wf-P02-ibm02n11', 'ibm-not-wf-P02-ibm02n12',
       'ibm-not-wf-P02-ibm02n13', 'ibm-not-wf-P02-ibm02n14',
       'ibm-not-wf-P02-ibm02n15', 'ibm-not-wf-P02-ibm02n16',
       'ibm-not-wf-P02-ibm02n17', 'ibm-not-wf-P02-ibm02n18',
       'ibm-not-wf-P02-ibm02n19', 'ibm-not-wf-P02-ibm02n20',
       'ibm-not-wf-P02-ibm02n21', 'ibm-not-wf-P02-ibm02n22',
       'ibm-not-wf-P02-ibm02n23', 'ibm-not-wf-P02-ibm02n24',
       'ibm-not-wf-P02-ibm02n25', 'ibm-not-wf-P02-ibm02n26',
       'ibm-not-wf-P02-ibm02n27', 'ibm-not-wf-P02-ibm02n28',
       'ibm-not-wf-P02-ibm02n29', 'ibm-not-wf-P02-ibm02n30',
       'ibm-not-wf-P02-ibm02n31', 'ibm-not-wf-P02-ibm02n32',
       'ibm-not-wf-P02-ibm02n33']},
     {testcases28, [], ['ibm-not-wf-P03-ibm03n01']},
     {testcases29, [],
      ['ibm-not-wf-P04-ibm04n01', 'ibm-not-wf-P04-ibm04n02',
       'ibm-not-wf-P04-ibm04n03', 'ibm-not-wf-P04-ibm04n04',
       'ibm-not-wf-P04-ibm04n05', 'ibm-not-wf-P04-ibm04n06',
       'ibm-not-wf-P04-ibm04n07', 'ibm-not-wf-P04-ibm04n08',
       'ibm-not-wf-P04-ibm04n09', 'ibm-not-wf-P04-ibm04n10',
       'ibm-not-wf-P04-ibm04n11', 'ibm-not-wf-P04-ibm04n12',
       'ibm-not-wf-P04-ibm04n13', 'ibm-not-wf-P04-ibm04n14',
       'ibm-not-wf-P04-ibm04n15', 'ibm-not-wf-P04-ibm04n16',
       'ibm-not-wf-P04-ibm04n17', 'ibm-not-wf-P04-ibm04n18']},
     {testcases30, [],
      ['ibm-not-wf-P05-ibm05n01', 'ibm-not-wf-P05-ibm05n02',
       'ibm-not-wf-P05-ibm05n03']},
     {testcases31, [],
      ['ibm-not-wf-P09-ibm09n01', 'ibm-not-wf-P09-ibm09n02',
       'ibm-not-wf-P09-ibm09n03', 'ibm-not-wf-P09-ibm09n04']},
     {testcases32, [],
      ['ibm-not-wf-P10-ibm10n01', 'ibm-not-wf-P10-ibm10n02',
       'ibm-not-wf-P10-ibm10n03', 'ibm-not-wf-P10-ibm10n04',
       'ibm-not-wf-P10-ibm10n05', 'ibm-not-wf-P10-ibm10n06',
       'ibm-not-wf-P10-ibm10n07', 'ibm-not-wf-P10-ibm10n08']},
     {testcases33, [],
      ['ibm-not-wf-P11-ibm11n01', 'ibm-not-wf-P11-ibm11n02',
       'ibm-not-wf-P11-ibm11n03', 'ibm-not-wf-P11-ibm11n04']},
     {testcases34, [],
      ['ibm-not-wf-P12-ibm12n01', 'ibm-not-wf-P12-ibm12n02',
       'ibm-not-wf-P12-ibm12n03']},
     {testcases35, [],
      ['ibm-not-wf-P13-ibm13n01', 'ibm-not-wf-P13-ibm13n02',
       'ibm-not-wf-P13-ibm13n03']},
     {testcases36, [],
      ['ibm-not-wf-P14-ibm14n01', 'ibm-not-wf-P14-ibm14n02',
       'ibm-not-wf-P14-ibm14n03']},
     {testcases37, [],
      ['ibm-not-wf-P15-ibm15n01', 'ibm-not-wf-P15-ibm15n02',
       'ibm-not-wf-P15-ibm15n03', 'ibm-not-wf-P15-ibm15n04']},
     {testcases38, [],
      ['ibm-not-wf-P16-ibm16n01', 'ibm-not-wf-P16-ibm16n02',
       'ibm-not-wf-P16-ibm16n03', 'ibm-not-wf-P16-ibm16n04']},
     {testcases39, [],
      ['ibm-not-wf-P17-ibm17n01', 'ibm-not-wf-P17-ibm17n02',
       'ibm-not-wf-P17-ibm17n03', 'ibm-not-wf-P17-ibm17n04']},
     {testcases40, [],
      ['ibm-not-wf-P18-ibm18n01', 'ibm-not-wf-P18-ibm18n02']},
     {testcases41, [],
      ['ibm-not-wf-P19-ibm19n01', 'ibm-not-wf-P19-ibm19n02',
       'ibm-not-wf-P19-ibm19n03']},
     {testcases42, [], ['ibm-not-wf-P20-ibm20n01']},
     {testcases43, [],
      ['ibm-not-wf-P21-ibm21n01', 'ibm-not-wf-P21-ibm21n02',
       'ibm-not-wf-P21-ibm21n03']},
     {testcases44, [],
      ['ibm-not-wf-P22-ibm22n01', 'ibm-not-wf-P22-ibm22n02',
       'ibm-not-wf-P22-ibm22n03']},
     {testcases45, [],
      ['ibm-not-wf-P23-ibm23n01', 'ibm-not-wf-P23-ibm23n02',
       'ibm-not-wf-P23-ibm23n03', 'ibm-not-wf-P23-ibm23n04',
       'ibm-not-wf-P23-ibm23n05', 'ibm-not-wf-P23-ibm23n06']},
     {testcases46, [],
      ['ibm-not-wf-P24-ibm24n01', 'ibm-not-wf-P24-ibm24n02',
       'ibm-not-wf-P24-ibm24n03', 'ibm-not-wf-P24-ibm24n04',
       'ibm-not-wf-P24-ibm24n05', 'ibm-not-wf-P24-ibm24n06',
       'ibm-not-wf-P24-ibm24n07', 'ibm-not-wf-P24-ibm24n08',
       'ibm-not-wf-P24-ibm24n09']},
     {testcases47, [],
      ['ibm-not-wf-P25-ibm25n01', 'ibm-not-wf-P25-ibm25n02']},
     {testcases48, [], ['ibm-not-wf-P26-ibm26n01']},
     {testcases49, [], ['ibm-not-wf-P27-ibm27n01']},
     {testcases50, [],
      ['ibm-not-wf-P28-ibm28n01', 'ibm-not-wf-P28-ibm28n02',
       'ibm-not-wf-P28-ibm28n03', 'ibm-not-wf-P28-ibm28n04',
       'ibm-not-wf-P28-ibm28n05', 'ibm-not-wf-P28-ibm28n06',
       'ibm-not-wf-P28-ibm28n07', 'ibm-not-wf-P28-ibm28n08']},
     {testcases51, [], ['ibm-not-wf-p28a-ibm28an01']},
     {testcases52, [],
      ['ibm-not-wf-P29-ibm29n01', 'ibm-not-wf-P29-ibm29n02',
       'ibm-not-wf-P29-ibm29n03', 'ibm-not-wf-P29-ibm29n04',
       'ibm-not-wf-P29-ibm29n05', 'ibm-not-wf-P29-ibm29n06',
       'ibm-not-wf-P29-ibm29n07']},
     {testcases53, [], ['ibm-not-wf-P30-ibm30n01']},
     {testcases54, [], ['ibm-not-wf-P31-ibm31n01']},
     {testcases55, [],
      ['ibm-not-wf-P32-ibm32n01', 'ibm-not-wf-P32-ibm32n02',
       'ibm-not-wf-P32-ibm32n03', 'ibm-not-wf-P32-ibm32n04',
       'ibm-not-wf-P32-ibm32n05', 'ibm-not-wf-P32-ibm32n06',
       'ibm-not-wf-P32-ibm32n07', 'ibm-not-wf-P32-ibm32n08',
       'ibm-not-wf-P32-ibm32n09']},
     {testcases56, [],
      ['ibm-not-wf-P39-ibm39n01', 'ibm-not-wf-P39-ibm39n02',
       'ibm-not-wf-P39-ibm39n03', 'ibm-not-wf-P39-ibm39n04',
       'ibm-not-wf-P39-ibm39n05', 'ibm-not-wf-P39-ibm39n06']},
     {testcases57, [],
      ['ibm-not-wf-P40-ibm40n01', 'ibm-not-wf-P40-ibm40n02',
       'ibm-not-wf-P40-ibm40n03', 'ibm-not-wf-P40-ibm40n04',
       'ibm-not-wf-P40-ibm40n05']},
     {testcases58, [],
      ['ibm-not-wf-P41-ibm41n01', 'ibm-not-wf-P41-ibm41n02',
       'ibm-not-wf-P41-ibm41n03', 'ibm-not-wf-P41-ibm41n04',
       'ibm-not-wf-P41-ibm41n05', 'ibm-not-wf-P41-ibm41n06',
       'ibm-not-wf-P41-ibm41n07', 'ibm-not-wf-P41-ibm41n08',
       'ibm-not-wf-P41-ibm41n09', 'ibm-not-wf-P41-ibm41n10',
       'ibm-not-wf-P41-ibm41n11', 'ibm-not-wf-P41-ibm41n12',
       'ibm-not-wf-P41-ibm41n13', 'ibm-not-wf-P41-ibm41n14']},
     {testcases59, [],
      ['ibm-not-wf-P42-ibm42n01', 'ibm-not-wf-P42-ibm42n02',
       'ibm-not-wf-P42-ibm42n03', 'ibm-not-wf-P42-ibm42n04',
       'ibm-not-wf-P42-ibm42n05']},
     {testcases60, [],
      ['ibm-not-wf-P43-ibm43n01', 'ibm-not-wf-P43-ibm43n02',
       'ibm-not-wf-P43-ibm43n04', 'ibm-not-wf-P43-ibm43n05']},
     {testcases61, [],
      ['ibm-not-wf-P44-ibm44n01', 'ibm-not-wf-P44-ibm44n02',
       'ibm-not-wf-P44-ibm44n03', 'ibm-not-wf-P44-ibm44n04']},
     {testcases62, [],
      ['ibm-not-wf-P45-ibm45n01', 'ibm-not-wf-P45-ibm45n02',
       'ibm-not-wf-P45-ibm45n03', 'ibm-not-wf-P45-ibm45n04',
       'ibm-not-wf-P45-ibm45n05', 'ibm-not-wf-P45-ibm45n06',
       'ibm-not-wf-P45-ibm45n07', 'ibm-not-wf-P45-ibm45n08',
       'ibm-not-wf-P45-ibm45n09']},
     {testcases63, [],
      ['ibm-not-wf-P46-ibm46n01', 'ibm-not-wf-P46-ibm46n02',
       'ibm-not-wf-P46-ibm46n03', 'ibm-not-wf-P46-ibm46n04',
       'ibm-not-wf-P46-ibm46n05']},
     {testcases64, [],
      ['ibm-not-wf-P47-ibm47n01', 'ibm-not-wf-P47-ibm47n02',
       'ibm-not-wf-P47-ibm47n03', 'ibm-not-wf-P47-ibm47n04',
       'ibm-not-wf-P47-ibm47n05', 'ibm-not-wf-P47-ibm47n06']},
     {testcases65, [],
      ['ibm-not-wf-P48-ibm48n01', 'ibm-not-wf-P48-ibm48n02',
       'ibm-not-wf-P48-ibm48n03', 'ibm-not-wf-P48-ibm48n04',
       'ibm-not-wf-P48-ibm48n05', 'ibm-not-wf-P48-ibm48n06',
       'ibm-not-wf-P48-ibm48n07']},
     {testcases66, [],
      ['ibm-not-wf-P49-ibm49n01', 'ibm-not-wf-P49-ibm49n02',
       'ibm-not-wf-P49-ibm49n03', 'ibm-not-wf-P49-ibm49n04',
       'ibm-not-wf-P49-ibm49n05', 'ibm-not-wf-P49-ibm49n06']},
     {testcases67, [], 
      ['ibm-not-wf-P50-ibm50n01','ibm-not-wf-P50-ibm50n02',
       'ibm-not-wf-P50-ibm50n03','ibm-not-wf-P50-ibm50n04',
       'ibm-not-wf-P50-ibm50n05','ibm-not-wf-P50-ibm50n06',
       'ibm-not-wf-P50-ibm50n07']},
     {testcases68, [], 
      ['ibm-not-wf-P51-ibm51n01','ibm-not-wf-P51-ibm51n02',
       'ibm-not-wf-P51-ibm51n03','ibm-not-wf-P51-ibm51n04',
       'ibm-not-wf-P51-ibm51n05','ibm-not-wf-P51-ibm51n06',
       'ibm-not-wf-P51-ibm51n07']},
     {testcases69, [],
      ['ibm-not-wf-P52-ibm52n01', 'ibm-not-wf-P52-ibm52n02',
       'ibm-not-wf-P52-ibm52n03', 'ibm-not-wf-P52-ibm52n04',
       'ibm-not-wf-P52-ibm52n05', 'ibm-not-wf-P52-ibm52n06']},
     {testcases70, [],
      ['ibm-not-wf-P53-ibm53n01', 'ibm-not-wf-P53-ibm53n02',
       'ibm-not-wf-P53-ibm53n03', 'ibm-not-wf-P53-ibm53n04',
       'ibm-not-wf-P53-ibm53n05', 'ibm-not-wf-P53-ibm53n06',
       'ibm-not-wf-P53-ibm53n07', 'ibm-not-wf-P53-ibm53n08']},
     {testcases71, [],
      ['ibm-not-wf-P54-ibm54n01', 'ibm-not-wf-P54-ibm54n02']},
     {testcases72, [],
      ['ibm-not-wf-P55-ibm55n01', 'ibm-not-wf-P55-ibm55n02',
       'ibm-not-wf-P55-ibm55n03']},
     {testcases73, [],
      ['ibm-not-wf-P56-ibm56n01', 'ibm-not-wf-P56-ibm56n02',
       'ibm-not-wf-P56-ibm56n03', 'ibm-not-wf-P56-ibm56n04',
       'ibm-not-wf-P56-ibm56n05', 'ibm-not-wf-P56-ibm56n06',
       'ibm-not-wf-P56-ibm56n07']},
     {testcases74, [], ['ibm-not-wf-P57-ibm57n01']},
     {testcases75, [],
      ['ibm-not-wf-P58-ibm58n01', 'ibm-not-wf-P58-ibm58n02',
       'ibm-not-wf-P58-ibm58n03', 'ibm-not-wf-P58-ibm58n04',
       'ibm-not-wf-P58-ibm58n05', 'ibm-not-wf-P58-ibm58n06',
       'ibm-not-wf-P58-ibm58n07', 'ibm-not-wf-P58-ibm58n08']},
     {testcases76, [],
      ['ibm-not-wf-P59-ibm59n01', 'ibm-not-wf-P59-ibm59n02',
       'ibm-not-wf-P59-ibm59n03', 'ibm-not-wf-P59-ibm59n04',
       'ibm-not-wf-P59-ibm59n05', 'ibm-not-wf-P59-ibm59n06']},
     {testcases77, [],
      ['ibm-not-wf-P60-ibm60n01', 'ibm-not-wf-P60-ibm60n02',
       'ibm-not-wf-P60-ibm60n03', 'ibm-not-wf-P60-ibm60n04',
       'ibm-not-wf-P60-ibm60n05', 'ibm-not-wf-P60-ibm60n06',
       'ibm-not-wf-P60-ibm60n07', 'ibm-not-wf-P60-ibm60n08']},
     {testcases78, [], ['ibm-not-wf-P61-ibm61n01']},
     {testcases79, [],
      ['ibm-not-wf-P62-ibm62n01', 'ibm-not-wf-P62-ibm62n02',
       'ibm-not-wf-P62-ibm62n03', 'ibm-not-wf-P62-ibm62n04',
       'ibm-not-wf-P62-ibm62n05', 'ibm-not-wf-P62-ibm62n06',
       'ibm-not-wf-P62-ibm62n07', 'ibm-not-wf-P62-ibm62n08']},
     {testcases80, [],
      ['ibm-not-wf-P63-ibm63n01', 'ibm-not-wf-P63-ibm63n02',
       'ibm-not-wf-P63-ibm63n03', 'ibm-not-wf-P63-ibm63n04',
       'ibm-not-wf-P63-ibm63n05', 'ibm-not-wf-P63-ibm63n06',
       'ibm-not-wf-P63-ibm63n07']},
     {testcases81, [],
      ['ibm-not-wf-P64-ibm64n01', 'ibm-not-wf-P64-ibm64n02',
       'ibm-not-wf-P64-ibm64n03']},
     {testcases82, [],
      ['ibm-not-wf-P65-ibm65n01', 'ibm-not-wf-P65-ibm65n02']},
     {testcases83, [],
      ['ibm-not-wf-P66-ibm66n01', 'ibm-not-wf-P66-ibm66n02',
       'ibm-not-wf-P66-ibm66n03', 'ibm-not-wf-P66-ibm66n04',
       'ibm-not-wf-P66-ibm66n05', 'ibm-not-wf-P66-ibm66n06',
       'ibm-not-wf-P66-ibm66n07', 'ibm-not-wf-P66-ibm66n08',
       'ibm-not-wf-P66-ibm66n09', 'ibm-not-wf-P66-ibm66n10',
       'ibm-not-wf-P66-ibm66n11', 'ibm-not-wf-P66-ibm66n12',
       'ibm-not-wf-P66-ibm66n13', 'ibm-not-wf-P66-ibm66n14',
       'ibm-not-wf-P66-ibm66n15']},
     {testcases84, [],
      ['ibm-not-wf-P68-ibm68n01', 'ibm-not-wf-P68-ibm68n02',
       'ibm-not-wf-P68-ibm68n03', 'ibm-not-wf-P68-ibm68n04',
       'ibm-not-wf-P68-ibm68n05', 'ibm-not-wf-P68-ibm68n06',
       'ibm-not-wf-P68-ibm68n07', 'ibm-not-wf-P68-ibm68n08',
       'ibm-not-wf-P68-ibm68n09', 'ibm-not-wf-P68-ibm68n10']},
     {testcases85, [],
      ['ibm-not-wf-P69-ibm69n01', 'ibm-not-wf-P69-ibm69n02',
       'ibm-not-wf-P69-ibm69n03', 'ibm-not-wf-P69-ibm69n04',
       'ibm-not-wf-P69-ibm69n05', 'ibm-not-wf-P69-ibm69n06',
       'ibm-not-wf-P69-ibm69n07']},
     {testcases86, [],
      ['ibm-not-wf-P71-ibm70n01', 'ibm-not-wf-P71-ibm71n01',
       'ibm-not-wf-P71-ibm71n02', 'ibm-not-wf-P71-ibm71n03',
       'ibm-not-wf-P71-ibm71n04', 'ibm-not-wf-P71-ibm71n05',
       'ibm-not-wf-P71-ibm71n06', 'ibm-not-wf-P71-ibm71n07',
       'ibm-not-wf-P71-ibm71n08']},
     {testcases87, [],
      ['ibm-not-wf-P72-ibm72n01', 'ibm-not-wf-P72-ibm72n02',
       'ibm-not-wf-P72-ibm72n03', 'ibm-not-wf-P72-ibm72n04',
       'ibm-not-wf-P72-ibm72n05', 'ibm-not-wf-P72-ibm72n06',
       'ibm-not-wf-P72-ibm72n07', 'ibm-not-wf-P72-ibm72n08',
       'ibm-not-wf-P72-ibm72n09']},
     {testcases88, [],
      ['ibm-not-wf-P73-ibm73n01', 'ibm-not-wf-P73-ibm73n03']},
     {testcases89, [], ['ibm-not-wf-P74-ibm74n01']},
     {testcases90, [],
      ['ibm-not-wf-P75-ibm75n01', 'ibm-not-wf-P75-ibm75n02',
       'ibm-not-wf-P75-ibm75n03', 'ibm-not-wf-P75-ibm75n04',
       'ibm-not-wf-P75-ibm75n05', 'ibm-not-wf-P75-ibm75n06',
       'ibm-not-wf-P75-ibm75n07', 'ibm-not-wf-P75-ibm75n08',
       'ibm-not-wf-P75-ibm75n09', 'ibm-not-wf-P75-ibm75n10',
       'ibm-not-wf-P75-ibm75n11', 'ibm-not-wf-P75-ibm75n12',
       'ibm-not-wf-P75-ibm75n13']},
     {testcases91, [],
      ['ibm-not-wf-P76-ibm76n01', 'ibm-not-wf-P76-ibm76n02',
       'ibm-not-wf-P76-ibm76n03', 'ibm-not-wf-P76-ibm76n04',
       'ibm-not-wf-P76-ibm76n05', 'ibm-not-wf-P76-ibm76n06',
       'ibm-not-wf-P76-ibm76n07']},
     {testcases92, [],
      ['ibm-not-wf-P77-ibm77n01', 'ibm-not-wf-P77-ibm77n02',
       'ibm-not-wf-P77-ibm77n03', 'ibm-not-wf-P77-ibm77n04']},
     {testcases93, [],
      ['ibm-not-wf-P78-ibm78n01', 'ibm-not-wf-P78-ibm78n02']},
     {testcases94, [],
      ['ibm-not-wf-P79-ibm79n01', 'ibm-not-wf-P79-ibm79n02']},
     {testcases95, [],
      ['ibm-not-wf-P80-ibm80n01', 'ibm-not-wf-P80-ibm80n02',
       'ibm-not-wf-P80-ibm80n03', 'ibm-not-wf-P80-ibm80n04',
       'ibm-not-wf-P80-ibm80n05', 'ibm-not-wf-P80-ibm80n06']},
     {testcases96, [],
      ['ibm-not-wf-P81-ibm81n01', 'ibm-not-wf-P81-ibm81n02',
       'ibm-not-wf-P81-ibm81n03', 'ibm-not-wf-P81-ibm81n04',
       'ibm-not-wf-P81-ibm81n05', 'ibm-not-wf-P81-ibm81n06',
       'ibm-not-wf-P81-ibm81n07', 'ibm-not-wf-P81-ibm81n08',
       'ibm-not-wf-P81-ibm81n09']},
     {testcases97, [],
      ['ibm-not-wf-P82-ibm82n01', 'ibm-not-wf-P82-ibm82n02',
       'ibm-not-wf-P82-ibm82n03', 'ibm-not-wf-P82-ibm82n04',
       'ibm-not-wf-P82-ibm82n05', 'ibm-not-wf-P82-ibm82n06',
       'ibm-not-wf-P82-ibm82n07', 'ibm-not-wf-P82-ibm82n08']},
     {testcases98, [],
      ['ibm-not-wf-P83-ibm83n01', 'ibm-not-wf-P83-ibm83n02',
       'ibm-not-wf-P83-ibm83n03', 'ibm-not-wf-P83-ibm83n04',
       'ibm-not-wf-P83-ibm83n05', 'ibm-not-wf-P83-ibm83n06']},
     {testcases99, [],
      ['ibm-not-wf-P85-ibm85n01', 'ibm-not-wf-P85-ibm85n02',
       'ibm-not-wf-P85-ibm85n03', 'ibm-not-wf-P85-ibm85n04',
       'ibm-not-wf-P85-ibm85n05', 'ibm-not-wf-P85-ibm85n06',
       'ibm-not-wf-P85-ibm85n07', 'ibm-not-wf-P85-ibm85n08',
       'ibm-not-wf-P85-ibm85n09', 'ibm-not-wf-P85-ibm85n10',
       'ibm-not-wf-P85-ibm85n100', 'ibm-not-wf-P85-ibm85n101',
       'ibm-not-wf-P85-ibm85n102', 'ibm-not-wf-P85-ibm85n103',
       'ibm-not-wf-P85-ibm85n104', 'ibm-not-wf-P85-ibm85n105',
       'ibm-not-wf-P85-ibm85n106', 'ibm-not-wf-P85-ibm85n107',
       'ibm-not-wf-P85-ibm85n108', 'ibm-not-wf-P85-ibm85n109',
       'ibm-not-wf-P85-ibm85n11', 'ibm-not-wf-P85-ibm85n110',
       'ibm-not-wf-P85-ibm85n111', 'ibm-not-wf-P85-ibm85n112',
       'ibm-not-wf-P85-ibm85n113', 'ibm-not-wf-P85-ibm85n114',
       'ibm-not-wf-P85-ibm85n115', 'ibm-not-wf-P85-ibm85n116',
       'ibm-not-wf-P85-ibm85n117', 'ibm-not-wf-P85-ibm85n118',
       'ibm-not-wf-P85-ibm85n119', 'ibm-not-wf-P85-ibm85n12',
       'ibm-not-wf-P85-ibm85n120', 'ibm-not-wf-P85-ibm85n121',
       'ibm-not-wf-P85-ibm85n122', 'ibm-not-wf-P85-ibm85n123',
       'ibm-not-wf-P85-ibm85n124', 'ibm-not-wf-P85-ibm85n125',
       'ibm-not-wf-P85-ibm85n126', 'ibm-not-wf-P85-ibm85n127',
       'ibm-not-wf-P85-ibm85n128', 'ibm-not-wf-P85-ibm85n129',
       'ibm-not-wf-P85-ibm85n13', 'ibm-not-wf-P85-ibm85n130',
       'ibm-not-wf-P85-ibm85n131', 'ibm-not-wf-P85-ibm85n132',
       'ibm-not-wf-P85-ibm85n133', 'ibm-not-wf-P85-ibm85n134',
       'ibm-not-wf-P85-ibm85n135', 'ibm-not-wf-P85-ibm85n136',
       'ibm-not-wf-P85-ibm85n137', 'ibm-not-wf-P85-ibm85n138',
       'ibm-not-wf-P85-ibm85n139', 'ibm-not-wf-P85-ibm85n14',
       'ibm-not-wf-P85-ibm85n140', 'ibm-not-wf-P85-ibm85n141',
       'ibm-not-wf-P85-ibm85n142', 'ibm-not-wf-P85-ibm85n143',
       'ibm-not-wf-P85-ibm85n144', 'ibm-not-wf-P85-ibm85n145',
       'ibm-not-wf-P85-ibm85n146', 'ibm-not-wf-P85-ibm85n147',
       'ibm-not-wf-P85-ibm85n148', 'ibm-not-wf-P85-ibm85n149',
       'ibm-not-wf-P85-ibm85n15', 'ibm-not-wf-P85-ibm85n150',
       'ibm-not-wf-P85-ibm85n151', 'ibm-not-wf-P85-ibm85n152',
       'ibm-not-wf-P85-ibm85n153', 'ibm-not-wf-P85-ibm85n154',
       'ibm-not-wf-P85-ibm85n155', 'ibm-not-wf-P85-ibm85n156',
       'ibm-not-wf-P85-ibm85n157', 'ibm-not-wf-P85-ibm85n158',
       'ibm-not-wf-P85-ibm85n159', 'ibm-not-wf-P85-ibm85n16',
       'ibm-not-wf-P85-ibm85n160', 'ibm-not-wf-P85-ibm85n161',
       'ibm-not-wf-P85-ibm85n162', 'ibm-not-wf-P85-ibm85n163',
       'ibm-not-wf-P85-ibm85n164', 'ibm-not-wf-P85-ibm85n165',
       'ibm-not-wf-P85-ibm85n166', 'ibm-not-wf-P85-ibm85n167',
       'ibm-not-wf-P85-ibm85n168', 'ibm-not-wf-P85-ibm85n169',
       'ibm-not-wf-P85-ibm85n17', 'ibm-not-wf-P85-ibm85n170',
       'ibm-not-wf-P85-ibm85n171', 'ibm-not-wf-P85-ibm85n172',
       'ibm-not-wf-P85-ibm85n173', 'ibm-not-wf-P85-ibm85n174',
       'ibm-not-wf-P85-ibm85n175', 'ibm-not-wf-P85-ibm85n176',
       'ibm-not-wf-P85-ibm85n177', 'ibm-not-wf-P85-ibm85n178',
       'ibm-not-wf-P85-ibm85n179', 'ibm-not-wf-P85-ibm85n18',
       'ibm-not-wf-P85-ibm85n180', 'ibm-not-wf-P85-ibm85n181',
       'ibm-not-wf-P85-ibm85n182', 'ibm-not-wf-P85-ibm85n183',
       'ibm-not-wf-P85-ibm85n184', 'ibm-not-wf-P85-ibm85n185',
       'ibm-not-wf-P85-ibm85n186', 'ibm-not-wf-P85-ibm85n187',
       'ibm-not-wf-P85-ibm85n188', 'ibm-not-wf-P85-ibm85n189',
       'ibm-not-wf-P85-ibm85n19', 'ibm-not-wf-P85-ibm85n190',
       'ibm-not-wf-P85-ibm85n191', 'ibm-not-wf-P85-ibm85n192',
       'ibm-not-wf-P85-ibm85n193', 'ibm-not-wf-P85-ibm85n194',
       'ibm-not-wf-P85-ibm85n195', 'ibm-not-wf-P85-ibm85n196',
       'ibm-not-wf-P85-ibm85n197', 'ibm-not-wf-P85-ibm85n198',
       'ibm-not-wf-P85-ibm85n20', 'ibm-not-wf-P85-ibm85n21',
       'ibm-not-wf-P85-ibm85n22', 'ibm-not-wf-P85-ibm85n23',
       'ibm-not-wf-P85-ibm85n24', 'ibm-not-wf-P85-ibm85n25',
       'ibm-not-wf-P85-ibm85n26', 'ibm-not-wf-P85-ibm85n27',
       'ibm-not-wf-P85-ibm85n28', 'ibm-not-wf-P85-ibm85n29',
       'ibm-not-wf-P85-ibm85n30', 'ibm-not-wf-P85-ibm85n31',
       'ibm-not-wf-P85-ibm85n32', 'ibm-not-wf-P85-ibm85n33',
       'ibm-not-wf-P85-ibm85n34', 'ibm-not-wf-P85-ibm85n35',
       'ibm-not-wf-P85-ibm85n36', 'ibm-not-wf-P85-ibm85n37',
       'ibm-not-wf-P85-ibm85n38', 'ibm-not-wf-P85-ibm85n39',
       'ibm-not-wf-P85-ibm85n40', 'ibm-not-wf-P85-ibm85n41',
       'ibm-not-wf-P85-ibm85n42', 'ibm-not-wf-P85-ibm85n43',
       'ibm-not-wf-P85-ibm85n44', 'ibm-not-wf-P85-ibm85n45',
       'ibm-not-wf-P85-ibm85n46', 'ibm-not-wf-P85-ibm85n47',
       'ibm-not-wf-P85-ibm85n48', 'ibm-not-wf-P85-ibm85n49',
       'ibm-not-wf-P85-ibm85n50', 'ibm-not-wf-P85-ibm85n51',
       'ibm-not-wf-P85-ibm85n52', 'ibm-not-wf-P85-ibm85n53',
       'ibm-not-wf-P85-ibm85n54', 'ibm-not-wf-P85-ibm85n55',
       'ibm-not-wf-P85-ibm85n56', 'ibm-not-wf-P85-ibm85n57',
       'ibm-not-wf-P85-ibm85n58', 'ibm-not-wf-P85-ibm85n59',
       'ibm-not-wf-P85-ibm85n60', 'ibm-not-wf-P85-ibm85n61',
       'ibm-not-wf-P85-ibm85n62', 'ibm-not-wf-P85-ibm85n63',
       'ibm-not-wf-P85-ibm85n64', 'ibm-not-wf-P85-ibm85n65',
       'ibm-not-wf-P85-ibm85n66', 'ibm-not-wf-P85-ibm85n67',
       'ibm-not-wf-P85-ibm85n68', 'ibm-not-wf-P85-ibm85n69',
       'ibm-not-wf-P85-ibm85n70', 'ibm-not-wf-P85-ibm85n71',
       'ibm-not-wf-P85-ibm85n72', 'ibm-not-wf-P85-ibm85n73',
       'ibm-not-wf-P85-ibm85n74', 'ibm-not-wf-P85-ibm85n75',
       'ibm-not-wf-P85-ibm85n76', 'ibm-not-wf-P85-ibm85n77',
       'ibm-not-wf-P85-ibm85n78', 'ibm-not-wf-P85-ibm85n79',
       'ibm-not-wf-P85-ibm85n80', 'ibm-not-wf-P85-ibm85n81',
       'ibm-not-wf-P85-ibm85n82', 'ibm-not-wf-P85-ibm85n83',
       'ibm-not-wf-P85-ibm85n84', 'ibm-not-wf-P85-ibm85n85',
       'ibm-not-wf-P85-ibm85n86', 'ibm-not-wf-P85-ibm85n87',
       'ibm-not-wf-P85-ibm85n88', 'ibm-not-wf-P85-ibm85n89',
       'ibm-not-wf-P85-ibm85n90', 'ibm-not-wf-P85-ibm85n91',
       'ibm-not-wf-P85-ibm85n92', 'ibm-not-wf-P85-ibm85n93',
       'ibm-not-wf-P85-ibm85n94', 'ibm-not-wf-P85-ibm85n95',
       'ibm-not-wf-P85-ibm85n96', 'ibm-not-wf-P85-ibm85n97',
       'ibm-not-wf-P85-ibm85n98', 'ibm-not-wf-P85-ibm85n99']},
     {testcases100, [],
      ['ibm-not-wf-P86-ibm86n01', 'ibm-not-wf-P86-ibm86n02',
       'ibm-not-wf-P86-ibm86n03', 'ibm-not-wf-P86-ibm86n04']},
     {testcases101, [],
      ['ibm-not-wf-P87-ibm87n01', 'ibm-not-wf-P87-ibm87n02',
       'ibm-not-wf-P87-ibm87n03', 'ibm-not-wf-P87-ibm87n04',
       'ibm-not-wf-P87-ibm87n05', 'ibm-not-wf-P87-ibm87n06',
       'ibm-not-wf-P87-ibm87n07', 'ibm-not-wf-P87-ibm87n08',
       'ibm-not-wf-P87-ibm87n09', 'ibm-not-wf-P87-ibm87n10',
       'ibm-not-wf-P87-ibm87n11', 'ibm-not-wf-P87-ibm87n12',
       'ibm-not-wf-P87-ibm87n13', 'ibm-not-wf-P87-ibm87n14',
       'ibm-not-wf-P87-ibm87n15', 'ibm-not-wf-P87-ibm87n16',
       'ibm-not-wf-P87-ibm87n17', 'ibm-not-wf-P87-ibm87n18',
       'ibm-not-wf-P87-ibm87n19', 'ibm-not-wf-P87-ibm87n20',
       'ibm-not-wf-P87-ibm87n21', 'ibm-not-wf-P87-ibm87n22',
       'ibm-not-wf-P87-ibm87n23', 'ibm-not-wf-P87-ibm87n24',
       'ibm-not-wf-P87-ibm87n25', 'ibm-not-wf-P87-ibm87n26',
       'ibm-not-wf-P87-ibm87n27', 'ibm-not-wf-P87-ibm87n28',
       'ibm-not-wf-P87-ibm87n29', 'ibm-not-wf-P87-ibm87n30',
       'ibm-not-wf-P87-ibm87n31', 'ibm-not-wf-P87-ibm87n32',
       'ibm-not-wf-P87-ibm87n33', 'ibm-not-wf-P87-ibm87n34',
       'ibm-not-wf-P87-ibm87n35', 'ibm-not-wf-P87-ibm87n36',
       'ibm-not-wf-P87-ibm87n37', 'ibm-not-wf-P87-ibm87n38',
       'ibm-not-wf-P87-ibm87n39', 'ibm-not-wf-P87-ibm87n40',
       'ibm-not-wf-P87-ibm87n41', 'ibm-not-wf-P87-ibm87n42',
       'ibm-not-wf-P87-ibm87n43', 'ibm-not-wf-P87-ibm87n44',
       'ibm-not-wf-P87-ibm87n45', 'ibm-not-wf-P87-ibm87n46',
       'ibm-not-wf-P87-ibm87n47', 'ibm-not-wf-P87-ibm87n48',
       'ibm-not-wf-P87-ibm87n49', 'ibm-not-wf-P87-ibm87n50',
       'ibm-not-wf-P87-ibm87n51', 'ibm-not-wf-P87-ibm87n52',
       'ibm-not-wf-P87-ibm87n53', 'ibm-not-wf-P87-ibm87n54',
       'ibm-not-wf-P87-ibm87n55', 'ibm-not-wf-P87-ibm87n56',
       'ibm-not-wf-P87-ibm87n57', 'ibm-not-wf-P87-ibm87n58',
       'ibm-not-wf-P87-ibm87n59', 'ibm-not-wf-P87-ibm87n60',
       'ibm-not-wf-P87-ibm87n61', 'ibm-not-wf-P87-ibm87n62',
       'ibm-not-wf-P87-ibm87n63', 'ibm-not-wf-P87-ibm87n64',
       'ibm-not-wf-P87-ibm87n66', 'ibm-not-wf-P87-ibm87n67',
       'ibm-not-wf-P87-ibm87n68', 'ibm-not-wf-P87-ibm87n69',
       'ibm-not-wf-P87-ibm87n70', 'ibm-not-wf-P87-ibm87n71',
       'ibm-not-wf-P87-ibm87n72', 'ibm-not-wf-P87-ibm87n73',
       'ibm-not-wf-P87-ibm87n74', 'ibm-not-wf-P87-ibm87n75',
       'ibm-not-wf-P87-ibm87n76', 'ibm-not-wf-P87-ibm87n77',
       'ibm-not-wf-P87-ibm87n78', 'ibm-not-wf-P87-ibm87n79',
       'ibm-not-wf-P87-ibm87n80', 'ibm-not-wf-P87-ibm87n81',
       'ibm-not-wf-P87-ibm87n82', 'ibm-not-wf-P87-ibm87n83',
       'ibm-not-wf-P87-ibm87n84', 'ibm-not-wf-P87-ibm87n85']},
     {testcases102, [],
      ['ibm-not-wf-P88-ibm88n01', 'ibm-not-wf-P88-ibm88n02',
       'ibm-not-wf-P88-ibm88n03', 'ibm-not-wf-P88-ibm88n04',
       'ibm-not-wf-P88-ibm88n05', 'ibm-not-wf-P88-ibm88n06',
       'ibm-not-wf-P88-ibm88n08', 'ibm-not-wf-P88-ibm88n09',
       'ibm-not-wf-P88-ibm88n10', 'ibm-not-wf-P88-ibm88n11',
       'ibm-not-wf-P88-ibm88n12', 'ibm-not-wf-P88-ibm88n13',
       'ibm-not-wf-P88-ibm88n14', 'ibm-not-wf-P88-ibm88n15',
       'ibm-not-wf-P88-ibm88n16']},
     {testcases103, [],
      ['ibm-not-wf-P89-ibm89n01', 'ibm-not-wf-P89-ibm89n02',
       'ibm-not-wf-P89-ibm89n03', 'ibm-not-wf-P89-ibm89n04',
       'ibm-not-wf-P89-ibm89n05', 'ibm-not-wf-P89-ibm89n06',
       'ibm-not-wf-P89-ibm89n07', 'ibm-not-wf-P89-ibm89n08',
       'ibm-not-wf-P89-ibm89n09', 'ibm-not-wf-P89-ibm89n10',
       'ibm-not-wf-P89-ibm89n11', 'ibm-not-wf-P89-ibm89n12']},
     {testcases25, [],
      [{group, testcases26}, {group, testcases27},
       {group, testcases28}, {group, testcases29},
       {group, testcases30}, {group, testcases31},
       {group, testcases32}, {group, testcases33},
       {group, testcases34}, {group, testcases35},
       {group, testcases36}, {group, testcases37},
       {group, testcases38}, {group, testcases39},
       {group, testcases40}, {group, testcases41},
       {group, testcases42}, {group, testcases43},
       {group, testcases44}, {group, testcases45},
       {group, testcases46}, {group, testcases47},
       {group, testcases48}, {group, testcases49},
       {group, testcases50}, {group, testcases51},
       {group, testcases52}, {group, testcases53},
       {group, testcases54}, {group, testcases55},
       {group, testcases56}, {group, testcases57},
       {group, testcases58}, {group, testcases59},
       {group, testcases60}, {group, testcases61},
       {group, testcases62}, {group, testcases63},
       {group, testcases64}, {group, testcases65},
       {group, testcases66}, 
%       {group, testcases67}, {group, testcases68},
       {group, testcases69}, {group, testcases70},
       {group, testcases71}, {group, testcases72},
       {group, testcases73}, {group, testcases74},
       {group, testcases75}, {group, testcases76},
       {group, testcases77}, {group, testcases78},
       {group, testcases79}, {group, testcases80},
       {group, testcases81}, {group, testcases82},
       {group, testcases83}, {group, testcases84},
       {group, testcases85}, {group, testcases86},
       {group, testcases87}, {group, testcases88},
       {group, testcases89}, {group, testcases90},
       {group, testcases91}, {group, testcases92},
       {group, testcases93}, {group, testcases94},
       {group, testcases95}, {group, testcases96},
       {group, testcases97}, {group, testcases98},
       {group, testcases99}, {group, testcases100},
       {group, testcases101}, {group, testcases102},
       {group, testcases103}]},
     {testcases105, [], ['ibm-valid-P01-ibm01v01']},
     {testcases106, [], ['ibm-valid-P02-ibm02v01']},
     {testcases107, [], ['ibm-valid-P03-ibm03v01']},
     {testcases108, [],
      ['ibm-valid-P09-ibm09v01', 'ibm-valid-P09-ibm09v02',
       'ibm-valid-P09-ibm09v03', 'ibm-valid-P09-ibm09v04',
       'ibm-valid-P09-ibm09v05']},
     {testcases109, [],
      ['ibm-valid-P10-ibm10v01', 'ibm-valid-P10-ibm10v02',
       'ibm-valid-P10-ibm10v03', 'ibm-valid-P10-ibm10v04',
       'ibm-valid-P10-ibm10v05', 'ibm-valid-P10-ibm10v06',
       'ibm-valid-P10-ibm10v07', 'ibm-valid-P10-ibm10v08']},
     {testcases110, [],
      ['ibm-valid-P11-ibm11v01', 'ibm-valid-P11-ibm11v02',
       'ibm-valid-P11-ibm11v03', 'ibm-valid-P11-ibm11v04']},
     {testcases111, [],
      ['ibm-valid-P12-ibm12v01','ibm-valid-P12-ibm12v02',
       'ibm-valid-P12-ibm12v03','ibm-valid-P12-ibm12v04']},
     {testcases112, [], ['ibm-valid-P13-ibm13v01']},
     {testcases113, [],
      ['ibm-valid-P14-ibm14v01', 'ibm-valid-P14-ibm14v02',
       'ibm-valid-P14-ibm14v03']},
     {testcases114, [],
      ['ibm-valid-P15-ibm15v01', 'ibm-valid-P15-ibm15v02',
       'ibm-valid-P15-ibm15v03', 'ibm-valid-P15-ibm15v04']},
     {testcases115, [],
      ['ibm-valid-P16-ibm16v01', 'ibm-valid-P16-ibm16v02',
       'ibm-valid-P16-ibm16v03']},
     {testcases116, [], ['ibm-valid-P17-ibm17v01']},
     {testcases117, [], ['ibm-valid-P18-ibm18v01']},
     {testcases118, [], ['ibm-valid-P19-ibm19v01']},
     {testcases119, [],
      ['ibm-valid-P20-ibm20v01', 'ibm-valid-P20-ibm20v02']},
     {testcases120, [], ['ibm-valid-P21-ibm21v01']},
     {testcases121, [],
      ['ibm-valid-P22-ibm22v01', 'ibm-valid-P22-ibm22v02',
       'ibm-valid-P22-ibm22v03', 'ibm-valid-P22-ibm22v04',
       'ibm-valid-P22-ibm22v05', 'ibm-valid-P22-ibm22v06',
       'ibm-valid-P22-ibm22v07']},
     {testcases122, [],
      ['ibm-valid-P23-ibm23v01', 'ibm-valid-P23-ibm23v02',
       'ibm-valid-P23-ibm23v03', 'ibm-valid-P23-ibm23v04',
       'ibm-valid-P23-ibm23v05', 'ibm-valid-P23-ibm23v06']},
     {testcases123, [],
      ['ibm-valid-P24-ibm24v01', 'ibm-valid-P24-ibm24v02']},
     {testcases124, [],
      ['ibm-valid-P25-ibm25v01', 'ibm-valid-P25-ibm25v02',
       'ibm-valid-P25-ibm25v03', 'ibm-valid-P25-ibm25v04']},
     {testcases125, [], ['ibm-valid-P26-ibm26v01']},
     {testcases126, [],
      ['ibm-valid-P27-ibm27v01', 'ibm-valid-P27-ibm27v02',
       'ibm-valid-P27-ibm27v03']},
     {testcases127, [],
      ['ibm-valid-P28-ibm28v01', 'ibm-valid-P28-ibm28v02']},
     {testcases128, [],
      ['ibm-valid-P29-ibm29v01', 'ibm-valid-P29-ibm29v02']},
     {testcases129, [],
      ['ibm-valid-P30-ibm30v01', 'ibm-valid-P30-ibm30v02']},
     {testcases130, [],
      ['ibm-valid-P31-ibm31v01']},
     {testcases131, [],
      ['ibm-valid-P32-ibm32v01', 'ibm-valid-P32-ibm32v02',
       'ibm-valid-P32-ibm32v03', 'ibm-valid-P32-ibm32v04']},
     {testcases132, [], ['ibm-valid-P33-ibm33v01']},
     {testcases133, [], ['ibm-valid-P34-ibm34v01']},
     {testcases134, [], ['ibm-valid-P35-ibm35v01']},
     {testcases135, [], ['ibm-valid-P36-ibm36v01']},
     {testcases136, [], ['ibm-valid-P37-ibm37v01']},
     {testcases137, [], ['ibm-valid-P38-ibm38v01']},
     {testcases138, [], ['ibm-valid-P39-ibm39v01']},
     {testcases139, [], ['ibm-valid-P40-ibm40v01']},
     {testcases140, [], ['ibm-valid-P41-ibm41v01']},
     {testcases141, [], ['ibm-valid-P42-ibm42v01']},
     {testcases142, [], ['ibm-valid-P43-ibm43v01']},
     {testcases143, [], ['ibm-valid-P44-ibm44v01']},
     {testcases144, [], ['ibm-valid-P45-ibm45v01']},
     {testcases145, [], ['ibm-valid-P47-ibm47v01']},
     {testcases146, [], ['ibm-valid-P49-ibm49v01']},
     {testcases147, [], ['ibm-valid-P50-ibm50v01']},
     {testcases148, [],
      ['ibm-valid-P51-ibm51v01', 'ibm-valid-P51-ibm51v02']},
     {testcases149, [], ['ibm-valid-P52-ibm52v01']},
     {testcases150, [],
      ['ibm-valid-P54-ibm54v01', 'ibm-valid-P54-ibm54v02',
       'ibm-valid-P54-ibm54v03']},
     {testcases151, [], ['ibm-valid-P55-ibm55v01']},
     {testcases152, [],
      ['ibm-valid-P56-ibm56v01', 'ibm-valid-P56-ibm56v02',
       'ibm-valid-P56-ibm56v03', 'ibm-valid-P56-ibm56v04',
       'ibm-valid-P56-ibm56v05', 'ibm-valid-P56-ibm56v06',
       'ibm-valid-P56-ibm56v07', 'ibm-valid-P56-ibm56v08',
       'ibm-valid-P56-ibm56v09', 'ibm-valid-P56-ibm56v10']},
     {testcases153, [], ['ibm-valid-P57-ibm57v01']},
     {testcases154, [],
      ['ibm-valid-P58-ibm58v01', 'ibm-valid-P58-ibm58v02']},
     {testcases155, [],
      ['ibm-valid-P59-ibm59v01', 'ibm-valid-P59-ibm59v02']},
     {testcases156, [],
      ['ibm-valid-P60-ibm60v01', 'ibm-valid-P60-ibm60v02',
       'ibm-valid-P60-ibm60v03', 'ibm-valid-P60-ibm60v04']},
     {testcases157, [],
      ['ibm-valid-P61-ibm61v01','ibm-valid-P61-ibm61v02']},
     {testcases158, [], 
      ['ibm-valid-P62-ibm62v01','ibm-valid-P62-ibm62v02',
       'ibm-valid-P62-ibm62v03','ibm-valid-P62-ibm62v04',
       'ibm-valid-P62-ibm62v05']},
     {testcases159, [],
      ['ibm-valid-P63-ibm63v01','ibm-valid-P63-ibm63v02',
       'ibm-valid-P63-ibm63v03','ibm-valid-P63-ibm63v04',
       'ibm-valid-P63-ibm63v05']},
     {testcases160, [], 
      ['ibm-valid-P64-ibm64v01','ibm-valid-P64-ibm64v02',
       'ibm-valid-P64-ibm64v03']},
     {testcases161, [], ['ibm-valid-P65-ibm65v01','ibm-valid-P65-ibm65v02']},
     {testcases162, [], ['ibm-valid-P66-ibm66v01']},
     {testcases163, [], ['ibm-valid-P67-ibm67v01']},
     {testcases164, [],
      ['ibm-valid-P68-ibm68v01', 'ibm-valid-P68-ibm68v02']},
     {testcases165, [],
      ['ibm-valid-P69-ibm69v01', 'ibm-valid-P69-ibm69v02']},
     {testcases166, [], ['ibm-valid-P70-ibm70v01']},
     {testcases167, [], ['ibm-valid-P78-ibm78v01']},
     {testcases168, [], ['ibm-valid-P79-ibm79v01']},
     {testcases169, [], ['ibm-valid-P82-ibm82v01']},
     {testcases170, [], ['ibm-valid-P85-ibm85v01']},
     {testcases171, [], ['ibm-valid-P86-ibm86v01']},
     {testcases172, [], ['ibm-valid-P87-ibm87v01']},
     {testcases173, [], ['ibm-valid-P88-ibm88v01']},
     {testcases174, [], ['ibm-valid-P89-ibm89v01']},
     {testcases104, [],
      [{group, testcases105}, {group, testcases106},
       {group, testcases107}, {group, testcases108},
       {group, testcases109}, {group, testcases110},
%       {group, testcases111}, {group,testcases112}, 
       {group, testcases113},
       {group, testcases114}, {group, testcases115},
       {group, testcases116}, {group, testcases117},
       {group, testcases118}, {group, testcases119},
       {group, testcases120}, {group, testcases121},
       {group, testcases122}, {group, testcases123},
       {group, testcases124}, {group, testcases125},
       {group, testcases126}, {group, testcases127},
       {group, testcases128}, {group, testcases129},
%       {group, testcases130}, 
       {group, testcases131},
       {group, testcases132}, {group, testcases133},
       {group, testcases134}, {group, testcases135},
       {group, testcases136}, {group, testcases137},
       {group, testcases138}, {group, testcases139},
       {group, testcases140}, {group, testcases141},
       {group, testcases142}, {group, testcases143},
       {group, testcases144}, {group, testcases145},
       {group, testcases146}, {group, testcases147},
       {group, testcases148}, {group, testcases149},
       {group, testcases150}, {group, testcases151},
       {group, testcases152}, {group, testcases153},
       {group, testcases154}, {group, testcases155},
       {group, testcases156}, % {group, testcases157}, 
%       {group, testcases158}, {group, testcases159}, 
%       {group, testcases160}, {group, testcases161},
       {group, testcases162}, {group, testcases163},
       {group, testcases164}, {group, testcases165},
       {group, testcases166}, %{group, testcases167},
       {group, testcases168}, {group, testcases169},
%       {group, testcases170}, 
       {group, testcases171},
       {group, testcases172}, {group, testcases173},
       {group, testcases174}]},
     {testcases8, [],
      [{group, testcases9}, {group, testcases25},
       {group, testcases104}]}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


%%======================================================================
%% Support Functions
%%======================================================================

%% Dir is a directory
rm_f_(Dir) ->
    {ok,CWD} = file:get_cwd(),
    {ok,FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD,Dir])),
    rm_files(FileList),
    file:set_cwd(CWD),
    ? line ok = file:del_dir(Dir).

rm_files([])->
    ok;
rm_files([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    rm_f_(F);
	_ ->
	    ok = file:delete(F)
    end,
    rm_files(Fs).


change_mode(Files) ->
    change_mode3(Files).
change_mode2(Dir)->
    {ok,CWD} = file:get_cwd(),
    {ok,FileList} = file:list_dir(Dir),
    file:set_cwd(filename:join([CWD,Dir])),
    change_mode3(FileList),
    file:set_cwd(CWD).
change_mode3([]) ->
    ok;
change_mode3([F|Fs]) ->
    case filelib:is_dir(F) of
	true ->
	    chmod(F),
	    change_mode2(F);
	_ ->
	    chmod(F)
    end,
    change_mode3(Fs).
    
chmod(F) ->
    case file:read_file_info(F) of
	{ok,FileInfo} ->
	    Mode= FileInfo#file_info.mode,
	    file:write_file_info(F,FileInfo#file_info{mode=8#00777 bor Mode});
	_ ->
	    ok
    end.

privdir(Config) ->
    proplists:get_value(priv_dir, Config).

%%----------------------------------------------------------------------
%% check_result 
check_result({fatal_error,_,_,_,_}, "error") -> 
   ok;
check_result({ok, _, _}, "invalid") -> 
   ok;
check_result({fatal_error,_,_,_,_}, "not-wf") -> 
   ok;
check_result({ok, _, _}, "valid") -> 
   ok.

