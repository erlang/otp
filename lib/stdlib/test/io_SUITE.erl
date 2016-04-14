%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2014. All Rights Reserved.
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
-module(io_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([error_1/1, float_g/1, otp_5403/1, otp_5813/1, otp_6230/1, 
         otp_6282/1, otp_6354/1, otp_6495/1, otp_6517/1, otp_6502/1,
         manpage/1, otp_6708/1, otp_7084/1, otp_7421/1,
	 io_lib_collect_line_3_wb/1, cr_whitespace_in_string/1,
	 io_fread_newlines/1, otp_8989/1, io_lib_fread_literal/1,
	 printable_range/1, bad_printable_range/1,
	 io_lib_print_binary_depth_one/1, otp_10302/1, otp_10755/1,
         otp_10836/1, io_lib_width_too_small/1,
         io_with_huge_message_queue/1, format_string/1,
	 maps/1, coverage/1]).

-export([pretty/2]).

%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(config(X,Y), foo).
-define(t, test_server).
-define(privdir(_), "./io_SUITE_priv").
-else.
-include_lib("test_server/include/test_server.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), ?config(priv_dir, Conf)).
-endif.


% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_Case, _Config) ->
    Dog = ?config(watchdog, _Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [error_1, float_g, otp_5403, otp_5813, otp_6230,
     otp_6282, otp_6354, otp_6495, otp_6517, otp_6502,
     manpage, otp_6708, otp_7084, otp_7421,
     io_lib_collect_line_3_wb, cr_whitespace_in_string,
     io_fread_newlines, otp_8989, io_lib_fread_literal,
     printable_range, bad_printable_range,
     io_lib_print_binary_depth_one, otp_10302, otp_10755, otp_10836,
     io_lib_width_too_small, io_with_huge_message_queue,
     format_string, maps, coverage].

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


error_1(doc) ->
    ["Error cases for output"];
error_1(suite) ->
    [];
error_1(Config) when is_list(Config) ->
    %% We don't do erroneous output on stdout - the test server
    %% seems to catch that somehow.
    ?line PrivDir = ?privdir(Config),
    ?line File = filename:join(PrivDir, "slask"),
    ?line {ok, F1} = file:open(File, [write]),
    ?line {'EXIT', _} = (catch io:format(muttru, "hej", [])),
    ?line {'EXIT', _} = (catch io:format(F1, pelle, "hej")),
    ?line {'EXIT', _} = (catch io:format(F1, 1, "hej")),
    ?line {'EXIT', _} = (catch io:format(F1, "~p~", [kaka])),
    ?line {'EXIT', _} = (catch io:format(F1, "~m~n", [kaka])),

    %% This causes the file process to die, and it is linked to us,
    %% so we can't catch the error this easily.
%    ?line {'EXIT', _} = (catch io:put_chars(F1, 666)),

    ?line file:close(F1),
    ?line {'EXIT', _} = (catch io:format(F1, "~p", ["hej"])),
    ok.

float_g(Config) when is_list(Config) ->
    ?line ["5.00000e-2",
	   "0.500000",
	   "5.00000",
	   "50.0000",
	   "500.000",
	   "5000.00",
	   "5.00000e+4",
	   "5.00000e+5"] = float_g_1("~g", 5.0, -2, 5),
    
    ?line ["-5.0000e-2",
	   "-0.50000",
	   "-5.0000",
	   "-50.000",
	   "-500.00",
	   "-5000.0",
	   "-5.0000e+4",
	   "-5.0000e+5"] = float_g_1("~.5g", -5.0, -2, 5),
    
    ?line ["5.000e-2",
	   "0.5000",
	   "5.000",
	   "50.00",
	   "500.0",
	   "5.000e+3",
	   "5.000e+4",
	   "5.000e+5"] = float_g_1("~.4g", 5.0, -2, 5),
    
    ?line ["-5.00e-2",
	   "-0.500",
	   "-5.00",
	   "-50.0",
	   "-5.00e+2",
	   "-5.00e+3",
	   "-5.00e+4",
	   "-5.00e+5"] = float_g_1("~.3g", -5.0, -2, 5),
    
    ?line ["5.0e-2",
	   "0.50",
	   "5.0",
	   "5.0e+1",
	   "5.0e+2",
	   "5.0e+3",
	   "5.0e+4",
	   "5.0e+5"] = float_g_1("~.2g", 5.0, -2, 5),
    
    ?line 
	case catch fmt("~.1g", [0.5]) of
	    "0.5" ->
		?line 
		    ["5.0e-2",
		     "0.5",
		     "5.0e+0",
		     "5.0e+1",
		     "5.0e+2",
		     "5.0e+3",
		     "5.0e+4",
		     "5.0e+5"] = float_g_1("~.1g", 5.0, -2, 5);
	    {'EXIT',_} -> ok
	end,
    
    ?line ["4.99999e-2",
	   "0.499999",
	   "4.99999",
	   "49.9999",
	   "499.999",
	   "4999.99",
	   "4.99999e+4",
	   "4.99999e+5"] = float_g_1("~g", 4.9999949999, -2, 5),
    
    ?line ["-5.00000e-2",
	   "-0.500000",
	   "-5.00000",
	   "-50.0000",
	   "-500.000",
	   "-5000.00",
	   "-5.00000e+4",
	   "-5.00000e+5"] = float_g_1("~g", -4.9999950001, -2, 5),
    ok.

float_g_1(Fmt, V, Min, Max) ->
    [fmt(Fmt, [V*math:pow(10, E)]) || E <- lists:seq(Min, Max)].

otp_5403(doc) ->
    ["OTP-5403. ~s formats I/O lists and a single binary."];
otp_5403(suite) ->
    [];
otp_5403(Config) when is_list(Config) ->
    ?line "atom" = fmt("~s", [atom]),
    ?line "binary" = fmt("~s", [<<"binary">>]),
    ?line "atail" = fmt("~s", [["a" | <<"tail">>]]),
    ?line "deepcharlist" = fmt("~s", [["deep",["char",["list"]]]]),
    ?line "somebinaries" = fmt("~s", [[<<"some">>,[<<"binaries">>]]]),
    ok.

otp_5813(doc) ->
    ["OTP-5813. read/3 is new."];
otp_5813(suite) ->
    [];
otp_5813(Config) when is_list(Config) ->
    ?line PrivDir = ?privdir(Config),
    ?line File = filename:join(PrivDir, "test"),

    ?line ok = file:write_file(File, <<"a. ">>),
    ?line {ok, Fd} = file:open(File, [read]),
    ?line {ok, a, 1} = io:read(Fd, '', 1),
    ?line {eof,1} = io:read(Fd, '', 1),
    ok = file:close(Fd),

    ?line ok = file:write_file(File, <<"[}.">>),
    ?line {ok, Fd2} = file:open(File, [read]),
    ?line {error,{1,_,_},1} = io:read(Fd2, '', 1),
    ?line ok = file:close(Fd),

    file:delete(File),
    ok.

otp_6230(doc) ->
    ["OTP-6230. ~p and ~P with (huge) binaries."];
otp_6230(suite) ->
    [];
otp_6230(Config) when is_list(Config) ->
    %% The problem is actually huge binaries, but the small tests here
    %% just run through most of the modified code.
    ?line "<<>>" = fmt("~P", [<<"">>,-1]),
    ?line "<<\"hej\">>" = fmt("~P", [<<"hej">>,-1]),
    ?line "{hej,...}" = fmt("~P", [{hej,<<"hej">>},2]),
    ?line "{hej,<<...>>}" = fmt("~P", [{hej,<<"hej">>},3]),
    ?line "{hej,<<\"hejs\"...>>}" = fmt("~P", [{hej,<<"hejsan">>},4]),
    ?line "{hej,<<\"hej\">>}" = fmt("~P", [{hej,<<"hej">>},6]),
    ?line "<<...>>" = fmt("~P", [<<"hej">>,1]),
    ?line "<<\"hejs\"...>>" = fmt("~P", [<<"hejsan">>,2]),
    ?line "<<\"hej\">>" = fmt("~P", [<<"hej">>,4]),
    ?line "{hej,<<127,...>>}" = 
        fmt("~P", [{hej,<<127:8,<<"hej">>/binary>>},4]),
    ?line "{hej,<<127,104,101,...>>}" = 
        fmt("~P", [{hej,<<127:8,<<"hej">>/binary>>},6]),

    B = list_to_binary(lists:duplicate(30000, $a)),
    ?line "<<\"aaaa"++_ = fmt("~P", [B, 20000]),
    ok.

otp_6282(doc) ->
    ["OTP-6282. ~p truncates strings (like binaries) depending on depth."];
otp_6282(suite) ->
    [];
otp_6282(Config) when is_list(Config) ->
    ?line "[]" = p("", 1, 20, 1),
    ?line "[]" = p("", 1, 20, -1),
    ?line "[...]" = p("a", 1, 20, 1),
    ?line "\"a\"" = p("a", 1, 20, 2),
    ?line "\"aa\"" = p("aa", 1, 20, 2),
    ?line "\"aaa\"" = p("aaa", 1, 20, 2),
    ?line "\"aaaa\"" = p("aaaa", 1, 20, 2),
    % ?line "\"aaaa\"..." = p("aaaaaa", 1, 20, 2),
    ?line "\"a\"" = p("a", 1, 20, -1),
    % ?line "\"aa\"..." = p([$a,$a,1000], 1, 20, 2),
    % ?line "\"aa\"..." = p([$a,$a,1000], 1, 20, 3),
    ?line "[97,97,1000]" = p([$a,$a,1000], 1, 20, 4),
    S1 = lists:duplicate(200,$a),
    ?line "[...]" = p(S1, 1, 20, 1),
    % ?line "\"aaaaaaaaaaaaaaaa\"\n \"aaaaaaaaaaaaaaaa\"\n \"aaaa\"..." = 
    % ?line "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"..." = 
    %          p(S1, 1, 20, 10),
    ?line true = "\"" ++ S1 ++ "\"" =:= p(S1, 1, 205, -1),
    ?line "[97,97,1000|...]" = p([$a,$a,1000,1000], 1, 20, 4),

    ?line "[[]]" = p([""], 1, 20, 2),
    ?line "[[]]" = p([""], 1, 20, -1),
    ?line "[[...]]" = p(["a"], 1, 20, 2),
    ?line "[\"a\"]" = p(["a"], 1, 20, 3),
    ?line "[\"aa\"]" = p(["aa"], 1, 20, 3),
    ?line "[\"aaa\"]" = p(["aaa"], 1, 20, 3),
    ?line "[\"a\"]" = p(["a"], 1, 20, -1),
    % ?line "[\"aa\"...]" = p([[$a,$a,1000]], 1, 20, 3),
    % ?line "[\"aa\"...]" = p([[$a,$a,1000]], 1, 20, 4),
    ?line "[[97,97,1000]]" = p([[$a,$a,1000]], 1, 20, 5),
    ?line "[[...]]" = p([S1], 1, 20, 2),
    % ?line "[\"aaaaaaaaaaaaaa\"\n  \"aaaaaaaaaaaaaa\"\n  \"aaaaaaaa\"...]" = 
    % ?line "[\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"...]" = 
    %            p([S1], 1, 20, 11),
    ?line true = "[\"" ++ S1 ++ "\"]" =:= p([S1], 1, 210, -1),
    ?line "[[97,97,1000|...]]" = p([[$a,$a,1000,1000]], 1, 20, 5),

    % ?line "[\"aaaa\"...]" = p(["aaaaa"], 1, 10, 3),
    ?line "[\"aaaaa\"]" = p(["aaaaa"], 1, 10, 6),

    ok.

otp_6354(doc) ->
    ["OTP-6354. io_lib_pretty rewritten."];
otp_6354(suite) ->
    [];
otp_6354(Config) when is_list(Config) ->
    %% A few tuples:
    ?line "{}" = p({}, 1, 20, -1),
    ?line "..." = p({}, 1, 20, 0),
    ?line "{}" = p({}, 1, 20, 1),
    ?line "{}" = p({}, 1, 20, 2),
    ?line "{a}" = p({a}, 1, 20, -1),
    ?line "..." = p({a}, 1, 20, 0),
    ?line "{...}" = p({a}, 1, 20, 1),
    ?line "{a}" = p({a}, 1, 20, 2),
    ?line "{a,b}" = p({a,b}, 1, 20, -1),
    ?line "..." = p({a,b}, 1, 20, 0),
    ?line "{...}" = p({a,b}, 1, 20, 1),
    ?line "{a,...}" = p({a,b}, 1, 20, 2),
    ?line "{a,b}" = p({a,b}, 1, 20, 3),
    ?line "{}" = p({}, 1, 1, -1),
    ?line "..." = p({}, 1, 1, 0),
    ?line "{}" = p({}, 1, 1, 1),
    ?line "{}" = p({}, 1, 1, 2),
    ?line "{a}" = p({a}, 1, 1, -1),
    ?line "..." = p({a}, 1, 1, 0),
    ?line "{...}" = p({a}, 1, 1, 1),
    ?line "{a}" = p({a}, 1, 1, 2),
    ?line "{a,\n b}" = p({a,b}, 1, 1, -1),
    ?line "{1,\n b}" = p({1,b}, 1, 1, -1),
    ?line "..." = p({a,b}, 1, 1, 0),
    ?line "{...}" = p({a,b}, 1, 1, 1),
    ?line "{a,...}" = p({a,b}, 1, 1, 2),
    ?line "{a,\n b}" = p({a,b}, 1, 1, 3),
    ?line "{{}}" = p({{}}, 1, 1, 2),
    ?line "{[]}" = p({[]}, 1, 1, 2),
    ?line bt(<<"{1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]}">>,
             p({1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]}, -1)),
    ?line bt(<<"{abcd,ddddd,\n      ddddd}">>,
             p({abcd,ddddd,ddddd}, 1,16, -1)),
    ?line bt(<<"{1,2,a,b,\n {sfdsf,sdfdsfs},\n [sfsdf,sdfsdf]}">>,
             p({1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]}, 1, 35, 100)),
    % With other terms than atomic ones on the same line:
%     ?line bt(<<"{1,2,a,b,{sfdsf,sdfdsfs},\n [sfsdf,sdfsdf]}">>,
%              p({1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]}, 1, 35, 100)),
    % With line breaks:
%     ?line bt(<<"{1,\n"
%                " 2,\n"
%                " a,\n"
%                " b,\n"
%                " {sfdsf,sdfdsfs},\n"
%                " [sfsdf,sdfsdf]}">>,
%              p({1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]}, 1, 35, 100)),
    ?line "{1,{1,{2,3}}}" = p({1,{1,{2,3}}}, 1, 80, 100),

    ?line bt(<<"{wwwww,{wwwww,{wwwww,{wwwww,{wwwww,lkjsldfj,klsdjfjklds,\n"
 "                                   sdkfjdsl,sdakfjdsklj,sdkljfsdj}}}}}">>,
             p({wwwww,{wwwww,{wwwww,{wwwww,{wwwww,lkjsldfj,klsdjfjklds,
                              sdkfjdsl,sdakfjdsklj,sdkljfsdj}}}}}, -1)),

    % With no restriction on number of characters per line:
%     ?line bt(<<"{wwwww,{wwwww,{wwwww,{wwwww,{wwwww,lkjsldfj,klsdjfjklds,"
%                "sdkfjdsl,sdakfjdsklj,\n"
%                "                                   sdkljfsdj}}}}}">>,
%              p({wwwww,{wwwww,{wwwww,{wwwww,{wwwww,lkjsldfj,klsdjfjklds,
%                               sdkfjdsl,sdakfjdsklj,sdkljfsdj}}}}}, -1)),

    % With line breaks:
%     ?line bt(<<"{wwwww,{wwwww,{wwwww,{wwwww,{wwwww,lkjsldfj,\n"
%               "                                   klsdjfjklds,\n"
%               "                                   sdkfjdsl,\n"
%               "                                   sdakfjdsklj,\n"
%               "                                   sdkljfsdj}}}}}">>,
%              p({wwwww,{wwwww,{wwwww,{wwwww,{wwwww,lkjsldfj,klsdjfjklds,
%                               sdkfjdsl,sdakfjdsklj,sdkljfsdj}}}}}, -1)),
    ?line bt(<<"{wwwww,\n"
              "    {wwwww,\n"
              "        {wwwww,\n"
              "            {wwwww,\n"
              "                {wwwww,\n"
              "                    {lkjsldfj,\n"
              "                        {klsdjfjklds,\n"
              "                            {klajsljls,\n"
              "                                #aaaaaaaaaaaaaaaaaaaaa"
                   "aaaaaaaaaaaaaaaaaaaaaaaaaaaaa{}}}}}}}}}">>,
             p({wwwww,{wwwww,{wwwww,{wwwww,{wwwww,{lkjsldfj,
                            {klsdjfjklds,{klajsljls,
                {aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa}}}}}}}}}, 
               -1)),
    ?line "{{...},...}" = p({{a,b},{a,b,c},{d,e,f}},1,8,2),
    %% Closing brackets and parentheses count:
    ?line "{{a,b,c},\n {{1,2,\n   3}}}" = p({{a,b,c},{{1,2,3}}},1,11,-1),
    % With line breaks:
%     ?line "{{a,b,c},\n {{1,\n   2,\n   3}}}" = p({{a,b,c},{{1,2,3}}},1,11,-1),
    ?line "{{a,b,c},\n [1,2,\n  3]}" = p({{a,b,c},[1,2,3]},1,10,-1),
    % With line breaks:
%     ?line "{{a,b,c},\n [1,\n  2,\n  3]}" = p({{a,b,c},[1,2,3]},1,10,-1),
    ?line "[{{a,b,c},\n  {1,2,\n   3}}]" = p([{{a,b,c},{1,2,3}}],1,12,-1),
    % With line breaks:
%     ?line "[{{a,b,c},\n  {1,\n   2,\n   3}}]" = p([{{a,b,c},{1,2,3}}],1,12,-1),

    %% A few lists:
    ?line "[]" = p([], 1, 20, -1),
    ?line "..." = p([], 1, 20, 0),
    ?line "[]" = p([], 1, 20, 1),
    ?line "[]" = p([], 1, 20, 2),
    ?line "[a]" = p([a], 1, 20, -1),
    ?line "..." = p([a], 1, 20, 0),
    ?line "[...]" = p([a], 1, 20, 1),
    ?line "[a]" = p([a], 1, 20, 2),
    ?line "[a,b]" = p([a,b], 1, 20, -1),
    ?line "..." = p([a,b], 1, 20, 0),
    ?line "[...]" = p([a,b], 1, 20, 1),
    ?line "[a|...]" = p([a,b], 1, 20, 2),
    ?line "[a,b]" = p([a,b], 1, 20, 3),
    ?line "[a|b]" = p([a|b], 1, 20, -1),
    ?line "..." = p([a|b], 1, 20, 0),
    ?line "[...]" = p([a|b], 1, 20, 1),
    ?line "[a|...]" = p([a|b], 1, 20, 2),
    ?line "[a|b]" = p([a|b], 1, 20, 3),
    ?line "[]" = p([], 1, 1, -1),
    ?line "..." = p([], 1, 1, 0),
    ?line "[]" = p([], 1, 1, 1),
    ?line "[]" = p([], 1, 1, 2),
    ?line "[a]" = p([a], 1, 1, -1),
    ?line "..." = p([a], 1, 1, 0),
    ?line "[...]" = p([a], 1, 1, 1),
    ?line "[a]" = p([a], 1, 1, 2),
    ?line "[a,\n b]" = p([a,b], 1, 1, -1),
    ?line "..." = p([a,b], 1, 1, 0),
    ?line "[...]" = p([a,b], 1, 1, 1),
    ?line "[a|...]" = p([a,b], 1, 1, 2),
    ?line "[a,\n b]" = p([a,b], 1, 1, 3),
    ?line "[a|\n b]" = p([a|b], 1, 1, -1),
    ?line "..." = p([a|b], 1, 1, 0),
    ?line "[...]" = p([a|b], 1, 1, 1),
    ?line "[a|...]" = p([a|b], 1, 1, 2),
    ?line "[a|\n b]" = p([a|b], 1, 1, 3),
    ?line "[{}]" = p([{}], 1, 1, 2),
    ?line "[[]]" = p([[]], 1, 1, 2),
    ?line bt(<<"[1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]]">>,
             p([1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]], -1)),
    ?line bt(<<"[1,2,a,b,\n {sfdsf,sdfdsfs},\n [sfsdf,sdfsdf]]">>,
             p([1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]], 1, 35, 100)),
    % With other terms than atomic ones on the same line:
%     ?line bt(<<"[1,2,a,b,{sfdsf,sdfdsfs},\n [sfsdf,sdfsdf]]">>,
%              p([1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]], 1, 35, 100)),
    % With line breaks:
%     ?line bt(<<"[1,\n"
%                " 2,\n"
%                " a,\n"
%                " b,\n"
%                " {sfdsf,sdfdsfs},\n"
%                " [sfsdf,sdfsdf]]">>,
%              p([1,2,a,b,{sfdsf,sdfdsfs},[sfsdf,sdfsdf]], 1, 35, 100)),
    %% Element #8 is not printable:
    ?line "[49," ++ _ = p("1234567"++[3,4,5,6,7], 1, 100, 9),
    % ?line "\"1234567\"..." = p("1234567"++[3,4,5,6,7], 1, 100, 8),

    %% A few records:
    %% -record(a, {}).
    %% -record(a, {}).
    ?line "..." = p({a}, 0),
    ?line "{...}" = p({a}, 1),
    ?line "#a{}" = p({a}, 2),
    ?line "#a{}" = p({a}, -1),
    %% -record(b, {f}).
    ?line "{...}" = p({b}, 1),
    ?line "..." = p({b,c}, 0),
    ?line "{...}" = p({b,c}, 1),
    ?line "#b{...}" = p({b,c}, 2),
    ?line "#b{f = c}" = p({b,c}, 3),
    ?line "#b{f = c}" = p({b,c}, -1),
    ?line "..." = p({b,{c,d}}, 0),
    ?line "{...}" = p({b,{c,d}}, 1),
    ?line "#b{...}" = p({b,{c,d}}, 2),
    ?line "#b{f = {...}}" = p({b,{c,d}}, 3),
    ?line "#b{f = {c,...}}" = p({b,{c,d}}, 4),
    ?line "#b{f = {c,d}}" = p({b,{c,d}}, 5),
    ?line "#b{f = {...}}" = p({b,{b,c}}, 3),
    ?line "#b{f = #b{...}}" = p({b,{b,c}}, 4),
    ?line "#b{f = #b{f = c}}" = p({b,{b,c}}, 5),
    %% -record(c, {f1, f2}).
    ?line "#c{f1 = d,f2 = e}" = p({c,d,e}, -1),
    ?line "..." = p({c,d,e}, 0),
    ?line "{...}" = p({c,d,e}, 1),
    ?line "#c{...}" = p({c,d,e}, 2),
    ?line "#c{f1 = d,...}" = p({c,d,e}, 3),
    ?line "#c{f1 = d,f2 = e}" = p({c,d,e}, 4),
    %% -record(d, {a..., b..., c.., d...}).
    ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,\n"
               "   cccccccccccccccccccc = 3,dddddddddddddddddddd = 4,\n"
               "   eeeeeeeeeeeeeeeeeeee = 5}">>,
             p({d,1,2,3,4,5}, -1)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,"
%                "cccccccccccccccccccc = 3,\n   dddddddddddddddddddd = 4,"
%                "eeeeeeeeeeeeeeeeeeee = 5}">>,
%              p({d,1,2,3,4,5}, -1)),
    % With line breaks:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
%               "   bbbbbbbbbbbbbbbbbbbb = 2,\n"
%               "   cccccccccccccccccccc = 3,\n"
%               "   dddddddddddddddddddd = 4,\n"
%               "   eeeeeeeeeeeeeeeeeeee = 5}">>,
%              p({d,1,2,3,4,5}, -1)),
    ?line "..." = p({d,1,2,3,4,5}, 0),
    ?line "{...}" = p({d,1,2,3,4,5}, 1),
    ?line "#d{...}" = p({d,1,2,3,4,5}, 2),
    ?line "#d{aaaaaaaaaaaaaaaaaaaa = 1,...}" = p({d,1,2,3,4,5}, 3),
    ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,...}">>,
             p({d,1,2,3,4,5}, 4)),
    ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,\n"
               "   cccccccccccccccccccc = 3,...}">>,
             p({d,1,2,3,4,5}, 5)), % longer than 80 characters...
    % With no restriction on number of characters per line:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,"
%                "cccccccccccccccccccc = 3,...}">>,
%              p({d,1,2,3,4,5}, 5)), % longer than 80 characters...
    % With line breaks:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
%               "   bbbbbbbbbbbbbbbbbbbb = 2,\n"
%               "   cccccccccccccccccccc = 3,...}">>,
%              p({d,1,2,3,4,5}, 5)),
    ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,\n"
               "   cccccccccccccccccccc = 3,dddddddddddddddddddd = 4,...}">>,
             p({d,1,2,3,4,5}, 6)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,"
%                "cccccccccccccccccccc = 3,\n   dddddddddddddddddddd = 4,...}">>,
%              p({d,1,2,3,4,5}, 6)),
    % With line breaks:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
%               "   bbbbbbbbbbbbbbbbbbbb = 2,\n"
%               "   cccccccccccccccccccc = 3,\n"
%               "   dddddddddddddddddddd = 4,...}">>,
%              p({d,1,2,3,4,5}, 6)),
    ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,\n"
               "   cccccccccccccccccccc = 3,dddddddddddddddddddd = 4,\n"
               "   eeeeeeeeeeeeeeeeeeee = 5}">>,
             p({d,1,2,3,4,5}, 7)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,bbbbbbbbbbbbbbbbbbbb = 2,"
%                "cccccccccccccccccccc = 3,\n   dddddddddddddddddddd = 4,"
%                "eeeeeeeeeeeeeeeeeeee = 5}">>,
%              p({d,1,2,3,4,5}, 7)),
    % With line breaks:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
%               "   bbbbbbbbbbbbbbbbbbbb = 2,\n"
%               "   cccccccccccccccccccc = 3,\n"
%               "   dddddddddddddddddddd = 4,\n"
%               "   eeeeeeeeeeeeeeeeeeee = 5}">>,
%              p({d,1,2,3,4,5}, 7)),
    ?line bt(<<"#rrrrr{\n"
              "    f1 = 1,\n"
              "    f2 = #rrrrr{f1 = a,f2 = b,f3 = c},\n"
              "    f3 = \n"
              "        #rrrrr{\n"
              "            f1 = h,f2 = i,\n"
              "            f3 = \n"
              "                #rrrrr{\n"
              "                    f1 = aa,\n"
              "                    f2 = \n"
              "                        #rrrrr{\n"
              "                            f1 = #rrrrr{f1 = a,f2 = b,f3 = c},\n"
              "                            f2 = 2,f3 = 3},\n"
              "                    f3 = bb}}}">>,
             p({rrrrr,1,{rrrrr,a,b,c},{rrrrr,h,i,
                                       {rrrrr,aa,{rrrrr,{rrrrr,a,b,c},
                                                  2,3},bb}}}, 
               -1)),
    % With other terms than atomic ones on the same line:
%     ?line bt(<<"#rrrrr{\n"
%               "    f1 = 1,f2 = #rrrrr{f1 = a,f2 = b,f3 = c},\n"
%               "    f3 = \n"
%               "        #rrrrr{\n"
%               "            f1 = h,f2 = i,\n"
%               "            f3 = \n"
%               "                #rrrrr{\n"
%               "                    f1 = aa,\n"
%               "                    f2 = \n"
%               "                        #rrrrr{\n"
%               "                            f1 = #rrrrr{f1 = a,f2 = b,"
%                                                "f3 = c},f2 = 2,f3 = 3},\n"
%               "                    f3 = bb}}}">>,
%              p({rrrrr,1,{rrrrr,a,b,c},{rrrrr,h,i,
%                                        {rrrrr,aa,{rrrrr,{rrrrr,a,b,c},
%                                                   2,3},bb}}}, 
%                -1)),
    % With line breaks:
%     ?line bt(<<"#rrrrr{\n"
%               "    f1 = 1,\n"
%               "    f2 = #rrrrr{f1 = a,f2 = b,f3 = c},\n"
%               "    f3 = \n"
%               "        #rrrrr{\n"
%               "            f1 = h,\n"
%               "            f2 = i,\n"
%               "            f3 = \n"
%               "                #rrrrr{\n"
%               "                    f1 = aa,\n"
%               "                    f2 = \n"
%               "                        #rrrrr{\n"
%               "                            f1 = #rrrrr{f1 = a,f2 = b,"
%                                                "f3 = c},\n"
%               "                            f2 = 2,\n"
%               "                            f3 = 3},\n"
%               "                    f3 = bb}}}">>,
%              p({rrrrr,1,{rrrrr,a,b,c},{rrrrr,h,i,
%                                        {rrrrr,aa,{rrrrr,{rrrrr,a,b,c},
%                                                   2,3},bb}}}, 
%                -1)),
    ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
              "   bbbbbbbbbbbbbbbbbbbb = \n"
              "       #d{aaaaaaaaaaaaaaaaaaaa = a,bbbbbbbbbbbbbbbbbbbb = b,\n"
              "          cccccccccccccccccccc = c,dddddddddddddddddddd = d,\n"
              "          eeeeeeeeeeeeeeeeeeee = e},\n"
              "   cccccccccccccccccccc = 3,\n"
              "   dddddddddddddddddddd = \n"
              "       #d{aaaaaaaaaaaaaaaaaaaa = h,bbbbbbbbbbbbbbbbbbbb = i,\n"
              "          cccccccccccccccccccc = \n"
              "              #d{aaaaaaaaaaaaaaaaaaaa = aa,"
                                       "bbbbbbbbbbbbbbbbbbbb = bb,\n"
              "                 cccccccccccccccccccc = \n"
              "                     #d{aaaaaaaaaaaaaaaaaaaa = 1,"
                                        "bbbbbbbbbbbbbbbbbbbb = 2,\n"
              "                        cccccccccccccccccccc = 3,"
                                        "dddddddddddddddddddd = 4,\n"
              "                        eeeeeeeeeeeeeeeeeeee = 5},\n"
              "                 dddddddddddddddddddd = dd,"
                                    "eeeeeeeeeeeeeeeeeeee = ee},\n"
              "          dddddddddddddddddddd = k,"
                              "eeeeeeeeeeeeeeeeeeee = l},\n"
              "   eeeeeeeeeeeeeeeeeeee = 5}">>,
             p({d,1,{d,a,b,c,d,e},3,{d,h,i,{d,aa,bb,{d,1,2,3,4,5},dd,ee},
                                     k,l},5}, -1)),
    % With line breaks:
%     ?line bt(<<"#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
%               "   bbbbbbbbbbbbbbbbbbbb = \n"
%               "       #d{aaaaaaaaaaaaaaaaaaaa = a,\n"
%               "          bbbbbbbbbbbbbbbbbbbb = b,\n"
%               "          cccccccccccccccccccc = c,\n"
%               "          dddddddddddddddddddd = d,\n"
%               "          eeeeeeeeeeeeeeeeeeee = e},\n"
%               "   cccccccccccccccccccc = 3,\n"
%               "   dddddddddddddddddddd = \n"
%               "       #d{aaaaaaaaaaaaaaaaaaaa = h,\n"
%               "          bbbbbbbbbbbbbbbbbbbb = i,\n"
%               "          cccccccccccccccccccc = \n"
%               "              #d{aaaaaaaaaaaaaaaaaaaa = aa,\n"
%               "                 bbbbbbbbbbbbbbbbbbbb = bb,\n"
%               "                 cccccccccccccccccccc = \n"
%               "                     #d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
%               "                        bbbbbbbbbbbbbbbbbbbb = 2,\n"
%               "                        cccccccccccccccccccc = 3,\n"
%               "                        dddddddddddddddddddd = 4,\n"
%               "                        eeeeeeeeeeeeeeeeeeee = 5},\n"
%               "                 dddddddddddddddddddd = dd,\n"
%               "                 eeeeeeeeeeeeeeeeeeee = ee},\n"
%               "          dddddddddddddddddddd = k,\n"
%               "          eeeeeeeeeeeeeeeeeeee = l},\n"
%               "   eeeeeeeeeeeeeeeeeeee = 5}">>,
%              p({d,1,{d,a,b,c,d,e},3,{d,h,i,{d,aa,bb,{d,1,2,3,4,5},dd,ee},
%                                      k,l},5}, -1)),

    A = aaaaaaaaaaaaa,
    %% Print the record with dots at the end of the line (Ll = 80).
    ?line "{aaaaaaa" ++ _ = 
          p({A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,
             {A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,
              {A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,
               {A,{A,{ggg,{hhh,{ii,{jj,{kk,{ll,{mm,{nn,{oo,{d,1,2,3,4,5}
                }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
                 }}}}}}}}}}}}}}}}, 146),
    ?line "{aaaaaaa" ++ _ = 
          p({A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,
             {A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,
              {A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{A,
               {A,{A,{A,{A,{A,{ggg,{hhh,{ii,{jj,{kk,{ll,{mm,{nn,{oo,{a}
                }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}
                 }}}}}}}}}}}}}}}}}}}, 152),

    ?line bt(<<"{aaaaaaaaaaaaa,\n"
               "    {aaaaaaaaaaaaa,\n"
               "        {aaaaaaaaaaaaa,\n"
               "            {aaaaaaaaaaaaa,\n"
               "                {aaaaaaaaaaaaa,\n"
               "                    {aaaaaaaaaaaaa,\n"
               "                        {g,{h,{i,{j,{k,{l,{m,{n,{o,#"
                                           "d{...}}}}}}}}}}}}}}}}">>,
             p({A,{A,{A,{A,{A,{A,
                {g,{h,{i,{j,{k,{l,{m,{n,{o,{d,1,2,3,4,5}}}}}}}}}}}}}}}}, 32)),
    ?line bt(<<"{a,#b{f = {c,{d,{e,{f,...}}}}}}">>,
             p({a,{b,{c,{d,{e,{f,g}}}}}}, 12)),
    ?line bt(<<"{aaaaaaaaaaaaa,\n"
              "    {aaaaaaaaaaaaa,\n"
              "        {aaaaaaaaaaaaa,\n"
              "            {aaaaaaaaaaaaa,\n"
              "                {aaaaaaaaaaaaa,\n"
              "                    {aaaaaaaaaaaaa,\n"
              "                        {aaaaaaaaaaaaa,\n"
              "                            {aaaaaaaaaaaaa,\n"
              "                                {aaaaaaaaaaaaa,#c{f1 = ddd,"
                          "f2 = eee}}}}}}}}}}">>,
             p({A,{A,{A,{A,{A,{A,{A,{A,{A,{c,ddd,eee}}}}}}}}}}, 100)),
    ?line bt(<<"{aaaaaaaaaaaaa,\n"
               "    {aaaaaaaaaaaaa,{aaaaaaaaaaaaa,{aaaaaaaaaaaaa,...}}}}">>, 
             p({A,{A,{A,{A,{b}}}}}, 8)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"{aaaaaaaaaaaaa,{aaaaaaaaaaaaa,{aaaaaaaaaaaaa,"
%               "{aaaaaaaaaaaaa,...}}}}">>, 
%              p({A,{A,{A,{A,{b}}}}}, 8)),
    ?line bt(<<"{aaaaaaaaaaaaa,\n"
            "    {aaaaaaaaaaaaa,\n"
            "        {aaaaaaaaaaaaa,{aaaaaaaaaaaaa,{aaaaaaaaaaaaa,...}}}}}">>,
             p({A,{A,{A,{A,{A,{b}}}}}}, 10)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"{aaaaaaaaaaaaa,\n"
%                "    {aaaaaaaaaaaaa,{aaaaaaaaaaaaa,{aaaaaaaaaaaaa,"
%                          "{aaaaaaaaaaaaa,...}}}}}">>,
%              p({A,{A,{A,{A,{A,{b}}}}}}, 10)),
    ?line bt(<<"{aaaaaaaaaaaaa,\n"
              "    {aaaaaaaaaaaaa,\n"
              "        {aaaaaaaaaaaaa,\n"
              "            {aaaaaaaaaaaaa,\n"
              "                {aaaaaaaaaaaaa,\n"
              "                    {aaaaaaaaaaaaa,\n"
              "                        {aaaaaaaaaaaaa,\n"
              "                            {aaaaaaaaaaaaa,\n"
              "                                {aaaaaaaaaaaaa,"
                      "{aaaaaaaaaaaaa,#a{}}}}}}}}}}}">>,
             p({A,{A,{A,{A,{A,{A,{A,{A,{A,{A,{a}}}}}}}}}}}, 23)),
    ?line bt(<<"{aaaaaaaaaaaaa,\n"
               "    {aaaaaaaaaaaaa,\n"
               "        {aaaaaaaaaaaaa,\n",
               "            #rrrrr{\n"
               "                f1 = kljlkjlksfdgkljlsdkjf,"
                         "f2 = kljkljsdaflkjlkjsdf,...}}}}">>,
             p({A,{A,{A,{rrrrr, kljlkjlksfdgkljlsdkjf, 
                         kljkljsdaflkjlkjsdf, 
                         asdfkldsjfklkljsdklfds}}}}, 10)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"{aaaaaaaaaaaaa,\n"
%                "    {aaaaaaaaaaaaa,\n"
%                "        {aaaaaaaaaaaaa,\n",
%                "            #rrrrr{f1 = kljlkjlksfdgkljlsdkjf,f2 = "
%                              "kljkljsdaflkjlkjsdf,...}}}}">>,
%              p({A,{A,{A,{rrrrr, kljlkjlksfdgkljlsdkjf, 
%                          kljkljsdaflkjlkjsdf, 
%                          asdfkldsjfklkljsdklfds}}}}, 10)),
    ?line bt(<<"{aaaaaaaaaaaaa,\n"
              "    {aaaaaaaaaaaaa,\n"
              "        {aaaaaaaaaaaaa,\n"
              "            {aaaaaaaaaaaaa,\n"
              "                {aaaaaaaaaaaaa,\n"
              "                    {aaaaaaaaaaaaa,\n"
              "                        {aaaaaaaaaaaaa,\n"
              "                            {g,{h,{i,{j,{k,{l,{m,{n,"
                               "{o,#a{}}}}}}}}}}}}}}}}}">>,
             p({A,{A,{A,{A,{A,{A,{A,
                      {g,{h,{i,{j,{k,{l,{m,{n,{o,{a}}}}}}}}}}}}}}}}}, 100)),
    ?line bt(<<"#c{\n"
              " f1 = \n"
              "  #c{\n"
              "   f1 = \n"
              "    #c{\n"
              "     f1 = \n"
              "      #c{\n"
              "       f1 = \n"
              "        #c{\n"
              "         f1 = \n"
              "          #c{\n"
              "           f1 = \n"
              "            #c{\n"
              "             f1 = \n"
              "              #c{\n"
              "               f1 = \n"
              "                #c{\n"
              "                 f1 = #c{f1 = #c{f1 = #c{f1 = a,"
                                     "f2 = b},f2 = b},f2 = b},\n"
              "                 f2 = b},\n"
              "               f2 = b},\n"
              "             f2 = b},\n"
              "           f2 = b},\n"
              "         f2 = b},\n"
              "       f2 = b},\n"
              "     f2 = b},\n"
              "   f2 = b},\n"
              " f2 = b}">>,
              p({c,{c,{c,{c,{c,{c,{c,{c,{c,{c,{c,{c,a,b},b},b},b},b},b},
                                b},b},b},b},b},b}, -1)),
    ?line bt(<<"#rrrrr{\n"
              " f1 = \n"
              "  #rrrrr{\n"
              "   f1 = \n"
              "    #rrrrr{\n"
              "     f1 = \n"
              "      #rrrrr{\n"
              "       f1 = \n"
              "        {rrrrr,{rrrrr,a,#rrrrr{f1 = {rrrrr,1,2},f2 = a,"
                                 "f3 = b}},b},\n"
              "       f2 = {rrrrr,c,d},\n"
              "       f3 = {rrrrr,1,2}},\n"
              "     f2 = 1,f3 = 2},\n"
              "   f2 = 3,f3 = 4},\n"
              " f2 = 5,f3 = 6}">>,
             p({rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,a,{rrrrr,
                              {rrrrr,1,2},a,b}},b},{rrrrr,c,d},{rrrrr,1,2}},
                              1,2},3,4},5,6}, -1)),
    % With other terms than atomic ones on the same line:
%     ?line bt(<<"#rrrrr{\n"
%               " f1 = \n"
%               "  #rrrrr{\n"
%               "   f1 = \n"
%               "    #rrrrr{\n"
%               "     f1 = \n"
%               "      #rrrrr{\n"
%               "       f1 = \n"
%               "        {rrrrr,{rrrrr,a,#rrrrr{f1 = {rrrrr,1,2},f2 = a,"
%                                  "f3 = b}},b},\n"
%               "       f2 = {rrrrr,c,d},f3 = {rrrrr,1,2}},\n"
%               "     f2 = 1,f3 = 2},\n"
%               "   f2 = 3,f3 = 4},\n"
%               " f2 = 5,f3 = 6}">>,
%              p({rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,a,{rrrrr,
%                               {rrrrr,1,2},a,b}},b},{rrrrr,c,d},{rrrrr,1,2}},
%                               1,2},3,4},5,6}, -1)),
    % With no restriction on number of characters per line:
%     ?line bt(<<"#rrrrr{\n"
%               " f1 = \n"
%               "  #rrrrr{\n"
%               "   f1 = \n"
%               "    #rrrrr{\n"
%               "     f1 = \n"
%               "      #rrrrr{\n"
%               "       f1 = {rrrrr,{rrrrr,a,#rrrrr{f1 = {rrrrr,1,2},f2 = a,"
%                                  "f3 = b}},b},\n"
%               "       f2 = {rrrrr,c,d},f3 = {rrrrr,1,2}},\n"
%               "     f2 = 1,f3 = 2},\n"
%               "   f2 = 3,f3 = 4},\n"
%               " f2 = 5,f3 = 6}">>,
%              p({rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,a,{rrrrr,
%                               {rrrrr,1,2},a,b}},b},{rrrrr,c,d},{rrrrr,1,2}},
%                               1,2},3,4},5,6}, -1)),
    % With line breaks:
%     ?line bt(<<"#rrrrr{\n"
%               " f1 = \n"
%               "  #rrrrr{\n"
%               "   f1 = \n"
%               "    #rrrrr{\n"
%               "     f1 = \n"
%               "      #rrrrr{\n"
%               "       f1 = {rrrrr,{rrrrr,a,#rrrrr{f1 = {rrrrr,1,2},f2 = a,"
%                                  "f3 = b}},b},\n"
%               "       f2 = {rrrrr,c,d},\n"
%               "       f3 = {rrrrr,1,2}},\n"
%               "     f2 = 1,\n"
%               "     f3 = 2},\n"
%               "   f2 = 3,\n"
%               "   f3 = 4},\n"
%               " f2 = 5,\n"
%               " f3 = 6}">>,
%              p({rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,{rrrrr,a,{rrrrr,
%                               {rrrrr,1,2},a,b}},b},{rrrrr,c,d},{rrrrr,1,2}},
%                               1,2},3,4},5,6}, -1)),
    ?line "{aaa,\n {aaa," ++ _ = 
        p({aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,
           {aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,
            {aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,
             {aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,{aaa,
              {aaa,a}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}, 
          1, 80, -1),

    %% A few other cases...
    ?line "{a,#Fun<" ++ _ = lists:flatten(io_lib_pretty:print({a,fun fmt/2})),
    ?line "#Fun<" ++ _ = io_lib_pretty:print(fun() -> foo end),
    % ?line "[<<\"foobar\">>|<<\"barf\"...>>]" = 
    %             p([<<"foobar">>|<<"barfoo">>], 1, 30, 4),
    %% No support for negative columns any more:
    ?line "[a,\n [b,\n  c,\n  d,\n  [e,\n   f]],\n c]" = 
          p([a,[b,c,d,[e,f]],c], -1, 2, 10),
    ?line "[a,\n [b,\n  c,\n  d,\n  [e,\n   f]],\n c]" = 
          p([a,[b,c,d,[e,f]],c], 0, 2, 10),
    %% 20 bytes are tried first, then the rest. Try 21 bytes:
    L = lists:duplicate(20, $a),
    % ?line bt(<<"<<\"aaaaaa\"\n  \"aaaaaa\"\n  \"aaaaaa\"\n  \"aaa\">>">>,
    ?line bt(<<"<<\"aaaaaaaaaaaaaaaaaaaaa\">>">>,
             p(list_to_binary([$a | L]), 1, 10, -1)),
    ?line "<<97," ++ _ = p(list_to_binary(L ++ [3]), 1, 10, -1),
    % ?line "<<\"aaaa\"...>>" = p(list_to_binary(L ++ [3]), 1, 10, 2),
    % ?line "<<\"aaaaaa\"\n  \"aa\"...>>" = 
    % ?line "<<\"aaaaaaaa\"...>>" =
    %               p(list_to_binary(L ++ [3]), 1, 10, 3),
    % ?line "<<\"aaaaaa\"\n  \"aaaaaa\"\n  \"aaaaaa\"\n  \"aa\"...>>" = 
    % ?line "<<\"aaaaaaaaaaaaaaaaaaaa\"...>>" =
    %        p(list_to_binary(L ++ [3]), 1, 10, 21),
    ?line "<<97," ++ _ = p(list_to_binary(L ++ [3]), 1, 10, 22),

    ?line "\"\\b\\t\\n\\v\\f\\r\\e\250\"" = 
              p([8,9,10,11,12,13,27,168], 1, 40, -1),
    % ?line "\"\\b\\t\\n\"\n \"\\v\\f\\r\"\n \"\\e\250\"" =
    ?line "\"\\b\\t\\n\\v\\f\\r\\e¨\"" =
              p([8,9,10,11,12,13,27,168], 1, 10, -1),
    ?line "\"\\b\\t\\n\\v\\f\\r\\e\250\"" = 
              p([8,9,10,11,12,13,27,168], 1, 40, 100),
    % ?line "\"\\e\\t\\nab\"\n \"cd\"" = 
    ?line "\"\\e\\t\\nabcd\"" = 
              p("\e\t\nabcd", 1, 12, -1),

    %% DEL (127) is special...
    ?line "[127]" = p("\d", 1, 10, -1),
    ?line "[127]" = p([127], 1, 10, 100),

    ?line "<<\"\\b\\t\\n\\v\\f\\r\\e\250\">>" = 
              p(<<8,9,10,11,12,13,27,168>>, 1, 40, -1),
    ?line "<<\"\\b\\t\\n\\v\\f\\r\\e\250\">>" = 
              p(<<8,9,10,11,12,13,27,168>>, 1, 10, -1),
    ?line "<<127>>" = p(<<127>>, 1, 10, 100),

    %% "Partial" string binaries:
    ?line "<<\"he\"...>>" = p(list_to_binary("he"++[3]), 1, 80, 2),
    ?line "<<\"he\"...>>" = p(list_to_binary("he"++[3]), 1, 80, 3),
    ?line "<<104,101,3>>" = p(list_to_binary("he"++[3]), 1, 80, 4),
    ?line "<<...>>" = p(list_to_binary([3] ++ "he"), 1, 80, 1),
    ?line "<<3,...>>" = p(list_to_binary([3] ++ "he"), 1, 80, 2),
    ?line "<<3,104,...>>" = p(list_to_binary([3] ++ "he"), 1, 80, 3),

    ?line "<<\"12345678901234567890\"...>>" = 
                 p(list_to_binary("12345678901234567890"++[3]), 1, 80, 8),
    ?line "<<\"12345678901234567890\"...>>" = 
                 p(list_to_binary("12345678901234567890"++[3]), 1, 80, 21),
    ?line "<<49," ++ _ = 
                 p(list_to_binary("12345678901234567890"++[3]), 1, 80, 22),

    ?line "{sdfsdfj,\n    23" ++ _ = 
           p({sdfsdfj,23423423342.23432423}, 1, 17, -1),

    ?line bt(<<"kljkljlksdjjlf kljalkjlsdajafasjdfj [kjljklasdf,kjlljsfd,sdfsdkjfsd,kjjsdf,jl,
                                     lkjjlajsfd|jsdf]">>,
             fmt("~w ~w ~p", 
                 [kljkljlksdjjlf,
                  kljalkjlsdajafasjdfj,
                  [kjljklasdf,kjlljsfd,sdfsdkjfsd,kjjsdf,jl,lkjjlajsfd | 
                   jsdf]])),

    %% Binaries are split as well:
    ?line bt(<<"<<80,100,0,55,55,55,55,55,55,55,55,55,\n  "
               "55,55,55,55,55,55,55,...>>">>, 
             p(<<80,100,0,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,
                 55,55,55,55,55,55,55,55,55,55,55,55>>,1,40,20)),
    ?line bt(<<"<<80,100,0,55,55,55,55,55,55,55,55,55,\n  "
               "55,55,55,55,55,55,55,55,55,55,55,55,\n  55,55,55,55,55,55>>">>,
             p(<<80,100,0,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,
                 55,55,55,55,55,55,55,55,55,55,55,55>>,1,40,-1)),
    ?line "<<0,0,0,\n  ...>>" = p(<<0,0,0,0,0>>, 1, 10, 4),

    %% ~W now uses ",..." when printing tuples
    ?line "[a,b|...]" = fmt("~W", [[a,b,c,d,e], 3]),
    ?line "{a,b,...}" = fmt("~W", [{a,b,c,d,e}, 3]),
    ok.

otp_6495(doc) ->
    ["OTP-6495. io_lib_pretty bugfix."];
otp_6495(suite) ->
    [];
otp_6495(Config) when is_list(Config) ->
    ?line bt(<<"[120,120,120,120,120,120,120,120,120,120,120,120,120,120,"
               "120,120,120,120,120]<<1>>">>,
             fmt("~w~p", ["xxxxxxxxxxxxxxxxxxx", <<1>>])),
    ok.

otp_6517(doc) ->
    ["OTP-6517. The Format argument of fwrite can be a binary."];
otp_6517(suite) ->
    [];
otp_6517(Config) when is_list(Config) ->
    ?line "string" = fmt(<<"~s">>, [<<"string">>]),
    ok.

otp_6502(doc) ->
    ["OTP-6502. Bits."];
otp_6502(suite) ->
    [];
otp_6502(Config) when is_list(Config) ->
    ?line bt(<<
     "[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]"
     "<<0,0,8,\n"
     "                                                                     "
     "  1:1>>">>,
             fmt("~w~p", [lists:seq(0, 25), <<17:25>>])),
    ok.

otp_7421(doc) ->
    ["OTP-7421. Soft limit of 60 chars removed when pretty printing."];
otp_7421(suite) ->
    [];
otp_7421(Config) when is_list(Config) ->
    bt(<<"{aa,bb,\n"
         "    c,dd,\n"
         "    eee,\n"
         "    fff}">>,
       rp({aa,bb,c,dd,eee,fff}, 1, 80, -1, 5, none)),
    bt(<<"{aa,bb,\n"
         "    c,\n"
         "    dd,\n"
         "    eee,\n"
         "    fff}">>,
       rp({aa,bb,c,dd,eee,fff}, 1, 80, -1, 4, none)),
    ok.

bt(Bin, R) ->
    R = binary_to_list(Bin).

p(Term, D) ->
    rp(Term, 1, 80, D).

p(Term, Col, Ll, D) ->
    rp(Term, Col, Ll, D, no_fun).

rp(Term, Col, Ll, D) ->
    rp(Term, Col, Ll, D, fun rfd/2).

-define(MAXCS, 60).

rp(Term, Col, Ll, D, RF) ->
    rp(Term, Col, Ll, D, ?MAXCS, RF).

rp(Term, Col, Ll, D, M, RF) ->
    %% io:format("~n~n*** Col = ~p Ll = ~p D = ~p~n~p~n-->~n", 
    %%           [Col, Ll, D, Term]),
    R = io_lib_pretty:print(Term, Col, Ll, D, M, RF),
    %% io:format("~s~n<--~n", [R]),
    lists:flatten(io_lib:format("~s", [R])).

fmt(Fmt, Args) ->
    FormatList = io_lib:scan_format(Fmt, Args),
    {Fmt2, Args2} = io_lib:unscan_format(FormatList),
    Chars1 = lists:flatten(io_lib:build_text(FormatList)),
    Chars2 = lists:flatten(io_lib:format(Fmt2, Args2)),
    Chars3 = lists:flatten(io_lib:format(Fmt, Args)),
    Chars1 = Chars2,
    Chars2 = Chars3,
    Chars3.

rfd(a, 0) ->
    [];
rfd(b, 1) ->
    [f];
rfd(c, 2) ->
    [f1, f2];
rfd(e, 3) ->
    [f, g, h];
rfd(d, 5) ->
    [aaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbb,
     cccccccccccccccccccc, dddddddddddddddddddd,
     eeeeeeeeeeeeeeeeeeee];
rfd(rrrrr, 3) ->
    [f1, f2, f3];
rfd(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, 0) ->
    [];
rfd(_, _) ->
    no.

manpage(doc) ->
    ["The examples in io(3) and io_lib(3)."];
manpage(suite) ->
    [];
manpage(Config) when is_list(Config) ->
    %% The examples that write or print only, not the ones that read...

    ?line bt(<<"Hello world!\n">>,
             fmt("Hello world!~n", [])),
    ?line bt(<<"|     aaaaa|bbbbb     |ccccc|\n">>, % bugfix
             fmt("|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c])),
    ?line bt(<<"|**********|\n">>,
             fmt("|~10w|~n", [{hey, hey, hey}])),
    ?line bt(<<"|{hey,hey,h|\n">>,
             fmt("|~10s|~n", [io_lib:write({hey, hey, hey})])),

    T = [{attributes,[[{id,age,1.50000},{mode,explicit},
        {typename,"INTEGER"}], [{id,cho},{mode,explicit},{typename,'Cho'}]]},
        {typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}],
    ?line bt(<<"[{attributes,[[{id,age,1.5},{mode,explicit},{typename,"
               "[73,78,84,69,71,69,82]}],[{id,cho},{mode,explicit},"
               "{typename,'Cho'}]]},{typename,'Person'},{tag,{'PRIVATE',3}},"
               "{mode,implicit}]\n">>,
             fmt("~w~n", [T])),
    ?line bt(<<"[{attributes,[[{id,age,1.5},\n"
               "               {mode,explicit},\n"
               "               {typename,\"INTEGER\"}],\n"
               "              [{id,cho},{mode,explicit},{typename,'Cho'}]]},\n"
               " {typename,'Person'},\n"
               " {tag,{'PRIVATE',3}},\n"
               " {mode,implicit}]\n">>,
             fmt("~62p~n", [T])),
    ?line bt(<<"Here T = [{attributes,[[{id,age,1.5},\n"
               "                        {mode,explicit},\n"
               "                        {typename,\"INTEGER\"}],\n"
               "                       [{id,cho},\n"
               "                        {mode,explicit},\n"
               "                        {typename,'Cho'}]]},\n"
               "          {typename,'Person'},\n"
               "          {tag,{'PRIVATE',3}},\n"
               "          {mode,implicit}]\n">>,
              fmt("Here T = ~62p~n", [T])),
    ?line bt(<<"[{attributes,[[{id,age,1.5},{mode,explicit},"
               "{typename,...}],[{id,cho},{mode,...},{...}]]},"
               "{typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}]\n">>,
             fmt("~W~n", [T,9])),
    ?line bt(<<"[{attributes,[[{id,age,1.5},{mode,explicit},{typename,...}],"
               "\n              "
               "[{id,cho},{mode,...},{...}]]},\n {typename,'Person'},\n "
               "{tag,{'PRIVATE',3}},\n {mode,implicit}]\n">>,
             fmt("~62P~n", [T,9])),

    ?line "1F\n" = fmt("~.16B~n", [31]),
    ?line "-10011\n" = fmt("~.2B~n", [-19]),
    ?line "5Z\n" = fmt("~.36B~n", [5*36+35]),
    ?line "10#31\n" = fmt("~X~n", [31,"10#"]),
    ?line "-0x1F\n" = fmt("~.16X~n", [-31,"0x"]),
    ?line "10#31\n" = fmt("~.10#~n", [31]),
    ?line "-16#1F\n" = fmt("~.16#~n", [-31]),
    ?line "abc def 'abc def'  {foo,1} A \n" = 
           fmt("~s ~w ~i ~w ~c ~n",
               ['abc def', 'abc def', {foo, 1},{foo, 1}, 65]),
    % fmt("~s", [65]),

    %% io_lib(3)
    ?line bt(<<"{1,[2],[3],[...],...}">>,
             lists:flatten(io_lib:write({1,[2],[3],[4,5],6,7,8,9}, 5))),
    ok.

otp_6708(doc) ->
    ["OTP-6708. Fewer newlines when pretty-printing."];
otp_6708(suite) ->
    [];
otp_6708(Config) when is_list(Config) ->
    ?line bt(<<"[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,\n"
               " 23,24,25,26,27,28,29|...]">>,
             p(lists:seq(1,1000), 30)),
    ?line bt(<<"{lkjasklfjsdak,mlkasjdflksj,klasdjfklasd,jklasdfjkl,\n"
               "               jklsdjfklsd,masdfjkkl}">>,
             p({lkjasklfjsdak,mlkasjdflksj,klasdjfklasd,jklasdfjkl,
                jklsdjfklsd, masdfjkkl}, -1)),
    ?line bt(<<"#b{f = {lkjljalksdf,jklaskfjd,kljasdlf,kljasdf,kljsdlkf,\n"
               "                    kjdd}}">>,
             p({b, {lkjljalksdf,jklaskfjd,kljasdlf,kljasdf,kljsdlkf,kjdd}}, 
               -1)),
    ?line bt(<<"#b{f = {lkjljalksdf,jklaskfjd,kljasdlf,kljasdf,kljsdlkf,\n"
               "                    kdd}}">>,
             p({b, {lkjljalksdf,jklaskfjd,kljasdlf,kljasdf,kljsdlkf,kdd}}, 
               -1)),
    ?line bt(<<"#e{f = undefined,g = undefined,\n"
               "   h = #e{f = 11,g = 22,h = 333}}">>,
             p({e,undefined,undefined,{e,11,22,333}}, -1)),
    ?line bt(<<"[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21|\n"
               " apa11]">>,
             p(lists:seq(1,21) ++ apa11, -1)),
    ?line bt(<<"[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,\n"
               " 23,\n"
               " {{abadalkjlasdjflksdajfksdklfsdjlkfdlskjflsdj"
                        "flsdjfldsdsdddd}}]">>,
          p(lists:seq(1,23) ++ 
            [{{abadalkjlasdjflksdajfksdklfsdjlkfdlskjflsdjflsdjfldsdsdddd}}],
            -1)),
    ?line bt(<<"{lkjasdf,\n"
               "    {kjkjsd,\n"
               "        {kjsd,\n"
               "            {kljsdf,\n"
               "                {kjlsd,{dkjsdf,{kjlds,{kljsd,{kljs,"
                                   "{kljlkjsd}}}}}}}}}}">>,
             p({lkjasdf,{kjkjsd,{kjsd,
                                 {kljsdf,
                                  {kjlsd,
                                   {dkjsdf,{kjlds,
                                            {kljsd,{kljs,{kljlkjsd}}}}}}}}}},
               -1)),
    ?line bt(<<"{lkjasdf,\n"
               "    {kjkjsd,\n"
               "        {kjsd,{kljsdf,{kjlsd,{dkjsdf,{kjlds,"
                                "{kljsd,{kljs}}}}}}}}}">>,
             p({lkjasdf,{kjkjsd,{kjsd,
                                 {kljsdf,{kjlsd,{dkjsdf,
                                                 {kjlds,{kljsd,{kljs}}}}}}}}},
               -1)),
    ?line bt(<<"<<1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,\n"
               "  22,23>>">>,
             p(list_to_binary(lists:seq(1,23)), -1)),
    ?line bt(<<"<<100,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,\n"
               "  27>>">>,
             p(list_to_binary([100|lists:seq(10,27)]), -1)),
    ?line bt(<<"<<100,101,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,\n"
               "  26>>">>,
             p(list_to_binary([100,101|lists:seq(10,26)]), -1)),
    ?line bt(<<"{{<<100,101,102,10,11,12,13,14,15,16,17,18,19,20,21,22,\n"
               "    23>>}}">>,
             p({{list_to_binary([100,101,102|lists:seq(10,23)])}}, -1)),
    ?line bt(<<"[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22|\n"
               " ap]">>,
             p(lists:seq(1,22) ++ ap, -1)),
    ?line bt(<<"[1,2,3,4,5,6,7,8,9,10,{},[],\n <<>>,11,12,13,14,15]">>,
             p(lists:seq(1,10) ++ [{},[],<<>>] ++ lists:seq(11,15),1,30,-1)),
    ?line bt(<<"[ddd,ddd,\n"
               " {1},\n"
               " [1,2],\n"
               " ddd,kdfd,\n"
               " [[1,2],a,b,c],\n"
               " <<\"foo\">>,<<\"bar\">>,1,\n"
               " {2}]">>,
             p([ddd,ddd,{1},[1,2],ddd,kdfd,[[1,2],a,b,c],<<"foo">>,<<"bar">>,
                1,{2}],1,50,-1)),

    ?line bt(<<"{dskljsadfkjsdlkjflksdjflksdjfklsdjklfjsdklfjlsdjfkl,jksd,\n"
               "                                                     "
                   "lkjsdf,kljsdf,kljsf,kljsdf,kljsdf,jkldf,jklsdf,kljsdf,\n"
               "                                                     "
                   "kljsdf,jklsdf,lkjfd,lkjsdf,kljsdf,kljsdf,lkjsdf,kljsdf,\n"
               "                                                     "
                   "lkjsdfsd,kljsdf,kjsfj}">>,
             p({dskljsadfkjsdlkjflksdjflksdjfklsdjklfjsdklfjlsdjfkl,jksd,
                lkjsdf,kljsdf,kljsf,kljsdf,kljsdf,jkldf,jklsdf,kljsdf,
                kljsdf,jklsdf,lkjfd,lkjsdf,kljsdf,kljsdf,lkjsdf,kljsdf,
                lkjsdfsd,kljsdf,kjsfj}, 1, 110, -1)),
    ?line bt(<<"{dskljsadfkjsdlkjflksdjflksdjfklsdjklfjsdklfjlsdjfkl,"
                  "#d{aaaaaaaaaaaaaaaaaaaa = 1,\n"
               "                                                        "
                  "bbbbbbbbbbbbbbbbbbbb = 2,cccccccccccccccccccc = 3,\n"
               "                                                        "
                  "dddddddddddddddddddd = 4,eeeeeeeeeeeeeeeeeeee = 5}}">>,
             rp({dskljsadfkjsdlkjflksdjflksdjfklsdjklfjsdklfjlsdjfkl,
                 {d,1,2,3,4,5}},1,200,-1)),
    ok.

-define(ONE(N), ((1 bsl N) - 1)).
-define(ALL_ONES, ((1 bsl 52) - 1)).

otp_7084(doc) ->
    ["OTP-7084. Printing floating point numbers nicely."];
otp_7084(suite) ->
    [];
otp_7084(Config) when is_list(Config) ->
    OldDog=?config(watchdog, Config),
    test_server:timetrap_cancel(OldDog),
    Timeout = 180,
    ?line Dog = test_server:timetrap({seconds,Timeout}),
    L = [{g_warm_up, fun g_warm_up/0},
         {g_big_pos_float, fun g_big_pos_float/0},
         {g_small_neg_float, fun g_small_neg_float/0},
         {g_close_to_zero, fun g_close_to_zero/0},
         {g_denormalized, fun g_denormalized/0},
         {g_normalized, fun g_normalized/0},
         {g_choice, fun g_choice/0},
         {g_misc, fun g_misc/0}],
    F = fun({M,T}) -> io:format("~p~n", [M]), T() end,
    R = try 
            lists:foreach(fun(T) -> F(T) end, L),
            ok
        catch throw:Reason -> 
            Reason
        end,
    ?line test_server:timetrap_cancel(Dog),
    R.

g_warm_up() ->
    g_t(0.5),
    g_t(-0.5),
    g_t((1 bsl 55) * 0.5),
    g_t(-(1 bsl 55) * 0.5),
    g_t(1.6799127650033296e+308),
    g_t(pack(1, 0, 2#1010101010001100101000010111100101000000101100110001)),
    g_t(pack(1, 0, 2#0001010000001001110100000101010101001110010001010110)),
    g_t(234324324.23432432432432),
    ok.

g_big_pos_float() ->
    %% The greatest positive float:
    ft({{0,2046,?ONE(52)}, 100, 0}),
    ok.

g_small_neg_float() ->
    %% The least negative float:
    ft({{1,2046,?ONE(52)}, 0, 100}),
    ok.

g_close_to_zero() ->
    %% A few denormalized floats close to zero:
    ft({{0,0,0}, 100, 100}),
    g_t(pack(1, 0, 0)), % -0.0
    ok.

g_denormalized() ->
    %% Denormalized floats (mantissa carry):
%    D = 5,
    %% Faster:
    D = 1,
    [ft({{S,0,?ONE(N)},D,D}) || S <- [0,1], N <- lists:seq(0, 52)],
    ok.

g_normalized() -> 
    %% Normalized floats (exponent carry):
%    D = 5,
    %% Faster:
    D = 1,
    [ft({{S,E,?ONE(52)},D,D}) || S <- [0,1], E <- lists:seq(0, 2045)],
    ok.

g_choice() ->
    %% Exponent should be used when and only when the string is shorter.
    %% (g_misc/0 checks this too, and probably more throughly).
    L = [0.0003, 3.0e-5, 3.3e-5,    3.3e-4,
         314.0,  314.1,  310.0,     3.1e6,   -100.0,
         3.34e4, 3.0e3,  3.34333e9, 3.3433323e10, 33433323700.0,
         0.00197963, 1.97963e-4],
    lists:foreach(fun(V) -> g_t(V) end, L),
    ok.

g_misc() -> 
    L_0_308 = lists:seq(0, 308),
    L_0_307 = lists:seq(0, 307),
%    L_1_9 = lists:seq(1, 9),
%    L_0_9 = lists:seq(0, 9),
    %% Faster:
    L_1_9 = [1,5,9],
    L_0_9 = [0,1,5,9],

    %% 1.0,10.0, ... 2.0,20.0, ... 9.0,90.0, ... -1,-10, ... -2.0,-20.0...
    [g_t(S*T*pow10(N)) || S <- [1.0, -1.0], T <- L_1_9, N <- L_0_307],

    %% 1.0,1.0/10,1.0/100,... 2.0,2.0/10,2.0/100, ... 9.0,9.0/10,9.0/100,
    %% -1.0,-1.0/10,... -9.0,-9.0/10...
    [g_t(S*T/pow10(N)) || S <- [1.0, -1.0], T <- L_1_9, N <- L_0_308],

    %% 0.0,1.0,2.0,...,9.0, 0.0,10.0,20.0,...,90.0,...
    %% 0.0,-1.0,-2.0,...,-9.0, 0.0,-10.0,-20.0,...,-90.0,...
    [g_t(S*list_to_float([D+$0]++lists:duplicate(N, $0)++".0")) ||
        S <- [1.0,-1.0], N <- lists:seq(0, 300), D <- L_0_9],

    %% 0.0,0.1,0.2,...0,9, 0.0,0.01,0.02,...,0.09,
    %% 0.0,-0.1,-0.2,...-0,9, 0.0,-0.01,-0.02,...,-0.09,
    [g_t(S*list_to_float("0."++lists:duplicate(N, $0)++[D+$0])) ||
        S <- [1.0,-1.0], N <- lists:seq(0, 300), D <- L_0_9],
    ok.

ft({{S,E,M}, L, G}) ->
    ft({pack(S, E, M), L, G});
ft({V, Less, Greater}) when is_float(V) ->
    _ = g_t(V),
    ft(V, fun inc/1, Greater),
    ft(V, fun dec/1, Less).

ft(V0, F, I) when I > 0, is_float(V0) ->
    V = F(V0),
    _ = g_t(V),
    ft(V, F, I - 1);
ft(V, _F, 0) when is_float(V) ->
    ok.

g_t(V) when is_float(V) ->
    %% io:format("Testing ~.17g~n", [V]),
    Io = io_lib:format("~p", [V]),
    Sv = binary_to_list(iolist_to_binary(Io)),
    ok = g_t(V, Sv),
    Sv.

%% -> ok | THROW

%% Checks that Sv is the shortest, correctly rounded string that
%% converts to V when read back with list_to_float/1.
%% Note: in a few cases the least significant digit has been
%% incremented by one, namely when the correctly rounded string
%% converts to another floating point number.
g_t(0.0, "0.0") ->
    ok;
g_t(V, Sv) ->
    try
        g_t_1(V, Sv)
    catch throw:Reason ->
        throw({Reason, V, Sv})
    end.

g_t_1(V, Sv) ->
    %% Check that the least significant digit is correct.
    %% If Sv is "3.14" then Sv- is "3.13" and Sv+ is "3.15".
    %% Check that |V - Sv| =< (V - Sv-) and
    %%       that |V - Sv| =< (Sv+ - V)
    Times = least_significant_digit(Sv),
    case Times of
        0 -> throw(least_significant_digit_is_zero);
        _ -> ok
    end,
    S = if V < 0 -> -1; true -> 1 end,
    SvMinus = incr_lsd(Sv, -S),
    SvPlus = incr_lsd(Sv, S),
    Svr = s2r(Sv),
    Svminusr = s2r(SvMinus),
    Svplusr = s2r(SvPlus),
    Vr = f2r(V),

    Abs_Sv_Vr = rat_abs(rat_minus(Svr, Vr)),
    Svminus_Vr = rat_minus(Vr, Svminusr),
    Svplus_Vr = rat_minus(Svplusr, Vr),
    %% The are 45 (negative) floats where SvMinus (SvPlus) is closer
    %% to V than Sv, but such that when reading SvMinus (SvPlus) wrong
    %% float would be returned.
    case rat_lte(Abs_Sv_Vr, Svminus_Vr) of
        true -> 
            ok;
        false ->  
             case list_to_float(SvMinus) of
                 V -> throw(vsminus_too_close_to_v);
                 _Vminus -> ok
             end
    end,
    case rat_lte(Abs_Sv_Vr, Svplus_Vr) of
        true -> 
            ok;
        false ->
             case list_to_float(SvPlus) of
                 V -> throw(vsplus_too_close_to_v);
                 _Vplus -> ok
             end
    end,

    %% Check that Sv is closer to V than to V- and V+.
    %% Check that |V - Sv| =< (V - V-) and
    %%       that |V - Sv| =< (V+ - V)
    %% (An alternative is  V- + V =< 2*Sv =< V + V+.)
    case inc(V) of
        inf -> 
            ok;
        Vplus ->
            Vplusr = f2r(Vplus),
            V_Vplusr = rat_minus(Vplusr, Vr),
            case rat_lte(Abs_Sv_Vr, V_Vplusr) of
                true -> ok;
                false -> throw(vplus_too_close_to_sv)
            end
    end,
    case dec(V) of
        '-inf' ->
            ok;
        Vminus ->
            Vminusr = f2r(Vminus),
            V_Vminusr = rat_minus(Vr, Vminusr),
            case rat_lte(Abs_Sv_Vr, V_Vminusr) of
                true -> ok;
                false -> throw(vminus_too_close_to_sv)
            end
    end,

    %% Check that no prefix of Sv yields V.
    %% If Sv is "3.14" then Svlow is "3.1" and Svhigh is "3.2".
    %%
    %% This is just one way of getting Svlow and Svhigh:
    if
        V < 0 ->
            SvHigh = step_lsd(Sv, -Times),
            SvLow = step_lsd(Sv, 10 - Times);
        true ->
            SvHigh = step_lsd(Sv, 10 - Times),
            SvLow = step_lsd(Sv, -Times)
    end,

    case catch list_to_float(SvLow) of
        V -> throw(low_is_v);
        _ -> ok
    end,

    case catch list_to_float(SvHigh) of
        V -> throw(high_is_v);
        _ -> ok
    end,

    %% Check that Sv has enough digits.
    case list_to_float(Sv) of
        V -> ok;
        _ -> throw(wrong_float) % cannot happen
    end,

    g_choice(Sv),

    ok.

%%% In "123450000.0", '5' is the lsd; 
%%% in "1234.0000", (the last) '0' is the lsd;
%%% in "1234.0", '4' is the lsd (the Erlang syntax requires the final zero).

%% Trailing zeroes are not significant ("3.0", "5.0e-324", "232000.0").
least_significant_digit("-"++Ds) ->
    least_significant_digit(Ds);
least_significant_digit("+"++Ds) ->
    least_significant_digit(Ds);
least_significant_digit(Ds) ->
    [MS|_E] = string:tokens(Ds, "eE"),
    lsd0(lists:reverse(MS))-$0.

lsd0("0."++Ds) ->
    lsd1(Ds);
lsd0([D | _Ds]) ->
    D.

lsd1("0"++Ds) ->
    lsd1(Ds);
lsd1([D | _Ds]) ->
    D.

step_lsd(Ds, 0) ->
    Ds;
step_lsd(Ds, N) when N > 0 ->
    NDs = incr_lsd(Ds, 1),
    step_lsd(NDs, N - 1);
step_lsd(Ds, N) when N < 0 ->
    NDs = incr_lsd(Ds, -1),
    step_lsd(NDs, N + 1).

%% Assumes Ds represents some other number than zero.
%% Increments or decrements the least significant digit.
incr_lsd("-"++Ds, I) ->
    "-"++incr_lsd(Ds, I);
incr_lsd(Ds, I) when I =:= 1; I =:= -1 -> 
    [MS|E] = string:tokens(Ds, "eE"),
    X = ["e" || true <- [E =/= []]],
    lists:flatten([incr_lsd0(lists:reverse(MS), I, []), X, E]).

incr_lsd0("0."++Ds, C, L) ->
    incr_lsd1(Ds, C, [$., $0 | L]);
incr_lsd0(Ds, C, L) ->
    incr_lsd2(Ds, C, L).

incr_lsd1("0"++Ds, C, L) ->
    incr_lsd1(Ds, C, [$0 | L]);
incr_lsd1(Ds, C, L) ->
    incr_lsd2(Ds, C, L).

incr_lsd2([], C, L) ->
    [C + $0 | L];
incr_lsd2("."++Ds, C, L) ->
    incr_lsd2(Ds, C, [$. | L]);
incr_lsd2("9"++Ds, 1=C, L) ->
    incr_lsd2(Ds, C, [$0 | L]);
incr_lsd2("0"++Ds, -1=C, L) ->
    incr_lsd2(Ds, C, [$9 | L]);
incr_lsd2([D | Ds], C, L) ->
    lists:reverse(Ds, [D + C | L]).

s2r(S) when is_list(S) ->
    case string:tokens(S, "eE") of
        [MS] ->
            s10(MS);
        [MS, ES] ->
            Mr = s10(MS),
            E = list_to_integer(ES),
            if 
                E < 0 ->
                    rat_multiply(Mr, {1,pow10(-E)});
                true ->
                    rat_multiply(Mr, {pow10(E), 1})
            end
    end.

s10("-"++S) ->
    rat_multiply({-1,1},s10(S));
s10(S) ->
    [AS, BS] = string:tokens(S, "."),
    Sc = length(BS),
    A = list_to_integer(AS),
    B = list_to_integer(BS),
    F = pow10(Sc),
    rat_multiply({1,1}, {A*F + B, F}).

pow10(X) ->
    int_pow(10, X).

int_pow(X, 0) when is_integer(X) ->
    1;
int_pow(X, N) when is_integer(X), is_integer(N), N > 0 ->
    int_pow(X, N, 1).

int_pow(X, N, R) when N < 2 ->
    R * X;
int_pow(X, N, R) ->
    int_pow(X * X, N bsr 1, case N band 1 of 1 -> R * X; 0 -> R end).

dec(F) when is_float(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    dec({S,BE,M});
dec({1,2046,?ALL_ONES}) ->
    '-inf';
dec({S,BE,M}) when 0 =< S, S =< 1,
                   0 =< BE, BE =< 2046,
                   0 =< M, M =< ?ALL_ONES ->
    {S1,BE1,M1} = dec1(S, BE, M),
    <<F:64/float>> = <<S:1, BE:11, M:52>>,
    <<F1:64/float>> = <<S1:1, BE1:11, M1:52>>,
    true = F1 < F,
    F1.
    

dec1(0, 0, 0) ->
    dec1(1, 0, 0);
dec1(0, BE, 0) ->
    {0,BE-1,?ALL_ONES};
dec1(0, BE, M) ->
    {0,BE,M-1};
dec1(1, BE, ?ALL_ONES) ->
    {1,BE+1,0};
dec1(1, BE, M) ->
    {1,BE,M+1}.

inc(F) when is_float(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    inc({S,BE,M});
inc({0,2046,?ALL_ONES}) ->
    inf;
inc({S,BE,M}) when 0 =< S, S =< 1,
                   0 =< BE, BE =< 2046,
                   0 =< M, M =< ?ALL_ONES ->
    {S1,BE1,M1} = inc1(S, BE, M),
    <<F:64/float>> = <<S:1, BE:11, M:52>>,
    <<F1:64/float>> = <<S1:1, BE1:11, M1:52>>,
    true = F1 > F,
    F1.

inc1(0, BE, ?ALL_ONES) ->
    {0,BE+1,0};
inc1(0, BE, M) ->
    {0,BE,M+1};
inc1(1, 0, 0) ->
    inc1(0, 0, 0);
inc1(1, BE, 0) ->
    {1,BE-1,?ALL_ONES};
inc1(1, BE, M) ->
    {1,BE,M-1}.

f2r(F) when is_float(F) ->
    <<S:1, BE:11, M:52>> = <<F:64/float>>,
    f2r({S,BE,M});
f2r({S,BE,M}) when 0 =< S, S =< 1,
                   0 =< BE, BE =< 2046,
                   0 =< M, M =< ?ALL_ONES ->
    Vr = {T,N} = f2r1(S, BE, M),
    <<F:64/float>> = <<S:1, BE:11, M:52>>,
    case catch T/N of
        {'EXIT', _} -> ok;
        TN -> true = F =:= TN
    end,
    Vr.

f2r1(S, 0, M) ->
    rat_multiply({sign(S),1}, {M, 1 bsl 1074});
f2r1(S, BE, M) when BE - 1075 >= 0 ->
    rat_multiply({sign(S),1}, {((1 bsl 52)+M) * (1 bsl (BE-1075)),1});
f2r1(S, BE, M) ->
    rat_multiply({sign(S),1}, {(1 bsl 52)+M, 1 bsl (1075-BE)}).

sign(0) ->
    1;
sign(1) ->
    -1.

%%% Rational numbers (very scetchy).

rat_abs({A,B}) when A < 0 ->
    {-A,B};
rat_abs({A,B}) ->
    {A,B}.

-ifdef(not_used).
rat_equal(R1, R2) ->
    R1 =:= R2.

rat_negate({A,B}) ->
    {-A,B}.

rat_divide({A,B},{C,D}) ->
    rat_multiply({A,B},{D,C}).
-endif.

rat_lte({A,B}, {C,D}) when B =/= 0, D =/= 0 ->
    A*D =< C*B.

rat_minus({A,B}, {C,D}) ->
    rat_plus({A,B}, {-C,D}).

rat_plus({A,B}, {C,D})  when B =/= 0, D =/= 0 ->
    rat_normalize({A*D+B*C, B*D}).

rat_multiply({A,B}, {C,D})  when B =/= 0, D =/= 0 ->
    rat_normalize({A * C, B * D}).

rat_normalize({T,N}) when N =/= 0 ->
    G = gcd(T, N),
    T2 = T div G,
    N2 = N div G,
    if
        T2 < 0 ->
            if 
                N2 < 0 -> {-T2,-N2};
                true -> {T2,N2}
            end;
        true ->
            if 
                N2 < 0 -> {-T2,-N2};
                true -> {T2,N2}
            end
    end.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

%%% End of rational numbers.

%% Check that there is an exponent if and only if characters are
%% saved. Note: this assumes floating point numbers "Erlang style"
%% (with a single zero before and after the dot, and no extra leading
%% zero in the exponent).
g_choice(S) when is_list(S) ->
    [MS | ES0] = string:tokens(S, "eE"),
    [IS, FS] = string:tokens(MS, "."),
    Il = length(IS),
    Fl = length(FS),
    Pre = z(MS),
    Post = z(lists:reverse(MS)),
    ES = lists:append(ES0),
    El = length(ES),
    I = list_to_integer(IS),
    if
        El =/= 0, ((I > 9) or (I < -9)) ->
            throw(too_many_digits_before_the_dot);
        El =/= 0, I =:= 0 ->
            throw(zero_before_the_dot);
        Pre =:= 0, Post > 0,   El =:= 0 -> % DDDD0000.0
            Saving = if
                         I < 0, Il =:= Post + 2 ->
                             Post;
                         I > 0, Il =:= Post + 1 ->
                             Post;
                         I =/= 0, true ->
                             Post + 1
                     end,
            Cost = 1 + length(integer_to_list(Il - 1)),
            if
                Cost < Saving ->
                    throw(with_exponent_is_shorter);
                true ->
                    ok
            end;
        Pre > 0,   Post =:= 0, El =:= 0 -> % 0.000DDDD
            Saving = if
                         Fl =:= Pre + 1 ->
                             Pre;
                         true ->
                             Pre + 1
                     end,
            Cost = 2 + length(integer_to_list(Pre + 1)),
            if
                Cost < Saving ->
                    throw(with_exponent_is_shorter);
                true ->
                    ok
            end;
        Pre =:= 0, Post =:= 0, El > 0 ->   % D.DDDeDD
            E = list_to_integer(ES),
            if 
                E >= 0 ->
                    Cost = E - (Fl - 1);
                E < 0 ->
                    Cost = -E
            end,
            Saving = length(ES) + 1,
            if
                Cost =:= Saving ->
                    throw(draw_but_choose_form_without_exponent);
                Cost < Saving ->
                    throw(without_exponent_is_shorter);
                true ->
                    ok
            end;
        Pre =:= 0, Post =:= 0, El =:= 0 -> % DDD.DDD
            ok;
        true ->
            throw(badly_formed_floating_point_string)
    end.

z("0."++Ds) ->
    length(lists:takewhile(fun(D) -> D =:= $0 end, Ds));
z(_Ds) ->
    0.

pack(Sign, Exp, Frac) ->
    <<Float:64/float>> = <<Sign:1, Exp:11, Frac:52>>,
    Float.

%% Whitebox test of io_lib:collect_line/3.
io_lib_collect_line_3_wb(Config) when is_list(Config) ->
    ?line do_collect_line(binary, "\n"),
    ?line do_collect_line(binary, "\r\n"),
    ?line do_collect_line(list, "\n"),
    ?line do_collect_line(list, "\r\n"),
    ok.

do_collect_line(Mode, Eol) ->
    First = "abcde",
    FirstNL = First++"\n",
    Second = "unterminated line",
    Data0 = First ++ Eol ++ Second,
    {Data1,Result0} = do_collect_line_combine(Mode, Data0, FirstNL, Second),
    do_collect_line_1(Mode, Data1, Result0, []),

    {Data,Result} = do_collect_line_combine(Mode, "unterm", "unterm", eof),
    do_collect_line_1(Mode, Data, Result, []).

do_collect_line_combine(binary, Data0, FirstNL, eof) ->
    {list_to_binary(Data0),
     {stop,list_to_binary(FirstNL),eof}};
do_collect_line_combine(binary, Data0, FirstNL, Second) ->
    {list_to_binary(Data0),
     {stop,list_to_binary(FirstNL),list_to_binary(Second)}};
do_collect_line_combine(list, Data0, FirstNL, Second) ->
    {Data0,{stop,FirstNL,Second}}.

do_collect_line_1(Mode, [H|T], Result, Acc0) ->
    Acc = [H|Acc0],
    Result = do_collect_line_2(lists:reverse(Acc), T),
    do_collect_line_1(Mode, T, Result, Acc);
do_collect_line_1(Mode, <<H,T/binary>>, Result, Acc0) ->
    Acc = [H|Acc0],
    Result = do_collect_line_2(list_to_binary(lists:reverse(Acc)), T),
    do_collect_line_1(Mode, T, Result, Acc);
do_collect_line_1(_Mode, [], _Result, _Acc) ->
    ok;
do_collect_line_1(_Mode, <<>>, _Result, _Acc) ->
    ok.

do_collect_line_2(Part1, Part2) ->
    Dummy = make_ref(),
    do_collect_line_3(start, [Part1,Part2,eof], Dummy).

do_collect_line_3(State0, [H|T], Dummy) ->
    case io_lib:collect_line(State0, H, Dummy) of
	{stop,Line,Rest} ->
	    {stop,Line,do_collect_line_adjust_rest(Rest, T)};
	State ->
	    do_collect_line_3(State, T, Dummy)
    end.

do_collect_line_adjust_rest(eof, []) -> eof;
do_collect_line_adjust_rest(eof, <<>>) -> eof;
do_collect_line_adjust_rest(Rest, [eof]) -> Rest;
do_collect_line_adjust_rest(Rest, [Bin|T]) when is_binary(Bin) ->
    do_collect_line_adjust_rest(<<Rest/binary,Bin/binary>>, T);
do_collect_line_adjust_rest(Rest, [List|T]) when is_list(List) ->
    do_collect_line_adjust_rest(Rest++List, T).



cr_whitespace_in_string(Config) when is_list(Config) ->
    ?line {ok,["abc"],[]} = io_lib:fread("~s", "\rabc").



io_fread_newlines(Config) when is_list(Config) ->
    ?line PrivDir = ?privdir(Config),
    ?line Fname = filename:join(PrivDir, "io_fread_newlines.txt"),
    ?line F0 = [[0,1,2,3,4,5,6,7,8,9]],
    ?line F1 = [[0,1,2,3,4,5,6,7,8],[9]],
    ?line F2 = [[0,1,2,3,4,5,6,7],[8,9]],
    ?line F3 = [[0,1,2,3,4,5,6],[7,8,9]],
    ?line F4 = [[0,1,2,3,4,5],[6,7,8,9]],
    ?line F5 = [[0,1,2,3,4],[5,6,7,8,9]],
    ?line F6 = [[0,1,2,3],[4,5,6,7],[8,9]],
    ?line F7 = [[0,1,2],[3,4,5],[6,7,8],[9]],
    ?line F8 = [[0,1],[2,3],[4,5],[6,7],[8,9]],
    ?line F9 = [[0],[1],[2],[3],[4],[5],[6],[7],[8],[9]],
    ?line Newlines = ["\n", "\r\n", "\r"],
    try
	?line io_fread_newlines_1([F0,F1,F2,F3,F4,F5,F6,F7,F8,F9],
				  Fname, Newlines)
    after
	file:delete(Fname)
    end.

io_fread_newlines_1(Fs, Fname, [Newline|Newlines]) ->
    ?line ok = io_fread_newlines_2(Fs, Fname, Newline),
    ?line io_fread_newlines_1(Fs, Fname, Newlines);
io_fread_newlines_1(_, _, []) -> ok.

io_fread_newlines_2([F|Fs], Fname, Newline) ->
    ?line N1 = write_newlines_file(Fname, F, Newline),
    ?line {F2,N2} = read_newlines_file(Fname),
    ?line io:format("~w ~p ~w~n~n", [N1,F,N2]),
    ?line F2 = lists:flatten(F),
    %% Intermediate newlines are not counted
    ?line N2 = N1 - (length(F) - 1)*length(Newline),
    ?line io_fread_newlines_2(Fs, Fname, Newline);
io_fread_newlines_2([], _, _) -> ok.


write_newlines_file(Fname, F, Newline) ->
    Bytes = list_to_binary(digit_lines(F, Newline)),
    io:format("Data: ~w~n~w~n", [Newline,Bytes]),
    ok = file:write_file(Fname, Bytes),
    size(Bytes).

digit_lines([L], _) ->
    digit_line(L);
digit_lines([L|Ls], Newline) ->
    [digit_line(L),Newline|digit_lines(Ls, Newline)].

digit_line([D]) ->
    integer_to_list(D);
digit_line([D|Ds]) ->
    [integer_to_list(D), " ", digit_line(Ds)].

read_newlines_file(Fname) ->
    {ok,Fd} = file:open(Fname, [read]),
    try {L, N0} = R0 = read_newlines(Fd, [], 0),
	case io:fread(Fd, "", "~*s~l") of
	    eof -> R0;
	    {ok,[N]} -> {L,N0+N}
	end
    after
	file:close(Fd)
    end.
    

read_newlines(Fd, Acc, N0) ->
    case io:fread(Fd, "", "~d~l") of
	{ok,[D,N]} ->
	    read_newlines(Fd, [D|Acc], N0+N);
	eof ->
	    {lists:reverse(Acc),N0}
    end.



otp_8989(doc) ->
    "OTP-8989 io:format for ~F.Ps ignores P in some cases";
otp_8989(Suite) when is_list(Suite) ->
    Hello = "Hello",
    ?line " Hello" = fmt("~6.6s", [Hello]),
    ?line " Hello" = fmt("~*.6s", [6,Hello]),
    ?line " Hello" = fmt("~6.*s", [6,Hello]),
    ?line " Hello" = fmt("~*.*s", [6,6,Hello]),
    %%
    ?line " Hello" = fmt("~6.5s", [Hello]),
    ?line " Hello" = fmt("~*.5s", [6,Hello]),
    ?line " Hello" = fmt("~6.*s", [5,Hello]),
    ?line " Hello" = fmt("~*.*s", [6,5,Hello]),
    %%
    ?line "  Hell" = fmt("~6.4s", [Hello]),
    ?line "  Hell" = fmt("~*.4s", [6,Hello]),
    ?line "  Hell" = fmt("~6.*s", [4,Hello]),
    ?line "  Hell" = fmt("~*.*s", [6,4,Hello]),
    %%
    ?line "Hello" = fmt("~5.5s", [Hello]),
    ?line "Hello" = fmt("~*.5s", [5,Hello]),
    ?line "Hello" = fmt("~5.*s", [5,Hello]),
    ?line "Hello" = fmt("~*.*s", [5,5,Hello]),
    %%
    ?line " Hell" = fmt("~5.4s", [Hello]),
    ?line " Hell" = fmt("~*.4s", [5,Hello]),
    ?line " Hell" = fmt("~5.*s", [4,Hello]),
    ?line " Hell" = fmt("~*.*s", [5,4,Hello]),
    %%
    ?line "Hell" = fmt("~4.4s", [Hello]),
    ?line "Hell" = fmt("~*.4s", [4,Hello]),
    ?line "Hell" = fmt("~4.*s", [4,Hello]),
    ?line "Hell" = fmt("~*.*s", [4,4,Hello]),
    %%
    ?line " Hel" = fmt("~4.3s", [Hello]),
    ?line " Hel" = fmt("~*.3s", [4,Hello]),
    ?line " Hel" = fmt("~4.*s", [3,Hello]),
    ?line " Hel" = fmt("~*.*s", [4,3,Hello]),
    %%
    %%
    ?line "Hello " = fmt("~-6.6s", [Hello]),
    ?line "Hello " = fmt("~*.6s", [-6,Hello]),
    ?line "Hello " = fmt("~-6.*s", [6,Hello]),
    ?line "Hello " = fmt("~*.*s", [-6,6,Hello]),
    %%
    ?line "Hello " = fmt("~-6.5s", [Hello]),
    ?line "Hello " = fmt("~*.5s", [-6,Hello]),
    ?line "Hello " = fmt("~-6.*s", [5,Hello]),
    ?line "Hello " = fmt("~*.*s", [-6,5,Hello]),
    %%
    ?line "Hell  " = fmt("~-6.4s", [Hello]),
    ?line "Hell  " = fmt("~*.4s", [-6,Hello]),
    ?line "Hell  " = fmt("~-6.*s", [4,Hello]),
    ?line "Hell  " = fmt("~*.*s", [-6,4,Hello]),
    %%
    ?line "Hello" = fmt("~-5.5s", [Hello]),
    ?line "Hello" = fmt("~*.5s", [-5,Hello]),
    ?line "Hello" = fmt("~-5.*s", [5,Hello]),
    ?line "Hello" = fmt("~*.*s", [-5,5,Hello]),
    %%
    ?line "Hell " = fmt("~-5.4s", [Hello]),
    ?line "Hell " = fmt("~*.4s", [-5,Hello]),
    ?line "Hell " = fmt("~-5.*s", [4,Hello]),
    ?line "Hell " = fmt("~*.*s", [-5,4,Hello]),
    %%
    ?line "Hell" = fmt("~-4.4s", [Hello]),
    ?line "Hell" = fmt("~*.4s", [-4,Hello]),
    ?line "Hell" = fmt("~-4.*s", [4,Hello]),
    ?line "Hell" = fmt("~*.*s", [-4,4,Hello]),
    %%
    ?line "Hel " = fmt("~-4.3s", [Hello]),
    ?line "Hel " = fmt("~*.3s", [-4,Hello]),
    ?line "Hel " = fmt("~-4.*s", [3,Hello]),
    ?line "Hel " = fmt("~*.*s", [-4,3,Hello]),
    ok.

io_lib_fread_literal(doc) ->
    "OTP-9439 io_lib:fread bug for literate at end";
io_lib_fread_literal(Suite) when is_list(Suite) ->
    ?line {more,"~d",0,""} = io_lib:fread("~d", ""),
    ?line {error,{fread,integer}} = io_lib:fread("~d", " "),
    ?line {more,"~d",1,""} = io_lib:fread(" ~d", " "),
    ?line {ok,[17],"X"} = io_lib:fread(" ~d", " 17X"),
    %%
    ?line {more,"d",0,""} = io_lib:fread("d", ""),
    ?line {error,{fread,input}} = io_lib:fread("d", " "),
    ?line {more,"d",1,""} = io_lib:fread(" d", " "),
    ?line {ok,[],"X"} = io_lib:fread(" d", " dX"),
    %%
    ?line {done,eof,_} = io_lib:fread([], eof, "~d"),
    ?line {done,eof,_} = io_lib:fread([], eof, " ~d"),
    ?line {more,C1} = io_lib:fread([], " \n", " ~d"),
    ?line {done,{error,{fread,input}},_} = io_lib:fread(C1, eof, " ~d"),
    ?line {done,{ok,[18]},""} = io_lib:fread(C1, "18\n", " ~d"),
    %%
    ?line {done,eof,_} = io_lib:fread([], eof, "d"),
    ?line {done,eof,_} = io_lib:fread([], eof, " d"),
    ?line {more,C2} = io_lib:fread([], " \n", " d"),
    ?line {done,{error,{fread,input}},_} = io_lib:fread(C2, eof, " d"),
    ?line {done,{ok,[]},[]} = io_lib:fread(C2, "d\n", " d"),
    ok.


printable_range(doc) ->
    "Check that the printable range set by the user actually works";
printable_range(Suite) when is_list(Suite) ->
    Pa = filename:dirname(code:which(?MODULE)),
    {ok, UNode} = test_server:start_node(printable_range_unicode, slave,
					 [{args, " +pc unicode -pa " ++ Pa}]),
    {ok, LNode} = test_server:start_node(printable_range_latin1, slave,
					 [{args, " +pc latin1 -pa " ++ Pa}]),
    {ok, DNode} = test_server:start_node(printable_range_default, slave,
					 [{args, " -pa " ++ Pa}]),
    unicode = rpc:call(UNode,io,printable_range,[]),
    latin1 = rpc:call(LNode,io,printable_range,[]),
    latin1 = rpc:call(DNode,io,printable_range,[]),
    test_server:stop_node(UNode),
    test_server:stop_node(LNode),
    {ok, UNode} = test_server:start_node(printable_range_unicode, slave,
					 [{args, " +pcunicode -pa " ++ Pa}]),
    {ok, LNode} = test_server:start_node(printable_range_latin1, slave,
					 [{args, " +pclatin1 -pa " ++ Pa}]),
    unicode = rpc:call(UNode,io,printable_range,[]),
    latin1 = rpc:call(LNode,io,printable_range,[]),
    PrettyOptions = [{column,1},
		     {line_length,109},
		     {depth,30},
		     {max_chars,60},
		     {record_print_fun,
		      fun(_,_) -> no end},
		     {encoding,unicode}],
    PrintableControls = "\t\v\b\f\e\r\n",

    1025 = print_max(UNode, [{hello, [1024,1025]},
			     PrettyOptions]),
    125 = print_max(LNode,  [{hello, [1024,1025]},
			     PrettyOptions]),
    125 = print_max(DNode,  [{hello, [1024,1025]},
			     PrettyOptions]),
    1025 = print_max(UNode, [{hello, <<1024/utf8,1025/utf8>>},
			     PrettyOptions]),
    125 = print_max(LNode,  [{hello, <<1024/utf8,1025/utf8>>},
			     PrettyOptions]),
    125 = print_max(DNode,  [{hello, <<1024/utf8,1025/utf8>>},
			     PrettyOptions]),
    $v = print_max(UNode, [PrintableControls,PrettyOptions]),
    $v = print_max(LNode, [PrintableControls,PrettyOptions]),
    $v = print_max(DNode, [PrintableControls,PrettyOptions]),
    16#10FFFF = print_max(UNode,
			  [<<16#10FFFF/utf8,"\t\v\b\f\e\r\n">>,
			   PrettyOptions]),
    $> = print_max(LNode,
		   [<<16#10FFFF/utf8,"\t\v\b\f\e\r\n">>,
		    PrettyOptions]),
    $> = print_max(DNode,
		   [<<16#10FFFF/utf8,"\t\v\b\f\e\r\n">>,
		    PrettyOptions]),
    
    1025 = format_max(UNode, ["~tp", [{hello, [1024,1025]}]]),
    125 = format_max(LNode,  ["~tp", [{hello, [1024,1025]}]]),
    125 = format_max(DNode,  ["~tp", [{hello, [1024,1025]}]]),
    1025 = format_max(UNode, ["~tp", [{hello, <<1024/utf8,1025/utf8>>}]]),
    125 = format_max(LNode,  ["~tp", [{hello, <<1024/utf8,1025/utf8>>}]]),
    125 = format_max(DNode,  ["~tp", [{hello, <<1024/utf8,1025/utf8>>}]]),

    $\e = format_max(UNode, ["~ts", [PrintableControls]]),
    $\e = format_max(LNode, ["~ts", [PrintableControls]]),
    $\e = format_max(DNode, ["~ts", [PrintableControls]]),

    test_server:stop_node(UNode),
    test_server:stop_node(LNode),
    test_server:stop_node(DNode),
    ok.

print_max(Node, Args) ->
    rpc_call_max(Node, io_lib_pretty, print, Args).

format_max(Node, Args) ->
    rpc_call_max(Node, io_lib, format, Args).

rpc_call_max(Node, M, F, Args) ->
    lists:max(lists:flatten(rpc:call(Node, M, F, Args))).

%% Make sure that a bad specification for a printable range is rejected.
bad_printable_range(Config) when is_list(Config) ->
    Cmd = lists:concat([lib:progname()," +pcunnnnnicode -run erlang halt"]),
    P = open_port({spawn, Cmd}, [stderr_to_stdout, {line, 200}]),
    ok = receive
	     {P, {data, {eol , "bad range of printable characters" ++ _}}} ->
		 ok;
	     Other ->
		 Other
	 after 1000 ->
		 timeout
	 end,
    catch port_close(P),
    flush_from_port(P),
    ok.

flush_from_port(P) ->
    receive {P, _} ->
	    flush_from_port(P)
    after 0 ->
	    ok
    end.

io_lib_print_binary_depth_one(doc) ->
    "Test binaries printed with a depth of one behave correctly";
io_lib_print_binary_depth_one(Suite) when is_list(Suite) ->
    ?line "<<>>" = fmt("~W", [<<>>, 1]),
    ?line "<<>>" = fmt("~P", [<<>>, 1]),
    ?line "<<...>>" = fmt("~W", [<<1>>, 1]),
    ?line "<<...>>" = fmt("~P", [<<1>>, 1]),
    ?line "<<...>>" = fmt("~W", [<<1:7>>, 1]),
    ?line "<<...>>" = fmt("~P", [<<1:7>>, 1]),
    ok.

otp_10302(doc) ->
    "OTP-10302. Unicode";
otp_10302(Suite) when is_list(Suite) ->
    Pa = filename:dirname(code:which(?MODULE)),
    {ok, UNode} = test_server:start_node(printable_range_unicode, slave,
					 [{args, " +pc unicode -pa " ++ Pa}]),
    {ok, LNode} = test_server:start_node(printable_range_latin1, slave,
					 [{args, " +pc latin1 -pa " ++ Pa}]),
    "\"\x{400}\"" = rpc:call(UNode,?MODULE,pretty,["\x{400}", -1]),
    "<<\"\x{400}\"/utf8>>" = rpc:call(UNode,?MODULE,pretty,
				      [<<"\x{400}"/utf8>>, -1]),

    "<<\"\x{400}foo\"/utf8>>" = rpc:call(UNode,?MODULE,pretty,
					 [<<"\x{400}foo"/utf8>>, 2]),
    "[1024]" = rpc:call(LNode,?MODULE,pretty,["\x{400}", -1]),
    "<<208,128>>" = rpc:call(LNode,?MODULE,pretty,[<<"\x{400}"/utf8>>, -1]),

    "<<208,...>>" = rpc:call(LNode,?MODULE,pretty,[<<"\x{400}foo"/utf8>>, 2]),
    test_server:stop_node(UNode),
    test_server:stop_node(LNode),

    "<<\"äppl\"/utf8>>" = pretty(<<"äppl"/utf8>>, 2),
    "<<\"äppl\"/utf8...>>" = pretty(<<"äpple"/utf8>>, 2),
    "<<\"apel\">>" = pretty(<<"apel">>, 2),
    "<<\"apel\"...>>" = pretty(<<"apelsin">>, 2),
    "<<\"äppl\">>" = fmt("~tp", [<<"äppl">>]),
    "<<\"äppl\"...>>" = fmt("~tP", [<<"äpple">>, 2]),
    "<<0,0,0,0,0,0,1,0>>" = fmt("~p", [<<256:64/unsigned-integer>>]),
    "<<0,0,0,0,0,0,1,0>>" = fmt("~tp", [<<256:64/unsigned-integer>>]),

    Chars = lists:seq(0, 512), % just a few...
    [] = [C || C <- Chars, S <- io_lib:write_char_as_latin1(C),
               not is_latin1(S)],
    L1 = [S || C <- Chars, S <- io_lib:write_char(C),
               not is_latin1(S)],
    L1 = lists:seq(256, 512),

    [] = [C || C <- Chars, S <- io_lib:write_string_as_latin1([C]),
               not is_latin1(S)],
    L2 = [S || C <- Chars, S <- io_lib:write_string([C]),
               not is_latin1(S)],
    L2 = lists:seq(256, 512),

    ok.

pretty(Term, Depth) when is_integer(Depth) ->
    Opts = [{column, 1}, {line_length, 20},
            {depth, Depth}, {max_chars, 60},
            {encoding, unicode}],
    pretty(Term, Opts);
pretty(Term, Opts) when is_list(Opts) ->
    R = io_lib_pretty:print(Term, Opts),
    lists:flatten(io_lib:format("~ts", [R])).

is_latin1(S) ->
    S >= 0 andalso S =< 255.

otp_10836(doc) ->
    "OTP-10836. ~ts extended to latin1";
otp_10836(Suite) when is_list(Suite) ->
    S = io_lib:format("~ts", [[<<"äpple"/utf8>>, <<"äpple">>]]),
    "äppleäpple" = lists:flatten(S),
    ok.

otp_10755(doc) ->
    "OTP-10755. The 'l' modifier";
otp_10755(Suite) when is_list(Suite) ->
    S = "string",
    "\"string\"" = fmt("~p", [S]),
    "[115,116,114,105,110,103]" = fmt("~lp", [S]),
    "\"string\"" = fmt("~P", [S, 2]),
    "[115|...]" = fmt("~lP", [S, 2]),
    {'EXIT',{badarg,_}} = (catch fmt("~ltp", [S])),
    {'EXIT',{badarg,_}} = (catch fmt("~tlp", [S])),
    {'EXIT',{badarg,_}} = (catch fmt("~ltP", [S])),
    {'EXIT',{badarg,_}} = (catch fmt("~tlP", [S])),
    Text =
        "-module(l_mod).\n"
        "-export([t/0]).\n"
        "t() ->\n"
        "    S = \"string\",\n"
        "    io:format(\"~ltp\", [S]),\n"
        "    io:format(\"~tlp\", [S]),\n"
        "    io:format(\"~ltP\", [S, 1]),\n"
        "    io:format(\"~tlP\", [S, 1]).\n",
    {ok,l_mod,[{_File,Ws}]} = compile_file("l_mod.erl", Text, Suite),
    ["format string invalid (invalid control ~lt)",
     "format string invalid (invalid control ~tl)",
     "format string invalid (invalid control ~lt)",
     "format string invalid (invalid control ~tl)"] =
        [lists:flatten(M:format_error(E)) || {_L,M,E} <- Ws],
    ok.

compile_file(File, Text, Config) ->
    PrivDir = ?privdir(Config),
    Fname = filename:join(PrivDir, File),
    ok = file:write_file(Fname, Text),
    try compile:file(Fname, [return])
    after ok %file:delete(Fname)
    end.

io_lib_width_too_small(_Config) ->
    "**" = lists:flatten(io_lib:format("~2.3w", [3.14])),
    "**" = lists:flatten(io_lib:format("~2.5w", [3.14])),
    ok.

%% Test that the time for a huge message queue is not
%% significantly slower than with an empty message queue.
io_with_huge_message_queue(Config) when is_list(Config) ->
    case {test_server:is_native(gen),test_server:is_cover()} of
	{true,_} ->
	    {skip,
	     "gen is native - huge message queue optimization "
	     "is not implemented"};
	{_,true} ->
	    {skip,"Running under cover"};
	{false,false} ->
	    do_io_with_huge_message_queue(Config)
    end.

do_io_with_huge_message_queue(Config) ->
    PrivDir = ?privdir(Config),
    File = filename:join(PrivDir, "slask"),
    {ok, F1} = file:open(File, [write]),
    Test = fun(Times) ->
		   {Time,ok} = timer:tc(fun() -> writes(Times, F1) end),
		   Time
	   end,
    {Times,EmptyTime} = calibrate(100, Test),

    [self() ! {msg,N} || N <- lists:seq(1, 500000)],
    erlang:garbage_collect(),
    FullTime = Test(Times),
    file:close(F1),
    file:delete(File),
    io:format("Number of writes: ~p", [Times]),
    io:format("Time for empty message queue: ~p", [EmptyTime]),
    io:format("Time for huge message queue: ~p", [FullTime]),

    case (FullTime+1) / (EmptyTime+1) of
	Q when Q < 10 ->
	    ok;
	Q ->
	    io:format("Q = ~p", [Q]),
	    ?t:fail()
    end,
    ok.

%% Make sure that the time is not too short. That could cause the
%% test case to fail.
calibrate(N, Test) when N =< 100000 ->
    case Test(N) of
	Time when Time < 50000 ->
	    calibrate(10*N, Test);
	Time ->
	    {N,Time}
    end;
calibrate(N, _) ->
    N.

writes(0, _) -> ok;
writes(N, F1) ->
    file:write(F1, "hello\n"),
    writes(N - 1, F1).

format_string(_Config) ->
    %% All but padding is tested by fmt/2.
    "xxxxxxsssx" = fmt("~10.4.xs", ["sss"]),
    "xxxxxxsssx" = fmt("~10.4.*s", [$x, "sss"]),
    ok.

maps(_Config) ->
    %% Note that order in which a map is printed is arbitrary.  In
    %% practice, small maps (non-HAMT) are printed in key order, but
    %% the breakpoint for creating big maps (HAMT) is lower in the
    %% debug-compiled run-time system than in the optimized run-time
    %% system.
    %%
    %% Therefore, play it completely safe by not assuming any order
    %% in a map with more than one element.

    "#{}" = fmt("~w", [#{}]),
    "#{a=>b}" = fmt("~w", [#{a=>b}]),
    re_fmt(<<"#\\{(a=>b|c=>d),[.][.][.]=>[.][.][.]\\}">>,
	     "~W", [#{a=>b,c=>d},2]),
    re_fmt(<<"#\\{(a=>b|c=>d|e=>f),[.][.][.]=>[.][.][.],[.][.][.]\\}">>,
	   "~W", [#{a=>b,c=>d,e=>f},2]),

    "#{}" = fmt("~p", [#{}]),
    "#{a => b}" = fmt("~p", [#{a=>b}]),
    "#{...}" = fmt("~P", [#{a=>b},1]),
    re_fmt(<<"#\\{(a => b|c => d),[.][.][.]\\}">>,
	   "~P", [#{a=>b,c=>d},2]),
    re_fmt(<<"#\\{(a => b|c => d|e => f),[.][.][.]\\}">>,
	   "~P", [#{a=>b,c=>d,e=>f},2]),

    List = [{I,I*I} || I <- lists:seq(1, 20)],
    Map = maps:from_list(List),

    "#{...}" = fmt("~P", [Map,1]),

    %% Print a map and parse it back to a map.
    S = fmt("~p\n", [Map]),
    io:format("~p\n", [S]),
    Map = parse_map(S),

    %% Smoke test of a map as key.
    MapAsKey = #{Map => "value"},
    io:format("~s\n", [fmt("~p", [MapAsKey])]),
    ok.

re_fmt(Pattern, Format, Args) ->
    S = list_to_binary(fmt(Format, Args)),
    case re:run(S, Pattern, [{capture,none}]) of
	nomatch ->
	    io:format("Pattern: ~s", [Pattern]),
	    io:format("Result:  ~s", [S]),
	    ?t:fail();
	match ->
	    ok
    end.

%% Parse a map consisting of integer keys and values.
parse_map(S0) ->
    S1 = parse_expect(S0, "#{"),
    {M,S2} = parse_map_1(S1),
    S = parse_expect(S2, "}"),
    S = "",
    M.

parse_map_1(S0) ->
    {Key,S1} = parse_number(S0),
    S2 = parse_expect(S1, "=>"),
    {Val,S3} = parse_number(S2),
    case S3 of
	","++S4 ->
	    S5 = parse_skip_ws(S4),
	    {Map,S} = parse_map_1(S5),
	    {Map#{Key=>Val},S};
	S ->
	    {#{Key=>Val},S}
    end.

parse_number(S) ->
    parse_number(S, none).

parse_number([C|S], Acc0) when $0 =< C, C =< $9 ->
    Acc = case Acc0 of
	      none -> 0;
	      _ when is_integer(Acc0) -> Acc0
	  end,
    parse_number(S, Acc*10+C-$0);
parse_number(S, Acc) ->
    {Acc,parse_skip_ws(S)}.

parse_expect([H|T1], [H|T2]) ->
    parse_expect(T1, T2);
parse_expect(S, []) ->
    parse_skip_ws(S).

parse_skip_ws([C|S]) when C =< $\s ->
    parse_skip_ws(S);
parse_skip_ws(S) ->
    S.

%% Cover the last uncovered lines for completeness.
coverage(_Config) ->
    S1 = io_lib_pretty:print({a,term}, fun(_, _) -> no end),
    io:format("~s\n", [S1]),

    %% The tuple of arity three will be ignored.
    S2 = io_lib_pretty:print(lists:seq(1, 100), [{depth,1,1}]),
    io:format("~s\n", [S2]),

    ok.
