%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2015. All Rights Reserved.
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

-module(epp_SUITE).
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

-export([rec_1/1, include_local/1, predef_mac/1,
	 upcase_mac_1/1, upcase_mac_2/1,
	 variable_1/1, otp_4870/1, otp_4871/1, otp_5362/1,
         pmod/1, not_circular/1, skip_header/1, otp_6277/1, otp_7702/1,
         otp_8130/1, overload_mac/1, otp_8388/1, otp_8470/1, otp_8503/1,
         otp_8562/1, otp_8665/1, otp_8911/1, otp_10302/1, otp_10820/1,
         otp_11728/1, encoding/1]).

-export([epp_parse_erl_form/2]).

%%
%% Define to run outside of test server
%%
%-define(STANDALONE,1).

-ifdef(STANDALONE).
-compile(export_all).
-define(line, put(line, ?LINE), ).
-define(config(A,B),config(A,B)).
%% -define(t, test_server).
-define(t, io).
config(priv_dir, _) ->
    filename:absname("./epp_SUITE_priv");
config(data_dir, _) ->
    filename:absname("./epp_SUITE_data").
-else.
-include_lib("test_server/include/test_server.hrl").
-export([init_per_testcase/2, end_per_testcase/2]).

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(_, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
end_per_testcase(_, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.
-endif.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [rec_1, {group, upcase_mac}, include_local, predef_mac,
     {group, variable}, otp_4870, otp_4871, otp_5362, pmod,
     not_circular, skip_header, otp_6277, otp_7702, otp_8130,
     overload_mac, otp_8388, otp_8470, otp_8503, otp_8562,
     otp_8665, otp_8911, otp_10302, otp_10820, otp_11728,
     encoding].

groups() -> 
    [{upcase_mac, [], [upcase_mac_1, upcase_mac_2]},
     {variable, [], [variable_1]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

rec_1(doc) ->
    ["Recursive macros hang or crash epp (OTP-1398)."];
rec_1(suite) ->
    [];
rec_1(Config) when is_list(Config) ->
    ?line File = filename:join(?config(data_dir, Config), "mac.erl"),
    ?line {ok, List} = epp_parse_file(File, [], []),
    %% we should encounter errors
    ?line {value, _} = lists:keysearch(error, 1, List),
    ?line check_errors(List),
    ok.

include_local(doc) ->
    [];
include_local(suite) ->
    [];
include_local(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line File = filename:join(DataDir, "include_local.erl"),
    FooHrl = filename:join([DataDir,"include","foo.hrl"]),
    BarHrl = filename:join([DataDir,"include","bar.hrl"]),
    %% include_local.erl includes include/foo.hrl which
    %% includes bar.hrl (also in include/) without requiring
    %% any additional include path, and overriding any file
    %% of the same name that the path points to
    ?line {ok, List} = epp:parse_file(File, [DataDir], []),
    ?line {value, {attribute,_,a,{true,true}}} =
	lists:keysearch(a,3,List),
    [{File,1},{FooHrl,1},{BarHrl,1},{FooHrl,5},{File,5}] =
        [ FileLine || {attribute,_,file,FileLine} <- List ],
    ok.

%%% Here is a little reimplementation of epp:parse_file, which times out
%%% after 4 seconds if the epp server doesn't respond. If we use the
%%% regular epp:parse_file, the test case will time out, and then epp
%%% server will go on growing until we dump core.
epp_parse_file(File, Inc, Predef) ->
    List = do_epp_parse_file(fun() ->
				     epp:open(File, Inc, Predef)
			     end),
    List = do_epp_parse_file(fun() ->
				     Opts = [{name, File},
					     {includes, Inc},
					     {macros, Predef}],
				     epp:open(Opts)
			     end),
    {ok, List}.

do_epp_parse_file(Open) ->
    {ok, Epp} = Open(),
    List = collect_epp_forms(Epp),
    epp:close(Epp),
    List.

collect_epp_forms(Epp) ->
    Result = epp_parse_erl_form(Epp),
    case Result of
	{error, _Error} ->
	    [Result | collect_epp_forms(Epp)];
	{ok, Form} ->
	    [Form | collect_epp_forms(Epp)];
	{eof, _} ->
	    [Result]
    end.

epp_parse_erl_form(Epp) ->
    P = spawn(?MODULE, epp_parse_erl_form, [Epp, self()]),
    receive
	{P, Result} ->
	    Result
    after 4000 ->
	    exit(Epp, kill),
	    exit(P, kill),
	    timeout
    end.

epp_parse_erl_form(Epp, Parent) ->
    Parent ! {self(), epp:parse_erl_form(Epp)}.

check_errors([]) ->
    ok;
check_errors([{error, Info} | Rest]) ->
    ?line {Line, Mod, Desc} = Info,
    ?line case Line of
              I when is_integer(I) -> ok;
              {L,C} when is_integer(L), is_integer(C), C >= 1 -> ok
          end,
    ?line Str = lists:flatten(Mod:format_error(Desc)),
    ?line [Str] = io_lib:format("~s", [Str]),
    check_errors(Rest);
check_errors([_ | Rest]) ->
    check_errors(Rest).


upcase_mac_1(doc) ->
    [];
upcase_mac_1(suite) ->
    [];
upcase_mac_1(Config) when is_list(Config) ->
    ?line File = filename:join(?config(data_dir, Config), "mac2.erl"),
    ?line {ok, List} = epp:parse_file(File, [], []),
    ?line [_, {attribute, _, plupp, Tuple} | _] = List,
    ?line Tuple = {1, 1, 3, 3},
    ok.

upcase_mac_2(doc) ->
    [];
upcase_mac_2(suite) ->
    [];
upcase_mac_2(Config) when is_list(Config) ->
    ?line File = filename:join(?config(data_dir, Config), "mac2.erl"),
    ?line {ok, List} = epp:parse_file(File, [], [{p, 5}, {'P', 6}]),
    ?line [_, {attribute, _, plupp, Tuple} | _] = List,
    ?line Tuple = {5, 5, 6, 6},
    ok.

predef_mac(doc) ->
    [];
predef_mac(suite) ->
    [];
predef_mac(Config) when is_list(Config) ->
    ?line File = filename:join(?config(data_dir, Config), "mac3.erl"),
    ?line {ok, List} = epp:parse_file(File, [], []),
    ?line [_,
	   {attribute, Anno, l, Line1},
	   {attribute, _, f, File},
	   {attribute, _, machine1, _},
	   {attribute, _, module, mac3},
	   {attribute, _, m, mac3},
	   {attribute, _, ms, "mac3"},
	   {attribute, _, machine2, _}
	   | _] = List,
    Line1 = erl_anno:line(Anno),
    ok.

variable_1(doc) ->
    [];
variable_1(suite) ->
    [];
variable_1(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line File = filename:join(DataDir, "variable_1.erl"),
    ?line true = os:putenv("VAR", DataDir),
    %% variable_1.erl includes variable_1_include.hrl and
    %% variable_1_include_dir.hrl.
    ?line {ok, List} = epp:parse_file(File, [], []),
    ?line {value, {attribute,_,a,{value1,value2}}} =
	lists:keysearch(a,3,List),
    ok.

otp_4870(doc) ->
    ["undef without module declaration"];
otp_4870(suite) ->
    [];
otp_4870(Config) when is_list(Config) ->
    Ts = [{otp_4870,
           <<"-undef(foo).
           ">>,
           []}],
    ?line [] = check(Config, Ts),
    ok.

otp_4871(doc) ->
    ["crashing erl_scan"];
otp_4871(suite) ->
    [];
otp_4871(Config) when is_list(Config) ->
    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "otp_4871.erl"),
    ?line ok = file:write_file(File, "-module(otp_4871)."),
    %% Testing crash in erl_scan. Unfortunately there currently is
    %% no known way to crash erl_scan so it is emulated by killing the
    %% file io server. This assumes lots of things about how
    %% the processes are started and how monitors are set up,
    %% so there are some sanity checks before killing.
    ?line {ok,Epp} = epp:open(File, []),
    timer:sleep(1),
    ?line true = current_module(Epp, epp),
    ?line {monitored_by,[Io]} = process_info(Epp, monitored_by),
    ?line true = current_module(Io, file_io_server),
    ?line exit(Io, emulate_crash),
    timer:sleep(1),
    ?line {error,{_Line,epp,cannot_parse}} = otp_4871_parse_file(Epp),
    ?line epp:close(Epp),
    ok.

current_module(Pid, Mod) ->
    case process_info(Pid, current_function) of
        {current_function, undefined} ->
            true = test_server:is_native(Mod);
        {current_function, {Mod, _, _}} ->
            true
    end.

otp_4871_parse_file(Epp) ->
    case epp:parse_erl_form(Epp) of
	{ok,_} -> otp_4871_parse_file(Epp);
	Other -> Other
    end.

otp_5362(doc) ->
    ["OTP-5362. The -file attribute is recognized."];
otp_5362(suite) ->
    [];
otp_5362(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),

    Copts = [return, strong_validation,{i,Dir}],

    File_Incl = filename:join(Dir, "incl_5362.erl"),
    File_Incl2 = filename:join(Dir, "incl2_5362.erl"),
    File_Incl3 = filename:join(Dir, "incl3_5362.erl"),
    Incl = <<"-module(incl_5362).

              -include(\"incl2_5362.erl\").

              -include_lib(\"incl3_5362.erl\").

              hi(There) -> % line 7
                   a.
           ">>,
    Incl2 = <<"-file(\"some.file\", 100).

               foo(Bar) -> % line 102
                   foo.
            ">>,
    Incl3 = <<"glurk(Foo) -> % line 1
                  bar.
            ">>,
    ?line ok = file:write_file(File_Incl, Incl),
    ?line ok = file:write_file(File_Incl2, Incl2),
    ?line ok = file:write_file(File_Incl3, Incl3),

    ?line {ok, incl_5362, InclWarnings} = compile:file(File_Incl, Copts),
    ?line true = message_compare(
                   [{File_Incl3,[{{1,1},erl_lint,{unused_function,{glurk,1}}},
                                 {{1,7},erl_lint,{unused_var,'Foo'}}]},
                    {File_Incl,[{{7,15},erl_lint,{unused_function,{hi,1}}},
                                {{7,18},erl_lint,{unused_var,'There'}}]},
                    {"some.file",[{{102,16},erl_lint,{unused_function,{foo,1}}},
                                  {{102,20},erl_lint,{unused_var,'Bar'}}]}],
                   lists:usort(InclWarnings)),

    file:delete(File_Incl),
    file:delete(File_Incl2),
    file:delete(File_Incl3),

    %% A -file attribute referring back to the including file.
    File_Back = filename:join(Dir, "back_5362.erl"),
    File_Back_hrl = filename:join(Dir, "back_5362.hrl"),
    Back = <<"-module(back_5362).

              -compile(export_all).

              -file(?FILE, 1).
              -include(\"back_5362.hrl\").

              foo(V) -> % line 4
                  bar.
              ">>,
    Back_hrl = [<<"
                  -file(\"">>,File_Back,<<"\", 2).
                 ">>],

    ?line ok = file:write_file(File_Back, Back),
    ?line ok = file:write_file(File_Back_hrl, list_to_binary(Back_hrl)),

    ?line {ok, back_5362, BackWarnings} = compile:file(File_Back, Copts),
    ?line true = message_compare(
                   [{File_Back,[{{4,19},erl_lint,{unused_var,'V'}}]}],
                   BackWarnings),
    file:delete(File_Back),
    file:delete(File_Back_hrl),

    %% Set filename but keep line.
    File_Change = filename:join(Dir, "change_5362.erl"),
    Change = [<<"-module(change_5362).

                -file(?FILE, 100).

                -compile(export_all).

                -file(\"other.file\", ?LINE). % like an included file...
                foo(A) -> % line 105
                    bar.

                -file(\"">>,File_Change,<<"\", 1000).

                bar(B) -> % line 1002
                    foo.
              ">>],

    ?line ok = file:write_file(File_Change, list_to_binary(Change)),

    ?line {ok, change_5362, ChangeWarnings} =
        compile:file(File_Change, Copts),
    ?line true = message_compare(
                   [{File_Change,[{{1002,21},erl_lint,{unused_var,'B'}}]},
                    {"other.file",[{{105,21},erl_lint,{unused_var,'A'}}]}],
                   lists:usort(ChangeWarnings)),

    file:delete(File_Change),

    %% -file attribute ending with a blank (not a newline).
    File_Blank = filename:join(Dir, "blank_5362.erl"),

    Blank = <<"-module(blank_5362).

               -compile(export_all).

               -
               file(?FILE, 18). q(Q) -> foo. % line 18

               a(A) -> % line 20
                   1.

               -file(?FILE, 42).

               b(B) -> % line 44
                   2.

               -file(?FILE, ?LINE). c(C) -> % line 47
                   3.
            ">>,
    ?line ok = file:write_file(File_Blank, Blank),
    ?line {ok, blank_5362, BlankWarnings} = compile:file(File_Blank, Copts),
    ?line true = message_compare(
             [{File_Blank,[{{18,3},erl_lint,{unused_var,'Q'}},
                           {{20,18},erl_lint,{unused_var,'A'}},
                           {{44,18},erl_lint,{unused_var,'B'}},
                           {{47,3},erl_lint,{unused_var,'C'}}]}],
              lists:usort(BlankWarnings)),
    file:delete(File_Blank),

    %% __FILE__ is set by inclusion and by -file attribute
    FILE_incl = filename:join(Dir, "file_5362.erl"),
    FILE_incl1 = filename:join(Dir, "file_incl_5362.erl"),
    FILE = <<"-module(file_5362).

              -export([ff/0, ii/0]).

              -include(\"file_incl_5362.erl\").

              -file(\"other_file\", 100).

              ff() ->
                  ?FILE.">>,
    FILE1 = <<"ii() -> ?FILE.
              ">>,
    FILE_Mod = file_5362,
    ?line ok = file:write_file(FILE_incl, FILE),
    ?line ok = file:write_file(FILE_incl1, FILE1),
    FILE_Copts = [return, {i,Dir},{outdir,Dir}],
    ?line {ok, file_5362, []} = compile:file(FILE_incl, FILE_Copts),
    AbsFile = filename:rootname(FILE_incl, ".erl"),
    ?line {module, FILE_Mod} = code:load_abs(AbsFile, FILE_Mod),
    ?line II = FILE_Mod:ii(),
    ?line "file_incl_5362.erl" = filename:basename(II),
    ?line FF = FILE_Mod:ff(),
    ?line "other_file" = filename:basename(FF),
    code:purge(file_5362),

    file:delete(FILE_incl),
    file:delete(FILE_incl1),

    ok.

pmod(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line Pmod = filename:join(DataDir, "pmod.erl"),
    ?line case epp:parse_file([Pmod], [], []) of
	      {ok,Forms} ->
		  %% ?line io:format("~p\n", [Forms]),
		  ?line [] = [F || {error,_}=F <- Forms],
		  ok
	  end,
    ok.

not_circular(Config) when is_list(Config) ->
    %% Used to generate a compilation error, wrongly saying that it
    %% was a circular definition.

    Ts = [{circular_1,
           <<"-define(S(S), ??S).\n"
             "t() -> \"string\" = ?S(string), ok.\n">>,
           ok}],
    ?line [] = run(Config, Ts),
    ok.

skip_header(doc) ->
    ["Skip some bytes in the beginning of the file."];
skip_header(suite) ->
    [];
skip_header(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line File = filename:join([PrivDir, "epp_test_skip_header.erl"]),
    ?line ok = file:write_file(File,
			       <<"some bytes
                                  in the beginning of the file
                                  that should be skipped
                                  -module(epp_test_skip_header).
                                  -export([main/1]).

                                  main(_) -> ?MODULE.

                                  ">>),
    ?line {ok, Fd} = file:open(File, [read]),
    ?line io:get_line(Fd, ''),
    ?line io:get_line(Fd, ''),
    ?line io:get_line(Fd, ''),
    ?line {ok, Epp} = epp:open(list_to_atom(File), Fd, 4, [], []),

    ?line Forms = epp:parse_file(Epp),
    ?line [] = [Reason || {error, Reason} <- Forms],
    ?line ok = epp:close(Epp),
    ?line ok = file:close(Fd),

    ok.

otp_6277(doc) ->
    ["?MODULE before module declaration."];
otp_6277(suite) ->
    [];
otp_6277(Config) when is_list(Config) ->
    Ts = [{otp_6277,
           <<"-undef(ASSERT).
              -define(ASSERT, ?MODULE).

              ?ASSERT().">>,
           [{error,{{4,16},epp,{undefined,'MODULE', none}}}]}],
    ?line [] = check(Config, Ts),
    ok.

otp_7702(doc) ->
    ["OTP-7702. Wrong line number in stringifying macro expansion."];
otp_7702(suite) ->
    [];
otp_7702(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, "file_7702.erl"),
    Contents = <<"-module(file_7702).

                  -export([t/0]).

                  -define(RECEIVE(Msg,Body),
                       receive
                           Msg -> Body;
                           M ->
                exit({unexpected_message,M,on_line,?LINE,was_expecting,??Msg})
                       after 10000 ->
                            exit({timeout,on_line,?LINE,was_expecting,??Msg})
                       end).
                   t() ->
                       ?RECEIVE(foo, bar).">>,
    ?line ok = file:write_file(File, Contents),
    ?line {ok, file_7702, []} =
        compile:file(File, [debug_info,return,{outdir,Dir}]),

    BeamFile = filename:join(Dir, "file_7702.beam"),
    {ok, AC} = beam_lib:chunks(BeamFile, [abstract_code]),

    {file_7702,[{abstract_code,{_,Forms}}]} = AC,
    Forms2 = unopaque_forms(Forms),
    ?line
        [{attribute,1,file,_},
         _,
         _,
         {function,_,t,0,
          [{clause,_,[],[],
            [{'receive',14,
              [_,
               {clause,14,
                [{var,14,'M'}],
                [],
                [{_,_,_,
                  [{tuple,14,
                    [{atom,14,unexpected_message},
                     {var,14,'M'},
                     {atom,14,on_line},
                     {integer,14,14},
                     {atom,14,was_expecting},
                     {string,14,"foo"}]}]}]}],
              {integer,14,10000},
              [{call,14,
                {atom,14,exit},
                [{tuple,14,
                  [{atom,14,timeout},
                   {atom,14,on_line},
                   {integer,14,14},
                   {atom,14,was_expecting},
                   {string,14,"foo"}]}]}]}]}]},
         {eof,14}] = Forms2,

    file:delete(File),
    file:delete(BeamFile),

    ok.

otp_8130(doc) ->
    ["OTP-8130. Misc tests."];
otp_8130(suite) ->
    [];
otp_8130(Config) when is_list(Config) ->
    true = os:putenv("epp_inc1", "stdlib"),
    Ts = [{otp_8130_1,
           <<"-define(M(A), ??A). "
             "t() ->  "
             "   L = \"{ 34 , \\\"1\\\\x{AAA}\\\" , \\\"34\\\" , X . a , $\\\\x{AAA} }\", "
             "   R = ?M({34,\"1\\x{aaa}\",\"34\",X.a,$\\x{aaa}}),"
             "   Lt = erl_scan:string(L, 1, [unicode]),"
             "   Rt = erl_scan:string(R, 1, [unicode]),"
             "   Lt = Rt, ok. ">>,
          ok},

          {otp_8130_2,
           <<"-define(M(A), ??B). "
             "t() -> B = 18, 18 = ?M(34), ok. ">>,
           ok},

          {otp_8130_2a,
           <<"-define(m(A), ??B). "
             "t() -> B = 18, 18 = ?m(34), ok. ">>,
           ok},

          {otp_8130_3,
           <<"-define(M1(A, B), {A,B}).\n"
             "t0() -> 1.\n"
             "t() ->\n"
             "   {2,7} =\n"
             "      ?M1(begin 1 = fun() -> 1 end(),\n" % Bug -R13B01
             "                2 end,\n"
             "          7),\n"
             "   {2,7} =\n"
             "      ?M1(begin 1 = fun t0/0(),\n"
             "                2 end,\n"
             "          7),\n"
             "   {2,7} =\n"
             "      ?M1(begin 2 = byte_size(<<\"34\">>),\n"
             "                2 end,\n"
             "          7),\n"
             "   R2 = math:sqrt(2.0),\n"
             "   {2,7} =\n"
             "      ?M1(begin yes = if R2 > 1 -> yes end,\n"
             "                2 end,\n"
             "          7),\n"
             "   {2,7} =\n"
             "      ?M1(begin yes = case R2 > 1  of true -> yes end,\n"
             "                2 end,\n"
             "          7),\n"
             "   {2,7} =\n"
             "      ?M1(begin yes = receive 1 -> 2 after 0 -> yes end,\n"
             "                2 end,\n"
             "          7),\n"
             "   {2,7} =\n"
             "      ?M1(begin yes = try 1 of 1 -> yes after foo end,\n"
             "                2 end,\n"
             "          7),\n"
             "ok.\n">>,
           ok},

          {otp_8130_4,
           <<"-define(M3(), A).\n"
             "t() -> A = 1, ?M3(), ok.\n">>,
           ok},

          {otp_8130_5,
           <<"-include_lib(\"$epp_inc1/include/qlc.hrl\").\n"
             "t() -> [1] = qlc:e(qlc:q([X || X <- [1]])), ok.\n">>,
           ok},

          {otp_8130_6,
           <<"-include_lib(\"kernel/include/file.hrl\").\n"
             "t() -> 14 = (#file_info{size = 14})#file_info.size, ok.\n">>,
           ok},

          {otp_8130_7_new,
           <<"-record(b, {b}).\n"
             "-define(A, {{a,#b.b).\n"
             "t() -> {{a,2}} = ?A}}, ok.">>,
           ok},

          {otp_8130_8,
           <<"\n-define(A(B), B).\n"
             "-undef(A).\n"
             "-define(A, ok).\n"
             "t() -> ?A.\n">>,
           ok},
          {otp_8130_9,
           <<"-define(a, 1).\n"
             "-define(b, {?a,?a}).\n"
             "t() -> ?b.\n">>,
           {1,1}}

         ],
    ?line [] = run(Config, Ts),

    Cs = [{otp_8130_c1,
           <<"-define(M1(A), if\n"
             "A =:= 1 -> B;\n"
             "true -> 2\n"
             "end).\n"
            "t() -> {?M1(1), ?M1(2)}. \n">>,
           {errors,[{{5,13},erl_lint,{unbound_var,'B'}},
                    {{5,21},erl_lint,{unbound_var,'B'}}],
            []}},

          {otp_8130_c2,
           <<"-define(M(A), A).\n"
             "t() -> ?M(1\n">>,
           {errors,[{{2,9},epp,{arg_error,'M'}}],[]}},

          {otp_8130_c3,
           <<"-define(M(A), A).\n"
             "t() -> ?M.\n">>,
           {errors,[{{2,9},epp,{mismatch,'M'}}],[]}},

          {otp_8130_c4,
           <<"-define(M(A), A).\n"
             "t() -> ?M(1, 2).\n">>,
           {errors,[{{2,9},epp,{mismatch,'M'}}],[]}},

          {otp_8130_c5,
           <<"-define(M(A), A).\n"
             "t() -> ?M().\n">>,
           {errors,[{{2,9},epp,{mismatch,'M'}}],[]}},

          {otp_8130_c6,
           <<"-define(M3(), A).\n"
             "t() -> A = 1, ?3.14159}.\n">>,
           {errors,[{{2,16},epp,{call,"?3.14159"}}],[]}},

          {otp_8130_c7,
           <<"\nt() -> ?A.\n">>,
           {errors,[{{2,9},epp,{undefined,'A', none}}],[]}},

          {otp_8130_c8,
           <<"\n-include_lib(\"$apa/foo.hrl\").\n">>,
           {errors,[{{2,2},epp,{include,lib,"$apa/foo.hrl"}}],[]}},


          {otp_8130_c9,
           <<"-define(S, ?S).\n"
             "t() -> ?S.\n">>,
           {errors,[{{2,9},epp,{circular,'S', none}}],[]}},

          {otp_8130_c10,
           <<"\n-file.">>,
           {errors,[{{2,2},epp,{bad,file}}],[]}},

          {otp_8130_c11,
           <<"\n-include_lib 92.">>,
           {errors,[{{2,2},epp,{bad,include_lib}}],[]}},

          {otp_8130_c12,
           <<"\n-include_lib(\"kernel/include/fopp.hrl\").\n">>,
           {errors,[{{2,2},epp,{include,lib,"kernel/include/fopp.hrl"}}],[]}},

          {otp_8130_c13,
           <<"\n-include(foo).\n">>,
           {errors,[{{2,2},epp,{bad,include}}],[]}},

          {otp_8130_c14,
           <<"\n-undef({foo}).\n">>,
           {errors,[{{2,2},epp,{bad,undef}}],[]}},

          {otp_8130_c15,
           <<"\n-define(a, 1).\n"
            "-define(a, 1).\n">>,
           {errors,[{{3,9},epp,{redefine,a}}],[]}},

          {otp_8130_c16,
           <<"\n-define(A, 1).\n"
            "-define(A, 1).\n">>,
           {errors,[{{3,9},epp,{redefine,'A'}}],[]}},

          {otp_8130_c17,
           <<"\n-define(A(B), B).\n"
            "-define(A, 1).\n">>,
           []},

          {otp_8130_c18,
           <<"\n-define(A, 1).\n"
            "-define(A(B), B).\n">>,
           []},

          {otp_8130_c19,
           <<"\n-define(a(B), B).\n"
            "-define(a, 1).\n">>,
           []},

          {otp_8130_c20,
           <<"\n-define(a, 1).\n"
            "-define(a(B), B).\n">>,
           []},

          {otp_8130_c21,
           <<"\n-define(A(B, B), B).\n">>,
           {errors,[{{2,2},epp,{bad,define}}],[]}},

          {otp_8130_c22,
           <<"\n-define(a(B, B), B).\n">>,
           {errors,[{{2,2},epp,{bad,define}}],[]}},

          {otp_8130_c23,
           <<"\n-file(?b, 3).\n">>,
           {errors,[{{2,8},epp,{undefined,b, none}}],[]}},

          {otp_8130_c24,
           <<"\n-include(\"no such file.erl\").\n">>,
           {errors,[{{2,2},epp,{include,file,"no such file.erl"}}],[]}},

          {otp_8130_7,
           <<"-record(b, {b}).\n"
             "-define(A, {{a,#b.b.\n"
             "t() -> {{a,2}} = ?A}}, ok.">>,
           {errors,[{{2,20},epp,missing_parenthesis},
                    {{3,19},epp,{undefined,'A',none}}],[]}}

          ],
    ?line [] = compile(Config, Cs),

    Cks = [{otp_check_1,
            <<"\n-include_lib(\"epp_test.erl\").\n">>,
            [{error,{{2,2},epp,{depth,"include_lib"}}}]},

           {otp_check_2,
            <<"\n-include(\"epp_test.erl\").\n">>,
            [{error,{{2,2},epp,{depth,"include"}}}]}
           ],
    ?line [] = check(Config, Cks),

    ?line Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "otp_8130.erl"),
    ?line ok = file:write_file(File,
                               "-module(otp_8130).\n"
                               "-define(a, 3.14).\n"
                               "t() -> ?a.\n"),
    ?line {ok,Epp} = epp:open(File, []),
    ?line ['BASE_MODULE','BASE_MODULE_STRING','BEAM','FILE','LINE',
           'MACHINE','MODULE','MODULE_STRING'] = macs(Epp),
    ?line {ok,[{'-',_},{atom,_,file}|_]} = epp:scan_erl_form(Epp),
    ?line {ok,[{'-',_},{atom,_,module}|_]} = epp:scan_erl_form(Epp),
    ?line {ok,[{atom,_,t}|_]} = epp:scan_erl_form(Epp),
    ?line {eof,_} = epp:scan_erl_form(Epp),
    ?line ['BASE_MODULE','BASE_MODULE_STRING','BEAM','FILE','LINE',
           'MACHINE','MODULE','MODULE_STRING',a] = macs(Epp),
    ?line epp:close(Epp),

    %% escript
    ModuleStr = "any_name",
    Module = list_to_atom(ModuleStr),
    fun() ->
            PreDefMacros = [{'MODULE', Module, redefine},
                            {'MODULE_STRING', ModuleStr, redefine},
                            a, {b,2}],
            ?line {ok,Epp2} = epp:open(File, [], PreDefMacros),
            ?line [{atom,_,true}] = macro(Epp2, a),
            ?line [{integer,_,2}] = macro(Epp2, b),
            ?line false = macro(Epp2, c),
            ?line epp:close(Epp2)
    end(),
    fun() ->
            PreDefMacros = [{a,b,c}],
            ?line {error,{bad,{a,b,c}}} = epp:open(File, [], PreDefMacros)
    end(),
    fun() ->
            PreDefMacros = [a, {a,1}],
            ?line {error,{redefine,a}} = epp:open(File, [], PreDefMacros)
    end(),
    fun() ->
            PreDefMacros = [{a,1},a],
            ?line {error,{redefine,a}} = epp:open(File, [], PreDefMacros)
    end(),

    ?line {error,enoent} = epp:open("no such file", []),
    ?line {error,enoent} = epp:parse_file("no such file", [], []),

    _ = ifdef(Config),

    ok.

macs(Epp) ->
    Macros = epp:macro_defs(Epp), % not documented
    lists:sort([MName || {{atom,MName},_} <- Macros]).

macro(Epp, N) ->
    case lists:keyfind({atom,N}, 1, epp:macro_defs(Epp)) of
        false -> false;
        {{atom,N},{_,V}} -> V;
        {{atom,N},Defs} -> lists:append([V || {_,{_,V}} <- Defs])
    end.

ifdef(Config) ->
    Cs = [{ifdef_c1,
           <<"-ifdef(a).\n"
             "a bug.\n"
             "-else.\n"
             "-ifdef(A).\n"
             "a bug.\n"
             "-endif.\n"
             "-else.\n"
             "t() -> ok.\n"
             "-endif.">>,
           {errors,[{{7,2},epp,{illegal,"repeated",'else'}}],[]}},

          {ifdef_c2,
           <<"-define(a, true).\n"
             "-ifdef(a).\n"
             "a bug.\n"
             "-endif.">>,
           {errors,[{{3,3},erl_parse,["syntax error before: ","bug"]}],[]}},

          {ifdef_c3,
           <<"-define(a, true).\n"
             "-ifdef(a).\n"
             "-endif">>,

           {errors,[{{3,2},epp,{bad,endif}},
                    {{3,7},epp,{illegal,"unterminated",ifdef}}],
          []}},

          {ifdef_c4,
           <<"\n-ifdef a.\n"
             "-endif.\n">>,
           {errors,[{{2,2},epp,{bad,ifdef}}],[]}},

          {ifdef_c5,
           <<"-ifdef(a).\n"
             "-else.\n"
             "-endif.\n"
             "-endif.\n">>,
           {errors,[{{4,2},epp,{illegal,"unbalanced",endif}}],[]}},

          {ifdef_c6,
           <<"-ifdef(a).\n"
             "-else.\n"
             "-endif.\n"
             "-else.\n">>,
           {errors,[{{4,2},epp,{illegal,"unbalanced",'else'}}],[]}},

          {ifdef_c7,
           <<"-ifndef(a).\n"
             "-else\n"
             "foo bar\n"
             "-else.\n"
             "t() -> a.\n"
             "-endif.\n">>,
           {errors,[{{2,2},epp,{bad,else}}],[]}},

          {ifdef_c8,
           <<"-ifdef(a).\n"
              "-foo bar.">>,
           {errors,[{{2,10},epp,{illegal,"unterminated",ifdef}}],[]}},

          {ifdef_c9,
           <<"-ifdef(a).\n"
              "3.3e12000.\n"
              "-endif.\n">>,
           []},

          {ifdef_c10,
           <<"\nt() -> 3.3e12000.\n">>,
           {errors,[{{2,8},erl_scan,{illegal,float}},
                    {{2,17},erl_parse,["syntax error before: ","'.'"]}], % ...
            []}},

          {ifndef_c1,
           <<"-ifndef(a).\n"
             "-ifndef(A).\n"
             "t() -> ok.\n"
             "-endif.\n"
             "-else.\n"
             "a bug.\n"
             "-else.\n"
             "a bug.\n"
             "-endif.">>,
           {errors,[{{7,2},epp,{illegal,"repeated",'else'}}],[]}},

          {ifndef_c3,
           <<"-ifndef(a).\n"
             "-endif">>,

           {errors,[{{2,2},epp,{bad,endif}},
                    {{2,7},epp,{illegal,"unterminated",ifndef}}],
          []}},

          {ifndef_c4,
           <<"\n-ifndef a.\n"
             "-endif.\n">>,
           {errors,[{{2,2},epp,{bad,ifndef}}],[]}},

          {define_c5,
           <<"-\ndefine a.\n">>,
           {errors,[{{2,1},epp,{bad,define}}],[]}},

          {define_c6,
           <<"\n-if.\n"
             "-endif.\n">>,
           {errors,[{{2,2},epp,{'NYI','if'}}],[]}},

          {define_c7,
           <<"-ifndef(a).\n"
             "-elif.\n"
             "-endif.\n">>,
           {errors,[{{2,2},epp,{'NYI',elif}}],[]}},

          {define_c7,
           <<"-ifndef(a).\n"
             "-if.\n"
             "-elif.\n"
             "-endif.\n"
             "-endif.\n"
             "t() -> a.\n">>,
           {errors,[{{2,2},epp,{'NYI','if'}}],[]}}
          ],
    ?line [] = compile(Config, Cs),

    Ts =  [{ifdef_1,
            <<"-ifdef(a).\n"
              "a bug.\n"
              "-else.\n"
              "-ifdef(A).\n"
              "a bug.\n"
              "-endif.\n"
              "t() -> ok.\n"
              "-endif.">>,
           ok},

           {ifdef_2,
            <<"-define(a, true).\n"
              "-ifdef(a).\n"
              "-define(A, true).\n"
              "-ifdef(A).\n"
              "t() -> ok.\n"
              "-else.\n"
              "a bug.\n"
              "-endif.\n"
              "-else.\n"
              "a bug.\n"
              "-endif.">>,
           ok},

           {ifdef_3,
            <<"\n-define(a, true).\n"
              "-ifndef(a).\n"
              "a bug.\n"
              "-else.\n"
              "-define(A, true).\n"
              "-ifndef(A).\n"
              "a bug.\n"
              "-else.\n"
              "t() -> ok.\n"
              "-endif.\n"
              "-endif.">>,
           ok},

           {ifdef_4,
                       <<"-ifdef(a).\n"
              "a bug.\n"
              "-ifdef(a).\n"
               "a bug.\n"
              "-else.\n"
              "-endif.\n"
             "-ifdef(A).\n"
              "a bug.\n"
             "-endif.\n"
             "-else.\n"
             "t() -> ok.\n"
             "-endif.">>,
            ok},

           {ifdef_5,
           <<"-ifdef(a).\n"
              "-ifndef(A).\n"
              "a bug.\n"
              "-else.\n"
              "-endif.\n"
             "a bug.\n"
             "-else.\n"
              "t() -> ok.\n"
             "-endif.">>,
            ok},

           {ifdef_6,
           <<"-ifdef(a).\n"
              "-if(A).\n"
              "a bug.\n"
              "-else.\n"
              "-endif.\n"
             "a bug.\n"
             "-else.\n"
              "t() -> ok.\n"
             "-endif.">>,
            ok}

           ],
    ?line [] = run(Config, Ts).



overload_mac(doc) ->
    ["Advanced test on overloading macros."];
overload_mac(suite) ->
    [];
overload_mac(Config) when is_list(Config) ->
    Cs = [
          %% '-undef' removes all definitions of a macro
          {overload_mac_c1,
           <<"-define(A, a).\n"
            "-define(A(X), X).\n"
            "-undef(A).\n"
            "t1() -> ?A.\n",
            "t2() -> ?A(1).">>,
           {errors,[{{4,10},epp,{undefined,'A', none}},
                    {{5,10},epp,{undefined,'A', 1}}],[]}},

          %% cannot overload predefined macros
          {overload_mac_c2,
           <<"-define(MODULE(X), X).">>,
           {errors,[{{1,50},epp,{redefine_predef,'MODULE'}}],[]}},

          %% cannot overload macros with same arity
          {overload_mac_c3,
           <<"-define(A(X), X).\n"
            "-define(A(Y), Y).">>,
           {errors,[{{2,9},epp,{redefine,'A'}}],[]}},

          {overload_mac_c4,
           <<"-define(A, a).\n"
            "-define(A(X,Y), {X,Y}).\n"
            "a(X) -> X.\n"
            "t() -> ?A(1).">>,
           {errors,[{{4,9},epp,{mismatch,'A'}}],[]}}
         ],
    ?line [] = compile(Config, Cs),

    Ts = [
          {overload_mac_r1,
           <<"-define(A, 1).\n"
            "-define(A(X), X).\n"
            "-define(A(X, Y), {X, Y}).\n"
            "t() -> {?A, ?A(2), ?A(3, 4)}.">>,
           {1, 2, {3, 4}}},

          {overload_mac_r2,
           <<"-define(A, 1).\n"
            "-define(A(X), X).\n"
            "t() -> ?A(?A).">>,
           1},

          {overload_mac_r3,
           <<"-define(A, ?B).\n"
            "-define(B, a).\n"
            "-define(B(X), {b,X}).\n"
            "a(X) -> X.\n"
            "t() -> ?A(1).">>,
           1}
          ],
    ?line [] = run(Config, Ts).


otp_8388(doc) ->
    ["OTP-8388. More tests on overloaded macros."];
otp_8388(suite) ->
    [];
otp_8388(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),
    ?line File = filename:join(Dir, "otp_8388.erl"),
    ?line ok = file:write_file(File, <<"-module(otp_8388)."
                                       "-define(LINE, a).">>),
    fun() ->
            PreDefMacros = [{'LINE', a}],
            ?line {error,{redefine_predef,'LINE'}} =
                epp:open(File, [], PreDefMacros)
    end(),

    fun() ->
            PreDefMacros = ['LINE'],
            ?line {error,{redefine_predef,'LINE'}} =
                epp:open(File, [], PreDefMacros)
    end(),

    Ts = [
          {macro_1,
           <<"-define(m(A), A).\n"
             "t() -> ?m(,).\n">>,
           {errors,[{{2,9},epp,{arg_error,m}}],[]}},
          {macro_2,
           <<"-define(m(A), A).\n"
             "t() -> ?m(a,).\n">>,
           {errors,[{{2,9},epp,{arg_error,m}}],[]}},
          {macro_3,
           <<"-define(LINE, a).\n">>,
           {errors,[{{1,50},epp,{redefine_predef,'LINE'}}],[]}},
          {macro_4,
           <<"-define(A(B, C, D), {B,C,D}).\n"
             "t() -> ?A(a,,3).\n">>,
           {errors,[{{2,9},epp,{mismatch,'A'}}],[]}},
          {macro_5,
           <<"-define(Q, {?F0(), ?F1(,,4)}).\n">>,
           {errors,[{{1,62},epp,{arg_error,'F1'}}],[]}},
          {macro_6,
           <<"-define(FOO(X), ?BAR(X)).\n"
             "-define(BAR(X), ?FOO(X)).\n"
             "-undef(FOO).\n"
             "test() -> ?BAR(1).\n">>,
           {errors,[{{4,12},epp,{undefined,'FOO',1}}],[]}}
         ],
    ?line [] = compile(Config, Ts),
    ok.

otp_8470(doc) ->
    ["OTP-8470. Bugfix (one request - two replies)."];
otp_8470(suite) ->
    [];
otp_8470(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),
    C = <<"-file(\"erl_parse.yrl\", 486).\n"
          "-file(\"erl_parse.yrl\", 488).\n">>,
    ?line File = filename:join(Dir, "otp_8470.erl"),
    ?line ok = file:write_file(File, C),
    ?line {ok, _List} = epp:parse_file(File, [], []),
    file:delete(File),
    ?line receive _ -> fail() after 0 -> ok end,
    ok.

otp_8503(doc) ->
    ["OTP-8503. Record with no fields is considered typed."];
otp_8503(suite) ->
    [];
otp_8503(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),
    C = <<"-record(r, {}).">>,
    ?line File = filename:join(Dir, "otp_8503.erl"),
    ?line ok = file:write_file(File, C),
    ?line {ok, List} = epp:parse_file(File, [], []),
    ?line [_] = [F || {attribute,_,type,{{record,r},[],[]}}=F <- List],
    file:delete(File),
    ?line receive _ -> fail() after 0 -> ok end,
    ok.

otp_8562(doc) ->
    ["OTP-8503. Record with no fields is considered typed."];
otp_8562(suite) ->
    [];
otp_8562(Config) when is_list(Config) ->
    Cs = [{otp_8562,
           <<"-define(P(), {a,b}.\n"
             "-define(P3, .\n">>,
           {errors,[{{1,60},epp,missing_parenthesis},
                    {{2,13},epp,missing_parenthesis}], []}}
         ],
    ?line [] = compile(Config, Cs),
    ok.

otp_8911(doc) ->
    ["OTP-8911. -file and file inclusion bug"];
otp_8911(suite) ->
    [];
otp_8911(Config) when is_list(Config) ->
    case test_server:is_cover() of
	true ->
	    {skip, "Testing cover, so can not run when cover is already running"};
	false ->
	    do_otp_8911(Config)
    end.
do_otp_8911(Config) ->
    ?line {ok, CWD} = file:get_cwd(),
    ?line ok = file:set_cwd(?config(priv_dir, Config)),

    File = "i.erl",
    Cont = <<"-module(i).
              -compile(export_all).
              -file(\"fil1\", 100).
              -include(\"i1.erl\").
              t() ->
                  a.
           ">>,
    ?line ok = file:write_file(File, Cont),
    Incl = <<"-file(\"fil2\", 35).
              t1() ->
                  b.
           ">>,
    File1 = "i1.erl",
    ?line ok = file:write_file(File1, Incl),

    ?line {ok, i} = cover:compile(File),
    ?line a = i:t(),
    ?line {ok,[{{i,6},1}]} = cover:analyse(i, calls, line),
    ?line cover:stop(),

    file:delete(File),
    file:delete(File1),
    ?line file:set_cwd(CWD),
    ok.

otp_8665(doc) ->
    ["OTP-8665. Bugfix premature end."];
otp_8665(suite) ->
    [];
otp_8665(Config) when is_list(Config) ->
    Cs = [{otp_8562,
           <<"-define(A, a)\n">>,
           {errors,[{{1,54},epp,premature_end}],[]}}
         ],
    ?line [] = compile(Config, Cs),
    ok.

otp_10302(doc) ->
    "OTP-10302. Unicode characters scanner/parser.";
otp_10302(suite) ->
    [];
otp_10302(Config) when is_list(Config) ->
    %% Two messages (one too many). Keeps otp_4871 happy.
    Cs = [{otp_8562,
           <<"%% coding: utf-8\n \n \x{E4}">>,
           {errors,[{3,epp,cannot_parse},
                    {3,file_io_server,invalid_unicode}],[]}}
         ],
    [] = compile(Config, Cs),
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, "otp_10302.erl"),
    utf8 = encoding("coding: utf-8", File),
    utf8 = encoding("coding: UTF-8", File),
    latin1 = encoding("coding: Latin-1", File),
    latin1 = encoding("coding: latin-1", File),
    none = encoding_com("coding: utf-8", File),
    none = encoding_com("\n\n%% coding: utf-8", File),
    none = encoding_nocom("\n\n coding: utf-8", File),
    utf8 = encoding_com("\n%% coding: utf-8", File),
    utf8 = encoding_nocom("\n coding: utf-8", File),
    none = encoding("coding: \nutf-8", File),
    latin1 = encoding("Encoding :  latin-1", File),
    utf8 = encoding("ccoding: UTF-8", File),
    utf8 = encoding("coding= utf-8", File),
    utf8 = encoding_com(" %% coding= utf-8", File),
    utf8 = encoding("coding  =  utf-8", File),
    none = encoding("coding: utf-16 coding: utf-8", File), %first is bad
    none = encoding("Coding: utf-8", File),    %capital c
    utf8 = encoding("-*- coding: utf-8 -*-", File),
    utf8 = encoding("-*-coding= utf-8-*-", File),
    utf8 = encoding("codingcoding= utf-8", File),
    ok = prefix("coding: utf-8", File, utf8),

    "coding: latin-1" = epp:encoding_to_string(latin1),
    "coding: utf-8" = epp:encoding_to_string(utf8),
    true = lists:member(epp:default_encoding(), [latin1, utf8]),

    ok.

prefix(S, File, Enc) ->
    prefix(0, S, File, Enc).

prefix(100, _S, _File, _) ->
    ok;
prefix(N, S, File, Enc) ->
    Enc = encoding(lists:duplicate(N, $\s) ++ S, File),
    prefix(N+1, S, File, Enc).

encoding(Enc, File) ->
    E = encoding_com("%% " ++ Enc, File),
    none = encoding_com(Enc, File),
    E = encoding_nocom(Enc, File).

encoding_com(Enc, File) ->
    B = list_to_binary(Enc),
    E = epp:read_encoding_from_binary(B),
    ok = file:write_file(File, Enc),
    {ok, Fd} = file:open(File, [read]),
    E = epp:set_encoding(Fd),
    ok = file:close(Fd),
    E = epp:read_encoding(File).

encoding_nocom(Enc, File) ->
    Options = [{in_comment_only, false}],
    B = list_to_binary(Enc),
    E = epp:read_encoding_from_binary(B, Options),
    ok = file:write_file(File, Enc),
    {ok, Fd} = file:open(File, [read]),
    ok = file:close(Fd),
    E = epp:read_encoding(File, Options).

otp_10820(doc) ->
    "OTP-10820. Unicode filenames.";
otp_10820(suite) ->
    [];
otp_10820(Config) when is_list(Config) ->
    L = [915,953,959,973,957,953,954,959,957,964],
    Dir = ?config(priv_dir, Config),
    File = filename:join(Dir, L++".erl"),
    C1 = <<"%% coding: utf-8\n -module(any).">>,
    ok = do_otp_10820(File, C1, "+pc latin1"),
    ok = do_otp_10820(File, C1, "+pc unicode"),
    C2 = <<"\n-module(any).">>,
    ok = do_otp_10820(File, C2, "+pc latin1"),
    ok = do_otp_10820(File, C2, "+pc unicode").

do_otp_10820(File, C, PC) ->
    {ok,Node} = start_node(erl_pp_helper, "+fnu " ++ PC),
    ok = rpc:call(Node, file, write_file, [File, C]),
    {ok, Forms} = rpc:call(Node, epp, parse_file, [File, [],[]]),
    [{attribute,1,file,{File,1}},
     {attribute,2,module,any},
     {eof,2}] = unopaque_forms(Forms),
    true = test_server:stop_node(Node),
    ok.

otp_11728(doc) ->
    ["OTP-11728. Bugfix circular macro."];
otp_11728(suite) ->
    [];
otp_11728(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),
    H = <<"-define(MACRO,[[]++?MACRO]).">>,
    HrlFile = filename:join(Dir, "otp_11728.hrl"),
    ok = file:write_file(HrlFile, H),
    C = <<"-module(otp_11728).
           -compile(export_all).

           -include(\"otp_11728.hrl\").

           function_name()->
               A=?MACRO, % line 7
               ok">>,
    ErlFile = filename:join(Dir, "otp_11728.erl"),
    ok = file:write_file(ErlFile, C),
    {ok, L} = epp:parse_file(ErlFile, [Dir], []),
    true = lists:member({error,{7,epp,{circular,'MACRO',none}}}, L),
    _ = file:delete(HrlFile),
    _ = file:delete(ErlFile),
    ok.

%% Check the new API for setting the default encoding.
encoding(Config) when is_list(Config) ->
    Dir = ?config(priv_dir, Config),
    ErlFile = filename:join(Dir, "encoding.erl"),

    %% Try a latin-1 file with no encoding given.
    C1 = <<"-module(encoding).
           %% ",246,"
	  ">>,
    ok = file:write_file(ErlFile, C1),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {error,_},
	 {error,{2,epp,cannot_parse}},
	 {eof,2}]} = epp_parse_file(ErlFile, []),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,3}]} =
	epp_parse_file(ErlFile, [{default_encoding,latin1}]),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,3}],[{encoding,none}]} =
	epp_parse_file(ErlFile, [{default_encoding,latin1},extra]),

    %% Try a latin-1 file with encoding given in a comment.
    C2 = <<"-module(encoding).
           %% encoding: latin-1
           %% ",246,"
	  ">>,
    ok = file:write_file(ErlFile, C2),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,4}]} =
	epp_parse_file(ErlFile, []),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,4}]} =
	epp_parse_file(ErlFile, [{default_encoding,latin1}]),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,4}]} =
	epp_parse_file(ErlFile, [{default_encoding,utf8}]),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,4}],[{encoding,latin1}]} =
	epp_parse_file(ErlFile, [extra]),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,4}],[{encoding,latin1}]} =
	epp_parse_file(ErlFile, [{default_encoding,latin1},extra]),
    {ok,[{attribute,1,file,_},
	 {attribute,1,module,encoding},
	 {eof,4}],[{encoding,latin1}]} =
	epp_parse_file(ErlFile, [{default_encoding,utf8},extra]),
    ok.


check(Config, Tests) ->
    eval_tests(Config, fun check_test/2, Tests).

compile(Config, Tests) ->
    eval_tests(Config, fun compile_test/2, Tests).

run(Config, Tests) ->
    eval_tests(Config, fun run_test/2, Tests).

eval_tests(Config, Fun, Tests) ->
    F = fun({N,P,E}, BadL) ->
                %% io:format("Testing ~p~n", [P]),
                Return = Fun(Config, P),
                case message_compare(E, Return) of
                    true ->
                        BadL;
                    false ->
                        ?t:format("~nTest ~p failed. Expected~n  ~p~n"
                                  "but got~n  ~p~n", [N, E, Return]),
			fail()
                end
        end,
    lists:foldl(F, [], Tests).


check_test(Config, Test) ->
    Filename = "epp_test.erl",
    ?line PrivDir = ?config(priv_dir, Config),
    ?line File = filename:join(PrivDir, Filename),
    ?line ok = file:write_file(File, Test),
    ?line case epp:parse_file(File, [PrivDir], []) of
              {ok,Forms} ->
                  [E || E={error,_} <- Forms];
              {error,Error} ->
                  Error
          end.

compile_test(Config, Test0) ->
    Test = [<<"-module(epp_test). -compile(export_all). ">>, Test0],
    Filename = "epp_test.erl",
    ?line PrivDir = ?config(priv_dir, Config),
    ?line File = filename:join(PrivDir, Filename),
    ?line ok = file:write_file(File, Test),
    Opts = [export_all,return,nowarn_unused_record,{outdir,PrivDir}],
    case compile_file(File, Opts) of
        {ok, Ws} -> warnings(File, Ws);
        Else -> Else
    end.

warnings(File, Ws) ->
    case lists:append([W || {F, W} <- Ws, F =:= File]) of
        [] -> [];
        L -> {warnings, L}
    end.

compile_file(File, Opts) ->
    case compile:file(File, Opts) of
        {ok, _M, Ws} -> {ok, Ws};
        {error, FEs, []} -> {errors, errs(FEs, File), []};
        {error, FEs, [{File,Ws}]} -> {error, errs(FEs, File), Ws}
    end.

errs([{File,Es}|L], File) ->
    Es ++ errs(L, File);
errs([_|L], File) ->
    errs(L, File);
errs([], _File) ->
    [].

epp_parse_file(File, Opts) ->
    case epp:parse_file(File, Opts) of
        {ok, Forms} ->
            {ok, unopaque_forms(Forms)};
        {ok, Forms, Other} ->
            {ok, unopaque_forms(Forms), Other}
    end.

unopaque_forms(Forms) ->
    [erl_parse:anno_to_term(Form) || Form <- Forms].

run_test(Config, Test0) ->
    Test = [<<"-module(epp_test). -compile(export_all). ">>, Test0],
    Filename = "epp_test.erl",
    ?line PrivDir = ?config(priv_dir, Config),
    ?line File = filename:join(PrivDir, Filename),
    ?line ok = file:write_file(File, Test),
    Opts = [return, {i,PrivDir},{outdir,PrivDir}],
    ?line {ok, epp_test, []} = compile:file(File, Opts),
    AbsFile = filename:rootname(File, ".erl"),
    ?line {module, epp_test} = code:load_abs(AbsFile, epp_test),
    ?line Reply = epp_test:t(),
    code:purge(epp_test),
    Reply.

fail() ->
    io:format("failed~n"),
    test_server:fail().

message_compare(T, T) ->
    true;
message_compare(T1, T2) ->
    ln(T1) =:= T2.

%% Replaces locations like {Line,Column} with Line.
ln({warnings,L}) ->
    {warnings,ln0(L)};
ln({errors,EL,WL}) ->
    {errors,ln0(EL),ln0(WL)};
ln(L) ->
    ln0(L).

ln0(L) ->
    lists:keysort(1, ln1(L)).

ln1([]) ->
    [];
ln1([{File,Ms}|MsL]) when is_list(File) ->
    [{File,ln0(Ms)}|ln1(MsL)];
ln1([M|Ms]) ->
    [ln2(M)|ln1(Ms)].

ln2({{L,_C},Mod,Mess}) ->
    {L,Mod,Mess};
ln2({error,M}) ->
    {error,ln2(M)};
ln2(M) ->
    M.

%% +fnu means a peer node has to be started; slave will not do
start_node(Name, Xargs) ->
    ?line PA = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, peer, [{args, "-pa " ++ PA ++ " " ++ Xargs}]).
