%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%%-------------------------------------------------------------------
%%% File    : erl_print_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 10 Mar 2005 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(erl_print_SUITE).
-author('rickard.s.green@ericsson.com').


%-define(line_trace, 1).

-define(DEFAULT_TIMEOUT, ?t:minutes(10)).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([erlang_display/1, integer/1, float/1, 
	 string/1, character/1, snprintf/1, quote/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_cases().

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


%%
%%
%% Test cases
%%
%%

test_cases() -> 
    [erlang_display, integer, float, string, character,
     snprintf, quote].

erlang_display(doc) -> [];
erlang_display(suite) -> [];
erlang_display(Config) when is_list(Config) ->
    ?line put(erlang_display_test, ok),
    OAIS = erts_debug:set_internal_state(available_internal_state, true),

    %% atoms
    ?line chk_display(atom, "atom"),
    ?line chk_display(true, "true"),
    ?line chk_display(false, "false"),
    ?line chk_display('DOWN', "'DOWN'"),
    ?line chk_display('EXIT', "'EXIT'"),
    ?line chk_display('asdDofw $@{}][', "'asdDofw $@{}]['"),

    %% integers
    ?line chk_display(0, "0"),
    ?line chk_display(1, "1"),
    ?line chk_display(4711, "4711"),
    ?line chk_display(((1 bsl 27) - 1), "134217727"),
    ?line chk_display((1 bsl 27), "134217728"),
    ?line chk_display((1 bsl 32), "4294967296"),
    ?line chk_display(11111111111, "11111111111"),
    ?line chk_display((1 bsl 59) - 1, "576460752303423487"),
    ?line chk_display(1 bsl 59, "576460752303423488"),
    ?line chk_display(111111111111111111111, "111111111111111111111"),
    ?line chk_display(123456789012345678901234567890,
		      "123456789012345678901234567890"),
    ?line chk_display(1 bsl 10000, str_1_bsl_10000()),
    ?line chk_display(-1, "-1"),
    ?line chk_display(-4711, "-4711"),
    ?line chk_display(-(1 bsl 27), "-134217728"),
    ?line chk_display(-((1 bsl 27) + 1), "-134217729"),
    ?line chk_display(-(1 bsl 32), "-4294967296"),
    ?line chk_display(-11111111111, "-11111111111"),
    ?line chk_display(-(1 bsl 59), "-576460752303423488"),
    ?line chk_display(-((1 bsl 59) + 1), "-576460752303423489"),
    ?line chk_display(-111111111111111111111, "-111111111111111111111"),
    ?line chk_display(-123456789012345678901234567890,
		      "-123456789012345678901234567890"),
    ?line chk_display(-(1 bsl 10000), [$- | str_1_bsl_10000()]),

    ?line MyCre = my_cre(),

    %% pids
    ?line chk_display(mk_pid_xstr({node(), MyCre}, 4711, 42)),
    ?line chk_display(mk_pid_xstr({node(), oth_cre(MyCre)}, 4711, 42)),
    ?line chk_display(mk_pid_xstr({node(), oth_cre(oth_cre(MyCre))}, 4711, 42)),

    ?line chk_display(mk_pid_xstr({a@b, MyCre}, 4711, 42)),
    ?line chk_display(mk_pid_xstr({a@b, oth_cre(MyCre)}, 4711, 42)),
    ?line chk_display(mk_pid_xstr({a@b, oth_cre(oth_cre(MyCre))}, 4711, 42)),

    %% ports
    ?line chk_display(mk_port_xstr({node(), MyCre}, 4711)),
    ?line chk_display(mk_port_xstr({node(), oth_cre(MyCre)}, 4711)),
    ?line chk_display(mk_port_xstr({node(), oth_cre(oth_cre(MyCre))}, 4711)),

    ?line chk_display(mk_port_xstr({c@d, MyCre}, 4711)),
    ?line chk_display(mk_port_xstr({c@d, oth_cre(MyCre)}, 4711)),
    ?line chk_display(mk_port_xstr({c@d, oth_cre(oth_cre(MyCre))}, 4711)),

    %% refs
    ?line chk_display(mk_ref_xstr({node(), MyCre}, [1,2,3])),
    ?line chk_display(mk_ref_xstr({node(), oth_cre(MyCre)}, [1,2,3])),
    ?line chk_display(mk_ref_xstr({node(), oth_cre(oth_cre(MyCre))}, [1,2,3])),

    ?line chk_display(mk_ref_xstr({e@f, MyCre},[1,2,3] )),
    ?line chk_display(mk_ref_xstr({e@f, oth_cre(MyCre)}, [1,2,3])),
    ?line chk_display(mk_ref_xstr({e@f, oth_cre(oth_cre(MyCre))}, [1,2,3])),

    %% Compund terms
    ?line {Pid, PidStr} = mk_pid_xstr({x@y, oth_cre(MyCre)}, 4712, 41),
    ?line {Port, PortStr} = mk_port_xstr({x@y, oth_cre(MyCre)}, 4712),
    ?line {Ref, RefStr} = mk_ref_xstr({e@f, oth_cre(MyCre)}, [11,12,13]),

    ?line chk_display({atom,-4711,Ref,{"hej",[Pid,222222222222222222222222,Port,4711]}},
		      "{atom,-4711,"++RefStr++",{\"hej\",["++PidStr++",222222222222222222222222,"++PortStr++",4711]}}"),
    ?line chk_display({{{{{{{{{{{{{{{{{{{{{{{hi}}}}}}}}}}}}}}}}}}}}}}},
		      "{{{{{{{{{{{{{{{{{{{{{{{hi}}}}}}}}}}}}}}}}}}}}}}}"),
    ?line chk_display([[[[[[[[[[[[[[[[[[[[[[[yo]]]]]]]]]]]]]]]]]]]]]]],
		      "[[[[[[[[[[[[[[[[[[[[[[[yo]]]]]]]]]]]]]]]]]]]]]]]"),
    ?line chk_display({[{[{[{[{[{[{[{[{[{[{[{[ii]}]}]}]}]}]}]}]}]}]}]}]},
		      "{[{[{[{[{[{[{[{[{[{[{[{[ii]}]}]}]}]}]}]}]}]}]}]}]}"),
    ?line chk_display([], "[]"), % Not really a compound term :)
    ?line chk_display([a|b], "[a|b]"),
    ?line chk_display([a,b,c|z], "[a,b,c|z]"),
    ?line chk_display([a,b,c], "[a,b,c]"),
    ?line chk_display([Pid,Port,Ref],
		      "["++PidStr++","++PortStr++","++RefStr++"]"),
    ?line chk_display("abcdefghijklmnopqrstuvwxyz",
		      "\"abcdefghijklmnopqrstuvwxyz\""),
    ?line chk_display("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
		      "\"ABCDEFGHIJKLMNOPQRSTUVWXYZ\""),
    ?line chk_display("H E J", "\"H E J\""),
    ?line chk_display("asdDofw $@{}][", "\"asdDofw $@{}][\""),
    
    %%
    %% TODO: Check binaries, fun and floats...
    %%

    erts_debug:set_internal_state(available_internal_state, OAIS),
    ?line ok = get(erlang_display_test).

get_chnl_no(NodeName) when is_atom(NodeName) ->
    erts_debug:get_internal_state({channel_number, NodeName}).

chk_display(Term, Expect) when is_list(Expect) ->
    Dstr = erts_debug:display(Term),
    case Expect ++ io_lib:nl() of
	Dstr ->
	    ?t:format("Test of \"~p\" succeeded.~n"
		      "  Expected and got: ~s~n",
		      [Term, io_lib:write_string(Dstr)]);
	DoExpect ->
	    ?t:format("***~n"
		      "*** Test of \"~p\" failed!~n"
		      "***       Expected: ~s~n"
		      "***            Got: ~s~n"
		      "***~n",
		      [Term,
		       io_lib:write_string(DoExpect),
		       io_lib:write_string(Dstr)]),
	    put(erlang_display_test, failed)
    end.

chk_display({Term, Expect}) ->
    chk_display(Term, Expect).

mk_pid_xstr({NodeName, Creation}, Number, Serial) ->
    Pid = mk_pid({NodeName, Creation}, Number, Serial),
    XStr = "<" ++ integer_to_list(get_chnl_no(NodeName))
	++ "." ++ integer_to_list(Number)
	++ "." ++ integer_to_list(Serial) ++ ">",
    {Pid, XStr}.

mk_port_xstr({NodeName, Creation}, Number) ->
    Port = mk_port({NodeName, Creation}, Number),
    XStr = "#Port<" ++ integer_to_list(get_chnl_no(NodeName))
	++ "." ++ integer_to_list(Number) ++ ">",
    {Port, XStr}.

mk_ref_xstr({NodeName, Creation}, Numbers) ->
    Ref = mk_ref({NodeName, Creation}, Numbers),
    XStr = "#Ref<" ++ integer_to_list(get_chnl_no(NodeName))
	++ ref_numbers_xstr(Numbers) ++ ">",
    {Ref, XStr}.

ref_numbers_xstr([]) ->
    [];
ref_numbers_xstr([N | Ns]) ->
    ref_numbers_xstr(Ns) ++ "." ++ integer_to_list(N).

-define(TESTCASE_IMPL(T), T(A) -> default_testcase_impl(A)).

?TESTCASE_IMPL(integer).
?TESTCASE_IMPL(float).
?TESTCASE_IMPL(string).
?TESTCASE_IMPL(character).
?TESTCASE_IMPL(snprintf).
?TESTCASE_IMPL(quote).

%%
%%
%% Auxiliary functions
%%
%%

default_testcase_impl(doc) -> [];
default_testcase_impl(suite) -> [];
default_testcase_impl(Config) when is_list(Config) -> ?line run_case(Config).

init_per_testcase(Case, Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    [{testcase, Case}, {watchdog, Dog} |Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

-define(TESTPROG, "erl_print_tests").
-define(FAILED_MARKER, $E,$P,$-,$T,$E,$S,$T,$-,$F,$A,$I,$L,$U,$R,$E).
-define(SKIPPED_MARKER, $E,$P,$-,$T,$E,$S,$T,$-,$S,$K,$I,$P).
-define(SUCCESS_MARKER, $E,$P,$-,$T,$E,$S,$T,$-,$S,$U,$C,$C,$E,$S,$S).
-define(PID_MARKER, $E,$P,$-,$T,$E,$S,$T,$-,$P,$I,$D).

port_prog_killer(EProc, OSProc) when is_pid(EProc), is_list(OSProc) ->
    ?line process_flag(trap_exit, true),
    ?line Ref = erlang:monitor(process, EProc),
    ?line receive
	      {'DOWN', Ref, _, _, Reason} when is_tuple(Reason),
					       element(1, Reason)
					       == timetrap_timeout ->
		  ?line Cmd = "kill -9 " ++ OSProc,
		  ?line ?t:format("Test case timed out. "
				  "Trying to kill port program.~n"
				  "  Executing: ~p~n", [Cmd]),
		  ?line case os:cmd(Cmd) of
			    [] ->
				ok;
			    OsCmdRes ->
				?line ?t:format("             ~s", [OsCmdRes])
			end;
	      {'DOWN', Ref, _, _, _} ->
		  %% OSProc is assumed to have terminated by itself
		  ?line ok 
	  end.

get_line(_Port, eol, Data) ->
    ?line Data;
get_line(Port, noeol, Data) ->
    ?line receive
	      {Port, {data, {Flag, NextData}}} ->
		  ?line get_line(Port, Flag, Data ++ NextData);
	      {Port, eof} ->
		  ?line ?t:fail(port_prog_unexpectedly_closed)
	  end.

read_case_data(Port, TestCase) ->
    ?line receive
	      {Port, {data, {eol, [?SUCCESS_MARKER]}}} ->
		  ?line ok;
	      {Port, {data, {Flag, [?SUCCESS_MARKER | CommentStart]}}} ->
		  ?line {comment, get_line(Port, Flag, CommentStart)};
	      {Port, {data, {Flag, [?SKIPPED_MARKER | CommentStart]}}} ->
		  ?line {skipped, get_line(Port, Flag, CommentStart)};
	      {Port, {data, {Flag, [?FAILED_MARKER | ReasonStart]}}} ->
		  ?line ?t:fail(get_line(Port, Flag, ReasonStart));
	      {Port, {data, {eol, [?PID_MARKER | PidStr]}}} ->
		  ?line ?t:format("Port program pid: ~s~n", [PidStr]),
		  ?line CaseProc = self(),
		  ?line _ = list_to_integer(PidStr), % Sanity check
		  spawn_opt(fun () ->
				    port_prog_killer(CaseProc, PidStr)
			    end,
			    [{priority, max}, link]),
		  read_case_data(Port, TestCase);
	      {Port, {data, {Flag, LineStart}}} ->
		  ?line ?t:format("~s~n", [get_line(Port, Flag, LineStart)]),
		  read_case_data(Port, TestCase);
	      {Port, eof} ->
		  ?line ?t:fail(port_prog_unexpectedly_closed)
	  end.

run_case(Config) ->
    run_case(Config, "").

run_case(Config, TestArgs) ->
    run_case(Config, TestArgs, fun (_Port) -> ok end).

run_case(Config, TestArgs, Fun) ->
    Test = atom_to_list(?config(testcase, Config)),
    TestProg = filename:join([?config(data_dir, Config),
			      ?TESTPROG
			      ++ "."
			      ++ atom_to_list(erlang:system_info(threads))]),
    Cmd = TestProg ++ " " ++ Test ++ " " ++ TestArgs,
    case catch open_port({spawn, Cmd}, [stream,
					use_stdio,
					stderr_to_stdout,
					eof,
					{line, 1024}]) of
	Port when is_port(Port) ->
	    ?line Fun(Port),
	    ?line CaseResult = read_case_data(Port, Test),
	    ?line receive
		      {Port, eof} ->
			  ?line ok
		  end,
	    ?line CaseResult;
	Error ->
	    ?line ?t:fail({open_port_failed, Error})
    end.


-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).


uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).



mk_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    mk_pid({atom_to_list(NodeName), Creation}, Number, Serial);
mk_pid({NodeName, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PID_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint32_be(Serial),
					      uint8(Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeName, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    mk_port({atom_to_list(NodeName), Creation}, Number);
mk_port({NodeName, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?PORT_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeName, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

mk_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    mk_ref({atom_to_list(NodeName), Creation}, Numbers);
mk_ref({NodeName, Creation}, [Number]) when is_list(NodeName),
					    is_integer(Creation),
					    is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?REFERENCE_EXT,
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint8(Creation)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, [Number]]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end;
mk_ref({NodeName, Creation}, Numbers) when is_list(NodeName),
					   is_integer(Creation),
					   is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ?NEW_REFERENCE_EXT,
					      uint16_be(length(Numbers)),
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint8(Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

my_cre() -> erlang:system_info(creation).

oth_cre(0) -> 1;
oth_cre(1) -> 2;
oth_cre(2) -> 3;
oth_cre(3) -> 1;
oth_cre(N) -> exit({invalid_creation, N}).

str_1_bsl_10000() ->
    "19950631168807583848837421626835850838234968318861924548520089498529438830221946631919961684036194597899331129423209124271556491349413781117593785932096323957855730046793794526765246551266059895520550086918193311542508608460618104685509074866089624888090489894838009253941633257850621568309473902556912388065225096643874441046759871626985453222868538161694315775629640762836880760732228535091641476183956381458969463899410840960536267821064621427333394036525565649530603142680234969400335934316651459297773279665775606172582031407994198179607378245683762280037302885487251900834464581454650557929601414833921615734588139257095379769119277800826957735674444123062018757836325502728323789270710373802866393031428133241401624195671690574061419654342324638801248856147305207431992259611796250130992860241708340807605932320161268492288496255841312844061536738951487114256315111089745514203313820202931640957596464756010405845841566072044962867016515061920631004186422275908670900574606417856951911456055068251250406007519842261898059237118054444788072906395242548339221982707404473162376760846613033778706039803413197133493654622700563169937455508241780972810983291314403571877524768509857276937926433221599399876886660808368837838027643282775172273657572744784112294389733810861607423253291974813120197604178281965697475898164531258434135959862784130128185406283476649088690521047580882615823961985770122407044330583075869039319604603404973156583208672105913300903752823415539745394397715257455290510212310947321610753474825740775273986348298498340756937955646638621874569499279016572103701364433135817214311791398222983845847334440270964182851005072927748364550578634501100852987812389473928699540834346158807043959118985815145779177143619698728131459483783202081474982171858011389071228250905826817436220577475921417653715687725614904582904992461028630081535583308130101987675856234343538955409175623400844887526162643568648833519463720377293240094456246923254350400678027273837755376406726898636241037491410966718557050759098100246789880178271925953381282421954028302759408448955014676668389697996886241636313376393903373455801407636741877711055384225739499110186468219696581651485130494222369947714763069155468217682876200362777257723781365331611196811280792669481887201298643660768551639860534602297871557517947385246369446923087894265948217008051120322365496288169035739121368338393591756418733850510970271613915439590991598154654417336311656936031122249937969999226781732358023111862644575299135758175008199839236284615249881088960232244362173771618086357015468484058622329792853875623486556440536962622018963571028812361567512543338303270029097668650568557157505516727518899194129711337690149916181315171544007728650573189557450920330185304847113818315407324053319038462084036421763703911550639789000742853672196280903477974533320468368795868580237952218629120080742819551317948157624448298518461509704888027274721574688131594750409732115080498190455803416826949787141316063210686391511681774304792596709376".
