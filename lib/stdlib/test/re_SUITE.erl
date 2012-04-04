%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2012. All Rights Reserved.
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
-module(re_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, pcre/1,compile_options/1,
	 run_options/1,combined_options/1,replace_autogen/1,
	 global_capture/1,replace_input_types/1,replace_return/1,
	 split_autogen/1,split_options/1,split_specials/1,
	 error_handling/1,pcre_cve_2008_2371/1,
	 pcre_compile_workspace_overflow/1,re_infinite_loop/1]).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [pcre, compile_options, run_options, combined_options,
     replace_autogen, global_capture, replace_input_types,
     replace_return, split_autogen, split_options,
     split_specials, error_handling, pcre_cve_2008_2371,
     pcre_compile_workspace_overflow, re_infinite_loop].

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


pcre(doc) ->
    ["Run all applicable tests from the PCRE testsuites."];
pcre(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    RootDir = ?config(data_dir, Config),
    Res = run_pcre_tests:test(RootDir),
    0 = lists:sum([ X || {X,_,_} <- Res ]),
    ?t:timetrap_cancel(Dog),
    {comment,Res}.

compile_options(doc) ->
    ["Test all documented compile options"];
compile_options(Config) when is_list(Config) ->
    ?line ok = ctest("ABDabcdABCD","abcd",[],true,{match,[{3,4}]}),
    ?line ok = ctest("ABDabcdABCD","abcd",[anchored],true,nomatch),
    ?line ok = ctest("ABDabcdABCD",".*abcd",[anchored],true,{match,[{0,7}]}),
    ?line ok = ctest("ABCabcdABC","ABCD",[],true,nomatch),
    ?line ok = ctest("ABCabcdABC","ABCD",[caseless],true,{match,[{3,4}]}),
    ?line ok = ctest("abcdABC\n","ABC$",[],true,{match,[{4,3}]}),
    ?line ok = ctest("abcdABC\n","ABC$",[dollar_endonly],true,nomatch),
    ?line ok = ctest("abcdABC\n","ABC.",[],true,nomatch),
    ?line ok = ctest("abcdABC\n","ABC.",[dotall],true,{match,[{4,4}]}),
    ?line ok = ctest("abcdABCD","ABC .",[],true,nomatch),
    ?line ok = ctest("abcdABCD","ABC .",[extended],true,{match,[{4,4}]}),
    ?line ok = ctest("abcd\nABCD","ABC",[],true,{match,[{5,3}]}),
    ?line ok = ctest("abcd\nABCD","ABC",[firstline],true,nomatch),
    ?line ok = ctest("abcd\nABCD","^ABC",[],true,nomatch),
    ?line ok = ctest("abcd\nABCD","^ABC",[multiline],true,{match,[{5,3}]}),
    ?line ok = ctest("abcdABCD","(ABC)",[],true,{match,[{4,3},{4,3}]}),
    ?line ok = ctest("abcdABCD","(ABC)",[no_auto_capture],true,{match,[{4,3}]}),
    ?line ok = ctest(notused,"(?<FOO>ABC)|(?<FOO>DEF)",[],false,notused),
    ?line ok = ctest("abcdABCD","(?<FOO>ABC)|(?<FOO>DEF)",[dupnames],true,{match,[{4,3},{4,3}]}),
    ?line ok = ctest("abcdABCDabcABCD","abcd.*D",[],true,{match,[{0,15}]}),
    ?line ok = ctest("abcdABCDabcABCD","abcd.*D",[ungreedy],true,{match,[{0,8}]}),
    ?line ok = ctest("abcdABCabcABC\nD","abcd.*D",[],true,nomatch),
    ?line ok = ctest("abcdABCabcABC\nD","abcd.*D",[{newline,cr}],true,{match,[{0,15}]}),
    ?line ok = ctest("abcdABCabcABC\rD","abcd.*D",[],true,{match,[{0,15}]}),
    ?line ok = ctest("abcdABCabcABC\rD","abcd.*D",[{newline,lf}],true,{match,[{0,15}]}),
    ?line ok = ctest("abcdABCabcd\r\n","abcd$",[{newline,lf}],true,nomatch),
    ?line ok = ctest("abcdABCabcd\r\n","abcd$",[{newline,cr}],true,nomatch),
    ?line ok = ctest("abcdABCabcd\r\n","abcd$",[{newline,crlf}],true,{match,[{7,4}]}),
    
     ?line ok = ctest("abcdABCabcd\r","abcd$",[{newline,crlf}],true,nomatch),
     ?line ok = ctest("abcdABCabcd\n","abcd$",[{newline,crlf}],true,nomatch),
    ?line ok = ctest("abcdABCabcd\r\n","abcd$",[{newline,anycrlf}],true,{match,[{7,4}]}),
    
     ?line ok = ctest("abcdABCabcd\r","abcd$",[{newline,anycrlf}],true,{match,[{7,4}]}),
     ?line ok = ctest("abcdABCabcd\n","abcd$",[{newline,anycrlf}],true,{match,[{7,4}]}),
    ok.

run_options(doc) ->
    ["Test all documented run specific options"];
run_options(Config) when is_list(Config) ->
    ?line rtest("ABCabcdABC","abc",[],[],true),
    ?line rtest("ABCabcdABC","abc",[anchored],[],false),
    % Anchored in run overrides unanchored in compilation
    ?line rtest("ABCabcdABC","abc",[],[anchored],false), 
    
    ?line rtest("","a?b?",[],[],true),
    ?line rtest("","a?b?",[],[notempty],false),

    ?line rtest("abc","^a",[],[],true),
    ?line rtest("abc","^a",[],[notbol],false),
    ?line rtest("ab\nc","^a",[multiline],[],true),
    ?line rtest("ab\nc","^a",[multiline],[notbol],false),
    ?line rtest("ab\nc","^c",[multiline],[notbol],true),

    ?line rtest("abc","c$",[],[],true),
    ?line rtest("abc","c$",[],[noteol],false),

    ?line rtest("ab\nc","b$",[multiline],[],true),
    ?line rtest("ab\nc","c$",[multiline],[],true),
    ?line rtest("ab\nc","b$",[multiline],[noteol],true),
    ?line rtest("ab\nc","c$",[multiline],[noteol],false),

    ?line rtest("abc","ab",[],[{offset,0}],true),
    ?line rtest("abc","ab",[],[{offset,1}],false),
    
    ?line rtest("abcdABCabcABC\nD","abcd.*D",[],[],false),
    ?line rtest("abcdABCabcABC\nD","abcd.*D",[],[{newline,cr}],true),
    ?line rtest("abcdABCabcABC\rD","abcd.*D",[],[],true),
    ?line rtest("abcdABCabcABC\rD","abcd.*D",[{newline,cr}],[{newline,lf}],true),
    ?line rtest("abcdABCabcd\r\n","abcd$",[],[{newline,lf}],false),
    ?line rtest("abcdABCabcd\r\n","abcd$",[],[{newline,cr}],false),
    ?line rtest("abcdABCabcd\r\n","abcd$",[],[{newline,crlf}],true),
    
    ?line rtest("abcdABCabcd\r","abcd$",[],[{newline,crlf}],false),
    ?line rtest("abcdABCabcd\n","abcd$",[],[{newline,crlf}],false),
    ?line rtest("abcdABCabcd\r\n","abcd$",[],[{newline,anycrlf}],true),
    
    ?line rtest("abcdABCabcd\r","abcd$",[],[{newline,anycrlf}],true),
    ?line rtest("abcdABCabcd\n","abcd$",[],[{newline,anycrlf}],true),
    
    ?line {ok,MP} = re:compile(".*(abcd).*"),
    ?line {match,[{0,10},{3,4}]} = re:run("ABCabcdABC",MP,[]),
    ?line {match,[{0,10},{3,4}]} = re:run("ABCabcdABC",MP,[{capture,all}]),
    ?line {match,[{0,10},{3,4}]} = re:run("ABCabcdABC",MP,[{capture,all,index}]),
    ?line {match,["ABCabcdABC","abcd"]} = re:run("ABCabcdABC",MP,[{capture,all,list}]),
    ?line {match,[<<"ABCabcdABC">>,<<"abcd">>]} = re:run("ABCabcdABC",MP,[{capture,all,binary}]),
    ?line {match,[{0,10}]} = re:run("ABCabcdABC",MP,[{capture,first}]),
    ?line {match,[{0,10}]} = re:run("ABCabcdABC",MP,[{capture,first,index}]),       ?line {match,["ABCabcdABC"]} = re:run("ABCabcdABC",MP,[{capture,first,list}]),       
    ?line {match,[<<"ABCabcdABC">>]} = re:run("ABCabcdABC",MP,[{capture,first,binary}]),    
    
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",MP,[{capture,all_but_first}]),
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",MP,[{capture,all_but_first,index}]),
    ?line {match,["abcd"]} = re:run("ABCabcdABC",MP,[{capture,all_but_first,list}]),
    ?line {match,[<<"abcd">>]} = re:run("ABCabcdABC",MP,[{capture,all_but_first,binary}]),
    
    ?line match = re:run("ABCabcdABC",MP,[{capture,none}]),
    ?line match = re:run("ABCabcdABC",MP,[{capture,none,index}]),
    ?line match = re:run("ABCabcdABC",MP,[{capture,none,list}]),
    ?line match = re:run("ABCabcdABC",MP,[{capture,none,binary}]),

    ?line {ok,MP2} = re:compile(".*(?<FOO>abcd).*"),
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",MP2,[{capture,[1]}]),
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",MP2,[{capture,['FOO']}]),
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",MP2,[{capture,["FOO"]}]),
    ?line {match,["abcd"]} = re:run("ABCabcdABC",MP2,[{capture,["FOO"],list}]),
    ?line {match,[<<"abcd">>]} = re:run("ABCabcdABC",MP2,[{capture,["FOO"],binary}]),
    
    ?line {match,[{-1,0}]} = re:run("ABCabcdABC",MP2,[{capture,[200]}]),
    ?line {match,[{-1,0}]} = re:run("ABCabcdABC",MP2,[{capture,['BAR']}]),
    ?line {match,[""]} = re:run("ABCabcdABC",MP2,[{capture,[200],list}]),
    ?line {match,[""]} = re:run("ABCabcdABC",MP2,[{capture,['BAR'],list}]),
    ?line {match,[<<>>]} = re:run("ABCabcdABC",MP2,[{capture,[200],binary}]),
    ?line {match,[<<>>]} = re:run("ABCabcdABC",MP2,[{capture,['BAR'],binary}]),

    ?line {ok, MP3} = re:compile(".*((?<FOO>abdd)|a(..d)).*"),
    ?line {match,[{0,10},{3,4},{-1,0},{4,3}]} = re:run("ABCabcdABC",MP3,[]),
    ?line {match,[{0,10},{3,4},{-1,0},{4,3}]} = re:run("ABCabcdABC",MP3,[{capture,all,index}]),
    ?line {match,[<<"ABCabcdABC">>,<<"abcd">>,<<>>,<<"bcd">>]} = re:run("ABCabcdABC",MP3,[{capture,all,binary}]),
    ?line {match,["ABCabcdABC","abcd",[],"bcd"]} = re:run("ABCabcdABC",MP3,[{capture,all,list}]),
    ok.
    

    
combined_options(doc) ->    
    ["Test compile options given directly to run"];
combined_options(Config) when is_list(Config) ->
    ?line ok = crtest("ABDabcdABCD","abcd",[],true,{match,[{3,4}]}),
    ?line ok = crtest("ABDabcdABCD","abcd",[anchored],true,nomatch),
    ?line ok = crtest("ABDabcdABCD",".*abcd",[anchored],true,{match,[{0,7}]}),
    ?line ok = crtest("ABCabcdABC","ABCD",[],true,nomatch),
    ?line ok = crtest("ABCabcdABC","ABCD",[caseless],true,{match,[{3,4}]}),
    ?line ok = crtest("abcdABC\n","ABC$",[],true,{match,[{4,3}]}),
    ?line ok = crtest("abcdABC\n","ABC$",[dollar_endonly],true,nomatch),
    ?line ok = crtest("abcdABC\n","ABC.",[],true,nomatch),
    ?line ok = crtest("abcdABC\n","ABC.",[dotall],true,{match,[{4,4}]}),
    ?line ok = crtest("abcdABCD","ABC .",[],true,nomatch),
    ?line ok = crtest("abcdABCD","ABC .",[extended],true,{match,[{4,4}]}),
    ?line ok = crtest("abcd\nABCD","ABC",[],true,{match,[{5,3}]}),
    ?line ok = crtest("abcd\nABCD","ABC",[firstline],true,nomatch),
    ?line ok = crtest("abcd\nABCD","^ABC",[],true,nomatch),
    ?line ok = crtest("abcd\nABCD","^ABC",[multiline],true,{match,[{5,3}]}),
    ?line ok = crtest("abcdABCD","(ABC)",[],true,{match,[{4,3},{4,3}]}),
    ?line ok = crtest("abcdABCD","(ABC)",[no_auto_capture],true,{match,[{4,3}]}),
    ?line ok = crtest(notused,"(?<FOO>ABC)|(?<FOO>DEF)",[],false,notused),
    ?line ok = crtest("abcdABCD","(?<FOO>ABC)|(?<FOO>DEF)",[dupnames],true,{match,[{4,3},{4,3}]}),
    ?line ok = crtest("abcdABCDabcABCD","abcd.*D",[],true,{match,[{0,15}]}),
    ?line ok = crtest("abcdABCDabcABCD","abcd.*D",[ungreedy],true,{match,[{0,8}]}),
    ?line ok = ctest("abcdABCabcABC\nD","abcd.*D",[],true,nomatch),
    ?line ok = crtest("abcdABCabcABC\nD","abcd.*D",[{newline,cr}],true,{match,[{0,15}]}),
    ?line ok = crtest("abcdABCabcABC\rD","abcd.*D",[],true,{match,[{0,15}]}),
    ?line ok = crtest("abcdABCabcABC\rD","abcd.*D",[{newline,lf}],true,{match,[{0,15}]}),
    ?line ok = crtest("abcdABCabcd\r\n","abcd$",[{newline,lf}],true,nomatch),
    ?line ok = crtest("abcdABCabcd\r\n","abcd$",[{newline,cr}],true,nomatch),
    ?line ok = crtest("abcdABCabcd\r\n","abcd$",[{newline,crlf}],true,{match,[{7,4}]}),
    
    ?line ok = crtest("abcdABCabcd\r","abcd$",[{newline,crlf}],true,nomatch),
    ?line ok = crtest("abcdABCabcd\n","abcd$",[{newline,crlf}],true,nomatch),
    ?line ok = crtest("abcdABCabcd\r\n","abcd$",[{newline,anycrlf}],true,{match,[{7,4}]}),
    
    ?line ok = crtest("abcdABCabcd\r","abcd$",[{newline,anycrlf}],true,{match,[{7,4}]}),
    ?line ok = crtest("abcdABCabcd\n","abcd$",[{newline,anycrlf}],true,{match,[{7,4}]}),

    ?line ok = crtest("abcdABCabcd\r\n","abcd$",[{newline,anycrlf},{capture,all,list}],true,{match,["abcd"]}),
    
    ?line ok = crtest("abcdABCabcd\r","abcd$",[{newline,anycrlf},{capture,all,list}],true,{match,["abcd"]}),
    
    ?line ok = crtest("abcdABCabcd\n","abcd$",[{newline,anycrlf},{capture,all,list}],true,{match,["abcd"]}),
    
    ?line ok = crtest("abcdABCabcd\r\n","abcd$",[{newline,anycrlf},{capture,all,binary}],true,{match,[<<"abcd">>]}),
    
    ?line ok = crtest("abcdABCabcd\r","abcd$",[{newline,anycrlf},{capture,all,binary}],true,{match,[<<"abcd">>]}),
    ?line ok = crtest("abcdABCabcd\n","abcd$",[{newline,anycrlf},{capture,all,binary}],true,{match,[<<"abcd">>]}),
    
    % Check that unique run-options fail in compile only case:
    ?line {'EXIT',{badarg,_}} = (catch re:compile("abcd$",[{newline,anycrlf},{capture,all,binary}])),
    ?line {'EXIT',{badarg,_}} = (catch re:compile("abcd$",[{newline,anycrlf},{offset,3}])),
    ?line {'EXIT',{badarg,_}} = (catch re:compile("abcd$",[{newline,anycrlf},notempty])),
    ?line {'EXIT',{badarg,_}} = (catch re:compile("abcd$",[{newline,anycrlf},notbol])),
    ?line {'EXIT',{badarg,_}} = (catch re:compile("abcd$",[{newline,anycrlf},noteol])),


    ?line {match,_} = re:run("abcdABCabcd\r\n","abcd$",[{newline,crlf}]),
    ?line nomatch = re:run("abcdABCabcd\r\nefgh","abcd$",[{newline,crlf}]),
    ?line {match,_} = re:run("abcdABCabcd\r\nefgh","abcd$",[{newline,crlf},multiline]),
    ?line nomatch = re:run("abcdABCabcd\r\nefgh","efgh$",[{newline,crlf},multiline,noteol]),
    ?line {match,_} = re:run("abcdABCabcd\r\nefgh","abcd$",[{newline,crlf},multiline,noteol]),
    ?line {match,_} = re:run("abcdABCabcd\r\nefgh","^abcd",[{newline,crlf},multiline,noteol]),
    ?line nomatch = re:run("abcdABCabcd\r\nefgh","^abcd",[{newline,crlf},multiline,notbol]),
    ?line {match,_} = re:run("abcdABCabcd\r\nefgh","^efgh",[{newline,crlf},multiline,notbol]),
    ?line {match,_} = re:run("ABC\nD","[a-z]*",[{newline,crlf}]),
    ?line nomatch = re:run("ABC\nD","[a-z]*",[{newline,crlf},notempty]),
    ok.

replace_autogen(doc) ->    
    ["Test replace with autogenerated erlang module"];
replace_autogen(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    re_testoutput1_replacement_test:run(),
    ?t:timetrap_cancel(Dog),
    ok.

global_capture(doc) ->    
    ["Tests capture options together with global searching"];
global_capture(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",".*(?<FOO>abcd).*",[{capture,[1]}]),
    ?line {match,[{10,4}]} = re:run("ABCabcdABCabcdA",".*(?<FOO>abcd).*",[{capture,[1]}]),
    ?line {match,[[{10,4}]]} = re:run("ABCabcdABCabcdA",".*(?<FOO>abcd).*",[global,{capture,[1]}]),
    ?line {match,[{3,4}]} = re:run("ABCabcdABC",".*(?<FOO>abcd).*",[{capture,['FOO']}]),
    ?line {match,[{10,4}]} = re:run("ABCabcdABCabcdA",".*(?<FOO>abcd).*",[{capture,['FOO']}]),
    ?line {match,[[{10,4}]]} = re:run("ABCabcdABCabcdA",".*(?<FOO>abcd).*",[global,{capture,['FOO']}]),
    ?line {match,[[{3,4},{3,4}],[{10,4},{10,4}]]} = re:run("ABCabcdABCabcdA","(?<FOO>abcd)",[global]),
    ?line {match,[[{3,4},{3,4}],[{10,4},{10,4}]]} = re:run("ABCabcdABCabcdA","(?<FOO>abcd)",[global,{capture,all}]),
    ?line {match,[[{3,4},{3,4}],[{10,4},{10,4}]]} = re:run("ABCabcdABCabcdA","(?<FOO>abcd)",[global,{capture,all,index}]),
    ?line {match,[[{3,4}],[{10,4}]]} = re:run("ABCabcdABCabcdA","(?<FOO>abcd)",[global,{capture,first}]),
    ?line {match,[[{3,4}],[{10,4}]]} = re:run("ABCabcdABCabcdA","(?<FOO>abcd)",[global,{capture,all_but_first}]),
    ?line {match,[[<<"bcd">>],[<<"bcd">>]]} = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,all_but_first,binary}]),
    ?line {match,[["bcd"],["bcd"]]} = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,all_but_first,list}]),
    ?line {match,[["abcd","bcd"],["abcd","bcd"]]} = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,all,list}]),
    ?line {match,[[<<"abcd">>,<<"bcd">>],[<<"abcd">>,<<"bcd">>]]} = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,all,binary}]),
    ?line {match,[[{3,4},{4,3}],[{10,4},{11,3}]]} = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,all,index}]),
    ?line match = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,none,index}]),
    ?line match = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,none,binary}]),
    ?line match = re:run("ABCabcdABCabcdA","a(?<FOO>bcd)",[global,{capture,none,list}]),
    ?line {match,[[<<195,133,98,99,100>>,<<"bcd">>],[<<"abcd">>,<<"bcd">>]]} = re:run("ABCÅbcdABCabcdA",".(?<FOO>bcd)",[global,{capture,all,binary},unicode]),
    ?line {match,[["Åbcd","bcd"],["abcd","bcd"]]} = re:run(<<"ABC",8#303,8#205,"bcdABCabcdA">>,".(?<FOO>bcd)",[global,{capture,all,list},unicode]),
    ?line {match,[["Åbcd","bcd"],["abcd","bcd"]]} = re:run("ABCÅbcdABCabcdA",".(?<FOO>bcd)",[global,{capture,all,list},unicode]),
    ?line {match,[[{3,5},{5,3}],[{11,4},{12,3}]]} = re:run("ABCÅbcdABCabcdA",".(?<FOO>bcd)",[global,{capture,all,index},unicode]),
    ?t:timetrap_cancel(Dog),
    ok.

replace_input_types(doc) ->
    ["Tests replace with different input types"];
replace_input_types(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    ?line <<"abcd">> = re:replace("abcd","Z","X",[{return,binary},unicode]),
    ?line <<"abcd">> = re:replace("abcd","\x{400}","X",[{return,binary},unicode]),
    ?line <<"a",208,128,"cd">> = re:replace(<<"abcd">>,"b","\x{400}",[{return,binary},unicode]),
    ?t:timetrap_cancel(Dog),
    ok.

replace_return(doc) ->    
    ["Tests return options of replace together with global searching"];
replace_return(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    ?line {'EXIT',{badarg,_}} = (catch re:replace("na","(a","")),
    ?line <<"nasse">> = re:replace(<<"nisse">>,"i","a",[{return,binary}]),
    ?line <<"ABCÅXABCXA">> = re:replace("ABC\305abcdABCabcdA","a(?<FOO>bcd)","X",[global,{return,binary}]),
    
    ?line [<<"ABCÅ">>,
	   <<"X">>,
	   <<"ABC">>,
	   <<"X">> | 
	   <<"A">> ] = 
	re:replace("ABCÅabcdABCabcdA","a(?<FOO>bcd)","X",[global,{return,iodata}]),
    ?line "ABCÅXABCXA" = re:replace("ABCÅabcdABCabcdA","a(?<FOO>bcd)","X",[global,{return,list},unicode]),
    ?line <<65,66,67,195,133,88,65,66,67,88,65>> = re:replace("ABCÅabcdABCabcdA","a(?<FOO>bcd)","X",[global,{return,binary},unicode]),
    ?line <<65,66,67,195,133,88,65,66,67,97,98,99,100,65>> = re:replace("ABCÅabcdABCabcdA","a(?<FOO>bcd)","X",[{return,binary},unicode]),
    ?line <<"iXk">> = re:replace("abcdefghijk","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\9X",[{return,binary}]),
    ?line <<"jXk">> = re:replace("abcdefghijk","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\10X",[{return,binary}]),
    ?line <<"Xk">> = re:replace("abcdefghijk","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\11X",[{return,binary}]),
    ?line <<"9X1">> = re:replace("12345678901","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\g9X",[{return,binary}]),
    ?line <<"0X1">> = re:replace("12345678901","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\g10X",[{return,binary}]),
    ?line <<"X1">> = re:replace("12345678901","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\g11X",[{return,binary}]),
    ?line <<"971">> = re:replace("12345678901","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\g{9}7",[{return,binary}]),
    ?line <<"071">> = re:replace("12345678901","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\g{10}7",[{return,binary}]),
    ?line <<"71">> = re:replace("12345678901","(.)(.)(.)(.)(.)(.)(.)(.)(.)(.)","\\g{11}7",[{return,binary}]),
    ?line "a\x{400}bcX" = re:replace("a\x{400}bcd","d","X",[global,{return,list},unicode]),
    ?line <<"a",208,128,"bcX">> = re:replace("a\x{400}bcd","d","X",[global,{return,binary},unicode]),
    ?line "a\x{400}bcd" = re:replace("a\x{400}bcd","Z","X",[global,{return,list},unicode]),
    ?line <<"a",208,128,"bcd">> = re:replace("a\x{400}bcd","Z","X",[global,{return,binary},unicode]),
    ?t:timetrap_cancel(Dog),
    ok.

rtest(Subj, RE, Copt, Ropt, true) ->
    {ok,MP} = re:compile(RE,Copt), 
    {match,_} = re:run(Subj,MP,Ropt),
    ok;
rtest(Subj, RE, Copt, Ropt, false) ->
    {ok,MP} = re:compile(RE,Copt), 
    nomatch = re:run(Subj,MP,Ropt),
    ok.

ctest(_,RE,Options,false,_) ->
    case re:compile(RE,Options) of
	{ok,_} ->
	    error;
	{error,_} ->
	    ok
    end;
ctest(Subject,RE,Options,true,Result) ->
    try
	{ok, Prog} = re:compile(RE,Options),
	Result = re:run(Subject,Prog,[]),
	ok
    catch
	_:_ ->
	    error
    end.
crtest(_,RE,Options,false,_) ->
    case (catch re:run("",RE,Options)) of
	{'EXIT',{badarg,_}} ->
	    ok;
	_ ->
	    error
    end;
crtest(Subject,RE,Options,true,Result) ->
    try
	Result = re:run(Subject,RE,Options),
	ok
    catch
	_:_ ->
	    error
    end.

split_autogen(doc) ->    
    ["Test split with autogenerated erlang module"];
split_autogen(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(3)),
    re_testoutput1_split_test:run(),
    ?t:timetrap_cancel(Dog),
    ok.

split_options(doc) ->
    ["Test special options to split."];
split_options(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(1)),
    ?line [[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],[<<"c">>,<<" ">>]] = re:split("a b c ","( )",[group,trim]),
    ?line [[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],[<<"c">>,<<" ">>]] = re:split("a b c ","( )",[group,{parts,0}]),
    ?line [[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],[<<"c">>,<<" ">>],[<<>>]] = 
	re:split("a b c ","( )",[{parts,infinity},group]),
    ?line [[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],[<<"c">>,<<" ">>],[<<>>]] = 
	re:split("a b c ","( )",[group]),
    ?line [[<<>>,<<" ">>],[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],
	   [<<"c">>,<<" ">>],[<<"d">>,<<" ">>]] = 
	re:split(" a b c d ","( +)",[group,trim]),
    ?line [[<<>>,<<" ">>],[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],
	   [<<"c">>,<<" ">>],[<<"d">>,<<" ">>]] = 
	re:split(" a b c d ","( +)",[{parts,0},group]),
    ?line [[<<>>,<<" ">>],[<<"a">>,<<" ">>],[<<"b">>,<<" ">>],
	   [<<"c">>,<<" ">>],[<<"d">>,<<" ">>],[<<>>]] =  
	re:split(" a b c d ","( +)",[{parts,infinity},group]),
    ?line [[<<"a">>,<<" ">>],[<<"b c d">>]] =
	re:split("a b c d","( +)",[{parts,2},group]),
    ?line [[[967]," "],["b c d"]] = 
	re:split([967]++" b c d","( +)",
		 [{parts,2},group,{return,list},unicode]),    
    ?line [[<<207,135>>,<<" ">>],[<<"b c d">>]] = 
	re:split([967]++" b c d","( +)",
		 [{parts,2},group,{return,binary},unicode]),
    ?line {'EXIT',{badarg,_}} = 
	(catch re:split([967]++" b c d","( +)",
		       [{parts,2},group,{return,binary}])),
    ?line {'EXIT',{badarg,_}} = 
	(catch re:split("a b c d","( +)",[{parts,-2}])),
    ?line {'EXIT',{badarg,_}} = 
	(catch re:split("a b c d","( +)",[{parts,banan}])),
    ?line {'EXIT',{badarg,_}} = 
	(catch re:split("a b c d","( +)",[{capture,all}])),
    ?line {'EXIT',{badarg,_}} = 
	(catch re:split("a b c d","( +)",[{capture,[],binary}])),
    % Parts 0 is equal to no parts specification (implicit strip)
    ?line ["a"," ","b"," ","c"," ","d"] = 
	re:split("a b c d","( *)",[{parts,0},{return,list}]),
    ?t:timetrap_cancel(Dog),
    ok.
    
join([]) -> [];
join([A]) -> [A];
join([H|T]) -> [H,<<":">>|join(T)].

split_specials(doc) ->
    ["Some special cases of split that are easy to get wrong."];
split_specials(Config) when is_list(Config) ->
    %% More or less just to remember these icky cases
    Dog = ?t:timetrap(?t:minutes(1)),
    ?line <<"::abd:f">> = 
	iolist_to_binary(join(re:split("abdf","^(?!(ab)de|x)(abd)(f)",[trim]))),
    ?line <<":abc2xyzabc3">> = 
	iolist_to_binary(join(re:split("abc1abc2xyzabc3","\\Aabc.",[trim]))),
    ?t:timetrap_cancel(Dog),
    ok.
    

%% Test that errors are handled correctly by the erlang code.
error_handling(_Config) ->
    case test_server:is_native(re) of
	true ->
	    %% Exceptions from native code look too different.
	    {skip,"re is native"};
	false ->
	    error_handling()
    end.
	    
error_handling() ->
    % This test checks the exception tuples manufactured in the erlang
    % code to hide the trapping from the user at least when it comes to errors
    Dog = ?t:timetrap(?t:minutes(1)),
    % The malformed precomiled RE is detected after 
    % the trap to re:grun from grun, in the grun function clause
    % that handles precompiled expressions
    ?line {'EXIT',{badarg,[{re,run,["apa",{1,2,3,4},[global]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:run("apa",{1,2,3,4},[global])),
    % An invalid capture list will also cause a badarg late, 
    % but with a non pre compiled RE, the exception should be thrown by the
    % grun function clause that handles RE's compiled implicitly by
    % the run/3 BIF before trapping.
    ?line {'EXIT',{badarg,[{re,run,["apa","p",[{capture,[1,{a}]},global]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:run("apa","p",[{capture,[1,{a}]},global])),
    % And so the case of a precompiled expression together with
    % a compile-option (binary and list subject):
    ?line {ok,RE} = re:compile("(p)"),
    ?line {match,[[{1,1},{1,1}]]} = re:run(<<"apa">>,RE,[global]),
    ?line {match,[[{1,1},{1,1}]]} = re:run("apa",RE,[global]),
    ?line {'EXIT',{badarg,[{re,run,
			    [<<"apa">>,
			     {re_pattern,1,0,_},
			     [global,unicode]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:run(<<"apa">>,RE,[global,unicode])),
    ?line {'EXIT',{badarg,[{re,run,
			    ["apa",
			     {re_pattern,1,0,_},
			     [global,unicode]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:run("apa",RE,[global,unicode])),
    ?line {'EXIT',{badarg,_}} = (catch re:run("apa","(p",[])),
    ?line {'EXIT',{badarg,_}} = (catch re:run("apa","(p",[global])),
    % The replace errors:
    ?line {'EXIT',{badarg,[{re,replace,["apa",{1,2,3,4},"X",[]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:replace("apa",{1,2,3,4},"X",[])),
    ?line {'EXIT',{badarg,[{re,replace,["apa",{1,2,3,4},"X",[global]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:replace("apa",{1,2,3,4},"X",[global])),
    ?line {'EXIT',{badarg,[{re,replace,
			    ["apa",
			     {re_pattern,1,0,_},
			     "X",
			     [unicode]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:replace("apa",RE,"X",[unicode])),
    ?line <<"aXa">> = iolist_to_binary(re:replace("apa","p","X",[])),
    ?line {'EXIT',{badarg,[{re,replace,
			    ["apa","p","X",[{capture,all,binary}]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch iolist_to_binary(re:replace("apa","p","X",
					  [{capture,all,binary}]))),
    ?line {'EXIT',{badarg,[{re,replace,
			    ["apa","p","X",[{capture,all}]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch iolist_to_binary(re:replace("apa","p","X",
					  [{capture,all}]))),
    ?line {'EXIT',{badarg,[{re,replace,
			    ["apa","p","X",[{return,banana}]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch iolist_to_binary(re:replace("apa","p","X",
					  [{return,banana}]))),
    ?line {'EXIT',{badarg,_}} = (catch re:replace("apa","(p","X",[])),
    % Badarg, not compile error.
    ?line {'EXIT',{badarg,[{re,replace,
			    ["apa","(p","X",[{return,banana}]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch iolist_to_binary(re:replace("apa","(p","X",
					  [{return,banana}]))),
    % And the split errors:
    ?line [<<"a">>,<<"a">>] = (catch re:split("apa","p",[])),
    ?line [<<"a">>,<<"p">>,<<"a">>] = (catch re:split("apa",RE,[])),
    ?line {'EXIT',{badarg,[{re,split,["apa","p",[global]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa","p",[global])),
    ?line {'EXIT',{badarg,[{re,split,["apa","p",[{capture,all}]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa","p",[{capture,all}])),
    ?line {'EXIT',{badarg,[{re,split,["apa","p",[{capture,all,binary}]],_},
			   {?MODULE, error_handling,0,_} | _]}} =
	(catch re:split("apa","p",[{capture,all,binary}])),
    ?line {'EXIT',{badarg,[{re,split,["apa",{1,2,3,4},[]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa",{1,2,3,4})),
    ?line {'EXIT',{badarg,[{re,split,["apa",{1,2,3,4},[]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa",{1,2,3,4},[])),
    ?line {'EXIT',{badarg,[{re,split,
			    ["apa",
			     RE,
			     [unicode]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa",RE,[unicode])),
    ?line {'EXIT',{badarg,[{re,split,
			    ["apa",
			     RE,
			     [{return,banana}]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa",RE,[{return,banana}])),
    ?line {'EXIT',{badarg,[{re,split,
			    ["apa",
			     RE,
			     [banana]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa",RE,[banana])),
    ?line {'EXIT',{badarg,_}} = (catch re:split("apa","(p")),
    %Exception on bad argument, not compilation error
    ?line {'EXIT',{badarg,[{re,split,
			    ["apa",
			     "(p",
			     [banana]],_},
			   {?MODULE,error_handling,0,_} | _]}} =
	(catch re:split("apa","(p",[banana])),
    ?t:timetrap_cancel(Dog),
    ok.
    
pcre_cve_2008_2371(doc) ->
    "Fix as in http://vcs.pcre.org/viewvc?revision=360&view=revision";
pcre_cve_2008_2371(Config) when is_list(Config) ->
    %% Make sure it doesn't crash the emulator.
    re:compile(<<"(?i)[\xc3\xa9\xc3\xbd]|[\xc3\xa9\xc3\xbdA]">>, [unicode]),
    ok.

pcre_compile_workspace_overflow(doc) ->
    "Patch from http://vcs.pcre.org/viewvc/code/trunk/pcre_compile.c?r1=504&r2=505&view=patch";
pcre_compile_workspace_overflow(Config) when is_list(Config) ->
    N = 819,
    ?line {error,{"internal error: overran compiling workspace",799}} =
	re:compile([lists:duplicate(N, $(), lists:duplicate(N, $))]),
    ok.
re_infinite_loop(doc) ->
    "Make sure matches that really loop infinitely actually fail";
re_infinite_loop(Config) when is_list(Config) ->
    Dog = ?t:timetrap(?t:minutes(1)),
    ?line Str =
        "http:/www.flickr.com/slideShow/index.gne?group_id=&user_id=69845378@N0",
    ?line EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
        ++ "(\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
        ++ "@.*([a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
        ++ "([a-zA-Z]{2}|com|org|net|gov|mil"
        ++ "|biz|info|mobi|name|aero|jobs|museum)",
    ?line nomatch = re:run(Str, EMail_regex),
    ?t:timetrap_cancel(Dog),
    ok.
