%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
-module(run_pcre_tests).
-export([test/1,gen_split_test/1,gen_repl_test/1]).

test(RootDir) ->
    put(verbose,false),
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("oldlimit: ~p~n",[ erts_debug:set_internal_state(re_loop_limit,10)]),
    Testfiles0 = ["testoutput1", "testoutput2", "testoutput3", "testoutput4",
		 "testoutput5", "testoutput6","mod_testoutput8","testoutput10"],
    Testfiles = [ filename:join([RootDir,FN]) || FN <- Testfiles0 ], 
    Res = [ begin io:format("~s~n",[X]), t(X) end || X <- Testfiles ],
    io:format("limit was: ~p~n",[ erts_debug:set_internal_state(re_loop_limit,default)]),
    Res2 = Res ++ [ begin io:format("~s~n",[X]), t(X) end || X <- Testfiles ],
    erts_debug:set_internal_state(available_internal_state,false),
    put(verbose,true),
    Res2.
t(OneFile) ->
    t(OneFile,infinite).
t(OneFile,Num) ->
    {ok,Bin} = file:read_file(OneFile),
    Lines = splitfile(0,Bin,1),
    Structured = stru(Lines),
    put(error_limit,Num),
    put(skipped,0),
    Res = 
	[test(Structured,true,index,false),
	 test(Structured,false,index,false),
	 test(Structured,true,index,true),
	 test(Structured,false,index,true),
	 test(Structured,true,binary,false),
	 test(Structured,false,binary,false),
	 test(Structured,true,list,false),
	 test(Structured,false,list,false)],
    {lists:sum(Res),length(Structured)*6,get(skipped)}.


pick_exec_options([{exec_option,Opt}|T]) ->
    {O,E} = pick_exec_options(T),
    {O,[Opt|E]};
pick_exec_options([unicode|T]) ->
    {O,E} = pick_exec_options(T),
    {[unicode|O],[unicode|E]};
pick_exec_options([Opt|T]) ->
    {O,E} = pick_exec_options(T),
    {[Opt|O],E};
pick_exec_options([]) ->
    {[],[]}.

test([],_,_,_) ->
    0;
test([{RE0,Line,Options0,Tests}|T],PreCompile,XMode,REAsList) ->
    Unicode = lists:member(unicode,Options0),
    RE = case REAsList of
	     true ->
		 if 
		     Unicode -> unicode:characters_to_list(RE0);
		     true -> binary_to_list(RE0)
		 end;
	     false ->
		 RE0
	 end,
    {Options,ExecOptions} = pick_exec_options(Options0),
    {Cres, Xopt} = case PreCompile of
		       true ->
			   {re:compile(RE,Options),[]};
		       _ ->
			   {{ok,RE},Options}
		   end,
    case Cres of
	{ok,P} ->
	    case (catch testrun(RE,P,Tests,ExecOptions,Xopt,XMode)) of
		N when is_integer(N) ->
		    N + test(T,PreCompile,XMode,REAsList);
		limit ->
		    io:format("Error limit reached.~n"),
		    1;
		skip ->
		    case get(skipped) of
			N when is_integer(N) ->
			    put(skipped,N+1);
			_ ->
			    put(skipped,1)
		    end,
		    test(T,PreCompile,XMode,REAsList)
	    end;
	{error,Err} ->
	    io:format("Compile error(~w): ~w~n",[Line,Err]), 
	    case get(error_limit) of
		infinite -> 1 + test(T,PreCompile,XMode,REAsList);
		X ->
		    case X-1 of
			Y when Y =< 0 ->
			    io:format("Error limit reached.~n"),
			    1;
			Y ->
			    put(error_limit,Y),
			    1 + test(T,PreCompile,XMode,REAsList)
		    end
	    end
    end.

contains_eightbit(<<>>) ->
    false;
contains_eightbit(<<X:8,_/binary>>) when X >= 128 ->
    true;
contains_eightbit(<<_,R/binary>>) ->
    contains_eightbit(R).

clean_duplicates([],_) -> 
    [];
clean_duplicates([{X,Y}|T],L) ->
    case lists:keymember(X,1,L) of
	true ->
	    clean_duplicates(T,L);
	false ->
	    [{X,Y}|clean_duplicates(T,L)]
    end;
clean_duplicates([bsr_anycrlf|T],L) ->
    case (lists:member(bsr_anycrlf,L) orelse lists:member(bsr_unicode,L)) of
	true ->
	    clean_duplicates(T,L);
	false ->
	    [bsr_anycrlf|clean_duplicates(T,L)]
    end;
clean_duplicates([bsr_unicode|T],L) ->
    case (lists:member(bsr_anycrlf,L) orelse lists:member(bsr_unicode,L)) of
	true ->
	    clean_duplicates(T,L);
	false ->
	    [bsr_unicode|clean_duplicates(T,L)]
    end;
clean_duplicates([X|T],L) ->
    case lists:member(X,L) of
	true ->
	   clean_duplicates(T,L);
	false ->
	   [X|clean_duplicates(T,L)] 
    end.


press([]) ->
    [];
press([H|T]) ->
    H++press(T).

testrun(_,_,[],_,_,_) ->
    0;
testrun(RE,P,[{Chal,Line,ExecOpt,Responses}|T],EO,Xopt0,XMode) ->
    Xopt = clean_duplicates(Xopt0,ExecOpt),

    case lists:keymember(newline,1,Xopt) of
	true ->
	    info("skipping inconsistent newlines "
		 "when compiling and running in one go (~p)~n",
		 [Line]),
	    throw(skip);
	false ->
	    ok
    end,
	    
    Res = 
	case lists:member(g,EO) of
	    true ->
		case XMode of
		    binary ->
			case re:run(Chal,P,ExecOpt++Xopt++
				    [global,{capture,all,binary}]) of
			    nomatch ->
				nomatch;
			    {match, Reslist} ->
				{match,press([bfix(R)|| R <- Reslist])}
			end;
		    list ->
			case re:run(Chal,P,ExecOpt++Xopt++
				    [global,{capture,all,list}]) of
			    nomatch ->
				nomatch;
			    {match, Reslist} ->
				UFix = lists:member(unicode,EO),
				{match,press([bfix([if UFix =:= true -> list_to_utf8(L); true -> list_to_binary(L) end || L <- R]) || R <- Reslist])}
			end;
		    index ->
			case re:run(Chal,P,ExecOpt++Xopt++[global]) of
			    nomatch ->
				nomatch;
			    {match, Reslist} ->
				{match,press([fixup(Chal,R,0) || R <- Reslist])}
			end
		end;
	    false ->
		case EO -- [accept_nonascii] of
		    EO ->
			case contains_eightbit(Chal) of
			    true ->
				info("skipping 8bit without LANG (~p)~n",
					 [Line]),
				throw(skip);
			    false ->
				ok
			end,

			case XMode of
			    binary ->
				case re:run(Chal,P,ExecOpt++Xopt++
					    [{capture,all,binary}]) of
				    nomatch ->
					nomatch;
				    {match, Reslist} ->
					{match,bfix(Reslist)}
				end;
			    list ->
				case re:run(Chal,P,ExecOpt++Xopt++
					    [{capture,all,list}]) of
				    nomatch ->
					nomatch;
				    {match, Reslist} ->
					UFix = lists:member(unicode,EO),
					{match,bfix([if 
							 UFix =:= true -> list_to_utf8(L); 
							 true -> list_to_binary(L) 
						     end || L <- Reslist])}
				end;
			    index ->
				case re:run(Chal,P,ExecOpt++Xopt) of
				    nomatch ->
					nomatch;
				    {match, Reslist} ->
					{match,fixup(Chal,Reslist,0)}
				end
			end;
		    _LesserOpt ->
			case XMode of
			    binary ->
				case re:run(Chal,P,ExecOpt++Xopt++
					    [{capture,all,binary}]) of
				    nomatch ->
					nomatch;
				    {match, Reslist} ->
					{match,bfix(Reslist)}
				end;
			    list ->
				case re:run(Chal,P,ExecOpt++Xopt++
					    [{capture,all,list}]) of
				    nomatch ->
					nomatch;
				    {match, Reslist} ->
					UFix = lists:member(unicode,EO),
					{match,bfix([if 
							 UFix =:= true -> list_to_utf8(L); 
							 true -> list_to_binary(L) 
						     end || L <- Reslist])}
				end;
			    index ->
				case re:run(Chal,P,ExecOpt++Xopt) of
				    nomatch ->
					nomatch;
				    {match, Reslist} ->
					{match,fixup(Chal,Reslist,0)}
				end
			end
		end
	end,
    case compare_sloppy(Res,Responses) of
	true ->
	    testrun(RE,P,T,EO,Xopt0,XMode);
	false ->
	    io:format("FAIL(~w): re = ~p, ~nmatched against = ~p(~w), ~nwith options = ~p. ~nexpected = ~p, ~ngot = ~p~n", 
		      [Line,RE,Chal,binary_to_list(Chal),{ExecOpt,EO,Xopt},Responses,Res]),
	    case get(error_limit) of
		infinite -> ok;
		X ->
		    case X-1 of
			Y when Y =< 0 ->
			    throw(limit);
			Y ->
			    put(error_limit,Y)
		    end
	    end,
	    1
    end.
compare_sloppy({A,L1},{A,L2}) ->
    compare_sloppy(L1,L2);
compare_sloppy(A,A) ->
    true;
compare_sloppy([{X,Y}|T1],[{X,Y}|T2]) ->
    compare_sloppy(T1,T2);
compare_sloppy([{X,[Y,_]}|T1],[{X,Y}|T2]) ->
    compare_sloppy(T1,T2);
compare_sloppy([{X,[_,Y]}|T1],[{X,Y}|T2]) ->
    compare_sloppy(T1,T2);
compare_sloppy(_,_) ->
    false.

bfix(RL) ->
    bfix(RL,0).
bfix([],_) ->
    [];
bfix([<<>>|T],N) ->
    [{N,[<<>>,<<"<unset>">>]}|bfix(T,N+1)]; % indeterminable
bfix([H|T],N) ->
    [{N,H}|bfix(T,N+1)].

fixup(List,Any,Any2) when is_list(List)->
    fixup(unicode:characters_to_binary(List,unicode,unicode),Any,Any2);
fixup(_,[],_) ->
    [];
fixup(Bin,[{-1,0}|T],N) ->
    [{N,<<"<unset>">>}|fixup(Bin,T,N+1)];
fixup(Bin,[{X,Y}|T],N) ->
    <<_:X/binary,Res:Y/binary,_/binary>> = Bin,
    [{N,Res}|fixup(Bin,T,N+1)].

splitfile(N,Bin,_Line) when N >= size(Bin) ->
    [];
splitfile(N,Bin,Line) ->
    {Res,NewN} = pickline(N,N,Bin),
    case emptyline(Res) of
	true ->
	    [{Line,<<>>}|splitfile(NewN,Bin,Line+1)];
	false ->
	    [{Line,Res}|splitfile(NewN,Bin,Line+1)]
    end.

emptyline(<<>>) ->
    true;
emptyline(<<$ ,R/binary>>) ->
    emptyline(R);
emptyline(_) ->
    false.
pickline(Start,Stop,Bin) when Stop >= size(Bin) ->
    Len = Stop - Start - 1,
    <<_:Start/binary,Res:Len/binary,_/binary>> = Bin,
    {Res,Stop};
    
pickline(Start,Stop,Bin) ->
    <<_:Stop/binary,Ch,_/binary>> = Bin,
    case Ch of
	$\n ->
	    Len = Stop - Start,
	    <<_:Start/binary,Res:Len/binary,_/binary>> = Bin,
	    {Res,Stop+1};
	_ ->
	    pickline(Start,Stop+1,Bin)
    end.

skip_until_empty([]) ->
    [];
skip_until_empty([{_,<<>>}|T]) ->
    T;
skip_until_empty([{_,_}|T]) ->
    skip_until_empty(T).

skip_debug([{_,<<$-,_/binary>>}|Con]) ->
    Con;
skip_debug([_|T]) ->
    skip_debug(T);
skip_debug([]) ->
    [].

skip_extra_info([{_,<<$ ,$ ,$ ,_/binary>>}=H|Con]) ->
    [H|Con];
skip_extra_info([{_,<<>>}|Con]) ->
    Con;
skip_extra_info([_|T]) ->
    skip_extra_info(T);
skip_extra_info([]) ->
    [].

stru([]) ->
    [];
stru([{_,<<>>}|T]) ->
    stru(T);
stru([{_Line,<<"< forbid ", _Rest/binary>>}|T0]) ->
    %% We do not handle lockout of modifiers from the tests...
    stru(T0);
stru([{Line,<<Ch,Re0/binary>>}|T0]) ->
    {T,Re} = find_rest_re(Ch,[{Line,Re0}|T0]),
    {NewRe,<< Ch, Options/binary >>} = end_of_re(Ch,Re),
    case interpret_options_x(backstrip(frontstrip(Options)),NewRe) of
	{Olist,<<>>} -> 
	    U = lists:member(unicode,Olist),
	    case T of
		[{_,<<$-,_/binary>>}|Con] ->
		    %%Debug output, we skip those
		    TmpT = skip_debug(Con),
		    {NewT,Matches} = stru2(TmpT,U),
		    [{NewRe,Line,Olist,Matches}|stru(NewT)];
		[{_,<<$C,$a,$p,$t,$u,$r,$i,$n,$g,_/binary>>}|_] ->
		    NewT0 = skip_extra_info(T),
		    {NewT,Matches} = stru2(NewT0,U),
		    [{NewRe,Line,Olist,Matches}|stru(NewT)];
		[{_,<<Bla,_/binary>>}|_] when Bla =/= $  ->
		    NewT = skip_until_empty(T),
		    stru(NewT);
		_ ->
		    {NewT,Matches} = stru2(T,U),
		    Matches1 = case U of
				   true ->
				       Matches ++
					   [ {unicode:characters_to_list(E1,unicode),E2,E3,E4} ||
					       {E1,E2,E3,E4} <- Matches];
				   false ->
				       Matches
			       end,
		    [{NewRe,Line,Olist,Matches1}|stru(NewT)]
	    end;
	{_,Rest} ->
	    NewT = skip_until_empty(T),
	    info("Skipping options ~s for now (~w)~n",[binary_to_list(Rest),Line]),
	    case NewT of
		[{Li,_}|_] ->
		    info("Skip to line ~p~n",[Li]);
		_ ->
		    ok
	    end,
	    stru(NewT)
    end.

contains_lang_sens(<<>>) ->
    false;
contains_lang_sens(<<$\\,$W,_/binary>>) ->
    true;
contains_lang_sens(<<$\\,$w,_/binary>>) ->
    true;
contains_lang_sens(<<$\\,$b,_/binary>>) ->
    true;
contains_lang_sens(<<_,R/binary>>) ->
    contains_lang_sens(R).


interpret_options_x(Options,RE) ->
    {O,R} = interpret_options(Options),
    case (contains_lang_sens(RE) or lists:member(caseless,O)) of
	false ->
	    {[{exec_option,accept_nonascii}|O],R};
	true ->
	    case lists:member(unicode,O) of
		true ->
		    {[{exec_option,accept_nonascii}|O],R};
		false ->
		    {O,R}
	    end
    end.
tr_option($i) ->
    [caseless];
tr_option($I) ->
    [];
tr_option($B) ->
    [];
tr_option($Z) ->
    [];
tr_option($x) ->
    [extended];
tr_option($s) ->
    [dotall];
tr_option($m) ->
    [multiline];
tr_option($J) ->
    [dupnames];
tr_option($N) ->
    [no_auto_capture];
tr_option($8) ->
    [unicode];
tr_option($U) ->
    [ungreedy];
tr_option($g) ->
    [{exec_option,g}];
tr_option(_) ->
    false.


interpret_options(<<$<,Rest0/binary>>) ->
    {Option,Rest} = pinch_cr(Rest0),
    case Option of
	{not_supported,{newline,_Offender}} ->
	    {[],<<$<,Rest0/binary>>};
	_ ->
	    {Olist,NRest} = interpret_options(Rest),
	    {[Option | Olist], NRest}
    end;
interpret_options(<<$L,$f,$r,$_,$F,$R,Rest/binary>>) ->
    info("Accepting (and ignoring) french locale~n",[]),
    {Olist,NRest} = interpret_options(Rest),
    {[{exec_option, accept_nonascii}|Olist],NRest};
interpret_options(<<Ch,Rest/binary>>) ->
    {Olist,NRest} = interpret_options(Rest),
    case tr_option(Ch) of
	false ->
	    {Olist,<<Ch,NRest/binary>>};
	Option ->
	     {Option ++ Olist, NRest}
    end;
interpret_options(<<>>) ->
    {[],<<>>}.

find_unsupported([{not_supported,X}|T]) ->
    [X | find_unsupported(T)];
find_unsupported([_|T]) ->
    find_unsupported(T);
find_unsupported([]) ->
    [].

backslash_end(<<>>) ->
    false;
backslash_end(<<$\\>>) ->
    true;
backslash_end(<<_>>) ->
    false;
backslash_end(<<_,R/binary>>) ->
    backslash_end(R).

stru2([{Line,<<$ ,Rest/binary>>} | T],U) ->
    %% A challenge
    case  (catch responses(T,U)) of
	{NewT,Rlist} ->
	    {NewNewT,StrList} = stru2(NewT,U),
	    %% Hack...
	    FS = case backstrip(frontstrip(Rest)) of
		     <<"\\">> -> 
			 %% Single backslash is to be considered 
			 %% an empty line in challenge
			 <<>>;
		     OFS ->
			 case backslash_end(OFS) of
			     true ->
				 <<OFS/binary,$ >>;
			     _ ->
				 OFS
			 end
		 end,
	    {ExecOpts,NFS} = escape(FS,U),
	    case find_unsupported(ExecOpts) of
		[] ->
		    {NewNewT,[{NFS,Line,ExecOpts,
			       case 
				   Rlist of nomatch -> nomatch; 
				   RR -> {match,RR} 
			       end} | StrList]};
		UList ->
		    info("WARNING(~w): the exec-option(s) ~p are unsupported, skipping challenge.~n",[Line,UList]),
		    {NewNewT,StrList}
	    end;
	fail ->
	    NewT = skip_until_empty(T),
	    {NewT,[]}
    end;
	
stru2(X,_) ->
    {X,[]}.

responses([{_Line,<< X:2/binary,$:,$ ,Resp/binary>>}|T],U) ->
    {NT,R2} = responses(T,U),
    NX=list_to_integer(binary_to_list(frontstrip(X))),
    {NT,[{NX,escape2(Resp,U)} | R2]};
responses([{_Line,<< X:3/binary,$:,$ ,Resp/binary>>}|T],U) ->
    {NT,R2} = responses(T,U),
    NX=list_to_integer(binary_to_list(frontstrip(X))),
    {NT,[{NX,escape2(Resp,U)} | R2]};
responses([{_Line,<<$N,$o,$ ,$m,$a,$t,$c,$h,_/binary>>}|T],_) ->
    {T,nomatch};
responses([{Line,<<$ ,No,Ch,_/binary>>}|T],U) when No >= $0, No =< $9, Ch >= $A, Ch =< $Z ->
    info("Skipping strange debug response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<$ ,$ ,Ch,_/binary>>}|T],U) when Ch =:= $G; Ch =:= $C ->
    info("Skipping stranger debug response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<C,_/binary>>=X}|_],_) when C =/= $  ->
    info("Offending response line(~w)! ~p~n",[Line,X]),
    throw(fail);
responses(X,_) ->
    {X,[]}.
    

end_of_re(_,<<>>) ->
    {<<>>,<<>>};
end_of_re(C,<<C,_Rest/binary>> = R) ->
    {<<>>,R};
end_of_re(C,<<$\\,C,Rest/binary>>) ->
    {Sub,Rest2} = end_of_re(C,Rest),
    {<<C,Sub/binary>>,Rest2};
end_of_re(C,<<Ch,Rest/binary>>) ->
    {Sub,Rest2} = end_of_re(C,Rest),
    {<<Ch,Sub/binary>>,Rest2}.

frontstrip(<<>>) ->
    <<>>;
frontstrip(<< $ ,Rest/binary>>) ->
    frontstrip(Rest);
frontstrip(Bin) ->
    Bin.

backstrip(<<>>) ->
    <<>>;
backstrip(<<$ >>) ->
    <<>>;
backstrip(<<X,Rest/binary>>) ->
    case backstrip(Rest) of
	Rest ->
	    <<X,Rest/binary>>; 
	Other ->
	    NRest = backstrip(Other),
	    <<X,NRest/binary>>
    end.

find_rest_re(_,[]) ->
    {<<>>,<<>>};
find_rest_re(Ch,[{_,H}|T]) -> 	    
    case end_of_re(Ch,H) of
	{_,<<>>} ->
	    {NewT,Rest} = find_rest_re(Ch,T),
	    {NewT,<<H/binary,$\n,Rest/binary>>};
	{_,_} ->
	    {T,H}
    end.

eopt($A) ->
    [anchored];
eopt($B) ->
    [notbol];
eopt(X) ->
    [{not_supported,X}].

pinch_cr(<<$c,$r,$>,Rest/binary>>) ->
    {{newline,cr},Rest};
pinch_cr(<<$l,$f,$>,Rest/binary>>) ->
    {{newline,lf},Rest};
pinch_cr(<<$c,$r,$l,$f,$>,Rest/binary>>) ->
    {{newline,crlf},Rest};
pinch_cr(<<$C,$R,$>,Rest/binary>>) ->
    {{newline,cr},Rest};
pinch_cr(<<$L,$F,$>,Rest/binary>>) ->
    {{newline,lf},Rest};
pinch_cr(<<$C,$R,$L,$F,$>,Rest/binary>>) ->
    {{newline,crlf},Rest};
pinch_cr(<<$a,$n,$y,$c,$r,$l,$f,$>,Rest/binary>>) ->
    {{newline,anycrlf},Rest};
pinch_cr(<<$b,$s,$r,$_,$a,$n,$y,$c,$r,$l,$f,$>,Rest/binary>>) ->
    {bsr_anycrlf,Rest};
pinch_cr(<<$b,$s,$r,$_,$u,$n,$i,$c,$o,$d,$e,$>,Rest/binary>>) ->
    {bsr_unicode,Rest};
pinch_cr(<<$a,$n,$y,$>,Rest/binary>>) ->
    {{newline,any},Rest};
pinch_cr(<<$A,$N,$Y,$>,Rest/binary>>) ->
    {{newline,any},Rest};
pinch_cr(Other) ->
    case splitby($>,Other,<<>>) of
	{Unk,Rest} ->
	    {{not_supported,{newline,Unk}},Rest};
	no ->
	    {{not_supported,$<},Other}
    end.
    
splitby(_,<<>>,_) ->
    no;
splitby(Ch,<<Ch,Rest/binary>>,Acc) ->
    {Acc,Rest};
splitby(Ch,<<OCh,Rest/binary>>,Acc) ->
    splitby(Ch,Rest,<<Acc/binary,OCh>>).

pick_number(N,<<Ch:8,Rest/binary>>) when Ch >= $0, Ch =< $9 ->
    pick_number(N*10+(Ch - $0),Rest);
pick_number(N,Rest) ->
    {N,Rest}.

pick_offset(Rest) ->
    {Int,NRest} = pick_number(0,Rest),
    {{offset,Int},NRest}.
	    

escape(<<>>,_) ->
    {[],<<>>};
escape(<<$\\, Ch, Rest/binary>>,U) when Ch >= $A, Ch =< $Z; Ch =:= $? ->
    %%Options in the string...
    NewOpts = eopt(Ch),
    {MoreOpts,Tail} = escape(Rest,U),
    {NewOpts ++ MoreOpts,Tail};
escape(<<$\\, $>, Rest/binary>>,U) ->
    %%Offset Options in the string...
    {NewOpt,NewRest} = pick_offset(Rest),
    {MoreOpts,Tail} = escape(NewRest,U),
    {[NewOpt|MoreOpts],Tail};
escape(<<$\\, $<, Rest/binary>>,U) ->
    %%CR Options in the string...
    {NewOpt,NewRest} = pinch_cr(Rest),
    {MoreOpts,Tail} = escape(NewRest,U),
    {[NewOpt|MoreOpts],Tail};
escape(<<$\\, Ch, Rest/binary>>,U) ->
    {C,NR} = case single_esc(Ch) of
		 no ->
		     case multi_esc(<<Ch,Rest/binary>>,U) of
			 {CharBin,NewRest} ->
			     {CharBin,NewRest};
			 no ->
			     {<<$\\>>,<<Ch,Rest/binary>>}
		     end;
		 CCC ->
		     {<<CCC>>,Rest}
	     end,
    {MoreOpts,Tail} = escape(NR,U),
    {MoreOpts,<<C/binary,Tail/binary>>};
escape(<<Ch,Rest/binary>>,U) ->
    {X,RR} = escape(<<Rest/binary>>,U),
    {X,<<Ch,RR/binary>>};
escape(Any,_) ->
    {[],Any}.
escape2(<<>>,_) ->
    <<>>;
escape2(<<$\\, Ch, Rest/binary>>,U) ->
    {C,NR} = case multi_esc(<<Ch,Rest/binary>>,U) of
		 {CharBin,NewRest} ->
		     {CharBin,NewRest};
		 no ->
		     {<<$\\>>,<<Ch,Rest/binary>>}
	     end,
    Tail = escape2(NR,U),
    <<C/binary,Tail/binary>>;
escape2(<<Ch,Rest/binary>>,U) ->
    RR = escape2(<<Rest/binary>>,U),
    <<Ch,RR/binary>>;
escape2(Any,_) ->
    Any.


trx(N) when ((N >= $0) and (N =< $9)) ->
    N - $0;
trx($A) -> 10;
trx($B) -> 11;
trx($C) -> 12;
trx($D) -> 13;
trx($E) -> 14;
trx($F) -> 15;
trx($a) -> 10;
trx($b) -> 11;
trx($c) -> 12;
trx($d) -> 13;
trx($e) -> 14;
trx($f) -> 15.


int_to_utf8(I) when I =< 16#7F ->
    <<I>>;
int_to_utf8(I) when I =< 16#7FF ->
    B2 = I band 16#3f,
    B1 = (I bsr 6) band 16#1f,  
    <<1:1,1:1,0:1,B1:5,1:1,0:1,B2:6>>;
int_to_utf8(I) when I =< 16#FFFF ->
    B3 = I band 16#3f,
    B2 = (I bsr 6) band 16#3f,
    B1 = (I bsr 12) band 16#f,
    <<1:1,1:1,1:1,0:1,B1:4,1:1,0:1,B2:6,1:1,0:1,B3:6>>;
int_to_utf8(I) when I =< 16#10FFFF ->
    B4 = I band 16#3f,
    B3 = (I bsr 6) band 16#3f,
    B2 = (I bsr 12) band 16#3f,
    B1 = (I bsr 18) band 16#7,
    <<1:1,1:1,1:1,1:1,0:1,B1:3,1:1,0:1,B2:6,1:1,0:1,B3:6,1:1,0:1,B4:6>>;
int_to_utf8(_) ->
    exit(unsupported_utf8).

list_to_utf8(L) when is_list(L); is_binary(L) ->   
    iolist_to_binary([int_to_utf8(I) || I <- L]);
list_to_utf8({Tag,_,_}) when Tag =:= incomplete ; Tag =:= error ->
    throw(skip).

multi_esc(<<M,N,O,Rest/binary>>,_) 
   when M >= $0, M =< $7, N >= $0, N =< $7, O  >= $0, O =< $7 ->
    Cha = ((M - $0) bsl 6) bor ((N - $0) bsl 3) bor (O - $0),
    {<<Cha>>,Rest};
multi_esc(<<N,O,Rest/binary>>,_) 
   when N >= $0, N =< $7, O  >= $0, O =< $7 ->
    Cha = ((N - $0) bsl 3) bor (O - $0),
    {<<Cha>>,Rest};
multi_esc(<<O,Rest/binary>>,_) 
   when O  >= $0, O =< $7 ->
    Cha = (O - $0),
    {<<Cha>>,Rest};


multi_esc(<<$x,${,N,O,$},Rest/binary>>,Unicode) 
    when ((((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f))) and 
	(((O >= $0) and (O =< $9)) or ((O >= $A) and (O =< $F)) or 
	 ((O >= $a) and (O =< $f)))) -> 
    Cha = (trx(N) bsl 4) bor trx(O),
    case Unicode of
	false ->
	    {<<Cha:8>>,Rest};
	_ ->
	    {int_to_utf8(Cha),Rest}
    end;
multi_esc(<<$x,${,N,O,P,$},Rest/binary>>,_) 
    when ((((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f))) and 
	(((O >= $0) and (O =< $9)) or ((O >= $A) and (O =< $F)) or 
	 ((O >= $a) and (O =< $f)))and 
	(((P >= $0) and (P =< $9)) or ((P >= $A) and (P =< $F)) or 
	 ((P >= $a) and (P =< $f)))) -> 
    Cha = (trx(N) bsl 8) bor (trx(O) bsl 4) bor trx(P),
    {int_to_utf8(Cha),Rest};
multi_esc(<<$x,${,N,O,P,Q,$},Rest/binary>>,_) 
    when ((((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f))) and 
	(((O >= $0) and (O =< $9)) or ((O >= $A) and (O =< $F)) or 
	 ((O >= $a) and (O =< $f))) and 
	(((P >= $0) and (P =< $9)) or ((P >= $A) and (P =< $F)) or 
	 ((P >= $a) and (P =< $f))) and 
	(((Q >= $0) and (Q =< $9)) or ((Q >= $A) and (Q =< $F)) or 
	 ((Q >= $a) and (Q =< $f)))) -> 
    Cha = (trx(N) bsl 12) bor (trx(O) bsl 8) bor (trx(P) bsl 4) bor trx(Q),
    {int_to_utf8(Cha),Rest};
multi_esc(<<$x,${,N,O,P,Q,R,$},Rest/binary>>,_) 
    when ((((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f))) and 
	(((O >= $0) and (O =< $9)) or ((O >= $A) and (O =< $F)) or 
	 ((O >= $a) and (O =< $f))) and 
	(((P >= $0) and (P =< $9)) or ((P >= $A) and (P =< $F)) or 
	 ((P >= $a) and (P =< $f))) and 
	(((Q >= $0) and (Q =< $9)) or ((Q >= $A) and (Q =< $F)) or 
	 ((Q >= $a) and (Q =< $f))) and 
	(((R >= $0) and (R =< $9)) or ((R >= $A) and (R =< $F)) or 
	 ((R >= $a) and (R =< $f)))) -> 
    Cha = (trx(N) bsl 16) bor (trx(O) bsl 12) bor (trx(P) bsl 8) bor (trx(Q) bsl 4) bor trx(R),
    {int_to_utf8(Cha),Rest};
multi_esc(<<$x,${,N,O,P,Q,R,S,$},Rest/binary>>,_) 
    when ((((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f))) and 
	(((O >= $0) and (O =< $9)) or ((O >= $A) and (O =< $F)) or 
	 ((O >= $a) and (O =< $f))) and 
	(((P >= $0) and (P =< $9)) or ((P >= $A) and (P =< $F)) or 
	 ((P >= $a) and (P =< $f))) and 
	(((Q >= $0) and (Q =< $9)) or ((Q >= $A) and (Q =< $F)) or 
	 ((Q >= $a) and (Q =< $f))) and 
	(((R >= $0) and (R =< $9)) or ((R >= $A) and (R =< $F)) or 
	 ((R >= $a) and (R =< $f))) and 
	(((S >= $0) and (S =< $9)) or ((S >= $A) and (S =< $F)) or 
	 ((S >= $a) and (S =< $f)))) -> 
    Cha = (trx(N) bsl 20) bor (trx(O) bsl 16) bor (trx(P) bsl 12) bor (trx(Q) bsl 8) bor (trx(R) bsl 4) bor trx(S),
    {int_to_utf8(Cha),Rest};
multi_esc(<<$x,N,O,Rest/binary>>,_) 
  when ((((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f))) and 
	(((O >= $0) and (O =< $9)) or ((O >= $A) and (O =< $F)) or 
	 ((O >= $a) and (O =< $f)))) -> 
    Cha = (trx(N) bsl 4) bor trx(O),
    {<<Cha>>,Rest};
multi_esc(<<$x,N,Rest/binary>>,_) 
  when (((N >= $0) and (N =< $9)) or ((N >= $A) and (N =< $F)) or 
	 ((N >= $a) and (N =< $f)))  -> 
    Cha = trx(N),
    {<<Cha>>,Rest};
multi_esc(_,_) ->
    no.

single_esc($") ->
    $";
single_esc($ ) ->
    $ ;
single_esc($') ->
    $';
single_esc($@) ->
    $@;
single_esc($t) ->
    $\t;
single_esc($n) ->
    $\n;
single_esc($r) ->
    $\r;
single_esc($f) ->
    $\f;
single_esc($e) ->
    $\e;
single_esc($b) ->
    $\b;
single_esc($$) ->
    $$;
single_esc($\\) ->
    $\\;
single_esc($a) ->
    7;
%%single_esc(Ch) when Ch >= $A, Ch =< $Z -> % eh?
%%    Ch;

single_esc(_) ->
    no.

info(Str,Lst) ->
    case get(verbose) of
	true ->
	    io:format(Str,Lst);
	_ ->
	    ok
    end.


%% Generate split tests from indatafile, 
%% you will need perl on the machine
gen_split_test(OneFile) ->
    {ok,Bin} = file:read_file(OneFile),
    Lines = splitfile(0,Bin,1),
    Structured = stru(Lines),
    PerlShellScript = OneFile++"_split_test_gen.sh",
    FunList = dumpsplit(Structured,PerlShellScript),
    ErlModule = "re_"++filename:basename(OneFile)++"_split_test",
    ErlFileName = ErlModule++".erl",
    {ok,F}= file:open(ErlFileName,[write]),
    io:format(F,"-module(~s).~n",[ErlModule]),
    io:format(F,"-export([run/0]).~n",[]),
    io:format(F,"-compile(no_native).~n",[]),
    io:format(F,"%% This file is generated by running ~w:gen_split_test(~p)~n",
	      [?MODULE,OneFile]),
    io:format(F,"join([]) -> [];~n",[]),
    io:format(F,"join([A]) -> [A];~n",[]),
    io:format(F,"join([H|T]) -> [H,<<\":\">>|join(T)].~n",[]),
    io:format(F,"run() ->~n",[]),
    [ io:format(F,"    ~s(),~n",[FunName]) || FunName <- FunList ],
    file:close(F),
    os:cmd("sh "++ PerlShellScript++" 2>/dev/null >> "++ErlFileName),
    io:format("~s~n",[os:cmd("wc -l "++ErlFileName)]),
    ok.

dumpsplit(S,Fname) ->
    {ok,F}= file:open(Fname,[write]),
    Res = dodumpsplit(F,S,0,[],0),
    file:close(F),
    Res.

dodumpsplit(F,[],_,Acc,_) ->
    io:format(F,"echo \"    ok.\"~n",[]),
    lists:reverse(Acc);
dodumpsplit(F,L,0,Acc,FunNum) ->
    NewFun = "run"++integer_to_list(FunNum),
    io:format(F,"echo \"    ok.\"~n",[]),
    io:format(F,"echo \"~s() ->\"~n",[NewFun]),
    dodumpsplit(F,L,20,[NewFun|Acc],FunNum+1);
dodumpsplit(F,[H|T],N,Acc,FunNum) ->
    dumponesplit(F,H),
    dodumpsplit(F,T,N-1,Acc,FunNum).

dumponesplit(F,{RE,Line,O,TS}) ->
    [begin
	 {NO,_} = pick_exec_options(O++Op),
	 SSS = opt_to_string(NO),
	 LLL = unicode:characters_to_list(RE),
	 case (catch iolist_to_binary(LLL)) of
	     X when is_binary(X) -> 
		 io:format(F,"perl -e '$x = join(\":\",split(/~s/~s,\"~s\")); "
			   "$x =~~ s/\\\\/\\\\\\\\/g; $x =~~ s/\\\"/\\\\\"/g; "
			   "print \"    <<\\\"$x\\\">> = "
			   "iolist_to_binary(join(re:split(\\\"~s\\\","
			   "\\\"~s\\\",~p))),\\n\";'~n",
			   [zsafe(safe(RE)),
			    SSS,
			    ysafe(safe(Str)),
			    dsafe(safe(Str)),
			    dsafe2(safe(RE)),
			    NO++[trim]]),
		 io:format(F,"perl -e '$x = join(\":\",split(/~s/~s,\"~s\",2)); "
			   "$x =~~ s/\\\\/\\\\\\\\/g; $x =~~ s/\\\"/\\\\\"/g; "
			   "print \"    <<\\\"$x\\\">> = "
			   "iolist_to_binary(join(re:split(\\\"~s\\\","
			   "\\\"~s\\\",~p))),\\n\";'~n",
			   [zsafe(safe(RE)),
			    SSS,
			    ysafe(safe(Str)),
			    dsafe(safe(Str)),
			    dsafe2(safe(RE)),
			    NO++[{parts,2}]]),
		 io:format(F,"perl -e '$x = join(\":\",split(/~s/~s,\"~s\",-1)); "
			   "$x =~~ s/\\\\/\\\\\\\\/g; $x =~~ s/\\\"/\\\\\"/g; "
			   "print \"    <<\\\"$x\\\">> = "
			   "iolist_to_binary(join(re:split(\\\"~s\\\","
			   "\\\"~s\\\",~p))),\\n\";'~n",
			   [zsafe(safe(RE)),
			    SSS,
			    ysafe(safe(Str)),
			    dsafe(safe(Str)),
			    dsafe2(safe(RE)),
			    NO]);
	     _ -> io:format("Found fishy character at line ~w~n",[Line])
	 end
     end ||
	{Str,_,Op,_} <- TS].

%% Generate replacement tests from indatafile, 
%% you will need perl on the machine
gen_repl_test(OneFile) ->
    rand:seed(exsplus, {1219,687731,62804}),
    {ok,Bin} = file:read_file(OneFile),
    Lines = splitfile(0,Bin,1),
    Structured = stru(Lines),
    PerlShellScript = OneFile++"_replacement_test_gen.sh",
    FunList = dump(Structured,PerlShellScript),
    ErlModule = "re_"++filename:basename(OneFile)++"_replacement_test",
    ErlFileName = ErlModule++".erl",
    {ok,F}= file:open(ErlFileName,[write]),
    io:format(F,"-module(~s).~n",[ErlModule]),
    io:format(F,"-export([run/0]).~n",[]),
    io:format(F,"-compile(no_native).~n",[]),
    io:format(F,"%% This file is generated by running ~w:gen_repl_test(~p)~n",
	      [?MODULE,OneFile]),
    io:format(F,"run() ->~n",[]),
    [ io:format(F,"    ~s(),~n",[FunName]) || FunName <- FunList ],
    file:close(F),
    os:cmd("sh "++ PerlShellScript++" 2>/dev/null >> "++ErlFileName),
    io:format("~s~n",[os:cmd("wc -l "++ErlFileName)]),
    ok.
dump(S,Fname) ->
    {ok,F}= file:open(Fname,[write]),
    Res = dodump(F,S,0,[],0),
    file:close(F),
    Res.
    
dodump(F,[],_,Acc,_) ->
    io:format(F,"echo \"    ok.\"~n",[]),
    lists:reverse(Acc);
dodump(F,L,0,Acc,FunNum) ->
    NewFun = "run"++integer_to_list(FunNum),
    io:format(F,"echo \"    ok.\"~n",[]),
    io:format(F,"echo \"~s() ->\"~n",[NewFun]),
    dodump(F,L,20,[NewFun|Acc],FunNum+1);
dodump(F,[H|T],N,Acc,FunNum) ->
    dumpone(F,H),
    dodump(F,T,N-1,Acc,FunNum).

dumpone(F,{RE,Line,O,TS}) ->
    [begin
	 {NO,_} = pick_exec_options(O++Op),
	 SSS = opt_to_string(NO),
	 RS = ranstring(),
	 LLL = unicode:characters_to_list(RE),
	 case (catch iolist_to_binary(LLL)) of
	     X when is_binary(X) -> io:format(F,"perl -e '$x = \"~s\"; $x =~~ s/~s/~s/~s; $x =~~ s/\\\\/\\\\\\\\/g; $x =~~ s/\\\"/\\\\\"/g; print \"    <<\\\"$x\\\">> = iolist_to_binary(re:replace(\\\"~s\\\",\\\"~s\\\",\\\"~s\\\",~p)), \\n\";'~n",[ysafe(safe(Str)),zsafe(safe(RE)),perlify(binary_to_list(RS)),SSS,dsafe(safe(Str)),dsafe(safe(RE)),xsafe(RS),NO]),
	 io:format(F,"perl -e '$x = \"~s\"; $x =~~ s/~s/~s/g~s; $x =~~ s/\\\\/\\\\\\\\/g; $x =~~ s/\\\"/\\\\\"/g; print \"    <<\\\"$x\\\">> = iolist_to_binary(re:replace(\\\"~s\\\",\\\"~s\\\",\\\"~s\\\",~p)), \\n\";'~n",[ysafe(safe(Str)),zsafe(safe(RE)),perlify(binary_to_list(RS)),SSS,dsafe(safe(Str)),dsafe(safe(RE)),xsafe(RS),NO++[global]]);
	     _ -> io:format("Found fishy character at line ~w~n",[Line])
	 end
     end ||
	{Str,_,Op,_} <- TS].

dsafe2([]) ->
    [];
dsafe2([$\',$\",$\',$\",$\'|T]) ->
    [$\',$\",$\',$\",$\' |dsafe2(T)];
dsafe2([$\"|T]) ->
    [$\\,$\\,$\\,$\" |dsafe2(T)];
dsafe2([$\\, $G|T]) ->
    [$\\,$\\,$\\,$\\,$A |dsafe2(T)];
dsafe2([$\\|T]) ->
    [$\\,$\\,$\\,$\\ |dsafe2(T)];
dsafe2([$$|T]) ->
    [$\\,$$|dsafe2(T)];
dsafe2([H|T]) ->
    [H|dsafe2(T)].

dsafe([]) ->
    [];
dsafe([$\',$\",$\',$\",$\'|T]) ->
    [$\',$\",$\',$\",$\' |dsafe(T)];
dsafe([$\"|T]) ->
    [$\\,$\\,$\\,$\" |dsafe(T)];
dsafe([$\\|T]) ->
    [$\\,$\\,$\\,$\\ |dsafe(T)];
dsafe([$$|T]) ->
    [$\\,$$|dsafe(T)];
dsafe([H|T]) ->
    [H|dsafe(T)].

xsafe(<<>>) ->
    [];
xsafe(<<$\\,R/binary>>) ->
    [$\\,$\\,$\\,$\\ | xsafe(R)];
xsafe(<<X,R/binary>>) ->
    [X|xsafe(R)].

zsafe([]) ->
    [];
zsafe([$$, $b|T]) ->
    [$\\,$$, $b | zsafe(T)];
zsafe([X|R]) ->
    [X|zsafe(R)].

ysafe([]) ->
    [];
ysafe([$\',$\",$\',$\",$\'|T]) ->
    [$\',$\",$\',$\",$\' |ysafe(T)];
ysafe([$\"|T]) ->
    [$\\,$\" |ysafe(T)];
ysafe([$\\|T]) ->
    [$\\,$\\ |ysafe(T)];
ysafe([$$|T]) ->
    [$\\,$$|ysafe(T)];
ysafe([H|T]) ->
    [H|ysafe(T)].

safe(<<>>) ->
    [];
safe(<<$\n>>) -> %chomp
    [];
safe(<<$\',R/binary>>) ->
    [$\',$\",$\',$\",$\' | safe(R)];
safe(<<X,R/binary>>) ->
    [X|safe(R)].
perlify([$\\,N|Rest]) when N >= $0, N =< $9 ->
    [$$,N|perlify(Rest)];
perlify([$& | Rest]) ->
    [$$,$& | perlify(Rest)];
perlify([H|T]) ->
    [H|perlify(T)];
perlify([]) ->
    [].

opt_to_string([]) ->
    [];
opt_to_string([A|T]) ->
    case btr(A) of
	false ->
	    opt_to_string(T);
	Ch ->
	    [Ch | opt_to_string(T)]
    end.

btr(caseless) ->
    $i;
btr(extended) ->
    $x;
btr(dotall) ->
    $s;
btr(multiline) ->
    $m;
btr(dupnames) ->
    $J;
btr(no_auto_capture) ->
    $N;
btr(unicode) ->
    $8;
btr(_) ->
    false.


ranchar() ->
    case rand:uniform(10) of
	9 -> $&;
        10 -> <<"\\1">>;		 
	N when N < 5 ->
	    rand:uniform($Z-$A)+$A-1;
	M when M < 9 ->
	    rand:uniform($z-$a)+$a-1
    end.

ranstring() ->
    iolist_to_binary([ranchar() || _ <- lists:duplicate(rand:uniform(20),0) ]).

