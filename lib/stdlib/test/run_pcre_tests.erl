%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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

-define(is_hex_char(C),(((C >= $0) and (C =< $9)) or ((C >= $A) and (C =< $F)) or ((C >= $a) and (C =< $f)))).
-define(SPACE,32). % space character ($ )

test(RootDir) ->
    %%put(verbose,true),
    erts_debug:set_internal_state(available_internal_state,true),
    io:format("oldlimit: ~p~n",[ erts_debug:set_internal_state(re_loop_limit,1)]),
    Testfiles0 = ["testoutput1",
                  "testoutput2",
                  "testoutput4",
                  "testoutput5",
                  "mod_testoutput8",
                  "testoutput10"
                 ],
    Testfiles = [ filename:join([RootDir,FN]) || FN <- Testfiles0 ], 
    Res = [ t(X) || X <- Testfiles ],
    io:format("limit was: ~p~n",[ erts_debug:set_internal_state(re_loop_limit,default)]),
    Res2 = Res ++ [ t(X) || X <- Testfiles ],
    erts_debug:set_internal_state(available_internal_state,false),
    Res2.

t(OneFile) ->
    t(OneFile,infinite).

t(OneFile,Num) ->
    put(testfile,filename:basename(OneFile)),
    {ok,Bin} = file:read_file(OneFile),
    Lines = splitfile(0,Bin,1),
    put(re_tested, 0),
    put(re_skipped, 0),
    Structured = stru(Lines),
    io:format("~s parsed: ~p regex to test, ~p regex skipped\n",
              [get(testfile), get(re_tested), get(re_skipped)]),
    put(error_limit,Num),
    put(skipped,0),
    put(re_run,0),
    put(re_compile,0),
    Res = 
	[test(Structured,true,index,false),
	 test(Structured,false,index,false),
	 test(Structured,true,index,true),
	 test(Structured,false,index,true),
	 test(Structured,true,binary,false),
	 test(Structured,false,binary,false),
	 test(Structured,true,list,false),
	 test(Structured,false,list,false)],
    io:format("Done with ~s. Call count:  re:compile ~p, re:run ~p\n",
              [get(testfile), get(re_compile), get(re_run)]),
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
test([{{replace,_},_,_,_,_}|T],PreCompile,index,REAsList) ->
    test(T,PreCompile,index,REAsList);
test([{R,RE0,Line,Options0,Tests}|T],PreCompile,XMode,REAsList) ->
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
			   {re_compile(RE,Options),[]};
		       _ ->
                           erase(re_compile_opts),
			   {{ok,RE},Options}
		   end,
    case Cres of
	{ok,P} ->
	    try testrun(R, RE,P,Tests,ExecOptions,Xopt,XMode) of
		N when is_integer(N) ->
		    N + test(T,PreCompile,XMode,REAsList)
            catch
		throw:limit ->
		    io:format("Error limit reached.~n"),
		    1;
		throw:skip ->
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

testrun(_,_,_,[],_,_,_) ->
    0;
testrun(ReFun, RE,P,[{Chal,Line,ExecOpt,Responses}|T],EO,Xopt0,XMode) ->
    Global = case lists:member(g, EO) of true -> [global]; false -> [] end,
    ReturnType = if ReFun =:= run -> [{capture,all,XMode}];
                    true -> [{return,XMode}] end,
    AcceptNonAscii = lists:member(accept_nonascii,EO),
    Xopt = clean_duplicates(Xopt0,ExecOpt) ++ ReturnType ++ Global,

    case lists:keymember(newline,1,Xopt) of
	true ->
	    info("skipping inconsistent newlines "
		 "when compiling and running in one go (~p)~n",
		 [Line]),
	    throw(skip);
	false ->
	    ok
    end,
    %% If global and accept_nonascii is not set, and subject has 8bit, skip
    case not lists:member(g,EO) andalso not AcceptNonAscii andalso contains_eightbit(Chal) of
    true -> 
        info("skipping 8bit without LANG (~p)~n",
                [Line]),
        throw(skip);
    false ->
        ok
    end,
    TestFun = case ReFun of
          run -> fun() -> re_run(Chal, P, ExecOpt++Xopt) end;
          {replace, Replacement} -> fun() ->
                {replace, re_replace(Chal, P, Replacement, ExecOpt++Xopt)}
            end
    end,
    Res = case TestFun() of
        %% Handle replace results
        {replace, NewChal} when is_binary(NewChal), XMode =:= binary ->
            {match, [{dont_care, NewChal}]};
        {replace, NewChal} when is_list(NewChal), XMode =:= list ->
            UFix = lists:member(unicode,EO),
            {match, [{dont_care, if
                UFix -> list_to_utf8(NewChal);
                true -> list_to_binary(NewChal)
            end}]};
        %%  Handle run results
        nomatch ->
            nomatch;
        {match, Reslist} when XMode =:= binary, Global =:= [global] ->
            {match,press([bfix(R)|| R <- Reslist])};
        {match, Reslist} when XMode =:= binary ->
            {match,bfix(Reslist)};
        {match, Reslist} when XMode =:= list, Global =:= [global] ->
            UFix = lists:member(unicode,EO),
            {match,press([bfix([if
                            UFix -> list_to_utf8(L);
                            true -> list_to_binary(L)
                        end || L <- R]) || R <- Reslist])};
        {match, Reslist} when XMode =:= list ->
            UFix = lists:member(unicode,EO),
            {match,bfix([if
                            UFix -> list_to_utf8(L);
                            true -> list_to_binary(L)
                        end || L <- Reslist])};
        {match, Reslist} when XMode =:= index, Global =:= [global] ->
            {match,press([fixup(Chal,R,0) || R <- Reslist])};
        {match, Reslist} when XMode =:= index ->
            {match,fixup(Chal,Reslist,0)}
    end,
	
    case compare_sloppy(Res,Responses) of
	true ->
	    testrun(ReFun, RE,P,T,EO,Xopt0,XMode);
	false ->
        case ReFun of
	        run -> io:format("~s: FAIL(~w): re = ~p, ~nmatched against = ~p(~w), ~nwith options = ~p. ~nexpected = ~p, ~ngot = ~p~n",
		      [get(testfile), Line,RE,Chal,binary_to_list(Chal),used_options(),Responses,Res]);
            {replace, Repl} -> io:format("~s: FAIL(~w): re = ~p, ~nmatched against = ~p(~w), ~nreplace with: ~p, ~nwith options = ~p. ~nexpected = ~p, ~ngot = ~p~n",
                [get(testfile), Line,RE,Chal,binary_to_list(Chal),Repl,used_options(),Responses,Res])
        end,
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
compare_sloppy([{dont_care, L1}|T1],[{_,L1}|T2]) ->
    compare_sloppy(T1,T2);
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
    case linetype(Res,first) of
	empty ->
	    [{Line,<<>>}|splitfile(NewN,Bin,Line+1)];
        comment ->
	    splitfile(NewN,Bin,Line+1);
        content ->
	    [{Line,Res}|splitfile(NewN,Bin,Line+1)]
    end.

linetype(<<>>, _) ->
    empty;
linetype(<<?SPACE,R/binary>>, _) ->
    linetype(R, space);
linetype(<<"\\=", _/binary>>, first) ->
    comment;
linetype(_, _) ->
    content.

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

skip_extra_info([{_,<<?SPACE,?SPACE,?SPACE,_/binary>>}=H|Con]) ->
    [H|Con];
skip_extra_info([{_,<<>>}|Con]) ->
    Con;
skip_extra_info([_|T]) ->
    skip_extra_info(T);
skip_extra_info([]) ->
    [].

skip_debug_stuff(T0) ->
    case T0 of
        [{_,<<$-,_/binary>>}|Con] ->
            %%Debug output, we skip those
            T1 = skip_debug(Con),
            skip_debug_stuff(T1);
        [{_,<<"Capture",_/binary>>}|_] ->
            T1 = skip_extra_info(T0),
            skip_debug_stuff(T1);
        [{_,<<Bla,_/binary>>}|_] when Bla =/= ?SPACE ->
            T1 = skip_until_empty(T0),
            {next, T1};
        _ ->
            {continue, T0}
    end.


stru([]) ->
    [];
stru([{_,<<>>}|T]) ->
    stru(T);
stru([{_Line,<<$#, _/binary>>=_Bin}|T0]) ->
    info("~p: stru skip comment: ~p\n", [_Line, _Bin]),
    stru(T0);
stru([{Line,<<Ch,Re0/binary>>}|T0]) ->
    {T1,Re} = find_rest_re(Ch,[{Line,Re0}|T0]),
    {NewRe,<< Ch, Options/binary >>} = end_of_re(Ch,Re),
    case interpret_options_x(backstrip(frontstrip(Options)),NewRe) of
	{Olist,[], Extra} ->
	    U = lists:member(unicode,Olist),
        case skip_debug_stuff(T1) of
            {next, T2} ->
                inc_counter(re_skipped),
                stru(T2);
            {continue, T2} ->
                inc_counter(re_tested),
                {NewT,Matches} = stru2(T2,U),
                Matches1 = case U of
                    true ->
                        Matches ++
                        [ {unicode:characters_to_list(E1,unicode),E2,E3,E4} ||
                            {E1,E2,E3,E4} <- Matches];
                    false ->
                        Matches
                    end,
                    case proplists:is_defined(replace, Extra) of
                        true -> [{{replace, proplists:get_value(replace, Extra)},NewRe,Line,Olist,Matches1}|stru(NewT)];
                        false -> [{run, NewRe,Line,Olist,Matches1}|stru(NewT)]
                end
	    end;
	{_,Rest, _} ->
	    NewT = skip_until_empty(T1),
	    SkipTo = case NewT of
                         [{ToLine,_}|_] -> integer_to_list(ToLine);
                         _ -> "end of file"
                     end,
	    info("Skipping options ~p for now, skip from line ~p to ~s\n",
                 [Rest, Line, SkipTo]),
            inc_counter(re_skipped),
	    stru(NewT)
    end.

contains_lang_sens(<<>>) ->
    false;
contains_lang_sens(<<"\\W",_/binary>>) ->
    true;
contains_lang_sens(<<"\\w",_/binary>>) ->
    true;
contains_lang_sens(<<"\\b",_/binary>>) ->
    true;
contains_lang_sens(<<_,R/binary>>) ->
    contains_lang_sens(R).

interpret_options_x(Options,RE) ->
    {O,R,E} = interpret_options(<<Options/binary, $,>>),
    case (contains_lang_sens(RE) or lists:member(caseless,O)) of
	false ->
	    {[{exec_option,accept_nonascii}|O],R,E};
	true ->
	    case lists:member(unicode,O) of
		true ->
		    {[{exec_option,accept_nonascii}|O],R,E};
		false ->
		    {O,R,E}
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
tr_option($n) ->
    [no_auto_capture];
tr_option($g) ->
    [{exec_option,g}];
tr_option($,) ->
    [];
tr_option(_) ->
    false.

bsr_opt(<<"anycrlf">>) -> bsr_anycrlf;
bsr_opt(<<"unicode">>) -> bsr_unicode;
bsr_opt(_Other) ->
    false.

interpret_options(<<>>) ->
    {[], [], []};
%% Supported, put in the list of supported options
interpret_options(<<"newline=",Rest0/binary>>) ->
    {NewLine, Rest1} = get_modifier(Rest0),
    {Olist, NRest, Extra} = interpret_options(Rest1),
    case newline_opt(NewLine) of
	false ->
	    {Olist, [NewLine | NRest], Extra};
	NL ->
	    {[{newline, NL} | Olist], NRest, Extra}
    end;
interpret_options(<<"bsr=",Rest0/binary>>) ->
    {Word, Rest1} = get_modifier(Rest0),
    {Olist, NRest, Extra} = interpret_options(Rest1),
    case bsr_opt(Word) of
	false ->
	    {Olist, [Word | NRest], Extra};
	BSR ->
	    {[BSR | Olist], NRest, Extra}
    end;
interpret_options(<<"utf,",Rest0/binary>>) ->
    {Olist, NRest, Extra} = interpret_options(Rest0),
    {[unicode | Olist], NRest, Extra};
interpret_options(<<"locale=fr_FR,",Rest/binary>>) ->
    info("Accepting (and ignoring) french locale~n",[]),
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[{exec_option, accept_nonascii}|Olist],NRest, Extra};
interpret_options(<<"dupnames,",Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[dupnames | Olist], NRest, Extra};
interpret_options(<<"anchored,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[anchored | Olist], NRest, Extra};
interpret_options(<<"ungreedy,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[ungreedy | Olist], NRest, Extra};
interpret_options(<<"caseless,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[caseless | Olist], NRest, Extra};
interpret_options(<<"dollar_endonly,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[dollar_endonly | Olist], NRest, Extra};
interpret_options(<<"dotall,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[dotall | Olist], NRest, Extra};
interpret_options(<<"global,",Rest/binary>>) ->
    {Olist, NRest, Extra} = interpret_options(Rest),
    {[{exec_option,g} | Olist], NRest, Extra};
interpret_options(<<"no_auto_capture,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[no_auto_capture | Olist], NRest, Extra};
interpret_options(<<"firstline,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[firstline | Olist], NRest, Extra};
interpret_options(<<"multiline,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[multiline | Olist], NRest, Extra};
interpret_options(<<"extended,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[extended | Olist], NRest, Extra};
interpret_options(<<"ucp,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[ucp | Olist], NRest, Extra};
interpret_options(<<"never_utf,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[never_utf | Olist], NRest, Extra};
interpret_options(<<"noteol,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[noteol | Olist], NRest, Extra};
interpret_options(<<"notempty,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[notempty | Olist], NRest, Extra};
interpret_options(<<"notempty_atstart,", Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[notempty_atstart | Olist], NRest, Extra};
interpret_options(<<"no_start_optimize,",Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {[no_start_optimize | Olist], NRest, Extra};
%% Support the regex but not the options (silently ignore the option)
interpret_options(<<"aftertext,",Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {Olist, NRest, Extra};
interpret_options(<<"mark,",Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {Olist, NRest, Extra};
interpret_options(<<"get=",Rest0/binary>>) ->
    {_Word, Rest1} = get_modifier(Rest0),
    {Olist,NRest, Extra} = interpret_options(Rest1),
    {Olist, NRest, Extra};
interpret_options(<<"getall,",Rest/binary>>) ->
    {Olist,NRest, Extra} = interpret_options(Rest),
    {Olist, NRest, Extra};
interpret_options(<<"copy=",Rest0/binary>>) ->
    {_Word, Rest1} = get_modifier(Rest0),
    {Olist,NRest, Extra} = interpret_options(Rest1),
    {Olist, NRest, Extra};
%% Replace is supported using re:replace, put in Extra
interpret_options(<<"replace=",Rest0/binary>>) ->
    {Word, Rest1} = get_modifier(Rest0),
    {Olist,NRest,Extra} = interpret_options(Rest1),
    Replacement = (fun F(Word_, Acc) -> case Word_ of
                <<$,, _/binary>> ->
                    [];
                %% do not support [10]
                <<$[, _/binary>> ->
                    [];
                %% do not support $*Mark
                <<$$, $*, _/binary>> ->
                    [];
                %% replace $0 with &, $1 with \\1, ${1} with \\1
                <<$$, $0, WRest/binary>> ->
                    F(WRest, <<Acc/binary, $&>>);
                <<$$, ${, $0, $}, WRest/binary>> ->
                    F(WRest, <<Acc/binary, $&>>);
                <<$$, D, WRest/binary>> when $1 =< D, D =< $9 ->
                    F(WRest, <<Acc/binary, $\\, D>>);
                <<$$, ${, D, $},WRest/binary>> when $1 =< D, D =< $9 ->
                    F(WRest, <<Acc/binary, $\\, D>>);
                <<$$, $$, WRest/binary>> ->
                    F(WRest, <<Acc/binary, $$>>);
                %% do not support any other starting with $, ${ i.e. ${Name}
                <<$$, _/binary>> ->
                    [];
                <<C, WRest/binary>> ->
                    F(WRest, <<Acc/binary, C>>);
                <<>> ->
                    Acc
            end
        end)(Word, <<>>),
    case Replacement of
        [] ->
            {Olist, ["replace="++Word | NRest], Extra};
        _ ->
            {Olist, NRest, [{replace, Replacement}|Extra]}
    end;
interpret_options(<<Bin/binary>>) ->
    [FirstWord, Rest] = binary:split(Bin, <<",">>),
    {Olist, Failed1, Extra} = interpret_options(Rest),
    case interpret_not_supported_options(FirstWord) of
        true ->
            {Olist, [FirstWord | Failed1], Extra};
        false ->
            case short_options(FirstWord, Olist, Failed1) of
                false ->
                    {Olist, [FirstWord | Failed1], Extra};
                {Options, Failed2} ->
                    {Options ++ Olist, Failed2, Extra}
            end
    end.
interpret_not_supported_options(Word) ->
    case binary:split(Word, <<"=">>) of
        [FirstWord, _] ->
            lists:member(FirstWord, [
                <<"allaftertext">>,
                <<"allcaptures">>,
                <<"allow_empty_class">>,
                <<"allow_lookaround_bsk">>,
                <<"allow_surrogate_escapes">>,
                <<"allvector">>,
                <<"alt_bsux">>,
                <<"alt_circumflex">>,
                <<"alt_verbnames">>,
                <<"altglobal">>,
                <<"ascii_all">>,
                <<"ascii_bsd">>,
                <<"ascii_bss">>,
                <<"ascii_bsw">>,
                <<"ascii_digit">>,
                <<"ascii_posix">>,
                <<"auto_callout">>,
                <<"bad_escape_is_literal">>,
                <<"bincode">>,
                <<"callout_info">>,
                <<"caseless_restrict">>,
                <<"endanchored">>,
                <<"escaped_cr_is_lf">>,
                <<"expand">>,
                <<"extended_more">>,
                <<"extra_alt_bsux">>,
                <<"hex">>,
                <<"info">>,
                <<"jitstack">>,
                <<"literal">>,
                <<"locale">>,
                <<"match_invalid_utf">>,
                <<"match_line">>,
                <<"match_unset_backref">>,
                <<"match_word">>,
                <<"max_pattern_length">>,
                <<"max_varlookbehind">>,
                <<"never_backslash_c">>,
                <<"never_ucp">>,
                <<"no_auto_possess">>,
                <<"no_dotstar_anchor">>,
                <<"no_jit">>,
                <<"no_utf_check">>,
                <<"null_context">>,
                <<"null_pattern">>,
                <<"ovector">>,
                <<"parens_nest_limit">>,
                <<"ph">>,
                <<"ps">>,
                <<"stackguard">>,
                <<"startchar">>,
                <<"subject_literal">>,
                <<"substitute_callout">>,
                <<"substitute_extended">>,
                <<"substitute_literal">>,
                <<"tables">>,
                <<"use_length">>,
                <<"use_offset_limit">>
            ]);
        _ ->
            false
    end.
short_options(<<>>, Olist, Failed) ->
    {Olist, Failed};
short_options(<<"xx", Rest/binary>>, Olist0, Failed0) ->
    %% xx (PCRE2_EXTENDED_MORE) notsup
    short_options(Rest, Olist0, ["xx" | Failed0]);
short_options(<<Ch, Rest/binary>>, Olist0, Failed0) ->
    case tr_option(Ch) of
        false ->
            false;
        Option ->
            short_options(Rest, Option ++ Olist0, Failed0)
    end.

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

stru2([{Line,<<?SPACE,Rest/binary>>} | T],U) ->
    %% A challenge
    try responses(T,U) of
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
				 <<OFS/binary,?SPACE>>;
			     _ ->
				 OFS
			 end
		 end,
	    {ExecOpts,NFS} = subject(FS,U),
	    case find_unsupported(ExecOpts) of
		[] ->
		    {NewNewT,[{NFS,Line,ExecOpts,
			       case Rlist of
                                   nomatch -> nomatch;
				   RR -> {match,RR}
			       end} | StrList]};
		UList ->
		    info("WARNING(~w): the exec-option(s) ~p are unsupported, skipping challenge.~n",[Line,UList]),
		    {NewNewT,StrList}
	    end

    catch throw:fail ->
	    NewT = skip_until_empty(T),
	    {NewT,[]}
    end;
	
stru2(X,_) ->
    {X,[]}.

responses([{Line,<<"MK: ",_/binary>>}|T],U) ->
    info("Skipping mark response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<?SPACE, _D,"C ",_/binary>>}|T],U) ->
    info("Skipping copy response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<?SPACE, _D,"G ",_/binary>>}|T],U) ->
    info("Skipping get response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<?SPACE, _D,"L ",_/binary>>}|T],U) ->
    info("Skipping getall response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<" 0+ ",_/binary>>}|T],U) ->
    info("Skipping aftertext response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line, <<"Partial match: ",_/binary>>}|T],U) ->
    info("Skipping partial match response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<?SPACE,_,$(, _, $), " Old",_/binary>>}|T],U) ->
    info("Skipping substitute response at line ~p~n",[Line]),
    responses(T,U);
responses([{_Line,<< X:2/binary,$:,?SPACE,Resp/binary>>}|T],U) ->
    {NT,R2} = responses(T,U),
    NX = binary_to_integer(frontstrip(X)),
    {NT,[{NX,escape2(Resp,U)} | R2]};
responses([{_Line,<< X:3/binary,$:,?SPACE,Resp/binary>>}|T],U) ->
    {NT,R2} = responses(T,U),
    NX = binary_to_integer(frontstrip(X)),
    {NT,[{NX,escape2(Resp,U)} | R2]};
responses([{_Line,<<"No match",_/binary>>}|T],_) ->
    {T,nomatch};
responses([{Line,<<?SPACE,No,Ch,_/binary>>}|T],U) when No >= $0, No =< $9, Ch >= $A, Ch =< $Z ->
    info("Skipping strange debug response at line ~p~n",[Line]),
    responses(T,U);
responses([{Line,<<C,_/binary>>=X}|_],_) when C =/= ?SPACE ->
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
frontstrip(<<?SPACE,Rest/binary>>) ->
    frontstrip(Rest);
frontstrip(Bin) ->
    Bin.

backstrip(<<>>) ->
    <<>>;
backstrip(<<?SPACE>>) ->
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

newline_opt(<<"cr">>) -> cr;
newline_opt(<<"CR">>) -> cr;
newline_opt(<<"lf">>) -> lf;
newline_opt(<<"LF">>) -> lf;
newline_opt(<<"crlf">>) -> crlf;
newline_opt(<<"CRLF">>) -> crlf;
newline_opt(<<"anycrlf">>) -> anycrlf;
newline_opt(<<"ANYCRLF">>) -> anycrlf;
newline_opt(<<"nul">>) -> nul;
newline_opt(<<"NUL">>) -> nul;
newline_opt(<<"any">>) -> any;
newline_opt(<<"ANY">>) -> any;
newline_opt(_Other) ->
    false.

pick_number(Bin) ->
    pick_number(0, Bin).

pick_number(N,<<Ch:8,Rest/binary>>) when Ch >= $0, Ch =< $9 ->
    pick_number(N*10+(Ch - $0),Rest);
pick_number(N,Rest) ->
    {N,Rest}.

pick_octal(Bin) ->
    pick_octal(0, Bin).

pick_octal(N,<<Ch:8,Rest/binary>>) when Ch >= $0, Ch =< $7 ->
    pick_octal(N*8+(Ch - $0), Rest);
pick_octal(N,Rest) ->
    {N,Rest}.


get_modifier(Bin) ->
    get_modifier(1, Bin).

get_modifier(Len, Bin) ->
    case Bin of
        <<_:Len/binary>> ->
            {Bin, <<>>};
        <<Mod:Len/binary, $,, Rest/binary>> ->
            {Mod, Rest};
        _ when Len < byte_size(Bin) ->
            get_modifier(Len+1, Bin)
    end.

modifier(<<"anchored">>) ->
    anchored;
modifier(<<"notbol">>) ->
    notbol;
modifier(<<"offset=", Rest/binary>>) ->
    {Offs, <<>>} = pick_number(Rest),
    {offset, Offs};
%% modifier(<<"ovector=", Rest/binary>>) ->
%%     {Sz, <<>>} = pick_number(Rest),
%%     {ovector,Sz};
modifier(Unknown) ->
    {not_supported, Unknown}.

subject_modifiers(<<>>) ->
    [];
subject_modifiers(Bin) ->
    {ModBin, Rest} = get_modifier(Bin),
    [modifier(ModBin) | subject_modifiers(Rest)].

repeat_bin(Bin, 1) ->
    Bin;
repeat_bin(Bin, N) when N > 1 ->
    Prefix = repeat_bin(Bin, N-1),
    <<Prefix/binary, Bin/binary>>.

subject(<<>>,_) ->
    {[],<<>>};
subject(<<"\\=", Modifiers/binary>>, _U) ->
    {subject_modifiers(Modifiers), <<>>};
subject(<<"\\[", Rest0/binary>>, U) ->
    %% Repeat string. Ex: "\[abc]{4}" is "abcabcabcabc"
    {StrLen, 2} = binary:match(Rest0, <<"]{">>),
    <<Str:StrLen/binary, "]{", Rest1/binary>> = Rest0,
    {Count, <<$}, Rest2/binary>>} = pick_number(Rest1),
    Result = repeat_bin(Str, Count),
    {Opts, Tail} = subject(Rest2, U),
    {Opts, <<Result/binary, Tail/binary>>};
subject(<<$\\, Ch, Rest/binary>>,U) ->
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
    {MoreOpts,Tail} = subject(NR,U),
    {MoreOpts,<<C/binary,Tail/binary>>};
subject(<<Ch,Rest/binary>>,U) ->
    {X,RR} = subject(<<Rest/binary>>,U),
    {X,<<Ch,RR/binary>>};
subject(Any,_) ->
    {[],Any}.

escape2(<<>>,_) ->
    <<>>;
escape2(<<$\\, Rest/binary>>,U) ->
    {CharBin,NewRest} =
        case multi_hex_esc(Rest,U) of
            {Ch, _} when (Ch =:= no) or ((Ch >= 32) and (Ch =< 126)) ->
                {<<$\\>>, Rest};

            {_, Tpl} ->
                Tpl
        end,
    Tail = escape2(NewRest, U),
    <<CharBin/binary, Tail/binary>>;
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
multi_esc(<<"o{", Rest0/binary>>, _) ->
    {Cha, <<$}, Rest1/binary>>} = pick_octal(Rest0),
    {int_to_utf8(Cha), Rest1};
multi_esc(Bin, Unicode) ->
    {_Cha, Tpl} = multi_hex_esc(Bin, Unicode),
    Tpl.

multi_hex_esc(<<"x{",N,$},Rest/binary>>,Unicode) when ?is_hex_char(N) ->
    Cha = trx(N),
    case Unicode of
	false ->
	    {Cha, {<<Cha:8>>,Rest}};
	_ ->
	    {Cha, {int_to_utf8(Cha),Rest}}
    end;
multi_hex_esc(<<"x{",N,O,$},Rest/binary>>,Unicode) when (?is_hex_char(N) and
                                                         ?is_hex_char(O)) ->
    Cha = (trx(N) bsl 4) bor trx(O),
    case Unicode of
	false ->
	    {Cha, {<<Cha:8>>,Rest}};
	_ ->
	    {Cha, {int_to_utf8(Cha),Rest}}
    end;
multi_hex_esc(<<"x{",N,O,P,$},Rest/binary>>,_) when (?is_hex_char(N) and
                                                     ?is_hex_char(O) and
                                                     ?is_hex_char(P)) ->
    Cha = (trx(N) bsl 8) bor (trx(O) bsl 4) bor trx(P),
    {Cha, {int_to_utf8(Cha),Rest}};
multi_hex_esc(<<"x{",N,O,P,Q,$},Rest/binary>>,_) when (?is_hex_char(N) and
                                                       ?is_hex_char(O) and
                                                       ?is_hex_char(P) and
                                                       ?is_hex_char(Q)) ->
    Cha = (trx(N) bsl 12) bor (trx(O) bsl 8) bor (trx(P) bsl 4) bor trx(Q),
    {Cha, {int_to_utf8(Cha),Rest}};
multi_hex_esc(<<"x{",N,O,P,Q,R,$},Rest/binary>>,_) when (?is_hex_char(N) and
                                                         ?is_hex_char(O) and
                                                         ?is_hex_char(P) and
                                                         ?is_hex_char(Q) and
                                                         ?is_hex_char(R)) ->
    Cha = (trx(N) bsl 16) bor (trx(O) bsl 12) bor (trx(P) bsl 8) bor (trx(Q) bsl 4) bor trx(R),
    {Cha, {int_to_utf8(Cha),Rest}};
multi_hex_esc(<<"x{",N,O,P,Q,R,S,$},Rest/binary>>,_) when (?is_hex_char(N) and
                                                           ?is_hex_char(O) and
                                                           ?is_hex_char(P) and
                                                           ?is_hex_char(Q) and
                                                           ?is_hex_char(R) and
                                                           ?is_hex_char(S)) ->
    Cha = (trx(N) bsl 20) bor (trx(O) bsl 16) bor (trx(P) bsl 12) bor (trx(Q) bsl 8) bor (trx(R) bsl 4) bor trx(S),
    {Cha, {int_to_utf8(Cha),Rest}};
multi_hex_esc(<<$x,N,O,Rest/binary>>,_) when (?is_hex_char(N) and
                                              ?is_hex_char(O)) ->
    Cha = (trx(N) bsl 4) bor trx(O),
    {Cha, {<<Cha>>,Rest}};
multi_hex_esc(<<$x,N,Rest/binary>>,_) when ?is_hex_char(N) ->
    Cha = trx(N),
    {Cha, {<<Cha>>,Rest}};
multi_hex_esc(_,_) ->
    {no, no}.

single_esc($") ->
    $";
single_esc(?SPACE) ->
    ?SPACE;
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
	    io:format("~s: " ++ Str, [get(testfile) | Lst]);
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
    file:write(F, copyright_generator:erlang()),
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
    convert_file_to_utf8(ErlFileName),
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

dumponesplit(F,{run,RE,Line,O,TS}) ->
    %% ScriptFormat is verbatim, however
    %% ~~ ~s ~w ~n is for io:format and \\ \" \n is for perl.
    ScriptFormat = """
        perl -e '
        $x = join(":",split(/~s/~s,"~s",~w));
        $x =~~ s/\\/\\\\/g;
        $x =~~ s/\"/\\"/g;
        print "    <<\"$x\">> = iolist_to_binary(join(re:split(\"~s\",\"~s\",~w))),\n";'~n
        """,
    [begin
	 {NO,_} = pick_exec_options(O++Op),
	 SSS = opt_to_string(NO),
	 LLL = unicode:characters_to_list(RE),
	 case (catch iolist_to_binary(LLL)) of
	     X when is_binary(X) ->
		 io:format(F, ScriptFormat,
			   [zsafe(safe(RE)),
			    SSS,
			    ysafe(safe(Str)),
                            0,
			    dsafe(safe(Str)),
			    dsafe2(safe(RE)),
			    NO++[trim]]),
		 io:format(F, ScriptFormat,
			   [zsafe(safe(RE)),
			    SSS,
			    ysafe(safe(Str)),
                            2,
			    dsafe(safe(Str)),
			    dsafe2(safe(RE)),
			    NO++[{parts,2}]]),
		 io:format(F, ScriptFormat,
			   [zsafe(safe(RE)),
			    SSS,
			    ysafe(safe(Str)),
                            -1,
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
    file:write(F, copyright_generator:erlang()),
    io:format(F,"-module(~s).~n",[ErlModule]),
    io:format(F,"-export([run/0]).~n",[]),
    io:format(F,"-compile(no_native).~n",[]),
    io:format(F,"%% This file is generated by running ~w:gen_repl_test(~p)~n",
	      [?MODULE,OneFile]),
    io:format(F,"run() ->~n",[]),
    [ io:format(F,"    ~s(),~n",[FunName]) || FunName <- FunList ],
    file:close(F),
    os:cmd("sh "++ PerlShellScript++" 2>/dev/null >> "++ErlFileName),
    convert_file_to_utf8(ErlFileName),
    io:format("~s~n",[os:cmd("wc -l "++ErlFileName)]),
    ok.

convert_file_to_utf8(FileName) ->
    TmpFile = FileName ++ ".tmp",
    case os:cmd("iconv -f ISO-8859-1 -t UTF-8 " ++ FileName ++ " > " ++ TmpFile) of
        [] ->
            ok = file:rename(TmpFile, FileName);
        Error ->
            io:format("ERROR: Convertion from Latin1 to UTF-8 seems to have failed:\n"
                      "~s\nFile ~w (probably) still in Latin1 format.",
                      Error, FileName)
    end.


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

dumpone(F,{run,RE,Line,O,TS}) ->
    %% ScriptFormat is verbatim, however
    %% ~~ ~s ~w ~n is for io:format and \\ \" \n is for perl.
    ScriptFormat = """
        perl -e '
        $x = "~s";
        $x =~~ s/~s/~s/~s;
        $x =~~ s/\\/\\\\/g;
        $x =~~ s/\"/\\"/g;
        print "    <<\"$x\">> = iolist_to_binary(re:replace(\"~s\",\"~s\",\"~s\",~w)),\n";'~n
        """,
    [begin
	 {NO,_} = pick_exec_options(O++Op),
	 SSS = opt_to_string(NO),
	 RS = ranstring(),
	 LLL = unicode:characters_to_list(RE),
	 case (catch iolist_to_binary(LLL)) of
	     X when is_binary(X) ->
                 io:format(F, ScriptFormat, [ysafe(safe(Str)),
                                             zsafe(safe(RE)),
                                             perlify(binary_to_list(RS)),
                                             SSS,
                                             dsafe(safe(Str)),
                                             dsafe(safe(RE)),
                                             xsafe(RS),
                                             NO]),
                 %% Same again but with global replace
                 io:format(F, ScriptFormat, [ysafe(safe(Str)),
                                             zsafe(safe(RE)),
                                             perlify(binary_to_list(RS)),
                                             [$g | SSS],
                                             dsafe(safe(Str)),
                                             dsafe(safe(RE)),
                                             xsafe(RS),
                                             NO++[global]]);
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

re_compile(RE, Options) ->
    inc_counter(re_compile),
    put(re_compile_opts, Options),
    re:compile(RE, Options).

re_run(Subj, RE, Opts) ->
    %%io:format("re:run(~p, ~p, ~p)\n", [Subj, RE, Opts]),
    inc_counter(re_run),
    put(re_run_opts, Opts),
    re:run(Subj, RE, Opts).

re_replace(Subj, RE, Repl, Opts) ->
    inc_counter(re_replace),
    put(re_run_opts, Opts),
    re:replace(Subj, RE, Repl, Opts).

used_options() ->
    RunOpts = get(re_run_opts),
    case get(re_compile_opts) of
        undefined ->
            {run, RunOpts};
        CompOpts ->
            {compile, CompOpts, run, RunOpts}
    end.

inc_counter(Name) ->
    case get(Name) of
        undefined -> put(Name, 1);
        N -> put(Name, N+1)
    end.
