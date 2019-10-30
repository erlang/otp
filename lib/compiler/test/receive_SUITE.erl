%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2018. All Rights Reserved.
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
%%% Purpose : Compiles various modules with tough code

-module(receive_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 export/1,recv/1,coverage/1,otp_7980/1,ref_opt/1,
	 wait/1,recv_in_try/1,double_recv/1,receive_var_zero/1,
         match_built_terms/1,elusive_common_exit/1,
         return_before_receive/1,trapping/1]).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    slow_group() ++ [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [recv,coverage,otp_7980,export,wait,
       recv_in_try,double_recv,receive_var_zero,
       match_built_terms,elusive_common_exit,
       return_before_receive,trapping]},
     {slow,[],[ref_opt]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

slow_group() ->
    case ?MODULE of
	receive_SUITE ->
            %% Canononical module name. Run slow cases.
            [{group,slow}];
        _ ->
            %% Cloned module. Don't run.
            []
    end.

-record(state, {ena = true}).

recv(Config) when is_list(Config) ->
    Pid = spawn_link(fun() -> loop(#state{}) end),
    Self = self(),
    Pid ! {Self,test},
    receive
	{ok,test} -> ok;
	{error,Other} ->
	    io:format("Got unpexected ~p", [Other]),
	    ct:fail(unexpected)
    after 10000 ->
	    ct:fail(no_answer)
    end,
    receive
	X ->
	    io:format("Unexpected extra message: ~p", [X]),
	    ct:fail(unexpected)
    after 10 ->
	    ok
    end,
    ok.

loop(S) ->
    receive
	_ when S#state.ena == false ->
	    loop(S);
	{P,test} ->
	    P ! {ok,test},
	    loop(S);
	_X ->
	    loop(S)
    end.

coverage(Config) when is_list(Config) ->
    do_link(self()),
    do_unlink(self()),
    do_monitor_node(node(), true),
    do_monitor_node(node(), false),
    do_group_leader(group_leader(), self()),
    id(node(self())),

    erlang:'!'(self(), {a,10}),
    self() ! {b,20},
    [{a,10},{b,20}] = receive_all(),
    self() ! {c,42},
    receive
	{c,42} ->
	    ok
    after infinity ->
	    exit(cant_happen)
    end,

    self() ! 17,
    self() ! 19,
    59 = tuple_to_values(infinity, x),
    61 = tuple_to_values(999999, x),
    0 = tuple_to_values(1, x),

    {'EXIT',{{badmap,[]},_}} = (catch monitor_plus_badmap(self())),

    ok.

monitor_plus_badmap(Pid) ->
    monitor(process, Pid) + []#{}.

receive_all() ->
    receive
	Any ->
	    [Any|receive_all()]
    after 0 ->
	    []
    end.

do_monitor_node(Node, Bool) ->
    monitor_node(Node, Bool).

do_link(Pid) ->
    link(Pid).

do_unlink(Pid) ->
    unlink(Pid).

do_group_leader(Leader, Pid) ->
    group_leader(Leader, Pid).


%% cover sys_core_fold:tuple_to_values/2
tuple_to_values(infinity, X) ->
    {A,B} = case X of
		x ->
		    receive
			Any ->
			    {42,Any}
		    end
	    end,
    A+B;
tuple_to_values(Timeout, X) ->
    {A,B} = case X of
		x ->
		    receive
			Any ->
			    {42,Any}
		    after Timeout ->
			    {0,0}
		    end
	    end,
    A+B.

%% OTP-7980. Thanks to Vincent de Phily. The following code would
%% be inccorrectly optimized by beam_jump.

otp_7980(Config) when is_list(Config) ->
    7 = otp_7980_add_clients(10),
    ok.

otp_7980_add_clients(Count) ->
    Timeout = 42,
    lists:foldl(fun(_, N) ->
			case N of
                           1 -> ok;
			    _ -> receive after Timeout -> ok end
			end,
			N - 1
		end, Count, [1,2,3]).

ref_opt(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Sources = filelib:wildcard(filename:join([DataDir,"ref_opt","*.{erl,S}"])),
    test_lib:p_run(fun(Src) ->
			   do_ref_opt(Src, PrivDir)
		   end, Sources),
    cover_recv_instructions(),
    ok.

do_ref_opt(Source, PrivDir) ->
    try
	Ext = filename:extension(Source),
	{ok,Mod} = compile:file(Source, [report_errors,report_warnings,
					 {outdir,PrivDir}] ++
					[from_asm || Ext =:= ".S" ]),
	Base = filename:rootname(filename:basename(Source), Ext),
	code:purge(list_to_atom(Base)),
	BeamFile = filename:join(PrivDir, Base),
	code:load_abs(BeamFile),
	ok = Mod:Mod(),
	{beam_file,Mod,_,_,_,Code} = beam_disasm:file(BeamFile),
	case Base of
	    "no_"++_ ->
		[] = collect_recv_opt_instrs(Code);
	    "yes_"++_ ->
		[{recv_mark,{f,L}},{recv_set,{f,L}}] =
		    collect_recv_opt_instrs(Code)
	end,
	ok
    catch Class:Error:Stk ->
	    io:format("~s: ~p ~p\n~p\n", [Source,Class,Error,Stk]),
	    error
    end.

collect_recv_opt_instrs(Code) ->
    L = [ [I || I <- Is,
		begin
		    case I of
			{recv_mark,{f,_}} -> true;
			{recv_set,{f,_}} -> true;
			_ -> false
		    end
		end] || {function,_,_,_,Is} <- Code],
    lists:append(L).

cover_recv_instructions() ->
    %% We want to cover the handling of recv_mark and recv_set in beam_utils.
    %% Since those instructions are introduced in a late optimization pass,
    %% beam_utils:live_opt() will not see them unless the compilation is
    %% started from a .S file. The compile_SUITE:asm/1 test case will
    %% compile all test suite files to .S and then run them through the
    %% compiler again.
    %%
    %% Here will we will ensure that this modules contains recv_mark
    %% and recv_set instructions.
    Pid = spawn_link(fun() ->
			     receive {Parent,Ref} ->
				     Parent ! Ref
			     end
		     end),
    Ref = make_ref(),
    Pid ! {self(),Ref},
    receive
	Ref -> ok
    end.

export(Config) when is_list(Config) ->
    Ref = make_ref(),
    self() ! {result,Ref,42},
    42 = export_1(Ref),
    {error,timeout} = export_1(Ref),

    self() ! {result,Ref},
    {ok,Ref} = export_2(),

    ok.

export_1(Reference) ->
    id(Reference),
    receive
	{result,Reference,Result} ->
	    Result
    after 1 ->
	    Result = {error,timeout}
    end,
    %% Result ({x,1}) is used, but not the return value ({x,0})
    %% of the receive. Used to be incorrectly optimized
    %% by beam_block.
    id({build,self()}),
    Result.

export_2() ->
    receive {result,Result} -> ok end,
    {ok,Result}.

wait(Config) when is_list(Config) ->
    self() ! <<42>>,
    <<42>> = wait_1(r, 1, 2),
    {1,2,3} = wait_1(1, 2, 3),
    {'EXIT',{timeout_value,_}} = (catch receive after [] -> timeout end),
    ok.

wait_1(r, _, _) ->
    receive
	B when byte_size(B) > 0 ->
	    B
    end;
%% beam_utils would wrongly assume that wait/1 could fall through
%% to the next clause.
wait_1(A, B, C) ->
    {A,B,C}.

recv_in_try(_Config) ->
    self() ! {ok,fh}, {ok,fh} = recv_in_try(infinity, native),
    self() ! {ok,ignored}, {ok,42} = recv_in_try(infinity, plain),
    self() ! {error,ignored}, nok = recv_in_try(infinity, plain),
    timeout = recv_in_try(1, plain),
    ok.

recv_in_try(Timeout, Format) ->
    try
	receive
	    {Status,History} ->
                %% {test,is_tuple,{f,148},[{x,0}]}.
                %% {test,test_arity,{f,148},[{x,0},2]}.
                %% {get_tuple_element,{x,0},0,{y,1}}.  %y1 is fragile.
                %%
                %% %% Here the fragility of y1 would be be progated to
                %% %% the 'catch' below. Incorrect, since get_tuple_element
                %% %% can't fail.
                %% {get_tuple_element,{x,0},1,{x,2}}.
                %%
                %% remove_message.                     %y1 fragility cleared.
		FH = case Format of
			native ->
                             id(History);
			plain ->
                             id(42)
		    end,
		case Status of
		    ok ->
			{ok,FH};
		    error ->
			nok
		end
	after Timeout ->
		timeout
	end
    catch
        %% The fragility of y1 incorrectly propagated to here.
        %% beam_validator would complain.
	throw:{error,Reason} ->
	    {nok,Reason}
    end.

%% ERL-703. The compiler would crash because beam_utils:anno_defs/1
%% failed to take into account that code after loop_rec_end is
%% unreachable.

double_recv(_Config) ->
    self() ! {more,{a,term}},
    ok = do_double_recv({more,{a,term}}, any),
    self() ! message,
    ok = do_double_recv(whatever, message),

    error = do_double_recv({more,42}, whatever),
    error = do_double_recv(whatever, whatever),
    ok.

do_double_recv({more, Rest}, _Msg) ->
    receive
        {more, Rest} ->
            ok
    after 0 ->
            error
    end;
do_double_recv(_, Msg) ->
    receive
        Msg ->
            ok
    after 0 ->
            error
    end.

%% Test 'after Z', when Z =:= 0 been propagated as an immediate by the type
%% optimization pass.
receive_var_zero(Config) when is_list(Config) ->
    self() ! x,
    self() ! y,
    Z = zero(),
    timeout = receive
                  z -> ok
              after Z -> timeout
              end,
    timeout = receive
              after Z -> timeout
              end,
    self() ! w,
    receive
	x -> ok;
	Other ->
	    ct:fail({bad_message,Other})
    end.

zero() -> 0.

%% ERL-862; the validator would explode when a term was constructed in a
%% receive guard.

-define(MATCH_BUILT_TERM(Ref, Expr),
        (fun() ->
                 Ref = make_ref(),
                 A = id($a),
                 B = id($b),
                 Built = id(Expr),
                 self() ! {Ref, A, B},
                 receive
                     {Ref, A, B} when Expr =:= Built ->
                         ok
                 after 5000 ->
                     ct:fail("Failed to match message with term built in "
                             "receive guard.")
                 end
         end)()).

match_built_terms(Config) when is_list(Config) ->
    ?MATCH_BUILT_TERM(Ref, [A, B]),
    ?MATCH_BUILT_TERM(Ref, {A, B}),
    ?MATCH_BUILT_TERM(Ref, <<A, B>>),
    ?MATCH_BUILT_TERM(Ref, #{ 1 => A, 2 => B}).

elusive_common_exit(_Config) ->
    self() ! {1, a},
    self() ! {2, b},
    {[z], [{2,b},{1,a}]} = elusive_loop([x,y,z], 2, []),

    CodeServer = whereis(code_server),
    Self = self(),
    Self ! {Self, abc},
    Self ! {CodeServer, []},
    Self ! {Self, other},
    try elusive2([]) of
        Unexpected ->
            ct:fail("Expected an exception; got ~p\n", [Unexpected])
    catch
        throw:[other, CodeServer, Self] ->
            ok
    end,

    ok.

elusive_loop(List, 0, Results) ->
    {List, Results};
elusive_loop(List, ToReceive, Results) ->
    {Result, RemList} =
        receive
            {_Pos, _R} = Res when List =/= [] ->
                [_H|T] = List,
                {Res, T};
            {_Pos, _R} = Res when List =:= [] ->
                {Res, []}
        end,
    %% beam_ssa_pre_codegen:fix_receives() would fail to find
    %% the common exit block for this receive. That would mean
    %% that it would not insert all necessary copy instructions.
    elusive_loop(RemList, ToReceive-1, [Result | Results]).


elusive2(Acc) ->
    receive
        {Pid, abc} ->
            ok;
        {Pid, []} ->
            ok;
        {Pid, Res} ->
            %% beam_ssa_pre_codegen:find_loop_exit/2 attempts to find
            %% the first block of the common code after the receive
            %% statement. It used to only look at the two last clauses
            %% of the receive. In this function, the last two clauses
            %% don't have any common block, so it would be assumed
            %% that there was no common block for any of the
            %% clauses. That would mean that copy instructions would
            %% not be inserted as needed.
            throw([Res | Acc])
    end,
    %% Common code.
    elusive2([Pid | Acc]).

return_before_receive(_Config) ->
    ref_received = do_return_before_receive(),
    ok.

do_return_before_receive() ->
    Ref = make_ref(),
    self() ! {ref,Ref},
    maybe_receive(id(false)),
    receive
        {ref,Ref} ->
            ref_received
    after 1 ->
            %% Can only be reached if maybe_receive/1 returned
            %% with the receive marker set.
            timeout
    end.

maybe_receive(Bool) ->
    NewRef = make_ref(),
    case Bool of
        true ->
            receive
                NewRef ->
                    ok
            end;
        false ->
            %% The receive marker must not be set when
            %% leaving this function.
            ok
    end.

trapping(_Config) ->
    ok = do_trapping(0),
    ok = do_trapping(1),
    ok.

%% Simplified from emulator's binary_SUITE:trapping/1.
do_trapping(N) ->
    Ref = make_ref(),
    self() ! Ref,
    case N rem 2 of
	0 ->
            %% Would generate recv_set _, label _, wait_timeout _ _,
            %% which the loader can't handle.
            receive after 1 -> ok end;
	1 ->
            void
    end,
    receive Ref -> ok end,
    receive after 1 -> ok end.

id(I) -> I.
