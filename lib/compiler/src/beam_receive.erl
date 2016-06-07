%%
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
%%

-module(beam_receive).
-export([module/2]).
-import(lists, [foldl/3,reverse/1,reverse/2]).

%%%
%%% In code such as:
%%%
%%%    Ref = make_ref(),        %Or erlang:monitor(process, Pid)
%%%      .
%%%      .
%%%      .
%%%    receive
%%%       {Ref,Reply} -> Reply
%%%    end.
%%%
%%% we know that none of the messages that exist in the message queue
%%% before the call to make_ref/0 can be matched out in the receive
%%% statement. Therefore we can avoid going through the entire message
%%% queue if we introduce two new instructions (here written as
%%% BIFs in pseudo-Erlang):
%%%
%%%    recv_mark(SomeUniqInteger),
%%%    Ref = make_ref(),
%%%      .
%%%      .
%%%      .
%%%    recv_set(SomeUniqInteger),
%%%    receive
%%%       {Ref,Reply} -> Reply
%%%    end.
%%%
%%% The recv_mark/1 instruction will save the current position and
%%% SomeUniqInteger in the process context. The recv_set
%%% instruction will verify that SomeUniqInteger is still stored
%%% in the process context. If it is, it will set the current pointer
%%% for the message queue (the next message to be read out) to the
%%% position that was saved by recv_mark/1.
%%%
%%% The remove_message instruction must be modified to invalidate
%%% the information stored by the previous recv_mark/1, in case there
%%% is another receive executed between the calls to recv_mark/1 and
%%% recv_set/1.
%%%
%%% We use a reference to a label (i.e. a position in the loaded code)
%%% as the SomeUniqInteger.
%%%

module({Mod,Exp,Attr,Fs0,Lc}, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    Code = {Mod,Exp,Attr,Fs,Lc},
    {ok,Code}.

%%%
%%% Local functions.
%%%

function({function,Name,Arity,Entry,Is}) ->
    try
	D = beam_utils:index_labels(Is),
	{function,Name,Arity,Entry,opt(Is, D, [])}
    catch
	Class:Error ->
	    Stack = erlang:get_stacktrace(),
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

opt([{call_ext,A,{extfunc,erlang,spawn_monitor,A}}=I0|Is0], D, Acc)
  when A =:= 1; A =:= 3 ->
    case ref_in_tuple(Is0) of
	no ->
	    opt(Is0, D, [I0|Acc]);
	{yes,Regs,Is1,MatchReversed} ->
	    %% The call creates a brand new reference. Now
	    %% search for a receive statement in the same
	    %% function that will match against the reference.
	    case opt_recv(Is1, Regs, D) of
		no ->
		    opt(Is0, D, [I0|Acc]);
		{yes,Is,Lbl} ->
		    opt(Is, D, MatchReversed++[I0,{recv_mark,{f,Lbl}}|Acc])
	    end
    end;
opt([{call_ext,Arity,{extfunc,erlang,Name,Arity}}=I|Is0], D, Acc) ->
    case creates_new_ref(Name, Arity) of
	true ->
	    %% The call creates a brand new reference. Now
	    %% search for a receive statement in the same
	    %% function that will match against the reference.
	    case opt_recv(Is0, regs_init_x0(), D) of
		no ->
		    opt(Is0, D, [I|Acc]);
		{yes,Is,Lbl} ->
		    opt(Is, D, [I,{recv_mark,{f,Lbl}}|Acc])
	    end;
	false ->
	    opt(Is0, D, [I|Acc])
    end;
opt([I|Is], D, Acc) ->
    opt(Is, D, [I|Acc]);
opt([], _, Acc) ->
    reverse(Acc).

ref_in_tuple([{test,is_tuple,_,[{x,0}]}=I1,
	      {test,test_arity,_,[{x,0},2]}=I2,
	      {block,[{set,[_],[{x,0}],{get_tuple_element,0}},
		      {set,[Dst],[{x,0}],{get_tuple_element,1}}|Bl]}=I3|Is]) ->
    ref_in_tuple_1(Bl, Dst, Is, [I3,I2,I1]);
ref_in_tuple([{test,is_tuple,_,[{x,0}]}=I1,
	      {test,test_arity,_,[{x,0},2]}=I2,
	      {block,[{set,[Dst],[{x,0}],{get_tuple_element,1}}|Bl]}=I3|Is]) ->
    ref_in_tuple_1(Bl, Dst, Is, [I3,I2,I1]);
ref_in_tuple(_) -> no.

ref_in_tuple_1(Bl, Dst, Is, MatchReversed) ->
    Regs0 = regs_init_singleton(Dst),
    Regs = opt_update_regs_bl(Bl, Regs0),
    {yes,Regs,Is,MatchReversed}.

%% creates_new_ref(Name, Arity) -> true|false.
%%  Return 'true' if the BIF Name/Arity will create a new reference.
creates_new_ref(monitor, 2) -> true;
creates_new_ref(make_ref, 0) -> true;
creates_new_ref(_, _) -> false.

%% opt_recv([Instruction], Regs, LabelIndex) -> no|{yes,[Instruction]}
%%  Search for a receive statement that will only retrieve messages
%%  that contain the newly created reference (which is currently in {x,0}).
opt_recv(Is, Regs, D) ->
    L = gb_sets:empty(),
    opt_recv(Is, D, Regs, L, []).

opt_recv([{label,L}=Lbl,{loop_rec,{f,Fail},_}=Loop|Is], D, R0, _, Acc) ->
    R = regs_kill_not_live(0, R0),
    case regs_empty(R) of
	false ->
	    %% We now have the new reference in Y registers
	    %% and the current instruction is the beginning of a
	    %% receive statement. We must now verify that only messages
	    %% that contain the reference will be matched.
	    case opt_ref_used(Is, R, Fail, D) of
		false ->
		    no;
		true ->
		    RecvSet = {recv_set,{f,L}},
		    {yes,reverse(Acc, [RecvSet,Lbl,Loop|Is]),L}
	    end;
	true ->
	    no
    end;
opt_recv([I|Is], D, R0, L0, Acc) ->
    {R,L} = opt_update_regs(I, R0, L0),
    case regs_empty(R) of
	true ->
	    %% The reference is no longer alive. There is no
	    %% point in continuing the search.
	    no;
	false ->
	    opt_recv(Is, D, R, L, [I|Acc])
    end;
opt_recv([], _, _, _, _) -> no.

opt_update_regs({block,Bl}, R, L) ->
    {opt_update_regs_bl(Bl, R),L};
opt_update_regs({call,_,_}, R, L) ->
    {regs_kill_not_live(0, R),L};
opt_update_regs({call_ext,_,_}, R, L) ->
    {regs_kill_not_live(0, R),L};
opt_update_regs({call_fun,_}, R, L) ->
    {regs_kill_not_live(0, R),L};
opt_update_regs({kill,Y}, R, L) ->
    {regs_kill([Y], R),L};
opt_update_regs({'catch',_,{f,Lbl}}, R, L) ->
    {R,gb_sets:add(Lbl, L)};
opt_update_regs({catch_end,_}, R, L) ->
    {R,L};
opt_update_regs({label,Lbl}, R, L) ->
    case gb_sets:is_member(Lbl, L) of
	false ->
	    %% We can't allow arbitrary labels (since the receive
	    %% could be entered without first creating the reference).
	    {regs_init(),L};
	true ->
	    %% A catch label for a previously seen catch instruction is OK.
	    {R,L}
    end;
opt_update_regs({try_end,_}, R, L) ->
    {R,L};
opt_update_regs({line,_}, R, L) ->
    {R,L};
opt_update_regs(_I, _R, L) ->
    %% Unrecognized instruction. Abort the search.
    {regs_init(),L}.

opt_update_regs_bl([{set,Ds,_,{alloc,Live,_}}|Is], Regs0) ->
    Regs1 = regs_kill_not_live(Live, Regs0),
    Regs = regs_kill(Ds, Regs1),
    opt_update_regs_bl(Is, Regs);
opt_update_regs_bl([{set,[Dst]=Ds,[Src],move}|Is], Regs0) ->
    Regs1 = regs_kill(Ds, Regs0),
    Regs = case regs_is_member(Src, Regs1) of
	       false -> Regs1;
	       true -> regs_add(Dst, Regs1)
	   end,
    opt_update_regs_bl(Is, Regs);
opt_update_regs_bl([{set,Ds,_,_}|Is], Regs0) ->
    Regs = regs_kill(Ds, Regs0),
    opt_update_regs_bl(Is, Regs);
opt_update_regs_bl([], Regs) -> Regs.

%% opt_ref_used([Instruction], RefRegs, FailLabel, LabelIndex) -> true|false
%%  Return 'true' if it is certain that only messages that contain the same
%%  reference as in RefRegs can be matched out. Otherwise return 'false'.
%%
%%  Basically, we follow all possible paths through the receive statement.
%%  If all paths are safe, we return 'true'.
%%
%%  A branch to FailLabel is safe, because it exits the receive statement
%%  and no further message may be matched out.
%%
%%  If a path hits an comparision between RefRegs and part of the message,
%%  that path is safe (any messages that may be matched further down the
%%  path is guaranteed to contain the reference).
%%
%%  Otherwise, if we hit a 'remove_message' instruction, we give up
%%  and return 'false' (the optimization is definitely unsafe). If
%%  we hit an unrecognized instruction, we also give up and return
%%  'false' (the optimization may be unsafe).

opt_ref_used(Is, RefRegs, Fail, D) ->
    Done = gb_sets:singleton(Fail),
    Regs = regs_init_x0(),
    try
	_ = opt_ref_used_1(Is, RefRegs, D, Done, Regs),
	true
    catch
	throw:not_used ->
	    false
    end.

%% This functions only returns if all paths through the receive
%% statement are safe, and throws an 'not_used' term otherwise.
opt_ref_used_1([{block,Bl}|Is], RefRegs, D, Done, Regs0) ->
    Regs = opt_ref_used_bl(Bl, Regs0),
    opt_ref_used_1(Is, RefRegs, D, Done, Regs);
opt_ref_used_1([{test,is_eq_exact,{f,Fail},Args}|Is],
	       RefRegs, D, Done0, Regs) ->
    Done = opt_ref_used_at(Fail, RefRegs, D, Done0, Regs),
    case is_ref_msg_comparison(Args, RefRegs, Regs) of
	false ->
	    opt_ref_used_1(Is, RefRegs, D, Done, Regs);
	true ->
	    %% The instructions that follow (Is) can only be executed
	    %% if the message contains the same reference as in RefRegs.
	    Done
    end;
opt_ref_used_1([{test,is_ne_exact,{f,Fail},Args}|Is],
	       RefRegs, D, Done0, Regs) ->
    Done = opt_ref_used_1(Is, RefRegs, D, Done0, Regs),
    case is_ref_msg_comparison(Args, RefRegs, Regs) of
	false ->
	    opt_ref_used_at(Fail, RefRegs, D, Done, Regs);
	true ->
	    Done
    end;
opt_ref_used_1([{test,_,{f,Fail},_}|Is], RefRegs, D, Done0, Regs) ->
    Done = opt_ref_used_at(Fail, RefRegs, D, Done0, Regs),
    opt_ref_used_1(Is, RefRegs, D, Done, Regs);
opt_ref_used_1([{select,_,_,{f,Fail},List}|_], RefRegs, D, Done, Regs) ->
    Lbls = [F || {f,F} <- List] ++ [Fail],
    opt_ref_used_in_all(Lbls, RefRegs, D, Done, Regs);
opt_ref_used_1([{label,Lbl}|Is], RefRegs, D, Done, Regs) ->
    case gb_sets:is_member(Lbl, Done) of
	true -> Done;
	false -> opt_ref_used_1(Is, RefRegs, D, Done, Regs)
    end;
opt_ref_used_1([{loop_rec_end,_}|_], _, _, Done, _) ->
    Done;
opt_ref_used_1([_I|_], _RefReg, _D, _Done, _Regs) ->
    %% The optimization may be unsafe.
    throw(not_used).

%% is_ref_msg_comparison(Args, RefRegs, RegisterSet) -> true|false.
%%  Return 'true' if Args denotes a comparison between the
%%  reference and message or part of the message.
is_ref_msg_comparison([R1,R2], RefRegs, Regs) ->
    (regs_is_member(R2, RefRegs) andalso regs_is_member(R1, Regs)) orelse
    (regs_is_member(R1, RefRegs) andalso regs_is_member(R2, Regs)).

opt_ref_used_in_all([L|Ls], RefRegs, D, Done0, Regs) ->
    Done = opt_ref_used_at(L, RefRegs, D, Done0, Regs),
    opt_ref_used_in_all(Ls, RefRegs, D, Done, Regs);
opt_ref_used_in_all([], _, _, Done, _) -> Done.

opt_ref_used_at(Fail, RefRegs, D, Done0, Regs) ->
    case gb_sets:is_member(Fail, Done0) of
	true ->
	    Done0;
	false ->
	    Is = beam_utils:code_at(Fail, D),
	    Done = opt_ref_used_1(Is, RefRegs, D, Done0, Regs),
	    gb_sets:add(Fail, Done)
    end.

opt_ref_used_bl([{set,[],[],remove_message}|_], _) ->
    %% We have proved that a message that does not depend on the
    %% reference can be matched out.
    throw(not_used);
opt_ref_used_bl([{set,Ds,Ss,_}|Is], Regs0) ->
    case regs_all_members(Ss, Regs0) of
	false ->
	    %% The destination registers may be assigned values that
	    %% are not dependent on the message being matched.
	    Regs = regs_kill(Ds, Regs0),
	    opt_ref_used_bl(Is, Regs);
	true ->
	    %% All the sources depend on the message directly or
	    %% indirectly.
	    Regs = regs_add_list(Ds, Regs0),
	    opt_ref_used_bl(Is, Regs)
    end;
opt_ref_used_bl([], Regs) -> Regs.

%%%
%%% Functions for keeping track of a set of registers.
%%%

%% regs_init() -> RegisterSet
%%  Return an empty set of registers.

regs_init() ->
    {0,0}.

%% regs_init_singleton(Register) -> RegisterSet
%%  Return a set that only contains one register.

regs_init_singleton(Reg) ->
    regs_add(Reg, regs_init()).

%% regs_init_x0() -> RegisterSet
%%  Return a set that only contains the {x,0} register.

regs_init_x0() ->
    {1 bsl 0,0}.

%% regs_empty(Register) -> true|false
%%  Test whether the register set is empty.

regs_empty(R) ->
    R =:= {0,0}.

%% regs_kill_not_live(Live, RegisterSet) -> RegisterSet'
%%  Kill all registers indicated not live by Live.

regs_kill_not_live(Live, {Xregs,Yregs}) ->
    {Xregs band ((1 bsl Live)-1),Yregs}.

%% regs_kill([Register], RegisterSet) -> RegisterSet'
%%  Kill all registers mentioned in the list of registers.

regs_kill([{x,N}|Rs], {Xregs,Yregs}) ->
    regs_kill(Rs, {Xregs band (bnot (1 bsl N)),Yregs});
regs_kill([{y,N}|Rs], {Xregs,Yregs}) ->
    regs_kill(Rs, {Xregs,Yregs band (bnot (1 bsl N))});
regs_kill([{fr,_}|Rs], Regs) ->
    regs_kill(Rs, Regs);
regs_kill([], Regs) -> Regs.

regs_add_list(List, Regs) ->
    foldl(fun(R, A) -> regs_add(R, A) end, Regs, List).

%% regs_add(Register, RegisterSet) -> RegisterSet'
%%  Add a new register to the set of registers.

regs_add({x,N}, {Xregs,Yregs}) ->
    {Xregs bor (1 bsl N),Yregs};
regs_add({y,N}, {Xregs,Yregs}) ->
    {Xregs,Yregs bor (1 bsl N)}.

%% regs_all_members([Register], RegisterSet) -> true|false
%%  Test whether all of the registers are part of the register set.

regs_all_members([R|Rs], Regs) ->
    regs_is_member(R, Regs) andalso regs_all_members(Rs, Regs);
regs_all_members([], _) -> true.

%% regs_is_member(Register, RegisterSet) -> true|false
%%  Test whether Register is part of the register set.

regs_is_member({x,N}, {Regs,_}) -> Regs band (1 bsl N) =/= 0;
regs_is_member({y,N}, {_,Regs}) -> Regs band (1 bsl N) =/= 0;
regs_is_member(_, _) -> false.
