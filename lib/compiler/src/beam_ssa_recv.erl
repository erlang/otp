%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

-module(beam_ssa_recv).
-export([module/2]).

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

-include("beam_ssa.hrl").
-import(lists, [all/2,reverse/2]).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
                    {'ok',beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, _Opts) ->
    Fs = [function(F) || F <- Fs0],
    {ok,Module#b_module{body=Fs}}.

%%%
%%% Local functions.
%%%

function(#b_function{anno=Anno,bs=Blocks0}=F) ->
    try
        Blocks = opt(Blocks0),
        F#b_function{bs=Blocks}
    catch
        Class:Error:Stack ->
            #{func_info:={_,Name,Arity}} = Anno,
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

opt(Blocks) ->
    Linear = beam_ssa:linearize(Blocks),
    opt(Linear, Blocks, []).

opt([{L,#b_blk{is=[#b_set{op=peek_message}|_]}=Blk0}|Bs], Blocks0, Preds) ->
    %% Search for a suitable reference creating call in one of the predecessor
    %% blocks. Whether we find such a call or not, we always clear the
    %% the list of predecessors to ensure that any nested receive can't
    %% search above the current receive.
    case recv_opt(Preds, L, Blocks0) of
        {yes,Blocks1} ->
            Blk = beam_ssa:add_anno(recv_set, L, Blk0),
            Blocks = Blocks1#{L:=Blk},
            opt(Bs, Blocks, []);
        no ->
            opt(Bs, Blocks0, [])
    end;
opt([{L,_}|Bs], Blocks, Preds) ->
    opt(Bs, Blocks, [L|Preds]);
opt([], Blocks, _) -> Blocks.

recv_opt([L|Ls], RecvLbl, Blocks) ->
    #b_blk{is=Is0} = Blk0 = map_get(L, Blocks),
    case recv_opt_is(L, {Is0,[]}, RecvLbl, Blocks) of
        {yes,Is} ->
            Blk = Blk0#b_blk{is=Is},
            {yes,Blocks#{L:=Blk}};
        no ->
            recv_opt(Ls, RecvLbl, Blocks)
    end;
recv_opt([], _, _Blocks) -> no.

recv_opt_is(L, {Is0,PreIs0}, RecvLbl, Blocks) ->
    case recv_opt_makes_ref(Is0, RecvLbl, Blocks, PreIs0) of
        {yes,Ref,[I0|Is],PreIs} ->
            %% This instruction creates a reference. Search the
            %% rest of the function to find whether it is always
            %% matched out by a receive.
            %%
            %% Note: It is important that we don't search the entire
            %% block that this instruction is part of, because the
            %% block could contain a remove_message instruction part
            %% of a previous receive. For an example, see
            %% receive_SUITE_data/ref_opt/yes_16.erl.
            TempBlk = (map_get(L, Blocks))#b_blk{is=Is},
            TempBlocks = Blocks#{L:=TempBlk},
            case opt_ref_used(L, Ref, TempBlocks) of
                true ->
                    %% This reference will always be matched to
                    %% a part of any message received.
                    I = beam_ssa:add_anno(recv_mark, RecvLbl, I0),
                    {yes,reverse(PreIs, [I|Is])};
                false ->
                    %% There could be more than one reference-creating
                    %% instruction in a block, so we will need to
                    %% continue the search. For an example, see
                    %% receive_SUITE_data/ref_opt/yes_15.erl.
                    recv_opt_is(L, {Is,[I0|PreIs]}, RecvLbl, Blocks)
            end;
        no ->
            no
    end.

recv_opt_makes_ref([#b_set{op=call}=I0|Is], RecvLbl, Blocks0, Acc) ->
    case makes_ref(I0, Blocks0) of
        no ->
            recv_opt_makes_ref(Is, RecvLbl, Blocks0, [I0|Acc]);
        {yes,Ref} ->
            %% This call creates a reference.
            {yes,Ref,[I0|Is],Acc}
    end;
recv_opt_makes_ref([I|Is], RecvLbl, Blocks, Acc) ->
    recv_opt_makes_ref(Is, RecvLbl, Blocks, [I|Acc]);
recv_opt_makes_ref([], _, _, _) -> no.

makes_ref(#b_set{dst=Dst,args=[Func0|_]}, Blocks) ->
    Func = case Func0 of
               #b_remote{mod=#b_literal{val=erlang},
                         name=#b_literal{val=Name},arity=A0} ->
                   {Name,A0};
               _ ->
                   none
           end,
    case Func of
        {make_ref,0} ->
            {yes,Dst};
        {monitor,2} ->
            {yes,Dst};
        {spawn_monitor,A} when A =:= 1; A =:= 3 ->
            ref_in_tuple(Dst, Blocks);
        _ ->
            no
    end.

ref_in_tuple(Tuple, Blocks) ->
    F = fun(#b_set{op=get_tuple_element,dst=Ref,
                   args=[#b_var{}=Tup,#b_literal{val=1}]}, no)
              when Tup =:= Tuple -> {yes,Ref};
           (_, A) -> A
        end,
    beam_ssa:fold_instrs_rpo(F, [0], no, Blocks).

opt_ref_used(L, Ref, Blocks) ->
    Vs = #{Ref=>ref,ref=>Ref,ref_matched=>false},
    case opt_ref_used_1(L, Vs, Blocks) of
        used -> true;
        not_used -> false;
        done -> false
    end.

opt_ref_used_1(L, Vs0, Blocks) ->
    #b_blk{is=Is} = Blk = map_get(L, Blocks),
    case opt_ref_used_is(Is, Vs0) of
        #{}=Vs ->
            opt_ref_used_last(Blk, Vs, Blocks);
        Result ->
            Result
    end.

opt_ref_used_is([#b_set{op=peek_message,dst=Msg}|Is], Vs0) ->
    Vs = Vs0#{Msg=>message},
    opt_ref_used_is(Is, Vs);
opt_ref_used_is([#b_set{op={bif,Bif},args=Args,dst=Dst}=I|Is],
                Vs0) ->
    S = case Bif of
            '=:=' -> true;
            '==' -> true;
            _ -> none
        end,
    case S of
        none ->
            Vs = update_vars(I, Vs0),
            opt_ref_used_is(Is, Vs);
        Bool when is_boolean(Bool) ->
            case is_ref_msg_comparison(Args, Vs0) of
                true ->
                    Vs = Vs0#{Dst=>{is_ref,Bool}},
                    opt_ref_used_is(Is, Vs);
                false ->
                    opt_ref_used_is(Is, Vs0)
            end
    end;
opt_ref_used_is([#b_set{op=remove_message}|_], Vs) ->
    case Vs of
        #{ref_matched:=true} ->
            used;
        #{ref_matched:=false} ->
            not_used
    end;
opt_ref_used_is([#b_set{op=recv_next}|_], _Vs) ->
    done;
opt_ref_used_is([#b_set{op=wait_timeout}|_], _Vs) ->
    done;
opt_ref_used_is([#b_set{op=wait}|_], _Vs) ->
    done;
opt_ref_used_is([#b_set{op=landingpad}|_], _Vs) ->
    %% The receive marker will be cleared when an exception
    %% is caught.
    done;
opt_ref_used_is([#b_set{op=call,
                        args=[#b_remote{mod=#b_literal{val=Mod},
                                        name=#b_literal{val=Name}}|Args]}=I|Is],
                Vs0) ->
    case erl_bifs:is_exit_bif(Mod, Name, length(Args)) of
        true ->
            done;
        false ->
            Vs = update_vars(I, Vs0),
            opt_ref_used_is(Is, Vs)
    end;
opt_ref_used_is([#b_set{}=I|Is], Vs0) ->
    Vs = update_vars(I, Vs0),
    opt_ref_used_is(Is, Vs);
opt_ref_used_is([], Vs) -> Vs.

opt_ref_used_last(#b_blk{last=Last}=Blk, Vs, Blocks) ->
    case Last of
        #b_br{bool=#b_var{}=Bool,succ=Succ,fail=Fail} ->
            case Vs of
                #{Bool:={is_ref,Matched}} ->
                    ref_used_in([{Succ,Vs#{ref_matched:=Matched}},
                                 {Fail,Vs#{ref_matched:=not Matched}}],
                                Blocks);
                #{} ->
                    ref_used_in([{Succ,Vs},{Fail,Vs}], Blocks)
            end;
        #b_ret{} ->
            not_used;
        _ ->
            SuccVs = [{Succ,Vs} || Succ <- beam_ssa:successors(Blk)],
            ref_used_in(SuccVs, Blocks)
    end.

ref_used_in([{L,Vs0}|Ls], Blocks) ->
    case opt_ref_used_1(L, Vs0, Blocks) of
        not_used ->
            not_used;
        used ->
            case ref_used_in(Ls, Blocks) of
                done -> used;
                Result -> Result
            end;
        done -> ref_used_in(Ls, Blocks)
    end;
ref_used_in([], _) -> done.

update_vars(#b_set{args=Args,dst=Dst}, Vs) ->
    Vars = [V || #b_var{}=V <- Args],
    All = all(fun(Var) ->
                      case Vs of
                          #{Var:=message} -> true;
                          #{} -> false
                      end
              end, Vars),
    case {Vars,All} of
        {[_|_],true} -> Vs#{Dst=>message};
        {_,_} -> Vs
    end.

%% is_ref_msg_comparison(Args, Variables) -> true|false.
%%  Return 'true' if Args denotes a comparison between the
%%  reference and message or part of the message.

is_ref_msg_comparison([#b_var{}=V1,#b_var{}=V2], Vs) ->
    case Vs of
        #{V1:=ref,V2:=message} -> true;
        #{V1:=message,V2:=ref} -> true;
        #{} -> false
    end;
is_ref_msg_comparison(_, _) -> false.
