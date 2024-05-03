%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2024. All Rights Reserved.
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
%% Purpose : Clean up, such as removing unused labels and unused functions.

-module(beam_clean).
-moduledoc false.

-export([module/2]).

-import(lists, [reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,_}, Opts) ->
    Fs1 = move_out_funs(Fs0),
    Order = [Lbl || {function,_,_,Lbl,_} <- Fs1],
    All = #{Lbl => Func || {function,_,_,Lbl,_}=Func <- Fs1},
    WorkList = rootset(Fs1, Exp, Attr),
    Used = find_all_used(WorkList, All, sets:from_list(WorkList, [{version, 2}])),
    Fs2 = remove_unused(Order, Used, All),
    {Fs3,Lc} = clean_labels(Fs2),
    Fs4 = fix_bs_create_bin(Fs3, Opts),
    Fs5 = fix_badrecord(Fs4, Opts),
    Fs = maybe_remove_lines(Fs5, Opts),
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

%% Determine the rootset, i.e. exported functions and
%% the on_load function (if any).

rootset(Fs, Root0, Attr) ->
    Root1 = case proplists:get_value(on_load, Attr) of
		undefined -> Root0;
		[OnLoad] -> [OnLoad|Root0]
	   end,
    Root = sofs:set(Root1, [function]),
    Map0 = [{{Name,Arity},Lbl} || {function,Name,Arity,Lbl,_} <- Fs],
    Map = sofs:relation(Map0, [{function,label}]),
    sofs:to_external(sofs:image(Map, Root)).

%% Remove the unused functions.

remove_unused(Fs, Used, All) ->
    [map_get(F, All) || F <- Fs, sets:is_element(F, Used)].

%% Find all used functions.

find_all_used([F|Fs0], All, Used0) ->
    {function,_,_,_,Code} = map_get(F, All),
    {Fs,Used} = update_work_list(Code, {Fs0,Used0}),
    find_all_used(Fs, All, Used);
find_all_used([], _All, Used) -> Used.

update_work_list([{call,_,{f,L}}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([{make_fun3,{f,L},_,_,_,_}|Is], Sets) ->
    update_work_list(Is, add_to_work_list(L, Sets));
update_work_list([_|Is], Sets) ->
    update_work_list(Is, Sets);
update_work_list([], Sets) -> Sets.

add_to_work_list(F, {Fs,Used}=Sets) ->
    case sets:is_element(F, Used) of
	true -> Sets;
	false -> {[F|Fs],sets:add_element(F, Used)}
    end.

%% Move out make_fun3 instructions from blocks. That is necessary because
%% they contain labels that must be seen and renumbered.

move_out_funs([{function,Name,Arity,Entry,Is0}|Fs]) ->
    Is = move_out_funs_is(Is0),
    [{function,Name,Arity,Entry,Is}|move_out_funs(Fs)];
move_out_funs([]) -> [].

move_out_funs_is([{block,Bl}|Is]) ->
    move_out_funs_block(Bl, Is, []);
move_out_funs_is([I|Is]) ->
    [I|move_out_funs_is(Is)];
move_out_funs_is([]) -> [].

move_out_funs_block([{set,[D],Ss,{make_fun3,F,I,U}}|Bl], Is, Acc) ->
    make_block(Acc) ++
        [{make_fun3,F,I,U,D,{list,Ss}} |
         move_out_funs_block(Bl, Is, [])];
move_out_funs_block([B|Bl], Is, Acc) ->
    move_out_funs_block(Bl, Is, [B|Acc]);
move_out_funs_block([], Is, Acc) ->
    make_block(Acc) ++ move_out_funs_is(Is).

make_block([_|_]=Is) -> [{block,reverse(Is)}];
make_block([]) -> [].

%%%
%%% Coalesce adjacent labels. Renumber all labels to eliminate gaps.
%%% This cleanup will slightly reduce file size and slightly speed up
%%% loading.
%%%

-type label() :: beam_asm:label().

-record(st, {lmap :: [{label(),label()}], %Translation tables for labels.
	     entry :: beam_asm:label(),   %Number of entry label.
	     lc :: non_neg_integer()      %Label counter
	     }).

clean_labels(Fs0) ->
    St0 = #st{lmap=[],entry=1,lc=1},
    {Fs1,#st{lmap=Lmap0,lc=Lc}} = function_renumber(Fs0, St0, []),
    Lmap = maps:from_list(Lmap0),
    Fs = function_replace(Fs1, Lmap, []),
    {Fs,Lc}.

function_renumber([{function,Name,Arity,_Entry,Asm0}|Fs], St0, Acc) ->
    {Asm,St} = renumber_labels(Asm0, [], St0),
    function_renumber(Fs, St, [{function,Name,Arity,St#st.entry,Asm}|Acc]);
function_renumber([], St, Acc) -> {Acc,St}.

renumber_labels([{label,Old}|Is], [{label,New}|_]=Acc, #st{lmap=D0}=St) ->
    D = [{Old,New}|D0],
    renumber_labels(Is, Acc, St#st{lmap=D});
renumber_labels([{label,Old}|Is], Acc, St0) ->
    New = St0#st.lc,
    D = [{Old,New}|St0#st.lmap],
    renumber_labels(Is, [{label,New}|Acc], St0#st{lmap=D,lc=New+1});
renumber_labels([{func_info,_,_,_}=Fi|Is], Acc, St0) ->
    renumber_labels(Is, [Fi|Acc], St0#st{entry=St0#st.lc});
renumber_labels([I|Is], Acc, St0) ->
    renumber_labels(Is, [I|Acc], St0);
renumber_labels([], Acc, St) -> {Acc,St}.

function_replace([{function,Name,Arity,Entry,Asm0}|Fs], Dict, Acc) ->
    Asm = try
              Fb = fun(Old) -> throw({error,{undefined_label,Old}}) end,
              beam_utils:replace_labels(Asm0, [], Dict, Fb)
	  catch
	      throw:{error,{undefined_label,Lbl}=Reason} ->
		  io:format("Function ~s/~w refers to undefined label ~w\n",
			    [Name,Arity,Lbl]),
		  exit(Reason)
	  end,
    function_replace(Fs, Dict, [{function,Name,Arity,Entry,Asm}|Acc]);
function_replace([], _, Acc) -> Acc.

%%%
%%% Remove line instructions if requested.
%%%

maybe_remove_lines(Fs, Opts) ->
    case proplists:get_bool(no_line_info, Opts) of
	false -> Fs;
	true -> fold_functions(fun remove_lines/1, Fs)
    end.

remove_lines([{line,_}|Is]) ->
    remove_lines(Is);
remove_lines([{block,Bl0}|Is]) ->
    Bl = remove_lines_block(Bl0),
    [{block,Bl}|remove_lines(Is)];
remove_lines([I|Is]) ->
    [I|remove_lines(Is)];
remove_lines([]) -> [].

remove_lines_block([{set,_,_,{line,_}}|Is]) ->
    remove_lines_block(Is);
remove_lines_block([I|Is]) ->
    [I|remove_lines_block(Is)];
remove_lines_block([]) -> [].

%%%
%%% If compatibility with a previous release (OTP 24 or earlier) has
%%% been requested, eliminate bs_create_bin instructions by translating
%%% them to the old binary syntax instructions.
%%%

fix_bs_create_bin(Fs, Opts) ->
    case proplists:get_bool(no_bs_create_bin, Opts) of
        false -> Fs;
        true -> fold_functions(fun fix_bs_create_bin/1, Fs)
    end.

fix_bs_create_bin([{bs_create_bin,Fail,Alloc,Live,Unit,Dst,{list,List}}|Is]) ->
    Tail = fix_bs_create_bin(Is),
    Flags = {field_flags,[]},
    try bs_pre_size_calc(List) of
        SizeCalc0 ->
            SizeCalc = fold_size_calc(SizeCalc0, 0, []),
            TmpDst = SizeReg = {x,Live},
            SizeIs0 = bs_size_calc(SizeCalc, Fail, SizeReg, {x,Live+1}),
            SizeIs = [{move,{integer,0},SizeReg}|SizeIs0],
            RestIs = bs_puts(List, Fail) ++ [{move,TmpDst,Dst}|Tail],
            case List of
                [{atom,append},_,_,_,Src|_] ->
                    SizeIs ++ [{bs_append,Fail,SizeReg,Alloc,Live+1,Unit,Src,Flags,TmpDst}|RestIs];
                [{atom,private_append},_,_,_,Src|_] ->
                    TestHeap = {test_heap,Alloc,Live+1},
                    SizeIs ++ [TestHeap,{bs_private_append,Fail,SizeReg,Unit,Src,Flags,TmpDst}|RestIs];
                _ ->
                    SizeIs ++ [{bs_init_bits,Fail,SizeReg,Alloc,Live+1,Flags,TmpDst}|RestIs]
            end
    catch
        throw:invalid_size ->
            [{move,{atom,badarg},{x,0}},
             {call_ext_only,1,{extfunc,erlang,error,1}}|Tail]
    end;
fix_bs_create_bin([I|Is]) ->
    [I|fix_bs_create_bin(Is)];
fix_bs_create_bin([]) -> [].

bs_pre_size_calc([Type,_Seg,Unit,_Flags,Src,Size|Segs]) ->
    case Type of
        {atom,T} when T =:= append; T =:= private_append ->
            bs_pre_size_calc(Segs);
        _ ->
            [bs_pre_size_calc_1(Type, Unit, Src, Size)|bs_pre_size_calc(Segs)]
    end;
bs_pre_size_calc([]) -> [].

bs_pre_size_calc_1({atom,Type}, Unit, Src, Size) ->
    case {Unit,Size} of
        {0,{atom,undefined}} ->
            %% No size/unit given.
            {8,case Type of
                   utf8 -> {{instr,bs_utf8_size},Src};
                   utf16 -> {{instr,bs_utf16_size},Src};
                   utf32 -> {term,{integer,4}}
               end};
        {Unit,_} ->
            case {Type,Size} of
                {binary,{atom,all}} ->
                    case Unit rem 8 of
                        0 -> {8,{{bif,byte_size},Src}};
                        _ -> {1,{{bif,bit_size},Src}}
                    end;
                {_,_} ->
                    ensure_valid_size(Size),
                    {Unit,{term,Size}}
            end
    end.

ensure_valid_size({x,_}) -> ok;
ensure_valid_size({y,_}) -> ok;
ensure_valid_size({integer,Size}) when Size >= 0 -> ok;
ensure_valid_size(_) -> throw(invalid_size).

fold_size_calc([{Unit,{term,{integer,Size}}}|T], Bits, Acc) ->
    fold_size_calc(T, Bits + Unit*Size, Acc);
fold_size_calc([{Unit,{{bif,Bif},{literal,Lit}}}=H|T], Bits, Acc) ->
    try erlang:Bif(Lit) of
        Result ->
            fold_size_calc([{Unit,{term,{integer,Result}}}|T], Bits, Acc)
    catch
        _:_ ->
            fold_size_calc(T, Bits, [H|Acc])
    end;
fold_size_calc([{U,_}=H|T], Bits, Acc) when U =:= 1; U =:= 8 ->
    fold_size_calc(T, Bits, [H|Acc]);
fold_size_calc([{U,Var}|T], Bits, Acc) ->
    fold_size_calc(T, Bits, [{1,{'*',{term,{integer,U}},Var}}|Acc]);
fold_size_calc([], Bits, Acc) ->
    Bytes = Bits div 8,
    RemBits = Bits rem 8,
    Sizes = [{1,{term,{integer,RemBits}}},{8,{term,{integer,Bytes}}}|Acc],
    [Pair || {_,Sz}=Pair <- Sizes, Sz =/= {term,{integer,0}}].

bs_size_calc([{Unit,{{bif,Bif},Reg}}|T], Fail, SizeReg, TmpReg) ->
    Live = element(2, SizeReg) + 1,
    [{gc_bif,Bif,Fail,Live,[Reg],TmpReg},
     {bs_add,Fail,[SizeReg,TmpReg,Unit],SizeReg}|bs_size_calc(T, Fail, SizeReg, TmpReg)];
bs_size_calc([{Unit,{'*',{term,Term1},{term,Term2}}}|T], Fail, SizeReg, TmpReg) ->
    Live = element(2, SizeReg) + 1,
    [{gc_bif,'*',Fail,Live,[Term1,Term2],TmpReg},
     {bs_add,Fail,[SizeReg,TmpReg,Unit],SizeReg}|bs_size_calc(T, Fail, SizeReg, TmpReg)];
bs_size_calc([{Unit,{{instr,Instr},Reg}}|T], Fail, SizeReg, TmpReg) ->
    [{Instr,Fail,Reg,TmpReg},
     {bs_add,Fail,[SizeReg,TmpReg,Unit],SizeReg}|bs_size_calc(T, Fail, SizeReg, TmpReg)];
bs_size_calc([{Unit,{term,Term}}|T], Fail, SizeReg, TmpReg) ->
    [{bs_add,Fail,[SizeReg,Term,Unit],SizeReg}|bs_size_calc(T, Fail, SizeReg, TmpReg)];
bs_size_calc([], _Fail, _SizeReg, _TmpReg) -> [].

bs_puts([{atom,string},_Seg,_Unit,_Flags,{string,_}=Str,{integer,Size}|Is], Fail) ->
    [{bs_put_string,Size,Str}|bs_puts(Is, Fail)];
bs_puts([{atom,append},_,_,_,_,_|Is], Fail) ->
    bs_puts(Is, Fail);
bs_puts([{atom,private_append},_,_,_,_,_|Is], Fail) ->
    bs_puts(Is, Fail);
bs_puts([{atom,Type},_Seg,Unit,Flags0,Src,Size|Is], Fail) ->
    Op = case Type of
             integer -> bs_put_integer;
             float   -> bs_put_float;
             binary  -> bs_put_binary;
             utf8    -> bs_put_utf8;
             utf16   -> bs_put_utf16;
             utf32   -> bs_put_utf32
         end,
    Flags = case Flags0 of
                nil -> [];
                {literal,Fs} -> Fs
            end,
    I = if
            Unit =:= 0 ->
                {bs_put,Fail,{Op,{field_flags,Flags}},[Src]};
            true ->
                {bs_put,Fail,{Op,Unit,{field_flags,Flags}},[Size,Src]}
        end,
    [I|bs_puts(Is, Fail)];
bs_puts([], _Fail) -> [].

%%%
%%% If compatibility with a previous release (OTP 24 or earlier) has
%%% been requested, eliminate badrecord instructions by translating
%%% them to calls to error({badrecord,Value}).
%%%

fix_badrecord(Fs, Opts) ->
    case proplists:get_bool(no_badrecord, Opts) of
        false -> Fs;
        true -> fold_functions(fun fix_badrecord/1, Fs)
    end.

fix_badrecord([{badrecord,Value}|Is]) ->
    [{move,Value,{x,0}},
     {test_heap,3,1},
     {put_tuple2,{x,0},{list,[{atom,badrecord},{x,0}]}},
     {call_ext_only,1,{extfunc,erlang,error,1}}|fix_badrecord(Is)];
fix_badrecord([I|Is]) ->
    [I|fix_badrecord(Is)];
fix_badrecord([]) -> [].


%%%
%%% Helpers.
%%%

fold_functions(F, [{function,N,A,Lbl,Is0}|T]) ->
    Is = F(Is0),
    [{function,N,A,Lbl,Is}|fold_functions(F, T)];
fold_functions(_F, []) -> [].
