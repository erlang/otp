%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2018. All Rights Reserved.
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
-module(beam_block_SUITE).

-export([all/0,suite/0,groups/0,init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 get_map_elements/1,otp_7345/1,move_opt_across_gc_bif/1,
	 erl_202/1,repro/1,local_cse/1,second_block_pass/1]).

%% The only test for the following functions is that
%% the code compiles and is accepted by beam_validator.
-export([encode_wildcards3/4,find_operands/4]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,[parallel],
      [get_map_elements,
       otp_7345,
       move_opt_across_gc_bif,
       erl_202,
       repro,
       local_cse,
       second_block_pass
      ]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

get_map_elements(_Config) ->
    [{pred,var}] = get_map_elements([{pred,var}], #{}, []),
    [{pred,var}] = get_map_elements([{pred,var}], #{pred=>[]}, []),
    acc = get_map_elements([], #{pred=>[]}, acc),
    ok.

get_map_elements([{Pred,Var}|Left], Map, Acc) ->
    case Map of
        #{Var := List} ->
            case lists:keyfind(Pred, 1, List) of
                false ->
                    get_map_elements(Left, Map, [{Pred,Var}|Acc])
            end;
        #{} ->
            get_map_elements(Left, Map, [{Pred,Var}|Acc])
    end;
get_map_elements([], _Map, Acc) ->
    Acc.

%% The following code
%%
%%    {get_list,{x,2},{x,0},{x,1}}.
%%    {gc_bif,length,{f,0},1,[{x,0}],{x,0}}.
%%    {move,{x,0},{x,1}}.
%%
%% was incorrectly optimized to
%%
%%    {get_list,{x,2},{x,0},{y,0}}.
%%    {gc_bif,length,{f,0},3,[{x,0}],{x,1}}.
%%
%% because beam_block:is_transparent({x,1},
%%                                  {gc_bif,length,{f,0},3,[{x,0}],{x,1}}
%% incorrectly returned true.

-record(contextId,{cid,device_type,contextRef}).
-record(dpRef,{cid,tlli,ms_device_context_id}).
-record(qosProfileBssgp,{peak_bit_rate_msb,
                              peak_bit_rate_lsb,
                              t_a_precedence}).
-record(llUnitdataReq,{sapi,
                            l3_pdu_length,
                            pdu_life}).
-record(ptmsi,{value}).

otp_7345(_Config) ->
    #llUnitdataReq{l3_pdu_length=3,pdu_life=4} =
	otp_7345(#contextId{}, 0, [[1,2,3],4,5]).


otp_7345(ObjRef, _RdEnv, Args) ->
    Cid = ObjRef#contextId.cid,
    _ =	#dpRef{cid = Cid,
		     ms_device_context_id = cid_id,
		     tlli = #ptmsi{value = 0}},
    _ =	#qosProfileBssgp{peak_bit_rate_msb = 0,
			 peak_bit_rate_lsb = 80,
			 t_a_precedence = 49},
    [Cpdu|_] = Args,
    LlUnitdataReq =
	#llUnitdataReq{sapi = 7,
		       l3_pdu_length = length(Cpdu),
		       pdu_life =
		       id(42)
		       div
		       10},
    id(LlUnitdataReq).


%% Doing move optimizations across GC bifs are in general not safe.
move_opt_across_gc_bif(_Config) ->
    [0,true,1] = positive(speaking),
    ok.

positive(speaking) ->
    try
	Positive = 0,
	[+Positive, case Positive of _ -> true end, paris([], Positive)]
    after
	mailing
    end.

paris([], P) -> P + 1.


%% See https://bugs.erlang.org/browse/ERL-202.
%% Test that move_allocates/1 in beam_block doesn't move allocate
%% when it would not be safe.

-record(erl_202_r1, {y}).
-record(erl_202_r2, {x}).

erl_202(_Config) ->
    Ref = make_ref(),
    Ref = erl_202({{1,2},Ref}, 42),

    {Ref} = erl_202({7,8}, #erl_202_r1{y=#erl_202_r2{x=Ref}}),

    ok.

erl_202({{_, _},X}, _) ->
    X;
erl_202({_, _}, #erl_202_r1{y=R2}) ->
    {R2#erl_202_r2.x}.

%% See https://bugs.erlang.org/browse/ERL-266.
%% Instructions with failure labels are not safe to include
%% in a block. Including get_map_elements in a block would
%% lead to unsafe code.

repro(_Config) ->
    [] = maps:to_list(repro([], #{}, #{})),
    [{tmp1,n}] = maps:to_list(repro([{tmp1,0}], #{}, #{})),
    [{tmp1,name}] = maps:to_list(repro([{tmp1,0}], #{}, #{0=>name})),
    ok.

repro([], TempNames, _Slots) ->
    TempNames;
repro([{Temp, Slot}|Xs], TempNames, Slots0) ->
    {Name, Slots} =
	case Slots0 of
	    #{Slot := Name0} -> {Name0, Slots0};
	    #{} ->              {n,     Slots0#{Slot => n}}
	end,
    repro(Xs, TempNames#{Temp => Name}, Slots).

%%%
%%% The only test of the following code is that it compiles.
%%%

%% Slightly simplifed from megaco_binary_term_id_gen.
%%  beam_block failed to note that the {gc_bif,'-'...} instruction could
%%  fail, and that therefore {y,0} need to be initialized.
%%    {allocate,8,6}.
%%                     %% {init,{y,0}} needed here.
%%    {get_list,{x,1},{x,6},{x,7}}.
%%    {'catch',{y,7},{f,3}}.
%%    {move,{x,4},{y,1}}.
%%    {move,{x,3},{y,2}}.
%%    {move,{x,2},{y,3}}.
%%    {move,{x,5},{y,4}}.
%%    {move,{x,7},{y,5}}.
%%    {move,{x,6},{y,6}}.
%%    {gc_bif,'-',{f,0},8,[{x,3},{x,6}],{x,0}}.
%%    {move,{x,0},{y,0}}.

encode_wildcards3([],[],_,_) -> [];
encode_wildcards3([Level|Levels],[BitsInLevel|BitsRest],LevelNo,TotSize) ->
    case (catch ?MODULE:encode_wildcard(Level,BitsInLevel,TotSize-BitsInLevel,
					length(Levels))) of
	{'EXIT',{Reason,Info}} ->
	    exit({Reason,{LevelNo,Info}});

	no_wildcard ->
	    encode_wildcards3(Levels,BitsRest,LevelNo+1,TotSize-BitsInLevel);

	{level,Wl} ->
	    [Wl|
	     encode_wildcards3(Levels,BitsRest,LevelNo+1,TotSize-BitsInLevel)];

	{recursive,Wr} ->
	    [Wr]
    end.

%% Slightly simplified code from hipe_rtl_ssapre.
%%  beam_block used to do the following incorrect optimization:
%%
%%    {gc_bif,length,{f,0},1,[{x,0}],{x,3}}.
%%                                   ^^^^^ Was {x,0} - changing to {x,3} is not safe.
%%    {gc_bif,'+',{f,0},0,[{y,2},{integer,1}],{x,0}}.
%%                     ^^^ Only one register live
%%     . . .
%%    {call_last,4,{f,2},4}.   %% beam_validator noted that {x,3} wasn't live.

find_operands(Cfg,XsiGraph,[],_Count) ->
    {Cfg,XsiGraph};
find_operands(Cfg,XsiGraph,ActiveList,Count) ->
    {NewCfg,TempActiveList}=?MODULE:find_operands_for_active_list(Cfg,XsiGraph,
								  ActiveList,[]),
    NewActiveList=lists:reverse(TempActiveList),
    [Count+1, length(NewActiveList), length(digraph:vertices(XsiGraph))],
    find_operands(NewCfg,XsiGraph,NewActiveList,Count+1).

%% Some tests of local common subexpression elimination (CSE).

local_cse(_Config) ->
    {Self,{ok,Self}} = local_cse_1(),

    local_cse_2([]),
    local_cse_2(lists:seq(1, 512)),
    local_cse_2(?MODULE:module_info()),

    {[b],[a,b]} = local_cse_3(a, b),

    {2000,Self,{Self,write_cache}} = local_cse_4(),

    ok.

local_cse_1() ->
    %% Cover handling of unsafe tuple construction in
    %% eliminate_use_of_from_reg/4. It became necessary to handle
    %% unsafe tuples when local CSE was introduced.

    {self(),{ok,self()}}.

local_cse_2(Term) ->
    case cse_make_binary(Term) of
        <<Size:8,BinTerm:Size/binary>> ->
            Term = binary_to_term(BinTerm);
        <<Size:8,SizeTerm:Size/binary,BinTerm/binary>> ->
            {'$size',TermSize} = binary_to_term(SizeTerm),
            TermSize = byte_size(BinTerm),
            Term = binary_to_term(BinTerm)
    end.

%% Copy of observer_backend:ttb_make_binary/1. During development of
%% the local CSE optimization this function was incorrectly optimized.

cse_make_binary(Term) ->
    B = term_to_binary(Term),
    SizeB = byte_size(B),
    if SizeB > 255 ->
            SB = term_to_binary({'$size',SizeB}),
            <<(byte_size(SB)):8, SB/binary, B/binary>>;
       true ->
            <<SizeB:8, B/binary>>
    end.

local_cse_3(X, Y) ->
    %% The following expression was incorrectly transformed to {[X,Y],[X,Y]}
    %% during development of the local CSE optimization.

    {[Y],[X,Y]}.

local_cse_4() ->
    do_local_cse_4(2000, self(), {self(), write_cache}).

do_local_cse_4(X, Y, Z) ->
    {X,Y,Z}.

%% Tests previously found bugs when running beam_block the second time.

second_block_pass(_Config) ->
    [#{dts:=5.0}] = second_1([#{dts => 10.0}], 2.0),
    ok.

second_1(Fs, TS) ->
    [F#{dts=>DTS / TS} || #{dts:=DTS} = F <- Fs].

%%%
%%% Common functions.
%%%

id(I) -> I.
