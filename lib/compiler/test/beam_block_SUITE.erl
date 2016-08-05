%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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
	 erl_202/1]).

%% The only test for the following functions is that
%% the code compiles and is accepted by beam_validator.
-export([encode_wildcards3/4,find_operands/4]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [get_map_elements,
       otp_7345,
       move_opt_across_gc_bif,
       erl_202
      ]}].

init_per_suite(Config) ->
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

%%%
%%% Common functions.
%%%

id(I) -> I.
