%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
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
%% The Initial Developer of the Original Code is Ericsson AB.
%% 
%% %CopyrightEnd%
%%

%%%-------------------------------------------------------------------
%%% File    : erts_alloc_config.erl
%%% Author  : Rickard Green
%%% Description : Generate an erts_alloc configuration suitable for
%%%               a limited amount of runtime scenarios.
%%%
%%% Created :  9 May 2007 by Rickard Green
%%%-------------------------------------------------------------------

-module(erts_alloc_config).

-record(state, {have_scenario = false,
		alloc}).


-record(alloc, {name,
		enabled,
		need_config_change,
		alloc_util,
		instances,
		strategy,
		acul,
		low_mbc_blocks_size,
		high_mbc_blocks_size,
		sbct,
		segments}).

-record(conf,
	{segments,
	 format_to}).

-record(segment, {size,number}).

-define(PRINT_WITDH, 76).

-define(SERVER, '__erts_alloc_config__').

-define(KB, 1024).
-define(MB, 1048576).

-define(B2KB(B), ((((B) - 1) div ?KB) + 1)).
-define(ROUNDUP(V, R), ((((V) - 1) div (R)) + 1)*(R)).

-define(LARGE_GROWTH_ABS_LIMIT, 20*?MB).
-define(MBC_MSEG_LIMIT, 150).
-define(FRAG_FACT, 1.25).
-define(GROWTH_SEG_FACT, 2).
-define(MIN_SEG_SIZE, 1*?MB).
-define(SMALL_GROWTH_SEGS, 5).

-define(ALLOC_UTIL_ALLOCATOR(A),
	A == binary_alloc;
	A == std_alloc;
	A == ets_alloc;
	A == fix_alloc;
	A == eheap_alloc;
	A == ll_alloc;
	A == sl_alloc;
	A == temp_alloc;
	A == driver_alloc).

-define(ALLOCATORS,
	[binary_alloc,
	 ets_alloc,
	 eheap_alloc,
	 fix_alloc,
	 ll_alloc,
	 mseg_alloc,
	 sl_alloc,
	 std_alloc,
	 sys_alloc,
	 temp_alloc,
	 driver_alloc]).

-define(MMBCS_DEFAULTS,
	[{binary_alloc, 131072},
	 {std_alloc, 131072},
	 {ets_alloc, 131072},
	 {fix_alloc, 131072},
	 {eheap_alloc, 524288},
	 {ll_alloc, 131072},
	 {sl_alloc, 131072},
	 {temp_alloc, 131072},
	 {driver_alloc, 131072}]).

%%%
%%% Exported interface
%%%

-export([save_scenario/0,
	 make_config/0,
	 make_config/1,
	 stop/0]).

%% Test and debug export
-export([state/0]).


save_scenario() ->
    req(save_scenario).

make_config() ->
    make_config(group_leader()).

make_config(FileName) when is_list(FileName) ->
    case file:open(FileName, [write]) of
	{ok, IODev} ->
	    Res = req({make_config, IODev}),
	    ok = file:close(IODev),
	    Res;
	Error ->
	    Error
    end;
make_config(IODev) ->
    req({make_config, IODev}).

stop() ->
    req(stop).


%% state() is intentionally undocumented, and is for testing
%% and debugging only...

state() ->
    req(state).

%%%
%%% Server
%%%

req(Req) ->
    Ref = make_ref(),
    ReqMsg = {request, self(), Ref, Req},
    req(ReqMsg, Ref, true).

req(ReqMsg, Ref, TryStart) ->
    req(ReqMsg, Ref, TryStart, erlang:monitor(process, ?SERVER)).

req(ReqMsg, Ref, TryStart, Mon) ->
    (catch ?SERVER ! ReqMsg),
    receive
	{response, Ref, Res} ->
	    erlang:demonitor(Mon, [flush]),
	    Res;
	{'DOWN', Mon, _, _, noproc} ->
	    case TryStart of
		true -> start_server(Ref, ReqMsg);
		false -> {error, server_died}
	    end;
	{'DOWN', Mon, _, _, Reason} ->
	    {error, Reason}
    end.

start_server(Ref, ReqMsg) ->
    Starter = self(),
    Pid = spawn(fun () ->
			register(?SERVER, self()),
			Starter ! {Ref, self(), started},
			server_loop(make_state())
		end),
    Mon = erlang:monitor(process, Pid),
    receive
	{Ref, Pid, started} ->
	    req(ReqMsg, Ref, false, Mon);
	{'DOWN', Mon, _, _, _} ->
	    req(ReqMsg, Ref, false)
    end.

server_loop(State) ->
    NewState = receive
		   {request, From, Ref, save_scenario} ->
		       Alloc = save_scenario(State#state.alloc),
		       From ! {response, Ref, ok},
		       State#state{alloc = Alloc, have_scenario = true};
		   {request, From, Ref, {make_config, IODev}} ->
		       case State#state.have_scenario of
			   true ->
			       Conf = #conf{segments = ?MBC_MSEG_LIMIT,
					    format_to = IODev},
			       Res = mk_config(Conf, State#state.alloc),
			       From ! {response, Ref, Res},
                               ok;
			   _ ->
			       From ! {response, Ref, no_scenario_saved},
                               ok
		       end,
		       State;
		   {request, From, Ref, stop} ->
		       From ! {response, Ref, ok},
		       exit(normal);
		   {request, From, Ref, state} ->
		       From ! {response, Ref, State},
		       State;
		   {request, From, Ref, Req} ->
		       From ! {response, Ref, {unknown_request, Req}},
		       State;
		   _ ->
		       State
	       end,
    server_loop(NewState).

carrier_migration_support(aoff) ->
    true;
carrier_migration_support(aoffcbf) ->
    true;
carrier_migration_support(aoffcaobf) ->
    true;
carrier_migration_support(_) ->
    false.

allocator_instances(ll_alloc, Strategy) ->
    case carrier_migration_support(Strategy) of
	true -> erlang:system_info(schedulers);
	false -> 1
    end;
allocator_instances(_A, undefined) ->
    1;
allocator_instances(_A, _Strategy) ->
    erlang:system_info(schedulers).

strategy(temp_alloc, _AI) ->
    af;
strategy(A, AI) ->
    try
	{A, OptList} = lists:keyfind(A, 1, AI),
	{as, S} = lists:keyfind(as, 1, OptList),
	S
    catch
	_ : _ ->
	    undefined
    end.

strategy_str(af) ->
    "A fit";
strategy_str(gf) ->
    "Good fit";
strategy_str(bf) ->
    "Best fit";
strategy_str(aobf) ->
    "Address order best fit";
strategy_str(aoff) ->
    "Address order first fit";
strategy_str(aoffcbf) ->
    "Address order first fit carrier best fit";
strategy_str(aoffcaobf) ->
    "Address order first fit carrier adress order best fit";
strategy_str(ageffcaoff) ->
    "Age order first fit carrier address order first fit";
strategy_str(ageffcbf) ->
    "Age order first fit carrier best fit";
strategy_str(ageffcaobf) ->
    "Age order first fit carrier adress order best fit".

default_acul(A, S) ->
    case carrier_migration_support(S) of
	false ->
	    0;
	true ->
	    case A of
		ll_alloc -> 85;
		eheap_alloc -> 45;
		_ -> 60
	    end
    end.

make_state() ->
    {_, _, _, AI} = erlang:system_info(allocator),
    #state{alloc = lists:map(fun (A) ->
				     S = strategy(A, AI),
				     #alloc{name = A,
					    strategy = S,
					    acul = default_acul(A, S),
					    instances = allocator_instances(A, S)}
			     end,
			     ?ALLOCATORS)}.

%%
%% Save scenario
%%

ai_value(Key1, Key2, AI) ->
    case lists:keysearch(Key1, 1, AI) of
	{value, {Key1, Value1}} ->
	    case lists:keysearch(Key2, 1, Value1) of
		{value, Result} -> Result;
		_ -> undefined
	    end;
	_ -> undefined
    end.


chk_mbcs_blocks_size(#alloc{low_mbc_blocks_size = undefined,
			    high_mbc_blocks_size = undefined} = Alc,
		     Min,
		     Max) ->
    Alc#alloc{low_mbc_blocks_size = Min,
	      high_mbc_blocks_size = Max,
	      enabled = true};
chk_mbcs_blocks_size(#alloc{low_mbc_blocks_size = LowBS,
			    high_mbc_blocks_size = HighBS} = Alc,
		     Min,
		     Max) ->
    true = is_integer(LowBS),
    true = is_integer(HighBS),
    Alc1 = case Min < LowBS of
	       true -> Alc#alloc{low_mbc_blocks_size = Min};
	       false -> Alc
	   end,
    case Max > HighBS of
	true -> Alc1#alloc{high_mbc_blocks_size = Max};
	false -> Alc1
    end.

set_alloc_util(#alloc{alloc_util = AU} = Alc, AU) ->
    Alc;
set_alloc_util(Alc, Val) ->
    Alc#alloc{alloc_util = Val}.

chk_sbct(#alloc{sbct = undefined} = Alc, AI) ->
    case ai_value(options, sbct, AI) of
	{sbct, Bytes} when is_integer(Bytes) -> Alc#alloc{sbct = b2kb(Bytes)};
	_ -> Alc
    end;
chk_sbct(Alc, _AI) ->
    Alc.

save_scenario(AlcList) ->
    %% The high priority is not really necessary. It is
    %% used since it will make retrieval of allocator
    %% information less spread out in time on a highly
    %% loaded system.
    OP = process_flag(priority, high),
    Res = do_save_scenario(AlcList),
    process_flag(priority, OP),
    Res.
    
save_ai2(Alc, AI) ->
    Alc1 = chk_sbct(Alc, AI),
    case ai_value(mbcs, blocks_size, AI) of
	{blocks_size, MinBS, _, MaxBS} ->
	    set_alloc_util(chk_mbcs_blocks_size(Alc1, MinBS, MaxBS), true);
	_ ->
	    set_alloc_util(Alc, false)
    end.

save_ai(Alc, [{instance, 0, AI}]) ->
    save_ai2(Alc, AI);
save_ai(Alc, [{instance, _, _}, {instance, _, _}| _]) ->
    Alc#alloc{enabled = true, need_config_change = true};
save_ai(Alc, AI) ->
    save_ai2(Alc, AI). % Non erts_alloc_util allocator

do_save_scenario(AlcList) ->
    lists:map(fun (#alloc{enabled = false} = Alc) ->
		      Alc;
		  (#alloc{name = Name} = Alc) ->
		      case erlang:system_info({allocator, Name}) of
			  undefined ->
			      exit({bad_allocator_name, Name});
			  false ->
			      Alc#alloc{enabled = false};
			  AI when is_list(AI) ->
			      save_ai(Alc, AI)
		      end
	      end,
	      AlcList).

%%
%% Make configuration
%%

conf_size(Bytes) when is_integer(Bytes), Bytes < 0 ->
    exit({bad_value, Bytes});
conf_size(Bytes) when is_integer(Bytes), Bytes < 1*?MB ->
    ?ROUNDUP(?B2KB(Bytes), 256);
conf_size(Bytes) when is_integer(Bytes), Bytes < 10*?MB ->
    ?ROUNDUP(?B2KB(Bytes), ?B2KB(1*?MB));
conf_size(Bytes) when is_integer(Bytes), Bytes < 100*?MB ->
    ?ROUNDUP(?B2KB(Bytes), ?B2KB(2*?MB));
conf_size(Bytes) when is_integer(Bytes), Bytes < 256*?MB ->
    ?ROUNDUP(?B2KB(Bytes), ?B2KB(5*?MB));
conf_size(Bytes) when is_integer(Bytes) ->
    ?ROUNDUP(?B2KB(Bytes), ?B2KB(10*?MB)).

sbct(#conf{format_to = FTO}, #alloc{name = A, sbct = SBCT}) ->
    fc(FTO, "Sbc threshold size of ~p kilobytes.", [SBCT]),
    format(FTO, " +M~csbct ~p~n", [alloc_char(A), SBCT]).

default_mmbcs(temp_alloc = A, _Insts) ->
    {value, {A, MMBCS_Default}} = lists:keysearch(A, 1, ?MMBCS_DEFAULTS),
    MMBCS_Default;
default_mmbcs(A, Insts) ->
    {value, {A, MMBCS_Default}} = lists:keysearch(A, 1, ?MMBCS_DEFAULTS),
    I = case Insts > 4 of
	    true -> 4;
	    _ -> Insts
	end,
    ?ROUNDUP(MMBCS_Default div I, ?B2KB(1*?KB)).

mmbcs(#conf{format_to = FTO},
      #alloc{name = A, instances = Insts, low_mbc_blocks_size = BlocksSize}) ->
    BS = case A of
	     temp_alloc -> BlocksSize;
	     _ -> BlocksSize div Insts
	 end,
    DefMMBCS = default_mmbcs(A, Insts),
    case {Insts, BS > DefMMBCS} of
	{1, true} ->
	    MMBCS = conf_size(BS),
	    fc(FTO, "Main mbc size of ~p kilobytes.", [MMBCS]),
	    format(FTO, " +M~cmmbcs ~p~n", [alloc_char(A), MMBCS]);
	_ ->
	    MMBCS = ?B2KB(DefMMBCS),
	    fc(FTO, "Main mbc size of ~p kilobytes.", [MMBCS]),
	    format(FTO, " +M~cmmbcs ~p~n", [alloc_char(A), MMBCS]),
	    ok
    end.

smbcs_lmbcs(#conf{format_to = FTO},
	    #alloc{name = A, segments = Segments}) ->
    MBCS = Segments#segment.size,
    AC = alloc_char(A),
    fc(FTO, "Mseg mbc size of ~p kilobytes.", [MBCS]),
    format(FTO, " +M~csmbcs ~p +M~clmbcs ~p~n", [AC, MBCS, AC, MBCS]),
    ok.

alloc_char(binary_alloc) -> $B;
alloc_char(std_alloc) -> $D;
alloc_char(ets_alloc) -> $E;
alloc_char(fix_alloc) -> $F;
alloc_char(eheap_alloc) -> $H;
alloc_char(ll_alloc) -> $L;
alloc_char(mseg_alloc) -> $M;
alloc_char(driver_alloc) -> $R;
alloc_char(sl_alloc) -> $S;
alloc_char(temp_alloc) -> $T;
alloc_char(sys_alloc) -> $Y;
alloc_char(Alloc) ->
    exit({bad_allocator, Alloc}).

conf_alloc(#conf{format_to = FTO},
	   #alloc{name = A, enabled = false}) ->
    fcl(FTO, A),
    fcp(FTO,
	"WARNING: ~p has been disabled. Consider enabling ~p by passing "
	"the \"+M~ce true\" command line argument and rerun "
	"erts_alloc_config.",
	[A, A, alloc_char(A)]);
conf_alloc(#conf{format_to = FTO},
	   #alloc{name = A, need_config_change = true}) ->
    fcl(FTO, A),
    fcp(FTO,
	"WARNING: ~p has been configured in a way that prevents "
	"erts_alloc_config from creating a configuration. The configuration "
	"will be automatically adjusted to fit erts_alloc_config if you "
	"use the \"+Mea config\" command line argument while running "
	"erts_alloc_config.",
	[A]);
conf_alloc(#conf{format_to = FTO} = Conf,
	   #alloc{name = A, alloc_util = true} = Alc) ->
    fcl(FTO, A),
    chk_xnote(Conf, Alc),
    au_conf_alloc(Conf, Alc),
    format(FTO, "#~n", []);
conf_alloc(#conf{format_to = FTO} = Conf, #alloc{name = A} = Alc) ->
    fcl(FTO, A),
    chk_xnote(Conf, Alc).

chk_xnote(#conf{format_to = FTO},
	  #alloc{name = sys_alloc}) ->
    fcp(FTO, "Cannot be configured. Default malloc implementation used.");
chk_xnote(#conf{format_to = FTO},
	  #alloc{name = mseg_alloc}) ->
    fcp(FTO, "Default configuration used.");
chk_xnote(#conf{format_to = FTO},
	  #alloc{name = ll_alloc}) ->
    fcp(FTO,
	"Note, blocks allocated with ll_alloc are very "
	"seldom deallocated. Placing blocks in mseg "
	"carriers is therefore very likely only a waste "
	"of resources.");
chk_xnote(#conf{}, #alloc{}) ->
    ok.

au_conf_alloc(#conf{format_to = FTO} = Conf,
	      #alloc{name = A,
		     alloc_util = true,
		     instances = Insts,
		     acul = Acul,
		     strategy = Strategy,
		     low_mbc_blocks_size = Low,
		     high_mbc_blocks_size = High} = Alc) ->
    fcp(FTO, "Usage of mbcs: ~p - ~p kilobytes", [?B2KB(Low), ?B2KB(High)]),
    case Insts of
	1 ->
	    fc(FTO, "One instance used."),
	    format(FTO, " +M~ct false~n", [alloc_char(A)]);
	_ ->
	    fc(FTO, "~p + 1 instances used.",
	       [Insts]),
	    format(FTO, " +M~ct true~n", [alloc_char(A)]),
	    case Strategy of
		undefined ->
		    ok;
		_ ->
		    fc(FTO, "Allocation strategy: ~s.",
		       [strategy_str(Strategy)]),
		    format(FTO, " +M~cas ~s~n", [alloc_char(A),
						 atom_to_list(Strategy)])
	    end,
	    case carrier_migration_support(Strategy) of
		false ->
		    ok;
		true ->
		    fc(FTO, "Abandon carrier utilization limit of ~p%.", [Acul]),
		    format(FTO, " +M~cacul ~p~n", [alloc_char(A), Acul])
	    end
    end,
    mmbcs(Conf, Alc),
    smbcs_lmbcs(Conf, Alc),
    sbct(Conf, Alc).

calc_seg_size(Growth, Segs) ->
    conf_size(round(Growth*?FRAG_FACT*?GROWTH_SEG_FACT) div Segs).

calc_growth_segments(Conf, AlcList0) ->
    CalcSmall = fun (#alloc{name = ll_alloc, instances = 1} = Alc, Acc) ->
			{Alc#alloc{segments = #segment{size = conf_size(0),
						       number = 0}},
			 Acc};
		    (#alloc{alloc_util = true,
			    instances = Insts,
			    low_mbc_blocks_size = LowMBC,
			    high_mbc_blocks_size = High} = Alc,
		     {SL, AL}) ->
			Low = case Insts of
				  1 -> LowMBC;
				  _ -> 0
			      end,
			Growth = High - Low,
			case Growth >= ?LARGE_GROWTH_ABS_LIMIT of
			    true ->
				{Alc, {SL, AL+1}};
			    false ->
				Segs = ?SMALL_GROWTH_SEGS,
				SegSize = calc_seg_size(Growth, Segs),
				{Alc#alloc{segments
					   = #segment{size = SegSize,
						      number = Segs}},
				 {SL - Segs, AL}}

			end;
		    (Alc, Acc) -> {Alc, Acc}
		end,
    {AlcList1, {SegsLeft, AllocsLeft}}
	= lists:mapfoldl(CalcSmall, {Conf#conf.segments, 0}, AlcList0),
    case AllocsLeft of
	0 ->
	    AlcList1;
	_ ->
	    SegsPerAlloc = case (SegsLeft div AllocsLeft) + 1 of
			       SPA when SPA < ?SMALL_GROWTH_SEGS ->
				   ?SMALL_GROWTH_SEGS;
			       SPA ->
				   SPA
			   end,
	    CalcLarge = fun (#alloc{alloc_util = true,
				    segments = undefined,
				    instances = Insts,
				    low_mbc_blocks_size = LowMBC,
				    high_mbc_blocks_size = High} = Alc) ->
				Low = case Insts of
					  1 -> LowMBC;
					  _ -> 0
				      end,
				Growth = High - Low,
				SegSize = calc_seg_size(Growth,
							SegsPerAlloc),
				Alc#alloc{segments
					  = #segment{size = SegSize,
						     number = SegsPerAlloc}};
			    (Alc) ->
				Alc
			end,
	    lists:map(CalcLarge, AlcList1)
    end.

mk_config(#conf{format_to = FTO} = Conf, AlcList) ->
    format_header(FTO),
    Res = lists:foreach(fun (Alc) -> conf_alloc(Conf, Alc) end,
			calc_growth_segments(Conf, AlcList)),
    format_footer(FTO),
    Res.

format_header(FTO) ->
    {Y,Mo,D} = erlang:date(),
    {H,Mi,S} = erlang:time(),
    fcl(FTO),
    fcl(FTO, "erts_alloc configuration"),
    fcl(FTO),
    fcp(FTO,
	"This erts_alloc configuration was automatically "
	"generated at ~w-~2..0w-~2..0w ~2..0w:~2..0w.~2..0w by "
	"erts_alloc_config.",
	[Y, Mo, D, H, Mi, S]),
    fcp(FTO,
	"~s was used when generating the configuration.",
	[string:trim(erlang:system_info(system_version), both, "$\n")]),
    case erlang:system_info(schedulers) of
	1 -> ok;
	Schdlrs ->
	    fcp(FTO,
		"NOTE: This configuration was made for ~p schedulers. "
		"It is very important that ~p schedulers are used.",
		[Schdlrs, Schdlrs])
    end,
    fcp(FTO,
	"This configuration is intended as a suggestion and "
	"may need to be adjusted manually. Instead of modifying "
	"this file, you are advised to write another configuration "
	"file and override values that you want to change. "
	"Doing it this way simplifies things when you want to "
	"rerun erts_alloc_config."),
    fcp(FTO,
	"This configuration is based on the actual use of "
	"multi-block carriers (mbcs) for a set of different "
	"runtime scenarios. Note that this configuration may "
	"perform bad, ever horrible, for other runtime "
	"scenarios."),
    fcp(FTO,
	"You are advised to rerun erts_alloc_config if the "
	"applications run when the configuration was made "
	"are changed, or if the load on the applications have "
	"changed since the configuration was made. You are also "
	"advised to rerun erts_alloc_config if the Erlang runtime "
	"system used is changed."),
    fcp(FTO,
	"Note, that the singel-block carrier (sbc) parameters "
	"very much effects the use of mbcs. Therefore, if you "
	"change the sbc parameters, you are advised to rerun "
	"erts_alloc_config."),
    fcp(FTO,
	"For more information see the erts_alloc_config(3) "
	"documentation."),
    ok.

format_footer(FTO) ->
    fcl(FTO).

%%%
%%% Misc.
%%%

b2kb(B) when is_integer(B) ->
    MaxKB = (1 bsl erlang:system_info(wordsize)*8) div 1024,
    case ?B2KB(B) of
	KB when KB > MaxKB -> MaxKB;
	KB -> KB
    end.

format(false, _Frmt) ->
    ok;
format(IODev, Frmt) ->
    io:format(IODev, Frmt, []).

format(false, _Frmt, _Args) ->
    ok;
format(IODev, Frmt, Args) ->
    io:format(IODev, Frmt, Args).

%% fcp: format comment paragraf
fcp(IODev, Frmt, Args) ->
    fc(IODev, Frmt, Args),
    format(IODev, "#~n").

fcp(IODev, Frmt) ->
    fc(IODev, Frmt),
    format(IODev, "#~n").

%% fc: format comment
fc(IODev, Frmt, Args) ->
    fc(IODev, lists:flatten(io_lib:format(Frmt, Args))).

fc(IODev, String) ->
    fc_aux(IODev, string:lexemes(String, " "), 0).

fc_aux(_IODev, [], 0) ->
    ok;
fc_aux(IODev, [], _Len) ->
    format(IODev, "~n");
fc_aux(IODev, [T|Ts], 0) ->
    Len = 2 + string:length(T),
    format(IODev, "# ~s", [T]),
    fc_aux(IODev, Ts, Len);
fc_aux(IODev, [T|Ts] = ATs, Len) ->
    TLength = string:length(T),
    case (TLength + Len) >= ?PRINT_WITDH of
        true ->
            format(IODev, "~n"),
            fc_aux(IODev, ATs, 0);
        false ->
            NewLen = Len + 1 + TLength,
            format(IODev, " ~s", [T]),
            fc_aux(IODev, Ts, NewLen)
    end.

%% fcl: format comment line
fcl(FTO) ->
    EndStr = "# ",
    Precision = string:length(EndStr),
    FieldWidth = -1*(?PRINT_WITDH),
    format(FTO, "~*.*.*s~n", [FieldWidth, Precision, $-, EndStr]).

fcl(FTO, A) when is_atom(A) ->
    fcl(FTO, atom_to_list(A));
fcl(FTO, Str) when is_list(Str) ->
    Str2 = "# --- " ++ Str ++ " ",
    Precision = string:length(Str2),
    FieldWidth = -1*(?PRINT_WITDH),
    format(FTO, "~*.*.*s~n", [FieldWidth, Precision, $-, Str2]).
