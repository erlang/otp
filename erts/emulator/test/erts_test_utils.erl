%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2024. All Rights Reserved.
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

-module(erts_test_utils).

%% Prior to OTP 26, maybe_expr used to require runtime support. As it's now
%% enabled by default, all modules are tagged with the feature even when they
%% don't use it. Therefore, we explicitly disable it until OTP 25 is out of
%% support.
-feature(maybe_expr, disable).
-compile(r24).

%%
%% THIS MODULE IS ALSO USED BY *OTHER* APPLICATIONS TEST CODE
%%

-export([mk_ext_pid/3,
         mk_ext_port/2,
         mk_ext_ref/2,
         available_internal_state/1,
         check_node_dist/0, check_node_dist/1, check_node_dist/3,
         ept_check_leaked_nodes/1]).



-define(VERSION_MAGIC,       131).

-define(ATOM_EXT,            100).
-define(REFERENCE_EXT,       101).
-define(PORT_EXT,            102).
-define(PID_EXT,             103).
-define(NEW_REFERENCE_EXT,   114).
-define(NEW_PID_EXT,         $X).
-define(NEW_PORT_EXT,        $Y).
-define(NEWER_REFERENCE_EXT, $Z).
-define(V4_PORT_EXT,         $x).

-define(OLD_MAX_PIDS_PORTS, ((1 bsl 28) - 1)).
-define(OLD_MAX_PID_NUM, ((1 bsl 15) - 1)).
-define(OLD_MAX_PID_SER, ((1 bsl 13) - 1)).

uint64_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 64 ->
    [(Uint bsr 56) band 16#ff,
     (Uint bsr 48) band 16#ff,
     (Uint bsr 40) band 16#ff,
     (Uint bsr 32) band 16#ff,
     (Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint64_be(Uint) ->
    exit({badarg, uint64_be, [Uint]}).

uint32_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 32 ->
    [(Uint bsr 24) band 16#ff,
     (Uint bsr 16) band 16#ff,
     (Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint32_be(Uint) ->
    exit({badarg, uint32_be, [Uint]}).

uint16_be(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 16 ->
    [(Uint bsr 8) band 16#ff,
     Uint band 16#ff];
uint16_be(Uint) ->
    exit({badarg, uint16_be, [Uint]}).

uint8(Uint) when is_integer(Uint), 0 =< Uint, Uint < 1 bsl 8 ->
    Uint band 16#ff;
uint8(Uint) ->
    exit({badarg, uint8, [Uint]}).

pid_tag(_, _, bad_creation) ->
    ?PID_EXT;
pid_tag(Num, Ser, Creation) when Num =< ?OLD_MAX_PID_NUM,
                                 Ser =< ?OLD_MAX_PID_SER,
                                 Creation =< 3 ->
    ?PID_EXT;
pid_tag(_Num, _Ser, _Creation) ->
    ?NEW_PID_EXT.

enc_creation(_, bad_creation) ->
    uint8(4);
enc_creation(Num, Creation) when is_integer(Num),
                                 Num =< ?OLD_MAX_PIDS_PORTS,
                                 Creation =< 3 ->
    uint8(Creation);
enc_creation(Nums, Creation) when is_list(Nums),
                                  length(Nums) =< 3,
                                  Creation =< 3 ->
    uint8(Creation);
enc_creation(_Num, Creation) ->
    uint32_be(Creation).

enc_creation(_, _, bad_creation) ->
    uint8(4);
enc_creation(Num, Ser, Creation) when Num =< ?OLD_MAX_PID_NUM,
                                      Ser =< ?OLD_MAX_PID_SER,
                                      Creation =< 3 ->
    uint8(Creation);
enc_creation(_Num, _Ser, Creation) ->
    uint32_be(Creation).

mk_ext_pid({NodeName, Creation}, Number, Serial) when is_atom(NodeName) ->
    mk_ext_pid({atom_to_list(NodeName), Creation}, Number, Serial);
mk_ext_pid({NodeName, Creation}, Number, Serial) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      pid_tag(Number, Serial, Creation),
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      uint32_be(Number),
					      uint32_be(Serial),
					      enc_creation(Number, Serial, Creation)])) of
	Pid when is_pid(Pid) ->
	    Pid;
	{'EXIT', {badarg, uint32_be, _}} ->
	    exit({badarg, mk_pid, [{NodeName, Creation}, Number, Serial]});
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_pid, [{NodeName, Creation}, Number, Serial]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

port_tag(_Num, bad_creation) ->
    ?PORT_EXT;
port_tag(Num, Creation) when 0 =< Num, Num =< ?OLD_MAX_PIDS_PORTS, Creation =< 3 ->
    ?PORT_EXT;
port_tag(Num, _Creation) when 0 =< Num, Num =< ?OLD_MAX_PIDS_PORTS ->
    ?NEW_PORT_EXT;
port_tag(_Num, _Creation) ->
    ?V4_PORT_EXT.

mk_ext_port({NodeName, Creation}, Number) when is_atom(NodeName) ->
    mk_ext_port({atom_to_list(NodeName), Creation}, Number);
mk_ext_port({NodeName, Creation}, Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      port_tag(Number, Creation),
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      case Number > ?OLD_MAX_PIDS_PORTS of
						  true -> uint64_be(Number);
						  false -> uint32_be(Number)
					      end,
					      enc_creation(Number, Creation)])) of
	Port when is_port(Port) ->
	    Port;
	{'EXIT', {badarg, Uint, _}} when Uint == uint64_be; Uint == uint32_be ->
	    exit({badarg, mk_port, [{NodeName, Creation}, Number]});
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_port, [{NodeName, Creation}, Number]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.

ref_tag(_Nums, bad_creation) -> ?NEW_REFERENCE_EXT;
ref_tag(Nums, Creation) when length(Nums) =< 3, Creation =< 3 -> ?NEW_REFERENCE_EXT;
ref_tag(_Nums, _Creation) -> ?NEWER_REFERENCE_EXT.

mk_ext_ref({NodeName, Creation}, Numbers) when is_atom(NodeName),
                                               is_list(Numbers) ->
    mk_ext_ref({atom_to_list(NodeName), Creation}, Numbers);
mk_ext_ref({NodeName, Creation}, [Number]) when is_list(NodeName),
                                                Creation =< 3,
                                                is_integer(Number) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
                                              ?REFERENCE_EXT,
                                              ?ATOM_EXT,
                                              uint16_be(length(NodeName)),
                                              NodeName,
                                              uint32_be(Number),
                                              uint8(Creation)])) of
        Ref when is_reference(Ref) ->
            Ref;
        {'EXIT', {badarg, _}} ->
            exit({badarg, mk_ref, [{NodeName, Creation}, [Number]]});
        Other ->
            exit({unexpected_binary_to_term_result, Other})
    end;
mk_ext_ref({NodeName, Creation}, Numbers) when is_list(NodeName),
                                               is_list(Numbers) ->
    case catch binary_to_term(list_to_binary([?VERSION_MAGIC,
					      ref_tag(Numbers, Creation),
					      uint16_be(length(Numbers)),
					      ?ATOM_EXT,
					      uint16_be(length(NodeName)),
					      NodeName,
					      enc_creation(Numbers, Creation),
					      lists:map(fun (N) ->
								uint32_be(N)
							end,
							Numbers)])) of
	Ref when is_reference(Ref) ->
	    Ref;
	{'EXIT', {badarg, _}} ->
	    exit({badarg, mk_ref, [{NodeName, Creation}, Numbers]});
	Other ->
	    exit({unexpected_binary_to_term_result, Other})
    end.


available_internal_state(Bool) when Bool == true; Bool == false ->
    case {Bool,
          (catch erts_debug:get_internal_state(available_internal_state))} of
        {true, true} ->
            true;
        {false, true} ->
            erts_debug:set_internal_state(available_internal_state, false),
            true;
        {true, _} ->
            erts_debug:set_internal_state(available_internal_state, true),
            false;
        {false, _} ->
            false
    end.


%%
%% Check reference counters for node- and dist entries.
%%
check_node_dist() ->
    check_node_dist(fun(ErrMsg) ->
                            io:format("check_node_dist ERROR:\n~p\n", [ErrMsg]),
                            error
                    end).

check_node_dist(Fail) ->
    AIS = available_internal_state(true),
    [erlang:garbage_collect(P) || P <- erlang:processes()],
    {{node_references, NodeRefs},
     {dist_references, DistRefs}} =
        erts_debug:get_internal_state(node_and_dist_references),
    R = check_node_dist(Fail, NodeRefs, DistRefs),
    available_internal_state(AIS),
    R.

check_node_dist(Fail, NodeRefs, DistRefs) ->
    AIS = available_internal_state(true),
    R = check_nd_refc({node(),erlang:system_info(creation)},
                  NodeRefs, DistRefs, Fail),
    available_internal_state(AIS),
    R.


check_nd_refc({ThisNodeName, ThisCreation}, NodeRefs, DistRefs, Fail) ->
    case catch begin
                   check_refc(ThisNodeName,ThisCreation,"node table",NodeRefs),
                   check_refc(ThisNodeName,ThisCreation,"dist table",DistRefs),
                   ok
               end of
        ok ->
            ok;
        {'EXIT', Reason} ->
            {Y,Mo,D} = date(),
            {H,Mi,S} = time(),
            ErrMsg = io_lib:format("~n"
                                   "*** Reference count check of node ~w "
                                   "failed (~p) at ~w~w~w ~w:~w:~w~n"
                                   "*** Node table references:~n ~p~n"
                                   "*** Dist table references:~n ~p~n",
                                   [node(), Reason, Y, Mo, D, H, Mi, S,
                                    NodeRefs, DistRefs]),
            Fail(lists:flatten(ErrMsg))
    end.


check_refc(ThisNodeName,ThisCreation,Table,EntryList) when is_list(EntryList) ->
    lists:foreach(
      fun ({Entry, Refc, ReferrerList}) ->
              {DelayedDeleteTimer,
               FoundRefs} =
              lists:foldl(
                fun ({Referrer, ReferencesList}, {DDT, A1}) ->
                        {case Referrer of
                             {system,delayed_delete_timer} ->
                                 true;
                             {system,thread_progress_delete_timer} ->
                                 true;
                             _ ->
                                 DDT
                         end,
                         A1 + lists:foldl(fun ({_T,Rs},A2) ->
                                                  A2+Rs
                                          end,
                                          0,
                                          ReferencesList)}
                end,
                {false, 0},
                ReferrerList),

              %% Reference count equals found references?
              case {Refc, FoundRefs, DelayedDeleteTimer} of
                  {X, X, _} ->
                      ok;
                  {0, 1, true} ->
                      ok;
                  _ ->
                      exit({invalid_reference_count, Table, Entry})
              end,

              %% All entries in table referred to?
              case {Entry, Refc} of
                  {ThisNodeName, 0} -> ok;
                  {{ThisNodeName, ThisCreation}, 0} -> ok;
                  {_, 0} when DelayedDeleteTimer == false ->
                      exit({not_referred_entry_in_table, Table, Entry});
                  {_, _} -> ok 
              end

      end,
      EntryList),
    ok.

%% To be called by end_per_testcase
%% to check and kill leaked node connections.
ept_check_leaked_nodes(Config) ->
    case nodes(connected) of
        [] -> true;
        Nodes ->
            [net_kernel:disconnect(N) || N <- Nodes],
            Leaked =  {"Leaked connections", Nodes},
            Fail = case proplists:get_value(tc_status, Config) of
                       ok -> Leaked;
                       {failed, Reason} ->
                           [Reason, {end_per_testcase, Leaked}];
                       {skipped, _}=Skipped ->
                           [Skipped, {end_per_testcase, Leaked}]
                   end,
            {fail, Fail}
    end.
