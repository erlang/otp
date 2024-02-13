%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

%%%
%%% This pass skips stack trace generation on erlang:throw/1 when they're
%%% guaranteed not to be inspected.
%%%
%%% It does so by finding all exception handlers in the module, determining
%%% which functions they cover, and then simulating the result of every throw/1
%%% in the module; if a particular throw/1 never reaches an instruction that
%%% creates a stack trace, then it's safe to rewrite it.
%%%

-module(beam_ssa_throw).
-moduledoc false.

-export([module/2]).

-import(lists, [foldl/3]).

-include("beam_ssa.hrl").
-include("beam_types.hrl").

-type exception_var() :: none | #b_var{}.
-type exception_vars() :: {Class :: exception_var(),
                           Reason :: exception_var(),
                           Stacktrace :: exception_var()}.
-type tentative() :: {tentative,
                      Start :: beam_ssa:label(),
                      Vars :: exception_vars(),
                      Blocks :: beam_ssa:block_map()}.

-type handler() :: tentative() | unsuitable | suitable.
-type handler_id() :: {#b_local{}, beam_ssa:label()}.

-type suitability() :: {tentative, beam_ssa:label()} |
                       unsuitable |
                       suitable.

%% Per-module scan state
-record(gst, {tlh_roots :: gb_trees:tree(#b_local{}, gb_sets:set(handler())),
              tlh_edges=#{} :: #{ #b_local{} => gb_sets:set(#b_local{}) },
              throws=sets:new([{version, 2}]) :: sets:set(#b_local{})}).

%% Per-function scan state
-record(lst, {suitability=#{} :: #{ #b_var{} => suitability() },
              handlers=#{} :: #{ handler_id() => handler() },
              blocks :: beam_ssa:block_map(),
              predecessors :: beam_ssa:predecessor_map()}).

-spec module(beam_ssa:b_module(), [compile:option()]) ->
          {'ok',beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, _Opts) ->
    case scan(Module) of
        {Throws, TLHs} ->
            Fs = opt(Fs0, Throws, TLHs),
            {ok, Module#b_module{body=Fs}};
        no_throws ->
            {ok, Module}
    end.

%% Builds a map with the top-level exception handlers (TLHs) for each function,
%% as well as the set of functions that call throw/1.
scan(#b_module{body=Fs0}=Module) ->
    scan_1(Fs0, init_gst(Module)).

init_gst(#b_module{exports=Exports}) ->
    %% We can't optimize any throw that leaves the module, so we'll add an
    %% unsuitable top-level handler to all exported functions.
    Unsuitable = gb_sets:singleton(unsuitable),
    Roots = foldl(fun({Name, Arity}, Acc) ->
                          Id = #b_local{name=#b_literal{val=Name},arity=Arity},
                          gb_trees:insert(Id, Unsuitable, Acc)
                  end, gb_trees:empty(), Exports),
    #gst{tlh_roots=Roots}.

scan_1([#b_function{bs=Blocks}=F | Fs], Gst0) ->
    Id = get_func_id(F),

    Preds = beam_ssa:predecessors(Blocks),
    Linear = beam_ssa:linearize(Blocks),

    Lst = #lst{blocks=Blocks,predecessors=Preds},
    {_, Gst} = scan_bs(Linear, Id, Lst, Gst0),

    scan_1(Fs, Gst);
scan_1([], Gst) ->
    #gst{tlh_roots=Roots,
         tlh_edges=Edges,
         throws=Throws} = Gst,

    case sets:is_empty(Throws) of
        true ->
            no_throws;
        false ->
            TLHs = propagate_tlhs(gb_trees:to_list(Roots), Edges, #{}),
            {Throws, TLHs}
    end.

%% Propagates all top-level handlers to the functions they cover. See
%% inherit_tlh/3 and add_tlh/3.
propagate_tlhs([{Id, HandlersA} | Roots], Edges, Acc0) ->
    HandlersB = maps:get(Id, Acc0, gb_sets:empty()),
    case gb_sets:is_subset(HandlersA, HandlersB) of
        true ->
            propagate_tlhs(Roots, Edges, Acc0);
        false ->
            Merged = gb_sets:union(HandlersA, HandlersB),
            Callees = pt_callees(Id, Merged, Edges),

            Acc = propagate_tlhs(Callees, Edges, Acc0#{ Id => Merged }),
            propagate_tlhs(Roots, Edges, Acc)
    end;
propagate_tlhs([], _Edges, Acc) ->
    Acc.

pt_callees(Id, Merged, Edges) ->
    case Edges of
        #{ Id := Callees } ->
            gb_sets:fold(fun(Callee, Acc) ->
                                 [{Callee, Merged} | Acc]
                         end, [], Callees);
        #{} ->
            []
    end.

%% We scan the blocks in post-order so we can finish analyzing handlers before
%% reaching their affected calls. This lets us avoid unnecessary indirection
%% when adding or inspecting top-level handlers.
scan_bs([{?EXCEPTION_BLOCK, _} | Bs], Id, Lst, Gst) ->
    scan_bs(Bs, Id, Lst, Gst);
scan_bs([{Lbl, #b_blk{last=Last,is=Is}} | Bs], Id, Lst0, Gst0) ->
    {Lst, Gst} = scan_bs(Bs, Id, Lst0, Gst0),
    si_is(Is, Id, Lbl, Last, Lst, Gst);
scan_bs([], _Id, Lst, Gst) ->
    {Lst, Gst}.

si_is([#b_set{op=landingpad,args=[_Kind, _Tag]} | Is],
      Id, Lbl, Last, Lst, Gst) ->
    %% Class:Reason:Stacktrace
    Vars = {none, none, none},
    si_handler_start(Is, Id, Lbl, Last, Vars, Lst, Gst);
si_is([#b_set{op=resume,args=[Stacktrace, _Reason]} | Is],
      Id, Lbl, Last, Lst, Gst) ->
    si_handler_end(Is, Id, Lbl, Last, Stacktrace, Lst, Gst);
si_is([#b_set{op=raw_raise,args=[_,_,Stacktrace]} | Is],
      Id, Lbl, Last, Lst, Gst) ->
    si_handler_end(Is, Id, Lbl, Last, Stacktrace, Lst, Gst);
si_is([#b_set{op=build_stacktrace,args=[Stacktrace]} | Is],
      Id, Lbl, Last, Lst, Gst) ->
    si_handler_end(Is, Id, Lbl, Last, Stacktrace, Lst, Gst);
si_is([#b_set{op=MakeFun,args=[#b_local{}=Callee | _]} | _Is],
      _Id, _Lbl, _Last, Lst, Gst)
  when MakeFun =:= make_fun;
       MakeFun =:= old_make_fun ->
    #gst{tlh_roots = Roots0} = Gst,

    %% Funs may be called from anywhere which may result in a throw escaping
    %% the module, so we'll add an unsuitable top-level handler to all funs.
    Handlers = case gb_trees:lookup(Callee, Roots0) of
                    {value, Handlers0} -> gb_sets:add(unsuitable, Handlers0);
                    none -> gb_sets:singleton(unsuitable)
                end,
    Roots = gb_trees:enter(Callee, Handlers, Roots0),

    {Lst, Gst#gst{tlh_roots=Roots}};
si_is([#b_set{op=call,
              dst=Dst,
              args=[#b_remote{mod=#b_literal{val=erlang},
                              name=#b_literal{val=throw},
                              arity=1}, _Term]},
       #b_set{op={succeeded,body},args=[Dst]}],
      Id, _Lbl, #b_br{fail=?EXCEPTION_BLOCK}, Lst, Gst) ->
    %% Tail throw, handled by caller. We'll need to visit this function again.
    #gst{throws=Throws0} = Gst,
    Throws = sets:add_element(Id, Throws0),
    {Lst, Gst#gst{throws=Throws}};
si_is([#b_set{op=call,dst=Dst,args=[#b_local{}=Callee | _]},
       #b_set{op={succeeded,body},args=[Dst]}],
      Id, _Lbl, #b_br{fail=?EXCEPTION_BLOCK}, Lst, Gst) ->
    %% No local handler, inherit our caller's top-level handlers.
    {Lst, inherit_tlh(Id, Callee, Gst)};
si_is([#b_set{op=call,dst=Dst,args=[#b_local{}=Callee | _]},
       #b_set{op={succeeded,body},args=[Dst]}],
      Id, _Lbl, #b_br{fail=Fail}, Lst, Gst) ->
    %% Local handler, register it as a top-level handler with our callee.
    HandlerId = {Id, Fail},
    {Lst, add_tlh(HandlerId, Callee, Lst, Gst)};
si_is([#b_set{} | Is], Id, Lbl, Last, Lst, Gst) ->
    si_is(Is, Id, Lbl, Last, Lst, Gst);
si_is([], _Id, _Lbl, _Last, Lst, Gst) ->
    {Lst, Gst}.

%% Marks the end of a handler. Note that we encounter this _before_ the start
%% since we iterate the blocks in post-order.
si_handler_end(Is, Id, EndLbl, Last, Stacktrace, Lst0, Gst) ->
    #lst{suitability=Suitability0} = Lst0,

    Marker = case Suitability0 of
                 #{ Stacktrace := {tentative, _} } ->
                     %% Multiple uses make partitioning more difficult, so
                     %% we'll disqualify this handler to save us the hassle.
                     unsuitable;
                 #{} ->
                     %% This handler may or may not be suitable depending on
                     %% what's thrown, so we keep it for later analysis.
                     {tentative, EndLbl}
             end,

    Suitability = Suitability0#{ Stacktrace => Marker },

    Lst = Lst0#lst{suitability=Suitability},
    si_is(Is, Id, EndLbl, Last, Lst, Gst).

si_handler_start([#b_set{op=extract,
                         dst=Dst,
                         args=[_, #b_literal{val=Idx}]} | Is],
                 Id, StartLbl, Last, Vars0, Lst, Gst) ->
    none = element(1 + Idx, Vars0),             %Assertion.
    Vars = setelement(1 + Idx, Vars0, Dst),
    si_handler_start(Is, Id, StartLbl, Last, Vars, Lst, Gst);
si_handler_start(Is, Id, StartLbl, Last, Vars, Lst0, Gst) ->
    HandlerId = {Id, StartLbl},

    #lst{blocks=Blocks,
         predecessors=Preds,
         suitability=Suitability,
         handlers=Handlers0} = Lst0,

    {_, _, Stacktrace} = Vars,

    Handlers = case Suitability of
                   #{ Stacktrace := {tentative, EndLbl} } ->
                       %% This handler's suitability depends on what we throw,
                       %% so we'll extract the blocks between handler start and
                       %% the first use of the stack trace for later analysis.
                       Path = beam_ssa:between(StartLbl, EndLbl, Preds, Blocks),
                       Partition = maps:with(Path, Blocks),

                       Handler = {tentative, StartLbl, Vars, Partition},
                       Handlers0#{ HandlerId => Handler };
                   #{} when Stacktrace =/= none ->
                       %% Stack trace is used in an unsupported manner.
                       Handlers0#{ HandlerId => unsuitable };
                   #{} when Stacktrace =:= none ->
                       %% Stack trace is never used, so it's always safe to
                       %% skip trace generation.
                       Handlers0#{ HandlerId => suitable }
               end,

    Lst = Lst0#lst{handlers=Handlers},

    si_is(Is, Id, StartLbl, Last, Lst, Gst).

%% Makes the callee inherit its callers top-level handlers
inherit_tlh(Caller, Callee, #gst{tlh_edges=Edges0}=Gst) ->
    Callees = case Edges0 of
                  #{ Caller := Callees0 } ->
                      gb_sets:add_element(Callee, Callees0);
                  #{} ->
                      gb_sets:singleton(Callee)
              end,
    Edges = Edges0#{ Caller => Callees },
    Gst#gst{tlh_edges=Edges}.

%% Registers the given handler as a top-level handler for Callee
add_tlh(Id, Callee, Lst, Gst) ->
    #lst{handlers=Handlers} = Lst,
    #gst{tlh_roots=Roots0} = Gst,

    #{ Id := Handler } = Handlers,

    TLHs0 = case gb_trees:lookup(Callee, Roots0) of
                none -> gb_sets:singleton(Handler);
                {value, V} -> V
            end,

    TLHs = case gb_sets:is_element(unsuitable, TLHs0) of
               true ->
                   %% One bad apple spoils the bunch.
                   TLHs0;
               false ->
                   gb_sets:add_element(Handler, TLHs0)
           end,

    Roots = gb_trees:enter(Callee, TLHs, Roots0),
    Gst#gst{tlh_roots=Roots}.

%% Goes through all functions that call throw/1, checking whether the stack
%% trace is unused in all of its top-level exception handlers (TLHs), and
%% rewriting such calls to `raise(throw, Term, [])`.
opt([#b_function{bs=Blocks0}=F | Fs], Throws, TLHs) ->
    Id = get_func_id(F),

    Blocks = case {sets:is_element(Id, Throws), TLHs} of
                 {true, #{ Id := Handlers } } ->
                     opt_function(Handlers, Blocks0);
                 {_, _} ->
                     %% We're either unreachable or never throw.
                     Blocks0
             end,

    [F#b_function{bs=Blocks} | opt(Fs, Throws, TLHs)];
opt([], _Throws, _TLHs) ->
    [].

get_func_id(#b_function{anno=Anno}) ->
    {_,Name,Arity} = maps:get(func_info, Anno),
    #b_local{name=#b_literal{val=Name},arity=Arity}.

opt_function(Handlers, Blocks) ->
    case gb_sets:is_member(unsuitable, Handlers) of
        true ->
            Blocks;
        false ->
            Linear0 = beam_ssa:linearize(Blocks),
            Linear = opt_bs(Linear0, gb_sets:to_list(Handlers)),
            maps:from_list(Linear)
    end.

opt_bs([{Lbl, #b_blk{last=Last,is=Is0}=Blk} | Bs], Hs) ->
    Is = opt_is(Is0, Last, Hs),
    [{Lbl, Blk#b_blk{is=Is}} | opt_bs(Bs, Hs)];
opt_bs([], _Hs) ->
    [].

opt_is([#b_set{op=call,
               dst=Dst,
               args=[#b_remote{mod=#b_literal{val=erlang},
                               name=#b_literal{val=throw},
                               arity=1}, _Term]}=I0,
        #b_set{op={succeeded,body},args=[Dst]}=Succ],
       #b_br{}, Hs) ->
    ThrownType = beam_ssa:get_anno(thrown_type, I0, any),
    I = opt_throw(Hs, ThrownType, I0),
    [I, Succ];
opt_is([I | Is], Last, Hs) ->
    [I | opt_is(Is, Last, Hs)];
opt_is([], _Last, _Hs) ->
    [].

opt_throw([suitable | Hs], ThrownType, I) ->
    opt_throw(Hs, ThrownType, I);
opt_throw([{tentative, Start, Vars, Blocks} | Hs], ThrownType, I) ->
    case opt_is_suitable(Start, Blocks, Vars, ThrownType) of
        true -> opt_throw(Hs, ThrownType, I);
        false -> I
    end;
opt_throw([], _ThrownType, #b_set{args=[_, Reason]}=I) ->
    %% All handlers caught us and ignored our stack trace. Rewrite ourselves to
    %% erlang:raise/3 with an empty stack trace.
    MFA = #b_remote{mod=#b_literal{val=erlang},
                    name=#b_literal{val=raise},
                    arity=3},
    Stacktrace = #b_literal{val=[]},
    I#b_set{args=[MFA, #b_literal{val=throw}, Reason, Stacktrace]}.

%% Checks whether we avoid building a stack trace when entering the handler
%% with the given throw type ("reason").
%%
%% The analysis is similar to that of the type pass, and I'd liked to have used
%% that more directly but it would require much more work for little benefit;
%% the watered-down version below is good enough to handle the most common
%% cases.
opt_is_suitable(Start, Blocks, Vars, ThrownType) ->
    Ts = ois_init_ts(Vars, ThrownType),
    ois_1([Start], Blocks, Ts).

ois_1([Lbl | Lbls], Blocks, Ts0) ->
    case Blocks of
        #{ Lbl := #b_blk{last=Last,is=Is} } ->
            case ois_is(Is, Ts0) of
                {ok, Ts} ->
                    Next = ois_successors(Last, Ts),
                    ois_1(Next ++ Lbls, Blocks, Ts);
                error ->
                    false
            end;
        #{} ->
            ois_1(Lbls, Blocks, Ts0)
    end;
ois_1([], _Blocks, _Ts) ->
    true.

ois_successors(#b_switch{fail=Fail,list=List}, _Ts) ->
    Lbls = [Lbl || {_, Lbl} <- List],
    [Fail | Lbls];
ois_successors(#b_br{bool=Bool,succ=Succ,fail=Fail}, Ts) ->
    case beam_types:get_singleton_value(ois_get_type(Bool, Ts)) of
        {ok, true} -> [Succ];
        {ok, false} -> [Fail];
        error -> [Succ, Fail]
    end.

ois_init_ts({Class, Reason, Stacktrace}, ThrownType) ->
    Ts = #{ Class => beam_types:make_atom(throw),
            Reason => ThrownType,
            Stacktrace => any },
    maps:remove(none, Ts).

ois_is([#b_set{op=build_stacktrace} | _], _Ts) ->
    error;
ois_is([#b_set{op=raw_raise} | _], _Ts) ->
    error;
ois_is([#b_set{op=resume} | _], _Ts) ->
    error;
ois_is([#b_set{op=get_hd,dst=Dst,args=[Src]} | Is], Ts0) ->
    SrcType = ois_get_type(Src, Ts0),
    {Type, _, _} = beam_call_types:types(erlang, hd, [SrcType]),
    Ts = Ts0#{ Dst => Type },
    ois_is(Is, Ts);
ois_is([#b_set{op=get_tl,dst=Dst,args=[Src]} | Is], Ts0) ->
    SrcType = ois_get_type(Src, Ts0),
    {Type, _, _} = beam_call_types:types(erlang, tl, [SrcType]),
    Ts = Ts0#{ Dst => Type },
    ois_is(Is, Ts);
ois_is([#b_set{op=get_tuple_element,
               dst=Dst,
               args=[Src, #b_literal{val=Offset}]} | Is], Ts0) ->
    Type = case Ts0 of
               #{ Src := #t_tuple{size=Size,elements=Es} } when Offset < Size ->
                   beam_types:get_tuple_element(Offset + 1, Es);
               #{} ->
                   any
           end,
    Ts = Ts0#{ Dst => Type },
    ois_is(Is, Ts);
ois_is([#b_set{op={bif,is_atom},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_atom{}, Is, Ts);
ois_is([#b_set{op={bif,is_bitstring},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_bitstring{}, Is, Ts);
ois_is([#b_set{op={bif,is_binary},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_bitstring{size_unit=8}, Is, Ts);
ois_is([#b_set{op={bif,is_float},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_float{}, Is, Ts);
ois_is([#b_set{op={bif,is_integer},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_integer{}, Is, Ts);
ois_is([#b_set{op={bif,is_list},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_list{}, Is, Ts);
ois_is([#b_set{op={bif,is_map},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_map{}, Is, Ts);
ois_is([#b_set{op={bif,is_number},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_number{}, Is, Ts);
ois_is([#b_set{op={bif,is_tuple},dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_tuple{}, Is, Ts);
ois_is([#b_set{op=is_nonempty_list,dst=Dst,args=[Src]} | Is], Ts) ->
    ois_type_test(Src, Dst, #t_cons{}, Is, Ts);
ois_is([#b_set{op=is_tagged_tuple,dst=Dst,args=[Src, Size, Tag]} | Is], Ts) ->
    Es = beam_types:set_tuple_element(1, ois_get_type(Tag, Ts), #{}),
    #b_literal{val=N} = Size,

    Type = #t_tuple{exact=true,size=N,elements=Es},

    ois_type_test(Src, Dst, Type, Is, Ts);
ois_is([#b_set{op={bif,tuple_size},dst=Dst,args=[Src]} | Is], Ts0) ->
    SrcType = ois_get_type(Src, Ts0),
    {Type, _, _} = beam_call_types:types(erlang, tuple_size, [SrcType]),
    Ts = Ts0#{ Dst => Type },
    ois_is(Is, Ts);
ois_is([#b_set{op={bif,'=:='},
               dst=Dst,
               args=[LHS,#b_literal{val=RHS}]} | Is], Ts0) ->
    Type = case beam_types:get_singleton_value(ois_get_type(LHS, Ts0)) of
               {ok, RHS} -> beam_types:make_atom(true);
               {ok, _Other} -> beam_types:make_atom(false);
               error -> beam_types:make_boolean()
           end,
    Ts = Ts0#{ Dst => Type },
    ois_is(Is, Ts);
ois_is([#b_set{} | Is], Ts) ->
    ois_is(Is, Ts);
ois_is([], Ts) ->
    {ok, Ts}.

ois_type_test(Src, Dst, RequiredType, Is, Ts0) ->
    GivenType = ois_get_type(Src, Ts0),
    Type = case beam_types:meet(GivenType, RequiredType) of
               GivenType -> beam_types:make_atom(true);
               none -> beam_types:make_atom(false);
               _Other -> beam_types:make_boolean()
           end,
    Ts = Ts0#{ Dst => Type },
    ois_is(Is, Ts).

ois_get_type(#b_literal{val=Value}, _Ts) ->
    beam_types:make_type_from_value(Value);
ois_get_type(#b_var{}=Arg, Ts) ->
    case Ts of
        #{ Arg := Type } -> Type;
        #{} -> any
    end.
