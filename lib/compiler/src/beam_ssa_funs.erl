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

%%%
%%% If a fun is defined locally and only used for calls, it can be replaced
%%% with direct calls to the relevant function. This greatly speeds up "named
%%% functions" (which rely on make_fun to recreate themselves) and macros that
%%% wrap their body in a fun.
%%%

-module(beam_ssa_funs).

-export([module/2]).

-include("beam_ssa.hrl").

-import(lists, [foldl/3]).

-spec module(Module, Options) -> Result when
      Module :: beam_ssa:b_module(),
      Options :: [compile:option()],
      Result :: {ok, beam_ssa:b_module()}.

module(#b_module{body=Fs0}=Module, _Opts) ->
    Trampolines = foldl(fun find_trampolines/2, #{}, Fs0),
    Fs = [lfo(F, Trampolines) || F <- Fs0],
    {ok, Module#b_module{body=Fs}}.

%% If a function does absolutely nothing beyond calling another function with
%% the same arguments in the same order, we can shave off a call by short-
%% circuiting it.
find_trampolines(#b_function{args=Args,bs=Blocks}=F, Trampolines) ->
    case map_get(0, Blocks) of
        #b_blk{is=[#b_set{op=call,
                          args=[#b_local{}=Actual | Args],
                          dst=Dst}],
               last=#b_ret{arg=Dst}} ->
            {_, Name, Arity} = beam_ssa:get_anno(func_info, F),
            Trampoline = #b_local{name=#b_literal{val=Name},arity=Arity},
            Trampolines#{Trampoline => Actual};
        _ ->
            Trampolines
    end.

lfo(#b_function{bs=Blocks0}=F, Trampolines) ->
    Linear0 = beam_ssa:linearize(Blocks0),
    Linear = lfo_optimize(Linear0, lfo_analyze(Linear0, #{}), Trampolines),
    F#b_function{bs=maps:from_list(Linear)}.

%% Gather a map of the locally defined funs that are only used for calls.
lfo_analyze([{_L,#b_blk{is=Is,last=Last}}|Bs], LFuns0) ->
    LFuns = lfo_analyze_last(Last, lfo_analyze_is(Is, LFuns0)),
    lfo_analyze(Bs, LFuns);
lfo_analyze([], LFuns) ->
    LFuns.

lfo_analyze_is([#b_set{op=make_fun,
                       dst=Dst,
                       args=[#b_local{} | FreeVars]}=Def | Is],
               LFuns0) ->
    LFuns = maps:put(Dst, Def, maps:without(FreeVars, LFuns0)),
    lfo_analyze_is(Is, LFuns);
lfo_analyze_is([#b_set{op=call,
                       args=[Fun | CallArgs]} | Is],
               LFuns) when is_map_key(Fun, LFuns) ->
    #b_set{args=[#b_local{arity=Arity} | FreeVars]} = map_get(Fun, LFuns),
    case length(CallArgs) + length(FreeVars) of
        Arity ->
            lfo_analyze_is(Is, maps:without(CallArgs, LFuns));
        _ ->
            %% This will `badarity` at runtime, and it's easier to disable the
            %% optimization than to simulate it.
            lfo_analyze_is(Is, maps:without([Fun | CallArgs], LFuns))
    end;
lfo_analyze_is([#b_set{args=Args} | Is], LFuns) when map_size(LFuns) =/= 0 ->
    %% We disqualify funs that are used outside calls because this forces them
    %% to be created anyway, and the slight performance gain from direct calls
    %% is not enough to offset the potential increase in stack frame size (the
    %% free variables need to be kept alive until the call).
    %%
    %% This is also a kludge to make HiPE work, as the latter will generate
    %% code with the assumption that the functions referenced in a make_fun
    %% will only be used by funs, which will not be the case if we mix it with
    %% direct calls. See cerl_cconv.erl for details.
    %%
    %% Future optimizations like delaying fun creation until use may require us
    %% to copy affected functions so that HiPE gets its own to play with (until
    %% HiPE is fixed anyway).
    lfo_analyze_is(Is, maps:without(Args, LFuns));
lfo_analyze_is([_ | Is], LFuns) ->
    lfo_analyze_is(Is, LFuns);
lfo_analyze_is([], LFuns) ->
    LFuns.

lfo_analyze_last(#b_switch{arg=Arg}, LFuns) ->
    maps:remove(Arg, LFuns);
lfo_analyze_last(#b_ret{arg=Arg}, LFuns) ->
    maps:remove(Arg, LFuns);
lfo_analyze_last(_, LFuns) ->
    LFuns.

%% Replace all calls of suitable funs with a direct call to their
%% implementation. Liveness optimization will get rid of the make_fun
%% instruction.
lfo_optimize(Linear, LFuns, _Trampolines) when map_size(LFuns) =:= 0 ->
    Linear;
lfo_optimize(Linear, LFuns, Trampolines) ->
    lfo_optimize_1(Linear, LFuns, Trampolines).

lfo_optimize_1([{L,#b_blk{is=Is0}=Blk}|Bs], LFuns, Trampolines) ->
    Is = lfo_optimize_is(Is0, LFuns, Trampolines),
    [{L,Blk#b_blk{is=Is}} | lfo_optimize_1(Bs, LFuns, Trampolines)];
lfo_optimize_1([], _LFuns, _Trampolines) ->
    [].

lfo_optimize_is([#b_set{op=call,
                        args=[Fun | CallArgs]}=Call0 | Is],
                LFuns, Trampolines) when is_map_key(Fun, LFuns) ->
    #b_set{args=[Local | FreeVars]} = map_get(Fun, LFuns),
    Args = [lfo_short_circuit(Local, Trampolines) | CallArgs ++ FreeVars],
    Call = beam_ssa:add_anno(local_fun_opt, Fun, Call0#b_set{args=Args}),
    [Call | lfo_optimize_is(Is, LFuns, Trampolines)];
lfo_optimize_is([I | Is], LFuns, Trampolines) ->
    [I | lfo_optimize_is(Is, LFuns, Trampolines)];
lfo_optimize_is([], _LFuns, _Trampolines) ->
    [].

lfo_short_circuit(Call, Trampolines) ->
    case maps:find(Call, Trampolines) of
        {ok, Other} -> lfo_short_circuit(Other, Trampolines);
        error -> Call
    end.
