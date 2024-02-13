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

-module(compile_prop).
-compile([export_all, nowarn_export_all]).

%% This module only supports proper, as we don't have an eqc license to test
%% with.

-proptest([proper]).

-ifdef(PROPER).

-define(BEAM_TYPES_INTERNAL, true).
-include_lib("compiler/src/beam_types.hrl").

-include_lib("proper/include/proper.hrl").
-define(MOD_eqc, proper).

-import(lists, [duplicate/2,foldl/3]).

compile() ->
    %% {weight, {yes_multi_field_init, 0}}
    Opts = [{weight, {yes_multi_field_init, 0}},
            {resize,true}],
    ?FORALL(Abstr, proper_erlang_abstract_code:module(Opts),
            compile(Abstr)).

compile(Forms) ->
    compile(Forms, compiler_variants()).

compile(Forms, [Opts|OptsL]) ->
    case spawn_compile(Forms, [return, binary | Opts]) of
        {ok,_Mod,Bin,_EsWs} when is_binary(Bin) ->
            %% Uncomment the following lines to print
            %% the generated source code.
            %% io:format("<S>\n~ts\n</S>\n",
            %%           [[erl_pp:form(F) || F <- Forms]]),

            %% Uncomment the following line to print the
            %% generated abstract code.
            %% io:format("<abstr>\n~p\n</abstr>\n", [Forms]),
            compile(Forms, OptsL);
        Err ->
            io:format("compile: ~p\n", [Err]),
            io:format("with options ~p\n", [Opts]),
            io:format("<S>\n~ts\n</S>\n",
                      [[erl_pp:form(F) || F <- Forms]]),
            false
    end;
compile(_Forms, []) ->
    true.

spawn_compile(Forms, Options) ->
    {Pid,Ref} = spawn_monitor(fun() ->
                                      exit(compile:noenv_forms(Forms, Options))
                              end),
    receive
        {'DOWN',Ref,process,Pid,Ret} ->
            Ret
    after 600_000 ->
            timeout
    end.

compiler_variants() ->
    [
     [ssalint,clint0,clint],
     [r24,ssalint],
     [r25,ssalint],
     [r26,ssalint],
     [no_type_opt,ssalint],
     [no_module_opt,ssalint],
     [no_copt,ssalint,clint0],
     [no_copt,no_bool_opt,no_share_opt,no_bsm_opt,no_fun_opt,
      no_ssa_opt,no_recv_opt,no_postopt,ssalint,clint0],
     [no_bool_opt,no_share_opt,no_bsm_opt,no_fun_opt,no_ssa_opt,
      no_recv_opt,ssalint,clint0,clint],
     [no_copt,no_bool_opt,no_share_opt,no_bsm_opt,no_fun_opt,
      no_ssa_opt,no_recv_opt,ssalint,clint0]
    ].

-endif.
