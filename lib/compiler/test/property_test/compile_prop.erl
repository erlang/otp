%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

%% This module only supports proper, it requires
%% the  'proper_erlang_abstract_code' functionality

-include_lib("common_test/include/ct_property_test.hrl").

-define(BEAM_TYPES_INTERNAL, true).
-include_lib("compiler/src/beam_types.hrl").

-import(lists, [duplicate/2,foldl/3]).

-ifdef(PROPER).

prop_compile() ->
    %% {weight, {yes_multi_field_init, 0}}
    Opts = [{weight, {yes_multi_field_init, 0}},
            {resize,true}],
    ?FORALL(Abstr, proper_erlang_abstract_code:module(Opts),
            compile(noenv_forms, Abstr, compiler_variants())).

-else.
-ifdef(EQC).

prop_compile() ->
    Opts = [{macros, false}],
    ?FORALL(String, eqc_erlang_program:module(eqc_generated_module, Opts),
            begin
                ok = file:write_file("eqc_generated_module.erl", String),
                compile(noenv_file, "eqc_generated_module.erl", compiler_variants())
            end).

-endif.
-endif.


compile(Function, FileOrForms, [Opts|OptsL]) ->
    AllOpts = [report_errors, return, binary, {feature,compr_assign,enable} | Opts],
    case spawn_compile(Function, FileOrForms, AllOpts) of
        {ok,_Mod,Bin,_EsWs} when is_binary(Bin) ->
            %% Uncomment the following lines to print
            %% the generated source code.
            %% io:format("<S>\n~ts\n</S>\n",
            %%           [[erl_pp:form(F) || F <- Forms]]),

            %% Uncomment the following line to print the
            %% generated abstract code.
            %% io:format("<abstr>\n~p\n</abstr>\n", [Forms]),
            compile(Function, FileOrForms, OptsL);
        Err ->
            io:format("compile: ~p\n", [Err]),
            io:format("with options ~p\n", [Opts]),
            case Function of
                noenv_file ->
                    {ok, Str} = file:read_file(FileOrForms),
                    io:format("~s",[Str]);
                noenv_forms ->
                    io:format("<S>\n~ts\n</S>\n",
                              [[erl_pp:form(F) || F <- FileOrForms]])
            end,
            false
    end;
compile(_, _FileOrForms, []) ->
    true.

spawn_compile(Compile, Forms, Options) ->
    {Pid,Ref} = spawn_monitor(fun() ->
                                      exit(compile:Compile(Forms, Options))
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
