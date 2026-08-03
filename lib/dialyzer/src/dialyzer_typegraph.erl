%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) Meta Platforms, Inc. and affiliates.
%% Copyright Ericsson AB 2022-2025. All Rights Reserved.
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
-module(dialyzer_typegraph).
-moduledoc false.

-export([module_type_deps/3]).

-export_type([type_mod_deps/0]).

-include("dialyzer.hrl").

%% Maps a module to those modules that depend on it
-type type_mod_deps() :: #{module() => [module()]}.

%% We track type dependecies so that we know which modules we ultimately
%% depend upon for type definitions which, in turn, affect the checking
%% of a module.

%% Any non-local types that are used in a spec (aka contract) / callback, any
%% locally-defined types that may depend on a non-local type, and any
%% implementations of a behaviour, introduce type-level dependencies on other
%% modules such that if the definition of that other module were to change,
%% this module would need to be checked again to account for those changes.

-spec module_type_deps(UseContracts :: boolean(), dialyzer_codeserver:codeserver(), [module()]) -> type_mod_deps().

%% The module type deps of a module are modules that depend on the module to
%% define types, contracts, callbacks or behaviours
module_type_deps(UseContracts, CodeServer, Modules) ->

  Contracts =
    case UseContracts of
      true -> maps:from_list(dict:to_list(dialyzer_codeserver:get_contracts(CodeServer)));
      false -> #{}
    end,
  Callbacks = maps:from_list(dialyzer_codeserver:get_callbacks(CodeServer)),
  TypeDefinitions =
    #{M => dialyzer_codeserver:lookup_mod_records(M, CodeServer) ||
      M <- Modules},
  Behaviours =
    #{M => get_behaviours_for_module(M, CodeServer) || M <- Modules},
  collect_module_type_deps(Contracts, Callbacks, TypeDefinitions, Behaviours).

-spec get_behaviours_for_module(module(), dialyzer_codeserver:codeserver()) -> [module()].

get_behaviours_for_module(M, CodeServer) ->
  ModCode = dialyzer_codeserver:lookup_mod_code(M, CodeServer),
  Attrs = cerl:module_attrs(ModCode),
  {Behaviours, _BehaviourLocations} = dialyzer_behaviours:get_behaviours(Attrs),
  Behaviours.

-spec collect_module_type_deps(Specs, Callbacks, TypeDefinitions, Behaviours) -> type_mod_deps() when
    Specs :: #{mfa() => dialyzer_contracts:file_contract()},
    Callbacks :: #{mfa() => dialyzer_contracts:file_contract()},
    TypeDefinitions :: #{module() => erl_types:type_table()},
    Behaviours :: #{module() => [module()]}.

collect_module_type_deps(Specs, Callbacks, TypeDefinitions, Behaviours) ->

  Contracts =
    [{M, Spec} || {M, _F, _A} := {_FileLine, Spec, _Extra} <- Specs] ++
    [{M, Callback} || {M, _F, _A} := {_FileLine, Callback, _Extra} <- Callbacks],

  ModulesMentionedInTypeDefinitions =
    [{FromTypeDefM, erl_types:module_type_deps_of_type_defs(TypeTable)}
     || FromTypeDefM := TypeTable <- TypeDefinitions],

  ModulesMentionedInContracts =
    [{FromContractM, module_type_deps_of_contract(C)}
      || {FromContractM, C} <- Contracts],

  ModulesMentionedAsBehaviours =
    maps:to_list(Behaviours),

  AllDepsRaw =
    ModulesMentionedInContracts ++
    ModulesMentionedInTypeDefinitions ++
    ModulesMentionedAsBehaviours,

  %% Find the union of module dependencies from all sources, removing
  %% self-references, and flipping the direction of the mapping to
  %% match the expectations of Dialyzer elsewhere,
  %% i.e., from:
  %%   module -> those modules it depends on
  %% to
  %%   module -> those modules that depend on it
  S0 = sofs:relation(AllDepsRaw, [{atom,[atom]}]),
  S1 = sofs:relation_to_family(S0),
  S2 = sofs:family_union(S1),
  S3 = sofs:family_to_relation(S2),
  S4 = sofs:converse(S3),
  S5 = sofs:strict_relation(S4),
  S6 = sofs:relation_to_family(S5),
  S7 = sofs:to_external(S6),
  ModuleToThoseModulesThatDependOnIt = maps:from_list(S7),

  ModuleToThoseModulesThatDependOnIt.

-spec module_type_deps_of_contract(#contract{}) -> [module()].

module_type_deps_of_contract(#contract{forms = Forms}) ->
  TypeForms = [TypeForm || {TypeForm, _Constraints} <- Forms],
  ConstraintForms =
    lists:append([Constraints || {_TypeForm, Constraints} <- Forms]),
  lists:usort(
    lists:append(
      erl_types:type_form_to_remote_modules(TypeForms),
      dialyzer_contracts:constraint_form_to_remote_modules(ConstraintForms))).
