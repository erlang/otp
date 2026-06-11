%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2026. All Rights Reserved.
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

%% Type version, must be bumped whenever the external type format changes.
-define(BEAM_TYPES_VERSION, 4).

-if(?MODULE =/= beam_types).
-import_record(beam_types,
               [t_atom, t_bitstring, t_bs_context, t_bs_matchable, t_cons,
                t_float, t_fun, t_integer, t_list, t_map, t_number,
                t_record, t_tuple, t_union]).
-endif.

-define(ATOM_SET_SIZE, 5).

%% Documented limits.
-define(MAX_FUNC_ARGS, 255).
-define(MAX_TUPLE_SIZE, (1 bsl 24) - 1).
-define(TUPLE_ELEMENT_LIMIT, 12).

-ifdef(BEAM_TYPES_INTERNAL).
%% Internal constants used by beam_types.erl and its whitebox tests
-define(TUPLE_SET_LIMIT, 12).
-define(MAX_TYPE_DEPTH, 4).
-endif.
