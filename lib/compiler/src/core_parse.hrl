%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2026. All Rights Reserved.
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
%% Purpose: Import Core Erlang native records.

-import_record(v3_core,
               [c_alias, c_apply,
                c_binary, c_bitstr,
                c_call, c_case, c_catch, c_clause, c_cons,
                c_fun,
                c_let, c_letrec, c_literal,
                c_map, c_map_pair,
                c_record, c_record_pair,
                c_module,
                c_opaque,
                c_primop,
                c_receive,
                c_seq,
                c_try, c_tuple,
                c_values, c_var]).
