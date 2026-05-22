%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025-2026. All Rights Reserved.
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

-module(ext_records).

-export([local/2]).

-export_record([vector, b_blk, b_set]).
-record #vector{x=10, y=1, z=5}.

-export_record([c_alias, c_apply,
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


%% Local records.
-record #local{x, y}.
-record #foreign{x, y}.

local(X, Y) ->
    #local{x=X, y=Y}.


%% Duplicates of compiler records.

-record #b_blk{anno=#{}, is, last}.
-record #b_set{anno=#{}, dst=none, op, args=[]}.

-record #c_alias{anno=[]}.
-record #c_apply{anno=[]}.
-record #c_binary{anno=[]}.
-record #c_bitstr{anno=[]}.
-record #c_call{anno=[]}.
-record #c_case{anno=[]}.
-record #c_catch{anno=[]}.
-record #c_clause{anno=[]}.
-record #c_cons{anno=[]}.
-record #c_fun{anno=[]}.
-record #c_let{anno=[]}.
-record #c_letrec{anno=[]}.
-record #c_literal{anno=[]}.
-record #c_map{anno=[]}.
-record #c_map_pair{anno=[]}.
-record #c_record{anno=[]}.
-record #c_record_pair{anno=[]}.
-record #c_module{anno=[]}.
-record #c_opaque{anno=[]}.
-record #c_primop{anno=[]}.
-record #c_receive{anno=[]}.
-record #c_seq{anno=[]}.
-record #c_try{anno=[]}.
-record #c_tuple{anno=[]}.
-record #c_values{anno=[]}.
-record #c_var{anno=[], name}.
