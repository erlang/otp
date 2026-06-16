%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

{module, guard_bif_arg_vector}.

{exports, [{guard_bif_arg_vector,0},
           {bench,2},
           {module_info,0},
           {module_info,1}]}.

{attributes, []}.

{labels, 17}.


{function, guard_bif_arg_vector, 0, 2}.
  {label,1}.
    {func_info,{atom,guard_bif_arg_vector},{atom,guard_bif_arg_vector},0}.
  {label,2}.
    {move,{literal,<<>>},{x,0}}.
    {move,{integer,0},{x,1}}.
    {move,{integer,0},{x,2}}.
    {call_only,3,{f,6}}.


{function, bench, 2, 4}.
  {label,3}.
    {func_info,{atom,guard_bif_arg_vector},{atom,bench},2}.
  {label,4}.
    {move,{integer,0},{x,2}}.
    {move,{integer,0},{x,3}}.
    {move,{integer,0},{x,4}}.
    {call_only,5,{f,9}}.


{function, guard, 3, 6}.
  {label,5}.
    {func_info,{atom,guard_bif_arg_vector},{atom,guard},3}.
  {label,6}.
    {allocate,3,3}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {gc_bif,binary_part,{f,7},3,[{y,0},{y,1},{y,2}],{x,0}}.
    {test,is_eq_exact,{f,7},[{tr,{x,0},{t_bitstring,8,false}},{literal,<<>>}]}.
    {move,{atom,true},{x,0}}.
    {deallocate,3}.
    return.
  {label,7}.
    {move,{atom,false},{x,0}}.
    {deallocate,3}.
    return.


{function, bench_loop, 5, 9}.
  {label,8}.
    {func_info,{atom,guard_bif_arg_vector},{atom,bench_loop},5}.
  {label,9}.
    {test,is_eq_exact,{f,10},[{x,0},{integer,0}]}.
    {move,{x,4},{x,0}}.
    return.
  {label,10}.
    {allocate,5,5}.
    {move,{x,0},{y,0}}.
    {move,{x,1},{y,1}}.
    {move,{x,2},{y,2}}.
    {move,{x,3},{y,3}}.
    {move,{x,4},{y,4}}.
    {gc_bif,binary_part,{f,11},5,[{y,1},{y,2},{y,3}],{x,0}}.
    {test,is_eq_exact,{f,11},[{tr,{x,0},{t_bitstring,8,false}},{literal,<<>>}]}.
    {gc_bif,'+',{f,0},5,[{y,4},{integer,1}],{x,4}}.
    {jump,{f,12}}.
  {label,11}.
    {move,{y,4},{x,4}}.
  {label,12}.
    {gc_bif,'-',{f,0},5,[{y,0},{integer,1}],{x,0}}.
    {move,{y,1},{x,1}}.
    {move,{y,2},{x,2}}.
    {move,{y,3},{x,3}}.
    {deallocate,5}.
    {call_only,5,{f,9}}.


{function, module_info, 0, 14}.
  {label,13}.
    {func_info,{atom,guard_bif_arg_vector},{atom,module_info},0}.
  {label,14}.
    {move,{atom,guard_bif_arg_vector},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 16}.
  {label,15}.
    {func_info,{atom,guard_bif_arg_vector},{atom,module_info},1}.
  {label,16}.
    {move,{x,0},{x,1}}.
    {move,{atom,guard_bif_arg_vector},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
