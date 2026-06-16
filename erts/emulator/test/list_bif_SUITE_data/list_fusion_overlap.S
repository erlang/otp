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

{module, list_fusion_overlap}.

{exports, [{list_fusion_overlap,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 8}.


{function, list_fusion_overlap, 0, 2}.
  {label,1}.
    {func_info,{atom,list_fusion_overlap},{atom,list_fusion_overlap},0}.
  {label,2}.
    {test_heap,4,0}.
    {put_list,{integer,2},nil,{x,2}}.
    {put_list,{integer,1},{x,2},{x,2}}.
    {test,is_nonempty_list,{f,3},[{x,2}]}.
    {get_list,{x,2},{x,3},{x,2}}.
    {test,is_eq_exact,{f,3},[{x,3},{integer,1}]}.
    {test,is_eq_exact,{f,3},[{x,2},{literal,[2]}]}.
    {move,{atom,ok},{x,0}}.
    return.
  {label,3}.
    {badmatch,{x,2}}.


{function, module_info, 0, 5}.
  {label,4}.
    {func_info,{atom,list_fusion_overlap},{atom,module_info},0}.
  {label,5}.
    {move,{atom,list_fusion_overlap},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 7}.
  {label,6}.
    {func_info,{atom,list_fusion_overlap},{atom,module_info},1}.
  {label,7}.
    {move,{x,0},{x,1}}.
    {move,{atom,list_fusion_overlap},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
