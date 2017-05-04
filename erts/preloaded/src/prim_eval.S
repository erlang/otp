%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2017. All Rights Reserved.
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
{module, prim_eval}.

%% This module uses low-level BEAM instructions for the message queue facility
%% to allow erl_eval to evaluate receive expressions correctly.

{exports, [{'receive',2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 14}.


{function, 'receive', 2, 2}.
  {label,1}.
    {func_info,{atom,prim_eval},{atom,'receive'},2}.
  {label,2}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
    %% Call arg_reg_alloc() in order to ensure
    %% that def_arg_reg[0] isn't clobbered
    {call,0,{f,7}}.
  {label,3}.
    {loop_rec,{f,5},{x,0}}.
    {move,{y,1},{x,1}}.
    {call_fun,1}.
    {test,is_ne_exact,{f,4},[{x,0},{atom,nomatch}]}.
    remove_message.
    {deallocate,2}.
    return.
  {label,4}.
    {loop_rec_end,{f,3}}.
  {label,5}.
    {wait_timeout,{f,3},{y,0}}.
    timeout.
    {move,{atom,timeout},{x,0}}.
    {deallocate,2}.
    return.

{function, arg_reg_alloc, 0, 7}.
  {label,6}.
    {func_info,{atom,prim_eval},{atom,arg_reg_alloc},0}.
  {label,7}.
    {allocate,0,0}.
    {move,{integer,134217727},{x,0}}.
    {call_ext,1,{extfunc,erlang,bump_reductions,1}}.
    {move,{atom,true},{x,3}}.
    {move,{atom,true},{x,4}}.
    {move,{atom,true},{x,2}}.
    {move,{atom,true},{x,5}}.
    {move,{atom,true},{x,1}}.
    {move,{atom,true},{x,6}}.
    {move,{atom,true},{x,0}}.
    {call_last,7,{f,9},0}.


{function, arg_reg_alloc, 7, 9}.
  {label,8}.
    {func_info,{atom,prim_eval},{atom,arg_reg_alloc},7}.
  {label,9}.
    {move,{atom,ok},{x,0}}.
    return.


{function, module_info, 0, 11}.
  {label,10}.
    {func_info,{atom,prim_eval},{atom,module_info},0}.
  {label,11}.
    {move,{atom,prim_eval},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 13}.
  {label,12}.
    {func_info,{atom,prim_eval},{atom,module_info},1}.
  {label,13}.
    {move,{x,0},{x,1}}.
    {move,{atom,prim_eval},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
