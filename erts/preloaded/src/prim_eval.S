%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
{module, prim_eval}.

%% This module uses low-level BEAM instructions for the message queue facility
%% to allow erl_eval to evaluate receive expressions correctly.

{exports, [{'receive',2},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 10}.


{function, 'receive', 2, 2}.
  {label,1}.
    {func_info,{atom,prim_eval},{atom,'receive'},2}.
  {label,2}.
    {allocate,2,2}.
    {move,{x,1},{y,0}}.
    {move,{x,0},{y,1}}.
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


{function, module_info, 0, 8}.
  {label,6}.
    {func_info,{atom,prim_eval},{atom,module_info},0}.
  {label,7}.
    {move,{atom,prim_eval},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 10}.
  {label,8}.
    {func_info,{atom,prim_eval},{atom,module_info},1}.
  {label,9}.
    {move,{x,0},{x,1}}.
    {move,{atom,prim_eval},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
