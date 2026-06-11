%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2026. All Rights Reserved.
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

%%
%% Compiled from get_two_tuple_elements.erl like this:
%%
%%     erlc +no_ssa_opt_sink get_two_tuple_elements.erl
%%
%% We have kept this .S file to ensure that the test continues to
%% be effective even if the compiler's code optimizations or code
%% generation will change in the future.
%%
%% In particular, we want to ensure that the following instruction
%% sequence in xx/2 is tested:
%%
%%     {get_tuple_element,{x,1},0,{x,0}}.
%%     {get_tuple_element,{x,1},1,{x,1}}.
%%     {get_tuple_element,{x,1},2,{x,1}}.
%%

{module, get_two_tuple_elements}.  %% version = 0

{exports, [{get_two_tuple_elements,0},{module_info,0},{module_info,1}]}.

{attributes, []}.

{labels, 14}.


{function, get_two_tuple_elements, 0, 2}.
  {label,1}.
    {line,[{location,"erts/emulator/test/tuple_SUITE_data/get_two_tuple_elements.erl",
                     26}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,get_two_tuple_elements},0}.
  {label,2}.
    {allocate,2,0}.
    {init_yregs,{list,[{y,0},{y,1}]}}.
    {move,{atom,x},{x,0}}.
    {line,[{location,"erts/emulator/test/tuple_SUITE_data/get_two_tuple_elements.erl",
                     27}]}.
    {call,1,{f,9}}. % id/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    {move,{x,0},{y,1}}.
    {move,{integer,1},{x,0}}.
    {call,1,{f,9}}. % id/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    {move,{x,0},{y,0}}.
    {move,{integer,2},{x,0}}.
    {call,1,{f,9}}. % id/1
    {'%',
        {var_info,
            {x,0},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    {test_heap,7,1}.
    {put_tuple2,{x,0},{list,[{atom,x},{atom,y},{x,0}]}}.
    {put_tuple2,{x,1},{list,[{y,0},{x,0}]}}.
    {move,{y,1},{x,0}}.
    {trim,2,0}.
    {call,2,{f,5}}. % xx/2
    {'%',{var_info,{x,0},[{type,#beam_types:t_integer{elements = {2,4}}}]}}.
    {test,is_eq_exact,
          {f,3},
          [{tr,{x,0},#beam_types:t_integer{elements = {2,4}}},{integer,3}]}.
    {move,{atom,ok},{x,0}}.
    {deallocate,0}.
    return.
  {label,3}.
    {badmatch,{x,0}}.


{function, xx, 2, 5}.
  {label,4}.
    {line,[{location,"erts/emulator/test/tuple_SUITE_data/get_two_tuple_elements.erl",
                     30}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,xx},2}.
  {label,5}.
    {'%',
        {var_info,
            {x,0},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    {'%',
     {var_info,
      {x,1},
      [{type,
        #beam_types:t_tuple{
         size = 2,exact = true,
         elements =
          #{1 =>
             #beam_types:t_union{
              atom = #beam_types:t_atom{elements = [x]},
              list = none,
              number = #beam_types:t_integer{elements = {1,2}},
              tuple_set = none,native_record_set = none,other = none},
            2 =>
             #beam_types:t_tuple{
              size = 3,exact = true,
              elements =
               #{1 => #beam_types:t_atom{elements = [x]},
                 2 => #beam_types:t_atom{elements = [y]},
                 3 =>
                  #beam_types:t_union{
                   atom = #beam_types:t_atom{elements = [x]},
                   list = none,
                   number = #beam_types:t_integer{elements = {1,2}},
                   tuple_set = none,native_record_set = none,
                   other = none}}}}}}]}}.
    {get_tuple_element,{x,1},0,{x,0}}.
    {get_tuple_element,{x,1},1,{x,1}}.
    {get_tuple_element,{x,1},2,{x,1}}.
    {move,nil,{x,2}}.
    {call_only,3,{f,7}}. % yy/3


{function, yy, 3, 7}.
  {label,6}.
    {line,[{location,"erts/emulator/test/tuple_SUITE_data/get_two_tuple_elements.erl",
                     39}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,yy},3}.
  {label,7}.
    {'%',
        {var_info,
            {x,0},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    {'%',
        {var_info,
            {x,1},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    {'%',{var_info,{x,2},[{type,nil}]}}.
    {line,[{location,"erts/emulator/test/tuple_SUITE_data/get_two_tuple_elements.erl",
                     40}]}.
    {gc_bif,'+',
        {f,0},
        2,
        [{tr,{x,0},
             #beam_types:t_union{
                 atom = #beam_types:t_atom{elements = [x]},
                 list = none,
                 number = #beam_types:t_integer{elements = {1,2}},
                 tuple_set = none,native_record_set = none,other = none}},
         {tr,{x,1},
             #beam_types:t_union{
                 atom = #beam_types:t_atom{elements = [x]},
                 list = none,
                 number = #beam_types:t_integer{elements = {1,2}},
                 tuple_set = none,native_record_set = none,other = none}}],
        {x,0}}.
    return.


{function, id, 1, 9}.
  {label,8}.
    {line,[{location,"erts/emulator/test/tuple_SUITE_data/get_two_tuple_elements.erl",
                     42}]}.
    {func_info,{atom,get_two_tuple_elements},{atom,id},1}.
  {label,9}.
    {'%',
        {var_info,
            {x,0},
            [{type,
                 #beam_types:t_union{
                     atom = #beam_types:t_atom{elements = [x]},
                     list = none,
                     number = #beam_types:t_integer{elements = {1,2}},
                     tuple_set = none,native_record_set = none,
                     other = none}}]}}.
    return.


{function, module_info, 0, 11}.
  {label,10}.
    {line,[]}.
    {func_info,{atom,get_two_tuple_elements},{atom,module_info},0}.
  {label,11}.
    {move,{atom,get_two_tuple_elements},{x,0}}.
    {call_ext_only,1,{extfunc,erlang,get_module_info,1}}.


{function, module_info, 1, 13}.
  {label,12}.
    {line,[]}.
    {func_info,{atom,get_two_tuple_elements},{atom,module_info},1}.
  {label,13}.
    {move,{x,0},{x,1}}.
    {move,{atom,get_two_tuple_elements},{x,0}}.
    {call_ext_only,2,{extfunc,erlang,get_module_info,2}}.
