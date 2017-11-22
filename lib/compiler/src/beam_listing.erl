%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(beam_listing).

-export([module/2]).

-include("core_parse.hrl").
-include("v3_kernel.hrl").
-include("beam_disasm.hrl").

-import(lists, [foreach/2]).

-type code() :: cerl:c_module()
              | beam_utils:module_code()
              | #k_mdef{}
              | [_].                            %form-based format

-spec module(file:io_device(), code()) -> 'ok'.

module(File, #c_module{}=Core) ->
    %% This is a core module.
    io:put_chars(File, core_pp:format(Core));
module(File, #k_mdef{}=Kern) ->
    %% This is a kernel module.
    io:put_chars(File, v3_kernel_pp:format(Kern));
    %%io:put_chars(File, io_lib:format("~p~n", [Kern]));
module(Stream, {Mod,Exp,Attr,Code,NumLabels}) ->
    %% This is output from v3_codegen.
    io:format(Stream, "{module, ~p}.  %% version = ~w\n",
	      [Mod, beam_opcodes:format_number()]),
    io:format(Stream, "\n{exports, ~p}.\n", [Exp]),
    io:format(Stream, "\n{attributes, ~p}.\n", [Attr]),
    io:format(Stream, "\n{labels, ~p}.\n", [NumLabels]),
    foreach(
      fun ({function,Name,Arity,Entry,Asm}) ->
	      io:format(Stream, "\n\n{function, ~w, ~w, ~w}.\n",
			[Name, Arity, Entry]),
	      io:put_chars(Stream, format_asm(Asm))
      end, Code);
module(Stream, [_|_]=Fs) ->
    %% Form-based abstract format.
    foreach(fun (F) -> io:format(Stream, "~p.\n", [F]) end, Fs).

format_asm([{label,L}|Is]) ->
    ["  {label,",integer_to_list(L),"}.\n"|format_asm(Is)];
format_asm([I|Is]) ->
    [io_lib:format("    ~p", [I]),".\n"|format_asm(Is)];
format_asm([]) -> [].
