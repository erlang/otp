%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2024. All Rights Reserved.
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
-moduledoc false.

-export([module/2]).

-include("core_parse.hrl").
-include("beam_ssa.hrl").
-include("beam_disasm.hrl").

-import(lists, [foldl/3, foreach/2]).

-type code() :: cerl:c_module()
              | beam_utils:module_code()
              | [_].                            %form-based format

-spec module(file:io_device(), code()) -> 'ok'.

module(File, #c_module{}=Core) ->
    %% This is a Core Erlang module.
    io:put_chars(File, core_pp:format(Core));
module(File, #b_module{name=Mod,exports=Exp,attributes=Attr,body=Fs}) ->
    %% This an SSA module.
    io:format(File, "module ~p.\n", [Mod]),
    io:format(File, "exports ~p.\n", [Exp]),
    io:format(File, "attributes ~kp.\n\n", [Attr]),
    PP = [beam_ssa_pp:format_function(F) || F <- Fs],
    io:put_chars(File, lists:join($\n, PP));
module(Stream, {Mod,Exp,Attr,Code,NumLabels}) ->
    %% This is BEAM code.
    io:format(Stream, "{module, ~kp}.  %% version = ~w\n",
	      [Mod, beam_opcodes:format_number()]),
    io:format(Stream, "\n{exports, ~p}.\n", [Exp]),
    io:format(Stream, "\n{attributes, ~kp}.\n", [Attr]),
    io:format(Stream, "\n{labels, ~p}.\n", [NumLabels]),
    Lbl2Fun = foldl(fun({function,Name,Arity,Entry,_}, Map) ->
                            Map#{ Entry => {Name,Arity} }
                    end, #{}, Code),
    foreach(
      fun ({function,Name,Arity,Entry,Asm}) ->
	      io:format(Stream, "\n\n{function, ~w, ~w, ~w}.\n",
			[Name, Arity, Entry]),
	      io:put_chars(Stream, format_asm(Asm, Lbl2Fun))
      end, Code);
module(Stream, [_|_]=Fs) ->
    %% Form-based abstract format.
    foreach(fun (F) -> io:format(Stream, "~kp.\n", [F]) end, Fs).

format_asm([{label,L}|Is], Lbl2Fun) ->
    [io_lib:format("  {label,~p}.\n", [L])|format_asm(Is, Lbl2Fun)];
format_asm([I={Call,_,L}|Is], Lbl2Fun) when Call =:= call; Call =:= call_only ->
    format_asm_call(L, I, Is, Lbl2Fun);
format_asm([I={call_last,_,L,_}|Is], Lbl2Fun) ->
    format_asm_call(L, I, Is, Lbl2Fun);
format_asm([I|Is], Lbl2Fun) ->
    [io_lib:format("    ~kp", [I]),".\n"|format_asm(Is, Lbl2Fun)];
format_asm([], _) -> [].

format_asm_call({f,L}, I, Is, Lbl2Fun) ->
    {N,A} = map_get(L, Lbl2Fun),
    [io_lib:format("    ~p. % ~p/~p\n", [I, N, A])|format_asm(Is, Lbl2Fun)].
