%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Basic Block Module
%%
%% Exports:
%% ~~~~~~~~
%% mk_bb(Code) - construct a basic block.
%% code(BB) - returns the code.
%% code_update(BB, NewCode) - replace the code in a basic block.
%% last(BB) - returns the last instruction.
%% butlast(BB) - returns the code with the last instruction removed.
%%

-module(hipe_bb).

-export([mk_bb/1,
	 code/1,
	 code_update/2,
	 is_bb/1,
	 last/1,
	 butlast/1]).

-include("hipe_bb.hrl").

%%
%% Constructs a basic block.
%% Returns a basic block: {bb, Code}
%%   * Code is a list of instructions

-spec mk_bb([_]) -> bb().

mk_bb(Code) ->
    #bb{code=Code}.

-spec is_bb(_) -> boolean().

is_bb(#bb{}) -> true;
is_bb(_) -> false.

-spec code_update(bb(), [_]) -> bb().

code_update(BB, Code) ->
    BB#bb{code = Code}.

-spec code(bb()) -> [_].

code(#bb{code = Code}) -> 
    Code.

-spec last(bb()) -> _.

last(#bb{code = Code}) -> 
    lists:last(Code).

-spec butlast(bb()) -> [_].

butlast(#bb{code = Code}) ->
    butlast_1(Code).

butlast_1([X|Xs]) -> butlast_1(Xs,X).

butlast_1([X|Xs],Y) -> [Y|butlast_1(Xs,X)];
butlast_1([],_) -> [].
