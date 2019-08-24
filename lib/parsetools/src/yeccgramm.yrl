%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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

%% This is the syntax (metagrammar) of grammar definitions of the yecc
%% parser generator.

Nonterminals
grammar declaration rule head symbol symbols strings attached_code
token tokens.

Terminals
atom float integer reserved_symbol reserved_word string char var
'->' ':' dot.

Rootsymbol grammar.

grammar -> declaration : '$1'.
grammar -> rule : '$1'.
declaration -> symbol symbols dot: {'$1', '$2'}.
declaration -> symbol strings dot: {'$1', '$2'}.
rule -> head '->' symbols attached_code dot: {rule, ['$1' | '$3'], '$4'}.
head -> symbol : '$1'.
symbols -> symbol : ['$1'].
symbols -> symbol symbols : ['$1' | '$2'].
strings -> string : ['$1'].
strings -> string strings : ['$1' | '$2'].
attached_code -> ':' tokens : {erlang_code, '$2'}.
attached_code -> '$empty' : {erlang_code,
                             [{atom, erl_anno:new(0), '$undefined'}]}.
tokens -> token : ['$1'].
tokens -> token tokens : ['$1' | '$2'].
symbol -> var : symbol('$1').
symbol -> atom : symbol('$1').
symbol -> integer : symbol('$1').
symbol -> reserved_word : symbol('$1').
token -> var : '$1'.
token -> atom : '$1'.
token -> float : '$1'.
token -> integer : '$1'.
token -> string : '$1'.
token -> char : '$1'.
token -> reserved_symbol : {value_of('$1'), anno_of('$1')}.
token -> reserved_word : {value_of('$1'), anno_of('$1')}.
token -> '->' : {'->', anno_of('$1')}. % Have to be treated in this
token -> ':' : {':', anno_of('$1')}.   % manner, because they are also
                                       % special symbols of the metagrammar

Erlang code.

-record(symbol, {anno, name}).

symbol(Symbol) ->
    #symbol{anno = anno_of(Symbol), name = value_of(Symbol)}.

value_of(Token) ->
    element(3, Token).

anno_of(Token) ->
    element(2, Token).
