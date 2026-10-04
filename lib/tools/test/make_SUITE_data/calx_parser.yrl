%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

%% Original from: https://github.com/knutin/calx

Nonterminals
expr
arg_list
.


Terminals
operator
digit
'('
')'
.

Rootsymbol expr.

expr -> '(' operator arg_list ')' : {expr, value_of('$2'), '$3'}.

arg_list -> expr : ['$1'].
arg_list -> digit : [{digit, ?l2i(value_of('$1'))}].
arg_list -> digit arg_list : [{digit, ?l2i(value_of('$1'))}] ++ '$2'.


Erlang code.

-define(l2i(L), list_to_integer(L)).
value_of({_,_,V}) -> V.
