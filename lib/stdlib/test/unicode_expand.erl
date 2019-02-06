%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(unicode_expand).

-export(['кlирилли́ческий атом'/0, 'кlирилли́ческий атом'/1,
        'кlирилли́ческий атомB'/1]).

-export_type(['кlирилли́ческий атом'/0]).

-type 'кlирилли́ческий атом'() :: integer().

'кlирилли́ческий атом'() ->
    'кlирилли́ческий атом'('кlирилли́ческий атом').

'кlирилли́ческий атом'(_Atom) ->
    ok.

'кlирилли́ческий атомB'(_B) ->
    true.
