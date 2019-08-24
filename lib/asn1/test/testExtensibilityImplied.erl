%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

-module(testExtensibilityImplied).
-export([main/0]).

main() ->
    M = 'ExtensibilityImplied',
    {'Seq2',true} = M:decode('Seq2', M:encode('Seq1', {'Seq1',true,42})),
    {'Set2',true} = M:decode('Set2', M:encode('Set1', {'Set1',true,42})),
    {asn1_enum,_} = M:decode('Enum2', M:encode('Enum1', ext)),
    ok.
