%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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
%%

-module(testExtensibilityImplied).
-export([main/0]).

main() ->
    M = 'ExtensibilityImplied',
    {'Seq2',true} = M:decode('Seq2', M:encode('Seq1', {'Seq1',true,42})),
    {'Set2',true} = M:decode('Set2', M:encode('Set1', {'Set1',true,42})),
    {asn1_enum,_} = M:decode('Enum2', M:encode('Enum1', ext)),
    ok.
