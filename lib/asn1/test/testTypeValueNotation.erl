%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
-module(testTypeValueNotation).

-export([main/2]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq', {octstr, int, bool, enum, bitstr, null, oid, vstr}).

main(Rule, Option) ->
    Value1 = #'Seq'{octstr = [1, 2, 3, 4],
                    int = 12,
                    bool = true,
                    enum = a,
                    bitstr = [1, 0, 1, 0],
                    null = 'NULL',
                    oid = {1, 2, 55},
                    vstr = "Hello World"},
    Value2 = #'Seq'{octstr = {'OctStr', [1, 2, 3, 4]},
                    int = {'Int', 12},
                    bool = {'Bool', true},
                    enum = {'Enum', a},
                    bitstr = {'BitStr', [1, 0, 1, 0]},
                    null = {'Null', 'NULL'},
                    oid = {'OId', {1, 2, 55}},
                    vstr = {'VStr', "Hello World"}},
    main(Rule, Option, Value1, Value2).

%% Value2 will fail for ber_bin_v2, per_bin with nifs (optimize) and uper_bin
main(ber_bin_v2, _,          Value1, Value2) -> encode_fail(Value1, Value2);
main(per_bin,    [optimize], Value1, Value2) -> encode_fail(Value1, Value2);
main(uper_bin,   [],         Value1, Value2) -> encode_fail(Value1, Value2);
main(_,          _,          Value1, Value2) -> encode_normal(Value1, Value2).

encode_normal(Value1, Value2) ->
    {ok, Bytes}      = asn1_wrapper:encode('SeqTypeRefPrim', 'Seq', Value1),
    {ok, Bytes}      = asn1_wrapper:encode('SeqTypeRefPrim', 'Seq', Value2),
    {ok, Value1}     = asn1_wrapper:decode('SeqTypeRefPrim', 'Seq', Bytes).

encode_fail(Value1, Value2) ->
    {ok, Bytes}      = asn1_wrapper:encode('SeqTypeRefPrim', 'Seq', Value1),
    {error, _Reason} = asn1_wrapper:encode('SeqTypeRefPrim', 'Seq', Value2),
    {ok, Value1}     = asn1_wrapper:decode('SeqTypeRefPrim', 'Seq', Bytes).
