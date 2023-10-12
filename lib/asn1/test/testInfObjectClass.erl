%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2021. All Rights Reserved.
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

-module(testInfObjectClass).


-export([main/1]).

-include_lib("common_test/include/ct.hrl").

main(Rule) ->
    %% this test is added for OTP-4591, to test that elements in decoded
    %% value has terms in right order.
    Val = case Rule of
              %% table constraints are not JER visible
              %% The binaries here are Open Types which in this case
              %% are JSON encodings of INTEGER
              jer -> {'Seq',<<"12">>,<<"13">>,2}; 
              _ -> {'Seq',12,13,2}
          end,
    roundtrip('Seq', Val),
    case Rule of
        jer -> ok; % table constraints are not JER visible
        _ ->
            %% OTP-5783
            {'Type not compatible with table constraint',
             {component,'ArgumentType'},
             {value,_},_} = enc_error('Seq', {'Seq',12,13,1}),
            Bytes2 = case Rule of
                         ber ->
                             <<48,9,2,1,12,2,1,11,2,1,1>>;
                         _ ->
                             <<1,12,1,11,1,1>>
                     end,
            {'Type not compatible with table constraint',
             {{component,_},
              {value,_B},_}} = dec_error('Seq', Bytes2),
            ok
    end.

roundtrip(T, V) ->
    asn1_test_lib:roundtrip('InfClass', T, V).

enc_error(T, V) ->
    {error,{asn1,{Reason,Stk}}} = 'InfClass':encode(T, V),
    [{_,_,_,_}|_] = Stk,
    Reason.

dec_error(T, Bin) ->
    {error,{asn1,{Reason,Stk}}} = 'InfClass':decode(T, Bin),
    [{_,_,_,_}|_] = Stk,
    Reason.
