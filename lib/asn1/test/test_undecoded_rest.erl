%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(test_undecoded_rest).

-export([test/2]).

-include_lib("common_test/include/ct.hrl").


%% testing OTP-5104

test(Opts, Config) ->
    {ok,Msg} = asn1ct:value('P-Record', 'PersonnelRecord',
			    [{i,proplists:get_value(case_dir, Config)}]),
    Bytes0 = encode(Opts, 'PersonnelRecord', Msg),
    Bytes1 = iolist_to_binary([Bytes0, <<55,55,55>>]),
    case proplists:get_bool(undec_rest, Opts) of
	true ->
            {Msg,R} = decode(Opts, 'PersonnelRecord', Bytes1),
            case R of
                <<55,55,55>> ->
		    ok;
                BStr when is_bitstring(BStr) ->
                    PadLen = (8 - (bit_size(BStr) rem 8)) rem 8,
                    <<0,55,55,55>> = <<0:PadLen, BStr/bitstring>>
            end;
        false ->
            Msg = decode(Opts, 'PersonnelRecord', Bytes1)
    end,
    ok.

encode(Opts, T, V) ->
    M = 'P-Record',
    case proplists:get_bool(no_ok_wrapper, Opts) of
	false ->
	    {ok,Enc} = M:encode(T, V),
	    Enc;
	true ->
	    Enc = M:encode(T, V),
	    true = is_binary(Enc),
	    Enc
    end.

decode(Opts, T, E) ->
    M = 'P-Record',
    case {proplists:get_bool(no_ok_wrapper, Opts),M:decode(T, E)} of
	{false,{ok,Val}} -> Val;
	{false,{ok,Val,Rest}} -> {Val,Rest};
	{true,Result} -> Result
    end.
