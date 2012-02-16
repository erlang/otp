%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
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
-module(test_undecoded_rest).

-export([test/2]).

-include_lib("test_server/include/test_server.hrl").


%% testing OTP-5104

test(Opt, Config) ->
    {ok, Msg} = asn1ct:value('P-Record', 'PersonnelRecord',
                             [{i, ?config(case_dir, Config)}]),
    {ok, Bytes} = asn1_wrapper:encode('P-Record', 'PersonnelRecord', Msg),
    Bytes2 = if  is_list(Bytes) ->
                     Bytes ++ [55, 55, 55];
                 is_binary(Bytes) ->
                     iolist_to_binary([Bytes, <<55, 55, 55>>])
             end,
    case Opt of
        undec_rest ->
            {ok, Msg, R} = asn1_wrapper:decode('P-Record', 'PersonnelRecord',
                                               Bytes2),
            case R of
                <<55, 55, 55>> -> ok;
                [55, 55, 55] -> ok;
                BStr when is_bitstring(BStr) ->
                    PadLen = (8 - (bit_size(BStr) rem 8)) rem 8,
                    <<0, 55, 55, 55>> = <<0:PadLen, BStr/bitstring>>
            end;
        _ ->
            {ok, Msg} = asn1_wrapper:decode('P-Record', 'PersonnelRecord',
                                            Bytes2)
    end,
    ok.
