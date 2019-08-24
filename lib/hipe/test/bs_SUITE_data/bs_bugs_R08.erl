%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% When executing this in R8 (and compiled with R8) the result was
%% {ok,[148,129,0,0]} but should be {ok,[145,148,113,129,0,0,0,0]}
%% Thanks to Kenneth Lundin for sending this to us.
%%-------------------------------------------------------------------

-module(bs_bugs_R08).

-export([test/0]).

test() ->
  List = [145,148,113,129,0,0,0,0],
  {ok, List} = msisdn_internal_storage(<<145,148,113,129,0,0,0,0>>, []),
  ok.

%% msisdn_internal_storage/3
%% Convert MSISDN binary to internal datatype (TBCD-octet list)

msisdn_internal_storage(<<>>, MSISDN) ->
  {ok, lists:reverse(MSISDN)};
msisdn_internal_storage(<<2#11111111:8,_Rest/binary>>, MSISDN) ->
  {ok, lists:reverse(MSISDN)};
msisdn_internal_storage(<<2#1111:4,DigitN:4,_Rest/binary>>, MSISDN) when
    DigitN < 10 ->
  {ok, lists:reverse([(DigitN bor 2#11110000)|MSISDN])};
msisdn_internal_storage(<<DigitNplus1:4,DigitN:4,Rest/binary>>, MSISDN) when
    DigitNplus1 < 10, DigitN < 10 ->
  NewMSISDN = [((DigitNplus1 bsl 4) bor DigitN)|MSISDN],
  msisdn_internal_storage(Rest, NewMSISDN);
msisdn_internal_storage(_Rest, _MSISDN) ->
  {fault}. %% Mandatory IE incorrect
