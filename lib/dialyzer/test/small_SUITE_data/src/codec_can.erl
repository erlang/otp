%%---------------------------------------------------------------------
%% From: Peer Stritzinger
%% Date: 1 May 2011
%% Subject: Dialyzer v2.2.0 crash
%%
%% Binaries of the form <<_:N,_:_*M>> in specs resulted in a crash:
%%     dialyzer: Analysis failed with error: {{case_clause,8},
%%		 [{erl_types,t_form_to_string,1},
%%		  {erl_types,t_form_to_string,1},
%%		  {dialyzer_contracts,contract_to_string_1,1},
%%		  {dialyzer_contracts,extra_contract_warning,6},
%%		  {dialyzer_contracts,picky_contract_check,7},
%% because function erl_types:t_form_to_string/1 was not the inverse
%% of erl_types:t_to_string/2.
%%
%% Fixed on the same date and send to OTP for inclusion.
%%---------------------------------------------------------------------
-module(codec_can).

-export([recv/3, decode/1]).

-record(can_pkt, {id, data :: binary(), timestamp}).

-type can_pkt() :: #can_pkt{}.
-type channel() :: atom() | pid() | {atom(),_}.

-spec recv(<<_:64,_:_*8>>, fun((can_pkt()) -> R), channel()) -> R.
recv(Packet, Fun, Chan) ->
  #can_pkt{id = Can_id, data = Can_data} = P = decode(Packet),
  Fun(P).

-spec decode(<<_:64,_:_*8>>) -> #can_pkt{id::<<_:11>>,timestamp::char()}.
decode(<<_:12, Len:4, Timestamp:16, 0:3, Id:11/bitstring, 0:18,
	 Data:Len/binary, _/binary>>) ->
  #can_pkt{id = Id, data = Data, timestamp = Timestamp}.
