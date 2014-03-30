%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Among testing other things, this module shows why performing LCM on
%% SPARC is currently problematic. SPARC does not mark untagged values
%% as dead when they are live over function calls which in turn causes
%% them to be traced by the garbage collector leading to crashes.
%%
%% A simple way to get this behaviour is to compile just the function
%%
%%            {bsextract,tid_internal_storage,2}
%%
%% with the compiler option "rtl_lcm" on and without.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bs_extract).

-export([test/0]).

-include("bs_decode_extract.hrl").

-define(PDU, <<30,16,0,90,0,1,0,0,255,255,255,255,81,67,101,7,0,0,0,96,
	       6,12,146,18,14,0,15,252,16,0,0,17,0,0,128,0,2,241,33,131,
	       0,20,7,97,112,110,48,49,51,97,8,101,114,105,99,115,115,
	       111,110,2,115,101,132,0,20,128,192,35,16,1,5,0,16,5,117,
	       115,101,114,53,5,112,97,115,115,53,133,0,4,172,28,12,1,
	       133,0,4,172,28,12,3,134,0,8,145,148,113,129,0,0,0,0>>).

-define(RES, {ok, {mvsgT_imsi, <<81,67,101,7,0,0,0,240>>}}).

test() ->
  ?RES = extract_v0_opt(1000, ?PDU),
  ok.

extract_v0_opt(0, Pdu) ->
  get_external_id(Pdu);
extract_v0_opt(N, Pdu) ->
  {ok,_} = get_external_id(Pdu),
  extract_v0_opt(N-1, Pdu).

get_external_id(<<0:3,_:4,0:1,1:8,_Length:16,SequenceNumber:16,
		 _FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		 _TID:8/binary-unit:8,_InformationElements/binary>>) ->
  {echo,#sesT_echoReqV0{},SequenceNumber};
%% Create PDP Context Request
%% GTP97, SNN=0
%% (No SNDCP N-PDU number)
get_external_id(<<0:3,_:4,0:1,16:8,_Length:16,_SequenceNumber:16,
		 _FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		 TID:8/binary-unit:8,_InformationElements/binary>>) ->
  {ok,_IMSI} = extract_imsi(TID);
%%% Update PDP Context Request
%%% GTP97, SNN=0
%%% (No SNDCP N-PDU number)
get_external_id(<<0:3,_:4,0:1,18:8,_Length:16,_SequenceNumber:16,
		 _FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		 TID:8/binary-unit:8,_InformationElements/binary>>) ->
  {ok,_IMSI} = extract_imsi(TID);
%%% Delete PDP Context Request
%%% GTP97, SNN=0
%%% (No SNDCP N-PDU number)
get_external_id(<<0:3,_:4,0:1,20:8,_Length:16,_SequenceNumber:16,
		 _FlowLabel:16,_SNDCP_N_PDU_Number:8,_:3/binary-unit:8,
		 TID:8/binary-unit:8,_InformationElements/binary>>) ->
  {ok,_IMSI} = extract_imsi(TID);
%%% Error handling: GTP Message Too Short
%%% Error handling: Unknown GTP Signalling message.
%%% Error handling: Unexpected GTP Signalling message.
get_external_id(_GTP_Message) ->
  fault.

%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% extract_imsi/1
%% Get the IMSI element from TID
extract_imsi(TID) ->
  {ok,#mvsgT_tid{imsi=IMSI}} = tid_internal_storage(TID,[]),
  {ok,IMSI}.

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%% tid_internal_storage/3
%%% Convert TID binary to internal datatype
tid_internal_storage(Bin,_) ->
  Size = byte_size(Bin) - 1,
  <<Front:Size/binary,NSAPI:4,DigitN:4>> = Bin,
  Result =
    case DigitN of
      2#1111 ->
	#mvsgT_tid{imsi = #mvsgT_imsi{value=Front}, nsapi = NSAPI};
      _ ->
	Value = <<Front/binary,2#1111:4,DigitN:4>>,
	#mvsgT_tid{imsi = #mvsgT_imsi{value = Value}, nsapi = NSAPI}
    end,
  {ok,Result}.
