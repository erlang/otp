%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023-2024. All Rights Reserved.
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
-module(inet_dns_tsig).
-moduledoc false.

%% DNS TSIG handling
%%
%% RFC 8945: Secret Key Transaction Authentication for DNS (TSIG)

%% WARNING: in the spirit of inet_dns.erl, this module only handles the
%% cryptographic operations and does not absolve you of your responsibities
%% that include:
%%  * implement a truncation policy as per RFC8945, section 5.2
%%  * verifying that for TSIG over TCP the last message contains a TSIG RR
%%  * truncated messages must include the TSIG RR as per RFC8945, section 5.3
%%  * if running as a server, generate suitable error responses
%%    as per RFC8945, section 5.2

-export([init/1, sign/2, sign/3, verify/3]).

-include("inet_dns.hrl").

-define(FUDGE_DEFAULT, 300). % RFC8945, section 10
-define(MAC_SIZE_MIN, 10). % RFC8945, section 5.2.2.1
-define(TTL, 0).
-define(NOW, (os:system_time(seconds))). % RFC8945, section 5.2.3

-define(ALG_DEFAULT, sha256). % RFC8945, section 6
-define(ALGS_SUPPORTED, [md5, sha, sha224, ?ALG_DEFAULT, sha384, sha512]).
-type tsig_algs() :: md5 | sha | sha224 | ?ALG_DEFAULT | sha384 | sha512.
-type tsig_alg()  :: {tsig_algs(),?MAC_SIZE_MIN..65535}.

-type tsig_key()   :: {string(),iodata()}.
-type tsig_error() :: ?BADSIG | ?BADKEY | ?BADTIME | ?BADTRUNC.

-record(
   tsig_state,
   {key   = []                  :: tsig_key() | list(tsig_key()),
    alg   = ?ALG_DEFAULT        :: tsig_alg(), % client only
    fudge = ?FUDGE_DEFAULT      :: 0..65535,

    qr    = 0                   :: 0..100,
    %% 0 (false): request, 1+ (true): response
    %% this value is overloaded to handle TSIG
    %% over TCP where we need to accept up to
    %% 99 messages without TSIG so each time a
    %% response with a TSIG RR is received this
    %% value is reset to 2

    id                          :: undefined | 0..65535,
    mac   = {?MODULE,undefined} ::
      {?MODULE,undefined | binary()} | {crypto,crypto:mac_state()}
}).
-opaque tsig_state() :: #tsig_state{}.
-export_type([tsig_state/0]).

-spec init(proplists:proplist()) -> tsig_state().
init(Config) ->
    Key0 = proplists:get_value(keys, Config, []), % server passes in keys
    Key = proplists:get_value(key, Config, Key0), % client passes in key
    true = is_tuple(Key) orelse Key =/= [],
    Alg = case proplists:get_value(alg, Config, ?ALG_DEFAULT) of
        {A,S} when is_atom(A), is_integer(S) ->
            true = lists:member(A, ?ALGS_SUPPORTED)
                    andalso
                   S >= ?MAC_SIZE_MIN
                    andalso
                   S >= maps:get(size, crypto:hash_info(A)) div 2,
            {A,S};
        A when is_atom(A) ->
            true = lists:member(A, ?ALGS_SUPPORTED),
            {A,maps:get(size, crypto:hash_info(A))}
    end,
    Fudge = proplists:get_value(fudge, Config, ?FUDGE_DEFAULT),
    true = is_integer(Fudge) andalso Fudge >= 0 andalso Fudge =< 65535,
    #tsig_state{ key = Key, alg = Alg, fudge = Fudge }.


-spec sign(binary(), tsig_state()) -> {ok,binary(),tsig_state()}.
sign(Pkt, TS) ->
    sign(Pkt, TS, ?NOERROR).

-spec sign(binary(), tsig_state(), tsig_error() | ?NOERROR) ->
          {ok,binary(),tsig_state()}.
sign(
  Pkt = <<Hdr:10/binary, Rest/binary>>,
  TS  = #tsig_state{ id = TSId, qr = TSQR },
  Error) ->
    case Hdr of
        %% client uses this
        <<Id:16, QR:1, _:15, _/binary>>
          when QR == 0, TSId == undefined ->
            sign(Pkt, TS#tsig_state{ id = Id }, Error, Hdr, Rest);
        %%
        %% client and server use this
        <<_Id:16, QR:1, _:15, _/binary>>
          when QR == 0, TSQR == 0;
               QR == 1, TSQR >= 1 ->
            sign(Pkt, TS, Error, Hdr, Rest)
    end.

sign(Pkt, TS, Error, Hdr, <<ARCount:16, Content/binary>>) ->
    {Name,_Secret} = TS#tsig_state.key,
    {Alg,_MACSize} = TS#tsig_state.alg,
    Now = ?NOW,
    OtherData = otherdata(Error, Now),
    MAC = mac(Pkt, TS, Error, Now, OtherData),
    TSigRR =
        #dns_rr_tsig{
           domain = Name,
           algname = inet_dns:encode_algname(Alg),
           now = Now,
           fudge = TS#tsig_state.fudge,
           mac = MAC,
           original_id = TS#tsig_state.id,
           error = Error,
           other_data = OtherData},
    %% for simplicity, and to avoid export hell, we encode a skeleton packet and
    %% extract our TSIG RR directly from it
    <<_:12/binary, TSigRRBin/binary>> =
        inet_dns:encode(
          #dns_rec{ header = #dns_header{}, arlist = [TSigRR]}),
    TSQR = TS#tsig_state.qr,
    {ok,
     <<Hdr/binary, (ARCount + 1):16, Content/binary, TSigRRBin/binary>>,
     TS#tsig_state{ qr = TSQR + 1, mac = {?MODULE,MAC} }}.


%% this function needs to work with the original response binary as the MAC
%% calculation is affected by each DNS implementation having their own varying
%% name compression algorithm
-spec verify(binary(), #dns_rec{}, tsig_state()) ->
                 {ok,tsig_state()} |
                 {error,formerr | {notauth,badkey | badsig | badtime}}.
verify(Pkt, Response, TS) ->
    try do_verify(Pkt, Response, TS) of
        R ->
            R
    catch
        Reason ->
            {error,Reason}
    end.

%% server uses this
do_verify(Pkt,
          Response = #dns_rec{
                         header = #dns_header{ qr = QR },
                         arlist = ARList },
          TS0 = #tsig_state{ id = TSId })
              when QR == false, TSId == undefined ->
    ARList == [] andalso throw({noauth,badsig}),
    #dns_rr_tsig{
        domain = Name,
        algname = AlgName,
        mac = MAC,
        original_id = OriginalId,
        error = Error
    } = lists:last(ARList),
    Key = lists:keyfind(Name, 1, TS0#tsig_state.key),
    Key == false andalso throw({notauth,badkey}),
    {Alg,AlgSize} = case inet_dns:decode_algname(AlgName) of
        {A,S} ->
            {A,S};
        A ->
            {A,maps:get(size, crypto:hash_info(A))}
    end,
    %% RFC8945, section 5.2.2.1: MAC Truncation
    MACSize = if
        Error == ?BADSIG; Error == ?BADKEY ->
            AlgSize;
        true ->
            byte_size(MAC)
    end,
    lists:member(Alg, ?ALGS_SUPPORTED) orelse throw({notauth,badkey}),
    MACValid = MACSize >= ?MAC_SIZE_MIN
                andalso
               MACSize >= AlgSize div 2
                andalso
               MACSize =< AlgSize,
    MACValid orelse throw(formerr),
    TS = TS0#tsig_state{ alg = {Alg,AlgSize}, key = Key, id = OriginalId },
    do_verify(Pkt, Response, TS);
%%
%% client and server use this
do_verify(Pkt = <<_Id:16, QR:1, _:15, _/binary>>,
          Response = #dns_rec{ arlist = ARList },
          TS = #tsig_state{ qr = TSQR })
              when QR == TSQR; QR == 1 andalso TSQR >= 2 ->
    case ARList =/= [] andalso lists:last(ARList) of
        %% RFC8945, section 5.3.1: TSIG on TCP Connections
        false when TSQR == 3 -> % not 2 as we must start with a TSIG RR
            MAC0 = mac0(TS),
            %% TS#tsig_state.mac is {?MODULE,binary()} (Previous MAC)
            MACN0 = macN(TS#tsig_state.mac, TS#tsig_state{ mac = MAC0 }),
            MACN = macN(Pkt, TS#tsig_state{ mac = MACN0 }),
            {ok,TS#tsig_state{ qr = TSQR + 1, mac = MACN }};
        false when TSQR >= 4, TSQR =< 99 ->
            MACN = macN(Pkt, TS),
            {ok,TS#tsig_state{ qr = TSQR + 1, mac = MACN }};
        TSigRR = #dns_rr_tsig{} ->
            do_verify(Pkt, Response, TS, TSigRR);
        false ->
            {error,{notauth,badsig}}
    end.

do_verify(Pkt, _Response, TS, TSigRR) ->
    Now = ?NOW,
    #dns_rr_tsig{
        offset = Offset,
        now = NowSigned,
        fudge = Fudge,
        mac = MAC,
        error = Error,
        other_data = OtherData
    } = TSigRR,
    PktS = iolist_to_binary([
        <<(TS#tsig_state.id):16>>,
        binary:part(Pkt, {2,8}),
        begin
            <<ARC:16>> = binary:part(Pkt, {10,2}),
            <<(ARC - 1):16>>
        end,
        binary:part(Pkt, {12,Offset - 12})
    ]),
    MACCalc = if
        element(1, TS#tsig_state.mac) == ?MODULE ->
            mac(PktS, TS, Error, Now, OtherData);
        %% RFC8945, section 5.3.1: TSIG on TCP Connections
        true ->
            mac(TS, Error, Now, OtherData)
    end,
    if
        %% RFC8945, section 5.2 - MUST check time after MAC
        MAC == MACCalc, NowSigned - Fudge < Now, NowSigned + Fudge > Now ->
            QR = if TS#tsig_state.qr == 0 -> 1; true -> 2 end,
            {ok,TS#tsig_state{ qr = QR, mac = {?MODULE,MAC} }};
        MAC == MACCalc ->
            {error,{notauth,badtime}};
        true ->
            {error,{notauth,badsig}}
    end.

%% similar to inet_dns:encode_name/3 but without compression
encode_name(Name) ->
    iolist_to_binary([
        [ <<(byte_size(L)):8, L/binary>>
            || L <- binary:split(list_to_binary(Name), <<".">>, [global]) ],
        <<0:8>>
    ]).

otherdata(?BADTIME, Now) -> <<Now:48>>;
otherdata(_Error, _Now) -> <<>>.

mac0(TS = #tsig_state{ mac = {?MODULE,MAC} }) ->
    {_Name,Secret} = TS#tsig_state.key,
    {Alg,_MACSize} = TS#tsig_state.alg,
    MACState = crypto:mac_init(hmac, Alg, Secret),
    {crypto,if
        %% RFC8945, section 5.3.1: TSIG on TCP Connections
        is_binary(MAC) ->
            MS = byte_size(MAC),
            crypto:mac_update(MACState, [<<MS:16>>, MAC]);
        true ->
            MACState
    end}.

macN({?MODULE,MAC}, #tsig_state{ mac = {crypto,MACState} }) ->
    {crypto,crypto:mac_update(MACState, MAC)};
macN(Pkt, TS = #tsig_state{ mac = {crypto,MACState} }) ->
    {crypto,crypto:mac_update(MACState, [
       <<(TS#tsig_state.id):16>>,
       binary:part(Pkt, {2,byte_size(Pkt) - 2})
    ])}.

%% RFC8945, section 5.3.2: Generation of TSIG on Error Returns
mac(_TS, Error, _Now, _OtherData)
  when Error == ?BADSIG; Error == ?BADKEY ->
    <<>>;
mac(TS = #tsig_state{ mac = {crypto,MACState0} }, Error, Now, OtherData) ->
    {Name,_Secret} = TS#tsig_state.key,
    {Alg,MACSize} = TS#tsig_state.alg,
    Fudge = TS#tsig_state.fudge,
    OtherLen = byte_size(OtherData),
    MACState = crypto:mac_update(MACState0, if
        TS#tsig_state.qr =< 1 ->
            [
                %% TSIG RR
                encode_name(Name), <<?C_ANY:16>>, <<?TTL:32/signed>>,
                %% TSIG RDATA
                encode_name(inet_dns:encode_algname(Alg)),
                <<Now:48, Fudge:16, Error:16, OtherLen:16>>,
                OtherData
            ];
        %% RFC8945, section 5.3.1: TSIG on TCP Connections
        true ->
            <<Now:48, Fudge:16>>
    end),
    crypto:mac_finalN(MACState, MACSize).

mac(Pkt, TS, Error, Now, OtherData) ->
    MAC0 = mac0(TS),
    MACN = macN(Pkt, TS#tsig_state{ mac = MAC0 }),
    mac(TS#tsig_state{ mac = MACN }, Error, Now, OtherData).
