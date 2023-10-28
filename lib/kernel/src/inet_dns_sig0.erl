%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
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
-module(inet_dns_sig0).

%% DNS SIG(0) handling
%%
%% RFC 2535: Domain Name System Security Extensions
%% RFC 2931: DNS Request and Transaction Signatures ( SIG(0)s )

%% WARNING: in the spirit of inet_dns.erl, this module only handles the
%% cryptographic operations and does not absolve you of your responsibities
%% that include:
%%  * truncated messages must include the SIG(0) RR as per RFC2931, section 3
%%  * if running as a server, generate a suitable error response REFUSED
%%  * messages passed in have a SIG(0) RR
%%  * provide all the KEY RR's for the signer; may require DNS lookups
%%  * check the KEY RR's are not marked authentication prohibited

-export([init/1, verify/3]).

-include("inet_dns.hrl").

-define(FUDGE_DEFAULT, 300). % RFC2931, section 3.3
-define(TTL, 0).
-define(NOW, (os:system_time(seconds))). % RFC2535, section 4.1.5

-define(ALG_DEFAULT, ecdsap256sha256). % RFC8624, section 3.1
-define(ALGS_SUPPORTED, [?ALG_DEFAULT]).
-type sig_alg() :: ecdsap256sha256.

% these are exposed only on the wire for TSIG requests (not SIG(0)) but are
% useful to communicate back to user code why the verification failed; the user
% code should ignore the SIG(0) was present and behave as if it had not been
%% included with the message (RFC2535, section 4.3); typically returning REFUSED
-type sig_error() :: badsig | badkey | badtime.

-record(
   sig0_state,
   {key       = []              :: list(key()),
    keypeer   = []              :: list(key()),
    alg       = ?ALG_DEFAULT    :: sig_alg(),
    fudge     = ?FUDGE_DEFAULT  :: non_neg_integer(),

    qr        = 0               :: 0 | 1,
    %% 0 (false): request, 1 (true): response

    pkt                         :: binary() | undefined
}).
-opaque sig0_state() :: #sig0_state{}.
-export_type([sig0_state/0]).

-record(
   key,
   {name :: string(),
    alg  :: sig_alg(),
    tag  :: keytag(),
    key  :: public_key:private_key() | public_key:public_key()
}).

-type key() :: #key{}.
-type keytag() :: 0..65535.

-spec init(proplists:proplist()) -> sig0_state().
init(Config) ->
    Key = lists:map(fun(RR = #dns_rr{ domain = Name, type = ?S_KEY, data = {_Flags,_Protocol,Algorithm,PrivateKey} }) ->
        #key{
            name = Name,
            alg = Algorithm,
            tag = keytag(RR),
            key = PrivateKey
        }
    end, proplists:get_value(key, Config, [])),
    KeyPeer = lists:map(fun(RR = #dns_rr{ domain = Name, type = ?S_KEY, data = {_Flags,_Protocol,Algorithm,PublicKey} }) ->
        #key{
            name = Name,
            alg = Algorithm,
            tag = keytag(RR),
            key = PublicKey
        }
    end, proplists:get_value(keypeer, Config, [])),
    Alg = proplists:get_value(alg, Config, ?ALG_DEFAULT),
    true = lists:member(Alg, ?ALGS_SUPPORTED),
    Fudge = proplists:get_value(fudge, Config, ?FUDGE_DEFAULT),
    true = is_integer(Fudge) andalso Fudge >= 0,
    #sig0_state{ key = Key, keypeer = KeyPeer, alg = Alg, fudge = Fudge }.

% sign()

% what to add to your zone file and/or include with your query
% generate_key()
% keyrr(PrivateKey | PublicKey) -> ...

%% this function needs to work with the original response binary as the
%% calculation is affected by each DNS implementation having their own varying
%% name compression algorithm
-spec verify(binary(), #dns_rec{}, sig0_state()) ->
                 {ok, sig0_state()} | {error, sig_error()}.
verify(Pkt, DnsRec, SS = #sig0_state{ keypeer = KeyPeer, qr = 0 }) when KeyPeer =/= [] ->
    Now = ?NOW,
    Sig0 = #dns_rr_sig{
        type_covered = 0,
        algorithm = Algorithm,
        signature_expiration = SigExp,
        signature_inception = SigInc,
        key_tag = KeyTag,
        signers_name = SignersName,
        signature = Signature
    } = lists:last(DnsRec#dns_rec.arlist),
    RData = rdata(Sig0#dns_rr_sig{ signature = <<>> }),
    Pkt0 = pkt0(Pkt, Sig0),
    Msg = <<RData/binary, Pkt0/binary>>,
    DigestType = digesttype(Algorithm),
    Keys = [ Key#key.key || Key <- KeyPeer, Key#key.name == SignersName andalso Key#key.alg == Algorithm andalso KeyTag == Key#key.tag ],
    Verified = lists:any(fun(Key) -> public_key:verify(Msg, DigestType, Signature, Key) end, Keys),
    if
        Verified, Now >= SigInc, Now < SigExp ->
            {ok, SS#sig0_state{ pkt = Pkt }};
        Verified ->
            {error, {notauth, badtime}};
        Keys == [] ->
            {error, {notauth, badkey}};
        true ->
            {error, {notauth, badsig}}
    end.

pkt0(Pkt, _Sig0 = #dns_rr_sig{ offset = Offset }) ->
    PktLen = byte_size(Pkt),
    <<PktH:10/binary, ARCount:16, PktT:(PktLen - 12 - (PktLen - Offset))/binary, _Sig0B:(PktLen - Offset)/binary>> = Pkt,
    <<PktH/binary, (ARCount - 1):16, PktT/binary>>.

rdata(RR) ->
    %% for simplicity, and to avoid export hell, we encode a skeleton packet
    <<_:12/binary, RRB/binary>> = inet_dns:encode(#dns_rec{ header = #dns_header{}, arlist = [RR] }),
    DName = case RR of
        (#dns_rr{ domain = D }) -> D;
        (#dns_rr_opt{ domain = D }) -> D;
        (#dns_rr_sig{ domain = D }) -> D;
        (#dns_rr_tsig{ domain = D }) -> D
    end,
    %% as the packet is otherwise empty name compression does not take place
    DNameLen = case DName of "." -> 1; _ -> length(DName) + 2 end,
    <<_:DNameLen/binary, _:8/binary, RDataLen:16, RData:RDataLen/binary>> = RRB,
    RData.

digesttype(Algorithm) when is_atom(Algorithm) ->
    digesttype(inet_dns:encode_dnssec_algname(Algorithm));
digesttype(?T_DNSSEC_ALGNUM_RSAMD5) -> md5;
digesttype(?T_DNSSEC_ALGNUM_ECDSAP256SHA256) -> sha256.

%% RFC2535, section 4.1.6 and Appendix C
%% RFC4034, section 3.1.6 and Appendix B
-spec keytag(#dns_rr{}) -> keytag().
keytag(RR = #dns_rr{ type = ?S_KEY, data = {_Flags,_Protocol,Algorithm,_PublicKey} }) ->
    keytag(rdata(RR), Algorithm).

-spec keytag(binary() | public_key:public_key(), sig_alg()) -> keytag().
keytag(RData, Algorithm) when is_binary(RData), Algorithm == ?S_DNSSEC_ALGNUM_RSAMD5 orelse Algorithm == ?T_DNSSEC_ALGNUM_RSAMD5 ->
    RDataLen = byte_size(RData),
    <<_:(RDataLen - 4)/binary, KeyTag:16, _:2/binary>> = RData,
    KeyTag;
keytag(RData, _Algorithm) when is_binary(RData) ->
    keytag(RData, 0, false);
keytag(PublicKey, Algorithm) ->
    keytag(inet_dns:encode_key_publickey(PublicKey, Algorithm), Algorithm).

keytag(<<>>, AC, _T) ->
    (AC + ((AC bsr 16) band 16#ffff)) band 16#ffff;
keytag(<<X:8, R/binary>>, AC, T) ->
    keytag(R, AC + if T -> X; true -> X bsl 8 end, not T).
