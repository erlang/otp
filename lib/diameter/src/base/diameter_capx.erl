%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% This module builds CER and CEA records for use during capabilities
%% exchange. All of a CER/CEA is built from AVP values configured on
%% the service in question but values for Supported-Vendor-Id,
%% Vendor-Specific-Application-Id, Auth-Application-Id and
%% Acct-Application-id are also obtained using an older method that
%% remains only for backwards compatibility. With this method, each
%% dictionary module was required to export a cer/0 that returned a
%% diameter_base_CER record (or corresponding list, although the list
%% is also a later addition). Each returned CER contributes its member
%% values for the aforementioned four AVPs to the resulting CER, with
%% remaining AVP's either unspecified or identical to those configured
%% on the service. Auth-Application-Id and Acct-Application-id were
%% originally treated a little differently, each callback being
%% required to return either no value of the same value as the other
%% callbacks, but this coupled the callback modules unnecessarily. (A
%% union is backwards compatible to boot.)
%%
%% Values obtained from the service and callbacks are all included
%% when building a CER. Older code with only callback can continue to
%% use them, newer code should probably stick to service configuration
%% (since this is more explicit) or mix at their own peril.
%%
%% The cer/0 callback is now undocumented (despite never being fully
%% documented to begin with) and should be considered deprecated even
%% by those poor souls still using it.
%%

-module(diameter_capx).

-export([build_CER/1,
         recv_CER/2,
         recv_CEA/2,
         make_caps/2]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").
-include("diameter_types.hrl").
-include("diameter_gen_base_rfc3588.hrl").

-define(SUCCESS, ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_SUCCESS').
-define(NOAPP, ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_NO_COMMON_APPLICATION').
-define(NOSECURITY, ?'DIAMETER_BASE_RESULT-CODE_DIAMETER_NO_COMMON_SECURITY').

-define(NO_INBAND_SECURITY, 0).
-define(TLS, 1).

%% ===========================================================================

-type tried(T) :: {ok, T} | {error, {term(), list()}}.

-spec build_CER(#diameter_caps{})
   -> tried(#diameter_base_CER{}).

build_CER(Caps) ->
    try_it([fun bCER/1, Caps]).

-spec recv_CER(#diameter_base_CER{}, #diameter_service{})
   -> tried({['Unsigned32'()], #diameter_caps{}, #diameter_base_CEA{}}).

recv_CER(CER, Svc) ->
    try_it([fun rCER/2, CER, Svc]).

-spec recv_CEA(#diameter_base_CEA{}, #diameter_service{})
   -> tried({['Unsigned32'()], ['Unsigned32'()], #diameter_caps{}}).

recv_CEA(CEA, Svc) ->
    try_it([fun rCEA/2, CEA, Svc]).

make_caps(Caps, Opts) ->
    try_it([fun mk_caps/2, Caps, Opts]).

%% ===========================================================================
%% ===========================================================================

try_it([Fun | Args]) ->
    try apply(Fun, Args) of
        T -> {ok, T}
    catch
        throw: ?FAILURE(Reason) -> {error, {Reason, Args}}
    end.

%% mk_caps/2

mk_caps(Caps0, Opts) ->
    {Caps, _} = lists:foldl(fun set_cap/2,
                            {Caps0, #diameter_caps{_ = false}},
                            Opts),
    Caps.

-define(SC(K,F),
        set_cap({K, Val}, {Caps, #diameter_caps{F = false} = C}) ->
            {Caps#diameter_caps{F = cap(K, Val)}, C#diameter_caps{F = true}}).

?SC('Origin-Host',         origin_host);
?SC('Origin-Realm',        origin_realm);
?SC('Host-IP-Address',     host_ip_address);
?SC('Vendor-Id',           vendor_id);
?SC('Product-Name',        product_name);
?SC('Origin-State-Id',     origin_state_id);
?SC('Supported-Vendor-Id', supported_vendor_id);
?SC('Auth-Application-Id', auth_application_id);
?SC('Inband-Security-Id',  inband_security_id);
?SC('Acct-Application-Id', acct_application_id);
?SC('Vendor-Specific-Application-Id', vendor_specific_application_id);
?SC('Firmware-Revision',   firmware_revision);

set_cap({Key, _}, _) ->
    ?THROW({duplicate, Key}).

cap(K, V)
  when K == 'Origin-Host';
       K == 'Origin-Realm';
       K == 'Vendor-Id';
       K == 'Product-Name' ->
    V;

cap('Host-IP-Address', Vs)
  when is_list(Vs) ->
    lists:map(fun ipaddr/1, Vs);

cap('Firmware-Revision', V) ->
    [V];

cap(_, Vs)
  when is_list(Vs) ->
    Vs;

cap(K, V) ->
    ?THROW({invalid, K, V}).

ipaddr(A) ->
    try
        diameter_lib:ipaddr(A)
    catch
        error: {invalid_address, _} = T ->
            ?THROW(T)
    end.

%% bCER/1
%%
%% Build a CER record to send to a remote peer.

%% Use the fact that diameter_caps has the same field names as CER.
bCER(#diameter_caps{} = Rec) ->
    #diameter_base_CER{}
        = list_to_tuple([diameter_base_CER | tl(tuple_to_list(Rec))]).

%% rCER/2
%%
%% Build a CEA record to send to a remote peer in response to an
%% incoming CER. RFC 3588 gives no guidance on what should be sent
%% here: should we advertise applications that the peer hasn't sent in
%% its CER (aside from the relay application) or not? If we send
%% applications that the peer hasn't advertised then the peer may have
%% to be aware of the possibility. If we don't then we just look like
%% a server that supports a subset (possibly) of what the client
%% advertised, so this feels like the path of least incompatibility.
%% However, the current draft standard (draft-ietf-dime-rfc3588bis-26,
%% expires 24 July 2011) says this in section 5.3, Capabilities
%% Exchange:
%%
%%   The receiver of the Capabilities-Exchange-Request (CER) MUST
%%   determine common applications by computing the intersection of its
%%   own set of supported Application Id against all of the application
%%   identifier AVPs (Auth-Application-Id, Acct-Application-Id and Vendor-
%%   Specific-Application-Id) present in the CER.  The value of the
%%   Vendor-Id AVP in the Vendor-Specific-Application-Id MUST NOT be used
%%   during computation.  The sender of the Capabilities-Exchange-Answer
%%   (CEA) SHOULD include all of its supported applications as a hint to
%%   the receiver regarding all of its application capabilities.
%%
%% Both RFC and the draft also say this:
%%
%%   The receiver only issues commands to its peers that have advertised
%%   support for the Diameter application that defines the command.  A
%%   Diameter node MUST cache the supported applications in order to
%%   ensure that unrecognized commands and/or AVPs are not unnecessarily
%%   sent to a peer.
%%
%% That is, each side sends all of its capabilities and is responsible for
%% not sending commands that the peer doesn't support.

%% 6.10.  Inband-Security-Id AVP
%%
%%   NO_INBAND_SECURITY                0
%%      This peer does not support TLS.  This is the default value, if the
%%      AVP is omitted.
%%
%%   TLS                               1
%%      This node supports TLS security, as defined by [TLS].

rCER(CER, #diameter_service{capabilities = LCaps} = Svc) ->
    #diameter_base_CEA{}
        = CEA
        = cea_from_cer(bCER(LCaps)),

    RCaps = capx_to_caps(CER),
    SApps = common_applications(LCaps, RCaps, Svc),

    {SApps,
     RCaps,
     build_CEA(SApps,
               LCaps,
               RCaps,
               CEA#diameter_base_CEA{'Result-Code' = ?SUCCESS})}.

%% TODO: 5.3 of RFC 3588 says we MUST return DIAMETER_NO_COMMON_APPLICATION
%%       in the CEA and SHOULD disconnect the transport. However, we have
%%       no way to guarantee the send before disconnecting.

build_CEA([], _, _, CEA) ->
    CEA#diameter_base_CEA{'Result-Code' = ?NOAPP};

build_CEA(_, LCaps, RCaps, CEA) ->
    case common_security(LCaps, RCaps) of
        [] ->
            CEA#diameter_base_CEA{'Result-Code' = ?NOSECURITY};
        [_] = IS ->
            CEA#diameter_base_CEA{'Inband-Security-Id' = IS}
    end.

%% common_security/2

common_security(#diameter_caps{inband_security_id = LS},
                #diameter_caps{inband_security_id = RS}) ->
    cs(LS, RS).

%% Unspecified is equivalent to NO_INBAND_SECURITY.
cs([], RS) ->
    cs([?NO_INBAND_SECURITY], RS);
cs(LS, []) ->
    cs(LS, [?NO_INBAND_SECURITY]);

%% Agree on TLS if both parties support it. When sending CEA, this is
%% to ensure the peer is clear that we will be expecting a TLS
%% handshake since there is no ssl:maybe_accept that would allow the
%% peer to choose between TLS or not upon reception of our CEA. When
%% receiving CEA it deals with a server that isn't explicit about its choice.
%% TODO: Make the choice configurable.
cs(LS, RS) ->
    Is = ordsets:to_list(ordsets:intersection(ordsets:from_list(LS),
                                              ordsets:from_list(RS))),
    case lists:member(?TLS, Is) of
        true ->
            [?TLS];
        false when [] == Is ->
            Is;
        false ->
            [hd(Is)]  %% probably NO_INBAND_SECURITY
    end.
%% The only two values defined by RFC 3588 are NO_INBAND_SECURITY and
%% TLS but don't enforce this. In theory this allows some other
%% security mechanism we don't have to know about, although in
%% practice something there may be a need for more synchronization
%% than notification by way of an event subscription offers.

%% cea_from_cer/1

%% CER is a subset of CEA, the latter adding Result-Code and a few
%% more AVP's.
cea_from_cer(#diameter_base_CER{} = CER) ->
    lists:foldl(fun(F,A) -> to_cea(CER, F, A) end,
                #diameter_base_CEA{},
                record_info(fields, diameter_base_CER)).

to_cea(CER, Field, CEA) ->
    try ?BASE:'#get-'(Field, CER) of
        V -> ?BASE:'#set-'({Field, V}, CEA)
    catch
        error: _ -> CEA
    end.
        
%% rCEA/2

rCEA(#diameter_base_CEA{'Result-Code' = RC}
     = CEA,
     #diameter_service{capabilities = LCaps}
     = Svc) ->
    RC == ?SUCCESS orelse ?THROW({'Result-Code', RC}),

    RCaps = capx_to_caps(CEA),
    SApps = common_applications(LCaps, RCaps, Svc),

    [] == SApps andalso ?THROW(no_common_applications),

    IS = common_security(LCaps, RCaps),

    [] == IS andalso ?THROW(no_common_security),

    {SApps, IS, RCaps};

rCEA(CEA, _Svc) ->
    ?THROW({invalid, CEA}).

%% capx_to_caps/1

capx_to_caps(#diameter_base_CEA{'Origin-Host' = OH,
                                'Origin-Realm' = OR,
                                'Host-IP-Address' = IP,
                                'Vendor-Id' = VId,
                                'Product-Name' = PN,
                                'Origin-State-Id' = OSI,
                                'Supported-Vendor-Id' = SV,
                                'Auth-Application-Id' = Auth,
                                'Inband-Security-Id' = IS,
                                'Acct-Application-Id' = Acct,
                                'Vendor-Specific-Application-Id' = VSA,
                                'Firmware-Revision' = FR,
                                'AVP' = X}) ->
    #diameter_caps{origin_host = OH,
                   origin_realm = OR,
                   vendor_id = VId,
                   product_name = PN,
                   origin_state_id = OSI,
                   host_ip_address = IP,
                   supported_vendor_id = SV,
                   auth_application_id = Auth,
                   inband_security_id = IS,
                   acct_application_id = Acct,
                   vendor_specific_application_id = VSA,
                   firmware_revision = FR,
                   avp = X};

capx_to_caps(#diameter_base_CER{} = CER) ->
    capx_to_caps(cea_from_cer(CER)).

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

%% common_applications/3
%%
%% Identify the (local) applications to be supported on the connection
%% in question.

common_applications(LCaps, RCaps, #diameter_service{applications = Apps}) ->
    LA = app_union(LCaps),
    RA = app_union(RCaps),

    lists:foldl(fun(I,A) -> ca(I, Apps, RA, A) end, [], LA).

ca(Id, Apps, RA, Acc) ->
    Relay = lists:member(?APP_ID_RELAY, RA),
    #diameter_app{alias = Alias} = find_app(Id, Apps),
    tcons(Relay                        %% peer is a relay
          orelse ?APP_ID_RELAY == Id   %% we're a relay
          orelse lists:member(Id, RA), %% app is supported by the peer
          Id,
          Alias,
          Acc).
%% 5.3 of the RFC states that a peer advertising itself as a relay must
%% be interpreted as having common applications.

%% Extract the list of all application identifiers from Auth-Application-Id,
%% Acct-Application-Id and Vendor-Specific-Application-Id.
app_union(#diameter_caps{auth_application_id = U,
                         acct_application_id = C,
                         vendor_specific_application_id = V}) ->
    set_list(U ++ C ++ lists:flatmap(fun vsa_apps/1, V)).

vsa_apps(#'diameter_base_Vendor-Specific-Application-Id'
         {'Auth-Application-Id' = U,
          'Acct-Application-Id' = C}) ->
    U ++ C;
vsa_apps(L) ->
    Rec = ?BASE:'#new-'('diameter_base_Vendor-Specific-Application-Id', L),
    vsa_apps(Rec).

%% It's a configuration error for a locally advertised application not
%% to be represented in Apps. Don't just match on lists:keyfind/3 in
%% order to generate a more helpful error.
find_app(Id, Apps) ->
    case lists:keyfind(Id, #diameter_app.id, Apps) of
        #diameter_app{} = A ->
            A;
        false ->
            ?THROW({app_not_configured, Id})
    end.

set_list(L) ->
    sets:to_list(sets:from_list(L)).

tcons(true, K, V, Acc) ->
    [{K,V} | Acc];
tcons(false, _, _, Acc) ->
    Acc.
