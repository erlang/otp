%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2017. All Rights Reserved.
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

-export([build_CER/2,
         recv_CER/3,
         recv_CEA/3,
         make_caps/2,
         binary_caps/1]).

-include_lib("diameter/include/diameter.hrl").
-include("diameter_internal.hrl").

-define(SUCCESS,    2001).  %% DIAMETER_SUCCESS
-define(NOAPP,      5010).  %% DIAMETER_NO_COMMON_APPLICATION
-define(NOSECURITY, 5017).  %% DIAMETER_NO_COMMON_SECURITY

-define(NO_INBAND_SECURITY, 0).
-define(TLS, 1).

%% ===========================================================================

-type tried(T) :: {ok, T} | {error, {term(), list()}}.

-spec build_CER(#diameter_caps{}, module())
   -> tried(CER)
 when CER :: tuple().

build_CER(Caps, Dict) ->
    try_it([fun bCER/2, Caps, Dict]).

-spec recv_CER(CER, #diameter_service{}, module())
   -> tried({[diameter:'Unsigned32'()],
             #diameter_caps{},
             CEA})
 when CER :: tuple(),
      CEA :: tuple().

recv_CER(CER, Svc, Dict) ->
    try_it([fun rCER/3, CER, Svc, Dict]).

-spec recv_CEA(CEA, #diameter_service{}, module())
   -> tried({[diameter:'Unsigned32'()],
             [diameter:'Unsigned32'()],
             #diameter_caps{}})
 when CEA :: tuple().

recv_CEA(CEA, Svc, Dict) ->
    try_it([fun rCEA/3, CEA, Svc, Dict]).

-spec make_caps(#diameter_caps{}, [{atom(), term()}])
   -> tried(#diameter_caps{}).

make_caps(Caps, Opts) ->
    try_it([fun mk_caps/2, Caps, Opts]).

%% ===========================================================================
%% ===========================================================================

try_it([Fun | Args]) ->
    try apply(Fun, Args) of
        T -> {ok, T}
    catch
        throw: ?FAILURE(Reason) -> {error, Reason}
    end.

%% mk_caps/2

mk_caps(Caps0, Opts) ->
    Fields = diameter_gen_base_rfc3588:'#info-'(diameter_base_CER, fields),
    Defs = lists:zip(Fields, tl(tuple_to_list(Caps0))),
    Unset = maps:from_list([{F, true} || F <- lists:droplast(Fields)]), %% no 'AVP'
    {Caps, _} = lists:foldl(fun set_cap/2, {Defs, Unset}, Opts),
    #diameter_caps{} = list_to_tuple([diameter_caps | [V || {_,V} <- Caps]]).

set_cap({F,V}, {Caps, Unset}) ->
    case Unset of
        #{F := true} ->
            {lists:keyreplace(F, 1, Caps, {F, cap(F, copy(V))}),
             maps:remove(F, Unset)};
        _ ->
            ?THROW({duplicate, F})
    end.

cap(K, V)
  when K == 'Origin-Host';
       K == 'Origin-Realm';
       K == 'Vendor-Id';
       K == 'Product-Name' ->
    V;

cap('Host-IP-Address', Vs)
  when is_list(Vs) ->
    lists:map(fun ipaddr/1, Vs);

cap(K, V)
  when K == 'Firmware-Revision';
       K == 'Origin-State-Id' ->
    [V];

cap(_, Vs)
  when is_list(Vs) ->
    Vs;

cap(K, V) ->
    ?THROW({invalid, {K,V}}).

ipaddr(A) ->
    try
        diameter_lib:ipaddr(A)
    catch
        error: {invalid_address, _} = T ->
            ?THROW(T)
    end.

%% bCER/2
%%
%% Build a CER record to send to a remote peer.

%% Use the fact that diameter_caps is expected to have the same field
%% names as CER.
bCER(#diameter_caps{} = Rec, Dict) ->
    RecName = Dict:msg2rec('CER'),
    Values = lists:zip(Dict:'#info-'(RecName, fields),
                       tl(tuple_to_list(Rec))),
    Dict:'#new-'(RecName, [{K, map(K, V, Dict)} || {K,V} <- Values]).

%% map/3
%%
%% Deal with differerences in common dictionary AVP's to make changes
%% transparent in service/transport config. In particular, one
%% annoying difference between RFC 3588 and RFC 6733.
%%
%% RFC 6773 changes the definition of Vendor-Specific-Application-Id,
%% giving Vendor-Id arity 1 instead of 3588's 1*. This causes woe
%% since the corresponding dictionaries expect different values for a
%% 'Vendor-Id': a list for 3588, an integer for 6733.

map('Vendor-Specific-Application-Id' = T, L, Dict) ->
    RecName = Dict:name2rec(T),
    Rec = Dict:'#new-'(RecName, []),
    Def = Dict:'#get-'('Vendor-Id', Rec),
    [vsa(V, Def) || V <- L];
map(_, V, _) ->
    V.

vsa({_, N, _, _} = Rec, [])
  when is_integer(N) ->
    setelement(2, Rec, [N]);

vsa({_, [N], _, _} = Rec, undefined)
  when is_integer(N) ->
    setelement(2, Rec, N);

vsa([_|_] = L, Def) ->
    [vid(T, Def) || T <- L];

vsa(T, _) ->
    T.

vid({'Vendor-Id' = K, N}, [])
  when is_integer(N) ->
    {K, [N]};

vid({'Vendor-Id' = K, [N]}, undefined) ->
    {K, N};

vid(T, _) ->
    T.

%% rCER/3
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

rCER(CER, #diameter_service{capabilities = LCaps} = Svc, Dict) ->
    CEA = cea_from_cer(bCER(LCaps, Dict), Dict),
    RCaps = capx_to_caps(CER, Dict),
    SApps = common_applications(LCaps, RCaps, Svc),

    {SApps,
     RCaps,
     build_CEA(SApps,
               LCaps,
               RCaps,
               Dict,
               Dict:'#set-'({'Result-Code', ?SUCCESS}, CEA))}.

build_CEA([], _, _, Dict, CEA) ->
    Dict:'#set-'({'Result-Code', ?NOAPP}, CEA);

build_CEA(_, LCaps, RCaps, Dict, CEA) ->
    case common_security(LCaps, RCaps) of
        [] ->
            Dict:'#set-'({'Result-Code', ?NOSECURITY}, CEA);
        [_] = IS ->
            Dict:'#set-'({'Inband-Security-Id', inband_security(IS)}, CEA)
    end.

%% Only set Inband-Security-Id if different from the default, since
%% RFC 6733 recommends against the AVP:
%%
%% 6.10.  Inband-Security-Id AVP
%%
%%    The Inband-Security-Id AVP (AVP Code 299) is of type Unsigned32 and
%%    is used in order to advertise support of the security portion of the
%%    application.  The use of this AVP in CER and CEA messages is NOT
%%    RECOMMENDED.  Instead, discovery of a Diameter entity's security
%%    capabilities can be done either through static configuration or via
%%    Diameter Peer Discovery as described in Section 5.2.

inband_security([?NO_INBAND_SECURITY]) ->
    [];
inband_security([_] = IS) ->
    IS.

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

%% cea_from_cer/2

%% CER is a subset of CEA, the latter adding Result-Code and a few
%% more AVP's.
cea_from_cer(CER, Dict) ->
    RecName = Dict:msg2rec('CEA'),
    [_ | Values] = Dict:'#get-'(CER),
    Dict:'#new-'([RecName | Values]).

%% rCEA/3

rCEA(CEA, #diameter_service{capabilities = LCaps} = Svc, Dict) ->
    RCaps = capx_to_caps(CEA, Dict),
    SApps = common_applications(LCaps, RCaps, Svc),
    IS = common_security(LCaps, RCaps),

    {SApps, IS, RCaps}.

%% capx_to_caps/2

capx_to_caps(CEX, Dict) ->
    [OH, OR, IP, VId, PN, OSI, SV, Auth, IS, Acct, VSA, FR, X]
        = Dict:'#get-'(['Origin-Host',
                        'Origin-Realm',
                        'Host-IP-Address',
                        'Vendor-Id',
                        'Product-Name',
                        'Origin-State-Id',
                        'Supported-Vendor-Id',
                        'Auth-Application-Id',
                        'Inband-Security-Id',
                        'Acct-Application-Id',
                        'Vendor-Specific-Application-Id',
                        'Firmware-Revision',
                        'AVP'],
                       CEX),
    #diameter_caps{origin_host = copy(OH),
                   origin_realm = copy(OR),
                   vendor_id = VId,
                   product_name = copy(PN),
                   origin_state_id = OSI,
                   host_ip_address = IP,
                   supported_vendor_id = SV,
                   auth_application_id = Auth,
                   inband_security_id = IS,
                   acct_application_id = Acct,
                   vendor_specific_application_id = VSA,
                   firmware_revision = FR,
                   avp = X}.

%% Copy binaries to avoid retaining a reference to a large binary
%% containing AVPs we aren't interested in.
copy(B)
  when is_binary(B) ->
    binary:copy(B);

copy(T) ->
    T.

%% binary_caps/1
%%
%% Encode stringish capabilities with {string_decode, false}.

binary_caps(Caps) ->
    lists:foldl(fun bcaps/2, Caps, [#diameter_caps.origin_host,
                                    #diameter_caps.origin_realm,
                                    #diameter_caps.product_name]).

bcaps(N, Caps) ->
    case element(N, Caps) of
        undefined ->
            Caps;
        V ->
            setelement(N, Caps, iolist_to_binary(V))
    end.

%% ---------------------------------------------------------------------------
%% ---------------------------------------------------------------------------

%% common_applications/3
%%
%% Identify the (local) applications to be supported on the connection
%% in question. The RFC says this:
%%
%% 2.4 Application Identifiers
%%
%%    Relay and redirect agents MUST advertise the Relay Application ID,
%%    while all other Diameter nodes MUST advertise locally supported
%%    applications.
%%
%% Taken literally, every Diameter node should then advertise support
%% for the Diameter common messages application, with id 0, since no
%% node can perform capabilities exchange without it. Expecting this,
%% or regarding the support as implicit, renders the Result-Code 5010
%% (DIAMETER_NO_COMMON_APPLICATION) meaningless however, since every
%% node would regard the common application as being in common with
%% the peer. In practice, nodes may or may not advertise support for
%% Diameter common messages.
%%
%% That only explicitly advertised applications should be considered
%% when computing the intersection with the peer is supported here:
%%
%% 5.3.  Capabilities Exchange
%%
%%    The receiver of the Capabilities-Exchange-Request (CER) MUST
%%    determine common applications by computing the intersection of its
%%    own set of supported Application Ids against all of the
%%    Application-Id AVPs (Auth-Application-Id, Acct-Application-Id, and
%%    Vendor-Specific-Application-Id) present in the CER.
%%
%% The same section also has the following about capabilities exchange
%% messages.
%%
%%    The receiver only issues commands to its peers that have advertised
%%    support for the Diameter application that defines the command.
%%
%% This statement is also difficult to interpret literally since it
%% would disallow D[WP]R and more when Diameter common messages isn't
%% advertised. In practice, diameter lets requests be sent as long as
%% there's a dictionary configured to support it, peer selection by
%% advertised application being possible to preempt by passing
%% candidate peers directly to diameter:call/4. The peer can always
%% answer 3001 (DIAMETER_COMMAND_UNSUPPORTED) or 3007
%% (DIAMETER_APPLICATION_UNSUPPORTED) if this is objectionable.

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

vsa_apps(Vals)
  when is_list(Vals) ->
    lists:flatmap(fun({'Vendor-Id', _}) -> []; ({_, Ids}) -> Ids end, Vals);
vsa_apps(Rec)
  when is_tuple(Rec) ->
    [_Name, _VendorId | Idss] = tuple_to_list(Rec),
    lists:append(Idss).

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
