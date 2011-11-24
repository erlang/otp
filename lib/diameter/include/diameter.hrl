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

-ifndef(diameter_hrl).
-define(diameter_hrl, true).

%% RFC 3588, 2.4:
-define(DIAMETER_APP_ID_COMMON,     0).
-define(DIAMETER_APP_ID_ACCOUNTING, 3).
-define(DIAMETER_APP_ID_RELAY,      16#FFFFFFFF).

%% Corresponding dictionaries:
-define(DIAMETER_DICT_COMMON,       diameter_gen_base_rfc3588).
-define(DIAMETER_DICT_ACCOUNTING,   diameter_gen_base_accounting).
-define(DIAMETER_DICT_RELAY,        diameter_gen_relay).

%% Events sent to processes that have subscribed with
%% diameter:subscribe/1.
%%
-record(diameter_event,
        {service,   %% name
         info}).    %% tuple()

%% diameter_packet records are passed through the encode/decode
%% interface supplied by a dictionary module configured on a Diameter
%% application. For an incoming message the bin field contains the
%% received binary and the header, avps, msg and errors fields the
%% result of decoding.

-record(diameter_packet,
        {header,     %% #diameter_header{}
         avps,       %% deep list() of #diameter_avp{}
         msg,        %% fully decoded message
         bin,        %% binary received/sent over the wire
         errors = [],%% list() of Result-Code | {Result-Code, #diameter_avp{}}
         transport_data}).

-record(diameter_header,
        {version,            %%  8-bit unsigned
         length,             %% 24-bit unsigned
         cmd_code,           %%  8-bit unsigned
         application_id,     %% 24-bit unsigned
         hop_by_hop_id,      %% 32-bit unsigned
         end_to_end_id,      %% 32-bit unsigned
         is_request,         %% boolean() R flag
         is_proxiable,       %% boolean() P flag
         is_error,           %% boolean() E flag
         is_retransmitted}). %% boolean() T flag

-record(diameter_avp,
        {code,      %% 32-bit unsigned
         vendor_id, %% 32-bit unsigned
         is_mandatory    = false, %% boolean() M flag
         need_encryption = false, %% boolean() P flag
         data,      %% encoded binary()
         name,      %% atom() AVP name
         value,     %% decoded term() decoded | undefined
         type,      %% atom() type name,
         index}).   %% non_neg_integer() | undefined

%% A diameter_caps record corresponds to capabilities configuration on
%% diameter:start_service/2. In application callbacks is identifies
%% the peer connection for which the callback is taking place, and in
%% this case each field is a 2-tuple specifying the host (ie. local)
%% and peer (ie. remote) values, host values having been configured
%% and peer values having been received at capabilities exchange.

-record(diameter_caps,
        {origin_host,               %% 'DiameterIdentity'()
         origin_realm,              %% 'DiameterIdentity'()
         host_ip_address = [],      %% ['Address'()]
         vendor_id,                 %% 'Unsigned32'()
         product_name,              %% 'OctetString'()
         origin_state_id = [],      %% ['Unsigned32'()]
         supported_vendor_id = [],  %% ['Unsigned32'()]
         auth_application_id = [],  %% ['Unsigned32'()]
         inband_security_id  = [],  %% ['Unsigned32'()]
         acct_application_id = [],  %% ['Unsigned32'()]
         vendor_specific_application_id = [], %% ['Grouped'()]
         firmware_revision   = [],  %% ['Unsigned32()]
         avp = []}).

%% AVP's of type DiameterURI are encoded as a diameter_uri record.
%% Note that AVP's of type IPFilterRule and QoSFilterRule are currently
%% encoded simply as OctetString's.

-record(diameter_uri,
        {type,  %% aaa | aaas
         fqdn,  %% string()
         port = 3868, %% non_neg_integer(),
         transport = sctp,       %% | tcp,
         protocol  = diameter}). %% | radius | 'tacacs+'

%% The diameter service and diameter_apps records are only passed
%% through the transport interface when starting a transport process,
%% although typically a transport implementation will (and probably
%% should) only be interested host_ip_address.

-record(diameter_service,
        {pid,
         capabilities,        %% #diameter_caps{}
         applications = []}). %% [#diameter_app{}]

-record(diameter_app,
        {alias,      %% option 'alias'
         dictionary, %% option 'dictionary', module()
         module,     %% [Mod | Args] callback module() and extra args
         init_state, %% option 'state', initial callback state
         id,         %% 32-bit unsigned application identifier = Dict:id()
         mutable = false, %% boolean(), do traffic callbacks modify state?
         answer_errors = report}). %% | callback | discard
                                   %% how to handle containing errors?

-endif. %% -ifdef(diameter_hrl).
