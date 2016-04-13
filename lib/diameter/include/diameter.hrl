%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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

-ifndef(diameter_hrl).
-define(diameter_hrl, true).

%% RFC 3588, 2.4:
-define(DIAMETER_APP_ID_COMMON,     0).
-define(DIAMETER_APP_ID_ACCOUNTING, 3).
-define(DIAMETER_APP_ID_RELAY,      16#FFFFFFFF).

%% Corresponding dictionaries. These macros are deprecated now that
%% there is an RFC6733 whose dictionaries are not strictly backwards
%% compatible. The RFC 6733 common and accounting dictionaries are
%% diameter_gen_base_rfc6733 and diameter_gen_acct_rfc6733
%% respectively.
-define(DIAMETER_DICT_COMMON,       diameter_gen_base_rfc3588).
-define(DIAMETER_DICT_ACCOUNTING,   diameter_gen_base_accounting).
-define(DIAMETER_DICT_RELAY,        diameter_gen_relay).

%% Events sent to processes that have subscribed with
%% diameter:subscribe/1.
%%
-record(diameter_event,
        {service,   %% name
         info}).    %% term()

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
         cmd_code,           %% 24-bit unsigned
         application_id,     %% 32-bit unsigned
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

%% A diameter_callback record can be specified as an application
%% module in order to selectively receive callbacks or alter their
%% form.
-record(diameter_callback,
        {peer_up,
         peer_down,
         pick_peer,
         prepare_request,
         prepare_retransmit,
         handle_request,
         handle_answer,
         handle_error,
         default,
         extra = []}).

%% The diameter service and diameter_app records are only passed
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
         options = [{answer_errors, discard},         %% | callback | report
                    {request_errors, answer_3xxx}]}). %% | callback | answer

-endif. %% -ifdef(diameter_hrl).
