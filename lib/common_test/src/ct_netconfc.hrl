%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File: ct_netconfc.hrl
%%
%% Description:
%%    This file defines constant values and records used by the
%%    netconf client ct_netconfc.
%%
%% Netconf Client Interface.
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------


%% Default port number (RFC 4742/IANA).
-define(DEFAULT_PORT, 830).

%% Default timeout to wait for netconf server to reply to a request
-define(DEFAULT_TIMEOUT, infinity). %% msec

%% Namespaces
-define(NETCONF_NAMESPACE_ATTR,[{xmlns,?NETCONF_NAMESPACE}]).
-define(ACTION_NAMESPACE_ATTR,[{xmlns,?ACTION_NAMESPACE}]).
-define(NETCONF_NOTIF_NAMESPACE_ATTR,[{xmlns,?NETCONF_NOTIF_NAMESPACE}]).
-define(NETMOD_NOTIF_NAMESPACE_ATTR,[{xmlns,?NETMOD_NOTIF_NAMESPACE}]).

-define(NETCONF_NAMESPACE,"urn:ietf:params:xml:ns:netconf:base:1.0").
-define(ACTION_NAMESPACE,"urn:com:ericsson:ecim:1.0").
-define(NETCONF_NOTIF_NAMESPACE,
       "urn:ietf:params:xml:ns:netconf:notification:1.0").
-define(NETMOD_NOTIF_NAMESPACE,"urn:ietf:params:xml:ns:netmod:notification").

%% Capabilities
-define(NETCONF_BASE_CAP,"urn:ietf:params:netconf:base:").
-define(NETCONF_BASE_CAP_VSN,"1.0").

%% Misc
-define(END_TAG,<<"]]>]]>">>).

-define(FORMAT(_F, _A), lists:flatten(io_lib:format(_F, _A))).
