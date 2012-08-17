%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File: ct_netconfc.hrl
%%
%% Description:
%%    This file defines constant values and records used by the
%%    netconf client ct_netconfc.
%%
%% @author Support
%% @doc Netconf Client Interface.
%% @end
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
