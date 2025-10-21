%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
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

-module(ssl_crl_cache_api).
-moduledoc """
API for a TLS CRL (Certificate Revocation List) cache.

When TLS performs certificate path validation according to
[RFC 5280 ](http://www.ietf.org/rfc/rfc5280.txt)it should also perform CRL
validation checks. To enable the CRL checks the application needs access to
CRLs. A database of CRLs can be set up in many different ways. This module
provides the behavior of the API needed to integrate an arbitrary CRL cache with
the erlang ssl application. It is also used by the application itself to provide
a simple default implementation of a CRL cache.
""".
-moduledoc(#{since => "OTP 18.0"}).
-include_lib("public_key/include/public_key.hrl").

-export_type([dist_point/0, crl_cache_ref/0, logger_info/0]).

-doc "Reference to the CRL cache.".
-type crl_cache_ref() :: any().

-doc """
For description see
[X509 certificates records](`e:public_key:public_key_records.md`)
""".
-type dist_point()  :: #'DistributionPoint'{}.

-doc "Information for ssl applications use of [Logger(3)](`m:logger`)".
-type logger_info()     :: {logger:level(), Report::#{description => string(), reason => term()}, logger:metadata()}.


-doc(#{equiv => lookup/3,since => <<"OTP 18.0">>}).
-doc """
Backwards compatibility, replaced by lookup/3
""".
-callback lookup(DistPoint::dist_point(), CacheRef::crl_cache_ref()) ->
    not_available | [public_key:der_encoded()] |
    {{logger, logger_info()}, [public_key:der_encoded()]}.


-doc(#{since => <<"OTP 19.0">>}).
-doc """
Lookup the CRLs belonging to the distribution point `Distributionpoint`. This
function may choose to only look in the cache or to follow distribution point
links depending on how the cache is administrated.

The `Issuer` argument contains the issuer name of the certificate to
be checked.  Normally the returned CRL should be issued by this
issuer, except if the `cRLIssuer` field of `DistributionPoint` has a
value, in which case that value should be used instead.

In an earlier version of this API, the `lookup` function received two
arguments, omitting `Issuer`. For compatibility, this is still
supported: if there is no [`lookup/3`](`c:lookup/3`) function in the
callback module,[`lookup/2`](`c:lookup/2`) is called instead.

It is possible to return logger info, since OTP 22.2, that will be used by the TLS connection to
produce log events.
""".
-callback lookup(Distpoint::dist_point(), Issuer::public_key:issuer_name(), CacheRef::crl_cache_ref()) ->
    not_available | [public_key:der_encoded()] |
    {{logger, logger_info()}, [public_key:der_encoded()]}.


-doc(#{since => <<"OTP 18.0">>}).
-doc """
Select the CRLs in the cache that are issued by `Issuer` unless the value is a
list of so called general names, see
[X509 certificates records](`e:public_key:public_key_records.md`), originating
form `#'DistributionPoint'.cRLissuer` and representing different mechanism to
obtain the CRLs. The cache callback needs to use the appropriate entry to
retrieve the CRLs or return an empty list if it does not exist.

It is possible to return logger info, since OTP 22.2, that will be used by the TLS connection to
produce log events.
""".
-callback select(IssuerOrDPLocations, CacheRef) ->
    [CRL] |
    {logger, logger_info(), [CRL]} when
      CRL:: public_key:der_encoded(),
      IssuerOrDPLocations::public_key:issuer_name() | list(),
      CacheRef :: crl_cache_ref().

-doc(#{since => <<"OTP 18.0">>}).
-doc """
`fun fresh_crl/2` will be used as input option `update_crl` to
`public_key:pkix_crls_validate/3`.

It is possible to return logger info, since OTP 22.2, that will be used by the TLS connection to
produce log events.
""".
-callback fresh_crl(DistPoint::dist_point(), CRL::public_key:der_encoded()) ->
    public_key:der_encoded() |
    {logger, logger_info(),  public_key:der_encoded()}.

-optional_callbacks([lookup/2]).
