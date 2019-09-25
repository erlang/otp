%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2016. All Rights Reserved.
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
-include_lib("public_key/include/public_key.hrl"). 

-export_type([dist_point/0, crl_cache_ref/0, logger_info/0]).

-type crl_cache_ref() :: any().
-type issuer_name() ::  {rdnSequence,[#'AttributeTypeAndValue'{}]}.
-type dist_point()  :: #'DistributionPoint'{}.
-type logger_info()     :: {logger:level(), Report::#{description => string(), reason => term()}, logger:metadata()}.

-callback lookup(dist_point(), issuer_name(), crl_cache_ref()) -> not_available | [public_key:der_encoded()] | 
                                                                  {{logger, logger_info()}, [public_key:der_encoded()]}.
-callback select(issuer_name() | list(), crl_cache_ref()) ->  [public_key:der_encoded()] |  
                                                     {logger, logger_info(), [public_key:der_encoded()]}.
-callback fresh_crl(dist_point(), public_key:der_encoded()) -> public_key:der_encoded() | 
                                                               {logger, logger_info(),  public_key:der_encoded()}.
