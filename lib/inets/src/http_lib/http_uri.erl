%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
-module(http_uri).
-moduledoc """
Old URI utility module, use uri_string instead

This module is deprecated since OTP 23. Use the module `m:uri_string` to
properly handle URIs, this is the recommended module since OTP 21.

### Data types

Type definitions that are related to URI:

- **`uri_part() = [byte()] | binary()`** - Syntax according to the URI
  definition in RFC 3986, for example, "http://www.erlang.org/"

For more information about URI, see
[RFC 3986](http://www.ietf.org/rfc/rfc3986.txt).
""".
-moduledoc(#{since => "OTP R15B01"}).

-export([encode/1, decode/1]).

-deprecated({encode, 1, "use uri_string:quote function instead"}).
-deprecated({decode, 1, "use uri_string:unquote function instead"}).
-removed({parse, 1, "use uri_string functions instead"}).
-removed({parse, 2, "use uri_string functions instead"}).
-removed({scheme_defaults, 0, "use uri_string functions instead"}).

-removed_type({uri, 0, "use uri_string instead"}).
-removed_type({user_info, 0, "use uri_string instead"}).
-removed_type({scheme, 0, "use uri_string instead"}).
-removed_type({host, 0, "use uri_string instead"}).
-removed_type({path, 0, "use uri_string instead"}).
-removed_type({query, 0, "use uri_string instead"}).
-removed_type({fragment, 0, "use uri_string instead"}).
-removed_type({default_scheme_port_number, 0, "use uri_string instead"}).

%%%=========================================================================
%%%  API
%%%=========================================================================

-doc """
Performs percent encoding.

> #### Warning {: .warning }
>
> Use `uri_string:quote/1` instead
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec encode(Data) -> QuotedData when
      Data :: unicode:chardata(),
      QuotedData :: unicode:chardata().
encode(Data) ->
    uri_string:quote(Data).

-doc """
Decodes a possibly percent encoded URI part

> #### Warning {: .warning }
>
> Use `uri_string:unquote/1` instead
""".
-doc(#{since => <<"OTP R15B01">>}).
-spec decode(QuotedData) -> Data when
      QuotedData :: unicode:chardata(),
      Data :: unicode:chardata().
decode(QuotedData) ->
    uri_string:unquote(QuotedData).
