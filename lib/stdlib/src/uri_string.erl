%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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
%% [RFC 3986, Chapter 2.2. Reserved Characters]
%%
%%   reserved    = gen-delims / sub-delims
%%
%%   gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
%%
%%   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
%%               / "*" / "+" / "," / ";" / "="
%%
%%
%% [RFC 3986, Chapter 2.3. Unreserved Characters]
%%
%%   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
%%
%%
%% [RFC 3986, Chapter 3. Syntax Components]
%%
%% The generic URI syntax consists of a hierarchical sequence of
%% components referred to as the scheme, authority, path, query, and
%% fragment.
%%
%%    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%%
%%    hier-part   = "//" authority path-abempty
%%                   / path-absolute
%%                   / path-rootless
%%                   / path-empty
%%
%%    The scheme and path components are required, though the path may be
%%    empty (no characters).  When authority is present, the path must
%%    either be empty or begin with a slash ("/") character.  When
%%    authority is not present, the path cannot begin with two slash
%%    characters ("//").  These restrictions result in five different ABNF
%%    rules for a path (Section 3.3), only one of which will match any
%%    given URI reference.
%%
%%    The following are two example URIs and their component parts:
%%
%%          foo://example.com:8042/over/there?name=ferret#nose
%%          \_/   \______________/\_________/ \_________/ \__/
%%           |           |            |            |        |
%%        scheme     authority       path        query   fragment
%%           |   _____________________|__
%%          / \ /                        \
%%          urn:example:animal:ferret:nose
%%
%%
%% [RFC 3986, Chapter 3.1. Scheme]
%%
%% Each URI begins with a scheme name that refers to a specification for
%% assigning identifiers within that scheme.
%%
%%    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
%%
%%
%% [RFC 3986, Chapter 3.2. Authority]
%%
%% Many URI schemes include a hierarchical element for a naming
%% authority so that governance of the name space defined by the
%% remainder of the URI is delegated to that authority (which may, in
%% turn, delegate it further).
%%
%%    authority   = [ userinfo "@" ] host [ ":" port ]
%%
%%
%% [RFC 3986, Chapter 3.2.1. User Information]
%%
%% The userinfo subcomponent may consist of a user name and, optionally,
%% scheme-specific information about how to gain authorization to access
%% the resource. The user information, if present, is followed by a
%% commercial at-sign ("@") that delimits it from the host.
%%
%%    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
%%
%%
%% [RFC 3986, Chapter 3.2.2. Host]
%%
%% The host subcomponent of authority is identified by an IP literal
%% encapsulated within square brackets, an IPv4 address in dotted-
%% decimal form, or a registered name.
%%
%%    host        = IP-literal / IPv4address / reg-name
%%
%%    IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
%%
%%    IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
%%
%%    IPv6address =                            6( h16 ":" ) ls32
%%                /                       "::" 5( h16 ":" ) ls32
%%                / [               h16 ] "::" 4( h16 ":" ) ls32
%%                / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
%%                / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
%%                / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
%%                / [ *4( h16 ":" ) h16 ] "::"              ls32
%%                / [ *5( h16 ":" ) h16 ] "::"              h16
%%                / [ *6( h16 ":" ) h16 ] "::"
%%
%%    ls32        = ( h16 ":" h16 ) / IPv4address
%%                ; least-significant 32 bits of address
%%
%%    h16         = 1*4HEXDIG
%%                ; 16 bits of address represented in hexadecimal
%%
%%    IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
%%
%%    dec-octet   = DIGIT                 ; 0-9
%%                / %x31-39 DIGIT         ; 10-99
%%                / "1" 2DIGIT            ; 100-199
%%                / "2" %x30-34 DIGIT     ; 200-249
%%                / "25" %x30-35          ; 250-255
%%
%%    reg-name    = *( unreserved / pct-encoded / sub-delims )
%%
%%
%% [RFC 3986, Chapter 3.2.2. Port]
%%
%% The port subcomponent of authority is designated by an optional port
%% number in decimal following the host and delimited from it by a
%% single colon (":") character.
%%
%%    port        = *DIGIT
%%
%%
%% [RFC 3986, Chapter 3.3. Path]
%%
%% The path component contains data, usually organized in hierarchical
%% form, that, along with data in the non-hierarchical query component
%% (Section 3.4), serves to identify a resource within the scope of the
%% URI's scheme and naming authority (if any).  The path is terminated
%% by the first question mark ("?") or number sign ("#") character, or
%% by the end of the URI.
%%
%%    path          = path-abempty    ; begins with "/" or is empty
%%                  / path-absolute   ; begins with "/" but not "//"
%%                  / path-noscheme   ; begins with a non-colon segment
%%                  / path-rootless   ; begins with a segment
%%                  / path-empty      ; zero characters
%%
%%    path-abempty  = *( "/" segment )
%%    path-absolute = "/" [ segment-nz *( "/" segment ) ]
%%    path-noscheme = segment-nz-nc *( "/" segment )
%%    path-rootless = segment-nz *( "/" segment )
%%    path-empty    = 0<pchar>
%%    segment       = *pchar
%%    segment-nz    = 1*pchar
%%    segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
%%                  ; non-zero-length segment without any colon ":"
%%
%%    pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
%%
%%
%% [RFC 3986, Chapter 3.4. Query]
%%
%% The query component contains non-hierarchical data that, along with
%% data in the path component (Section 3.3), serves to identify a
%% resource within the scope of the URI's scheme and naming authority
%% (if any).  The query component is indicated by the first question
%% mark ("?") character and terminated by a number sign ("#") character
%% or by the end of the URI.
%%
%%    query       = *( pchar / "/" / "?" )
%%
%%
%% [RFC 3986, Chapter 3.5. Fragment]
%%
%% The fragment identifier component of a URI allows indirect
%% identification of a secondary resource by reference to a primary
%% resource and additional identifying information.
%%
%%    fragment    = *( pchar / "/" / "?" )
%%
%%
%% [RFC 3986, Chapter 4.1. URI Reference]
%%
%% URI-reference is used to denote the most common usage of a resource
%% identifier.
%%
%%    URI-reference = URI / relative-ref
%%
%%
%% [RFC 3986, Chapter 4.2. Relative Reference]
%%
%% A relative reference takes advantage of the hierarchical syntax
%% (Section 1.2.3) to express a URI reference relative to the name space
%% of another hierarchical URI.
%%
%%    relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
%%
%%    relative-part = "//" authority path-abempty
%%                  / path-absolute
%%                  / path-noscheme
%%                  / path-empty
%%
%%
%% [RFC 3986, Chapter 4.3. Absolute URI]
%%
%% Some protocol elements allow only the absolute form of a URI without
%% a fragment identifier.  For example, defining a base URI for later
%% use by relative references calls for an absolute-URI syntax rule that
%% does not allow a fragment.
%%
%%    absolute-URI  = scheme ":" hier-part [ "?" query ]
%%
-module(uri_string).
-moduledoc """
URI processing functions.

This module contains functions for parsing and handling URIs
([RFC 3986](https://www.ietf.org/rfc/rfc3986.txt)) and form-urlencoded query
strings ([HTML 5.2](https://www.w3.org/TR/html52/)).

Parsing and serializing non-UTF-8 form-urlencoded query strings are also
supported ([HTML 5.0](https://www.w3.org/TR/html50/)).

A URI is an identifier consisting of a sequence of characters matching the
syntax rule named _URI_ in [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt).

The generic URI syntax consists of a hierarchical sequence of components
referred to as the scheme, authority, path, query, and fragment:

```text
    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
    hier-part   = "//" authority path-abempty
                   / path-absolute
                   / path-rootless
                   / path-empty
    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    authority   = [ userinfo "@" ] host [ ":" port ]
    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

    reserved    = gen-delims / sub-delims
    gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
    sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
                / "*" / "+" / "," / ";" / "="

    unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
```

The interpretation of a URI depends only on the characters used and not on how
those characters are represented in a network protocol.

The functions implemented by this module cover the following use cases:

- Parsing URIs into its components and returing a map: `parse/1`
- Recomposing a map of URI components into a URI string: `recompose/1`
- Changing inbound binary and percent-encoding of URIs: `transcode/2`
- Transforming URIs into a normalized form: `normalize/1`, `normalize/2`
- Composing form-urlencoded query strings from a list of key-value pairs:
  `compose_query/1`, `compose_query/2`
- Dissecting form-urlencoded query strings into a list of key-value pairs:
  `dissect_query/1`
- Decoding percent-encoded triplets in URI map or a specific component of URI:
  `percent_decode/1`
- Preparing and retrieving application specific data included in URI
  components:
  `quote/1`, `quote/2`, `unquote/1`

There are four different encodings present during the handling of URIs:

- Inbound binary encoding in binaries
- Inbound percent-encoding in lists and binaries
- Outbound binary encoding in binaries
- Outbound percent-encoding in lists and binaries

Functions with `t:uri_string/0` argument accept lists, binaries and mixed lists
(lists with binary elements) as input type. All of the functions but
[`transcode/2`](`transcode/2`) expects input as lists of unicode codepoints,
UTF-8 encoded binaries and UTF-8 percent-encoded URI parts ("%C3%B6" corresponds
to the unicode character "ö").

Unless otherwise specified the return value type and encoding are the same as
the input type and encoding. That is, binary input returns binary output, list
input returns a list output but mixed input returns list output.

In case of lists there is only percent-encoding. In binaries, however, both
binary encoding and percent-encoding shall be considered.
[`transcode/2`](`transcode/2`) provides the means to convert between the
supported encodings, it takes a `t:uri_string/0` and a list of options
specifying inbound and outbound encodings.

[RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) does not mandate any specific
character encoding and it is usually defined by the protocol or surrounding
text. This library takes the same assumption, binary and percent-encoding are
handled as one configuration unit, they cannot be set to different values.

Quoting functions are intended to be used by URI producing application during
component preparation or retrieval phase to avoid conflicts between data and
characters used in URI syntax. Quoting functions use percent encoding, but with
different rules than for example during execution of
[`recompose/1`](`recompose/1`). It is user responsibility to provide quoting
functions with application data only and using their output to combine an URI
component.  
Quoting functions can for instance be used for constructing a path component
with a segment containing '/' character which should not collide with '/' used
as general delimiter in path component.
""".
-moduledoc(#{since => "OTP 21.0"}).

%%-------------------------------------------------------------------------
%% External API
%%-------------------------------------------------------------------------
-export([allowed_characters/0,
         compose_query/1,
         compose_query/2,
         dissect_query/1,
         normalize/1,
         normalize/2,
         percent_decode/1,
         parse/1,
         recompose/1,
         resolve/2,
         resolve/3,
         transcode/2,
         quote/1,
         quote/2,
         unquote/1]).
-export_type([error/0,
              uri_map/0,
              uri_string/0]).


%%-------------------------------------------------------------------------
%% Internal API
%%-------------------------------------------------------------------------
-export([is_host/1, is_path/1]).  % suppress warnings


%%-------------------------------------------------------------------------
%% Macros
%%-------------------------------------------------------------------------
-define(CHAR(Char), <<Char/utf8>>).
-define(STRING_EMPTY, <<>>).
-define(STRING(MatchStr), <<MatchStr/binary>>).
-define(STRING_REST(MatchStr, Rest), <<MatchStr/utf8, Rest/binary>>).

-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
        end).

-define(HEX2DEC(X),
        if ((X) >= $0) andalso ((X) =< $9) -> (X) - $0;
           ((X) >= $A) andalso ((X) =< $F) -> (X) - $A + 10;
           ((X) >= $a) andalso ((X) =< $f) -> (X) - $a + 10
        end).


%%%=========================================================================
%%%  API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% URI compliant with RFC 3986
%% ASCII %x21 - %x7A ("!" - "z") except
%%   %x34    "    double quote
%%   %x60    <    less than
%%   %x62    >    greater than
%%   %x92    \    backslash
%%   %x94    ^    caret / circumflex
%%   %x96    `    grave / accent
%%-------------------------------------------------------------------------
-doc """
List of unicode codepoints, a UTF-8 encoded binary, or a mix of the two,
representing an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant URI
(_percent-encoded form_). A URI is a sequence of characters from a very limited
set: the letters of the basic Latin alphabet, digits, and a few special
characters.
""".
-type uri_string() :: iodata().
-doc """
Error tuple indicating the type of error. Possible values of the second
component:

- `invalid_character`
- `invalid_encoding`
- `invalid_input`
- `invalid_map`
- `invalid_percent_encoding`
- `invalid_scheme`
- `invalid_uri`
- `invalid_utf8`
- `missing_value`

The third component is a term providing additional information about the cause
of the error.
""".
-type error() :: {error, atom(), term()}.


%%-------------------------------------------------------------------------
%% RFC 3986, Chapter 3. Syntax Components
%%-------------------------------------------------------------------------
-doc "Map holding the main components of a URI.".
-type uri_map() ::
  #{fragment => unicode:chardata(),
    host => unicode:chardata(),
    path => unicode:chardata(),
    port => non_neg_integer() | undefined,
    query => unicode:chardata(),
    scheme => unicode:chardata(),
    userinfo => unicode:chardata()}.


%%-------------------------------------------------------------------------
%% Normalize URIs
%%-------------------------------------------------------------------------
-doc """
Transforms an `URI` into a normalized form using Syntax-Based Normalization as
defined by [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt).

This function implements case normalization, percent-encoding normalization,
path segment normalization and scheme based normalization for HTTP(S) with basic
support for FTP, SSH, SFTP and TFTP.

_Example:_

```erlang
1> uri_string:normalize("/a/b/c/./../../g").
"/a/g"
2> uri_string:normalize(<<"mid/content=5/../6">>).
<<"mid/6">>
3> uri_string:normalize("http://localhost:80").
"http://localhost/"
4> uri_string:normalize(#{scheme => "http",port => 80,path => "/a/b/c/./../../g",
4> host => "localhost-örebro"}).
"http://localhost-%C3%B6rebro/a/g"
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec normalize(URI) -> NormalizedURI when
      URI :: uri_string() | uri_map(),
      NormalizedURI :: uri_string()
                     | error().
normalize(URIMap) ->
    normalize(URIMap, []).


-doc """
Same as [`normalize/1`](`normalize/1`) but with an additional `Options`
parameter, that controls whether the normalized URI shall be returned as an
uri_map().

There is one supported option: `return_map`.

_Example:_

```erlang
1> uri_string:normalize("/a/b/c/./../../g", [return_map]).
#{path => "/a/g"}
2> uri_string:normalize(<<"mid/content=5/../6">>, [return_map]).
#{path => <<"mid/6">>}
3> uri_string:normalize("http://localhost:80", [return_map]).
#{scheme => "http",path => "/",host => "localhost"}
4> uri_string:normalize(#{scheme => "http",port => 80,path => "/a/b/c/./../../g",
4> host => "localhost-örebro"}, [return_map]).
#{scheme => "http",path => "/a/g",host => "localhost-örebro"}
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec normalize(URI, Options) -> NormalizedURI when
      URI :: uri_string() | uri_map(),
      Options :: [return_map],
      NormalizedURI :: uri_string() | uri_map()
                     | error().
normalize(URIMap, []) when is_map(URIMap) ->
    try recompose(normalize_map(URIMap))
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
normalize(URIMap, [return_map]) when is_map(URIMap) ->
    try normalize_map(URIMap)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
normalize(URIString, []) ->
    case parse(URIString) of
        Value when is_map(Value) ->
            try recompose(normalize_map(Value))
            catch
                throw:{error, Atom, RestData} -> {error, Atom, RestData}
            end;
        Error ->
            Error
    end;
normalize(URIString, [return_map]) ->
    case parse(URIString) of
        Value when is_map(Value) ->
            try normalize_map(Value)
            catch
                throw:{error, Atom, RestData} -> {error, Atom, RestData}
            end;
        Error ->
            Error
    end.


%%-------------------------------------------------------------------------
%% Parse URIs
%%-------------------------------------------------------------------------
-doc """
Parses an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant
`t:uri_string/0` into a `t:uri_map/0`, that holds the parsed components of the
`URI`. If parsing fails, an error tuple is returned.

See also the opposite operation `recompose/1`.

_Example:_

```erlang
1> uri_string:parse("foo://user@example.com:8042/over/there?name=ferret#nose").
#{fragment => "nose",host => "example.com",
  path => "/over/there",port => 8042,query => "name=ferret",
  scheme => foo,userinfo => "user"}
2> uri_string:parse(<<"foo://user@example.com:8042/over/there?name=ferret">>).
#{host => <<"example.com">>,path => <<"/over/there">>,
  port => 8042,query => <<"name=ferret">>,scheme => <<"foo">>,
  userinfo => <<"user">>}
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec parse(URIString) -> URIMap when
      URIString :: uri_string(),
      URIMap :: uri_map()
              | error().
parse(URIString) when is_binary(URIString) ->
    try parse_uri_reference(URIString, #{})
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
parse(URIString) when is_list(URIString) ->
    try
        Binary = unicode:characters_to_binary(URIString),
        Map = parse_uri_reference(Binary, #{}),
        convert_mapfields_to_list(Map)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end.


%%-------------------------------------------------------------------------
%% Recompose URIs
%%-------------------------------------------------------------------------
-doc """
Creates an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant
`URIString` (percent-encoded), based on the components of `URIMap`. If the
`URIMap` is invalid, an error tuple is returned.

See also the opposite operation `parse/1`.

_Example:_

```erlang
1> URIMap = #{fragment => "nose", host => "example.com", path => "/over/there",
1> port => 8042, query => "name=ferret", scheme => "foo", userinfo => "user"}.
#{fragment => "nose",host => "example.com",
  path => "/over/there",port => 8042,query => "name=ferret",
  scheme => "foo",userinfo => "user"}

2> uri_string:recompose(URIMap).
"foo://example.com:8042/over/there?name=ferret#nose"
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec recompose(URIMap) -> URIString when
      URIMap :: uri_map(),
      URIString :: uri_string()
                 | error().
recompose(Map) ->
    case is_valid_map(Map) of
        false ->
            {error, invalid_map, Map};
        true ->
            try
                T0 = update_scheme(Map, empty),
                T1 = update_userinfo(Map, T0),
                T2 = update_host(Map, T1),
                T3 = update_port(Map, T2),
                T4 = update_path(Map, T3),
                T5 = update_query(Map, T4),
                update_fragment(Map, T5)
            catch
                throw:{error, Atom, RestData} -> {error, Atom, RestData}
            end
    end.


%%-------------------------------------------------------------------------
%% Resolve URIs
%%-------------------------------------------------------------------------
-doc """
Convert a `RefURI` reference that might be relative to a given base URI into the
parsed components of the reference's target, which can then be recomposed to
form the target URI.

_Example:_

```erlang
1> uri_string:resolve("/abs/ol/ute", "http://localhost/a/b/c?q").
"http://localhost/abs/ol/ute"
2> uri_string:resolve("../relative", "http://localhost/a/b/c?q").
"http://localhost/a/relative"
3> uri_string:resolve("http://localhost/full", "http://localhost/a/b/c?q").
"http://localhost/full"
4> uri_string:resolve(#{path => "path", query => "xyz"}, "http://localhost/a/b/c?q").
"http://localhost/a/b/path?xyz"
```
""".
-doc(#{since => <<"OTP 22.3">>}).
-spec resolve(RefURI, BaseURI) -> TargetURI when
      RefURI :: uri_string() | uri_map(),
      BaseURI :: uri_string() | uri_map(),
      TargetURI :: uri_string()
                 | error().
resolve(URIMap, BaseURIMap) ->
    resolve(URIMap, BaseURIMap, []).


-doc """
Same as [`resolve/2`](`resolve/2`) but with an additional `Options` parameter,
that controls whether the target URI shall be returned as an uri_map(). There is
one supported option: `return_map`.

_Example:_

```erlang
1> uri_string:resolve("/abs/ol/ute", "http://localhost/a/b/c?q", [return_map]).
#{host => "localhost",path => "/abs/ol/ute",scheme => "http"}
2> uri_string:resolve(#{path => "/abs/ol/ute"}, #{scheme => "http",
2> host => "localhost", path => "/a/b/c?q"}, [return_map]).
#{host => "localhost",path => "/abs/ol/ute",scheme => "http"}
```
""".
-doc(#{since => <<"OTP 22.3">>}).
-spec resolve(RefURI, BaseURI, Options) -> TargetURI when
      RefURI :: uri_string() | uri_map(),
      BaseURI :: uri_string() | uri_map(),
      Options :: [return_map],
      TargetURI :: uri_string() | uri_map()
                 | error().
resolve(URIMap, BaseURIMap, Options) when is_map(URIMap) ->
    case resolve_map(URIMap, BaseURIMap) of
        TargetURIMap when is_map(TargetURIMap) ->
            case Options of
                [return_map] ->
                    TargetURIMap;
                [] ->
                    recompose(TargetURIMap)
            end;
        Error ->
            Error
    end;
resolve(URIString, BaseURIMap, Options) ->
    case parse(URIString) of
        URIMap when is_map(URIMap) ->
            resolve(URIMap, BaseURIMap, Options);
        Error ->
            Error
    end.


%%-------------------------------------------------------------------------
%% Transcode URIs
%%-------------------------------------------------------------------------
-doc """
Transcodes an [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) compliant
`URIString`, where `Options` is a list of tagged tuples, specifying the inbound
(`in_encoding`) and outbound (`out_encoding`) encodings.

`in_encoding` and `out_encoding` specifies both binary encoding and percent-encoding
for the input and output data. Mixed encoding, where binary encoding is not the same as
percent-encoding, is not supported. If an argument is invalid, an error tuple is
returned.

_Example:_

```erlang
1> uri_string:transcode(<<"foo%00%00%00%F6bar"/utf32>>,
1> [{in_encoding, utf32},{out_encoding, utf8}]).
<<"foo%C3%B6bar"/utf8>>
2> uri_string:transcode("foo%F6bar", [{in_encoding, latin1},
2> {out_encoding, utf8}]).
"foo%C3%B6bar"
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec transcode(URIString, Options) -> Result when
      URIString :: uri_string(),
      Options :: [{in_encoding, unicode:encoding()}|{out_encoding, unicode:encoding()}],
      Result :: uri_string()
              | error().
transcode(URIString, Options) when is_binary(URIString) ->
    try
        InEnc = proplists:get_value(in_encoding, Options, utf8),
        OutEnc = proplists:get_value(out_encoding, Options, utf8),
        List = convert_to_list(URIString, InEnc),
        Output = transcode(List, [], InEnc, OutEnc),
        convert_to_binary(Output, utf8, OutEnc)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
transcode(URIString, Options) when is_list(URIString) ->
    InEnc = proplists:get_value(in_encoding, Options, utf8),
    OutEnc = proplists:get_value(out_encoding, Options, utf8),
    Flattened = flatten_list(URIString, InEnc),
    try transcode(Flattened, [], InEnc, OutEnc)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end.


%%-------------------------------------------------------------------------
%% Misc
%%-------------------------------------------------------------------------
-doc """
This is a utility function meant to be used in the shell for printing the
allowed characters in each major URI component, and also in the most important
characters sets.

Note that this function does not replace the ABNF rules defined by the standards,
these character sets are derived directly from those aformentioned rules. For more
information see the
[Uniform Resource Identifiers](uri_string_usage.md#percent_encoding) chapter in
stdlib's Users Guide.
""".
-doc(#{since => <<"OTP 23.2">>}).
-spec allowed_characters() -> [{atom(), list()}].
allowed_characters() ->
    Input = lists:seq(0,127),
    Scheme = lists:filter(fun is_scheme/1, Input),
    UserInfo = lists:filter(fun is_userinfo/1, Input),
    Host = lists:filter(fun is_host/1, Input),
    IPv4 = lists:filter(fun is_ipv4/1, Input),
    IPv6 = lists:filter(fun is_ipv6/1, Input),
    RegName = lists:filter(fun is_reg_name/1, Input),
    Path = lists:filter(fun is_path/1, Input),
    Query = lists:filter(fun is_query/1, Input),
    Fragment = lists:filter(fun is_fragment/1, Input),
    Reserved = lists:filter(fun is_reserved/1, Input),
    Unreserved = lists:filter(fun is_unreserved/1, Input),
    [{scheme, Scheme},
     {userinfo, UserInfo},
     {host, Host},
     {ipv4, IPv4},
     {ipv6, IPv6},
     {regname,RegName},
     {path,Path},
     {query, Query},
     {fragment,Fragment},
     {reserved, Reserved},
     {unreserved, Unreserved}].

-doc """
Decodes all percent-encoded triplets in the input that can be both a
`t:uri_string/0` and a `t:uri_map/0`.

Note, that this function performs raw decoding and it shall be used on already
parsed URI components. Applying this function directly on a standard URI can
effectively change it.

If the input encoding is not UTF-8, an error tuple is returned.

_Example:_

```erlang
1> uri_string:percent_decode(#{host => "localhost-%C3%B6rebro",path => [],
1> scheme => "http"}).
#{host => "localhost-örebro",path => [],scheme => "http"}
2> uri_string:percent_decode(<<"%C3%B6rebro">>).
<<"örebro"/utf8>>
```

> #### Warning {: .warning }
>
> Using `uri_string:percent_decode/1` directly on a URI is not safe. This
> example shows, that after each consecutive application of the function the
> resulting URI will be changed. None of these URIs refer to the same resource.
>
> ```erlang
> 3> uri_string:percent_decode(<<"http://local%252Fhost/path">>).
> <<"http://local%2Fhost/path">>
> 4> uri_string:percent_decode(<<"http://local%2Fhost/path">>).
> <<"http://local/host/path">>
> ```
""".
-doc(#{since => <<"OTP 23.2">>}).
-spec percent_decode(URI) -> Result when
      URI :: uri_string() | uri_map(),
      Result :: uri_string() |
                uri_map() |
                {error, {invalid, {atom(), {term(), term()}}}} | error().
percent_decode(URIMap) when is_map(URIMap)->
    Fun = fun (K,V) when K =:= userinfo; K =:= host; K =:= path;
                         K =:= query; K =:= fragment ->
                  case raw_decode(V) of
                      {error, Reason, Input} ->
                          throw({error, {invalid, {K, {Reason, Input}}}});
                      Else ->
                          Else
                  end;
              %% Handle port and scheme
              (_,V) ->
                  V
          end,
    try maps:map(Fun, URIMap)
    catch throw:Return ->
            Return
    end;
percent_decode(URI) when is_list(URI) orelse
                         is_binary(URI) ->
    raw_decode(URI).

-doc """
Replaces characters out of unreserved set with their percent encoded
equivalents.

Unreserved characters defined in
[RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) are not quoted.

_Example:_

```erlang
1> uri_string:quote("SomeId/04").
"SomeId%2F04"
2> uri_string:quote(<<"SomeId/04">>).
<<"SomeId%2F04">>
```

> #### Warning {: .warning }
>
> Function is not aware about any URI component context and should not be used
> on whole URI. If applied more than once on the same data, might produce
> unexpected results.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec quote(Data) -> QuotedData when
      Data :: unicode:chardata(),
      QuotedData :: unicode:chardata().
quote(D) ->
    encode(D, fun is_unreserved/1).

-doc """
Same as [`quote/1`](`quote/1`), but `Safe` allows user to provide a list of
characters to be protected from encoding.

_Example:_

```erlang
1> uri_string:quote("SomeId/04", "/").
"SomeId/04"
2> uri_string:quote(<<"SomeId/04">>, "/").
<<"SomeId/04">>
```

> #### Warning {: .warning }
>
> Function is not aware about any URI component context and should not be used
> on whole URI. If applied more than once on the same data, might produce
> unexpected results.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec quote(Data, Safe) -> QuotedData when
      Data :: unicode:chardata(),
      Safe :: string(),
      QuotedData :: unicode:chardata().
quote(D, Safe) ->
    UnreservedOrSafe =
        fun(C) ->
                is_unreserved(C) orelse lists:member(C, Safe)
        end,
    encode(D, UnreservedOrSafe).

-doc """
Percent decode characters.

_Example:_

```erlang
1> uri_string:unquote("SomeId%2F04").
"SomeId/04"
2> uri_string:unquote(<<"SomeId%2F04">>).
<<"SomeId/04">>
```

> #### Warning {: .warning }
>
> Function is not aware about any URI component context and should not be used
> on whole URI. If applied more than once on the same data, might produce
> unexpected results.
""".
-doc(#{since => <<"OTP 25.0">>}).
-spec unquote(QuotedData) -> Data when
      QuotedData :: unicode:chardata(),
      Data :: unicode:chardata().
unquote(D) ->
    raw_decode(D).

%%-------------------------------------------------------------------------
%% Functions for working with the query part of a URI as a list
%% of key/value pairs.
%% HTML 5.2 - 4.10.21.6 URL-encoded form data - WHATWG URL (10 Jan 2018) - UTF-8
%% HTML 5.0 - 4.10.22.6 URL-encoded form data - non UTF-8
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Compose urlencoded query string from a list of unescaped key/value pairs.
%% (application/x-www-form-urlencoded encoding algorithm)
%%-------------------------------------------------------------------------
-doc """
Composes a form-urlencoded `QueryString` based on a `QueryList`, a list of
non-percent-encoded key-value pairs.

Form-urlencoding is defined in section 4.10.21.6 of the [HTML 5.2](https://www.w3.org/TR/html52/)
specification and in section 4.10.22.6 of the [HTML 5.0](https://www.w3.org/TR/html50/)
specification for non-UTF-8 encodings.

See also the opposite operation `dissect_query/1`.

_Example:_

```erlang
1> uri_string:compose_query([{"foo bar","1"},{"city","örebro"}]).
"foo+bar=1&city=%C3%B6rebro"
2> uri_string:compose_query([{<<"foo bar">>,<<"1">>},
2> {<<"city">>,<<"örebro"/utf8>>}]).
<<"foo+bar=1&city=%C3%B6rebro">>
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec compose_query(QueryList) -> QueryString when
      QueryList :: [{unicode:chardata(), unicode:chardata() | true}],
      QueryString :: uri_string()
                   | error().
compose_query(List) ->
    compose_query(List, [{encoding, utf8}]).


-doc """
Same as [`compose_query/1`](`compose_query/1`) but with an additional `Options`
parameter, that controls the encoding ("charset") used by the encoding
algorithm.

There are two supported encodings: `utf8` (or `unicode`) and `latin1`.

Each character in the entry's name and value that cannot be expressed using the
selected character encoding, is replaced by a string consisting of a U+0026
AMPERSAND character (&), a "#" (U+0023) character, one or more ASCII digits
representing the Unicode code point of the character in base ten, and finally a
";" (U+003B) character.

Bytes that are out of the range 0x2A, 0x2D, 0x2E, 0x30 to 0x39, 0x41 to 0x5A,
0x5F, 0x61 to 0x7A, are percent-encoded (U+0025 PERCENT SIGN character (%)
followed by uppercase ASCII hex digits representing the hexadecimal value of the
byte).

See also the opposite operation `dissect_query/1`.

_Example:_

```erlang
1> uri_string:compose_query([{"foo bar","1"},{"city","örebro"}],
1> [{encoding, latin1}]).
"foo+bar=1&city=%F6rebro"
2> uri_string:compose_query([{<<"foo bar">>,<<"1">>},
2> {<<"city">>,<<"東京"/utf8>>}], [{encoding, latin1}]).
<<"foo+bar=1&city=%26%2326481%3B%26%2320140%3B">>
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec compose_query(QueryList, Options) -> QueryString when
      QueryList :: [{unicode:chardata(), unicode:chardata() | true}],
      Options :: [{encoding, atom()}],
      QueryString :: uri_string()
                   | error().
compose_query([],_Options) ->
    [];
compose_query(List, Options) ->
    try compose_query(List, Options, false, <<>>)
    catch
      throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end.
%%
compose_query([{Key,true}|Rest], Options, IsList, Acc) ->
    Separator = get_separator(Rest),
    K = form_urlencode(Key, Options),
    IsListNew = IsList orelse is_list(Key),
    compose_query(Rest, Options, IsListNew, <<Acc/binary,K/binary,Separator/binary>>);
compose_query([{Key,Value}|Rest], Options, IsList, Acc) ->
    Separator = get_separator(Rest),
    K = form_urlencode(Key, Options),
    V = form_urlencode(Value, Options),
    IsListNew = IsList orelse is_list(Key) orelse is_list(Value),
    compose_query(Rest, Options, IsListNew, <<Acc/binary,K/binary,"=",V/binary,Separator/binary>>);
compose_query([], _Options, IsList, Acc) ->
    case IsList of
        true -> convert_to_list(Acc, utf8);
        false -> Acc
    end.


%%-------------------------------------------------------------------------
%% Dissect a query string into a list of unescaped key/value pairs.
%% (application/x-www-form-urlencoded decoding algorithm)
%%-------------------------------------------------------------------------
-doc """
Dissects an urlencoded `QueryString` and returns a `QueryList`, a list of
non-percent-encoded key-value pairs.

Form-urlencoding is defined in section 4.10.21.6 of the [HTML 5.2](https://www.w3.org/TR/html52/)
specification and in section 4.10.22.6 of the [HTML 5.0](https://www.w3.org/TR/html50/)
specification for non-UTF-8 encodings.

See also the opposite operation `compose_query/1`.

_Example:_

```erlang
1> uri_string:dissect_query("foo+bar=1&city=%C3%B6rebro").
[{"foo bar","1"},{"city","örebro"}]
2> uri_string:dissect_query(<<"foo+bar=1&city=%26%2326481%3B%26%2320140%3B">>).
[{<<"foo bar">>,<<"1">>},
 {<<"city">>,<<230,157,177,228,186,172>>}]
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec dissect_query(QueryString) -> QueryList when
      QueryString :: uri_string(),
      QueryList :: [{unicode:chardata(), unicode:chardata() | true}]
                 | error().
dissect_query(<<>>) ->
    [];
dissect_query([]) ->
    [];
dissect_query(QueryString) when is_list(QueryString) ->
    try
        B = convert_to_binary(QueryString, utf8, utf8),
        dissect_query_key(B, true, [], <<>>, <<>>)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end;
dissect_query(QueryString) ->
    try dissect_query_key(QueryString, false, [], <<>>, <<>>)
    catch
        throw:{error, Atom, RestData} -> {error, Atom, RestData}
    end.


%%%========================================================================
%%% Internal functions
%%%========================================================================

%%-------------------------------------------------------------------------
%% Converts Map fields to lists
%%-------------------------------------------------------------------------
convert_mapfields_to_list(Map) ->
    Fun = fun (_, V) when is_binary(V) -> unicode:characters_to_list(V);
              (_, V) -> V end,
    maps:map(Fun, Map).


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 4.1. URI Reference]
%%
%% URI-reference is used to denote the most common usage of a resource
%% identifier.
%%
%%    URI-reference = URI / relative-ref
%%-------------------------------------------------------------------------
-spec parse_uri_reference(binary(), uri_map()) -> uri_map().
parse_uri_reference(<<>>, _) -> #{path => <<>>};
parse_uri_reference(URIString, URI) ->
    try parse_scheme_start(URIString, URI)
    catch
        throw:{_,_,_} ->
            parse_relative_part(URIString, URI)
    end.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 4.2. Relative Reference]
%%
%% A relative reference takes advantage of the hierarchical syntax
%% (Section 1.2.3) to express a URI reference relative to the name space
%% of another hierarchical URI.
%%
%%    relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
%%
%%    relative-part = "//" authority path-abempty
%%                  / path-absolute
%%                  / path-noscheme
%%                  / path-empty
%%-------------------------------------------------------------------------
-spec parse_relative_part(binary(), uri_map()) -> uri_map().
parse_relative_part(?STRING_REST("//", Rest), URI) ->
    %% Parse userinfo - "//" is NOT part of authority
    try parse_userinfo(Rest, URI) of
        {T, URI1} ->
            Userinfo = calculate_parsed_userinfo(Rest, T),
            URI2 = maybe_add_path(URI1),
            URI2#{userinfo => Userinfo}
    catch
        throw:{_,_,_} ->
            {T, URI1} = parse_host(Rest, URI),
            Host = calculate_parsed_host_port(Rest, T),
            URI2 = maybe_add_path(URI1),
            URI2#{host => remove_brackets(Host)}
    end;
parse_relative_part(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    Path = calculate_parsed_part(Rest, T),
    URI1#{path => ?STRING_REST($/, Path)};
parse_relative_part(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    URI2 = maybe_add_path(URI1),
    URI2#{query => Query};
parse_relative_part(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    URI2 = maybe_add_path(URI1),
    URI2#{fragment => Fragment};
parse_relative_part(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true ->
            {T, URI1} = parse_segment_nz_nc(Rest, URI),  % path-noscheme
            Path = calculate_parsed_part(Rest, T),
            URI1#{path => ?STRING_REST(Char, Path)};
        false -> throw({error,invalid_uri,[Char]})
    end.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.3. Path]
%%
%% The path component contains data, usually organized in hierarchical
%% form, that, along with data in the non-hierarchical query component
%% (Section 3.4), serves to identify a resource within the scope of the
%% URI's scheme and naming authority (if any).  The path is terminated
%% by the first question mark ("?") or number sign ("#") character, or
%% by the end of the URI.
%%
%%    path          = path-abempty    ; begins with "/" or is empty
%%                  / path-absolute   ; begins with "/" but not "//"
%%                  / path-noscheme   ; begins with a non-colon segment
%%                  / path-rootless   ; begins with a segment
%%                  / path-empty      ; zero characters
%%
%%    path-abempty  = *( "/" segment )
%%    path-absolute = "/" [ segment-nz *( "/" segment ) ]
%%    path-noscheme = segment-nz-nc *( "/" segment )
%%    path-rootless = segment-nz *( "/" segment )
%%    path-empty    = 0<pchar>
%%    segment       = *pchar
%%    segment-nz    = 1*pchar
%%    segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
%%                  ; non-zero-length segment without any colon ":"
%%
%%    pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%%    path-abempty
%%-------------------------------------------------------------------------
-spec parse_segment(binary(), uri_map()) -> {binary(), uri_map()}.
parse_segment(?STRING_REST($/, Rest), URI) ->
    parse_segment(Rest, URI);  % segment
parse_segment(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_segment(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_segment(?STRING_REST(Char, Rest), URI) ->
    case is_pchar(Char) of
        true -> parse_segment(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_segment(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.


%%-------------------------------------------------------------------------
%%    path-noscheme
%%-------------------------------------------------------------------------
-spec parse_segment_nz_nc(binary(), uri_map()) -> {binary(), uri_map()}.
parse_segment_nz_nc(?STRING_REST($/, Rest), URI) ->
    parse_segment(Rest, URI);  % segment
parse_segment_nz_nc(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_segment_nz_nc(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_segment_nz_nc(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true -> parse_segment_nz_nc(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_segment_nz_nc(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.


%% Check if char is pchar.
-spec is_pchar(char()) -> boolean().
is_pchar($%) -> true;  % pct-encoded
is_pchar($:) -> true;
is_pchar($@) -> true;
is_pchar(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).

%% Check if char is segment_nz_nc.
-spec is_segment_nz_nc(char()) -> boolean().
is_segment_nz_nc($%) -> true;  % pct-encoded
is_segment_nz_nc($@) -> true;
is_segment_nz_nc(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.1. Scheme]
%%
%% Each URI begins with a scheme name that refers to a specification for
%% assigning identifiers within that scheme.
%%
%%    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
%%-------------------------------------------------------------------------
-spec parse_scheme_start(binary(), uri_map()) -> uri_map().
parse_scheme_start(?STRING_REST(Char, Rest), URI) ->
    case is_alpha(Char) of
        true  -> {T, URI1} = parse_scheme(Rest, URI),
                 Scheme = calculate_parsed_scheme(Rest, T),
                 URI2 = maybe_add_path(URI1),
		 URI2#{scheme => ?STRING_REST(Char, Scheme)};
        false -> throw({error,invalid_uri,[Char]})
    end.

%% Add path component if it missing after parsing the URI.
%% According to the URI specification there is always a
%% path component in every URI-reference and it can be
%% empty.
maybe_add_path(Map) ->
    case maps:is_key(path, Map) of
        false ->
            Map#{path => <<>>};
        _Else ->
            Map
    end.



-spec parse_scheme(binary(), uri_map()) -> {binary(), uri_map()}.
parse_scheme(?STRING_REST($:, Rest), URI) ->
    {_, URI1} = parse_hier(Rest, URI),
    {Rest, URI1};
parse_scheme(?STRING_REST(Char, Rest), URI) ->
    case is_scheme(Char) of
        true  -> parse_scheme(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_scheme(?STRING_EMPTY, _URI) ->
    throw({error,invalid_uri,<<>>}).


%% Check if char is allowed in scheme
-spec is_scheme(char()) -> boolean().
is_scheme($+) -> true;
is_scheme($-) -> true;
is_scheme($.) -> true;
is_scheme(Char) -> is_alpha(Char) orelse is_digit(Char).


%%-------------------------------------------------------------------------
%%    hier-part   = "//" authority path-abempty
%%                   / path-absolute
%%                   / path-rootless
%%                   / path-empty
%%-------------------------------------------------------------------------
-spec parse_hier(binary(), uri_map()) -> {binary(), uri_map()}.
parse_hier(?STRING_REST("//", Rest), URI) ->
    % Parse userinfo - "//" is NOT part of authority
    try parse_userinfo(Rest, URI) of
        {T, URI1} ->
            Userinfo = calculate_parsed_userinfo(Rest, T),
	    {Rest, URI1#{userinfo => Userinfo}}
    catch
        throw:{_,_,_} ->
            {T, URI1} = parse_host(Rest, URI),
            Host = calculate_parsed_host_port(Rest, T),
	    {Rest, URI1#{host => remove_brackets(Host)}}
    end;
parse_hier(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_hier(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_hier(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_hier(?STRING_REST(Char, Rest), URI) ->  % path-rootless
    case is_pchar(Char) of
        true ->  % segment_nz
            {T, URI1} = parse_segment(Rest, URI),
            Path = calculate_parsed_part(Rest, T),
            {Rest, URI1#{path => ?STRING_REST(Char, Path)}};
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_hier(?STRING_EMPTY, URI) ->
    {<<>>, URI}.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.2. Authority]
%%
%% Many URI schemes include a hierarchical element for a naming
%% authority so that governance of the name space defined by the
%% remainder of the URI is delegated to that authority (which may, in
%% turn, delegate it further).
%%
%% The authority component is preceded by a double slash ("//") and is
%% terminated by the next slash ("/"), question mark ("?"), or number
%% sign ("#") character, or by the end of the URI.
%%
%%    authority   = [ userinfo "@" ] host [ ":" port ]
%%
%%
%% [RFC 3986, Chapter 3.2.1. User Information]
%%
%% The userinfo subcomponent may consist of a user name and, optionally,
%% scheme-specific information about how to gain authorization to access
%% the resource. The user information, if present, is followed by a
%% commercial at-sign ("@") that delimits it from the host.
%%
%%    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
%%-------------------------------------------------------------------------
-spec parse_userinfo(binary(), uri_map()) -> {binary(), uri_map()}.
parse_userinfo(?CHAR($@), URI) ->
    {?STRING_EMPTY, URI#{host => <<>>}};
parse_userinfo(?STRING_REST($@, Rest), URI) ->
    {T, URI1} = parse_host(Rest, URI),
    Host = calculate_parsed_host_port(Rest, T),
    {Rest, URI1#{host => remove_brackets(Host)}};
parse_userinfo(?STRING_REST(Char, Rest), URI) ->
    case is_userinfo(Char) of
        true -> parse_userinfo(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_userinfo(?STRING_EMPTY, _URI) ->
    %% URI cannot end in userinfo state
    throw({error,invalid_uri,<<>>}).


%% Check if char is allowed in userinfo
-spec is_userinfo(char()) -> boolean().
is_userinfo($%) -> true;  % pct-encoded
is_userinfo($:) -> true;
is_userinfo(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.2.2. Host]
%%
%% The host subcomponent of authority is identified by an IP literal
%% encapsulated within square brackets, an IPv4 address in dotted-
%% decimal form, or a registered name.
%%
%%    host        = IP-literal / IPv4address / reg-name
%%
%%    IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
%%
%%    IPvFuture  = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
%%
%%    IPv6address =                            6( h16 ":" ) ls32
%%                /                       "::" 5( h16 ":" ) ls32
%%                / [               h16 ] "::" 4( h16 ":" ) ls32
%%                / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
%%                / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
%%                / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
%%                / [ *4( h16 ":" ) h16 ] "::"              ls32
%%                / [ *5( h16 ":" ) h16 ] "::"              h16
%%                / [ *6( h16 ":" ) h16 ] "::"
%%
%%    ls32        = ( h16 ":" h16 ) / IPv4address
%%                ; least-significant 32 bits of address
%%
%%    h16         = 1*4HEXDIG
%%                ; 16 bits of address represented in hexadecimal
%%
%%    IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
%%
%%    dec-octet   = DIGIT                 ; 0-9
%%                / %x31-39 DIGIT         ; 10-99
%%                / "1" 2DIGIT            ; 100-199
%%                / "2" %x30-34 DIGIT     ; 200-249
%%                / "25" %x30-35          ; 250-255
%%
%%    reg-name    = *( unreserved / pct-encoded / sub-delims )
%%-------------------------------------------------------------------------
-spec parse_host(binary(), uri_map()) -> {binary(), uri_map()}.
parse_host(?STRING_REST($:, Rest), URI) ->
    {T, URI1} = parse_port(Rest, URI),
    H = calculate_parsed_host_port(Rest, T),
    Port = get_port(H),
    {Rest, URI1#{port => Port}};
parse_host(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_host(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_host(?STRING_REST($[, Rest), URI) ->
    parse_ipv6_bin(Rest, [], URI);
parse_host(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_host(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true ->
            try parse_ipv4_bin(Rest, [Char], URI)
            catch
                throw:{_,_,_} ->
                    parse_reg_name(?STRING_REST(Char, Rest), URI)
            end;
        false -> parse_reg_name(?STRING_REST(Char, Rest), URI)
    end;
parse_host(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.


-spec parse_reg_name(binary(), uri_map()) -> {binary(), uri_map()}.
parse_reg_name(?STRING_REST($:, Rest), URI) ->
    {T, URI1} = parse_port(Rest, URI),
    H = calculate_parsed_host_port(Rest, T),
    Port = get_port(H),
    {Rest, URI1#{port => Port}};
parse_reg_name(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_reg_name(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_reg_name(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_reg_name(?STRING_REST(Char, Rest), URI) ->
    case is_reg_name(Char) of
        true -> parse_reg_name(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_reg_name(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

%% Check if char is allowed in reg-name
-spec is_reg_name(char()) -> boolean().
is_reg_name($%) -> true;
is_reg_name(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).


-spec parse_ipv4_bin(binary(), list(), uri_map()) -> {binary(), uri_map()}.
parse_ipv4_bin(?STRING_REST($:, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_port(Rest, URI),
    H = calculate_parsed_host_port(Rest, T),
    Port = get_port(H),
    {Rest, URI1#{port => Port}};
parse_ipv4_bin(?STRING_REST($/, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_ipv4_bin(?STRING_REST($?, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_ipv4_bin(?STRING_REST($#, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_ipv4_bin(?STRING_REST(Char, Rest), Acc, URI) ->
    case is_ipv4(Char) of
        true -> parse_ipv4_bin(Rest, [Char|Acc], URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_ipv4_bin(?STRING_EMPTY, Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {?STRING_EMPTY, URI}.


%% Check if char is allowed in IPv4 addresses
-spec is_ipv4(char()) -> boolean().
is_ipv4($.) -> true;
is_ipv4(Char) -> is_digit(Char).

-spec validate_ipv4_address(list()) -> list().
validate_ipv4_address(Addr) ->
    case inet:parse_ipv4strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw({error,invalid_uri,Addr})
    end.


-spec parse_ipv6_bin(binary(), list(), uri_map()) -> {binary(), uri_map()}.
parse_ipv6_bin(?STRING_REST($], Rest), Acc, URI) ->
    _ = validate_ipv6_address(lists:reverse(Acc)),
    parse_ipv6_bin_end(Rest, URI);
parse_ipv6_bin(?STRING_REST(Char, Rest), Acc, URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6_bin(Rest, [Char|Acc], URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_ipv6_bin(?STRING_EMPTY, _Acc, _URI) ->
    throw({error,invalid_uri,<<>>}).

%% Check if char is allowed in IPv6 addresses
-spec is_ipv6(char()) -> boolean().
is_ipv6($:) -> true;
is_ipv6($.) -> true;
is_ipv6(Char) -> is_hex_digit(Char).


-spec parse_ipv6_bin_end(binary(), uri_map()) -> {binary(), uri_map()}.
parse_ipv6_bin_end(?STRING_REST($:, Rest), URI) ->
    {T, URI1} = parse_port(Rest, URI),
    H = calculate_parsed_host_port(Rest, T),
    Port = get_port(H),
    {Rest, URI1#{port => Port}};
parse_ipv6_bin_end(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_ipv6_bin_end(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_ipv6_bin_end(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_ipv6_bin_end(?STRING_REST(Char, Rest), URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6_bin_end(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_ipv6_bin_end(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec validate_ipv6_address(list()) -> list().
validate_ipv6_address(Addr) ->
    case inet:parse_ipv6strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw({error,invalid_uri,Addr})
    end.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.2.2. Port]
%%
%% The port subcomponent of authority is designated by an optional port
%% number in decimal following the host and delimited from it by a
%% single colon (":") character.
%%
%%    port        = *DIGIT
%%-------------------------------------------------------------------------
-spec parse_port(binary(), uri_map()) -> {binary(), uri_map()}.
parse_port(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_port(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => Query}};
parse_port(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_port(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true -> parse_port(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_port(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.4. Query]
%%
%% The query component contains non-hierarchical data that, along with
%% data in the path component (Section 3.3), serves to identify a
%% resource within the scope of the URI's scheme and naming authority
%% (if any).  The query component is indicated by the first question
%% mark ("?") character and terminated by a number sign ("#") character
%% or by the end of the URI.
%%
%%    query       = *( pchar / "/" / "?" )
%%-------------------------------------------------------------------------
-spec parse_query(binary(), uri_map()) -> {binary(), uri_map()}.
parse_query(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => Fragment}};
parse_query(?STRING_REST(Char, Rest), URI) ->
    case is_query(Char) of
        true -> parse_query(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_query(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.


%% Check if char is allowed in query
-spec is_query(char()) -> boolean().
is_query($/) -> true;
is_query($?) -> true;
is_query(Char) -> is_pchar(Char).


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 3.5. Fragment]
%%
%% The fragment identifier component of a URI allows indirect
%% identification of a secondary resource by reference to a primary
%% resource and additional identifying information.
%%
%%    fragment    = *( pchar / "/" / "?" )
%%-------------------------------------------------------------------------
-spec parse_fragment(binary(), uri_map()) -> {binary(), uri_map()}.
parse_fragment(?STRING_REST(Char, Rest), URI) ->
    case is_fragment(Char) of
        true -> parse_fragment(Rest, URI);
        false -> throw({error,invalid_uri,[Char]})
    end;
parse_fragment(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.


%% Check if char is allowed in fragment
-spec is_fragment(char()) -> boolean().
is_fragment($/) -> true;
is_fragment($?) -> true;
is_fragment(Char) -> is_pchar(Char).


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 2.2. Reserved Characters]
%%
%%   reserved    = gen-delims / sub-delims
%%
%%   gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
%%
%%   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
%%               / "*" / "+" / "," / ";" / "="
%%
%%-------------------------------------------------------------------------

%% Return true if input char is reserved.
-spec is_reserved(char()) -> boolean().
is_reserved($:) -> true;
is_reserved($/) -> true;
is_reserved($?) -> true;
is_reserved($#) -> true;
is_reserved($[) -> true;
is_reserved($]) -> true;
is_reserved($@) -> true;

is_reserved($!) -> true;
is_reserved($$) -> true;
is_reserved($&) -> true;
is_reserved($') -> true;
is_reserved($() -> true;
is_reserved($)) -> true;

is_reserved($*) -> true;
is_reserved($+) -> true;
is_reserved($,) -> true;
is_reserved($;) -> true;
is_reserved($=) -> true;
is_reserved(_) -> false.


%% Check if char is sub-delim.
-spec is_sub_delim(char()) -> boolean().
is_sub_delim($!) -> true;
is_sub_delim($$) -> true;
is_sub_delim($&) -> true;
is_sub_delim($') -> true;
is_sub_delim($() -> true;
is_sub_delim($)) -> true;

is_sub_delim($*) -> true;
is_sub_delim($+) -> true;
is_sub_delim($,) -> true;
is_sub_delim($;) -> true;
is_sub_delim($=) -> true;
is_sub_delim(_) -> false.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 2.3. Unreserved Characters]
%%
%%   unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
%%
%%-------------------------------------------------------------------------
-spec is_unreserved(char()) -> boolean().
is_unreserved($-) -> true;
is_unreserved($.) -> true;
is_unreserved($_) -> true;
is_unreserved($~) -> true;
is_unreserved(Char) -> is_alpha(Char) orelse is_digit(Char).

-spec is_alpha(char()) -> boolean().
is_alpha(C)
  when $A =< C, C =< $Z;
       $a =< C, C =< $z -> true;
is_alpha(_) -> false.

-spec is_digit(char()) -> boolean().
is_digit(C)
  when $0 =< C, C =< $9 -> true;
is_digit(_) -> false.

-spec is_hex_digit(char()) -> boolean().
is_hex_digit(C)
  when $0 =< C, C =< $9;$a =< C, C =< $f;$A =< C, C =< $F -> true;
is_hex_digit(_) -> false.


%% Remove enclosing brackets from binary
-spec remove_brackets(binary()) -> binary().
remove_brackets(<<$[/utf8, Rest/binary>>) ->
    {H,T} = split_binary(Rest, byte_size(Rest) - 1),
    case T =:= <<$]/utf8>> of
        true -> H;
        false -> Rest
    end;
remove_brackets(Addr) -> Addr.


%%-------------------------------------------------------------------------
%% Helper functions for calculating the parsed binary.
%%-------------------------------------------------------------------------
-spec calculate_parsed_scheme(binary(), binary()) -> binary().
calculate_parsed_scheme(Input, <<>>) ->
    strip_last_char(Input, [$:]);
calculate_parsed_scheme(Input, Unparsed) ->
    get_parsed_binary(Input, Unparsed).


-spec calculate_parsed_part(binary(), binary()) -> binary().
calculate_parsed_part(Input, <<>>) ->
    strip_last_char(Input, [$?,$#]);
calculate_parsed_part(Input, Unparsed) ->
    get_parsed_binary(Input, Unparsed).


-spec calculate_parsed_userinfo(binary(), binary()) -> binary().
calculate_parsed_userinfo(Input, <<>>) ->
    strip_last_char(Input, [$?,$#,$@]);
calculate_parsed_userinfo(Input, Unparsed) ->
    get_parsed_binary(Input, Unparsed).


-spec calculate_parsed_host_port(binary(), binary()) -> binary().
calculate_parsed_host_port(Input, <<>>) ->
    strip_last_char(Input, [$:,$?,$#,$/]);
calculate_parsed_host_port(Input, Unparsed) ->
    get_parsed_binary(Input, Unparsed).


calculate_parsed_query_fragment(Input, <<>>) ->
    strip_last_char(Input, [$#]);
calculate_parsed_query_fragment(Input, Unparsed) ->
    get_parsed_binary(Input, Unparsed).


get_port(<<>>) ->
    undefined;
get_port(B) ->
    try binary_to_integer(B)
    catch
        error:badarg ->
            throw({error, invalid_uri, B})
    end.


%% Strip last char if it is in list
%%
%% This function is optimized for speed: parse/1 is about 10% faster than
%% with an alternative implementation based on lists and sets.
strip_last_char(<<>>, _) -> <<>>;
strip_last_char(Input, [C0]) ->
    case binary:last(Input) of
        C0 ->
            init_binary(Input);
        _Else ->
            Input
    end;
strip_last_char(Input, [C0,C1]) ->
    case binary:last(Input) of
        C0 ->
            init_binary(Input);
        C1 ->
            init_binary(Input);
        _Else ->
            Input
    end;
strip_last_char(Input, [C0,C1,C2]) ->
    case binary:last(Input) of
        C0 ->
            init_binary(Input);
        C1 ->
            init_binary(Input);
        C2 ->
            init_binary(Input);
        _Else ->
            Input
    end;
strip_last_char(Input, [C0,C1,C2,C3]) ->
    case binary:last(Input) of
        C0 ->
            init_binary(Input);
        C1 ->
            init_binary(Input);
        C2 ->
            init_binary(Input);
        C3 ->
            init_binary(Input);
        _Else ->
            Input
    end.


%% Get parsed binary
get_parsed_binary(Input, Unparsed) ->
    {First, _} = split_binary(Input, byte_size(Input) - byte_size_exl_head(Unparsed)),
    First.


%% Return all bytes of the binary except the last one. The binary must be non-empty.
init_binary(B) ->
    {Init, _} =
        split_binary(B, byte_size(B) - 1),
    Init.


%% Returns the size of a binary exluding the first element.
%% Used in calls to split_binary().
-spec byte_size_exl_head(binary()) -> number().
byte_size_exl_head(<<>>) -> 0;
byte_size_exl_head(Binary) -> byte_size(Binary) + 1.


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 2.1.  Percent-Encoding]
%%
%% A percent-encoding mechanism is used to represent a data octet in a
%% component when that octet's corresponding character is outside the
%% allowed set or is being used as a delimiter of, or within, the
%% component.  A percent-encoded octet is encoded as a character
%% triplet, consisting of the percent character "%" followed by the two
%% hexadecimal digits representing that octet's numeric value.  For
%% example, "%20" is the percent-encoding for the binary octet
%% "00100000" (ABNF: %x20), which in US-ASCII corresponds to the space
%% character (SP).  Section 2.4 describes when percent-encoding and
%% decoding is applied.
%%
%%   pct-encoded = "%" HEXDIG HEXDIG
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Percent-encode
%%-------------------------------------------------------------------------

%% Only validates as scheme cannot have percent-encoded characters
-spec encode_scheme(list()|binary()) -> list() | binary().
encode_scheme([]) ->
    throw({error,invalid_scheme,""});
encode_scheme(<<>>) ->
    throw({error,invalid_scheme,<<>>});
encode_scheme(Scheme) ->
    case validate_scheme(Scheme) of
        true -> Scheme;
        false -> throw({error,invalid_scheme,Scheme})
    end.

-spec encode_userinfo(list()|binary()) -> list() | binary().
encode_userinfo(Cs) ->
    encode(Cs, fun is_userinfo/1).

-spec encode_host(list()|binary()) -> list() | binary().
encode_host(Cs) ->
    case classify_host(Cs) of
        regname -> Cs;
        ipv4 -> Cs;
        ipv6 -> bracket_ipv6(Cs);
        other -> encode(Cs, fun is_reg_name/1)
    end.

-spec encode_path(list()|binary()) -> list() | binary().
encode_path(Cs) ->
    encode(Cs, fun is_path/1).

-spec encode_query(list()|binary()) -> list() | binary().
encode_query(Cs) ->
    encode(Cs, fun is_query/1).

-spec encode_fragment(list()|binary()) -> list() | binary().
encode_fragment(Cs) ->
    encode(Cs, fun is_fragment/1).

%%-------------------------------------------------------------------------
%% Helper funtions for percent-decode
%%-------------------------------------------------------------------------

-spec decode(list()|binary()) -> list() | binary().
decode(Cs) ->
    decode(Cs, <<>>).
%%
decode(L, Acc) when is_list(L) ->
    B0 = unicode:characters_to_binary(L),
    B1 = decode(B0, Acc),
    unicode:characters_to_list(B1);
decode(<<$%,C0,C1,Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            %% [2.4] When a URI is dereferenced, the components and subcomponents
            %% significant to the scheme-specific dereferencing process (if any)
            %% must be parsed and separated before the percent-encoded octets within
            %% those components can be safely decoded, as otherwise the data may be
            %% mistaken for component delimiters.  The only exception is for
            %% percent-encoded octets corresponding to characters in the unreserved
            %% set, which can be decoded at any time.
            case is_unreserved(B) of
                false ->
                    %% [2.2] Characters in the reserved set are protected from
                    %% normalization.
                    %% [2.1] For consistency, URI producers and normalizers should
                    %% use uppercase hexadecimal digits for all percent-
                    %% encodings.
                    H0 = hex_to_upper(C0),
                    H1 = hex_to_upper(C1),
                    decode(Cs, <<Acc/binary,$%,H0,H1>>);
                true ->
                    decode(Cs, <<Acc/binary, B>>)
            end;
        false -> throw({error,invalid_percent_encoding,<<$%,C0,C1>>})
    end;
decode(<<C,Cs/binary>>, Acc) ->
    decode(Cs, <<Acc/binary, C>>);
decode(<<>>, Acc) ->
    check_utf8(Acc).

-spec raw_decode(list()|binary()) -> list() | binary() | error().
raw_decode(Cs) ->
    raw_decode(Cs, <<>>).
%%
raw_decode(L, Acc) when is_list(L) ->
    try
        B0 = unicode:characters_to_binary(L),
        B1 = raw_decode(B0, Acc),
        unicode:characters_to_list(B1)
    catch
        throw:{error, Atom, RestData} ->
            {error, Atom, RestData}
    end;
raw_decode(<<$%,C0,C1,Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            raw_decode(Cs, <<Acc/binary, B>>);
        false ->
            throw({error,invalid_percent_encoding,<<$%,C0,C1>>})
    end;
raw_decode(<<C,Cs/binary>>, Acc) ->
    raw_decode(Cs, <<Acc/binary, C>>);
raw_decode(<<>>, Acc) ->
    check_utf8(Acc).

%% Returns Cs if it is utf8 encoded.
check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {incomplete,_,_} ->
            throw({error,invalid_utf8,Cs});
        {error,_,_} ->
            throw({error,invalid_utf8,Cs});
        _ -> Cs
    end.

%% Convert hex digit to uppercase form
hex_to_upper(H) when $a =< H, H =< $f ->
    H - 32;
hex_to_upper(H) when $0 =< H, H =< $9;$A =< H, H =< $F->
    H;
hex_to_upper(H) ->
    throw({error,invalid_input, H}).

%% Check if char is allowed in host
-doc false.
-spec is_host(char()) -> boolean().
is_host($:) -> true;
is_host(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).

%% Check if char is allowed in path
-doc false.
-spec is_path(char()) -> boolean().
is_path($/) -> true;
is_path(Char) -> is_pchar(Char).


%%-------------------------------------------------------------------------
%% Helper functions for percent-encode
%%-------------------------------------------------------------------------
-spec encode(list()|binary(), fun()) -> list() | binary().
encode(Component, Fun) when is_list(Component) ->
    B = unicode:characters_to_binary(Component),
    unicode:characters_to_list(encode(B, Fun, <<>>));
encode(Component, Fun) when is_binary(Component) ->
    encode(Component, Fun, <<>>).
%%
encode(<<Char/utf8, Rest/binary>>, Fun, Acc) ->
    C = encode_codepoint_binary(Char, Fun),
    encode(Rest, Fun, <<Acc/binary,C/binary>>);
encode(<<Char, Rest/binary>>, _Fun, _Acc) ->
    throw({error,invalid_input,<<Char,Rest/binary>>});
encode(<<>>, _Fun, Acc) ->
    Acc.


-spec encode_codepoint_binary(integer(), fun()) -> binary().
encode_codepoint_binary(C, Fun) ->
    case Fun(C) of
        false -> percent_encode_binary(C);
        true -> <<C>>
    end.


-spec percent_encode_binary(integer()) -> binary().
percent_encode_binary(Code) ->
    percent_encode_binary(<<Code/utf8>>, <<>>).


percent_encode_binary(<<A:4,B:4,Rest/binary>>, Acc) ->
    percent_encode_binary(Rest, <<Acc/binary,$%,(?DEC2HEX(A)),(?DEC2HEX(B))>>);
percent_encode_binary(<<>>, Acc) ->
    Acc.


%%-------------------------------------------------------------------------
%%-------------------------------------------------------------------------
validate_scheme([]) -> true;
validate_scheme([H|T]) ->
    case is_scheme(H) of
        true -> validate_scheme(T);
        false -> false
    end;
validate_scheme(<<>>) -> true;
validate_scheme(<<H, Rest/binary>>) ->
    case is_scheme(H) of
        true -> validate_scheme(Rest);
        false -> false
    end.


%%-------------------------------------------------------------------------
%% Classifies hostname into the following categories:
%% regname, ipv4 - address does not contain reserved characters to be
%%           percent-encoded
%% ipv6 - address does not contain reserved characters but it shall be
%%        encolsed in brackets
%% other - address shall be percent-encoded
%%-------------------------------------------------------------------------
classify_host([]) -> other;
classify_host(Addr) when is_binary(Addr) ->
    A = unicode:characters_to_list(Addr),
    classify_host_ipv6(A);
classify_host(Addr) ->
    classify_host_ipv6(Addr).

classify_host_ipv6(Addr) ->
    case is_ipv6_address(Addr) of
        true -> ipv6;
        false -> classify_host_ipv4(Addr)
    end.

classify_host_ipv4(Addr) ->
    case is_ipv4_address(Addr) of
        true -> ipv4;
        false -> classify_host_regname(Addr)
    end.

classify_host_regname([]) -> regname;
classify_host_regname([H|T]) ->
    case is_reg_name(H) of
        true -> classify_host_regname(T);
        false -> other
    end.

is_ipv4_address(Addr) ->
    case inet:parse_ipv4strict_address(Addr) of
        {ok, _} -> true;
        {error, _} -> false
    end.

is_ipv6_address(Addr) ->
    case inet:parse_ipv6strict_address(Addr) of
        {ok, _} -> true;
        {error, _} -> false
    end.

bracket_ipv6(Addr) when is_binary(Addr) ->
    concat(<<$[,Addr/binary>>,<<$]>>);
bracket_ipv6(Addr) when is_list(Addr) ->
    [$[|Addr] ++ "]".


%%-------------------------------------------------------------------------
%% Helper funtions for recompose
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Checks if input Map has valid combination of fields that can be
%% recomposed into a URI.
%%
%% The implementation is based on a decision tree that fulfills the
%% following rules:
%%   - 'path' shall always be present in the input map
%%       URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%%       hier-part   = "//" authority path-abempty
%%                      / path-absolute
%%                      / path-rootless
%%                      / path-empty
%%   - 'host' shall be present in the input map when 'path' starts with
%%     two slashes ("//")
%%       path          = path-abempty    ; begins with "/" or is empty
%%                     / path-absolute   ; begins with "/" but not "//"
%%                     / path-noscheme   ; begins with a non-colon segment
%%                     / path-rootless   ; begins with a segment
%%                     / path-empty      ; zero characters
%%       path-abempty  = *( "/" segment )
%%       segment       = *pchar
%%   - 'host' shall be present if userinfo or port is present in input map
%%       authority   = [ userinfo "@" ] host [ ":" port ]
%%   - All fields shall be valid (scheme, userinfo, host, port, path, query
%%     or fragment).
%%-------------------------------------------------------------------------
is_valid_map(#{path := Path} = Map) ->
    ((starts_with_two_slash(Path) andalso is_valid_map_host(Map))
     orelse
       (maps:is_key(userinfo, Map) andalso is_valid_map_host(Map))
     orelse
       (maps:is_key(port, Map) andalso is_valid_map_host(Map))
     orelse
     all_fields_valid(Map));
is_valid_map(#{}) ->
    false.


is_valid_map_host(Map) ->
    maps:is_key(host, Map) andalso all_fields_valid(Map).


all_fields_valid(Map) ->
    Fun = fun(scheme, _, Acc) -> Acc;
             (userinfo, _, Acc) -> Acc;
             (host, _, Acc) -> Acc;
             (port, _, Acc) -> Acc;
             (path, _, Acc) -> Acc;
             (query, _, Acc) -> Acc;
             (fragment, _, Acc) -> Acc;
             (_, _, _) -> false
          end,
    maps:fold(Fun, true, Map).


starts_with_two_slash([$/,$/|_]) ->
    true;
starts_with_two_slash(?STRING_REST("//", _)) ->
    true;
starts_with_two_slash(_) -> false.


update_scheme(#{scheme := Scheme}, _) ->
    add_colon_postfix(encode_scheme(Scheme));
update_scheme(#{}, _) ->
    empty.


update_userinfo(#{userinfo := Userinfo}, empty) ->
    add_auth_prefix(encode_userinfo(Userinfo));
update_userinfo(#{userinfo := Userinfo}, URI) ->
    concat(URI,add_auth_prefix(encode_userinfo(Userinfo)));
update_userinfo(#{}, empty) ->
    empty;
update_userinfo(#{}, URI) ->
    URI.


update_host(#{host := Host}, empty) ->
    add_auth_prefix(encode_host(Host));
update_host(#{host := Host} = Map, URI) ->
    concat(URI,add_host_prefix(Map, encode_host(Host)));
update_host(#{}, empty) ->
    empty;
update_host(#{}, URI) ->
    URI.


%% URI cannot be empty for ports. E.g. ":8080" is not a valid URI
update_port(#{port := undefined}, URI) ->
    concat(URI, <<":">>);
update_port(#{port := Port}, URI) ->
    concat(URI,add_colon(encode_port(Port)));
update_port(#{}, URI) ->
    URI.


update_path(#{path := Path}, empty) ->
    encode_path(Path);
update_path(#{host := _, path := Path0}, URI) ->
    %% When host is present in a URI the path must begin with "/" or be empty.
    Path1 = maybe_flatten_list(Path0),
    Path = make_path_absolute(Path1),
    concat(URI,encode_path(Path));
update_path(#{path := Path}, URI) ->
    concat(URI,encode_path(Path));
update_path(#{}, empty) ->
    empty;
update_path(#{}, URI) ->
    URI.


update_query(#{query := Query}, empty) ->
    encode_query(Query);
update_query(#{query := Query}, URI) ->
    concat(URI,add_question_mark(encode_query(Query)));
update_query(#{}, empty) ->
    empty;
update_query(#{}, URI) ->
    URI.


update_fragment(#{fragment := Fragment}, empty) ->
    add_hashmark(encode_fragment(Fragment));
update_fragment(#{fragment := Fragment}, URI) ->
    concat(URI,add_hashmark(encode_fragment(Fragment)));
update_fragment(#{}, empty) ->
    "";
update_fragment(#{}, URI) ->
    URI.

%%-------------------------------------------------------------------------
%% Concatenates its arguments that can be lists and binaries.
%% The result is a list if at least one of its argument is a list and
%% binary otherwise.
%%-------------------------------------------------------------------------
concat(A, B) when is_binary(A), is_binary(B) ->
    <<A/binary, B/binary>>;
concat(A, B) when is_binary(A), is_list(B) ->
    unicode:characters_to_list(A) ++ B;
concat(A, B) when is_list(A) ->
    A ++ maybe_to_list(B).

add_hashmark(Comp) when is_binary(Comp) ->
    <<$#, Comp/binary>>;
add_hashmark(Comp) when is_list(Comp) ->
    [$#|Comp].

add_question_mark(Comp) when is_binary(Comp) ->
    <<$?, Comp/binary>>;
add_question_mark(Comp) when is_list(Comp) ->
    [$?|Comp].

add_colon(Comp) when is_binary(Comp) ->
    <<$:, Comp/binary>>.

add_colon_postfix(Comp) when is_binary(Comp) ->
    <<Comp/binary,$:>>;
add_colon_postfix(Comp) when is_list(Comp) ->
    Comp ++ ":".

add_auth_prefix(Comp) when is_binary(Comp) ->
    <<"//", Comp/binary>>;
add_auth_prefix(Comp) when is_list(Comp) ->
    [$/,$/|Comp].

add_host_prefix(#{userinfo := _}, Host) when is_binary(Host) ->
    <<$@,Host/binary>>;
add_host_prefix(#{}, Host) when is_binary(Host) ->
    <<"//",Host/binary>>;
add_host_prefix(#{userinfo := _}, Host) when is_list(Host) ->
    [$@|Host];
add_host_prefix(#{}, Host) when is_list(Host) ->
    [$/,$/|Host].

maybe_to_list(Comp) when is_binary(Comp) -> unicode:characters_to_list(Comp);
maybe_to_list(Comp) -> Comp.

encode_port(Port) ->
    integer_to_binary(Port).

%% URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
%%
%% hier-part     = "//" authority path-abempty
%%               / path-absolute
%%               / path-rootless
%%               / path-empty
%%
%% path          = path-abempty    ; begins with "/" or is empty
%%               / path-absolute   ; begins with "/" but not "//"
%%               / path-noscheme   ; begins with a non-colon segment
%%               / path-rootless   ; begins with a segment
%%               / path-empty      ; zero characters
make_path_absolute(<<>>) ->
    <<>>;
make_path_absolute("") ->
    "";
make_path_absolute(<<"/",_/binary>> = Path) ->
    Path;
make_path_absolute([$/|_] = Path) ->
    Path;
make_path_absolute(Path) when is_binary(Path) ->
    concat(<<$/>>, Path);
make_path_absolute(Path) when is_list(Path) ->
    concat("/", Path).

maybe_flatten_list(Path) when is_binary(Path) ->
    Path;
maybe_flatten_list(Path) ->
    unicode:characters_to_list(Path).

%%-------------------------------------------------------------------------
%% Helper functions for resolve
%%-------------------------------------------------------------------------

resolve_map(URIMap=#{scheme := _}, _) ->
    normalize_path_segment(URIMap);
resolve_map(URIMap, #{scheme := _}=BaseURIMap) ->
    resolve_map(URIMap, BaseURIMap, resolve_path_type(URIMap));
resolve_map(_URIMap, BaseURIMap) when is_map(BaseURIMap) ->
    {error,invalid_scheme,""};
resolve_map(URIMap, BaseURIString) ->
    case parse(BaseURIString) of
        BaseURIMap = #{scheme := _} ->
            resolve_map(URIMap, BaseURIMap, resolve_path_type(URIMap));
        BaseURIMap when is_map(BaseURIMap) ->
            {error,invalid_scheme,""};
        Error ->
            Error
    end.

resolve_path_type(URIMap) ->
    case iolist_to_binary(maps:get(path, URIMap, <<>>)) of
        <<>> -> empty_path;
        <<$/,_/bits>> -> absolute_path;
        _ -> relative_path
    end.

resolve_map(URI=#{host := _}, #{scheme := Scheme}, _) ->
    normalize_path_segment(URI#{scheme => Scheme});
resolve_map(URI, BaseURI, empty_path) ->
    Keys = case maps:is_key(query, URI) of
        true -> [scheme, userinfo, host, port, path];
        false -> [scheme, userinfo, host, port, path, query]
    end,
    maps:merge(URI, maps:with(Keys, BaseURI));
resolve_map(URI, BaseURI, absolute_path) ->
    normalize_path_segment(maps:merge(
        URI,
        maps:with([scheme, userinfo, host, port], BaseURI)));
resolve_map(URI=#{path := Path}, BaseURI, relative_path) ->
    normalize_path_segment(maps:merge(
        URI#{path => merge_paths(Path, BaseURI)},
        maps:with([scheme, userinfo, host, port], BaseURI))).

merge_paths(Path, BaseURI=#{path := BasePath0}) ->
    case {BaseURI, iolist_size(BasePath0)} of
        {#{host := _}, 0} ->
            merge_paths_absolute(Path);
        _ ->
            case string:split(BasePath0, <<$/>>, trailing) of
                [BasePath, _] when is_binary(Path) -> unicode:characters_to_binary([BasePath, $/, Path]);
                [BasePath, _] when is_list(Path) -> unicode:characters_to_list([BasePath, $/, Path]);
                [_] -> Path
            end
    end.

merge_paths_absolute(Path) when is_binary(Path) ->
    <<$/, Path/binary>>;
merge_paths_absolute(Path) when is_list(Path) ->
    unicode:characters_to_list([$/, Path]).


%%-------------------------------------------------------------------------
%% Helper functions for transcode
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% uri_string:transcode(<<"x%00%00%00%F6"/utf32>>).
%% 1. Convert (transcode/2) input to list form (list of unicode codepoints)
%%    "x%00%00%00%F6"
%% 2. Accumulate characters until percent-encoded segment (transcode/4).
%%    Acc = "x"
%% 3. Convert percent-encoded triplets to binary form (transcode_pct/4)
%%    <<0,0,0,246>>
%% 4. Transcode in-encoded binary to out-encoding (utf32 -> utf8):
%%    <<195,182>>
%% 5. Percent-encode out-encoded binary:
%%    <<"%C3%B6"/utf8>> = <<37,67,51,37,66,54>>
%% 6. Convert binary to list form, reverse it and append the accumulator
%%    "6B%3C%" + "x"
%% 7. Reverse Acc and return it
%%-------------------------------------------------------------------------
transcode([$%,_C0,_C1|_Rest] = L, Acc, InEnc, OutEnc) ->
    transcode_pct(L, Acc, <<>>, InEnc, OutEnc);
transcode([_C|_Rest] = L, Acc, InEnc, OutEnc) ->
    transcode(L, Acc, [], InEnc, OutEnc).
%%
transcode([$%,_C0,_C1|_Rest] = L, Acc, List, InEncoding, OutEncoding) ->
    transcode_pct(L, List ++ Acc, <<>>, InEncoding, OutEncoding);
transcode([C|Rest], Acc, List, InEncoding, OutEncoding) ->
    transcode(Rest, Acc, [C|List], InEncoding, OutEncoding);
transcode([], Acc, List, _InEncoding, _OutEncoding) ->
    lists:reverse(List ++ Acc).


%% Transcode percent-encoded segment
transcode_pct([$%,C0,C1|Rest] = L, Acc, B, InEncoding, OutEncoding) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            Int = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            transcode_pct(Rest, Acc, <<B/binary, Int>>, InEncoding, OutEncoding);
        false -> throw({error, invalid_percent_encoding,L})
    end;
transcode_pct([_C|_Rest] = L, Acc, B, InEncoding, OutEncoding) ->
    OutBinary = convert_to_binary(B, InEncoding, OutEncoding),
    PctEncUtf8 = percent_encode_segment(OutBinary),
    Out = lists:reverse(convert_to_list(PctEncUtf8, utf8)),
    transcode(L, Out ++ Acc, [], InEncoding, OutEncoding);
transcode_pct([], Acc, B, InEncoding, OutEncoding) ->
    OutBinary = convert_to_binary(B, InEncoding, OutEncoding),
    PctEncUtf8 = percent_encode_segment(OutBinary),
    Out = convert_to_list(PctEncUtf8, utf8),
    lists:reverse(Acc, Out).


%% Convert to binary
convert_to_binary(Binary, InEncoding, OutEncoding) ->
    case unicode:characters_to_binary(Binary, InEncoding, OutEncoding) of
        {error, _List, RestData} ->
            throw({error, invalid_input, RestData});
        {incomplete, _List, RestData} ->
            throw({error, invalid_input, RestData});
        Result ->
            Result
    end.


%% Convert to list
convert_to_list(Binary, InEncoding) ->
    case unicode:characters_to_list(Binary, InEncoding) of
        {error, _List, RestData} ->
            throw({error, invalid_input, RestData});
        {incomplete, _List, RestData} ->
            throw({error, invalid_input, RestData});
        Result ->
            Result
    end.


%% Flatten input list
flatten_list([], _) ->
    [];
flatten_list(L, InEnc) ->
    flatten_list(L, InEnc, []).
%%
flatten_list([H|T], InEnc, Acc) when is_binary(H) ->
    L = convert_to_list(H, InEnc),
    flatten_list(T, InEnc, lists:reverse(L, Acc));
flatten_list([H|T], InEnc, Acc) when is_list(H) ->
    flatten_list(H ++ T, InEnc, Acc);
flatten_list([H|T], InEnc, Acc) ->
    flatten_list(T, InEnc, [H|Acc]);
flatten_list([], _InEnc, Acc) ->
    lists:reverse(Acc);
flatten_list(Arg, _, _) ->
    throw({error, invalid_input, Arg}).


percent_encode_segment(Segment) ->
    percent_encode_binary(Segment, <<>>).


%%-------------------------------------------------------------------------
%% Helper functions for compose_query
%%-------------------------------------------------------------------------

%% Returns separator to be used between key-value pairs
get_separator([]) ->
    <<>>;
get_separator(_L) ->
    <<"&">>.


%% HTML 5.2 - 4.10.21.6 URL-encoded form data - WHATWG URL (10 Jan 2018) - UTF-8
%% HTML 5.0 - 4.10.22.6 URL-encoded form data - encoding (non UTF-8)
form_urlencode(Cs, [{encoding, latin1}]) when is_list(Cs) ->
    B = convert_to_binary(Cs, utf8, utf8),
    html5_byte_encode(base10_encode(B));
form_urlencode(Cs, [{encoding, latin1}]) when is_binary(Cs) ->
    html5_byte_encode(base10_encode(Cs));
form_urlencode(Cs, [{encoding, Encoding}])
  when is_list(Cs), Encoding =:= utf8; Encoding =:= unicode ->
    B = convert_to_binary(Cs, utf8, Encoding),
    html5_byte_encode(B);
form_urlencode(Cs, [{encoding, Encoding}])
  when is_binary(Cs), Encoding =:= utf8; Encoding =:= unicode ->
    html5_byte_encode(Cs);
form_urlencode(Cs, [{encoding, Encoding}]) when is_list(Cs); is_binary(Cs) ->
    throw({error,invalid_encoding, Encoding});
form_urlencode(Cs, _) ->
    throw({error,invalid_input, Cs}).


%% For each character in the entry's name and value that cannot be expressed using
%% the selected character encoding, replace the character by a string consisting of
%% a U+0026 AMPERSAND character (&), a "#" (U+0023) character, one or more ASCII
%% digits representing the Unicode code point of the character in base ten, and
%% finally a ";" (U+003B) character.
base10_encode(Cs) ->
    base10_encode(Cs, <<>>).
%%
base10_encode(<<>>, Acc) ->
    Acc;
base10_encode(<<H/utf8,T/binary>>, Acc) when H > 255 ->
    Base10 = convert_to_binary(integer_to_list(H,10), utf8, utf8),
    base10_encode(T, <<Acc/binary,"&#",Base10/binary,$;>>);
base10_encode(<<H/utf8,T/binary>>, Acc) ->
    base10_encode(T, <<Acc/binary,H>>).


html5_byte_encode(B) ->
    html5_byte_encode(B, <<>>).
%%
html5_byte_encode(<<>>, Acc) ->
    Acc;
html5_byte_encode(<<$ ,T/binary>>, Acc) ->
    html5_byte_encode(T, <<Acc/binary,$+>>);
html5_byte_encode(<<H,T/binary>>, Acc) ->
    case is_url_char(H) of
        true ->
            html5_byte_encode(T, <<Acc/binary,H>>);
        false ->
            <<A:4,B:4>> = <<H>>,
            html5_byte_encode(T, <<Acc/binary,$%,(?DEC2HEX(A)),(?DEC2HEX(B))>>)
    end;
html5_byte_encode(H, _Acc) ->
    throw({error,invalid_input, H}).


%% Return true if input char can appear in form-urlencoded string
%% Allowed chararacters:
%%   0x2A, 0x2D, 0x2E, 0x30 to 0x39, 0x41 to 0x5A,
%%   0x5F, 0x61 to 0x7A
is_url_char(C)
  when C =:= 16#2A; C =:= 16#2D;
       C =:= 16#2E; C =:= 16#5F;
       16#30 =< C, C =< 16#39;
       16#41 =< C, C =< 16#5A;
       16#61 =< C, C =< 16#7A -> true;
is_url_char(_) -> false.


%%-------------------------------------------------------------------------
%% Helper functions for dissect_query
%%-------------------------------------------------------------------------
dissect_query_key(<<$=,T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_value(T, IsList, Acc, Key, Value);
dissect_query_key(<<"&#",T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_key(T, IsList, Acc, <<Key/binary,"&#">>, Value);
dissect_query_key(T = <<$&,_/binary>>, IsList, Acc, Key, <<>>) ->
    dissect_query_value(T, IsList, Acc, Key, true);
dissect_query_key(<<H,T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_key(T, IsList, Acc, <<Key/binary,H>>, Value);
dissect_query_key(T = <<>>, IsList, Acc, Key, <<>>) ->
    dissect_query_value(T, IsList, Acc, Key, true).

dissect_query_value(<<$&,T/binary>>, IsList, Acc, Key, Value) ->
    K = form_urldecode(IsList, Key),
    V = form_urldecode(IsList, Value),
    dissect_query_key(T, IsList, [{K,V}|Acc], <<>>, <<>>);
dissect_query_value(<<H,T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_value(T, IsList, Acc, Key, <<Value/binary,H>>);
dissect_query_value(<<>>, IsList, Acc, Key, Value) ->
    K = form_urldecode(IsList, Key),
    V = form_urldecode(IsList, Value),
    lists:reverse([{K,V}|Acc]).

%% HTML 5.2 - 4.10.21.6 URL-encoded form data - WHATWG URL (10 Jan 2018) - UTF-8
%% HTML 5.0 - 4.10.22.6 URL-encoded form data - decoding (non UTF-8)
form_urldecode(_, true) ->
    true;
form_urldecode(true, B) ->
    Result = base10_decode(form_urldecode(B, <<>>)),
    convert_to_list(Result, utf8);
form_urldecode(false, B) ->
    base10_decode(form_urldecode(B, <<>>));
form_urldecode(<<>>, Acc) ->
    Acc;
form_urldecode(<<$+,T/binary>>, Acc) ->
    form_urldecode(T, <<Acc/binary,$ >>);
form_urldecode(<<$%,C0,C1,T/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            V = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            form_urldecode(T, <<Acc/binary, V>>);
        false ->
            L = convert_to_list(<<$%,C0,C1,T/binary>>, utf8),
            throw({error, invalid_percent_encoding, L})
    end;
form_urldecode(<<H/utf8,T/binary>>, Acc) ->
    form_urldecode(T, <<Acc/binary,H/utf8>>);
form_urldecode(<<H,_/binary>>, _Acc) ->
    throw({error, invalid_character, [H]}).

base10_decode(Cs) ->
    base10_decode(Cs, <<>>).
%
base10_decode(<<>>, Acc) ->
    Acc;
base10_decode(<<"&#",T/binary>>, Acc) ->
    base10_decode_unicode(T, Acc);
base10_decode(<<H/utf8,T/binary>>, Acc) ->
    base10_decode(T,<<Acc/binary,H/utf8>>);
base10_decode(<<H,_/binary>>, _) ->
    throw({error, invalid_input, [H]}).


base10_decode_unicode(B, Acc) ->
    base10_decode_unicode(B, 0, Acc).
%%
base10_decode_unicode(<<H/utf8,T/binary>>, Codepoint, Acc) when $0 =< H, H =< $9 ->
    Res = Codepoint * 10 + (H - $0),
    base10_decode_unicode(T, Res, Acc);
base10_decode_unicode(<<$;,T/binary>>, Codepoint, Acc) ->
    base10_decode(T, <<Acc/binary,Codepoint/utf8>>);
base10_decode_unicode(<<H,_/binary>>, _, _) ->
    throw({error, invalid_input, [H]}).


%%-------------------------------------------------------------------------
%% Helper functions for normalize
%%-------------------------------------------------------------------------

normalize_map(URIMap) ->
    normalize_path_segment(
      normalize_scheme_based(
        normalize_undefined_port(
          normalize_percent_encoding(
            normalize_case(URIMap))))).


%% 6.2.2.1.  Case Normalization
normalize_case(#{scheme := Scheme, host := Host} = Map) ->
    Map#{scheme => to_lower(Scheme),
         host => to_lower(Host)};
normalize_case(#{host := Host} = Map) ->
    Map#{host => to_lower(Host)};
normalize_case(#{scheme := Scheme} = Map) ->
    Map#{scheme => to_lower(Scheme)};
normalize_case(#{} = Map) ->
    Map.


%% 6.2.2.2.  Percent-Encoding Normalization
normalize_percent_encoding(Map) ->
    Fun = fun (K,V) when K =:= userinfo; K =:= host; K =:= path;
                         K =:= query; K =:= fragment ->
                  decode(V);
              %% Handle port and scheme
              (_,V) ->
                  V
          end,
    maps:map(Fun, Map).


to_lower(Cs) when is_list(Cs) ->
    B = convert_to_binary(Cs, utf8, utf8),
    convert_to_list(to_lower(B), utf8);
to_lower(Cs) when is_binary(Cs) ->
    to_lower(Cs, <<>>).
%%
to_lower(<<C,Cs/binary>>, Acc) when $A =< C, C =< $Z ->
    to_lower(Cs, <<Acc/binary,(C + 32)>>);
to_lower(<<C,Cs/binary>>, Acc) ->
    to_lower(Cs, <<Acc/binary,C>>);
to_lower(<<>>, Acc) ->
    Acc.


%% 6.2.2.3. Path Segment Normalization
%% 5.2.4.   Remove Dot Segments
normalize_path_segment(Map) ->
    Path = maps:get(path, Map, <<>>),
    Map#{path => remove_dot_segments(Path)}.


remove_dot_segments(Path) when is_binary(Path) ->
    remove_dot_segments(Path, <<>>);
remove_dot_segments(Path) when is_list(Path) ->
    B = convert_to_binary(Path, utf8, utf8),
    B1 = remove_dot_segments(B, <<>>),
    convert_to_list(B1, utf8).
%%
remove_dot_segments(<<>>, Output) ->
    Output;
remove_dot_segments(<<"../",T/binary>>, Output) ->
    remove_dot_segments(T, Output);
remove_dot_segments(<<"./",T/binary>>, Output) ->
    remove_dot_segments(T, Output);
remove_dot_segments(<<"/./",T/binary>>, Output) ->
    remove_dot_segments(<<$/,T/binary>>, Output);
remove_dot_segments(<<"/.">>, Output) ->
    remove_dot_segments(<<$/>>, Output);
remove_dot_segments(<<"/../",T/binary>>, Output) ->
    Out1 = remove_last_segment(Output),
    remove_dot_segments(<<$/,T/binary>>, Out1);
remove_dot_segments(<<"/..">>, Output) ->
    Out1 = remove_last_segment(Output),
    remove_dot_segments(<<$/>>, Out1);
remove_dot_segments(<<$.>>, Output) ->
    remove_dot_segments(<<>>, Output);
remove_dot_segments(<<"..">>, Output) ->
    remove_dot_segments(<<>>, Output);
remove_dot_segments(Input, Output) ->
    {First, Rest} = first_path_segment(Input),
    remove_dot_segments(Rest, <<Output/binary,First/binary>>).


first_path_segment(Input) ->
    F = first_path_segment(Input, <<>>),
    split_binary(Input, byte_size(F)).
%%
first_path_segment(<<$/,T/binary>>, Acc) ->
    first_path_segment_end(<<T/binary>>, <<Acc/binary,$/>>);
first_path_segment(<<C,T/binary>>, Acc) ->
    first_path_segment_end(<<T/binary>>, <<Acc/binary,C>>).


first_path_segment_end(<<>>, Acc) ->
    Acc;
first_path_segment_end(<<$/,_/binary>>, Acc) ->
    Acc;
first_path_segment_end(<<C,T/binary>>, Acc) ->
    first_path_segment_end(<<T/binary>>, <<Acc/binary,C>>).


remove_last_segment(<<>>) ->
    <<>>;
remove_last_segment(B) ->
    {Init, Last} = split_binary(B, byte_size(B) - 1),
    case Last of
        <<$/>> ->
            Init;
        _Char ->
            remove_last_segment(Init)
    end.


%% RFC 3986, 6.2.3.  Scheme-Based Normalization
normalize_scheme_based(Map) ->
    Scheme = maps:get(scheme, Map, undefined),
    Port = maps:get(port, Map, undefined),
    Path= maps:get(path, Map, undefined),
    normalize_scheme_based(Map, Scheme, Port, Path).
%%
normalize_scheme_based(Map, Scheme, Port, Path)
  when Scheme =:= "http"; Scheme =:= <<"http">> ->
    normalize_http(Map, Port, Path);
normalize_scheme_based(Map, Scheme, Port, Path)
  when Scheme =:= "https"; Scheme =:= <<"https">> ->
    normalize_https(Map, Port, Path);
normalize_scheme_based(Map, Scheme, Port, _Path)
  when Scheme =:= "ftp"; Scheme =:= <<"ftp">> ->
    normalize_ftp(Map, Port);
normalize_scheme_based(Map, Scheme, Port, _Path)
  when Scheme =:= "ssh"; Scheme =:= <<"ssh">> ->
    normalize_ssh_sftp(Map, Port);
normalize_scheme_based(Map, Scheme, Port, _Path)
  when Scheme =:= "sftp"; Scheme =:= <<"sftp">> ->
    normalize_ssh_sftp(Map, Port);
normalize_scheme_based(Map, Scheme, Port, _Path)
  when Scheme =:= "tftp"; Scheme =:= <<"tftp">> ->
    normalize_tftp(Map, Port);
normalize_scheme_based(Map, _, _, _) ->
    Map.


normalize_http(Map, Port, Path) ->
    M1 = normalize_default_port(Map, Port, 80),
    normalize_http_path(M1, Path).


normalize_https(Map, Port, Path) ->
    M1 = normalize_default_port(Map, Port, 443),
    normalize_http_path(M1, Path).


normalize_ftp(Map, Port) ->
    normalize_default_port(Map, Port, 21).


normalize_ssh_sftp(Map, Port) ->
    normalize_default_port(Map, Port, 22).


normalize_tftp(Map, Port) ->
    normalize_default_port(Map, Port, 69).


%% RFC 3986, 3.2.3.  Port
%% RFC 3986, 6.2.3.  Scheme-Based Normalization
normalize_default_port(Map, Port, Default) ->
    case Port of
        Default ->
            maps:remove(port, Map);
        _Else ->
            Map
    end.


normalize_undefined_port(#{port := undefined} = Map) ->
    maps:remove(port, Map);
normalize_undefined_port(#{} = Map) ->
    Map.


normalize_http_path(Map, Path) ->
    case Path of
        "" ->
            Map#{path => "/"};
        <<>> ->
            Map#{path => <<"/">>};
        _Else ->
            Map
    end.
