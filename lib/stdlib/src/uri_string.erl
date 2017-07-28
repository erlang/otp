%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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


-export([compose_query/1, create_uri_reference/2, dissect_query/1, normalize/1,
         parse/1, recompose/1, resolve_uri_reference/2, transcode/2]).
-export_type([uri_map/0, uri_string/0, bytelist/0]).


%%%=========================================================================
%%%  API
%%%=========================================================================


-type bytelist() :: maybe_improper_list(
                0..255 |
                binary() | bytelist(),
                binary() | []).

%% URI compliant with RFC 3986
%% ASCII %x21 - %x7A ("!" - "z") except
%%   %x34    "    double quote
%%   %x60    <    less than
%%   %x62    >    greater than
%%   %x92    \    backslash
%%   %x94    ^    caret / circumflex
%%   %x96    `    grave / accent
-type uri_string() :: bytelist() | binary().


%% RFC 3986, Chapter 3. Syntax Components
-type uri_map() ::
  #{fragment := unicode:chardata(),
    host := unicode:chardata(),
    path := unicode:chardata(),
    port := non_neg_integer(),
    query := unicode:chardata(),
    scheme := atom(),
    userinfo := unicode:chardata()}.

%% Parse URIs
-spec parse(URIString) -> URIMap when
      URIString :: uri_string(),
      URIMap :: uri_map().
parse(_) ->
    ok.

%% Recompose URIs
-spec recompose(URIMap) -> URIString when
      URIMap :: uri_map(),
      URIString :: uri_string().
recompose(_) ->
    ok.

%% Resolve references
-spec resolve_uri_reference(RelativeURI, AbsoluteBaseURI) -> AbsoluteDestURI when
      RelativeURI :: uri_string(),
      AbsoluteBaseURI :: uri_string(),
      AbsoluteDestURI :: uri_string().
resolve_uri_reference(_,_) ->
    ok.

%% Create references
-spec create_uri_reference(AbsoluteSourceURI, AbsoluteBaseURI) -> RelativeDestURI when
      AbsoluteSourceURI :: uri_string(),
      AbsoluteBaseURI :: uri_string(),
      RelativeDestURI :: uri_string().
create_uri_reference(_,_) ->
    ok.

%% Normalize URIs
-spec normalize(URIString) -> NormalizedURI when
      URIString :: uri_string(),
      NormalizedURI :: uri_string().
normalize(_) ->
    ok.

%% Transcode URIs
-spec transcode(URIString, Options) -> URIString when
      URIString :: uri_string(),
      Options :: [{in_encoding, unicode:encoding()}|{out_encoding, unicode:encoding()}].
transcode(_, _) ->
    ok.


%% Working with query strings
%% HTML 2.0 - application/x-www-form-urlencoded
%% RFC 1866 [8.2.1]

%% Compose urlencoded query string from a list of unescaped key/value pairs.
-spec compose_query(QueryList) -> QueryString when
      QueryList :: [{unicode:chardata(), unicode:chardata()}],
      QueryString :: uri_string().
compose_query(_) ->
    ok.

%% Dissect a query string into a list of unescaped key/value pairs.
-spec dissect_query(QueryString) -> QueryList when
      QueryString :: uri_string(),
      QueryList :: [{unicode:chardata(), unicode:chardata()}].
dissect_query(_) ->
     ok.
