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

%%-------------------------------------------------------------------------
%% External API
%%-------------------------------------------------------------------------
-export([compose_query/1, compose_query/2,
         dissect_query/1, normalize/1, normalize/2, parse/1,
         recompose/1, transcode/2]).
-export_type([error/0, uri_map/0, uri_string/0]).


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
-type uri_string() :: iodata().
-type error() :: {error, atom(), term()}.


%%-------------------------------------------------------------------------
%% RFC 3986, Chapter 3. Syntax Components
%%-------------------------------------------------------------------------
-type uri_map() ::
  #{fragment => unicode:chardata(),
    host => unicode:chardata(),
    path => unicode:chardata(),
    port => non_neg_integer() | undefined,
    query => unicode:chardata(),
    scheme => unicode:chardata(),
    userinfo => unicode:chardata()} | #{}.


%%-------------------------------------------------------------------------
%% Normalize URIs
%%-------------------------------------------------------------------------
-spec normalize(URI) -> NormalizedURI when
      URI :: uri_string() | uri_map(),
      NormalizedURI :: uri_string()
                     | error().
normalize(URIMap) ->
    normalize(URIMap, []).


-spec normalize(URI, Options) -> NormalizedURI when
      URI :: uri_string() | uri_map(),
      Options :: [return_map],
      NormalizedURI :: uri_string() | uri_map().
normalize(URIMap, []) when is_map(URIMap) ->
    recompose(normalize_map(URIMap));
normalize(URIMap, [return_map]) when is_map(URIMap) ->
    normalize_map(URIMap);
normalize(URIString, []) ->
    case parse(URIString) of
        Value when is_map(Value) ->
            recompose(normalize_map(Value));
        Error ->
            Error
    end;
normalize(URIString, [return_map]) ->
    case parse(URIString) of
        Value when is_map(Value) ->
            normalize_map(Value);
        Error ->
            Error
    end.


%%-------------------------------------------------------------------------
%% Parse URIs
%%-------------------------------------------------------------------------
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
%% Transcode URIs
%%-------------------------------------------------------------------------
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
%% Functions for working with the query part of a URI as a list
%% of key/value pairs.
%% HTML 5.2 - 4.10.21.6 URL-encoded form data - WHATWG URL (10 Jan 2018) - UTF-8
%% HTML 5.0 - 4.10.22.6 URL-encoded form data - non UTF-8
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Compose urlencoded query string from a list of unescaped key/value pairs.
%% (application/x-www-form-urlencoded encoding algorithm)
%%-------------------------------------------------------------------------
-spec compose_query(QueryList) -> QueryString when
      QueryList :: [{unicode:chardata(), unicode:chardata()}],
      QueryString :: uri_string()
                   | error().
compose_query(List) ->
    compose_query(List, [{encoding, utf8}]).


-spec compose_query(QueryList, Options) -> QueryString when
      QueryList :: [{unicode:chardata(), unicode:chardata()}],
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
-spec dissect_query(QueryString) -> QueryList when
      QueryString :: uri_string(),
      QueryList :: [{unicode:chardata(), unicode:chardata()}]
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
            URI2#{userinfo => decode_userinfo(Userinfo)}
    catch
        throw:{_,_,_} ->
            {T, URI1} = parse_host(Rest, URI),
            Host = calculate_parsed_host_port(Rest, T),
            URI2 = maybe_add_path(URI1),
            URI2#{host => decode_host(remove_brackets(Host))}
    end;
parse_relative_part(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    Path = calculate_parsed_part(Rest, T),
    URI1#{path => decode_path(?STRING_REST($/, Path))};
parse_relative_part(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    URI2 = maybe_add_path(URI1),
    URI2#{query => decode_query(Query)};
parse_relative_part(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    URI2 = maybe_add_path(URI1),
    URI2#{fragment => decode_fragment(Fragment)};
parse_relative_part(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true ->
            {T, URI1} = parse_segment_nz_nc(Rest, URI),  % path-noscheme
            Path = calculate_parsed_part(Rest, T),
            URI1#{path => decode_path(?STRING_REST(Char, Path))};
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
    {Rest, URI1#{query => decode_query(Query)}};
parse_segment(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
    {Rest, URI1#{query => decode_query(Query)}};
parse_segment_nz_nc(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
	    {Rest, URI1#{userinfo => decode_userinfo(Userinfo)}}
    catch
        throw:{_,_,_} ->
            {T, URI1} = parse_host(Rest, URI),
            Host = calculate_parsed_host_port(Rest, T),
	    {Rest, URI1#{host => decode_host(remove_brackets(Host))}}
    end;
parse_hier(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    Path = calculate_parsed_part(Rest, T),
    {Rest, URI1#{path => decode_path(?STRING_REST($/, Path))}};
parse_hier(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => decode_query(Query)}};
parse_hier(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
parse_hier(?STRING_REST(Char, Rest), URI) ->  % path-rootless
    case is_pchar(Char) of
        true ->  % segment_nz
            {T, URI1} = parse_segment(Rest, URI),
            Path = calculate_parsed_part(Rest, T),
            {Rest, URI1#{path => decode_path(?STRING_REST(Char, Path))}};
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
    {Rest, URI1#{host => decode_host(remove_brackets(Host))}};
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
    {Rest, URI1#{path => decode_path(?STRING_REST($/, Path))}};
parse_host(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => decode_query(Query)}};
parse_host(?STRING_REST($[, Rest), URI) ->
    parse_ipv6_bin(Rest, [], URI);
parse_host(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
parse_host(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true -> parse_ipv4_bin(Rest, [Char], URI);
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
    {Rest, URI1#{path => decode_path(?STRING_REST($/, Path))}};
parse_reg_name(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => decode_query(Query)}};
parse_reg_name(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
    {Rest, URI1#{path => decode_path(?STRING_REST($/, Path))}};
parse_ipv4_bin(?STRING_REST($?, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => decode_query(Query)}};
parse_ipv4_bin(?STRING_REST($#, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
    {Rest, URI1#{path => decode_path(?STRING_REST($/, Path))}};
parse_ipv6_bin_end(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => decode_query(Query)}};
parse_ipv6_bin_end(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
    {Rest, URI1#{path => decode_path(?STRING_REST($/, Path))}};
parse_port(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    Query = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{query => decode_query(Query)}};
parse_port(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    Fragment = calculate_parsed_query_fragment(Rest, T),
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
    {Rest, URI1#{fragment => decode_fragment(Fragment)}};
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
-spec decode_userinfo(binary()) -> binary().
decode_userinfo(Cs) ->
    check_utf8(decode(Cs, fun is_userinfo/1, <<>>)).

-spec decode_host(binary()) -> binary().
decode_host(Cs) ->
    check_utf8(decode(Cs, fun is_host/1, <<>>)).

-spec decode_path(binary()) -> binary().
decode_path(Cs) ->
    check_utf8(decode(Cs, fun is_path/1, <<>>)).

-spec decode_query(binary()) -> binary().
decode_query(Cs) ->
    check_utf8(decode(Cs, fun is_query/1, <<>>)).

-spec decode_fragment(binary()) -> binary().
decode_fragment(Cs) ->
    check_utf8(decode(Cs, fun is_fragment/1, <<>>)).


%% Returns Cs if it is utf8 encoded.
check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {incomplete,_,_} ->
            throw({error,invalid_utf8,Cs});
        {error,_,_} ->
            throw({error,invalid_utf8,Cs});
        _ -> Cs
    end.

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
decode(<<$%,C0,C1,Cs/binary>>, Fun, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            decode(Cs, Fun, <<Acc/binary, B>>);
        false -> throw({error,invalid_percent_encoding,<<$%,C0,C1>>})
    end;
decode(<<C,Cs/binary>>, Fun, Acc) ->
    case Fun(C) of
        true -> decode(Cs, Fun, <<Acc/binary, C>>);
        false -> throw({error,invalid_percent_encoding,<<C,Cs/binary>>})
    end;
decode(<<>>, _Fun, Acc) ->
    Acc.

%% Check if char is allowed in host
-spec is_host(char()) -> boolean().
is_host($:) -> true;
is_host(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).

%% Check if char is allowed in path
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
    lists:reverse(Acc) ++ Out.


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
    flatten_list(T, InEnc, lists:reverse(L) ++ Acc);
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
get_separator(L) when length(L) =:= 0 ->
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
dissect_query_key(<<$&,_T/binary>>, _IsList, _Acc, _Key, _Value) ->
    throw({error, missing_value, "&"});
dissect_query_key(<<H,T/binary>>, IsList, Acc, Key, Value) ->
    dissect_query_key(T, IsList, Acc, <<Key/binary,H>>, Value);
dissect_query_key(B, _, _, _, _) ->
    throw({error, missing_value, B}).


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
          normalize_case(URIMap))).


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
    Path = maps:get(path, Map, undefined),
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
    M1 = normalize_port(Map, Port, 80),
    normalize_http_path(M1, Path).


normalize_https(Map, Port, Path) ->
    M1 = normalize_port(Map, Port, 443),
    normalize_http_path(M1, Path).


normalize_ftp(Map, Port) ->
    normalize_port(Map, Port, 21).


normalize_ssh_sftp(Map, Port) ->
    normalize_port(Map, Port, 22).


normalize_tftp(Map, Port) ->
    normalize_port(Map, Port, 69).


normalize_port(Map, Port, Default) ->
    case Port of
        Default ->
            maps:remove(port, Map);
        _Else ->
            Map
    end.


normalize_http_path(Map, Path) ->
    case Path of
        "" ->
            Map#{path => "/"};
        <<>> ->
            Map#{path => <<"/">>};
        _Else ->
            Map
    end.
