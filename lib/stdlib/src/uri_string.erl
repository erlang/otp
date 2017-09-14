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
-export_type([uri_map/0, uri_string/0]).

-define(CHAR(Char), <<Char/utf8>>).
-define(STRING_EMPTY, <<>>).
-define(STRING(MatchStr), <<MatchStr/binary>>).
-define(STRING_REST(MatchStr, Rest), <<MatchStr/utf8, Rest/binary>>).


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


%% RFC 3986, Chapter 3. Syntax Components
-type uri_map() ::
  #{fragment => unicode:chardata(),
    host => unicode:chardata(),
    path => unicode:chardata(),
    port => non_neg_integer(),
    query => unicode:chardata(),
    scheme => unicode:chardata(),
    userinfo => unicode:chardata()} | #{}.

%% Parse URIs
-spec parse(URIString) -> URIMap when
      URIString :: uri_string(),
      URIMap :: uri_map().
parse(URIString) ->
    if is_binary(URIString) ->
            parse_uri_reference(URIString, #{});
       true ->
            parse_uri_reference(URIString, [], #{})
    end.

%% Recompose URIs
-spec recompose(URIMap) -> URIString when
      URIMap :: uri_map(),
      URIString :: uri_string().
recompose(_) ->
    "".

%% Resolve references
-spec resolve_uri_reference(RelativeURI, AbsoluteBaseURI) -> AbsoluteDestURI when
      RelativeURI :: uri_string(),
      AbsoluteBaseURI :: uri_string(),
      AbsoluteDestURI :: uri_string().
resolve_uri_reference(_,_) ->
    "".

%% Create references
-spec create_uri_reference(AbsoluteSourceURI, AbsoluteBaseURI) -> RelativeDestURI when
      AbsoluteSourceURI :: uri_string(),
      AbsoluteBaseURI :: uri_string(),
      RelativeDestURI :: uri_string().
create_uri_reference(_,_) ->
    "".

%% Normalize URIs
-spec normalize(URIString) -> NormalizedURI when
      URIString :: uri_string(),
      NormalizedURI :: uri_string().
normalize(_) ->
    "".

%% Transcode URIs
-spec transcode(URIString, Options) -> URIString when
      URIString :: uri_string(),
      Options :: [{in_encoding, unicode:encoding()}|{out_encoding, unicode:encoding()}].
transcode(_, _) ->
    "".


%% Working with query strings
%% HTML 2.0 - application/x-www-form-urlencoded
%% RFC 1866 [8.2.1]

%% Compose urlencoded query string from a list of unescaped key/value pairs.
-spec compose_query(QueryList) -> QueryString when
      QueryList :: [{unicode:chardata(), unicode:chardata()}],
      QueryString :: uri_string().
compose_query(_) ->
    "".

%% Dissect a query string into a list of unescaped key/value pairs.
-spec dissect_query(QueryString) -> QueryList when
      QueryString :: uri_string(),
      QueryList :: [{unicode:chardata(), unicode:chardata()}].
dissect_query(_) ->
     "".


%%%========================================================================
%%% Internal functions
%%%========================================================================


%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 4.1. URI Reference]
%%
%% URI-reference is used to denote the most common usage of a resource
%% identifier.
%%
%%    URI-reference = URI / relative-ref
%%-------------------------------------------------------------------------
-spec parse_uri_reference(iolist(), list(), uri_map()) -> uri_map().
parse_uri_reference([], _, _) -> #{};
parse_uri_reference(URIString, Acc, URI) ->
    try parse_scheme_start(URIString, Acc, URI) of
        Res -> Res
    catch
        throw:uri_parse_error ->
            parse_relative_part(URIString, Acc, URI)
    end.

-spec parse_uri_reference(binary(), uri_map()) -> uri_map().
parse_uri_reference(<<>>, _) -> #{};
parse_uri_reference(URIString, URI) ->
    try parse_scheme_start(URIString, URI) of
        Res -> Res
    catch
        throw:uri_parse_error ->
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
	    {Userinfo, _} = split_binary(Rest, byte_size(Rest) - byte_size(T) - 1),
	    URI1#{userinfo => Userinfo}
    catch
        throw:uri_parse_error ->
            {T, URI1} = parse_host(Rest, URI),
	    {Host, _} = split_binary(Rest, byte_size_exl_single_slash(Rest) - byte_size_exl_head(T)),
	    URI1#{host => remove_brackets(Host)}
    end;
parse_relative_part(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    URI1#{path => ?STRING_REST($/, Path)};
parse_relative_part(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    URI1#{query => ?STRING_REST($?, Query)};
parse_relative_part(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    URI1#{fragment => Fragment};
parse_relative_part(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true ->
            {T, URI1} = parse_segment_nz_nc(Rest, URI),  % path-noscheme
            {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
            URI1#{path => ?STRING_REST(Char, Path)};
        false -> throw(uri_parse_error)
    end.

-spec parse_relative_part(iolist(), list(), uri_map()) -> uri_map().
parse_relative_part([H|Rest], Acc, URI) when is_binary(H) ->
    parse_relative_part(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_relative_part([H|Rest], Acc, URI) when is_list(H) ->
    parse_relative_part(H ++ Rest, Acc, URI);
parse_relative_part("//" ++ Rest, Acc, URI) ->
     % Parse userinfo
     try parse_userinfo(Rest, Acc, URI) of
         Res -> Res
     catch
         throw:uri_parse_error ->
             parse_host(Rest, Acc, URI)
     end;
parse_relative_part([$/|Rest], _Acc, URI) ->
    parse_segment(Rest, [$/], URI);  % path-absolute
parse_relative_part([$?|Rest], _Acc, URI) ->
    parse_query(Rest, [$?], URI);  % path-empty ?query
parse_relative_part([$#|Rest], _Acc, URI) ->
    parse_fragment(Rest, [], URI);  % path-empty
parse_relative_part([Char|Rest], _, URI) ->
    case is_segment_nz_nc(Char) of
        true -> parse_segment_nz_nc(Rest, [Char], URI);  % path-noscheme
        false -> throw(uri_parse_error)
    end.


%% Returns size of 'Rest' for proper calculation of splitting position.
%% Solves the following special case:
%%
%%    #{host := <<>>, path := <<"/">>} = uri_string:parse(<<"///">>).
%%
%% While keeping the following true:
%%
%%     #{host := <<"hostname">>} = uri_string:parse(<<"//hostname">>).
%%     #{host := <<>>, path := <<"/hostname">>} = uri_string:parse(<<"///hostname">>).
%%
-spec byte_size_exl_single_slash(uri_string()) -> number().
byte_size_exl_single_slash(<<$/>>) -> 0;
byte_size_exl_single_slash(Rest) -> byte_size(Rest).


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
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_segment(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_segment(?STRING_REST(Char, Rest), URI) ->
    case is_pchar(Char) of
        true -> parse_segment(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_segment(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_segment(iolist(), list(), uri_map()) -> uri_map().
parse_segment(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_segment(unicode:characters_to_list(Str), Acc, URI);
parse_segment([H|Rest], Acc, URI) when is_binary(H) ->
    parse_segment(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_segment([H|Rest], Acc, URI) when is_list(H) ->
    parse_segment(H ++ Rest, Acc, URI);
parse_segment([$/|Rest], Acc, URI) ->
    parse_segment(Rest, [$/|Acc], URI);  % segment
parse_segment([$?|Rest], Acc, URI) ->
    parse_query(Rest, [$?], URI#{path => lists:reverse(Acc)});  % ?query
parse_segment([$#|Rest], Acc, URI) ->
    parse_fragment(Rest, [], URI#{path => lists:reverse(Acc)});
parse_segment([Char|Rest], Acc, URI) ->
    case is_pchar(Char) of
        true -> parse_segment(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_segment([], Acc, URI) ->
    URI#{path => lists:reverse(Acc)}.

%%-------------------------------------------------------------------------
%%    path-noscheme
%%-------------------------------------------------------------------------
-spec parse_segment_nz_nc(binary(), uri_map()) -> {binary(), uri_map()}.
parse_segment_nz_nc(?STRING_REST($/, Rest), URI) ->
    parse_segment(Rest, URI);  % segment
parse_segment_nz_nc(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_segment_nz_nc(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_segment_nz_nc(?STRING_REST(Char, Rest), URI) ->
    case is_segment_nz_nc(Char) of
        true -> parse_segment_nz_nc(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_segment_nz_nc(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_segment_nz_nc(iolist(), list(), uri_map()) -> uri_map().
parse_segment_nz_nc(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_segment_nz_nc(unicode:characters_to_list(Str), Acc, URI);
parse_segment_nz_nc([H|Rest], Acc, URI) when is_binary(H) ->
    parse_segment_nz_nc(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_segment_nz_nc([H|Rest], Acc, URI) when is_list(H) ->
    parse_segment_nz_nc(H ++ Rest, Acc, URI);
parse_segment_nz_nc([$/|Rest], Acc, URI) ->
    parse_segment(Rest, [$/|Acc], URI);  % segment
parse_segment_nz_nc([$?|Rest], Acc, URI) ->
    parse_query(Rest, [$?], URI#{path => lists:reverse(Acc)});  % ?query
parse_segment_nz_nc([$#|Rest], Acc, URI) ->
    parse_fragment(Rest, [], URI#{path => lists:reverse(Acc)});
parse_segment_nz_nc([Char|Rest], Acc, URI) ->
    case is_segment_nz_nc(Char) of
        true -> parse_segment_nz_nc(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_segment_nz_nc([], Acc, URI) ->
    URI#{path => lists:reverse(Acc)}.

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
		 {Scheme, _} = split_binary(Rest, byte_size(Rest) - byte_size(T) - 1),
		 URI1#{scheme => ?STRING_REST(Char, Scheme)};
        false -> throw(uri_parse_error)
    end.

-spec parse_scheme_start(iolist(), list(), uri_map()) -> uri_map().
parse_scheme_start([H|Rest], Acc, URI) when is_binary(H) ->
    parse_scheme_start(unicode:characters_to_list(H, utf8) ++ Rest,
                       Acc, URI);
parse_scheme_start([H|Rest], Acc, URI) when is_list(H) ->
    parse_scheme_start(H ++ Rest, Acc, URI);
parse_scheme_start([Char|Rest], Acc, URI) ->
    case is_alpha(Char) of
        true  -> parse_scheme(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end.


-spec parse_scheme(binary(), uri_map()) -> {binary(), uri_map()}.
parse_scheme(?STRING_REST($:, Rest), URI) ->
    {_, URI1} = parse_hier(Rest, URI),
    {Rest, URI1};
parse_scheme(?STRING_REST(Char, Rest), URI) ->
    case is_scheme(Char) of
        true  -> parse_scheme(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_scheme(?STRING_EMPTY, _URI) ->
    throw(uri_parse_error).

-spec parse_scheme(iolist(), list(), uri_map()) -> uri_map().
parse_scheme(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_scheme(unicode:characters_to_list(Str), Acc, URI);
parse_scheme([H|Rest], Acc, URI) when is_binary(H) ->
    parse_scheme(unicode:characters_to_list(H, utf8) ++ Rest,
                 Acc, URI);
parse_scheme([H|Rest], Acc, URI) when is_list(H) ->
    parse_scheme(H ++ Rest, Acc, URI);
parse_scheme([$:|Rest], Acc, URI) ->
    parse_hier(Rest, [], URI#{scheme => lists:reverse(Acc)});
parse_scheme([Char|Rest], Acc, URI) ->
    case is_scheme(Char) of
        true  -> parse_scheme(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_scheme([], _Acc, _URI) ->
    throw(uri_parse_error).

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
	    {Userinfo, _} = split_binary(Rest, byte_size(Rest) - byte_size(T) - 1),
	    {Rest, URI1#{userinfo => Userinfo}}
    catch
        throw:uri_parse_error ->
            {T, URI1} = parse_host(Rest, URI),
	    {Host, _} = split_binary(Rest, byte_size_exl_single_slash(Rest) - byte_size_exl_head(T)),
	    {Rest, URI1#{host => remove_brackets(Host)}}
    end;
parse_hier(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-absolute
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_hier(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_hier(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_hier(?STRING_REST(Char, Rest), URI) ->  % path-rootless
    case is_pchar(Char) of
        true ->  % segment_nz
            {T, URI1} = parse_segment(Rest, URI),
            {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
            {Rest, URI1#{path => ?STRING_REST(Char, Path)}};
        false -> throw(uri_parse_error)
    end;
parse_hier(?STRING_EMPTY, URI) ->
    {<<>>, URI}.

-spec parse_hier(iolist(), list(), uri_map()) -> uri_map().
parse_hier(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_hier(unicode:characters_to_list(Str), Acc, URI);
parse_hier([H|Rest], Acc, URI) when is_binary(H) ->
    parse_hier(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_hier([H|Rest], Acc, URI) when is_list(H) ->
    parse_hier(H ++ Rest, Acc, URI);
parse_hier("//" ++ Rest, Acc, URI) ->
     % Parse userinfo
     try parse_userinfo(Rest, Acc, URI) of
         Res -> Res
     catch
         throw:uri_parse_error ->
             parse_host(Rest, [], URI)
     end;
parse_hier([$/|Rest], _Acc, URI) ->
    parse_segment(Rest, [$/], URI);  % path-absolute
parse_hier([$?|Rest], _Acc, URI) ->
    parse_query(Rest, [$?], URI);  % path-empty ?query
parse_hier([$#|Rest], _Acc, URI) ->
    parse_fragment(Rest, [], URI);  % path-empty
parse_hier([Char|Rest], _, URI) ->  % path-rootless
    case is_pchar(Char) of
        true -> parse_segment(Rest, [Char], URI);
        false -> throw(uri_parse_error)
    end;
parse_hier([], _, URI) ->
    URI.


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
parse_userinfo(?CHAR($@), _URI) ->
    %% URI cannot end in userinfo state
    throw(uri_parse_error);
parse_userinfo(?STRING_REST($@, Rest), URI) ->
    {T, URI1} = parse_host(Rest, URI),
    {Host, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{host => remove_brackets(Host)}};
parse_userinfo(?STRING_REST(Char, Rest), URI) ->
    case is_userinfo(Char) of
        true -> parse_userinfo(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_userinfo(?STRING_EMPTY, _URI) ->
    %% URI cannot end in userinfo state
    throw(uri_parse_error).

-spec parse_userinfo(iolist(), list(), uri_map()) -> uri_map().
parse_userinfo(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_userinfo(unicode:characters_to_list(Str), Acc, URI);
parse_userinfo([H|Rest], Acc, URI) when is_binary(H) ->
    parse_userinfo(unicode:characters_to_list(H, utf8) ++ Rest,
                   Acc, URI);
parse_userinfo([H|Rest], Acc, URI) when is_list(H) ->
    parse_userinfo(H ++ Rest, Acc, URI);
parse_userinfo([$@], _Acc, _URI) ->
    %% URI cannot end in userinfo state
    throw(uri_parse_error);
parse_userinfo([$@|Rest], Acc, URI) ->
    parse_host(Rest, [],  URI#{userinfo => lists:reverse(Acc)});
parse_userinfo([Char|Rest], Acc, URI) ->
    case is_userinfo(Char) of
        true -> parse_userinfo(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error) % URI#{userinfo => lists:reverse(Acc)}
    end;
parse_userinfo([], _Acc, _URI) ->
    %% URI cannot end in userinfo state
    throw(uri_parse_error).

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
    {H, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    Port = binary_to_integer(H),
    {Rest, URI1#{port => Port}};
parse_host(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_host(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_host(?STRING_REST($[, Rest), URI) ->
    parse_ipv6_bin(Rest, [], URI);
parse_host(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_host(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true -> parse_ipv4_bin(Rest, [Char], URI);
        false -> parse_reg_name(?STRING_REST(Char, Rest), URI)
    end;
parse_host(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_host(iolist(), list(), uri_map()) -> uri_map().
parse_host(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_host(unicode:characters_to_list(Str), Acc, URI);
parse_host([H|Rest], Acc, URI) when is_binary(H) ->
    parse_host(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_host([H|Rest], Acc, URI) when is_list(H) ->
    parse_host(H ++ Rest, Acc, URI);
parse_host([$:|Rest], Acc, URI) ->
    parse_port(Rest, [], URI#{host => lists:reverse(Acc)});
parse_host([$/|Rest], Acc, URI) ->
    parse_segment(Rest, [$/], URI#{host => lists:reverse(Acc)});  % path-abempty
parse_host([$?|Rest], Acc, URI) ->
    parse_query(Rest, [$?], URI#{host => lists:reverse(Acc)});  % path-empty ?query
parse_host([$#|Rest], Acc, URI) ->
    parse_fragment(Rest, [], URI#{host => lists:reverse(Acc)});  % path-empty
parse_host([$[|Rest], _Acc, URI) ->
    parse_ipv6(Rest, [], URI);
parse_host([Char|Rest], Acc, URI) ->
    case is_digit(Char) of
        true -> parse_ipv4(Rest, [Char|Acc], URI);
        false -> parse_reg_name([Char|Rest], Acc, URI)
    end;
parse_host([], Acc, URI) ->
    URI#{host => lists:reverse(Acc)}.


-spec parse_reg_name(binary(), uri_map()) -> {binary(), uri_map()}.
parse_reg_name(?STRING_REST($:, Rest), URI) ->
    {T, URI1} = parse_port(Rest, URI),
    {H, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    Port = binary_to_integer(H),
    {Rest, URI1#{port => Port}};
parse_reg_name(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_reg_name(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_reg_name(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_reg_name(?STRING_REST(Char, Rest), URI) ->
    case is_reg_name(Char) of
        true -> parse_reg_name(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_reg_name(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_reg_name(iolist(), list(), uri_map()) -> uri_map().
parse_reg_name(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_reg_name(unicode:characters_to_list(Str), Acc, URI);
parse_reg_name([H|Rest], Acc, URI) when is_binary(H) ->
    parse_reg_name(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_reg_name([H|Rest], Acc, URI) when is_list(H) ->
    parse_reg_name(H ++ Rest, Acc, URI);
parse_reg_name([$:|Rest], Acc, URI) ->
    parse_port(Rest, [], URI#{host => lists:reverse(Acc)});
parse_reg_name([$/|Rest], Acc, URI) ->
    parse_segment(Rest, [$/], URI#{host => lists:reverse(Acc)});  % path-abempty
parse_reg_name([$?|Rest], Acc, URI) ->
    parse_query(Rest, [$?], URI#{host => lists:reverse(Acc)});  % path-empty ?query
parse_reg_name([$#|Rest], Acc, URI) ->
    parse_fragment(Rest, [], URI#{host => lists:reverse(Acc)});  % path-empty
parse_reg_name([Char|Rest], Acc, URI) ->
    case is_reg_name(Char) of
        true -> parse_reg_name(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_reg_name([], Acc, URI) ->
    URI#{host => lists:reverse(Acc)}.

%% Check if char is allowed in reg-name
-spec is_reg_name(char()) -> boolean().
is_reg_name($%) -> true;
is_reg_name(Char) -> is_unreserved(Char) orelse is_sub_delim(Char).


-spec parse_ipv4_bin(binary(), list(), uri_map()) -> {binary(), uri_map()}.
parse_ipv4_bin(?STRING_REST($:, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_port(Rest, URI),
    {H, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    Port = binary_to_integer(H),
    {Rest, URI1#{port => Port}};
parse_ipv4_bin(?STRING_REST($/, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_ipv4_bin(?STRING_REST($?, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_ipv4_bin(?STRING_REST($#, Rest), Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_ipv4_bin(?STRING_REST(Char, Rest), Acc, URI) ->
    case is_ipv4(Char) of
        true -> parse_ipv4_bin(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_ipv4_bin(?STRING_EMPTY, Acc, URI) ->
    _ = validate_ipv4_address(lists:reverse(Acc)),
    {?STRING_EMPTY, URI}.

-spec parse_ipv4(iolist(), list(), uri_map()) -> uri_map().
parse_ipv4(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_ipv4(unicode:characters_to_list(Str), Acc, URI);
parse_ipv4([H|Rest], Acc, URI) when is_binary(H) ->
    parse_ipv4(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_ipv4([H|Rest], Acc, URI) when is_list(H) ->
    parse_ipv4(H ++ Rest, Acc, URI);
parse_ipv4([$:|Rest], Acc, URI) ->
    parse_port(Rest, [], URI#{host => validate_ipv4_address(lists:reverse(Acc))});
parse_ipv4([$/|Rest], Acc, URI) ->
    parse_segment(Rest, [$/], URI#{host => validate_ipv4_address(lists:reverse(Acc))});  % path-abempty
parse_ipv4([$?|Rest], Acc, URI) ->
    parse_query(Rest, [$?], URI#{host => validate_ipv4_address(lists:reverse(Acc))});  % path-empty ?query
parse_ipv4([$#|Rest], Acc, URI) ->
    parse_fragment(Rest, [], URI#{host => validate_ipv4_address(lists:reverse(Acc))});  % path-empty
parse_ipv4([Char|Rest], Acc, URI) ->
    case is_ipv4(Char) of
        true -> parse_ipv4(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_ipv4([], Acc, URI) ->
    URI#{host => validate_ipv4_address(lists:reverse(Acc))}.

%% Check if char is allowed in IPv4 addresses
-spec is_ipv4(char()) -> boolean().
is_ipv4($.) -> true;
is_ipv4(Char) -> is_digit(Char).

-spec validate_ipv4_address(list()) -> list().
validate_ipv4_address(Addr) ->
    case inet:parse_ipv4strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw(uri_parse_error)
    end.


-spec parse_ipv6_bin(binary(), list(), uri_map()) -> {binary(), uri_map()}.
parse_ipv6_bin(?STRING_REST($], Rest), Acc, URI) ->
    _ = validate_ipv6_address(lists:reverse(Acc)),
    parse_ipv6_bin_end(Rest, URI);
parse_ipv6_bin(?STRING_REST(Char, Rest), Acc, URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6_bin(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_ipv6_bin(?STRING_EMPTY, _Acc, _URI) ->
    throw(uri_parse_error).

-spec parse_ipv6(iolist(), list(), uri_map()) -> uri_map().
parse_ipv6(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_ipv6(unicode:characters_to_list(Str), Acc, URI);
parse_ipv6([H|Rest], Acc, URI) when is_binary(H) ->
    parse_ipv6(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_ipv6([H|Rest], Acc, URI) when is_list(H) ->
    parse_ipv6(H ++ Rest, Acc, URI);
parse_ipv6([$]|Rest], Acc, URI) ->
    parse_ipv6_end(Rest, [], URI#{host => validate_ipv6_address(lists:reverse(Acc))});
parse_ipv6([Char|Rest], Acc, URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6(Rest, [Char|Acc], URI);
        false ->
            io:format("# DEBUG Char: >>~c<<~n", [Char]),
            io:format("# DEBUG Rest: >>~s<<~n", [Rest]),
            throw(uri_parse_error)
    end;
parse_ipv6([], _Acc, _URI) ->
    throw(uri_parse_error).

%% Check if char is allowed in IPv6 addresses
-spec is_ipv6(char()) -> boolean().
is_ipv6($:) -> true;
is_ipv6($.) -> true;
is_ipv6(Char) -> is_hex_digit(Char).


-spec parse_ipv6_bin_end(binary(), uri_map()) -> {binary(), uri_map()}.
parse_ipv6_bin_end(?STRING_REST($:, Rest), URI) ->
    {T, URI1} = parse_port(Rest, URI),
    {H, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    Port = binary_to_integer(H),
    {Rest, URI1#{port => Port}};
parse_ipv6_bin_end(?STRING_REST($/, Rest), URI) ->
    {T, URI1} = parse_segment(Rest, URI),  % path-abempty
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_ipv6_bin_end(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_ipv6_bin_end(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_ipv6_bin_end(?STRING_REST(Char, Rest), URI) ->
    case is_ipv6(Char) of
        true -> parse_ipv6_bin_end(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_ipv6_bin_end(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_ipv6_end(iolist(), list(), uri_map()) -> uri_map().
parse_ipv6_end(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_ipv6_end(unicode:characters_to_list(Str), Acc, URI);
parse_ipv6_end([H|Rest], Acc, URI) when is_binary(H) ->
    parse_ipv6_end(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_ipv6_end([H|Rest], Acc, URI) when is_list(H) ->
    parse_ipv6_end(H ++ Rest, Acc, URI);
parse_ipv6_end([$:|Rest], _Acc, URI) ->
    parse_port(Rest, [], URI);
parse_ipv6_end([$/|Rest], _Acc, URI) ->
    parse_segment(Rest, [$/], URI);  % path-abempty
parse_ipv6_end([$?|Rest], _Acc, URI) ->
    parse_query(Rest, [$?], URI);  % path-empty ?query
parse_ipv6_end([$#|Rest], _Acc, URI) ->
    parse_fragment(Rest, [], URI);  % path-empty
parse_ipv6_end([], _Acc, URI) ->
    URI.


-spec validate_ipv6_address(list()) -> list().
validate_ipv6_address(Addr) ->
    case inet:parse_ipv6strict_address(Addr) of
        {ok, _} -> Addr;
        {error, _} -> throw(uri_parse_error)
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
    {Path, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{path => ?STRING_REST($/, Path)}};
parse_port(?STRING_REST($?, Rest), URI) ->
    {T, URI1} = parse_query(Rest, URI),  % path-empty ?query
    {Query, _} = split_binary(Rest, byte_size(Rest) - byte_size_exl_head(T)),
    {Rest, URI1#{query => ?STRING_REST($?, Query)}};
parse_port(?STRING_REST($#, Rest), URI) ->
    {T, URI1} = parse_fragment(Rest, URI),  % path-empty
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_port(?STRING_REST(Char, Rest), URI) ->
    case is_digit(Char) of
        true -> parse_port(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_port(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_port(iolist(), list(), uri_map()) -> uri_map().
parse_port(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_port(unicode:characters_to_list(Str), Acc, URI);
parse_port([H|Rest], Acc, URI) when is_binary(H) ->
    parse_port(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_port([H|Rest], Acc, URI) when is_list(H) ->
    parse_port(H ++ Rest, Acc, URI);
parse_port([$/|Rest], Acc, URI) ->
    {Port, _} = string:to_integer(lists:reverse(Acc)),
    parse_segment(Rest, [$/], URI#{port => Port});  % path-abempty
parse_port([$?|Rest], Acc, URI) ->
    {Port, _} = string:to_integer(lists:reverse(Acc)),
    parse_query(Rest, [$?], URI#{port => Port});  % path-empty ?query
parse_port([$#|Rest], Acc, URI) ->
    {Port, _} = string:to_integer(lists:reverse(Acc)),
    parse_fragment(Rest, [], URI#{port => Port});  % path-empty
parse_port([Char|Rest], Acc, URI) ->
    case is_digit(Char) of
        true -> parse_port(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_port([], Acc, URI) ->
    {Port, _} = string:to_integer(lists:reverse(Acc)),
    URI#{port => Port}.


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
    {Fragment, _} = split_binary(Rest, byte_size(Rest) - byte_size(T)),
    {Rest, URI1#{fragment => Fragment}};
parse_query(?STRING_REST(Char, Rest), URI) ->
    case is_query(Char) of
        true -> parse_query(Rest, URI);
        false -> throw(uri_parse_error)
    end;
parse_query(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_query(iolist(), list(), uri_map()) -> uri_map().
parse_query(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_query(unicode:characters_to_list(Str), Acc, URI);
parse_query([H|Rest], Acc, URI) when is_binary(H) ->
    parse_query(unicode:characters_to_list(H, utf8) ++ Rest,
               Acc, URI);
parse_query([H|Rest], Acc, URI) when is_list(H) ->
    parse_query(H ++ Rest, Acc, URI);
parse_query([$#|Rest], Acc, URI) ->
    parse_fragment(Rest, [], URI#{query => lists:reverse(Acc)});
parse_query([Char|Rest], Acc, URI) ->
    case is_query(Char) of
        true -> parse_query(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_query([], Acc, URI) ->
    URI#{query => lists:reverse(Acc)}.

%% Check if char is allowed in query
-spec is_query(char()) -> boolean().
is_query($/) -> true;
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
        false -> throw(uri_parse_error)
    end;
parse_fragment(?STRING_EMPTY, URI) ->
    {?STRING_EMPTY, URI}.

-spec parse_fragment(iolist(), list(), uri_map()) -> uri_map().
parse_fragment(?STRING(Str), Acc, URI) when is_list(Acc) ->
    parse_fragment(unicode:characters_to_list(Str), Acc, URI);
parse_fragment([H|Rest], Acc, URI) when is_binary(H) ->
    parse_fragment(unicode:characters_to_list(H, utf8) ++ Rest,
                   Acc, URI);
parse_fragment([H|Rest], Acc, URI) when is_list(H) ->
    parse_fragment(H ++ Rest, Acc, URI);
parse_fragment([Char|Rest], Acc, URI) ->
    case is_fragment(Char) of
        true -> parse_fragment(Rest, [Char|Acc], URI);
        false -> throw(uri_parse_error)
    end;
parse_fragment([], Acc, URI) ->
    URI#{fragment => lists:reverse(Acc)}.

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
%% %% Return true if input char is reserved.
%% -spec is_reserved(char()) -> boolean().
%% is_reserved(Char) ->
%%     is_gen_delim(Char) orelse is_sub_delim(Char).

%% %% Check if char is reserved.
%% -spec is_gen_delim(char()) -> boolean().
%% is_gen_delim($:) -> true;
%% is_gen_delim($/) -> true;
%% is_gen_delim($?) -> true;
%% is_gen_delim($#) -> true;
%% is_gen_delim($[) -> true;
%% is_gen_delim($]) -> true;
%% is_gen_delim($@) -> true;
%% is_gen_delim(_) -> false.

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

%% Returns the size of a binary exluding the first element.
%% Used in calls to split_binary().
-spec byte_size_exl_head(binary()) -> number().
byte_size_exl_head(<<>>) -> 0;
byte_size_exl_head(Binary) -> byte_size(Binary) + 1.

% Remove brackets from binary
-spec remove_brackets(binary()) -> binary().
remove_brackets(?STRING_REST($[,Addr)) ->
    A1 = binary:replace(Addr, <<$[>>, <<>>),
    binary:replace(A1, <<$]>>, <<>>);
remove_brackets(Addr) -> Addr.
