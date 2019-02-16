%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
-module(uri_string_recompose).

-compile(export_all).

-proptest(eqc).
-proptest([triq,proper]).

-ifndef(EQC).
-ifndef(PROPER).
-ifndef(TRIQ).
-define(EQC,true).
-endif.
-endif.
-endif.

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-define(MOD_eqc,eqc).

-else.
-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(MOD_eqc,proper).

-else.
-ifdef(TRIQ).
-define(MOD_eqc,triq).
-include_lib("triq/include/triq.hrl").

-endif.
-endif.
-endif.


-define(STRING_REST(MatchStr, Rest), <<MatchStr/utf8, Rest/binary>>).

-define(SCHEME, {scheme, scheme()}).
-define(USER, {userinfo, unicode()}).
-define(HOST, {host, host_map()}).
-define(PORT, {port, port()}).
-define(PATH_ABE, {path, path_abempty_map()}).
-define(PATH_ABS, {path, path_absolute_map()}).
-define(PATH_NOS, {path, path_noscheme_map()}).
-define(PATH_ROO, {path, path_rootless_map()}).
-define(PATH_EMP, {path, path_empty_map()}).
-define(QUERY, {query, query_map()}).
-define(FRAGMENT, {fragment, fragment_map()}).

%% Non-unicode
-define(USER_NU, {userinfo, non_unicode()}).
-define(HOST_NU, {host, host_map_nu()}).
-define(PATH_ABE_NU, {path, path_abempty_map_nu()}).
-define(PATH_ABS_NU, {path, path_absolute_map_nu()}).
-define(PATH_NOS_NU, {path, path_noscheme_map_nu()}).
-define(PATH_ROO_NU, {path, path_rootless_map_nu()}).
-define(QUERY_NU, {query, query_map_nu()}).
-define(FRAGMENT_NU, {fragment, fragment_map_nu()}).

%%%========================================================================
%%% Properties
%%%========================================================================

prop_recompose() ->
    ?FORALL(Map, map_no_unicode(),
           Map =:= uri_string:parse(uri_string:recompose(Map))).

prop_normalize() ->
    ?FORALL(Map, map(),
            uri_string:normalize(Map, [return_map]) =:=
                uri_string:normalize(uri_string:parse(uri_string:recompose(Map)),
                                     [return_map])).

%% Stats
prop_map_key_length_collect() ->
    ?FORALL(List, map(),
            collect(length(maps:keys(List)), true)).

prop_map_collect() ->
    ?FORALL(List, map(),
            collect(lists:sort(maps:keys(List)), true)).

prop_scheme_collect() ->
    ?FORALL(List, scheme(),
            collect(length(List), true)).


%%%========================================================================
%%% Generators
%%%========================================================================

map() ->
    ?LET(Gen, comp_proplist(), proplist_to_map(Gen)).

map_no_unicode() ->
    ?LET(Gen, comp_proplist_nu(), proplist_to_map(Gen)).

comp_proplist() ->
    frequency([
               {2, [?SCHEME,?PATH_ABS]},
               {2, [?SCHEME,?PATH_ROO]},
               {2, [?SCHEME,?PATH_EMP]},
               {2, [?SCHEME,?HOST,?PATH_ABE]},
               {2, [?SCHEME,?USER,?HOST,?PATH_ABE]},
               {2, [?SCHEME,?HOST,?PORT,?PATH_ABE]},
               {2, [?SCHEME,?USER,?HOST,?PORT,?PATH_ABE]},

               {2, [?PATH_ABS]},
               {2, [?PATH_NOS]},
               {2, [?PATH_EMP]},
               {2, [?HOST,?PATH_ABE]},
               {2, [?USER,?HOST,?PATH_ABE]},
               {2, [?HOST,?PORT,?PATH_ABE]},
               {2, [?USER,?HOST,?PORT,?PATH_ABE]},


               {2, [?SCHEME,?PATH_ABS,?QUERY]},
               {2, [?SCHEME,?PATH_ROO,?QUERY]},
               {2, [?SCHEME,?PATH_EMP,?QUERY]},
               {2, [?SCHEME,?HOST,?PATH_ABE,?QUERY]},
               {2, [?SCHEME,?USER,?HOST,?PATH_ABE,?QUERY]},
               {2, [?SCHEME,?HOST,?PORT,?PATH_ABE,?QUERY]},
               {2, [?SCHEME,?USER,?HOST,?PORT,?PATH_ABE,?QUERY]},

               {2, [?PATH_ABS,?QUERY]},
               {2, [?PATH_NOS,?QUERY]},
               {2, [?PATH_EMP,?QUERY]},
               {2, [?HOST,?PATH_ABE,?QUERY]},
               {2, [?USER,?HOST,?PATH_ABE,?QUERY]},
               {2, [?HOST,?PORT,?PATH_ABE,?QUERY]},
               {2, [?USER,?HOST,?PORT,?PATH_ABE,?QUERY]},


               {2, [?SCHEME,?PATH_ABS,?FRAGMENT]},
               {2, [?SCHEME,?PATH_ROO,?FRAGMENT]},
               {2, [?SCHEME,?PATH_EMP,?FRAGMENT]},
               {2, [?SCHEME,?HOST,?PATH_ABE,?FRAGMENT]},
               {2, [?SCHEME,?USER,?HOST,?PATH_ABE,?FRAGMENT]},
               {2, [?SCHEME,?HOST,?PORT,?PATH_ABE,?FRAGMENT]},
               {2, [?SCHEME,?USER,?HOST,?PORT,?PATH_ABE,?FRAGMENT]},

               {2, [?PATH_ABS,?FRAGMENT]},
               {2, [?PATH_NOS,?FRAGMENT]},
               {2, [?PATH_EMP,?FRAGMENT]},
               {2, [?HOST,?PATH_ABE,?FRAGMENT]},
               {2, [?USER,?HOST,?PATH_ABE,?FRAGMENT]},
               {2, [?HOST,?PORT,?PATH_ABE,?FRAGMENT]},
               {2, [?USER,?HOST,?PORT,?PATH_ABE,?FRAGMENT]},


               {2, [?SCHEME,?PATH_ABS,?QUERY,?FRAGMENT]},
               {2, [?SCHEME,?PATH_ROO,?QUERY,?FRAGMENT]},
               {2, [?SCHEME,?PATH_EMP,?QUERY,?FRAGMENT]},
               {2, [?SCHEME,?HOST,?PATH_ABE,?QUERY,?FRAGMENT]},
               {2, [?SCHEME,?USER,?HOST,?PATH_ABE,?QUERY,?FRAGMENT]},
               {2, [?SCHEME,?HOST,?PORT,?PATH_ABE,?QUERY,?FRAGMENT]},
               {2, [?SCHEME,?USER,?HOST,?PORT,?PATH_ABE,?QUERY,?FRAGMENT]},

               {2, [?PATH_ABS,?QUERY,?FRAGMENT]},
               {2, [?PATH_NOS,?QUERY,?FRAGMENT]},
               {2, [?PATH_EMP,?QUERY,?FRAGMENT]},
               {2, [?HOST,?PATH_ABE,?QUERY,?FRAGMENT]},
               {2, [?USER,?HOST,?PATH_ABE,?QUERY,?FRAGMENT]},
               {2, [?HOST,?PORT,?PATH_ABE,?QUERY,?FRAGMENT]},
               {2, [?USER,?HOST,?PORT,?PATH_ABE,?QUERY,?FRAGMENT]}
              ]).

comp_proplist_nu() ->
    frequency([
               {2, [?SCHEME,?PATH_ABS_NU]},
               {2, [?SCHEME,?PATH_ROO_NU]},
               {2, [?SCHEME,?PATH_EMP]},
               {2, [?SCHEME,?HOST_NU,?PATH_ABE_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PATH_ABE_NU]},
               {2, [?SCHEME,?HOST_NU,?PORT,?PATH_ABE_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU]},

               {2, [?PATH_ABS_NU]},
               {2, [?PATH_NOS_NU]},
               {2, [?PATH_EMP]},
               {2, [?HOST_NU,?PATH_ABE_NU]},
               {2, [?USER_NU,?HOST_NU,?PATH_ABE_NU]},
               {2, [?HOST_NU,?PORT,?PATH_ABE_NU]},
               {2, [?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU]},


               {2, [?SCHEME,?PATH_ABS_NU,?QUERY_NU]},
               {2, [?SCHEME,?PATH_ROO_NU,?QUERY_NU]},
               {2, [?SCHEME,?PATH_EMP,?QUERY_NU]},
               {2, [?SCHEME,?HOST_NU,?PATH_ABE_NU,?QUERY_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PATH_ABE_NU,?QUERY_NU]},
               {2, [?SCHEME,?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU]},

               {2, [?PATH_ABS_NU,?QUERY_NU]},
               {2, [?PATH_NOS_NU,?QUERY_NU]},
               {2, [?PATH_EMP,?QUERY_NU]},
               {2, [?HOST_NU,?PATH_ABE_NU,?QUERY_NU]},
               {2, [?USER_NU,?HOST_NU,?PATH_ABE_NU,?QUERY_NU]},
               {2, [?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU]},
               {2, [?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU]},


               {2, [?SCHEME,?PATH_ABS_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?PATH_ROO_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?PATH_EMP,?FRAGMENT_NU]},
               {2, [?SCHEME,?HOST_NU,?PATH_ABE_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PATH_ABE_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?HOST_NU,?PORT,?PATH_ABE_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU,?FRAGMENT_NU]},

               {2, [?PATH_ABS_NU,?FRAGMENT_NU]},
               {2, [?PATH_NOS_NU,?FRAGMENT_NU]},
               {2, [?PATH_EMP,?FRAGMENT_NU]},
               {2, [?HOST_NU,?PATH_ABE_NU,?FRAGMENT_NU]},
               {2, [?USER_NU,?HOST_NU,?PATH_ABE_NU,?FRAGMENT_NU]},
               {2, [?HOST_NU,?PORT,?PATH_ABE_NU,?FRAGMENT_NU]},
               {2, [?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU,?FRAGMENT_NU]},


               {2, [?SCHEME,?PATH_ABS_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?PATH_ROO_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?PATH_EMP,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?HOST_NU,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?SCHEME,?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},

               {2, [?PATH_ABS_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?PATH_NOS_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?PATH_EMP,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?HOST_NU,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?USER_NU,?HOST_NU,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]},
               {2, [?USER_NU,?HOST_NU,?PORT,?PATH_ABE_NU,?QUERY_NU,?FRAGMENT_NU]}
              ]).


%%-------------------------------------------------------------------------
%% Path
%%-------------------------------------------------------------------------
path_abempty_map() ->
    frequency([{90, path_abe_map()},
               {10, path_empty_map()}]).

path_abempty_map_nu() ->
    frequency([{90, path_abe_map_nu()},
               {10, path_empty_map()}]).


path_abe_map() ->
    ?SIZED(Length, path_abe_map(Length, [])).
%%
path_abe_map(0, Segments) ->
    ?LET(Gen, Segments, lists:append(Gen));
path_abe_map(N, Segments) ->
    path_abe_map(N-1, [slash(),segment()|Segments]).

path_abe_map_nu() ->
    ?SIZED(Length, path_abe_map_nu(Length, [])).
%%
path_abe_map_nu(0, Segments) ->
    ?LET(Gen, Segments, lists:append(Gen));
path_abe_map_nu(N, Segments) ->
    path_abe_map_nu(N-1, [slash(),segment_nu()|Segments]).


path_absolute_map() ->
    ?SIZED(Length, path_absolute_map(Length, [])).
%%
path_absolute_map(0, Segments) ->
    ?LET(Gen, [slash(),segment_nz()|Segments], lists:append(Gen));
path_absolute_map(N, Segments) ->
    path_absolute_map(N-1, [slash(),segment()|Segments]).

path_absolute_map_nu() ->
    ?SIZED(Length, path_absolute_map_nu(Length, [])).
%%
path_absolute_map_nu(0, Segments) ->
    ?LET(Gen, [slash(),segment_nz_nu()|Segments], lists:append(Gen));
path_absolute_map_nu(N, Segments) ->
    path_absolute_map_nu(N-1, [slash(),segment_nu()|Segments]).


path_noscheme_map() ->
    ?SIZED(Length, path_noscheme_map(Length, [])).
%%
path_noscheme_map(0, Segments) ->
    ?LET(Gen, [segment_nz_nc()|Segments], lists:append(Gen));
path_noscheme_map(N, Segments) ->
    path_noscheme_map(N-1, [slash(),segment()|Segments]).

path_noscheme_map_nu() ->
    ?SIZED(Length, path_noscheme_map_nu(Length, [])).
%%
path_noscheme_map_nu(0, Segments) ->
    ?LET(Gen, [segment_nz_nc_nu()|Segments], lists:append(Gen));
path_noscheme_map_nu(N, Segments) ->
    path_noscheme_map_nu(N-1, [slash(),segment_nu()|Segments]).


path_rootless_map() ->
    ?SIZED(Length, path_rootless_map(Length, [])).
%%
path_rootless_map(0, Segments) ->
    ?LET(Gen, [segment_nz()|Segments], lists:append(Gen));
path_rootless_map(N, Segments) ->
    path_rootless_map(N-1, [slash(),segment()|Segments]).

path_rootless_map_nu() ->
    ?SIZED(Length, path_rootless_map_nu(Length, [])).
%%
path_rootless_map_nu(0, Segments) ->
    ?LET(Gen, [segment_nz_nu()|Segments], lists:append(Gen));
path_rootless_map_nu(N, Segments) ->
    path_rootless_map_nu(N-1, [slash(),segment_nu()|Segments]).


segment_nz() ->
    non_empty(segment()).

segment_nz_nu() ->
    non_empty(segment_nu()).


segment_nz_nc() ->
    ?LET(Gen,
         non_empty(list(frequency([{30, unreserved()},
                                   {10, ptc_encoded_reserved()},
                                   {10, sub_delims()},
                                   {10, unicode_char()},
                                   {5, oneof([$@])}
                                  ]))),
         lists:flatten(Gen)).

segment_nz_nc_nu() ->
    ?LET(Gen,
         non_empty(list(frequency([{30, unreserved()},
                                   {10, ptc_encoded_reserved()},
                                   {10, sub_delims()},
                                   {5, oneof([$@])}
                                  ]))),
         lists:flatten(Gen)).

segment() ->
    ?LET(Gen,
         list(frequency([{30, unreserved()},
                         {10, ptc_encoded_reserved()},
                         {10, sub_delims()},
                         {10, unicode_char()},
                         {5, oneof([$:, $@])}
                        ])),
         lists:flatten(Gen)).

segment_nu() ->
    ?LET(Gen,
         list(frequency([{30, unreserved()},
                         {10, ptc_encoded_reserved()},
                         {10, sub_delims()},
                         {5, oneof([$:, $@])}
                        ])),
         lists:flatten(Gen)).

slash() ->
    "/".

path_empty_map() ->
    "".


%%-------------------------------------------------------------------------
%% Host
%%-------------------------------------------------------------------------
host_map() ->
    frequency([{30, reg_name()},
               {30, ip_address()}
              ]).

host_map_nu() ->
    frequency([{30, reg_name_nu()},
               {30, ip_address()}
              ]).

reg_name() ->
    ?LET(Gen,
         list(frequency([{30, alpha()},
                         {10, sub_delims()},
                         {10, ptc_encoded_reserved()},
                         {10, unicode_char()}
                        ])),
         lists:flatten(Gen)).

reg_name_nu() ->
    ?LET(Gen,
         list(frequency([{30, alpha()},
                         {10, sub_delims()},
                         {10, ptc_encoded_reserved()}
                        ])),
         lists:flatten(Gen)).


ip_address() ->
    oneof(["127.0.0.1", "::127.0.0.1",
           "2001:0db8:0000:0000:0000:0000:1428:07ab",
           "2001:0db8:0000:0000:0000::1428:07ab",
           "2001:0db8:0:0:0:0:1428:07ab",
           "2001:0db8:0::0:1428:07ab"]).

%% Generating only reg-names
host_uri() ->
    ?LET(Gen,
         non_empty(list(frequency([{30, unreserved()},
                                   {10, sub_delims()},
                                   {10, ptc_encoded_reserved()},
                                   {10, pct_encoded()}
                                  ]))),
         lists:flatten(Gen)).

%%-------------------------------------------------------------------------
%% Port, Query, Fragment
%%-------------------------------------------------------------------------
port() ->
    frequency([{10, undefined},
               {10, range(1,65535)}
              ]).

query_map() ->
    unicode().

query_map_nu() ->
    non_unicode().


query_uri() ->
    [$?| non_empty(list(frequency([{20, pchar()},
                                   {5, oneof([$/, $?])} % punctuation
                                  ])))].

fragment_map() ->
    unicode().

fragment_map_nu() ->
    non_unicode().


fragment_uri() ->
    [$?| non_empty(list(frequency([{20, pchar()},
                                   {5, oneof([$/, $?])} % punctuation
                                  ])))].


%%-------------------------------------------------------------------------
%% Scheme
%%-------------------------------------------------------------------------
scheme() ->
    ?SIZED(Length, scheme_start(Length, [])).
%%
scheme_start(0, L) ->
    ?LET(Gen, L, lists:reverse(Gen));
scheme_start(N, L) ->
    scheme(N-1,[alpha()|L]).

scheme(0, L) ->
    ?LET(Gen, L, lists:reverse(Gen));
scheme(N, L) ->
    scheme(N-1, [scheme_char()|L]).


%%-------------------------------------------------------------------------
%% Misc
%%-------------------------------------------------------------------------
unicode() ->
    list(frequency([{20, alpha()},                    % alpha
                    {10, digit()},                    % digit
                    {10, unicode_char()}              % unicode
                   ])).

non_unicode() ->
    list(frequency([{20, alpha()},                   % alpha
                    {10, digit()}                    % digit
                   ])).

scheme_char() ->
    frequency([{20, alpha()},                    % alpha
               {20, digit()},                    % digit
               {5, oneof([$+, $-, $.])}          % punctuation
              ]).

sub_delims() ->
    oneof([$!, $$, $&, $', $(, $),
           $*, $+, $,,$;, $=]).

pchar() ->
    frequency([{20, unreserved()},
               {5, ptc_encoded_reserved()},
               {5, pct_encoded()},
               {5, sub_delims()},
               {1, oneof([$:, $@])}              % punctuation
              ]).

unreserved() ->
    frequency([{20, alpha()},
               {5, digit()},
               {1, oneof([$-, $., $_, $~])}      % punctuation
              ]).

unicode_char() ->
    range(913, 1023).

alpha() ->
    frequency([{20, range($a, $z)},              % letters
               {20, range($A, $Z)}]).            % letters

digit() ->
    range($0, $9).                               % numbers

pct_encoded() ->
    oneof(["%C3%A4", "%C3%A5", "%C3%B6"]).

%%-------------------------------------------------------------------------
%% [RFC 3986, Chapter 2.2. Reserved Characters]
%%
%%   reserved    = gen-delims / sub-delims
%%
%%   gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
%%                 3A    2F    3F    23    5B    5D    40
%%   sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
%%                 21    24    26    27    28    29
%%               / "*" / "+" / "," / ";" / "="
%%                 2A    2B    2C    3B    3D
%%-------------------------------------------------------------------------
ptc_encoded_reserved() ->
    oneof(["%3A","%2F","%3F","%23","%5B","%5D","%40",
           "%21","%24","%26","%27","%28","%29",
           "%2A","%2B","%2C","%3B","3D"]).

%%%========================================================================
%%% Helpers
%%%========================================================================
proplist_to_map(L) ->
    lists:foldl(fun({K,V},M) -> M#{K => V};
                  (_,M) -> M
               end, #{}, L).

map_scheme_host_to_lower(Map) ->
    Fun = fun (scheme,V) ->
                  string:to_lower(V);
              (host,V) ->
                  string:to_lower(V);
              (_,V) ->
                  V
          end,
    maps:map(Fun, Map).
