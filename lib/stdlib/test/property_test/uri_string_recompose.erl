%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
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


%%%========================================================================
%%% Properties
%%%========================================================================

prop_recompose() ->
    ?FORALL(Map, map(),
            Map =:= uri_string:parse(uri_string:recompose(Map))
           ).

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


%%-------------------------------------------------------------------------
%% Path
%%-------------------------------------------------------------------------
path_abempty_map() ->
    frequency([{90, path_abe_map()},
               {10, path_empty_map()}]).

path_abe_map() ->
    ?SIZED(Length, path_abe_map(Length, [])).
%%
path_abe_map(0, Segments) ->
    ?LET(Gen, Segments, lists:append(Gen));
path_abe_map(N, Segments) ->
    path_abe_map(N-1, [slash(),segment()|Segments]).


path_absolute_map() ->
    ?SIZED(Length, path_absolute_map(Length, [])).
%%
path_absolute_map(0, Segments) ->
    ?LET(Gen, [slash(),segment_nz()|Segments], lists:append(Gen));
path_absolute_map(N, Segments) ->
    path_absolute_map(N-1, [slash(),segment()|Segments]).


path_noscheme_map() ->
    ?SIZED(Length, path_noscheme_map(Length, [])).
%%
path_noscheme_map(0, Segments) ->
    ?LET(Gen, [segment_nz_nc()|Segments], lists:append(Gen));
path_noscheme_map(N, Segments) ->
    path_noscheme_map(N-1, [slash(),segment()|Segments]).

path_rootless_map() ->
    ?SIZED(Length, path_rootless_map(Length, [])).
%%
path_rootless_map(0, Segments) ->
    ?LET(Gen, [segment_nz()|Segments], lists:append(Gen));
path_rootless_map(N, Segments) ->
    path_rootless_map(N-1, [slash(),segment()|Segments]).


segment_nz() ->
    non_empty(segment()).

segment_nz_nc() ->
    non_empty(list(frequency([{30, unreserved()},
                              {10, sub_delims()},
                              {10, unicode_char()},
                              {5, oneof([$@])}
                             ]))).


segment() ->
    list(frequency([{30, unreserved()},
                    {10, sub_delims()},
                    {10, unicode_char()},
                    {5, oneof([$:, $@])}
                   ])).

slash() ->
    "/".

path_empty_map() ->
    "".


%%-------------------------------------------------------------------------
%% Path
%%-------------------------------------------------------------------------
host_map() ->
    frequency([{30, reg_name()},
               {30, ip_address()}
              ]).


reg_name() ->
    list(frequency([{30, alpha()},
                              {10, sub_delims()},
                              {10, unicode_char()}
                             ])).

ip_address() ->
    oneof(["127.0.0.1", "::127.0.0.1",
           "2001:0db8:0000:0000:0000:0000:1428:07ab",
           "2001:0db8:0000:0000:0000::1428:07ab",
           "2001:0db8:0:0:0:0:1428:07ab",
           "2001:0db8:0::0:1428:07ab"]).

%% Generating only reg-names
host_uri() ->
    non_empty(list(frequency([{30, unreserved()},
                              {10, sub_delims()},
                              {10, pct_encoded()}
                             ]))).

%%-------------------------------------------------------------------------
%% Port, Query, Fragment
%%-------------------------------------------------------------------------
port() ->
    frequency([{10, undefined},
               {10, range(1,65535)}
              ]).

query_map() ->
    unicode().


query_uri() ->
    [$?| non_empty(list(frequency([{20, pchar()},
                                   {5, oneof([$/, $?])} % punctuation
                                  ])))].

fragment_map() ->
    unicode().

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


%%%========================================================================
%%% Helpers
%%%========================================================================
proplist_to_map(L) ->
    lists:foldl(fun({K,V},M) -> M#{K => V};
                  (_,M) -> M
               end, #{}, L).
