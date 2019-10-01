%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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
-module(binary).
%%
%% Implemented in this module:
-export([replace/3,replace/4]).

-export_type([cp/0]).

-opaque cp() :: {'am' | 'bm', reference()}.
-type part() :: {Start :: non_neg_integer(), Length :: integer()}.

%%% BIFs.

-export([at/2, bin_to_list/1, bin_to_list/2, bin_to_list/3,
         compile_pattern/1, copy/1, copy/2, decode_unsigned/1,
         decode_unsigned/2, encode_unsigned/1, encode_unsigned/2,
         first/1, last/1, list_to_bin/1, longest_common_prefix/1,
         longest_common_suffix/1, match/2, match/3, matches/2,
         matches/3, part/2, part/3, referenced_byte_size/1,
         split/2, split/3]).

-spec at(Subject, Pos) -> byte() when
      Subject :: binary(),
      Pos :: non_neg_integer().

at(_, _) ->
    erlang:nif_error(undef).

-spec bin_to_list(Subject) -> [byte()] when
      Subject :: binary().

bin_to_list(Subject) ->
    binary_to_list(Subject).

-spec bin_to_list(Subject, PosLen) -> [byte()] when
      Subject :: binary(),
      PosLen :: part().

bin_to_list(Subject, {Pos, Len}) ->
    bin_to_list(Subject, Pos, Len);
bin_to_list(_Subject, _BadArg) ->
    erlang:error(badarg).

-spec bin_to_list(Subject, Pos, Len) -> [byte()] when
      Subject :: binary(),
      Pos :: non_neg_integer(),
      Len :: integer().

bin_to_list(Subject, Pos, Len) when not is_binary(Subject);
                                    not is_integer(Pos);
                                    not is_integer(Len) ->
    %% binary_to_list/3 allows bitstrings as long as the slice fits, and we
    %% want to badarg when Pos/Len aren't integers instead of raising badarith
    %% when adjusting args for binary_to_list/3.
    erlang:error(badarg);
bin_to_list(Subject, Pos, 0) when Pos >= 0, Pos =< byte_size(Subject) ->
    %% binary_to_list/3 doesn't handle this case.
    [];
bin_to_list(_Subject, _Pos, 0) ->
    erlang:error(badarg);
bin_to_list(Subject, Pos, Len) when Len < 0 ->
    bin_to_list(Subject, Pos + Len, -Len);
bin_to_list(Subject, Pos, Len) when Len > 0 ->
    binary_to_list(Subject, Pos + 1, Pos + Len).

-spec compile_pattern(Pattern) -> cp() when
      Pattern :: binary() | [binary()].

compile_pattern(_) ->
    erlang:nif_error(undef).

-spec copy(Subject) -> binary() when
      Subject :: binary().

copy(_) ->
    erlang:nif_error(undef).

-spec copy(Subject, N) -> binary() when
      Subject :: binary(),
      N :: non_neg_integer().

copy(_, _) ->
    erlang:nif_error(undef).

-spec decode_unsigned(Subject) -> Unsigned when
      Subject :: binary(),
      Unsigned :: non_neg_integer().

decode_unsigned(_) ->
    erlang:nif_error(undef).

-spec decode_unsigned(Subject, Endianness) -> Unsigned when
      Subject :: binary(),
      Endianness :: big | little,
      Unsigned :: non_neg_integer().

decode_unsigned(_, _) ->
    erlang:nif_error(undef).

-spec encode_unsigned(Unsigned) -> binary() when
      Unsigned :: non_neg_integer().

encode_unsigned(_) ->
    erlang:nif_error(undef).

-spec encode_unsigned(Unsigned, Endianness) -> binary() when
      Unsigned :: non_neg_integer(),
      Endianness :: big | little.

encode_unsigned(_, _) ->
    erlang:nif_error(undef).

-spec first(Subject) -> byte() when
      Subject :: binary().

first(_) ->
    erlang:nif_error(undef).

-spec last(Subject) -> byte() when
      Subject :: binary().

last(_) ->
    erlang:nif_error(undef).

-spec list_to_bin(ByteList) -> binary() when
      ByteList :: iolist().

list_to_bin(_) ->
    erlang:nif_error(undef).

-spec longest_common_prefix(Binaries) -> non_neg_integer() when
      Binaries :: [binary()].

longest_common_prefix(_) ->
    erlang:nif_error(undef).

-spec longest_common_suffix(Binaries) -> non_neg_integer() when
      Binaries :: [binary()].

longest_common_suffix(_) ->
    erlang:nif_error(undef).

-spec match(Subject, Pattern) -> Found | nomatch when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: part().

match(_, _) ->
    erlang:nif_error(undef).

-spec match(Subject, Pattern, Options) -> Found | nomatch when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: part(),
      Options :: [Option],
      Option :: {scope, part()}.

match(_, _, _) ->
    erlang:nif_error(undef).

-spec matches(Subject, Pattern) -> Found when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: [part()].

matches(_, _) ->
    erlang:nif_error(undef).

-spec matches(Subject, Pattern, Options) -> Found when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Found :: [part()],
      Options :: [Option],
      Option :: {scope, part()}.

matches(_, _, _) ->
    erlang:nif_error(undef).

-spec part(Subject, PosLen) -> binary() when
      Subject :: binary(),
      PosLen :: part().

part(_, _) ->
    erlang:nif_error(undef).

-spec part(Subject, Pos, Len) -> binary() when
      Subject :: binary(),
      Pos :: non_neg_integer(),
      Len :: integer().

part(_, _, _) ->
    erlang:nif_error(undef).

-spec referenced_byte_size(Binary) -> non_neg_integer() when
      Binary :: binary().

referenced_byte_size(_) ->
    erlang:nif_error(undef).

-spec split(Subject, Pattern) -> Parts when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Parts :: [binary()].

split(_, _) ->
    erlang:nif_error(undef).

-spec split(Subject, Pattern, Options) -> Parts when
      Subject :: binary(),
      Pattern :: binary() | [binary()] | cp(),
      Options :: [Option],
      Option :: {scope, part()} | trim | global | trim_all,
      Parts :: [binary()].

split(_, _, _) ->
    erlang:nif_error(undef).

%%% End of BIFs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec replace(Subject, Pattern, Replacement) -> Result when
      Subject :: binary(),
      Pattern :: binary() | [ binary() ] | cp(),
      Replacement :: binary(),
      Result :: binary().

replace(H,N,R) ->
    replace(H,N,R,[]).

-spec replace(Subject, Pattern, Replacement, Options) -> Result when
      Subject :: binary(),
      Pattern :: binary() | [ binary() ] | cp(),
      Replacement :: binary(),
      Options :: [Option],
      Option :: global | {scope, part()} | {insert_replaced, InsPos},
      InsPos :: OnePos | [ OnePos ],
      OnePos :: non_neg_integer(),
      Result :: binary().

replace(Haystack,Needles,Replacement,Options) ->
    try
	true = is_binary(Replacement), % Make badarg instead of function clause
	{Part,Global,Insert} = get_opts_replace(Options,{no,false,[]}),
	Moptlist = case Part of
		       no ->
			   [];
		       {A,B} ->
			   [{scope,{A,B}}]
		   end,
	MList = if
		    Global ->
			binary:matches(Haystack,Needles,Moptlist);
		    true ->
			case binary:match(Haystack,Needles,Moptlist) of
			    nomatch -> [];
			    Match -> [Match]
			end
		end,
	ReplList = case Insert of
		       [] ->
			   Replacement;
		       Y when is_integer(Y) ->
			   splitat(Replacement,0,[Y]);
		       Li when is_list(Li) ->
			   splitat(Replacement,0,lists:sort(Li))
		   end,
	erlang:iolist_to_binary(do_replace(Haystack,MList,ReplList,0))
   catch
       _:_ ->
	    erlang:error(badarg)
   end.


do_replace(H,[],_,N) ->
    [binary:part(H,{N,byte_size(H)-N})];
do_replace(H,[{A,B}|T],Replacement,N) ->
    [binary:part(H,{N,A-N}),
     if
	 is_list(Replacement) ->
	     do_insert(Replacement, binary:part(H,{A,B}));
	 true ->
	     Replacement
     end
     | do_replace(H,T,Replacement,A+B)].

do_insert([X],_) ->
    [X];
do_insert([H|T],R) ->
    [H,R|do_insert(T,R)].

splitat(H,N,[]) ->
    [binary:part(H,{N,byte_size(H)-N})];
splitat(H,N,[I|T]) ->
    [binary:part(H,{N,I-N})|splitat(H,I,T)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_opts_replace([],{Part,Global,Insert}) ->
    {Part,Global,Insert};
get_opts_replace([{scope,{A,B}} | T],{_Part,Global,Insert}) ->
    get_opts_replace(T,{{A,B},Global,Insert});
get_opts_replace([global | T],{Part,_Global,Insert}) ->
    get_opts_replace(T,{Part,true,Insert});
get_opts_replace([{insert_replaced,N} | T],{Part,Global,_Insert}) ->
    get_opts_replace(T,{Part,Global,N});
get_opts_replace(_,_) ->
    throw(badopt).

