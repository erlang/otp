%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(binary).
%%
%% The following functions implemented as BIF's
%%  binary:compile_pattern/1
%%  binary:match/{2,3}
%%  binary:matches/{2,3}
%%  binary:longest_common_prefix/1
%%  binary:longest_common_suffix/1
%%  binary:first/1
%%  binary:last/1
%%  binary:at/2
%%  binary:part/{2,3}
%%  binary:bin_to_list/{1,2,3}
%%  binary:list_to_bin/1
%%  binary:copy/{1,2}
%%  binary:referenced_byte_size/1
%%  binary:decode_unsigned/{1,2}
%% - Not yet:
%%
%% Implemented in this module:
-export([split/2,split/3,replace/3,replace/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% split
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split(H,N) ->
    split(H,N,[]).
split(Haystack,Needles,Options) ->
    try
	{Part,Global,Trim} = get_opts_split(Options,{no,false,false}),
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
	do_split(Haystack,MList,0,Trim)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

do_split(H,[],N,true) when N >= byte_size(H) ->
    [];
do_split(H,[],N,_) ->
    [binary:part(H,{N,byte_size(H)-N})];
do_split(H,[{A,B}|T],N,Trim) ->
    case binary:part(H,{N,A-N}) of
	<<>> ->
	    Rest =  do_split(H,T,A+B,Trim),
	    case {Trim, Rest} of
		{true,[]} ->
		    [];
		_ ->
		    [<<>> | Rest]
	    end;
	Oth ->
	    [Oth | do_split(H,T,A+B,Trim)]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace(H,N,R) ->
    replace(H,N,R,[]).
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

get_opts_split([],{Part,Global,Trim}) ->
    {Part,Global,Trim};
get_opts_split([{scope,{A,B}} | T],{_Part,Global,Trim}) ->
    get_opts_split(T,{{A,B},Global,Trim});
get_opts_split([global | T],{Part,_Global,Trim}) ->
    get_opts_split(T,{Part,true,Trim});
get_opts_split([trim | T],{Part,Global,_Trim}) ->
    get_opts_split(T,{Part,Global,true});
get_opts_split(_,_) ->
    throw(badopt).

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

