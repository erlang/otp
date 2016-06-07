-module(binref).

-export([compile_pattern/1,match/2,match/3,matches/2,matches/3,
	 split/2,split/3,replace/3,replace/4,first/1,last/1,at/2,
	 part/2,part/3,copy/1,copy/2,encode_unsigned/1,encode_unsigned/2,
	 decode_unsigned/1,decode_unsigned/2,referenced_byte_size/1,
	 longest_common_prefix/1,longest_common_suffix/1,bin_to_list/1,
	 bin_to_list/2,bin_to_list/3,list_to_bin/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile_pattern, a dummy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compile_pattern(Pattern) when is_binary(Pattern) ->
    {[Pattern]};
compile_pattern(Pattern) ->
    try
	[ true = is_binary(P) || P <- Pattern ],
	{Pattern}
    catch
	_:_ ->
	    erlang:error(badarg)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% match and matches
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
match(H,N) ->
    match(H,N,[]).
match(Haystack,Needle,Options) when is_binary(Needle) ->
    match(Haystack,[Needle],Options);
match(Haystack,{Needles},Options) ->
    match(Haystack,Needles,Options);
match(Haystack,Needles,Options) ->
    try
	true = is_binary(Haystack) and is_list(Needles), % badarg, not function_clause
	case get_opts_match(Options,nomatch) of
	    nomatch ->
		mloop(Haystack,Needles);
	    {A,B} when B > 0 ->
		<<_:A/binary,SubStack:B/binary,_/binary>> = Haystack,
		mloop(SubStack,Needles,A,B+A);
	    {A,B} when B < 0 ->
		Start = A + B,
		Len = -B,
		<<_:Start/binary,SubStack:Len/binary,_/binary>> = Haystack,
		mloop(SubStack,Needles,Start,Len+Start);
	    _ ->
		nomatch
	end
    catch
	_:_ ->
	    erlang:error(badarg)
    end.
matches(H,N) ->
    matches(H,N,[]).
matches(Haystack,Needle,Options) when is_binary(Needle) ->
    matches(Haystack,[Needle],Options);
matches(Haystack,{Needles},Options) ->
    matches(Haystack,Needles,Options);
matches(Haystack,Needles,Options) ->
    try
	true = is_binary(Haystack) and is_list(Needles), % badarg, not function_clause
	case get_opts_match(Options,nomatch) of
	    nomatch ->
		msloop(Haystack,Needles);
	    {A,B} when B > 0 ->
		<<_:A/binary,SubStack:B/binary,_/binary>> = Haystack,
		msloop(SubStack,Needles,A,B+A);
	    {A,B} when B < 0 ->
		Start = A + B,
		Len = -B,
		<<_:Start/binary,SubStack:Len/binary,_/binary>> = Haystack,
		msloop(SubStack,Needles,Start,Len+Start);
	    _ ->
		[]
	end
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

mloop(Haystack,Needles) ->
    mloop(Haystack,Needles,0,byte_size(Haystack)).

mloop(_Haystack,_Needles,N,M) when N >= M ->
    nomatch;
mloop(Haystack,Needles,N,M) ->
    case mloop2(Haystack,Needles,N,nomatch) of
	nomatch ->
	    %% Not found
	    <<_:8,NewStack/binary>> = Haystack,
	    mloop(NewStack,Needles,N+1,M);
	{N,Len} ->
	    {N,Len}
    end.

msloop(Haystack,Needles) ->
    msloop(Haystack,Needles,0,byte_size(Haystack)).

msloop(_Haystack,_Needles,N,M) when N >= M ->
    [];
msloop(Haystack,Needles,N,M) ->
    case mloop2(Haystack,Needles,N,nomatch) of
	nomatch ->
	    %% Not found
	    <<_:8,NewStack/binary>> = Haystack,
	    msloop(NewStack,Needles,N+1,M);
	{N,Len} ->
	    NewN = N+Len,
	    if
		NewN >= M ->
		    [{N,Len}];
		true ->
		    <<_:Len/binary,NewStack/binary>> = Haystack,
		    [{N,Len} | msloop(NewStack,Needles,NewN,M)]
	    end
    end.

mloop2(_Haystack,[],_N,Res) ->
    Res;
mloop2(Haystack,[Needle|Tail],N,Candidate) ->
    NS = byte_size(Needle),
    case Haystack of
	<<Needle:NS/binary,_/binary>> ->
	    NewCandidate = case Candidate of
			       nomatch ->
				   {N,NS};
			       {N,ONS} when ONS < NS ->
				   {N,NS};
			       Better ->
				   Better
			   end,
	    mloop2(Haystack,Tail,N,NewCandidate);
	_ ->
	    mloop2(Haystack,Tail,N,Candidate)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% split
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
split(H,N) ->
    split(H,N,[]).
split(Haystack,{Needles},Options) ->
    split(Haystack, Needles, Options);
split(Haystack,Needles0,Options) ->
    try
	Needles = if
		      is_list(Needles0) ->
			  Needles0;
		      is_binary(Needles0) ->
			  [Needles0];
		      true ->
			  exit(badtype)
		  end,
	{Part,Global,Trim,TrimAll} =
        get_opts_split(Options,{nomatch,false,false,false}),
	{Start,End,NewStack} =
	    case Part of
		nomatch ->
		    {0,byte_size(Haystack),Haystack};
		{A,B} when B >= 0 ->
		    <<_:A/binary,SubStack:B/binary,_/binary>> = Haystack,
		    {A,A+B,SubStack};
		{A,B} when B < 0 ->
		    S = A + B,
		    L = -B,
		    <<_:S/binary,SubStack:L/binary,_/binary>> = Haystack,
		    {S,S+L,SubStack}
	    end,
	MList = if
		    Global ->
			msloop(NewStack,Needles,Start,End);
		    true ->
			case mloop(NewStack,Needles,Start,End) of
			    nomatch ->
				[];
			    X ->
				[X]
			end
		end,
	do_split(Haystack,MList,0,Trim,TrimAll)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

do_split(H,[],N,true,_) when N >= byte_size(H) ->
    [];
do_split(H,[],N,_,true) when N >= byte_size(H) ->
    [];
do_split(H,[],N,_,_) ->
    [part(H,{N,byte_size(H)-N})];
do_split(H,[{A,B}|T],N,Trim,TrimAll) ->
    case part(H,{N,A-N}) of
	<<>> when TrimAll == true ->
	    do_split(H,T,A+B,Trim,TrimAll);
	<<>> ->
	    Rest =  do_split(H,T,A+B,Trim,TrimAll),
	    case {Trim, Rest} of
		{true,[]} ->
		    [];
		_ ->
		    [<<>> | Rest]
	    end;
	Oth ->
	    [Oth | do_split(H,T,A+B,Trim,TrimAll)]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% replace
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace(H,N,R) ->
    replace(H,N,R,[]).
replace(Haystack,{Needles},Replacement,Options) ->
    replace(Haystack,Needles,Replacement,Options);

replace(Haystack,Needles0,Replacement,Options) ->
    try
	Needles = if
		      is_list(Needles0) ->
			  Needles0;
		      is_binary(Needles0) ->
			  [Needles0];
		      true ->
			  exit(badtype)
		  end,
	true = is_binary(Replacement), % Make badarg instead of function clause
	{Part,Global,Insert} = get_opts_replace(Options,{nomatch,false,[]}),
	{Start,End,NewStack} =
	    case Part of
		nomatch ->
		    {0,byte_size(Haystack),Haystack};
		{A,B} when B >= 0 ->
		    <<_:A/binary,SubStack:B/binary,_/binary>> = Haystack,
		    {A,A+B,SubStack};
		{A,B} when B < 0 ->
		    S = A + B,
		    L = -B,
		    <<_:S/binary,SubStack:L/binary,_/binary>> = Haystack,
		    {S,S+L,SubStack}
	    end,
	MList = if
		    Global ->
			msloop(NewStack,Needles,Start,End);
		    true ->
			case mloop(NewStack,Needles,Start,End) of
			    nomatch ->
				[];
			    X ->
				[X]
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
    [part(H,{N,byte_size(H)-N})];
do_replace(H,[{A,B}|T],Replacement,N) ->
    [part(H,{N,A-N}),
     if
	 is_list(Replacement) ->
	     do_insert(Replacement, part(H,{A,B}));
	 true ->
	     Replacement
     end
     | do_replace(H,T,Replacement,A+B)].

do_insert([X],_) ->
    [X];
do_insert([H|T],R) ->
    [H,R|do_insert(T,R)].

splitat(H,N,[]) ->
    [part(H,{N,byte_size(H)-N})];
splitat(H,N,[I|T]) ->
    [part(H,{N,I-N})|splitat(H,I,T)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% first, last and at
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
first(Subject) ->
    try
	<<A:8,_/binary>> = Subject,
	A
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

last(Subject) ->
    try
	N = byte_size(Subject) - 1,
	<<_:N/binary,A:8>> = Subject,
	A
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

at(Subject,X) ->
    try
	<<_:X/binary,A:8,_/binary>> = Subject,
	A
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% bin_to_list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bin_to_list(Subject) ->
    try
	binary_to_list(Subject)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

bin_to_list(Subject,T) ->
    try
	{A0,B0} = T,
	{A,B} = if
		    B0 < 0 ->
			{A0+B0,-B0};
		    true ->
			{A0,B0}
		end,
	binary_to_list(Subject,A+1,A+B)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

bin_to_list(Subject,A,B) ->
    try
	bin_to_list(Subject,{A,B})
    catch
	_:_ ->
	    erlang:error(badarg)
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% list_to_bin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_to_bin(List) ->
    try
	erlang:list_to_binary(List)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% longest_common_prefix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
longest_common_prefix(LB) ->
    try
	true = is_list(LB) and (length(LB) > 0), % Make badarg instead of function clause
	do_longest_common_prefix(LB,0)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

do_longest_common_prefix(LB,X) ->
    case do_lcp(LB,X,no) of
	true ->
	    do_longest_common_prefix(LB,X+1);
	false ->
	    X
    end.
do_lcp([],_,_) ->
    true;
do_lcp([Bin|_],X,_) when byte_size(Bin) =< X ->
    false;
do_lcp([Bin|T],X,no) ->
    Ch = at(Bin,X),
    do_lcp(T,X,Ch);
do_lcp([Bin|T],X,Ch) ->
    Ch2 = at(Bin,X),
    if
	Ch =:= Ch2 ->
	    do_lcp(T,X,Ch);
	true ->
	    false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% longest_common_suffix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
longest_common_suffix(LB) ->
    try
	true = is_list(LB) and (length(LB) > 0), % Make badarg instead of function clause
	do_longest_common_suffix(LB,0)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

do_longest_common_suffix(LB,X) ->
    case do_lcs(LB,X,no) of
	true ->
	    do_longest_common_suffix(LB,X+1);
	false ->
	    X
    end.
do_lcs([],_,_) ->
    true;
do_lcs([Bin|_],X,_) when byte_size(Bin) =< X ->
    false;
do_lcs([Bin|T],X,no) ->
    Ch = at(Bin,byte_size(Bin) - 1 - X),
    do_lcs(T,X,Ch);
do_lcs([Bin|T],X,Ch) ->
    Ch2 = at(Bin,byte_size(Bin) - 1 - X),
    if
	Ch =:= Ch2 ->
	    do_lcs(T,X,Ch);
	true ->
	    false
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
part(Subject,Part) ->
    try
	do_part(Subject,Part)
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

part(Subject,Pos,Len) ->
    part(Subject,{Pos,Len}).

do_part(Bin,{A,B}) when B >= 0 ->
    <<_:A/binary,Sub:B/binary,_/binary>> = Bin,
    Sub;
do_part(Bin,{A,B}) when B < 0 ->
    S = A + B,
    L = -B,
    <<_:S/binary,Sub:L/binary,_/binary>> = Bin,
    Sub.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% copy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
copy(Subject) ->
    copy(Subject,1).
copy(Subject,N) ->
    try
	true = is_integer(N) and (N >= 0) and is_binary(Subject), % Badarg, not function clause
	erlang:list_to_binary(lists:duplicate(N,Subject))
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% encode_unsigned
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode_unsigned(Unsigned) ->
    encode_unsigned(Unsigned,big).
encode_unsigned(Unsigned,Endian) ->
    try
	true = is_integer(Unsigned) and (Unsigned >= 0),
	if
	    Unsigned =:= 0 ->
		<<0>>;
	    true ->
		case Endian of
		    big ->
			list_to_binary(do_encode(Unsigned,[]));
		    little ->
			list_to_binary(do_encode_r(Unsigned))
		end
	end
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

do_encode(0,L) ->
    L;
do_encode(N,L) ->
    Byte = N band 255,
    NewN = N bsr 8,
    do_encode(NewN,[Byte|L]).

do_encode_r(0) ->
    [];
do_encode_r(N) ->
    Byte = N band 255,
    NewN = N bsr 8,
    [Byte|do_encode_r(NewN)].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% decode_unsigned
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
decode_unsigned(Subject) ->
    decode_unsigned(Subject,big).

decode_unsigned(Subject,Endian) ->
    try
	true = is_binary(Subject),
	case Endian of
	    big ->
		do_decode(Subject,0);
	    little ->
		do_decode_r(Subject,0)
	end
    catch
	_:_ ->
	    erlang:error(badarg)
    end.

do_decode(<<>>,N) ->
    N;
do_decode(<<X:8,Bin/binary>>,N) ->
    do_decode(Bin,(N bsl 8) bor X).

do_decode_r(<<>>,N) ->
    N;
do_decode_r(Bin,N) ->
    Sz = byte_size(Bin) - 1,
    <<NewBin:Sz/binary,X>> = Bin,
    do_decode_r(NewBin, (N bsl 8) bor X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% referenced_byte_size cannot
%% be implemented in pure
%% erlang
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
referenced_byte_size(Bin) when is_binary(Bin) ->
    erlang:error(not_implemented);
referenced_byte_size(_) ->
    erlang:error(badarg).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Simple helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Option "parsing"
get_opts_match([],Part) ->
    Part;
get_opts_match([{scope,{A,B}} | T],_Part) ->
    get_opts_match(T,{A,B});
get_opts_match(_,_) ->
    throw(badopt).

get_opts_split([],{Part,Global,Trim,TrimAll}) ->
    {Part,Global,Trim,TrimAll};
get_opts_split([{scope,{A,B}} | T],{_Part,Global,Trim,TrimAll}) ->
    get_opts_split(T,{{A,B},Global,Trim,TrimAll});
get_opts_split([global | T],{Part,_Global,Trim,TrimAll}) ->
    get_opts_split(T,{Part,true,Trim,TrimAll});
get_opts_split([trim | T],{Part,Global,_Trim,TrimAll}) ->
    get_opts_split(T,{Part,Global,true,TrimAll});
get_opts_split([trim_all | T],{Part,Global,Trim,_TrimAll}) ->
    get_opts_split(T,{Part,Global,Trim,true});
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
