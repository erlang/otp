%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_acl.erl
%% 
%% Description:
%%    Handling ACL's (Access Control Lists).
%%
%% Creation date: 040723
%%
%%-----------------------------------------------------------------
-module(orber_acl).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([init_acl/1, init_acl/2, clear_acl/0, 
	 match/2, match/3, verify/3, range/1, range/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-define(ACL_DB, orber_acl_db).

-define(DEBUG_LEVEL, 5).
-define(CONTINUE, -1).
-define(STOP, -2).

-define(FORMAT(_F, _A), {error, lists:flatten(io_lib:format(_F, _A))}).
-define(EFORMAT(_F, _A), exit(lists:flatten(io_lib:format(_F, _A)))).

%%-----------------------------------------------------------------
%% Record Definitions
%%-----------------------------------------------------------------
-record(acl, {key, bits = ?CONTINUE, mask, interfaces = [], 
	      ports = 0, flags = 0}).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
    
%%-----------------------------------------------------------------
%% function : verify/1
%% Arguments: IP - string()
%%            Filter - string() see init_acl
%%            Family - inet | inet6
%% Returns  : boolean()
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
verify(IP, Filter, Family) ->
    DB = ets:new(orber_temporary_acl_table_created_by_user, 
		 [set, public, {keypos, 2}]),
    Result = 
	case catch verify_helper(IP, Filter, Family, DB) of
	    true ->
		true;
	    {ok, Low, High} ->
		{false, Low, High};
	    What ->
		{error, ?FORMAT("Uknown Error: ~p\n", [What])}
	end,
    ets:delete(DB),
    Result.

verify_helper(IP, Filter, Family, DB) ->
    init_acl([{tcp_in, Filter}], Family, DB),
    {ok, IPTuple} = inet:getaddr(IP, Family),
    case match_helper(tuple_to_list(IPTuple), tcp_in, DB, false, tcp_in) of
	true ->
	    true;
	false ->
	    range(Filter, Family)
    end.

%%-----------------------------------------------------------------
%% function : range/1/2
%% Arguments: Filter - string(). See init_acl
%%            Family - inet | inet6
%% Returns  : {ok, From, To}
%%            From - To - string()
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
range(Filter) ->
    range(Filter, inet).

range(Filter, inet) ->
    range_safe(Filter, inet, ".", 16#FF, "255", 8, "~p.", "~p", 3);
range(Filter, Family) ->
    range_safe(Filter, Family, ":", 16#FFFF, "FFFF", 16, "~.16B:", "~.16B", 7).

range_safe(Filter, Family, Separator, Max, MaxStr, N, F1, F2, X) ->
    case catch range_helper(Filter, Family, Separator, Max, MaxStr, N, F1, F2, X) of
	{ok, Low, High} ->
	    {ok, Low, High};
	{'EXIT',{format,Why}} ->
	    {error, ?FORMAT("Unable to format string: ~p\n", [Why])};
	{'EXIT', E} ->
	    {error, ?FORMAT("Exit: ~p\n", [E])};
	What ->
	    {error, ?FORMAT("Unknown Error: ~p\n", [What])}
    end.

range_helper(Filter, Family, Separator, Max, MaxStr, N, F1, F2, X) ->
    {MaskStr, Bits, _Ports} = tokenize(Filter, Family),
    {ok, MaskTuple} = inet:getaddr(MaskStr, Family), 
    NoOfFull = Bits div N,
    Mask = get_mask(N, (Bits rem N)),
    case split(NoOfFull, tuple_to_list(MaskTuple)) of
	{Full, [Partial|_DontCare]} ->
	    Beginning = pp(Full, [], F1),
	    MiddleLow = io_lib:format(F2, [(Mask band Partial) + ((Mask bxor Max) band 0)]),
	    MiddleHigh = io_lib:format(F2, [(Mask band Partial) + ((Mask bxor Max) band Max)]),
	    EndLow = lists:duplicate((X-NoOfFull), Separator ++ "0"),
	    EndHigh = lists:duplicate((X-NoOfFull), Separator ++ MaxStr),
	    Low = lists:flatten([Beginning, MiddleLow, EndLow]),
	    High = lists:flatten([Beginning, MiddleHigh, EndHigh]),
	    {ok, Low, High};
	{Full, []} ->
	    Address = lists:flatten(pp(Full, [], F1)),
	    {ok, Address, Address}
    end.
  
pp([], Acc, _) ->
    Acc;
pp([H|T], Acc, Format) ->
    pp(T, Acc ++ io_lib:format(Format, [H]), Format).

split(N, List) when is_integer(N) andalso N >= 0 andalso is_list(List) ->
    case split(N, List, []) of
        Fault when is_atom(Fault) ->
            erlang:error(Fault, [N,List]);
        Result ->
            Result
    end;
split(N, List) ->
    erlang:error(badarg, [N,List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H|T], R) ->
    split(N-1, T, [H|R]);
split(_, [], _) ->
    badarg.


%%-----------------------------------------------------------------
%% function : clear_acl/0
%% Arguments: -
%% Returns  : 
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
clear_acl() ->
    clear_acl(?ACL_DB).
clear_acl(DB) ->
    (catch ets:delete(DB)),
    ok.

%%-----------------------------------------------------------------
%% function : init_acl/1/2
%% Arguments: Filters - [{Direction, Filter}] | [{Direction, Filter, [Interfaces]}]
%%            Direction - tcp_in | ssl_in | tcp_out | ssl_out
%%            Filter - string(). Examples:
%%     * "123.456.789.10" - match against all bits.
%%     * "123.456.789.10/17" - match against the 17 most significant bits.
%%     * "123.456.789.10/17#4001" - as above but only allow port 4001
%%     * "123.456.789.10/17#4001/5001" - as above but only allow port 4001-5001
%%            Family - inet | inet6
%% Returns  : ok | {'EXCEPTION', E}
%% Exception: 'BAD_PARAM'
%% Effect   : 
%%-----------------------------------------------------------------
init_acl(Filters) ->
    DB = ets:new(?ACL_DB, [set, public, named_table, {keypos, 2}]),
    case ?ORB_FLAG_TEST(orber:get_flags(), ?ORB_ENV_USE_IPV6) of
	false ->
	    init_acl(Filters, inet, DB);
	true ->
	    init_acl(Filters, inet6, DB)
    end.

init_acl(Filters, Family) ->
    DB = ets:new(?ACL_DB, [set, public, named_table, {keypos, 2}]),
    init_acl(Filters, Family, DB).

init_acl([], _, DB) ->
    {ok, DB};
init_acl([Data|T], Family, DB) ->
    {Direction, Filter, Interfaces} =
	case Data of
	    {D, F, I} ->
		{D, F, I};
	    {D, F} ->
		{D, F, []}
	end,
    {MaskStr, Bits, Ports} = tokenize(Filter, Family),
    case inet:getaddr(MaskStr, Family) of
	{ok, Addr} when size(Addr) == 4 ->
	    create_mask(tuple_to_list(Addr), Bits div 8, 
			get_mask8((Bits rem 8)), DB, Direction, Interfaces, Ports), 
	    init_acl(T, Family, DB);
	{ok, Addr} ->
	    create_mask(tuple_to_list(Addr), Bits div 16, 
			get_mask16((Bits rem 16)), DB, Direction, Interfaces, Ports), 
	    init_acl(T, Family, DB)
    end.

create_mask(List, Div, Mask, DB, Direction, Interfaces, Ports) ->
    case split(Div, List) of
	{[], [Partial|_DontCare]} ->
	    %% Less than 8/16 bits (depends on family).
	    add_parts([], Direction, (Partial band Mask), Mask, DB, 
		      Interfaces, Ports);
	{Full, [Partial|_DontCare]} ->
	    add_parts(Full, Direction, (Partial band Mask), Mask, DB, 
		      Interfaces, Ports);
	{Full, []} ->
	    %% 32 bits.
	    add_parts(Full, Direction, ?STOP, Mask, DB, Interfaces, Ports)
    end.

add_parts([], Parent, Bits, Mask, DB, Interfaces, Ports) ->
    ets:insert(DB, #acl{key = Parent, bits = Bits, 
			mask = Mask, interfaces = Interfaces, ports = Ports});
add_parts([H|T], Parent, Bits, Mask, DB, Interfaces, Ports) ->
    Key = {Parent, H},
    ets:insert(DB, #acl{key = Key}),
    add_parts(T, Key, Bits, Mask, DB, Interfaces, Ports).


%%-----------------------------------------------------------------
%% function : match/1/2
%% Arguments: IP - tuple() | [integer()]
%%            Direction - tcp_in | ssl_in | tcp_out | ssl_out
%%            All - boolean()
%% Returns  : 
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
match(IPTuple, Direction) when is_tuple(IPTuple) ->
    match_helper(tuple_to_list(IPTuple), Direction, ?ACL_DB, false, Direction);
match(IPList, Direction) ->
    match_helper(IPList, Direction, ?ACL_DB, false, Direction).

match(IPTuple, Direction, All) when is_tuple(IPTuple) ->
    match_helper(tuple_to_list(IPTuple), Direction, ?ACL_DB, All, Direction);
match(IPList, Direction, All) ->
    match_helper(IPList, Direction, ?ACL_DB, All, Direction).

match_helper([], _, _, false, _) -> false;
match_helper([], _, _, true, _) -> {false, [], 0};
match_helper([H|T], Parent, DB, All, Direction) ->
    case ets:lookup(DB, {Parent, H}) of
	[#acl{bits = ?CONTINUE}] ->
	    match_helper(T, {Parent, H}, DB, All, Direction);
	[#acl{bits = ?STOP}] when All == false ->
	    true;
	[#acl{bits = ?STOP, interfaces = I, ports = Ports}] ->
	    {true, I, Ports};
	[#acl{bits = Bits, mask = Mask}] when All == false ->
	    Bits == (hd(T) band Mask);
	[#acl{bits = Bits, mask = Mask, interfaces = I, ports = Ports}] ->
	    {Bits == (hd(T) band Mask), I, Ports};
	_ ->
	    %% Less than 8/16 significant bits (depends on family). 
	    %% Should we even allow this?
	    case ets:lookup(DB, Direction) of
		[#acl{bits = Bits, mask = Mask}] when is_integer(Bits) andalso
						      All == false  ->
		    Bits == (H band Mask);
		[#acl{bits = Bits, mask = Mask, 
		      interfaces = I, ports = Ports}] when is_integer(Bits) ->
		    {Bits == (H band Mask), I, Ports};
		_ when All == false ->
		    false;
		_ ->
		    {false, [], 0}
	    end
    end.
	    

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% function : tokenize/1
%% Arguments: Filter - string(). Examples:
%%     * "123.456.789.10" - match against all.
%%     * "123.456.789.10/17" - match against the 17 most significant bits.
%%     * "123.456.789.10/17#4001" - as above but only allow port 4001
%%     * "123.456.789.10/17#4001/5001" - as above but only allow port 4001-5001
%%            Family - inet | inet6
%% Returns  : {MaskStr, Bits, Ports}
%%            MaskStr - string()
%%            Bits    - integer()
%%            Ports   - integer() | {integer(), integer()}
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
tokenize(Filter, Family) ->
    case string:tokens(Filter, "/#") of
	[MaskStr] when Family == inet ->
	    {MaskStr, 32, 0};
	[MaskStr] when Family == inet6 ->
	    {MaskStr, 128, 0};
	[MaskStr, BitString] ->
	    {MaskStr, list_to_integer(BitString), 0};
	[MaskStr, BitString, Port] ->
	    {MaskStr, list_to_integer(BitString), list_to_integer(Port)};
	[MaskStr, BitString, MinPort, MaxPort] ->
	    {MaskStr, list_to_integer(BitString), 
	     {list_to_integer(MinPort), list_to_integer(MaxPort)}};
	What ->
	    ?EFORMAT("Invalid Filter: ~p\nReason: ~p\n", [Filter, What])
    end.


%%-----------------------------------------------------------------
%% function : get_mask/2
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
get_mask(8, Bits) ->
    get_mask8(Bits);
get_mask(16, Bits) ->
    get_mask16(Bits).

%%-----------------------------------------------------------------
%% function : get_mask8/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
get_mask8(0) -> 2#00000000;
get_mask8(1) -> 2#10000000;
get_mask8(2) -> 2#11000000;
get_mask8(3) -> 2#11100000;
get_mask8(4) -> 2#11110000;
get_mask8(5) -> 2#11111000;
get_mask8(6) -> 2#11111100;
get_mask8(7) -> 2#11111110.

%%-----------------------------------------------------------------
%% function : get_mask16/1
%% Arguments: 
%% Returns  : 
%% Exception: 
%% Effect   : 
%%-----------------------------------------------------------------
get_mask16(0)  -> 2#0000000000000000;
get_mask16(1)  -> 2#1000000000000000;
get_mask16(2)  -> 2#1100000000000000;
get_mask16(3)  -> 2#1110000000000000;
get_mask16(4)  -> 2#1111000000000000;
get_mask16(5)  -> 2#1111100000000000;
get_mask16(6)  -> 2#1111110000000000;
get_mask16(7)  -> 2#1111111000000000;
get_mask16(8)  -> 2#1111111100000000;
get_mask16(9)  -> 2#1111111110000000;
get_mask16(10) -> 2#1111111111000000;
get_mask16(11) -> 2#1111111111100000;
get_mask16(12) -> 2#1111111111110000;
get_mask16(13) -> 2#1111111111111000;
get_mask16(14) -> 2#1111111111111100;
get_mask16(15) -> 2#1111111111111110.
    

%%-----------------------------------------------------------------
%%------------- END OF MODULE -------------------------------------
%%-----------------------------------------------------------------
