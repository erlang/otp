%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2011. All Rights Reserved.
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
%%
-module(http_util).

-export([
	 to_upper/1, to_lower/1, 
	 convert_netscapecookie_date/1,
	 hexlist_to_integer/1, integer_to_hexlist/1, 
	 convert_month/1, 
	 is_hostname/1,
	 timestamp/0, timeout/2,
	 html_encode/1
	]).


%%%=========================================================================
%%%  Internal application API
%%%=========================================================================
to_upper(Str) ->
    string:to_upper(Str).

to_lower(Str) ->
    string:to_lower(Str).

%% Example: Mon, 09-Dec-2002 13:46:00 GMT
convert_netscapecookie_date([_D,_A,_Y, $,, $ ,
			     D1,D2, $-,
			     M,O,N, $-,
			     Y1,Y2,Y3,Y4, $ ,
			     H1,H2, $:,
			     M1,M2, $:,
			     S1,S2|_Rest]) -> 
    Year  = list_to_integer([Y1,Y2,Y3,Y4]),
    Day   = list_to_integer([D1,D2]),
    Month = convert_month([M,O,N]),
    Hour  = list_to_integer([H1,H2]),
    Min   = list_to_integer([M1,M2]),
    Sec   = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

convert_netscapecookie_date([_D,_A,_Y, $,, $ ,
			     D1,D2, $-,
			     M,O,N, $-,
			     Y3,Y4, $ ,
			     H1,H2, $:,
			     M1,M2, $:,
			     S1,S2|_Rest]) -> 
    {CurrentYear, _, _} = date(),
    [Y1,Y2|_] = integer_to_list(CurrentYear),
    Year      = list_to_integer([Y1,Y2,Y3,Y4]),
    Day       = list_to_integer([D1,D2]),
    Month     = convert_month([M,O,N]),
    Hour      = list_to_integer([H1,H2]),
    Min       = list_to_integer([M1,M2]),
    Sec       = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

convert_netscapecookie_date([_D,_A,_Y, $ ,
			     D1,D2, $-,
			     M,O,N, $-,
			     Y1,Y2,Y3,Y4, $ ,
			     H1,H2, $:,
			     M1,M2, $:,
			     S1,S2|_Rest]) -> 
    Year  = list_to_integer([Y1,Y2,Y3,Y4]),
    Day   = list_to_integer([D1,D2]),
    Month = convert_month([M,O,N]),
    Hour  = list_to_integer([H1,H2]),
    Min   = list_to_integer([M1,M2]),
    Sec   = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

convert_netscapecookie_date([_D,_A,_Y, $ ,
			     D1,D2, $-,
			     M,O,N, $-,
			     Y3,Y4, $ ,
			     H1,H2, $:,
			     M1,M2, $:,
			     S1,S2|_Rest]) -> 
    {CurrentYear, _, _} = date(),
    [Y1,Y2|_] = integer_to_list(CurrentYear),
    Year      = list_to_integer([Y1,Y2,Y3,Y4]),
    Day       = list_to_integer([D1,D2]),
    Month     = convert_month([M,O,N]),
    Hour      = list_to_integer([H1,H2]),
    Min       = list_to_integer([M1,M2]),
    Sec       = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

%% Example: Tue Jan 01 08:00:01 2036 GMT
convert_netscapecookie_date([_D,_A,_Y, $ ,
			     M,O,N, $ ,
			     D1,D2, $ ,
			     H1,H2, $:,
			     M1,M2, $:,
			     S1,S2, $ ,
			     Y1,Y2,Y3,Y4, $ |_Rest]) -> 
    Year  = list_to_integer([Y1,Y2,Y3,Y4]),
    Day   = list_to_integer([D1,D2]),
    Month = convert_month([M,O,N]),
    Hour  = list_to_integer([H1,H2]),
    Min   = list_to_integer([M1,M2]),
    Sec   = list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

%% Sloppy...
convert_netscapecookie_date([_D,_A,_Y, $,, _SP,
                             D1,D2,_DA,
                             M,O,N,_DA,
                             Y1,Y2,Y3,Y4,_SP,
                             H1,H2,_Col,
                             M1,M2,_Col,
                             S1,S2|_Rest]) ->
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1,D2]),
    Month=convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}};

convert_netscapecookie_date([_D,_A,_Y, _SP,
                             D1,D2,_DA,
                             M,O,N,_DA,
                             Y1,Y2,Y3,Y4,_SP,
                             H1,H2,_Col,
                             M1,M2,_Col,
                             S1,S2|_Rest]) ->
    Year=list_to_integer([Y1,Y2,Y3,Y4]),
    Day=list_to_integer([D1,D2]),
    Month=convert_month([M,O,N]),
    Hour=list_to_integer([H1,H2]),
    Min=list_to_integer([M1,M2]),
    Sec=list_to_integer([S1,S2]),
    {{Year,Month,Day},{Hour,Min,Sec}}.

hexlist_to_integer([]) ->
    empty;
%%When the string only contains one value its eaasy done.
%% 0-9
hexlist_to_integer([Size]) when (Size >= 48) andalso (Size =< 57) ->
   Size - 48;
%% A-F
hexlist_to_integer([Size]) when (Size >= 65) andalso (Size =< 70) ->
    Size - 55;
%% a-f
hexlist_to_integer([Size]) when (Size >= 97) andalso (Size =< 102) ->
    Size - 87;
hexlist_to_integer([_Size]) ->
    not_a_num;

hexlist_to_integer(Size) ->
    Len = string:span(Size, "1234567890abcdefABCDEF"),
    hexlist_to_integer2(Size, 16 bsl (4 *(Len-2)),0).

integer_to_hexlist(Num)->
    integer_to_hexlist(Num, get_size(Num), []).

convert_month("Jan") -> 1;
convert_month("Feb") -> 2;
convert_month("Mar") -> 3; 
convert_month("Apr") -> 4;
convert_month("May") -> 5;
convert_month("Jun") -> 6;
convert_month("Jul") -> 7;
convert_month("Aug") -> 8;
convert_month("Sep") -> 9;
convert_month("Oct") -> 10;
convert_month("Nov") -> 11;
convert_month("Dec") -> 12.

is_hostname(Dest) ->
    inet_parse:domain(Dest).


timestamp() ->
    {A,B,C} = os:timestamp(),
    A*1000000000+B*1000+(C div 1000).

timeout(Timeout, Started) ->
    %% NewTimeout = Timeout - (timestamp() - Started),
    case Timeout - (timestamp() - Started) of
	NewTimeout when Timeout > 0 ->
	    NewTimeout;
	_ ->
	    0
    end.
    

html_encode(Chars) ->
    Reserved = sets:from_list([$&, $<, $>, $\", $', $/]),
    lists:append([char_to_html_entity(Char, Reserved) || Char <- Chars]).


%%%========================================================================
%%% Internal functions
%%%========================================================================
hexlist_to_integer2([],_Pos,Sum)->
    Sum;
hexlist_to_integer2([HexVal | HexString], Pos, Sum) 
  when HexVal >= 48, HexVal =< 57 ->
    hexlist_to_integer2(HexString, Pos bsr 4, Sum + ((HexVal-48) * Pos));

hexlist_to_integer2([HexVal | HexString], Pos, Sum) 
  when HexVal >= 65, HexVal =<70 ->
    hexlist_to_integer2(HexString, Pos bsr 4, Sum + ((HexVal-55) * Pos));

hexlist_to_integer2([HexVal | HexString], Pos, Sum)
  when HexVal>=97, HexVal=<102 ->
    hexlist_to_integer2(HexString, Pos bsr 4, Sum + ((HexVal-87) * Pos));

hexlist_to_integer2(_AfterHexString, _Pos, Sum)->
    Sum.

integer_to_hexlist(Num, Pot, Res) when Pot < 0 ->
    convert_to_ascii([Num | Res]);

integer_to_hexlist(Num,Pot,Res) ->
    Position = (16 bsl (Pot*4)),
    PosVal = Num div Position,
    integer_to_hexlist(Num - (PosVal*Position), Pot-1, [PosVal | Res]).

get_size(Num)->
    get_size(Num, 0).

get_size(Num, Pot) when Num < (16 bsl(Pot *4))  ->
    Pot-1;

get_size(Num, Pot) ->
    get_size(Num, Pot+1).

convert_to_ascii(RevesedNum) ->
    convert_to_ascii(RevesedNum, []).

convert_to_ascii([], Num)->
    Num;
convert_to_ascii([Num | Reversed], Number) 
  when (Num > -1) andalso (Num < 10) ->
    convert_to_ascii(Reversed, [Num + 48 | Number]);
convert_to_ascii([Num | Reversed], Number) 
  when (Num > 9) andalso (Num < 16) ->
    convert_to_ascii(Reversed, [Num + 55 | Number]).

char_to_html_entity(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
	true ->
	    "&#" ++ integer_to_list(Char) ++ ";";
	false ->
	    [Char]
    end.
