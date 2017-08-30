%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

hexlist_to_integer(List) ->
    list_to_integer(List, 16).

integer_to_hexlist(Int) ->
    integer_to_list(Int, 16).

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

char_to_html_entity(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
	true ->
	    "&#" ++ integer_to_list(Char) ++ ";";
	false ->
	    [Char]
    end.
