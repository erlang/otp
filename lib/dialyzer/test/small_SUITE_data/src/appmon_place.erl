%%---------------------------------------------------------------------
%% This is added as a test because it was giving a false positive
%% (function move/4 will never be called) due to the strange use of
%% self-recursive fun construction in placex/3.
%%
%% The analysis was getting confused that the foldl call will never
%% terminate (due to a wrong hard-coded type for foldl) and inferred
%% that the remaining calls in the body of placex/3 will not be
%% reached.  Fixed 11 March 2005.
%%---------------------------------------------------------------------

-module(appmon_place).
-export([place/2]).

place(DG, Root) ->
    case appmon_dg:get(data, DG, Root) of
	false -> [0];
	_Other ->
	    placey(DG, Root, 1),
	    placex(DG, Root, [])
    end.

placey(DG, V, Y) ->
    appmon_dg:set(y, DG, V, Y),
    Y1 = Y+1,
    lists:foreach(fun(C) -> placey(DG, C, Y1) end, appmon_dg:get(out, DG, V)).

placex(DG, V, LastX) ->
    Ch = appmon_dg:get(out, DG, V),
    ChLX = lists:foldl(fun(C, Accu) -> placex(DG, C, Accu) end,
		       tll(LastX),
		       Ch),
    Width	= appmon_dg:get(w, DG, V),
    MyX		= calc_mid(DG, Width, Ch),
    DeltaX	= calc_delta(MyX, hdd(LastX)+20),
    appmon_dg:set(x, DG, V, MyX),
    move(DG, V, [MyX+Width | ChLX], DeltaX).

move(_DG, _L, LastX, 0) -> LastX;
move(DG, V, LastX, DeltaX) -> move2(DG, V, LastX, DeltaX).

move2(DG, V, LastX, DeltaX) ->
    NewX = appmon_dg:get(x, DG, V)+DeltaX,
    appmon_dg:set(x, DG, V, NewX),
    ChLX = lists:foldl(fun(C, LX) -> move2(DG, C, LX, DeltaX) end,
		       tll(LastX),
		       appmon_dg:get(out, DG, V)),
    [max(NewX+appmon_dg:get(w, DG, V), hdd(LastX)) | ChLX].

max(A, B) when A>B -> A;
max(_, B) -> B.

calc_mid(_DG, _Width, []) -> 0;
calc_mid(DG, Width, ChList) ->
    LeftMostX = appmon_dg:get(x, DG, hd(ChList)),
    Z2 = lists:last(ChList),
    RightMostX = appmon_dg:get(x, DG, Z2)+appmon_dg:get(w, DG, Z2),
    trunc((LeftMostX+RightMostX)/2)-trunc(Width/2).

calc_delta(Mid, Right) ->
    if  Right>Mid	-> Right-Mid;
	true		-> 0
    end.

%% Special head and tail
%% Handles empty list in a non-standard way
tll([]) -> [];
tll([_|T]) -> T.
hdd([]) -> 0;
hdd([H|_]) -> H.
