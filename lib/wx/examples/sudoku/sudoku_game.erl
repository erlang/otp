%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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

-module(sudoku_game).

-export([init/1,
         indx/1, rcm/1, level/1]).
-include("sudoku.hrl").

init(GFX) ->
    Empty = empty_table(#s{}),
    Add = fun({Butt,Val},SN) ->
		  validate(rcm(Butt),Val,false,SN)
 	  end,

    Game = test(),
    GFX ! {init, Game},
    Self = self(),
    Gen = spawn_opt(fun() -> create_games(levels(),Self) end, 
		    [link, {priority,low}]),
    loop(lists:foldl(Add,Empty#s{gfx=GFX, gen=Gen},Game)).

%%%%%%%%%%%%%%%%%% Game Engine %%%%%%%%%%%%%%%%%%

empty_table(S) ->
    Nine = lists:seq(1,9),
    D    = gb_sets:from_ordset(Nine),
    Mat  = list_to_tuple([D || _ <- Nine]),    
    Poss = list_to_tuple([D || _ <- lists:seq(1,9*9)]),
    Vals = list_to_tuple([0 || _ <- lists:seq(1,9*9)]),
    Must = list_to_tuple([gb_sets:empty() || _ <- lists:seq(1,9*3)]),
    S#s{p=Poss,m=Mat,mr=Must,mc=Must,v=Vals}.

loop(S0 = #s{gfx = Gfx, v=Vs}) ->
    receive 
	quit -> 
	    halt;
	{'EXIT', Gfx, Reason} ->
	    io:format("The GUI crashed: ~p~n", [Reason]);
	{validate, Butt, Val} ->
	    Ix = indx(Butt),
	    case element(Ix,Vs) of
		Val -> loop(S0);
		0 -> 
		    S = validate(rcm(Butt),Val,true,S0),
		    loop(S);
		_ ->
		    S1 = S0#s{v=setelement(Ix,Vs,0)},
		    S2 = rebuild_all(rcm(Butt),S1),
		    S  = validate(rcm(Butt),Val,true,S2),
		    loop(S)
	    end;
	{loaded, Game} ->
	    S1  = empty_table(S0),
	    Add = fun({Butt,Val},SN) ->
			  validate(rcm(Butt),Val,true,SN)
		  end,
	    loop(lists:foldl(Add,S1,Game));
	{op,?EMPTY} ->
	    loop(empty_table(S0));
	{op,?NEW, Level} ->
	    case find_game(Level,S0) of
		{Game, S1} ->
		    S0#s.gen ! {gen_game, Level},
		    Gfx ! {busy,start},
		    Gfx ! {init, Game};
		false -> 
		    S1 = S0,
		    Gfx ! {busy,start},
		    Temp = new_game(S0),
		    Game = pick_shown(Temp,Level,Gfx),
		    S0#s.gen ! {gen_game, Level},
		    Game
	    end,
	    S2  = empty_table(S1),
	    Add = fun({Butt,Val},SN) ->
			  validate(rcm(Butt),Val,false, SN)
		  end,
	    Gfx ! {init, Game},
	    Gfx ! {busy,stop},
	    loop(lists:foldl(Add,S2,Game));
	{solve, All} ->
	    Res =  solve(S0, All),
	    [Gfx ! {set_val, Ind, Val} || {Ind,Val} <- element(2,Res)],
	    loop(S0);
	{get_game, Pid} ->	    
	    Pid ! {game, get_known(S0)},
	    loop(S0);
	{game, Game} ->
	    loop(S0#s{games=[Game|S0#s.games]});
	CMD ->
	    io:format("Game loop got ~p~n", [CMD]),
	    ?MODULE:loop(S0)
    end.

validate({R,C,_M},0,Send,St = #s{gfx=Gfx}) ->
    if Send -> Gfx ! {correct, {R,C}}; true -> ok end,
    St;
validate(RCM={R,C,_M},Val,Send,St = #s{gfx=Gfx,v=Vs}) ->
    S = poss(RCM,St),
    case gb_sets:is_member(Val,S) of
	true ->
	    if Send -> Gfx ! {correct, {R,C}}; true -> ok end,
	    add(RCM,Val,St);
	false ->
	    if Send -> Gfx ! {wrong, {R,C}}; true -> ok end,
	    St#s{v=setelement(indx(R,C),Vs,Val)}
    end.

rebuild_all(_, S0) ->
    Solved = get_known(S0),
    S1 = empty_table(S0),
    lists:foldl(fun({Indx,Val},Acc) ->
			add(rcm(Indx),Val,Acc)
		end, S1, Solved).

test() ->  %% Known to solvable
    [{{1,2},6}, {{1,4},1}, {{1,6},4}, {{1,8},5}, 
     {{2,3},8}, {{2,4},3}, {{2,6},5}, {{2,7},6}, 
     {{3,1},2}, {{3,9},1}, 
     {{4,1},8}, {{4,4},4}, {{4,6},7}, {{4,9},6},
     {{5,3},6}, {{5,7},3},
     {{6,1},7}, {{6,4},9}, {{6,6},1}, {{6,9},4}, 
     {{7,1},5}, {{7,9},2}, 
     {{8,3},7}, {{8,4},2}, {{8,6},6}, {{8,7},9}, 
     {{9,2},4}, {{9,4},5}, {{9,6},8}, {{9,8},7}].

new_game(S) ->
    rand:seed(exsplus),
    case new_game(1,1,gb_sets:empty(),empty_table(S#s{}),[], 0) of
	stop -> new_game(S); 
	Game -> Game
    end.
    

new_game(_,_,_,_St,_Acc,Cnt) when Cnt > 200 -> 
    %% Backtracked 200 times, Bad path lets start over
    stop;
new_game(R,C,BT,St,Acc,Cnt) when R < 10, C < 10 ->
    M = mat(R,C),
    U = poss({R,C,M},St),
    S = gb_sets:difference(U,BT),
    case gb_sets:size(S) of
	0 ->
	    [{{BR,BC},BVal,BBT,BST}|BAcc] = Acc,
	    new_game(BR,BC,gb_sets:add(BVal,BBT),BST,BAcc,Cnt+1);
	Size -> 
	    Ind = rand:uniform(Size),
	    V = lists:nth(Ind,gb_sets:to_list(S)),
	    new_game(R,C+1,gb_sets:empty(),
		     add({R,C,M},V,St),		    
		     [{{R,C},V,BT,St}|Acc], Cnt)
    end;
new_game(R,_C,Bt,S,Acc,Cnt) when R < 10 ->
    new_game(R+1,1,Bt,S,Acc,Cnt);
new_game(_,_,_,S,_Acc,_Cnt) -> 
%%    io:format("Backtracked ~p ~n",[_Cnt]),
    S.

pick_shown(S0, Level, Gfx) ->
    Given = gb_sets:from_ordset([I || I <- lists:seq(1,9*9)]),
    get_known(pick_shown(Given,Given,S0,level(Level),Gfx)).

get_known(#s{v=Vals}) -> 
    lists:foldl(fun(Index,Acc) ->
			case element(Index,Vals) of
			    0 -> Acc;
			    Val -> 
				{R,C,_} = rcm(Index),
				[{{R,C},Val}|Acc]
			end
		end, [], lists:seq(1,9*9)).

pick_shown(Given,Left,S0,Level,Gfx) ->    
    LeftSz  = gb_sets:size(Left),
    GivenSz = gb_sets:size(Given),
    if LeftSz == 0 -> 
	    io:format("No left ~p~n", [GivenSz]),
            S0;
       GivenSz < Level -> 
	    io:format("Below level ~p ~p~n", [GivenSz,Level]),
            S0;
       true ->
	    Ran = rand:uniform(LeftSz),
	    V   = lists:nth(Ran,gb_sets:to_list(Left)),
	    S1  = rebuild_all(rcm(V),S0#s{v=setelement(V,S0#s.v,0)}),
	    case solve(S1, true) of
		{true, _, _} -> 
		    catch Gfx ! {working, 100-LeftSz},
		    pick_shown(gb_sets:delete(V,Given), 
			       gb_sets:delete(V,Left), 
			       S1, Level,Gfx);
		{false,_,_} ->
		    pick_shown(Given,gb_sets:delete(V,Left), 
			       S0, Level,Gfx)
	    end
    end.

solve(St=#s{v=Vals},All) ->
    Unsolved = [I || I <- lists:seq(1,9*9), element(I,Vals) == 0],
    solve(Unsolved, All, St, [], [], lists:reverse(Unsolved)).

solve(Rem, false, _St, [Solved|_], Unsolved, _) -> {true, [Solved], Rem ++ Unsolved};
solve([], _, _St, Solved, [], _) ->                {true, Solved, []};
solve([], _, _St, Solved, Unsolved, Unsolved) ->   {false, Solved, Unsolved};
solve([], _, St, Solved, Unsolved, _Orig) -> 
    solve(Unsolved,true,St,Solved,[],lists:reverse(Unsolved));
solve([Index|Rest],All, St, S, US, Orig) ->
    RCM = rcm(Index),
    Poss = poss(RCM,St),
    case gb_sets:size(Poss) of 
	1 ->
	    %%	    io:format("S1 ~n",[]),
	    [Val] = gb_sets:to_list(Poss),
	    solve(Rest, All, add(RCM,Val,St), [{Index,Val}|S],US,Orig);
	_ ->
	    case solve_1(RCM, Poss, St) of
		false ->
		    solve(Rest, All, St, S, [Index|US],Orig);
		Val ->
		    solve(Rest, All, add(RCM,Val,St), [{Index,Val}|S],US,Orig)
	    end
    end.

solve_1(RCM={R,C,_M}, Avail, St) ->
    All = all(RCM),
    Poss = fun({RI,CI},Acc) when (RI == R) and (CI == C) -> Acc;
	      ({RI,CI},Acc) -> gb_sets:union(poss(rcm({RI,CI}),St),Acc)
	   end, 
    D = fun({RI,CI},Acc) when (RI == R) and (CI == C) -> 
		io:format("~p:~p: ignore~n",[RI,CI]), 
		Acc;	      
	   ({RI,CI},Acc) -> 
		Res = gb_sets:union(poss(rcm({RI,CI}),St),Acc),
		io:format("~p:~p: ~p => ~p ~n",[RI,CI,gb_sets:to_list(poss(rcm({RI,CI}),St)),gb_sets:to_list(Res)]), 
		Res
	end, 
    solve_2(All,{Poss,D},Avail).  

solve_2([],_, _) -> false;
solve_2([First|R],{Poss,D},Avail) -> 
    All = lists:foldl(Poss, gb_sets:empty(), First),
    Res = gb_sets:difference(Avail, All),
    case gb_sets:size(Res) of
	1 ->
	    %%	    lists:foldl(D, gb_sets:empty(), First),
	    %%	    io:format("Poss: ~w~nA: ~p O:~p ~n",[First,gb_sets:to_list(Avail),gb_sets:to_list(All)]),
	    [Val] = gb_sets:to_list(Res),
	    Val;
	_ ->
	    solve_2(R,{Poss,D},Avail)
    end.

all({RI,CI,MI}) -> all(RI,CI,MI).
all(RI,CI,MI) ->
    MR = ((MI-1) div 3)*3,
    MC = ((MI-1) rem 3)*3,
    Ri = [{RI,N} || N <- lists:seq(1,9)],
    Ci = [{N,CI} || N <- lists:seq(1,9)],
    Mi = [{1+MR,1+MC},{1+MR,2+MC},{1+MR,3+MC},
	  {2+MR,1+MC},{2+MR,2+MC},{2+MR,3+MC},
	  {3+MR,1+MC},{3+MR,2+MC},{3+MR,3+MC}],
    [Ri,Ci,Mi].

other_mats(N) -> 
    if N < 4 -> P1=3, P2= 6;
       N < 7 -> P1=-3,P2= 3;
       true  -> P1=-6,P2=-3
    end,
    case (N-1) rem 3 of
	0 -> [N+1,N+2,N+P1,N+P2];
	1 -> [N-1,N+1,N+P1,N+P2];
	2 -> [N-2,N-1,N+P1,N+P2]
    end.

check_must(S=#s{p=Poss,m=MS,mr=MR0,mc=MC0}) ->
    List = lists:seq(1,9),
    {MR,MC} = lists:foldl(fun(Val,{MRT,MCT}) ->
				  check_must2(List,Val,Poss,MS,MRT,MCT)
			  end, {MR0,MC0}, List),
    S#s{mr=MR,mc=MC}.

check_must2([M|Rest],Val,Poss,Ms,MR0,MC0) ->
    case gb_sets:is_member(Val, element(M,Ms)) of
	true ->
	    {Rows,Cols} = rc_in_mat(M),
	    MR1  = check_must3(Rows,Val,Poss,row,MR0),
	    MC1  = check_must3(Cols,Val,Poss,col,MC0),
	    check_must2(Rest,Val,Poss,Ms,MR1,MC1);
	false ->
	    check_must2(Rest,Val,Poss,Ms,MR0,MC0)
    end;
check_must2([],_,_,_,MR,MC) -> {MR,MC}.

check_must3({F1,F2,F3},Val,Check,Type,Must0) ->
    R1 = not gb_sets:is_member(Val, get_poss(F1,Check,gb_sets:empty())),
    R2 = not gb_sets:is_member(Val, get_poss(F2,Check,gb_sets:empty())),
    R3 = not gb_sets:is_member(Val, get_poss(F3,Check,gb_sets:empty())),
    %%    io:format("M=~p ~p ~p ~p ~p~n",[M,[R1,R2,R3],gb_sets:to_list(element(F1,Check)),gb_sets:to_list(element(F2,Check)),gb_sets:to_list(element(F3,Check))]),  
    if R1,R2 -> update_must(Type,F3,Val,Must0);
       R1,R3 -> update_must(Type,F2,Val,Must0);
       R2,R3 -> update_must(Type,F1,Val,Must0);
       true  -> Must0
    end.

update_must(Type,[Indx|_],Val,Must) ->
    N = mindx(Type, Indx),
    %%    io:format("~p ~p ~p must contain ~p~n",[Type,N,rcm(Indx),Val]),
    Set = element(N,Must),
    setelement(N,Must, gb_sets:add(Val,Set)).

add(RCM={R,C,M},Val,S=#s{p=P0,m=MS,v=Vals,mr=MR0,mc=MC0}) ->
    Ri = mindx(R,M),
    Ci = mindx(M,C),
    MR = delete(Val,Ri,MR0),
    MC = delete(Val,Ci,MC0),
    P1 = setelement(indx(RCM),P0,gb_sets:empty()),
    check_must(S#s{p=delete(Val,lists:flatten(all(RCM)),P1),
		   m=delete(Val,M,MS),
		   mr=MR,mc=MC,
		   v=setelement(indx(RCM),Vals,Val)}).

poss(RCM={R,C,M}, #s{p=P,v=Vals,mr=MR,mc=MC}) ->
    I = indx(R,C),
    case element(I, Vals) of
	0 ->
	    Rm = mindx(R,M),
	    Cm = mindx(M,C),
	    T1 = gb_sets:intersection(element(Rm,MR),element(Cm,MC)),
	    case gb_sets:size(T1) of
		1 -> T1;
		_ ->
		    Not = get_nots(RCM,MR,MC),
		    gb_sets:difference(element(I,P),Not)
	    end;
	_ ->
	    gb_sets:empty()
    end.

get_nots({R,C,M},MR,MC) ->		    
    [RM1,RM2,CM1,CM2] = other_mats(M),
    R1 = get_poss([mindx(R,RM1),mindx(R,RM2)],MR,gb_sets:empty()),
    R2 = get_poss([mindx(CM1,C),mindx(CM2,C)],MC,R1),
    %%     io:format("~p:~p:~p ~p ~p~n", 
    %% 	      [C,CM1,CM2,
    %% 	       gb_sets:to_list(element(mindx(CM1,C),MC)),
    %% 	       gb_sets:to_list(element(mindx(CM2,C),MC))]),
    R2.

get_poss([],_,Tot) -> Tot;
get_poss([H|R],What,Tot) ->
    %%     io:format("~p~n",[H]),
    get_poss(R,What, gb_sets:union(element(H,What),Tot)).

mindx(row,Indx) -> 
    {R,_C,M} = rcm(Indx),
    mindx(R,M);
mindx(col,Indx) -> 
    {_R,C,M} = rcm(Indx),
    mindx(M,C);

mindx(R,M) ->
    1+(R-1)*3 + (M-1) rem 3.

rcm(Indx) when is_integer(Indx) ->
    rcm({((Indx-1) div 9)+1, (Indx-1) rem 9+1});
rcm({R,C}) ->
    M = mat(R,C),
    {R,C,M}.
mat(R,C) ->
    1+(C-1) div 3 + ((R-1) div 3)*3.

rc_in_mat(M) -> 
    R1 = 1+3*((M-1) div 3),
    C1 = 1+3*((M-1) rem 3),
    {{[indx({R1+0,C1+0}),indx({R1+0,C1+1}),indx({R1+0,C1+2})],
      [indx({R1+1,C1+0}),indx({R1+1,C1+1}),indx({R1+1,C1+2})],
      [indx({R1+2,C1+0}),indx({R1+2,C1+1}),indx({R1+2,C1+2})]},

     {[indx({R1+0,C1+0}),indx({R1+1,C1+0}),indx({R1+2,C1+0})],
      [indx({R1+0,C1+1}),indx({R1+1,C1+1}),indx({R1+2,C1+1})],
      [indx({R1+0,C1+2}),indx({R1+1,C1+2}),indx({R1+2,C1+2})]}}.

indx(Indx) when is_integer(Indx) -> Indx;
indx({Row, Col}) ->
    indx(Row,Col);
indx({Row, Col,_}) ->
    indx(Row,Col).
indx(Row, Col) ->
    (Row-1)*9+Col.

delete(_Val,[],S0) -> S0;
delete(Val,[I1|R],S0) ->
    I = if is_integer(I1) -> I1;
	   true -> indx(I1)
	end,
    S = setelement(I,S0,gb_sets:delete_any(Val, element(I,S0))),
    delete(Val,R,S);
delete(Val,I,S) ->
    setelement(I,S,gb_sets:delete_any(Val, element(I,S))).

%%%%%%%%%%%%%%%%%%%%%%%%%

%% Pre generate games on low priority
create_games(Levels,Engine) ->
    gen_loop(Levels, Engine, 5).

gen_loop([], Engine,_) ->
    receive 
	{gen_game, Level} ->
	    gen_loop([Level], Engine,5)
    end;
gen_loop([Level|Ls], Engine, N) when N > 0 ->    
    Empty = empty_table(#s{}),
    Temp = new_game(Empty),
    Game = pick_shown(Temp,Level,undefined),    
    ResLev = length(Game),
    Engine ! {game, {ResLev, Game}},
    case ResLev =< level(Level) of
	true -> 
	    gen_loop(Ls,Engine, 5);
	false -> 
	    gen_loop([Level|Ls],Engine, N-1)
    end;
gen_loop([_|Ls],Engine, _) ->
    gen_loop(Ls,Engine, 5).

find_game(_, #s{games=[]}) -> false;
find_game(hardest, S = #s{games=Gs0}) ->
    Hard = level(hard),
    case lists:sort(Gs0) of
	[{Level,G}|Gs] when Level < (Hard-5) -> 
	    {G, S#s{games=Gs}};
	_ -> false
    end;
find_game(Level, S = #s{games=Gs0}) -> 
    case find_game2(level(Level), lists:reverse(lists:sort(Gs0)), []) of
	false -> false;
	{Game, Gs} -> {Game,S#s{games=Gs}}
    end.

find_game2(Hard, [{Level,G}|Gs], Acc) when Level =< Hard, Level > (Hard-5)  -> 
    {G, Gs ++ Acc};
find_game2(Hard, [G|Gs], Acc) -> 
    find_game2(Hard, Gs, [G|Acc]);
find_game2(_Hard, [], _ ) -> false.
		   	    
levels() ->
    [trivial,easy,normal,hard,hardest].

level(Level) when is_atom(Level) ->
    case Level of
	all ->   100;
	trivial -> 40;
	easy ->   35;
	normal -> 30;
	hard ->   25;
	hardest -> 0
    end;
level(Int) when is_integer(Int) ->
    if 
	Int =< 20 -> hardest;
	Int =< 25 -> hard;
	Int =< 30 -> normal;
	Int =< 35 -> easy;
	true -> trivial
    end.
	     

     
