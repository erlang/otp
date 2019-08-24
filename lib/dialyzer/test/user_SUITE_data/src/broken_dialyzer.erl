-module(broken_dialyzer).

-export([do_move_next/1]).

-define(ap_indices, 512).
-define(dp_indices, 504).


-record(apR,{a,c=[],n=[],nc=0,nn=0,nl=[]}).
-define(apL(L), [#apR{a=A} || A <- L]).

-define(gr, get(my_return_value)).
-define(pr(PR), put(my_return_value, PR)).
-record(bit,{i,c,n,s}).           % index, current, next, state


do_move_next({BL,AL}) ->
    Max = max(length(BL), length(AL)),
    Max2 = max(length(BL)*2, length(AL)),
    MoveTo = [A || A <- AL, A#apR.nn < Max, A#apR.nn+A#apR.nc < Max2],
    MoveFrom = [A || A <- AL,
                     (A#apR.nn > Max) orelse (A#apR.nn+A#apR.nc > Max2)],
    Unchanged = (AL--MoveTo)--MoveFrom,
    {BL1,{AL1,{AL2,AL3}}} =
        lists:mapfoldl(
          fun(B=#bit{i=I,c=C,s=S,n=Next}, {From,{To,FilledUp}})
             when S==ok;S==lost_replica;S==moved_replica ->
                  case lists:keysearch(Next,#apR.a,From) of
                      {value, F=#apR{n=N1,nn=NN1,nc=NC1}}
                      when (NN1>Max) or (NN1+NC1>Max2) ->
                          case C of
                              [] ->
                                  {B, {From,{To,FilledUp}}};
                              ShortList ->
                                  T=#apR{a=NewNext,n=N2,nn=NN2} =
                                      find_next(Next,ShortList),
                                  {value, {C,NL_from}} =
                                      lists:keysearch(C,1,F#apR.nl),
                                  {value, {C,NL_to}} =
                                      lists:keysearch(C,1,T#apR.nl),
                                  NewNL_from = lists:keyreplace(
                                                 C,1,F#apR.nl,{C,NL_from--[I]}),
                                  NewNL_to = lists:keyreplace(
                                               C,1,T#apR.nl,{C,[I|NL_to]}),

                                  NewT = T#apR{n=[I|N2],nn=NN2+1,
                                               nl=NewNL_to},

                                  {B#bit{n=NewNext,
                                         s = if
                                                 S == lost_replica ->
                                                     lost_replica;
                                                 true ->
                                                     moved_replica
                                             end},
                                   {lists:keyreplace(
                                      Next,#apR.a,From,
                                      F#apR{n=N1--[I],nn=NN1-1,nl=NewNL_from}),
                                    if
                                        (NewT#apR.nn+NewT#apR.nc >= Max2)
                                        or (NewT#apR.nn >= Max) ->
                                            {lists:keydelete(NewNext,#apR.a,To),
                                             [NewT|FilledUp]};
                                        true ->
                                            {lists:keyreplace(
                                               NewNext,#apR.a,To,NewT),
                                             FilledUp}
                                    end}}
                          end;
                      _ ->
                          {B, {From,{To,FilledUp}}}
                  end;
             (B, A) ->
                  {B, A}
          end, {MoveFrom,{MoveTo,[]}},BL),
    {BL1,Unchanged++AL1++AL2++AL3}.

%%% -----------------------------------------------------------------
%%% find_next/2
%%%
%%% ------------------------------------------------------------------

find_next(Ap,L) ->
    hd(catch
       lists:foreach(
         fun(SelVal) ->
                 case [ApR ||
                          ApR <- L,
                          begin
                              {value,{Ap,NL}} =
                                  lists:keysearch(Ap,1,ApR#apR.nl),
                              length(NL) =< SelVal
                          end] of
                     [] ->
                         ok;
                     ShortList ->
                         throw(ShortList)
                 end
         end,
         lists:seq(0,?ap_indices))).

%%% -----------------------------------------------------------------
%%% max/2
%%%
%%% Calculates max number of indices per AP, given number of indices
%%% and number of APs.
%%% -----------------------------------------------------------------
max(F,S) ->
    (F div S) + if
                    (F rem S) == 0 ->
                        0;
                    true ->
                        1
                end.

%%% ==============================================================
%%%      ADMINISTRATIVE INFORMATION
%%% ==============================================================
%%% #Copyright (C) 2005
%%% by ERICSSON TELECOM AB
%%% S - 125 26  STOCKHOLM
%%% SWEDEN, tel int + 46 8 719 0000
%%%
%%% The program may be used and/or copied only with the written
%%% permission from ERICSSON TELECOM AB, or in accordance with
%%% the terms and conditions stipulated in the agreement/contract
%%% under which the program has been supplied.
%%%
%%% All rights reserved
%%%
