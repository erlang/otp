%%% Copyright 2010-2013 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%
%%%

%%% @copyright 2010-2013 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis
%%% @doc Common parts of user and internal header files


%%------------------------------------------------------------------------------
%% Test generation macros
%%------------------------------------------------------------------------------

-define(FORALL(X,RawType,Prop), proper:forall(RawType,fun(X) -> Prop end)).
-define(IMPLIES(Pre,Prop), proper:implies(Pre,?DELAY(Prop))).
-define(WHENFAIL(Action,Prop), proper:whenfail(?DELAY(Action),?DELAY(Prop))).
-define(TRAPEXIT(Prop), proper:trapexit(?DELAY(Prop))).
-define(TIMEOUT(Limit,Prop), proper:timeout(Limit,?DELAY(Prop))).
%% TODO: -define(ALWAYS(Tests,Prop), proper:always(Tests,?DELAY(Prop))).
%% TODO: -define(SOMETIMES(Tests,Prop), proper:sometimes(Tests,?DELAY(Prop))).


%%------------------------------------------------------------------------------
%% Generator macros
%%------------------------------------------------------------------------------

-define(FORCE(X), (X)()).
-define(DELAY(X), fun() -> X end).
-define(LAZY(X), proper_types:lazy(?DELAY(X))).
-define(SIZED(SizeArg,Gen), proper_types:sized(fun(SizeArg) -> Gen end)).
-define(LET(X,RawType,Gen), proper_types:bind(RawType,fun(X) -> Gen end,false)).
-define(SHRINK(Gen,AltGens),
	proper_types:shrinkwith(?DELAY(Gen),?DELAY(AltGens))).
-define(LETSHRINK(Xs,RawType,Gen),
	proper_types:bind(RawType,fun(Xs) -> Gen end,true)).
-define(SUCHTHAT(X,RawType,Condition),
	proper_types:add_constraint(RawType,fun(X) -> Condition end,true)).
-define(SUCHTHATMAYBE(X,RawType,Condition),
	proper_types:add_constraint(RawType,fun(X) -> Condition end,false)).
