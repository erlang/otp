%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2018. All Rights Reserved.
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
%% Purpose : Function inlining optimisation for Core.

%% This simple function inliner works in two stages:
%%
%% 1. First it extracts all the inlineable functions, either given
%% explicitly or of light enough weight, and inlines them with
%% themselves.  This inlining only uses lighter functions to save
%% recursion and a real code explosion.
%%
%% 2. Run through the rest of the functions inlining all calls to
%% inlineable functions.
%%
%% The weight function is VERY simple, we count the number of nodes in
%% the function body.  We would like to remove non-exported,
%% inlineable functions but this is not trivial as they may be
%% (mutually) recursive.
%%
%% This module will catch many access functions and allow code to use
%% extra functions for clarity which are then explicitly inlined for
%% speed with a compile attribute.  See the example below.
%%
%% It is not clear that inlining will give you very much.

-module(sys_core_inline).

-export([module/2]).

-import(lists, [member/2,map/2,foldl/3,mapfoldl/3,keydelete/3]).

-include("core_parse.hrl").

%% Inline status.
-record(inline, {exports=[],thresh=0,inline=[]}).

%% General function info.
-record(fstat, {func  :: atom(),		%Function name
		arity :: byte(),		%         arity
		def,				%Original definition
		weight=0,			%Weight
		inline=false   :: boolean(),	%Inline func flag
		modified=false :: boolean()}).	%Mod flag

%% Inlineable function info.
-record(ifun, {func  :: atom(),			%Function name
	       arity :: byte(),			%         arity
	       vars,				%Fun vars
	       body,				%    body
	       weight}).			%Weight

-spec module(#c_module{}, [_]) -> {'ok', #c_module{}}.

module(#c_module{exports=Es,defs=Ds0}=Mod, Opts) ->
    case inline_option(Opts) of
	{Thresh,Fs} when is_integer(Thresh), Thresh > 0; Fs =/= [] ->
	    case proplists:get_bool(verbose, Opts) of
		true ->
		    io:format("Old inliner: threshold=~p functions=~p\n",
			      [Thresh,Fs]);
		false -> ok
	    end,
	    Ds1 = inline(Ds0, #inline{exports=Es,thresh=Thresh,inline=Fs}),
	    {ok,Mod#c_module{defs=Ds1}};
	_Other -> {ok,Mod}
    end.
    
inline_option(Opts) ->
    foldl(fun ({inline,{_,_}=Val}, {T,Fs}) ->
		  {T,[Val|Fs]};
	      ({inline,Val}, {T,Fs}) when is_list(Val) ->
		  {T,Val ++ Fs};
	      ({inline,Val}, {_,Fs}) when is_integer(Val) ->
		  {Val,Fs};
	      (_Opt, {_,_}=Def) -> Def
	  end, {0,[]}, Opts).

%% inline([Func], Stat) -> [Func].
%%  Here we do all the work.

inline(Fs0, St0) ->
    %% Generate list of augmented functions.
    Fs1 = map(fun ({#c_var{name={F,A}},#c_fun{body=B}}=Def) ->
		      Weight = cerl_trees:fold(fun weight_func/2, 0, B),
		      #fstat{func=F,arity=A,def=Def,weight=Weight}
	      end, Fs0),
    %% Get inlineable functions, and inline them with themselves.
    {Fs2,Is0} = mapfoldl(fun (Fst, Ifs) ->
			      case is_inlineable(Fst, St0#inline.thresh,
						 St0#inline.inline) of
				  true ->
				      {_,Ffun} = Fst#fstat.def,
				      If = #ifun{func=Fst#fstat.func,
						 arity=Fst#fstat.arity,
						 vars=Ffun#c_fun.vars,
						 body=Ffun#c_fun.body,
						 weight=Fst#fstat.weight},
				      {Fst#fstat{inline=true},[If|Ifs]};
				  false -> {Fst,Ifs}
			      end
		      end, [], Fs1),
    Is1 = map(fun (#ifun{body=B}=If) ->
		      If#ifun{body=cerl_trees:map(match_fail_fun(), B)}
	      end, Is0),
    Is2 = [inline_inline(If, Is1) || If <- Is1],
    %% We would like to remove inlined, non-exported functions here,
    %% but this can be difficult as they may be recursive.
    %% Use fixed inline functions on all functions.
    Fs = [inline_func(F, Is2) || F <- Fs2],
    %% Regenerate module body.
    [Def || #fstat{def=Def} <- Fs].

%% is_inlineable(Fstat, Thresh, [Inline]) -> boolean().

is_inlineable(#fstat{weight=W}, Thresh, _Ofs) when W =< Thresh -> true;
is_inlineable(#fstat{func=F,arity=A}, _Thresh, Ofs) ->
    member({F,A}, Ofs).

%% inline_inline(Ifun, [Inline]) -> Ifun.
%%  Try to inline calls in an inlineable function.  To save us from a
%%  to great code explosion we only inline functions "smaller" than
%%  ourselves.

inline_inline(#ifun{body=B,weight=Iw}=If, Is) ->
    Inline = fun (#c_apply{op=#c_var{name={F,A}},args=As}=Call) ->
		     case find_inl(F, A, Is) of
			 #ifun{vars=Vs,body=B2,weight=W} when W < Iw ->
			     #c_let{vars=Vs,
				     arg=kill_id_anns(core_lib:make_values(As)),
				    body=kill_id_anns(B2)};
			 _Other -> Call
		     end;
		 (Core) -> Core
	     end,
    If#ifun{body=cerl_trees:map(Inline, B)}.

%% inline_func(Fstat, [Inline]) -> Fstat.
%%  Try to inline calls in a normal function.  Here we inline anything
%%  in the inline list.

inline_func(#fstat{def={Name,F0}}=Fstat, Is) ->
    Inline = fun (#c_apply{op=#c_var{name={F,A}},args=As}=Call, Mod) ->
		     case find_inl(F, A, Is) of
			 #ifun{vars=Vs,body=B} ->
			     {#c_let{vars=Vs,
				     arg=kill_id_anns(core_lib:make_values(As)),
				     body=kill_id_anns(B)},
			      true};			%Have modified
			 _Other -> {Call,Mod}
		     end;
		 (Core, Mod) -> {Core,Mod}
	     end,
    {F1,Mod} = cerl_trees:mapfold(Inline, false, F0),
    Fstat#fstat{def={Name,F1},modified=Mod}.

weight_func(_Core, Acc) -> Acc + 1.

%% match_fail_fun() -> fun/1.
%% Return a function to use with map to fix inlineable functions
%% function_clause match_fail (if they have one).

match_fail_fun() ->
    fun (#c_primop{anno=Anno0,name=#c_literal{val=match_fail}}=P) ->
	    Anno = keydelete(function_name, 1, Anno0),
	    P#c_primop{anno=Anno};
	(Other) -> Other
    end.

%% find_inl(Func, Arity, [Inline]) -> #ifun{} | no.

find_inl(F, A, [#ifun{func=F,arity=A}=If|_]) -> If;
find_inl(F, A, [_|Is]) -> find_inl(F, A, Is);
find_inl(_, _, []) -> no.

%% kill_id_anns(Body) -> Body'

kill_id_anns(Body) ->
    cerl_trees:map(fun(#c_fun{anno=A0}=CFun) ->
			   A = kill_id_anns_1(A0),
			   CFun#c_fun{anno=A};
                      (#c_var{anno=A0}=Var) ->
			   A = kill_id_anns_1(A0),
			   Var#c_var{anno=A};
		      (Expr) ->
			   %% Mark everything as compiler generated to
			   %% suppress bogus warnings.
			   A = compiler_generated(cerl:get_ann(Expr)),
			   cerl:set_ann(Expr, A)
		   end, Body).

kill_id_anns_1([{'id',_}|As]) ->
    kill_id_anns_1(As);
kill_id_anns_1([A|As]) ->
    [A|kill_id_anns_1(As)];
kill_id_anns_1([]) -> [].

compiler_generated([compiler_generated|_]=Anno) ->
    Anno;
compiler_generated(Anno) ->
    [compiler_generated|Anno -- [compiler_generated]].
