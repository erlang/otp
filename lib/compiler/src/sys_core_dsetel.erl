%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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
%% Purpose : Using dsetelement to make multiple-field record updates
%% faster.

%% The expansion of record field updates, when more than one field is
%% updated, but not a majority of the fields, will create a sequence of
%% calls to 'erlang:setelement(Index, Value, Tuple)' where Tuple in the
%% first call is the original record tuple, and in the subsequent calls
%% Tuple is the result of the previous call. Furthermore, all Index
%% values are constant positive integers, and the first call to
%% 'setelement' will have the greatest index. Thus all the following
%% calls do not actually need to test at run-time whether Tuple has type
%% tuple, nor that the index is within the tuple bounds.
%%
%% Since this introduces destructive updates in the Core Erlang code, it
%% must be done as a last stage before going to lower-level code.
%%
%% NOTE: Because there are currently no write barriers in the system,
%% this kind of optimization can only be done when we are sure that
%% garbage collection will not be triggered between the creation of the
%% tuple and the destructive updates - otherwise we might insert
%% pointers from an older generation to a newer.
%%
%% The rewriting is done as follows:
%%
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in call 'erlang':'setelement(3, X1, Value2)
%%  =>
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop dsetelement(3, X1, Value2)
%%       X1
%% and
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in let X2 = call 'erlang':'setelement(3, X1, Value2)
%%    in ...
%%  =>
%%    let X2 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop 'dsetelement(3, X2, Value2)
%%       ...
%% if X1 is used exactly once.
%% Thus, we need to track variable usage.
%%

-module(sys_core_dsetel).

-export([module/2]).

-include("core_parse.hrl").

-spec module(cerl:c_module(), [compile:option()]) -> {'ok', cerl:c_module()}.

module(M0, _Options) ->
    M = visit_module(M0),
    {ok,M}.

visit_module(#c_module{defs=Ds0}=R) ->
    Env = #{},
    Ds = visit_module_1(Ds0, Env, []),
    R#c_module{defs=Ds}.

visit_module_1([{Name,F0}|Fs], Env, Acc) ->
    try visit(Env, F0) of
	{F,_} ->
	    visit_module_1(Fs, Env, [{Name,F}|Acc])
    catch
        Class:Error:Stack ->
	    #c_var{name={Func,Arity}} = Name,
	    io:fwrite("Function: ~w/~w\n", [Func,Arity]),
	    erlang:raise(Class, Error, Stack)
    end;
visit_module_1([], _, Acc) ->
    lists:reverse(Acc).

visit(Env, #c_var{name={_,_}}=R) ->
    %% Ignore local function name.
    {R, Env};
visit(Env0, #c_var{name=X}=R) ->
    %% There should not be any free variables. If there are,
    %% the case will fail with an exception.
    case Env0 of
	#{X:=N} ->
	    {R, Env0#{X:=N+1}}
    end;
visit(Env, #c_literal{}=R) ->
    {R, Env};
visit(Env0, #c_tuple{es=Es0}=R) ->
    {Es1,Env1} = visit_list(Env0, Es0),
    {R#c_tuple{es=Es1}, Env1};
visit(Env0, #c_map{es=Es0}=R) ->
    {Es1,Env1} = visit_list(Env0, Es0),
    {R#c_map{es=Es1}, Env1};
visit(Env0, #c_map_pair{key=K0,val=V0}=R) ->
    {K,Env1} = visit(Env0, K0),
    {V,Env2} = visit(Env1, V0),
    {R#c_map_pair{key=K,val=V}, Env2};
visit(Env0, #c_cons{hd=H0,tl=T0}=R) ->
    {H1,Env1} = visit(Env0, H0),
    {T1,Env2} = visit(Env1, T0),
    {R#c_cons{hd=H1,tl=T1}, Env2};
visit(Env0, #c_binary{segments=Segs}=R) ->
    Env = visit_bin_segs(Env0, Segs),
    {R, Env};
visit(Env0, #c_values{es=Es0}=R) ->
    {Es1,Env1} = visit_list(Env0, Es0),
    {R#c_values{es=Es1}, Env1};
visit(Env0, #c_fun{vars=Vs, body=B0}=R) ->
    {Xs, Env1} = bind_vars(Vs, Env0),
    {B1,Env2} = visit(Env1, B0),
    {R#c_fun{body=B1}, restore_vars(Xs, Env0, Env2)};
visit(Env0, #c_let{vars=Vs, arg=A0, body=B0}=R) ->
    {A1,Env1} = visit(Env0, A0),
    {Xs,Env2} = bind_vars(Vs, Env1),
    {B1,Env3} = visit(Env2, B0),
    rewrite(R#c_let{arg=A1,body=B1}, Env3, restore_vars(Xs, Env1, Env3));
visit(Env0, #c_seq{arg=A0, body=B0}=R) ->
    {A1,Env1} = visit(Env0, A0),
    {B1,Env2} = visit(Env1, B0),
    {R#c_seq{arg=A1,body=B1}, Env2};
visit(Env0, #c_case{arg=A0,clauses=Cs0}=R) ->
    {A1,Env1} = visit(Env0, A0),
    {Cs1,Env2} = visit_list(Env1, Cs0),
    {R#c_case{arg=A1,clauses=Cs1}, Env2};
visit(Env0, #c_clause{pats=Ps,guard=G0,body=B0}=R) ->
    {Vs, Env1} = visit_pats(Ps, Env0),
    {G1,Env2} = visit(Env1, G0),
    {B1,Env3} = visit(Env2, B0),
    {R#c_clause{guard=G1,body=B1}, restore_vars(Vs, Env0, Env3)};
visit(Env0, #c_receive{clauses=Cs0,timeout=T0,action=A0}=R) ->
    {T1,Env1} = visit(Env0, T0),
    {Cs1,Env2} = visit_list(Env1, Cs0),
    {A1,Env3} = visit(Env2, A0),
    {R#c_receive{clauses=Cs1,timeout=T1,action=A1}, Env3};
visit(Env0, #c_apply{op=Op0, args=As0}=R) ->
    {Op1,Env1} = visit(Env0, Op0),
    {As1,Env2} = visit_list(Env1, As0),
    {R#c_apply{op=Op1,args=As1}, Env2};
visit(Env0, #c_call{module=M0,name=N0,args=As0}=R) ->
    {M1,Env1} = visit(Env0, M0),
    {N1,Env2} = visit(Env1, N0),
    {As1,Env3} = visit_list(Env2, As0),
    {R#c_call{module=M1,name=N1,args=As1}, Env3};
visit(Env0, #c_primop{name=N0, args=As0}=R) ->
    {N1,Env1} = visit(Env0, N0),
    {As1,Env2} = visit_list(Env1, As0),
    {R#c_primop{name=N1,args=As1}, Env2};
visit(Env0, #c_try{arg=E0, vars=Vs, body=B0, evars=Evs, handler=H0}=R) ->
    {E1,Env1} = visit(Env0, E0),
    {Xs, Env2} = bind_vars(Vs, Env1),
    {B1,Env3} = visit(Env2, B0),
    Env4 = restore_vars(Xs, Env1, Env3),
    {Ys, Env5} = bind_vars(Evs, Env4),
    {H1,Env6} = visit(Env5, H0),
    {R#c_try{arg=E1,body=B1,handler=H1}, restore_vars(Ys, Env4, Env6)};
visit(Env0, #c_catch{body=B0}=R) ->
    {B1,Env1} = visit(Env0, B0),
    {R#c_catch{body=B1}, Env1};
visit(Env0, #c_letrec{defs=Ds0,body=B0}=R) ->
    {Xs, Env1} = bind_vars([V || {V,_} <- Ds0], Env0),
    {Ds1,Env2} = visit_def_list(Env1, Ds0),
    {B1,Env3} = visit(Env2, B0),
    {R#c_letrec{defs=Ds1,body=B1}, restore_vars(Xs, Env0, Env3)}.
%% The following general code for handling modules is slow if a module
%% contains very many functions. There is special code in visit_module/1
%% which is much faster.
%% visit(Env0, #c_module{defs=D0}=R) ->
%%     {R1,Env1} = visit(Env0, #c_letrec{defs=D0,body=#c_nil{}}),
%%     {R#c_module{defs=R1#c_letrec.defs}, Env1};

visit_list(Env, L) ->
    lists:mapfoldl(fun (E, A) -> visit(A, E) end, Env, L).

visit_def_list(Env, L) ->
    lists:mapfoldl(fun ({Name,V0}, E0) ->
			   {V1,E1} = visit(E0, V0),
			   {{Name,V1}, E1}
		   end, Env, L).

visit_bin_segs(Env, Segs) ->
    lists:foldl(fun (#c_bitstr{val=Val,size=Sz}, E0) ->
			{_, E1} = visit(E0, Val),
			{_, E2} = visit(E1, Sz),
			E2
		end, Env, Segs).
    
bind_vars(Vs, Env) ->
    bind_vars(Vs, Env, []).

bind_vars([#c_var{name=X}|Vs], Env0, Xs)->
    bind_vars(Vs, Env0#{X=>0}, [X|Xs]);
bind_vars([], Env,Xs) ->
    {Xs, Env}.

visit_pats(Ps, Env) ->
    visit_pats(Ps, Env, []).

visit_pats([P|Ps], Env0, Vs0) ->
    {Vs1, Env1} = visit_pat(Env0, P, Vs0),
    visit_pats(Ps, Env1, Vs1);
visit_pats([], Env, Vs) ->
    {Vs, Env}.

visit_pat(Env0, #c_var{name=V}, Vs) ->
    {[V|Vs], Env0#{V=>0}};
visit_pat(Env0, #c_tuple{es=Es}, Vs) ->
    visit_pats(Es, Env0, Vs);
visit_pat(Env0, #c_map{es=Es}, Vs) ->
    visit_pats(Es, Env0, Vs);
visit_pat(Env0, #c_map_pair{op=#c_literal{val=exact},key=V,val=K}, Vs0) ->
    {Vs1, Env1} = visit_pat(Env0, V, Vs0),
    visit_pat(Env1, K, Vs1);
visit_pat(Env0, #c_cons{hd=H,tl=T}, Vs0) ->
    {Vs1, Env1} = visit_pat(Env0, H, Vs0),
    visit_pat(Env1, T, Vs1);
visit_pat(Env0, #c_binary{segments=Segs}, Vs) ->
    visit_pats(Segs, Env0, Vs);
visit_pat(Env0, #c_bitstr{val=Val,size=Sz}, Vs0) ->
    {Vs1, Env1} =
	case Sz of
	    #c_var{name=V} ->
		%% We don't tolerate free variables.
		case Env0 of
		    #{V:=N} ->
			{Vs0, Env0#{V:=N+1}}
		end;
	    _ ->
		visit_pat(Env0, Sz, Vs0)
	end,
    visit_pat(Env1, Val, Vs1);
visit_pat(Env0, #c_alias{pat=P,var=#c_var{name=V}}, Vs) ->
    visit_pat(Env0#{V=>0}, P, [V|Vs]);
visit_pat(Env, #c_literal{}, Vs) ->
    {Vs, Env}.

restore_vars([V|Vs], Env0, Env1) ->
    case Env0 of
	#{V:=N} ->
	    restore_vars(Vs, Env0, Env1#{V=>N});
	_ ->
	    restore_vars(Vs, Env0, maps:remove(V, Env1))
    end;
restore_vars([], _, Env1) ->
    Env1.


%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in call 'erlang':'setelement(3, X1, Value2)
%%  =>
%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop dsetelement(3, X1, Value2)
%%       X1

rewrite(#c_let{vars=[#c_var{name=X}=V]=Vs,
	       arg=#c_call{module=#c_literal{val='erlang'},
			   name=#c_literal{val='setelement'},
			   args=[#c_literal{val=Index1}, _Tuple, _Val1]
			  }=A,
 	       body=#c_call{anno=Banno,module=#c_literal{val='erlang'},
 			    name=#c_literal{val='setelement'},
  			    args=[#c_literal{val=Index2},
				  #c_var{name=X},
				  Val2]
  			   }
	      }=R,
	_BodyEnv, FinalEnv)
  when is_integer(Index1), is_integer(Index2), Index2 > 0, Index1 > Index2 ->
    case is_safe(Val2) of
	true ->
	    {R#c_let{vars=Vs,
		     arg=A,
		     body=#c_seq{arg=#c_primop{
				   anno=Banno,
				   name=#c_literal{val='dsetelement'},
				   args=[#c_literal{val=Index2},
					 V,
					 Val2]},
				 body=V}
		    },
	     FinalEnv};
	false ->
	    {R, FinalEnv}
    end;

%%    let X1 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in let X2 = 'erlang':'setelement(3, X1, Value2)
%%    in ...
%%  =>
%%    let X2 = call 'erlang':'setelement(5, Tuple, Value1)
%%    in do primop dsetelement(3, X2, Value2)
%%       ...
%% if X1 is used exactly once.

rewrite(#c_let{vars=[#c_var{name=X1}],
	       arg=#c_call{module=#c_literal{val='erlang'},
			   name=#c_literal{val='setelement'},
			   args=[#c_literal{val=Index1}, _Tuple, _Val1]
			  }=A,
	       body=#c_let{vars=[#c_var{}=V]=Vs,
			   arg=#c_call{anno=Banno,
				       module=#c_literal{val='erlang'},
				       name=#c_literal{val='setelement'},
				       args=[#c_literal{val=Index2},
					     #c_var{name=X1},
					     Val2]},
			   body=B}
	      }=R,
	BodyEnv, FinalEnv)
  when is_integer(Index1), is_integer(Index2), Index2 > 0, Index1 > Index2 ->
    case is_single_use(X1, BodyEnv) andalso is_safe(Val2) of
	true ->
	    {R#c_let{vars=Vs,
		     arg=A,
		     body=#c_seq{arg=#c_primop{
				   anno=Banno,
				   name=#c_literal{val='dsetelement'},
				   args=[#c_literal{val=Index2},
					 V,
					 Val2]},
				 body=B}
		    },
	     FinalEnv};
	false ->
	    {R, FinalEnv}
    end;

rewrite(R, _, FinalEnv) ->
    {R, FinalEnv}.

%% is_safe(CoreExpr) -> true|false
%%  Determines whether the Core expression can cause a GC collection at run-time.
%%  Note: Assumes that the constant pool is turned on.

is_safe(#c_var{}) -> true;
is_safe(#c_literal{}) -> true;
is_safe(_) -> false.

is_single_use(V, Env) ->
    case Env of
	#{V:=1} ->
	    true;
	_ ->
	    false
    end.
