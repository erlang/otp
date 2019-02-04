%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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
%% Purpose : Inline high order lists functions from the lists module.

-module(sys_core_fold_lists).

-export([call/4]).

-include("core_parse.hrl").

%% We inline some very common higher order list operations.
%% We use the same evaluation order as the library function.

-spec call(cerl:c_call(), atom(), atom(), [cerl:cerl()]) ->
		  'none' | cerl:cerl().

call(#c_call{anno=Anno}, lists, all, [Arg1,Arg2]) ->
    Loop = #c_var{name={'lists^all',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{anno=Anno,
                    pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_apply{anno=Anno, op=Loop, args=[Xs]}},
    CC2 = #c_clause{anno=Anno,
                    pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=#c_literal{val=false}},
    CC3 = #c_clause{anno=Anno,
                    pats=[X], guard=#c_literal{val=true},
		    body=match_fail(Anno, Err1)},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_case{arg=#c_apply{anno=Anno, op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{anno=Anno,
                   pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=1}]},
		   body=#c_literal{val=true}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, F, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^all',1}}|Anno], Err2)},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L]}}};
call(#c_call{anno=Anno}, lists, any, [Arg1,Arg2]) ->
    Loop = #c_var{name={'lists^any',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{anno=Anno,
                    pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_literal{val=true}},
    CC2 = #c_clause{anno=Anno,
                    pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=#c_apply{anno=Anno, op=Loop, args=[Xs]}},
    CC3 = #c_clause{anno=Anno,
                    pats=[X], guard=#c_literal{val=true},
		    body=match_fail(Anno, Err1)},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_case{arg=#c_apply{anno=Anno, op=F, args=[X]},
				clauses = [CC1, CC2, CC3]}},
    C2 = #c_clause{anno=Anno,
                   pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=1}]},
		   body=#c_literal{val=false}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, F, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^any',1}}|Anno], Err2)},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L]}}};
call(#c_call{anno=Anno}, lists, foreach, [Arg1,Arg2]) ->
    Loop = #c_var{name={'lists^foreach',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_seq{arg=#c_apply{anno=Anno, op=F, args=[X]},
			       body=#c_apply{anno=Anno, op=Loop, args=[Xs]}}},
    C2 = #c_clause{anno=Anno, pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=1}]},
		   body=#c_literal{val=ok}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^foreach',1}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L]}}};
call(#c_call{anno=Anno}, lists, map, [Arg1,Arg2]) ->
    Loop = #c_var{name={'lists^map',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[H], arg=#c_apply{anno=Anno,
						      op=F,
						      args=[X]},
			       body=#c_cons{hd=H,
					    anno=[compiler_generated],
					    tl=#c_apply{anno=Anno,
							op=Loop,
							args=[Xs]}}}},
    C2 = #c_clause{anno=Anno, pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=1}]},
		   body=#c_literal{val=[]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, Xs]},
    C3 = #c_clause{pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^map',1}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L]}}};
call(#c_call{anno=Anno}, lists, flatmap, [Arg1,Arg2]) ->
    Loop = #c_var{name={'lists^flatmap',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    H = #c_var{name='H'},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[H],
			       arg=#c_apply{anno=Anno, op=F, args=[X]},
			       body=#c_call{anno=[compiler_generated|Anno],
					    module=#c_literal{val=erlang},
					    name=#c_literal{val='++'},
					    args=[H,
						  #c_apply{anno=Anno,
							   op=Loop,
							   args=[Xs]}]}}},
    C2 = #c_clause{anno=Anno, pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=1}]},
		   body=#c_literal{val=[]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^flatmap',1}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L]}}};
call(#c_call{anno=Anno}, lists, filter, [Arg1,Arg2]) ->
    Loop = #c_var{name={'lists^filter',1}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    B = #c_var{name='B'},
    Err1 = #c_tuple{es=[#c_literal{val='case_clause'}, X]},
    CC1 = #c_clause{anno=Anno,
                    pats=[#c_literal{val=true}], guard=#c_literal{val=true},
		    body=#c_cons{anno=[compiler_generated], hd=X, tl=Xs}},
    CC2 = #c_clause{anno=Anno,
                    pats=[#c_literal{val=false}], guard=#c_literal{val=true},
		    body=Xs},
    CC3 = #c_clause{anno=Anno, pats=[X], guard=#c_literal{val=true},
		    body=match_fail(Anno, Err1)},
    Case = #c_case{arg=B, clauses = [CC1, CC2, CC3]},
    C1 = #c_clause{pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_let{vars=[B],
			       arg=#c_apply{anno=Anno, op=F, args=[X]},
			       body=#c_let{vars=[Xs],
					   arg=#c_apply{anno=Anno,
							op=Loop,
							args=[Xs]},
					   body=Case}}},
    C2 = #c_clause{anno=Anno,
                   pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=1}]},
		   body=#c_literal{val=[]}},
    Err2 = #c_tuple{es=[#c_literal{val='function_clause'}, F, Xs]},
    C3 = #c_clause{anno=Anno,
                   pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^filter',1}}|Anno], Err2)},
    Fun = #c_fun{vars=[Xs],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, L], arg=#c_values{es=[Arg1, Arg2]},
		body=#c_letrec{defs=[{Loop,Fun}],
			       body=#c_apply{anno=Anno, op=Loop, args=[L]}}};
call(#c_call{anno=Anno}, lists, foldl, [Arg1,Arg2,Arg3]) ->
    Loop = #c_var{name={'lists^foldl',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_apply{anno=Anno,
				 op=Loop,
				 args=[Xs, #c_apply{anno=Anno,
						    op=F,
						    args=[X, A]}]}},
    C2 = #c_clause{anno=Anno, pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=2}]},
		   body=A},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, A, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^foldl',2}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L, A]}}};
call(#c_call{anno=Anno}, lists, foldr, [Arg1,Arg2,Arg3]) ->
    Loop = #c_var{name={'lists^foldr',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    A = #c_var{name='A'},
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=#c_apply{anno=Anno,
				 op=F,
				 args=[X, #c_apply{anno=Anno,
						   op=Loop,
						   args=[Xs, A]}]}},
    C2 = #c_clause{anno=Anno, pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=2}]},
		   body=A},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, A, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^foldr',2}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs, A],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, A, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
	   body=#c_letrec{defs=[{Loop,Fun}],
			  body=#c_apply{anno=Anno, op=Loop, args=[L, A]}}};
call(#c_call{anno=Anno}, lists, mapfoldl, [Arg1,Arg2,Arg3]) ->
    Loop = #c_var{name={'lists^mapfoldl',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Avar = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{anno=Anno, pats=[P], guard=#c_literal{val=true}, body=E},
		Err = #c_tuple{es=[#c_literal{val='badmatch'}, X]},
		C2 = #c_clause{anno=Anno, pats=[X], guard=#c_literal{val=true},
			       body=match_fail(Anno, Err)},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{anno=Anno,
                   pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
		   body=Match(#c_apply{anno=Anno, op=F, args=[X, Avar]},
			      #c_tuple{es=[X, Avar]},
%%% Tuple passing version
			      Match(#c_apply{anno=Anno,
					     op=Loop,
					     args=[Xs, Avar]},
				    #c_tuple{es=[Xs, Avar]},
				    #c_tuple{anno=[compiler_generated],
					     es=[#c_cons{anno=[compiler_generated],
							 hd=X, tl=Xs},
						 Avar]})
%%% Multiple-value version
%%% 			      #c_let{vars=[Xs,A],
%%% 				     %% The tuple here will be optimised
%%% 				     %% away later; no worries.
%%% 				     arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 				     body=#c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 							A]}}
			     )},
    C2 = #c_clause{anno=Anno, pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=2}]},
%%% Tuple passing version
		   body=#c_tuple{anno=[compiler_generated],
				 es=[#c_literal{val=[]}, Avar]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_literal{val=[]}, A]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, Avar, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^mapfoldl',2}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs, Avar],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, Avar, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
	   body=#c_letrec{defs=[{Loop,Fun}],
%%% Tuple passing version
			  body=#c_apply{anno=Anno,
					op=Loop,
					args=[L, Avar]}}};
%%% Multiple-value version
%%% 			       body=#c_let{vars=[Xs, A],
%%% 					   arg=#c_apply{op=Loop,
%%% 							args=[L, A]},
%%% 					   body=#c_tuple{es=[Xs, A]}}}};
call(#c_call{anno=Anno}, lists, mapfoldr, [Arg1,Arg2,Arg3]) ->
    Loop = #c_var{name={'lists^mapfoldr',2}},
    F = #c_var{name='F'},
    Xs = #c_var{name='Xs'},
    X = #c_var{name='X'},
    Avar = #c_var{name='A'},
    Match =
	fun (A, P, E) ->
		C1 = #c_clause{anno=Anno, pats=[P], guard=#c_literal{val=true}, body=E},
		Err = #c_tuple{es=[#c_literal{val='badmatch'}, X]},
		C2 = #c_clause{anno=Anno, pats=[X], guard=#c_literal{val=true},
			       body=match_fail(Anno, Err)},
		#c_case{arg=A, clauses=[C1, C2]}
	end,
    C1 = #c_clause{anno=Anno, pats=[#c_cons{hd=X, tl=Xs}], guard=#c_literal{val=true},
%%% Tuple passing version
		   body=Match(#c_apply{anno=Anno,
				       op=Loop,
				       args=[Xs, Avar]},
			      #c_tuple{es=[Xs, Avar]},
			      Match(#c_apply{anno=Anno, op=F, args=[X, Avar]},
				    #c_tuple{es=[X, Avar]},
				    #c_tuple{anno=[compiler_generated],
					     es=[#c_cons{anno=[compiler_generated],
							 hd=X, tl=Xs}, Avar]}))
%%% Multiple-value version
%%% 		   body=#c_let{vars=[Xs,A],
%%% 			       %% The tuple will be optimised away
%%% 			       arg=#c_apply{op=Loop, args=[Xs, A]},
%%% 			       body=Match(#c_apply{op=F, args=[X, A]},
%%% 					  #c_tuple{es=[X, A]},
%%% 					  #c_values{es=[#c_cons{hd=X, tl=Xs},
%%% 						        A]})}
		  },
    C2 = #c_clause{anno=Anno,
                   pats=[#c_literal{val=[]}],
		   guard=#c_call{module=#c_literal{val=erlang},
				 name=#c_literal{val=is_function},
				 args=[F, #c_literal{val=2}]},
%%% Tuple passing version
		   body=#c_tuple{anno=[compiler_generated],
				 es=[#c_literal{val=[]}, Avar]}},
%%% Multiple-value version
%%% 		   body=#c_values{es=[#c_literal{val=[]}, A]}},
    Err = #c_tuple{es=[#c_literal{val='function_clause'}, F, Avar, Xs]},
    C3 = #c_clause{anno=Anno, pats=[Xs], guard=#c_literal{val=true},
		   body=match_fail([{function_name,{'lists^mapfoldr',2}}|Anno], Err)},
    Fun = #c_fun{vars=[Xs, Avar],
		 body=#c_case{arg=Xs, clauses=[C1, C2, C3]}},
    L = #c_var{name='L'},
    #c_let{vars=[F, Avar, L], arg=#c_values{es=[Arg1, Arg2, Arg3]},
	   body=#c_letrec{defs=[{Loop,Fun}],
%%% Tuple passing version
			  body=#c_apply{anno=Anno,
					op=Loop,
					args=[L, Avar]}}};
%%% Multiple-value version
%%% 			       body=#c_let{vars=[Xs, A],
%%% 					   arg=#c_apply{op=Loop,
%%% 							args=[L, A]},
%%% 					   body=#c_tuple{es=[Xs, A]}}}};
call(_, _, _, _) ->
    none.

match_fail(Ann, Arg) ->
    Name = cerl:abstract(match_fail),
    Args = [Arg],
    cerl:ann_c_primop(Ann, Name, Args).
