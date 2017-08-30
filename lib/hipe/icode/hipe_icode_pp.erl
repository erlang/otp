%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2003 by Erik Stenman.  
%% ====================================================================
%%  Filename : 	hipe_icode_pp.erl
%%  Module   :	hipe_icode_pp
%%  Purpose  :  Pretty-printer for Icode.
%%  Notes    : 
%%  History  :	* 2003-04-16 (stenman@epfl.ch): Created.
%%  CVS      :
%%              $Author$
%%              $Date$
%%              $Revision$
%% ====================================================================
%% 
%% @doc
%%   Icode Pretty-Printer.
%% @end
%%
%% ====================================================================

-module(hipe_icode_pp).

-export([pp/1, pp/2, pp_block/1]).

-ifdef(DEBUG_ICODE).
-export([pp_instrs/2]).
-endif.

-include("hipe_icode.hrl").

%%---------------------------------------------------------------------

-spec pp(#icode{}) -> 'ok'.
%% @doc Prettyprints linear Icode on stdout.
%%  <p> Badly formed or unknown instructions are printed surrounded
%%      by three stars "***".</p>
pp(Icode) ->
  pp(standard_io, Icode).

-spec pp(io:device(), #icode{}) -> 'ok'.
%% @doc Prettyprints linear Icode on IoDevice.
%%  <p> Badly formed or unknown instructions are printed surrounded by
%%      three stars "***".</p>
pp(Dev, Icode) ->
  {Mod, Fun, Arity} = hipe_icode:icode_fun(Icode),
  Args = hipe_icode:icode_params(Icode),
  io:format(Dev, "~w:~w/~w(", [Mod, Fun, Arity]),
  pp_args(Dev, Args),
  io:format(Dev, ") ->~n", []),
  io:format(Dev, "%% Info:~p\n",
	    [[case hipe_icode:icode_is_closure(Icode) of
		true -> 'Closure'; 
		false -> 'Not a closure'
	      end,
	      case hipe_icode:icode_is_leaf(Icode) of
		true -> 'Leaf function'; 
		false -> 'Not a leaf function'
	      end |
	      hipe_icode:icode_info(Icode)]]),
  pp_instrs(Dev, hipe_icode:icode_code(Icode)),
  io:format(Dev, "%% Data:\n", []),
  hipe_data_pp:pp(Dev, hipe_icode:icode_data(Icode), icode, "").

-spec pp_block(icode_instrs()) -> 'ok'.
pp_block(Code) ->
  pp_instrs(standard_io, Code).

-spec pp_instrs(io:device(), icode_instrs()) -> 'ok'.
%% @doc Prettyprints a list of Icode instructions.
pp_instrs(Dev, Is) ->
  lists:foreach(fun (I) -> pp_instr(Dev, I) end, Is).

%%---------------------------------------------------------------------

pp_instr(Dev, I) ->
  case I of 
    #icode_label{} ->
      io:format(Dev, "~p:~n", [hipe_icode:label_name(I)]);
    #icode_comment{} ->
      Txt = hipe_icode:comment_text(I),
      Str = case io_lib:deep_char_list(Txt) of
	      true -> Txt;
	      false -> io_lib:format("~p", [Txt])
	    end,
      io:format(Dev, "    % ~s~n", [Str]);
    #icode_phi{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, hipe_icode:phi_dst(I)),
      io:format(Dev, " := phi(", []),
      pp_phi_args(Dev, hipe_icode:phi_arglist(I)),
      io:format(Dev, ")~n", []);
    #icode_move{} ->
      io:format(Dev, "    ", []),
      pp_arg(Dev, hipe_icode:move_dst(I)),
      io:format(Dev, " := ", []),
      pp_arg(Dev, hipe_icode:move_src(I)),
      io:format(Dev, "~n", []);
    #icode_call{} ->
      io:format(Dev, "    ", []),
      case hipe_icode:call_dstlist(I) of
	[] -> %% result is unused -- e.g. taken out by dead code elimination
	  io:format(Dev, "_ := ", []);
	DstList ->
	  pp_args(Dev, DstList),
	  io:format(Dev, " := ", [])
      end,
      pp_fun(Dev, hipe_icode:call_fun(I),
	     hipe_icode:call_args(I),
	     hipe_icode:call_type(I),
	     hipe_icode:call_in_guard(I)),
      case hipe_icode:call_continuation(I) of
	[] ->
	  ok;
	CC ->
	  io:format(Dev, " -> ~w", [CC])
      end,
      case hipe_icode:call_fail_label(I) of
	[] -> io:format(Dev, "~n", []);
	Fail -> io:format(Dev, ", #fail ~w~n", [Fail])
      end;
    #icode_enter{} ->
      io:format(Dev, "    ", []),
      pp_fun(Dev, hipe_icode:enter_fun(I),
	     hipe_icode:enter_args(I),
	     hipe_icode:enter_type(I)),
      io:format(Dev, "~n", []);
    #icode_return{} ->
      io:format(Dev, "    return(", []),
      pp_args(Dev, hipe_icode:return_vars(I)),
      io:format(Dev, ")~n", []);
    #icode_begin_try{} ->
      io:format(Dev, "    begin_try -> ~w cont ~w~n", 
		[hipe_icode:begin_try_label(I), 
		 hipe_icode:begin_try_successor(I)]);
    #icode_begin_handler{} ->
      io:format(Dev, "    ", []),
      pp_args(Dev, hipe_icode:begin_handler_dstlist(I)),
      io:format(Dev, " := begin_handler()~n",[]);
    #icode_end_try{} ->
      io:format(Dev, "    end_try~n", []);
    #icode_fail{} ->
      Type = hipe_icode:fail_class(I),
      io:format(Dev, "    fail(~w, [", [Type]),
      pp_args(Dev, hipe_icode:fail_args(I)),
      case hipe_icode:fail_label(I) of
	[] -> io:put_chars(Dev, "])\n");
	Fail -> io:format(Dev, "]) -> ~w\n", [Fail])
      end;
    #icode_if{} ->
      io:format(Dev, "    if ~w(", [hipe_icode:if_op(I)]),
      pp_args(Dev, hipe_icode:if_args(I)),
      io:format(Dev, ") then ~p (~.2f) else ~p~n", 
		[hipe_icode:if_true_label(I), hipe_icode:if_pred(I),
		 hipe_icode:if_false_label(I)]);
    #icode_switch_val{} ->
      io:format(Dev, "    switch_val ",[]),
      pp_arg(Dev, hipe_icode:switch_val_term(I)),
      pp_switch_cases(Dev, hipe_icode:switch_val_cases(I)),
      io:format(Dev, "    fail -> ~w\n", 
		[hipe_icode:switch_val_fail_label(I)]);
    #icode_switch_tuple_arity{} ->
      io:format(Dev, "    switch_tuple_arity ",[]),
      pp_arg(Dev, hipe_icode:switch_tuple_arity_term(I)),
      pp_switch_cases(Dev,hipe_icode:switch_tuple_arity_cases(I)),
      io:format(Dev, "    fail -> ~w\n", 
		[hipe_icode:switch_tuple_arity_fail_label(I)]);
    #icode_type{} ->
      io:format(Dev, "    if is_", []),
      pp_type(Dev, hipe_icode:type_test(I)),
      io:format(Dev, "(", []),
      pp_args(Dev, hipe_icode:type_args(I)),
      io:format(Dev, ") then ~p (~.2f) else ~p~n", 
		[hipe_icode:type_true_label(I), hipe_icode:type_pred(I), 
		 hipe_icode:type_false_label(I)]);
    #icode_goto{} ->
      io:format(Dev, "    goto ~p~n", [hipe_icode:goto_label(I)])
  end.

pp_fun(Dev, Fun, Args, Type) ->
  pp_fun(Dev, Fun, Args, Type, false).

pp_fun(Dev, Fun, Args, Type, Guard) ->
  case Type of
    primop ->
      hipe_icode_primops:pp(Dev, Fun);
    local ->
      {_,F,A} = Fun,
      io:format(Dev, "~w/~w", [F, A]);
    remote ->
      {M,F,A} = Fun,
      io:format(Dev, "~w:~w/~w", [M, F, A])
  end,
  io:format(Dev, "(", []),
  pp_args(Dev, Args),
  case Guard of
    true ->
      case Type of
	primop ->
	  io:format(Dev, ") (primop,guard)", []);
	_ ->
	  io:format(Dev, ") (guard)", [])
      end;
    false ->
      case Type of
	primop ->
	  io:format(Dev, ") (primop)", []);
	_ ->
	  io:format(Dev, ")", [])
      end
  end.

pp_arg(Dev, Arg) ->
  case hipe_icode:is_variable(Arg) of
    true ->
      case hipe_icode:is_var(Arg) of 
	true ->
	  N = hipe_icode:var_name(Arg),
	  io:format(Dev, "v~p", [N]);
	false ->
	  case hipe_icode:is_reg(Arg) of
	    true ->
	      N = hipe_icode:reg_name(Arg),
	      io:format(Dev, "r~p", [N]);
	    false ->
	      N = hipe_icode:fvar_name(Arg),
	      io:format(Dev, "fv~p", [N])
	  end
      end,
      case hipe_icode:is_annotated_variable(Arg) of
	true ->
	  {_,Val,Fun} = hipe_icode:variable_annotation(Arg),
	  io:format(Dev, " (~s)", [Fun(Val)]);
	false ->
	  ok
      end;
    false ->
      Const = hipe_icode:const_value(Arg), 
      io:format(Dev, "~p", [Const])  % ~p because it also prints ""
  end.

pp_args(_Dev, []) -> ok;
pp_args(Dev, [A]) ->
  pp_arg(Dev, A);
pp_args(Dev, [A|Args]) ->
  pp_arg(Dev, A),
  io:format(Dev, ", ", []),
  pp_args(Dev, Args).

pp_phi_args(_Dev, []) -> ok;
pp_phi_args(Dev, [{Pred,A}]) ->
  io:format(Dev, "{~w, ", [Pred]),
  pp_arg(Dev, A),
  io:format(Dev, "}", []);
pp_phi_args(Dev, [{Pred,A}|Args]) ->
  io:format(Dev, "{~w, ", [Pred]),
  pp_arg(Dev, A),
  io:format(Dev, "}, ", []),
  pp_phi_args(Dev, Args).

pp_type(Dev, T) ->
  io:format(Dev, "~w", [T]).

pp_switch_cases(Dev, Cases) ->
  io:format(Dev, " of\n",[]),
  pp_switch_cases(Dev, Cases,1),
  io:format(Dev, "",[]).

pp_switch_cases(Dev, [{Val,L}], _Pos) -> 
  io:format(Dev, "        ",[]),
  pp_arg(Dev, Val),
  io:format(Dev, " -> ~w\n", [L]);
pp_switch_cases(Dev, [{Val, L}|Ls], Pos) -> 
  io:format(Dev, "        ",[]),
  pp_arg(Dev, Val),
  io:format(Dev, " -> ~w;\n", [L]),
  NewPos = Pos,
  %%    case Pos of
  %%      5 -> io:format(Dev, "\n              ",[]),
  %%	   0;
  %%      N -> N + 1
  %%    end,
  pp_switch_cases(Dev, Ls, NewPos);
pp_switch_cases(_Dev, [], _) -> ok.

