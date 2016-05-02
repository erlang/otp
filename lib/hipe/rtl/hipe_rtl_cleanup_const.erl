%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% 
%%% %CopyrightEnd%
%%%
%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_cleanup_const.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2004 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------

%% Big constants (floats, bignums) can be used as arguments to
%% arbitrary instructions in RTL. Since these are located in the
%% constants area and the only instruction that currently can access
%% them is load_address, the constants have to be moved out of the
%% instruction and loaded into temporary variables before the
%% instruction.
%%
%% Some backends can make use of the information that the arguments
%% are really constants. Here is the place to add new backend-specific
%% behaviour depending on this.

%%--------------------------------------------------------------------

-module(hipe_rtl_cleanup_const).

-export([cleanup/1]).

-include("hipe_rtl.hrl").

%%--------------------------------------------------------------------

%%-spec cleanup(#rtl{}) -> #rtl{}.

cleanup(Rtl) ->
  Code = cleanup(hipe_rtl:rtl_code(Rtl), []),
  hipe_rtl:rtl_code_update(Rtl, Code).

cleanup([I|Left], Acc) ->
  Args = hipe_rtl:args(I),
  case [X || X <- Args, hipe_rtl:is_const_label(X)] of
    [] ->
      cleanup(Left, [I|Acc]);
    ConstArgs ->
      NewIns = cleanup_instr(ConstArgs, I),
      cleanup(Left, NewIns ++ Acc)
  end;
cleanup([], Acc) ->
  lists:reverse(Acc).

cleanup_instr(Consts, I) ->
  cleanup_instr(ordsets:from_list(Consts), I, []).

cleanup_instr([Const|Left], I, Acc) ->
  Dst = hipe_rtl:mk_new_var(),
  ConstLabel = hipe_rtl:const_label_label(Const),
  Load = hipe_rtl:mk_load_address(Dst, ConstLabel, constant),
  case I of
    X when is_record(X, fp_unop) orelse is_record(X, fp) ->
      Fdst = hipe_rtl:mk_new_fpreg(),
      Fconv = hipe_tagscheme:unsafe_untag_float(Fdst, Dst),		  
      NewI = hipe_rtl:subst_uses([{Const, Fdst}], I),
      cleanup_instr(Left, NewI, Fconv ++ [Load|Acc]);
    _ ->
      NewI = hipe_rtl:subst_uses([{Const, Dst}], I),
      cleanup_instr(Left, NewI, [Load|Acc])
  end;
cleanup_instr([], I, Acc) ->
  [I|Acc].
