%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Module   :	hipe_rtl_varmap
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@it.uu.se): Created.
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_varmap).

-export([init/1,
	 ivs2rvs/2,
	 icode_var2rtl_var/2,
	 icode_label2rtl_label/2]).

%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("../icode/hipe_icode.hrl").

%-------------------------------------------------------------------------

%% @spec init(IcodeRecord::#icode{}) -> {Args, VarMap}
%%
%% @doc Initializes gensym for RTL.

-spec init(#icode{}) -> {[_], _}.  % XXX: fix me please

init(IcodeRecord) ->
  hipe_gensym:init(rtl),
  hipe_gensym:set_var(rtl, hipe_rtl_arch:first_virtual_reg()),
  hipe_gensym:set_label(rtl, 0),
  VarMap = new_var_map(),
  {_Args, _VarMap1} = ivs2rvs(hipe_icode:icode_params(IcodeRecord), VarMap).


%%------------------------------------------------------------------------
%%
%% Mapping of labels and variables from Icode to RTL.
%%
%%------------------------------------------------------------------------


%% @spec icode_label2rtl_label(Icode_Label::term(), LabelMap::term()) ->
%%           {RTL_Label, NewLabelMap}
%%
%% @doc Converts an Icode label to an RTL label.

icode_label2rtl_label(LabelName, Map) ->
  case lookup(LabelName, Map) of
    {value, NewLabel} ->
      {NewLabel, Map};
    none ->
      NewLabel = hipe_rtl:mk_new_label(),
      {NewLabel, insert(LabelName, NewLabel, Map)}
  end.


%% @spec ivs2rvs(Icode_Vars::[term()], VarMap::term()) -> {[RTL_Var],NewVarMap}
%%
%% @doc Converts a list of Icode variables to a list of RTL variables.

ivs2rvs([], VarMap) ->
  {[], VarMap};
ivs2rvs([V|Vs], VarMap) ->
  {NewV, VarMap0} = icode_var2rtl_var(V, VarMap),
  {NewVs, VarMap1} = ivs2rvs(Vs, VarMap0),
  {[NewV|NewVs], VarMap1}.


%% @spec icode_var2rtl_var(Icode_Var::term(), VarMap::term()) ->
%%           {RTL_Var, NewVarMap}
%%
%% @doc Converts an Icode variable to an RTL variable.

icode_var2rtl_var(Var, Map) ->
  Value = lookup(Var, Map),
  case Value of
    none ->
      case type_of_var(Var) of
	fvar ->
	  NewVar = hipe_rtl:mk_new_fpreg(),
	  {NewVar, insert(Var, NewVar, Map)};
	var ->
	  NewVar = hipe_rtl:mk_new_var(),
	  {NewVar, insert(Var, NewVar, Map)};
	{reg, IsGcSafe} ->
	  NewVar =
	    case IsGcSafe of
              true -> hipe_rtl:mk_new_reg_gcsafe();
	      false -> hipe_rtl:mk_new_reg()
	    end,
	  {NewVar, insert(Var, NewVar, Map)}
      end;
    {value, NewVar} ->
      {NewVar, Map}
  end.

%%
%% Simple type test
%%

type_of_var(X) ->
  case hipe_icode:is_fvar(X) of
    true ->
      fvar;
    false ->
      case hipe_icode:is_var(X) of
	true ->
	  var;
	false ->
	  case hipe_icode:is_reg(X) of
	    true ->
	      {reg, hipe_icode:reg_is_gcsafe(X)};
	    false ->
	      %% Sanity check
	      case hipe_icode:is_const(X) of
		true -> const;
		false ->
		  exit({"Unknown Icode variable", X})
	      end
	  end
      end
  end.

%%
%% Helping utilities
%% 

new_var_map() ->
  gb_trees:empty().

lookup(V, Map) ->
  gb_trees:lookup(V, Map).

insert(Key, Val, Map) ->
  gb_trees:insert(Key, Val, Map).
