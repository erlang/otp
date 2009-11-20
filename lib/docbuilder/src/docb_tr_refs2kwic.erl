%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_tr_refs2kwic).

-export([extension/0, transform/3, rule/2]).

%% Output parts of a parsetree that contains a series of reference
%% manual pages. The tags considered are: module, file, app, com and lib
%% (and their corresponding *summary tags), and name, fsummary, c, em,
%% ret and pcdata.

extension() ->
    ".kwc".

transform(File, Tree, Opts) -> 
    {refs, [], Trees} = Tree,
    FileTree = {srcfile, [], [{pcdata, [], File}]},
    AppName = docb_util:lookup_option(name, Opts, "unknown"),
    AppTree = {appname, [], [{pcdata, [], AppName}]},
    Vsn = docb_util:lookup_option(vsn, Opts, "unknown"),
    VsnTree = {appvsn, [], [{pcdata, [], Vsn}]},
    NewTree = {refs, [], [FileTree, AppTree, VsnTree| Trees]},
    {NewTree, Opts}.

rule([refs|_],_) ->
    {"%% Automatically generated. Do not edit.\n", ""};

rule([srcfile| _], _) ->
    {"{srcfile, \"", "\"}.\n"};

rule([appname| _], _) ->
    {"{appname, \"", "\"}.\n"};

rule([appvsn| _], _) ->
    {"{appvsn, \"", "\"}.\n"};

rule([erlref|_ ], _) ->
    {"", ""};

rule([fileref|_ ], _) ->
    {"", ""};

rule([appref|_ ], _) ->
    {"", ""};

rule([comref|_ ], _) ->
    {"", ""};

rule([cref|_ ], _) ->
    {"", ""};

rule([module| _], {_, [File], _}) ->
    {drop, "{module, \"" ++ File ++ "\"}.\n"};

rule([file|_], {_, [File], _}) ->
    {drop, "{file, \"" ++ File ++ "\"}.\n"};

rule([app|_], {_, [File], _}) ->
    {drop, "{app, \"" ++ File ++ "\"}.\n"};

rule([com|_], {_, [File], _}) ->
    {drop, "{com, \"" ++ File ++ "\"}.\n"};

rule([lib|_], {_, [File], _}) ->
    {drop, "{lib, \"" ++ File ++ "\"}.\n"};

rule([modulesummary|_], _) ->
    {"{modulesummary, \"", "\"}.\n"};

rule([filesummary|_], _) ->
    {"{filesummary, \"", "\"}.\n"};

rule([appsummary|_], _) ->
    {"{appsummary, \"", "\"}.\n"};

rule([comsummary|_], _) ->
    {"{comsummary, \"", "\"}.\n"};

rule([libsummary|_], _) ->
    {"{libsummary, \"", "\"}.\n"};

rule([funcs|_ ], _) ->
    {"", ""};

rule([func|_ ], _) ->
    {"", ""};

rule([name,func,funcs,cref|_], {_,[_File], [_Ret,{pcdata,[],Name}]}) ->
    FName = lists:flatten(docb_html_util:pcdata_to_html(Name)),
    TName = docb_util:trim(FName), 
    case catch docb_util:fknidx(TName, "/") of
	{'EXIT',_} ->
	    {drop, ["{name, \"", escq(TName), "\"}.\n"]};
	FuncName ->
	    {drop, ["{name, \"", escq(FuncName), "\"}.\n"]}
    end;

rule([name,func,funcs,erlref|_], {_,[_File], [{pcdata,[],Name}]}) ->
    FName = lists:flatten(docb_html_util:pcdata_to_html(Name)),
    TName = docb_util:trim(FName), 
    case catch docb_util:fknidx(TName, "/") of
	{'EXIT',_} ->
	    {drop, ["{name, \"", escq(TName), "\"}.\n"]};
	FuncName ->
	    {drop, ["{name, \"", escq(FuncName), "\"}.\n"]}
    end;

rule([name, func| _], {_, [_File], [{pcdata, [], Name}]}) ->
    FName = lists:flatten(docb_html_util:pcdata_to_html(Name)),
    TName = docb_util:trim(FName), 
    Cmd = case string:tokens(TName, " ") of
	      [Cmd0| _] ->
		  Cmd0;
	      _ ->
		  TName
	  end,
    {drop, ["{name, \"", escq(Cmd), "\"}.\n"]};

rule([fsummary| _], _) ->
    {"{fsummary, \"", "\"}.\n"}; 

rule([c, fsummary|_], _) ->
    {"", ""};

rule([em, fsummary|_], _) ->
    {"", ""};

rule([pcdata| _], {_, _, Data}) ->
    FData = lists:flatten(docb_html_util:pcdata_to_html(Data)),
    Out = lists:map(fun($\n) -> $ ; (C) -> C end, FData),
    {drop, escq(Out)};

rule(_, _) ->
  {drop, ""}.

escq(Cs) ->
    lists:flatmap(fun($") -> 
			  "\\\"";
		      (C) -> [C]
		  end,
		  Cs).
