%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% ------------------------------------------------------------------------------
%% File    : inviso_rt_lib.erl
%% Author  : Lennart Öhman <lennart@st.se>
%% Description : 
%%
%% Created : 27 Sep 2005 by Lennart Öhman <lennart@st.se>
%% ------------------------------------------------------------------------------
-module(inviso_rt_lib).

-export([expand_regexp/2,expand_regexp/3,expand_regexp/4]).
-export([is_tracerdata/1]).
-export([transform/2]).

-export([rpc/4,rpc/5,match_modules/2,match_modules/3]).
-export([debug/3]).

%% ------------------------------------------------------------------------------

%% ==============================================================================
%% Exported API functions.
%% ==============================================================================

%% ------------------------------------------------------------------------------
%% expand_regexp(Nodes,RegExpDir,RegExpMod,Opts) = [{Node,Answer},...] | {error,Reason}
%% expand_regexp(Nodes,RegExpMod,Opts) = [{Node,Answer},...] | {error,Reason}
%% expand_regexp(RegExpDir,RegExpMod,Opts) = ListOfModules | {error,Reason}
%% expand_regexp(RegExpMod,Opts) = ListOfModules | {error,Reason}
%%   Nodes=List of all nodes (atoms) where to expand.
%%   RegExpDir=Reg.exp (string) specifying directories.
%%   RegExpMod=Reg.exp (string) specifying module names.
%%   Node=node name (atom).
%%   Opts=[Opt,...]
%%   Opt=only_loaded
%%   Answer=List of modules (atoms) | 'badrpc'
%%
%% Expands, concurrently, the regular expression on Nodes and returns a list
%% of what modules it expanded to on the different nodes. Note that it may
%% differ between Erlang nodes depending on whether the modules are the same
%% or not. Also note that all modules becomes loaded as a result.
%% RegExpDir can further limit the modules. It introduces the requirement that
%% a module must be loaded from a directory with a path satisfying the RegExpDir.
%% All regular expression are according to the standard lib regexp module.
expand_regexp(RegExpMod,Opts) when is_list(RegExpMod),is_list(Opts) ->
    match_modules(RegExpMod,Opts);
expand_regexp(RegExpMod,Opts) ->
    {error,{badarg,[RegExpMod,Opts]}}.
expand_regexp(NodesOrRegExpDir,RegExpMod,Opts)
  when is_list(NodesOrRegExpDir),is_list(RegExpMod),is_list(Opts) ->
    case is_list_of_atoms(NodesOrRegExpDir) of
	true ->                              % Interpret as list of nodes.
	    lists:foreach(fun(N)->spawn(?MODULE,rpc,[self(),N,RegExpMod,Opts]) end,
			  NodesOrRegExpDir),
	    expand_regexp_answers(NodesOrRegExpDir,[]);
	false ->                             % Interpret as a string.
	    match_modules(NodesOrRegExpDir,RegExpMod,Opts)
    end;
expand_regexp(NodesOrRegExpDir,RegExpMod,Opts) ->
    {error,{badarg,[NodesOrRegExpDir,RegExpMod,Opts]}}.
expand_regexp(Nodes,RegExpDir,RegExpMod,Opts)
  when is_list(Nodes),is_list(RegExpDir),is_list(RegExpMod),is_list(Opts) ->
    lists:foreach(fun(N)->
			  spawn(?MODULE,rpc,[self(),N,RegExpDir,RegExpMod,Opts])
		  end,
		  Nodes),
    expand_regexp_answers(Nodes,[]);
expand_regexp(Nodes,RegExpDir,RegExpMod,Opts) ->
    {error,{badarg,[Nodes,RegExpDir,RegExpMod,Opts]}}.

expand_regexp_answers([],Answers) -> Answers;  % List of [{Node,Answer},...].
expand_regexp_answers(Nodes,Answers) ->
    receive
	{?MODULE,Node,Answer} ->
	    expand_regexp_answers(lists:delete(Node,Nodes),[{Node,Answer}|Answers])
    end.
%% ------------------------------------------------------------------------------

%% is_tracerdata(TracerData)=true|false
%% Answers the question if TracerData is proper tracerdata. Note that true can be
%% returned if it resembles tracerdata very closely.
is_tracerdata({Fun,_Data}) when is_function(Fun) -> true;
is_tracerdata({relayer,To}) when is_pid(To);is_atom(To) -> true;
is_tracerdata(collector) -> true;
is_tracerdata({file,Param}) when is_tuple(Param);is_list(Param) -> true;
is_tracerdata({ip,_Param}) -> true;
is_tracerdata([{trace,LogTD}|Rest]) ->
    case is_tracerdata(LogTD) of
	true ->
	    is_tracerdata(Rest);
	false ->
	    false
    end;
is_tracerdata([{ti,TiData}|Rest]) ->
    case is_tidata(TiData) of
	true ->
	    is_tracerdata(Rest);
	false ->
	    false
    end;
is_tracerdata([]) ->
    true;
is_tracerdata(_) ->
    false.

is_tidata({file,FileName}) when is_list(FileName) -> true; 
is_tidata({file,FileName,{M,F,Args}}) when is_list(FileName),is_atom(M),is_atom(F),is_list(Args) ->
    true;
is_tidata(_) -> false.
%% ------------------------------------------------------------------------------		


%% ==============================================================================
%% Help functions.
%% ==============================================================================

%% Help function intended to be run in its own process. Will report with
%% a message when done.
%% This function will be spawned on.
rpc(Parent,Node,RegExpMod,Opts) ->			   
    case rpc:call(Node,?MODULE,match_modules,[RegExpMod,Opts]) of
	{badrpc,_Reason} ->                  % The node is probably not healthy.
	    Parent ! {?MODULE,Node,badrpc};
	Modules ->
	    Parent ! {?MODULE,Node,Modules}
    end.

rpc(Parent,Node,RegExpDir,RegExpMod,Opts) ->			   
    case rpc:call(Node,?MODULE,match_modules,[RegExpDir,RegExpMod,Opts]) of
	{badrpc,_Reason} ->                  % The node is probably not healthy.
	    Parent ! {?MODULE,Node,badrpc};
	Modules ->
	    Parent ! {?MODULE,Node,Modules}
    end.
%% ------------------------------------------------------------------------------


%% ==============================================================================
%% Exported function which actually shall be in code.erl.
%% ==============================================================================

%% match_modules(RegExpMod,Actions) = [Module,...] | {error,Reason}
%% match_modules(RegExpDir,RegExpMod,Actions)=[Module,...] | {error,Reason}
%%   RegExpMod=Erlang regular expression describing module names (string).
%%   RegExpDir=Erlang regular expression describing directory paths(string) |
%%             void
%%   Actions=List of;'only_loaded'.
%%
%% Function which matches a regular expresion against module names. The function
%% can also match the directory from where the module is loaded or will be loaded
%% against a regular expresion for directory paths.
%% The function uses the same strategy as code-loading if the same module is
%% discovered in several places.
%% (1) An already loaded module shadows all other occurancies.
%% (2) .beams found in by a path shadows .beams found by paths later in the
%%   code paths.
%%
%% Description of actions:
%% only_loaded: Only consider modules which are loaded.
match_modules(RegExpMod,Actions) ->
    match_modules(void,RegExpMod,Actions).
match_modules(RegExpDir,RegExpMod,Actions) ->
    AllLoaded=code:all_loaded(),
    Mods1=handle_expand_regexp_2(AllLoaded,RegExpDir,RegExpMod,[]),
    case lists:member(only_loaded,Actions) of % Shall we do not loaded too?
	false ->                             % Ok, search all paths too then.
	    Paths=code:get_path(),
	    handle_expand_regexp_3(Paths,RegExpDir,RegExpMod,AllLoaded,Mods1);
	true ->                              % Only loaded modules then.
	    Mods1
    end.


%% Help function which traverses all loaded modules and determines
%% which shall be returned. First we check that the module satisfies the
%% module-regexp. Then we, if a dir reg-exp is given, checks that the
%% module is loaded from an approved path. Note that if it can not be
%% determined from where it was loaded (like preloaded or cover-compiled
%% etc), but dir reg-exps are used. That module will be excluded.
%% Returns a list of modules.
handle_expand_regexp_2([{Mod,Path}|Rest],RegExpDir,RegExpMod,Result) ->
    ModStr=atom_to_list(Mod),
    ModLen=length(ModStr),
    case regexp:first_match(ModStr,RegExpMod) of
	{match,1,ModLen} ->                  % Ok, The regexp matches the module.
	    if
		is_list(RegExpDir),is_atom(Path) -> % Preloaded or covercompiled...
		    handle_expand_regexp_2(Rest,RegExpDir,RegExpMod,Result);
		is_list(RegExpDir),is_list(Path) -> % Dir reg-exp is used!
		    PathOnly=filename:dirname(Path), % Must remove beam-file name.
		    case regexp:first_match(PathOnly,RegExpDir) of
			{match,_,_} ->       % Did find a match, that is enough!
			    handle_expand_regexp_2(Rest,RegExpDir,RegExpMod,[Mod|Result]);
			_ ->                 % Either error or nomatch.
			    handle_expand_regexp_2(Rest,RegExpDir,RegExpMod,Result)
		    end;
		true ->                      % Otherwise already done!
		    handle_expand_regexp_2(Rest,RegExpDir,RegExpMod,[Mod|Result])
	    end;
	_ ->                                 % Then Mod is not part of the set.
	    handle_expand_regexp_2(Rest,RegExpDir,RegExpMod,Result)
    end;
handle_expand_regexp_2([],_,_,Result) -> Result.

%% Help function which traverses all paths and looks for modules satisfying
%% the module reg.exp.
%% Returns a list of unique module names.
handle_expand_regexp_3([Path|Rest],RegExpDir,RegExpMod,AllLoaded,Result) ->
    if
	is_list(RegExpDir) ->                   % We must consider the directory name.
	    AbsPath=
		case filename:pathtype(Path) of
		    absolute ->              % Is already abs.
			Path;
		    relative ->              % Then it must be made absolute.
			filename:absname(Path);
		    volumerelative ->        % Only on Windows!?
			filename:absname(Path)
		end,
	    case regexp:first_match(AbsPath,RegExpDir) of
		{match,_,_} ->               % Ok, the directory is allowed.
		    NewResult=handle_expand_regexp_3_1(Path,RegExpMod,AllLoaded,Result),
		    handle_expand_regexp_3(Rest,RegExpDir,RegExpMod,AllLoaded,NewResult);
		_ ->                         % This directory does not qualify.
		    handle_expand_regexp_3(Rest,RegExpDir,RegExpMod,AllLoaded,Result)
	    end;
	true ->                              % RegExpDir is not used!
	    NewResult=handle_expand_regexp_3_1(Path,RegExpMod,AllLoaded,Result),
	    handle_expand_regexp_3(Rest,RegExpDir,RegExpMod,AllLoaded,NewResult)
    end;
handle_expand_regexp_3([],_,_,_,Result) -> Result.

handle_expand_regexp_3_1(Path,RegExpMod,AllLoaded,Result) ->
    case file:list_dir(Path) of
	{ok,FileNames} ->
	    handle_expand_regexp_3_2(FileNames,RegExpMod,AllLoaded,Result);
	{error,_Reason} ->                   % Bad path!? Skip it.
	    Result
    end.

handle_expand_regexp_3_2([File|Rest],RegExpMod,AllLoaded,Result) ->
    case filename:extension(File) of
	".beam" ->                           % It is a beam-file. Consider it!
	    ModStr=filename:basename(File,".beam"),
	    Mod=list_to_atom(ModStr),
	    case {lists:keysearch(Mod,1,AllLoaded),lists:member(Mod,Result)} of
		{false,false} ->             % This module is not tried before.
		    ModLen=length(ModStr),
		    case regexp:first_match(ModStr,RegExpMod) of
			{match,1,ModLen} ->  % This module satisfies the regexp.
			    handle_expand_regexp_3_2(Rest,RegExpMod,AllLoaded,[Mod|Result]);
			_ ->                 % Error or not perfect match.
			    handle_expand_regexp_3_2(Rest,RegExpMod,AllLoaded,Result)
		    end;
		{_,_} ->                     % This module is already tested.
		    handle_expand_regexp_3_2(Rest,RegExpMod,AllLoaded,Result)
	    end;
	_ ->                                 % Not a beam-file, skip it.
	    handle_expand_regexp_3_2(Rest,RegExpMod,AllLoaded,Result)
    end;
handle_expand_regexp_3_2([],_,_,Result) -> Result.
%% ------------------------------------------------------------------------------

%% Help function which finds out if its argument is a list of zero or more
%% atoms.
%% Returns 'true' or 'false'.
is_list_of_atoms([A|Rest]) when is_atom(A) ->
    is_list_of_atoms(Rest);
is_list_of_atoms([_|_]) ->
    false;
is_list_of_atoms([]) ->
    true.
%% ------------------------------------------------------------------------------


%% =============================================================================
%% Functions transforming function calls in trace-case file.
%% =============================================================================

%% transform(Exprs,Translations)=NewExprs
%%   Exprs=list(); List of abstract format erlang terms, as returned by
%%     io:parse_erl_exprs/2.
%%   Translations=list(); List of translations from function calls to other
%%     function calls. [{Mod,Func,Arity,{NewMod,NewFunc,ParamTransformMF}},...]
%%     Mod can actually be omitted, ParamTransformMF shall be {M,F} where F is
%%     a function taking one argument (the parameter list), and returning the
%%     new parameter list. It can also be anything else should no transformation
%%     of the parameters be the case.
%%
%% Function that transforms function calls in a trace-case file. The transform/2
%% can only transform shallow function calls. I.e where both module and function
%% name are specified as atoms. Any binding-environment is not examined.
transform([Expr|Rest],Translations) ->
    [transform_2(Expr,Translations)|transform(Rest,Translations)];
transform([],_) ->
    [].

%% Help function handling a single expr.
transform_2({call,L1,{remote,L2,ModExpr,FuncExpr},Params},Translations) ->
    case transform_2(ModExpr,Translations) of
	{atom,L3,M} ->
	    case transform_2(FuncExpr,Translations) of
		{atom,L4,F} ->               % Now we have a M:F/Arity!
		    case do_call_translation(M,F,Params,Translations) of
			{ok,NewM,NewF,NewP} ->
			    NewParams=transform(NewP,Translations),
			    {call,L1,{remote,L2,{atom,L3,NewM},{atom,L4,NewF}},NewParams};
			false ->             % No translation or faulty.
			    NewParams=transform(Params,Translations),
			    {call,L1,{remote,L2,ModExpr,FuncExpr},NewParams}
		    end;
		NewFuncExpr ->               % Not translated to a shallow term.
		    NewParams=transform(Params,Translations),
		    {call,L1,{remote,L2,ModExpr,NewFuncExpr},NewParams}
	    end;
	NewModExpr ->                        % Not translated to a shallow term.
	    NewFuncExpr=transform_2(FuncExpr,Translations),
	    NewParams=transform(Params,Translations),
	    {call,L1,{remote,L2,NewModExpr,NewFuncExpr},NewParams}
    end;
transform_2({call,L1,FuncExpr,Params},Translations) ->
    case transform_2(FuncExpr,Translations) of
	{atom,L3,F} ->                       % Now we have a M:F/Arity!
	    case do_call_translation(F,Params,Translations) of
		{ok,NewM,NewF,NewP} ->       % It is turned into a global call.
		    NewParams=transform(NewP,Translations),
		    {call,L1,{remote,L1,{atom,L3,NewM},{atom,L3,NewF}},NewParams};
		false ->                     % No translation or faulty.
		    NewParams=transform(Params,Translations),
		    {call,L1,FuncExpr,NewParams}
	    end;
	NewFuncExpr ->                       % Not translated to a shallow term.
	    NewParams=transform(Params,Translations),
	    {call,L1,NewFuncExpr,NewParams}
    end;
transform_2({match,L,P,E},Translations) ->
    NewPattern=transform_2(P,Translations),
    NewExpr=transform_2(E,Translations),
    {match,L,NewPattern,NewExpr};
transform_2({op,L,Op,Arg1,Arg2},Translations) ->
    NewArg1=transform_2(Arg1,Translations),
    NewArg2=transform_2(Arg2,Translations),
    {op,L,Op,NewArg1,NewArg2};
transform_2({op,L,Op,Arg},Translations) ->
    NewArg=transform_2(Arg,Translations),
    {op,L,Op,NewArg};
transform_2({block,L,Body},Translations) ->
    NewBody=transform(Body,Translations),
    {block,L,NewBody};
transform_2({'if',L,Clauses},Translations) ->
    NewClauses=transform_clauses(Clauses,Translations),
    {'if',L,NewClauses};
transform_2({'case',L,Func,Clauses},Translations) ->
    NewFunc=transform_2(Func,Translations),
    NewClauses=transform_clauses(Clauses,Translations),
    {'case',L,NewFunc,NewClauses};
transform_2({'fun',L,{clauses,Clauses}},Translations) ->
    NewClauses=transform_clauses(Clauses,Translations),
    {'fun',L,NewClauses};
transform_2({lc,L,Items,GeneratorsFilters},Translations) ->
    NewItem=transform_2(Items,Translations),
    NewGensAndFilters=transform_gensandfilters(GeneratorsFilters,Translations),
    {lc,L,NewItem,NewGensAndFilters};
transform_2({'catch',L,Expr},Translations) ->
    NewExpr=transform_2(Expr,Translations),
    {'catch',L,NewExpr};
transform_2({tuple,L,Elements},Translations) ->
    NewElements=transform(Elements,Translations),
    {tuple,L,NewElements};
transform_2({cons,L,Element,Tail},Translations) ->
    NewElement=transform_2(Element,Translations),
    NewTail=transform_2(Tail,Translations),
    {cons,L,NewElement,NewTail};
transform_2({nil,L},_) ->
    {nil,L};
transform_2({bin,L,Elements},Translations) ->
    NewElements=transform_binary(Elements,Translations),
    {bin,L,NewElements};
transform_2(Expr,_) ->                       % Can be a var for instance.
    Expr.

transform_binary([{bin_element,L,Val,Size,TSL}|Rest],Translations) ->
    NewVal=transform_2(Val,Translations),
    NewSize=transform_2(Size,Translations),
    [{bin_element,L,NewVal,NewSize,TSL}|transform_binary(Rest,Translations)];
transform_binary([],_) ->
    [].

transform_clauses([{clause,L,Pattern,Guards,Body}|Rest],Translations) ->
    NewPattern=transform(Pattern,Translations),
    NewBody=transform(Body,Translations),
    [{clause,L,NewPattern,Guards,NewBody}|transform_clauses(Rest,Translations)];
transform_clauses([],_Translations) ->
    [].

transform_gensandfilters([{generator,L,Pattern,Exprs}|Rest],Translations) ->
    NewExprs=transform(Exprs,Translations),
    [{generator,L,Pattern,NewExprs}|transform_gensandfilters(Rest,Translations)];
transform_gensandfilters([Expr|Rest],Translations) ->
    [transform_2(Expr,Translations)|transform_gensandfilters(Rest,Translations)];
transform_gensandfilters([],_) ->
    [].
%% ------------------------------------------------------------------------------

%% This is the heart of the translation functionality. Here we actually try to
%% replace calls to certain functions with other calls. This can include removing
%% arguments.
do_call_translation(M,F,Params,Translations) ->
    case lists:keysearch({M,F,length(Params)},1,Translations) of
	{value,{_,{NewM,NewF,ArgFun}}} ->    % Lets transform the function.
	    do_call_translation_2(Params,NewM,NewF,ArgFun);
	_ ->
	    false                            % No translations at all.
    end.
do_call_translation(F,Params,Translations) ->
    case lists:keysearch({F,length(Params)},1,Translations) of
	{value,{_,{NewM,NewF,ArgFun}}} ->    % Lets transform the function.
	    do_call_translation_2(Params,NewM,NewF,ArgFun);
	_ ->
	    false                            % No translations at all.
    end.

do_call_translation_2(Params,NewM,NewF,ArgFun) ->
    case ArgFun of
	{M,F} when is_atom(M),is_atom(F) ->
	    case catch M:F(Params) of
		{'EXIT',_Reason} ->
		    false;                   % If it does not work, skipp it.
		MungedParams when is_list(MungedParams) ->
		    {ok,NewM,NewF,MungedParams};
		_ ->
		    false
	    end;
	_ ->                                 % No munging of parameters.
	    {ok,NewM,NewF,Params}
    end.
%% ------------------------------------------------------------------------------


%% =============================================================================
%% Functions for the runtime component internal debugging system.
%% =============================================================================

%% The debug system is meant to provide tracing of ttb at different levels.
%%
%% debug(What,Level,Description) -> nothing significant.
%%   What : controls what kind of event. This can both be certain parts of ttb
%%          as well as certain levels (info to catastrophy).
%%   Level: Determines if What shall be printed or not.
%%   Description: this is what happend.
debug(off,_What,_Description) ->
    true;                                    % Debug is off, no action.
debug(On,What,Description) ->
    debug_2(On,What,Description).

debug_2(_,What,Description) ->
    io:format("INVISO DEBUG:~w, ~p~n",[What,Description]).
%% -----------------------------------------------------------------------------
