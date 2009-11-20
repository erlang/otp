% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
%% Description:
%% Support module to the inviso tool.
%%
%% Authors:
%% Lennart Öhman, lennart.ohman@st.se
%% -----------------------------------------------------------------------------

-module(inviso_tool_lib).

%% -----------------------------------------------------------------------------
%% Exported library APIs
%% -----------------------------------------------------------------------------
-export([inviso_cmd/3,expand_module_names/3,make_patterns/7,std_tdg/2]).
-export([mk_tdg_args/2,mk_complete_tdg_args/2,get_datetime_from_tdg_args/1]).
-export([debug/3]).

%% -----------------------------------------------------------------------------
%% Constants.
%% -----------------------------------------------------------------------------
-define(DBG_OFF,off).                        % No internal debug indicator.


%% =============================================================================
%% Functions for inviso_cmd
%% =============================================================================

%% Help function which executes a trace control call. The reason for having a special
%% function is that we either want to do rpc if the trace control component is
%% located on another Erlang node than this one. Or call trace_c directly if
%% it actually is on this node.
%% Returns whatever the inviso function returns. In case of badrpc it is wrapped
%% in an error-tuple.
inviso_cmd(NodeName,Func,Args) ->
    case node() of
	NodeName ->                          % Control component on this node.
	    apply(inviso,Func,Args);
	_ ->                                 % On other node, must do RPC.
	    case rpc:call(NodeName,inviso,Func,Args) of
		{badrpc,Reason} ->
		    {error,{badrpc,Reason}};
		Result ->
		    Result
	    end
    end.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Functions for expand_module_names
%% =============================================================================

%% Help function which expands the module name depending on how it is expressed.
%% Setting Nodes to 'void' makes it non-distributed, expanding only here.
%% The following special cases are handled:
%% '_'  =All modules, no expansion into individual module names. Instead
%%       it is intended to use the '_' mechanism in the trace_pattern BIF.
%%       Can therefore not be combined with a directory regexp.
%% "*"  =Is translated into ".*".
%% "abc"=Means ".*abc.*". Can only be used for module or directory names
%%       containing upper or lowercase, digits and a slash.
%% Returns {multinode_expansion,NodeModules},
%%         {singlenode_expansion,Modules},
%%         'module', 'wildcard' or {error,Reason},
%% To limit the places where expansion is done, the option {expand_only_at,Node}
%% can be provided in the Opts list.
%% In the non-distributed case the singlenode_expansion will be returned.
expand_module_names(_Nodes,Mod={_,'_'},_) ->
    {error,{faulty_regexp_combination,Mod}};
expand_module_names(Nodes,{DirStr,ModStr},Opts) when list(DirStr), list(ModStr) ->
    case expand_module_names_special_regexp(DirStr) of
	{ok,NewDirStr} ->
	    case expand_module_names_special_regexp(ModStr) of
		{ok,NewModStr} ->
		    expand_module_names_2(Nodes,NewDirStr,NewModStr,Opts);
		{error,_Reason} ->
		    {error,{faulty_regexp,ModStr}}
	    end;
	{error,_Reason} ->
	    {error,{faulty_regexp,DirStr}}
    end;
expand_module_names(_,'_',_Opts) ->          % If we want to trace all modules
    wildcard;                                % we shall not expand it.
expand_module_names(_Nodes,Mod,_Opts) when atom(Mod) ->
    module;                                  % If it is an atom, no expansion.
expand_module_names(Nodes,"*",Opts) ->       % Treat this as a reg.exp.
    expand_module_names(Nodes,".*",Opts);
expand_module_names(Nodes,ModStr,Opts) when list(ModStr) ->
    case expand_module_names_special_regexp(ModStr) of
	{ok,NewModStr} ->
	    expand_module_names_2(Nodes,NewModStr,Opts);
	{error,_Reason} ->
	    {error,{faulty_regexp,ModStr}}
    end.

expand_module_names_2(void,ModStr,Opts) ->   % Non-distributed case.
    {singlenode_expansion,inviso_rt_lib:expand_regexp(ModStr,Opts)};
expand_module_names_2(Nodes,ModStr,Opts) ->
    case get_expand_regexp_at_opts(Opts) of
	{ok,Node} ->                         % Expansion only at this node.
	    case inviso_rt_lib:expand_regexp([Node],ModStr,Opts) of
		[{Node,Modules}] when list(Modules) ->
		    {singlenode_expansion,Modules};
		[{Node,_}] ->                % Most likely badrpc.
		    {error,{faulty_node,Node}}
	    end;
	false ->                             % Expand on all nodes.
	    Result=inviso_rt_lib:expand_regexp(Nodes,ModStr,Opts),
	    {multinode_expansion,Result}
    end.
expand_module_names_2(void,DirStr,ModStr,Opts) -> % Non-distributed case.
    {singlenode_expansion,inviso_rt_lib:expand_regexp(DirStr,ModStr,Opts)};
expand_module_names_2(Nodes,DirStr,ModStr,Opts) ->
    case get_expand_regexp_at_opts(Opts) of
	{ok,Node} ->                         % Expansion only at this node.
	    case inviso_rt_lib:expand_regexp([Node],DirStr,ModStr,Opts) of
		[{Node,Modules}] when list(Modules) ->
		    {singlenode_expansion,Modules};
		[{Node,_}] ->                % Most likely badrpc.
		    {error,{faulty_node,Node}}
	    end;
	false ->                             % Expand on all nodes.
	    Result=inviso_rt_lib:expand_regexp(Nodes,DirStr,ModStr,Opts),
	    {multinode_expansion,Result}
    end.

%% Help function which converts a special regexp into a proper one. With
%% special regexps we mean e.g:"abc" which is supposed to mean ".*abc.*".
%% Always returns a regexp or {error,Reason}. 
expand_module_names_special_regexp(Str) ->
    StrLen=length(Str),
    case regexp:first_match(Str,"[0-9a-zA-Z_/]*") of
	{match,1,StrLen} ->                  % Ok, it is the special case.
	    {ok,".*"++Str++".*"};            % Convert it to a proper regexp.
	{match,_,_} ->
	    {ok,Str};                        % Keep it and hope it is a regexp.
	nomatch ->
	    {ok,Str};                        % Keep it and hope it is a regexp.
	{error,Reason} ->                    % Can't continue with this!
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Functions for make_pattern.
%% =============================================================================

-define(DEPTH,3).                            % Max recursive safety catch depth.

%% Help function that creates trace-patterns for each module in the list.
%% It can handle both lists of modules or lists of nodes and modules.
%% It will also in the process apply safety catches, if such are not disabled,
%% in order to prevent certain patterns to form.
%% The safety catch function is supposed to return either 'ok' or {new,NewTracePattern}.
%% Where the NewTracePattern is a list of zero or more {M,F,Arity,MS}. The
%% NewTracePatter is then the replacement for the tried trace-pattern.
%% Note that the new trace-pattern(s) are also tried against all safety catches.
%% This can possibly result in even replacements of the replacements. There is
%% a depth meter to prevent the safety catch mechanism from circularly expanding
%% trace patterns for ever.
%% Returns a list of [{Node,PatternList},...] or [Pattern,...].
%% The latter is used when the modules have been expanded on a single node.
make_patterns(Catches,Opts,Dbg,NodeModsOrMods,F,A,MS) ->
    OwnArg=get_ownarg_opts(Opts),
    case get_disable_safety_opts(Opts) of
	true ->                              % Do not use the safety catches.
	    make_patterns_2(void,OwnArg,Dbg,NodeModsOrMods,F,A,MS);
	false ->
	    make_patterns_2(Catches,OwnArg,Dbg,NodeModsOrMods,F,A,MS)
    end.
	    
make_patterns_2(Catches,OwnArg,Dbg,[{Node,Mods}|Rest],F,A,MS) when list(Mods) ->
    TPs=make_patterns_3(Catches,OwnArg,Dbg,Mods,F,A,MS,[]),
    [{Node,join_patterns(TPs)}|make_patterns_2(Catches,OwnArg,Dbg,Rest,F,A,MS)];
make_patterns_2(Catches,OwnArg,Dbg,[{_Node,_}|Rest],F,A,MS) -> % badrpc!?
    make_patterns_2(Catches,OwnArg,Dbg,Rest,F,A,MS);
make_patterns_2(Catches,OwnArg,Dbg,Modules,F,A,MS) when list(Modules) ->
    TPs=make_patterns_3(Catches,OwnArg,Dbg,Modules,F,A,MS,[]),
    join_patterns(TPs);
make_patterns_2(_,_,_,[],_,_,_) ->
    [].

make_patterns_3(void,OwnArg,Dbg,[M|Rest],F,A,MS,Result) -> % S-catches not used!
    make_patterns_3(void,OwnArg,Dbg,Rest,F,A,MS,[{M,F,A,MS}|Result]);
make_patterns_3(Catches,OwnArg,Dbg,[M|Rest],F,A,MS,Result) ->
    NewTPs=try_safety_catches(Catches,OwnArg,[{M,F,A,MS}],Dbg,[],?DEPTH),
    make_patterns_3(Catches,OwnArg,Dbg,Rest,F,A,MS,[NewTPs|Result]);
make_patterns_3(_,_,_,[],_,_,_,Result) ->
    lists:flatten(Result).

try_safety_catches(_Catches,_OwnArg,TPs,Dbg,_Accum,0) -> % Max depth here!
    debug(max_catch_depth,Dbg,[TPs]),
    TPs;                                     % Just return them unchanged.
try_safety_catches(Catches,OwnArg,[TP={M,F,A,MS}|Rest],Dbg,Accum,Depth) ->
    case try_safety_catch(Catches,OwnArg,M,F,A,MS,Dbg) of
	ok ->                                % This pattern is safe!
	    try_safety_catches(Catches,OwnArg,Rest,Dbg,[TP|Accum],?DEPTH);
	{new,NewTPs} ->                      % Then we must try them too!
	    NewTPs2=try_safety_catches(Catches,OwnArg,NewTPs,Dbg,[],Depth-1),
	    try_safety_catches(Catches,OwnArg,Rest,Dbg,[NewTPs2|Accum],?DEPTH)
    end;
try_safety_catches(_,_,[],_,Accum,_) ->
    Accum.

try_safety_catch([{SafetyMod,SafetyFunc}|Rest],OwnArg,M,F,A,MS,Dbg) ->
    case (catch apply(SafetyMod,SafetyFunc,[M,F,A,MS,OwnArg])) of
	ok ->                                % This catch has no oppinion about it.
	    try_safety_catch(Rest,OwnArg,M,F,A,MS,Dbg); % Continue with the next.
	{new,NewTPs} ->                      % Replace it with this or these new.
	    debug(safety_catch,Dbg,[new,{SafetyMod,SafetyFunc},M,F,A,MS,NewTPs]),
	    {new,NewTPs};                    % and stop trying safety cathes.
	{'EXIT',Reason} ->                   % Something wrong with the safety catch.
	    debug(safety_catch,Dbg,['EXIT',{SafetyMod,SafetyFunc},M,F,A,MS,Reason]),
	    try_safety_catch(Rest,OwnArg,M,F,A,MS,Dbg) % Skip it and go on.
    end;
try_safety_catch([],_,_,_,_,_,_) ->
    ok.                                      % Since it passed all, it is safe!
%% -----------------------------------------------------------------------------

%% Help function that joins patterns together. This is necessary since you can
%% only set the pattern once for a module-function-arity. This function can not
%% remove conflicting match-spec "commands". Match-specs will simply be concatenated.
%% Returns a list of patterns where each mod-func-arity is unique.
join_patterns(Patterns) ->
    join_patterns_2(Patterns,[]).

join_patterns_2([{M,F,Arity,MS}|Rest],Result) ->
    case join_patterns_is_already_done(M,F,Arity,Result) of
	false ->                             % No we have not collapsed this one.
	    case join_patterns_3(M,F,Arity,Rest) of
		[] ->                        % No this combination is unique.
		    join_patterns_2(Rest,[{M,F,Arity,MS}|Result]);
		MSs ->                       % We got a list of all other TPs.
		    join_patterns_2(Rest,[{M,F,Arity,MS++MSs}|Result])
	    end;
	true ->                              % We already joined this M-F-Arity.
	    join_patterns_2(Rest,Result)     % Simply skip it, already done.
    end;
join_patterns_2([],Result) ->
    Result.                                  % Reversed but does not matter!

%% Help function checking if we have already built a trace-pattern for
%% this M-F-Arity. If so, the found M-F-Arity is already handled.
join_patterns_is_already_done(M,F,Arity,[{M,F,Arity,_}|_]) ->
    true;
join_patterns_is_already_done(M,F,Arity,[_|Rest]) ->
    join_patterns_is_already_done(M,F,Arity,Rest);
join_patterns_is_already_done(_,_,_,[]) ->
    false.

%% Help function which simply concatenates all match-specs for this
%% M-F-Arity.
join_patterns_3(M,F,Arity,[{M,F,Arity,MS}|Rest]) ->
    [MS|join_patterns_3(M,F,Arity,Rest)];
join_patterns_3(M,F,Arity,[_|Rest]) ->
    join_patterns_3(M,F,Arity,Rest);
join_patterns_3(_,_,_,[]) ->
    [].
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Function for tracer data creation.
%% =============================================================================

-define(I2L(Arg),integer_to_list(Arg)).

%% The inviso_tool uses a tracer-data generator function to create the tracer_data
%% specification for each node that shall participate in tracing controlled
%% through the inviso-tool. If no own tracer data generator function is specified,
%% this function is used.
std_tdg(Node,{{Y,Mo,D},{H,Mi,S}}) ->
    NameStr=atom_to_list(Node)++"_"++?I2L(Y)++"-"++?I2L(Mo)++"-"++?I2L(D)++"_"++
	?I2L(H)++"-"++?I2L(Mi)++"-"++?I2L(S),
    LogTD={file,NameStr++".log"},
    TiTD={file,NameStr++".ti"},
    [{trace,LogTD},{ti,TiTD}].
%% ------------------------------------------------------------------------------

%% mk_tdg_args(DateTime,Args)=TDGargs
%%   DateTime={Date,Time}
%%     Date=tuple(),
%%     Time=tuple(),
%%   Args=list()
%%   TDGargs=list(),
%% Creates the TDGargs list used when calling functions making the CompleteTDGargs.
mk_tdg_args(DateTime,Args) ->
    [DateTime|Args].
%% ------------------------------------------------------------------------------

%% mk_complete_tdg_args(Node,TDGargs)=CompleteTDGargs
%% Returns the list of all arguments a tracer data generator function must accept.
mk_complete_tdg_args(Node,TDGargs) ->
    [Node|TDGargs].
%% ------------------------------------------------------------------------------

%% get_datetime_from_tdg_args(TDGargs)=DateTime
%% Function returning the DateTime tuple in a TDGargs list.
get_datetime_from_tdg_args([DateTime|_]) ->
    DateTime.
%% ------------------------------------------------------------------------------


%% =============================================================================
%% Various help functions.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% Functions handling set trace-pattern options.
%% -----------------------------------------------------------------------------

%% Gets additional arguments given to various configurable functions.
%% Returns a list.
get_ownarg_opts(Opts) ->
    case lists:keysearch(arg,1,Opts) of
	{value,{_,OwnArg}} when list(OwnArg) ->
	    OwnArg;
	{value,{_,OwnArg}} ->
	    [OwnArg];
	false ->
	    []
    end.
%% -----------------------------------------------------------------------------

get_disable_safety_opts(Opts) ->
    case lists:member(disable_safety,Opts) of
	true ->
	    true;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_expand_regexp_at_opts(Opts) ->
    case lists:keysearch(expand_only_at,1,Opts) of
	{value,{_,Node}} when atom(Node) ->
	    {ok,Node};
	_ ->
	    false
    end.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Functions for the internal debugging system.
%% =============================================================================

%% The debug system is meant to provide tracing of inviso tool at different levels.
%%
%% debug(What,Level,Description) -> nothing significant.
%%   What : controls what kind of event. This can both be certain parts of the tool
%%          as well as certain levels (info to catastrophy).
%%   Level: Determines if What shall be printed or not.
%%   Description: this is what happend.
debug(_What,?DBG_OFF,_Description) ->
    true;                                    % Debug is off, no action.
debug(What,On,Description) ->
    debug_2(What,On,Description).

debug_2(What,_,Description) ->
    io:format("INVISO DEBUG:~w, ~p~n",[What,Description]).
%% -----------------------------------------------------------------------------
