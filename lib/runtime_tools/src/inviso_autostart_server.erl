%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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
%% Author: Lennart Öhman, lennart.ohman@st.se
%%
-module(inviso_autostart_server).
-export([init/1]).

%% -----------------------------------------------------------------------------
%% Internal exports
%% -----------------------------------------------------------------------------
-export([cmd_file_interpreter_init/4]).
%% -----------------------------------------------------------------------------


%% This module provides a (well working) example of how to program an
%% autostart server responsible for initializing trace, setting patterns
%% and flags.
%%
%% The general idea is that this code spawns interpreter processes in order to
%% execute commands concurrently. Each of the interpreter processes opens one or
%% several files (in sequence) containing erlang function calls which are evaluated
%% in the interpreter process context.
%% The argument provided to init shall be a list of options controlling
%% how to initialize tracing, which file(s) to open and variable bindings.
%%
%% This autostart_server interpreters understands standard inviso trace case files.
%%
%% The runtime component provides an API very similar to the API provided
%% by the control component. It is therefore easy to translate inviso calls to
%% inviso_rt calls.
%%
%% This process may be killed by the inviso_rt process if stop_tracing is called.
%% The reason is that there is no time limit to the interpreter processes. Hence
%% they should be killed if tracing is not possible anylonger.
%% =============================================================================


%% -----------------------------------------------------------------------------

%% The independent autostart process spawned by the runtime component to carry
%% out initializations is spawened on this function (if using the example
%% autostart which comes with inviso).
%% ArgsFromConfig is as can be heard from the name comming from a paramater in
%% the autostart configuration file. Here it is supposed to be:
%%   ArgsFromConfig=[ServerParam,...]
%%     ServerParam={tracerdata,TracerData}|{cmdfiles,Files}|{bindings,Bindings}|
%%       {translations,Translations}|{debug,DbgLevel}
%%       TracerData=tracerdata given to inviso_rt:init_tracing/1 function.
%%       Files=[FileNameSpecs,...] where each FileNameSpecs will be executed in
%%         a separate process. Making each FileNameSpec parallel.
%%         FileNameSpecs=[FileNameSpec,...]
%%           FileNameSpec=FileName | {FileName,Bindings}
%%       Bindings=[{Var,Value},...] variable environment understood by
%%         erl_eval:exprs/2.
%%       Translations=[Translation,...]
%%       A translation file is a text-file with following tuples
%%         Translation={{Mod,Func,Arity,{Mod2,Func2,ParamMF}}}|
%%                     {{Func,Arity,{Mod2,Func2,ParamMF}}}
%%           ParamMF={M,F} | any()
%%           Translates Mod:Func/Arity to Mod2:Func2 with the arguments to
%%             Mod:Func translated using M:F/1. Note that ParamMF is not
%%             necessarily an MF. If no translation shall be done, ParamMF
%%             shall be anything else but an MF.
%%           Also note that Mod is optional in a Translation. That means that
%%           function calls without a module in the trace case file will
%%           be translated according to that translation.
init(ArgsFromConfig) ->
    case get_tracerdata_opts(ArgsFromConfig) of
	{ok,TracerData} ->                  % Otherwise we can not start a trace!
	    case inviso_rt:init_tracing(TracerData) of
		{ok,_} ->                   % Ok, tracing has been initiated.
		    case get_cmdfiles_opts(ArgsFromConfig) of
			{ok,CmdFiles} ->    % List of cmd-files.
			    Bindings=get_initialbindings_opts(ArgsFromConfig),
			    Translations=get_translations_opts(ArgsFromConfig),
			    Dbg=get_dbg_opts(ArgsFromConfig),
			    Procs=start_cmd_file_interpreters(CmdFiles,
							      Bindings,
							      Translations,
							      Dbg),
			    loop(Procs,Dbg); % Wait for procs to be done.
			false ->            % Then we can terminate normally.
			    true
		    end;
		{error,Reason} ->           % This is fault, lets terminate abnormally.
		    exit({inviso,{error,Reason}})
	    end;
	false ->                            % Then there is not much use then.
	    true                            % Just terminate normally.
    end.
%% -----------------------------------------------------------------------------

%% Help function which starts a process for each item found in the FileNames
%% list. The idea is that each item will be processed concurrently. The items
%% them selves may be a sequence of filenames.
%% Returns a list of spawned interpret processes.
start_cmd_file_interpreters([FileNames|Rest],Bindings,Translations,Dbg) ->
    P=spawn_link(?MODULE,cmd_file_interpreter_init,[FileNames,Bindings,Translations,Dbg]),
    MRef=erlang:monitor(process,P),         % Can't trap exits in this process.
    [{P,MRef}|start_cmd_file_interpreters(Rest,Bindings,Translations,Dbg)];
start_cmd_file_interpreters([],_,_,_) ->
    [].
%% -----------------------------------------------------------------------------


%% The loop where this process simply waits for all of the interpreters to be
%% done. Note that that may take som time. An interpreter may take as long time
%% necessary to do its task.
loop(Procs,Dbg) ->
    receive
	{'DOWN',MRef,process,Pid,_Reason} ->
	    case lists:keysearch(MRef,1,Procs) of
		{value,{Pid,_}} ->          % It was an interpreter that terminated.
		    case lists:keydelete(MRef,1,Procs) of
			[] ->               % No more interpreters.
			    true;           % Then terminate.
			NewProcs ->
			    loop(NewProcs,Dbg)
		    end;
		false ->
		    loop(Procs,Dbg)
	    end;
	_ ->
	    loop(Procs,Dbg)
    end.


%% -----------------------------------------------------------------------------
%% The interpret process.
%%
%% An interpreter process executes trace case files. Several interpreter processes
%% may be running in parallel. It is not within the scoop of this implementation
%% of an autostart server to solve conflicts. (You may implement your own autostart
%% server!).
%% An interpret process may run for as long as necessary. Hence the function called
%% within the trace case file can contain wait functions, waiting for a certain
%% system state to occure before continuing.
%% Note that this process also mixes global and local bindings. GlobalBindings
%% is a binding() structure, where LocalBindings is a list of {Var,Value}.
%% Further it is possible to let FileName be a {inviso,Func,Args} tuple instead.
%% -----------------------------------------------------------------------------

%% Init function for an interpreter process instance.
cmd_file_interpreter_init(FileNames,GlobalBindings,Translations,Dbg) ->
    interpret_cmd_files(FileNames,GlobalBindings,Translations,Dbg).

interpret_cmd_files([{FileName,LocalBindings}|Rest],GlobalBindings,Translations,Dbg) ->
    Bindings=join_local_and_global_vars(LocalBindings,GlobalBindings),
    interpret_cmd_files_1(FileName,Bindings,Translations,Dbg),
    interpret_cmd_files(Rest,GlobalBindings,Translations,Dbg);
interpret_cmd_files([FileName|Rest],GlobalBindings,Translations,Dbg) ->
    interpret_cmd_files_1(FileName,GlobalBindings,Translations,Dbg),
    interpret_cmd_files(Rest,GlobalBindings,Translations,Dbg);
interpret_cmd_files([],_,_,_) ->            % Done, return nothing significant!
    true.

%% This is "inline" inviso calls.
interpret_cmd_files_1({inviso,F,Args},Bindings,Translations,Dbg) ->
    {ok,Tokens1,_}=erl_scan:string("inviso:"++atom_to_list(F)++"("),
    Tokens2=tokenize_args(Args),
    {ok,Tokens3,_}=erl_scan:string(")."),
    case erl_parse:parse_exprs(Tokens1++Tokens2++Tokens3) of
	{ok,Exprs} ->
	    interpret_cmd_files_3(Bindings,Exprs,Translations,Dbg);
	{error,_Reason} ->
	    error
    end;
interpret_cmd_files_1({Mod,Func,Args},_Bindings,_Translations,_Dbg) ->
    catch apply(Mod,Func,Args);
%% This is the case when it actually is a trace case file.
interpret_cmd_files_1(FileName,Bindings,Translations,Dbg) ->
    case file:open(FileName,[read]) of
	{ok,FD} ->
	    interpret_cmd_files_2(FD,Bindings,io:parse_erl_exprs(FD,""),Translations,Dbg),
	    file:close(FD);
	{error,Reason} ->                   % Something wrong with the file.
	    inviso_rt_lib:debug(Dbg,interpret_cmd_files,[FileName,{error,Reason}])
    end.

%% Help function which handles Exprs returned from io:parse_erl_exprs and
%% tries to eval them. It is the side-effects we are interested in, like
%% setting flags and patterns. Note that we will get a failure should there
%% be a variable conflict.
%% Also note that there is logic to translate control component API calls to
%% corresponding runtime component calls.
%% Returns nothing significant.
interpret_cmd_files_2(FD,Bindings,{ok,Exprs,_},Translations,Dbg) ->
    {next,NewBindings}=interpret_cmd_files_3(Bindings,Exprs,Translations,Dbg),
    interpret_cmd_files_2(FD,NewBindings,io:parse_erl_exprs(FD,""),Translations,Dbg);
interpret_cmd_files_2(FD,Bindings,{error,ErrorInfo,Line},Translations,Dbg) ->
    inviso_rt_lib:debug(Dbg,parse_erl_exprs,[ErrorInfo,Line]),
    interpret_cmd_files_2(FD,Bindings,io:parse_erl_exprs(FD,""),Translations,Dbg);
interpret_cmd_files_2(_,_,{eof,_},_,_) ->    % End of file.
    true.

interpret_cmd_files_3(Bindings,Exprs,Translations,Dbg) ->
    case catch inviso_rt_lib:transform(Exprs,Translations) of
	NewExprs when is_list(NewExprs) ->     % We may have translated the API.
	    case catch erl_eval:exprs(NewExprs,Bindings) of
		{'EXIT',Reason} ->
		    inviso_rt_lib:debug(Dbg,exprs,[Exprs,Bindings,{'EXIT',Reason}]),
		    {next,Bindings};
		{value,_Val,NewBindings} -> % Only interested in the side effects!
		    {next,NewBindings}
	    end;
	{'EXIT',Reason} ->
	    inviso_rt_lib:debug(Dbg,translate2runtime_funcs,[Exprs,Reason]),
	    {next,Bindings}
    end.

%% Help function adding variables to a bindings structure. If the variable already
%% is assigned in the structure, it will be overridden. Returns a new
%% bindings structure.
join_local_and_global_vars([{Var,Val}|Rest],Bindings) when is_atom(Var) ->
    join_local_and_global_vars(Rest,erl_eval:add_binding(Var,Val,Bindings));
join_local_and_global_vars([_|Rest],Bindings) ->
    join_local_and_global_vars(Rest,Bindings);
join_local_and_global_vars([],Bindings) ->
    Bindings.

%% Help function returning a string of tokens, including "," separation
%% between the arguments.
tokenize_args(Args=[Arg|Rest]) when length(Args)>1 ->
    AbsTerm=erl_parse:abstract(Arg),
    Tokens=erl_parse:tokens(AbsTerm),
    {ok,Token,_}=erl_scan:string(","),
    Tokens++Token++tokenize_args(Rest);
tokenize_args([Arg]) ->
    AbsTerm=erl_parse:abstract(Arg),
    erl_parse:tokens(AbsTerm);
tokenize_args([]) ->
    "".
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Help functions working on the options given as argument to init during spawn.
%% -----------------------------------------------------------------------------

get_tracerdata_opts(ArgsFromConfig) ->
    case lists:keysearch(tracerdata,1,ArgsFromConfig) of
	{value,{_,{mfa,{M,F,CompleteTDGargs}}}} -> % Dynamic tracerdata.
	    case catch apply(M,F,CompleteTDGargs) of
		{'EXIT',_Reason} ->
		    false;
		TracerData ->
		    {ok,TracerData}
	    end;
	{value,{_,TracerData}} ->           % Interpret this as static tracerdata.
	    {ok,TracerData};
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_cmdfiles_opts(ArgsFromConfig) ->
    case lists:keysearch(cmdfiles,1,ArgsFromConfig) of
	{value,{_,CmdFiles}} ->
	    {ok,CmdFiles};
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

get_initialbindings_opts(ArgsFromConfig) ->
    case lists:keysearch(bindings,1,ArgsFromConfig) of
	{value,{_,Bindings}} ->
	    Bindings;
	false ->                            % Then we use empty bindings.
	    erl_eval:new_bindings()
    end.
%% -----------------------------------------------------------------------------

get_translations_opts(ArgsFromConfig) ->
    case lists:keysearch(translations,1,ArgsFromConfig) of
	{value,{_,Translations}} ->
	    Translations;
	false ->                            % This becomes nearly point less.
	    []
    end.
%% -----------------------------------------------------------------------------

get_dbg_opts(ArgsFromConfig) ->
    case lists:keysearch(debug,1,ArgsFromConfig) of
	{value,{_,DbgLevel}} ->
	    DbgLevel;
	false ->
	    off
    end.
%% -----------------------------------------------------------------------------

%% EOF



