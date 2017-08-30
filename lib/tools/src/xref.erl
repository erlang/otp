%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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

-module(xref).

-behaviour(gen_server).

%% External exports
-export([start/1, start/2, stop/1]).

-export([m/1, d/1,
	 add_release/2, add_release/3,
	 add_application/2, add_application/3, 
	 add_module/2, add_module/3,
	 add_directory/2, add_directory/3,
	 replace_module/3, replace_module/4,
	 replace_application/3, replace_application/4,
	 remove_module/2, remove_application/2, remove_release/2,
	 get_library_path/1, set_library_path/2, set_library_path/3,
	 q/2, q/3, info/1, info/2, info/3, 
	 update/1, update/2, 
	 forget/1, forget/2, variables/1, variables/2,
	 analyze/2, analyze/3, analyse/2, analyse/3,
	 get_default/1, get_default/2, 
	 set_default/2, set_default/3]).

-export([format_error/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

-import(lists, [keydelete/3, keysearch/3]).

-import(sofs, [to_external/1, is_sofs_set/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% add_release(Servername, Directory) -> 
%%         {ok, ReleaseName} | Error
%% add_release(Servername, Directory, Options) ->
%%         {ok, ReleaseName} | Error
%% add_application(Servername, Directory) -> 
%%         {ok, AppName} | Error
%% add_application(Servername, Directory, Options) ->
%%         {ok, AppName} | Error
%% add_module(ServerName, Filename) ->
%%     {ok, ModuleName} | Error
%% add_module(ServerName, Filename, Options) ->
%%     {ok, ModuleName} | Error
%% add_directory(ServerName, Directory) ->
%%     {ok, [ModuleName]} | Error
%% add_directory(ServerName, Directory, Options) ->
%%     {ok, [ModuleName]} | Error
%% replace_module(ServerName, Module, Filename) ->
%%     {ok, Module} | Error
%% replace_module(ServerName, Module, Filename, Options) ->
%%     {ok, Module} | Error
%% replace_application(ServerName, Application, Directory) ->
%%     {ok, AppName} | Error
%% replace_application(ServerName, Application, Directory, Options) ->
%%     {ok, AppName} | Error
%% remove_module(ServerName, Module) -> ok | Error
%% remove_application(ServerName, Application) -> ok | Error
%% remove_release(ServerName, Release) -> ok | Error
%% get_library_path(Servername) -> {ok, Path}
%% set_library_path(Servername, Path) -> ok | Error
%% set_library_path(Servername, Path, Options) -> ok | Error
%% info(Servername) -> InfoList
%% info(Servername, What) -> [{what(), InfoList}] | Error
%% info(Servername, What, Qual) -> [{what(), InfoList}] | Error
%% update(Servername) -> {ok, [Module]} | Error
%% update(Servername, Options) -> {ok, [Module]} | Error
%% forget(Servername) -> ok
%% forget(Servername, VariableName) -> ok | Error
%% variables(Servername) -> {ok, [{VarType, [VariableName]}]} | Error
%% variables(Servername, [VarType]) -> {ok, [{VarType, [VariableName]}]}
%% analyze(ServerName, What) -> {ok, Answer} | Error
%% analyze(ServerName, What, Options) -> {ok, Answer} | Error
%% q(Servername, Query) -> {ok, Answer} | Error
%% q(Servername, Query, Options) -> {ok, Answer} | Error
%% get_default(ServerName, Option) -> {ok, Value} | Error
%% set_default(ServerName, Option, Value) -> {ok, OldValue} | Error
%% get_default(ServerName) -> [{Option, Value}]
%% set_default(ServerName, [{Option, Value}]) -> ok | Error
%% format_error(Error) -> io_string()
%% m(Module) -> [Result] | Error
%% m(File) -> [Result] | Error
%% d(Directory) -> [Result] | Error

%% -> [Faulty] | Error; Faulty = {undefined, Calls} | {unused, Funs}
%% No user variables have been assigned digraphs, so there is no
%% need to call xref_base:delete/1.
m(Module) when is_atom(Module) ->
    case xref_utils:find_beam(Module) of
	{ok, File} ->
	    Fun = fun(S) -> 
                          xref_base:add_module(S, File, {builtins,true})
                  end,
	    case catch do_functions_analysis(Fun) of
		{error, _, {no_debug_info, _}} ->
		    catch do_modules_analysis(Fun);
		Result ->
		    Result
	    end;
	Error -> Error
    end;
m(File) ->
    case xref_utils:split_filename(File, ".beam") of
	false ->
	    {error, xref_base, {invalid_filename, File}};
	{Dir, BaseName} ->
	    BeamFile = filename:join(Dir, BaseName),
	    Fun = fun(S) -> 
                          xref_base:add_module(S, BeamFile, {builtins, true})
                  end,
	    case catch do_functions_analysis(Fun) of
		{error, _, {no_debug_info, _}} ->
		    catch do_modules_analysis(Fun);
		Result ->
		    Result
	    end
    end.

%% -> [Faulty] | Error; Faulty = {undefined, Calls} | {unused, Funs}
d(Directory) ->
    Fun = fun(S) ->
                  xref_base:add_directory(S, Directory, {builtins, true})
          end,
    Fun1 = fun(S) ->
		   case Fun(S) of
		       {ok, [], _S} -> 
			   no_modules;
		       Reply -> 
			   Reply
		   end
	   end,
    case catch do_functions_analysis(Fun1) of
	no_modules ->
	    catch do_modules_analysis(Fun);
	Result -> 
	    Result
    end.

start(Name) when is_atom(Name) ->
    start(Name, []);
start(Opts0) when is_list(Opts0) ->
    {Args, Opts} = split_args(Opts0),
    gen_server:start(xref, Args, Opts).

start(Name, Opts0) when is_list(Opts0) ->
    {Args, Opts} = split_args(Opts0),
    gen_server:start({local, Name}, xref, Args, Opts);
start(Name, Opt) ->
    start(Name, [Opt]).

split_args(Opts) ->
    case keysearch(xref_mode, 1, Opts) of
	{value, Mode} ->
	    {[Mode], keydelete(xref_mode, 1, Opts)};
	false ->
	    {[], Opts}
    end.

stop(Name) ->
    gen_server:call(Name, stop, infinity).

add_release(Name, Dir) ->
    gen_server:call(Name, {add_release, Dir}, infinity).

add_release(Name, Dir, Options) ->
    gen_server:call(Name, {add_release, Dir, Options}, infinity).

add_application(Name, Dir) ->
    gen_server:call(Name, {add_application, Dir}, infinity).

add_application(Name, Dir, Options) ->
    gen_server:call(Name, {add_application, Dir, Options}, infinity).

add_module(Name, File) ->
    gen_server:call(Name, {add_module, File}, infinity).

add_module(Name, File, Options) ->
    gen_server:call(Name, {add_module, File, Options}, infinity).

add_directory(Name, Dir) ->
    gen_server:call(Name, {add_directory, Dir}, infinity).

add_directory(Name, Dir, Options) ->
    gen_server:call(Name, {add_directory, Dir, Options}, infinity).

replace_module(Name, Module, File) ->
    gen_server:call(Name, {replace_module, Module, File}, infinity).

replace_module(Name, Module, File, Options) ->
    gen_server:call(Name, {replace_module, Module, File, Options}, infinity).

replace_application(Name, App, Dir) ->
    gen_server:call(Name, {replace_application, App, Dir}, infinity).

replace_application(Name, App, Dir, Options) ->
    gen_server:call(Name, {replace_application, App, Dir, Options}, infinity).

remove_module(Name, Mod) ->
    gen_server:call(Name, {remove_module, Mod}, infinity).

remove_application(Name, App) ->
    gen_server:call(Name, {remove_application, App}, infinity).

remove_release(Name, Rel) ->
    gen_server:call(Name, {remove_release, Rel}, infinity).

get_library_path(Name) ->
    gen_server:call(Name, get_library_path, infinity).

set_library_path(Name, Path) ->
    gen_server:call(Name, {set_library_path, Path}, infinity).

set_library_path(Name, Path, Options) ->
    gen_server:call(Name, {set_library_path, Path, Options}, infinity).

info(Name) ->
    gen_server:call(Name, info, infinity).

info(Name, What) ->
    gen_server:call(Name, {info, What}, infinity).

info(Name, What, Qual) ->
    gen_server:call(Name, {info, What, Qual}, infinity).

update(Name) ->
    gen_server:call(Name, update, infinity).

update(Name, Options) ->
    gen_server:call(Name, {update, Options}, infinity).

forget(Name) ->
    gen_server:call(Name, forget, infinity).

forget(Name, Variable) ->
    gen_server:call(Name, {forget, Variable}, infinity).

variables(Name) ->
    gen_server:call(Name, variables, infinity).

variables(Name, Options) ->
    gen_server:call(Name, {variables, Options}, infinity).

analyse(Name, What) ->
    gen_server:call(Name, {analyze, What}, infinity).

analyse(Name, What, Options) ->
    gen_server:call(Name, {analyze, What, Options}, infinity).

analyze(Name, What) ->
    gen_server:call(Name, {analyze, What}, infinity).

analyze(Name, What, Options) ->
    gen_server:call(Name, {analyze, What, Options}, infinity).

q(Name, Q) ->
    gen_server:call(Name, {qry, Q}, infinity).

q(Name, Q, Options) ->
    gen_server:call(Name, {qry, Q, Options}, infinity).

get_default(Name) ->
    gen_server:call(Name, get_default, infinity).

get_default(Name, Option) ->
    gen_server:call(Name, {get_default, Option}, infinity).

set_default(Name, OptionValues) ->
    gen_server:call(Name, {set_default, OptionValues}, infinity).

set_default(Name, Option, Value) ->
    gen_server:call(Name, {set_default, Option, Value}, infinity).

format_error({error, Module, Error}) ->
    Module:format_error(Error);
format_error(E) ->
    io_lib:format("~tp~n", [E]).

%%%----------------------------------------------------------------------
%%%Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(Args) ->
    case xref_base:new(Args) of
	{ok, S} ->
	    {ok, S};
	{error, _Module, Reason} ->
	    {stop, Reason}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({add_release, Dir}, _From, State) ->
    case xref_base:add_release(State, Dir) of
	{ok, ReleaseName, NewState} ->
	    {reply, {ok, ReleaseName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_release, Dir, Options}, _From, State) ->
    case xref_base:add_release(State, Dir, Options) of
	{ok, ReleaseName, NewState} ->
	    {reply, {ok, ReleaseName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_application, Dir}, _From, State) ->
    case xref_base:add_application(State, Dir) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_application, Dir, Options}, _From, State) ->
    case xref_base:add_application(State, Dir, Options) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_module, File}, _From, State) ->
    case xref_base:add_module(State, File) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error,  State}
    end;
handle_call({add_module, File, Options}, _From, State) ->
    case xref_base:add_module(State, File, Options) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error,  State}
    end;
handle_call({replace_application, Appl, Dir}, _From, State) ->
    case xref_base:replace_application(State, Appl, Dir) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_application, Appl, Dir, Opts}, _From, State) ->
    case xref_base:replace_application(State, Appl, Dir, Opts) of
	{ok, AppName, NewState} ->
	    {reply, {ok, AppName}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_module, Mod}, _From, State) ->
    case xref_base:remove_module(State, Mod) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_application, Appl}, _From, State) ->
    case xref_base:remove_application(State, Appl) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({remove_release, Rel}, _From, State) ->
    case xref_base:remove_release(State, Rel) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_directory, Dir}, _From, State) ->
    case xref_base:add_directory(State, Dir) of
	{ok, Modules, NewState} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({add_directory, Dir, Options}, _From, State) ->
    case xref_base:add_directory(State, Dir, Options) of
	{ok, Modules, NewState} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(get_library_path, _From, State) ->
    Path = xref_base:get_library_path(State),
    {reply, Path, State};
handle_call({set_library_path, Path}, _From, State) ->
    case xref_base:set_library_path(State, Path) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({set_library_path, Path, Options}, _From, State) ->
    case xref_base:set_library_path(State, Path, Options) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_module, Module, File}, _From, State) ->
    case xref_base:replace_module(State, Module, File) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({replace_module, Module, File, Options}, _From, State) ->
    case xref_base:replace_module(State, Module, File, Options) of
	{ok, Module, NewState} ->
	    {reply, {ok, Module}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(info, _From, State) ->
    {reply, xref_base:info(State), State};
handle_call({info, What}, _From, State) ->
    {reply, xref_base:info(State, What), State};
handle_call({info, What, Qual}, _From, State) ->
    {reply, xref_base:info(State, What, Qual), State};
handle_call(update, _From, State) ->
    case xref_base:update(State) of
	{ok, NewState, Modules} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({update, Options}, _From, State) ->
    case xref_base:update(State, Options) of
	{ok, NewState, Modules} ->
	    {reply, {ok, Modules}, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(forget, _From, State) ->
    {ok, NewState} = xref_base:forget(State),
    {reply, ok, NewState};
handle_call({forget, Variable}, _From, State) ->
    case xref_base:forget(State, Variable) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call(variables, _From, State) ->
    %% The reason the ok-Error pattern is broken for variables, q and
    %% analyze is that the state may have been updated even if an
    %% error occurs.
    {Reply, NewState} = xref_base:variables(State),
    {reply, Reply, NewState};
handle_call({variables, Options}, _From, State) ->
    {Reply, NewState} = xref_base:variables(State, Options),
    {reply, Reply, NewState};
handle_call({analyze, What}, _From, State) ->
    {Reply, NewState} = xref_base:analyze(State, What),
    {reply, unsetify(Reply), NewState};
handle_call({analyze, What, Options}, _From, State) ->
    {Reply, NewState} = xref_base:analyze(State, What, Options),
    {reply, unsetify(Reply), NewState};
handle_call({qry, Q}, _From, State) ->
    {Reply, NewState} = xref_base:q(State, Q),
    {reply, unsetify(Reply), NewState};
handle_call({qry, Q, Options}, _From, State) ->
    {Reply, NewState} = xref_base:q(State, Q, Options),
    {reply, unsetify(Reply), NewState};
handle_call(get_default, _From, State) ->
    Reply = xref_base:get_default(State),
    {reply, Reply, State};
handle_call({get_default, Option}, _From, State) ->
    Reply = xref_base:get_default(State, Option),
    {reply, Reply, State};
handle_call({set_default, OptionValues}, _From, State) ->
    case xref_base:set_default(State, OptionValues) of
	{ok, NewState} ->
	    {reply, ok, NewState};
	Error ->
	    {reply, Error, State}
    end;
handle_call({set_default, Option, Value}, _From, State) ->
    case xref_base:set_default(State, Option, Value) of
	{ok, OldValue, NewState} ->
	    {reply, {ok, OldValue}, NewState};
	Error ->
	    {reply, Error, State}
    end.

%%----------------------------------------------------------------------
%% Func: handle_cast/2 
%% Returns: {noreply, State} | 
%% {noreply, State, Timeout} | 
%% {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_functions_analysis(FFun) ->
    {ok, State} = xref_base:new(),
    {ok, State1} = xref_base:set_library_path(State, code_path),
    {ok, State2} = xref_base:set_default(State1, 
					 [{verbose,false},{warnings,false}]),
    State3 = case FFun(State2) of
		 {ok, _, S} -> S;
		 Error2 -> throw(Error2)
	     end,
    {Undef, State4} = do_analysis(State3, undefined_function_calls),
    {Unused, State5} = do_analysis(State4, locals_not_used),
    {Deprecated, _} = do_analysis(State5, deprecated_function_calls),
    [{deprecated,to_external(Deprecated)},
     {undefined,to_external(Undef)}, 
     {unused,to_external(Unused)}].

do_modules_analysis(FFun) ->
    {ok, State} = xref_base:new({xref_mode, modules}),
    {ok, State1} = xref_base:set_library_path(State, code_path),
    {ok, State2} = xref_base:set_default(State1, 
					 [{verbose,false},{warnings,false}]),
    State3 = case FFun(State2) of
		 {ok, _, S} -> S;
		 Error2 -> throw(Error2)
	     end,
    {Undef, State4} = do_analysis(State3, undefined_functions),
    {Deprecated, _} = do_analysis(State4, deprecated_functions),
    [{deprecated,to_external(Deprecated)},
     {undefined,to_external(Undef)}].

do_analysis(State, Analysis) ->
    case xref_base:analyze(State, Analysis) of
	{{ok, Reply}, NewState} ->
	    {Reply, NewState};
	{Error, _} ->
	    throw(Error)
    end.

unsetify(Reply={ok, X}) ->
    case is_sofs_set(X) of
	true -> {ok, to_external(X)};
	false -> Reply
    end;
unsetify(Reply) ->
    Reply.
