%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
-module(cover).

%%
%% This module implements the Erlang coverage tool.
%% 
%% ARCHITECTURE
%% The coverage tool consists of one process on each node involved in
%% coverage analysis. The process is registered as 'cover_server'
%% (?SERVER).  The cover_server on the 'main' node is in charge, and
%% it monitors the cover_servers on all remote nodes. When it gets a
%% 'DOWN' message for another cover_server, it marks the node as
%% 'lost'. If a nodeup is received for a lost node the main node
%% ensures that the cover compiled modules are loaded again. If the
%% remote node was alive during the disconnected periode, cover data
%% for this periode will also be included in the analysis.
%%
%% The cover_server process on the main node is implemented by the
%% functions init_main/1 and main_process_loop/1. The cover_server on
%% the remote nodes are implemented by the functions init_remote/2 and
%% remote_process_loop/1.
%%
%% TABLES
%% Each nodes has two tables: cover_internal_data_table (?COVER_TABLE) and.
%% cover_internal_clause_table (?COVER_CLAUSE_TABLE). 
%% ?COVER_TABLE contains the bump data i.e. the data about which lines
%% have been executed how many times.
%% ?COVER_CLAUSE_TABLE contains information about which clauses in which modules
%% cover is currently collecting statistics.
%% 
%% The main node owns tables named
%% 'cover_collected_remote_data_table' (?COLLECTION_TABLE) and
%% 'cover_collected_remote_clause_table' (?COLLECTION_CLAUSE_TABLE). 
%% These tables contain data which is collected from remote nodes (either when a
%% remote node is stopped with cover:stop/1 or when analysing). When
%% analysing, data is even moved from the COVER tables on the main
%% node to the COLLECTION tables.
%%
%% The main node also has a table named 'cover_binary_code_table'
%% (?BINARY_TABLE). This table contains the binary code for each cover
%% compiled module. This is necessary so that the code can be loaded
%% on remote nodes that are started after the compilation.
%%
%% PARALLELISM
%% To take advantage of SMP when doing the cover analysis both the data 
%% collection and analysis has been parallelized. One process is spawned for
%% each node when collecting data, and on the remote node when collecting data
%% one process is spawned per module. 
%% 
%% When analyzing data it is possible to issue multiple analyse(_to_file)/X 
%% calls at once. They are however all calls (for backwards compatibility
%% reasons) so the user of cover will have to spawn several processes to to the
%% calls ( or use async_analyse_to_file ). 
%%

%% External exports
-export([start/0, start/1,
	 compile/1, compile/2, compile_module/1, compile_module/2,
	 compile_directory/0, compile_directory/1, compile_directory/2,
	 compile_beam/1, compile_beam_directory/0, compile_beam_directory/1,
	 analyse/0, analyse/1, analyse/2, analyse/3,
	 analyze/0, analyze/1, analyze/2, analyze/3,
	 analyse_to_file/0,
	 analyse_to_file/1, analyse_to_file/2, analyse_to_file/3,
	 analyze_to_file/0,
	 analyze_to_file/1, analyze_to_file/2, analyze_to_file/3,
	 async_analyse_to_file/1,async_analyse_to_file/2,
	 async_analyse_to_file/3, async_analyze_to_file/1,
	 async_analyze_to_file/2, async_analyze_to_file/3,
	 export/1, export/2, import/1,
	 modules/0, imported/0, imported_modules/0, which_nodes/0, is_compiled/1,
	 reset/1, reset/0,
	 flush/1,
	 stop/0, stop/1]).
-export([remote_start/1,get_main_node/0]).

%% Used internally to ensure we upgrade the code to the latest version.
-export([main_process_loop/1,remote_process_loop/1]).

-record(main_state, {compiled=[],           % [{Module,File}]
		     imported=[],           % [{Module,File,ImportFile}]
		     stopper,               % undefined | pid()
		     nodes=[],              % [Node]
		     lost_nodes=[]}).       % [Node]

-record(remote_state, {compiled=[],         % [{Module,File}]
		       main_node}).         % atom()

-record(bump, {module   = '_',              % atom()
	       function = '_',              % atom()
	       arity    = '_',              % integer()
	       clause   = '_',              % integer()
	       line     = '_'               % integer()
	      }).
-define(BUMP_REC_NAME,bump).
-define(CHUNK_SIZE, 20000).

-record(vars, {module,                      % atom() Module name
	       
	       init_info=[],                % [{M,F,A,C,L}]

	       function,                    % atom()
	       arity,                       % int()
	       clause,                      % int()
	       lines,                       % [int()]
               no_bump_lines,               % [int()]
	       depth,                       % int()
	       is_guard=false               % boolean
	      }).

-define(COVER_TABLE, 'cover_internal_data_table').
-define(COVER_CLAUSE_TABLE, 'cover_internal_clause_table').
-define(BINARY_TABLE, 'cover_binary_code_table').
-define(COLLECTION_TABLE, 'cover_collected_remote_data_table').
-define(COLLECTION_CLAUSE_TABLE, 'cover_collected_remote_clause_table').
-define(TAG, cover_compiled).
-define(SERVER, cover_server).

%% Line doesn't matter.
-define(BLOCK(Expr), {block,erl_anno:new(0),[Expr]}).
-define(BLOCK1(Expr), 
        if 
            element(1, Expr) =:= block ->
                Expr;
            true -> ?BLOCK(Expr)
        end).

-define(SPAWN_DBG(Tag,Value),put(Tag,Value)).

-include_lib("stdlib/include/ms_transform.hrl").

%%%----------------------------------------------------------------------
%%% External exports
%%%----------------------------------------------------------------------

%% start() -> {ok,Pid} | {error,Reason}
%%   Pid = pid()
%%   Reason = {already_started,Pid} | term()
start() ->
    case whereis(?SERVER) of
	undefined ->
	    Starter = self(),
	    Pid = spawn(fun() -> 
				?SPAWN_DBG(start,[]),
				init_main(Starter) 
			end),
	    Ref = erlang:monitor(process,Pid),
	    Return = 
		receive 
		    {?SERVER,started} -> 
			{ok,Pid};
		    {'DOWN', Ref, _Type, _Object, Info} -> 
			{error,Info}
		end,
	    erlang:demonitor(Ref),
	    Return;
	Pid ->
	    {error,{already_started,Pid}}
    end.

%% start(Nodes) -> {ok,StartedNodes}
%%   Nodes = Node | [Node,...]
%%   Node = atom()
start(Node) when is_atom(Node) ->
    start([Node]);
start(Nodes) ->
    call({start_nodes,remove_myself(Nodes,[])}).

%% compile(ModFiles) ->
%% compile(ModFiles, Options) ->
%% compile_module(ModFiles) -> Result
%% compile_module(ModFiles, Options) -> Result
%%   ModFiles = ModFile | [ModFile]
%%   ModFile = Module | File
%%     Module = atom()
%%     File = string()
%%   Options = [Option]
%%     Option = {i,Dir} | {d,Macro} | {d,Macro,Value}
%%   Result = {ok,Module} | {error,File}
compile(ModFile) ->
    compile_module(ModFile, []).
compile(ModFile, Options) ->
    compile_module(ModFile, Options).
compile_module(ModFile) when is_atom(ModFile);
			     is_list(ModFile) ->
    compile_module(ModFile, []).
compile_module(ModFile, Options) when is_atom(ModFile);
				      is_list(ModFile), is_integer(hd(ModFile)) ->
    [R] = compile_module([ModFile], Options),
    R;
compile_module(ModFiles, Options) when is_list(Options) ->
    AbsFiles =
	[begin
	     File =
		 case ModFile of
		     _ when is_atom(ModFile) -> atom_to_list(ModFile);
		     _ when is_list(ModFile) -> ModFile
		 end,
	     WithExt = case filename:extension(File) of
			   ".erl" ->
			       File;
			   _ ->
			       File++".erl"
		       end,
	     filename:absname(WithExt)
	 end || ModFile <- ModFiles],
    compile_modules(AbsFiles, Options).

%% compile_directory() ->
%% compile_directory(Dir) ->
%% compile_directory(Dir, Options) -> [Result] | {error,Reason}
%%   Dir = string()
%%   Options - see compile/1
%%   Result - see compile/1
%%   Reason = eacces | enoent
compile_directory() ->
    case file:get_cwd() of
	{ok, Dir} ->
	    compile_directory(Dir, []);
	Error ->
	    Error
    end.
compile_directory(Dir) when is_list(Dir) ->
    compile_directory(Dir, []).
compile_directory(Dir, Options) when is_list(Dir), is_list(Options) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    ErlFiles = [filename:join(Dir, File) ||
			   File <- Files,
			   filename:extension(File) =:= ".erl"],
	    compile_modules(ErlFiles, Options);
	Error ->
	    Error
    end.

compile_modules(Files,Options) ->
    Options2 = filter_options(Options),
    %% compile_modules(Files,Options2,[]).
    call({compile, Files, Options2}).

%% compile_modules([File|Files], Options, Result) ->
%%     R = call({compile, File, Options}),
%%     compile_modules(Files,Options,[R|Result]);
%% compile_modules([],_Opts,Result) ->
%%     lists:reverse(Result).

filter_options(Options) ->
    lists:filter(fun(Option) ->
                         case Option of
                             {i, Dir} when is_list(Dir) -> true;
                             {d, _Macro} -> true;
                             {d, _Macro, _Value} -> true;
                             export_all -> true;
                             _ -> false
                         end
                 end,
                 Options).

%% compile_beam(ModFile) -> Result | {error,Reason}
%%   ModFile - see compile/1
%%   Result - see compile/1
%%   Reason = non_existing | already_cover_compiled
compile_beam(ModFile0) when is_atom(ModFile0);
			    is_list(ModFile0), is_integer(hd(ModFile0)) ->
    case compile_beams([ModFile0]) of
	[{error,{non_existing,_}}] ->
	    %% Backwards compatibility
	    {error,non_existing};
	[Result] ->
	    Result
    end;
compile_beam(ModFiles) when is_list(ModFiles) ->
    compile_beams(ModFiles).


%% compile_beam_directory(Dir) -> [Result] | {error,Reason}
%%   Dir - see compile_directory/1
%%   Result - see compile/1
%%   Reason = eacces | enoent
compile_beam_directory() ->
    case file:get_cwd() of
	{ok, Dir} ->
	    compile_beam_directory(Dir);
	Error ->
	    Error
    end.
compile_beam_directory(Dir) when is_list(Dir) ->
    case file:list_dir(Dir) of
	{ok, Files} ->
	    BeamFiles =  [filename:join(Dir, File) ||
			     File <- Files,
			     filename:extension(File) =:= ".beam"],
	    compile_beams(BeamFiles);
	Error ->
	    Error
    end.

compile_beams(ModFiles0) ->
    ModFiles = get_mods_and_beams(ModFiles0,[]),
    call({compile_beams,ModFiles}).

get_mods_and_beams([Module|ModFiles],Acc) when is_atom(Module) ->
    case code:which(Module) of
	non_existing ->
	    get_mods_and_beams(ModFiles,[{error,{non_existing,Module}}|Acc]);
	File ->
	    get_mods_and_beams([{Module,File}|ModFiles],Acc)
    end;
get_mods_and_beams([File|ModFiles],Acc) when is_list(File) ->
    {WithExt,WithoutExt}
	= case filename:rootname(File,".beam") of
	      File ->
		  {File++".beam",File};
	      Rootname ->
		  {File,Rootname}
	      end,
    AbsFile = filename:absname(WithExt),
    Module = list_to_atom(filename:basename(WithoutExt)),
    get_mods_and_beams([{Module,AbsFile}|ModFiles],Acc);
get_mods_and_beams([{Module,File}|ModFiles],Acc) ->
    %% Check for duplicates
    case lists:keyfind(Module,2,Acc) of
	{ok,Module,File} ->
	    %% Duplicate, but same file so ignore
	    get_mods_and_beams(ModFiles,Acc);
	{ok,Module,_OtherFile} ->
	    %% Duplicate and differnet file - error
	    get_mods_and_beams(ModFiles,[{error,{duplicate,Module}}|Acc]);
	_ ->
	    get_mods_and_beams(ModFiles,[{ok,Module,File}|Acc])
    end;
get_mods_and_beams([],Acc) ->
    lists:reverse(Acc).


%% analyse(Modules) ->
%% analyse(Analysis) ->
%% analyse(Level) ->
%% analyse(Modules, Analysis) ->
%% analyse(Modules, Level) ->
%% analyse(Analysis, Level)
%% analyse(Modules, Analysis, Level) -> {ok,Answer} | {error,Error}
%%   Modules = Module | [Module]
%%   Module = atom()
%%   Analysis = coverage | calls
%%   Level = line | clause | function | module
%%   Answer = {Module,Value} | [{Item,Value}]
%%     Item = Line | Clause | Function
%%      Line = {M,N}
%%      Clause = {M,F,A,C}
%%      Function = {M,F,A}
%%        M = F = atom()
%%        N = A = C = integer()
%%     Value = {Cov,NotCov} | Calls
%%       Cov = NotCov = Calls = integer()
%%   Error = {not_cover_compiled,Module} | not_main_node
-define(is_analysis(__A__),
	(__A__=:=coverage orelse __A__=:=calls)).
-define(is_level(__L__),
	(__L__=:=line orelse __L__=:=clause orelse
	 __L__=:=function orelse __L__=:=module)).
analyse() ->
    analyse('_').

analyse(Analysis) when ?is_analysis(Analysis) ->
    analyse('_', Analysis);
analyse(Level) when ?is_level(Level) ->
    analyse('_', Level);
analyse(Module) ->
    analyse(Module, coverage).

analyse(Analysis, Level) when ?is_analysis(Analysis) andalso
			      ?is_level(Level) ->
    analyse('_', Analysis, Level);
analyse(Module, Analysis) when ?is_analysis(Analysis) ->
    analyse(Module, Analysis, function);
analyse(Module, Level) when ?is_level(Level) ->
    analyse(Module, coverage, Level).

analyse(Module, Analysis, Level) when ?is_analysis(Analysis),
				      ?is_level(Level) ->
    call({{analyse, Analysis, Level}, Module}).

analyze() -> analyse( ).
analyze(Module) -> analyse(Module).
analyze(Module, Analysis) -> analyse(Module, Analysis).
analyze(Module, Analysis, Level) -> analyse(Module, Analysis, Level).

%% analyse_to_file() ->
%% analyse_to_file(Modules) ->
%% analyse_to_file(Modules, Options) ->
%%   Modules = Module | [Module]
%%   Module = atom()
%%   OutFile = string()
%%   Options = [Option]
%%     Option = html | {outfile,filename()} | {outdir,dirname()}
%%   Error = {not_cover_compiled,Module} | no_source_code_found |
%%           {file,File,Reason}
%%     File = string()
%%     Reason = term()
%%
%% Kept for backwards compatibility:
%% analyse_to_file(Modules, OutFile) ->
%% analyse_to_file(Modules, OutFile, Options) -> {ok,OutFile} | {error,Error}
analyse_to_file() ->
    analyse_to_file('_').
analyse_to_file(Arg) ->
    case is_options(Arg) of
	true ->
	    analyse_to_file('_',Arg);
	false ->
	    analyse_to_file(Arg,[])
    end.
analyse_to_file(Module, OutFile) when is_list(OutFile), is_integer(hd(OutFile)) ->
    %% Kept for backwards compatibility
    analyse_to_file(Module, [{outfile,OutFile}]);
analyse_to_file(Module, Options) when is_list(Options) ->
    call({{analyse_to_file, Options}, Module}).
analyse_to_file(Module, OutFile, Options) when is_list(OutFile) ->
    %% Kept for backwards compatibility
    analyse_to_file(Module,[{outfile,OutFile}|Options]).

analyze_to_file() -> analyse_to_file().
analyze_to_file(Module) -> analyse_to_file(Module).
analyze_to_file(Module, OptOrOut) -> analyse_to_file(Module, OptOrOut).
analyze_to_file(Module, OutFile, Options) -> 
    analyse_to_file(Module, OutFile, Options).

async_analyse_to_file(Module) ->
    do_spawn(?MODULE, analyse_to_file, [Module]).
async_analyse_to_file(Module, OutFileOrOpts) ->
    do_spawn(?MODULE, analyse_to_file, [Module, OutFileOrOpts]).
async_analyse_to_file(Module, OutFile, Options) ->
    do_spawn(?MODULE, analyse_to_file, [Module, OutFile, Options]).

is_options([html]) ->
    true; % this is not 100% safe - could be a module named html...
is_options([html|Opts]) ->
    is_options(Opts);
is_options([{Opt,_}|_]) when Opt==outfile; Opt==outdir ->
    true;
is_options(_) ->
    false.

do_spawn(M,F,A) ->
    spawn_link(fun() ->
		  case apply(M,F,A) of
		      {ok, _} ->
			  ok;
		      {error, Reason} ->
			  exit(Reason)
		  end
	  end).

async_analyze_to_file(Module) ->
    async_analyse_to_file(Module).
async_analyze_to_file(Module, OutFileOrOpts) ->
    async_analyse_to_file(Module, OutFileOrOpts).
async_analyze_to_file(Module, OutFile, Options) ->
    async_analyse_to_file(Module, OutFile, Options).

outfilename(undefined, Module, HTML) ->
    outfilename(Module, HTML);
outfilename(OutDir, Module, HTML) ->
    filename:join(OutDir, outfilename(Module, HTML)).

outfilename(Module, true) ->
    atom_to_list(Module)++".COVER.html";
outfilename(Module, false) ->
    atom_to_list(Module)++".COVER.out".


%% export(File)
%% export(File,Module) -> ok | {error,Reason}
%%   File = string(); file to write the exported data to
%%   Module = atom()
export(File) ->
    export(File, '_').
export(File, Module) ->
    call({export,File,Module}).

%% import(File) -> ok | {error, Reason}
%%   File = string(); file created with cover:export/1,2
import(File) ->
    call({import,File}).

%% modules() -> [Module]
%%   Module = atom()
modules() ->
   call(modules).

%% imported_modules() -> [Module]
%%   Module = atom()
imported_modules() ->
   call(imported_modules).

%% imported() -> [ImportFile]
%%   ImportFile = string()
imported() ->
   call(imported).

%% which_nodes() -> [Node]
%%   Node = atom()
which_nodes() ->
   call(which_nodes).

%% is_compiled(Module) -> {file,File} | false
%%   Module = atom()
%%   File = string()
is_compiled(Module) when is_atom(Module) ->
    call({is_compiled, Module}).

%% reset(Module) -> ok | {error,Error}
%% reset() -> ok
%%   Module = atom()
%%   Error = {not_cover_compiled,Module}
reset(Module) when is_atom(Module) ->
    call({reset, Module}).
reset() ->
    call(reset).

%% stop() -> ok
stop() ->
    call(stop).

stop(Node) when is_atom(Node) ->
    stop([Node]);
stop(Nodes) ->
    call({stop,remove_myself(Nodes,[])}).

%% flush(Nodes) -> ok | {error,not_main_node}
%%   Nodes = [Node] | Node
%%   Node = atom()
%%   Error = {not_cover_compiled,Module}
flush(Node) when is_atom(Node) ->
    flush([Node]);
flush(Nodes) ->
    call({flush,remove_myself(Nodes,[])}).

%% Used by test_server only. Not documented.
get_main_node() ->
    call(get_main_node).

%% bump(Module, Function, Arity, Clause, Line)
%%   Module = Function = atom()
%%   Arity = Clause = Line = integer()
%% This function is inserted into Cover compiled modules, once for each
%% executable line.
%bump(Module, Function, Arity, Clause, Line) ->
%    Key = #bump{module=Module, function=Function, arity=Arity, clause=Clause,
%		line=Line},
%    ets:update_counter(?COVER_TABLE, Key, 1).

call(Request) ->
    Ref = erlang:monitor(process,?SERVER),
    receive {'DOWN', Ref, _Type, _Object, noproc} -> 
	    erlang:demonitor(Ref),
            {ok,_} = start(),
	    call(Request)
    after 0 ->
	    ?SERVER ! {self(),Request},
	    Return = 
		receive 
		    {'DOWN', Ref, _Type, _Object, Info} -> 
			exit(Info);
		    {?SERVER,Reply} -> 
			Reply
		end,
	    erlang:demonitor(Ref, [flush]),
	    Return
    end.

reply(From, Reply) ->
    From ! {?SERVER,Reply},
    ok.

is_from(From) ->
    is_pid(From).

remote_call(Node,Request) ->
    Ref = erlang:monitor(process,{?SERVER,Node}),
    receive {'DOWN', Ref, _Type, _Object, noproc} -> 
	    erlang:demonitor(Ref),
	    {error,node_dead}
    after 0 ->
	    {?SERVER,Node} ! Request,
	    Return = 
		receive 
		    {'DOWN', Ref, _Type, _Object, _Info} -> 
			case Request of
			    {remote,stop} -> ok;
			    _ -> {error,node_dead}
			end;
		    {?SERVER,Reply} -> 
			Reply
		end,
	    erlang:demonitor(Ref, [flush]),
	    Return
    end.
    
remote_reply(Proc,Reply) when is_pid(Proc) ->
    Proc ! {?SERVER,Reply},
    ok;
remote_reply(MainNode,Reply) ->
    {?SERVER,MainNode} ! {?SERVER,Reply},
    ok.

%%%----------------------------------------------------------------------
%%% cover_server on main node
%%%----------------------------------------------------------------------

init_main(Starter) ->
    register(?SERVER,self()),
    %% Having write concurrancy here gives a 40% performance boost
    %% when collect/1 is called. 
    ?COVER_TABLE = ets:new(?COVER_TABLE, [set, public, named_table,
                                          {write_concurrency, true}]),
    ?COVER_CLAUSE_TABLE = ets:new(?COVER_CLAUSE_TABLE, [set, public,
                                                        named_table]),
    ?BINARY_TABLE = ets:new(?BINARY_TABLE, [set, public, named_table]),
    ?COLLECTION_TABLE = ets:new(?COLLECTION_TABLE, [set, public,
                                                    named_table]),
    ?COLLECTION_CLAUSE_TABLE = ets:new(?COLLECTION_CLAUSE_TABLE, [set, public,
                                                                  named_table]),
    ok = net_kernel:monitor_nodes(true),
    Starter ! {?SERVER,started},
    main_process_loop(#main_state{}).

main_process_loop(State) ->
    receive
	{From, {start_nodes,Nodes}} ->
	    {StartedNodes,State1} = do_start_nodes(Nodes, State),
	    reply(From, {ok,StartedNodes}),
	    main_process_loop(State1);

	{From, {compile, Files, Options}} ->
	    {R,S} = do_compile(Files, Options, State),
	    reply(From,R),
	    %% This module (cover) could have been reloaded. Make
	    %% sure we run the new code.
	    ?MODULE:main_process_loop(S);

	{From, {compile_beams, ModsAndFiles}} ->
	    {R,S} = do_compile_beams(ModsAndFiles,State),
	    reply(From,R),
	    %% This module (cover) could have been reloaded. Make
	    %% sure we run the new code.
	    ?MODULE:main_process_loop(S);

	{From, {export,OutFile,Module}} ->
	    spawn(fun() ->
			  ?SPAWN_DBG(export,{OutFile, Module}),
			  do_export(Module, OutFile, From, State)
		  end),
	    main_process_loop(State);
	
	{From, {import,File}} ->
	    case file:open(File,[read,binary,raw]) of
		{ok,Fd} ->
		    Imported = do_import_to_table(Fd,File,
						  State#main_state.imported),
		    reply(From, ok),
		    ok = file:close(Fd),
		    main_process_loop(State#main_state{imported=Imported});
		{error,Reason} ->
		    reply(From, {error, {cant_open_file,File,Reason}}),
		    main_process_loop(State)
	    end;

	{From, modules} ->
	    %% Get all compiled modules which are still loaded
	    {LoadedModules,Compiled} = 
		get_compiled_still_loaded(State#main_state.nodes,
					  State#main_state.compiled),
	    
	    reply(From, LoadedModules),
	    main_process_loop(State#main_state{compiled=Compiled});

	{From, imported_modules} ->
	    %% Get all modules with imported data
	    ImportedModules = lists:map(fun({Mod,_File,_ImportFile}) -> Mod end,
					State#main_state.imported),
	    reply(From, ImportedModules),
	    main_process_loop(State);

	{From, imported} ->
	    %% List all imported files
	    reply(From, get_all_importfiles(State#main_state.imported,[])),
	    main_process_loop(State);

	{From, which_nodes} ->
	    %% List all imported files
	    reply(From, State#main_state.nodes),
	    main_process_loop(State);

	{From, reset} ->
	    lists:foreach(
	      fun({Module,_File}) -> 
		      do_reset_main_node(Module,State#main_state.nodes)
	      end, 
	      State#main_state.compiled),
	    reply(From, ok),
	    main_process_loop(State#main_state{imported=[]});

	{From, {stop,Nodes}} ->
	    remote_collect('_',Nodes,true),
	    reply(From, ok),
	    Nodes1 = State#main_state.nodes--Nodes,
	    LostNodes1 = State#main_state.lost_nodes--Nodes,
	    main_process_loop(State#main_state{nodes=Nodes1,
					       lost_nodes=LostNodes1});

	{From, {flush,Nodes}} ->
	    remote_collect('_',Nodes,false),
	    reply(From, ok),
	    main_process_loop(State);

	{From, stop} ->
	    lists:foreach(
	      fun(Node) -> 
		      remote_call(Node,{remote,stop})
	      end,
	      State#main_state.nodes),
	    reload_originals(State#main_state.compiled),
            ets:delete(?COVER_TABLE),
            ets:delete(?COVER_CLAUSE_TABLE),
            ets:delete(?BINARY_TABLE),
            ets:delete(?COLLECTION_TABLE),
            ets:delete(?COLLECTION_CLAUSE_TABLE),
            unregister(?SERVER),
	    reply(From, ok);

	{From, {{analyse, Analysis, Level}, '_'}} ->
	    R = analyse_all(Analysis, Level, State),
	    reply(From, R),
	    main_process_loop(State);

	{From, {{analyse, Analysis, Level}, Modules}} when is_list(Modules) ->
	    R = analyse_list(Modules, Analysis, Level, State),
	    reply(From, R),
	    main_process_loop(State);

	{From, {{analyse, Analysis, Level}, Module}} ->
	    S = try 
		    Loaded = is_loaded(Module, State),
		    spawn(fun() ->
				  ?SPAWN_DBG(analyse,{Module,Analysis, Level}),
				  do_parallel_analysis(
				    Module, Analysis, Level, 
				    Loaded, From, State)
			  end),
		    State
		catch throw:Reason ->
			reply(From,{error, {not_cover_compiled,Module}}),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);

	{From, {{analyse_to_file, Opts},'_'}} ->
	    R = analyse_all_to_file(Opts, State),
	    reply(From,R),
	    main_process_loop(State);

	{From, {{analyse_to_file, Opts},Modules}} when is_list(Modules) ->
	    R = analyse_list_to_file(Modules, Opts, State),
	    reply(From,R),
	    main_process_loop(State);

	{From, {{analyse_to_file, Opts},Module}} ->
	    S = try 
		    Loaded = is_loaded(Module, State),
		    spawn_link(fun() ->
				  ?SPAWN_DBG(analyse_to_file,{Module,Opts}),
				  do_parallel_analysis_to_file(
				    Module, Opts, Loaded, From, State)
			  end),
		    State
		catch throw:Reason ->
			reply(From,{error, {not_cover_compiled,Module}}),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);

	{From, {is_compiled, Module}} ->
	    S = try is_loaded(Module, State) of
		    {loaded, File} ->
			reply(From,{file, File}),
			State;
		    {imported,_File,_ImportFiles} ->
			reply(From,false),
			State
		catch throw:Reason ->
			reply(From,false),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);

	{From, {reset, Module}} ->
	    S = try 
		    Loaded = is_loaded(Module,State),
		    R = case Loaded of
			    {loaded, _File} ->
				do_reset_main_node(
				  Module, State#main_state.nodes);
			    {imported, _File, _} ->
				do_reset_collection_table(Module)
			end,
		    Imported = 
			remove_imported(Module,
					State#main_state.imported),
		    reply(From, R),
		    State#main_state{imported=Imported}
		catch throw:Reason ->
			reply(From,{error, {not_cover_compiled,Module}}),
			not_loaded(Module, Reason, State)
		end,
	    main_process_loop(S);		    
	
	{'DOWN', _MRef, process, {?SERVER,Node}, _Info} ->
	    %% A remote cover_server is down, mark as lost
	    {Nodes,Lost} =
		case lists:member(Node,State#main_state.nodes) of
		    true ->
			N = State#main_state.nodes--[Node],
			L = [Node|State#main_state.lost_nodes],
			{N,L};
		    false -> % node stopped
			{State#main_state.nodes,State#main_state.lost_nodes}
		end,
	    main_process_loop(State#main_state{nodes=Nodes,lost_nodes=Lost});

	{nodeup,Node} ->
	    State1 =
		case lists:member(Node,State#main_state.lost_nodes) of
		    true ->
			sync_compiled(Node,State);
		    false ->
			State
	    end,
	    main_process_loop(State1);

	{nodedown,_} ->
	    %% Will be taken care of when 'DOWN' message arrives
	    main_process_loop(State);
	
	{From, get_main_node} ->
	    reply(From, node()),
	    main_process_loop(State);

	get_status ->
	    io:format("~tp~n",[State]),
	    main_process_loop(State)
    end.

%%%----------------------------------------------------------------------
%%% cover_server on remote node
%%%----------------------------------------------------------------------

init_remote(Starter,MainNode) ->
    register(?SERVER,self()),
    %% write_concurrency here makes otp_8270 break :(
    ?COVER_TABLE = ets:new(?COVER_TABLE, [set, public, named_table
                                          %,{write_concurrency, true}
                                         ]),
    ?COVER_CLAUSE_TABLE = ets:new(?COVER_CLAUSE_TABLE, [set, public,
                                                        named_table]),
    Starter ! {self(),started},
    remote_process_loop(#remote_state{main_node=MainNode}).



remote_process_loop(State) ->
    receive 
	{remote,load_compiled,Compiled} ->
	    Compiled1 = load_compiled(Compiled,State#remote_state.compiled),
	    remote_reply(State#remote_state.main_node, ok),
	    ?MODULE:remote_process_loop(State#remote_state{compiled=Compiled1});

	{remote,unload,UnloadedModules} ->
	    unload(UnloadedModules),
	    Compiled = 
		update_compiled(UnloadedModules, State#remote_state.compiled),
	    remote_reply(State#remote_state.main_node, ok),
	    remote_process_loop(State#remote_state{compiled=Compiled});

	{remote,reset,Module} ->
	    do_reset(Module),
	    remote_reply(State#remote_state.main_node, ok),
	    remote_process_loop(State);

	{remote,collect,Module,CollectorPid} ->
	    self() ! {remote,collect,Module,CollectorPid, ?SERVER};

	{remote,collect,Modules0,CollectorPid,From} ->
	    Modules = case Modules0 of
			  '_' -> [M || {M,_} <- State#remote_state.compiled];
			  _ -> Modules0
		      end,
            spawn(fun() ->
                          ?SPAWN_DBG(remote_collect, 
                                     {Modules, CollectorPid, From}),
                          do_collect(Modules, CollectorPid, From)
                  end),
	    remote_process_loop(State);

	{remote,stop} ->
	    reload_originals(State#remote_state.compiled),
	    ets:delete(?COVER_TABLE),
            ets:delete(?COVER_CLAUSE_TABLE),
            unregister(?SERVER),
	    ok; % not replying since 'DOWN' message will be received anyway

	{remote,get_compiled} ->
	    remote_reply(State#remote_state.main_node,
			 State#remote_state.compiled),
	    remote_process_loop(State);

	{From, get_main_node} ->
	    remote_reply(From, State#remote_state.main_node),
	    remote_process_loop(State);

	get_status ->
	    io:format("~tp~n",[State]),
	    remote_process_loop(State);

	M ->
	    io:format("WARNING: remote cover_server received\n~p\n",[M]),
	    case M of
		{From,_} ->
		    case is_from(From) of
			true ->
			    reply(From,{error,not_main_node});
		        false ->
			    ok
		    end;
		_ ->
		    ok
	    end,
	    remote_process_loop(State)
	    
    end.

do_collect(Modules, CollectorPid, From) ->
    _ = pmap(
          fun(Module) ->
                  Pattern = {#bump{module=Module, _='_'}, '$1'},
                  MatchSpec = [{Pattern,[{'=/=','$1',0}],['$_']}],
                  Match = ets:select(?COVER_TABLE,MatchSpec,?CHUNK_SIZE),
                  send_chunks(Match, CollectorPid, [])
          end,Modules),
    CollectorPid ! done,
    remote_reply(From, ok).

send_chunks('$end_of_table', _CollectorPid, Mons) ->
    get_downs(Mons);
send_chunks({Chunk,Continuation}, CollectorPid, Mons) ->
    Mon = spawn_monitor(
	    fun() ->
		    lists:foreach(fun({Bump,_N}) ->
					  ets:insert(?COVER_TABLE, {Bump,0})
				  end,
				  Chunk) end),
    send_chunk(CollectorPid,Chunk),
    send_chunks(ets:select(Continuation), CollectorPid, [Mon|Mons]).

send_chunk(CollectorPid,Chunk) ->
    CollectorPid ! {chunk,Chunk,self()},
    receive continue -> ok end.

get_downs([]) ->
    ok;
get_downs(Mons) ->
    receive
	{'DOWN', Ref, _Type, Pid, _Reason} = Down ->
	    case lists:member({Pid,Ref},Mons) of
		true ->
		    get_downs(lists:delete({Pid,Ref},Mons));
		false ->
		    %% This should be handled somewhere else
		    self() ! Down,
		    get_downs(Mons)
	    end
    end.

reload_originals(Compiled) ->
    _ = pmap(fun do_reload_original/1, [M || {M,_} <- Compiled]),
    ok.

do_reload_original(Module) ->
    case code:which(Module) of
	?TAG ->
	    _ = code:purge(Module),     % remove code marked as 'old'
	    _ = code:delete(Module),    % mark cover compiled code as 'old'
	    %% Note: original beam code must be loaded before the cover
	    %% compiled code is purged, in order to for references to
	    %% 'fun M:F/A' and %% 'fun F/A' funs to be correct (they
	    %% refer to (M:)F/A in the *latest* version  of the module)
            _ = code:load_file(Module), % load original code
	    _ = code:purge(Module);     % remove cover compiled code
	_ ->
	    ignore
    end.

load_compiled([{Module,File,Binary,InitialTable}|Compiled],Acc) ->
    %% Make sure the #bump{} records are available *before* the
    %% module is loaded.
    insert_initial_data(InitialTable),
    Sticky = case code:is_sticky(Module) of
                 true ->
                     code:unstick_mod(Module),
                     true;
                 false ->
                     false
             end,
    NewAcc = case code:load_binary(Module, ?TAG, Binary) of
                 {module,Module} ->
                     add_compiled(Module, File, Acc);
                 _  ->
                     do_clear(Module),
                     Acc
             end,
    case Sticky of
        true -> code:stick_mod(Module);
        false -> ok
    end,
    load_compiled(Compiled,NewAcc);
load_compiled([],Acc) ->
    Acc.

insert_initial_data([Item|Items]) when is_atom(element(1,Item)) ->
    ets:insert(?COVER_CLAUSE_TABLE, Item),
    insert_initial_data(Items);
insert_initial_data([Item|Items]) ->
    ets:insert(?COVER_TABLE, Item),
    insert_initial_data(Items);
insert_initial_data([]) ->
    ok.
    

unload([Module|Modules]) ->
    do_clear(Module),
    do_reload_original(Module),
    unload(Modules);
unload([]) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%%--Handling of remote nodes--------------------------------------------

do_start_nodes(Nodes, State) ->
    ThisNode = node(),
    StartedNodes =
	lists:foldl(
	  fun(Node,Acc) ->
		  case rpc:call(Node,cover,remote_start,[ThisNode]) of
		      {ok,_RPid} ->
			  erlang:monitor(process,{?SERVER,Node}),
			  [Node|Acc];
		      Error ->
			  io:format("Could not start cover on ~w: ~tp\n",
				    [Node,Error]),
			  Acc
		  end
	  end,
	  [],
	  Nodes),

    %% In case some of the compiled modules have been unloaded they
    %% should not be loaded on the new node.
    {_LoadedModules,Compiled} =
	get_compiled_still_loaded(State#main_state.nodes,
				  State#main_state.compiled),
    remote_load_compiled(StartedNodes,Compiled),

    State1 =
	State#main_state{nodes = State#main_state.nodes ++ StartedNodes,
			 compiled = Compiled},
    {StartedNodes, State1}.

%% start the cover_server on a remote node
remote_start(MainNode) ->
    case whereis(?SERVER) of
	undefined ->
	    Starter = self(),
	    Pid = spawn(fun() -> 
				?SPAWN_DBG(remote_start,{MainNode}),
				init_remote(Starter,MainNode) 
			end),
	    Ref = erlang:monitor(process,Pid),
	    Return = 
		receive 
		    {Pid,started} -> 
			{ok,Pid};
		    {'DOWN', Ref, _Type, _Object, Info} -> 
			{error,Info}
		end,
	    erlang:demonitor(Ref),
	    Return;
	Pid ->
	    {error,{already_started,Pid}}
    end.

%% If a lost node comes back, ensure that main and remote node has the
%% same cover compiled modules. Note that no action is taken if the
%% same {Mod,File} eksists on both, i.e. code change is not handled!
sync_compiled(Node,State) ->
    #main_state{compiled=Compiled0,nodes=Nodes,lost_nodes=Lost}=State,
    State1 =
	case remote_call(Node,{remote,get_compiled}) of
	    {error,node_dead} ->
		{_,S} = do_start_nodes([Node],State),
		S;
	    {error,_} ->
		State;
	    RemoteCompiled ->
		{_,Compiled} =  get_compiled_still_loaded(Nodes,Compiled0),
		Unload = [UM || {UM,_}=U <- RemoteCompiled,
			       false == lists:member(U,Compiled)],
		remote_unload([Node],Unload),
		Load = [L || L <- Compiled,
			     false == lists:member(L,RemoteCompiled)],
		remote_load_compiled([Node],Load),
		State#main_state{compiled=Compiled, nodes=[Node|Nodes]}
	end,
    State1#main_state{lost_nodes=Lost--[Node]}.

%% Load a set of cover compiled modules on remote nodes,
%% We do it ?MAX_MODS modules at a time so that we don't
%% run out of memory on the cover_server node. 
-define(MAX_MODS, 10).
remote_load_compiled(Nodes,Compiled) ->
    remote_load_compiled(Nodes, Compiled, [], 0).
remote_load_compiled(_Nodes, [], [], _ModNum) ->
    ok;
remote_load_compiled(Nodes, Compiled, Acc, ModNum) 
  when Compiled == []; ModNum == ?MAX_MODS ->
    RemoteLoadData = get_downs_r(Acc),
    lists:foreach(
      fun(Node) -> 
	      remote_call(Node,{remote,load_compiled,RemoteLoadData})
      end,
      Nodes),
    remote_load_compiled(Nodes, Compiled, [], 0);
remote_load_compiled(Nodes, [MF | Rest], Acc, ModNum) ->
    remote_load_compiled(
      Nodes, Rest,
      [spawn_job_r(fun() -> get_data_for_remote_loading(MF) end) | Acc],
      ModNum + 1).

spawn_job_r(Fun) ->
    spawn_monitor(fun() -> exit(Fun()) end).

get_downs_r([]) ->
    [];
get_downs_r(Mons) ->
    receive
	{'DOWN', Ref, _Type, Pid, R={_,_,_,_}} ->
	    [R|get_downs_r(lists:delete({Pid,Ref},Mons))];
	{'DOWN', Ref, _Type, Pid, Reason} = Down ->
	    case lists:member({Pid,Ref},Mons) of
		true ->
		    %% Something went really wrong - don't hang!
		    exit(Reason);
		false ->
		    %% This should be handled somewhere else
		    self() ! Down,
		    get_downs_r(Mons)
	    end
    end.


%% Read all data needed for loading a cover compiled module on a remote node
%% Binary is the beam code for the module and InitialTable is the initial
%% data to insert in ?COVER_TABLE.
get_data_for_remote_loading({Module,File}) ->
    [{Module,Binary}] = ets:lookup(?BINARY_TABLE,Module),
    %%! The InitialTable list will be long if the module is big - what to do??
    InitialBumps = ets:select(?COVER_TABLE,ms(Module)),
    InitialClauses = ets:lookup(?COVER_CLAUSE_TABLE,Module),

    {Module,File,Binary,InitialBumps ++ InitialClauses}.

%% Create a match spec which returns the clause info {Module,InitInfo} and 
%% all #bump keys for the given module with 0 number of calls.
ms(Module) ->
    ets:fun2ms(fun({Key,_}) when Key#bump.module=:=Module -> 
		       {Key,0}
	       end).

%% Unload modules on remote nodes
remote_unload(Nodes,UnloadedModules) ->
    lists:foreach(
      fun(Node) -> 
	      remote_call(Node,{remote,unload,UnloadedModules})
      end,
      Nodes).    

%% Reset one or all modules on remote nodes
remote_reset(Module,Nodes) ->
    lists:foreach(
      fun(Node) -> 
	      remote_call(Node,{remote,reset,Module})
      end,
      Nodes).        

%% Collect data from remote nodes - used for analyse or stop(Node)
remote_collect(Modules,Nodes,Stop) ->
    _ = pmap(
          fun(Node) -> 
                  ?SPAWN_DBG(remote_collect, 
                             {Modules, Nodes, Stop}),
                  do_collection(Node, Modules, Stop)
          end, Nodes),
    ok.

do_collection(Node, Module, Stop) ->
    CollectorPid = spawn(fun collector_proc/0),
    case remote_call(Node,{remote,collect,Module,CollectorPid, self()}) of
	{error,node_dead} ->
	    CollectorPid ! done,
	    ok;
	ok when Stop ->
	    remote_call(Node,{remote,stop});
	ok ->
	    ok
    end.

%% Process which receives chunks of data from remote nodes - either when
%% analysing or when stopping cover on the remote nodes.
collector_proc() ->
    ?SPAWN_DBG(collector_proc, []),
    receive 
	{chunk,Chunk,From} ->
	    insert_in_collection_table(Chunk),
	    From ! continue,
	    collector_proc();
	done ->
	    ok
    end.

insert_in_collection_table([{Key,Val}|Chunk]) ->
    insert_in_collection_table(Key,Val),
    insert_in_collection_table(Chunk);
insert_in_collection_table([]) ->
    ok.

insert_in_collection_table(Key,Val) ->
    case ets:member(?COLLECTION_TABLE,Key) of
	true ->
	    _ = ets:update_counter(?COLLECTION_TABLE, Key,Val),
            ok;
	false ->
	    %% Make sure that there are no race conditions from ets:member
	    case ets:insert_new(?COLLECTION_TABLE,{Key,Val}) of
		false ->
		    insert_in_collection_table(Key,Val);
		_ ->
		    ok
	    end
    end.


remove_myself([Node|Nodes],Acc) when Node=:=node() ->
    remove_myself(Nodes,Acc);
remove_myself([Node|Nodes],Acc) ->
    remove_myself(Nodes,[Node|Acc]);
remove_myself([],Acc) ->
    Acc.

%%%--Handling of modules state data--------------------------------------

analyse_info(_Module,[]) ->
    ok;
analyse_info(Module,Imported) ->
    imported_info("Analysis",Module,Imported).

export_info(_Module,[]) ->
    ok;
export_info(_Module,_Imported) ->
    %% Do not print that the export includes imported modules
    ok.

export_info([]) ->
    ok;
export_info(_Imported) ->
    %% Do not print that the export includes imported modules
    ok.

get_all_importfiles([{_M,_F,ImportFiles}|Imported],Acc) ->
    NewAcc = do_get_all_importfiles(ImportFiles,Acc),
    get_all_importfiles(Imported,NewAcc);
get_all_importfiles([],Acc) ->
    Acc.

do_get_all_importfiles([ImportFile|ImportFiles],Acc) ->
    case lists:member(ImportFile,Acc) of
	true ->
	    do_get_all_importfiles(ImportFiles,Acc);
	false ->
	    do_get_all_importfiles(ImportFiles,[ImportFile|Acc])
    end;
do_get_all_importfiles([],Acc) ->
    Acc.

imported_info(Text,Module,Imported) ->
    case lists:keysearch(Module,1,Imported) of
	{value,{Module,_File,ImportFiles}} ->
	    io:format("~ts includes data from imported files\n~tp\n",
		      [Text,ImportFiles]);
	false ->
	    ok
    end.
    
    

add_imported(Module, File, ImportFile, Imported) ->
    add_imported(Module, File, filename:absname(ImportFile), Imported, []).

add_imported(M, F1, ImportFile, [{M,_F2,ImportFiles}|Imported], Acc) ->
    case lists:member(ImportFile,ImportFiles) of
	true ->
	    io:fwrite("WARNING: Module ~w already imported from ~tp~n"
		      "Not importing again!~n",[M,ImportFile]),
	    dont_import;
	false ->
	    NewEntry = {M, F1, [ImportFile | ImportFiles]},
	    {ok, lists:reverse([NewEntry | Acc]) ++ Imported}
    end;
add_imported(M, F, ImportFile, [H|Imported], Acc) ->
    add_imported(M, F, ImportFile, Imported, [H|Acc]);
add_imported(M, F, ImportFile, [], Acc) ->
    {ok, lists:reverse([{M, F, [ImportFile]} | Acc])}.
    
%% Removes a module from the list of imported modules and writes a warning
%% This is done when a module is compiled.
remove_imported(Module,Imported) ->
    case lists:keysearch(Module,1,Imported) of
	{value,{Module,_,ImportFiles}} ->
	    io:fwrite("WARNING: Deleting data for module ~w imported from~n"
		      "~tp~n",[Module,ImportFiles]),
	    lists:keydelete(Module,1,Imported);
	false ->
	    Imported
    end.

%% Adds information to the list of compiled modules, preserving time order
%% and without adding duplicate entries.
add_compiled(Module, File1, [{Module,_File2}|Compiled]) ->
    [{Module,File1}|Compiled];
add_compiled(Module, File, [H|Compiled]) ->
    [H|add_compiled(Module, File, Compiled)];
add_compiled(Module, File, []) ->
    [{Module,File}].

are_loaded([Module|Modules], State, Loaded, Imported, Error) ->
    try is_loaded(Module,State) of
	{loaded,File} ->
	    are_loaded(Modules, State, [{Module,File}|Loaded], Imported, Error);
	{imported,File,_} ->
	    are_loaded(Modules, State, Loaded, [{Module,File}|Imported], Error)
    catch throw:_ ->
	    are_loaded(Modules, State, Loaded, Imported,
		       [{not_cover_compiled,Module}|Error])
    end;
are_loaded([], _State, Loaded, Imported, Error) ->
    {Loaded, Imported, Error}.

is_loaded(Module, State) ->
    case get_file(Module, State#main_state.compiled) of
	{ok, File} ->
	    case code:which(Module) of
		?TAG -> {loaded, File};
		_ -> throw(unloaded)
	    end;
	false ->
	    case get_file(Module,State#main_state.imported) of
		{ok,File,ImportFiles} ->
		    {imported, File, ImportFiles};
		false ->
		    throw(not_loaded)
	    end
    end.

get_file(Module, [{Module, File}|_T]) ->
    {ok, File};
get_file(Module, [{Module, File, ImportFiles}|_T]) ->
    {ok, File, ImportFiles};
get_file(Module, [_H|T]) ->
    get_file(Module, T);
get_file(_Module, []) ->
    false.

get_beam_file(Module,?TAG,Compiled) ->
    {value,{Module,File}} = lists:keysearch(Module,1,Compiled),
    case filename:extension(File) of
	".erl" -> {error,no_beam};
	".beam" -> {ok,File}
    end;
get_beam_file(_Module,BeamFile,_Compiled) ->
    {ok,BeamFile}.

get_modules(Compiled) ->
    lists:map(fun({Module, _File}) -> Module end, Compiled).

update_compiled([Module|Modules], [{Module,_File}|Compiled]) ->
    update_compiled(Modules, Compiled);
update_compiled(Modules, [H|Compiled]) ->
    [H|update_compiled(Modules, Compiled)];
update_compiled(_Modules, []) ->
    [].

%% Get all compiled modules which are still loaded, and possibly an
%% updated version of the Compiled list.
get_compiled_still_loaded(Nodes,Compiled0) ->
    %% Find all Cover compiled modules which are still loaded
    CompiledModules = get_modules(Compiled0),
    LoadedModules = lists:filter(fun(Module) ->
					 case code:which(Module) of
					     ?TAG -> true;
					     _ -> false
					 end
				 end,
				 CompiledModules),

    %% If some Cover compiled modules have been unloaded, update the database.
    UnloadedModules = CompiledModules--LoadedModules,
    Compiled = 
	case UnloadedModules of
	    [] ->
		Compiled0;
	    _ ->
		lists:foreach(fun(Module) -> do_clear(Module) end,
			      UnloadedModules),
		remote_unload(Nodes,UnloadedModules),
		update_compiled(UnloadedModules, Compiled0)
	end,
    {LoadedModules,Compiled}.


%%%--Compilation---------------------------------------------------------

do_compile_beams(ModsAndFiles, State) ->
    Result0 = pmap(fun({ok,Module,File}) ->
			  do_compile_beam(Module,File,State);
		     (Error) ->
			  Error
		  end,
		  ModsAndFiles),
    Compiled = [{M,F} || {ok,M,F} <- Result0],
    remote_load_compiled(State#main_state.nodes,Compiled),
    fix_state_and_result(Result0,State,[]).

do_compile_beam(Module,BeamFile0,State) ->
    case get_beam_file(Module,BeamFile0,State#main_state.compiled) of
	{ok,BeamFile} ->
	    UserOptions = get_compile_options(Module,BeamFile),
	    case do_compile_beam1(Module,BeamFile,UserOptions) of
		{ok, Module} ->
		    {ok,Module,BeamFile};
		error ->
		    {error, BeamFile};
		{error,Reason} -> % no abstract code or no 'file' attribute
		    {error, {Reason, BeamFile}}
	    end;
	{error,no_beam} ->
	    %% The module has first been compiled from .erl, and now
	    %% someone tries to compile it from .beam
	    {error,{already_cover_compiled,no_beam_found,Module}}
    end.

fix_state_and_result([{ok,Module,BeamFile}|Rest],State,Acc) ->
    Compiled = add_compiled(Module,BeamFile,State#main_state.compiled),
    Imported = remove_imported(Module,State#main_state.imported),
    NewState = State#main_state{compiled=Compiled,imported=Imported},
    fix_state_and_result(Rest,NewState,[{ok,Module}|Acc]);
fix_state_and_result([Error|Rest],State,Acc) ->
    fix_state_and_result(Rest,State,[Error|Acc]);
fix_state_and_result([],State,Acc) ->
    {lists:reverse(Acc),State}.


do_compile(Files, Options, State) ->
    Result0 = pmap(fun(File) ->
			   do_compile(File, Options)
		   end,
		   Files),
    Compiled = [{M,F} || {ok,M,F} <- Result0],
    remote_load_compiled(State#main_state.nodes,Compiled),
    fix_state_and_result(Result0,State,[]).

do_compile(File, Options) ->    
    case do_compile1(File, Options) of
	{ok, Module} ->
	    {ok,Module,File};
	error ->
	    {error,File}
    end.

%% do_compile1(File, Options) -> {ok,Module} | error
do_compile1(File, UserOptions) ->
    Options = [debug_info,binary,report_errors,report_warnings] ++ UserOptions,
    case compile:file(File, Options) of
	{ok, Module, Binary} ->
	    do_compile_beam1(Module,Binary,UserOptions);
	error ->
	    error
    end.

%% Beam is a binary or a .beam file name
do_compile_beam1(Module,Beam,UserOptions) ->
    %% Clear database
    do_clear(Module),
    
    %% Extract the abstract format and insert calls to bump/6 at
    %% every executable line and, as a side effect, initiate
    %% the database
    
    case get_abstract_code(Module, Beam) of
	no_abstract_code=E ->
	    {error,E};
	encrypted_abstract_code=E ->
	    {error,E};
	{raw_abstract_v1,Code} ->
            Forms0 = epp:interpret_file_attribute(Code),
	    case find_main_filename(Forms0) of
		{ok,MainFile} ->
		    do_compile_beam2(Module,Beam,UserOptions,Forms0,MainFile);
		Error ->
		    Error
	    end;
	{_VSN,_Code} ->
	    %% Wrong version of abstract code. Just report that there
	    %% is no abstract code.
	    {error,no_abstract_code}
    end.

get_abstract_code(Module, Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
	{ok, {Module, [{abstract_code, AbstractCode}]}} ->
	    AbstractCode;
	{error,beam_lib,{key_missing_or_invalid,_,_}} ->
	    encrypted_abstract_code;
	Error -> Error
    end.

do_compile_beam2(Module,Beam,UserOptions,Forms0,MainFile) ->
    {Forms,Vars} = transform(Forms0, Module, MainFile),

    %% We need to recover the source from the compilation
    %% info otherwise the newly compiled module will have
    %% source pointing to the current directory
    SourceInfo = get_source_info(Module, Beam),

    %% Compile and load the result
    %% It's necessary to check the result of loading since it may
    %% fail, for example if Module resides in a sticky directory
    {ok, Module, Binary} = compile:forms(Forms, SourceInfo ++ UserOptions),
    case code:load_binary(Module, ?TAG, Binary) of
	{module, Module} ->

	    %% Store info about all function clauses in database
	    InitInfo = lists:reverse(Vars#vars.init_info),
	    ets:insert(?COVER_CLAUSE_TABLE, {Module, InitInfo}),

	    %% Store binary code so it can be loaded on remote nodes
	    ets:insert(?BINARY_TABLE, {Module, Binary}),

	    {ok, Module};

	_Error ->
	    do_clear(Module),
	    error
    end.

get_source_info(Module, Beam) ->
    Compile = get_compile_info(Module, Beam),
    case lists:keyfind(source, 1, Compile) of
        { source, _ } = Tuple -> [Tuple];
        false -> []
    end.

get_compile_options(Module, Beam) ->
    Compile = get_compile_info(Module, Beam),
    case lists:keyfind(options, 1, Compile) of
        {options, Options } -> filter_options(Options);
        false -> []
    end.

get_compile_info(Module, Beam) ->
    case beam_lib:chunks(Beam, [compile_info]) of
	{ok, {Module, [{compile_info, Compile}]}} ->
		Compile;
	_ ->
		[]
    end.

transform(Code, Module, MainFile) ->
    Vars0 = #vars{module=Module},
    {ok,MungedForms,Vars} = transform_2(Code,[],Vars0,MainFile,on),
    {MungedForms,Vars}.
    
%% Helpfunction which returns the first found file-attribute, which can
%% be interpreted as the name of the main erlang source file.
find_main_filename([{attribute,_,file,{MainFile,_}}|_]) ->
    {ok,MainFile};
find_main_filename([_|Rest]) ->
    find_main_filename(Rest);
find_main_filename([]) ->
    {error, no_file_attribute}.


transform_2([Form0|Forms],MungedForms,Vars,MainFile,Switch) ->
    Form = expand(Form0),
    case munge(Form,Vars,MainFile,Switch) of
	ignore ->
	    transform_2(Forms,MungedForms,Vars,MainFile,Switch);
	{MungedForm,Vars2,NewSwitch} ->
	    transform_2(Forms,[MungedForm|MungedForms],Vars2,MainFile,NewSwitch)
    end;
transform_2([],MungedForms,Vars,_,_) ->
    {ok, lists:reverse(MungedForms), Vars}.

%% Expand short-circuit Boolean expressions.
expand(Expr) ->
    AllVars = sets:from_list(ordsets:to_list(vars([], Expr))),
    {Expr1,_} = expand(Expr, AllVars, 1),
    Expr1.

expand({clause,Line,Pattern,Guards,Body}, Vs, N) ->
    {ExpandedBody,N2} = expand(Body, Vs, N),
    {{clause,Line,Pattern,Guards,ExpandedBody},N2};
expand({op,_Line,'andalso',ExprL,ExprR}, Vs, N) ->
    {ExpandedExprL,N2} = expand(ExprL, Vs, N),
    {ExpandedExprR,N3} = expand(ExprR, Vs, N2),
    Anno = element(2, ExpandedExprL),
    {bool_switch(ExpandedExprL, 
                 ExpandedExprR,
                 {atom,Anno,false},
                 Vs, N3),
     N3 + 1};
expand({op,_Line,'orelse',ExprL,ExprR}, Vs, N) ->
    {ExpandedExprL,N2} = expand(ExprL, Vs, N),
    {ExpandedExprR,N3} = expand(ExprR, Vs, N2),
    Anno = element(2, ExpandedExprL),
    {bool_switch(ExpandedExprL,
                 {atom,Anno,true},
                 ExpandedExprR,
                 Vs, N3),
     N3 + 1};
expand(T, Vs, N) when is_tuple(T) ->
    {TL,N2} = expand(tuple_to_list(T), Vs, N),
    {list_to_tuple(TL),N2};
expand([E|Es], Vs, N) ->
    {E2,N2} = expand(E, Vs, N),
    {Es2,N3} = expand(Es, Vs, N2),
    {[E2|Es2],N3};
expand(T, _Vs, N) ->
    {T,N}.

vars(A, {var,_,V}) when V =/= '_' ->
    [V|A];
vars(A, T) when is_tuple(T) ->
    vars(A, tuple_to_list(T));
vars(A, [E|Es]) ->
    vars(vars(A, E), Es);
vars(A, _T) ->
    A.

bool_switch(E, T, F, AllVars, AuxVarN) ->
    Line = element(2, E),
    AuxVar = {var,Line,aux_var(AllVars, AuxVarN)},
    {'case',Line,E,
     [{clause,Line,[{atom,Line,true}],[],[T]},
      {clause,Line,[{atom,Line,false}],[],[F]},
      {clause,Line,[AuxVar],[],
       [{call,Line,
         {remote,Line,{atom,Line,erlang},{atom,Line,error}},
         [{tuple,Line,[{atom,Line,badarg},AuxVar]}]}]}]}.

aux_var(Vars, N) ->
    Name = list_to_atom(lists:concat(['_', N])),
    case sets:is_element(Name, Vars) of
        true -> aux_var(Vars, N + 1);
        false -> Name
    end.

%% This code traverses the abstract code, stored as the abstract_code
%% chunk in the BEAM file, as described in absform(3).
%% The switch is turned off when we encounter other files than the main file.
%% This way we will be able to exclude functions defined in include files.
munge({function,Line,Function,Arity,Clauses},Vars,_MainFile,on) ->
    Vars2 = Vars#vars{function=Function,
		      arity=Arity,
		      clause=1,
		      lines=[],
                      no_bump_lines=[],
		      depth=1},
    {MungedClauses, Vars3} = munge_clauses(Clauses, Vars2),
    {{function,Line,Function,Arity,MungedClauses},Vars3,on};
munge(Form={attribute,_,file,{MainFile,_}},Vars,MainFile,_Switch) ->
    {Form,Vars,on};                     % Switch on tranformation!
munge(Form={attribute,_,file,{_InclFile,_}},Vars,_MainFile,_Switch) ->
    {Form,Vars,off};                    % Switch off transformation!
munge({attribute,_,compile,{parse_transform,_}},_Vars,_MainFile,_Switch) ->
    %% Don't want to run parse transforms more than once.
    ignore;
munge(Form,Vars,_MainFile,Switch) ->    % Other attributes and skipped includes.
    {Form,Vars,Switch}.

munge_clauses(Clauses, Vars) ->
    munge_clauses(Clauses, Vars, Vars#vars.lines, []).

munge_clauses([Clause|Clauses], Vars, Lines, MClauses) ->
    {clause,Line,Pattern,Guards,Body} = Clause,
    {MungedGuards, _Vars} = munge_exprs(Guards, Vars#vars{is_guard=true},[]),

    case Vars#vars.depth of
	1 -> % function clause
	    {MungedBody, Vars2} = munge_body(Body, Vars#vars{depth=2}),
	    ClauseInfo = {Vars2#vars.module,
			  Vars2#vars.function,
			  Vars2#vars.arity,
			  Vars2#vars.clause,
			  length(Vars2#vars.lines)}, % Not used?
	    InitInfo = [ClauseInfo | Vars2#vars.init_info],
	    Vars3 = Vars2#vars{init_info=InitInfo,
			       clause=(Vars2#vars.clause)+1,
			       lines=[],
                               no_bump_lines=[],
			       depth=1},
            NewBumps = Vars2#vars.lines,
            NewLines = NewBumps ++ Lines,
	    munge_clauses(Clauses, Vars3, NewLines,
			  [{clause,Line,Pattern,MungedGuards,MungedBody}|
			   MClauses]);

	2 -> % receive-,  case-, if-, or try-clause
            Lines0 = Vars#vars.lines,
	    {MungedBody, Vars2} = munge_body(Body, Vars),
            NewBumps = new_bumps(Vars2, Vars),
            NewLines = NewBumps ++ Lines,
	    munge_clauses(Clauses, Vars2#vars{lines=Lines0},
                          NewLines,
			  [{clause,Line,Pattern,MungedGuards,MungedBody}|
			   MClauses])
    end;
munge_clauses([], Vars, Lines, MungedClauses) -> 
    {lists:reverse(MungedClauses), Vars#vars{lines = Lines}}.

munge_body(Expr, Vars) ->
    munge_body(Expr, Vars, [], []).

munge_body([Expr|Body], Vars, MungedBody, LastExprBumpLines) ->
    %% Here is the place to add a call to cover:bump/6!
    Line = erl_anno:line(element(2, Expr)),
    Lines = Vars#vars.lines,
    case lists:member(Line,Lines) of
	true -> % already a bump at this line
	    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
            NewBumps = new_bumps(Vars2, Vars),
            NoBumpLines = [Line|Vars#vars.no_bump_lines],
            Vars3 = Vars2#vars{no_bump_lines = NoBumpLines},
            MungedBody1 = 
                maybe_fix_last_expr(MungedBody, Vars3, LastExprBumpLines),
            MungedExprs1 = [MungedExpr|MungedBody1],
	    munge_body(Body, Vars3, MungedExprs1, NewBumps);
	false ->
	    ets:insert(?COVER_TABLE, {#bump{module   = Vars#vars.module,
					    function = Vars#vars.function,
					    arity    = Vars#vars.arity,
					    clause   = Vars#vars.clause,
					    line     = Line},
				      0}),
            Bump = bump_call(Vars, Line),
%	    Bump = {call, 0, {remote, 0, {atom,0,cover}, {atom,0,bump}},
%		    [{atom, 0, Vars#vars.module},
%		     {atom, 0, Vars#vars.function},
%		     {integer, 0, Vars#vars.arity},
%		     {integer, 0, Vars#vars.clause},
%		     {integer, 0, Line}]},
	    Lines2 = [Line|Lines],
	    {MungedExpr, Vars2} = munge_expr(Expr, Vars#vars{lines=Lines2}),
            NewBumps = new_bumps(Vars2, Vars),
            NoBumpLines = subtract(Vars2#vars.no_bump_lines, NewBumps),
            Vars3 = Vars2#vars{no_bump_lines = NoBumpLines},
            MungedBody1 =
                maybe_fix_last_expr(MungedBody, Vars3, LastExprBumpLines),
            MungedExprs1 = [MungedExpr,Bump|MungedBody1],
	    munge_body(Body, Vars3, MungedExprs1, NewBumps)
    end;
munge_body([], Vars, MungedBody, _LastExprBumpLines) ->
    {lists:reverse(MungedBody), Vars}.

%%% Fix last expression (OTP-8188). A typical example:
%%%
%%%  3:   case X of
%%%  4:       1 -> a; % Bump line 5 after "a" has been evaluated!
%%%  5:       2 -> b; 3 -> c end, F()
%%%
%%% Line 5 wasn't bumped just before "F()" since it was already bumped
%%% before "b" (and before "c") (one mustn't bump a line more than
%%% once in a single "evaluation"). The expression "case X ... end" is
%%% now traversed again ("fixed"), this time adding bumps of line 5
%%% where appropriate, in this case when X matches 1.
%%%
%%% This doesn't solve all problems with expressions on the same line,
%%% though. 'case' and 'try' are tricky. An example:
%%%
%%% 7:    case case X of 1 -> foo(); % ?
%%% 8:                   2 -> bar() end of a -> 1;
%%% 9:                                     b -> 2 end.
%%%
%%% If X matches 1 and foo() evaluates to a then line 8 should be
%%% bumped, but not if foo() evaluates to b. In other words, line 8
%%% cannot be bumped after "foo()" on line 7, so one has to bump line
%%% 8 before "begin 1 end". But if X matches 2 and bar evaluates to a
%%% then line 8 would be bumped twice (there has to be a bump before
%%% "bar()". It is like one would have to have two copies of the inner
%%% clauses, one for each outer clause. Maybe the munging should be
%%% done on some of the compiler's "lower level" format.
%%%
%%% 'fun' is also problematic since a bump inside the body "shadows"
%%% the rest of the line.

maybe_fix_last_expr(MungedExprs, Vars, LastExprBumpLines) ->
    case last_expr_needs_fixing(Vars, LastExprBumpLines) of
        {yes, Line} ->
            fix_last_expr(MungedExprs, Line, Vars);
        no ->
            MungedExprs
    end.

last_expr_needs_fixing(Vars, LastExprBumpLines) ->
    case common_elems(Vars#vars.no_bump_lines, LastExprBumpLines) of
        [Line] -> {yes, Line};
        _ -> no
    end.

fix_last_expr([MungedExpr|MungedExprs], Line, Vars) ->
    %% No need to update ?COVER_TABLE.
    Bump = bump_call(Vars, Line),
    [fix_expr(MungedExpr, Line, Bump)|MungedExprs].

fix_expr({'if',L,Clauses}, Line, Bump) -> 
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    {'if',L,FixedClauses};
fix_expr({'case',L,Expr,Clauses}, Line, Bump) ->
    FixedExpr = fix_expr(Expr, Line, Bump),
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    {'case',L,FixedExpr,FixedClauses};
fix_expr({'receive',L,Clauses}, Line, Bump) -> 
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    {'receive',L,FixedClauses};
fix_expr({'receive',L,Clauses,Expr,Body}, Line, Bump) ->
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    FixedExpr = fix_expr(Expr, Line, Bump),
    FixedBody = fix_expr(Body, Line, Bump),
    {'receive',L,FixedClauses,FixedExpr,FixedBody};
fix_expr({'try',L,Exprs,Clauses,CatchClauses,After}, Line, Bump) ->
    FixedExprs = fix_expr(Exprs, Line, Bump),
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    FixedCatchClauses = fix_clauses(CatchClauses, Line, Bump),
    FixedAfter = fix_expr(After, Line, Bump),
    {'try',L,FixedExprs,FixedClauses,FixedCatchClauses,FixedAfter};
fix_expr([E | Es], Line, Bump) ->
    [fix_expr(E, Line, Bump) | fix_expr(Es, Line, Bump)];
fix_expr(T, Line, Bump) when is_tuple(T) ->
    list_to_tuple(fix_expr(tuple_to_list(T), Line, Bump));
fix_expr(E, _Line, _Bump) ->
    E.

fix_clauses([], _Line, _Bump) ->
    [];
fix_clauses(Cs, Line, Bump) ->
    case bumps_line(lists:last(Cs), Line) of
        true ->
            fix_cls(Cs, Line, Bump);
        false ->
            Cs
    end.

fix_cls([], _Line, _Bump) ->
    [];
fix_cls([Cl | Cls], Line, Bump) ->
    case bumps_line(Cl, Line) of
        true ->
            [fix_expr(C, Line, Bump) || C <- [Cl | Cls]];
        false ->
            {clause,CL,P,G,Body} = Cl,
            UniqueVarName = list_to_atom(lists:concat(["$cover$ ",Line])),
            A = erl_anno:new(0),
            V = {var,A,UniqueVarName},
            [Last|Rest] = lists:reverse(Body),
            Body1 = lists:reverse(Rest, [{match,A,V,Last},Bump,V]),
            [{clause,CL,P,G,Body1} | fix_cls(Cls, Line, Bump)]
    end.

bumps_line(E, L) ->
    try bumps_line1(E, L) catch true -> true end.

bumps_line1({call,_,{remote,_,{atom,_,ets},{atom,_,update_counter}},
             [{atom,_,?COVER_TABLE},{tuple,_,[_,_,_,_,_,{integer,_,Line}]},_]},
            Line) ->
    throw(true);
bumps_line1([E | Es], Line) ->
    bumps_line1(E, Line),
    bumps_line1(Es, Line);
bumps_line1(T, Line) when is_tuple(T) ->
    bumps_line1(tuple_to_list(T), Line);
bumps_line1(_, _) ->
    false.

%%% End of fix of last expression.

bump_call(Vars, Line) ->
    A = erl_anno:new(0),
    {call,A,{remote,A,{atom,A,ets},{atom,A,update_counter}},
     [{atom,A,?COVER_TABLE},
      {tuple,A,[{atom,A,?BUMP_REC_NAME},
                {atom,A,Vars#vars.module},
                {atom,A,Vars#vars.function},
                {integer,A,Vars#vars.arity},
                {integer,A,Vars#vars.clause},
                {integer,A,Line}]},
      {integer,A,1}]}.

munge_expr({match,Line,ExprL,ExprR}, Vars) ->
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{match,Line,MungedExprL,MungedExprR}, Vars3};
munge_expr({tuple,Line,Exprs}, Vars) ->
    {MungedExprs, Vars2} = munge_exprs(Exprs, Vars, []),
    {{tuple,Line,MungedExprs}, Vars2};
munge_expr({record,Line,Name,Exprs}, Vars) ->
    {MungedExprFields, Vars2} = munge_exprs(Exprs, Vars, []),
    {{record,Line,Name,MungedExprFields}, Vars2};
munge_expr({record,Line,Arg,Name,Exprs}, Vars) ->
    {MungedArg, Vars2} = munge_expr(Arg, Vars),
    {MungedExprFields, Vars3} = munge_exprs(Exprs, Vars2, []),
    {{record,Line,MungedArg,Name,MungedExprFields}, Vars3};
munge_expr({record_field,Line,ExprL,ExprR}, Vars) ->
    {MungedExprR, Vars2} = munge_expr(ExprR, Vars),
    {{record_field,Line,ExprL,MungedExprR}, Vars2};
munge_expr({map,Line,Fields}, Vars) ->
    %% EEP 43
    {MungedFields, Vars2} = munge_exprs(Fields, Vars, []),
    {{map,Line,MungedFields}, Vars2};
munge_expr({map,Line,Arg,Fields}, Vars) ->
    %% EEP 43
    {MungedArg, Vars2} = munge_expr(Arg, Vars),
    {MungedFields, Vars3} = munge_exprs(Fields, Vars2, []),
    {{map,Line,MungedArg,MungedFields}, Vars3};
munge_expr({map_field_assoc,Line,Name,Value}, Vars) ->
    %% EEP 43
    {MungedName, Vars2} = munge_expr(Name, Vars),
    {MungedValue, Vars3} = munge_expr(Value, Vars2),
    {{map_field_assoc,Line,MungedName,MungedValue}, Vars3};
munge_expr({map_field_exact,Line,Name,Value}, Vars) ->
    %% EEP 43
    {MungedName, Vars2} = munge_expr(Name, Vars),
    {MungedValue, Vars3} = munge_expr(Value, Vars2),
    {{map_field_exact,Line,MungedName,MungedValue}, Vars3};
munge_expr({cons,Line,ExprH,ExprT}, Vars) ->
    {MungedExprH, Vars2} = munge_expr(ExprH, Vars),
    {MungedExprT, Vars3} = munge_expr(ExprT, Vars2),
    {{cons,Line,MungedExprH,MungedExprT}, Vars3};
munge_expr({op,Line,Op,ExprL,ExprR}, Vars) ->
    {MungedExprL, Vars2} = munge_expr(ExprL, Vars),
    {MungedExprR, Vars3} = munge_expr(ExprR, Vars2),
    {{op,Line,Op,MungedExprL,MungedExprR}, Vars3};
munge_expr({op,Line,Op,Expr}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {{op,Line,Op,MungedExpr}, Vars2};
munge_expr({'catch',Line,Expr}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {{'catch',Line,MungedExpr}, Vars2};
munge_expr({call,Line1,{remote,Line2,ExprM,ExprF},Exprs},
	   Vars) ->
    {MungedExprM, Vars2} = munge_expr(ExprM, Vars),
    {MungedExprF, Vars3} = munge_expr(ExprF, Vars2),
    {MungedExprs, Vars4} = munge_exprs(Exprs, Vars3, []),
    {{call,Line1,{remote,Line2,MungedExprM,MungedExprF},MungedExprs}, Vars4};
munge_expr({call,Line,Expr,Exprs}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    {MungedExprs, Vars3} = munge_exprs(Exprs, Vars2, []),
    {{call,Line,MungedExpr,MungedExprs}, Vars3};
munge_expr({lc,Line,Expr,Qs}, Vars) ->
    {MungedExpr, Vars2} = munge_expr(?BLOCK1(Expr), Vars),
    {MungedQs, Vars3} = munge_qualifiers(Qs, Vars2),
    {{lc,Line,MungedExpr,MungedQs}, Vars3};
munge_expr({bc,Line,Expr,Qs}, Vars) ->
    {MungedExpr,Vars2} = munge_expr(?BLOCK1(Expr), Vars),
    {MungedQs, Vars3} = munge_qualifiers(Qs, Vars2),
    {{bc,Line,MungedExpr,MungedQs}, Vars3};
munge_expr({block,Line,Body}, Vars) ->
    {MungedBody, Vars2} = munge_body(Body, Vars),
    {{block,Line,MungedBody}, Vars2};
munge_expr({'if',Line,Clauses}, Vars) -> 
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars),
    {{'if',Line,MungedClauses}, Vars2};
munge_expr({'case',Line,Expr,Clauses}, Vars) ->
    {MungedExpr,Vars2} = munge_expr(Expr, Vars),
    {MungedClauses,Vars3} = munge_clauses(Clauses, Vars2),
    {{'case',Line,MungedExpr,MungedClauses}, Vars3};
munge_expr({'receive',Line,Clauses}, Vars) -> 
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars),
    {{'receive',Line,MungedClauses}, Vars2};
munge_expr({'receive',Line,Clauses,Expr,Body}, Vars) ->
    {MungedExpr, Vars1} = munge_expr(Expr, Vars),
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars1),
    {MungedBody,Vars3} = 
        munge_body(Body, Vars2#vars{lines = Vars1#vars.lines}),
    Vars4 = Vars3#vars{lines = Vars2#vars.lines ++ new_bumps(Vars3, Vars2)},
    {{'receive',Line,MungedClauses,MungedExpr,MungedBody}, Vars4};
munge_expr({'try',Line,Body,Clauses,CatchClauses,After}, Vars) ->
    {MungedBody, Vars1} = munge_body(Body, Vars),
    {MungedClauses, Vars2} = munge_clauses(Clauses, Vars1),
    {MungedCatchClauses, Vars3} = munge_clauses(CatchClauses, Vars2),
    {MungedAfter, Vars4} = munge_body(After, Vars3),
    {{'try',Line,MungedBody,MungedClauses,MungedCatchClauses,MungedAfter}, 
     Vars4};
munge_expr({'fun',Line,{clauses,Clauses}}, Vars) ->
    {MungedClauses,Vars2}=munge_clauses(Clauses, Vars),
    {{'fun',Line,{clauses,MungedClauses}}, Vars2};
munge_expr({named_fun,Line,Name,Clauses}, Vars) ->
    {MungedClauses,Vars2}=munge_clauses(Clauses, Vars),
    {{named_fun,Line,Name,MungedClauses}, Vars2};
munge_expr({bin,Line,BinElements}, Vars) ->
    {MungedBinElements,Vars2} = munge_exprs(BinElements, Vars, []),
    {{bin,Line,MungedBinElements}, Vars2};
munge_expr({bin_element,Line,Value,Size,TypeSpecifierList}, Vars) ->
    {MungedValue,Vars2} = munge_expr(Value, Vars),
    {MungedSize,Vars3} = munge_expr(Size, Vars2),
    {{bin_element,Line,MungedValue,MungedSize,TypeSpecifierList},Vars3};
munge_expr(Form, Vars) ->
    {Form, Vars}.

munge_exprs([Expr|Exprs], Vars, MungedExprs) when Vars#vars.is_guard=:=true,
						  is_list(Expr) ->
    {MungedExpr, _Vars} = munge_exprs(Expr, Vars, []),
    munge_exprs(Exprs, Vars, [MungedExpr|MungedExprs]);
munge_exprs([Expr|Exprs], Vars, MungedExprs) ->
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_exprs(Exprs, Vars2, [MungedExpr|MungedExprs]);
munge_exprs([], Vars, MungedExprs) ->
    {lists:reverse(MungedExprs), Vars}.

%% Every qualifier is decorated with a counter.
munge_qualifiers(Qualifiers, Vars) ->
    munge_qs(Qualifiers, Vars, []).

munge_qs([{generate,Line,Pattern,Expr}|Qs], Vars, MQs) ->
    L = element(2, Expr),
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_qs1(Qs, L, {generate,Line,Pattern,MungedExpr}, Vars, Vars2, MQs);
munge_qs([{b_generate,Line,Pattern,Expr}|Qs], Vars, MQs) ->
    L = element(2, Expr),
    {MExpr, Vars2} = munge_expr(Expr, Vars),
    munge_qs1(Qs, L, {b_generate,Line,Pattern,MExpr}, Vars, Vars2, MQs);
munge_qs([Expr|Qs], Vars, MQs) ->
    L = element(2, Expr),
    {MungedExpr, Vars2} = munge_expr(Expr, Vars),
    munge_qs1(Qs, L, MungedExpr, Vars, Vars2, MQs);
munge_qs([], Vars, MQs) ->
    {lists:reverse(MQs), Vars}.

munge_qs1(Qs, Line, NQ, Vars, Vars2, MQs) ->
    case new_bumps(Vars2, Vars) of
        [_] ->
            munge_qs(Qs, Vars2, [NQ | MQs]);
        _ -> 
            {MungedTrue, Vars3} = munge_expr(?BLOCK({atom,Line,true}), Vars2),
            munge_qs(Qs, Vars3, [NQ, MungedTrue | MQs])
    end.

new_bumps(#vars{lines = New}, #vars{lines = Old}) ->
    subtract(New, Old).

subtract(L1, L2) ->
    [E || E <- L1, not lists:member(E, L2)].

common_elems(L1, L2) ->
    [E || E <- L1, lists:member(E, L2)].

%%%--Analysis------------------------------------------------------------

%% Collect data for all modules
collect(Nodes) ->
    %% local node
    AllClauses = ets:tab2list(?COVER_CLAUSE_TABLE),
    Mon1 = spawn_monitor(fun() -> pmap(fun move_modules/1,AllClauses) end),

    %% remote nodes
    Mon2 = spawn_monitor(fun() -> remote_collect('_',Nodes,false) end),
    get_downs([Mon1,Mon2]).

%% Collect data for a list of modules
collect(Modules,Nodes) ->
    MS = [{{'$1','_'},[{'==','$1',M}],['$_']} || M <- Modules],
    Clauses = ets:select(?COVER_CLAUSE_TABLE,MS),
    Mon1 = spawn_monitor(fun() -> pmap(fun move_modules/1,Clauses) end),

    %% remote nodes
    Mon2 = spawn_monitor(fun() -> remote_collect('_',Nodes,false) end),
    get_downs([Mon1,Mon2]).

%% Collect data for one module
collect(Module,Clauses,Nodes) ->
    %% local node
    move_modules({Module,Clauses}),
    
    %% remote nodes
    remote_collect([Module],Nodes,false).


%% When analysing, the data from the local ?COVER_TABLE is moved to the
%% ?COLLECTION_TABLE. Resetting data in ?COVER_TABLE
move_modules({Module,Clauses}) ->
    ets:insert(?COLLECTION_CLAUSE_TABLE,{Module,Clauses}),
    Pattern = {#bump{module=Module, _='_'}, '_'},
    MatchSpec = [{Pattern,[],['$_']}],
    Match = ets:select(?COVER_TABLE,MatchSpec,?CHUNK_SIZE),
    do_move_module(Match).
    
do_move_module({Bumps,Continuation}) ->
    lists:foreach(fun({Key,Val}) ->
			  ets:insert(?COVER_TABLE, {Key,0}),
			  insert_in_collection_table(Key,Val)
		  end,
		  Bumps),
    do_move_module(ets:select(Continuation));
do_move_module('$end_of_table') ->
    ok.

%% Given a .beam file, find the .erl file. Look first in same directory as
%% the .beam file, then in ../src, then in compile info.
find_source(Module, File0) ->
    try
        Root = filename:rootname(File0, ".beam"),
        Root == File0 andalso throw(File0),  %% not .beam
        %% Look for .erl in pwd.
        File = Root ++ ".erl",
        throw_file(File),
        %% Not in pwd: look in ../src.
        BeamDir = filename:dirname(File),
        Base = filename:basename(File),
        throw_file(filename:join([BeamDir, "..", "src", Base])),
        %% Not in ../src: look for source path in compile info, but
        %% first look relative the beam directory.
        Info =
            try lists:keyfind(source, 1, Module:module_info(compile))
            catch error:undef ->
                    %% The module might have been imported
                    %% and the beam not available
                    throw({beam, File0})
            end,
        false == Info andalso throw({beam, File0}),  %% stripped
        {source, SrcFile} = Info,
        throw_file(splice(BeamDir, SrcFile)),  %% below ../src
        throw_file(SrcFile),                   %% or absolute
        %% No success means that source is either not under ../src or
        %% its relative path differs from that of compile info. (For
        %% example, compiled under src/x but installed under src/y.)
        %% An option to specify an arbitrary source path explicitly is
        %% probably a better solution than either more heuristics or a
        %% potentially slow filesystem search.
        {beam, File0}
    catch
        Path -> Path
    end.

throw_file(Path) ->
    false /= Path andalso filelib:is_file(Path) andalso throw(Path).

%% Splice the tail of a source path, starting from the last "src"
%% component, onto the parent of a beam directory, or return false if
%% no "src" component is found.
%%
%% Eg. splice("/path/to/app-1.0/ebin", "/compiled/path/to/app/src/x/y.erl")
%%        --> "/path/to/app-1.0/ebin/../src/x/y.erl"
%%
%% This handles the case of source in subdirectories of ../src with
%% beams that have moved since compilation.
%%
splice(BeamDir, SrcFile) ->
    case lists:splitwith(fun(C) -> C /= "src" end, revsplit(SrcFile)) of
        {T, [_|_]} ->  %% found src component
            filename:join([BeamDir, "..", "src" | lists:reverse(T)]);
        {_, []} ->     %% or not
            false
    end.

revsplit(Path) ->
    lists:reverse(filename:split(Path)).

analyse_list(Modules, Analysis, Level, State) ->
    {LoadedMF, ImportedMF, Error} = are_loaded(Modules, State, [], [], []),
    Loaded = [M || {M,_} <- LoadedMF],
    Imported = [M || {M,_} <- ImportedMF],
    collect(Loaded, State#main_state.nodes),
    MS = [{{'$1','_'},[{'==','$1',M}],['$_']} || M <- Loaded ++ Imported],
    AllClauses = ets:select(?COLLECTION_CLAUSE_TABLE,MS),
    Fun = fun({Module,Clauses}) ->
		  do_analyse(Module, Analysis, Level, Clauses)
	  end,
    {result, lists:flatten(pmap(Fun, AllClauses)), Error}.

analyse_all(Analysis, Level, State) ->
    collect(State#main_state.nodes),
    AllClauses = ets:tab2list(?COLLECTION_CLAUSE_TABLE),
    Fun = fun({Module,Clauses}) ->
		  do_analyse(Module, Analysis, Level, Clauses)
	  end,
    {result, lists:flatten(pmap(Fun, AllClauses)), []}.

do_parallel_analysis(Module, Analysis, Level, Loaded, From, State) ->
    analyse_info(Module,State#main_state.imported),
    C = case Loaded of
	    {loaded, _File} ->
		[{Module,Clauses}] = 
		    ets:lookup(?COVER_CLAUSE_TABLE,Module),
		collect(Module,Clauses,State#main_state.nodes),
		Clauses;
	    _ ->
		[{Module,Clauses}] = 
		    ets:lookup(?COLLECTION_CLAUSE_TABLE,Module),
		Clauses
	end,
    R = do_analyse(Module, Analysis, Level, C),
    reply(From, {ok,R}).

%% do_analyse(Module, Analysis, Level, Clauses)-> {ok,Answer} | {error,Error}
%%   Clauses = [{Module,Function,Arity,Clause,Lines}]
do_analyse(Module, Analysis, line, _Clauses) ->
    Pattern = {#bump{module=Module},'_'},
    Bumps = ets:match_object(?COLLECTION_TABLE, Pattern),
    Fun = case Analysis of
	      coverage ->
		  fun({#bump{line=L}, 0}) ->
			  {{Module,L}, {0,1}};
		     ({#bump{line=L}, _N}) ->
			  {{Module,L}, {1,0}}
		  end;
	      calls ->
		  fun({#bump{line=L}, N}) ->
			  {{Module,L}, N}
		  end
	  end,
    lists:keysort(1, lists:map(Fun, Bumps));
do_analyse(Module, Analysis, clause, _Clauses) ->
    Pattern = {#bump{module=Module},'_'},
    Bumps = lists:keysort(1,ets:match_object(?COLLECTION_TABLE, Pattern)),
    analyse_clause(Analysis,Bumps);
do_analyse(Module, Analysis, function, Clauses) ->
    ClauseResult = do_analyse(Module, Analysis, clause, Clauses),
    merge_clauses(ClauseResult, merge_fun(Analysis));
do_analyse(Module, Analysis, module, Clauses) ->
    FunctionResult = do_analyse(Module, Analysis, function, Clauses),
    Result = merge_functions(FunctionResult, merge_fun(Analysis)),
    {Module,Result}.

analyse_clause(_,[]) ->
    [];
analyse_clause(coverage,
	       [{#bump{module=M,function=F,arity=A,clause=C},_}|_]=Bumps) ->
    analyse_clause_cov(Bumps,{M,F,A,C},0,0,[]);
analyse_clause(calls,Bumps) ->
    analyse_clause_calls(Bumps,{x,x,x,x},[]).

analyse_clause_cov([{#bump{module=M,function=F,arity=A,clause=C},N}|Bumps],
		   {M,F,A,C}=Clause,Ls,NotCov,Acc) ->
    analyse_clause_cov(Bumps,Clause,Ls+1,if N==0->NotCov+1; true->NotCov end,Acc);
analyse_clause_cov([{#bump{module=M1,function=F1,arity=A1,clause=C1},_}|_]=Bumps,
		    Clause,Ls,NotCov,Acc) ->
    analyse_clause_cov(Bumps,{M1,F1,A1,C1},0,0,[{Clause,{Ls-NotCov,NotCov}}|Acc]);
analyse_clause_cov([],Clause,Ls,NotCov,Acc) ->
    lists:reverse(Acc,[{Clause,{Ls-NotCov,NotCov}}]).

analyse_clause_calls([{#bump{module=M,function=F,arity=A,clause=C},_}|Bumps],
		     {M,F,A,C}=Clause,Acc) ->
    analyse_clause_calls(Bumps,Clause,Acc);
analyse_clause_calls([{#bump{module=M1,function=F1,arity=A1,clause=C1},N}|Bumps],
		     _Clause,Acc) ->
    analyse_clause_calls(Bumps,{M1,F1,A1,C1},[{{M1,F1,A1,C1},N}|Acc]);
analyse_clause_calls([],_Clause,Acc) ->
    lists:reverse(Acc).

merge_fun(coverage) ->
    fun({Cov1,NotCov1}, {Cov2,NotCov2}) ->
	    {Cov1+Cov2, NotCov1+NotCov2}
    end;
merge_fun(calls) ->
    fun(Calls1, Calls2) ->
	    Calls1+Calls2
    end.

merge_clauses(Clauses, MFun) -> merge_clauses(Clauses, MFun, []).
merge_clauses([{{M,F,A,_C1},R1},{{M,F,A,C2},R2}|Clauses], MFun, Result) ->
    merge_clauses([{{M,F,A,C2},MFun(R1,R2)}|Clauses], MFun, Result);
merge_clauses([{{M,F,A,_C},R}|Clauses], MFun, Result) ->
    merge_clauses(Clauses, MFun, [{{M,F,A},R}|Result]);
merge_clauses([], _Fun, Result) ->
    lists:reverse(Result).

merge_functions([{_MFA,R}|Functions], MFun) ->
    merge_functions(Functions, MFun, R);
merge_functions([],_MFun) ->         % There are no clauses.
    {0,0}.                           % No function can be covered or notcov.

merge_functions([{_MFA,R}|Functions], MFun, Result) ->
    merge_functions(Functions, MFun, MFun(Result, R));
merge_functions([], _MFun, Result) ->
    Result.

analyse_list_to_file(Modules, Opts, State) ->
    {LoadedMF, ImportedMF, Error} = are_loaded(Modules, State, [], [], []),
    collect([M || {M,_} <- LoadedMF], State#main_state.nodes),
    OutDir = proplists:get_value(outdir,Opts),
    HTML = lists:member(html,Opts),
    Fun = fun({Module,File}) ->
		  OutFile = outfilename(OutDir,Module,HTML),
		  do_analyse_to_file(Module,File,OutFile,HTML,State)
	  end,
    {Ok,Error1} = split_ok_error(pmap(Fun, LoadedMF++ImportedMF),[],[]),
    {result,Ok,Error ++ Error1}.

analyse_all_to_file(Opts, State) ->
    collect(State#main_state.nodes),
    AllModules = get_all_modules(State),
    OutDir = proplists:get_value(outdir,Opts),
    HTML = lists:member(html,Opts),
    Fun = fun({Module,File}) ->
		  OutFile = outfilename(OutDir,Module,HTML),
		  do_analyse_to_file(Module,File,OutFile,HTML,State)
	  end,
    {Ok,Error} = split_ok_error(pmap(Fun, AllModules),[],[]),
    {result,Ok,Error}.

get_all_modules(State) ->
    get_all_modules(State#main_state.compiled ++ State#main_state.imported,[]).
get_all_modules([{Module,File}|Rest],Acc) ->
    get_all_modules(Rest,[{Module,File}|Acc]);
get_all_modules([{Module,File,_}|Rest],Acc) ->
    case lists:keymember(Module,1,Acc) of
	true -> get_all_modules(Rest,Acc);
	false -> get_all_modules(Rest,[{Module,File}|Acc])
    end;
get_all_modules([],Acc) ->
    Acc.

split_ok_error([{ok,R}|Result],Ok,Error) ->
    split_ok_error(Result,[R|Ok],Error);
split_ok_error([{error,R}|Result],Ok,Error) ->
    split_ok_error(Result,Ok,[R|Error]);
split_ok_error([],Ok,Error) ->
    {Ok,Error}.

do_parallel_analysis_to_file(Module, Opts, Loaded, From, State) ->
    File = case Loaded of
	       {loaded, File0} ->
		   [{Module,Clauses}] = 
		       ets:lookup(?COVER_CLAUSE_TABLE,Module),
		   collect(Module, Clauses,
			   State#main_state.nodes),
		   File0;
	       {imported, File0, _} ->
		   File0
	   end,
    HTML = lists:member(html,Opts),
    OutFile =
	case proplists:get_value(outfile,Opts) of
	    undefined ->
		outfilename(proplists:get_value(outdir,Opts),Module,HTML);
	    F ->
		F
	end,
    reply(From, do_analyse_to_file(Module,File,OutFile,HTML,State)).

do_analyse_to_file(Module,File,OutFile,HTML,State) ->
    case find_source(Module, File) of
	{beam,_BeamFile} ->
	    {error,{no_source_code_found,Module}};
	ErlFile ->
	    analyse_info(Module,State#main_state.imported),
	    do_analyse_to_file1(Module,OutFile,ErlFile,HTML)
    end.

%% do_analyse_to_file1(Module,OutFile,ErlFile) -> {ok,OutFile} | {error,Error}
%%   Module = atom()
%%   OutFile = ErlFile = string()
do_analyse_to_file1(Module, OutFile, ErlFile, HTML) ->
    case file:open(ErlFile, [read,raw,read_ahead]) of
	{ok, InFd} ->
	    case file:open(OutFile, [write,raw,delayed_write]) of
		{ok, OutFd} ->
                    Enc = encoding(ErlFile),
		    if HTML -> 
                           Header =
                               ["<!DOCTYPE HTML PUBLIC "
                                "\"-//W3C//DTD HTML 3.2 Final//EN\">\n"
                                "<html>\n"
                                "<head>\n"
                                "<meta http-equiv=\"Content-Type\""
                                " content=\"text/html; charset=",
                                html_encoding(Enc),"\"/>\n"
                                "<title>",OutFile,"</title>\n"
                                "</head>"
                                "<body style='background-color: white;"
                                " color: black'>\n"
                                "<pre>\n"],
                            H1Bin = unicode:characters_to_binary(Header,Enc,Enc),
                            ok = file:write(OutFd,H1Bin);
		       true -> ok
		    end,
		    
		    %% Write some initial information to the output file
		    {{Y,Mo,D},{H,Mi,S}} = calendar:local_time(),
                   Timestamp =
                       io_lib:format("~p-~s-~s at ~s:~s:~s",
                                     [Y,
                                      string:pad(integer_to_list(Mo), 2, leading, $0),
                                      string:pad(integer_to_list(D),  2, leading, $0),
                                      string:pad(integer_to_list(H),  2, leading, $0),
                                      string:pad(integer_to_list(Mi), 2, leading, $0),
                                      string:pad(integer_to_list(S),  2, leading, $0)]),

                    H2Bin = unicode:characters_to_binary(
                              ["File generated from ",ErlFile," by COVER ",
                                Timestamp,"\n\n"
                                "**************************************"
                                "**************************************"
                                "\n\n"],
                              Enc, Enc),
                    ok = file:write(OutFd, H2Bin),

		    Pattern = {#bump{module=Module,line='$1',_='_'},'$2'},
		    MS = [{Pattern,[{is_integer,'$1'},{'>','$1',0}],[{{'$1','$2'}}]}],
                    CovLines0 =
                        lists:keysort(1, ets:select(?COLLECTION_TABLE, MS)),
                    CovLines = merge_dup_lines(CovLines0),
		    print_lines(Module, CovLines, InFd, OutFd, 1, HTML),
		    
		    if HTML ->
                           ok = file:write(OutFd, "</pre>\n</body>\n</html>\n");
		       true -> ok
		    end,

		    ok = file:close(OutFd),
		    ok = file:close(InFd),

		    {ok, OutFile};

		{error, Reason} ->
		    {error, {file, OutFile, Reason}}
	    end;

	{error, Reason} ->
	    {error, {file, ErlFile, Reason}}
    end.

merge_dup_lines(CovLines) ->
    merge_dup_lines(CovLines, []).
merge_dup_lines([{L, N}|T], [{L, NAcc}|TAcc]) ->
    merge_dup_lines(T, [{L, NAcc + N}|TAcc]);
merge_dup_lines([{L, N}|T], Acc) ->
    merge_dup_lines(T, [{L, N}|Acc]);
merge_dup_lines([], Acc) ->
    lists:reverse(Acc).

print_lines(Module, CovLines, InFd, OutFd, L, HTML) ->
    case file:read_line(InFd) of
	eof ->
	    ignore;
	{ok,RawLine} ->
	    Line = escape_lt_and_gt(RawLine,HTML),
	    case CovLines of
	       [{L,N}|CovLines1] ->
                    if N=:=0, HTML=:=true ->
                           LineNoNL = Line -- "\n",
                           Str = "     0",
                           %%Str = string:pad("0", 6, leading, $\s),
                           RedLine = ["<font color=red>",Str,fill1(),
                                      LineNoNL,"</font>\n"],
                           ok = file:write(OutFd, RedLine);
                       N < 1000000 ->
                           Str = string:pad(integer_to_list(N), 6, leading, $\s),
                           ok = file:write(OutFd, [Str,fill1(),Line]);
                       N < 10000000 ->
                           Str = integer_to_list(N),
                           ok = file:write(OutFd, [Str,fill2(),Line]);
                       true ->
                           Str = integer_to_list(N),
                           ok = file:write(OutFd, [Str,fill3(),Line])
                    end,
		    print_lines(Module, CovLines1, InFd, OutFd, L+1, HTML);
		_ ->                            %Including comment lines
		    ok = file:write(OutFd, [tab(),Line]),
		    print_lines(Module, CovLines, InFd, OutFd, L+1, HTML)
	    end
    end.

tab() ->  "        |  ".
fill1() ->      "..|  ".
fill2() ->       ".|  ".
fill3() ->        "|  ".

%%%--Export--------------------------------------------------------------
do_export(Module, OutFile, From, State) ->
    case file:open(OutFile,[write,binary,raw,delayed_write]) of
	{ok,Fd} ->
	    Reply = 
		case Module of
		    '_' ->
			export_info(State#main_state.imported),
			collect(State#main_state.nodes),
			do_export_table(State#main_state.compiled,
					State#main_state.imported,
					Fd);
		    _ ->
			export_info(Module,State#main_state.imported),
			try is_loaded(Module, State) of
			    {loaded, File} ->
				[{Module,Clauses}] = 
				    ets:lookup(?COVER_CLAUSE_TABLE,Module),
				collect(Module, Clauses,
					State#main_state.nodes),
				do_export_table([{Module,File}],[],Fd);
			    {imported, File, ImportFiles} ->
				%% don't know if I should allow this - 
				%% export a module which is only imported
				Imported = [{Module,File,ImportFiles}],
				do_export_table([],Imported,Fd)
			catch throw:_ ->
				{error,{not_cover_compiled,Module}}
			end
		end,
	    ok = file:close(Fd),
	    reply(From, Reply);
	{error,Reason} ->
	    reply(From, {error, {cant_open_file,OutFile,Reason}})

    end.

do_export_table(Compiled, Imported, Fd) ->
    ModList = merge(Imported,Compiled),
    write_module_data(ModList,Fd).

merge([{Module,File,_ImportFiles}|Imported],ModuleList) ->
    case lists:keymember(Module,1,ModuleList) of
	true ->
	    merge(Imported,ModuleList);
	false ->
	    merge(Imported,[{Module,File}|ModuleList])
    end;
merge([],ModuleList) ->
    ModuleList.

write_module_data([{Module,File}|ModList],Fd) ->
    write({file,Module,File},Fd),
    [Clauses] = ets:lookup(?COLLECTION_CLAUSE_TABLE,Module),
    write(Clauses,Fd),
    ModuleData = ets:match_object(?COLLECTION_TABLE,{#bump{module=Module},'_'}),
    do_write_module_data(ModuleData,Fd),
    write_module_data(ModList,Fd);
write_module_data([],_Fd) ->
    ok.

do_write_module_data([H|T],Fd) ->
    write(H,Fd),
    do_write_module_data(T,Fd);
do_write_module_data([],_Fd) ->
    ok.

write(Element,Fd) ->
    Bin = term_to_binary(Element,[compressed]),
    case byte_size(Bin) of
	Size when Size > 255 ->
	    SizeBin = term_to_binary({'$size',Size}),
	    ok = file:write(Fd, <<(byte_size(SizeBin)):8,SizeBin/binary,Bin/binary>>);
	Size ->
	    ok = file:write(Fd,<<Size:8,Bin/binary>>)
    end,
    ok.    

%%%--Import--------------------------------------------------------------
do_import_to_table(Fd,ImportFile,Imported) ->
    do_import_to_table(Fd,ImportFile,Imported,[]).
do_import_to_table(Fd,ImportFile,Imported,DontImport) ->
    case get_term(Fd) of
	{file,Module,File} ->
	    case add_imported(Module, File, ImportFile, Imported) of
		{ok,NewImported} ->
		    do_import_to_table(Fd,ImportFile,NewImported,DontImport);
		dont_import ->
		    do_import_to_table(Fd,ImportFile,Imported,
				       [Module|DontImport])
	    end;
	{Key=#bump{module=Module},Val} ->
	    case lists:member(Module,DontImport) of
		false ->
		    insert_in_collection_table(Key,Val);
		true ->
		    ok
	    end,
	    do_import_to_table(Fd,ImportFile,Imported,DontImport);
	{Module,Clauses} ->
	    case lists:member(Module,DontImport) of
		false ->
		    ets:insert(?COLLECTION_CLAUSE_TABLE,{Module,Clauses});
		true ->
			    ok
	    end,
	    do_import_to_table(Fd,ImportFile,Imported,DontImport);
	eof ->
	    Imported
    end.
	    

get_term(Fd) ->
    case file:read(Fd,1) of
	{ok,<<Size1:8>>} ->
	    {ok,Bin1} = file:read(Fd,Size1),
	    case binary_to_term(Bin1) of
		{'$size',Size2} -> 
		    {ok,Bin2} = file:read(Fd,Size2),
		    binary_to_term(Bin2);
		Term ->
		    Term
	    end;
	eof ->
	    eof
    end.

%%%--Reset---------------------------------------------------------------

%% Reset main node and all remote nodes
do_reset_main_node(Module,Nodes) ->
    do_reset(Module),
    do_reset_collection_table(Module),
    remote_reset(Module,Nodes).

do_reset_collection_table(Module) ->
    ets:delete(?COLLECTION_CLAUSE_TABLE,Module),
    ets:match_delete(?COLLECTION_TABLE, {#bump{module=Module},'_'}).

%% do_reset(Module) -> ok
%% The reset is done on ?CHUNK_SIZE number of bumps to avoid building
%% long lists in the case of very large modules
do_reset(Module) ->
    Pattern = {#bump{module=Module, _='_'}, '$1'},
    MatchSpec = [{Pattern,[{'=/=','$1',0}],['$_']}],
    Match = ets:select(?COVER_TABLE,MatchSpec,?CHUNK_SIZE),
    do_reset2(Match).

do_reset2({Bumps,Continuation}) ->
    lists:foreach(fun({Bump,_N}) ->
			  ets:insert(?COVER_TABLE, {Bump,0})
		  end,
		  Bumps),
    do_reset2(ets:select(Continuation));
do_reset2('$end_of_table') ->
    ok.    

do_clear(Module) ->
    ets:match_delete(?COVER_CLAUSE_TABLE, {Module,'_'}),
    ets:match_delete(?COVER_TABLE, {#bump{module=Module},'_'}),
    case lists:member(?COLLECTION_TABLE, ets:all()) of
	true ->
	    %% We're on the main node
	    ets:match_delete(?COLLECTION_TABLE, {#bump{module=Module},'_'});
	false ->
	    ok
    end.

not_loaded(Module, unloaded, State) ->
    do_clear(Module),
    remote_unload(State#main_state.nodes,[Module]),
    Compiled = update_compiled([Module],
			       State#main_state.compiled),
    State#main_state{ compiled = Compiled };
not_loaded(_Module,_Else, State) ->
    State.



%%%--Div-----------------------------------------------------------------

escape_lt_and_gt(Rawline,HTML) when HTML =/= true ->
    Rawline;
escape_lt_and_gt(Rawline,_HTML) ->
    escape_lt_and_gt1(Rawline,[]).

escape_lt_and_gt1([$<|T],Acc) ->
    escape_lt_and_gt1(T,[$;,$t,$l,$&|Acc]);
escape_lt_and_gt1([$>|T],Acc) ->
    escape_lt_and_gt1(T,[$;,$t,$g,$&|Acc]);
escape_lt_and_gt1([$&|T],Acc) ->
    escape_lt_and_gt1(T,[$;,$p,$m,$a,$&|Acc]);
escape_lt_and_gt1([],Acc) ->
    lists:reverse(Acc);
escape_lt_and_gt1([H|T],Acc) ->
    escape_lt_and_gt1(T,[H|Acc]).

%%%--Internal functions for parallelization------------------------------
pmap(Fun,List) ->
    NTot = length(List),
    NProcs = erlang:system_info(schedulers) * 2,
    NPerProc = (NTot div NProcs) + 1,
    Mons = pmap_spawn(Fun,NPerProc,List,[]),
    pmap_collect(Mons,[]).

pmap_spawn(_,_,[],Mons) ->
    Mons;
pmap_spawn(Fun,NPerProc,List,Mons) ->
    {L1,L2} = if length(List)>=NPerProc -> lists:split(NPerProc,List);
		 true -> {List,[]} % last chunk
	      end,
    Mon =
	spawn_monitor(
	  fun() ->
		  exit({pmap_done,lists:map(Fun,L1)})
	  end),
    pmap_spawn(Fun,NPerProc,L2,[Mon|Mons]).

pmap_collect([],Acc) ->
    lists:append(Acc);
pmap_collect(Mons,Acc) ->
    receive
	{'DOWN', Ref, process, Pid, {pmap_done,Result}} ->
	    pmap_collect(lists:delete({Pid,Ref},Mons),[Result|Acc]);
	{'DOWN', Ref, process, Pid, Reason} = Down ->
	    case lists:member({Pid,Ref},Mons) of
		true ->
		    %% Something went really wrong - don't hang!
		    exit(Reason);
		false ->
		    %% This should be handled somewhere else
		    self() ! Down,
		    pmap_collect(Mons,Acc)
	    end
    end.

%%%-----------------------------------------------------------------
%%% Decide which encoding to use when analyzing to file.
%%% The target file contains the file path, so if either the file name
%%% encoding or the encoding of the source file is utf8, then we need
%%% to use utf8.
encoding(File) ->
    case file:native_name_encoding() of
        latin1 ->
            case epp:read_encoding(File) of
                none ->
                    epp:default_encoding();
                E ->
                    E
            end;
        utf8 ->
            utf8
    end.

html_encoding(latin1) ->
    "iso-8859-1";
html_encoding(utf8) ->
    "utf-8".
