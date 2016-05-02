%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%%% @doc Common Test Framework code coverage support module.
%%%
%%% <p>This module exports help functions for performing code 
%%%    coverage analysis.</p>

-module(ct_cover).

-export([get_spec/1, add_nodes/1, remove_nodes/1, cross_cover_analyse/2]).

-include("ct_util.hrl").

-include_lib("kernel/include/file.hrl").

%%%-----------------------------------------------------------------
%%% @spec add_nodes(Nodes) -> {ok,StartedNodes} | {error,Reason}
%%%    Nodes = [atom()]
%%%    StartedNodes = [atom()]
%%%    Reason = cover_not_running | not_main_node
%%%
%%% @doc Add nodes to current cover test (only works if cover support 
%%%      is active!). To have effect, this function should be called
%%%      from init_per_suite/1 before any actual tests are performed.
%%% 
add_nodes([]) ->
    {ok,[]};
add_nodes(Nodes) ->
    case whereis(cover_server) of
	undefined ->
	    {error,cover_not_running};
	_ ->
	    Nodes0 = cover:which_nodes(),
	    Nodes1 = [Node || Node <- Nodes,
			      lists:member(Node,Nodes0) == false],
	    ct_logs:log("COVER INFO",
			"Adding nodes to cover test: ~w", [Nodes1]),
	    case cover:start(Nodes1) of
		Result = {ok,StartedNodes} ->
		    ct_logs:log("COVER INFO",
				"Successfully added nodes to cover test: ~w",
				[StartedNodes]),
		    Result;
		Error ->
		    ct_logs:log("COVER INFO",
				"Failed to add nodes to cover test: ~tp",
				[Error]),
		    Error
	    end
    end.


%%%-----------------------------------------------------------------
%%% @spec remove_nodes(Nodes) -> ok | {error,Reason}
%%%    Nodes = [atom()]
%%%    Reason = cover_not_running | not_main_node
%%%
%%% @doc Remove nodes from current cover test. Call this function
%%%      to stop cover test on nodes previously added with add_nodes/1. 
%%%      Results on the remote node are transferred to the Common Test 
%%%      node.
%%% 
remove_nodes([]) ->
    ok;
remove_nodes(Nodes) ->
    case whereis(cover_server) of
	undefined ->
	    {error,cover_not_running};
	_ ->
	    Nodes0 = cover:which_nodes(),
	    ToRemove = [Node || Node <- Nodes, lists:member(Node,Nodes0)],
	    ct_logs:log("COVER INFO",
			"Removing nodes from cover test: ~w", [ToRemove]),
	    case cover:stop(ToRemove) of
		ok ->
		    ct_logs:log("COVER INFO",
				"Successfully removed nodes from cover test.",
				[]),
		    ok;
		Error ->
		    ct_logs:log("COVER INFO",
				"Failed to remove nodes from cover test: ~tp",
				[Error]),
		    Error
	    end
    end.
    
    
%%%-----------------------------------------------------------------
%%% @spec cross_cover_analyse(Level,Tests) -> ok
%%%    Level = overview | details
%%%    Tests = [{Tag,Dir}]
%%%    Tag = atom()
%%%    Dir = string()
%%%
%%% @doc Accumulate cover results over multiple tests.
%%%      See the chapter about <seealso
%%%      marker="cover_chapter#cross_cover">cross cover
%%%      analysis</seealso> in the users's guide.
%%%
cross_cover_analyse(Level,Tests) ->
    test_server_ctrl:cross_cover_analyse(Level,Tests).


%%%-----------------------------------------------------------------
%%% @hidden 

%% Read cover specification file and return the parsed info.
%% -> CoverSpec: {CoverFile,Nodes,Import,Export,AppCoverInfo}
get_spec(File) ->
    catch get_spec_test(File).

get_spec_test(File) ->
    Dir = filename:dirname(File), % always abs path in here, set in ct_run
    case filelib:is_file(File) of
	true ->
	    case file:consult(File) of
		{ok,Terms} ->
		    Import = 
			case lists:keysearch(import, 1, Terms) of
			    {value,{_,Imps=[S|_]}} when is_list(S) ->
				ImpsFN = lists:map(fun(F) -> 
							  filename:absname(F,Dir)
						  end, Imps),
				test_files(ImpsFN, ImpsFN);
			    {value,{_,Imp=[IC|_]}} when is_integer(IC) ->
				ImpFN = filename:absname(Imp,Dir),
				test_files([ImpFN], [ImpFN]);
			    _ -> 
				[]
			end,
		    Export = 
			case lists:keysearch(export, 1, Terms) of
			    {value,{_,Exp=[EC|_]}} when is_integer(EC) -> 
				filename:absname(Exp,Dir);
			    {value,{_,[Exp]}} ->
				filename:absname(Exp,Dir);
			    _ -> 
				undefined
			end,
		    Nodes = 
			case lists:keysearch(nodes, 1, Terms) of
			    {value,{_,Ns}} -> 
				Ns;
			    _ -> 
				[]
			end,
		    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		    %% NOTE! We can read specifications with multiple %%
		    %% apps, but since we don't have support for      %%
		    %% running cover on more than one app at a time,  %%
		    %% we just allow 1 app per spec for now.          %%
		    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		    case collect_apps(Terms, []) of
			Res when Res == [] ; length(Res) == 1 -> % 1 app = ok
			   Apps = case Res of
				     [] -> [#cover{app=none, level=details}];
				     _ -> Res
				 end,
			    case get_cover_opts(Apps, Terms, Dir, []) of
				E = {error,_} -> 
				    E;
				[CoverSpec] ->
				    CoverSpec1 = remove_excludes_and_dups(CoverSpec),
				    {File,Nodes,Import,Export,CoverSpec1};
				_ ->
				    {error,multiple_apps_in_cover_spec}
			    end;
			Apps when is_list(Apps) ->
			    {error,multiple_apps_in_cover_spec}
		    end;
		Error ->			% file:consult/1 fails
		    {error,{invalid_cover_spec,Error}}
	    end;
	false ->
	    {error,{cant_read_cover_spec_file,File}}
    end.

collect_apps([{level,Level}|Ts], Apps) ->
    collect_apps(Ts, [#cover{app=none, level=Level}|Apps]);
collect_apps([{incl_app,App,Level}|Ts], Apps) ->
    collect_apps(Ts, [#cover{app=App, level=Level}|Apps]);
collect_apps([_|Ts], Apps) ->
    collect_apps(Ts, Apps);
collect_apps([], Apps) ->
    Apps.

%% get_cover_opts(Terms) -> AppCoverInfo
%% AppCoverInfo: [#cover{app=App,...}]

get_cover_opts([App | Apps], Terms, Dir, CoverInfo) ->
    case get_app_info(App, Terms, Dir) of
	E = {error,_} -> E;
	AppInfo ->
	    AppInfo1 = files2mods(AppInfo),
	    get_cover_opts(Apps, Terms, Dir, [AppInfo1|CoverInfo])
    end;
get_cover_opts([], _, _, CoverInfo) ->
    lists:reverse(CoverInfo).

%% get_app_info(App, Terms, Dir) -> App1

get_app_info(App=#cover{app=none}, [{incl_dirs,Dirs}|Terms], Dir) ->
    get_app_info(App, [{incl_dirs,none,Dirs}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{incl_dirs,Name,Dirs}|Terms], Dir) ->
    case get_files(Dirs, Dir, ".beam", false, []) of
	E = {error,_} -> E;
	Mods1 ->
	    Mods = App#cover.incl_mods,
	    get_app_info(App#cover{incl_mods=Mods++Mods1},Terms,Dir)
    end;

get_app_info(App=#cover{app=none}, [{incl_dirs_r,Dirs}|Terms], Dir) ->
    get_app_info(App, [{incl_dirs_r,none,Dirs}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{incl_dirs_r,Name,Dirs}|Terms], Dir) ->
    case get_files(Dirs, Dir, ".beam", true, []) of
	E = {error,_} -> E;
	Mods1 ->
	    Mods = App#cover.incl_mods,
	    get_app_info(App#cover{incl_mods=Mods++Mods1},Terms,Dir)
    end;

get_app_info(App=#cover{app=none}, [{incl_mods,Mods1}|Terms], Dir) ->
    get_app_info(App, [{incl_mods,none,Mods1}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{incl_mods,Name,Mods1}|Terms], Dir) ->
    Mods = App#cover.incl_mods,
    get_app_info(App#cover{incl_mods=Mods++Mods1},Terms,Dir);

get_app_info(App=#cover{app=none}, [{excl_dirs,Dirs}|Terms], Dir) ->
    get_app_info(App, [{excl_dirs,none,Dirs}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{excl_dirs,Name,Dirs}|Terms], Dir) ->
    case get_files(Dirs, Dir, ".beam", false, []) of
	E = {error,_} -> E;
	Mods1 ->
	    Mods = App#cover.excl_mods,
	    get_app_info(App#cover{excl_mods=Mods++Mods1},Terms,Dir)
    end;

get_app_info(App=#cover{app=none}, [{excl_dirs_r,Dirs}|Terms],Dir) ->
    get_app_info(App, [{excl_dirs_r,none,Dirs}|Terms],Dir);
get_app_info(App=#cover{app=Name}, [{excl_dirs_r,Name,Dirs}|Terms],Dir) ->
    case get_files(Dirs, Dir, ".beam", true, []) of
	E = {error,_} -> E;
	Mods1 ->
	    Mods = App#cover.excl_mods,
	    get_app_info(App#cover{excl_mods=Mods++Mods1},Terms,Dir)
    end;

get_app_info(App=#cover{app=none}, [{excl_mods,Mods1}|Terms], Dir) ->
    get_app_info(App, [{excl_mods,none,Mods1}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{excl_mods,Name,Mods1}|Terms], Dir) ->
    Mods = App#cover.excl_mods,
    get_app_info(App#cover{excl_mods=Mods++Mods1},Terms,Dir);

get_app_info(App=#cover{app=none}, [{cross,Cross}|Terms], Dir) ->
    get_app_info(App, [{cross,none,Cross}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{cross,Name,Cross1}|Terms], Dir) ->
    Cross = App#cover.cross,
    get_app_info(App#cover{cross=Cross++Cross1},Terms,Dir);

get_app_info(App=#cover{app=none}, [{src_dirs,Dirs}|Terms], Dir) ->
    get_app_info(App, [{src_dirs,none,Dirs}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{src_dirs,Name,Dirs}|Terms], Dir) ->
    case get_files(Dirs, Dir, ".erl", false, []) of
	E = {error,_} -> E;
	Src1 ->
	    Src = App#cover.src,
	    get_app_info(App#cover{src=Src++Src1},Terms,Dir)
    end;

get_app_info(App=#cover{app=none}, [{src_dirs_r,Dirs}|Terms], Dir) ->
    get_app_info(App, [{src_dirs_r,none,Dirs}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{src_dirs_r,Name,Dirs}|Terms], Dir) ->
    case get_files(Dirs, Dir, ".erl", true, []) of
	E = {error,_} -> E;
	Src1 ->
	    Src = App#cover.src,
	    get_app_info(App#cover{src=Src++Src1},Terms,Dir)
    end;

get_app_info(App=#cover{app=none}, [{src_files,Src1}|Terms], Dir) ->
    get_app_info(App, [{src_files,none,Src1}|Terms], Dir);
get_app_info(App=#cover{app=Name}, [{src_files,Name,Src1}|Terms], Dir) ->
    Src = App#cover.src,
    get_app_info(App#cover{src=Src++Src1},Terms,Dir);

get_app_info(App, [_|Terms], Dir) ->
    get_app_info(App, Terms, Dir);

get_app_info(App, [], _) ->
    App.

%% get_files(...)
    
get_files([Dir|Dirs], RootDir, Ext, Recurse, Files) ->
    DirAbs = filename:absname(Dir, RootDir),
    case file:list_dir(DirAbs) of
	{ok,Entries} ->
	    {SubDirs,Matches} = analyse_files(Entries, DirAbs, Ext, [], []),
	    if Recurse == false ->
		    get_files(Dirs, RootDir, Ext, Recurse, Files++Matches);
	       true ->
		    Files1 = get_files(SubDirs, RootDir, Ext, Recurse, Files++Matches),
		    get_files(Dirs, RootDir, Ext, Recurse, Files1)
	    end;
	{error,Reason} ->
	    {error,{Reason,DirAbs}}
    end;
get_files([], _RootDir, _Ext, _R, Files) ->
    Files.
	    
%% analyse_files(...)

analyse_files([F|Fs], Dir, Ext, Dirs, Matches) ->
    Fullname = filename:absname(F, Dir),
    {ok,Info} = file:read_file_info(Fullname),
    case Info#file_info.type of
	directory ->
	    analyse_files(Fs, Dir, Ext, 
			  [Fullname|Dirs], Matches);
	_ ->
	    case filename:extension(Fullname) of
		".beam" when Ext == ".beam" ->
		    %% File = {file,Dir,filename:rootname(F)},
		    Mod = list_to_atom(filename:rootname(F)),
		    analyse_files(Fs, Dir, Ext, Dirs, 
				  [Mod|Matches]);
		".erl" when Ext == ".erl" ->
		    analyse_files(Fs, Dir, Ext, Dirs, 
				  [Fullname|Matches]);
		_ ->
		    analyse_files(Fs, Dir, Ext, Dirs, Matches)
	    end
    end;
analyse_files([], _Dir, _Ext, Dirs, Matches) ->
    {Dirs,Matches}.


test_files([F|Fs], Ret) ->
    case filelib:is_file(F) of
	true ->
	    test_files(Fs, Ret);
	false ->
	    throw({error,{invalid_cover_file,F}})
    end;
test_files([], Ret) ->
    Ret.

remove_excludes_and_dups(CoverData=#cover{excl_mods=Excl,incl_mods=Incl}) ->
    Incl1 = [Mod || Mod <- Incl, lists:member(Mod, Excl) == false],
    %% delete duplicates and sort
    Incl2 = lists:sort(lists:foldl(fun(M,L) -> 
					   case lists:member(M,L) of 
					       true -> L; 
					       false -> [M|L] 
					   end 
				   end, [], Incl1)),
    CoverData#cover{incl_mods=Incl2}.
	     
    
files2mods(Info=#cover{excl_mods=ExclFs,
		       incl_mods=InclFs,
		       cross=Cross}) ->
    Info#cover{excl_mods=files2mods1(ExclFs),
	       incl_mods=files2mods1(InclFs),
	       cross=[{Tag,files2mods1(Fs)} || {Tag,Fs} <- Cross]}.

files2mods1([M|Fs]) when is_atom(M) ->
    [M|files2mods1(Fs)];
files2mods1([F|Fs]) when is_list(F) ->
    M = filename:rootname(filename:basename(F)),
    [list_to_atom(M)|files2mods1(Fs)];
files2mods1([]) ->
    [].
