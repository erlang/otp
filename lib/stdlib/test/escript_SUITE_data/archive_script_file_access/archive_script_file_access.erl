%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(archive_script_file_access).
-behaviour(escript).

-export([main/1]).

-include_lib("kernel/include/file.hrl").

main([RelArchiveFile]) ->

    AbsArchiveFile = filename:absname(RelArchiveFile),
    DotSlashArchiveFile = "./" ++ RelArchiveFile,

    Beam = atom_to_list(?MODULE) ++ ".beam",
    AbsBeam = filename:join(AbsArchiveFile,Beam),
    RelBeam = filename:join(RelArchiveFile,Beam),
    DotSlashBeam = filename:join(DotSlashArchiveFile,Beam),
    Dir = "dir1",
    AbsDir = filename:join(AbsArchiveFile,Dir),
    RelDir = filename:join(RelArchiveFile,Dir),
    DotSlashDir = filename:join(DotSlashArchiveFile,Dir),
    SubDir = "subdir1",
    AbsSubDir = filename:join(AbsDir,SubDir),
    RelSubDir = filename:join(RelDir,SubDir),
    DotSlashSubDir = filename:join(DotSlashDir,SubDir),

    {ok,List1} = erl_prim_loader:list_dir(AbsArchiveFile),
    {ok,List1} = erl_prim_loader:list_dir(RelArchiveFile),
    {ok,List1} = erl_prim_loader:list_dir(DotSlashArchiveFile),
    {ok,List1} = erl_prim_loader:list_dir(AbsArchiveFile ++ "/"),
    {ok,List1} = erl_prim_loader:list_dir(AbsArchiveFile ++ "/."),
    {ok,List1} = erl_prim_loader:list_dir(filename:join([AbsDir,".."])),
    {ok,List1} = erl_prim_loader:list_dir(filename:join([RelDir,".."])),
    {ok,List1} = erl_prim_loader:list_dir(filename:join([DotSlashDir,".."])),
    {ok,List1} = erl_prim_loader:list_dir(filename:join([AbsSubDir,"..",".."])),
    {ok,List1} = erl_prim_loader:list_dir(filename:join([RelSubDir,"..",".."])),
    {ok,List1} = erl_prim_loader:list_dir(filename:join([DotSlashSubDir,"..",".."])),
    false = lists:member([],List1),

    %% If symlinks are supported on this platform...
    RelSymlinkArchiveFile = "symlink_to_" ++ RelArchiveFile,
    case element(1,os:type()) =:= win32 orelse file:read_link(RelSymlinkArchiveFile) of
	{ok,_} ->
	    DotSlashSymlinkArchiveFile = "./" ++ RelSymlinkArchiveFile,
	    AbsSymlinkArchiveFile=filename:join(filename:dirname(AbsArchiveFile),
						RelSymlinkArchiveFile),
	    {ok,List1} = erl_prim_loader:list_dir(AbsSymlinkArchiveFile),
	    {ok,List1} = erl_prim_loader:list_dir(RelSymlinkArchiveFile),
	    {ok,List1} = erl_prim_loader:list_dir(DotSlashSymlinkArchiveFile);
	_ -> % not supported
	    ok
    end,


    {ok,List2} = erl_prim_loader:list_dir(AbsDir),
    {ok,List2} = erl_prim_loader:list_dir(RelDir),
    {ok,List2} = erl_prim_loader:list_dir(DotSlashDir),
    false = lists:member([],List2),

    error = erl_prim_loader:list_dir(AbsBeam),
    error = erl_prim_loader:list_dir(RelBeam),
    error = erl_prim_loader:list_dir(DotSlashBeam),

    error = erl_prim_loader:get_file(AbsArchiveFile),
    error = erl_prim_loader:get_file(RelArchiveFile),
    error = erl_prim_loader:get_file(DotSlashArchiveFile),
    error = erl_prim_loader:get_file(AbsArchiveFile ++ "/"),
    error = erl_prim_loader:get_file(AbsArchiveFile ++ "/."),
    {ok,Bin,AbsBeam} = erl_prim_loader:get_file(AbsBeam),
    {ok,Bin,RelBeam} = erl_prim_loader:get_file(RelBeam),
    {ok,Bin,DotSlashBeam} = erl_prim_loader:get_file(DotSlashBeam),

    {ok,#file_info{type=directory}=DFI} =
	erl_prim_loader:read_file_info(AbsArchiveFile),
    {ok,DFI} = erl_prim_loader:read_file_info(RelArchiveFile),
    {ok,DFI} = erl_prim_loader:read_file_info(DotSlashArchiveFile),
    {ok,DFI} = erl_prim_loader:read_file_info(AbsArchiveFile ++ "/"),
    {ok,DFI} = erl_prim_loader:read_file_info(AbsArchiveFile ++ "/."),
    {ok,#file_info{type=regular}=RFI} = erl_prim_loader:read_file_info(AbsBeam),
    {ok,RFI} = erl_prim_loader:read_file_info(RelBeam),
    {ok,RFI} = erl_prim_loader:read_file_info(DotSlashBeam),

    F = AbsArchiveFile ++ ".extension",
    error = erl_prim_loader:list_dir(F),
    {ok,_,_} = erl_prim_loader:get_file(F),
    {ok,#file_info{type=regular}} = erl_prim_loader:read_file_info(F),

    ok.
