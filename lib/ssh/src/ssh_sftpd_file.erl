%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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

%%

%%% Description: Default Callback module for ssh_sftpd

-module(ssh_sftpd_file).

-behaviour(ssh_sftpd_file_api).

%% API
-export([close/2, delete/2, del_dir/2, get_cwd/1, is_dir/2, list_dir/2, 
	 make_dir/2, make_symlink/3, open/3, position/3, read/3,
	 read_file_info/2, read_link/2, read_link_info/2, rename/3,
	 write/3, write_file_info/3]).

close(IoDevice, State) ->
    {file:close(IoDevice), State}.

delete(Path, State) ->
    {file:delete(Path), State}.

del_dir(Path, State) ->
    {file:del_dir(Path), State}.

get_cwd(State) ->
    {file:get_cwd(), State}.

is_dir(AbsPath, State) ->
    {filelib:is_dir(AbsPath), State}.

list_dir(AbsPath, State) ->
    {file:list_dir(AbsPath), State}.
     
make_dir(Dir, State) ->
    {file:make_dir(Dir), State}.
     
make_symlink(Path2, Path, State) ->
    {file:make_symlink(Path2, Path), State}.

open(Path, Flags, State) ->
    {file:open(Path, Flags), State}.
     
position(IoDevice, Offs, State) ->
    {file:position(IoDevice, Offs), State}.

read(IoDevice, Len, State) ->
    {file:read(IoDevice, Len), State}.
          
read_link(Path, State) ->
    {file:read_link(Path), State}.

read_link_info(Path, State) ->
    {file:read_link_info(Path), State}.
     
read_file_info(Path, State) ->
    {file:read_file_info(Path), State}.

rename(Path, Path2, State) ->
    {file:rename(Path, Path2), State}.

write(IoDevice, Data, State) ->
    {file:write(IoDevice, Data), State}.
     
write_file_info(Path,Info, State) ->
    {file:write_file_info(Path, Info), State}.
