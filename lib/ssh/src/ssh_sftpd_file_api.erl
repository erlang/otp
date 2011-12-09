%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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

-module(ssh_sftpd_file_api).

%% To be further specified later
-callback close(IoDevice::term(), State::term()) -> 
    ok | {error, Reason::term()}.
-callback delete(Path::term(), State::term()) ->
     ok | {error, Reason::term()}.
-callback del_dir(Path::term(), State::term()) ->
    ok | {error, Reason::term()}.
-callback get_cwd(State::term()) ->
    {ok, Dir::term()} | {error, Reason::term()}.
-callback is_dir(AbsPath::term(), State::term()) ->
    boolean().
-callback list_dir(AbsPath::term(), State::term()) ->
    {ok, Filenames::term()} | {error, Reason::term()}.
-callback make_dir(Dir::term(), State::term()) ->
    ok | {error, Reason::term()}.
-callback make_symlink(Path2::term(), Path::term(), State::term()) ->
    ok | {error, Reason::term()}.
-callback open(Path::term(), Flags::term(), State::term()) ->
    {ok, IoDevice::term()} | {error, Reason::term()}.
-callback position(IoDevice::term(), Offs::term(), State::term()) ->
    {ok, NewPosition::term()} | {error, Reason::term()}.
-callback read(IoDevice::term(), Len::term(), State::term()) ->
    {ok, Data::term()} | eof | {error, Reason::term()}.
-callback read_link(Path::term(), State::term()) ->
    {ok, FileName::term()} | {error, Reason::term()}.
-callback read_link_info(Path::term(), State::term()) ->
    {ok, FileInfo::term()} | {error, Reason::term()}.
-callback read_file_info(Path::term(), State::term()) ->
    {ok, FileInfo::term()} | {error, Reason::term()}.
-callback rename(Path::term(), Path2::term(), State::term()) ->
    ok | {error, Reason::term()}.
-callback write(IoDevice::term(), Data::term(), State::term()) ->
    ok | {error, Reason::term()}.
-callback write_file_info(Path::term(),Info::term(), State::term()) ->
    ok | {error, Reason::term()}.



