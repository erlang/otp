%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2012. All Rights Reserved.
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
-callback close(file:io_device(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.
-callback delete(file:name(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.
-callback del_dir(file:name(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.
-callback get_cwd(State::term()) ->
    {{ok, Dir::term()}, State::term()} | {{error, Reason::term()}, State::term()}.
-callback is_dir(file:name(), State::term()) ->
    {boolean(), State::term()}.
-callback list_dir(file:name(), State::term()) ->
    {{ok, Filenames::term()}, State::term()} | {{error, Reason::term()}, State::term()}.
-callback make_dir(Dir::term(), State::term()) ->
    {{ok, State::term()},State::term()} | {{error, Reason::term()}, State::term()}.
-callback make_symlink(Path2::term(), Path::term(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.
-callback open(Path::term(), Flags::term(), State::term()) ->
    {{ok, IoDevice::term()}, State::term()} | {{error, Reason::term()}, State::term()}.
-callback position(file:io_device(), Offs::term(), State::term()) ->
    {{ok, NewPosition::term()}, State::term()} | {{error, Reason::term()}, State::term()}.
-callback read(file:io_device(), Len::term(), State::term()) ->
    {{ok, Data::term()},State::term()} | {eof, State::term()} | {{error, Reason::term()}, State::term()}.
-callback read_link(file:name(), State::term()) ->
    {{ok, FileName::term()}, State::term()} | {{error, Reason::term()}, State::term()}.
-callback read_link_info(file:name(), State::term()) ->
    {{ok, FileInfo::term()}, State::term()} | {{error, Reason::term()}, State::term()}.
-callback read_file_info(file:name(), State::term()) ->
    {{ok, FileInfo::term()}, State::term()} | {{error, Reason::term()},State::term()}.
-callback rename(file:name(), file:name(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.
-callback write(file:io_device(), Data::term(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.
-callback write_file_info(file:name(),Info::term(), State::term()) ->
    {ok, State::term()} | {{error, Reason::term()}, State::term()}.



