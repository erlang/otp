%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2010. All Rights Reserved.
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

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
     {close, 2}, 
     {delete, 2}, 
     {del_dir, 2}, 
     {get_cwd, 1}, 
     {is_dir, 2}, 
     {list_dir, 2}, 
     {make_dir, 2}, 
     {make_symlink, 3}, 
     {open, 3}, 
     {position, 3}, 
     {read, 3},
     {read_file_info, 2}, 
     {read_link, 2}, 
     {read_link_info, 2}, 
     {rename, 3},
     {write, 3}, 
     {write_file_info, 3}
    ];
behaviour_info(_) ->
    undefined.
