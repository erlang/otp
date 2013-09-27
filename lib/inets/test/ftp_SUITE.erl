%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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

%% 
%% ct:run("../inets_test", ftp_SUITE).
%%

-module(ftp_SUITE).

-include_lib("kernel/include/file.hrl").
-include_lib("common_test/include/ct.hrl").
-include("inets_test_lib.hrl").

%% Note: This directive should only be used in test suites.
-compile(export_all).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     {group, ftp_passive},
     {group, ftp_active},
     {group, ftps_passive},
     {group, ftps_active}
    ].

groups() ->
    [
     {ftp_passive, [], ftp_tests()},
     {ftp_active, [], ftp_tests()},
     {ftps_passive, [], ftp_tests()},
     {ftps_active, [], ftp_tests()}
    ].

ftp_tests()->
    [
     user, 
     pwd, 
     cd, 
     lcd,
     ls, 
     nlist, 
     rename, 
     delete, 
     mkdir, 
     send, 
     send_bin, 
     send_chunk, 
     append, 
     append_bin,
     append_chunk, 
     recv, 
     recv_bin, 
     recv_chunk, 
     type, 
     quote, 
     ip_v6_disabled
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
init_per_group(_Group, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

%%--------------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Internal functions  -----------------------------------------------
%%--------------------------------------------------------------------
