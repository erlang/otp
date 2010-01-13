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

-module(ftp_ticket_test).

-compile(export_all).

-define(LIB_MOD,ftp_suite_lib).
-define(CASE_WRAPPER(_A_,_B_,_C_),?LIB_MOD:wrapper(_A_,_B_,_C_)).
-define(PLATFORM,"Solaris 8 sparc ").


%% Test server callbacks
init_per_testcase(Case, Config) ->
    ftp_suite_lib:init_per_testcase(Case, Config).

end_per_testcase(Case, Config) ->
    ftp_suite_lib:end_per_testcase(Case, Config).


all(suite) ->
    {conf,init,tickets(),fin}.

init(Config) ->
    ?LIB_MOD:ftpd_init(ticket_test, Config).

tickets() ->
    [ticket_6035].


fin(Config) ->
    ?LIB_MOD:ftpd_fin(Config).

ticket_6035(X) -> ?LIB_MOD:ticket_6035(X).
