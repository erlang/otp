%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Test suite for the backends of the IDL compiler
%%%----------------------------------------------------------------------

-module(ic_be_SUITE).
-include("test_server.hrl").


-export([all/1,plain/1]).     


-define(OUT(X), filename:join([?config(priv_dir, Config), gen, to_list(X)])).


%% Top of cases

all(suite) -> [plain].



plain(doc) ->
    ["Checking code for the plain backend."];
plain(suite) -> [];
plain(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, plain),
    
    ?line ok = ic:gen(File,stdopts(OutDir)++[{be,erl_plain}]),
    
    ok.




%%--------------------------------------------------------------------
%%
%% Utilities


stdopts(OutDir) ->
    [{outdir, OutDir}, {maxerrs, infinity}].





to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) -> X.

