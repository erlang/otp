%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : ignore_cores.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : 
%%%
%%% Created : 11 Feb 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------

-module(ignore_cores).

-include_lib("test_server/include/test_server.hrl").

-export([init/1, fini/1, setup/3, setup/4, restore/1, dir/1]).

-record(ignore_cores, {org_cwd,
		       org_path,
		       org_pwd_env,
		       ign_dir = false,
		       cores_dir = false}).

%%
%% Takes a testcase config
%%

init(Config) ->
    {ok, OrgCWD} = file:get_cwd(),
    [{ignore_cores,
      #ignore_cores{org_cwd = OrgCWD,
		    org_path = code:get_path(),
		    org_pwd_env = os:getenv("PWD")}}
     | lists:keydelete(ignore_cores, 1, Config)].
    
fini(Config) ->
    #ignore_cores{org_cwd = OrgCWD,
		  org_path = OrgPath,
		  org_pwd_env = OrgPWD} = ?config(ignore_cores, Config),
    ok = file:set_cwd(OrgCWD),
    true = code:set_path(OrgPath),
    case OrgPWD of
	false -> ok;
	_ -> true = os:putenv("PWD", OrgPWD)
    end,
    lists:keydelete(ignore_cores, 1, Config).

setup(Suite, Testcase, Config) ->
    setup(Suite, Testcase, Config, false).

setup(Suite, Testcase, Config, SetCwd) when is_atom(Suite), 
                                            is_atom(Testcase),
                                            is_list(Config) ->
    #ignore_cores{org_cwd = OrgCWD,
		  org_path = OrgPath,
		  org_pwd_env = OrgPWD} = ?config(ignore_cores, Config),
    Path = lists:map(fun (".") -> OrgCWD; (Dir) -> Dir end, OrgPath),
    true = code:set_path(Path),
    PrivDir = ?config(priv_dir, Config),
    IgnDir = filename:join([PrivDir,
			 atom_to_list(Suite)
			 ++ "_"
			 ++ atom_to_list(Testcase)
			 ++ "_wd"]),
    ok = file:make_dir(IgnDir),
    case SetCwd of
	false ->
	    ok;
	_ ->
	    ok = file:set_cwd(IgnDir),
	    OrgPWD = case os:getenv("PWD") of
			 false -> false;
			 PWD ->
			     os:putenv("PWD", IgnDir),
			     PWD
		     end
    end,
    ok = file:write_file(filename:join([IgnDir, "ignore_core_files"]), <<>>),
    %% cores are dumped in /cores on MacOS X
    CoresDir = case {?t:os_type(), filelib:is_dir("/cores")} of
		   {{unix,darwin}, true} ->
		       filelib:fold_files("/cores",
					  "^core.*$",
					  false,
					  fun (C,Cs) -> [C|Cs] end,
					  []);
		   _ ->
		       false
	       end,
    lists:keyreplace(ignore_cores,
		     1,
		     Config,
		     {ignore_cores,
		      #ignore_cores{org_cwd = OrgCWD,
				    org_path = OrgPath,
				    org_pwd_env = OrgPWD,
				    ign_dir = IgnDir,
				    cores_dir = CoresDir}}).

restore(Config) ->
    #ignore_cores{org_cwd = OrgCWD,
		  org_path = OrgPath,
		  org_pwd_env = OrgPWD,
		  ign_dir = IgnDir,
		  cores_dir = CoresDir} = ?config(ignore_cores, Config),
    try
	case CoresDir of
	    false ->
		ok;
	    _ ->
		%% Move cores dumped by these testcases in /cores
		%% to cwd.
		lists:foreach(fun (C) ->
				      case lists:member(C, CoresDir) of
					  true -> ok;
					  _ ->
					      Dst = filename:join(
						      [IgnDir,
						       filename:basename(C)]),
					      {ok, _} = file:copy(C, Dst),
					      file:delete(C)
				      end
			      end,
			      filelib:fold_files("/cores",
						 "^core.*$",
						 false,
						 fun (C,Cs) -> [C|Cs] end,
						 []))
	end
    after
	catch file:set_cwd(OrgCWD),
	catch code:set_path(OrgPath),
	case OrgPWD of
	    false -> ok;
	    _ -> catch os:putenv("PWD", OrgPWD)
	end
    end.


dir(Config) ->
    #ignore_cores{ign_dir = Dir} = ?config(ignore_cores, Config),
    Dir.
