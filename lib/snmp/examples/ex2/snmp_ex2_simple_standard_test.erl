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
%%----------------------------------------------------------------------
%% This module examplifies how to write test suites for your SNMP agent.
%%----------------------------------------------------------------------

-module(snmp_ex2_simple_standard_test).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([start/0, start/1, start/3]).

-include_lib("snmp/include/snmp_types.hrl").

%% -define(USER,     agent_tester).
%% -define(USER_MOD, ?MODULE).
%% -define(USER_MOD, snmpm_user_default).

-record(command, {tag, desc, cmd, verify}).

start() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, Addr}     = inet:getaddr(Hostname, inet),
    start(Addr).

start(Addr) ->
    start([std_mib("STANDARD-MIB")], Addr, [{community, "public"}]).

start(Mibs, AgentAddr, AgentConfig) ->
    Conf = [{mibs, Mibs}],
    {ok, _Pid} = snmp_ex2_manager:start_link(Conf),
    snmp_ex2_manager:agent(AgentAddr, AgentConfig),
    simple_standard_test(AgentAddr),
    snmp_ex2_manager:stop().

simple_standard_test(AgentAddr) ->
    Commands = 
	[
	 #command{tag    = 1,
		  desc   = "get-next [1,1]", 
		  cmd    = fun() -> 
				   gn(AgentAddr, [1,1])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 2,
		  desc   = "get-next [1,3]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 3,
		  desc   = "get-next [1,3,6]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3,6])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 4,
		  desc   = "get-next [1,3,6]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3,6,1])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 5,
		  desc   = "get-next [1,3,6,1,2]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3,6])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 6,
		  desc   = "get-next [1,3,6,1,2,1]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3,6])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 7,
		  desc   = "get-next [1,3,6,1,2,1,1]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3,6])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 8,
		  desc   = "get-next [1,3,6,1,2,1,1,1]", 
		  cmd    = fun() ->
				 gn(AgentAddr, [1,3,6])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 9,
		  desc   = "get [sysDescr,0]", 
		  cmd    = fun() ->
				 g(AgentAddr, [sysDescr,0])
			   end,
		  verify = fun({ok, Res, _}) ->
				   verify_vbs(Res, 
					      [{sysDescr, 'OCTET STRING'}]);
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end},
	 #command{tag    = 10,
		  desc   = "get [1,3,6,1,2,1,1,1]", 
		  cmd    = fun() ->
				 g(AgentAddr, [1,3,6,1,2,1,1,1])
			   end,
		  verify = fun({ok, {noError, 0, [Vb]}, _}) ->
				   case Vb of
				       #varbind{oid   = [1,3,6,1,2,1,1,1],
						value = noSuchInstance} ->
					   {ok,  
					    lists:flatten(
					      io_lib:format("~n   noSuchInstance", []))};
				       #varbind{oid   = [1,3,6,1,2,1,1,1],
						value = noSuchName} ->
					   {ok, 
					    lists:flatten(
					      io_lib:format("~n   noSuchName", 
							    []))};
				       _ ->
					   {error, {unexpected_vb, Vb}}
				   end;
			      ({error, Reason}) ->
				   {error, {command_failed, Reason}};
			      (Crap) ->
				   {error, {unexpected_command_result, Crap}}
			   end}
	],
    ok = commands(Commands),
    io:format("Test completed.~n").


commands([]) ->
    ok;
commands([#command{tag    = Tag, 
		   desc   = Desc, 
		   cmd    = Cmd,
		   verify = Verify}|Commands]) ->
    io:format("Command ~2w (~s): ", [Tag, Desc]),
    case Verify((catch Cmd())) of
	{ok, Val} ->
	    io:format("ok~s~n", [Val]),
	    ok;
	{error, Reason} ->
	    io:format("error"
		      "~n   ~p"
		      "~n", [Reason])
    end,
    commands(Commands).


%% --- Command shorts ---

gn(Addr, [H|_] = Oids) when is_list(H) ->
    snmp_ex2_manager:sync_get_next(Addr, Oids);
gn(Addr, Oid) ->
    gn(Addr, [Oid]).

g(Addr, [H|_] = Oids) when is_list(H) ->
    snmp_ex2_manager:sync_get(Addr, Oids);
g(Addr, Oid) ->
    g(Addr, [Oid]).


%% Verify that all varbinds have the expected name and type
verify_vbs({noError, 0, Vbs}, NameAndTypes) ->
    (catch verify_vbs(Vbs, NameAndTypes, ""));
verify_vbs(Res, _) ->
    {error, {unexpected_result, Res}}.

verify_vbs([], _, Acc) ->
    {ok, Acc};
verify_vbs([Vb|T], NameAndTypes, Acc) ->
    Val  = verify_vb(Vb, NameAndTypes),
    Acc2 = lists:flatten(io_lib:format("~s~n   ~s", [Acc, Val])),
    verify_vbs(T, NameAndTypes, Acc2).

verify_vb(#varbind{oid = Oid, variabletype = Type, value = Val} = Vb, 
	  NameAndTypes) ->
    case lists:reverse(Oid) of
	[0|RevOid] ->
	    case snmp_ex2_manager:oid_to_name(lists:reverse(RevOid)) of
		{ok, Name} ->
		    case lists:keysearch(Name, 1, NameAndTypes) of
			{value, {Name, Type}} ->
			    Val;
			{value, {Name, WrongType}} ->
			    error({wrong_type, {WrongType, Vb}});
			false ->
			    error({unexpected_name, {Name, Vb}})
		    end;
		{error, Reason} ->
		    error({unexpected_oid, {Reason, Vb}})
	    end;
	_ ->
	    case lists:keysearch(Oid, 1, NameAndTypes) of
		{value, {Oid, Type}} ->
		    Val;
		{value, {Oid, WrongType}} ->
		    error({wrong_type, {WrongType, Vb}});
		false ->
		    error({unexpected_oid, Vb})
	    end
    end.


std_mib(MibName) ->
    j(std_dir(), MibName).

std_dir() -> j(code:priv_dir(snmp), "mibs").

j(Dir, Filename) ->
    filename:join(Dir, Filename).


error(Reason) ->
    throw({error, Reason}).


