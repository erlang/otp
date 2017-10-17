%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
-module(jinterface_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([transport_factory/1,
	 nodename/1, register_and_whereis/1, get_names/1, boolean_atom/1,
	 node_ping/1, mbox_ping/1,
	 java_erlang_send_receive/1,
	 java_internal_send_receive_same_node/1,
	 java_internal_send_receive_different_nodes/1,
	 java_internal_send_receive_self/1,
	 java_link_and_exit/1, erl_link_and_exit/1,
	 erl_link_java_exit/1, java_link_erl_exit/1,
	 internal_link_linking_exits/1, internal_link_linked_exits/1,
	 internal_unlink_linking_exits/1, internal_unlink_linked_exits/1,
	 normal_exit/1, kill_mbox/1,kill_erl_proc_from_java/1,
	 kill_mbox_from_erlang/1,
	 erl_exit_with_reason_any_term/1,
	 java_exit_with_reason_any_term/1,
	 status_handler_localStatus/1, status_handler_remoteStatus/1,
	 status_handler_connAttempt/1,
	 maps/1,
	 fun_equals/1,
	 core_match_bind/1
     ]).

-include_lib("common_test/include/ct.hrl").

-define(debug,true).
-ifdef(debug).
-define(dbg(Str,Args), io:format(Str,Args)).
-else.
-define(dbg(Str,Args), ok).
-endif.

-define(link_test_reason,link_test_reason).

%% Test cases in MboxSendReceive.java
-define(java_erlang_send_receive,1).
-define(java_internal_send_receive_same_node,2).
-define(java_internal_send_receive_different_nodes,3).
-define(java_internal_send_receive_self,4).

%% Test cases in MboxLinkUnlink.java
-define(java_link_and_exit, 1).
-define(erl_link_and_exit, 2).
-define(erl_link_java_exit, 3).
-define(java_link_erl_exit, 4).
-define(internal_link_linking_exits, 5).
-define(internal_link_linked_exits, 6).
-define(internal_unlink_linking_exits,7).
-define(internal_unlink_linked_exits,8).
-define(normal_exit,9).
-define(kill_mbox,10).
-define(kill_erl_proc_from_java,11).
-define(kill_mbox_from_erlang,12).
-define(erl_exit_with_reason_any_term,13).
-define(java_exit_with_reason_any_term,14).


%% Test cases in NodeStatusHandler.java
-define(status_handler_localStatus,1).
-define(status_handler_remoteStatus,2).
-define(status_handler_connAttempt,3).

%%%-----------------------------------------------------------------
%%% INIT/END
%%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    lists:append([fundamental(), ping(), send_receive(),
		  link_unlink(), status_handler()]).

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


fundamental() ->
    [
     transport_factory,    % TransportFactoryTest.java
     nodename,             % Nodename.java
     register_and_whereis, % RegisterAndWhereis.java
     get_names,            % GetNames.java
     boolean_atom,         % BooleanAtom.java
     maps,                 % Maps.java
     fun_equals,           % FunEquals.java
     core_match_bind       % CoreMatchBind.java
    ].

ping() ->
    [
     %% Implemented in NodePing.java
     node_ping,

     %% Implemented in MboxPing.java
     mbox_ping
     ].


send_receive() ->
    [
     %% Implemented in MboxSendReceive.java
     java_erlang_send_receive,
     java_internal_send_receive_same_node,
     java_internal_send_receive_different_nodes,
     java_internal_send_receive_self
    ].

%% Note:
%%
%% The test cases in MboxLinkUnlink.java and in
%% NodePing.java, all uses default cookie, and if there
%% is a problem with having the same default cookie in
%% erlang vs jinterface, e.g because the home directory
%% does not get the same in some cases on Windows
%% - they will all fail.

link_unlink() ->
    [
     %% Implemented in MboxLinkUnlink.java
     java_link_and_exit,
     erl_link_and_exit,
     erl_link_java_exit,
     java_link_erl_exit,
     internal_link_linking_exits,
     internal_link_linked_exits,
     internal_unlink_linking_exits,
     internal_unlink_linked_exits,
     normal_exit,
     kill_mbox,
     kill_erl_proc_from_java,
     kill_mbox_from_erlang,
     erl_exit_with_reason_any_term,
     java_exit_with_reason_any_term
    ].

status_handler() ->
    [
     %% Implemented in NodeStatusHandler.java
     status_handler_localStatus,
     status_handler_remoteStatus,
     status_handler_connAttempt
    ].


init_per_suite(Config) when is_list(Config) ->
    case case code:priv_dir(jinterface) of
	     {error,bad_name} -> false;
	     P -> filelib:is_dir(P) end of
	true ->
            case hostname_resolves() of
                true ->
                    jitu:init_all(Config);
                Skip ->
                    Skip
            end;
	false ->
	    {skip,"No jinterface application"}
    end.

%% Check if inet:gethostname() can be resolved by
%% the native resolver. If it can, we know that
%% jinterface name resolution works. If it cannot
%% jinterface tests will fail.
hostname_resolves() ->
    {ok, HN} = inet:gethostname(),
    case inet_gethost_native:gethostbyname(HN) of
        {ok, _} ->
            true;
        _ ->
            {skip, "Cannot resolve short hostname, add " ++ HN ++ " to /etc/hosts"}
    end.

end_per_suite(Config) when is_list(Config) ->
    jitu:finish_all(Config).

init_per_testcase(Case, _Config) 
  when Case =:= kill_mbox;
       Case =:= kill_mbox_from_erlang ->
    {skip, "Not yet implemented"};
init_per_testcase(_Case,Config) ->
    Dog = ?t:timetrap({seconds,30}),
    [{watch_dog,Dog}|Config].

end_per_testcase(_Case,Config) ->
    case whereis(erl_link_server) of
	undefined -> ok;
	Pid -> exit(Pid,kill)
    end,
    jitu:kill_all_jnodes(),
    ?t:timetrap_cancel(?config(watch_dog,Config)),
    ok.


%%%-----------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------
transport_factory(doc) ->
    ["TransportFactoryTest.java: Test custom OTP Transport Factory"];
transport_factory(suite) ->
    [];
transport_factory(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "TransportFactoryTest").

%%%-----------------------------------------------------------------
nodename(doc) ->
    ["Nodename.java: "
     "Test OtpNode.node(), OtpNode.alive() and OtpNode.host()"];
nodename(suite) ->
    [];
nodename(Config) when is_list(Config) ->
    [_,Host] = string:tokens(atom_to_list(node()),"@"),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "Nodename",
		   [list_to_atom(Host)]).

%%%-----------------------------------------------------------------
register_and_whereis(doc) ->
    ["RegisterAndWhereis.java: "
     "Test OtpNode.registerName(...), OtpMbox.registerName(...) and "
     "OtpNode.whereis(...)"];
register_and_whereis(suite) ->
    [];
register_and_whereis(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "RegisterAndWhereis",
		   []).

%%%-----------------------------------------------------------------
get_names(doc) ->
    ["GetNames.java: "
     "Test OtpNode.getNames()"];
get_names(suite) ->
    [];
get_names(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "GetNames",
		   []).

%%%-----------------------------------------------------------------
boolean_atom(doc) ->
    ["BooleanAtom.java: "
     "Test OtpErlangAtom.booleanValue()"];
boolean_atom(suite) ->
    [];
boolean_atom(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "BooleanAtom",
		   []).

%%%-----------------------------------------------------------------
node_ping(doc) ->
    ["NodePing.java: "
     "Test OtpNode.ping(java.lang.String node, long timeout)"];
node_ping(suite) ->
    [];
node_ping(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "NodePing",
		   [erlang:get_cookie(),node()]).

%%%-----------------------------------------------------------------
mbox_ping(doc) ->
    ["MboxPing.java: "
     "Test OtpNode.createMbox(...) and OtpMbox.ping(...)"];
mbox_ping(suite) ->
    [];
mbox_ping(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "MboxPing",
		   [erlang:get_cookie(),node()]).

%%%-----------------------------------------------------------------
java_erlang_send_receive(doc) ->
    ["Test sending/receiving of erlang messages between erlang and java"];
java_erlang_send_receive(suite) ->
    [];
java_erlang_send_receive(Config) when is_list(Config) ->
    send_receive(?java_erlang_send_receive, fun echo_loop/0, Config).

echo_loop() ->
    receive
	{From,Msg} ->
	    ?dbg("erl_send_receive_server received ~p",[{From,Msg}]),
	    ?dbg("erl_send_receive_server sending ~p",[Msg]),
	    From ! Msg,
	    echo_loop();
	done ->
	    ok
    end.


%%%-----------------------------------------------------------------
java_internal_send_receive_same_node(doc) ->
    ["MboxSendReceive.java: "
     "Test sending/receiving of erlang messages between mboxes "
     "on the same java node."];
java_internal_send_receive_same_node(suite) ->
    [];
java_internal_send_receive_same_node(Config) when is_list(Config) ->
    send_receive(?java_internal_send_receive_same_node,
		 fun() -> receive done -> ok end end,
		 Config).

%%%-----------------------------------------------------------------
java_internal_send_receive_different_nodes(doc) ->
    ["MboxSendReceive.java: "
     "Test sending/receiving of erlang messages between mboxes "
     "on different java nodes."];
java_internal_send_receive_different_nodes(suite) ->
    [];
java_internal_send_receive_different_nodes(Config) when is_list(Config) ->
    send_receive(?java_internal_send_receive_different_nodes,
		 fun() -> receive done -> ok end end,
		 Config).

%%%-----------------------------------------------------------------
java_internal_send_receive_self(doc) ->
    ["MboxSendReceive.java: "
     "Test sending/receiving of erlang messages from an mbox to itself"];
java_internal_send_receive_self(suite) ->
    [];
java_internal_send_receive_self(Config) when is_list(Config) ->
    send_receive(?java_internal_send_receive_self,
		 fun() -> receive done -> ok end end,
		 Config).

%%%-----------------------------------------------------------------
java_link_and_exit(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link between erlang process and java mailbox."
     "Java mailbox links and exits"];
java_link_and_exit(suite) ->
    [];
java_link_and_exit(Config) when is_list(Config) ->
    LinkFun =
	fun(Mbox) ->
		Mbox ! {?java_link_and_exit,self(),?link_test_reason},
		receive after infinity -> ok end
	end,
    erl_java_link(LinkFun,java_link_and_exit,Config).


%%%-----------------------------------------------------------------
erl_link_and_exit(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link between erlang process and java mailbox."
     "Erlang process links and exits"];
erl_link_and_exit(suite) ->
    [];
erl_link_and_exit(Config) when is_list(Config) ->
    LinkFun = fun(Mbox) ->
		      link(Mbox),
		      Mbox ! {?erl_link_and_exit,self(),?link_test_reason},
		      receive ok -> ok end,
		      exit(?link_test_reason)
	      end,
    erl_java_link(LinkFun,erl_link_and_exit,Config).

%%%-----------------------------------------------------------------
erl_link_java_exit(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link between erlang process and java mailbox."
     "Erlang process links and java mailbox exits"];
erl_link_java_exit(suite) ->
    [];
erl_link_java_exit(Config) when is_list(Config) ->
    LinkFun = fun(Mbox) ->
		      link(Mbox),
		      Mbox ! {?erl_link_java_exit,self(),?link_test_reason},
		      receive after infinity -> ok end
	      end,
    erl_java_link(LinkFun,erl_link_java_exit,Config).


%%%-----------------------------------------------------------------
java_link_erl_exit(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link between erlang process and java mailbox."
     "Java mailbox links and erlang process exits"];
java_link_erl_exit(suite) ->
    [];
java_link_erl_exit(Config) when is_list(Config) ->
    LinkFun =
	fun(Mbox) ->
		Mbox ! {?java_link_erl_exit,self(),?link_test_reason},
		receive ok -> ok end,
		exit(?link_test_reason)
	end,
    erl_java_link(LinkFun,java_link_erl_exit,Config).

%%%-----------------------------------------------------------------
internal_link_linking_exits(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link between two java mailboxes."
     "The mailbox which creates the link is the one exiting"];
internal_link_linking_exits(suite) ->
    [];
internal_link_linking_exits(Config) when is_list(Config) ->
     internal_link_unlink(?internal_link_linking_exits,
			  internal_link_linking_exits,
			  Config).

%%%-----------------------------------------------------------------
internal_link_linked_exits(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link between two java mailboxes."
     "The mailbox which dies not create the link is the one exiting"];
internal_link_linked_exits(suite) ->
    [];
internal_link_linked_exits(Config) when is_list(Config) ->
    internal_link_unlink(?internal_link_linked_exits,
			 internal_link_linked_exits,
			 Config).

%%%-----------------------------------------------------------------
internal_unlink_linking_exits(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link and unlink between two java mailboxes. "
     "Mailbox1 creates a link to mailbox2 and removes it. "
     "Then it creates another link and mailbox2 removes is. "
     "Finally mailbox1 exits - mailbox2 must survive"];
internal_unlink_linking_exits(suite) ->
    [];
internal_unlink_linking_exits(Config) when is_list(Config) ->
     internal_link_unlink(?internal_unlink_linking_exits,
			  internal_unlink_linking_exits,
			  Config).

%%%-----------------------------------------------------------------
internal_unlink_linked_exits(doc) ->
    ["MboxLinkUnlink.java: "
     "Test link and unlink between two java mailboxes. "
     "Mailbox1 creates a link to mailbox2 and removes it. "
     "Then it creates another link and mailbox2 removes is. "
     "Finally mailbox2 exits - mailbox1 must survive"];
internal_unlink_linked_exits(suite) ->
    [];
internal_unlink_linked_exits(Config) when is_list(Config) ->
    internal_link_unlink(?internal_unlink_linked_exits,
			 internal_unlink_linked_exits,
			 Config).

%%%-----------------------------------------------------------------
normal_exit(doc) ->
    ["MboxLinkUnlink.java: "
     "Test that mbox.close() uses exit reason 'normal', i.e. "
     "that linked processes are not terminated."];
normal_exit(suite) ->
    [];
normal_exit(Config) when is_list(Config) ->
    Fun =
	fun() ->
		register(erl_link_server,self()),
		process_flag(trap_exit,true),
		receive
		    {Main,Mbox} when is_pid(Main), is_pid(Mbox) ->
			?dbg("Erlang sending \"~p\"",[normal_exit]),
			link(Mbox),
			Pid = spawn_link(fun() ->
						 link(Mbox),
						 Mbox ! {?normal_exit},
						 receive after infinity -> ok end
					 end),
			receive
			    {'EXIT',Mbox,normal} ->
				%% Make sure that we don't get the
				%% exit signal from Pid, and Pid
				%% should still be alive.
				receive
				    {'EXIT',Pid,Reason} ->
					?dbg("Got unexpected exit signal: ~p",
					     [{'EXIT',Pid,Reason}]),
					exit({unexpected,{'EXIT',Pid,Reason}})
				after 500 ->
					true = erlang:is_process_alive(Pid),
					exit(Pid,kill)
				end,
				receive done -> Main ! done end
			after 1000 ->
				receive
				    Other ->
					?dbg("Got garbage when waiting for exit:"
					     " ~p", [Other]),
					Main ! done,
					exit({got_unexpected,Other})
				after 0 ->
					ok
				end
			end;
		    Other ->
			?dbg("Got garbage: ~p",[Other]),
			exit(Other)
		end
	end,

    spawn_link(Fun),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "MboxLinkUnlink",
		   [erlang:get_cookie(),node()]).


%%%-----------------------------------------------------------------
kill_mbox(doc) ->
    ["MboxLinkUnlink.java: "
     "Test that mbox.exit(new OtpErlangAtom(\"kill\") causes linked "
     "processes to exit with reason 'killed', which can be trapped."];
kill_mbox(suite) ->
    {skip, "Not yet implemented"};
kill_mbox(Config) when is_list(Config) ->
    Fun =
	fun() ->
		register(erl_link_server,self()),
		process_flag(trap_exit,true),
		receive
		    {Main,Mbox} when is_pid(Main), is_pid(Mbox) ->
			?dbg("Erlang sending \"~p\"",[kill_mbox]),
			Pid = spawn_link(fun() ->
						 process_flag(trap_exit,true),
						 link(Mbox),
						 Mbox ! {?kill_mbox},
						 receive
						     {'EXIT',Mbox,killed} ->
							 exit(correct_reason);
						     {'EXIT',Mbox,R} ->
							 exit({faulty_reason,R})
						 end
					 end),
			receive
			    {'EXIT',Pid,{faulty_reason,Reason}} ->
				receive done -> Main ! done end,
				exit({faulty_reason,Reason});
			    {'EXIT',Pid,im_killed} ->
				receive done -> Main ! done end
			after 1000 ->
				receive
				    Other ->
					?dbg("Got garbage when waiting for exit:"
					     " ~p", [Other]),
					Main ! done,
					exit({got_unexpected,Other})
				after 0 ->
					ok
				end
			end;
		    Other ->
			?dbg("Got garbage: ~p",[Other]),
			exit(Other)
		end
	end,

    spawn_link(Fun),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "MboxLinkUnlink",
		   [erlang:get_cookie(),node()]).

%%%-----------------------------------------------------------------
kill_erl_proc_from_java(doc) ->
    ["MboxLinkUnlink.java: "
     "Test that mbox.exit(pid, new OtpErlangAtom(\"kill\") causes erlang "
     "processes <pid> to be killed, even if trapping exits"];
kill_erl_proc_from_java(suite) ->
    [];
kill_erl_proc_from_java(Config) when is_list(Config) ->
    LinkFun = fun(Mbox) ->
		      process_flag(trap_exit,true),
		      link(Mbox),
		      Mbox ! {?kill_erl_proc_from_java, self()},
		      receive after infinity -> ok end
	      end,
    erl_java_link(LinkFun,kill_erl_proc_from_java,killed,Config).

%%%-----------------------------------------------------------------
kill_mbox_from_erlang(doc) ->
    ["MboxLinkUnlink.java: "
     "Test that exit(Mbox,kill) causes linked the Mbox to be killed, and"
     "linked processes to exit with reason 'killed', even if trapping exits"];
kill_mbox_from_erlang(suite) ->
    {skip, "Not yet implemented"};
kill_mbox_from_erlang(Config) when is_list(Config) ->
    LinkFun = fun(Mbox) ->
		      link(Mbox),
		      Mbox ! {?kill_mbox_from_erlang},
		      exit(Mbox,kill),
		      receive after infinity -> ok end
	      end,
    erl_java_link(LinkFun,kill_mbox_from_erlang,killed,Config).

%%%-----------------------------------------------------------------
erl_exit_with_reason_any_term(doc) ->
    ["MboxLinkUnlink.java: "
     "Test that any erlang term can be used as exit reason when erlang "
     "process exits and is linked to an mbox."];
erl_exit_with_reason_any_term(suite) ->
    [];
erl_exit_with_reason_any_term(Config) when is_list(Config) ->
    Reason = [hei,self(),{this,is,"a",[different,"reason"]}],
    LinkFun = fun(Mbox) ->
		      link(Mbox),
		      Mbox ! {?erl_exit_with_reason_any_term,self(),Reason},
		      receive ok -> ok end,
		      exit(Reason)
	      end,
    erl_java_link(LinkFun,erl_exit_with_reason_any_term,Reason,Config).

%%%-----------------------------------------------------------------
java_exit_with_reason_any_term(doc) ->
    ["MboxLinkUnlink.java: "
     "Test that any erlang term can be used as exit reason when mbox "
     "exits and is linked to an erlang process."];
java_exit_with_reason_any_term(suite) ->
    [];
java_exit_with_reason_any_term(Config) when is_list(Config) ->
    Reason = [hei,self(),{this,is,"a",[different,"reason"]}],
    LinkFun =
	fun(Mbox) ->
		Mbox ! {?java_exit_with_reason_any_term,self(),Reason},
		receive after infinity -> ok end
	end,
    erl_java_link(LinkFun,java_exit_with_reason_any_term,Reason,Config).


%%%-----------------------------------------------------------------
status_handler_localStatus(doc) ->
    ["NodeStatusHandler.java: "
     "Test OtpNode.registerStatusHandler(...) and the callback "
     "OtpNodeStatus.localStatus(...)"];
status_handler_localStatus(suite) ->
    [];
status_handler_localStatus(Config) when is_list(Config) ->
    spawn_link(fun() ->
		       erl_status_server([{opt,{localStatus,"javanode1",true}},
					  {localStatus,"javanode1",false}])
	       end),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "NodeStatusHandler",
		   [erlang:get_cookie(),node(),?status_handler_localStatus]).

%%%-----------------------------------------------------------------
status_handler_remoteStatus(doc) ->
    ["NodeStatusHandler.java: "
     "Test OtpNode.registerStatusHandler(...) and the callback "
     "OtpNodeStatus.remoteStatus(...)"];
status_handler_remoteStatus(suite) ->
    [];
status_handler_remoteStatus(Config) when is_list(Config) ->
    spawn_link(fun() ->
		       erl_status_server([{opt,{localStatus,"javanode1",true}},
					  {remoteStatus,"javanode2",true},
					  {remoteStatus,"javanode2",false}])
	       end),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "NodeStatusHandler",
		   [erlang:get_cookie(),node(),?status_handler_remoteStatus]).


%%%-----------------------------------------------------------------
status_handler_connAttempt(doc) ->
    ["NodeStatusHandler.java: "
     "Test OtpNode.registerStatusHandler(...) and the callback "
     "OtpNodeStatus.connAttempt(...)"];
status_handler_connAttempt(suite) ->
    [];
status_handler_connAttempt(Config) when is_list(Config) ->
    spawn_link(fun() ->
		       erl_status_server([{opt,{localStatus,"javanode1",true}},
					  {connAttempt,"unknown",true},
					  {connAttempt,"javanode3",false}])
	       end),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "NodeStatusHandler",
		   [erlang:get_cookie(),node(),?status_handler_connAttempt]).

%%%-----------------------------------------------------------------
maps(doc) ->
    ["Maps.java: "
     "Tests OtpErlangMap encoding, decoding, toString, get"];
maps(suite) ->
    [];
maps(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
           ?config(data_dir, Config),
           "Maps",
           []).

%%%-----------------------------------------------------------------
fun_equals(doc) ->
    ["FunEquals.java: "
     "Test OtpErlangFun.equals()"];
fun_equals(suite) ->
    [];
fun_equals(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
           ?config(data_dir, Config),
           "FunEquals",
           []).

%%%-----------------------------------------------------------------
core_match_bind(doc) ->
    ["CoreMatchBind.java: "
     "Test OtpErlangObject.match() and bind()"];
core_match_bind(suite) ->
    [];
core_match_bind(Config) when is_list(Config) ->
    ok = jitu:java(?config(java, Config),
           ?config(data_dir, Config),
           "CoreMatchBind",
           []).

%%%-----------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------
send_receive(TestCaseTag,Fun,Config) ->
    spawn(fun() ->
		  register(erl_send_receive_server,self()),
		  receive
		      From when is_pid(From) ->
			  JavaNode = node(From),
			  [JavaNode] = nodes(hidden),
			  From ! {TestCaseTag,self()},
			  Fun(),
			  unregister(erl_send_receive_server)
		  end
	  end),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "MboxSendReceive",
		   [erlang:get_cookie(),node()]).


internal_link_unlink(Tag,Msg,Config) ->
    Fun =
	fun() ->
		register(erl_link_server,self()),
		process_flag(trap_exit,true),
		receive
		    {Main,Mbox} when is_pid(Main), is_pid(Mbox) ->
			?dbg("Erlang sending \"~p\"",[Msg]),
			Mbox ! {Tag,self(),?link_test_reason},
			receive done -> Main ! done end;
		    Other ->
			?dbg("Got garbage: ~p",[Other]),
			exit(Other)
		end
	end,

    spawn_link(Fun),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "MboxLinkUnlink",
		   [erlang:get_cookie(),node()]).


erl_java_link(LinkFun,Msg,Config) ->
    erl_java_link(LinkFun,Msg,?link_test_reason,Config).

erl_java_link(LinkFun,Msg,Reason,Config) ->
    Fun =
	fun() ->
		register(erl_link_server,self()),
		process_flag(trap_exit,true),
		receive
		    {Main,Mbox} when is_pid(Mbox), is_pid(Mbox) ->
			?dbg("Erlang sending \"~p\"",[Msg]),
			Pid = spawn_link(fun() -> LinkFun(Mbox) end),
			receive
			    {'EXIT',Pid,Reason} ->
				receive done -> Main ! done end
			after 1000 ->
				receive
				    Other ->
					?dbg("Got garbage when waiting for exit:"
					     " ~p", [Other]),
					Main ! done,
					exit({got_unexpected,Other})
				after 0 ->
					ok
				end
			end;
		    Other ->
			?dbg("Got garbage: ~p",[Other]),
			exit(Other)
		end
	end,

    spawn_link(Fun),
    ok = jitu:java(?config(java, Config),
		   ?config(data_dir, Config),
		   "MboxLinkUnlink",
		   [erlang:get_cookie(),node()]).

erl_status_server(List) ->
    register(erl_status_server,self()),
    erl_status_server(List,undefined).
erl_status_server([{opt,{Tag,NodeName,Up}},{Tag2,NodeName2,Up2}|Rest],_) ->
    receive
	{Tag,Node,Up,From} = M ->
	    ?dbg("erl_status_server got: ~p",[M]),
	    true = lists:prefix(NodeName,Node),
	    erl_status_server([{Tag2,NodeName2,Up2}|Rest],From);
	{Tag2,Node2,Up2,From2} = M2 ->
	    ?dbg("erl_status_server got: ~p",[M2]),
	    true = lists:prefix(NodeName2,Node2),
	    erl_status_server(Rest,From2)
    end;
erl_status_server([{Tag,NodeName,Up}|Rest],_) ->
    receive
	{Tag,Node,Up,From} = M ->
	    ?dbg("erl_status_server got: ~p",[M]),
	    true = lists:prefix(NodeName,Node),
	    erl_status_server(Rest,From);
	Other ->
	    ?dbg("erl_status_server got garbage: ~p",[Other]),
	    exit(Other)
    end;
erl_status_server([],From) ->
    From ! done.
