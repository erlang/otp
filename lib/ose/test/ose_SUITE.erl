%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
-module(ose_SUITE).

%-compile(export_all).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,init_per_testcase/2,
	 end_per_testcase/2]).
-export([
	 basic/1,stress/1,multi_msg_numbers/1,multi_mailboxes/1,
	 hunt/1,multi_hunt/1,dehunt/1,multi_dehunt/1,
	 attach/1,multi_attach/1,detach/1,multi_detach/1,
	 open_errors/1,close_errors/1,get_id_errors/1,get_name_errors/1,
	 hunt_errors/1,dehunt_errors/1,attach_errors/1,detach_errors/1,
	 send_errors/1,send_w_s_errors/1,listen_errors/1
	]).

-define(INTERFACE,ose).


init_per_testcase(_Func, Config) ->
    Config.
end_per_testcase(_Func, _Config) ->
    ok.

suite() -> [{timeout,{30,seconds}}].

all() ->
    [
     basic,stress,multi_msg_numbers,multi_mailboxes,
     hunt,multi_hunt,dehunt,multi_dehunt,
     attach,multi_attach,detach,multi_detach,

     open_errors,close_errors,get_id_errors,get_name_errors,
     hunt_errors,dehunt_errors,attach_errors,detach_errors,
     send_errors,send_w_s_errors,listen_errors
    ].

groups() ->
    [].

init_per_suite(Config) ->
    case os:type() of
	{ose,_} ->
	    Config;
	_Else ->
	    {skip,"Only run on OSE"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

basic(_Config) ->

    [P1,P2] = multi_open(2,[42]),
    P1Id = ?INTERFACE:get_id(P1),
    P2Id = ?INTERFACE:get_id(P2),

    ok = ?INTERFACE:send(P2,P1Id,42,<<"ping">>),
    receive
	{message,P1,V1} ->
	    {P2Id,P1Id,42,<<"ping">>} = V1,
	    ?INTERFACE:send(P1,P2Id,42,<<"pong">>);
	Else1 ->
	    ct:fail({got_wrong_message,Else1})
    end,

    receive
	{message,P2,V2} ->
	    {P1Id,P2Id,42,<<"pong">>} = V2;
	Else2 ->
	    ct:fail({got_wrong_message,Else2})
    end,

    ?INTERFACE:close(P1),
    ?INTERFACE:close(P2).

%% Send 1000 messages and see if we can cope and that msg order is preserved
stress(_Config) ->

    Iterations = 1000,

    [P1,P2] = multi_open(2,[42]),
    P1Id = ?INTERFACE:get_id(P1),
    P2Id = ?INTERFACE:get_id(P2),

    spawn(fun() ->
		  n(fun(N) ->
			    Msg = [<<"ping">>|integer_to_list(N)],
			    ?INTERFACE:send(P2,P1Id,42,Msg)
		    end,Iterations)
	  end),
    timer:sleep(100),
    n(fun(N) ->
	      receive
		  {message,P1,Value} ->
		      Int = integer_to_binary(N),
		      {P2Id,P1Id,42,<<"ping",Int/binary>>} = Value,
		      ok;
		  Else ->
		      ct:fail({got_wrong_message,Else})
	      end
      end,Iterations),

    ?INTERFACE:close(P1),
    ?INTERFACE:close(P2).

%% Listen to 1000 different message numbers and send some random messages
multi_msg_numbers(_Config) ->

    Iterations = 100,

    [P1,P2] = multi_open(2,lists:seq(2000,3000)),
    P1Id = ?INTERFACE:get_id(P1),

    n(fun(_) ->
	      Num = random:uniform(1000)+2000,
	      ?INTERFACE:send(P2,P1Id,Num,<<"ping",(integer_to_binary(Num))/binary>>)
      end,Iterations),

    n(fun(_) ->
	      receive
		  {message,P1,{_,_,Id,<<"ping",Num/binary>>}} when Id > 2000;
								   Id =< 3000 ->
		      Id = binary_to_integer(Num),
		      ok;
		  Else ->
		      ct:fail({got_wrong_message,Else})
	      end
      end,Iterations),

    ?INTERFACE:close(P1),
    ?INTERFACE:close(P2).


%% Create 100 mailboxes and send messages to them
multi_mailboxes(_Config) ->

    Mailboxes = 100,

    [P1|MBs] = multi_open(Mailboxes,[42]),

    [?INTERFACE:send(P1,?INTERFACE:get_id(P),42,[<<"ping">>,?INTERFACE:get_name(P,?INTERFACE:get_id(P))]) || P <- MBs],

    [receive
	 {message,P,Value} ->
	     Name = ?INTERFACE:get_name(P,?INTERFACE:get_id(P)),
	     {_,_,42,<<"ping",Name/binary>>} = Value,
	     ok
     end || P <- MBs],

    [?INTERFACE:close(P) || P <- [P1|MBs]],
    ok.

hunt(_Config) ->
    [P1,P2] = multi_open(2,[]),

    Ref = ?INTERFACE:hunt(P1,"p2"),
    receive
	{mailbox_up,P1,Ref,Pid} ->
	    Pid = ?INTERFACE:get_id(P2),
	    ?INTERFACE:close(P1),
	    ?INTERFACE:close(P2);
	Else ->
	    ct:fail({got_wrong_message,Else,Ref})
    end.

multi_hunt(_Config) ->

    Iterations = 100,

    P = ?INTERFACE:open("p"),

    Refs = [?INTERFACE:hunt(P,"p"++integer_to_list(N))|| N <- lists:seq(1,Iterations)],

    Pids = [begin
		Prt = ?INTERFACE:open("p"++integer_to_list(N)),
		Pid = ?INTERFACE:get_id(Prt),
		?INTERFACE:close(Prt),
		Pid
	    end || N <- lists:seq(1,Iterations)],

    [receive
	 {mailbox_up,P,Ref,Pid} ->
	     ok
     after 10 ->
	     ct:fail({did_not_get,Pid,Ref})
     end || {Pid,Ref} <- lists:zip(Pids,Refs)],
    ?INTERFACE:close(P).


dehunt(_Config)  ->
    [P1] = multi_open(1,[]),
    Ref = ?INTERFACE:hunt(P1,"p2"),
    receive
	_Else -> ct:fail({got,_Else})
    after 1000 ->
	    ok
    end,
    P2 = ?INTERFACE:open("p2"),

    % Make sure any messages are sent
    receive after 10 -> ok end,

    ok = ?INTERFACE:dehunt(P1,Ref),

    % Make sure no messages are received
    receive
	_Else2 -> ct:fail({got,_Else2})
    after 1000 ->
	    ?INTERFACE:close(P1),
	    ?INTERFACE:close(P2)
    end.

%%%
%%% This testcase basically:
%%%  spawn 10 processes that in parallel
%%%        adds some hunts for different OSE processes
%%%        maybe create hunted OSE process
%%%        dehunt half of the hunts
%%%        create more hunts
%%%        if not created create hunted OSE process
%%%        veryify that all expected hunt messages are received
%%%  verify that all processes exited correctly
%%%
%%% This complex test is done to make sure that the internal handling
%%% of dehunt works as expected.
%%%
multi_dehunt(_Config) ->
    [P1] = multi_open(1,[]),

    Scenario =
	fun(Iterations) ->

	      Hunted = "p"++integer_to_list(Iterations),
	      %% Start a couple of hunts
	      Refs = [?INTERFACE:hunt(P1,Hunted) || _ <- lists:seq(1,Iterations)],

	      %% We alternate if the process is opened before or after the dehunt
	      P2O = if Iterations rem 2 == 0 ->
			    ?INTERFACE:open(Hunted);
		       true ->
			    undefined
		    end,

	      %% Remove half of them
	      {RemRefs,_} = lists:mapfoldl(fun(Ref,Acc) when Acc rem 2 == 0 ->
						   ok = ?INTERFACE:dehunt(P1,Ref),
						   {[],Acc+1};
					      (Ref,Acc) ->
						   {Ref,Acc+1}
					   end,0,Refs),

	      %% Add some new ones
	      NewRefs = [?INTERFACE:hunt(P1,Hunted)
			 || _ <- lists:seq(1,Iterations div 4)]
		    ++ lists:flatten(RemRefs),

	      P2 = if P2O == undefined ->
			   ?INTERFACE:open(Hunted);
		      true ->
			   P2O
		   end,
	      P2Id = ?INTERFACE:get_id(P2),

	      %% Receive all the expected ones
	      lists:foreach(fun(Ref) ->
				    receive
					{mailbox_up,P1,Ref,P2Id} ->
					    ok
				    after 1000 ->
					    io:format("Flush: ~p~n",[flush()]),
					    io:format("~p~n",[{Iterations,{did_not_get, Ref}}]),
					    ok = Ref
				    end
			    end,NewRefs),

	      %% Check that no other have arrived
	      receive
		  _Else ->
		      io:format("Flush: ~p~n",[flush()]),
		      io:format("~p~n",[{Iterations,{got, _Else}}]),
		      ok = _Else
	      after 100 ->
		      ok
	      end,
	      ?INTERFACE:close(P2)
      end,

    Self = self(),

    n(fun(N) ->
	      spawn(fun() -> Self !
				 Scenario(N*25)
		    end),
	      ok
      end,10),

    n(fun(_N) ->
	      receive ok -> ok
	      after 60000 -> ct:fail(failed)
	      end
      end,10),
    ?INTERFACE:close(P1).

attach(_Config) ->
    [P1,P2] = multi_open(2,[]),

    P2Id = ?INTERFACE:get_id(P2),
    Ref = ?INTERFACE:attach(P1,P2Id),
    ?INTERFACE:close(P2),
    receive
	{mailbox_down,P1,Ref,P2Id} ->
	    ?INTERFACE:close(P1);
	_Else ->
	    ct:fail({got,_Else, {P1,Ref,P2Id}})
    after 1000 ->
	    ct:fail({did_not_get,P1,Ref,P2Id})
    end.

multi_attach(_Config) ->

    Iterations = 100,

    [P1|Pids] = multi_open(Iterations,[]),

    Refs = [{?INTERFACE:get_id(Pid),?INTERFACE:attach(P1,?INTERFACE:get_id(Pid))} || Pid <- Pids],

    [?INTERFACE:close(Pid) || Pid <- Pids],

    [receive
	 {mailbox_down,P1,Ref,Pid} ->
	     ok
     after 10000 ->
	     ct:fail({did_not_get,Pid,Ref})
     end || {Pid,Ref} <- Refs],
    ?INTERFACE:close(P1).

detach(_Config)  ->
    [P1,P2] = multi_open(2,[]),
    P2Id = ?INTERFACE:get_id(P2),
    Ref = ?INTERFACE:attach(P1,P2Id),
    receive
	_Else -> ct:fail({got,_Else})
    after 100 ->
	    ok
    end,

    ?INTERFACE:close(P2),

    % Make sure any messages are sent
    receive after 10 -> ok end,

    ?INTERFACE:detach(P1,Ref),

    % Make sure no messages are received
    receive
	_Else2 -> ct:fail({got,_Else2})
    after 1000 ->
	    ?INTERFACE:close(P1)
    end.

%%%
%%% This testcase basically:
%%%  spawn 10 processes that in parallel
%%%        adds some attach for different OSE processes
%%%        maybe close OSE process
%%%        dehunt half of the hunts
%%%        create more hunts
%%%        if not closed close attached OSE process
%%%        veryify that all expected attach messages are received
%%%  verify that all processes exited correctly
%%%
%%% This complex test is done to make sure that the internal handling
%%% of dehunt works as expected.
%%%
multi_detach(_Config) ->
    [P1] = multi_open(1,[]),

    Scenario =
	fun(Iterations) ->

	      Attached = ?INTERFACE:open("p"++integer_to_list(Iterations)),
	      AttachedId = ?INTERFACE:get_id(Attached),
	      %% Start a couple of attachs
	      Refs = [?INTERFACE:attach(P1,AttachedId) || _ <- lists:seq(1,Iterations)],

	      %% We alternate if the process is closed before or after the detach
	      P2O = if Iterations rem 2 == 0 ->
			    ?INTERFACE:close(Attached);
		       true ->
			    undefined
		    end,

	      %% Remove half of them
	      {RemRefs,_} = lists:mapfoldl(fun(Ref,Acc) when Acc rem 2 == 0 ->
						   ok = ?INTERFACE:detach(P1,Ref),
						   {[],Acc+1};
					      (Ref,Acc) ->
						   {Ref,Acc+1}
					   end,0,Refs),

	      %% Add some new ones
	      NewRefs = [?INTERFACE:attach(P1,AttachedId)
			 || _ <- lists:seq(1,Iterations div 4)]
		    ++ lists:flatten(RemRefs),

	      if P2O == undefined ->
		      ?INTERFACE:close(Attached);
		 true ->
		      P2O
	      end,

	      %% Receive all the expected ones
	      lists:foreach(fun(Ref) ->
				    receive
					{mailbox_down,P1,Ref,AttachedId} ->
					    ok
				    after 1000 ->
					    io:format("Flush: ~p~n",[flush()]),
					    io:format("~p~n",[{Iterations,{did_not_get, Ref}}]),
					    ok = Ref
				    end
			    end,NewRefs),

	      %% Check that no other have arrived
	      receive
		  _Else ->
		      io:format("Flush: ~p~n",[flush()]),
		      io:format("~p~n",[{Iterations,{got, _Else}}]),
		      ok = _Else
	      after 100 ->
		      ok
	      end
      end,

    Self = self(),

    n(fun(N) ->
	      spawn(fun() -> Self !
				 Scenario(N*5)
		    end),
	      ok
      end,10),

    n(fun(_N) ->
	      receive ok -> ok
	      after 60000 -> ct:fail(failed)
	      end
      end,10),
    ?INTERFACE:close(P1).


open_errors(_Config) ->
    {'EXIT',{badarg,[{?INTERFACE,open,[inval],_}|_]}} =
	(catch ?INTERFACE:open(inval)),
    {'EXIT',{badarg,[{?INTERFACE,open,[["p"|1]],_}|_]}} =
	(catch ?INTERFACE:open(["p"|1])),
    {'EXIT',{badarg,[{?INTERFACE,open,[["p",1234]],_}|_]}} =
	(catch ?INTERFACE:open(["p",1234])),

    ok.

close_errors(_Config) ->
    {'EXIT',{badarg,[{?INTERFACE,close,[inval],_}|_]}} =
	(catch ?INTERFACE:close(inval)),

    P1 = ?INTERFACE:open("p1"),
    ok = ?INTERFACE:close(P1),
    ok = ?INTERFACE:close(P1).


get_id_errors(_Config) ->
    {'EXIT',{badarg,[{?INTERFACE,get_id,[inval],_}|_]}} =
	(catch ?INTERFACE:get_id(inval)),

    P1 = ?INTERFACE:open("p1"),
    ok = ?INTERFACE:close(P1),
    {'EXIT',{badarg,[{?INTERFACE,get_id,[P1],_}|_]}} =
	(catch ?INTERFACE:get_id(P1)),

    ok.

get_name_errors(_Config) ->
    P1 = ?INTERFACE:open("p1"),
    {'EXIT',{badarg,[{?INTERFACE,get_name,[P1,inval],_}|_]}} =
	(catch ?INTERFACE:get_name(P1,inval)),

    undefined = ?INTERFACE:get_name(P1,1234),

    P2 = ?INTERFACE:open("p2"),
    P2Id = ?INTERFACE:get_id(P2),
    ok = ?INTERFACE:close(P1),
    {'EXIT',{badarg,[{?INTERFACE,get_name,[P1,P2Id],_}|_]}} =
	(catch ?INTERFACE:get_name(P1,P2Id)),
    ?INTERFACE:close(P2),

    P3 = ?INTERFACE:open([255]),
    <<255>> = ?INTERFACE:get_name(P3, ?INTERFACE:get_id(P3)),
    ?INTERFACE:close(P3),

    ok.

hunt_errors(_Config) ->

    {'EXIT',{badarg,[{?INTERFACE,hunt,[inval,"hello"],_}|_]}} =
	(catch ?INTERFACE:hunt(inval,"hello")),

    P1 = ?INTERFACE:open("p1"),
    {'EXIT',{badarg,[{?INTERFACE,hunt,[P1,["hello",12345]],_}|_]}} =
	(catch ?INTERFACE:hunt(P1,["hello",12345])),

    P2 = ?INTERFACE:open(<<255>>),
    P2Pid = ?INTERFACE:get_id(P2),
    Ref = ?INTERFACE:hunt(P1,[255]),
    receive
	{mailbox_up,P1,Ref,P2Pid} ->
	    ok;
	Else ->
	    ct:fail({got,Else,{mailbox_up,P1,Ref,P2Pid}})
    after 150 ->
	    ct:fail({did_not_get,{mailbox_up,P1,Ref,P2Pid}})
    end,

    ok = ?INTERFACE:close(P1),
    ok = ?INTERFACE:close(P2),
    {'EXIT',{badarg,[{?INTERFACE,hunt,[P1,["hello"]],_}|_]}} =
	(catch ?INTERFACE:hunt(P1,["hello"])),

    ok.

dehunt_errors(_Config) ->
    P1 = ?INTERFACE:open("p1"),
    Ref = ?INTERFACE:hunt(P1,"p2"),

    {'EXIT',{badarg,[{?INTERFACE,dehunt,[inval,Ref],_}|_]}} =
	(catch ?INTERFACE:dehunt(inval,Ref)),

    {'EXIT',{badarg,[{?INTERFACE,dehunt,[P1,inval],_}|_]}} =
	(catch ?INTERFACE:dehunt(P1,inval)),

    ok = ?INTERFACE:dehunt(P1,Ref),
    ok = ?INTERFACE:dehunt(P1,Ref),

    ok = ?INTERFACE:close(P1),

    {'EXIT',{badarg,[{?INTERFACE,dehunt,[P1,Ref],_}|_]}} =
	(catch ?INTERFACE:dehunt(P1,Ref)),

    case ?INTERFACE of
	ose -> ok;
	_ ->
	    P2 = ?INTERFACE:open("p2"),
	    ok = ?INTERFACE:close(P2)
    end,

    receive
	Else -> ct:fail({got,Else})
    after 100 ->
	    ok
    end.

attach_errors(_Config) ->
    P1 = ?INTERFACE:open("p1"),
    P2 = ?INTERFACE:open("p2"),
    P2Id = ?INTERFACE:get_id(P2),

    {'EXIT',{badarg,[{?INTERFACE,attach,[inval,P2Id],_}|_]}} =
	(catch ?INTERFACE:attach(inval,P2Id)),

    {'EXIT',{badarg,[{?INTERFACE,attach,[P1,[12345]],_}|_]}} =
	(catch ?INTERFACE:attach(P1,[12345])),

    ok = ?INTERFACE:close(P1),
    ok = ?INTERFACE:close(P2),
    {'EXIT',{badarg,[{?INTERFACE,attach,[P1,P2Id],_}|_]}} =
	(catch ?INTERFACE:attach(P1,P2Id)),

    ok.

detach_errors(_Config) ->
    P1 = ?INTERFACE:open("p1"),
    P2 = ?INTERFACE:open("p2"),
    P2Id = ?INTERFACE:get_id(P2),

    Ref = ?INTERFACE:attach(P1,P2Id),

    {'EXIT',{badarg,[{?INTERFACE,detach,[inval,Ref],_}|_]}} =
	(catch ?INTERFACE:detach(inval,Ref)),

    {'EXIT',{badarg,[{?INTERFACE,detach,[P1,inval],_}|_]}} =
	(catch ?INTERFACE:detach(P1,inval)),

    ok = ?INTERFACE:detach(P1,Ref),
    ok = ?INTERFACE:detach(P1,Ref),

    case ?INTERFACE of
	ose -> ok;
	_ ->
	    ok = ?INTERFACE:close(P1)
    end,

    ok = ?INTERFACE:close(P2),
    ok = ?INTERFACE:close(P1),

    {'EXIT',{badarg,[{?INTERFACE,detach,[P1,Ref],_}|_]}} =
	(catch ?INTERFACE:detach(P1,Ref)),

    receive
	Else -> ct:fail({got,Else})
    after 100 ->
	    ok
    end.

send_errors(_Config) ->
    P1 = ?INTERFACE:open("p1"),
    P2 = ?INTERFACE:open("p2"),
    P2Id = ?INTERFACE:get_id(P2),

    {'EXIT',{badarg,[{?INTERFACE,send,[inval,P2Id,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(inval,P2Id,42,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P1,inval,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P1,inval,42,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P1,P2Id,inval,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P1,P2Id,inval,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P1,P2Id,42,inval],_}|_]}} =
	(catch ?INTERFACE:send(P1,P2Id,42,inval)),

    ok = ?INTERFACE:close(P2),
    ok = ?INTERFACE:send(P1,P2Id,42,"hello"),
    ok = ?INTERFACE:close(P1),

    {'EXIT',{badarg,[{?INTERFACE,send,[P1,P2Id,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P1,P2Id,42,"hello")),

    receive
	Else -> ct:fail({got,Else})
    after 100 ->
	    ok
    end.

send_w_s_errors(_Config) ->
    P1 = ?INTERFACE:open("p1"),
    P1Id = ?INTERFACE:get_id(P1),
    P2 = ?INTERFACE:open("p2"),
    P2Id = ?INTERFACE:get_id(P2),
    P3 = ?INTERFACE:open("p3"),
    P3Id = ?INTERFACE:get_id(P3),

    {'EXIT',{badarg,[{?INTERFACE,send,[inval,P2Id,P1Id,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(inval,P2Id,P1Id,42,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P2,-1,P1Id,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P2,-1,P1Id,42,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P2,P2Id,1 bsl 32,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P2,P2Id,1 bsl 32,42,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P2,P2Id,P1Id,inval,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P2,P2Id,P1Id,inval,"hello")),
    {'EXIT',{badarg,[{?INTERFACE,send,[P2,P2Id,P1Id,42,inval],_}|_]}} =
	(catch ?INTERFACE:send(P2,P2Id,P1Id,42,inval)),

    ok = ?INTERFACE:close(P3),
    ok = ?INTERFACE:send(P2,P3Id,P1Id,42,"hello"),

    ok = ?INTERFACE:close(P1),
    ok = ?INTERFACE:send(P2,P2Id,P1Id,42,"hello"),
    ok = ?INTERFACE:close(P2),

    {'EXIT',{badarg,[{?INTERFACE,send,[P1,P2Id,P1Id,42,"hello"],_}|_]}} =
	(catch ?INTERFACE:send(P1,P2Id,P1Id,42,"hello")),

    receive
	Else -> ct:fail({got,Else})
    after 100 ->
	    ok
    end.

listen_errors(_Config) ->

    P1 = ?INTERFACE:open("p1"),
    P1Id = ?INTERFACE:get_id(P1),

    {'EXIT',{badarg,[{?INTERFACE,listen,[inval,[42]],_}|_]}} =
	(catch ?INTERFACE:listen(inval,[42])),
    {'EXIT',{badarg,[{?INTERFACE,listen,[P1,inval],_}|_]}} =
	(catch ?INTERFACE:listen(P1,inval)),
    {'EXIT',{badarg,[{?INTERFACE,listen,[P1,[1 bsl 33]],_}|_]}} =
	(catch ?INTERFACE:listen(P1,[1 bsl 33])),

    ok = ?INTERFACE:listen(P1,[42,42,42,42,42,42,42,42,42,42,42,42,42]),

    case ?INTERFACE of
	ose -> ok;
	_ ->
	    ?INTERFACE:send(P1,P1Id,42,"hello"),
	    timer:sleep(50),
	    ?INTERFACE:listen(P1,[]),
	    ?INTERFACE:send(P1,P1Id,42,"hello2"),

	    receive
		{message,P1,42,"hello"} -> ok
	    end,

	    receive
		Else -> ct:fail({got,Else})
	    after 100 ->
		    ok
	    end
    end,

    ok = ?INTERFACE:close(P1),
    {'EXIT',{badarg,[{?INTERFACE,listen,[P1,[42]],_}|_]}} =
	(catch ?INTERFACE:listen(P1,[42])),

    ok.

%%
%% Internal functions
%%
multi_open(N,ListenNums) ->
    multi_open(N,ListenNums,[]).

multi_open(0,_,Acc) ->
    Acc;
multi_open(N,ListenNums,Acc) ->
    P = ?INTERFACE:open("p"++integer_to_list(N)),
    ok = ?INTERFACE:listen(P,ListenNums),
    multi_open(N-1,ListenNums,[P|Acc]).

n(_F,0) ->
    ok;
n(F,N) ->
    ok = F(N),
    n(F,N-1).


flush() ->
    receive
	Msg ->
	    [Msg|flush()]
    after 0 ->
	    []
    end.
