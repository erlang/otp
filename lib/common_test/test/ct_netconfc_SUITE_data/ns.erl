%%--------------------------------------------------------------------
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2017. All Rights Reserved.
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
%% A netconf server used for testing of netconfc
-module(ns).

%-compile(export_all).
-include_lib("common_test/src/ct_netconfc.hrl").


%%%-----------------------------------------------------------------
%%% API
-export([start/1,
	 stop/1,
	 hello/1,
	 hello/2,
	 expect/1,
	 expect/2,
	 expect_reply/2,
	 expect_reply/3,
	 expect_do/2,
	 expect_do/3,
	 expect_do_reply/3,
	 expect_do_reply/4,
	 hupp/1,
	 hupp/2]).

%%%-----------------------------------------------------------------
%%% ssh_channel callbacks
-export([init/1,
	 terminate/2,
	 handle_ssh_msg/2,
	 handle_msg/2]).

%%%-----------------------------------------------------------------
%% Server specifications
-define(SERVER_DATA_NAMESPACE, "ClientTest").
-define(CAPABILITIES,?CAPABILITIES_VSN("1.0")).
-define(CAPABILITIES_VSN(Vsn),
	[
	 ?NETCONF_BASE_CAP ++ Vsn,
	 "urn:ietf:params:netconf:capability:writable-running:1.0",
	 "urn:ietf:params:netconf:capability:candidate:1.0",
	 "urn:ietf:params:netconf:capability:confirmed-commit:1.0",
	 "urn:ietf:params:netconf:capability:rollback-on-error:1.0",
	 "urn:ietf:params:netconf:capability:startup:1.0",
	 "urn:ietf:params:netconf:capability:url:1.0",
	 "urn:ietf:params:netconf:capability:xpath:1.0",
	 "urn:ietf:params:netconf:capability:notification:1.0",
	 "urn:ietf:params:netconf:capability:interleave:1.0",
	 ?ACTION_NAMESPACE,
	 ?SERVER_DATA_NAMESPACE
	]).
-define(SSH_PORT, 2060).
-define(ssh_config(Dir),[{port, ?SSH_PORT},
			 {interface, {127,0,0,1}},
			 {system_dir, Dir},
			 {user_dir, Dir},
			 {user_passwords, [{"xxx","xxx"}]},
			 {password, "global-xxx"}]).

%% Some help for debugging
%-define(dbg(F,A),io:format(F,A)).
-define(dbg(F,A),ok).
-define(dbg_event(Event,Expect),
	?dbg("Event: ~p~nExpected: ~p~n",[Event,Expect])).

%% State
-record(session, {cb,
		  connection,
		  buffer = <<>>,
		  session_id}).


%%%-----------------------------------------------------------------
%%% API

%% Start the netconf server and use the given directory as system_dir
%% and user_dir
start(Dir) ->
    spawn(fun() -> init_server(Dir) end).

%% Stop the netconf server
stop(Pid) ->
    Ref = erlang:monitor(process,Pid),
    Pid ! stop,
    receive {'DOWN',Ref,process,Pid,_} -> ok end.

%% Set the session id for the hello message.
%% If this is not called prior to starting the session, no hello
%% message will be sent.
%% 'Stuff' indicates some special handling to e.g. provoke error cases
hello(SessionId) ->
    hello(SessionId,undefined).
hello(SessionId,Stuff) ->
    insert(hello,{SessionId,Stuff}).

%% Tell server to expect the given message without doing any further
%% actions. To be called directly before sending a request.
expect(Expect) ->
    expect_do_reply(Expect,undefined,undefined).
expect(SessionId,Expect) ->
    expect_do_reply(SessionId,Expect,undefined,undefined).

%% Tell server to expect the given message and reply with the give
%% reply. To be called directly before sending a request.
expect_reply(Expect,Reply) ->
    expect_do_reply(Expect,undefined,Reply).
expect_reply(SessionId,Expect,Reply) ->
    expect_do_reply(SessionId,Expect,undefined,Reply).

%% Tell server to expect the given message and perform an action. To
%% be called directly before sending a request.
expect_do(Expect,Do) ->
    expect_do_reply(Expect,Do,undefined).
expect_do(SessionId,Expect,Do) ->
    expect_do_reply(SessionId,Expect,Do,undefined).

%% Tell server to expect the given message, perform an action and
%% reply with the given reply. To be called directly before sending a
%% request.
expect_do_reply(Expect,Do,Reply) ->
    add_expect(1,{Expect,Do,Reply}).
expect_do_reply(SessionId,Expect,Do,Reply) ->
    add_expect(SessionId,{Expect,Do,Reply}).

%% Hupp the server - i.e. tell it to do something -
%% e.g. hupp(send_event) will cause send_event(State) to be called on
%% the session channel process.
hupp({send_events,N}) ->
    hupp(send,[make_msg({event,N})]);
hupp(kill) ->
    hupp(1,fun hupp_kill/1,[]).

hupp(send,Data) ->
    hupp(1,fun hupp_send/2,[Data]).

hupp(SessionId,Fun,Args) when is_function(Fun) ->
    [{_,Pid}] = lookup({channel_process,SessionId}),
    Pid ! {hupp,Fun,Args}.

%%%-----------------------------------------------------------------
%%% Main loop of the netconf server
init_server(Dir) ->
    register(main_ns_proc,self()),
    ets:new(ns_tab,[set,named_table,public]),
    Config = ?ssh_config(Dir),
    {_,Host} = lists:keyfind(interface, 1, Config),
    {_,Port} = lists:keyfind(port, 1, Config),
    Opts = lists:filter(fun({Key,_}) ->
				lists:member(Key,[system_dir,
						  password,
						  user_passwords,
						  pwdfun])
			end,
			Config),
    {ok, Daemon} =
	ssh:daemon(Host, Port,
		   [{subsystems,[{"netconf",{?MODULE,[]}}]}
		    |Opts]),
    loop(Daemon).

loop(Daemon) ->
    receive
	stop ->
	    ssh:stop_daemon(Daemon),
	    ok;
	{table_trans,Fun,Args,From} ->
	    %% Simple transaction mechanism for ets table
	    R = apply(Fun,Args),
	    From ! {table_trans_done,R},
	    loop(Daemon)
    end.

%%----------------------------------------------------------------------
%% Behaviour callback functions (ssh_channel)
%%----------------------------------------------------------------------
init([]) ->
    {ok, undefined}.

terminate(_Reason, _State) ->
    ok.

handle_ssh_msg({ssh_cm,CM,{data, Ch, _Type = 0, Data}}, State) ->
    %% io:format("~p~n",[{self(),Data,CM,Ch,State}]),
    data_for_channel(CM, Ch, Data, State);
handle_ssh_msg({ssh_cm,CM,{closed, Ch}}, State)  ->
    %% erlang:display({self(),closed,CM,Ch,State}),
    stop_channel(CM, Ch, State);
handle_ssh_msg({ssh_cm,CM,{eof, Ch}}, State) ->
    %% erlang:display({self(),eof,CM,Ch,State}),
    data_for_channel(CM,Ch, <<>>, State).


handle_msg({'EXIT', _Pid, _Reason}, State) ->
    {ok, State};
handle_msg({ssh_channel_up,Ch,CM},undefined) ->
    %% erlang:display({self(),up,CM,Ch}),
    ConnRef = {CM,Ch},
    SessionId = maybe_hello(ConnRef),
    insert({channel_process,SessionId},self()), % used to hupp the server
    {ok, #session{connection = ConnRef,
		  session_id = SessionId}};
handle_msg({hupp,Fun,Args},State) ->
    {ok,apply(Fun,Args ++ [State])}.

data_for_channel(CM, Ch, Data, State) ->
    try data(Data, State) of
	{ok, NewState} ->
	    case erase(stop) of
		true ->
		    stop_channel(CM, Ch, NewState);
		_ ->
		    {ok, NewState}
	    end
    catch
	Class:Reason:Stacktrace ->
	    error_logger:error_report([{?MODULE, data_for_channel},
				       {request, Data},
				       {buffer, State#session.buffer},
				       {reason, {Class, Reason}},
				       {stacktrace, Stacktrace}]),
	    stop_channel(CM, Ch, State)
    end.

data(Data, State = #session{connection = ConnRef,
			    buffer = Buffer,
			    session_id = SessionId}) ->
    AllData = <<Buffer/binary,Data/binary>>,
    case find_endtag(AllData) of
	{ok,Msgs,Rest} ->
	    [check_expected(SessionId,ConnRef,Msg) || Msg <- Msgs],
	    {ok,State#session{buffer=Rest}};
	need_more ->
	    {ok,State#session{buffer=AllData}}
    end.

stop_channel(CM, Ch, State) ->
    ssh_connection:close(CM,Ch),
    {stop, Ch, State}.


%%%-----------------------------------------------------------------
%%% Functions to trigg via hupp/1:

%% Send data spontaneously - e.g. an event
hupp_send(Data,State = #session{connection = ConnRef}) ->
    send(ConnRef,Data),
    State.
hupp_kill(State = #session{connection = ConnRef}) ->
    kill(ConnRef),
    State.

%%%-----------------------------------------------------------------
%%% Internal functions


%%% Send ssh data to the client
send({CM,Ch},Data) ->
    ssh_connection:send(CM, Ch, Data).

%%% Split into many small parts and send to client
send_frag({CM,Ch},Data) ->
    Sz = rand:uniform(1000),
    case Data of
	<<Chunk:Sz/binary,Rest/binary>> ->
	    ssh_connection:send(CM, Ch, Chunk),
	    send_frag({CM,Ch},Rest);
	Chunk ->
	    ssh_connection:send(CM, Ch, Chunk)
    end.


%%% Kill ssh connection
kill({CM,Ch}) ->
    ssh_connection:close(CM,Ch).

add_expect(SessionId,Add) ->
    table_trans(fun do_add_expect/2,[SessionId,Add]).

table_trans(Fun,Args) ->
    S = self(),
    case whereis(main_ns_proc) of
	S ->
	    apply(Fun,Args);
	Pid ->
	    Ref = erlang:monitor(process,Pid),
	    Pid ! {table_trans,Fun,Args,self()},
	    receive
		{table_trans_done,Result} ->
		    erlang:demonitor(Ref,[flush]),
		    Result;
		{'DOWN',Ref,process,Pid,Reason} ->
		    exit({main_ns_proc_died,Reason})
	    after 20000 ->
		    exit(table_trans_timeout)
	    end
    end.

do_add_expect(SessionId,Add) ->
    case lookup({expect,SessionId}) of
	[] ->
	    insert({expect,SessionId},[Add]);
	[{_,First}] ->
	    insert({expect,SessionId},First ++ [Add])
    end,
    ok.

do_get_expect(SessionId) ->
    case lookup({expect,SessionId}) of
	[{_,[{Expect,Do,Reply}|Rest]}] ->
	    insert({expect,SessionId},Rest),
	    {Expect,Do,Reply};
	_ ->
	    error
    end.

insert(Key,Value) ->
    ets:insert(ns_tab,{Key,Value}).
lookup(Key) ->
    ets:lookup(ns_tab,Key).

maybe_hello(ConnRef) ->
    case lookup(hello) of
	[{hello,{SessionId,Stuff}}] ->
	    %% erlang:display({SessionId,Stuff}),
	    ets:delete(ns_tab,hello),
	    insert({session,SessionId},ConnRef),
	    reply(ConnRef,{hello,SessionId,Stuff}),
	    SessionId;
	[] ->
	    undefined
    end.

find_endtag(Data) ->
    case binary:split(Data,[?END_TAG],[global]) of
	[Data] ->
	    need_more;
	Msgs ->
	    {ok,lists:sublist(Msgs,length(Msgs)-1),lists:last(Msgs)}
    end.

check_expected(SessionId,ConnRef,Msg) ->
    %% io:format("~p~n",[{check_expected,SessionId,Msg}]),
    case table_trans(fun do_get_expect/1,[SessionId]) of
	{Expect,Do,Reply} ->
	    %% erlang:display({got,io_lib:format("~s",[Msg])}),
	    %% erlang:display({expected,Expect}),
	    match(Msg,Expect),
	    do(ConnRef, Do),
	    reply(ConnRef,Reply);
	error ->
	    ct:sleep(1000),
	    exit({error,{got_unexpected,SessionId,Msg,ets:tab2list(ns_tab)}})
    end.

match(Msg,Expect) ->
    ?dbg("Match: ~p~n",[Msg]),
    {ok,ok,<<>>} = xmerl_sax_parser:stream(Msg,[{event_fun,fun event/3},
						{event_state,Expect}]).

event(Event,_Loc,Expect) ->
    ?dbg_event(Event,Expect),
    event(Event,Expect).

event(startDocument,Expect) -> match(Expect);
event({startElement,_,Name,_,Attrs},[{se,Name}|Match]) ->
    msg_id(Name,Attrs),
    Match;
event({startElement,_,Name,_,Attrs},[ignore,{se,Name}|Match]) ->
    msg_id(Name,Attrs),
    Match;
event({startElement,_,Name,_,Attrs},[{se,Name,As}|Match]) ->
    msg_id(Name,Attrs),
    match_attrs(Name,As,Attrs),
    Match;
event({startElement,_,Name,_,Attrs},[ignore,{se,Name,As}|Match]) ->
    msg_id(Name,Attrs),
    match_attrs(Name,As,Attrs),
    Match;
event({startPrefixMapping,_,Ns},[{ns,Ns}|Match]) -> Match;
event({startPrefixMapping,_,Ns},[ignore,{ns,Ns}|Match]) -> Match;
event({endPrefixMapping,_},Match) -> Match;
event({characters,Chs},[{characters,Chs}|Match]) -> Match;
event({endElement,_,Name,_},[{ee,Name}|Match]) -> Match;
event({endElement,_,Name,_},[ignore,{ee,Name}|Match]) -> Match;
event(endDocument,Match) when Match==[]; Match==[ignore] -> ok;
event(_,[ignore|_]=Match) -> Match;
event(Event,Match) -> throw({nomatch,{Event,Match}}).

msg_id("rpc",Attrs) ->
    case lists:keyfind("message-id",3,Attrs) of
	{_,_,_,Str} -> put(msg_id,Str);
	false -> erase(msg_id)
    end;
msg_id(_,_) ->
    ok.

match_attrs(Name,[{Key,Value}|As],Attrs) ->
    case lists:keyfind(atom_to_list(Key),3,Attrs) of
	{_,_,_,Value} -> match_attrs(Name,As,Attrs);
	false -> throw({missing_attr,Key,Name,Attrs});
	_ -> throw({faulty_attr_value,Key,Name,Attrs})
    end;
match_attrs(_,[],_) ->
    ok.

do(ConnRef, close) ->
    ets:match_delete(ns_tab,{{session,'_'},ConnRef}),
    put(stop,true);
do(_ConnRef, {kill,SessionId}) ->
    case lookup({session,SessionId}) of
	[{_,Owner}] ->
	    ets:delete(ns_tab,{session,SessionId}),
	    kill(Owner);
	_ ->
	    exit({no_session_to_kill,SessionId})
    end;
do(_, undefined) ->
    ok.

reply(_,undefined) ->
    ?dbg("no reply~n",[]),
    ok;
reply(ConnRef,{fragmented,Reply}) ->
    ?dbg("Reply fragmented: ~p~n",[Reply]),
    send_frag(ConnRef,make_msg(Reply));
reply(ConnRef,Reply) ->
    ?dbg("Reply: ~p~n",[Reply]),
    send(ConnRef, make_msg(Reply)).

from_simple(Simple) ->
    unicode_c2b(xmerl:export_simple_element(Simple,xmerl_xml)).

xml(Content) when is_binary(Content) ->
    xml([Content]);
xml(Content) when is_list(Content) ->
    Msgs = [<<Msg/binary,"\n",?END_TAG/binary>> || Msg <- Content],
    MsgsBin = list_to_binary(Msgs),
    <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", MsgsBin/binary>>.

rpc_reply(Content) when is_binary(Content) ->
    MsgId = case erase(msg_id) of
		undefined -> <<>>;
		Id -> unicode_c2b([" message-id=\"",Id,"\""])
	    end,
    <<"<rpc-reply xmlns=\"",?NETCONF_NAMESPACE,"\"",MsgId/binary,">\n",
      Content/binary,"\n</rpc-reply>">>;
rpc_reply(Content) ->
    rpc_reply(unicode_c2b(Content)).

session_id(no_session_id) ->
    <<>>;
session_id(SessionId0) ->
    SessionId = unicode_c2b(integer_to_list(SessionId0)),
    <<"<session-id>",SessionId/binary,"</session-id>\n">>.

capabilities(undefined) ->
    CapsXml = unicode_c2b([["<capability>",C,"</capability>\n"]
			      || C <- ?CAPABILITIES]),
    <<"<capabilities>\n",CapsXml/binary,"</capabilities>\n">>;
capabilities({base,Vsn}) ->
    CapsXml = unicode_c2b([["<capability>",C,"</capability>\n"]
			      || C <- ?CAPABILITIES_VSN(Vsn)]),
    <<"<capabilities>\n",CapsXml/binary,"</capabilities>\n">>;
capabilities(no_base) ->
    [_|Caps] = ?CAPABILITIES,
    CapsXml = unicode_c2b([["<capability>",C,"</capability>\n"] || C <- Caps]),
    <<"<capabilities>\n",CapsXml/binary,"</capabilities>\n">>;
capabilities(no_caps) ->
    <<>>.

%%%-----------------------------------------------------------------
%%% Match received netconf message from the client.  Add a new clause
%%% for each new message to recognize. The clause argument shall match
%%% the Expect argument in expect/1, expect_reply/2 or
%%% expect_do_reply/3.
%%%
%%% match(term()) -> [Match].
%%% Match = ignore | {se,Name} | {se,Name,Attrs} | {ee,Name} |
%%%         {ns,Namespace} | {characters,Chs}
%%% Name = string()
%%% Chs = string()
%%% Attrs = [{atom(),string()}]
%%% Namespace = string()
%%%
%%% 'se' means start element, 'ee' means end element - i.e. to match
%%% an XML element you need one 'se' entry and one 'ee' entry with the
%%% same name in the match list. 'characters' can be used for matching
%%% character data (cdata) inside an element.
match(hello) ->
    [ignore,{se,"hello"},ignore,{ee,"hello"},ignore];
match('close-session') ->
    [ignore,{se,"rpc"},{se,"close-session"},
     {ee,"close-session"},{ee,"rpc"},ignore];
match('edit-config') ->
    [ignore,{se,"rpc"},{se,"edit-config"},{se,"target"},ignore,{ee,"target"},
     {se,"config"},ignore,{ee,"config"},{ee,"edit-config"},{ee,"rpc"},ignore];
match({'edit-config',{'default-operation',DO}}) ->
    [ignore,{se,"rpc"},{se,"edit-config"},{se,"target"},ignore,{ee,"target"},
     {se,"default-operation"},{characters,DO},{ee,"default-operation"},
     {se,"config"},ignore,{ee,"config"},{ee,"edit-config"},{ee,"rpc"},ignore];
match('get') ->
    match({get,subtree});
match({'get',FilterType}) ->
    [ignore,{se,"rpc"},{se,"get"},{se,"filter",[{type,atom_to_list(FilterType)}]},
     ignore,{ee,"filter"},{ee,"get"},{ee,"rpc"},ignore];
match('get-config') ->
    match({'get-config',subtree});
match({'get-config',FilterType}) ->
    [ignore,{se,"rpc"},{se,"get-config"},{se,"source"},ignore,{ee,"source"},
     {se,"filter",[{type,atom_to_list(FilterType)}]},ignore,{ee,"filter"},
     {ee,"get-config"},{ee,"rpc"},ignore];
match('copy-config') ->
    [ignore,{se,"rpc"},{se,"copy-config"},{se,"target"},ignore,{ee,"target"},
     {se,"source"},ignore,{ee,"source"},{ee,"copy-config"},{ee,"rpc"},ignore];
match('delete-config') ->
    [ignore,{se,"rpc"},{se,"delete-config"},{se,"target"},ignore,{ee,"target"},
     {ee,"delete-config"},{ee,"rpc"},ignore];
match('lock') ->
    [ignore,{se,"rpc"},{se,"lock"},{se,"target"},ignore,{ee,"target"},
     {ee,"lock"},{ee,"rpc"},ignore];
match('unlock') ->
    [ignore,{se,"rpc"},{se,"unlock"},{se,"target"},ignore,{ee,"target"},
     {ee,"unlock"},{ee,"rpc"},ignore];
match('kill-session') ->
    [ignore,{se,"rpc"},{se,"kill-session"},{se,"session-id"},ignore,
     {ee,"session-id"},{ee,"kill-session"},{ee,"rpc"},ignore];
match(action) ->
    [ignore,{se,"rpc"},{ns,?ACTION_NAMESPACE},{se,"action"},{se,"data"},ignore,
     {ee,"data"},{ee,"action"},{ee,"rpc"},ignore];
match({'create-subscription',Content}) ->
    [ignore,{se,"rpc"},{ns,?NETCONF_NOTIF_NAMESPACE},
     {se,"create-subscription"}] ++
	lists:flatmap(fun(X) ->
			      [{se,atom_to_list(X)},ignore,{ee,atom_to_list(X)}]
		      end, Content) ++
	[{ee,"create-subscription"},{ee,"rpc"},ignore];
match(any) ->
    [ignore].



%%%-----------------------------------------------------------------
%%% Make message to send to the client.
%%% Add a new clause for each new message that shall be sent. The
%%% clause shall match the Reply argument in expect_reply/2 or
%%% expect_do_reply/3.
make_msg({hello,SessionId,Stuff}) ->
    SessionIdXml = session_id(SessionId),
    CapsXml = capabilities(Stuff),
    xml(<<"<hello xmlns=\"",?NETCONF_NAMESPACE,"\">\n",CapsXml/binary,
	  SessionIdXml/binary,"</hello>">>);
make_msg(ok) ->
    xml(rpc_reply("<ok/>"));

make_msg({ok,Data}) ->
    xml(rpc_reply(from_simple({ok,Data})));

make_msg({data,Data}) ->
    xml(rpc_reply(from_simple({data,Data})));

make_msg({event,N}) ->
    Notification = <<"<notification xmlns=\"",?NETCONF_NOTIF_NAMESPACE,"\">"
	  "<eventTime>2012-06-14T14:50:54+02:00</eventTime>"
	  "<event xmlns=\"http://my.namespaces.com/event\">"
	  "<severity>major</severity>"
	  "<description>Something terrible happened</description>"
	  "</event>"
	  "</notification>">>,
    xml(lists:duplicate(N,Notification));
make_msg(Xml) when is_binary(Xml) orelse
		   (is_list(Xml) andalso is_binary(hd(Xml))) ->
    xml(Xml);
make_msg(Simple) when is_tuple(Simple) ->
    xml(from_simple(Simple)).

%%%-----------------------------------------------------------------
%%% Convert to unicode binary, since we use UTF-8 encoding in XML
unicode_c2b(Characters) ->
    unicode:characters_to_binary(Characters).
