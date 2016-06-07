%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

%%

-module(gstk).
-compile([{nowarn_deprecated_function,{gs,assq,2}},
          {nowarn_deprecated_function,{gs,creation_error,2}}]).

-export([start_link/4,
	 stop/1,
	 create/2,
	 config/2,
	 read/2,
	 destroy/2,
	 pid_died/2,
	 event/2,
	 request/2,
	 init/1,
	 create_impl/2,
	 config_impl/3,
	 read_impl/3,
	 destroy_impl/2,
	 worker_init/1,
	 worker_do/1,
	 make_extern_id/2,
	 to_color/1,
	 to_ascii/1,
	 exec/1,
	 call/1]).

-include("gstk.hrl").

start_link(GsId,FrontendNode,Owner,Options) ->
    case gs:assq(node,Options) of
	false ->
	    Gstk = spawn_link(gstk, init,[{GsId, FrontendNode, Owner, Options}]),
	    receive
		{ok, _PortHandler} ->
		    {ok, Gstk};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{value, Node} ->
	    rpc:call(Node,gen_server,start_link,[gstk, {Owner,Options},[]])
    end.

stop(BackendServ) -> 
    request(BackendServ,stop).

create(BackendServ,Args) ->
    request(BackendServ,{create,Args}).

config(BackendServ,Args) ->
    request(BackendServ,{config,Args}).

read(BackendServ,Args) ->
    request(BackendServ,{read,Args}).

destroy(BackendServ,Args) ->
    request(BackendServ,{destroy,Args}).

pid_died(BackendServ,Pid) ->
    request(BackendServ,{pid_died,Pid}).

call(Cmd) ->
    %%io:format("Call:~p~n",[Cmd]),
    gstk_port_handler:call(get(port_handler),Cmd).

exec(Cmd) ->
    gstk_port_handler:exec(Cmd). 

make_extern_id(IntId, DB) ->
    [{_,Node}] = ets:lookup(DB,frontend_node),
    {IntId,Node}.

event(BackendServ,Event) ->
    BackendServ!{event,Event}.

%% -----------------------------------------------------------------------------

request(Who,Msg) ->
    Who ! {self(),Msg},
    receive
	{gstk_reply,R} -> R;
	{'EXIT',Who,Reason} ->
	    self() ! {'EXIT',Who,Reason},
	    {error,Reason}
    end.


-record(state,{db,frontendnode,port_handler}).

%% ------------------------------------------------------------
%% Initialize
%%
init({GsId,FrontendNode,Owner,Opts}) ->
    put(gs_frontend,Owner),
    case gstk_port_handler:start_link(self()) of
	{error, Reason} ->
	    FrontendNode ! {error, Reason},
	    exit(normal);
	{ok, PortHandler} ->
	    FrontendNode ! {ok, PortHandler},
	    put(port_handler,PortHandler),
	    {ok,Port} = gstk_port_handler:ping(PortHandler),
	    put(port,Port),
	    exec("wm withdraw ."),
	    DB = gstk_db:init(Opts),
	    ets:insert(DB,{frontend_node,FrontendNode}),
	    put(worker,spawn_link(gstk,worker_init,[0])),
	    Gstkid = #gstkid{id=GsId,widget="",owner=Owner,objtype=gs},
	    gstk_db:insert_gs(DB,Gstkid),
	    gstk_font:init(),
	    loop(#state{db=DB,frontendnode=FrontendNode})
    end.

loop(State) ->
    receive
	X ->
	    case (doit(X,State)) of
		done -> loop(State);
		stop -> bye
	    end
    end.

reply(To,Msg) ->
    To ! {gstk_reply,Msg},
    done.

doit({From,{config, {Id, Opts}}},#state{db=DB}) ->
    reply(From,config_impl(DB,Id,Opts));
doit({From,{create, Args}}, #state{db=DB}) ->
    reply(From,create_impl(DB,Args));
doit({From,{read,{Id,Opt}}},#state{db=DB}) ->
    reply(From,read_impl(DB,Id,Opt));
doit({From,{pid_died, Pid}}, #state{db=DB}) ->
    pid_died_impl(DB, Pid),
    reply(From,gstk_db:get_deleted(DB));
doit({From,{destroy, Id}}, #state{db=DB}) ->
    destroy_impl(DB, gstk_db:lookup_gstkid(DB,Id)),
    reply(From,gstk_db:get_deleted(DB));

doit({From,dump_db},State) ->
    io:format("gstk_db:~p~n",[lists:sort(ets:tab2list(State#state.db))]),
    io:format("events:~p~n",[lists:sort(ets:tab2list(get(events)))]),
    io:format("options:~p~n",[lists:sort(ets:tab2list(get(options)))]),
    io:format("defaults:~p~n",[lists:sort(ets:tab2list(get(defaults)))]),
    io:format("kids:~p~n",[lists:sort(ets:tab2list(get(kids)))]),
    reply(From,State);

doit({From,stop},_State) ->
    gstk_port_handler:stop(get(port_handler)),
    exit(get(worker),kill),
    reply(From,stopped),
    stop;

doit({event,{Id, Etag, Args}},#state{db=DB}) ->
    case gstk_db:lookup_event(DB, Id, Etag) of
        {Etype, Edata} ->
            Gstkid = gstk_db:lookup_gstkid(DB, Id),
            apply(gstk_widgets:objmod(Gstkid),event,[DB,Gstkid,Etype,Edata,Args]);
        _ -> true
    end,
    done.


%%----------------------------------------------------------------------
%% Implementation of create,config,read,destroy
%% Comment: In the gstk process there is not concept call 'name', only
%%          pure oids. Names are stripped of by 'gs' and this simplifies
%%          gstk a lot.
%% Comment: For performance reasons gstk.erl ans gs.erl communicats through
%%          tuples. This is unfortunate but we don't want to pack the same
%%          thing too many times.
%% Pre (for all functions): GS guarantees that the object (and parent if
%%          necessary) exists.
%%----------------------------------------------------------------------


create_impl(DB, {Owner, {Objtype, Id, Parent, Opts}}) ->
    Pgstkid = gstk_db:lookup_gstkid(DB, Parent),
    GstkId=#gstkid{id=Id,owner=Owner,parent=Parent,objtype=Objtype},
    gstk_db:insert_opt(DB,Id,{data,[]}),
    RealOpts=apply(gstk_widgets:objmod(Pgstkid),
		   mk_create_opts_for_child,[DB,GstkId,Pgstkid,Opts]),
    case gstk_widgets:type2mod(Objtype) of
	{error,Reason} -> {error,Reason};
	ObjMod ->
	    case apply(ObjMod, create, [DB, GstkId, RealOpts]) of
		{bad_result, BR} ->
		    gstk_db:delete_gstkid(DB,GstkId),
		    gs:creation_error(GstkId,{bad_result, BR});
		Ngstkid when is_record(Ngstkid,gstkid) ->
		    gstk_db:insert_widget(DB, Ngstkid),
		    ok;
		{error,Reason} -> {error,Reason};
		ok -> ok
	    end
    end.

config_impl(DB,Id,Opts) ->
    Gstkid = gstk_db:lookup_gstkid(DB, Id), 
    case apply(gstk_widgets:objmod(Gstkid), config, [DB, Gstkid, Opts]) of
	ok -> ok;
	{bad_result,R} -> {error,R};
	{error,Reason} -> {error,Reason};
	Q -> {error,Q}
    end.


read_impl(DB,Id,Opt) ->
    Gstkid = gstk_db:lookup_gstkid(DB, Id), 
    case apply(gstk_widgets:objmod(Gstkid), read, [DB, Gstkid, Opt]) of
	{bad_result,R} -> {error,R};
	{error,R} -> {error,R};
	Res -> Res
    end.



%%-----------------------------------------------------------------------------
%%			DESTROYING A WIDGET
%%-----------------------------------------------------------------------------

destroy_impl(DB, Gstkid) ->
    worker_do({delay_is,50}),
    Widget = delete_only_this_widget(DB,Gstkid),
    destroy_widgets([Widget], DB),
    worker_do({delay_is,5}),
    true.

delete_only_this_widget(DB,Gstkid) ->
    #gstkid{id=ID,objtype=OT,parent=P} = Gstkid,
    delete_widgets(gstk_db:lookup_kids(DB, ID), DB),
    Widget = apply(gstk_widgets:type2mod(OT), delete, [DB, Gstkid]),
    gstk_db:delete_kid(DB, P, ID),
    Widget.


pid_died_impl(DB, Pid) ->
    case lists:sort(gstk_db:lookup_ids(DB, Pid)) of
	[ID | IDs] ->
	    Gstkid = gstk_db:lookup_gstkid(DB, ID),
	    destroy_impl(DB, Gstkid),
	    Tops = get_tops(IDs, DB),
	    destroy_widgets(Tops, DB);
	_ ->
	    true
    end.


get_tops([ID | IDs], DB) ->
    case gstk_db:lookup_gstkid(DB, ID) of
	undefined ->
	    get_tops(IDs, DB);
	Gstkid -> 
	    Parent = Gstkid#gstkid.parent,
	    case lists:member(Parent, IDs) of
		true  ->
		    delete_widgets([ID], DB),
		    get_tops(IDs, DB);
		false ->
		    Widget = delete_only_this_widget(DB,Gstkid),
		    [Widget | get_tops(IDs, DB)]
	    end
    end;
get_tops([], _DB) -> [].


delete_widgets([ID | Rest], DB) ->
    delete_widgets(gstk_db:lookup_kids(DB, ID), DB),
    case gstk_db:lookup_gstkid(DB, ID) of
	undefined ->
	    delete_widgets(Rest, DB);
	Gstkid ->
	    apply(gstk_widgets:objmod(Gstkid), delete, [DB, Gstkid]),
	    delete_widgets(Rest, DB)
    end;
delete_widgets([], _) -> true.



destroy_widgets(Widgets, DB) ->
    case destroy_wids(Widgets, DB) of
	[]       -> true;
	Destroys -> exec(["destroy ", Destroys])
    end.


destroy_wids([{Parent, ID, Objmod, Args} | Rest], DB) ->
    gstk_db:delete_kid(DB, Parent, ID),
    apply(Objmod, destroy, [DB | Args]),
    destroy_wids(Rest, DB);

destroy_wids([W | Rest], DB) ->
    [W, " "| destroy_wids(Rest, DB)];

destroy_wids([], _DB) -> [].


%% ----- The Color Model -----

to_color({R,G,B}) ->
    [$#,dec2hex(2,R),dec2hex(2,G),dec2hex(2,B)];
to_color(Color) when is_atom(Color) -> atom_to_list(Color).

%% ------------------------------------------------------------
%% Decimal to Hex converter
%% M is number of digits we want
%% N is the decimal to be converted

dec2hex(M,N) -> dec2hex(M,N,[]).

dec2hex(0,_N,Ack) -> Ack;
dec2hex(M,N,Ack) -> dec2hex(M-1,N bsr 4,[d2h(N band 15)|Ack]).

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.


%% ----- Value to String -----

to_ascii(V) when is_list(V)    -> [$",to_ascii(V,[],[]),$"];  %% it's a string
to_ascii(V) when is_integer(V) -> integer_to_list(V);
to_ascii(V) when is_float(V)   -> float_to_list(V);
to_ascii(V) when is_atom(V)    -> to_ascii( atom_to_list(V));
to_ascii(V) when is_tuple(V)   -> to_ascii(lists:flatten(io_lib:format("~w",[V])));
to_ascii(V) when is_pid(V)     -> pid_to_list(V).

						% FIXME: Currently we accept newlines in strings and handle this at
						% the Tcl side. Is this the best way or should we translate to "\n"
						% here?
to_ascii([$[|R], Y, X) ->  to_ascii(R, Y, [$[, $\\ | X]);
				    to_ascii([$]|R], Y, X) ->  to_ascii(R, Y, [$], $\\ | X]);
to_ascii([${|R], Y, X) ->  to_ascii(R, Y, [${, $\\ | X]);
				    to_ascii([$}|R], Y, X) ->  to_ascii(R, Y, [$}, $\\ | X]);
to_ascii([$"|R], Y, X) ->  to_ascii(R, Y, [$", $\\ | X]);
to_ascii([$$|R], Y, X) ->  to_ascii(R, Y, [$$, $\\ | X]);
to_ascii([$\\|R], Y, X) ->  to_ascii(R, Y, [$\\, $\\ | X]);
to_ascii([C|R], Y, X) when is_list(C) -> to_ascii(C, [R|Y], X);
to_ascii([C|R], Y, X) -> to_ascii(R, Y, [C|X]);
to_ascii([], [Y1|Y], X) -> to_ascii(Y1, Y, X);
to_ascii([], [], X) -> lists:reverse(X).

worker_do(Msg) ->
    get(worker) ! Msg.

worker_init(Delay) ->
    receive
	{delay_is,D} ->
	    worker_init(D);
	{match_delete,DBExprs} ->
	    worker_match(DBExprs),
	    if Delay > 0 ->
		    receive
			{delay_is,D} ->
			    worker_init(D)
		    after Delay ->
			    worker_init(Delay)
		    end;
	       true -> 
		    worker_init(Delay)
	    end
    end.

worker_match([{DB,[Expr|Exprs]}|DbExprs]) ->
    ets:match_delete(DB,Expr),
    worker_match([{DB,Exprs}|DbExprs]);
worker_match([{_DB,[]}|DbExprs]) ->
    worker_match(DbExprs);
worker_match([]) -> done.
