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
%% ------------------------------------------------------------
%%
%% Database interface for `gstk'.
%% 
%% ------------------------------------------------------------

-module(gstk_db).

-export([init/1,
	 insert/3,
	 lookup/2,
	 lookup_event/3,
	 insert_bgrp/2,
	 delete_bgrp/2,
	 insert_gs/2,
	 insert_widget/2,
	 delete_kid/3,
	 insert_opts/3,
	 lookup_def/3,
	 opt_or_not/3,
	 lookup_gstkid/3,
	 lookup_ids/2,
	 lookup_item/3,
	 delete_widget/2,
	 delete_gstkid/2,
	 get_deleted/1,
	 delete_event/3,
	 insert_event/4,
	 update_widget/2,
	 is_inserted/3,
	 lookup_kids/2,
	 insert_def/3,
	 opt/4,
	 opt/3,
	 insert_opt/3,
	 default_container_opts/3,
	 default_opts/3,
	 counter/2,
	 lookup_gstkid/2]).

-include("gstk.hrl").


%% ------------------------------------------------------------
%%                      INITIALIZATION
%% ------------------------------------------------------------

init(_Opts) ->
    put(events,ets:new(gstk_db, [public, set])),
    put(kids,ets:new(gstk_db, [public, bag])),
    put(defaults,ets:new(gstk_db, [public, bag])),
    put(deleted,ets:new(gstk_db, [public, bag])),
    put(options,ets:new(gstk_db, [public, set])),
    ets:new(gstk_db, [public, set]).

%% -----------------------------------------------------------------
%%                 PRIMITIVE DB INTERFACE
%% -----------------------------------------------------------------

insert(DB, Key, Value) ->
    ets:insert(DB, {Key, Value}).


lookup(DB, Key) ->
    Result =
	case ets:lookup(DB, Key) of
	    [{Key, Value}] -> Value;
	    _ -> undefined
	end,
    Result.


delete(DB, Key) ->
    ets:delete(DB, Key).



%% -----------------------------------------------------------------
%%               NOT SO PRIMITIVE DB INTERFACE
%% -----------------------------------------------------------------

%% -----------------------------------------------------------------
%%                  HANDLE EVENTS
%% -----------------------------------------------------------------
insert_event(DB, Gstkid, Etype, Edata) ->
    ID = Gstkid#gstkid.id,
    Rdata =
	case Edata of
	    [] -> opt(DB,ID,data);
	    _Other1 -> Edata
	end,
    Events = lookup_events(DB, ID),
    case lists:keysearch(Etype, 2, Events) of
	{value, {Etag, _, _}} ->
	    NewEvents =
		lists:keyreplace(Etype, 2, Events, {Etag, Etype, Rdata}),
	    ets:insert(get(events), {{events, ID}, NewEvents}),
	    [$#, gstk:to_ascii(ID), " ", Etag];
	_Other2 ->
	    Etag = etag(Etype),
	    NewEvents = [{Etag, Etype, Rdata} | Events],
	    ets:insert(get(events), {{events, ID}, NewEvents}),
	    [$#, gstk:to_ascii(ID), " ", Etag]
    end.

etag(Etype) ->
    case Etype of
	click -> "c";
	doubleclick -> "dc";
	configure -> "co";
	enter -> "e";
	leave -> "l";
	motion -> "m";
	buttonpress -> "bp";
	buttonrelease -> "br";
	focus -> "f";
	destroy -> "d";
	keypress -> "kp";
	keyrelease -> "kr"
    end.

lookup_events(_DB, ID) ->
    case lookup(get(events), {events, ID}) of
	undefined -> [];
	Events -> Events
    end.

lookup_event(DB, ID, Etag) ->
    case lists:keysearch(Etag, 1, lookup_events(DB, ID)) of
	{value, {Etag, Etype, Edata}} ->
	    {Etype, Edata};
	_Other ->
	    nonexisting_event
    end.

delete_event(DB, Gstkid, Etype) ->
    ID = Gstkid#gstkid.id,
    NewEvents = lists:keydelete(Etype, 2, lookup_events(DB, ID)),
    ets:insert(get(events), {{events, ID}, NewEvents}).

%% -----------------------------------------------------------------
%%                  HANDLE BUTTON GROUPS
%% -----------------------------------------------------------------
insert_bgrp(DB, Key) ->
    case ets:lookup(DB, Key) of
	[] ->
	    {_Bgrp, RG, _Owner} = Key,
	    insert(DB, Key, {0, RG}),
	    RG;
	[{_, {Counter, RG}}] ->
	    insert(DB, Key, {Counter+1, RG}),
	    RG
    end.


delete_bgrp(DB, Key) ->
    case ets:lookup(DB, Key) of
	[] ->
	    true;
	[{_, {0, _RG}}] ->
	    delete(DB, Key),
	    true;
	[{_, {Counter, RG}}] ->
	    insert(DB, Key, {Counter-1, RG}),
	    true
    end.


%% -----------------------------------------------------------------
%%  insert things

update_widget(DB, Gstkid) ->
    ID = Gstkid#gstkid.id,
    insert(DB, ID, Gstkid),
    Gstkid.

insert_gs(DB,Gstkid) ->
    update_widget(DB,Gstkid).

insert_widget(DB, Gstkid) ->
    ID = Gstkid#gstkid.id,
    insert_kid(DB, Gstkid#gstkid.parent, ID),
    insert(DB, ID, Gstkid),
    Gstkid.

insert_kid(_DB, Parent, Kid) ->
    ets:insert(get(kids), {{kids, Parent},Kid}).

delete_kid(_DB, Parent, Kid) ->
    ets:match_delete(get(kids), {{kids, Parent},Kid}).

lookup_kids(_DB, Parent) ->
    ril(ets:match(get(kids), {{kids, Parent},'$1'})).

%%----------------------------------------------------------------------
%% Options are stored as {{Id,Opt},Val}
%%----------------------------------------------------------------------
insert_opt(_DB,Id,{default,ObjType,Opt}) ->
    insert_def(Id,ObjType,Opt);
insert_opt(_DB,#gstkid{id=Id},{Key,Val}) ->
    ets:insert(get(options),{{Id,Key},Val});
insert_opt(_DB,Id,{Key,Val}) ->
    ets:insert(get(options),{{Id,Key},Val}).

insert_opts(_DB,_Id,[]) -> done;
insert_opts(DB,Id,[Opt|Opts]) ->
    insert_opt(DB,Id,Opt),
    insert_opts(DB,Id,Opts).

insert_def(#gstkid{id=ID},ObjType,{Key,Val}) ->
    insert_def(ID,ObjType,{Key,Val});
insert_def(ID,ObjType,{Key,Val}) ->
    Def = get(defaults),
    ets:match_delete(Def,{{ID,ObjType},{Key,'_'}}),
    ets:insert(Def,{{ID,ObjType},{Key,Val}}).

lookup_def(ID,ObjType,Key) ->
    case ets:match(get(defaults),{{ID,ObjType},{Key,'$1'}}) of
	[] -> false;
	[[Val]] -> {value,Val}
    end.

opt(DB,#gstkid{id=Id},Opt) -> opt(DB,Id,Opt);
opt(_DB,Id,Opt) ->
    [{_, Value}] = ets:lookup(get(options), {Id,Opt}),
    Value.

opt_or_not(DB,#gstkid{id=Id},Opt) -> opt_or_not(DB,Id,Opt);
opt_or_not(_DB,Id,Opt) ->
    case ets:lookup(get(options), {Id,Opt}) of
	[{_, Value}] -> {value, Value};
	_ -> false
    end.

opt(DB,#gstkid{id=Id},Opt,ElseVal) -> opt(DB,Id,Opt,ElseVal);
opt(_DB,Id,Opt,ElseVal) ->
    case ets:lookup(get(options), {Id,Opt}) of
	[{_, Value}] ->
	    Value;
	_ -> ElseVal
    end.

%%----------------------------------------------------------------------
%% Returns: list of {Key,Val}
%%----------------------------------------------------------------------
default_container_opts(_DB,Id,ChildType) ->
    L =	ets:match(get(defaults),{{Id,'$1'},'$2'}),
    lists:sort(fix_def_for_container(L,ChildType)).

default_opts(_DB,Id,ChildType) ->
    L1 = ets:lookup(get(defaults),{Id,ChildType}),
    L2 = ets:lookup(get(defaults),{Id,all}),
    lists:sort(fix_def(L1,L2)).

fix_def([{_,Opt}|Opts],Opts2) ->
    [Opt|fix_def(Opts,Opts2)];
fix_def([],[]) -> [];
fix_def([],Opts) ->
    fix_def(Opts,[]).

%%----------------------------------------------------------------------
%% Purpose: Extracs {default,ObjType,DefsultOpt} for the ChildType
%% and keeps default options since it is a container object.
%% Returns: list of options
%%----------------------------------------------------------------------
fix_def_for_container([[all,{Key,Val}]|Opts],ChildType) ->
    [{{default,all,Key},Val},{Key,Val}
     |fix_def_for_container(Opts,ChildType)];
fix_def_for_container([[ChildType,{Key,Val}]|Opts],ChildType) ->
    [{{default,ChildType,Key},Val},{Key,Val}
     |fix_def_for_container(Opts,ChildType)];
fix_def_for_container([[ChildType2,{Key,Val}]|Opts],_ChildType) ->
    [{{default,ChildType2,Key},Val}|fix_def_for_container(Opts,ChildType2)];
fix_def_for_container([],_) -> [].

%% -----------------------------------------------------------------
%%  lookup things

lookup_gstkid(DB, Name, Owner) when is_atom(Name) ->
    ID = lookup(DB, {Owner, Name}),
    lookup(DB, ID);

lookup_gstkid(DB, ID, _Owner) ->
    lookup(DB, ID).


lookup_gstkid(_DB, Name) when is_atom(Name) ->
    exit({'must use owner',Name});

lookup_gstkid(DB, ID) ->
    lookup(DB, ID).


lookup_ids(DB, Pid) ->
    ril(ets:match(DB, {'$1', {gstkid,'_','_','_',Pid,'_','_'}})).

lookup_item(DB, TkW, Item) ->
						%    [[Id]] = ets:match(DB, {'$1', {gstkid,'_',TkW, Item,'_','_','_'}}),
						%    Id.
    %% OTP-4167 Gif images gstkids are stored differently from other objects
    case ets:match(DB, {'$1', {gstkid,'_',TkW, Item,'_','_','_'}}) of
	[[Id]] ->
	    Id;
	[] ->
	    Pattern = {'$1', {gstkid,'_',TkW, {'_',Item},'_','_',image}},
	    [[Id]] = ets:match(DB, Pattern),
	    Id
    end.


%% -----------------------------------------------------------------
%% counters

counter(DB, Key) ->
    Result =
	case ets:lookup(DB, Key) of
	    [{Key, Value}] -> Value+1;
	    _ -> 0
	end,
    ets:insert(DB, {Key, Result}),
    Result.


%% -----------------------------------------------------------------
%% delete things

delete_widgets(DB, [ID | Rest]) ->
    delete_widget(DB, ID),
    delete_widgets(DB, Rest);
delete_widgets(_, []) ->
    true.


delete_widget(DB, #gstkid{id = ID}) ->
    delete_widget(DB, ID);
delete_widget(DB, ID) ->
    delete_widgets(DB, lookup_kids(DB, ID)),
    delete_id(DB, ID).

delete_gstkid(DB,Gstkid) ->
    delete_id(DB,Gstkid).

delete_id(DB, ID) ->
    case lookup_gstkid(DB, ID) of
	undefined ->
	    true;
	_Gstkid     ->
	    gstk:worker_do({match_delete,[{get(options),[{{ID,'_'},'_'}]},
					  {get(defaults),[{{ID,'_'},'_'}]}]}),
	    ets:insert(get(deleted),{deleted,ID}),
	    delete(DB, ID)
    end,
    ets:delete(get(kids), {kids, ID}),
    delete(get(events), {events, ID}),
    true.

get_deleted(_DB) ->
    Dd = get(deleted),
    R=fix_deleted(ets:lookup(Dd,deleted)),
    ets:delete(Dd,deleted),
    R.

fix_deleted([{_,Id}|Dd]) ->
    [Id | fix_deleted(Dd)];
fix_deleted([]) -> [].

%% -----------------------------------------------------------------
%% odd stuff

%% check if an event is in the database, used by read_option
is_inserted(DB, #gstkid{id = ID}, What) ->
    is_inserted(DB, ID, What);
is_inserted(_DB, ID, What) ->
    case lookup(get(events), {events, ID}) of
	undefined -> false;
	Events -> 
	    case lists:keysearch(What, 2, Events) of
		{value, _} -> true;
		_Other      -> false
	    end
    end.

%% -----------------------------------------------------------------
%%                    PRIMITIVES
%% -----------------------------------------------------------------

%% remove irritating lists
ril([[Foo] | Rest]) -> [Foo | ril(Rest)];
ril([]) -> [].



