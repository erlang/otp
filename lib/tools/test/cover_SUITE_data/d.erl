-module(d).

-export([start/0, stop/0]).
-export([store/2, store/3, move/2,
	 location/1, who_are_at/1, who_are_older/1,
	 size/0]).
-export([init/0]). % spawn

-record(person, {name, age :: integer(), location, moved=false :: boolean()}).

%%%----------------------------------------------------------------------
%%% User interface functions
%%%----------------------------------------------------------------------

%%% start() -> pid()
start() ->
    spawn(?MODULE, init, []).

%%% stop()
stop() ->
    arne ! stop.

%%% store(Name, Location) ->
%%% store(Name, Age, Location) -> ok | {error,Reason}
%%%   Name = Location = atom()
%%%   Age = integer()
%%%   Reason = not_started | no_response | {internal_error,term()}
store(Name, Location) ->
    store(Name, ?AGE, Location).
store(Name, Age, Location) when atom(Name), integer(Age), atom(Location) ->
    send({store, Name, Age, Location}).

%%% move(OldLocation, NewLocation) -> Names | {error,Reason}
%%%   OldLocation = NewLocation = atom()
%%%   Names = [Name]
%%%     Name = atom()
%%%   Reason = not_started | no_response | {internal_error,term()}
move(OldLocation, NewLocation) ->
    send({move, OldLocation, NewLocation}).

%%% location(Name) -> Location | no_such_person | {error,Reason}
%%%   Name = atom()
%%%   Reason = not_started | no_response | {internal_error,term()}
location(Name) when atom(Name) ->
    send({location, Name}).

%%% who_are_at(Location) -> Names | {error,Reason}
%%%   Location = atom()
%%%   Names = [Name]
%%%     Name = atom()
%%%   Reason = not_started | no_response | {internal_error,term()}
who_are_at(Location) when atom(Location) ->
    send({who_are_at, Location}).

%%% who_are_older(Age) -> Names | {error,Reason}
%%%   Age = integer()
%%%   Names = [Name]
%%%     Name = atom()
%%%   Reason = not_started | no_response | {internal_error,term()}
who_are_older(Age) when integer(Age) ->
    send({who_are_older, Age}).

%%% size() -> N | {error,Reason}
%%%   N = integer()
%%%   Reason = not_started | no_response | {internal_error,term()}
size() ->
    send(size).

%%%----------------------------------------------------------------------
%%% Main loop
%%%----------------------------------------------------------------------
send(Request) ->
    Pid = whereis(arne),
    if
	Pid==undefined ->
	    {error, not_started};
	true ->
	    send(Pid, Request)
    end.
send(Pid, Request) ->
    Pid ! {request, self(), Request},
    receive
	{reply, Reply} ->
	    Reply
    after
	1000 ->
	    {error, no_response}
    end.
		
init() ->
    register(arne, self()),
    loop([]).

loop(Db) ->
    receive
	stop ->
	    true;
	{request, From, Request} ->
	    case catch handle(Request, Db) of
		{reply, Reply, NewDb} ->
		    From ! {reply, Reply},
		    loop(NewDb);
		{'EXIT', Reason} ->
		    From ! {reply, {error, {internal_error, Reason}}},
		    loop(Db)
	    end
    end.

%%%----------------------------------------------------------------------
%%% DB functionality
%%%----------------------------------------------------------------------
handle({store, Name, Age, Location}, Db) ->
    {reply, ok, [#person{name=Name, age=Age, location=Location} | Db]};
handle({move, OldLocation, NewLocation}, Db) ->
    {Names, NewDb} = move(OldLocation, NewLocation, Db, [], []),
    {reply, Names, NewDb};
handle({location, Name}, Db) ->
    case lists:keysearch(Name, #person.name, Db) of
	{value, #person{location=Location}} when atom(Location) ->
	    {reply, Location, Db};
	false ->
	    {reply, no_such_name, Db}
    end;
handle({who_are_at, Location}, Db) ->
    Result = lists:foldl(fun(Person, Names) ->
				 case Person#person.location of
				     Location ->
					 [Person#person.name | Names];
				     _OtherLocation ->
					 Names
				 end
			 end,
			 [],
			 Db),
    {reply, Result, Db};
handle({who_are_older, Old}, Db) ->
    Result = [Name || {person,Name,Age,Location} <- Db,
		   Age>Old],
    {reply, Result, Db};
handle(size, Db) ->
    Result = count(Db, 0), {reply, Result, Db}.

count([H|T], N) ->
    count(T, N+1);
count([], N) ->
    N.

move(OldLoc, NewLoc, [#person{location=OldLoc} = Person|T], Db, Names) ->
    NewPerson = Person#person{location=NewLoc,
			      moved=true},
    NewNames = [Person#person.name|Names],
    move(OldLoc, NewLoc, T, [NewPerson|Db], NewNames);
move(OldLoc, NewLoc, [Person|T], Db, Names) ->
    move(OldLoc, NewLoc, T, [Person|Db], Names);
move(OldLoc, NewLoc, [], Db, Names) ->
    {lists:reverse(Names), lists:reverse(Db)}.
