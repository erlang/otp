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
-module(auth).
-behaviour(gen_server).

-export([start_link/0]).

%% Old documented interface - deprecated
-export([is_auth/1, cookie/0, cookie/1, node_cookie/1, node_cookie/2]).
-deprecated([{is_auth,1}, {cookie,'_'}, {node_cookie, '_'}]).

%% New interface - meant for internal use within kernel only
-export([get_cookie/0, get_cookie/1,
	 set_cookie/1, set_cookie/2,
	 sync_cookie/0,
	 print/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(COOKIE_ETS_PROTECTION, protected). 

-type cookie() :: atom().
-record(state, {
	  our_cookie    :: cookie(),  %% Our own cookie
	  other_cookies :: ets:tab()  %% The send-cookies of other nodes
	 }).
-type state() :: #state{}.

-include("../include/file.hrl").

%%----------------------------------------------------------------------
%% Exported functions
%%----------------------------------------------------------------------

-spec start_link() -> {'ok',pid()} | {'error', term()} | 'ignore'.

start_link() ->
    gen_server:start_link({local, auth}, auth, [], []).

%%--Deprecated interface------------------------------------------------

-spec is_auth(Node) -> 'yes' | 'no' when
      Node :: node().

is_auth(Node) ->
    case net_adm:ping(Node) of
	pong -> yes;
	pang -> no
    end.

-spec cookie() -> Cookie when
      Cookie :: cookie().

cookie() ->
    get_cookie().

-spec cookie(TheCookie) -> 'true' when
      TheCookie :: Cookie | [Cookie],
      Cookie :: cookie().

cookie([Cookie]) ->
    set_cookie(Cookie);
cookie(Cookie) ->
    set_cookie(Cookie).

-spec node_cookie(Cookies :: [node() | cookie(),...]) -> 'yes' | 'no'.

node_cookie([Node, Cookie]) ->
    node_cookie(Node, Cookie).

-spec node_cookie(Node, Cookie) -> 'yes' | 'no' when
      Node :: node(),
      Cookie :: cookie().

node_cookie(Node, Cookie) ->
    set_cookie(Node, Cookie),
    is_auth(Node).

%%--"New" interface-----------------------------------------------------

-spec get_cookie() -> 'nocookie' | cookie().

get_cookie() ->
    get_cookie(node()).

-spec get_cookie(Node :: node()) -> 'nocookie' | cookie().

get_cookie(_Node) when node() =:= nonode@nohost ->
    nocookie;
get_cookie(Node) ->
    gen_server:call(auth, {get_cookie, Node}).

-spec set_cookie(Cookie :: cookie()) -> 'true'.

set_cookie(Cookie) ->
    set_cookie(node(), Cookie).

-spec set_cookie(Node :: node(), Cookie :: cookie()) -> 'true'.

set_cookie(_Node, _Cookie) when node() =:= nonode@nohost ->
    erlang:error(distribution_not_started);
set_cookie(Node, Cookie) ->
    gen_server:call(auth, {set_cookie, Node, Cookie}).

-spec sync_cookie() -> any().

sync_cookie() ->
    gen_server:call(auth, sync_cookie).

-spec print(Node :: node(), Format :: string(), Args :: [_]) -> 'ok'.

print(Node, Format, Args) ->
    (catch gen_server:cast({auth, Node}, {print, Format, Args})).

%%--gen_server callbacks------------------------------------------------

-spec init([]) -> {'ok', state()}.

init([]) ->
    process_flag(trap_exit, true),
    {ok, init_cookie()}.

%% Opened is a list of servers we have opened up
%% The net kernel will let all message to the auth server 
%% through as is

-type calls() :: 'echo' | 'sync_cookie'
               | {'get_cookie', node()}
               | {'set_cookie', node(), term()}.

-spec handle_call(calls(), {pid(), term()}, state()) ->
        {'reply', 'hello' | 'true' | 'nocookie' | cookie(), state()}.

handle_call({get_cookie, Node}, {_From,_Tag}, State) when Node =:= node() ->
    {reply, State#state.our_cookie, State};
handle_call({get_cookie, Node}, {_From,_Tag}, State) ->
    case ets:lookup(State#state.other_cookies, Node) of
	[{Node, Cookie}] ->
	    {reply, Cookie, State};
	[] ->
	    {reply, State#state.our_cookie, State}
    end;
handle_call({set_cookie, Node, Cookie}, {_From,_Tag}, State) 
  when Node =:= node() ->
    {reply, true, State#state{our_cookie = Cookie}};

%%
%% Happens when the distribution is brought up and 
%% someone might have set up the cookie for our new node name.
%%

handle_call({set_cookie, Node, Cookie}, {_From,_Tag}, State)  ->
    ets:insert(State#state.other_cookies, {Node, Cookie}),
    {reply, true, State};
    
handle_call(sync_cookie, _From, State) ->
    case ets:lookup(State#state.other_cookies, node()) of
	[{_N,C}] ->
	    ets:delete(State#state.other_cookies, node()),
	    {reply, true, State#state{our_cookie = C}};
	[] ->
	    {reply, true, State}
    end;

handle_call(echo, _From, O) -> 
    {reply, hello, O}.

%%
%% handle_cast/2
%%

-spec handle_cast({'print', string(), [term()]}, state()) ->
        {'noreply', state()}.

handle_cast({print,What,Args}, O) ->
  %% always allow print outs
  error_logger:error_msg(What, Args),
  {noreply, O}.

%% A series of bad messages that may come (from older distribution versions).

-spec handle_info(term(), state()) -> {'noreply', state()}.

handle_info({From,badcookie,net_kernel,{From,spawn,_M,_F,_A,_Gleader}}, O) ->
    auth:print(node(From) ,"~n** Unauthorized spawn attempt to ~w **~n",
	      [node()]),
    erlang:disconnect_node(node(From)), 
    {noreply, O};
handle_info({From,badcookie,net_kernel,{From,spawn_link,_M,_F,_A,_Gleader}}, O) ->
    auth:print(node(From),
	      "~n** Unauthorized spawn_link attempt to ~w **~n",
	      [node()]),
    erlang:disconnect_node(node(From)), 
    {noreply, O};
handle_info({_From,badcookie,ddd_server,_Mess}, O) ->
    %% Ignore bad messages to the ddd server, they will be resent
    %% If the authentication is successful
    {noreply, O};
handle_info({From,badcookie,rex,_Msg}, O) ->
    auth:print(getnode(From), 
	       "~n** Unauthorized rpc attempt to ~w **~n", [node()]),
    disconnect_node(node(From)), 
    {noreply, O};
%% These two messages have to do with the old auth:is_auth() call (net_adm:ping)
handle_info({From,badcookie,net_kernel,{'$gen_call',{From,Tag},{is_auth,_Node}}}, O) -> %% ho ho
    From ! {Tag, no},
    {noreply, O};
handle_info({_From,badcookie,To,{{auth_reply,N},R}}, O) ->%% Let auth replys through
    catch To ! {{auth_reply,N},R}, 
    {noreply, O};
handle_info({From,badcookie,Name,Mess}, Opened) ->
    %% This may be registered send as well as pid send.
    case lists:member(Name, Opened) of
	true ->
	    catch Name ! Mess;
	false ->
	    case catch lists:member(element(1, Mess), Opened) of
		true ->   
		    catch Name ! Mess;  %% Might be a pid as well
		_ ->
		    auth:print(getnode(From), 
			      "~n** Unauthorized send attempt ~w to ~w **~n",
			      [Mess,node()]),
		    erlang:disconnect_node(getnode(From))
	    end
    end, 
    {noreply, Opened};
handle_info(_, O) ->   % Ignore anything else especially EXIT signals
    {noreply, O}.

-spec code_change(term(), state(), term()) -> {'ok', state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, _State) ->
    ok.

getnode(P) when is_pid(P) -> node(P);
getnode(P) -> P.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% 	Cookie functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Read cookie from $HOME/.erlang.cookie and set it.
init_cookie() ->
    case init:get_argument(nocookie) of
	error ->
	    case init:get_argument(setcookie) of
		{ok, [[C0]]} ->
		    C = list_to_atom(C0),
		    #state{our_cookie = C,
			   other_cookies = ets:new(cookies,
						   [?COOKIE_ETS_PROTECTION])};
		_ ->
		    %% Here is the default 
		    case read_cookie() of
			{error, Error} ->
			    error_logger:error_msg(Error, []),
			    %% Is this really this serious?
			    erlang:error(Error);
			{ok, Co}  ->
			    #state{our_cookie = list_to_atom(Co),
				   other_cookies = ets:new(
						     cookies,
						     [?COOKIE_ETS_PROTECTION])}
		    end
	    end;
	_Other ->
	    #state{our_cookie = nocookie,
		   other_cookies = ets:new(cookies, [?COOKIE_ETS_PROTECTION])}
    end.

read_cookie() ->
    case init:get_argument(home) of
	{ok, [[Home]]} ->
	    read_cookie(filename:join(Home, ".erlang.cookie"));
	_ ->
	    {error, "No home for cookie file"}
    end.

read_cookie(Name) ->
    case file:raw_read_file_info(Name) of
	{ok, #file_info {type=Type, mode=Mode, size=Size}} ->
	    case check_attributes(Name, Type, Mode, os:type()) of
		ok -> read_cookie(Name, Size);
		Error -> Error
	    end;
	{error, enoent} ->
	    case create_cookie(Name) of
		ok -> read_cookie(Name);
		Error -> Error
	    end;
	{error, Reason} ->
	    {error, make_error(Name, Reason)}
    end.

read_cookie(Name, Size) ->
    case file:open(Name, [raw, read]) of
	{ok, File} ->
	    case file:read(File, Size) of
		{ok, List} ->
		    ok = file:close(File),
		    check_cookie(List, []);
		{error, Reason} ->
		    make_error(Name, Reason)
	    end;
	{error, Reason} ->
	    make_error(Name, Reason)
    end.
	
make_error(Name, Reason) ->
    {error, "Error when reading " ++ Name ++ ": " ++ atom_to_list(Reason)}.

%% Verifies that only the owner can access the cookie file.

check_attributes(Name, Type, _Mode, _Os) when Type =/= regular ->
    {error, "Cookie file " ++ Name ++ " is of type " ++ Type};
check_attributes(Name, _Type, Mode, {unix, _}) when (Mode band 8#077) =/= 0 ->
    {error, "Cookie file " ++ Name ++ " must be accessible by owner only"};
check_attributes(_Name, _Type, _Mode, _Os) ->
    ok.

%% Checks that the cookie has the correct format.

check_cookie([Letter|Rest], Result) when $\s =< Letter, Letter =< $~ ->
    check_cookie(Rest, [Letter|Result]);
check_cookie([X|Rest], Result) ->
    check_cookie1([X|Rest], Result);
check_cookie([], Result) ->
    check_cookie1([], Result).

check_cookie1([$\n|Rest], Result) ->
    check_cookie1(Rest, Result);
check_cookie1([$\r|Rest], Result) ->
    check_cookie1(Rest, Result);
check_cookie1([$\s|Rest], Result) ->
    check_cookie1(Rest, Result);
check_cookie1([_|_], _Result) ->
    {error, "Bad characters in cookie"};
check_cookie1([], []) ->
    {error, "Too short cookie string"};
check_cookie1([], Result) ->
    {ok, lists:reverse(Result)}.

%% Creates a new, random cookie. 
   
create_cookie(Name) ->
    Seed = abs(erlang:monotonic_time()
	       bxor erlang:unique_integer()),
    Cookie = random_cookie(20, Seed, []),
    case file:open(Name, [write, raw]) of
	{ok, File} ->
	    R1 = file:write(File, Cookie),
	    ok = file:close(File),
	    R2 = file:raw_write_file_info(Name, make_info(Name)),
	    case {R1, R2} of
		{ok, ok} ->
		    ok;
		{{error,Reason}, _} ->
		    {error,
		     lists:flatten(
		       io_lib:format("Failed to write to cookie file '~ts': ~p", [Name, Reason]))};
		{ok, {error, Reason}} ->
		    {error, "Failed to change mode: " ++ atom_to_list(Reason)}
	    end;
	{error,Reason} ->
	    {error,
	     lists:flatten(
	       io_lib:format("Failed to create cookie file '~ts': ~p", [Name, Reason]))}
    end.

random_cookie(0, _, Result) ->
    Result;
random_cookie(Count, X0, Result) ->
    X = next_random(X0),
    Letter = X*($Z-$A+1) div 16#1000000000 + $A,
    random_cookie(Count-1, X, [Letter|Result]).

%% Returns suitable information for a new cookie.
%%
%% Note: Since the generated cookie depends on the time the file was
%% created, and the time can be seen plainly in the file, we will
%% round down the file creation times to the nearest midnight to
%% give crackers some more work.

make_info(Name) ->
    Midnight =
	case file:raw_read_file_info(Name) of
	    {ok, #file_info{atime={Date, _}}} ->
		{Date, {0, 0, 0}};
	    _ ->
		{{1990, 1, 1}, {0, 0, 0}}
	    end,
    #file_info{mode=8#400, atime=Midnight, mtime=Midnight, ctime=Midnight}.

%% This RNG is from line 21 on page 102 in Knuth: The Art of Computer Programming,
%% Volume II, Seminumerical Algorithms.
%%
%% Returns an integer in the range 0..(2^35-1).

next_random(X) ->
    (X*17059465+1) band 16#fffffffff.
