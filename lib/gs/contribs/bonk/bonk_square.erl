%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(bonk_square).
-export([start/1,init/5,alarm/3]).



start(Bmp) ->
    spawn_link(bonk_square, init, [Bmp, self(),
				   random:uniform(10000),
				   random:uniform(10000),
				   random:uniform(10000)]).

init(Bmp, BoardPid, Seed1, Seed2, Seed3) ->
    random:seed(Seed1,Seed2,Seed3),
    idle(Bmp, BoardPid).


idle(Bmp, BoardPid) ->
    receive
	start ->
	    Level = 1,
	    sleep(Level, Bmp, BoardPid, alarm(sleep_time(Level), wake_up));
	quit ->
	    exit(normal);
	_Other ->
	    idle(Bmp, BoardPid)
    end.


sleep(Level, Bmp, BoardPid, Alarm) ->
    receive
	stop -> 
	    Alarm ! quit,
	    idle(Bmp, BoardPid);
	quit -> 
	    Alarm ! quit,
	    exit(normal);
	{new_level, NewLevel} ->
	    sleep(NewLevel, Bmp, BoardPid, Alarm);
	{Alarm, wake_up} ->
	    show_me(BoardPid, Bmp),
	    show(Level, Bmp, BoardPid, alarm(2500, missed));
	_Other ->
	    sleep(Level, Bmp, BoardPid, Alarm)
    end.


show(Level, Bmp, BoardPid, Alarm) ->
    receive
	stop ->
	    Alarm ! quit,
	    idle(Bmp, BoardPid);		
	quit -> 
	    Alarm ! quit,
	    exit(normal);			
	{new_level, NewLevel} ->
	    show(NewLevel, Bmp, BoardPid, Alarm);
	sleep ->				% The board was too crowded.
	    Alarm ! quit,
	    sleep(Level, Bmp, BoardPid, alarm(sleep_time(Level), wake_up));	
	bonk ->
	    bonk_me(BoardPid, Bmp),
	    Alarm ! quit,
	    bbmed(Level, Bmp, BoardPid, alarm(1500, hide));
	bomb ->
	    bomb_me(BoardPid, Bmp),
	    Alarm ! quit,
	    bbmed(Level, Bmp, BoardPid, alarm(1000, hide));
	{Alarm, missed} ->
	    missed_me(BoardPid, Bmp),
	    bbmed(Level, Bmp, BoardPid, alarm(1500, hide));
	_Other ->
	    show(Level, Bmp, BoardPid, Alarm)
    end.

%% bonked, bombed or missed
bbmed(Level, Bmp, BoardPid, Alarm) ->
    receive
	stop -> 
	    Alarm ! quit,
	    idle(Bmp, BoardPid);
	quit -> 
	    Alarm ! quit,
	    exit(normal);
	{new_level, NewLevel} ->
	    bbmed(NewLevel, Bmp, BoardPid, Alarm);
	{Alarm, hide} ->
	    hide_me(BoardPid, Bmp),
	    sleep(Level, Bmp, BoardPid, alarm(sleep_time(Level), wake_up));
	_Other ->
	    bbmed(Level, Bmp, BoardPid, Alarm)
    end.


show_me(BoardPid, Bmp) ->
    BoardPid ! {show, self(), Bmp}.

hide_me(BoardPid, Bmp) ->
    BoardPid ! {hide, self(), Bmp}.

bonk_me(BoardPid, Bmp) ->
    BoardPid ! {bonked, self(), Bmp}.

bomb_me(BoardPid, Bmp) ->
    BoardPid ! {bombed, self(), Bmp}.

missed_me(BoardPid, Bmp) ->
    BoardPid ! {missed, self(), Bmp}.


%% Count sleep time

sleep_time(Level) ->
    random:uniform((19000 div (Level+1))*2+1500).

%% Set an alarm

alarm(Time, Msg) ->
    spawn(bonk_square, alarm, [Time, Msg, self()]).

alarm(Time, Msg, Pid) ->
    receive
	quit  -> exit(normal)
    after
	Time -> Pid ! {self(), Msg}
    end.
