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
-module(bonk_sound).
-export([start/0]).

start() ->
    random:seed(),
    sounder:start(),
    {ok,Bonk}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/bonk.au")),
    {ok,Ouch}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/ouch!!!.au")),
    {ok,Damn}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/damn.au")),
    {ok,Bomb}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/explosion.au")),
    {ok,Missed}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/missedme.au")),
    {ok,Game_over}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/gameover.au")),
    {ok,New_level}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/level.au")),
    {ok,Music}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/trumpet.au")),
    {ok,Start}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/hehee.au")),
    {ok,BestS}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/praisejesus.au")),
    {ok,HighS}=sounder:new(lists:append(bonk:bonk_dir(),"sounds/yes.au")),
    loop(Bonk, Ouch, Damn, Bomb, Missed, Game_over, New_level,
	 Music, Start, BestS, HighS).


loop(Bonk, Ouch, Damn, Bomb, Missed, Game_over, New_level, Music, Start, BestS, HighS) ->
    R=random:uniform(1000),
    receive
	bonk ->
	    if
		R < 75 -> play_sound(Damn);
		R < 275 -> play_sound(Ouch);
		true    -> play_sound(Bonk)
	    end;
	bomb       -> play_sound(Bomb);
	missed     -> play_sound(Missed);
	game_over  -> play_sound(Game_over);
	new_level  -> play_sound(New_level);
	music      -> play_sound(Music);
	start      -> play_sound(Start);
	best_score -> play_sound(BestS);
	high_score -> play_sound(HighS);
	quit ->
	    sounder:stop(),
	    exit(normal)
    end,
    loop(Bonk, Ouch, Damn, Bomb, Missed, Game_over, New_level, Music, Start, BestS, HighS).

play_sound(Snd) ->
    case catch sounder:play(Snd) of
	{'EXIT', _Reason} ->
	    io:format("Cannot use audio device!\n"),
	    sounder:stop(),
	    silent_loop();
	_Other ->
	    true
    end.

silent_loop() ->
    receive
	quit ->
	    exit(normal);
	_Other ->
	    silent_loop()
    end.
