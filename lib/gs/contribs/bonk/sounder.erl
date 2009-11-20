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
-module(sounder).
-export([start/0,play/1,new/1,go/0,stop/0,nosound/0,silent/0]).
-include_lib("kernel/include/file.hrl").
%%----------------------------------------------------------------------
%% sounder.erl  -  An interface to /dev/audio 
%%
%% Created  by:  {lennarto,mike}@erix.ericsson.se
%% Modified by:  EV,tobbe@erix.ericsson.se
%%
%% Mod: 6 Jun 1996 by tobbe@erix.ericsson.se
%%   The executable sounder will no be looked for in the
%%   same directory as from where sounder.jam is loaded.
%%
%%
%% start()      -  Returns either: ok ,or: silent ,where silent means
%%                 that no audio capabilities exists but the sounder
%%                 will work "silently" in order to not break any code.
%%
%% stop()       - Returns: ok
%%
%% new(File)    - Tries to load the File. At success, a number refering
%%                to the File is returned that shall be used with send/1.
%%                Otherwise {error,Reason} is returned.
%%
%% play(No)     - Tries to execute the sound registered with the number No
%%                Returns: ok , or: {error,Reason}
%%
%% silent()     - Returns: true ,if no audio capabilities exists, else: false
%%
%% Note: It is also possible to receive: {error,sounder_not_started}
%%
%%----------------------------------------------------------------------

start() ->
    case whereis(sounder) of
	undefined ->
	    %% first we check if the workstation has audio capabilities
	    case file:read_file_info('/dev/audio') of
		{ok, FI} when FI#file_info.access==read_write ->
		    register(sounder, spawn(sounder,go,[])),
		    ok;
		_Other ->
		    register(sounder, spawn(sounder,nosound,[])),
		    silent
	    end;
	_Pid ->
	    ok
    end.

stop() -> 
    catch begin check(),
    sounder ! {stop},
    ok end.

new(File) when list(File) -> new(list_to_atom(File));
new(File) when atom(File) ->
    catch begin check(),
    sounder ! {new,File,self()},
    wait_for_ack(sounder) end.

play(No) when integer(No) ->
    catch begin check(),
    sounder ! {play, No, self()},
    wait_for_ack(sounder) end.

silent() ->
    catch begin check(),
    sounder ! {play,silent,self()},
    receive {sounder,Answer} -> Answer end end.

go()  ->
    Port = open_port({spawn,lists:append(bonk:bonk_dir(), "sounder")},[{packet, 2}]),
    loop(Port).

loop(Port) ->
    receive
	{new, File, From} when atom(File) ->
	    Port ! {self(),{command,lists:append([0],atom_to_list(File))}},
	    From ! {sounder,wait_for_ack(Port)},
	    loop(Port);
	{play,silent,From} ->
	    From ! {sounder,false},
	    loop(Port);
	{play,No,From} when integer(No) ->
	    Port ! {self(),{command,[No]}},
	    From ! {sounder,wait_for_ack(Port)},
	    loop(Port);
	{stop} ->
	    Port ! {self(),close},
	    exit(normal);
	_ ->
	    loop(Port)
    end.

%% The application using sounds can check on silence itself
%% and refrain from playing sounds.
%% Or it can try to play sounds that will be "consumed in silence"

nosound() ->
    receive
	{new,File,From} when atom(File) ->
	    From ! {sounder,{ok,silent}},
	    nosound();
	{play,silent,From} ->
	    From ! {sounder,true},
	    nosound();
	{play,No,From} when integer(No) ->
	    From ! {sounder,{error,no_audio_cap}},
	    nosound();
	{stop} ->
	    exit(normal);
	_ ->
	    nosound()
    end.

wait_for_ack(sounder) ->
    receive {sounder,Res} -> Res end;
wait_for_ack(Port) when port(Port) ->
    receive
	{Port,{data,"ok"}} ->
	    ok;
	{Port,{data,[No]}} ->
	    {ok,No};
	{Port,{data,Msg}} ->
	    {error,list_to_atom(Msg)};
	{'EXIT',Port,_} ->
	    exit(port_exited)
    end.

check() ->
    case whereis(sounder) of
	Pid when pid(Pid) -> 
	    ok;
	undefined ->
	    throw({error,sounder_not_started})
    end.

    
	    
