%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(os).

%% Provides a common operating system interface.

-export([type/0, version/0, cmd/1, find_executable/1, find_executable/2]).

-include("file.hrl").

-spec type() -> 'vxworks' | {'unix',atom()} | {'win32',atom()} | {'ose',atom()}.
type() ->
    case erlang:system_info(os_type) of
	{vxworks, _} ->
	    vxworks;
	Else -> Else
    end.

-spec version() -> string() | {non_neg_integer(),non_neg_integer(),non_neg_integer()}.
version() ->
    erlang:system_info(os_version).

-spec find_executable(string()) -> string() | 'false'.
find_executable(Name) ->
    case os:getenv("PATH") of
	false -> find_executable(Name, []);
	Path  -> find_executable(Name, Path)
    end.

-spec find_executable(string(), string()) -> string() | 'false'.
find_executable(Name, Path) ->
    Extensions = extensions(),
    case filename:pathtype(Name) of
	relative ->
	    find_executable1(Name, split_path(Path), Extensions);
	_ ->
	    case verify_executable(Name, Extensions) of
		{ok, Complete} ->
		    Complete;
		error ->
		    false
	    end
    end.

find_executable1(Name, [Base|Rest], Extensions) ->
    Complete0 = filename:join(Base, Name),
    case verify_executable(Complete0, Extensions) of
	{ok, Complete} ->
	    Complete;
	error ->
	    find_executable1(Name, Rest, Extensions)
    end;
find_executable1(_Name, [], _Extensions) ->
    false.

verify_executable(Name0, [Ext|Rest]) ->
    Name1 = Name0 ++ Ext,
    case os:type() of
	vxworks ->
	    %% We consider all existing VxWorks files to be executable
	    case file:read_file_info(Name1) of
		{ok, _} ->
		    {ok, Name1};
		_ ->
		    verify_executable(Name0, Rest)
	    end;
	_ ->
	    case file:read_file_info(Name1) of
		{ok, #file_info{mode=Mode}} when Mode band 8#111 =/= 0 ->
		    %% XXX This test for execution permission is not full-proof
		    %% on Unix, since we test if any execution bit is set.
		    {ok, Name1};
		_ ->
		    verify_executable(Name0, Rest)
	    end
    end;
verify_executable(_, []) ->
    error.

split_path(Path) ->
    case type() of
	{win32, _} ->
	    {ok,Curr} = file:get_cwd(),
	    split_path(Path, $;, [], [Curr]);
	_ -> 
	    split_path(Path, $:, [], [])
    end.

split_path([Sep|Rest], Sep, Current, Path) ->
    split_path(Rest, Sep, [], [reverse_element(Current)|Path]);
split_path([C|Rest], Sep, Current, Path) ->
    split_path(Rest, Sep, [C|Current], Path);
split_path([], _, Current, Path) ->
    lists:reverse(Path, [reverse_element(Current)]).

reverse_element([]) -> ".";
reverse_element([$"|T]) ->	%"
    case lists:reverse(T) of
	[$"|List] -> List;	%"
	List -> List ++ [$"]	%"
    end;
reverse_element(List) ->
    lists:reverse(List).

-spec extensions() -> [string()].
extensions() ->
    case type() of
	{win32, _} -> [".exe",".com",".cmd",".bat"];
	{unix, _} -> [""];
	vxworks -> [""]
    end.

%% Executes the given command in the default shell for the operating system.
-spec cmd(atom() | string() | [string()]) -> string().
cmd(Cmd) ->
    validate(Cmd),
    case type() of
	{unix, _} ->
	    unix_cmd(Cmd);
	{win32, Wtype} ->
	    Command = case {os:getenv("COMSPEC"),Wtype} of
			  {false,windows} -> lists:concat(["command.com /c", Cmd]);
			  {false,_} -> lists:concat(["cmd /c", Cmd]);
			  {Cspec,_} -> lists:concat([Cspec," /c",Cmd])
		      end,
	    Port = open_port({spawn, Command}, [stream, in, eof, hide]),
	    get_data(Port, []);
	%% VxWorks uses a 'sh -c hook' in 'vxcall.c' to run os:cmd.
	vxworks ->
	    Command = lists:concat(["sh -c '", Cmd, "'"]),
	    Port = open_port({spawn, Command}, [stream, in, eof]),
	    get_data(Port, [])
    end.

unix_cmd(Cmd) ->
    Tag = make_ref(),
    {Pid,Mref} = erlang:spawn_monitor(
		   fun() ->
			   process_flag(trap_exit, true),
			   Port = start_port(),
			   erlang:port_command(Port, mk_cmd(Cmd)),
			   exit({Tag,unix_get_data(Port)})
		   end),
    receive
	{'DOWN',Mref,_,Pid,{Tag,Result}} ->
	    Result;
	{'DOWN',Mref,_,Pid,Reason} ->
	    exit(Reason)
    end.

%% The -s flag implies that only the positional parameters are set,
%% and the commands are read from standard input. We set the 
%% $1 parameter for easy identification of the resident shell.
%%
-define(SHELL, "/bin/sh -s unix:cmd 2>&1").

%%
%% Serializing open_port through a process to avoid smp lock contention
%% when many concurrent os:cmd() want to do vfork (OTP-7890).
%%
-spec start_port() -> port().
start_port() ->
    {Ref,Client} = {make_ref(),self()},
    try (os_cmd_port_creator ! {Ref,Client})
    catch
	error:_ -> spawn(fun() -> start_port_srv({Ref,Client}) end)
    end,
    receive
	{Ref,Port} when is_port(Port) -> Port;
	{Ref,Error} -> exit(Error)
    end.

start_port_srv(Request) ->
    StayAlive = try register(os_cmd_port_creator, self())
		catch
		    error:_ -> false
		end,
    start_port_srv_loop(Request, StayAlive).

start_port_srv_loop({Ref,Client}, StayAlive) ->
    Reply = try open_port({spawn, ?SHELL},[stream]) of
		Port when is_port(Port) ->
		    port_connect(Port, Client),
		    unlink(Port),
		    Port
	    catch
		error:Reason ->
		    {Reason,erlang:get_stacktrace()}	    
	    end,
    Client ! {Ref,Reply},
    case StayAlive of
	true -> start_port_srv_loop(receive Msg -> Msg end, true);
	false -> exiting
    end.

%%
%%  unix_get_data(Port) -> Result
%%
unix_get_data(Port) ->
    unix_get_data(Port, []).

unix_get_data(Port, Sofar) ->
    receive
	{Port,{data, Bytes}} ->
	    case eot(Bytes) of
		{done, Last} ->
		    lists:flatten([Sofar|Last]);
		more  ->
		    unix_get_data(Port, [Sofar|Bytes])
	    end;
	{'EXIT', Port, _} ->
	    lists:flatten(Sofar)
    end.

%%
%% eot(String) -> more | {done, Result}
%%
eot(Bs) ->
    eot(Bs, []).

eot([4| _Bs], As) ->
    {done, lists:reverse(As)};
eot([B| Bs], As) ->
    eot(Bs, [B| As]);
eot([], _As) ->
    more.

%%
%% mk_cmd(Cmd) -> {ok, ShellCommandString} | {error, ErrorString}
%%
%% We do not allow any input to Cmd (hence commands that want
%% to read from standard input will return immediately).
%% Standard error is redirected to standard output.
%%
%% We use ^D (= EOT = 4) to mark the end of the stream.
%%
mk_cmd(Cmd) when is_atom(Cmd) ->		% backward comp.
    mk_cmd(atom_to_list(Cmd));
mk_cmd(Cmd) ->
    %% We insert a new line after the command, in case the command
    %% contains a comment character.
    io_lib:format("(~s\n) </dev/null; echo  \"\^D\"\n", [Cmd]).


validate(Atom) when is_atom(Atom) ->
    ok;
validate(List) when is_list(List) ->
    validate1(List).

validate1([C|Rest]) when is_integer(C), 0 =< C, C < 256 ->
    validate1(Rest);
validate1([List|Rest]) when is_list(List) ->
    validate1(List),
    validate1(Rest);
validate1([]) ->
    ok.

get_data(Port, Sofar) ->
    receive
	{Port, {data, Bytes}} ->
	    get_data(Port, [Sofar|Bytes]);
	{Port, eof} ->
	    Port ! {self(), close}, 
	    receive
		{Port, closed} ->
		    true
	    end, 
	    receive
		{'EXIT',  Port,  _} -> 
		    ok
	    after 1 ->				% force context switch
		    ok
	    end, 
	    lists:flatten(Sofar)
    end.
