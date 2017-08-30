%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(os).

%% Provides a common operating system interface.

-export([type/0, version/0, cmd/1, find_executable/1, find_executable/2]).

-include("file.hrl").

%%% BIFs

-export([getenv/0, getenv/1, getenv/2, getpid/0,
         perf_counter/0, perf_counter/1,
         putenv/2, set_signal/2, system_time/0, system_time/1,
	 timestamp/0, unsetenv/1]).

-spec getenv() -> [string()].

getenv() -> erlang:nif_error(undef).

-spec getenv(VarName) -> Value | false when
      VarName :: string(),
      Value :: string().

getenv(_) ->
    erlang:nif_error(undef).

-spec getenv(VarName, DefaultValue) -> Value when
      VarName :: string(),
      DefaultValue :: string(),
      Value :: string().

getenv(VarName, DefaultValue) ->
    case os:getenv(VarName) of
        false ->
           DefaultValue;
        Value ->
            Value
    end.

-spec getpid() -> Value when
      Value :: string().

getpid() ->
    erlang:nif_error(undef).

-spec perf_counter() -> Counter when
      Counter :: integer().

perf_counter() ->
    erlang:nif_error(undef).

-spec perf_counter(Unit) -> integer() when
      Unit :: erlang:time_unit().

perf_counter(Unit) ->
      erlang:convert_time_unit(os:perf_counter(), perf_counter, Unit).

-spec putenv(VarName, Value) -> true when
      VarName :: string(),
      Value :: string().

putenv(_, _) ->
    erlang:nif_error(undef).

-spec system_time() -> integer().

system_time() ->
    erlang:nif_error(undef).

-spec system_time(Unit) -> integer() when
      Unit :: erlang:time_unit().

system_time(_Unit) ->
    erlang:nif_error(undef).

-spec timestamp() -> Timestamp when
      Timestamp :: erlang:timestamp().

timestamp() ->
    erlang:nif_error(undef).

-spec unsetenv(VarName) -> true when
      VarName :: string().

unsetenv(_) ->
    erlang:nif_error(undef).

-spec set_signal(Signal, Option) -> 'ok' when
      Signal :: 'sighup'  | 'sigquit' | 'sigabrt' | 'sigalrm' |
                'sigterm' | 'sigusr1' | 'sigusr2' | 'sigchld' |
                'sigstop' | 'sigtstp',
      Option :: 'default' | 'handle' | 'ignore'.

set_signal(_Signal, _Option) ->
    erlang:nif_error(undef).

%%% End of BIFs

-spec type() -> {Osfamily, Osname} when
      Osfamily :: unix | win32,
      Osname :: atom().

type() ->
    erlang:system_info(os_type).

-spec version() -> VersionString | {Major, Minor, Release} when
      VersionString :: string(),
      Major :: non_neg_integer(),
      Minor :: non_neg_integer(),
      Release :: non_neg_integer().
version() ->
    erlang:system_info(os_version).

-spec find_executable(Name) -> Filename | 'false' when
      Name :: string(),
      Filename :: string().
find_executable(Name) ->
    find_executable(Name, os:getenv("PATH", "")).

-spec find_executable(Name, Path) -> Filename | 'false' when
      Name :: string(),
      Path :: string(),
      Filename :: string().
find_executable(Name, Path) ->
    Extensions = extensions(),
    case filename:pathtype(Name) of
	relative ->
	    find_executable1(Name, split_path(Path), Extensions);
	_ ->
	    case verify_executable(Name, Extensions, Extensions) of
		{ok, Complete} ->
		    Complete;
		error ->
		    false
	    end
    end.

find_executable1(Name, [Base|Rest], Extensions) ->
    Complete0 = filename:join(Base, Name),
    case verify_executable(Complete0, Extensions, Extensions) of
	{ok, Complete} ->
	    Complete;
	error ->
	    find_executable1(Name, Rest, Extensions)
    end;
find_executable1(_Name, [], _Extensions) ->
    false.

verify_executable(Name0, [Ext|Rest], OrigExtensions) ->
    Name1 = Name0 ++ Ext,
    case file:read_file_info(Name1) of
	{ok, #file_info{type=regular,mode=Mode}}
	when Mode band 8#111 =/= 0 ->
	    %% XXX This test for execution permission is not fool-proof
	    %% on Unix, since we test if any execution bit is set.
	    {ok, Name1};
	_ ->
	    verify_executable(Name0, Rest, OrigExtensions)
    end;
verify_executable(Name, [], OrigExtensions) when OrigExtensions =/= [""] -> %% Windows
    %% Will only happen on windows, hence case insensitivity
    case can_be_full_name(string:to_lower(Name),OrigExtensions) of
	true ->
	    verify_executable(Name,[""],[""]);
	_ ->
	    error
    end;
verify_executable(_, [], _) ->
    error.

can_be_full_name(_Name,[]) ->
    false;
can_be_full_name(Name,[H|T]) ->
    case lists:suffix(H,Name) of %% Name is in lowercase, cause this is a windows thing
	true ->
	    true;
	_ ->
	    can_be_full_name(Name,T)
    end.

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

-spec extensions() -> [string(),...].
%% Extensions in lower case
extensions() ->
    case type() of
	{win32, _} -> [".exe",".com",".cmd",".bat"];
	{unix, _} -> [""]
    end.

%% Executes the given command in the default shell for the operating system.
-spec cmd(Command) -> string() when
      Command :: atom() | io_lib:chars().
cmd(Cmd) ->
    validate(Cmd),
    {SpawnCmd, SpawnOpts, SpawnInput, Eot} = mk_cmd(os:type(), Cmd),
    Port = open_port({spawn, SpawnCmd}, [binary, stderr_to_stdout,
                                         stream, in, hide | SpawnOpts]),
    MonRef = erlang:monitor(port, Port),
    true = port_command(Port, SpawnInput),
    Bytes = get_data(Port, MonRef, Eot, []),
    demonitor(MonRef, [flush]),
    String = unicode:characters_to_list(Bytes),
    if  %% Convert to unicode list if possible otherwise return bytes
	is_list(String) -> String;
	true -> binary_to_list(Bytes)
    end.

mk_cmd({win32,Wtype}, Cmd) ->
    Command = case {os:getenv("COMSPEC"),Wtype} of
                  {false,windows} -> lists:concat(["command.com /c", Cmd]);
                  {false,_} -> lists:concat(["cmd /c", Cmd]);
                  {Cspec,_} -> lists:concat([Cspec," /c",Cmd])
              end,
    {Command, [], [], <<>>};
mk_cmd(OsType,Cmd) when is_atom(Cmd) ->
    mk_cmd(OsType, atom_to_list(Cmd));
mk_cmd(_,Cmd) ->
    %% Have to send command in like this in order to make sh commands like
    %% cd and ulimit available
    {"/bin/sh -s unix:cmd", [out],
     %% We insert a new line after the command, in case the command
     %% contains a comment character.
     %%
     %% The </dev/null closes stdin, which means that programs
     %% that use a closed stdin as an termination indicator works.
     %% An example of such a program is 'more'.
     %%
     %% The "echo ^D" is used to indicate that the program has executed
     %% and we should return any output we have gotten. We cannot use
     %% termination of the child or closing of stdin/stdout as then
     %% starting background jobs from os:cmd will block os:cmd.
     %%
     %% I tried changing this to be "better", but got bombarded with
     %% backwards incompatibility bug reports, so leave this as it is.
     ["(", unicode:characters_to_binary(Cmd), "\n) </dev/null; echo \"\^D\"\n"],
     <<$\^D>>}.

validate(Atom) when is_atom(Atom) ->
    ok;
validate(List) when is_list(List) ->
    validate1(List).

validate1([C|Rest]) when is_integer(C) ->
    validate1(Rest);
validate1([List|Rest]) when is_list(List) ->
    validate1(List),
    validate1(Rest);
validate1([]) ->
    ok.

get_data(Port, MonRef, Eot, Sofar) ->
    receive
	{Port, {data, Bytes}} ->
            case eot(Bytes, Eot) of
                more ->
                    get_data(Port, MonRef, Eot, [Sofar,Bytes]);
                Last ->
                    catch port_close(Port),
                    flush_until_down(Port, MonRef),
                    iolist_to_binary([Sofar, Last])
            end;
        {'DOWN', MonRef, _, _, _} ->
	    flush_exit(Port),
	    iolist_to_binary(Sofar)
    end.

eot(_Bs, <<>>) ->
    more;
eot(Bs, Eot) ->
    case binary:match(Bs, Eot) of
        nomatch -> more;
        {Pos, _} ->
            binary:part(Bs,{0, Pos})
    end.

%% When port_close returns we know that all the
%% messages sent have been sent and that the
%% DOWN message is after them all.
flush_until_down(Port, MonRef) ->
    receive
        {Port, {data, _Bytes}} ->
            flush_until_down(Port, MonRef);
        {'DOWN', MonRef, _, _, _} ->
            flush_exit(Port)
    end.

%% The exit signal is always delivered before
%% the down signal, so we can be sure that if there
%% was an exit message sent, it will be in the
%% mailbox now.
flush_exit(Port) ->
    receive
        {'EXIT',  Port,  _} ->
            ok
    after 0 ->
            ok
    end.
