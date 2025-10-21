%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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
-module(logger_simple_h).
-moduledoc false.

-export([adding_handler/1, removing_handler/1, log/2]).

-behaviour(logger_handler).

%% This module implements a simple handler for logger. It is the
%% default used during system start.

%%%-----------------------------------------------------------------
%%% Logger callback

adding_handler(#{id:=simple}=Config) ->
    Me = self(),
    case whereis(?MODULE) of
        undefined ->
            {Pid,Ref} = spawn_opt(fun() -> init(Me) end,
                                  [link,monitor,{message_queue_data,off_heap}]),
            receive
                {'DOWN',Ref,process,Pid,Reason} ->
                    {error,Reason};
                {Pid,started} ->
                    erlang:demonitor(Ref),
                    {ok,Config}
            end;
        _ ->
            {error,{handler_process_name_already_exists,?MODULE}}
    end.

removing_handler(#{id:=simple}) ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Pid ->
            Ref = erlang:monitor(process,Pid),
            Pid ! stop,
            receive {'DOWN',Ref,process,Pid,_} ->
                    ok
            end
    end.

log(#{meta:=#{error_logger:=#{tag:=info_report,type:=Type}}} = Log,_Meta)
  when Type =/= std_info ->
    case logger:allow(debug, ?MODULE) of
        false ->
            %% Skip info reports that are not 'std_info' (ref simple logger in
            %% error_logger)
            ok;
        true ->
            %% If log level is debug or all we emit even these reports
            do_log(Log)
    end;
log(Log,_Config) ->
    _ = do_log(Log),
    ok.

do_log(#{msg:=_,meta:=#{time:=_}=M}=Log) ->
    case whereis(?MODULE) of
        undefined ->
            %% Is the node on the way down? Real emergency?
            %% Log directly from client just to get it out
            case maps:get(internal_log_event, M, false) of
                false ->
                    log_internal(
                      simple,
                      #{level=>error,
                        msg=>{report,{error,simple_handler_process_dead}},
                        meta=>#{time=>logger:timestamp()}});
                true ->
                    ok
            end,
            log_internal(simple,Log);
        _ ->
            ?MODULE ! {log,Log}
    end;
do_log(_) ->
    %% Unexpected log.
    %% We don't want to crash the simple logger, so ignore this.
    ok.

%%%-----------------------------------------------------------------
%%% Process
init(Starter) ->
    register(?MODULE,self()),
    Starter ! {self(),started},
    loop(rich, #{buffer_size=>20,dropped=>0,buffer=>[]}).

loop(Mode, Buffer) ->
    receive
        stop ->
            %% We replay the logger messages if there is
            %% a default handler when the simple handler
            %% is removed.
            case logger:get_handler_config(default) of
                {ok, _} ->
                    replay_buffer(Buffer);
                _ ->
                    ok
            end,
            %% Before stopping, we unlink the logger process to avoid
            %% an unexpected EXIT message
            unlink(whereis(logger)),
            ok;
        {log,#{meta:=#{error_logger:=#{tag:=info_report,type:=Type}}} = Log}
          when Type =/= std_info ->
            %% When we get a std_info message, we just want to replay it,
            %% no need to print it right now
            loop(Mode, update_buffer(Buffer,Log));
        {log,#{msg:=_,meta:=#{time:=_}}=Log} ->
            NewMode = log_internal(Mode, Log),
            loop(NewMode, update_buffer(Buffer,Log));
        _ ->
            %% Unexpected message - flush it!
            loop(Mode, Buffer)
    end.

update_buffer(#{buffer_size:=0,dropped:=D}=Buffer,_Log) ->
    Buffer#{dropped=>D+1};
update_buffer(#{buffer_size:=S,buffer:=B}=Buffer,Log) ->
    Buffer#{buffer_size=>S-1,buffer=>[Log|B]}.

replay_buffer(#{ dropped := D, buffer := Buffer }) ->
    lists:foreach(
      fun F(#{msg := {Tag, Msg}} = L) when Tag =:= string; Tag =:= report ->
              F(L#{ msg := Msg });
          F(#{ level := Level, msg := Msg, meta := MD}) ->
              logger:log(Level, Msg, MD)
      end, lists:reverse(Buffer, drop_msg(D))).

drop_msg(0) ->
    [];
drop_msg(N) ->
    [#{level=>info,
       msg=>{"Simple handler buffer full, dropped ~w messages",[N]},
       meta=>#{time=>logger:timestamp()}}].

%%%-----------------------------------------------------------------
%%% Internal

%% If the init process is busy (for instance doing a shutdown)
%% we can get blocked while trying to load code. So we spawn a process
%% for each log message that can potentially block. If the logging cannot
%% be done within 300ms, we instead log the raw log message to stdout
%% and switch mode to always log using the raw format.
log_internal(simple, Log) ->
    display_log(Log), simple;
log_internal(rich = Mode, Log) ->

    {Pid, Ref} =
        spawn_monitor(
          fun() ->
                  Str = logger_formatter:format(
                          Log,
                          #{ legacy_header => true, single_line => false,
                             depth => unlimited, time_offset => ""
                           }),
                  erlang:display_string(stdout, lists:flatten(unicode:characters_to_list(Str)))
          end),
    receive
        {'DOWN', Ref, _, _, normal} ->
            Mode;
        {'DOWN', Ref, _, _, _Else} ->
            display_log(Log),
            Mode
    after 300 ->
            %% init:terminate/3 sleeps for 500 ms before exiting,
            %% so we wait for 300 ms for the log to happen
            exit(Pid, kill),
            receive
                {'DOWN', Ref, _, _, normal} ->
                    Mode;
                {'DOWN', Ref, _, _, _Else} ->
                    display_log(Log),
                    simple
            end
    end.

display_log(#{msg:={report,Report},
         meta:=#{time:=T,error_logger:=#{type:=Type}}}) ->
    display_date(T),
    display_report(Type,Report);
display_log(#{msg:=Msg,meta:=#{time:=T}}) ->
    display_date(T),
    display(Msg).

display_date(Timestamp) when is_integer(Timestamp) ->
    Micro = Timestamp rem 1000000,
    Sec = Timestamp div 1000000,
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime_to_localtime(
                            erlang:posixtime_to_universaltime(Sec)),
    erlang:display_string(
      stdout,
      integer_to_list(Y) ++ "-" ++
	  pad(Mo,2) ++ "-" ++
	  pad(D,2)  ++ " " ++
	  pad(H,2)  ++ ":" ++
	  pad(Mi,2) ++ ":" ++
	  pad(S,2)  ++ "." ++
          pad(Micro,6) ++ " ").

pad(Int,Size) when is_integer(Int) ->
    pad(integer_to_list(Int),Size);
pad(Str,Size) when length(Str)==Size ->
    Str;
pad(Str,Size) ->
    pad([$0|Str],Size).

display({string,Chardata}) ->
    try unicode:characters_to_list(Chardata) of
        String -> erlang:display_string(stdout, String),
                  erlang:display_string(stdout, "\n")
    catch _:_ -> erlang:display(Chardata)
    end;
display({report,Report}) when is_map(Report) ->
    display_report(maps:to_list(Report));
display({report,Report}) ->
    display_report(Report);
display({F, A}) when is_list(F), is_list(A) ->
    erlang:display_string(stdout, F ++ "\n"),
    [begin
	 erlang:display_string(stdout, "\t"),
	 erlang:display(Arg)
     end || Arg <- A],
    ok.

display_report(Atom, A) when is_atom(Atom) ->
    %% The widest atom seems to be 'supervisor_report' at 17.
    ColumnWidth = 20,
    AtomString = atom_to_list(Atom),
    AtomLength = length(AtomString),
    Padding = lists:duplicate(ColumnWidth - AtomLength, $\s),
    erlang:display_string(stdout, AtomString ++ Padding),
    display_report(A);
display_report(F, A) ->
    erlang:display({F, A}).

display_report(#{ report := Report }) ->
    display_report(Report);
display_report([A, []]) ->
    %% Special case for crash reports when process has no links
    display_report(A);
display_report(A = [_|_]) ->
    case lists:all(fun({Key,_Value}) -> is_atom(Key); (_) -> false end, A) of
	true ->
	    erlang:display_string(stdout, "\n"),
	    lists:foreach(
	      fun({Key, Value}) ->
		      erlang:display_string(
                        stdout,
                        "    " ++ atom_to_list(Key) ++ ": "),
		      erlang:display(Value)
	      end, A);
	false ->
	    erlang:display(A)
    end;
display_report(A) ->
    erlang:display(A).
