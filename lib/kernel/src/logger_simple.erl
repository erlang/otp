%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
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
-module(logger_simple).

-export([adding_handler/2, removing_handler/2, log/2]).
-export([get_buffer/0]).

%% This module implements a simple handler for logger. It is the
%% default used during system start.

%%%-----------------------------------------------------------------
%%% API
get_buffer() ->
    case whereis(?MODULE) of
        undefined ->
            {error,noproc};
        Pid ->
            Ref = erlang:monitor(process,Pid),
            Pid ! {get_buffer,self()},
            receive
                {buffer,Buffer} ->
                    erlang:demonitor(Ref,[flush]),
                    {ok,Buffer};
                {'DOWN',Ref,process,Pid,Reason} ->
                    {error,Reason}
            end
    end.

%%%-----------------------------------------------------------------
%%% Logger callback

adding_handler(?MODULE,Config) ->
    Me = self(),
    case whereis(?MODULE) of
        undefined ->
            {Pid,Ref} = spawn_opt(fun() -> init(Me,Config) end,
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

removing_handler(?MODULE,_Config) ->
    case whereis(?MODULE) of
        undefined ->
            ok;
        Pid ->
            Ref = erlang:monitor(process,Pid),
            unlink(Pid),
            Pid ! stop,
            receive {'DOWN',Ref,process,Pid,_} ->
                    ok
            end
    end.

log(#{meta:=#{error_logger:=#{tag:=info_report,type:=Type}}},_Config)
  when Type=/=std_info ->
    %% Skip info reports that are not 'std_info' (ref simple logger in
    %% error_logger)
    ok;
log(#{msg:=_,meta:=#{time:=_}}=Log,_Config) ->
    _ = case whereis(?MODULE) of
            undefined ->
                %% Is the node on the way down? Real emergency?
                %% Log directly from client just to get it out
                do_log(
                  #{level=>error,
                    msg=>{report,{error,simple_handler_process_dead}},
                    meta=>#{time=>erlang:monotonic_time(microsecond)}}),
                do_log(Log);
            _ ->
                ?MODULE ! {log,Log}
        end,
    ok;
log(_,_) ->
    %% Unexpected log.
    %% We don't want to crash the simple logger, so ignore this.
    ok.

%%%-----------------------------------------------------------------
%%% Process
init(Starter,Config) ->
    register(?MODULE,self()),
    Starter ! {self(),started},
    BufferSize =
        case Config of
            #{?MODULE:=#{buffer:=true}} ->
                10;
            _ ->
                infinity
        end,
    loop(#{buffer_size=>BufferSize,dropped=>0,buffer=>[]},infinity).

loop(Buffer,Timeout) ->
    receive
        stop ->
            ok;
        {get_buffer,From} ->
            loop(Buffer#{send_to=>From},0);
        {log,#{msg:=_,meta:=#{time:=_}}=Log} ->
            do_log(Log),
            loop(update_buffer(Buffer,Log),Timeout);
        _ ->
            %% Unexpected message - flush it!
            loop(Buffer,Timeout)
    after Timeout ->
            #{dropped:=D,buffer:=B,send_to:=Pid} = Buffer,
            LogList = lists:reverse(B) ++ drop_msg(D),
            Pid ! {buffer,LogList},
            loop(Buffer#{buffer_size=>infinity,
                         dropped=>0,
                         buffer=>[],
                         send_to=>false},
                 infinity)
    end.

update_buffer(#{buffer_size:=infinity}=Buffer,_Log) ->
    Buffer;
update_buffer(#{buffer_size:=0,dropped:=D}=Buffer,_Log) ->
    Buffer#{dropped=>D+1};
update_buffer(#{buffer_size:=S,buffer:=B}=Buffer,Log) ->
    Buffer#{buffer_size=>S-1,buffer=>[Log|B]}.

drop_msg(0) ->
    [];
drop_msg(N) ->
    [#{level=>info,
       msg=>{"Simple handler buffer full, dropped ~w messages",[N]},
       meta=>#{time=>erlang:monotonic_time(microsecond)}}].

%%%-----------------------------------------------------------------
%%% Internal

%% Can't do io_lib:format

do_log(#{msg:={report,Report},
         meta:=#{time:=T,error_logger:=#{type:=Type}}}) ->
    display_date(T),
    display_report(Type,Report);
do_log(#{msg:=Msg,meta:=#{time:=T}}) ->
    display_date(T),
    display(Msg).

display_date(Timestamp0) when is_integer(Timestamp0) ->
    Timestamp = Timestamp0 + erlang:time_offset(microsecond),
    Micro = Timestamp rem 1000000,
    Sec = Timestamp div 1000000,
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime_to_localtime(
                            erlang:posixtime_to_universaltime(Sec)),
    erlang:display_string(
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
        String -> erlang:display_string(String), erlang:display_string("\n")
    catch _:_ -> erlang:display(Chardata)
    end;
display({report,Report}) when is_map(Report) ->
    display_report(maps:to_list(Report));
display({report,Report}) ->
    display_report(Report);
display({F, A}) when is_list(F), is_list(A) ->
    erlang:display_string(F ++ "\n"),
    [begin
	 erlang:display_string("\t"),
	 erlang:display(Arg)
     end || Arg <- A],
    ok.

display_report(Atom, A) when is_atom(Atom) ->
    %% The widest atom seems to be 'supervisor_report' at 17.
    ColumnWidth = 20,
    AtomString = atom_to_list(Atom),
    AtomLength = length(AtomString),
    Padding = lists:duplicate(ColumnWidth - AtomLength, $\s),
    erlang:display_string(AtomString ++ Padding),
    display_report(A);
display_report(F, A) ->
    erlang:display({F, A}).

display_report([A, []]) ->
    %% Special case for crash reports when process has no links
    display_report(A);
display_report(A = [_|_]) ->
    case lists:all(fun({Key,_Value}) -> is_atom(Key); (_) -> false end, A) of
	true ->
	    erlang:display_string("\n"),
	    lists:foreach(
	      fun({Key, Value}) ->
		      erlang:display_string(
			"    " ++
			    atom_to_list(Key) ++
			    ": "),
		      erlang:display(Value)
	      end, A);
	false ->
	    erlang:display(A)
    end;
display_report(A) ->
    erlang:display(A).
