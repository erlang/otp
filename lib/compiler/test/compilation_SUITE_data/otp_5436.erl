%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
-module(otp_5436).
-compile(export_all).

?MODULE() ->
    ok.

-record(readerState, {action_index,
                      log_index,
                      log_name,
                      time_period,
                      rec_id_period,
                      result_format,
                      action_status,
                      filter_type,
                      event_list,
                      sender_list,
                      read_status}).

handle_call(delete,_From,State) ->
    case catch debug:filter(console,logReader) of
        true ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,306,console,self(),sysAssert:format_time2(erlang:now())])|"delete, State: ~p ~n"],[State]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["delete, State: ~p ~n",[State]]),
                    ok
            end;
        false ->
            disabled;
        {'EXIT',{undef,_}} ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,306,console,self(),sysAssert:format_time2(erlang:now())])|"delete, State: ~p ~n"],[State]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["delete, State: ~p ~n",[State]]),
                    ok
            end;
        {'EXIT',_} ->
            debug:filter(console,logReader);
        _ ->
            exit({badmatch,{{debug,filter,[console,logReader]},debug:filter(console,logReader)}})
    end,
    ?MODULE:clean_result(State),
    {stop,normal,ok,State};
handle_call(die,_,State) ->
    {stop,normal,ok,State};
handle_call(_Action,_From,#readerState{action_status = 2} = State) ->
    {reply,error,State};
handle_call(update_action_attr,_From,State) ->
    NewState = ?MODULE:handle_update_action_attr(State),
    case catch debug:filter(console,logReader) of
        true ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,317,console,self(),sysAssert:format_time2(erlang:now())])|"update_action_attr, State: ~p ~n"],[NewState]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["update_action_attr, State: ~p ~n",[NewState]]),
                    ok
            end;
        false ->
            disabled;
        {'EXIT',{undef,_}} ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,317,console,self(),sysAssert:format_time2(erlang:now())])|"update_action_attr, State: ~p ~n"],[NewState]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["update_action_attr, State: ~p ~n",[NewState]]),
                    ok
            end;
        {'EXIT',_} ->
            debug:filter(console,logReader);
        _ ->
            exit({badmatch,{{debug,filter,[console,logReader]},debug:filter(console,logReader)}})
    end,
    {reply,ok,NewState};
handle_call(update_event_filter,_From,State) ->
    NewState = State#readerState{event_list = ?MODULE:get_event_list(State#readerState.action_index)},
    case catch debug:filter(console,logReader) of
        true ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,323,console,self(),sysAssert:format_time2(erlang:now())])|"update_event_filter, State: ~p ~n"],[NewState]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["update_event_filter, State: ~p ~n",[NewState]]),
                    ok
            end;
        false ->
            disabled;
        {'EXIT',{undef,_}} ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,323,console,self(),sysAssert:format_time2(erlang:now())])|"update_event_filter, State: ~p ~n"],[NewState]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["update_event_filter, State: ~p ~n",[NewState]]),
                    ok
            end;
        {'EXIT',_} ->
            debug:filter(console,logReader);
        _ ->
            exit({badmatch,{{debug,filter,[console,logReader]},debug:filter(console,logReader)}})
    end,
    {reply,ok,NewState};
handle_call(update_sender_filter,_From,State) ->
    NewState = State#readerState{sender_list = ?MODULE:get_sender_list(State#readerState.action_index)},
    case catch debug:filter(console,logReader) of
        true ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,329,console,self(),sysAssert:format_time2(erlang:now())])|"update_sender_filter, State: ~p ~n"],[NewState]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["update_sender_filter, State: ~p ~n",[NewState]]),
                    ok
            end;
        false ->
            disabled;
        {'EXIT',{undef,_}} ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,329,console,self(),sysAssert:format_time2(erlang:now())])|"update_sender_filter, State: ~p ~n"],[NewState]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["update_sender_filter, State: ~p ~n",[NewState]]),
                    ok
            end;
        {'EXIT',_} ->
            debug:filter(console,logReader);
        _ ->
            exit({badmatch,{{debug,filter,[console,logReader]},debug:filter(console,logReader)}})
    end,
    {reply,ok,NewState};
handle_call(Request,_From,State) ->
    case catch debug:filter(console,logReader) of
        true ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,332,console,self(),sysAssert:format_time2(erlang:now())])|"Call ~p, State: ~p ~n"],[Request,State]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["Call ~p, State: ~p ~n",[Request,State]]),
                    ok
            end;
        false ->
            disabled;
        {'EXIT',{undef,_}} ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,332,console,self(),sysAssert:format_time2(erlang:now())])|"Call ~p, State: ~p ~n"],[Request,State]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["Call ~p, State: ~p ~n",[Request,State]]),
                    ok
            end;
        {'EXIT',_} ->
            debug:filter(console,logReader);
        _ ->
            exit({badmatch,{{debug,filter,[console,logReader]},debug:filter(console,logReader)}})
    end,
    {stop,{error,unknown,Request},State}.

handle_info(Request,State) ->
    case catch debug:filter(readlog,logReader) of
        true ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,345,readlog,self(),sysAssert:format_time2(erlang:now())])|"Info ~p, State: ~p ~n"],[Request,State]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["Info ~p, State: ~p ~n",[Request,State]]),
                    ok
            end;
        false ->
            disabled;
        {'EXIT',{undef,_}} ->
            case io:format([io_lib:format("~nSYS_DBG {~p,~p,~p}, ~w ~s:~n-- ",[logReader,345,readlog,self(),sysAssert:format_time2(erlang:now())])|"Info ~p, State: ~p ~n"],[Request,State]) of
                ok ->
                    ok;
                _ ->
                    io:format("*** Bad format (~p, ~p) ***~n",["Info ~p, State: ~p ~n",[Request,State]]),
                    ok
            end;
        {'EXIT',_} ->
            debug:filter(readlog,logReader);
        _ ->
            exit({badmatch,{{debug,filter,[readlog,logReader]},debug:filter(readlog,logReader)}})
    end,
    {stop,{error,unknown,Request},State}.
