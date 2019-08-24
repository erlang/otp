%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(sasl_report_file_h).

%%%
%%% A handler that can be connected to the error_logger
%%% event handler.
%%% Writes all sasl_* events formatted to file
%%%

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

init({File, Modes0, Type}) when is_list(Modes0) ->
    process_flag(trap_exit, true),
    Modes1 =
        case lists:keymember(encoding,1,Modes0) of
            true -> Modes0;
            false -> [{encoding,utf8}|Modes0]
        end,
    Modes =
        case [M || M <- Modes1, lists:member(M,[write,append,exclusive])] of
            [] ->
                [write|Modes1];
            _ ->
                Modes1
        end,
    case file:open(File, Modes) of
	{ok,Fd} ->
	    {ok, {Fd, File, Type}};
	What ->
	    What
    end.

handle_event({_Type, GL, _Msg}, State) when node(GL) /= node() ->
    {ok, State};
handle_event(Event, {Fd, File, Type}) ->
    _ = sasl_report:write_report(Fd, Type, tag_event(Event)),
    {ok, {Fd, File, Type}};
handle_event(_, State) ->
    {ok, State}.

handle_info({'EXIT', Fd, _Reason}, {Fd, _File, _Type}) ->
    remove_handler;
handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, _State) -> {error, bad_query}.

terminate(_, {Fd, _File, _Type}) ->
    _ = file:close(Fd),
    [].

tag_event(Event) ->    
    {calendar:local_time(), Event}.
