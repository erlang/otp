%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
-module(error_logger_forwarder).

%% API.
-export([register/0]).

%% Internal export for error_logger.
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

%% Any crash report messages generated will be forwarded
%% to the current process (the one doing the call to register/0).
%%
register() ->
    error_logger:add_report_handler(?MODULE, self()).

init(Tester) ->
    {ok,Tester}.
    
handle_event(Event, Tester) ->
    Tester ! Event,
    {ok,Tester}.

handle_info(_, State) ->
    {ok,State}.

handle_call(_Query, State) -> {ok,{error,bad_query},State}.

terminate(_Reason, State) ->
    State.
