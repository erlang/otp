%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
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
-module(error_logger_forwarder).

%% API.
-export([register/0, unregister/0]).

%% Internal export for error_logger.
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

%% Any crash report messages generated will be forwarded
%% to the current process (the one doing the call to register/0).
%%
register() ->
    error_logger:add_report_handler(?MODULE, self()).

unregister() ->
    Self = self(),
    Self = error_logger:delete_report_handler(?MODULE).

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
