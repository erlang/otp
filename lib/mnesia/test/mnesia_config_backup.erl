%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

%%
-module(mnesia_config_backup).
-author('peterl@erix.ericsson.se').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% This module is used for testing the backup module config parameter.
%%
%% This module is an impostor for the mnesia_backup module.
%%
%%
%% Original doc below:
%%
%% This module contains one implementation of callback functions
%% used by Mnesia at backup and restore. The user may however
%% write an own module the same interface as mnesia_backup and
%% configure Mnesia so the alternate module performs the actual
%% accesses to the backup media. This means that the user may put
%% the backup on medias that Mnesia does not know about, possibly
%% on hosts where Erlang is not running.
%%
%% The OpaqueData argument is never interpreted by other parts of
%% Mnesia. It is the property of this module. Alternate implementations
%% of this module may have different interpretations of OpaqueData.
%% The OpaqueData argument given to open_write/1 and open_read/1
%% are forwarded directly from the user.
%%
%% All functions must return {ok, NewOpaqueData} or {error, Reason}.
%%
%% The NewOpaqueData arguments returned by backup callback functions will
%% be given as input when the next backup callback function is invoked.
%% If any return value does not match {ok, _} the backup will be aborted.
%%
%% The NewOpaqueData arguments returned by restore callback functions will
%% be given as input when the next restore callback function is invoked
%% If any return value does not match {ok, _} the restore will be aborted.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([
         open_write/1, write/2, commit_write/1, abort_write/1,
         open_read/1, read/1, close_read/1
        ]).

-record(backup, {name, mode, items}).

open_write(Name) -> 
    file:delete(Name),
    {ok, #backup{name = Name, mode = write, items = []}}.

write(Opaque, Item) when Opaque#backup.mode == write ->
    %% Build the list in reverse order
    {ok, Opaque#backup{items = [Item | Opaque#backup.items]}}.

commit_write(Opaque) when Opaque#backup.mode == write ->
    Bin = term_to_binary(Opaque#backup.items),
    case file:write_file(Opaque#backup.name, Bin) of
	ok ->
	    {ok, Opaque#backup{mode = closed, items = []}};
	{error, Reason} ->
	    {error, {commit_write, Reason}}
    end.

abort_write(Opaque) ->  
    {ok, Opaque#backup{mode = closed, items = []}}.

open_read(Name) ->
    case file:read_file(Name) of
	{ok, Bin} ->
	    ReverseList = binary_to_term(Bin),
	    List = lists:reverse(ReverseList),
	    {ok, #backup{name = Name, mode = read, items = List}};
	{error, Reason} ->
	    %% {error, {open_read, Reason}}
	    {Reason, error} %% Testing error handling in mnesia
    end.

read(Opaque) when Opaque#backup.mode == read ->
    case Opaque#backup.items of
	[Head | Tail] ->
	    {ok, Opaque#backup{items = Tail}, Head};
	[] ->
	    {ok, Opaque#backup{mode = closed}, []}
    end.

close_read(Opaque) ->
    {ok, Opaque#backup{mode = closed, items = []}}.
