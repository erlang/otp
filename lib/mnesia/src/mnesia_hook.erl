%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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

-module(mnesia_hook).

-include("mnesia.hrl").

-export([
         register_hook/2,
         do_post_commit/2
        ]).

-define(hook(NAME), {mnesia_hook, NAME}).

-type post_commit_hook_data() ::
        #{ node => node()
         , ram_copies => list()
         , disc_copies => list()
         , disc_only_copies => list()
         , ext => list()
         , schema_ops => list()
         }.

-type post_commit_hook() :: fun((_Tid, post_commit_hook_data()) -> ok).

-spec register_hook(post_commit, post_commit_hook()) -> ok | {error, term()}.
register_hook(post_commit, Hook) when is_function(Hook, 2) ->
    persistent_term:put(?hook(post_commit), Hook);
register_hook(_, _) ->
    {error, bad_type}.

-spec do_post_commit(_Tid, #commit{}) -> ok.
do_post_commit(Tid, Commit) ->
    case persistent_term:get(?hook(post_commit), undefined) of
        undefined ->
            ok;
        Fun ->
            #commit{ node = Node
                   , ram_copies = Ram
                   , disc_copies = Disc
                   , disc_only_copies = DiscOnly
                   , ext = Ext
                   , schema_ops = SchemaOps
                   } = Commit,
            CommitData = #{ node => Node
                          , ram_copies => Ram
                          , disc_copies => Disc
                          , disc_only_copies => DiscOnly
                          , ext => Ext
                          , schema_ops => SchemaOps
                          },
            try Fun(Tid, CommitData)
            catch EC:Err:Stack ->
                    mnesia_lib:error("Mnesia post_commit hook failed: ~p:~p~nStacktrace:~p~n", [EC, Err, Stack])
            end,
            ok
    end.
