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

-module(efile_SUITE).
-export([all/1]).
-export([iter_max_files/1]).

-include("test_server.hrl").

all(suite) -> [iter_max_files].

%%
%% Open as many files as possible. Do this several times and check 
%% that we get the same number of files every time.
%%

iter_max_files(suite) -> [];
iter_max_files(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line TestFile = filename:join(DataDir, "existing_file"),
    ?line L = do_iter_max_files(10, TestFile),
    ?line io:format("Number of files opened in each test:~n~w\n", [L]),
    ?line all_equal(L),
    Head = hd(L),
    if  Head >= 2 -> ok;
	true -> ?line test_server:fail(too_few_files)
    end,
    {comment, "Max files: " ++ integer_to_list(hd(L))}.

do_iter_max_files(N, Name) when N > 0 -> 
    ?line [max_files(Name)| do_iter_max_files(N-1, Name)];
do_iter_max_files(_, _) ->
    [].

all_equal([E, E| T]) ->
    ?line all_equal([E| T]);
all_equal([_]) ->
    ok;
all_equal([]) ->
    ok.
    
max_files(Name) ->
    ?line Fds = open_files(Name),
    ?line N = length(Fds),
    ?line close_files(Fds),
    N.

close_files([Fd| Fds]) ->
    ?line file:close(Fd),
    ?line close_files(Fds);
close_files([]) ->
    ok.

open_files(Name) ->
    ?line case file:open(Name, [read,raw]) of
	      {ok, Fd} ->
		  [Fd| open_files(Name)];
	      {error, Reason} ->
		  io:format("Error reason: ~p", [Reason]),
		  []
	  end.
