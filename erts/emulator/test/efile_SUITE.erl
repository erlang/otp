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

-module(efile_SUITE).
-export([all/0, suite/0]).
-export([iter_max_files/1, async_dist/1]).

-export([do_iter_max_files/2, do_async_dist/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [iter_max_files, async_dist].

do_async_dist(Dir) ->
    X = 100,
    AT = erlang:system_info(thread_pool_size),
    Keys = file_keys(Dir,AT*X,[],[]),
    Tab = ets:new(x,[ordered_set]),
    [ ets:insert(Tab,{N,0}) || N <- lists:seq(0,AT-1) ],
    [ ets:update_counter(Tab,(N rem AT),1) || N <- Keys ],
    Res = [ V || {_,V} <- ets:tab2list(Tab) ],
    ets:delete(Tab),
    {Res, sdev(Res)/X}.

sdev(List) ->
    Len = length(List),
    Mean = lists:sum(List)/Len,
    math:sqrt(lists:sum([ (X - Mean) * (X - Mean) || X <- List ]) / Len).

file_keys(_,0,FdList,FnList) ->
    [ file:close(FD) || FD <- FdList ],
    [ file:delete(FN) || FN <- FnList ],
    [];
file_keys(Dir,Num,FdList,FnList) ->
    Name = "dummy"++integer_to_list(Num),
    FN = filename:join([Dir,Name]),
    case file:open(FN,[write,raw]) of
        {ok,FD} ->
            {file_descriptor,prim_file,{Port,_}} = FD,
            <<X:32/integer-big>> = 
            iolist_to_binary(erlang:port_control(Port,$K,[])),
            [X | file_keys(Dir,Num-1,[FD|FdList],[FN|FnList])];
        {error,_} ->
            % Try freeing up FD's if there are any
            case FdList of 
                [] ->
                    exit({cannot_open_file,FN});
                _ ->
                    [ file:close(FD) || FD <- FdList ],
                    [ file:delete(F) || F <- FnList ],
                    file_keys(Dir,Num,[],[])
            end
    end.

%% Check that the distribution of files over async threads is fair
async_dist(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    TestFile = filename:join(DataDir, "existing_file"),
    Dir = filename:dirname(code:which(?MODULE)),
    AsyncSizes = [7,10,100,255,256,64,63,65],
    Max = 0.5,

    lists:foreach(fun(Size) ->
                          {ok,Node} = 
                          test_server:start_node
                          (test_iter_max_files,slave,
                           [{args,
                             "+A "++integer_to_list(Size)++
                             " -pa " ++ Dir}]),
                          {Distr,SD} = rpc:call(Node,?MODULE,do_async_dist,
                                                [DataDir]),
                          test_server:stop_node(Node),
                          if
                              SD > Max ->
                                  io:format("Bad async queue distribution for "
                                            "~p async threads:~n"
                                            "    Standard deviation is ~p~n"
                                            "    Key distribution:~n    ~lp~n",
                                            [Size,SD,Distr]),
                                  exit({bad_async_dist,Size,SD,Distr});
                              true ->
                                  io:format("OK async queue distribution for "
                                            "~p async threads:~n"
                                            "    Standard deviation is ~p~n"
                                            "    Key distribution:~n    ~lp~n",
                                            [Size,SD,Distr]),
                                  ok
                          end
                  end, AsyncSizes),
    ok.

%%
%% Open as many files as possible. Do this several times and check 
%% that we get the same number of files every time.
%%

iter_max_files(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    TestFile = filename:join(DataDir, "existing_file"),
    N = 10,
    %% Run on a different node in order to set the max ports
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(test_iter_max_files,slave,
                                       [{args,"+Q 1524 -pa " ++ Dir}]),
    L = rpc:call(Node,?MODULE,do_iter_max_files,[N, TestFile]),
    test_server:stop_node(Node),
    io:format("Number of files opened in each test:~n~w\n", [L]),
    all_equal(L),
    Head = hd(L),
    if  Head >= 2 -> ok;
        true -> ct:fail(too_few_files)
    end,
    {comment, "Max files: " ++ integer_to_list(hd(L))}.

do_iter_max_files(N, Name) when N > 0 -> 
    [max_files(Name)| do_iter_max_files(N-1, Name)];
do_iter_max_files(_, _) ->
    [].

all_equal([E, E| T]) ->
    all_equal([E| T]);
all_equal([_]) ->
    ok;
all_equal([]) ->
    ok.

max_files(Name) ->
    Fds = open_files(Name),
    N = length(Fds),
    close_files(Fds),
    N.

close_files([Fd| Fds]) ->
    file:close(Fd),
    close_files(Fds);
close_files([]) ->
    ok.

open_files(Name) ->
    case file:open(Name, [read,raw]) of
        {ok, Fd} ->
            [Fd| open_files(Name)];
        {error, _Reason} ->
            %		  io:format("Error reason: ~p", [_Reason]),
            []
    end.
