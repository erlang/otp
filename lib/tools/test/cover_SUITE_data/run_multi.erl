%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(run_multi).

%% API
-export([run/1]).

%%%===================================================================
%%% API
%%%===================================================================
run([File]) ->
    NewFile = lists:flatten(io_lib:format("test/cover_SUITE_data/~s", [File])),
    io:format("file is ~s~n", [NewFile]),
    {ok, _} = cover:compile_beam(multi),
    multi:b(),
    multi:c(),
    multi:d(),
    cover:analyse_to_file(multi, NewFile, [html]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
