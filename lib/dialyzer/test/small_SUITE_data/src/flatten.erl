%%%-------------------------------------------------------------------
%%% File    : flatten.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description :
%%%
%%% Created :  4 Nov 2007 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(flatten).

-export([t/1]).

t(Dir) ->
  case file:list_dir(Dir) of
    {ok,FileList} ->
      FileList;
    {error,Reason} ->
      {error,lists:flatten("Can't open directory "++Dir++": "++Reason)}
  end.
