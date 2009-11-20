-module(mexpand).

-export([file/1]).

-import(lists, [foreach/2]).

file(File) ->
    case epp:parse_file(File ++ ".erl", [],[]) of
	{ok, L} ->
	    {ok, Stream} = file:open(File ++ ".out", write),
	    foreach(fun(X) ->
			    io:format(Stream,"~s~n",
				      [erl_pp:form(X)])
		    end, L),
	    file:close(Stream)
    end.
