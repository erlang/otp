#!/usr/bin/env escript
%% -*- erlang -*-

main([InFile,OutFile]) ->
    {ok,In} = file:open(InFile,read),
    {ok,Out} = file:open(OutFile,write),
    write_file(Out, read_file(In)),
    file:close(In),
    file:close(Out).

write_file(D, {ok,Ms}) ->
    io:format(D,'-define(dh_default_groups,~n    ~p~n    ).~n',[Ms]).

one_line(Line, Acc) when is_binary(Line) -> 
    one_line(binary_to_list(Line), Acc);
one_line("#"++_, Acc) ->
    Acc;
one_line(Line, Acc) when is_list(Line) -> 
    try
	[_Time,_Type,_Tests,_Tries,Size,G,P] = string:tokens(Line," \r\n"),
	[{list_to_integer(Size),
	  {list_to_integer(G), list_to_integer(P,16)}
	 } | Acc]
    catch
	_:_ -> io:format("*** skip line ~p",[Line]),
	       Acc
    end.


collect_per_size(L) ->
    lists:foldr(
      fun({Sz,GP}, [{Sz,GPs}|Acc]) -> [{Sz,[GP|GPs]}|Acc];
	 ({Sz,GP}, Acc) -> [{Sz,[GP]}|Acc]
      end, [], lists:sort(L)).


read_file(D) ->
    read_file(D, []).

read_file(D, Acc) ->
    case io:get_line(D,"") of
	{error,Error} ->
	    {error,Error};
	eof ->
	    {ok, collect_per_size(Acc)};
	Data ->
	    read_file(D, one_line(Data,Acc))
    end.


