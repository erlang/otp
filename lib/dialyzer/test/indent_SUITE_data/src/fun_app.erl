%% This is taken from the code of distel.

-module(fun_app).
-export([html_index/2]). % , lines/3, curry/2]).

html_index(file,Dir) ->
  fold_file(curry(fun lines/3,Dir),[],filename:join([Dir,"doc","man_index.html"])).

fold_file(Fun,Acc0,File) ->
  {ok, FD} = file:open(File, [read]),
  Acc = fold_file_lines(FD,Fun,Acc0),
  file:close(FD),
  Acc.

fold_file_lines(FD,Fun,Acc) ->
  case io:get_line(FD, "") of
    eof -> Acc;
    Line -> fold_file_lines(FD,Fun,Fun(trim_nl(Line),Acc))
  end.

trim_nl(Str) -> lists:reverse(tl(lists:reverse(Str))).

lines(Line,_,Dir) ->
  case string:tokens(Line, "<> \"") of
    ["TD", "A", "HREF=", "../"++Href, M|_] ->
      case filename:basename(Href, ".html") of
	"index" -> ok;
	M -> e_set({file,M}, filename:join([Dir,Href]))
      end;
    _ -> ok
  end.

e_set(Key,Val) -> ets:insert(?MODULE, {Key,Val}).

curry(F, Arg) ->
  case erlang:fun_info(F,arity) of
    {_,1} -> fun() -> F(Arg) end;
    {_,2} -> fun(A) -> F(A,Arg) end;
    {_,3} -> fun(A,B) -> F(A,B,Arg) end;
    {_,4} -> fun(A,B,C) -> F(A,B,C,Arg) end
  end.
