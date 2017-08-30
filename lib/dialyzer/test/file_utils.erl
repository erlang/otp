-module(file_utils).

-export([list_dir/3, file_type/1, diff/2]).

-include_lib("kernel/include/file.hrl").

-type ext_posix()::posix()|'badarg'.
-type posix()::atom().

-spec list_dir(file:filename(), string(), boolean()) ->
		      {error, ext_posix()} | {ok, [file:filename()]}.

list_dir(Dir, Extension, Dirs) ->
    case file:list_dir(Dir) of
	{error, _} = Error-> Error;
	{ok, Filenames} ->
	    FullFilenames = [filename:join(Dir, F) || F <-Filenames ],
	    Matches1 = case Dirs of
			   true ->
			       [F || F <- FullFilenames,
				     file_type(F) =:= {ok, 'directory'}];
			   false -> []
		       end,
	    Matches2 = [F || F <- FullFilenames,
			     file_type(F) =:= {ok, 'regular'},
			     filename:extension(F) =:= Extension],
	    {ok, lists:sort(Matches1 ++ Matches2)}
    end.

-spec file_type(file:filename()) ->
		       {ok, 'device' | 'directory' | 'regular' | 'other'} |
		       {error, ext_posix()}.

file_type(Filename) ->
    case file:read_file_info(Filename) of
	{ok, FI} -> {ok, FI#file_info.type};
	Error    -> Error
    end.

-type diff_result()::'same' | {'differ', diff_list()} |
		     {error, {file:filename(), term()}}.
-type diff_list()::[{id(), line(), string()}].
-type id()::'new'|'old'.
-type line()::non_neg_integer().

-spec diff(file:filename(), file:filename()) -> diff_result().

diff(Filename1, Filename2) ->
    File1 =
	case file:open(Filename1, [read]) of
	    {ok, F1} -> {file, F1};
	    _        -> empty
	end,
    File2 =
	case file:open(Filename2, [read]) of
	    {ok, F2} -> {file, F2};
	    _        -> empty
	end,
    case diff1(File1, File2) of
	{error, {N, Error}} ->
	    case N of
		1 -> {error, {Filename1, Error}};
		2 -> {error, {Filename2, Error}}
	    end;
	[]       -> 'same';
	DiffList -> {'differ', DiffList}
    end.

diff1(File1, File2) ->
    case file_to_lines(File1) of
	{error, Error} -> {error, {1, Error}};
	Lines1 ->
	    case file_to_lines(File2) of
		{error, Error} -> {error, {2, Error}};
		Lines2 ->
		    Common = lcs_fast(Lines1, Lines2),
		    diff2(Lines1, 1, Lines2, 1, Common, [])
	    end
    end.

diff2([], _, [], _, [], Acc) -> lists:keysort(2,Acc);
diff2([H1|T1], N1, [], N2, [], Acc) ->
    diff2(T1, N1+1, [], N2, [], [{new, N1, H1}|Acc]);
diff2([], N1, [H2|T2], N2, [], Acc) ->
    diff2([], N1, T2, N2+1, [], [{old, N2, H2}|Acc]);
diff2([H1|T1], N1, [H2|T2], N2, [], Acc) ->
    diff2(T1, N1+1, T2, N2+1, [], [{new, N1, H1}, {old, N2, H2}|Acc]);
diff2([H1|T1]=L1, N1, [H2|T2]=L2, N2, [HC|TC]=LC, Acc) ->
    case H1 =:= H2 of
	true  -> diff2(T1, N1+1, T2, N2+1, TC, Acc);
	false ->
	    case H1 =:= HC of
		true  -> diff2(L1, N1, T2, N2+1, LC, [{old, N2, H2}|Acc]);
		false -> diff2(T1, N1+1, L2, N2, LC, [{new, N1, H1}|Acc])
	    end
    end.

-spec lcs_fast([string()], [string()]) -> [string()].

lcs_fast(S1, S2) ->
  M = length(S1),
  N = length(S2),
  Acc = array:new(M*N, {default, 0}),
  {L, _} = lcs_fast(S1, S2, 1, 1, N, Acc), 
  L.

-spec lcs_fast([string()], [string()],
	       pos_integer(), pos_integer(),
	       non_neg_integer(), array:array()) -> {[string()], array:array()}.

lcs_fast([], _, _, _, _, Acc) ->
  {[], Acc};
lcs_fast(_, [], _, _, _, Acc) ->
  {[], Acc};
lcs_fast([H1|T1] = S1, [H2|T2] = S2, N1, N2, N, Acc) ->
  I = (N1-1) * N + N2 - 1,
  case array:get(I, Acc) of
    0 ->
      case string:equal(H1, H2) of
	true ->
	  {T, NAcc} = lcs_fast(T1, T2, N1+1, N2+1, N, Acc),
	  L = [H1|T],
	  {L, array:set(I, L, NAcc)};
	false ->
	  {L1, NAcc1} = lcs_fast(S1, T2, N1, N2+1, N, Acc), 
	  {L2, NAcc2} = lcs_fast(T1, S2, N1+1, N2, N, NAcc1),
	  L = longest(L1, L2), 
	  {L, array:set(I, L, NAcc2)}
      end;
    L -> 
      {L, Acc}
  end.

-spec longest([string()], [string()]) -> [string()].

longest(S1, S2) ->
  case length(S1) > length(S2) of
    true -> S1;
    false -> S2
  end.

file_to_lines(empty) ->
    [];
file_to_lines({file, File}) ->
    case file_to_lines(File, []) of
	{error, _} = Error -> Error;
	Lines              -> lists:reverse(Lines)
    end.

file_to_lines(File, Acc) ->
    case io:get_line(File, "") of
	{error, _}=Error -> Error;
	eof              -> Acc;
	A                -> file_to_lines(File, [A|Acc])
    end.
