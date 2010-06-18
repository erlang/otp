-module(dialyzer_test).

-export([dialyzer_test/6]).

-include("test_server.hrl").

-define(test_case_dir, "src").
-define(results_dir,"results").
-define(plt_filename,".dialyzer_plt").
-define(required_modules, "kernel stdlib compiler erts").

dialyzer_test(Options, TestCase, Kind, Dir, OutDir, Dog) ->
    PltFilename = filename:join(OutDir, ?plt_filename),
    case file:read_file_info(PltFilename) of
	{ok, _} -> ok;
	{error, _ } -> create_plt(OutDir, Dog)
    end,
    SrcDir = filename:join(Dir, ?test_case_dir),
    ResDir = filename:join(Dir, ?results_dir),
    TestCaseString = atom_to_list(TestCase),
    Filename = filename:join(SrcDir, TestCaseString),
    CorrectOptions = convert_relative_paths(Options, Dir),
    FilesOption =
	case Kind of
	    file -> {files, [Filename ++ ".erl"]};
	    dir  -> {files_rec, [Filename]}
	end,
    ResFile = TestCaseString,
    NewResFile = filename:join(OutDir, ResFile),
    OldResFile = filename:join(ResDir, ResFile),
    RawWarns = dialyzer:run([FilesOption,
			     {init_plt, PltFilename},
			     {from, src_code},
			     {check_plt, false} | CorrectOptions]),
    Warns = lists:sort([dialyzer:format_warning(W) || W <- RawWarns]),
    case Warns of
	[] -> ok;
	_  ->
	    case file:open(NewResFile,['write']) of
		{ok, OutFile} ->
		    io:format(OutFile,"\n~s",[Warns]),
		    file:close(OutFile);
		Other -> erlang:error(Other)
	    end
    end,
    case diff(NewResFile, OldResFile) of
	'same' -> file:delete(NewResFile),
		  'same';
	Any    -> Any
    end.

create_plt(OutDir, Dog) ->
    PltFilename = filename:join(OutDir, ?plt_filename),
    ?t:timetrap_cancel(Dog),
    ?t:format("Generating plt..."),
    HomeDir = os:getenv("HOME"),
    HomePlt = filename:join(HomeDir, ?plt_filename),
    file:copy(HomePlt, PltFilename),
    try
	AddCommand = "dialyzer --add_to_plt --output_plt " ++
	    PltFilename ++ " --apps " ++ ?required_modules,
	?t:format(AddCommand ++ "\n"),
	?t:format(os:cmd(AddCommand)),
	dialyzer:run([{analysis_type, plt_check},
		      {init_plt, PltFilename}]) of
	[] -> ok
    catch
	_:_ ->
	    BuildCommand = "dialyzer --build_plt --output_plt " ++
		PltFilename ++ " --apps " ++ ?required_modules,
	    ?t:format(BuildCommand ++ "\n"),
	    ?t:format(os:cmd(BuildCommand))
    end.

convert_relative_paths(Options, Dir) ->
    convert_relative_paths(Options, Dir, []).

convert_relative_paths([], _Dir, Acc) ->
    Acc;
convert_relative_paths([{include_dirs, Paths}|Rest], Dir, Acc) ->
    AbsolutePaths = convert_relative_paths_1(Paths, Dir, []),
    convert_relative_paths(Rest, Dir, [{include_dirs, AbsolutePaths}|Acc]);
convert_relative_paths([Option|Rest], Dir, Acc) ->
    convert_relative_paths(Rest, Dir, [Option|Acc]).

convert_relative_paths_1([], _Dir, Acc) ->
    Acc;
convert_relative_paths_1([Path|Rest], Dir, Acc) ->
    convert_relative_paths_1(Rest, Dir, [filename:join(Dir, Path)|Acc]).

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
	       non_neg_integer(), array()) -> {[string()], array()}.

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


