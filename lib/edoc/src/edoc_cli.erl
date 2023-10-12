%% @doc EDoc command line interface
-module(edoc_cli).
-export([main/1]).

%% TODO: accept `private'/`hidden' and forward accordingly

main([]) ->
    print(usage());
main(Args) ->
    Opts = parse_args(Args),
    print("Running with opts:\n~p\n", [Opts]),
    ok = code:add_pathsa(maps:get(code_paths, Opts)),
    case Opts of
        #{run := app, app := App} ->
            edoc:application(App, edoc_opts(Opts));
        #{run := files, files := Files} ->
            edoc:files(Files, edoc_opts(Opts))
    end.

parse_args(Args) ->
    Init = #{mode => default,
	     run => app,
	     app => no_app,
	     files => [],
	     code_paths => [],
	     out_dir => undefined,
	     include_paths => [],
	     continue => false},
    check_opts(maps:without([continue], parse_args(Args, Init))).

parse_args([], Opts) ->
    Opts;
parse_args(["-" ++ _ = Arg | Args], #{continue := Cont} = Opts) when Cont /= false ->
    parse_args([Arg | Args], Opts#{continue := false});

parse_args(["-chunks" | Args], Opts) ->
    parse_args(Args, Opts#{mode := chunks});

parse_args(["-o", OutDir | Args], Opts) ->
    parse_args(Args, Opts#{out_dir := OutDir});

parse_args(["-pa", Path | Args], Opts) ->
    #{code_paths := Paths} = Opts,
    parse_args(Args, Opts#{code_paths := Paths ++ [Path]});

parse_args(["-I", Path | Args], Opts) ->
    #{include_paths := Paths} = Opts,
    parse_args(Args, Opts#{include_paths := Paths ++ [Path]});

parse_args(["-app", App | Args], Opts) ->
    parse_args(Args, Opts#{run := app, app := list_to_atom(App)});

parse_args(["-files" | Args], Opts) ->
    parse_args(Args, Opts#{run := files, continue := files});
parse_args([File | Args], #{continue := files} = Opts) ->
    #{files := Files} = Opts,
    parse_args(Args, Opts#{files := Files ++ [File]});

parse_args([Unknown | Args], Opts) ->
    print("Unknown option: ~ts\n", [Unknown]),
    parse_args(Args, Opts).

check_opts(Opts) ->
    case Opts of
	#{run := app, app := App} when is_atom(App), App /= no_app -> ok;
	#{run := app, app := no_app} -> quit(no_app, Opts);
	#{run := files, files := [_|_]} -> ok;
	#{run := files, files := []} -> quit(no_files, Opts)
    end,
    #{mode := Mode,
      out_dir := OutDir,
      code_paths := CodePaths,
      include_paths := IncludePaths} = Opts,
    lists:member(Mode, [default, chunks]) orelse erlang:error(mode, Opts),
    if
	is_list(OutDir) -> ok;
	OutDir =:= undefined -> ok;
	OutDir =/= undefined -> erlang:error(out_dir, Opts)
    end,
    is_list(CodePaths) orelse erlang:error(code_paths),
    is_list(IncludePaths) orelse erlang:error(include_paths),
    Opts.

quit(Reason, _Opts) ->
    case Reason of
	no_app ->
	    print("No app name specified\n");
	no_files ->
	    print("No files to process\n")
    end,
    print("\n"),
    print(usage()),
    erlang:halt(1).

edoc_opts(Opts) ->
    EdocOpts = case maps:get(mode, Opts) of
		   default ->
		       [{preprocess, true}];
		   chunks ->
		       [{doclet, edoc_doclet_chunks},
			{layout, edoc_layout_chunks},
			{preprocess, true}]
	       end,
    OutDir = maps:get(out_dir, Opts),
    [{includes, maps:get(include_paths, Opts)} | EdocOpts] ++
    [{dir, OutDir} || OutDir /= undefined].

print(Text) ->
    print(Text, []).

print(Fmt, Args) ->
    io:format(Fmt, Args).

usage() ->
    "Usage: edoc [options] -app App\n"
    "       edoc [options] -files Source...\n"
    "\n"
    "Run EDoc from the command line:\n"
    "  -app App       \truns edoc:application/2; App is the application name\n"
    "  -files Sources \truns edoc:files/2; Sources are .erl files\n"
    "\n"
    "Options:\n"
    "  -chunks        \twhen present, only doc chunks are generated\n"
    "  -o Dir         \tuse Dir for doc output\n"
    "  -I IncPath     \tadd IncPath to EDoc include file search path;\n"
    "                 \tcan be used multiple times\n"
    "  -pa CodePath   \tadd CodePath to Erlang code path; can be used multiple times\n".
