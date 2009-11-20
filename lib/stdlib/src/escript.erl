%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

-module(escript).

%% Useful functions that can be called from scripts.
-export([script_name/0, foldl/3]).

%% Internal API.
-export([start/0, start/1]).

-record(state, {file,
                module,
                forms_or_bin,
                source,
                n_errors,
                mode,
                exports_main,
                has_records}).
    
script_name() ->
    [ScriptName|_] = init:get_plain_arguments(),
    ScriptName.

%% Apply Fun(Name, GetInfo, GetBin, Acc) for each file in the escript.
%% 
%% Fun/2 must return a new accumulator which is passed to the next call.
%% The function returns the final value of the accumulator. Acc0 is
%% returned if the escript contain an empty archive.
%% 
%% GetInfo/0 is a fun that returns a #file_info{} record for the file.
%% GetBin/0 is a fun that returns a the contents of the file as a binary.
%%
%% An escript may contain erlang code, beam code or an archive:
%%
%% archive - the Fun/2 will be applied for each file in the archive
%% beam - the Fun/2 will be applied once and GetInfo/0 returns the file
%%        info for the (entire) escript file
%% erl - the Fun/2 will be applied once, GetInfo/0 returns the file
%%       info for the (entire) escript file and the GetBin returns
%%       the compiled beam code

%%-spec foldl(fun((string(),
%%                 fun(() -> #file_info()), 
%%                 fun(() -> binary() -> term()),
%%                 term()) -> term()),
%%            term(),
%%            string()).
foldl(Fun, Acc0, File) when is_function(Fun, 4) ->
    case parse_file(File, false) of
        {text, _, Forms, _Mode} when is_list(Forms) ->
            GetInfo = fun() -> file:read_file_info(File) end,
            GetBin =
                fun() ->
                        case compile:forms(Forms, [return_errors, debug_info]) of
                            {ok, _, BeamBin} ->
                                BeamBin;
                            {error, _Errors, _Warnings} ->
				fatal("There were compilation errors.")
                        end
                end,
            try
                {ok, Fun(".", GetInfo, GetBin, Acc0)}
            catch
                throw:Reason ->
                    {error, Reason}
            end;
        {beam, _, BeamBin, _Mode} when is_binary(BeamBin) ->
            GetInfo = fun() -> file:read_file_info(File) end,
            GetBin = fun() -> BeamBin end,
            try
                {ok, Fun(".", GetInfo, GetBin, Acc0)}
            catch
                throw:Reason ->
                    {error, Reason}
            end;
        {archive, _, ArchiveBin, _Mode} when is_binary(ArchiveBin) ->
	    ZipFun =
		fun({Name, GetInfo, GetBin}, A) ->
			A2 = Fun(Name, GetInfo, GetBin, A),
			{true, false, A2}
		end,
            case prim_zip:open(ZipFun, Acc0, {File, ArchiveBin}) of
                {ok, PrimZip, Res} ->
                    ok = prim_zip:close(PrimZip),
                    {ok, Res};
                {error, bad_eocd} ->
                    {error, "Not an archive file"};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%
%% Internal API.
%%

start() ->
    start([]).

start(EscriptOptions) ->
    try 
        %% Commands run using -run or -s are run in a process
        %% trap_exit set to false. Because this behaviour is
        %% surprising for users of escript, make sure to reset
        %% trap_exit to false.
        process_flag(trap_exit, false),
        case init:get_plain_arguments() of
            [File|Args] ->
                parse_and_run(File, Args, EscriptOptions);
            [] ->
                io:format("escript: Missing filename\n", []),
                my_halt(127)
        end
    catch
        throw:Str ->
            io:format("escript: ~s\n", [Str]),
            my_halt(127);
        _:Reason ->
            io:format("escript: Internal error: ~p\n", [Reason]),
            io:format("~p\n", [erlang:get_stacktrace()]),
            my_halt(127)
    end.

parse_and_run(File, Args, Options) ->
    CheckOnly = lists:member("s", Options),
    {Source, Module, FormsOrBin, Mode} = parse_file(File, CheckOnly),
    Mode2 =
        case lists:member("d", Options) of
            true  -> 
		debug;
            false ->
		case lists:member("c", Options) of
		    true  -> 
			compile;
		    false ->
			case lists:member("i", Options) of
			    true  -> interpret;
			    false -> Mode
			end
		end
        end,
    if
        is_list(FormsOrBin) ->
            case Mode2 of
                interpret ->
                    interpret(FormsOrBin, File, Args);
                compile ->
                    case compile:forms(FormsOrBin, [report]) of
                        {ok, Module, BeamBin} ->
                            {module, Module} = code:load_binary(Module, File, BeamBin),
                            run(Module, Args);
                        _Other ->
                            fatal("There were compilation errors.")
                    end;
                debug ->
                    case compile:forms(FormsOrBin, [report, debug_info]) of
                        {ok,Module,BeamBin} ->
                            {module, Module} = code:load_binary(Module, File, BeamBin),
                            debug(Module, {Module, File, File, BeamBin}, Args);
                        _Other ->
                            fatal("There were compilation errors.")
                    end
            end;                    
        is_binary(FormsOrBin) ->
            case Source of
                archive ->
                    case code:set_primary_archive(File, FormsOrBin) of
                        ok when CheckOnly ->
			    case code:load_file(Module) of
				{module, _} ->
				    case erlang:function_exported(Module, main, 1) of
					true ->
					    my_halt(0);
					false ->
					    Text = lists:concat(["Function ", Module, ":main/1 is not exported"]),
					    fatal(Text)
				    end;
				_ ->
				    Text = lists:concat(["Cannot load module ", Module, " from archive"]),
				    fatal(Text)
			    end;
			ok ->
                            case Mode2 of
                                run   -> run(Module, Args);
                                debug -> debug(Module, Module, Args)
                            end;
                        {error, bad_eocd} ->
                            fatal("Not an archive file");
                        {error, Reason} ->
                            fatal(Reason)
                    end;
                beam ->
                    case Mode2 of
                        run ->
                            {module, Module} = code:load_binary(Module, File, FormsOrBin),
                            run(Module, Args);
                        debug -> 
			    [Base | Rest] = lists:reverse(filename:split(File)),
			    Base2 = filename:basename(Base, code:objfile_extension()),
			    Rest2 =
				case Rest of
				    ["ebin" | Top] -> ["src" | Top];
				    _ -> Rest
				end,
			    SrcFile = filename:join(lists:reverse([Base2 ++ ".erl" | Rest2])),
			    debug(Module, {Module, SrcFile, File, FormsOrBin}, Args)
                    end             
            end             
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_file(File, CheckOnly) ->
    S = #state{file = File,
               n_errors = 0,
               mode = interpret,
               exports_main = false,
               has_records = false},
    {ok, Fd} = 
        case file:open(File, [read]) of
            {ok, Fd0} ->
                {ok, Fd0};
            {error, R} ->
                fatal(lists:concat([file:format_error(R), ": '", File, "'"]))
        end,
    {HeaderSz, StartLine, ScriptType} = skip_header(Fd, 1),
    #state{mode = Mode,
	   source = Source,
	   module = Module,
	   forms_or_bin = FormsOrBin} =
	case ScriptType of
	    archive ->
		%% Archive file
		ok = file:close(Fd),
		parse_archive(S, File, HeaderSz);
	    beam ->
		%% Beam file
		ok = file:close(Fd),
		parse_beam(S, File, HeaderSz, CheckOnly);
	    source ->
		%% Source code
		parse_source(S, File, Fd, StartLine, HeaderSz, CheckOnly)
        end,
    {Source, Module, FormsOrBin, Mode}.

%% Skip header and make a heuristic guess about the script type
skip_header(P, LineNo) ->
    %% Skip shebang on first line
    {ok, HeaderSz0} = file:position(P, cur),
    Line1 = get_line(P),
    case classify_line(Line1) of
        shebang ->
            find_first_body_line(P, LineNo);
	archive ->
	    {HeaderSz0, LineNo, archive};
	beam ->
            {HeaderSz0, LineNo, beam};
	_ ->
            find_first_body_line(P, LineNo)
    end.

find_first_body_line(P, LineNo) ->
    {ok, HeaderSz1} = file:position(P, cur),
    %% Look for special comment on second line
    Line2 = get_line(P),
    {ok, HeaderSz2} = file:position(P, cur),
    case classify_line(Line2) of
	emu_args ->
	    %% Skip special comment on second line
	    Line3 = get_line(P),
	    {HeaderSz2, LineNo + 2, guess_type(Line3)};
	_ ->
	    %% Look for special comment on third line
	    Line3 = get_line(P),
	    {ok, HeaderSz3} = file:position(P, cur),
	    case classify_line(Line3) of
		emu_args -> 
		    %% Skip special comment on third line
		    Line4 = get_line(P),
		    {HeaderSz3, LineNo + 3, guess_type(Line4)};
		_ ->
		    %% Just skip shebang on first line
		    {HeaderSz1, LineNo + 1, guess_type(Line2)}
	    end
    end.
        
classify_line(Line) ->
    case Line of
        [$\#, $\! | _] ->
	    shebang;
	[$P, $K | _] ->
	    archive;
	[$F, $O, $R, $1 | _] ->
	    beam;
	[$\%, $\%, $\! | _] ->
	    emu_args;
	 _ ->
	    undefined
   end.

guess_type(Line) ->
    case classify_line(Line) of
	archive -> archive;
	beam    -> beam;
	_       -> source
    end.

get_line(P) ->
    case io:get_line(P, '') of
        eof ->
            fatal("Premature end of file reached");
        Line ->
            Line
    end.

parse_archive(S, File, HeaderSz) ->
    case file:read_file(File) of
	{ok, <<_FirstLine:HeaderSz/binary, Bin/binary>>} ->
	    Mod = 
		case init:get_argument(escript) of
		    {ok, [["main", M]]} ->
			%% Use explicit module name
			list_to_atom(M);
		    _ ->
			%% Use escript name without extension as module name
			RevBase = lists:reverse(filename:basename(File)),
			RevBase2 = 
			    case lists:dropwhile(fun(X) -> X =/= $. end, RevBase) of
                                [$. | Rest] -> Rest;
                                [] -> RevBase
                            end,
                        list_to_atom(lists:reverse(RevBase2))
		end,
	    
	    S#state{source = archive,
		    mode = run,
		    module = Mod,
		    forms_or_bin = Bin};
	{ok, _} ->
	    fatal("Illegal archive format");
	{error, Reason} ->
	    fatal(file:format_error(Reason))
    end.


parse_beam(S, File, HeaderSz, CheckOnly) ->
    {ok, <<_FirstLine:HeaderSz/binary, Bin/binary>>} =
        file:read_file(File),
    case beam_lib:chunks(Bin, [exports]) of
	{ok, {Module, [{exports, Exports}]}} ->
	    case CheckOnly of
		true ->
		    case lists:member({main, 1}, Exports) of
			true ->
			    my_halt(0);
			false ->
			    Text = lists:concat(["Function ", Module, ":main/1 is not exported"]),
			    fatal(Text)
		    end;
		false ->
		    S#state{source = beam,
			    mode = run,
			    module = Module,
			    forms_or_bin = Bin}
	    end;
	{error, beam_lib, Reason} when is_tuple(Reason) ->
            fatal(element(1, Reason));
        {error, beam_lib, Reason} ->
            fatal(Reason)
    end.

parse_source(S, File, Fd, StartLine, HeaderSz, CheckOnly) ->
    {PreDefMacros, Module} = pre_def_macros(File),
    IncludePath = [],
    {ok, _} = file:position(Fd, {bof, HeaderSz}),
    case epp:open(File, Fd, StartLine, IncludePath, PreDefMacros) of
        {ok, Epp} ->
            {ok, FileForm} = epp:parse_erl_form(Epp),
            OptModRes = epp:parse_erl_form(Epp),
            S2 = S#state{source = text, module = Module},
            S3 = 
                case OptModRes of
                    {ok, {attribute,_, module, M} = Form} ->
                        epp_parse_file(Epp, S2#state{module = M}, [Form, FileForm]);
                    {ok, _} ->
                        ModForm = {attribute,1,module, Module},
                        epp_parse_file2(Epp, S2, [ModForm, FileForm], OptModRes);
                    {error, _} ->
                        epp_parse_file2(Epp, S2, [FileForm], OptModRes);
                    {eof,LastLine} ->
                        S#state{forms_or_bin = [FileForm, {eof,LastLine}]}
                end,
            ok = epp:close(Epp),
            ok = file:close(Fd),
	    check_source(S3, CheckOnly);
	{error, Reason} ->
	    io:format("escript: ~p\n", [Reason]),
	    fatal("Preprocessor error")
    end.

check_source(S, CheckOnly) ->
    case S of
	#state{n_errors = Nerrs} when Nerrs =/= 0 ->
	    fatal("There were compilation errors.");
	#state{exports_main = ExpMain, 
	       has_records = HasRecs,
	       forms_or_bin = [FileForm2, ModForm2 | Forms]} ->
	    %% Optionally add export of main/1
	    Forms2 =
		case ExpMain of
		    false -> [{attribute,0,export, [{main,1}]} | Forms];
		    true  -> Forms
		end,
	    Forms3 = [FileForm2, ModForm2 | Forms2],
	    case CheckOnly of
		true ->
		    %% Optionally expand records
		    Forms4 =
			case HasRecs of
			    false -> Forms3;
			    true  -> erl_expand_records:module(Forms3, [])
			end,
		    %% Strong validation and halt
		    case compile:forms(Forms4, [report,strong_validation]) of
			{ok,_} ->
			    my_halt(0);
			_Other ->
			    fatal("There were compilation errors.")
		    end;
		false ->
		    %% Basic validation before execution
		    case erl_lint:module(Forms3) of
			{ok,Ws} ->
			    report_warnings(Ws);
			{error,Es,Ws} ->
			    report_errors(Es),
			    report_warnings(Ws),
			    fatal("There were compilation errors.")
		    end,
		    %% Optionally expand records
		    Forms4 =
			case HasRecs of
			    false -> Forms3;
			    true  -> erl_expand_records:module(Forms3, [])
			end,
		    S#state{forms_or_bin = Forms4}
	    end
    end.

pre_def_macros(File) ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    Replace = fun(Char) -> 
		      case Char of
			  $\. -> $\_;
                          _ -> Char
                      end
              end, 
    CleanBase = lists:map(Replace, filename:basename(File)),
    ModuleStr =
	CleanBase ++ "__" ++
        "escript__" ++
        integer_to_list(MegaSecs) ++ "__" ++
        integer_to_list(Secs) ++ "__" ++
        integer_to_list(MicroSecs),
    Module = list_to_atom(ModuleStr),
    PreDefMacros = [{'MODULE', Module, redefine},
                    {'MODULE_STRING', ModuleStr, redefine}],
    {PreDefMacros, Module}.

epp_parse_file(Epp, S, Forms) ->
    Parsed = epp:parse_erl_form(Epp),
    epp_parse_file2(Epp, S, Forms, Parsed).

epp_parse_file2(Epp, S, Forms, Parsed) ->
    %% io:format("~p\n", [Parsed]),
    case Parsed of
        {ok, Form} ->
            case Form of
                {attribute,Ln,record,{Record,Fields}} ->
                    S2 = S#state{has_records = true},
                    case epp:normalize_typed_record_fields(Fields) of
                        {typed, NewFields} ->
                            epp_parse_file(Epp, S2,
                                           [{attribute, Ln, record, {Record, NewFields}},
                                            {attribute, Ln, type, 
                                             {{record, Record}, Fields, []}} | Forms]);
                        not_typed ->
                            epp_parse_file(Epp, S2, [Form | Forms])
                    end;
                {attribute,Ln,mode,NewMode} ->
                    S2 = S#state{mode = NewMode},
                    if
                        NewMode =:= compile; NewMode =:= interpret; NewMode =:= debug ->
                            epp_parse_file(Epp, S2, [Form | Forms]);
                        true ->
                            Args = lists:flatten(io_lib:format("illegal mode attribute: ~p", [NewMode])),
                            io:format("~s:~w ~s\n", [S#state.file,Ln,Args]),
                            Error = {error,{Ln,erl_parse,Args}},
                            Nerrs= S#state.n_errors + 1,
                            epp_parse_file(Epp, S2#state{n_errors = Nerrs}, [Error | Forms])
                    end;
                {attribute,_,export,Fs} ->
                    case lists:member({main,1}, Fs) of
                        false ->
                            epp_parse_file(Epp, S, [Form | Forms]);
                        true ->
                            epp_parse_file(Epp, S#state{exports_main = true}, [Form | Forms])
                    end;
                _ ->
                    epp_parse_file(Epp, S, [Form | Forms])
            end;
        {error,{Ln,Mod,Args}} = Form ->
            io:format("~s:~w: ~s\n",
                      [S#state.file,Ln,Mod:format_error(Args)]),
            epp_parse_file(Epp, S#state{n_errors = S#state.n_errors + 1}, [Form | Forms]);
        {eof,LastLine} ->
            S#state{forms_or_bin = lists:reverse([{eof, LastLine} | Forms])}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluate script
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug(Module, AbsMod, Args) ->
    case hidden_apply(debugger, debugger, start, []) of
	{ok, _} ->
	    case hidden_apply(debugger, int, i, [AbsMod]) of
		{module, _} ->
		    hidden_apply(debugger, debugger, auto_attach, [[init]]),
		    run(Module, Args);
		error ->
		    Text = lists:concat(["Cannot load the code for ", Module, " into the debugger"]),
		    fatal(Text)
	    end;
	_ ->
	    fatal("Cannot start the debugger")
    end.

run(Module, Args) ->
    try
        Module:main(Args),
        my_halt(0)
    catch
        Class:Reason ->
            fatal(format_exception(Class, Reason))
    end.

interpret(Forms, File, Args) ->
    Dict  = parse_to_dict(Forms),
    ArgsA = erl_parse:abstract(Args, 0),
    Call = {call,0,{atom,0,main},[ArgsA]},
    try
        erl_eval:expr(Call,
                      erl_eval:new_bindings(),
                      {value,fun(I, J) -> code_handler(I, J, Dict, File) end}),
        my_halt(0)
    catch
        Class:Reason ->
            fatal(format_exception(Class, Reason))
    end.

report_errors(Errors) ->
    lists:foreach(fun ({{F,_L},Eds}) -> list_errors(F, Eds);
                      ({F,Eds}) -> list_errors(F, Eds) end,
                  Errors).

list_errors(F, [{Line,Mod,E}|Es]) ->
    io:fwrite("~s:~w: ~s\n", [F,Line,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(F, [{Mod,E}|Es]) ->
    io:fwrite("~s: ~s\n", [F,Mod:format_error(E)]),
    list_errors(F, Es);
list_errors(_F, []) -> ok.

report_warnings(Ws0) ->
    Ws1 = lists:flatmap(fun({{F,_L},Eds}) -> format_message(F, Eds);
                           ({F,Eds}) -> format_message(F, Eds) end,
                  Ws0),
    Ws = ordsets:from_list(Ws1),
    lists:foreach(fun({_,Str}) -> io:put_chars(Str) end, Ws).

format_message(F, [{Line,Mod,E}|Es]) ->
    M = {{F,Line},io_lib:format("~s:~w: Warning: ~s\n", [F,Line,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(F, [{Mod,E}|Es]) ->
    M = {none,io_lib:format("~s: Warning: ~s\n", [F,Mod:format_error(E)])},
    [M|format_message(F, Es)];
format_message(_, []) -> [].

parse_to_dict(L) -> parse_to_dict(L, dict:new()).

parse_to_dict([{function,_,Name,Arity,Clauses}|T], Dict0) ->
    Dict = dict:store({local, Name,Arity}, Clauses, Dict0),
    parse_to_dict(T, Dict);
parse_to_dict([{attribute,_,import,{Mod,Funcs}}|T], Dict0) ->
    Dict = lists:foldl(fun(I, D) ->
                               dict:store({remote,I}, Mod, D)
                       end, Dict0, Funcs),
    parse_to_dict(T, Dict);
parse_to_dict([_|T], Dict) ->
    parse_to_dict(T, Dict);
parse_to_dict([], Dict) ->
    Dict.

code_handler(local, [file], _, File) ->
    File;
code_handler(Name, Args, Dict, File) ->
    %%io:format("code handler=~p~n",[{Name, Args}]),
    Arity = length(Args),
    case dict:find({local,Name,Arity}, Dict) of
        {ok, Cs} ->
            LF = {value,fun(I, J) -> code_handler(I, J, Dict, File) end},
            case erl_eval:match_clause(Cs, Args,erl_eval:new_bindings(),LF) of
                {Body, Bs} ->
                    eval_exprs(Body, Bs, LF, none, none);
                nomatch ->
                    erlang:error({function_clause,[{local,Name,Args}]})
            end;
        error ->
            case dict:find({remote,{Name,Arity}}, Dict) of
                {ok, Mod} ->
                    %% io:format("Calling:~p~n",[{Mod,Name,Args}]),
                    apply(Mod, Name, Args);
                error ->
                    io:format("Script does not export ~w/~w\n", [Name,Arity]),
                    my_halt(127)
            end
    end.

eval_exprs([E], Bs0, Lf, Ef, _RBs) ->
    RBs1 = value,
    erl_eval:expr(E, Bs0, Lf, Ef, RBs1);
eval_exprs([E|Es], Bs0, Lf, Ef, RBs) ->
    RBs1 = none,
    {value,_V,Bs} = erl_eval:expr(E, Bs0, Lf, Ef, RBs1),
    eval_exprs(Es, Bs, Lf, Ef, RBs).

format_exception(Class, Reason) ->
    PF = fun(Term, I) -> 
                 io_lib:format("~." ++ integer_to_list(I) ++ "P", [Term, 50]) 
         end,
    StackTrace = erlang:get_stacktrace(),
    StackFun = fun(M, _F, _A) -> (M =:= erl_eval) or (M =:= ?MODULE) end,
    lib:format_exception(1, Class, Reason, StackTrace, StackFun, PF).

fatal(Str) ->
    throw(Str).
                                
my_halt(Reason) ->
    case process_info(group_leader(), status) of
        {_,waiting} ->
            %% Now all output data is down in the driver.
            %% Give the driver some extra time before halting.
            receive after 1 -> ok end,
            halt(Reason);
        _ ->
            %% Probably still processing I/O requests.
            erlang:yield(),
            my_halt(Reason)
    end.

hidden_apply(App, M, F, Args) ->
    try
	apply(fun() -> M end(), F, Args)
    catch
	error:undef ->
	    case erlang:get_stacktrace() of
		[{M,F,Args} | _] ->
		    Arity = length(Args),
		    Text = io_lib:format("Call to ~w:~w/~w in application ~w failed.\n",
					 [M, F, Arity, App]),
		    fatal(Text);		    
		Stk ->
		    erlang:raise(error, undef, Stk)
	    end
    end.
