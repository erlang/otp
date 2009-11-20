%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_gen).

-export([module/1, module/2, users_guide/1, users_guide/2]).

-record(args, {suffix=".xml",
	       layout=docb_edoc_xml_cb,
	       def=[],
	       includes=[],
	       preprocess=false,
	       sort_functions=true}).

%% module(File) -> ok | {error, Reason}
%% module(File, Opts) -> ok | {error, Reason}
%%   File = string(), file name with or without ".erl" extension
%%   Opts -- see code
%%   Reason = badfile | {badopt, Term}
module(File0) ->
    module(File0, []).
module(File0, RawOpts) ->
    File = case filename:extension(File0) of
	       ".erl" -> File0;
	       _ -> File0++".erl"
	   end,
    case filelib:is_regular(File) of
	true ->
	    case parse(RawOpts, #args{}) of
		{ok, Args} ->
		    Opts = [{def,         Args#args.def},
			    {includes,    Args#args.includes},
			    {preprocess,  Args#args.preprocess},
			    {sort_functions, Args#args.sort_functions},

			    {app_default, "OTPROOT"},
			    {file_suffix, Args#args.suffix},
			    {dir,         "."},
			    {layout,      Args#args.layout}],
		    edoc:file(File, Opts);
		Error ->
		    Error
	    end;
	false ->
	    {error, badfile}
    end.

%% users_guide(File) -> ok | {error, Reason}
%% users_guide(File, Opts) -> ok | {error, Reason}
%%   File = string()
%%   Opts -- see code
%%   Reason = badfile | {badopt, Opt}
users_guide(File) ->
    users_guide(File, []).
users_guide(File, RawOpts) ->
    case filelib:is_regular(File) of
	true ->
	    case parse(RawOpts, #args{}) of
		{ok, Args} ->
		    Opts = [{def,         Args#args.def},
			    {app_default, "OTPROOT"},
			    {file_suffix, Args#args.suffix},
			    {layout,      Args#args.layout}],

		    Env = edoc_lib:get_doc_env(Opts),

		    {ok, Tags} =
			edoc_extract:file(File, overview, Env, Opts),
		    Data =
			edoc_data:overview("Overview", Tags, Env, Opts),
		    F = fun(M) -> M:overview(Data, Opts) end,
		    Text = edoc_lib:run_layout(F, Opts),

		    OutFile = "chapter" ++ Args#args.suffix,
		    edoc_lib:write_file(Text, ".", OutFile);
		Error ->
		    Error
	    end;
	false ->
	    {error, badfile}
    end.

parse([{output,xml} | RawOpts], Args) ->
    parse(RawOpts, Args); % default, no update of record necessary
parse([{output,sgml} | RawOpts], Args) ->
    parse(RawOpts, Args#args{suffix=".sgml", layout=docb_edoc_sgml_cb});
parse([{def,Defs} | RawOpts], Args) ->
    case parse_defs(Defs) of
	true ->
	    Args2 = Args#args{def=Args#args.def++Defs},
	    parse(RawOpts, Args2);
	false ->
	    {error, {badopt, {def,Defs}}}
    end;
parse([{includes,Dirs} | RawOpts], Args) ->
    case parse_includes(Dirs) of
	true ->
	    Args2 = Args#args{includes=Args#args.includes++Dirs},
	    parse(RawOpts, Args2);
	false ->
	    {error, {badopt, {includes,Dirs}}}
    end;
parse([{preprocess,Bool} | RawOpts], Args) when Bool==true;
						Bool==false ->
    parse(RawOpts, Args#args{preprocess=Bool});
parse([{sort_functions,Bool} | RawOpts], Args) when Bool==true;
						    Bool==false ->
    parse(RawOpts, Args#args{sort_functions=Bool});
parse([], Args) ->
    {ok, Args};
parse([Opt | _RawOpts], _Args) ->
    {error, {badopt, Opt}}.

parse_defs(Defs) ->
    lists:all(fun({Key,Val}) when is_atom(Key), is_list(Val) -> true;
		 (_) -> false
	      end,
	      Defs).

parse_includes(Dirs) ->
    lists:all(fun(Dir) when is_list(Dir) -> true;
		 (_) -> false
	      end,
	      Dirs).
