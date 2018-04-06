#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%----------------------------------------------------------------------
%% File    : xml_from_edoc.escript
%%
%% Created : 12 Dec 2009 by Lars Thorsen 
%%----------------------------------------------------------------------


%%======================================================================
%% Records 
%%======================================================================
-record(args, {suffix=".xml",
               dir=".",
	       layout=docgen_edoc_xml_cb,
	       def=[],
	       includes=[],
	       preprocess=false,
	       sort_functions=true}).


%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: main/1
%% Description:
%%----------------------------------------------------------------------
main(RawOpts) ->
    case catch parse(RawOpts, erlref, #args{}) of
	{ok, File, Type, Args} ->
	    case Type of
		erlref ->
		    module(File, Args);
		chapter ->
		    users_guide(File, Args) 
	    end;
	{error, Msg} ->
	    io:format("~p\n", [Msg]),
	    usage()
    end;
main(_) ->
    usage().
        
%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function: usage/0
%% Description:
%%----------------------------------------------------------------------
usage() ->
    io:format("usage:  xml_from_edoc.escript [<options>] <file> \n"),
    halt(1).


%%----------------------------------------------------------------------
%% Function: module/2
%% Description:
%%----------------------------------------------------------------------
module(File, Args) ->
    case filelib:is_regular(File) of
	true ->
	    Opts = [{def,         Args#args.def},
		    {includes,    Args#args.includes},
		    {preprocess,  Args#args.preprocess},
		    {sort_functions, Args#args.sort_functions},
		    
		    {app_default, "OTPROOT"},
		    {file_suffix, Args#args.suffix},
		    {dir,         Args#args.dir},
		    {layout,      Args#args.layout}],
	    edoc:file(File, Opts);
	false ->
	    io:format("~s: not a regular file\n", [File]),
	    usage()
    end.


%%----------------------------------------------------------------------
%% Function: users_guide/2
%% Description:
%%----------------------------------------------------------------------
users_guide(File, Args) ->
    case filelib:is_regular(File) of
	true ->
            Enc = epp:read_encoding(File, [{in_comment_only, false}]),
            Encoding = [{encoding, Enc} || Enc =/= none],
	    Opts = [{def,         Args#args.def},
		    {app_default, "OTPROOT"},
		    {file_suffix, Args#args.suffix},
		    {layout,      Args#args.layout}] ++ Encoding,
	    
	    Env = edoc_lib:get_doc_env(Opts),
	    
	    {ok, Tags} =
		edoc_extract:file(File, overview, Env, Opts),
	    Data =
		edoc_data:overview("Overview", Tags, Env, Opts),
	    F = fun(M) -> M:overview(Data, Opts) end,
	    Text = edoc_lib:run_layout(F, Opts),
	    
	    OutFile = "chapter" ++ Args#args.suffix,
	    edoc_lib:write_file(Text, Args#args.dir, OutFile, Encoding);
	false ->
	    io:format("~s: not a regular file\n", [File]),
	    usage()
    end.



parse(["-xml" |RawOpts], Type, Args) ->
    parse(RawOpts, Type, Args); % default, no update of record necessary
parse(["-sgml" |RawOpts], Type, Args) ->
    parse(RawOpts, Type, Args#args{suffix=".sgml", layout=docgen_edoc_sgml_cb});
parse(["-chapter" |RawOpts], _Type, Args) ->
    parse(RawOpts, chapter, Args);
parse(["-def", Key, Val |RawOpts], Type, Args) ->
    Args2 = Args#args{def=Args#args.def++[{list_to_atom(Key), Val}]},
    parse(RawOpts, Type, Args2);

parse(["-i", Dir |RawOpts], Type, Args) ->
    Args2 = Args#args{includes=Args#args.includes++[Dir]},
    parse(RawOpts, Type, Args2);
parse(["-dir", Dir |RawOpts], Type, Args) ->
    parse(RawOpts, Type, Args#args{dir=Dir});
parse(["-preprocess", Bool |RawOpts], Type, Args) when Bool == "true";
						       Bool == "false" ->
    parse(RawOpts, Type, Args#args{preprocess=list_to_atom(Bool)});
parse(["-sort_functions", Bool |RawOpts], Type, Args) when Bool == "true";
							   Bool == "false" ->
    parse(RawOpts, Type, Args#args{sort_functions=list_to_atom(Bool)});
parse([File], Type, Args) ->
    {ok, File, Type, Args};
parse([Opt | _RawOpts], _Type, _Args) ->
    {error, io_lib:format("Bad option: ~p", [Opt])}.

