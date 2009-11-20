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
-module(docb_transform).

-export([file/1, file/2]).

%% file(File) -> ok | {error, Reason}
%% file(File, Opts) -> ok | {error, Reason}
%%   File = string(), file name with or without ".xml" extension
%%   Opts = [Opt]
%%   Reason = badfile | {badopt, Term}
file(File0) ->
    file(File0, []).
file(File0, RawOpts) ->
    File = filename:rootname(File0), % without extension
    Ext = case filename:extension(File0) of
	      ".xml" -> ".xml";
	      ".sgml" -> ".sgml";
	      "" ->
		  %% If the file is given without extension, we try to
		  %% infer if the source file is XML or SGML.
		  %% SGML is supported *internally within OTP* for
		  %% backwards compatibility reasons.
		  case filelib:is_regular(File++".xml") of
		      true -> ".xml";
		      false -> ".sgml"
		  end;
	      _Ext0 -> % this is probably an error...
		  ".xml"
	  end,
    case filelib:is_regular(File++Ext) of
	true ->
	    case parse(RawOpts) of
		{ok, Opts0} ->
		    {ok, Cwd} = file:get_cwd(),
		    Opts = [{src_type,Ext},
			    {src_dir,Cwd},
			    {src_file,File},
			    {{local_defs,term},[]},
			    {{local_defs,cite},[]} | Opts0],
		    case docb_main:process(File, Opts) of
			errors -> error;
			ok -> ok
		    end;
		Error -> % {error, {badopt,Term}}
		    Error
	    end;
	false ->
	    {error, badfile}
    end.

parse(RawOpts) ->
    parse(RawOpts, []).

%% Officially supported options

parse([{html_mod,Module} | RawOpts], Opts) when is_atom(Module) ->
    parse(RawOpts, [{html_mod,Module} | Opts]);
parse([{outdir,Dir} | RawOpts], Opts) when is_list(Dir) ->
    parse(RawOpts, [{outdir,Dir} | Opts]);
parse([{number,N} | RawOpts], Opts) when is_integer(N) ->
    parse(RawOpts, [{number,integer_to_list(N)} | Opts]);
parse([{number,Nstr} | RawOpts], Opts) -> % list when called from script
    parse(RawOpts, [{number,Nstr} | Opts]);
parse([{ptype,Type} | RawOpts], Opts) when Type==unix;
					   Type==windows ->
    parse(RawOpts, [{ptype,atom_to_list(Type)} | Opts]);
parse([{ptype,Type} | RawOpts], Opts) -> % list when called from script
    parse(RawOpts, [{ptype,Type} | Opts]);
parse([silent | RawOpts], Opts) ->
    put(option_silent, true),
    parse(RawOpts, [silent | Opts]);
parse([{top,Index} | RawOpts], Opts) when is_list(Index) ->
    parse(RawOpts, [{top,Index} | Opts]);
parse([{vsn,Vsn} | RawOpts], Opts) when is_list(Vsn) ->
    parse(RawOpts, [{vsn,Vsn} | Opts]);

parse([{term_defs,File} | RawOpts], Opts) when is_list(File) ->
    Opts2 = get_defs(term, File, Opts),
    parse(RawOpts, Opts2);
parse([{cite_defs,File} | RawOpts], Opts) when is_list(File) ->
    Opts2 = get_defs(cite, File, Opts),
    parse(RawOpts, Opts2);

%% OTP internal options (SGML and PDF support etc.)

parse([html | RawOpts], Opts) ->
    parse(RawOpts, [html | Opts]);
parse([latex | RawOpts], Opts) ->
    parse(RawOpts, [latex | Opts]);
parse([{man,Level} | RawOpts], Opts) -> % Level = 1..9
    parse(RawOpts, [{man,Level} | Opts]);

parse([{booksty,StyFile} | RawOpts], Opts) -> % "otpA4" | "otpBOOK"
    parse(RawOpts, [{booksty,StyFile} | Opts]);
parse([{includepath,Dir} | RawOpts], Opts) ->
    parse(RawOpts, [{includepath,Dir} | Opts]);
parse([showpaths | RawOpts], Opts) ->
    parse(RawOpts, [showpaths | Opts]);
parse([straight | RawOpts], Opts) ->
    parse(RawOpts, [straight | Opts]);
parse([{ent,Ent} | RawOpts], Opts) ->
    parse(RawOpts, [{ent,Ent} | Opts]);

%% Undocumented options

parse([{name, Name} | RawOpts], Opts) ->
    parse(RawOpts, [{name, Name} | Opts]);
parse([framework_only | RawOpts], Opts) ->
    parse(RawOpts, [framework_only | Opts]);
parse([kwicindex_only | RawOpts], Opts) ->
    parse(RawOpts, [kwicindex_only | Opts]);

parse([], Opts) ->
    {ok, Opts};
parse([Opt | _RawOpts], _Opts) ->
    {error, {badopt, Opt}}.

%% Type = term | cite
get_defs(Type, File, Opts) ->
    Key = {defs,Type},
    {PrevDefs, Opts2} =
	case lists:keysearch(Key, 1, Opts) of
	    {value, {_, Defs0}} ->
		{Defs0, lists:keydelete(Key, 1, Opts)};
	    false ->
		{[], Opts}
	end,
    NewDefs = case file:consult(File) of
		  {ok, [DefL]} when is_list(DefL) ->
		      DefL;
		  {ok, _Terms} ->
		      docb_util:message(error,
					"Skipping defs file ~s, does "
					"not contain one list", [File]),
		      [];
		  {error, Error} ->
		      Expl = lists:flatten(file:format_error(Error)),
		      docb_util:message(error,
					"Skipping defs file ~s, ~s",
					[File, Expl]),
		      []
	      end,
    [{Key,PrevDefs++NewDefs} | Opts2].
	    
    
