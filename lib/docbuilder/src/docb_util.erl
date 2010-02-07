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
-module(docb_util).  

-export([version/0, old_docb_dir/0, dtd_dir/0]).
-export([html_snippet/2, html_snippet/3]).
-export([lookup_option/2, lookup_option/3, lookup_options/2,
	 an_option/2]).
-export([outfile/3, full_file_name/4]).
-export([message/2, message/3]).
-export([ltrim/1, rtrim/1, trim/1]).
-export([join/2]).
-export([fknidx/2]).

-include("docb_util.hrl").

%%--DocBuilder info-----------------------------------------------------

%% version() -> string()
%% Returns the DocBuilder application version.
version() ->
    DocbDir = code:lib_dir(docbuilder),
    case string:tokens(filename:basename(DocbDir), "-") of
	[_, Vsn] -> Vsn;
	_ -> "unknown"
    end.

%% old_docb_dir() -> string()
%% Returns the root directory of Old_DocBuilder (OTP internal).
old_docb_dir() ->
    "/home/otp/sgml/docb".

%% dtd_dir() -> string()
%% Returns the directory where the XML DTDs are located.
dtd_dir() ->
    DocbDir = code:lib_dir(docbuilder),
    filename:join(DocbDir, "dtd").

%%--User defined HTML snippets------------------------------------------

%% html_snippet(What, Opts) -> HTML
%% html_snippet(What, Arg, Opts) -> HTML
%%   What = head | seealso
%%   HTML = string()
html_snippet(What, Opts) ->
    case lookup_option(html_mod, Opts) of
	false -> "";
	Module ->
	    case catch Module:What() of
		HTML when is_list(HTML) ->
		    HTML;
		{'EXIT', {undef, _}} ->
		    "";
		{'EXIT', Reason} ->
		    message(warning,
			    "Callback function ~p:~p() => ~p",
			    [Module, What, Reason]),
		    "";
		Other ->
		    message(warning,
			    "Callback function ~p:~p() => ~p",
			    [Module, What, Other]),
		    ""
	    end
    end.
html_snippet(What, Arg, Opts) ->
    case lookup_option(html_mod, Opts) of
	false -> "";
	Module ->
	    case catch Module:What(Arg) of
		HTML when is_list(HTML) ->
		    HTML;
		{'EXIT', {undef, _}} ->
		    "";
		{'EXIT', Reason} ->
		    message(warning,
			    "Callback function ~p:~p(~p) => ~p",
			    [Module, What, Arg, Reason]),
		    "";
		Other ->
		    message(warning,
			    "Callback function ~p:~p(~p) => ~p",
			    [Module, What, Arg, Other]),
		    ""
	    end
    end.

%%--Option utilities----------------------------------------------------

%% Opts = [{Opt,Value} | Opt]

%% lookup_option(Opt, Opts) -> Value | false
lookup_option(Opt, Opts) ->
  case lists:keyfind(Opt, 1, Opts) of
      {Opt,Value} -> Value;
      false -> false
  end.

%% lookup_option(Opt, Opts, DefaultValue) -> Value | DefaultValue
lookup_option(Opt, Opts, DefaultValue) ->
    case lookup_option(Opt,Opts) of
	false -> DefaultValue;
	Value -> Value
    end.

%% lookup_options(Opt, Opts) -> [Value]
%% Used when the same option can be defined several times and returns
%% the (possibly empty) list of values.
lookup_options(Opt, Opts) ->
    [V || {O, V} <- Opts, O == Opt].

%% an_option(Opt, Opts) -> bool()
an_option(Opt, Opts) ->
    lists:member(Opt, Opts).

%%--File handling-------------------------------------------------------

%% outfile(File0, Extension, Opts) -> File
%% Build the full filename for where to place a resulting file.
outfile(File0, Extension, Opts) ->
    File =
	case regexp:match(File0, "[^/]*\$") of
	    {match,Start,Length} ->
		string:substr(File0, Start, Length);
	    _ ->
		File0
	end,
    full_file_name(File, Extension, outdir, Opts).

%% full_file_name(File, Extension, What, Opts) -> File'
%%   File = string()
%%   What = outdir | includepath
%% Prepend the full path name.
full_file_name(File, Extension, What, Opts) ->
    Path = lookup_option(What, Opts, ""),
    full_file_name(File, Extension, Path).

full_file_name(File0, Extension, Path) ->
    File = case filename:extension(File0) of
	       Extension -> File0;
	       _ -> File0++Extension
	   end,

    case File of
	[$/|_] -> File;
	[$~|_] -> File;
	_ when Path=/="" -> filename:join(Path, File);
	_ -> File
    end.

%%--Messages to the user------------------------------------------------

%% message(Class, Format)
%% message(Class, Format, Values) -> ok
%%   Class = info | warning | error
%%   Format, Values -- as in io:format/2
%% Prints a warning or error message.
%% Call as util:message(warning, "~w is undefined", [foo]).
message(Class, Format) ->
    message(Class, Format, []).
message(Class, Format, Values) ->
    Prefix = case Class of
		 info -> "";
		 warning -> "*** Warning: ";
		 error -> "*** Error: "
	    end,
    case get(option_silent) of
	true when Class==warning ->
	    ok;
	_ ->
	    io:format(Prefix, []),
	    io:format(Format, Values),
	    io:nl()
    end.

%%--String handling-----------------------------------------------------

%% ltrim(Str) -> Str'
%% rtrim(Str) -> Str'
%% trim(Str) -> Str'
%% Strips whitespace from left, right or both.
ltrim(Str) ->
    lists:dropwhile(fun white_space/1, Str).
rtrim(Str) ->
    lists:reverse(ltrim(lists:reverse(Str))).
trim(Str) ->
    rtrim(ltrim(Str)).

white_space($ ) -> true;
white_space(C) when C<$  -> true;
white_space($\n) -> true;
white_space($\t) -> true;
white_space(_) -> false.

%% join(Strings, With) -> string()
join([H1, H2| T], S) ->
        H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].

%%--Other---------------------------------------------------------------

%% fknidx(FNdef0, Fn_arity_sep) -> string()
%% Get me the function name and arity.
fknidx(FNdef0, Fn_arity_sep) ->
    FNdef = string:strip(FNdef0),
    case string:tokens(FNdef,"(") of
	[FNdef] ->
	    %% No parentheses, assume variable: remove nl:s at end,
	    %% and strip blanks.
	    string:strip(string:strip(FNdef, right, $\n));
	[Name0|Args0] ->
	    [Args1|_] = string:tokens(string:strip(hd(Args0)), "-"),
	    Arity = case Args1 of
			[$)|_] -> 0;
			_ ->
			    length(string:tokens(Args1, ","))
		    end,
	    string:strip(Name0)++Fn_arity_sep++integer_to_list(Arity)
    end.
