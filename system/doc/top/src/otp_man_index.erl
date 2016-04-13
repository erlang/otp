%%--------------------------------------------------------------------
%%
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
%%
%%-----------------------------------------------------------------
%% File: otp_man_index.erl
%% 
%% Description:
%%    This file generates the module overview in the documentation.
%%
%%-----------------------------------------------------------------

-module(otp_man_index). 

-export([gen/1, gen/2]).
-include_lib("kernel/include/file.hrl").


gen([Source, RootDir, OutFile])  when is_atom(RootDir),  is_atom(OutFile)->
    gen(Source, RootDir, OutFile).

gen(RootDir, OutFile) ->
    gen(rel, RootDir, OutFile).

gen(Source, RootDir, OutFile) ->
    Bases = [{"../lib/", filename:join(RootDir, "lib")},
	     {"../", RootDir}],
    Apps = find_application_paths(Source, Bases),
    RefPages = find_ref_files(Apps),
    gen_html(RefPages, atom_to_list(OutFile)).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Find Reference files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_ref_files(Apps) ->
    find_ref_files(Apps, []).

find_ref_files([], Acc) ->
    Acc;
find_ref_files([{App, Vsn, AppPath, RelPath} |Apps], Acc) ->
    case filelib:wildcard(filename:join(AppPath, "*.html")) of
	[] ->
	    find_ref_files(Apps, Acc);
	Result ->

	    Refs1 = lists:filter(fun(Ref) -> 
				      case file:read_file(Ref) of
					  {ok, Bin} ->
					     case re:run(Bin, ".*<!-- refpage -->.*",[]) of
						 {match, _} ->
						     true;
						 nomatch ->
						     false
					     end;
					 {error, Reason} ->
					      exit(io_lib:format("~p : ~s\n", [Reason, Ref]))
				      end  
			      end,
			      Result),

	    Refs2 = lists:map(fun(Ref) -> 
				      Module = filename:basename(Ref, ".html"),
				      {string:to_lower(Module),
				      Module,
				      App ++ "-" ++ Vsn,
				      RelPath, 
				      filename:join(RelPath, filename:basename(Ref))}
			     end,
			     Refs1),
	    find_ref_files(Apps, Refs2 ++ Acc)
    end.

find_application_paths(_, []) ->
    [];
find_application_paths(Source, [{URL, Dir} | Paths]) ->

    AppDirs = get_app_dirs(Dir),
    AppPaths = get_app_paths(Source, AppDirs, URL),
    AppPaths ++ find_application_paths(Source, Paths).

get_app_paths(src, AppDirs, URL) ->
    Sub1 = "doc/html",
    lists:map(
      fun({App, AppPath}) -> 
	      VsnFile = filename:join(AppPath, "vsn.mk"),
	      VsnStr = 
		  case file:read_file(VsnFile) of
		      {ok, Bin} ->
			  case re:run(Bin, ".*VSN\s*=\s*([0-9\.]+).*",[{capture,[1],list}]) of
			      {match, [V]} ->
				  V;
			      nomatch ->
				  exit(io_lib:format("No VSN variable found in ~s\n",
						     [VsnFile]))
			  end;
		      {error, Reason} ->
			  exit(io_lib:format("~p : ~s\n", [Reason, VsnFile]))
		  end,
	      AppURL = URL ++ App ++ "-" ++ VsnStr,
	      {App, VsnStr, AppPath ++ "/" ++ Sub1, AppURL ++ "/" ++ Sub1}
      end, AppDirs);
get_app_paths(rel, AppDirs, URL) ->
    Sub1 = "doc/html",
    lists:map(
      fun({App, AppPath}) -> 
	      [AppName, VsnStr] = string:tokens(App, "-"),
	      AppURL = URL ++ App,
	      {AppName, VsnStr, AppPath ++ "/" ++ Sub1, AppURL ++ "/" ++ Sub1}
      end, AppDirs).

    
get_app_dirs(Dir) ->
    {ok, Files} = file:list_dir(Dir),
    AFiles = 
	lists:map(fun(File) -> {File, filename:join([Dir, File])} end, Files),
    lists:zf(fun is_app_with_doc/1, AFiles).

is_app_with_doc({"." ++ _ADir, _APath}) ->
    false;
is_app_with_doc({ADir, APath}) ->
    case file:read_file_info(filename:join([APath, "info"])) of
	{ok, _FileInfo} ->
	    {true, {ADir, APath}};
	_ ->
	    false
    end.




gen_html(RefPages, OutFile)->
    case file:open(OutFile, [write]) of
	{ok, Out} ->
	    io:fwrite(Out, "~s\n", [html_header()]),

	    SortedPages = lists:sort(RefPages),
	    
	    lists:foreach(fun({_,Module, App, AppDocDir, RefPagePath}) -> 
				  io:fwrite(Out, "  <TR>\n",[]),
				  io:fwrite(Out, "    <TD><A HREF=\"~s\">~s</A></TD>\n", 
					    [RefPagePath, Module]),
				  io:fwrite(Out, "    <TD><A HREF=\"~s\">~s</A></TD>\n", 
					    [filename:join(AppDocDir, "index.html"), 
					     App]),
				  io:fwrite(Out, "  </TR>\n",[])
			  end, 
			  SortedPages),
	    
	    {Year, _, _} = date(),
	    io:fwrite(Out, "~s\n", [html_footer(integer_to_list(Year))]);
	{error, Reason} ->
	    exit("~p: ~s\n",[Reason, OutFile])
    end.
	  


html_header() ->
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"
	"<!-- This file was generated by the otp_man_index  -->\n"
	"<HTML>\n"
	"<HEAD>\n"
	"  <link rel=\"stylesheet\" href=\"otp_doc.css\" type=\"text/css\"/>\n"
	"  <TITLE>Erlang/OTP Manual Page Index</TITLE>\n"
	"</HEAD>\n"
	"<BODY BGCOLOR=\"#FFFFFF\" TEXT=\"#000000\" LINK=\"#0000FF\" VLINK=\"#FF00FF\" ALINK=\"#FF0000\">\n"
	"<CENTER>\n"
	"<!-- A HREF=\"http://www.erlang.org/\">\n"
	"<img alt=\"Erlang logo\" src=\"erlang-logo.png\"/>\n"
	"</A><BR -->\n"
	"<SMALL>\n"
	"[<A HREF=\"index.html\">Up</A> | <A HREF=\"http://www.erlang.org/\">Erlang</A>]\n"
	"</SMALL><BR>\n"
	"<P/><FONT SIZE=\"+4\">OTP Reference Page Index</FONT><BR>\n"
	"</CENTER>\n"
	"<CENTER>\n"
	"<P/>\n"
	"<TABLE BORDER=1>\n"
	"<TR>\n"
	"  <TH>Manual Page</TH><TH>Application</TH>\n"
	"</TR>\n".



html_footer(Year) ->
    "</TABLE>\n"
	"</CENTER>\n"
	"<P/>\n"
	"<CENTER>\n"
	"<HR/>\n"
	"<SMALL>\n"
	"Copyright &copy; 1991-" ++ Year ++ "\n"
	"<a href=\"http://www.ericsson.com/technology/opensource/erlang/\">\n"
	"Ericsson AB</a>\n"
	"</SMALL>\n"
	"</CENTER>\n"
	"</BODY>\n"
	"</HTML>\n".
