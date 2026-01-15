%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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
%%
-module(mod_alias).
-moduledoc """
URL aliasing.

Erlang web server internal API for handling of, for example, interaction data
exported by module `mod_alias`.
""".

-export([do/1, 
	 real_name/3,
	 real_script_name/3,
	 default_index/2,
	 store/2,
	 path/3]).

-include("httpd.hrl").
-include("httpd_internal.hrl").
-include("inets_internal.hrl").

-define(VMODULE,"ALIAS").

%% do

-doc false.
do(#mod{data = Data} = Info) ->
    case proplists:get_value(status, Data) of
	%% A status code has been generated!
	{_StatusCode, _PhraseArgs, _Reason} ->
	    {proceed, Data};
	%% No status code has been generated!
	undefined ->
	    case proplists:get_value(response, Data) of
		%% No response has been generated!
		undefined ->
		    do_alias(Info);
		%% A response has been generated or sent!
		_Response ->
		    {proceed, Data}
	    end
    end.

do_alias(#mod{config_db   = ConfigDB, 
	      request_uri = ReqURI,
	      data        = Data}) ->
    {ShortPath, Path, AfterPath} = 
	real_name(ConfigDB, ReqURI, which_alias(ConfigDB)),
    %% Relocate if a trailing slash is missing else proceed!
    LastChar = lists:last(ShortPath),
    case file:read_file_info(ShortPath) of 
	{ok, FileInfo} when ((FileInfo#file_info.type =:= directory) andalso 
			     (LastChar =/= $/)) ->
            {ReqPath, ReqQuery} = httpd_util:split_path(ReqURI),
            URL = ReqPath ++ "/" ++ ["?" ++ ReqQuery || [] /= ReqQuery],
	    ReasonPhrase = httpd_util:reason_phrase(301),
	    Message = httpd_util:message(301, URL, ConfigDB),
	    {proceed,
	     [{response,
	       {301, ["Location: ", URL, "\r\n"
		      "Content-Type: text/html\r\n",
		      "\r\n",
		      "<HTML>\n<HEAD>\n<TITLE>",ReasonPhrase,
		      "</TITLE>\n</HEAD>\n"
		      "<BODY>\n<H1>",ReasonPhrase,
		      "</H1>\n", Message, 
		      "\n</BODY>\n</HTML>\n"]}}|
	      [{real_name, {Path, AfterPath}} | Data]]};
	_NoFile ->
	    {proceed, [{real_name, {Path, AfterPath}} | Data]}
    end.

%% real_name

-doc """
`real_name/3` traverses `Aliases`, typically extracted from
`ConfigDB`, and matches each `FakeName` with `RequestURI`. If a match is found,
`FakeName` is replaced with `RealName` in the match. The resulting path is split
into two parts, `ShortPath` and `AfterPath`, as defined in
`httpd_util:split_path/1`. `Path` is generated from `ShortPath`, that is, the
result from `default_index/2` with `ShortPath` as
an argument. `config_db()` is the server config file in ETS table format as
described in [Inets User's Guide](http_server.md).
""".
-spec real_name(ConfigDB, RequestURI, Aliases) -> ReturnPath when
      ConfigDB :: ets:tid(),
      RequestURI :: string(),
      Aliases :: [{FakeName, RealName}],
      ReturnPath :: {ShortPath, Path, AfterPath},
      FakeName :: re:mp() | iodata() | unicode:charlist() | string(),
      RealName :: string(),
      ShortPath :: string(),
      Path :: string(),
      AfterPath :: string().
real_name(ConfigDB, RequestURI, []) ->
    {Prefix, DocumentRoot} = which_document_root(ConfigDB), 
    RealName = DocumentRoot ++ RequestURI,
    {ShortPath, _AfterPath} = httpd_util:split_path(RealName),
    {Path, AfterPath} = 
	httpd_util:split_path(default_index(ConfigDB, RealName)),
    {Prefix ++ ShortPath, Prefix ++ Path, AfterPath};
real_name(ConfigDB, RequestURI, [{MP,Replacement}| _] = Aliases)
  when element(1, MP) =:= re_pattern ->
    case longest_match(Aliases, RequestURI) of
	{match, {MP, Replacement}} ->
	    NewURI = re:replace(RequestURI, MP, Replacement, [{return,list}]),
	    {ShortPath,_} = httpd_util:split_path(NewURI),
	    {Path,AfterPath} =
		httpd_util:split_path(default_index(ConfigDB, NewURI)),
	    {ShortPath, Path, AfterPath};
	nomatch ->
	    real_name(ConfigDB, RequestURI, [])
    end;

real_name(ConfigDB, RequestURI,  [{_,_}|_] = Aliases) ->
    case longest_match(Aliases, RequestURI) of
	{match, {FakeName, RealName}} ->
	    ActualName = re:replace(RequestURI, FakeName, RealName, [{return,list}]),
 	    {ShortPath, _AfterPath} = httpd_util:split_path(ActualName),
	    {Path, AfterPath} =
		httpd_util:split_path(default_index(ConfigDB, ActualName)),
	    {ShortPath, Path, AfterPath};
	nomatch ->
	    real_name(ConfigDB, RequestURI, [])
    end.

longest_match(Aliases, RequestURI) ->
    longest_match(Aliases, RequestURI, _LongestNo = 0, _LongestAlias = undefined).

longest_match([{FakeName, RealName} | Rest], RequestURI, LongestNo, LongestAlias) ->
    case re:run(RequestURI, FakeName, [{capture, first}]) of
	{match, [{_, Length}]} ->
	    if
		Length > LongestNo ->
		    longest_match(Rest, RequestURI, Length, {FakeName, RealName});
		true ->
		    longest_match(Rest, RequestURI, LongestNo, LongestAlias)
	    end;
	nomatch ->
	    longest_match(Rest, RequestURI, LongestNo, LongestAlias)
    end;
longest_match([], _RequestURI, 0, _LongestAlias) ->
    nomatch;
longest_match([], _RequestURI, _LongestNo, LongestAlias) ->
    {match, LongestAlias}.

%% real_script_name

-doc """
`real_script_name/3` traverses `ScriptAliases`,
typically extracted from `ConfigDB`, and matches each `FakeName` with
`RequestURI`. If a match is found, `FakeName` is replaced with `RealName` in the
match. If the resulting match is not an executable script, `not_a_script` is
returned. If it is a script, the resulting script path is in two parts,
`ShortPath` and `AfterPath`, as defined in `httpd_util:split_script_path/1`.
`config_db()` is the server config file in ETS table format as described in
[Inets User's Guide](http_server.md).
""".
-spec real_script_name(ConfigDB, RequestURI, ScriptAliases) -> ReturnPath | not_a_script when
      ConfigDB :: ets:tid(),
      RequestURI :: string(),
      ScriptAliases :: list() | [{FakeName, RealName}],
      ReturnPath :: {ShortPath, AfterPath},
      FakeName :: re:mp() | iodata() | unicode:charlist() | string(),
      RealName :: string(),
      ShortPath :: string(),
      AfterPath :: term().
real_script_name(_ConfigDB, _RequestURI, []) ->
    not_a_script;
real_script_name(ConfigDB, RequestURI, [{FakeName,RealName} | Rest]) ->
    case re:run(RequestURI, FakeName, [{capture, none}]) of
	match ->
	    ActualName0 =
		re:replace(RequestURI, FakeName, RealName,  [{return,list}]),
            ActualName = abs_script_path(ConfigDB, ActualName0),
	    httpd_util:split_script_path(default_index(ConfigDB, ActualName));
	nomatch ->
	    real_script_name(ConfigDB, RequestURI, Rest)
    end.

%% ERL-574: relative path in script_alias property results in malformed url
abs_script_path(ConfigDB, [$.|_] = RelPath) ->
    Root = httpd_util:lookup(ConfigDB, server_root),
    Root ++ "/" ++ RelPath;
abs_script_path(_, RelPath) ->
    RelPath.

%% default_index

-doc """
If `Path` is a directory, `default_index/2`, it starts
searching for resources or files that are specified in the config directive
`DirectoryIndex`. If an appropriate resource or file is found, it is appended to
the end of `Path` and then returned. `Path` is returned unaltered if no
appropriate file is found or if `Path` is not a directory. `config_db()` is the
server config file in ETS table format as described in
[Inets User's Guide](http_server.md).

""".
-spec default_index(ConfigDB, Path) -> NewPath when
      ConfigDB :: ets:tid(),
      Path :: string(),
      NewPath :: string().
default_index(ConfigDB, Path) ->
    case file:read_file_info(Path) of
	{ok, FileInfo} when FileInfo#file_info.type =:= directory ->
	    DirectoryIndex = which_directory_index(ConfigDB),
	    append_index(Path, DirectoryIndex);
	_ ->
	    Path
    end.

append_index(RealName, []) ->
    RealName;
append_index(RealName, [Index | Rest]) ->
    case file:read_file_info(filename:join(RealName, Index)) of
	{error, _Reason} ->
	    append_index(RealName, Rest);
	_ ->
	    filename:join(RealName, Index)
    end.

%% path

-doc """
`path/3` returns the file `Path` in the `RequestURI` (see
[RFC 1945](https://www.ietf.org/rfc/rfc1945.txt)). If the interaction data
`{real_name, {Path, AfterPath}}` has been exported by `mod_alias`, `Path` is
returned. If no interaction data has been exported, `ServerRoot` is used to
generate a file `Path`. `config_db()` and `interaction_data()` are as defined in
[Inets User's Guide](http_server.md).
""".
-spec path(Data, ConfigDB, RequestURI) -> Path when
      Data :: [{real_name, {Path, AfterPath}}],
      ConfigDB :: ets:tid(),
      RequestURI :: string(),
      AfterPath :: string(),
      Path :: string().
path(Data, ConfigDB, RequestURI0) ->
    case proplists:get_value(real_name, Data) of
	undefined ->
            {Prefix, DocumentRoot} = which_document_root(ConfigDB),
            RequestURI = percent_decode_path(RequestURI0),
            {Path, _AfterPath} =    
                httpd_util:split_path(DocumentRoot ++ RequestURI),
            Prefix ++ Path;
	{Path, _AfterPath} ->
	    Path
    end.

percent_decode_path(InitPath) ->
    case uri_string:percent_decode(InitPath) of
        {error, _} ->
            InitPath;
        Path0 -> %% Protect against vulnerabilities
            case uri_string:normalize(Path0) of
                {error, _, _} ->
                    InitPath;
                Path ->
                    Path
            end
    end.
%%
%% Configuration
%%
-doc false.
store({directory_index, Value} = Conf, _) when is_list(Value) ->
    case is_directory_index_list(Value) of
	true ->
	    {ok, Conf};
	false ->
	    {error, {wrong_type, {directory_index, Value}}}
    end;
store({directory_index, Value}, _) ->
    {error, {wrong_type, {directory_index, Value}}};
store({alias, {Fake, Real}}, _)
  when is_list(Fake), is_list(Real) ->
    {ok, {alias,{"^"++Fake,Real}}};
store({alias, {MP, _}} = Conf, _)
  when element(1, MP) =:= re_pattern ->
    {ok, Conf};
store({alias, Value}, _) ->
    {error, {wrong_type, {alias, Value}}};
store({re_write, {Re, Replacement}} = Conf, _)
  when is_list(Re), is_list(Replacement) ->
    case re:compile("^"++Re) of
	{ok, MP} ->
	    {ok, {alias, {MP, Replacement}}};
	{error,_} ->
	    {error, {re_compile, Conf}}
    end;
store({re_write, _} = Conf, _) ->
    {error, {wrong_type, Conf}};
store({script_alias, {Fake, Real}}, _)
  when is_list(Fake), is_list(Real) ->
    {ok, {script_alias,{"^"++Fake,Real}}};
store({script_alias, {MP, _}} = Conf, _)
  when element(1, MP) =:= re_pattern ->
    {ok, Conf};
store({script_alias, Value}, _) ->
    {error, {wrong_type, {script_alias, Value}}};
store({script_re_write, {Re, Replacement}} = Conf, _)
  when is_list(Re), is_list(Replacement) ->
    case re:compile("^"++Re) of
	{ok, MP} ->
	    {ok, {script_alias, {MP, Replacement}}};
	{error,_} ->
	    {error, {re_compile, Conf}}
    end;
store({script_re_write, _} = Conf, _) ->
    {error, {wrong_type, Conf}}.

is_directory_index_list([]) ->
    true;
is_directory_index_list([Head | Tail]) when is_list(Head) ->
    is_directory_index_list(Tail);
is_directory_index_list(_) ->
    false.


%% ---------------------------------------------------------------------

which_alias(ConfigDB) ->
    httpd_util:multi_lookup(ConfigDB, alias). 

which_document_root(ConfigDB) ->
    Root = httpd_util:lookup(ConfigDB, document_root, ""),
    case string:tokens(Root, ":") of 
        [Prefix, Path] ->
            {Prefix ++ ":", Path};
        [Path] ->
            {"", Path}
    end.

which_directory_index(ConfigDB) ->
    httpd_util:lookup(ConfigDB, directory_index, []).
