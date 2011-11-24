%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
%%
-module(systools_lib).

%% Purpose : Internal stuff called by systools.erl
%%         : Some of this stuff is quite useful and should *eventually* 
%%         : find its way into the standard libraries
%%         

-export([file_term2binary/2, read_term/1, read_term_from_stream/2,
	 get_dirs/1, get_path/1, werror/2]).

-include_lib("kernel/include/file.hrl").

%% reads a single term form a file - convert it to binary and 
%% dump it in a file

file_term2binary(FileIn, FileOut) ->
    case read_term(FileIn) of
	{ok, Term} ->
	    file:write_file(FileOut, term_to_binary(Term)),
	    ok;
	Other ->
	    Other
    end.

%%______________________________________________________________________    
%% read_term(File) -> {ok, Term} | Error
	
read_term(File) ->
    case file:open(File, [read]) of
	{ok, Stream} ->
	    Res = read_term_from_stream(Stream, File),
	    file:close(Stream),
	    Res;
	{error, Error} ->
	    {error, {open,File,Error}}
    end.

read_term_from_stream(Stream, File) ->
    R = io:request(Stream, {get_until,'',erl_scan,tokens,[1]}),
    case R of
	{ok,Toks,_EndLine} ->
	    case erl_parse:parse_term(Toks) of
		{ok, Term} ->
		    {ok, Term};
		{error, Error} ->
		    {error, {parse, File, Error}}
	    end;
	{error,_E,_EndLine} ->
	    {error,{read,File}};
	{eof,_EndLine} ->
	    {error, {read,File}}
    end.

%%% ----------------------------------------------------
%%% Expand a directory name given with wildcards (*) 
%%% to a list of matching directory names.
%%% The only handled wildcard is '*' which is translated
%%% into the regular expression [^/]* .
%%% If '*' is given as only character between two '/'
%%% it is instead translated into the regular expression
%%% [^/]+ , i.e. where must be at least one character 
%%% between two '/'.
%%%
%%% Returns: {ok, Dirs} | {error, What}
%%% ----------------------------------------------------

get_dirs(RegPath) when is_list(RegPath) ->
    Names = filename:split(RegPath),
    ExpNames = expand_names(Names),
    catch get_dirs(ExpNames, [], true);
get_dirs(_) ->
    {error, badarg}.

get_path(RegPath) when is_list(RegPath) ->
    F = fun(RegP) ->
		case get_dirs(RegP) of
		    {ok, Dirs} -> {true, Dirs};
		    _          -> false
		end
	end,
    flat(lists:zf(F, RegPath), []);
get_path(_) ->
    [].

%%
%% expand_names([Name]) -> {true, Name'} | {false, Name}
%%
%% Expand "*" ==> "[^/]+"
%%        "...*..." ==> "[^/]*"
%%
%% A single .../*/... is expanded to one or more whatever
%% except a '/' because it is a place holder for a directory.
%%
expand_names(Names) ->
    lists:map(fun("*") ->
		      {true, "[^/]+"};
		 (N) ->
		      case lists:member($*, N) of
			  true -> {true, expand(N, [])};
			  _    -> {false, N}
		      end
	      end, Names).

expand([$*|T], Ack) ->
    expand(T, "*]/^[" ++ Ack);  %% "[^/]*"
expand([H|T], Ack) ->
    expand(T, [H|Ack]);
expand([], Ack) ->
    lists:reverse(Ack).

%%
%% get_dirs(ExpName, FoundSoFar, Root) -> 
%%    {ok, Dirs} | {error, Error}
%%
%% Use the regular expression RegName to match all
%% directories at a certain level.
%%

get_dirs([{false,Name}|T], F, Root) ->
    get_dirs(T, add_dir(Name, F, Root), false);
get_dirs([{true,RegName}|T], F, Root) ->
    get_dirs(T, add_dirs(RegName, F, Root), false);
get_dirs([], F, _) ->
    {ok, F}.

add_dir(Name, [], true) -> %% root
    case dir_p(Name) of
	true -> [Name];
	_    -> []
    end;
add_dir(Name, Dirs, _Root) ->
    lists:zf(fun(D0) ->
		     D = filename:join(D0, Name),
		     case dir_p(D) of
			 true -> {true, D};
			 _    -> false
		     end
	     end, Dirs).

add_dirs(RegName, _Dirs, true) ->
    case regexp_match(RegName, ".", true) of
	{true, AddDirs} -> AddDirs;
	_               -> []
    end;
add_dirs(RegName, Dirs, Root) ->
    Fun = fun(Dir) ->
		  regexp_match(RegName, Dir, Root)
	  end,
    flat(lists:zf(Fun, Dirs), []).

%%
%% Keep all directories (names) matching RegName and
%% create full directory names Dir ++ "/" ++ Name.
%%
%% Called from lists:zf.
%% Returns: {true, [Dir]} | false
%%
regexp_match(RegName, D0, Root) ->
    case file:list_dir(D0) of
	{ok, Files} when length(Files) > 0 ->
	    case re:compile(RegName) of
		{ok, MP} ->
		    FR = fun(F) ->
				 case re:run(F, MP) of
				     {match,[{0,N}]} when N == length(F) ->
					 DirF = join(D0, F, Root),
					 case dir_p(DirF) of
					     true ->
						 {true, DirF};
					     _ ->
						 false
					 end;
				     _ ->
					 false
				 end
			 end,
		    {true,lists:zf(FR, Files)};
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

%% Only join if not root directory.
join(_, F, true) -> F;
join(Dir, F, _)  -> filename:join(Dir, F).

dir_p(DirF) ->
    case file:read_file_info(DirF) of
	{ok, Info} when Info#file_info.type==directory -> true;
	_                             -> false
    end.
    

flat([H|T], Ack) when is_list(hd(H)) ->
    flat(T, lists:reverse(H) ++ Ack);
flat([[]|T], Ack) ->
    flat(T, Ack);
flat([H|T], Ack) ->
    flat(T, [H|Ack]);
flat([], Ack) ->
    lists:reverse(Ack).

werror(Options, Warnings) ->
    lists:member(warnings_as_errors, Options) andalso Warnings =/= [].

