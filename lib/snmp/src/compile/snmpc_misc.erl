%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

-module(snmpc_misc).

%% need definition of mib record
-include("snmp_types.hrl").
-include("snmpc_misc.hrl").


-export([assq/2,
	 bits_to_int/2,
	 ensure_trailing_dir_delimiter/1,
	 foreach/3,
	 is_string/1,
	 read_mib/1,
	 read_noexit/2,
	 strip_extension_from_filename/2,
	 to_upper/1]).


%%--------------------------------------------------
%% Not a real assq, but what the heck, it's useful.
%%--------------------------------------------------
assq(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {Key, Val}} -> {value, Val};
	_ -> false
    end.


%%----------------------------------------------------------------------
%% Converts a list of named bits to the integer value.
%% Returns: integer()|error
%%----------------------------------------------------------------------
bits_to_int(Val,Kibbles) ->
    bits_to_int(Val,Kibbles,0).

bits_to_int([],_Kibbles,Res) -> Res;
bits_to_int([Kibble|Ks],Kibbles,Res) ->
    case snmp_misc:assq(Kibble,Kibbles) of
	{value,V} ->
	    bits_to_int(Ks,Kibbles,Res + round(math:pow(2,V)));
	_ ->
	    error
    end.


ensure_trailing_dir_delimiter([]) -> "/";
ensure_trailing_dir_delimiter(DirSuggestion) ->
    case lists:last(DirSuggestion) of
	$/ -> DirSuggestion;
	_ -> lists:append(DirSuggestion,"/")
    end.


strip_extension_from_filename(FileName, Ext) when is_atom(FileName) ->
    strip_extension_from_filename(atom_to_list(FileName), Ext);

strip_extension_from_filename(FileName, Ext) when is_list(FileName) ->
    case lists:suffix(Ext, FileName) of
	true -> lists:sublist(FileName, 1, length(FileName) - length(Ext));
	false -> FileName
    end.


to_upper([C|Cs]) when (C >= $a) andalso (C =< $z) -> [C-($a-$A)|to_upper(Cs)];
to_upper([C|Cs]) -> [C|to_upper(Cs)];
to_upper([]) -> [].


is_string([]) -> true;
is_string([Tkn | Str]) 
  when is_integer(Tkn) andalso (Tkn >= 0) andalso (Tkn =< 255) ->
    is_string(Str);
is_string(_) -> false.


foreach(Function, ExtraArgs, [H | T]) ->
    apply(Function, [H | ExtraArgs]),
    foreach(Function, ExtraArgs, T);
foreach(_Function, _ExtraArgs, []) -> 
    true.



%%----------------------------------------------------------------------
%% Returns: {ok, Mib}|{error, Reason}
%% The reason for having the function if this module is:
%% The compiler package and the agent package are separated, this is
%% the only common module.
%%----------------------------------------------------------------------
read_mib(FileName) ->
    (catch do_read_mib(FileName)).

do_read_mib(FileName) ->
    ?read_mib(FileName).


%% Ret. {ok, Res} | {error, Line, Error} | {error, open_file}
read_noexit(File, CheckFunc) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    case loop(Fd, [], CheckFunc, 1, File) of
		{error, Line, R} ->
		    file:close(Fd),
		    {error, Line, R};
		Res ->
		    file:close(Fd),
		    {ok, Res}
	    end;
	_Error ->
	    {error, open_file}
    end.


%%-----------------------------------------------------------------
%% Ret: {error, Line, Reason} | Row
%%-----------------------------------------------------------------
loop(Fd, Res, Func, StartLine, File) ->
    case read(Fd, "", StartLine) of
	{ok, Row, EndLine} ->
	    case (catch apply(Func, [Row])) of
		{ok, NewRow} ->
		    loop(Fd, [NewRow | Res], Func, EndLine, File);
		true -> 
		    loop(Fd, [Row | Res], Func, EndLine, File);
		Error ->
		    {error, EndLine, Error}
	    end;
	{error, EndLine, Error} ->
	    {error, EndLine, Error};
	eof ->
	    Res
    end.


%%-----------------------------------------------------------------
%% io:read modified to give us line numbers.
%%-----------------------------------------------------------------
read(Io, Prompt, StartLine) ->
    case io:request(Io, {get_until, Prompt, erl_scan, tokens, [StartLine]}) of
	{ok, Toks, EndLine} ->
	    case erl_parse:parse_term(Toks) of
		{ok, Term} ->
		    {ok, Term, EndLine};
		{error, {Line, erl_parse, Error}} ->
		    {error, Line, {parse_error, Error}}
	    end;
	{error, E, EndLine} ->
	    {error, EndLine, E};
	{eof, _EndLine} ->
	    eof;
	Other ->
	    Other
    end.

