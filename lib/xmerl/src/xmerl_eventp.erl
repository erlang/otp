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

%%% Description  : Simple event-based processor (front-end to xmerl_scan)

%% @doc Simple event-based front-ends to xmerl_scan for processing
%% of XML documents in streams and for parsing in SAX style.
%% Each contain more elaborate settings of xmerl_scan that makes usage of
%% the customization functions.
%% 
%% @type xmlElement() = #xmlElement{}.
%%
%% @type option_list(). <p>Options allow to customize the behaviour of the
%%     scanner.
%% See also <a href="xmerl_examples.html">tutorial</a> on customization
%% functions.
%% </p>
%% <p>
%% Possible options are:
%% </p>
%% <dl>
%%  <dt><code>{acc_fun, Fun}</code></dt>
%%    <dd>Call back function to accumulate contents of entity.</dd>
%%  <dt><code>{continuation_fun, Fun} |
%%            {continuation_fun, Fun, ContinuationState}</code></dt>
%%    <dd>Call back function to decide what to do if the scanner runs into EOF
%%     before the document is complete.</dd>
%%  <dt><code>{event_fun, Fun} |
%%            {event_fun, Fun, EventState}</code></dt>
%%    <dd>Call back function to handle scanner events.</dd>
%%  <dt><code>{fetch_fun, Fun} |
%%            {fetch_fun, Fun, FetchState}</code></dt>
%%    <dd>Call back function to fetch an external resource.</dd>
%%  <dt><code>{hook_fun, Fun} |
%%            {hook_fun, Fun, HookState}</code></dt>
%%    <dd>Call back function to process the document entities once
%%     identified.</dd>
%%  <dt><code>{close_fun, Fun}</code></dt>
%%    <dd>Called when document has been completely parsed.</dd>
%%  <dt><code>{rules, ReadFun, WriteFun, RulesState} |
%%            {rules, Rules}</code></dt>
%%    <dd>Handles storing of scanner information when parsing.</dd>
%%  <dt><code>{user_state, UserState}</code></dt>
%%    <dd>Global state variable accessible from all customization functions</dd>
%%
%%  <dt><code>{fetch_path, PathList}</code></dt>
%%    <dd>PathList is a list of
%%     directories to search when fetching files. If the file in question
%%     is not in the fetch_path, the URI will be used as a file
%%     name.</dd>
%%  <dt><code>{space, Flag}</code></dt>
%%    <dd>'preserve' (default) to preserve spaces, 'normalize' to
%%    accumulate consecutive whitespace and replace it with one space.</dd>
%%  <dt><code>{line, Line}</code></dt>
%%    <dd>To specify starting line for scanning in document which contains
%%    fragments of XML.</dd>
%%  <dt><code>{namespace_conformant, Flag}</code></dt>
%%    <dd>Controls whether to behave as a namespace conformant XML parser,
%%    'false' (default) to not otherwise 'true'.</dd>
%%  <dt><code>{validation, Flag}</code></dt>
%%    <dd>Controls whether to process as a validating XML parser:
%%    'off' (default) no validation, or validation 'dtd' by DTD or 'schema'
%%    by XML Schema. 'false' and 'true' options are obsolete
%%    (i.e. they may be removed in a future release), if used 'false'
%%    equals 'off' and 'true' equals 'dtd'.</dd>
%%  <dt><code>{schemaLocation, [{Namespace,Link}|...]}</code></dt>
%%    <dd>Tells explicitly which XML Schema documents to use to validate
%%    the XML document. Used together with the
%%    <code>{validation,schema}</code> option.</dd>
%%  <dt><code>{quiet, Flag}</code></dt>
%%    <dd>Set to 'true' if xmerl should behave quietly and not output any
%%    information to standard output (default 'false').</dd>
%%  <dt><code>{doctype_DTD, DTD}</code></dt>
%%    <dd>Allows to specify DTD name when it isn't available in the XML
%%    document. This option has effect only together with
%%    <code>{validation,'dtd'</code> option.</dd>
%%  <dt><code>{xmlbase, Dir}</code></dt>
%%    <dd>XML Base directory. If using string/1 default is current directory.
%%    If using file/1 default is directory of given file.</dd>
%%  <dt><code>{encoding, Enc}</code></dt>
%%    <dd>Set default character set used (default UTF-8).
%%    This character set is used only if not explicitly given by the XML
%%    declaration. </dd>
%%  <dt><code>{document, Flag}</code></dt>
%%    <dd>Set to 'true' if xmerl should return a complete XML document
%%    as an xmlDocument record (default 'false').</dd>
%%  <dt><code>{comments, Flag}</code></dt>
%%    <dd>Set to 'false' if xmerl should skip comments otherwise they will
%%    be returned as xmlComment records (default 'true').</dd>
%%  <dt><code>{default_attrs, Flag}</code></dt>
%%    <dd>Set to 'true' if xmerl should add to elements missing attributes
%%    with a defined default value (default 'false').</dd>
%% </dl> 
%% 
-module(xmerl_eventp).
-vsn('0.19').
-date('03-09-17').

-export([stream/2,stream_sax/4, file_sax/4, string_sax/4]).

% -export([cont/3, rules_read/3,rules_write/4,fetch/2,close/1]).

-include("xmerl.hrl").
-include("xmerl_internal.hrl").
-include_lib("kernel/include/file.hrl").

%% @spec stream(Fname::string(), Options::option_list()) -> xmlElement()
%%
%% @doc Parse file containing an XML document as a stream, DOM style.
%% Wrapper for a call to the XML parser <code>xmerl_scan</code> with a
%% <code>continuation_fun</code> for handling streams of XML data.
%% Note that the <code>continuation_fun</code>, <code>acc_fun</code>,
%% <code>fetch_fun</code>, <code>rules</code> and <code>close_fun</code>
%% options cannot be user defined using this parser.
stream(Fname, Options) ->
    AccF = fun(X, Acc, S) -> acc(X,Acc,S) end,
    case file:open(Fname, [read, raw, binary]) of
	{ok, Fd} ->
	    B0 = list_to_binary([]),
	    ContS = [{B0, Fname, Fd}],
	    Opts=scanner_options(Options,
				 [{continuation_fun, fun cont/3, ContS},
				  {acc_fun, AccF},
				  {fetch_fun, fun fetch/2},
				  {rules,fun rules_read/3,fun rules_write/4,""},
				  {close_fun, fun close/1}]),
	    xmerl_scan:string([], Opts);
	Error ->
	    Error
    end.


%% @spec stream_sax(Fname,CallBackModule,UserState,Options) -> xmlElement()
%%       Fname = string()
%%       CallBackModule = atom()
%%       Options = option_list()
%%
%% @doc Parse file containing an XML document as a stream, SAX style.
%% Wrapper for a call to the XML parser <code>xmerl_scan</code> with a
%% <code>continuation_fun</code> for handling streams of XML data.
%% Note that the <code>continuation_fun</code>, <code>acc_fun</code>,
%% <code>fetch_fun</code>, <code>rules</code>, <code>hook_fun</code>,
%% <code>close_fun</code> and <code>user_state</code> options cannot be user
%% defined using this parser.
stream_sax(Fname, CallBack, UserState,Options) ->
    US={xmerl:callbacks(CallBack), UserState},
    AccF = fun(X, Acc, S) -> acc(X,Acc,S) end,
    HookF=
	fun(ParsedEntity, S) ->
		{CBs,Arg}=xmerl_scan:user_state(S),
%		?dbg("stream_sax Arg=~p~n",[Arg]),
		case ParsedEntity of
		    #xmlComment{} -> % Toss away comments...
			{[],S};
		    _ ->  % Use callback module for the rest
%		?dbg("stream_sax ParsedEntity=~p~n",[ParsedEntity]),
			case xmerl:export_element(ParsedEntity,CBs,Arg) of
			    {error,Reason} ->
				throw({error,Reason});
			    Resp ->
%		?dbg("stream_sax Resp=~p~n",[Resp]),
				{Resp,xmerl_scan:user_state({CBs,Resp},S)}
			end
		end
	end,
    case file:open(Fname, [read, raw, binary]) of
	{ok, Fd} ->
	    B0 = list_to_binary([]),
	    ContS = [{B0, Fname, Fd}],
	    Opts=scanner_options(Options,
				 [{acc_fun, AccF},
				  {close_fun, fun close/1},
				  {continuation_fun, fun cont/3, ContS},
				  {fetch_fun, fun fetch/2},
				  {hook_fun,HookF},
				  {rules,fun rules_read/3,fun rules_write/4,""},
				  {user_state,US}]),
	    xmerl_scan:string([], Opts);
	Error ->
	    Error
    end.


%% @spec file_sax(Fname::string(), CallBackModule::atom(), UserState,
%%        Options::option_list()) -> NewUserState
%%
%% @doc Parse file containing an XML document, SAX style.
%% Wrapper for a call to the XML parser <code>xmerl_scan</code> with a
%% <code>hook_fun</code> for using xmerl export functionality directly after
%% an entity is parsed.
file_sax(Fname,CallBack, UserState, Options) ->
    US={xmerl:callbacks(CallBack), UserState},
    AccF=fun(X,Acc,S) -> {[X|Acc], S} end,
    HookF=
	fun(ParsedEntity, S) ->
		{CBs,Arg}=xmerl_scan:user_state(S),
		case ParsedEntity of
		    #xmlComment{} -> % Toss away comments...
			{[],S};
		    _ ->  % Use callback module for the rest
			case xmerl:export_element(ParsedEntity,CBs,Arg) of
			    {error,Reason} ->
				throw({error,Reason});
			    Resp ->
				{Resp,xmerl_scan:user_state({CBs,Resp},S)}
			end
		end
	end,
    
    Opts=scanner_options(Options,[{acc_fun, AccF},
				  {hook_fun,HookF},
				  {user_state,US}]),
    xmerl_scan:file(Fname,Opts).


%% @spec string_sax(String::list(), CallBackModule::atom(), UserState,
%%        Options::option_list()) ->
%%      xmlElement()
%%
%% @doc Parse file containing an XML document, SAX style.
%% Wrapper for a call to the XML parser <code>xmerl_scan</code> with a
%% <code>hook_fun</code> for using xmerl export functionality directly after
%% an entity is parsed.
string_sax(String,CallBack, UserState, Options) ->
    US={xmerl:callbacks(CallBack), UserState},
    AccF=fun(X,Acc,S) -> {[X|Acc], S} end,
    HookF=
	fun(ParsedEntity, S) ->
		{CBs,Arg}=xmerl_scan:user_state(S),
		case ParsedEntity of
		    #xmlComment{} -> % Toss away comments...
			{[],S};
		    _ ->  % Use callback module for the rest
			case xmerl:export_element(ParsedEntity,CBs,Arg) of
			    {error,Reason} ->
				throw({error,Reason});
			    Resp ->
				{Resp,xmerl_scan:user_state({CBs,Resp},S)}
			end
		end
	end,
    
    Opts=scanner_options(Options,[{acc_fun, AccF},
				  {hook_fun,HookF},
				  {user_state,US}]),
    xmerl_scan:string(String,Opts).



%%% ----------------------------------------------------------------------------
%%% Streaming support functions

%%% Continuation callback function for xmerl_scan
cont(F, Exception, S) ->
    case xmerl_scan:cont_state(S) of
	[{_Fname, eof}|_] ->
	    Exception(S);
	[{Sofar, Fname, Fd}|T] ->
	    cont2(F, Exception, Sofar, Fd, Fname, T, S)
    end.


cont2(F, Exception, Sofar, Fd, Fname, T, S) ->
    case catch read_chunk(Fd, Fname, Sofar) of
	{ok, Bin} ->
	    find_good_split(list_to_binary([Sofar,Bin]),
			    F,Exception,Fd,Fname,T,S);
	eof ->
	    ok = file:close(Fd),
	    NewS = xmerl_scan:cont_state([{Fname, eof}|T], S),
	    F(binary_to_list(Sofar), NewS);
	Error ->
	    exit(Error)
    end.
    
read_chunk(Fd, _Fname, _Sofar) ->
    file:read(Fd, 8192).

-ifndef(no_bitsyntax).

find_good_split(Bin, F, Exception, Fd, Fname, T, S) ->
    find_good_split(size(Bin)-1, Bin, F, Exception, Fd, Fname, T, S).

find_good_split(0, B, F, Exception, Fd, Fname, T, S) ->
    cont2(F, Exception, B, Fd, Fname, T, S);
find_good_split(Size, B, F, Exception, Fd, Fname, T, S) ->
    case B of
	<<_Bytes:Size/binary, H/integer, Tail/binary>> when ?whitespace(H) ->
	    {SubB,_} = split_binary(B, Size+1),
	    NewS = xmerl_scan:cont_state([{Tail, Fname, Fd}|T], S),
	    F(binary_to_list(SubB), NewS);
	_ ->
	    find_good_split(Size-1, B, F, Exception, Fd, Fname, T, S)
    end.

-else.

find_good_split(Bin, F, Exception, Fd, Fname, T, S) ->
    find_good_split(size(Bin), Bin, F, Exception, Fd, Fname, T, S).

find_good_split(0, B, F, Exception, Fd, Fname, T, S) ->
    cont2(F, Exception, B, Fd, Fname, T, S);
find_good_split(Size, B, F, Exception, Fd, Fname, T, S) ->
    case binary_to_list(B, Size, Size) of
	[H] when ?whitespace(H) ->
	    {SubB,Tail} = split_binary(B, Size),
	    NewS = xmerl_scan:cont_state([{Tail, Fname, Fd}|T], S),
	    F(binary_to_list(SubB), NewS);
	_ ->
	    find_good_split(Size-1, B, F, Exception, Fd, Fname, T, S)
    end.

-endif.



%%% Accumulator callback function for xmerl_scan
acc(X = #xmlText{value = Text}, Acc, S) ->
    case detect_nul_text(Text) of 
	ok->
	    {[X#xmlText{value = lists:flatten(Text)}|Acc], S};
   	nok->
	    {Acc,S}
    end;
acc(X, Acc, S) ->
    {[X|Acc], S}.

%%% don't acc xmlText when text contains only " " , "\n" and "\t".
detect_nul_text([H|T]) when H==10; H==32; H==9->
    detect_nul_text(T);
detect_nul_text([]) ->
    nok;
detect_nul_text(_)->
    ok.



%%% Fetch callback function for xmerl_scan
fetch({system, URI}, S) ->
    fetch_URI(URI, S);
fetch({public, _PublicID, URI}, S) ->
    fetch_URI(URI, S).

fetch_URI(URI, S) ->
    %% assume URI is a filename
    Split = filename:split(URI),
    Filename = lists:last(Split),
    Fullname = 
	case Split of
	    ["/", _|_] ->
		%% absolute path name
		URI;
	    ["file:",Name]->
		%% file:/dtd_name 
		filename:join(S#xmerl_scanner.xmlbase, Name);
	    _ ->
		filename:join(S#xmerl_scanner.xmlbase, URI)
	end,
    File = path_locate(S#xmerl_scanner.fetch_path, Filename, Fullname),
    ?dbg("fetch(~p) -> {file, ~p}.~n", [URI, File]),
    case file:open(File, [read, raw, binary]) of
	{ok, Fd} ->
	    ContS=xmerl_scan:cont_state(S),
	    NewS=xmerl_scan:cont_state([{list_to_binary([]),File,Fd}|ContS],S),
	    {ok, {string, []}, NewS};
	_Error ->
	    ?dbg("ERROR fetch(~p) -> ~p~n", [URI, _Error]),
	    {ok, not_fetched, S}
    end.

path_locate([Dir|Dirs], FN, FullName) ->
    F = filename:join(Dir, FN),
    case file:read_file_info(F) of
	{ok, #file_info{type = regular}} ->
	    F;
	_ ->
	    path_locate(Dirs, FN, FullName)
    end;
path_locate([], _FN, FullName) ->
    FullName.

%%% Close callback function for xmerl_scan
close(S) ->
    ContS = xmerl_scan:cont_state(S),
    case ContS of
	[{_Fname, eof}|T] ->
	    xmerl_scan:cont_state(T, S);
	[{_Sofar, _Fname, Fd}|T] ->
	    ok = file:close(Fd),
	    xmerl_scan:cont_state(T, S)
    end.


%%% Rules callback functions for xmerl_scan
rules_write(Context, Name, Value, #xmerl_scanner{rules = undefined}=S) ->
    Tab = ets:new(rules, [set, public]),
    rules_write(Context, Name, Value, S#xmerl_scanner{rules = Tab});
rules_write(Context, Name, Value, #xmerl_scanner{rules = T} = S) ->
    ets:insert(T, {{Context, Name}, Value}),
    S.

rules_read(_Context, _Name, #xmerl_scanner{rules = undefined}) ->
    undefined;
rules_read(Context, Name, #xmerl_scanner{rules = T}) ->
    case ets:lookup(T, {Context, Name}) of
	[] ->
	    undefined;
	[{_K, V}] ->
	    V
    end.



%%% ----------------------------------------------------------------------------
%%% Generic helper functions

scanner_options([H|T], Opts) ->
    case catch keyreplace(H, 1, Opts) of
	false ->
	    scanner_options(T, [H|Opts]);
	NewOpts ->
	    scanner_options(T, NewOpts)
    end;
scanner_options([], Opts) ->
    Opts.

keyreplace(X, Pos, [H|T]) when element(Pos, X) == element(Pos, H) ->
    [X|T];
keyreplace(X, Pos, [H|T]) ->
    [H|keyreplace(X, Pos, T)];
keyreplace(_, _Pos, []) ->
    throw(false).


