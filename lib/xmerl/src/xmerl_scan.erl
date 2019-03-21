%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

%% Description  : Simgle-pass XML scanner. See xmerl.hrl for data defs.

%% @doc This module is the interface to the XML parser, it handles XML 1.0.
%%     The XML parser is activated through
%%     <tt>xmerl_scan:string/[1,2]</tt> or
%%     <tt>xmerl_scan:file/[1,2]</tt>.
%%     It returns records of the type defined in xmerl.hrl.
%% See also <a href="xmerl_examples.html">tutorial</a> on customization
%% functions.
%% @type global_state(). <p>
%% The global state of the scanner, represented by the #xmerl_scanner{} record.
%% </p>
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
%% @type xmlElement() = #xmlElement{}.
%% The record definition is found in xmerl.hrl.
%% @type xmlDocument() = #xmlDocument{}.
%% The record definition is found in xmerl.hrl.
%% @type document() = xmlElement() | xmlDocument(). <p>
%% The document returned by <tt>xmerl_scan:string/[1,2]</tt> and
%% <tt>xmerl_scan:file/[1,2]</tt>. The type of the returned record depends on
%% the value of the document option passed to the function.
%% </p>

-module(xmerl_scan).
-vsn('0.20').
-date('03-09-16').

%% main API
-export([string/1, string/2,
	 file/1, file/2]).

%% access functions for various states
-export([user_state/1, user_state/2,
	 event_state/1, event_state/2,
	 hook_state/1, hook_state/2,
	 rules_state/1, rules_state/2,
	 fetch_state/1, fetch_state/2,
	 cont_state/1, cont_state/2]).

%% helper functions. To xmerl_lib ??
-export([accumulate_whitespace/4]).

%-define(debug, 1).
-include("xmerl.hrl").		% record def, macros
-include("xmerl_internal.hrl").
-include_lib("kernel/include/file.hrl").


-define(fatal(Reason, S),
	if
	    S#xmerl_scanner.quiet ->
		ok;
	    true ->
		error_logger:error_msg("~p- fatal: ~p~n", [?LINE, Reason]),
		ok
	end,
	fatal(Reason, S)).


-define(ustate(U, S), S#xmerl_scanner{user_state = U}).


%% Functions to access the various states

%%% @spec user_state(S::global_state()) -> global_state()
%%% @equiv user_state(UserState,S)
user_state(#xmerl_scanner{user_state = S}) -> S.

%%% @spec event_state(S::global_state()) -> global_state()
%%% @equiv event_state(EventState,S)
event_state(#xmerl_scanner{fun_states = #xmerl_fun_states{event = S}}) -> S.

%%% @spec hook_state(S::global_state()) -> global_state()
%%% @equiv hook_state(HookState,S)
hook_state(#xmerl_scanner{fun_states = #xmerl_fun_states{hook = S}}) -> S.

%%% @spec rules_state(S::global_state()) -> global_state()
%%% @equiv rules_state(RulesState,S)
rules_state(#xmerl_scanner{fun_states = #xmerl_fun_states{rules = S}}) -> S.

%%% @spec fetch_state(S::global_state()) -> global_state()
%%% @equiv fetch_state(FetchState,S)
fetch_state(#xmerl_scanner{fun_states = #xmerl_fun_states{fetch = S}}) -> S.

%%% @spec cont_state(S::global_state()) -> global_state()
%%% @equiv cont_state(ContinuationState,S)
cont_state(#xmerl_scanner{fun_states = #xmerl_fun_states{cont = S}}) -> S.


%%%% Functions to modify the various states

%%% @spec user_state(UserState, S::global_state()) -> global_state()
%%% @doc For controlling the UserState, to be used in a user function.
%%% See <a href="xmerl_examples.html">tutorial</a> on customization functions.
user_state(X, S) ->
    S#xmerl_scanner{user_state = X}.

%%% @spec event_state(EventState, S::global_state()) -> global_state()
%%% @doc For controlling the EventState, to be used in an event
%%% function, and called at the beginning and at the end of a parsed entity.
%%% See <a href="xmerl_examples.html">tutorial</a> on customization functions.
event_state(X, S=#xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{event = X},
    S#xmerl_scanner{fun_states = FS1}.

%%% @spec hook_state(HookState, S::global_state()) -> global_state()
%%% @doc For controlling the HookState, to be used in a hook
%%% function, and called when the parser has parsed a complete entity.
%%% See <a href="xmerl_examples.html">tutorial</a> on customization functions.
hook_state(X, S=#xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{hook = X},
    S#xmerl_scanner{fun_states = FS1}.

%%% @spec rules_state(RulesState, S::global_state()) -> global_state()
%%% @doc For controlling the RulesState, to be used in a rules
%%% function, and called when the parser store scanner information in a rules
%%% database.
%%% See <a href="xmerl_examples.html">tutorial</a> on customization functions.
rules_state(X, S=#xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{rules = X},
    S#xmerl_scanner{fun_states = FS1}.

%%% @spec fetch_state(FetchState, S::global_state()) -> global_state()
%%% @doc For controlling the FetchState, to be used in a fetch
%%% function, and called when the parser fetch an external resource (eg. a DTD).
%%% See <a href="xmerl_examples.html">tutorial</a> on customization functions.
fetch_state(X, S=#xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{fetch = X},
    S#xmerl_scanner{fun_states = FS1}.

%%% @spec cont_state(ContinuationState, S::global_state()) -> global_state()
%%% @doc For controlling the ContinuationState, to be used in a continuation
%%% function, and called when the parser encounters the end of the byte stream.
%%% See <a href="xmerl_examples.html">tutorial</a> on customization functions.
cont_state(X, S=#xmerl_scanner{fun_states = FS}) ->
    FS1 = FS#xmerl_fun_states{cont = X},
    S#xmerl_scanner{fun_states = FS1}.


%% @spec file(Filename::string()) -> {xmlElement(),Rest}
%%   Rest = list()
%% @equiv file(Filename, [])
file(F) ->
    file(F, []).

%% @spec file(Filename::string(), Options::option_list()) -> {document(),Rest}
%%   Rest = list()
%%% @doc Parse file containing an XML document
file(F, Options) ->
    ExtCharset=case lists:keysearch(encoding,1,Options) of
		   {value,{_,Val}} -> Val;
		   false -> undefined
	       end,
    case int_file(F,Options,ExtCharset) of
	{Res, Tail,S=#xmerl_scanner{close_fun=Close}} ->
	    Close(S), % for side effects only - final state is dropped
	    {Res,Tail};
	{error, Reason} ->
	    {error, Reason}
    end.

int_file(F, Options,_ExtCharset) ->
     %%?dbg("int_file F=~p~n",[F]),
    case file:read_file(F) of
	{ok, Bin} ->
	    int_string(binary_to_list(Bin), Options, filename:dirname(F),F);
	Error ->
	    Error
    end.

int_file_decl(F, Options,_ExtCharset) ->
%     ?dbg("int_file_decl F=~p~n",[F]),
    case file:read_file(F) of
	{ok, Bin} ->
	    int_string_decl(binary_to_list(Bin), Options, filename:dirname(F),F);
	Error ->
	    Error
    end.

%% @spec string(Text::list()) -> {xmlElement(),Rest}
%%   Rest = list()
%% @equiv string(Text, [])
string(Str) ->
    string(Str, []).

%% @spec string(Text::list(),Options::option_list()) -> {document(),Rest}
%%   Rest = list()
%%% @doc Parse string containing an XML document
string(Str, Options) ->
     {Res, Tail, S=#xmerl_scanner{close_fun = Close}} =
	int_string(Str, Options,file_name_unknown),
    Close(S),    % for side effects only - final state is dropped
    {Res,Tail}.

int_string(Str, Options,FileName) ->
    {ok,  XMLBase} = file:get_cwd(),
    int_string(Str, Options, XMLBase, FileName).

int_string(Str, Options, XMLBase, FileName) ->
    S0=initial_state0(Options,XMLBase),
    S = S0#xmerl_scanner{filename=FileName},
    %%?dbg("int_string1, calling xmerl_lib:detect_charset~n",[]),

    %% In case of no encoding attribute in document utf-8 is default, but
    %% another character set may be detected with help of Byte Order Marker or
    %% with help of the encoding of the first 4 bytes.
    case xmerl_lib:detect_charset(S#xmerl_scanner.encoding,Str) of
	{auto,'iso-10646-utf-1',Str2} ->
	    scan_document(Str2, S#xmerl_scanner{encoding="iso-10646-utf-1"});
	{external,'iso-10646-utf-1',Str2} ->
	    scan_document(Str2, S#xmerl_scanner{encoding="iso-10646-utf-1"});
	{undefined,undefined,Str2} -> %% no auto detection
	    scan_document(Str2, S);
	{external,ExtCharset,Str2} ->
	    %% no auto detection, ExtCharset is an explicitly provided
	    %% 7 bit,8 bit or utf-8 encoding
	    scan_document(Str2, S#xmerl_scanner{encoding=atom_to_list(ExtCharset)})
    end.

int_string_decl(Str, Options, XMLBase, FileName) ->
    S0=initial_state0(Options,XMLBase),
    S = S0#xmerl_scanner{filename=FileName},
    case xmerl_lib:detect_charset(S#xmerl_scanner.encoding,Str) of
	{auto,'iso-10646-utf-1',Str2} ->
	    scan_decl(Str2, S#xmerl_scanner{encoding="iso-10646-utf-1"});
	{external,'iso-10646-utf-1',Str2} ->
	    scan_decl(Str2, S#xmerl_scanner{encoding="iso-10646-utf-1"});
	{undefined,undefined,Str2} ->
	    scan_decl(Str2, S);
	{external,ExtCharset,Str2} ->
	    scan_decl(Str2, S#xmerl_scanner{encoding=atom_to_list(ExtCharset)})
    end.



initial_state0(Options,XMLBase) ->
    CommonData = common_data(),
    initial_state(Options, #xmerl_scanner{
		    event_fun = fun event/2,
		    hook_fun = fun hook/2,
		    acc_fun = fun acc/3,
		    fetch_fun = fun fetch/2,
		    close_fun = fun close/1,
		    continuation_fun = fun cont/3,
		    rules_read_fun = fun rules_read/3,
		    rules_write_fun = fun rules_write/4,
		    rules_delete_fun= fun rules_delete/3,
		    xmlbase = XMLBase,
                    common_data = CommonData
		   }).

initial_state([{event_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{event_fun = F});
initial_state([{event_fun, F, ES}|T], S) ->
    S1 = event_state(ES, S#xmerl_scanner{event_fun = F}),
    initial_state(T, S1);
initial_state([{acc_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{acc_fun = F});
initial_state([{hook_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{hook_fun = F});
initial_state([{hook_fun, F, HS}|T], S) ->
    S1 = hook_state(HS, S#xmerl_scanner{hook_fun = F}),
    initial_state(T, S1);
initial_state([{close_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{close_fun = F});
initial_state([{fetch_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{fetch_fun = F});
initial_state([{fetch_fun, F, FS}|T], S) ->
    S1 = fetch_state(FS, S#xmerl_scanner{fetch_fun = F}),
    initial_state(T, S1);
initial_state([{fetch_path, P}|T], S) ->
    initial_state(T, S#xmerl_scanner{fetch_path = P});
initial_state([{continuation_fun, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{continuation_fun = F});
initial_state([{continuation_fun, F, CS}|T], S) ->
    S1 = cont_state(CS, S#xmerl_scanner{continuation_fun = F}),
    initial_state(T, S1);
initial_state([{rules, R}|T], S) ->
    initial_state(T, S#xmerl_scanner{rules = R,
				     keep_rules = true});
initial_state([{rules, Read, Write, RS}|T], S) ->
    S1 = rules_state(RS, S#xmerl_scanner{rules_read_fun = Read,
					 rules_write_fun = Write,
					 keep_rules = true}),
    initial_state(T, S1);
initial_state([{user_state, F}|T], S) ->
    initial_state(T, S#xmerl_scanner{user_state = F});
initial_state([{space, L}|T], S) ->
    initial_state(T, S#xmerl_scanner{space = L});
initial_state([{line, L}|T], S) ->
    initial_state(T, S#xmerl_scanner{line = L});
initial_state([{namespace_conformant, F}|T], S) when F==true; F==false ->
    initial_state(T, S#xmerl_scanner{namespace_conformant = F});
initial_state([{validation, F}|T], S)
  when F==off; F==dtd; F==schema; F==true; F==false ->
    initial_state(T, S#xmerl_scanner{validation = validation_value(F)});
initial_state([{schemaLocation, SL}|T], S) when is_list(SL) ->
    initial_state(T, S#xmerl_scanner{schemaLocation=SL});
initial_state([{quiet, F}|T], S) when F==true; F==false ->
    initial_state(T, S#xmerl_scanner{quiet = F});
initial_state([{doctype_DTD,DTD}|T], S) ->
    initial_state(T,S#xmerl_scanner{doctype_DTD = DTD});
initial_state([{document, F}|T], S) when is_boolean(F) ->
    initial_state(T,S#xmerl_scanner{document = F});
initial_state([{comments, F}|T], S) when is_boolean(F) ->
    initial_state(T,S#xmerl_scanner{comments = F});
initial_state([{default_attrs, F}|T], S) when is_boolean(F) ->
    initial_state(T,S#xmerl_scanner{default_attrs = F});
initial_state([{text_decl,Bool}|T], S) ->
    initial_state(T,S#xmerl_scanner{text_decl=Bool});
initial_state([{environment,Env}|T], S) ->
    initial_state(T,S#xmerl_scanner{environment=Env});
initial_state([{xmlbase, D}|T], S) ->
    initial_state(T, S#xmerl_scanner{xmlbase = D});
initial_state([{encoding, Enc}|T], S) ->
    initial_state(T, S#xmerl_scanner{encoding = Enc});
initial_state([], S=#xmerl_scanner{rules = undefined}) ->
    Tab = ets:new(rules, [set, public]),
    S#xmerl_scanner{rules = Tab};
initial_state([], S) ->
    S.

validation_value(true) ->
    dtd;
validation_value(false) ->
    off;
validation_value(F) ->
    F.

%% Used for compacting (some) indentations.
%% See also fast_accumulate_whitespace().
common_data() ->
    {comdata(lists:duplicate(60, $\s), []),
     comdata(lists:duplicate(15, $\t), []),
     "\n"}.

comdata([], CD)->
    list_to_tuple(CD);
comdata([_ | T]=L, CD) ->
    comdata(T, [[$\n | L] | CD]).

%%% -----------------------------------------------------
%%% Default modifier functions

%%% Hooks:
%%% - {element, Line, Name, Attrs, Content}
%%% - {processing_instruction, Line, Data}

hook(X, State) ->
    {X, State}.

%%% Events:
%%%
%%% #xmerl_event{event : started | ended,
%%%              line  : integer(),
%%%		 col   : integer(),
%%%              data}
%%%
%%% Data		Events
%%% document		started, ended
%%% #xmlElement		started, ended
%%% #xmlAttribute	ended
%%% #xmlPI		ended
%%% #xmlComment		ended
%%% #xmlText		ended
event(_X, S) ->
    S.

%% The acc/3 function can return either {Acc´, S'} or {Acc', Pos', S'},
%% where Pos' can be derived from X#xmlElement.pos, X#xmlText.pos, or
%% X#xmlAttribute.pos (whichever is the current object type.)
%% The acc/3 function is not allowed to redefine the type of object
%% being defined, but _is_ allowed to either ignore it or split it
%% into multiple objects (in which case {Acc',Pos',S'} should be returned.)
%% If {Acc',S'} is returned, Pos will be incremented by 1 by default.
%% Below is an example of an acceptable operation
acc(#xmlText{value = Text}, [X = #xmlText{value = AccText}], S) ->
    {[X#xmlText{value = AccText ++ Text}], S};
acc(X, Acc, S) ->
    {[X|Acc], S}.

fetch({system, URI}, S) ->
    fetch_URI(URI, S);
fetch({public, _PublicID, URI}, S) ->
    fetch_URI(URI, S).

%%% Always assume an external resource can be found locally! Thus
%%% don't bother fetching with e.g. HTTP. Returns the path where the
%%% resource is found.  The path to the external resource is given by
%%% URI directly or the option fetch_path (additional paths) or
%%% directory (base path to external resource)
fetch_URI(URI, S) ->
    %% assume URI is a filename
    Split = filename:split(URI),
    Filename = fun([])->[];(X)->lists:last(X) end (Split),
    Fullname =
	case Split of %% how about Windows systems?
	    ["file:"|Name]-> %% absolute path, see RFC2396 sect 3
		%% file:/dtd_name
		filename:join(["/"|Name]);
	    ["/"|Rest] when Rest /= [] ->
		%% absolute path name
		URI;
	    ["http:"|_Rest] ->
		{http,URI};
	    [] -> %% empty systemliteral
		[];
	    _ ->
		filename:join(S#xmerl_scanner.xmlbase, URI)
	end,
    Path = path_locate(S#xmerl_scanner.fetch_path, Filename, Fullname),
    ?dbg("fetch(~p) -> {file, ~p}.~n", [URI, Path]),
    {ok, Path, S}.

path_locate(_, _, {http,_}=URI) ->
    URI;
path_locate(_, _, []) ->
    [];
path_locate([Dir|Dirs], FN, FullName) ->
    F = filename:join(Dir, FN),
    case file:read_file_info(F) of
	{ok, #file_info{type = regular}} ->
	    {file,F};
	_ ->
	    path_locate(Dirs, FN, FullName)
    end;
path_locate([], _FN, FullName) ->
    {file,FullName}.


cont(_F, Exception, US) ->
    Exception(US).

close(S) ->
    S.


%%% -----------------------------------------------------
%%% Scanner

%%% [1] document ::= prolog element Misc*
scan_document(Str0, S=#xmerl_scanner{event_fun = Event,
				     line = L, col = C,
				     environment=Env,
				     encoding=Charset,
				     document=Document,
				     validation=ValidateResult}) ->
    S1 = Event(#xmerl_event{event = started,
			    line = L,
			    col = C,
			    data = document}, S),

    %% Transform to given character set.
    %% Note that if another character set is given in the encoding
    %% attribute in a XML declaration that one will be used later
    Str=if
	    Charset == "utf-8" ->
		Str0;
	    Charset =/= undefined -> % Default character set is UTF-8
		xmerl_ucs:to_unicode(Str0, list_to_atom(Charset));
	    true -> %% Charset is undefined if no external input is
                    %% given, and no auto detection of character
                    %% encoding was made.
		Str0
	end,
%%     M1 = erlang:memory(),
%%     ?dbg("Memory status before prolog: ~p~n",[M1]),
    {Prolog, Pos, T1, S2} = scan_prolog(Str, S1, _StartPos = 1),
%%     M2 = erlang:memory(),
%%     ?dbg("Memory status after prolog: ~p~n",[M2]),
    %%?dbg("scan_document 2, prolog parsed~n",[]),
    T2 = scan_mandatory("<", T1, 1, S2, expected_element_start_tag),
%%     M3 = erlang:memory(),
%%     ?dbg("Memory status before element: ~p~n",[M3]),
    {Res, T3, S3} = scan_element(T2,S2,Pos),
%%     M4 = erlang:memory(),
%%     ?dbg("Memory status after element: ~p~n",[M4]),
    {Misc, _Pos1, Tail, S4}=scan_misc(T3, S3, Pos + 1),
%%     M5 = erlang:memory(),
%%     ?dbg("Memory status after misc: ~p~n",[M5]),

    S5 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S4#xmerl_scanner.line,
					       col = S4#xmerl_scanner.col,
					       data = document}, S4),

    {Res2, S6} = case validation_mode(ValidateResult) of
	     off ->
		 {Res, cleanup(S5)};
	     dtd when Env == element; Env == prolog ->
		 check_decl2(S5),
		 case xmerl_validate:validate(S5, Res) of
		     {'EXIT', {error, Reason}} ->
			 S5b = cleanup(S5),
			 ?fatal({failed_validation, Reason}, S5b);
		     {'EXIT', Reason} ->
			 S5b = cleanup(S5),
			 ?fatal({failed_validation, Reason}, S5b);
		     {error, Reason} ->
			 S5b = cleanup(S5),
			 ?fatal({failed_validation, Reason}, S5b);
		     {error, Reason, _Next} ->
			 S5b = cleanup(S5),
			 ?fatal({failed_validation, Reason}, S5b);
		     _XML ->
			 {Res, cleanup(S5)}
		 end;
	     schema ->
		 case schemaLocations(Res, S5) of
		     {ok, Schemas} ->
			 _ = cleanup(S5),
			 %%?dbg("Schemas: ~p~nRes: ~p~ninhertih_options(S): ~p~n",
			 %%          [Schemas,Res,inherit_options(S5)]),
			 XSDRes = xmerl_xsd:process_validate(Schemas, Res,
							     inherit_options(S5)),
			 handle_schema_result(XSDRes, S5);
		     _ ->
			 {Res, cleanup(S5)}
		 end;
	     _ ->
		 {Res, cleanup(S5)}
	 end,

    Res3 =
	case Document of
	    true ->
		Content = lists:reverse(Prolog, [Res2 | lists:reverse(Misc)]),
		#xmlDocument{content = Content};
	    false ->
		Res2
	end,
    {Res3, Tail, S6}.


scan_decl(Str, S=#xmerl_scanner{event_fun = Event,
				line = L, col = C,
				environment=_Env,
				encoding=_Charset,
				validation=_ValidateResult}) ->
    S1 = Event(#xmerl_event{event = started,
			    line = L,
			    col = C,
			    data = document}, S),

    case scan_prolog(Str, S1, _StartPos = 1) of
	{_,_,T2="<"++_, S2} ->
	    {{S2#xmerl_scanner.user_state,T2},[],S2};
	{_,_,[], S2}->
	    {[],[],S2};
	{_,_,T2, S2} ->
	    {_,_,S3} = scan_content(T2,S2,[],_Attrs=[],S2#xmerl_scanner.space,
				    _Lang=[],_Parents=[],#xmlNamespace{}),
	    {T2,[],S3}
    end.


%%% [22] Prolog
%%% prolog    ::=    XMLDecl? Misc* (doctypedecl Misc*)?
%%%
%% empty text declarations are handled by the first function clause.
scan_prolog(T, S, Pos) ->
    scan_prolog(T, S, Pos, []).
scan_prolog([], S=#xmerl_scanner{continuation_fun = F}, Pos, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_prolog(MoreBytes, S1, Pos, Acc) end,
      fun(S1) -> {Acc, Pos, [], S1} end,
      S);
scan_prolog("<?xml"++T,
	    S0=#xmerl_scanner{encoding=Charset0,col=Col,line=L},
	    Pos,Acc) when ?whitespace(hd(T)) ->
    {Charset, T3, S3} =
    if
	Col==1,L==1,S0#xmerl_scanner.text_decl==true ->
	    ?dbg("prolog(\"<?xml\")~n", []),
	    ?bump_col(5),
	    {_,T1,S1} = mandatory_strip(T,S),
	    {Decl,T2, S2}=scan_text_decl(T1,S1),
	    Encoding=Decl#xmlDecl.encoding,
	    {Encoding, T2, S2#xmerl_scanner{encoding=Encoding}};
	Col==1,L==1 ->
	    ?dbg("prolog(\"<?xml\")~n", []),
	    ?bump_col(5),
	    {Decl,T2, S2}=scan_xml_decl(T, S),
	    Encoding=Decl#xmlDecl.encoding,
	    {Encoding, T2, S2#xmerl_scanner{encoding=Encoding}};
	true ->
	    ?fatal({xml_declaration_must_be_first_in_doc,Col,L},S0)
    end,
    %% Charset0 is either (1) 'iso-10646-utf-1' (transformation by
    %% auto detection), (2) undefined (no auto detection and no
    %% external encoding), (3) any other encoding format that must be
    %% conformant to the internal explicitly given encoding. The two
    %% former cases implies that the explicit internal encoding
    %% (Charset) may be different from Charset0.

    %% Now transform to declared character set.
    if
	Charset==Charset0 -> % Document already transformed to this charset!
	    scan_prolog(T3, S3, Pos, Acc);
	Charset0=/=undefined ->
	    %% For example may an external entity
	    %% have the BOM for utf-16 and the internal
	    %% explicit encoding='utf-16', then it will be auto
	    %% detected and transformed, Charset0 will be
	    %% 'iso-10646-utf-1', and Charset will be 'utf-16', all
	    %% legal.
	    %%
	    scan_prolog(T3,S3#xmerl_scanner{encoding=Charset0},Pos,Acc);
	Charset == "utf-8" ->
	    scan_prolog(T3, S3, Pos, Acc);
	Charset=/=undefined -> % Document not previously transformed
	    T4=xmerl_ucs:to_unicode(T3,list_to_atom(Charset)),
	    scan_prolog(T4, S3, Pos, Acc);
	true -> % No encoding info given
	    scan_prolog(T3, S3, Pos, Acc)
    end;
scan_prolog("<!DOCTYPE" ++ T,
	    S0=#xmerl_scanner{environment=prolog,encoding=_Charset},
	    Pos, Acc) ->
    ?dbg("prolog(\"<!DOCTYPE\")~n", []),
    ?bump_col(9),
    %% If no known character set assume it is UTF-8
    T1=if
	%%   Charset==undefined -> xmerl_ucs:to_unicode(T,'utf-8');
	   true -> T
       end,
    {T2, S1} = scan_doctype(T1, S),
    scan_misc(T2, S1, Pos, Acc);
scan_prolog(Str="%"++_T,S=#xmerl_scanner{environment={external,_}},
	    Pos,Acc) ->
    {T, S1} = scan_ext_subset(Str,S),
    {Acc, Pos, T, S1};
scan_prolog(Str, S0 = #xmerl_scanner{user_state=_US,encoding=_Charset},
	    Pos,Acc) ->
    ?dbg("prolog(\"<\")~n", []),

    %% Check for Comments, PI before possible DOCTYPE declaration
    ?bump_col(1),
    %% If no known character set assume it is UTF-8
    T=if
%%	  Charset==undefined -> xmerl_ucs:to_unicode(Str,'utf-8');
	  true -> Str
      end,
    {Acc1, Pos1, T1, S1}=scan_misc(T, S, Pos, Acc),
    scan_prolog2(T1,S1,Pos1,Acc1).



scan_prolog2([], S=#xmerl_scanner{continuation_fun = F}, Pos, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_prolog2(MoreBytes, S1, Pos, Acc) end,
      fun(S1) -> {Acc, Pos, [], S1} end,
      S);
scan_prolog2("<!DOCTYPE" ++ T, S0=#xmerl_scanner{environment=prolog},
	     Pos, Acc) ->
    ?dbg("prolog(\"<!DOCTYPE\")~n", []),
    ?bump_col(9),
    {T1, S1} = scan_doctype(T, S),
    scan_misc(T1, S1, Pos, Acc);
scan_prolog2(Str = "<!" ++ _, S, Pos, Acc) ->
    ?dbg("prolog(\"<!\")~n", []),
    %% In e.g. a DTD, we jump directly to markup declarations
    {T, S1} = scan_ext_subset(Str, S),
    {Acc, Pos, T, S1};
scan_prolog2(Str, S0 = #xmerl_scanner{user_state=_US},Pos,Acc) ->
    ?dbg("prolog(\"<\")~n", []),

    %% Here we consider the DTD provided by doctype_DTD option,
    S1 =
	case S0 of
	    #xmerl_scanner{validation=dtd,doctype_DTD=DTD} when is_list(DTD) ->
		S=fetch_DTD(undefined,S0),
		check_decl(S),
		S;
	    _ -> S0
	end,
    %% Check for more Comments and PI after DOCTYPE declaration
%    ?bump_col(1),
    scan_misc(Str, S1, Pos, Acc).




%%% [27] Misc ::=   	Comment | PI | S
%% Note:
%% - Neither of Comment and PI are returned in the resulting parsed
%%   structure.
%% - scan_misc/3 implements Misc* as that is how the rule is always used
scan_misc(T, S, Pos) ->
    scan_misc(T, S, Pos, []).
scan_misc([], S=#xmerl_scanner{continuation_fun = F}, Pos, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_misc(MoreBytes, S1, Pos, Acc) end,
      fun(S1) -> {Acc, Pos, [], S1} end,
      S);
scan_misc("<!--" ++ T, S0=#xmerl_scanner{acc_fun = F, comments=CF}, Pos, Acc) -> % Comment
    ?bump_col(4),
    {C, T1, S1} = scan_comment(T, S, Pos, _Parents = [], _Lang = []),
    case CF of
	true ->
	    {Acc2, Pos2, S3} =
		case F(C, Acc, S1) of
		    {Acc1, S2} ->
			{Acc1, Pos + 1, S2};
		    {Acc1, Pos1, S2} ->
			{Acc1, Pos1, S2}
		end,
	    scan_misc(T1, S3, Pos2, Acc2);
	false ->
	    scan_misc(T1, S1, Pos, Acc)
    end;
scan_misc("<?" ++ T, S0=#xmerl_scanner{acc_fun = F}, Pos, Acc) -> % PI
    ?dbg("prolog(\"<?\")~n", []),
    ?bump_col(2),
    {PI, T1, S1} = scan_pi(T, S, Pos, []),
    {Acc2, Pos2, S3} = case F(PI, Acc, S1) of
			   {Acc1, S2} ->
			       {Acc1, Pos + 1, S2};
			   {Acc1, Pos1, S2} ->
			       {Acc1, Pos1, S2}
		       end,
    scan_misc(T1,S3,Pos2,Acc2);
scan_misc(T=[H|_T], S, Pos, Acc) when ?whitespace(H) ->
    ?dbg("prolog(whitespace)~n", []),
    {_,T1,S1}=strip(T,S),
    scan_misc(T1,S1,Pos,Acc);
scan_misc(T,S,Pos,Acc) ->
    {Acc,Pos,T,S}.


cleanup(S=#xmerl_scanner{keep_rules = false,
			 rules = Rules}) ->
    ets:delete(Rules),
    S#xmerl_scanner{rules = undefined};
cleanup(S) ->
    S.

%%% Prolog and Document Type Declaration XML 1.0 Section 2.8
%% [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
%% [24] VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
scan_xml_decl(T, S) ->
    %% VersionInfo [24] is mandatory
    {_,T1,S1} = mandatory_strip(T,S),
    {T2,S2} =
	case T1 of
	    "version" ++ _T2 ->
		{_T2,S1#xmerl_scanner{col=S1#xmerl_scanner.col+7}};
	    _ -> ?fatal(expected_version_attribute,S1)
	end,
    {T3, S3} = scan_eq(T2, S2),
    {Vsn, T4, S4} = scan_xml_vsn(T3, S3),
    Attr = #xmlAttribute{name = version,
			 parents = [{xml, _XMLPos = 1}],
			 value = Vsn},
    scan_xml_decl(T4, S4, #xmlDecl{vsn = Vsn,
				   attributes = [Attr]}).

scan_xml_decl([], S=#xmerl_scanner{continuation_fun = F}, Decl) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_xml_decl(MoreBytes, S1, Decl) end,
      fun(S1) -> {[], [], S1} end,
      S);
scan_xml_decl("?>" ++ T, S0, Decl) ->
    ?bump_col(2),
    return_xml_decl(T,S,Decl);
scan_xml_decl(T,S=#xmerl_scanner{event_fun = _Event},Decl) when ?whitespace(hd(T)) ->
    {_,T1,S1}=mandatory_strip(T,S),
    scan_xml_decl2(T1,S1,Decl);
scan_xml_decl(_T,S=#xmerl_scanner{event_fun = _Event},_Decl) ->
    ?fatal(preformat([expected,one,'of:'],['?>',whitespace_character],","),S).

scan_xml_decl2("?>" ++ T, S0,Decl) ->
    ?bump_col(2),
    return_xml_decl(T,S,Decl);
scan_xml_decl2("encoding" ++ T, S0 = #xmerl_scanner{event_fun = Event},
	      Decl0 = #xmlDecl{attributes = Attrs}) ->
    %% [80] EncodingDecl
    ?bump_col(8),
    {T1, S1} = scan_eq(T, S),
    {EncName, T2, S2} = scan_enc_name(T1, S1),
    LowEncName=xmerl_lib:to_lower(EncName),
    Attr = #xmlAttribute{name = encoding,
			 parents = [{xml, _XMLPos = 1}],
			 value = LowEncName},
    Decl = Decl0#xmlDecl{encoding = LowEncName,
			 attributes = [Attr|Attrs]},
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S0#xmerl_scanner.line,
					       col = S0#xmerl_scanner.col,
					       data = Attr}, S2),
    case T2 of
	"?>" ++ _T3 ->
	    scan_xml_decl3(T2,S3,Decl);
	_ ->
	    {_,T3,S4} = mandatory_strip(T2,S3),
	    scan_xml_decl3(T3, S4, Decl)
    end;
scan_xml_decl2(T="standalone" ++ _T,S,Decl) ->
    scan_xml_decl3(T,S,Decl);
scan_xml_decl2(_BadString,S,_Decl) ->
        ?fatal(preformat([expected,one,'of:'],['?>',standalone,encoding],","),S).
%    ?fatal(lists:flatten(io_lib:format("~s ~s ~s: ~s, ~s, ~s",[expected,one,'of','?>',standalone,encoding])),S).
%    ?fatal({expected_one_of,"?>",standalone,encoding},S).

scan_xml_decl3("?>" ++ T, S0,Decl) ->
    ?bump_col(2),
    return_xml_decl(T,S,Decl);
scan_xml_decl3("standalone" ++ T,S0 = #xmerl_scanner{event_fun = Event},
	      Decl0 = #xmlDecl{attributes = Attrs}) ->
    %% [32] SDDecl
    ?bump_col(10),
    {T1, S1} = scan_eq(T, S),
    {StValue,T2,S2}=scan_standalone_value(T1,S1),
    Attr = #xmlAttribute{name = standalone,
			 parents = [{xml, _XMLPos = 1}],
			 value = StValue},
    Decl = Decl0#xmlDecl{standalone = StValue,
			 attributes = [Attr|Attrs]},
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S0#xmerl_scanner.line,
					       col = S0#xmerl_scanner.col,
					       data = Attr}, S2),
    {_,T3,S4} = strip(T2,S3),
    T4 = scan_mandatory("?>",T3,2,S4,expected_xml_decl_endtag),
%%    "?>" ++ T4 = T3,
    return_xml_decl(T4, S4#xmerl_scanner{col=S4#xmerl_scanner.col+2}, Decl).


return_xml_decl(T,S=#xmerl_scanner{hook_fun = _Hook,
				   event_fun = Event},
		Decl0 = #xmlDecl{attributes = Attrs}) ->
    ?strip1,
    Decl = Decl0#xmlDecl{attributes = lists:reverse(Attrs)},
    S2 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S#xmerl_scanner.line,
					       col = S#xmerl_scanner.col,
					       data = Decl}, S1),
%%    {Ret, S3} = Hook(Decl, S2),
%%    {Ret, T1, S3}.
    {Decl, T1, S2}.


scan_standalone_value("'yes'" ++T,S0)->
    ?bump_col(5),
    {'yes',T,S#xmerl_scanner{standalone=yes}};
scan_standalone_value("\"yes\"" ++T,S0)->
    ?bump_col(5),
    {'yes',T,S#xmerl_scanner{standalone=yes}};
scan_standalone_value("'no'" ++T,S0) ->
    ?bump_col(4),
    {'no',T,S};
scan_standalone_value("\"no\"" ++T,S0) ->
    ?bump_col(4),
    {'no',T,S}.

%%%
%%% Text declaration XML 1.0 section 4.3.1
%%% [77] TextDecl  ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
scan_text_decl(T,S=#xmerl_scanner{event_fun = Event}) ->
    {#xmlDecl{attributes=Attrs}=Decl0,T1,S1} = scan_optional_version(T,S),
    T2 =
	case T1 of
	    "encoding" ++ _T2 -> _T2;
	    _ ->
		?fatal(expected_encoding_attribute,S1)
        end,
    S2 = S1#xmerl_scanner{col = S1#xmerl_scanner.col + 8},
    {T3, S3} = scan_eq(T2, S2),
    {EncName, T4, S4} = scan_enc_name(T3, S3),
    LowEncName=xmerl_lib:to_lower(EncName),
    ?strip5,
    Attr = #xmlAttribute{name = encoding,
			 parents = [{xml,1}],
			 value = LowEncName},
    Decl = Decl0#xmlDecl{encoding = LowEncName,
 			 attributes = [Attr|Attrs]},
    S6=#xmerl_scanner{} = Event(#xmerl_event{event = ended,
					     line = S5#xmerl_scanner.line,
					     col = S5#xmerl_scanner.col,
					     data = Attr}, S5),
    scan_text_decl(T5,S6,Decl).

scan_text_decl("?>"++T,S0 = #xmerl_scanner{hook_fun = _Hook,
					   event_fun = Event},
	       Decl0 = #xmlDecl{attributes = Attrs}) ->
    ?bump_col(2),
    ?strip1,
    Decl = Decl0#xmlDecl{attributes = lists:reverse(Attrs)},
    S2 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = S0#xmerl_scanner.line,
					       col = S0#xmerl_scanner.col,
					       data = Decl}, S1),
%%     {Ret, S3} = Hook(Decl, S2),
%%     {Ret, T1, S3};
    {Decl, T1, S2};
scan_text_decl([H|_T],S,_) ->
    ?fatal({unexpected_character_in_text_declaration,H},S).

scan_optional_version("version"++T,S0) ->
    ?bump_col(7),
    ?strip1,
    {T2, S2} = scan_eq(T1, S1),
    {Vsn, T3, S3} = scan_xml_vsn(T2, S2),
    {_,T4,S4} = mandatory_strip(T3,S3),
    Attr = #xmlAttribute{name = version,parents = [{xml,1}],value = Vsn},
    {#xmlDecl{attributes=[Attr]},T4,S4};
scan_optional_version(T,S) ->
    {#xmlDecl{attributes=[]},T,S}.



%%%%%%% [81] EncName
scan_enc_name([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_enc_name(MoreBytes, S1) end,
      fatal_fun(expected_encoding_name),
      S);
scan_enc_name([H|T], S0) when H >= $"; H =< $' ->
    ?bump_col(1),
    scan_enc_name(T, S, H, []).


scan_enc_name([], S=#xmerl_scanner{continuation_fun = F}, Delim, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_enc_name(MoreBytes, S1, Delim, Acc) end,
      fatal_fun(expected_encoding_name),
      S);
scan_enc_name([H|T], S0, Delim, Acc) when H >= $a, H =< $z ->
    ?bump_col(1),
    scan_enc_name2(T, S, Delim, [H|Acc]);
scan_enc_name([H|T], S0, Delim, Acc) when H >= $A, H =< $Z ->
    ?bump_col(1),
    scan_enc_name2(T, S, Delim, [H|Acc]);
scan_enc_name([H|_T],S,_Delim,_Acc) ->
    ?fatal({error,{unexpected_character_in_Enc_Name,H}},S).

scan_enc_name2([], S=#xmerl_scanner{continuation_fun = F}, Delim, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_enc_name2(MoreBytes, S1, Delim, Acc) end,
      fatal_fun(expected_encoding_name),
      S);
scan_enc_name2([H|T], S0, H, Acc) ->
    ?bump_col(1),
    {lists:reverse(Acc), T, S};
scan_enc_name2([H|T], S0, Delim, Acc) when H >= $a, H =< $z ->
    ?bump_col(1),
    scan_enc_name2(T, S, Delim, [H|Acc]);
scan_enc_name2([H|T], S0, Delim, Acc) when H >= $A, H =< $Z ->
    ?bump_col(1),
    scan_enc_name2(T, S, Delim, [H|Acc]);
scan_enc_name2([H|T], S0, Delim, Acc) when H >= $0, H =< $9 ->
    ?bump_col(1),
    scan_enc_name2(T, S, Delim, [H|Acc]);
scan_enc_name2([H|T], S0, Delim, Acc) when H == $.; H == $_; H == $- ->
    ?bump_col(1),
    scan_enc_name2(T, S, Delim, [H|Acc]).


%%%%%%% [26] VersionNum
%%% VersionNum    ::=    ([a-zA-Z0-9_.:] | '-')+
scan_xml_vsn([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_xml_vsn(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_xml_vsn([H|T], S) when H==$"; H==$'->
    xml_vsn(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, H, []).

xml_vsn([], S=#xmerl_scanner{continuation_fun = F}, Delim, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> xml_vsn(MoreBytes, S1, Delim, Acc) end,
      fatal_fun(unexpected_end),
      S);
xml_vsn([H|T], S=#xmerl_scanner{col = C}, H, Acc) ->
    {lists:reverse(Acc), T, S#xmerl_scanner{col = C+1}};
xml_vsn([H|T], S=#xmerl_scanner{col = C},Delim, Acc) when H >= $a, H =< $z ->
    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
xml_vsn([H|T], S=#xmerl_scanner{col = C},Delim, Acc) when H >= $A, H =< $Z ->
    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
xml_vsn([H|T], S=#xmerl_scanner{col = C},Delim, Acc) when H >= $0, H =< $9 ->
    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
xml_vsn([H|T], S=#xmerl_scanner{col = C}, Delim, Acc) ->
    case lists:member(H, "_.:-") of
	true ->
	    xml_vsn(T, S#xmerl_scanner{col = C+1}, Delim, [H|Acc]);
	false ->
	    ?fatal({invalid_vsn_char, H}, S)
    end.

%%%%%%% [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'

scan_pi([], S=#xmerl_scanner{continuation_fun = F}, Pos, Ps) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_pi(MoreBytes, S1, Pos, Ps) end,
      fatal_fun(unexpected_end),
      S);
scan_pi(Str = [H1,H2,H3 | T],S0=#xmerl_scanner{line = L, col = C}, Pos, Ps)
  when H1==$x;H1==$X ->
    %% names beginning with [xX][mM][lL] are reserved for future use.
    ?bump_col(3),
    if
	((H2==$m) or (H2==$M)) and
	((H3==$l) or (H3==$L)) ->
	    scan_wellknown_pi(T,S,Pos,Ps);
	true ->
	    {Target, _NamespaceInfo, T1, S1} = scan_name(Str, S),
	    scan_pi(T1, S1, Target, L, C, Pos, Ps, [])
    end;
scan_pi(Str, S=#xmerl_scanner{line = L, col = C}, Pos, Ps) ->
    {Target, _NamespaceInfo, T1, S1} = scan_name(Str, S),
    scan_pi(T1, S1, Target, L, C, Pos, Ps, []).


%%% More info on xml-stylesheet can be found at:
%%%   "Associating Style Sheets with XML documents", Version 1.0,
%%%   W3C Recommendation 29 June 1999 (http://www.w3.org/TR/xml-stylesheet/)
scan_wellknown_pi("-stylesheet"++T, S0=#xmerl_scanner{line=L,col=C},Pos,Ps) ->
    ?dbg("prolog(\"<?xml-stylesheet\")~n", []),
    ?bump_col(16),
    scan_pi(T, S, "xml-stylesheet",L,C,Pos,Ps,[]);
scan_wellknown_pi(Str,S,_Pos,_Ps) ->
    ?fatal({invalid_target_name, lists:sublist(Str, 1, 10)}, S).



scan_pi([], S=#xmerl_scanner{continuation_fun = F}, Target,
	L, C, Pos, Ps, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_pi(MoreBytes, S1, Target,
				    L, C, Pos, Ps, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_pi("?>" ++ T, S0 = #xmerl_scanner{hook_fun = Hook,
				       event_fun = Event},
	Target, L, C, Pos, Ps, Acc) ->
    ?bump_col(2),
    PI = #xmlPI{name = Target,
		parents = Ps,
		pos = Pos,
		value = lists:reverse(Acc)},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = PI}, S),
    {Ret, S2} = Hook(PI, S1),
    {Ret, T, S2};
scan_pi([H|T], S, Target, L, C, Pos, Ps, Acc) when ?whitespace(H) ->
    ?strip1,
    scan_pi2(T1, S1, Target, L, C, Pos, Ps, Acc);
scan_pi([H|_T],S,_Target, _L, _C, _Pos, _Ps, _Acc) ->
    ?fatal({expected_whitespace_OR_end_of_PI,{char,H}}, S).

scan_pi2([], S=#xmerl_scanner{continuation_fun = F}, Target,
	 L, C, Pos, Ps, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_pi2(MoreBytes, S1, Target,
				     L, C, Pos, Ps, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_pi2("?>" ++ T, S0 = #xmerl_scanner{hook_fun = Hook,
				       event_fun = Event},
	 Target, L, C, Pos, Ps, Acc) ->
    ?bump_col(2),
    PI = #xmlPI{name = Target,
		parents = Ps,
		pos = Pos,
		value = lists:reverse(Acc)},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = PI}, S),
    {Ret, S2} = Hook(PI, S1),
    {Ret, T, S2};
scan_pi2(Str, S0, Target, L, C, Pos, Ps, Acc) ->
    ?bump_col(1),
    {Ch,T} = wfc_legal_char(Str,S),
    scan_pi2(T, S, Target, L, C, Pos, Ps, [Ch|Acc]).



%% [28] doctypedecl ::=
%%   '<!DOCTYPE' S Name (S ExternalID)? S? ('[' intSubset ']' S?)? '>'
scan_doctype([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_doctype(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_doctype(T, S) ->
    {_,T1,S1} = mandatory_strip(T,S),
    {DTName, _NamespaceInfo, T2, S2} = scan_name(T1, S1),
    ?strip3,
    scan_doctype1(T3, S3#xmerl_scanner{doctype_name =  DTName}).


%% [75] ExternalID ::= 'SYSTEM' S SystemLiteral
%%		     | 'PUBLIC' S PubidLiteral S SystemLiteral
scan_doctype1([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_doctype1(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_doctype1("PUBLIC" ++ T, S0) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {PIDL, T2, S2} = scan_pubid_literal(T1, S1),
    {_,T3,S3} = mandatory_strip(T2,S2),
    {SL, T4, S4} = scan_system_literal(T3, S3),
    ?strip5,
    scan_doctype2(T5, S5, {public, PIDL, SL});
scan_doctype1("SYSTEM" ++ T, S0) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {SL, T2, S2} = scan_system_literal(T1, S1),
    ?strip3,
    scan_doctype2(T3, S3, {system, SL});
scan_doctype1(T, S) ->
    scan_doctype2(T, S, undefined).


scan_doctype2([], S=#xmerl_scanner{continuation_fun = F},DTD) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_doctype2(MoreBytes, S1, DTD) end,
      fatal_fun(unexpected_end),
      S);
scan_doctype2("[" ++ T, S0, DTD) ->
    ?bump_col(1),
    ?strip1,
    scan_doctype3(T1, S1, DTD);
scan_doctype2(">" ++ T, S0, DTD) ->
    ?bump_col(1),
    ?strip1,
    S2 = fetch_DTD(DTD, S1),
    check_decl(S2),
    {T1, S2};
scan_doctype2(_T,S,_DTD) ->
    ?fatal(expected_end_of_DOCTYPE_declaration, S).

%% [28a] DeclSep   ::= PEReference | S
%% [28b] intSubset ::= (markupdecl | DeclSep)*
scan_doctype3([], S=#xmerl_scanner{continuation_fun = F},DTD) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_doctype3(MoreBytes, S1,DTD) end,
      fatal_fun(unexpected_end),
      S);
scan_doctype3("%" ++ T, S0, DTD) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ?strip2,
    case expand_pe_reference(PERefName, S2,as_PE) of
	{system, _} = Name ->
	    S3 = fetch_DTD(Name, S2),
	    check_decl(S3),
	    scan_doctype3(T2, S3, DTD);
	{public, _} = Name ->
	    S3 = fetch_DTD(Name, S2),
	    check_decl(S3),
	    scan_doctype3(T2, S3, DTD);
	{public, _, _} = Name ->
	    S3 = fetch_DTD(Name, S2),
	    check_decl(S3),
	    scan_doctype3(T2, S3, DTD);
	ExpRef when is_list(ExpRef) -> % Space added, see Section 4.4.8
	    {_,T3,S3} = strip(ExpRef++T2,S2),
	    scan_doctype3(T3,S3,DTD)
    end;
scan_doctype3("]" ++ T, S0, DTD) ->
    ?bump_col(1),
    ?strip1,
    S2 = fetch_DTD(DTD, S1),
    check_decl(S2),
    T2 = scan_mandatory(">",T1,1,S2,expected_doctype_end_tag),
%%    ">" ++ T2 = T1,
    {T2, S2};
scan_doctype3(T, S, DTD) ->
    {_, T1, S1} = scan_markup_decl(T, S),
    scan_doctype3(T1, S1, DTD).



fetch_DTD(undefined, S=#xmerl_scanner{doctype_DTD=URI}) when is_list(URI)->
    %% allow to specify DTD name when it isn't available in xml stream
    fetch_DTD({system,URI},S#xmerl_scanner{doctype_DTD=option_provided});
fetch_DTD(undefined, S) ->
    S;
% fetch_DTD(_,S=#xmerl_scanner{validation=false}) ->
%     S;
fetch_DTD(DTDSpec, S)->
    case fetch_and_parse(DTDSpec,S,[{text_decl,true},
				    {environment,{external,subset}}]) of
	NewS when is_record(NewS,xmerl_scanner) ->
	    NewS;
	{_Res,_Tail,_Sx} -> % Continue with old scanner data, result in Rules
	    S
    end.

fetch_and_parse(ExtSpec,S=#xmerl_scanner{fetch_fun=Fetch,
					 rules=Rules,
					 xmlbase = XMLBase},
		Options0) ->
    RetS =
    case Fetch(ExtSpec, S) of
	{ok, NewS} ->
	    %% For backward compatibility only. This will be removed later!!
	    NewS;
	{ok, not_fetched,NewS} ->
	    NewS;
	{ok, DataRet, NewS = #xmerl_scanner{
			fetch_path=FetchPath,
			user_state = UState,
			event_fun = Event,
			hook_fun = Hook,
			fetch_fun = Fetch1,
			close_fun = Close1,
			continuation_fun = Cont,
			acc_fun = Acc,
			rules_read_fun = Read,
			rules_write_fun = Write,
			validation = Valid,
			quiet = Quiet,
			encoding = Charset
		       }} ->
	    EvS = event_state(NewS),
	    HoS = hook_state(NewS),
	    FeS = fetch_state(NewS),
	    CoS = cont_state(NewS),
	    Options = Options0++[{fetch_path,FetchPath},
				 {user_state, UState},
				 {rules, Rules},
				 {event_fun, Event, EvS},
				 {hook_fun, Hook, HoS},
				 {fetch_fun, Fetch1, FeS},
				 {close_fun, Close1},
				 {continuation_fun, Cont, CoS},
				 {rules, Read, Write, ""},
				 {acc_fun, Acc},
				 {validation,Valid},
				 {quiet,Quiet},
				 {encoding,Charset}],

	    case DataRet of
		{file, F} ->
		    int_file_decl(F, Options,Charset);
		{string, String} ->
		    int_string_decl(String, Options,XMLBase,file_name_unknown);
		 _ ->
		    %% other scheme
		    {DataRet,[],NewS}
	    end;
	Error ->
	    ?fatal({error_fetching_DTD, {ExtSpec, Error}}, S)
    end,
    case RetS of
	#xmerl_scanner{} ->
	    RetS#xmerl_scanner{text_decl=false,
			       environment=S#xmerl_scanner.environment};
	_ -> RetS
    end.


fetch_not_parse(ExtSpec,S=#xmerl_scanner{fetch_fun=Fetch}) ->
    case Fetch(ExtSpec,S) of
	{ok, not_fetched,_NewS} ->
	    ?fatal({error_fetching_external_source,ExtSpec},S);
	{ok, DataRet, NewS} ->
	    {String,LocationName} =
		case DataRet of
		    {file,F} ->
			{get_file(F,S),F};
		    {string,Str} ->
			{binary_to_list(Str),file_name_unknown};
		    {http,URI} ->
			{{http,URI},URI};
		    _ -> DataRet
		end,
	    {String, NewS#xmerl_scanner{filename=LocationName}};
	 _ ->
	    ?fatal({error_fetching_external_resource,ExtSpec},S)
    end.

get_file(F,S) ->
%     ?dbg("get_file F=~p~n",[F]),
    case file:read_file(F) of
	{ok,Bin} ->
	    binary_to_list(Bin);
	Err ->
	    ?fatal({error_reading_file,F,Err},S)
    end.
%% check_decl/1
%% Now it is necessary to check that all referenced types is declared,
%% since it is legal to reference some xml types before they are
%% declared.
check_decl(#xmerl_scanner{validation=V}) when V =/= dtd ->
    ok;
check_decl(#xmerl_scanner{rules=Tab} = S) ->
    check_notations(Tab,S),
    check_elements(Tab,S), %% check also attribute defs for element
    check_entities(Tab,S).

check_notations(Tab,S) ->
    case ets:match(Tab,{{notation,'$1'},undeclared}) of
	[[]] -> ok;
	[] ->  ok;
	[L] when is_list(L) ->
	    ?fatal({error_missing_declaration_in_DTD,hd(L)},S);
	Err ->
	    ?fatal({error_missing_declaration_in_DTD,Err},S)
    end.

check_elements(Tab,S) ->
    case catch ets:match(Tab,{{elem_def,'_'},'$2'},10) of
	{_,_}=M ->
	    Fun = fun({Match,'$end_of_table'},_F) ->
			  lists:foreach(fun(X)->check_elements2(X,S) end,
					Match),
			  ok;
		     ('$end_of_table',_) ->
			  ok;
		     ({Match,Cont},F) ->
			  lists:foreach(fun(X)->check_elements2(X,S) end,
					Match),
			  F(ets:match(Cont),F)
		  end,
	    Fun(M,Fun);
	'$end_of_table' -> ok;
	Err -> ?fatal({error_missing_declaration_in_DTD,Err},S)
    end.

% it is not an error to declare attributes for an element that is not
% declared.
check_elements2([#xmlElement{attributes=Attrs}],S) ->
    check_attributes(Attrs,S);
check_elements2(_,_) ->
    ok.

check_attributes([{N1,'ID',_,_,_}=Attr|Rest],S) ->
    case lists:keysearch('ID',2,Rest) of
	{value,Att2} ->
	    ?fatal({error_more_than_one_ID_def,N1,element(1,Att2)},S);
	_ ->
	    ok
    end,
    vc_ID_Attribute_Default(Attr,S),
    check_attributes(Rest,S);
check_attributes([{_,{enumeration,_},_,_,_}=Attr|T],S) ->
    vc_Enumeration(Attr,S),
    check_attributes(T,S);
check_attributes([{_,Ent,_,_,_}=Attr|T],S)
  when Ent=='ENTITY';Ent=='ENTITIES' ->
    vc_Entity_Name(Attr,S),
    check_attributes(T,S);
check_attributes([_|T],S) ->
    check_attributes(T,S);
check_attributes([],_S) ->
    ok.

check_entities(Tab,S=#xmerl_scanner{validation=dtd}) ->
    case ets:match(Tab,{{entity,'$1'},undeclared}) of
	[[]] -> ok;
	[] ->  ok;
	[L] when is_list(L) ->
	    ?fatal({error_missing_declaration_in_DTD,hd(L)},S);
	Err ->
	    ?fatal({error_missing_declaration_in_DTD,Err},S)
    end;
check_entities(_,_) ->
    ok.


%% check_decl2/1: checks that all referenced ID attributes are declared
check_decl2(S=#xmerl_scanner{rules=Tab}) ->
    check_referenced_ids(Tab,S).


check_referenced_ids(Tab,S) ->
    case ets:match(Tab,{{id,'$1'},undeclared}) of
	[[]] -> ok;
	[] ->  ok;
	[L] when is_list(L) ->
	    ?fatal({error_missing_declaration_in_DTD,hd(L)},S);
	Err ->
	    ?fatal({error_missing_declaration_in_DTD,Err},S)
    end.

%%%%%%% [30] extSubSet ::= TextDecl? extSubsetDecl

scan_ext_subset([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_ext_subset(MoreBytes, S1) end,
      fun(S1) -> {[], S1} end,
      S);
scan_ext_subset("%" ++ T, S0) ->
    %% DeclSep [28a]: WFC: PE Between Declarations.
    %% The replacement text of a parameter entity reference in a
    %% DeclSep must match the production extSubsetDecl.
    ?bump_col(1),
    {T1,S1} = scan_decl_sep(T,S),
    scan_ext_subset(T1, S1);
scan_ext_subset("<![" ++ T, S0) ->
    ?bump_col(3),
    ?strip1,
    {_, T2, S2} = scan_conditional_sect(T1, S1),
    scan_ext_subset(T2,S2);
scan_ext_subset(T, S) when ?whitespace(hd(T)) ->
    {_,T1,S1} = strip(T,S),
    scan_ext_subset(T1, S1);
scan_ext_subset(T, S) ->
    {_, T1, S1} = scan_markup_decl(T, S),
    scan_ext_subset(T1, S1).


%%%%%%% [28a] DeclSep ::= PEReference | S
scan_decl_sep(T,S) ->
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    {ExpandedRef,S2} =
	case expand_pe_reference(PERefName,S1,as_PE) of
	    Tuple when is_tuple(Tuple) ->
		%% {system,URI} or {public,URI}
		{ExpRef,_Sx}=fetch_not_parse(Tuple,S1),
		{ExpRef,S1};
	    ExpRef ->
		{ExpRef,S1}
	end,
    {_,TRef,S3} = strip(ExpandedRef,S2),
    {_,S4}=scan_ext_subset(TRef,S3),
    {T1,S4}.
% scan_decl_sep(T,S=#xmerl_scanner{rules_read_fun=Read,
% 				 rules_write_fun=Write,
% 				 rules_delete_fun=Delete}) ->
%     {PERefName, T1, S1} = scan_pe_reference(T, S),
%     {ExpandedRef,S2} =
% 	case expand_pe_reference(PERefName,S1,as_PE) of
% 	    Tuple when tuple(Tuple) ->
% 		%% {system,URI} or {public,URI}
% 		{ExpRef,Sx}=fetch_not_parse(Tuple,S1),
% 		{EntV,_,_S2} = scan_entity_value(ExpRef, Sx, no_delim,
% 						 PERefName,parameter),
% 		%% should do an update Write(parameter_entity) so next
% 		%% expand_pe_reference is faster
% 		Delete(parameter_entity,PERefName,_S2),
% 		_S3 = Write(parameter_entity,PERefName,EntV,_S2),
% 		EntV2 = Read(parameter_entity,PERefName,_S3),
% 		{" " ++ EntV2 ++ " ",_S3};
% 	    ExpRef ->
% 		{ExpRef,S1}
% 	end,
%     {_, T3, S3} = strip(ExpandedRef,S2),
%     {_T4,S4} = scan_ext_subset(T3,S3),
%     strip(T1,S4).

%%%%%%% [61] ConditionalSect ::= includeSect | ignoreSect

scan_conditional_sect([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_conditional_sect(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_conditional_sect("IGNORE" ++ T, S0) ->
    ?bump_col(6),
    ?strip1,
    T2 = scan_mandatory("[",T1,1,S,expected_IGNORE_bracket),
%    "[" ++ T2 = T1,
    {_,T3,S3} = strip(T2,S1),
    scan_ignore(T3,S3);
scan_conditional_sect("INCLUDE" ++ T, S0) ->
    ?bump_col(7),
    ?strip1,
    T2 = scan_mandatory("[",T1,1,S,expected_INCLUDE_bracket),
%    "[" ++ T2 = T1,
    {_,T3,S3} = strip(T2,S1),
    scan_include(T3, S3);
scan_conditional_sect("%"++T,S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2} = strip(ExpRef ++ T1,S1),
    scan_conditional_sect(T2,S2).


%%%% [63] ignoreSect	 ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
%%%% [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
%%%% [65] Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
scan_ignore(Str,S) ->
    scan_ignore(Str,S,0).

scan_ignore([], S=#xmerl_scanner{continuation_fun = F},Level) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_ignore(MoreBytes, S1,Level) end,
      fatal_fun(unexpected_end),
      S);
scan_ignore("<![" ++ T, S0,Level) ->
    %% nested conditional section. Topmost condition is ignore, though
    ?bump_col(3),
    scan_ignore(T, S,Level+1);
scan_ignore("]]>" ++ T, S0,0) ->
    ?bump_col(3),
    {[], T, S};
scan_ignore("]]>" ++ T, S0,Level) ->
    ?bump_col(3),
    scan_ignore(T, S,Level-1);
scan_ignore([_H|T],S0,Level) ->
    ?bump_col(1),
    scan_ignore(T,S,Level).


%%%%%%% [62] includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
scan_include([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_include(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_include("]]>" ++ T, S0) ->
    ?bump_col(3),
    {[], T, S};
scan_include("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2} = strip(ExpRef ++ T1,S1),
    scan_include(T2, S2);
scan_include("<![" ++ T, S0) ->
    ?bump_col(3),
    ?strip1,
    {_, T2, S2} = scan_conditional_sect(T1, S1),
    ?strip3,
    scan_include(T3,S3);
scan_include(T, S) ->
    {_, T1, S1} = scan_markup_decl(T, S),
    scan_include(T1, S1).


%%%%%%% [29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl |
%%%%%%%                     NotationDecl | PI |Comment
%%%%%%% [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'

%% Validity constraint: Unique Type Declaration: No element type may be
%% declared more than once.
%%
scan_markup_decl([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_markup_decl(MoreBytes, S1) end,
      fun(S1) -> {[], [], S1} end,
      S);
scan_markup_decl("<!--" ++ T, S0) ->
    ?bump_col(4),
    scan_comment(T, S);
scan_markup_decl("<?" ++ T, S0) ->
    ?bump_col(2),
    {_PI, T1, S1} = scan_pi(T, S,_Pos=markup,[]),
    strip(T1, S1);
scan_markup_decl("<!ELEMENT" ++ T,
		 #xmerl_scanner{rules_read_fun = Read,
				rules_write_fun = Write,
				rules_delete_fun = Delete} = S0) ->
    ?bump_col(9),
    {_,T1,S1} = mandatory_strip(T,S),
    {Ename, _NamespaceInfo, T2, S2} = scan_name(T1, S1),
    Element  =
	case Read(elem_def, Ename, S2) of
	    El = #xmlElement{elementdef=Decl} when Decl =/= undeclared ->
		case S2#xmerl_scanner.validation of
		    dtd ->
			?fatal({already_defined, Ename}, S2);
		    _ ->
			Delete(elem_def,Ename,S2),
			El
		end;
	    El = #xmlElement{} ->
		Delete(elem_def,Ename,S2),
		El;
	    undefined ->
		#xmlElement{}
	end,
    {_,T3,S3} = mandatory_strip(T2,S2),
    {Edef, T4, S4} = scan_contentspec(T3, S3),
    ?strip5,
    {">" ++ T6,S6} = scan_element_completion(T5,S5),
    S7 = Write(elem_def, Ename,
	       Element#xmlElement{name = Ename,
				  content = Edef,
				  elementdef=S6#xmerl_scanner.environment},
	       S6#xmerl_scanner{col=S6#xmerl_scanner.col+1}),
    strip(T6,S7);
scan_markup_decl("<!ENTITY" ++ T, S0) ->
    %% <!ENTITY [%] entity.name NDATA notation.name>
    %% <!ENTITY [%] entity.name "replacement text">
    %% <!ENTITY [%] entity.name SYSTEM "system.identifier">
    %% <!ENTITY [%] entity.name PUBLIC public.identifier "system.identifier">
    ?bump_col(8),
    {_,T1,S1} = mandatory_strip(T,S),
    {T2, S2} = scan_entity(T1, S1),
    strip(T2,S2);
scan_markup_decl("<!NOTATION" ++ T, S0) ->
    %% <!NOTATION notation.name "public.identifier" "helper.application">
    ?bump_col(10),
    {_,T1,S1} = mandatory_strip(T,S),
    {T2, S2} = scan_notation_decl(T1, S1),
    strip(T2,S2);
scan_markup_decl("<!ATTLIST" ++ T,
		 #xmerl_scanner{rules_read_fun = Read,
				rules_write_fun = Write,
				rules_delete_fun= Delete} = S0) ->
    %% <!ATTLIST Ename ( AttrName Type Value )*>
    ?bump_col(9),
    {_,T1,S1} = mandatory_strip(T,S),
    {Ename, _NamespaceInfo, T2, S2} = scan_name(T1, S1),
%    ?strip3,
    {Attributes, T4, S4} = scan_attdef(T2, S2),
    {EDEF,MergedAttrs} =
	case Read(elem_def, Ename, S4) of
	    undefined -> %% this may happen when the ELEMENT is declared in
		%% the external DTD but the ATTLIST in the
		%% internal DTD.
		{#xmlElement{},update_attributes(Attributes,[])};
	    Edef = #xmlElement{attributes = OldAttrs} ->
		Delete(elem_def,Ename,S4),
		%% the slot in rules table must be empty so that the
		%% later write has the assumed effect. Read maybe
		%% should empty the table slot.
		{Edef,update_attributes(Attributes, OldAttrs)}
	end,
    NewEdef = EDEF#xmlElement{name=Ename,attributes = MergedAttrs},
    S5 = Write(elem_def, Ename, NewEdef, S4),
    T5 = T4,
    strip(T5,S5);
scan_markup_decl(_Str,S) ->
    ?fatal(expected_markup,S).

scan_element_completion(T,S) ->
    scan_markup_completion_gt(T,S).

update_attributes(NewAttrs, OldAttrs) ->
    update_attributes1(NewAttrs,lists:reverse(OldAttrs)).

update_attributes1([A = {Name,_Type,_DefaultV,_DefaultD,_Env}|Attrs],
		   OldAttrs) ->
    case lists:keymember(Name, 1, OldAttrs) of
	true ->
	    update_attributes1(Attrs, OldAttrs);
	false ->
	    update_attributes1(Attrs, [A|OldAttrs])
    end;
update_attributes1([],Acc) ->
    lists:reverse(Acc).


%%%%%%% [53] AttDef

scan_attdef([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_attdef(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_attdef(T, S) ->
    scan_attdef(T, S, _AttrAcc = []).


scan_attdef([], S=#xmerl_scanner{continuation_fun = F}, Attrs) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_attdef(MoreBytes, S1, Attrs) end,
      fatal_fun(unexpected_end),
      S);
scan_attdef(">" ++ T, S0, Attrs) ->
    ?bump_col(1),
    {lists:reverse(Attrs), T, S};
scan_attdef("%" ++ _T, S=#xmerl_scanner{environment=prolog}, _Attrs) ->
     ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_attdef("%" ++ T, S0, Attrs) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2} = strip(ExpRef ++ T1,S1),
    scan_attdef(T2, S2, Attrs);
scan_attdef(T,S,Attrs) ->
    {_,T1,S1} = mandatory_strip(T,S),
    scan_attdef2(T1,S1,Attrs).

scan_attdef2(">" ++ T, S0, Attrs) ->
    ?bump_col(1),
    {lists:reverse(Attrs), T, S};
scan_attdef2("%" ++ _T, S=#xmerl_scanner{environment=prolog}, _Attrs) ->
     ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_attdef2("%" ++ T, S0, Attrs) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2} = strip(ExpRef ++ T1,S1),
    scan_attdef2(T2, S2, Attrs);
scan_attdef2(T, S, Attrs) ->
    {AttName, _NamespaceInfo, T1, S1} = scan_name(T, S),
    {_,T2,S2} = mandatory_strip(T1,S1),
    {AttType, T3, S3} = scan_att_type(T2, S2),
    {_,T4,S4} = mandatory_strip(T3,S3),
    {{DefaultDecl,DefaultValue}, T5, S5} = scan_default_decl(T4, S4, AttType),
    ?strip6,
    Attr = {AttName, AttType,DefaultValue,DefaultDecl,
	    S#xmerl_scanner.environment},
    scan_attdef2(T6, S6, [Attr|Attrs]).


%%% [54] StringType
scan_att_type([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_att_type(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_att_type("CDATA" ++ T, S0) ->
    ?bump_col(5),
    {'CDATA', T, S};
%%% [55] TokenizedType
scan_att_type("IDREFS" ++ T, S0) ->
    ?bump_col(6),
    {'IDREFS', T, S};
scan_att_type("IDREF" ++ T, S0) ->
    ?bump_col(5),
    {'IDREF', T, S};
scan_att_type("ID" ++ T, S0) ->
    ?bump_col(2),
    {'ID', T, S};
scan_att_type("ENTITY" ++ T, S0) ->
    ?bump_col(6),
    {'ENTITY', T, S};
scan_att_type("ENTITIES" ++ T, S0) ->
    ?bump_col(8),
    {'ENTITIES', T, S};
scan_att_type("NMTOKENS" ++ T, S0) ->
    ?bump_col(8),
    {'NMTOKENS', T, S};
scan_att_type("NMTOKEN" ++ T, S0) ->
    ?bump_col(7),
    {'NMTOKEN', T, S};
%%% [57] EnumeratedType
scan_att_type("NOTATION" ++ T, S0) ->
    ?bump_col(8),
    {_,T1,S1} = mandatory_strip(T,S),
    T2 = scan_mandatory("(",T1,1,S1,expected_parenthesis_after_NOTATION),
%    "(" ++ T2 = T1,
    S2 = S1,
    ?strip3,
    {Name, _NamespaceInfo, T4, S4} = scan_name(T3, S3),
    notation_exists(Name, S4),
    ?strip5,
    scan_notation_type(T5, S5, [Name]);
scan_att_type("(" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    {NmToken, _NamespaceInfo, T2, S2} = scan_nmtoken(T1, S1),
    ?strip3,
    scan_enumeration(T3, S3, [NmToken]);
scan_att_type("%" ++ _T, S=#xmerl_scanner{environment=prolog}) ->
    ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_att_type("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,in_literal),
    {ExpRef,T1,S1}.

%%% [58] NotationType

scan_notation_type([], S=#xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_notation_type(MoreBytes, S1, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_notation_type(")" ++ T, S0, Acc) ->
    ?bump_col(1),
    {{notation, lists:reverse(Acc)}, T, S};
scan_notation_type("|" ++ T, S0, Acc) ->
    ?bump_col(1),
    ?strip1,
    {Name, _NamespaceInfo, T2, S2} = scan_name(T1, S1),
    notation_exists(Name, S2),
    ?strip3,
    scan_notation_type(T3, S3, [Name | Acc]).

%%% Validity constraint for NotationType:
%%% The used notation names must be declared in the DTD, but they may
%%% be declared later.
notation_exists(Name, #xmerl_scanner{rules_read_fun = Read,
				     rules_write_fun = Write } = S) ->
    case Read(notation, Name, S) of
	undefined ->
	    %% this is legal, since the referenced NOTATION
	    %% may be declared later in internal or external
	    %% subset.
	    Write(notation,Name,undeclared,S);
	_Value ->
	    ok
    end.

%%% [59] Enumeration

scan_enumeration([], S=#xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_enumeration(MoreBytes, S1, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_enumeration(")" ++ T, S0, Acc) ->
    ?bump_col(1),
    {{enumeration, lists:reverse(Acc)}, T, S};
scan_enumeration("|" ++ T, S0, Acc) ->
    ?bump_col(1),
    ?strip1,
    {NmToken, _NamespaceInfo, T2, S2} = scan_nmtoken(T1, S1),
    ?strip3,
    scan_enumeration(T3, S3, [NmToken|Acc]).


%%%%%%% [60] DefaultDecl

scan_default_decl([], S=#xmerl_scanner{continuation_fun = F}, Type) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_default_decl(MoreBytes, S1, Type) end,
      fatal_fun(unexpected_end),
      S);
scan_default_decl("#REQUIRED" ++ T, S0, _Type) ->
    ?bump_col(9),
    {{'#REQUIRED',no_value}, T, S};
scan_default_decl("#IMPLIED" ++ T, S0, _Type) ->
    ?bump_col(8),
    {{'#IMPLIED',no_value}, T, S};
scan_default_decl("#FIXED" ++ T, S0, Type) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {Value,T2,S2,_} = default_value(T1, S1, Type),
    {{'#FIXED',Value},T2,S2};
scan_default_decl(Str, S, Type) ->
    {Value,T1,S1,_} = default_value(Str, S, Type),
    {{no_decl,Value},T1,S1}.


%% There is room here to validate against Type, but we don't do it at
%% the moment.
default_value(T, S, Type) ->
    {_Val, _T1, _S1,_} = scan_att_value(T, S, Type).


%%%%%%% [71] EntityDef

scan_entity([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_entity(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_entity("%" ++ T, #xmerl_scanner{rules_write_fun = Write} = S0) ->
    %% parameter entity
    ?bump_col(1),
    {_,T1,S1} = mandatory_strip(T,S),
    {PEName, _NamespaceInfo, T2, S2} = scan_name_no_colons(T1, S1),
    {_,T3,S3} = mandatory_strip(T2,S2),
    {PEDef, T4, S4} = scan_pe_def(T3, S3, PEName),
    ?strip5,
    {">" ++ T6,S6} = scan_entity_completion(T5,S5),
    S7 = Write(parameter_entity, PEName, PEDef, S6),
    {T6, S7};
scan_entity(T, #xmerl_scanner{rules_write_fun = Write,
			      rules_read_fun = Read,
			      rules_delete_fun = Delete} = S) ->
    %% generic entity
    {EName, _NamespaceInfo, T1, S1} = scan_name_no_colons(T, S),
    {_,T2,S2} = mandatory_strip(T1,S1),
    {EDef, EntType, T3, S3} = scan_entity_def(T2, S2, EName),
    check_entity_recursion(EName,S3),
    ?strip4,
    {">" ++ T5,S5} = scan_entity_completion(T4,S4),
    case Read(entity,EName,S5) of
	undeclared -> Delete(entity,EName,S5);
	_ -> ok
    end,
    S6 = Write(entity, EName, {S5#xmerl_scanner.environment,EntType,EDef}, S5),
    {T5, S6}.

scan_entity_completion(T,S) ->
    scan_markup_completion_gt(T,S).

%%%%%%% [73] EntityDef

scan_entity_def([], S=#xmerl_scanner{continuation_fun = F}, EName) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_entity_def(MoreBytes, S1, EName) end,
      fatal_fun(unexpected_end),
      S);
scan_entity_def("'" ++ T, S0, EName) ->
    ?bump_col(1),
    {EVal,T1,S1}=scan_entity_value(T, S, $', EName,general),
    {EVal,internal,T1,S1};
scan_entity_def("\"" ++ T, S0, EName) ->
    ?bump_col(1),
    {EVal,T1,S1}=scan_entity_value(T, S, $", EName,general),
    {EVal,internal,T1,S1};
%% external general entity, parsed or unparsed.
scan_entity_def(Str, S, EName) ->
    {ExtID, T1, S1} = scan_external_id(Str, S),
    {NData, T2, S2} = scan_ndata_decl(T1, S1),
    case NData of
	{ndata,_} ->
	    %% if NDATA exists it is an unparsed ENTITY
	    {{ExtID,NData},external,T2,S2};
	_ ->
	    case fetch_and_parse(ExtID,S2,
				 [{text_decl,true},
				  {environment,{external,{entity,EName}}}]) of
		{{_USret,Entity},_Tail,_Sx} ->
		    {Entity, external,T2, S2};
		{Entity,_Tail,Sx} ->
			OldRef=S2#xmerl_scanner.entity_references,
			NewRef=Sx#xmerl_scanner.entity_references,
		    {Entity,external,T2,
		     S2#xmerl_scanner{entity_references=OldRef++NewRef}};
		{error,enoent} -> % this bad entity is declared,
                                       % but it may not be referenced,
                                       % then it would not be an
                                       % error.
		    {{error,enoent},external,T2,S2}
	    end
    end.


scan_ndata_decl([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_ndata_decl(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_ndata_decl(Str = ">"++_T, S) ->
    {[], Str, S};
scan_ndata_decl(T, S) ->
    {_,T1,S1} = mandatory_strip(T,S),
    scan_ndata_decl2(T1,S1).
scan_ndata_decl2(Str = ">"++_T,S) ->
    {[], Str, S};
scan_ndata_decl2("NDATA" ++ T,S0 = #xmerl_scanner{rules_read_fun = Read,
						rules_write_fun = Write}) ->
    ?bump_col(5),
    {_,T1,S1} = mandatory_strip(T,S),
    {Name, _NamespaceInfo, T2, S2} = scan_name(T1, S1),
    case Read(notation, Name, S2) of
	undefined -> %% this is legal, since the referenced NOTATION
                     %% may be declared later in internal or external
                     %% subset.
	    Write(notation,Name,undeclared,S2),
	    {{ndata,Name},T2,S2};
	_Value ->
	    {{ndata, Name}, T2, S2}
    end.

%%%%%%% [39] element

scan_element(T, S, Pos) ->
    scan_element(T, S, Pos, S#xmerl_scanner.space,
		 _Lang = [], _Parents = [], #xmlNamespace{}).

scan_element(T, S=#xmerl_scanner{line=L,col=C},
	     Pos, SpaceDefault,Lang, Parents, NS) ->
    {Name, NamespaceInfo, T1, S1} = scan_name(T, S),
    vc_Element_valid(Name,NamespaceInfo,S),
    ?strip2,
    scan_element(T2, S2, Pos, Name, L, C, _Attrs = [],
		 Lang, Parents, NamespaceInfo, NS,
		 SpaceDefault).


scan_element("/", S=#xmerl_scanner{continuation_fun = F},
	     Pos, Name, StartL, StartC, Attrs, Lang, Parents,
	     NSI, NS, SpaceDefault) ->
    ?dbg("trailing / detected~n", []),
    F(fun(MoreBytes, S1) -> scan_element("/" ++ MoreBytes, S1,
					 Pos, Name, StartL, StartC, Attrs,
					 Lang,Parents,NSI,NS,SpaceDefault) end,
      fatal_fun(unexpected_end),
      S);
scan_element([], S=#xmerl_scanner{continuation_fun = F},
	     Pos, Name, StartL, StartC, Attrs, Lang, Parents,
	     NSI, NS, SpaceDefault) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_element(MoreBytes, S1,
					 Pos, Name, StartL, StartC, Attrs,
					 Lang,Parents,NSI,NS,SpaceDefault) end,
      fatal_fun(unexpected_end),
      S);
scan_element("/>" ++ T, S0 = #xmerl_scanner{hook_fun = Hook,
					    event_fun = Event,
					    line = L, col = C,
					    xmlbase_cache=XMLBase}, Pos,
	     Name, _StartL, _StartC, Attrs0, Lang, Parents, NSI,
	     Namespace, _SpaceDefault) ->
    ?bump_col(2),
    Attrs = lists:reverse(Attrs0),
    E=processed_whole_element(S, Pos, Name, Attrs, Lang, Parents,NSI,Namespace),

    #xmlElement{attributes = Attrs1} = E,
    wfc_unique_att_spec(Attrs1,S),
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = E}, S0),
    {Ret, S2} = Hook(E, S1),
    S2b=S2#xmerl_scanner{xmlbase=XMLBase},
    {Ret, T, S2b};
scan_element(">", S=#xmerl_scanner{continuation_fun = F},
	     Pos, Name, StartL, StartC, Attrs, Lang, Parents,
	     NSI, NS, SpaceDefault) ->
    ?dbg("trailing > detected~n", []),
    F(fun(MoreBytes, S1) -> scan_element(">" ++ MoreBytes, S1,
					 Pos, Name, StartL, StartC, Attrs,
					 Lang,Parents,NSI,NS,SpaceDefault) end,
      fatal_fun(unexpected_end),
      S);
scan_element(">" ++ T, S0 = #xmerl_scanner{event_fun = Event,
					   hook_fun = Hook,
					   line = L, col = C,
					   xmlbase_cache=XMLBase,
					   space = SpaceOption},
	     Pos, Name, StartL, StartC, Attrs0, Lang, Parents,
	     NSI, Namespace, SpaceDefault) ->
    ?bump_col(1),
    Attrs = lists:reverse(Attrs0),
    E0=processed_whole_element(S,Pos,Name,Attrs,Lang,Parents,NSI,Namespace),

    #xmlElement{attributes = Attrs1} = E0,
    wfc_unique_att_spec(Attrs1,S),
    XMLSpace = case lists:keysearch('xml:space', #xmlAttribute.name, Attrs1) of
		   false ->			SpaceDefault;
		   {value, #xmlAttribute{value="default"}} ->	SpaceOption;
		   {value, #xmlAttribute{value="preserve"}} ->	preserve;
		   _ ->				SpaceDefault
	       end,

    E0=processed_whole_element(S,Pos,Name,Attrs1,Lang,Parents,NSI,Namespace),
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = started,
					       line = StartL,
					       col = StartC,
					       data = E0}, S),

    {Content, T1, S2} = scan_content(T, S1, Name, Attrs1, XMLSpace,
				     E0#xmlElement.language,
				     [{Name, Pos}|Parents], Namespace),

    Element=E0#xmlElement{content=Content,
			  xmlbase=E0#xmlElement.xmlbase},
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
					       line = L,
					       col = C,
					       data = Element}, S2),
    {Ret, S4} = Hook(Element, S3),
    S4b=S4#xmerl_scanner{xmlbase=XMLBase},
    {Ret, T1, S4b};
scan_element(T, S, Pos, Name, StartL, StartC, Attrs, Lang, Parents,
	     NSI, NS, SpaceDefault) ->
    {AttName, NamespaceInfo, T1, S1} = scan_name(T, S),
    {T2, S2} = scan_eq(T1, S1),
    {AttType,_DefaultDecl} = get_att_type(S2,AttName,Name),
    {AttValue, T3a, S3a,IsNorm} = scan_att_value(T2, S2, AttType),
%%    check_default_value(S3,DefaultDecl,AttValue),
    NewNS = check_namespace(AttName, NamespaceInfo, AttValue, NS),
    {T3,S3} = wfc_whitespace_betw_attrs(T3a,S3a),
    ?strip4,
    AttrPos = case Attrs of
		  [] ->
		      1;
		  [#xmlAttribute{pos = P}|_] ->
		      P+1
	      end,
    Attr = #xmlAttribute{name = AttName,
			 parents = [{Name, Pos}|Parents],
			 pos = AttrPos,
			 language = Lang,
			 nsinfo = NamespaceInfo,
			 value = AttValue,
			 normalized = IsNorm},
    XMLBase=if
		AttName=='xml:base' ->
		    resolve_relative_uri(AttValue,S4#xmerl_scanner.xmlbase);
		true ->
		    S4#xmerl_scanner.xmlbase
	    end,

    #xmerl_scanner{event_fun = Event,
		   line = Line,
		   col = Col} = S4,
    S5 = Event(#xmerl_event{event = ended,
			    line = Line,
			    col = Col,
			    data = Attr},
	       S4#xmerl_scanner{xmlbase=XMLBase,
				xmlbase_cache=S#xmerl_scanner.xmlbase}),
    scan_element(T4, S5, Pos, Name, StartL, StartC, [Attr|Attrs],
		 Lang, Parents, NSI, NewNS, SpaceDefault).

get_default_attrs(S = #xmerl_scanner{rules_read_fun = Read}, ElemName) ->
    case Read(elem_def, ElemName, S) of
	#xmlElement{attributes = Attrs} ->
	    [ {AttName, AttValue} ||
	      {AttName, _, AttValue, _, _} <- Attrs, AttValue =/= no_value ];
	_ -> []
    end.

get_att_type(S=#xmerl_scanner{rules_read_fun=Read},AttName,ElemName) ->
    case Read(elem_def,ElemName,S) of
	#xmlElement{attributes = Attrs} ->
	    case lists:keysearch(AttName,1,Attrs) of
		{value,{_,AttType,_,DefaultDecl,_}} ->
		    {AttType,DefaultDecl};
		_ -> {'CDATA',no_value} %% undefined attribute shall be treated as CDATA
	    end;
	_ -> {'CDATA',no_value}
    end.

resolve_relative_uri(NewBase="/"++_,CurrentBase) ->
    case xmerl_uri:parse(CurrentBase) of
	{error,_Reason} ->
	    NewBase;
	{Scheme,Host,Port,_Path,_Query} ->
	    atom_to_list(Scheme)++Host++":"++integer_to_list(Port)++NewBase
    end;
resolve_relative_uri(NewBase,CurrentBase) ->
     filename:join(CurrentBase,NewBase).


processed_whole_element(S=#xmerl_scanner{hook_fun = _Hook,
					 xmlbase = XMLBase,
					 line = _L, col = _C,
					 event_fun = _Event},
			Pos, Name, Attrs, Lang, Parents, NSI, Namespace) ->
    Language = check_language(Attrs, Lang),

    AllAttrs =
	case S#xmerl_scanner.default_attrs of
	    true ->
            DefaultAttrs =
                [ #xmlAttribute{name = AttName,
                                parents = [{Name, Pos} | Parents],
                                language = Lang,
                                nsinfo = NSI,
                                namespace = Namespace,
                                value = AttValue,
                                normalized = true} ||
                  {AttName, AttValue} <- get_default_attrs(S, Name),
                  AttValue =/= no_value,
                  not lists:keymember(AttName, #xmlAttribute.name, Attrs) ],
            lists:append(Attrs, DefaultAttrs);
	    false ->
		Attrs
	end,

    {ExpName, ExpAttrs} =
	case S#xmerl_scanner.namespace_conformant of
	    true ->
		%% expand attribute names. We need to do this after having
		%% scanned all attributes of the element, since (as far as
		%% I can tell), XML Names only specifies that namespace attrs
		%% are valid within the whole scope of the element in which
		%% they are declared, which should also mean that even if they
		%% are declared after some other attributes, the namespace
		%% should apply to those attributes as well.
		%% Note that the default URI does not apply to attrbute names.
		TempNamespace = Namespace#xmlNamespace{default = []},
		ExpAttrsX =
		    [A#xmlAttribute{
		       namespace=Namespace,
		       expanded_name=expanded_name(
				       A#xmlAttribute.name,
				       A#xmlAttribute.nsinfo,
						% NSI,
				       TempNamespace, S)} || A <- AllAttrs],
		{expanded_name(Name, NSI, Namespace, S), ExpAttrsX};
	    false ->
		{Name, AllAttrs}
	end,

    #xmlElement{name = Name,
		xmlbase = XMLBase,
		pos = Pos,
		parents = Parents,
		attributes = ExpAttrs,
		language = Language,
		expanded_name = ExpName,
		nsinfo = NSI,
		namespace = Namespace}.


check_language([#xmlAttribute{name='xml:lang',value=Lang}|_], _) ->
    Lang;
check_language([_|T], Lang) ->
    check_language(T, Lang);
check_language([], Lang) ->
    Lang.


check_namespace(xmlns, _, Value, NS) ->
    NS#xmlNamespace{default = list_to_atom(Value)};
check_namespace(_, {"xmlns", Prefix}, Value,
		NS = #xmlNamespace{nodes = Ns}) ->
    NS#xmlNamespace{nodes = keyreplaceadd(
			      Prefix, 1, Ns, {Prefix, list_to_atom(Value)})};
check_namespace(_, _, _, NS) ->
    NS.


expanded_name(Name, [], #xmlNamespace{default = []}, _S) ->
    Name;
expanded_name(Name, [], #xmlNamespace{default = URI}, S) ->
    case URI of
	'http://www.w3.org/XML/1998/namespace' ->
	    ?fatal(cannot_bind_default_namespace_to_xml_namespace_name, S);
	'http://www.w3.org/2000/xmlns/' ->
	    ?fatal(cannot_bind_default_namespace_to_xmlns_namespace_name, S);
	_ ->
	    {URI, Name}
    end;
expanded_name(Name, N = {"xmlns", Local}, #xmlNamespace{nodes = Ns}, S) ->
    {_, Value} = lists:keyfind(Local, 1, Ns),
    case Name of
	'xmlns:xml' when Value =:= 'http://www.w3.org/XML/1998/namespace' ->
	    N;
        'xmlns:xml' when Value =/= 'http://www.w3.org/XML/1998/namespace' ->
	    ?fatal({xml_prefix_cannot_be_redeclared, Value}, S);
	'xmlns:xmlns' ->
	    ?fatal({xmlns_prefix_cannot_be_declared, Value}, S);
	_ ->
	    case Value of
		'http://www.w3.org/XML/1998/namespace' ->
		    ?fatal({cannot_bind_prefix_to_xml_namespace, Local}, S);
		'http://www.w3.org/2000/xmlns/' ->
		    ?fatal({cannot_bind_prefix_to_xmlns_namespace, Local}, S);
		_ ->
		    N
	    end
    end;
expanded_name(_Name, {"xml", Local}, _NS, _S) ->
    {'http://www.w3.org/XML/1998/namespace', list_to_atom(Local)};
expanded_name(_Name, {Prefix, Local}, #xmlNamespace{nodes = Ns}, S) ->
    case lists:keysearch(Prefix, 1, Ns) of
	{value, {_, URI}} ->
	    {URI, list_to_atom(Local)};
	false ->
	    %% A namespace constraint of XML Names is that the prefix
	    %% must be declared
	    ?fatal({namespace_prefix_not_declared, Prefix}, S)
    end.

keyreplaceadd(K, Pos, [H|T], Obj) when K == element(Pos, H) ->
    [Obj|T];
keyreplaceadd(K, Pos, [H|T], Obj) ->
    [H|keyreplaceadd(K, Pos, T, Obj)];
keyreplaceadd(_K, _Pos, [], Obj) ->
    [Obj].

%%%%%%% [10] AttValue
%% normalize the attribute value according to XML 1.0 section 3.3.3

scan_att_value([], S=#xmerl_scanner{continuation_fun = F},AT) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_att_value(MoreBytes, S1, AT) end,
      fatal_fun(unexpected_end),
      S);
scan_att_value("%"++_T,S=#xmerl_scanner{environment=prolog},_AttType) ->
    ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_att_value("%"++T,S0=#xmerl_scanner{rules_read_fun=Read,
					rules_write_fun=Write,
					rules_delete_fun=Delete},AttType) ->
    ?bump_col(1),
    {Name,T1,S1} = scan_pe_reference(T,S),
    {ExpandedRef,S2} =
	case expand_pe_reference(Name,S1,in_literal) of
	    Tuple when is_tuple(Tuple) ->
		%% {system,URI} or {public,URI}
		%% Included in literal, just get external file.
		{ExpRef,Sx}=fetch_not_parse(Tuple,S1),
		{EntV,_,_S2} = scan_entity_value(ExpRef, Sx, no_delim,
						Name,parameter),
		%% should do an update Write(parameter_entity) so next
		%% expand_pe_reference is faster
		Delete(parameter_entity,Name,_S2),
		_S3 = Write(parameter_entity,Name,EntV,_S2),
		EntV2 = Read(parameter_entity,Name,_S3),
		{EntV2,_S3};
	    ExpRef ->
		{ExpRef,S1}
	end,
    {_,T2,S3} = strip(ExpandedRef ++ T1,S2),
    scan_att_value(T2,S3,AttType);
scan_att_value([H|T], S0,'CDATA'=AT) when H == $"; H == $' ->
    ?bump_col(1),
    scan_att_chars(T, S, H, [],[], AT,false);
scan_att_value([H|T], S0,AttType) when H == $"; H == $' ->
    ?bump_col(1),
    {T1,S1,IsNorm} = normalize(T,S,false),
    scan_att_chars(T1, S1, H, [],[], AttType,IsNorm).

scan_att_chars([],S=#xmerl_scanner{continuation_fun=F},H,Acc,TmpAcc,AT,IsNorm)->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_att_chars(MoreBytes, S1, H, Acc,TmpAcc,AT,IsNorm)
      end,
      fatal_fun(unexpected_end),
      S);
scan_att_chars([H|T], S0, H, Acc, TmpAcc,AttType,IsNorm) -> % End quote
    ?bump_col(1),
    check_att_default_val(S#xmerl_scanner.validation,TmpAcc,AttType,S),
    {Acc2,S2,IsNorm2} =
	if
	    AttType == 'CDATA' -> {Acc,S,IsNorm};
	    true ->
		normalize(Acc,S,IsNorm)
	end,
    {lists:flatten(lists:reverse(Acc2)), T, S2,IsNorm2};
scan_att_chars("&" ++ T, S0, Delim, Acc, TmpAcc,AT,IsNorm) -> % Reference
    ?bump_col(1),
    {ExpRef, T1, S1} = scan_reference(T, S),
    case markup_delimeter(ExpRef) of
	true ->
	    scan_att_chars(T1,S1,Delim,[ExpRef|Acc],[ExpRef|TmpAcc],AT,IsNorm);
	_ ->
            case T of
                "#" ++ _ ->
                    %% normalization rules (sec 3.3.3) require that for
                    %% character references, the referenced character be
                    %% added directly to the normalized value
                    {T2,S2,IsNorm2} =
                        if
                            ?whitespace(hd(ExpRef)) ->
                                normalize(T1, S1, IsNorm);
                            true ->
                                {T1, S1, IsNorm}
                        end,
                    scan_att_chars(T2, S2, Delim, ExpRef ++ Acc, TmpAcc, AT, IsNorm2);
                _ ->
                    Ch = string_to_char_set(S#xmerl_scanner.encoding, ExpRef),
                    scan_att_chars(Ch ++ T1, S1, Delim, Acc, TmpAcc, AT, IsNorm)
            end
    end;
scan_att_chars("<" ++ _T, S0, _Delim, _Acc,_, _,_) -> % Tags not allowed here
    ?fatal(unexpected_char, S0);
scan_att_chars([H|T], S0, Delim, Acc, _TmpAcc,'CDATA',IsNorm)
  when ?whitespace(H) ->
    ?bump_col(1),
    scan_att_chars(T, S, Delim, [$\s|Acc], [],'CDATA',IsNorm);
scan_att_chars([H|T], S0, Delim, Acc, TmpAcc,AT,IsNorm)
  when ?whitespace(H) ->
    ?bump_col(1),
    {T1,S1,IsNorm2} = normalize(T,S,IsNorm),
    check_att_default_val(S#xmerl_scanner.validation,TmpAcc,AT,S1),
    scan_att_chars(T1, S1, Delim, [$\s|Acc],[], AT,IsNorm2);
scan_att_chars(Str, S0, Delim, Acc, TmpAcc,AT,IsNorm) ->
    ?bump_col(1),
    {Ch,T} = to_ucs(S#xmerl_scanner.encoding,Str),
    valid_Char(S#xmerl_scanner.validation,AT,Ch,S),
    scan_att_chars(T, S, Delim, [Ch|Acc], [Ch|TmpAcc],AT,IsNorm).

markup_delimeter("&")->    true;
markup_delimeter("\"") ->  true;
markup_delimeter("\'") ->  true;
markup_delimeter("<") ->   true;
markup_delimeter(">") ->   true;
markup_delimeter("%") ->   true;
markup_delimeter(_) ->     false.

check_att_default_val(dtd,[],_Ent,_S) ->
    ok;
check_att_default_val(dtd,RevName,Ent,S) ->
    check_att_default_val(lists:reverse(RevName),Ent,S);
check_att_default_val(_,_,_,_) ->
    ok.

check_att_default_val(Name,Ent,S=#xmerl_scanner{rules_write_fun=Write})
  when Ent == 'ENTITY'; Ent == 'ENTITIES' ->
    case xmerl_lib:is_letter(hd(Name)) of
	true -> ok;
	_ -> ?fatal({illegal_first_character,Ent,Name},S)
    end,
    SName = list_to_atom(Name),
    Write(entity,SName,undeclared,S);
check_att_default_val(Name,IDR,S=#xmerl_scanner{rules_write_fun=Write})
  when IDR == 'IDREF'; IDR == 'IDREFS' ->
    case xmerl_lib:is_letter(hd(Name)) of
	true -> ok;
	_ -> ?fatal({illegal_first_character,IDR,Name},S)
    end,
    SName = list_to_atom(Name),
    Write(id,SName,undeclared,S);
check_att_default_val(Name,'ID',S=#xmerl_scanner{rules_write_fun=Write,
						 rules_read_fun=Read,
						 rules_delete_fun=Delete}) ->
    case xmerl_lib:is_name(Name) of
	false ->
	    ?fatal({'ID_names_must_be_Name_production',Name},S);
	_ ->
	    ok
    end,
    SName = if
		is_list(Name) -> list_to_atom(Name);
		true -> Name
	    end,
    case Read(id,SName,S) of
	undeclared -> %% was referenced in IDREF/IDREFS before defined
	    Delete(id,SName,S);
	SName -> ?fatal({values_must_be_unique,'ID',SName},S);
	undefined -> ok
    end,
    Write(id,SName,SName,S);
check_att_default_val(_,_,_) ->
    ok.

valid_Char(dtd,AT,C,S) when AT=='NMTOKEN';AT=='NMTOKENS' ->
    vc_Valid_Char(AT,C,S);
valid_Char(_,_,[C],S) ->
    case xmerl_lib:is_char(C) of
	true ->
	    ok;
	false ->
	    ?fatal({unexpected_char,C}, S)
    end;
valid_Char(_,_,C,S) ->
    case xmerl_lib:is_char(C) of
	true ->
	    ok;
	false ->
	    ?fatal({unexpected_char,C}, S)
    end.



%%%%%%% [43] content

scan_content(T, S, Name, Attrs, Space, Lang, Parents, NS) ->
    scan_content(T, S, _Pos = 1, Name, Attrs, Space,
                 Lang, Parents, NS, _Acc = [],_MarkupDel=[]).

scan_content("<", S= #xmerl_scanner{continuation_fun = F},
            Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,_) ->
    ?dbg("trailing < detected~n", []),
    F(fun(MoreBytes, S1) -> scan_content("<" ++ MoreBytes, S1,
					 Pos, Name, Attrs,
					 Space, Lang, Parents, NS, Acc,[]) end,
      fatal_fun(unexpected_end),
      S);
scan_content([], S=#xmerl_scanner{environment={external,{entity,_}}},
             _Pos, _Name, _Attrs, _Space, _Lang, _Parents, _NS, Acc,_) ->
    {lists:reverse(Acc),[],S};
scan_content([], S=#xmerl_scanner{environment=internal_parsed_entity},
             _Pos, _Name, _Attrs, _Space, _Lang, _Parents, _NS, Acc,_) ->
    {lists:reverse(Acc),[],S};
scan_content([], S=#xmerl_scanner{continuation_fun = F},
             Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,_) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_content(MoreBytes, S1,
					 Pos, Name, Attrs,
					 Space, Lang, Parents, NS, Acc,[]) end,
      fatal_fun(unexpected_end),
      S);
scan_content("</" ++ T, S0, _Pos, Name, _Attrs, _Space, _Lang,
	     _Parents, _NS, Acc,[]) ->
    ?bump_col(2),
    {ETagName, _NamespaceInfo, T1, S1} = scan_name(T, S),
    if ETagName == Name ->
            ok;
       true ->
            ?fatal({endtag_does_not_match, {was,ETagName,should_have_been, Name}}, S)
    end,
    ?strip2,
    case T2 of
        ">" ++ T3 ->
            {lists:reverse(Acc), T3, S2};
        _ ->
	    ?fatal({error,{unexpected_end_of_STag}},S)
    end;
scan_content([$&|_T]=Str,
	     #xmerl_scanner{environment={external,{entity,EName}}} = S0,
	     Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,_) ->
    {_EntV,T1,S1}=scan_entity_value(Str,S0 ,[],EName,general),
    %%This is a problem. All referenced entities in the external entity must be checked for recursion, thus parse the contentbut,skip result.
    scan_content(T1,S1,Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,[]);
scan_content("&"++T,
	     #xmerl_scanner{environment=internal_parsed_entity} = S,
	     Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,_) ->
    {_, T1, S1} = scan_reference(T, S),
    scan_content(T1,S1,Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,[]);
scan_content("&" ++ T, S0, Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,[]) ->
    ?bump_col(1),
    {ExpRef, T1, S1} = scan_reference(T, S),
    case markup_delimeter(ExpRef) of
	true -> scan_content(ExpRef++T1,S1,Pos,Name,Attrs,Space,Lang,Parents,NS,Acc,ExpRef);
	_ ->
	    scan_content(string_to_char_set(S1#xmerl_scanner.encoding,ExpRef)++T1,S1,Pos,Name,Attrs,Space,Lang,Parents,NS,Acc,[])
    end;
scan_content("<!--" ++ T, S0=#xmerl_scanner{acc_fun = F, comments=CF}, Pos, Name, Attrs, Space,
	     Lang, Parents, NS, Acc,[]) ->
    ?bump_col(4),
    {C, T1, S1} = scan_comment(T, S, Pos, Parents, Lang),
    case CF of
	true ->
	    {Acc2, Pos2, S3} =
		case F(C, Acc, S1) of
		    {Acc1, S2} ->
			{Acc1, Pos + 1, S2};
		    {Acc1, Pos1, S2} ->
			{Acc1, Pos1, S2}
		end,
	    scan_content(T1, S3, Pos2, Name, Attrs, Space, Lang, Parents, NS, Acc2,[]);
	false ->
	    scan_content(T1, S1, Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,[])
    end;
scan_content("<" ++ T, S0, Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,[]) ->
    ?bump_col(1),
    {Markup, T1, S1} =
        scan_content_markup(T, S, Pos, Name, Attrs, Space, Lang, Parents, NS),
    AccF = S1#xmerl_scanner.acc_fun,
    {NewAcc, NewPos, NewS} = case AccF(Markup, Acc, S1) of
				 {Acc2, S2} ->
				     {Acc2, Pos+1, S2};
				 {Acc2, Pos2, S2} ->
				     {Acc2, Pos2, S2}
			     end,
    scan_content(T1, NewS, NewPos, Name, Attrs, Space, Lang,
		 Parents, NS, NewAcc,[]);
scan_content([_H|T], S= #xmerl_scanner{environment={external,{entity,_}}},
 	     Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,_) ->
    %% Guess we have to scan the content to find any internal entity
    %% references.
    scan_content(T,S,Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,[]);
scan_content(T, S=#xmerl_scanner{acc_fun = F,
				 event_fun = Event,
				 hook_fun=Hook,
				 line = _L},
             Pos, Name, Attrs, Space, Lang, Parents, NS, Acc,MarkupDel) ->
    Text0 = #xmlText{pos = Pos,
                     parents = Parents},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = started,
                                               line = S#xmerl_scanner.line,
                                               data = Text0}, S),
    {Data, T1, S2} =  scan_char_data(T, S1, Space,MarkupDel),
    Text = Text0#xmlText{value = Data},
    {Ret,S2b} = Hook(Text,S2),
    S3 = #xmerl_scanner{} = Event(#xmerl_event{event = ended,
                                               line = S2b#xmerl_scanner.line,
                                               data = Ret}, S2b),
    {NewAcc, NewPos, NewS} = case F(Ret, Acc, S3) of
				 {Acc4, S4} ->
				     {Acc4, Pos+1, S4};
				 {Acc4, Pos4, S4} ->
				     {Acc4, Pos4, S4}
			     end,
    scan_content(T1, NewS, NewPos, Name, Attrs, Space, Lang,
		 Parents, NS, NewAcc,[]).


scan_content_markup([], S=#xmerl_scanner{continuation_fun = F},
		    Pos, Name, Attrs, Space, Lang, Parents, NS) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_content_markup(
			      MoreBytes,S1,Pos,Name,
			      Attrs,Space,Lang,Parents,NS) end,
      fatal_fun(unexpected_end),
      S);
scan_content_markup("![CDATA[" ++ T, S0, Pos, _Name, _Attrs,
		    _Space, _Lang, Parents, _NS) ->
    ?bump_col(8),
    scan_cdata(T, S, Pos, Parents);
scan_content_markup("?"++T,S0,Pos,_Name,_Attrs,_Space,_Lang,Parents,_NS) ->
    ?bump_col(1),
    scan_pi(T, S, Pos, Parents);
scan_content_markup(T, S, Pos, _Name, _Attrs, Space, Lang, Parents, NS) ->
    scan_element(T, S, Pos, Space, Lang, Parents, NS).

scan_char_data(T, S, Space,MUD) ->
    scan_char_data(T, S, Space,MUD, _Acc = []).

%%%%%%% [14] CharData

scan_char_data([], S=#xmerl_scanner{environment={external,{entity,_}}},
	       _Space,_MUD, Acc) ->

    {lists:reverse(Acc), [], S};
scan_char_data([], S=#xmerl_scanner{environment=internal_parsed_entity},
	       _Space, _MUD,Acc) ->

    {lists:reverse(Acc), [], S};
scan_char_data([], S=#xmerl_scanner{continuation_fun = F}, Space, _MUD,Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_char_data(MoreBytes,S1,Space,_MUD,Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_char_data([$&|T], S,Space,"&",Acc) ->
    scan_char_data(T, S, Space,[], [$&|Acc]);
scan_char_data(T=[$&|_], S,_Space,_MUD,Acc) ->

    {lists:reverse(Acc), T, S};
scan_char_data("]]>" ++ _T, S, _Space,_MUD, _Acc) ->
    %% See Section 2.4: Especially:
    %% "The right angle bracket (>) MAY be represented using the string "&gt;",
    %% and MUST, for compatibility, be escaped using either "&gt;" or a
    %% character reference when it appears in the string "]]>" in content, when
    %% that string is not marking the end of a CDATA section.
    ?fatal(unexpected_cdata_end, S);
scan_char_data([$<|T],S,Space,"<", Acc) ->
    scan_char_data(T, S, Space,[], [$<|Acc]);
scan_char_data(T = [$<|_], S, _Space,_MUD,Acc) ->

    {lists:reverse(Acc), T, S};
scan_char_data(T = [H|R], S, Space,MUD, Acc) when ?whitespace(H) ->
    if
        MUD =:= [], Acc =:= [], H =:= $\n, Space =:= preserve ->
            case fast_accumulate_whitespace(R, S, T) of
                {done, Reply} ->
                    Reply;
                {NewAcc, T1, S1} ->
                    scan_char_data(T1, S1, Space, MUD, NewAcc)
            end;
        true ->
            {NewAcc, T1, S1} = accumulate_whitespace(T, S, Space, Acc),
            scan_char_data(T1, S1, Space,MUD,NewAcc)
    end;
scan_char_data([H1,H2|_T],S,_Space,_MUD,_Acc) when ?non_character(H1,H2) ->
    ?fatal({error,{not_allowed_to_use_Unicode_noncharacters}},S);
scan_char_data("]]>"++_T,S,_Space,_MUD,_Acc) ->
    ?fatal({error,{illegal_character_in_content,"]]>"}},S);
scan_char_data(Str,S0,Space,MUD,Acc) ->
    ?bump_col(1),
    {Ch,T} = wfc_legal_char(Str,S),
    scan_char_data(T,S,Space,MUD,[Ch|Acc]).



%%%%%%% [18]-[21] CDATA

scan_cdata(Str, S, Pos, Parents) ->
    scan_cdata(Str, S, Pos, Parents, _Acc = []).


scan_cdata([], S=#xmerl_scanner{continuation_fun = F}, Pos, Parents, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_cdata(MoreBytes, S1, Pos, Parents, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_cdata("]]>" ++ T, S0, Pos, Parents, Acc) ->
    ?bump_col(3),
    {#xmlText{pos = Pos,
	      parents = Parents,
	      value = lists:reverse(Acc),
	      type = cdata}, T, S};
scan_cdata(Str, S0, Pos, Parents, Acc) ->
    {Ch,T} = to_ucs(S0#xmerl_scanner.encoding,Str),
    case xmerl_lib:is_char(Ch) of
	true ->
	    ?bump_col(1),
	    scan_cdata(T, S, Pos, Parents, [Ch|Acc]);
	false ->
	    ?fatal({unexpected_char,Ch}, S0)
    end.


%%%%%%% [67] Reference
%% returns a three tuple {Result,RestBuf,State}

scan_reference([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_reference(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_reference("#x" ++ T, S0) ->
    %% [66] CharRef
    ?bump_col(1),
    if hd(T) /= $; ->
	    scan_char_ref_hex(T, S, 0);
       true ->
	    ?fatal(invalid_char_ref, S)
    end;
scan_reference("#" ++ T, S0) ->
    %% [66] CharRef
    ?bump_col(1),
    if hd(T) /= $; ->
	    scan_char_ref_dec(T, S, []);
       true ->
	    ?fatal(invalid_char_ref, S)
    end;
scan_reference(T, S) ->
    case catch scan_entity_ref(T, S) of
	{'EXIT', _} ->
	    ?fatal(error_scanning_entity_ref,S);
	Other ->
	    Other
    end.


%% Chapter 4.4.2: ... the replacement text of entities used to escape
%% markup delimiters (the entities amp, lt, gt, apos, quot) is always treated
%% as data. (The string "AT&amp;T;" expands to "AT&T;" and the remaining
%% ampersand is not recognized as an entity-reference delimiter.)"
%%
%% How to achieve this? My current approach is to insert the *strings* "&",
%% "<", ">", "'", and "\"" instead of the characters. The processor will
%% ignore them when performing multiple expansions. This means, for now, that
%% the character data output by the processor is (1-2 levels) deep.
%% At some suitable point, we should flatten these, so that application-level
%% processors should not have to be aware of this detail.

scan_entity_ref([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_entity_ref(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_entity_ref("amp;" ++ T, S0) ->
    ?bump_col(4),
    {"&", T, S};
scan_entity_ref("lt;" ++ T, S0) ->
    ?bump_col(3),
    {"<", T, S};
scan_entity_ref("gt;" ++ T, S0) ->
    ?bump_col(3),
    {">", T, S};
scan_entity_ref("apos;" ++ T, S0) ->
    ?bump_col(5),
    {"'", T, S};
scan_entity_ref("quot;" ++ T, S0) ->
    ?bump_col(5),
    {"\"", T, S};
scan_entity_ref(T, S) ->
    {Name, _NamespaceInfo, T1, S1} = scan_name(T, S),
    T2 = scan_mandatory(";",T1,1,S1,expected_entity_reference_semicolon),
%    ";" ++ T2 = T1,
    S2 = S1,
    Entity = expand_reference(Name, S2),
    {Entity, T2, S2}.


%%%%%%% [69] PEReference

scan_pe_reference(T, S) ->
    {Name, _NamespaceInfo, T1, S1} = scan_name(T, S),
    T2 = scan_mandatory(";",T1,1,S1,expected_parsed_entity_reference_semicolon),
%    ";" ++ T2 = T1,
    {Name, T2, S1#xmerl_scanner{col = S1#xmerl_scanner.col+1}}.

expand_pe_reference(Name, #xmerl_scanner{rules_read_fun = Read} = S,WS) ->
    case Read(parameter_entity, Name, S) of
	undefined ->
	    ?fatal({unknown_parameter_entity, Name}, S); % WFC or VC failure
	Err={error,_Reason} ->
	    ?fatal(Err,S);
	Tuple when is_tuple(Tuple) ->
	    Tuple;
	Result ->
	    if
		WS == in_literal -> Result;
		true -> " "++Result++" "
	    end
    end.

% Currently unused
%
% expand_external_pe_reference(Name, #xmerl_scanner{rules_read_fun = Read} = S) ->
%     case Read(parameter_entity, Name, S) of
% 	undefined ->
% 	    ?fatal({unknown_parameter_entity, Name}, S);
% 	Result ->
% 	    fetch_DTD(Result,S)
%     end.


%%%%%%% [68] EntityReference

expand_reference(Name, #xmerl_scanner{environment={external,{entity,_}}}) ->
    atom_to_list(Name);
expand_reference(Name, #xmerl_scanner{environment=internal_parsed_entity}) ->
    atom_to_list(Name);
expand_reference(Name, #xmerl_scanner{rules_read_fun = Read} = S) ->
    case Read(entity, Name, S) of
	undefined ->
	    ?fatal({unknown_entity_ref, Name}, S);
	{_,external,{error,enoent}} ->
	    ?fatal({error,{entity_target_not_found,{error,enoent},Name}},S);
	{DefEnv,EntType,Value} ->
	    wfc_Entity_Declared(DefEnv,S,Name),
	    Value2 = string_to_char_set(S#xmerl_scanner.encoding,Value),
	    wfc_Internal_parsed_entity(EntType,Value2,S),
	    Value
    end.


%%%%%%% [66] CharRef

scan_char_ref_dec([], S=#xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_char_ref_dec(MoreBytes, S1, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_char_ref_dec([H|T], S0, Acc) when H >= $0, H =< $9 ->
    ?bump_col(1),
    scan_char_ref_dec(T, S, [H|Acc]);
scan_char_ref_dec(";" ++ T, S0, Acc) ->
    ?bump_col(1),
    Ref = list_to_integer(lists:reverse(Acc)),
    {Ch,_} = wfc_legal_char(Ref,S),
    {[Ch], T, S}. %% changed return value from [[Ref]]


scan_char_ref_hex([], S=#xmerl_scanner{continuation_fun = F}, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_char_ref_hex(MoreBytes, S1, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_char_ref_hex([H|T], S0, Acc) when H >= $0, H =< $9 ->
    ?bump_col(1),
    Dec = H - $0,
    scan_char_ref_hex(T, S, (Dec bor (Acc bsl 4)));
scan_char_ref_hex([H|T], S0, Acc) when H >= $a, H =< $f ->
    ?bump_col(1),
    Dec = (H - $a) + 10,
    scan_char_ref_hex(T, S, (Dec bor (Acc bsl 4)));
scan_char_ref_hex([H|T], S0, Acc) when H >= $A, H =< $F ->
    ?bump_col(1),
    Dec = (H - $A) + 10,
    scan_char_ref_hex(T, S, (Dec bor (Acc bsl 4)));
scan_char_ref_hex(";" ++ T, S0, Acc) ->
    ?bump_col(1),
    {Ch,_} = wfc_legal_char(Acc,S),
    {[Ch], T, S}. %% changed return value from [[Acc]]



%%%%%%% [25] Eq
%%% Eq    ::=    S? '=' S?
scan_eq(T, S) ->
    ?strip1,
    case T1 of
 	[$=|T2] ->
	    S2 = S1#xmerl_scanner{col=S1#xmerl_scanner.col+1},
	    ?strip3,
	    {T3, S3};
	_ ->
	    ?fatal(assignment_expected,S)
    end.


%% scan_name/2
%%
%% We perform some checks here to make sure that the names conform to
%% the "Namespaces in XML" specification. This is an option.
%%
%% Qualified Name:
%% [6]      QName ::= (Prefix ':')? LocalPart
%% [7]     Prefix ::= NCName
%% [8]  LocalPart ::= NCName
%% [4]     NCName ::= (Letter | '_') (NCNameChar)*
%% [5] NCNameChar ::= Letter | Digit | '.' | '-' | '_'
%%                    | CombiningChar | Extender


%% The effect of XML Names (namespace) conformance is that:
%% - All element types and attribute names contain either zero or one colon
%% - No entity names, PI targets, or notation names contain any colons.
%%
%% scan_name_no_colons/2 will ensure that the name contains no colons iff
%% the scanner has been told to be namespace conformant. Otherwise, it will
%% behave exactly like scan_name/2.
%%
scan_name_no_colons(Str, S) ->
    NSC = S#xmerl_scanner.namespace_conformant,
    case NSC of
	true ->
	    {Target, NSI, T1, S1} =
		scan_name(Str,S#xmerl_scanner{namespace_conformant=no_colons}),
	    {Target,NSI,T1,S1#xmerl_scanner{namespace_conformant=NSC}};
	false ->
	    scan_name(Str, S)
    end.



%% [5] Name ::= (Letter | '_' | ':') (NameChar)*
scan_name([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_name(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_name(Str = [$:|T], S0 = #xmerl_scanner{namespace_conformant = NSC}) ->
    if NSC == false ->
	    ?bump_col(1),
	    scan_nmtoken(T, S, [$:], NSC);
       NSC == no_colons ->
	    ?fatal({invalid_NCName, lists:sublist(Str, 1, 6)}, S0);
       true ->
	    %% In order to conform with the "Namespaces in XML" spec,
	    %% we cannot allow names to begin with ":"
	    ?fatal({invalid_NCName, lists:sublist(Str, 1, 6)}, S0)
    end;
scan_name([$_|T], S0 = #xmerl_scanner{namespace_conformant = NSC}) ->
    ?bump_col(1),
    scan_nmtoken(T, S, [$_], NSC);
scan_name("%"++_T,S=#xmerl_scanner{environment=prolog}) ->
    ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_name("%"++T,S0=#xmerl_scanner{environment={external,_}}) ->
    %% parameter entity that expands to a name
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2} = strip(ExpRef ++ T1,S1),
    scan_name(T2,S2);
scan_name(Str, S0 = #xmerl_scanner{namespace_conformant = NSC}) ->
    {Ch,T} = to_ucs(S0#xmerl_scanner.encoding,Str),
    case xmerl_lib:is_letter(Ch) of
	true ->
	    ?bump_col(1),
	    scan_nmtoken(T, S, [Ch], NSC);
	false ->
	    ?fatal({invalid_name, lists:sublist(Str, 1, 6)}, S0)
    end;
scan_name(Str, S) ->
    ?fatal({invalid_name, Str}, S).






scan_nmtoken(Str, S, Acc, NSC) ->
    scan_nmtoken(Str, S, Acc, _Prefix = [], _Local = Acc, NSC,isLatin1(hd(Acc),true)).

%% scan_nmtoken/2
%% [7] NmToken ::= (NameChar)+
scan_nmtoken([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_nmtoken(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_nmtoken("%"++T, S0=#xmerl_scanner{environment={external,_}}) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2}  = strip(ExpRef ++ T1,S1),
    scan_nmtoken(T2,S2);
scan_nmtoken(Str, S) ->
    {Ch,T} = to_ucs(S#xmerl_scanner.encoding,Str),
    case xmerl_lib:is_namechar(Ch) of
	true ->
	    scan_nmtoken(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1},
			 _Acc = [Ch], _Prefix = [], _Local = [Ch],
			 _NamespaceConformant = false,isLatin1(Ch,true));
	false ->
	    ?fatal({invalid_nmtoken, lists:sublist(Str, 1, 6)}, S)
    end.


scan_nmtoken([], S=#xmerl_scanner{continuation_fun = F},
	     Acc, Prefix, Local, NSC,IsLatin1) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_nmtoken(MoreBytes,S1,Acc,Prefix,Local,NSC,IsLatin1) end,
      fun(S1) -> {list_to_atom(lists:reverse(Acc)),
		  namespace_info(Prefix, Local),[],S1} end,
      S);
%% whitespace marks the end of a name
scan_nmtoken(Str = [H|_], S, Acc, Prefix, Local, _NSC,true) when ?whitespace(H) ->
    %% we don't strip here because the occurrence of whitespace may be an error
    %% e.g. <!ELEMENT spec (front, body, back ?)>
    NmString = lists:reverse(Acc),
    {list_to_atom(NmString), namespace_info(Prefix, Local), Str, S};
scan_nmtoken(Str = [$:|_], S, Acc, [], _Local, no_colons,_IsLatin1) ->
    ?fatal({invalid_NCName,
	    lists:sublist(lists:reverse(Acc) ++ Str, 1, 6)}, S);
scan_nmtoken([$:|T], S0, Acc, [], Local, NSC, IsLatin1) ->
    ?bump_col(1),
    scan_nmtoken(T, S, [$:|Acc], lists:reverse(Local), [], NSC,IsLatin1);
scan_nmtoken(Str = [$:|_T], S, Acc, _Prefix, _Local, _NSC = true,_IsLatin1) ->
    %% non-empty Prefix means that we've encountered a ":" already.
    %% Conformity with "Namespaces in XML" requires
    %% at most one colon in a name
    ?fatal({invalid_NCName,
	    lists:sublist(lists:reverse(Acc) ++ Str, 1, 6)}, S);

%% non-namechar also marks the end of a name
scan_nmtoken(Str, S0, Acc, Prefix, Local, NSC,IsLatin1) ->
    ?bump_col(1),
    {Ch,T} = to_ucs(S#xmerl_scanner.encoding,Str),
    case {xmerl_lib:is_namechar(Ch),IsLatin1} of
	{true,_} ->
	    scan_nmtoken(T, S, [Ch|Acc], Prefix, [Ch|Local], NSC,isLatin1(Ch,IsLatin1));
	{_,true} ->
	    NmStr = lists:reverse(Acc),
	    {list_to_atom(NmStr), namespace_info(Prefix, Local), Str, S};
	_ ->
	    {lists:reverse(Acc), namespace_info(Prefix, Local), Str, S}
    end.

namespace_info([], _) ->
    [];
namespace_info(Prefix, Local) ->
    {Prefix, lists:reverse(Local)}.

isLatin1(_Ch,false) ->
    false;
isLatin1(Ch,_) when Ch > 255 ->
    false;
isLatin1(_,_) ->
    true.

%%%%%%% [11] SystemLiteral

scan_system_literal([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_system_literal(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_system_literal("\"" ++ T, S) ->
    scan_system_literal(T, S, $", []);
scan_system_literal("'" ++ T, S) ->
    scan_system_literal(T, S, $', []).


scan_system_literal([], S=#xmerl_scanner{continuation_fun = F},
		    Delimiter, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_system_literal(MoreBytes,S1,Delimiter,Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_system_literal([H|T], S, H, Acc) ->
    {lists:reverse(Acc), T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}};
scan_system_literal("#"++_R, S, _H, _Acc) ->
    %% actually not a fatal error
    ?fatal(fragment_identifier_in_system_literal,S);
scan_system_literal(Str, S, Delimiter, Acc) ->
    {Ch,T} = to_ucs(S#xmerl_scanner.encoding,Str),
    scan_system_literal(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1},
			Delimiter, [Ch|Acc]).


%%%%%%% [12] PubidLiteral

scan_pubid_literal([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_pubid_literal(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_pubid_literal([H|T], S) when H == $"; H == $' ->
    scan_pubid_literal(T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}, H, []);
scan_pubid_literal([H|_T], S) ->
    ?fatal({invalid_pubid_char, H}, S).


scan_pubid_literal([], S=#xmerl_scanner{continuation_fun = F},
		   Delimiter, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_pubid_literal(MoreBytes,S1,Delimiter,Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_pubid_literal([H|T], S, H, Acc) ->
    {lists:reverse(Acc), T, S#xmerl_scanner{col = S#xmerl_scanner.col+1}};
scan_pubid_literal(Str = [H|_], S, Delimiter, Acc) when ?whitespace(H) ->
    %% Before matching public identifiers, all whitespace must be normalized,
    %% so we do that here
    {_, T, S1} = pub_id_strip(Str, S),
    scan_pubid_literal(T, S1, Delimiter, [16#20|Acc]);
scan_pubid_literal([H|T], S, Delimiter, Acc) ->
    case is_pubid_char(H) of
	true ->
	    scan_pubid_literal(
	      T, S#xmerl_scanner{col = S#xmerl_scanner.col+1},
	      Delimiter, [H|Acc]);
	false ->
	    ?fatal({invalid_pubid_char, H}, S)
    end.

%% We do not match whitespace here, even though they're allowed in public
%% identifiers. This is because we normalize this whitespace as we scan
%% (see above in scan_pubid_literal())
%%
is_pubid_char(X) when X >= $a, X =< $z -> true;
is_pubid_char(X) when X >= $A, X =< $Z -> true;
is_pubid_char(X) when X >= $0, X =< $9 -> true;
is_pubid_char(X) ->
    lists:member(X, "-'()+,./:=?;!*#@$_%").


%%%%%%% [46] contentspec

scan_contentspec([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_contentspec(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_contentspec("EMPTY" ++ T, S0) ->
    ?bump_col(5),
    {empty, T, S};
scan_contentspec("ANY" ++ T, S0) ->
    ?bump_col(3),
    {any, T, S};
scan_contentspec("%" ++ _T, S=#xmerl_scanner{environment=prolog}) ->
    ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_contentspec("%" ++ T, S0) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2}  = strip(ExpRef ++ T1,S1),
    scan_contentspec(T2, S2);
scan_contentspec("(" ++ T, S0) ->
    ?bump_col(1),
    ?strip1,
    scan_elem_content(T1, S1);
scan_contentspec(_Str,S) ->
    ?fatal(unexpected_character,S).


%%%%%%% [47] children
%%%%%%% [51] Mixed

scan_elem_content(T, S) ->
    scan_elem_content(T, S, _Context = children, _Mode = unknown, _Acc = []).

scan_elem_content([], S=#xmerl_scanner{continuation_fun = F},
		  Context, Mode, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes,S1) -> scan_elem_content(MoreBytes,S1,Context,Mode,Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_elem_content(")" ++ T, S0, Context, Mode0, Acc0) ->
    ?bump_col(1),
    {Mode, Acc} = case {Mode0, Acc0} of
		      {unknown, [_X]} ->
			  {seq, Acc0};
		      {M, _L} when M == seq; M == choice ->
			  {Mode0, lists:reverse(Acc0)}
		  end,
    {Occurrence, T1, S1} = scan_occurrence(T, S),
    vc_No_Duplicate_Types(S,Context,Acc),
    case {Occurrence, Context,Acc} of
	{once, mixed,['#PCDATA']} -> ok; % It is not ok when there are
                                         % more names than '#PCDATA'
                                         % and no '*'.
	{'*', mixed,_} -> ok;
	{Other, mixed,_} ->
	    ?fatal({illegal_for_mixed_content, Other}, S1);
	_ ->
	    ok
    end,
    ?strip2,
    {format_elem_content({Occurrence, {Mode, Acc}}), T2, S2};
scan_elem_content("#PCDATA" ++ _T, S, not_mixed, _Mode, _Acc) ->
    ?fatal({error,{extra_set_of_parenthesis}},S);
scan_elem_content("#PCDATA" ++ _T, S, _Cont, Mode, Acc)
  when Mode==choice;Mode==seq;Acc/=[] ->
    ?fatal({error,{invalid_format_of_mixed_content}},S);
scan_elem_content("#PCDATA" ++ T, S0, _Context, Mode, Acc) ->
    ?bump_col(7),
    ?strip1,
    scan_elem_content(T1, S1, mixed, Mode, ['#PCDATA'|Acc]);
scan_elem_content("," ++ _T, S, _Context, choice, _Acc) ->
    ?fatal({mixing_comma_and_vertical_bar_in_content_model},S);
scan_elem_content("," ++ T, S0, Context, _Mode, Acc) ->
    ?bump_col(1),
    ?strip1,
    scan_elem_content2(T1, S1, Context, seq, Acc);
scan_elem_content("|" ++ _T, S, _Context, seq, _Acc) ->
    ?fatal({mixing_comma_and_vertical_bar_in_content_model},S);
scan_elem_content("|" ++ T, S0, Context, _Mode, Acc) ->
    ?bump_col(1),
    ?strip1,
    scan_elem_content2(T1, S1, Context, choice, Acc);
scan_elem_content(T, S, Context, Mode, Acc) ->
    scan_elem_content2(T, S, Context, Mode, Acc).

scan_elem_content2("(" ++ _T, S, mixed, _Mode, _Acc) ->
    ?fatal({error,
	{element_names_must_not_be_parenthesized_in_mixed_content}},S);
scan_elem_content2("(" ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(1),
    ?strip1,
    {Inner, T2, S2} = scan_elem_content(T1, S1, not_mixed, unknown, []),
    scan_elem_content(T2, S2, Context, Mode, [Inner|Acc]);
scan_elem_content2("%" ++ _T,S=#xmerl_scanner{environment=prolog},_Context,_Mode,_Acc) ->
    ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
scan_elem_content2("%" ++ T, S0, Context, Mode, Acc) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    ExpRef = expand_pe_reference(PERefName, S1,as_PE),
    {_,T2,S2}=strip(ExpRef++T1,S1),
    scan_elem_content(T2, S2, Context, Mode, Acc);
scan_elem_content2(T, S, Context, Mode, Acc) ->
    {Name, _NameStr, T1, S1} = scan_name(T, S),
    {Occurrence, T2, S2} = scan_occurrence(T1, S1),
    case {Occurrence, Context} of
	{once, mixed} -> ok;
	{Other, mixed} ->
	    ?fatal({illegal_for_mixed_content, Other}, S1);
	_ ->
	    ok
    end,
    ?strip3,
    mandatory_delimeter_wfc(T3,S3),
    NewAcc = [format_elem_content({Occurrence, Name}) | Acc],
    scan_elem_content(T3, S3, Context, Mode, NewAcc).


format_elem_content({once, What}) -> What;
format_elem_content(Other) -> Other.


scan_occurrence([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_occurrence(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_occurrence([$?|T], S0) ->
    ?bump_col(1),
    {'?', T, S};
scan_occurrence([$+|T], S0) ->
    ?bump_col(1),
    {'+', T, S};
scan_occurrence([$*|T], S0) ->
    ?bump_col(1),
    {'*', T, S};
scan_occurrence(T, S) ->
    {once, T , S}.

%%% Tests of Validity Constraints


%% first part of VC: Name Token
vc_Valid_Char(_AT,C,S) ->
    case xmerl_lib:is_namechar(C) of
	true ->
	    ok;
	_ ->
	    ?fatal({error,{validity_constraint_Name_Token,C}},S)
    end.



vc_ID_Attribute_Default(_,#xmerl_scanner{validation=Valid})
  when Valid /= dtd ->
    ok;
vc_ID_Attribute_Default({_,'ID',_,Def,_},_S)
  when Def=='#IMPLIED';Def=='#REQUIRED' ->
    ok;
vc_ID_Attribute_Default({_,'ID',_,Def,_},S) ->
    ?fatal({error,{validity_constraint_error_ID_Attribute_Default,Def}},S).

vc_Enumeration({_Name,{_,NameList},DefaultVal,_,_},S)
  when is_list(DefaultVal) ->
    case lists:member(list_to_atom(DefaultVal),NameList) of
	true ->
	    ok;
	_ ->
	    ?fatal({error,{vc_enumeration,list_to_atom(DefaultVal),NameList}},S)
    end;
vc_Enumeration({_Name,{_,_NameList},_DefaultVal,_,_},_S) ->
    ok.

vc_Entity_Name({_Name,'ENTITY',DefaultVal,_,_},S) when is_list(DefaultVal) ->
    Read = S#xmerl_scanner.rules_read_fun,
    case Read(entity,list_to_atom(DefaultVal),S) of
	{_,external,{_,{ndata,_}}} ->
	    ok;
	_ -> ?fatal({error,{vc_Entity_Name,list_to_atom(DefaultVal)}},S)
    end;
vc_Entity_Name({_Name,'ENTITY',_,_,_},_S) ->
    ok;
vc_Entity_Name({_,'ENTITIES',DefaultVal,_,_},S) when is_list(DefaultVal) ->
    Read = S#xmerl_scanner.rules_read_fun,
    NameListFun = fun([],Acc,_St,_Fun) ->
		       lists:reverse(Acc);
		  (Str,Acc,St,Fun) ->
		       {N,_,St2,Str2} = scan_name(Str,St),
		       Fun(Str2,[N|Acc],St2,Fun)
	       end,
    NameList = NameListFun(DefaultVal,[],S,NameListFun),
    VcFun =
	fun(X) ->
		case Read(entity,X,S) of
		    {_,external,{_,{ndata,_}}} ->
			ok;
		    _ -> ?fatal({error,{vc_Entity_Name,X}},S)
		end
	end,
    lists:foreach(VcFun,NameList);
vc_Entity_Name({_,'ENTITIES',_,_,_},_S) ->
    ok.

vc_No_Duplicate_Types(#xmerl_scanner{validation=dtd} = S,mixed,Acc) ->
    CheckDupl =
	fun([H|T],F) ->
		case lists:member(H,T) of
		    true ->
			?fatal({no_duplicate_types_allowed,H},S);
		    _ -> F(T,F)
		end;
	   ([],_) -> ok
	end,
    CheckDupl(Acc,CheckDupl);
vc_No_Duplicate_Types(_,_,_) ->
    ok.


%%% Tests of Well-Formededness Constraints


mandatory_delimeter_wfc(","++_T,_S) ->
    ok;
mandatory_delimeter_wfc("|"++_T,_S) ->
    ok;
mandatory_delimeter_wfc(")"++_T,_S) ->
    ok;
mandatory_delimeter_wfc("%"++_T,_S) ->
    %% a parameter reference is ok
    ok;
mandatory_delimeter_wfc(T,S) ->
    ?fatal({comma_or_vertical_bar_mandatory_between_names_in_content_model,T},S).


wfc_unique_att_spec([],_S) ->
    ok;
wfc_unique_att_spec([#xmlAttribute{name=N,expanded_name=EN}|Atts],S) ->
    case lists:keymember(N,#xmlAttribute.name,Atts) of
	true ->
	    ?fatal({error,{unique_att_spec_required,N}},S);
	_ ->
	    case S#xmerl_scanner.namespace_conformant andalso
		    lists:keymember(EN, #xmlAttribute.expanded_name, Atts) of
		true ->
		    ?fatal({error,{unique_att_spec_required,EN}},S);
		_ ->
		    wfc_unique_att_spec(Atts,S)
	    end
    end.

wfc_legal_char(Chars,S) when is_list(Chars)->
    {Ch,Rest} = to_ucs(S#xmerl_scanner.encoding,Chars),
    case xmerl_lib:is_char(Ch) of
	true ->
	    {Ch,Rest};
	_ ->
	    ?fatal({error,{wfc_Legal_Character,Ch}},S)
    end;
wfc_legal_char(Ch,S) ->
    case xmerl_lib:is_char(Ch) of
	true ->
	    {Ch,[]};
	_ ->
	    ?fatal({error,{wfc_Legal_Character,Ch}},S)
    end.


wfc_whitespace_betw_attrs([WS |_]=L,S) when ?whitespace(WS) ->
    {L,S};
wfc_whitespace_betw_attrs([$/ |_]=L,S) ->
    {L,S};
wfc_whitespace_betw_attrs([$> |_]=L,S) ->
    {L,S};
wfc_whitespace_betw_attrs([],S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> wfc_whitespace_betw_attrs(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
wfc_whitespace_betw_attrs(_,S) ->
    ?fatal({whitespace_required_between_attributes},S).

wfc_Entity_Declared({external,_},S=#xmerl_scanner{standalone=yes},Name) ->
    ?fatal({reference_to_externally_defed_entity_standalone_doc,Name},S);
wfc_Entity_Declared({external,_},_S,_) ->
    ok;
wfc_Entity_Declared(_Env,_S,_) ->
    ok.

wfc_Internal_parsed_entity(internal,Value,S) ->
    %% WFC test that replacement text matches production content
    scan_content(Value,S#xmerl_scanner{environment=internal_parsed_entity},
		 _Name=[],[],S#xmerl_scanner.space,_Lang=[],_Prnt=[],
		 #xmlNamespace{});
wfc_Internal_parsed_entity(_,_,_) ->
    ok.

vc_Element_valid(_Name, {"xmlns", _},
		 S = #xmerl_scanner{namespace_conformant = true}) ->
    ?fatal({error,{illegal_element_prefix,xmlns}},S);
vc_Element_valid(Name, _, S) ->
    vc_Element_valid(Name, S).

vc_Element_valid(_Name,#xmerl_scanner{environment=internal_parsed_entity}) ->
    ok;
vc_Element_valid(Name,S=#xmerl_scanner{rules_read_fun=Read,
				       validation=dtd}) ->
    case Read(elem_def,Name,S) of
	#xmlElement{elementdef=undeclared} ->
	    ?fatal({error,{error_missing_element_declaration_in_DTD,Name}},S);        undefined ->
	    ?fatal({error,{error_missing_element_declaration_in_DTD,Name}},S);        _ -> ok
    end;
vc_Element_valid(_,_) ->
    ok.

%%%%%%% [74] PEDef


scan_pe_def([], S=#xmerl_scanner{continuation_fun = F}, PEName) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_pe_def(MoreBytes, S1, PEName) end,
      fatal_fun(unexpected_end),
      S);
scan_pe_def("'" ++ T, S0, PEName) ->
    ?bump_col(1),
    scan_entity_value(T, S, $', PEName,parameter);
scan_pe_def("\"" ++ T, S0, PEName) ->
    ?bump_col(1),
    scan_entity_value(T, S, $", PEName,parameter);
scan_pe_def(Str, S, _PEName) ->
    scan_external_id(Str, S).


%%%%%%% [82] NotationDecl

scan_notation_decl(T, #xmerl_scanner{rules_write_fun = Write,
				     rules_read_fun=Read,
				     rules_delete_fun=Delete} = S) ->
    {Name, _NameStr, T1, S1} = scan_name_no_colons(T, S),
    {_,T2,S2} = mandatory_strip(T1,S1),
    {Def, T3, S3} = scan_notation_decl1(T2, S2),
    ?strip4,
    T5 = scan_mandatory(">",T4,1,S4,expected_end_tag_notation_declaration),
%    ">" ++ T5 = T4,
    case Read(notation,Name,S) of
	undeclared -> Delete(notation,Name,S4);
	_ -> ok
    end,
    S5 = Write(notation, Name, Def, S4),
    {T5, S5}.

scan_notation_decl1([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_notation_decl1(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_notation_decl1("SYSTEM" ++ T, S0) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {SL, T2, S2} = scan_system_literal(T1, S1),
    {{system, SL}, T2, S2};
scan_notation_decl1("PUBLIC" ++ T, S0) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {PIDL, T2, S2} = scan_pubid_literal(T1, S1),
    ?strip3,
    case T3 of
	">" ++ _ ->
	    {{public, PIDL}, T3,
	     S3#xmerl_scanner{col = S3#xmerl_scanner.col+1}};
	_ ->
	    {SL, T4, S4} = scan_system_literal(T3, S3),
	    {{public, PIDL, SL}, T4, S4}
    end.

%%%%%%% [75] ExternalID

scan_external_id([], S=#xmerl_scanner{continuation_fun = F}) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_external_id(MoreBytes, S1) end,
      fatal_fun(unexpected_end),
      S);
scan_external_id("SYSTEM" ++ T, S0) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {SL, T2, S2} = scan_system_literal(T1, S1),
    {{system, SL}, T2, S2};
scan_external_id("PUBLIC" ++ T, S0) ->
    ?bump_col(6),
    {_,T1,S1} = mandatory_strip(T,S),
    {PIDL, T2, S2} = scan_pubid_literal(T1, S1),
    {_,T3,S3} = mandatory_strip(T2,S2),
    {SL, T4, S4} = scan_system_literal(T3, S3),
    {{public, PIDL, SL}, T4, S4}.


%%%%%%% [9] EntityValue

%% Note that we have two different scan functions for EntityValue
%% They differ in that this one checks for recursive calls to the same
%% parameter entity.

scan_entity_value(Str, S, Delim, Name, Namespace) ->
    scan_entity_value(Str, S, Delim, _Acc = [], Name, Namespace,[]).


scan_entity_value([], S=#xmerl_scanner{environment={external,{entity,_}}},
		  _Delim,Acc,_,_,[]) ->
    {lists:flatten(lists:reverse(Acc)), [], S};
scan_entity_value([], S=#xmerl_scanner{environment={external,{entity,_}},
				       validation=dtd},
		  _Delim,_Acc,PEName,_,_) ->
    {{error,{failed_VC_Proper_Declaration_PE_Nesting,1,PEName}},[],S};
scan_entity_value([],S,
		  no_delim,Acc,_,_,[]) ->
    {lists:flatten(lists:reverse(Acc)),[],S};
scan_entity_value([],S=#xmerl_scanner{validation=dtd},
		  no_delim,_Acc,PEName,_,_PENesting) ->
    {{error,{failed_VC_Proper_Declaration_PE_Nesting,2,PEName}},[],S};
scan_entity_value([], S=#xmerl_scanner{continuation_fun = F},
		  Delim, Acc, PEName,Namespace,PENesting) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) ->
	      scan_entity_value(MoreBytes,S1,
				Delim,Acc,PEName,Namespace,PENesting)
      end,
      fatal_fun(unexpected_end),
      S);
scan_entity_value([Delim|T], S=#xmerl_scanner{validation=dtd},
		  Delim,_Acc,PEName,_NS,PENesting) when length(PENesting) /= 0 ->
    {{error,{failed_VC_Proper_Declaration_PE_Nesting,3,PEName}},T,S};
scan_entity_value([Delim|T], S0,
		  Delim, Acc, _PEName,_NS,_PENesting) ->
    ?bump_col(1),
    {lists:flatten(lists:reverse(Acc)), T, S};
scan_entity_value("%" ++ _T,S=#xmerl_scanner{environment=prolog},_,_,_,_,_) ->
    ?fatal({error,{wfc_PEs_In_Internal_Subset}},S);
% %% This is a PEdecl in an external entity
% scan_entity_value([$%,WS|T], S0, Delim, Acc, PEName,Namespace,PENesting)
%   when ?whitespace(WS) ->
%     ?bump_col(2),
%     scan_entity_value(T, S, Delim, [WS,$%|Acc], PEName,Namespace,PENesting);
scan_entity_value("%" ++ T, S0, Delim, Acc, PEName,Namespace,PENesting) ->
    ?bump_col(1),
    {PERefName, T1, S1} = scan_pe_reference(T, S),
    if PERefName == PEName,Namespace==parameter ->
	    ?fatal({illegal_recursion_in_PE, PEName}, S1);
       true ->
	    {ExpandedRef,S2} =
		case expand_pe_reference(PERefName, S1, in_literal) of
		    %% actually should pe ref be expanded as_PE but
		    %% handle whitespace explicitly in this case.
		    Tuple when is_tuple(Tuple) ->
			%% {system,URI} or {public,URI}
			%% Included in literal.
			{ExpRef,Sx}=fetch_not_parse(Tuple,S1),
			{EntV, _, S5} =
		 	    scan_entity_value(ExpRef, Sx, no_delim,[],
					      PERefName,parameter,[]),
			%% should do an update Write(parameter_entity)
			%% so next expand_pe_reference is faster
			{string_to_char_set(S5#xmerl_scanner.encoding, EntV), S5};
		     ExpRef ->
			{string_to_char_set(S1#xmerl_scanner.encoding, ExpRef) ,S1}
		end,
	    %% single or duoble qoutes are not treated as delimeters
	    %% in passages "included in literal"
	    S3 = S2#xmerl_scanner{col=S2#xmerl_scanner.col+1},
	    {Acc2,_,S4} = scan_entity_value(ExpandedRef,S3,no_delim,Acc,
					    PEName,Namespace,[]),
% 	    {_,T2,S5} = strip(" "++T1,S4),
	    scan_entity_value(T1,S4#xmerl_scanner{line=S3#xmerl_scanner.line,
						  col=S3#xmerl_scanner.col},
			      Delim,lists:reverse(Acc2),
 			      PEName,Namespace,PENesting)
% 	    scan_entity_value(T1,S4,Delim,lists:reverse(Acc2),
% 			      PEName,Namespace,PENesting)
    end;
scan_entity_value("&" ++ T, S0, Delim, Acc, PEName,Namespace,PENesting) ->
    %% This is either a character entity or a general entity (internal
    %% or external) reference. An internal general entity shall not be
    %% expanded in an entity def XML1.0 section 4.5.
    ?bump_col(1),
    case T of
	"#"++_T ->
	    {ExpRef, T1, S1} = scan_reference(T, S),
	    Tok = pe_nesting_token(ExpRef++T1,Namespace,S1#xmerl_scanner.validation),
	    case markup_delimeter(ExpRef) of
		true ->
		    scan_entity_value(T1, S1, Delim, [ExpRef|Acc], PEName,
				      Namespace,pe_push(Tok,PENesting,S1));
		_ ->
		    ExpRef2 = string_to_char_set(S#xmerl_scanner.encoding,ExpRef),
		    scan_entity_value(ExpRef2 ++ T1, S1, Delim, Acc, PEName,
				      Namespace,pe_push(Tok,PENesting,S1))
	    end;
	_ -> %% General Entity is bypassed, though must check for
             %% recursion: save referenced name now and check for
             %% recursive reference after the whole entity definition is
             %% completed.
	    {Name, _NamespaceInfo, T1, S1} = scan_name(T,S),
	    T2=scan_mandatory(";",T1,1,S1,expected_entity_reference_semicolon),
	    S2=save_refed_entity_name(Name,PEName,S1),
	    scan_entity_value(T2,S2,Delim,[";",atom_to_list(Name),"&"|Acc],PEName,Namespace,PENesting)
    end;
%% The following clauses is for PE Nesting VC constraint
%% Start delimeter for ConditionalSection
scan_entity_value("<!["++T,S0,Delim,Acc,PEName,parameter=NS,PENesting)->
    ?bump_col(3),
    scan_entity_value(T,S,Delim,["<!["|Acc],PEName,NS,
		      pe_push("<![",PENesting,S));
%% Start delimeter for ConditionalSection (2)
scan_entity_value("["++T,S0,Delim,Acc,PEName,parameter=NS,PENesting)->
    ?bump_col(1),
    scan_entity_value(T,S,Delim,["["|Acc],PEName,NS,
		      pe_push("[",PENesting,S));
%% Start delimeter for comment
scan_entity_value("<!--"++T,S0,Delim,Acc,PEName,parameter=NS,PENesting)->
    ?bump_col(4),
    scan_entity_value(T,S,Delim,["<!--"|Acc],PEName,NS,
		      pe_push("<!--",PENesting,S));
%% Start delimeter for ElementDecl, AttListDecl,EntityDecl,NotationDecl
scan_entity_value("<!"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(2),
    scan_entity_value(T,S,Delim,["<!"|Acc],PEName,NS,
		      pe_push("<!",PENesting,S));
%% Start delimeter for PI
scan_entity_value("<?"++T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(2),
    scan_entity_value(T,S,Delim,["<?"|Acc],PEName,NS,
		      pe_push("<?",PENesting,S));
%% Start delimeter for elements that matches the proper stop delimeter
%% for a markupdecl
scan_entity_value("</"++T,S0,Delim,Acc,PEName,parameter=NS,PENesting)->
    ?bump_col(2),
    scan_entity_value(T,S,Delim,["</"|Acc],PEName,NS,
		      pe_push("</",PENesting,S));
scan_entity_value("<"++T,S0,Delim,Acc,PEName,parameter=NS,PENesting)->
    ?bump_col(1),
    scan_entity_value(T,S,Delim,["<"|Acc],PEName,NS,
		      pe_push("<",PENesting,S));
%% Delimeter for contentspecs
scan_entity_value("("++T,S0,Delim,Acc,PEName,parameter=NS,PENesting)->
    ?bump_col(1),
    scan_entity_value(T,S,Delim,["("|Acc],PEName,NS,
		      pe_push("(",PENesting,S));
%% Stop delimeter for ElementDecl, AttListDecl,EntityDecl,NotationDecl
scan_entity_value(">"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(1),
    scan_entity_value(T,S,Delim,[">"|Acc],PEName,NS,
		      pe_pop(">",PENesting,S));
%% Stop delimeter for PI
scan_entity_value("?>"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(2),
    scan_entity_value(T,S,Delim,["?>"|Acc],PEName,NS,
		      pe_pop("?>",PENesting,S));
%% Stop delimeter for comment
scan_entity_value("-->"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(3),
    scan_entity_value(T,S,Delim,["-->"|Acc],PEName,NS,
		      pe_pop("-->",PENesting,S));
%% Stop delimeter for ConditionalSection
scan_entity_value("]]>"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(3),
    scan_entity_value(T,S,Delim,["]]>"|Acc],PEName,NS,
		      pe_pop("]]>",PENesting,S));
%% Stop delimeter added to match a content start delimeter included
scan_entity_value("/>"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(2),
    scan_entity_value(T,S,Delim,["/>"|Acc],PEName,NS,
		      pe_pop("/>",PENesting,S));
scan_entity_value(")"++ T,S0,Delim,Acc,PEName, parameter=NS,PENesting) ->
    ?bump_col(1),
    scan_entity_value(T,S,Delim,[")"|Acc],PEName,NS,
		      pe_pop(")",PENesting,S));
scan_entity_value("\n"++T, S, Delim, Acc, PEName,Namespace,PENesting) ->
    scan_entity_value(T, S#xmerl_scanner{line=S#xmerl_scanner.line+1},
		      Delim, ["\n"|Acc], PEName,Namespace,PENesting);
scan_entity_value(Str, S0, Delim, Acc, PEName,Namespace,PENesting) ->
    {Ch,T} = to_ucs(S0#xmerl_scanner.encoding,Str),
    case xmerl_lib:is_char(Ch) of
	true ->
	    ?bump_col(1),
	    scan_entity_value(T, S, Delim, [Ch|Acc], PEName,Namespace,PENesting);
	false ->
	    ?fatal({unexpected_char,Ch}, S0)
    end.



save_refed_entity_name(Name,PEName,S) ->
    case predefined_entity(Name) of
	true ->
	    S;
	_ ->
	    save_refed_entity_name1(Name,PEName,S)
    end.

save_refed_entity_name1(Name,PEName,
			S=#xmerl_scanner{entity_references=ERefs}) ->
    case lists:keysearch(PEName,1,ERefs) of
	{value,{_,Refs}} ->
	    NewRefs =
		case lists:member(Name,Refs) of
		    true ->Refs;
		    _ -> [Name|Refs]
		end,
	    S#xmerl_scanner{entity_references=lists:keyreplace(PEName,1,ERefs,
							       {PEName,NewRefs})
			   };
	_ ->
	    S#xmerl_scanner{entity_references=[{PEName,[Name]}|ERefs]}
    end.



pe_push(Tok,Stack,_S) when Tok=="<!";Tok=="<?";Tok=="<!--";Tok=="<![";
			   Tok=="[";Tok=="<";Tok=="</";Tok=="(" ->
    [Tok|Stack];
pe_push(Tok,Stack,#xmerl_scanner{validation=dtd})
  when Tok==")";Tok==">";Tok=="?>";Tok=="]]>";Tok=="-->";Tok=="/>"->
    [Tok|Stack];
pe_push(_,Stack,_S) ->
    Stack.

pe_pop(">",["<!"|Rest],_S) ->        Rest;
pe_pop("?>",["<?"|Rest],_S) ->       Rest;
pe_pop("-->",["<!--"|Rest],_S) ->    Rest;
pe_pop("]]>",["[","<!["|Rest],_S) -> Rest;
pe_pop("/>",["<"|Rest],_S) ->        Rest;
pe_pop(">",["<"|Rest],_S) ->         Rest;
pe_pop(">",["</"|Rest],_S) ->        Rest;
pe_pop(")",["("|Rest],_S) ->         Rest;
pe_pop(Token,_Stack,S=#xmerl_scanner{validation=dtd}) ->
    ?fatal({error,{failed_VC_Proper_Declaration_PE_Nesting,5,Token}},S);
pe_pop(_,Rest,_) ->
    Rest.

pe_nesting_token("<!"++_T,parameter,dtd) ->   "<!";
pe_nesting_token("<?"++_T,parameter,dtd) ->   "<?";
pe_nesting_token("<!--"++_T,parameter,dtd) -> "<!--";
pe_nesting_token("<!["++_T,parameter,dtd) ->  "<![";
pe_nesting_token("["++_T,parameter,dtd) ->    "[";
pe_nesting_token("("++_T,parameter,dtd) ->    "(";
pe_nesting_token(">"++_T,parameter,dtd) ->    ">";
pe_nesting_token("?>"++_T,parameter,dtd) ->   "?>";
pe_nesting_token("-->"++_T,parameter,dtd) ->  "-->";
pe_nesting_token("]]>"++_T,parameter,dtd) ->  "]]>";
pe_nesting_token(")"++_T,parameter,dtd) ->    ")";
pe_nesting_token("/>"++_T,parameter,dtd) ->   "/>";
pe_nesting_token(_,_,_) ->                     false.

predefined_entity(amp) ->  true;
predefined_entity(lt) ->   true;
predefined_entity(gt) ->   true;
predefined_entity(apos) -> true;
predefined_entity(quot) -> true;
predefined_entity(_) ->    false.

check_entity_recursion(EName,
		       S=#xmerl_scanner{entity_references=EntityRefList}) ->
    Set = sofs:family(EntityRefList),
    case catch sofs:family_to_digraph(Set, [acyclic]) of
	{'EXIT',{cyclic,_}} ->
	    ?fatal({illegal_recursion_in_Entity, EName}, S);
	DG ->
	    digraph:delete(DG),
	    ok
    end.




%%%%%%% [15] Comment
scan_comment(Str, S) ->
    scan_comment(Str, S, _Pos = undefined, _Parents = [], _Lang = []).

scan_comment(Str,S=#xmerl_scanner{col=C,event_fun=Event}, Pos, Parents, Lang) ->
    Comment = #xmlComment{pos = Pos,
			  parents = Parents,
			  language = Lang,
			  value = undefined},
    S1 = #xmerl_scanner{} = Event(#xmerl_event{event = started,
					       line = S#xmerl_scanner.line,
					       col = C,
					       pos = Pos,
					       data = Comment}, S),

    scan_comment1(Str, S1, Pos, Comment, _Acc = []).

scan_comment1([], S=#xmerl_scanner{continuation_fun = F},
	     Pos, Comment, Acc) ->
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> scan_comment1(MoreBytes, S1, Pos, Comment, Acc) end,
      fatal_fun(unexpected_end),
      S);
scan_comment1("-->" ++ T, S0 = #xmerl_scanner{col = C,
					     event_fun = Event,
					     hook_fun = Hook},
	     _Pos, Comment, Acc) ->
    ?bump_col(3),
    Comment1 = Comment#xmlComment{value = lists:reverse(Acc)},
    S1=#xmerl_scanner{}=Event(#xmerl_event{event = ended,
						   line=S#xmerl_scanner.line,
						   col = C,
						   data = Comment1}, S),
    {Ret, S2} = Hook(Comment1, S1),
    {_,T3,S3}=strip(T,S2),
    {Ret,T3,S3};
scan_comment1("--"++T,S,_Pos,_Comment,_Acc) ->
    ?fatal({invalid_comment,"--"++[hd(T)]}, S);
scan_comment1("\n" ++ T, S=#xmerl_scanner{line = L}, Pos, Cmt, Acc) ->
    scan_comment1(T, S#xmerl_scanner{line=L+1,col=1},Pos, Cmt, "\n" ++ Acc);
scan_comment1("\r\n" ++ T, S=#xmerl_scanner{line = L}, Pos, Cmt, Acc) ->
    %% CR followed by LF is read as a single LF
    scan_comment1(T, S#xmerl_scanner{line=L+1,col=1}, Pos, Cmt, "\n" ++ Acc);
scan_comment1("\r" ++ T, S=#xmerl_scanner{line = L}, Pos, Cmt, Acc) ->
    %% CR not followed by LF is read as a LF
    scan_comment1(T, S#xmerl_scanner{line=L+1,col=1}, Pos, Cmt, "\n" ++ Acc);
scan_comment1(Str, S=#xmerl_scanner{col = C}, Pos, Cmt, Acc) ->
    {Ch,T} = wfc_legal_char(Str,S),
    scan_comment1(T, S#xmerl_scanner{col=C+1}, Pos, Cmt, [Ch|Acc]).

%%%%%%%

scan_markup_completion_gt([$>|_R]=T,S) ->
    {T,S};
scan_markup_completion_gt([$%|T],S0) ->
    ?bump_col(1),
    {Name,T1,S1} = scan_pe_reference(T,S),
    ExpandedRef =  expand_pe_reference(Name,S1,as_PE),
    {_,T2,S2} = strip(ExpandedRef++T1,S1),
    scan_markup_completion_gt(T2,S2);
scan_markup_completion_gt(T,S) ->
    ?fatal({error,{malformed_syntax_entity_completion,T}},S).


scan_mandatory(Pattern,T,N,S,ErrorMsg) ->
    case lists:prefix(Pattern,T) of
	true ->
	    lists:nthtail(N,T);
	_ ->
	    ?fatal(ErrorMsg,S)
    end.


strip(Str,S) ->
    strip(Str,S,all).

strip([], S=#xmerl_scanner{continuation_fun = F},_) ->
    ?dbg("cont()... stripping whitespace~n", []),
    F(fun(MoreBytes, S1) -> strip(MoreBytes, S1) end,
      fun(S1) -> {[], [], S1} end,
      S);
strip("\s" ++ T, S=#xmerl_scanner{col = C},Lim) ->
    strip(T, S#xmerl_scanner{col = C+1},Lim);
strip("\t" ++ _T, S ,no_tab) ->
    ?fatal({error,{no_tab_allowed}},S);
strip("\t" ++ T, S=#xmerl_scanner{col = C},Lim) ->
    strip(T, S#xmerl_scanner{col = expand_tab(C)},Lim);
strip("\n" ++ T, S=#xmerl_scanner{line = L},Lim) ->
    strip(T, S#xmerl_scanner{line = L+1, col = 1},Lim);
strip("\r\n" ++ T, S=#xmerl_scanner{line = L},Lim) ->
    %% CR followed by LF is read as a single LF
    strip(T, S#xmerl_scanner{line = L+1, col = 1},Lim);
strip("\r" ++ T, S=#xmerl_scanner{line = L},Lim) ->
    %% CR not followed by LF is read as a LF
    strip(T, S#xmerl_scanner{line = L+1, col = 1},Lim);
strip(Str, S,_Lim) ->
    {[], Str, S}.

%% demands a whitespace, though a parameter entity is ok, it will
%% expand with a whitespace on each side.
mandatory_strip([],S) ->
    ?fatal({error,{whitespace_was_expected}},S);
mandatory_strip(T,S) when ?whitespace(hd(T)) ->
    strip(T,S,all);
mandatory_strip([$%|T],S) when ?whitespace(hd(T)) -> %this is not a PERefence, but an PEDeclaration
    ?fatal({error,{whitespace_was_expected}},S);
mandatory_strip([$%|_T]=T,S) ->
    {[],T,S};
mandatory_strip(_T,S) ->
    ?fatal({error,{whitespace_was_expected}},S).

%% strip but don't accept tab
pub_id_strip(Str, S) ->
    strip(Str,S,no_tab).


normalize("&"++T,S,IsNorm) ->
    case scan_reference(T, S) of
	{ExpRef, T1, S1} when ?whitespace(hd(ExpRef)) ->
	    ExpRef2 = string_to_char_set(S#xmerl_scanner.encoding,ExpRef),
	    normalize(ExpRef2++T1,S1,IsNorm);
	_ ->
	    {"&"++T,S,IsNorm}
    end;
normalize(T,S,IsNorm) ->
    case strip(T,S) of
	{_,T,S} ->
	    {T,S,IsNorm};
	{_,T1,S1} ->
	    normalize(T1,S1,true)
    end.


%% Optimization:
%% - avoid building list of spaces or tabs;
%% - avoid reverse;
%% - compact two common indentation patterns.
%% Note: only to be called when a \n was found.
fast_accumulate_whitespace(" " ++ T, S, _) ->
    fast_acc_spaces(T, S, 1);
fast_accumulate_whitespace("\t"++T, S, _) ->
    fast_acc_tabs(T, S, 1);
fast_accumulate_whitespace("<"++_=R, S, _T) ->
    #xmerl_scanner{common_data = CD, line = Line} = S,
    {done, {element(3, CD), R, S#xmerl_scanner{col = 1, line = Line + 1}}};
fast_accumulate_whitespace(_, S, T) ->
    accumulate_whitespace(T, S, []).

fast_acc_spaces(" " ++ T, S, N) ->
    fast_acc_spaces(T, S, N + 1);
fast_acc_spaces(T, S, N) ->
    fast_acc_end(T, S, N, N, $\s, 1).

fast_acc_tabs("\t" ++ T, S, N) ->
    fast_acc_tabs(T, S, N + 1);
fast_acc_tabs(T, S, N) ->
    fast_acc_end(T, S, N, N * 8 + 1, $\t, 2).

fast_acc_end(T, S, N, Col, C, CD_I) ->
    #xmerl_scanner{common_data = CD, line = Line0} = S,
    Line = Line0 + 1,
    try
        $< = hd(T),
        {done,{element(N, element(CD_I, CD)), T,
               S#xmerl_scanner{col = Col, line = Line}}}
    catch _:_ ->
        accumulate_whitespace(T, S, Line, Col, lists:duplicate(N, C)++"\n")
    end.


%%% @spec accumulate_whitespace(T::string(),S::global_state(),
%%%                             atom(),Acc::string()) -> {Acc, T1, S1}
%%%
%%% @doc Function to accumulate and normalize whitespace.
accumulate_whitespace(T, S, preserve, Acc) ->
    accumulate_whitespace(T, S, Acc);
accumulate_whitespace(T, S, normalize, Acc) ->
    {_WsAcc, T1, S1} = accumulate_whitespace(T, S, []),
    {[$\s|Acc], T1, S1}.

accumulate_whitespace(T, S, Acc) ->
    #xmerl_scanner{line = Line, col = Col} = S,
    accumulate_whitespace(T, S, Line, Col, Acc).

accumulate_whitespace([], S0, Line, Col, Acc) ->
    #xmerl_scanner{continuation_fun = F} = S0,
    S = S0#xmerl_scanner{line = Line, col = Col},
    ?dbg("cont()...~n", []),
    F(fun(MoreBytes, S1) -> accumulate_whitespace(MoreBytes, S1, Acc) end,
      fun(S1) -> {Acc, [], S1} end,
      S);
accumulate_whitespace("\s" ++ T, S, Line, Col, Acc) ->
    accumulate_whitespace(T, S, Line, Col+1, [$\s|Acc]);
accumulate_whitespace("\t" ++ T, S, Line, Col, Acc) ->
    accumulate_whitespace(T, S, Line, expand_tab(Col), [$\t|Acc]);
accumulate_whitespace("\n" ++ T, S, Line, _Col, Acc) ->
    accumulate_whitespace(T, S, Line+1, 1, [$\n|Acc]);
accumulate_whitespace("\r\n" ++ T, S, Line, _Col, Acc) ->
    %% CR followed by LF is read as a single LF
    accumulate_whitespace(T, S, Line+1, 1, [$\n|Acc]);
accumulate_whitespace("\r" ++ T, S, Line, _Col, Acc) ->
    %% CR not followed by LF is read as a LF
    accumulate_whitespace(T, S, Line+1, 1, [$\n|Acc]);
accumulate_whitespace(Str, S, Line, Col, Acc) ->
    {Acc, Str, S#xmerl_scanner{line = Line, col = Col}}.

expand_tab(Col) ->
    Rem = (Col-1) rem 8,
    _NewCol = Col + 8 - Rem.

%% validation_mode(Validation)
%% Validation = off | dtd | schema | true | false
%% true and false are obsolete
validation_mode(false) ->
    off;
validation_mode(true) ->
    dtd;
validation_mode(Other) ->
    Other.


schemaLocations(El,#xmerl_scanner{schemaLocation=[]}) ->
    schemaLocations(El);
schemaLocations(El,#xmerl_scanner{schemaLocation=SL}) ->
    case SL of
	[{_,_}|_] ->
	    {ok,SL};
	_ ->
	    schemaLocations(El)
    end.

schemaLocations(#xmlElement{attributes=Atts,xmlbase=_Base}) ->
    Pred = fun(#xmlAttribute{name=schemaLocation}) -> false;
	      (#xmlAttribute{nsinfo={_,"schemaLocation"}}) -> false;
	      (_) -> true
	   end,
    case lists:dropwhile(Pred,Atts) of
	[#xmlAttribute{value=Paths}|_] ->

	    case string:tokens(Paths," \n\t\r") of
		L when length(L) > 0 ->
		    case length(L) rem 2 of
			0 ->
			    PairList =
				fun([],_Fun) ->
					[];
				   ([SLNS,SLLoc|Rest],Fun) ->
					[{SLNS,SLLoc}|Fun(Rest,Fun)]
				end,
			    {ok,PairList(L,PairList)};
			_ ->
			    {error,{schemaLocation_attribute,namespace_location_not_in_pair}}
		    end;
		_ ->
		    {error,{missing_schemaLocation}}
	    end;
	[] ->
	    {error,{missing_schemaLocation}}
    end.

inherit_options(S) ->
    %%?dbg("xsdbase: ~p~n",[S#xmerl_scanner.xmlbase]),
    [{xsdbase,S#xmerl_scanner.xmlbase}].

handle_schema_result({XSDRes=#xmlElement{},_},S5) ->
    {XSDRes,S5};
handle_schema_result({error,Reason},S5) ->
    ?fatal({failed_schema_validation,Reason},S5).

%%% Helper functions

-compile({inline, [fatal_fun/1]}).

-spec fatal_fun(_) -> fun((_) -> no_return()).

fatal_fun(Reason) ->
    fun(S) -> ?fatal(Reason, S) end.

fatal(Reason, S) ->
    exit({fatal, {Reason,
		  {file,S#xmerl_scanner.filename},
		  {line,S#xmerl_scanner.line},
		  {col,S#xmerl_scanner.col}}}).

%% preformat formats tokens in L1 and L2, L2 separated by Sep into a
%% list
preformat(L1,L2,Sep) ->
    Format1= lists:flatten(lists:duplicate(length(L1)-1,"~s ")++"~s"),
    Format2 = lists:flatten(lists:duplicate(length(L2)-1,
					    " ~s"++Sep)++" ~s"),

    lists:flatten(io_lib:format(Format1++Format2,L1++L2)).


%% BUG when we are many <!ATTLIST ..> balise none attributes has save in rules
rules_write(Context, Name, Value, #xmerl_scanner{rules = T} = S) ->
    case ets:lookup(T, {Context, Name}) of
	[] ->
	    ets:insert(T, {{Context, Name}, Value});
	_ ->
	    ok
    end,
    S.


rules_read(Context, Name, #xmerl_scanner{rules = T}) ->
    case ets:lookup(T, {Context, Name}) of
	[] ->
	    undefined;
	[{_, V}] ->
	    V
    end.

rules_delete(Context,Name,#xmerl_scanner{rules = T}) ->
    ets:delete(T,{Context,Name}).

to_ucs(Encoding, Chars) when Encoding=="utf-8"; Encoding == undefined ->
    utf8_2_ucs(Chars);
to_ucs(_,[C|Rest]) ->
    {C,Rest}.

utf8_2_ucs([A,B,C,D|Rest]) when A band 16#f8 =:= 16#f0,
			      B band 16#c0 =:= 16#80,
			      C band 16#c0 =:= 16#80,
			      D band 16#c0 =:= 16#80 ->
    %% 11110vvv 10vvvvvv 10vvvvvv 10vvvvvv
    case ((D band 16#3f) bor ((C band 16#3f) bsl 6) bor
	  ((B band 16#3f) bsl 12) bor ((A band 16#07) bsl 18)) of
	Ch when Ch >= 16#10000 ->
	    {Ch,Rest};
	Ch ->
	    {{error,{bad_character,Ch}},Rest}
    end;
utf8_2_ucs([A,B,C|Rest]) when A band 16#f0 =:= 16#e0,
			    B band 16#c0 =:= 16#80,
			    C band 16#c0 =:= 16#80 ->
    %% 1110vvvv 10vvvvvv 10vvvvvv
    case ((C band 16#3f) bor ((B band 16#3f) bsl 6) bor
	  ((A band 16#0f) bsl 12)) of
	Ch when Ch >= 16#800 ->
	    {Ch,Rest};
	Ch ->
	    {{error,{bad_character,Ch}},Rest}
    end;
utf8_2_ucs([A,B|Rest]) when A band 16#e0 =:= 16#c0,
			  B band 16#c0 =:= 16#80 ->
    %% 110vvvvv 10vvvvvv
    case ((B band 16#3f) bor ((A band 16#1f) bsl 6)) of
	Ch when Ch >= 16#80 ->
	    {Ch,Rest};
	Ch ->
	    {{error,{bad_character,Ch}},Rest}
    end;
utf8_2_ucs([A|Rest]) when A < 16#80 ->
    {A,Rest};
utf8_2_ucs([A|Rest]) ->
    {{error,{bad_character,A}},Rest}.

%% to_char_set("iso-10646-utf-1",Ch) ->
%%     [Ch];
%% to_char_set(UTF8,Ch) when UTF8 =:= "utf-8"; UTF8 =:= undefined ->
%%     ucs_2_utf8(Ch);
%% to_char_set(_,Ch) ->
%%     [Ch].

ucs_2_utf8(Ch) when Ch < 128 ->
    %% 0vvvvvvv
    [Ch];
ucs_2_utf8(Ch) when Ch < 16#0800 ->
    %% Ch: -----vvv vvvvvvvv
    %% 110vvvvv 10vvvvvv
    %% O1 = (Ch band 16#07c0) bsr 6,
    %% O2 = (Ch band 16#003f),
    [((Ch band 16#07c0) bsr 6) bor 16#c0,(Ch band 16#003f) bor 16#80];
ucs_2_utf8(Ch) when Ch < 16#10000 ->
    %% Ch: vvvvvvvv vvvvvvvv
    %% 1110vvvv 10vvvvvv 10vvvvvv
    %% O1 = (Ch band 16#f000) bsr 12
    %% O2 = (Ch band 16#0fc0) bsr 6
    %% O3 = (Ch band 16#003f)
    [((Ch band 16#f000) bsr 12) bor 16#e0,
     ((Ch band 16#0fc0) bsr 6) bor 16#80,
     (Ch band 16#003f) bor 16#80];
ucs_2_utf8(Ch) when Ch < 16#200000 ->
    %% Ch: ---vvvvv vvvvvvvv vvvvvvvv
    %% 11110vvv 10vvvvvv 10vvvvvv 10vvvvvv
    %% O1 = (Ch band 16#1c0000) bsr 18
    %% O2 = (Ch band 16#03f000) bsr 12
    %% O3 = (Ch band 16#000fc0) bsr 6
    %% O4 = (Ch band 16#00003f)
    [((Ch band 16#1c0000) bsr 18) bor 16#f0,
     ((Ch band 16#03f000) bsr 12) bor 16#80,
     ((Ch band 16#000fc0) bsr 6) bor 16#80,
     (Ch band 16#00003f) bor 16#80].


string_to_char_set(Enc,Str) when Enc =:= "utf-8"; Enc =:= undefined ->
    lists:flatten([ucs_2_utf8(X)||X <- Str]);
string_to_char_set(_,Str) ->
    Str.

%% diagnose(Line) ->
%%     Mem=erlang:memory(),
%%     {OldTot,OldLine} = get_total(),
%%     NewTot =
%%     case {lists:keysearch(total,1,Mem),OldTot*1.1} of
%% 	{{_,{_,Tot}},Tot110} when Tot > Tot110 ->
%% 	    ?dbg("From ~p to ~p, total memory: ~p (~p)~n",[OldLine,Line,Tot,OldTot]),
%% 	    Tot;
%% 	{{_,{_,Tot}},_} ->
%% 	    Tot
%%     end,
%%     put_total({NewTot,Line}).

%% get_total() ->
%%     case get(xmerl_mem) of
%% 	undefined ->
%% 	    put(xmerl_mem,{0,0}),
%% 	    {0,0};
%% 	M -> M
%%     end.

%% put_total(M) ->
%%     put(xmerl_mem,M).
