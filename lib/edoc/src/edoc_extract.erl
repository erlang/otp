%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc EDoc documentation extraction.

-module(edoc_extract).

-export([source/3, source/4, source/5, header/3, header/4, header/5,
	 file/4, text/4]).

-import(edoc_report, [report/3, warning/3]).

%% %% @headerfile "edoc.hrl" (disabled until it can be made private)
-include("edoc.hrl").

%% @type filename() = //kernel/file:filename().
%% @type proplist() = //stdlib/proplists:property().
%% @type syntaxTree() = //syntax_tools/erl_syntax:syntaxTree().

%% @spec source(File::filename(), Env::edoc_env(), Options::proplist())
%%             -> {ModuleName, edoc:edoc_module()}
%%    ModuleName = atom()
%%    proplist() = [term()]
%%
%% @doc Like {@link source/5}, but reads the syntax tree and the
%% comments from the specified file.
%%
%% @see edoc:read_comments/2
%% @see edoc:read_source/2
%% @see source/4

source(File, Env, Opts) ->
    Forms = edoc:read_source(File, Opts),
    Comments = edoc:read_comments(File, Opts),
    source(Forms, Comments, File, Env, Opts).

%% @spec source(Forms, Comments::[edoc:comment()], File::filename(),
%%              Env::edoc_env(), Options::proplist()) ->
%%           {ModuleName, edoc:edoc_module()}
%%
%%    Forms = syntaxTree() | [syntaxTree()]
%%    ModuleName = atom()
%%
%% @doc Like {@link source/4}, but first inserts the given comments in
%% the syntax trees. The syntax trees must contain valid position
%% information. (Cf. {@link edoc:read_comments/2}.)
%%
%% @see edoc:read_comments/2
%% @see edoc:read_source/2
%% @see source/3
%% @see source/4
%% @see //syntax_tools/erl_recomment

source(Forms, Comments, File, Env, Opts) when is_list(Forms) ->
    Forms1 = erl_syntax:form_list(Forms),
    source(Forms1, Comments, File, Env, Opts);
source(Forms, Comments, File, Env, Opts) ->
    Tree = erl_recomment:quick_recomment_forms(Forms, Comments),
    TypeDocs = find_type_docs(Forms, Comments, Env, File),
    source1(Tree, File, Env, Opts, TypeDocs).

%% @spec source(Forms, File::filename(), Env::edoc_env(),
%%              Options::proplist()) ->
%%           {ModuleName, edoc:edoc_module()}
%%
%%	    Forms = syntaxTree() | [syntaxTree()]
%%	    ModuleName = atom()
%% @type edoc_env() = edoc_lib:edoc_env()
%%
%% @doc Extracts EDoc documentation from commented source code syntax
%% trees. The given `Forms' must be a single syntax tree of
%% type `form_list', or a list of syntax trees representing
%% "program forms" (cf. {@link edoc:read_source/2}.
%% `Env' is an environment created by {@link
%% edoc_lib:get_doc_env/3}. The `File' argument is used for
%% error reporting and output file name generation only.
%%
%% See {@link edoc:get_doc/2} for descriptions of the `def',
%% `hidden', `private', and `todo' options.
%%
%% @see edoc:read_comments/2
%% @see edoc:read_source/2
%% @see source/5
%% @see //syntax_tools/erl_recomment

%% Note that the actual module name found in the source file will be
%% used for generating the documentation, creating relative links, etc.

%% INHERIT-OPTIONS: add_macro_defs/3
%% INHERIT-OPTIONS: edoc_data:module/4

source(Forms, File, Env, Opts) when is_list(Forms) ->
    source(erl_syntax:form_list(Forms), File, Env, Opts);
source(Tree, File0, Env, Opts) ->
    TypeDocs = find_type_docs(Tree, [], Env, File0),
    source1(Tree, File0, Env, Opts, TypeDocs).

%% Forms0 and Comments is used for extracting Erlang type documentation.
source1(Tree, File0, Env, Opts, TypeDocs) ->
    Forms = preprocess_forms(Tree),
    File = edoc_lib:filename(File0),
    Module = get_module_info(Tree, File),
    {Header, Footer, Entries} = collect(Forms, Module),
    Name = Module#module.name,
    Env1 = Env#env{module = Name,
		   root = ""},
    Env2 = add_macro_defs(module_macros(Env1), Opts, Env1),
    Entries1 = get_tags([Header, Footer | Entries], Env2, File, TypeDocs),
    Entries2 = edoc_specs:add_data(Entries1, Opts, File, Module),
    edoc_tags:check_types(Entries2, Opts, File),
    Data = edoc_data:module(Module, Entries2, Env2, Opts),
    {Name, Data}.

%% @spec header(File::filename(), Env::edoc_env(), Options::proplist())
%%             -> {ok, Tags} | {error, Reason}
%%   Tags = [term()]
%%   Reason = term()
%%
%% @doc Similar to {@link header/5}, but reads the syntax tree and the
%% comments from the specified file.
%%
%% @see edoc:read_comments/2
%% @see edoc:read_source/2
%% @see header/4

header(File, Env, Opts) ->
    Forms = edoc:read_source(File),
    Comments = edoc:read_comments(File),
    header(Forms, Comments, File, Env, Opts).

%% @spec header(Forms, Comments::[edoc:comment()], File::filename(),
%%              Env::edoc_env(), Options::proplist()) ->
%%       {ok, Tags} | {error, Reason}
%%   Forms = syntaxTree() | [syntaxTree()]
%%   Tags = [term()]
%%   Reason = term()
%%
%% @doc Similar to {@link header/4}, but first inserts the given
%% comments in the syntax trees. The syntax trees must contain valid
%% position information. (Cf. {@link edoc:read_comments/2}.)
%%
%% @see header/3
%% @see header/4
%% @see //syntax_tools/erl_recomment

header(Forms, Comments, File, Env, Opts) when is_list(Forms) ->
    Forms1 = erl_syntax:form_list(Forms),
    header(Forms1, Comments, File, Env, Opts);
header(Forms, Comments, File, Env, Opts) ->
    Tree = erl_recomment:quick_recomment_forms(Forms, Comments),
    header(Tree, File, Env, Opts).

%% @spec header(Forms, File::filename(), Env::edoc_env(),
%%              Options::proplist()) ->
%%       {ok, Tags} | {error, Reason}
%%   Forms = syntaxTree() | [syntaxTree()]
%%   Tags = [term()]
%%   Reason = term()
%%
%% @doc Extracts EDoc documentation from commented header file syntax
%% trees. Similar to {@link source/5}, but ignores any documentation
%% that occurs before a module declaration or a function definition.
%% (Warning messages are printed if content may be ignored.) `Env' is
%% assumed to already be set up with a suitable module context.
%%
%% @see header/5
%% @see //syntax_tools/erl_recomment

header(Forms, File, Env, Opts) when is_list(Forms) ->
    header(erl_syntax:form_list(Forms), File, Env, Opts);
header(Tree, File0, Env, _Opts) ->
    Forms = preprocess_forms(Tree),
    File = edoc_lib:filename(File0),
    Module = #module{name = Env#env.module},  % a dummy module record
    %% We take only "footer" tags, i.e., any kind of definition will
    %% kill all the information above it up to that point. Then we call
    %% this the 'header' to make error reports make better sense.
    {Header, Footer, Entries} = collect(Forms, Module),
    if Header#entry.data /= {[],[],[]} ->
	    warning(File, "documentation before module declaration is ignored by @headerfile", []);
       true -> ok
    end,
    if Entries /= [] ->
	    warning(File, "documentation before function definitions is ignored by @headerfile", []);
       true -> ok
    end,
    [Entry] = get_tags([Footer#entry{name = header}], Env, File),
    Entry#entry.data.

%% NEW-OPTIONS: def
%% DEFER-OPTIONS: source/4

add_macro_defs(Defs0, Opts, Env) ->
    Defs = proplists:append_values(def, Opts),
    edoc_macros:check_defs(Defs),
    Env#env{macros = Defs ++ Defs0 ++ Env#env.macros}.

%% @spec file(File::filename(), Context, Env::edoc_env(),
%%            Options::proplist()) -> {ok, Tags} | {error, Reason}
%%   Context = overview
%%   Tags = [term()]
%%   Reason = term()
%%
%% @doc Reads a text file and returns the list of tags in the file. Any
%% lines of text before the first tag are ignored. `Env' is an
%% environment created by {@link edoc_lib:get_doc_env/3}. Upon error,
%% `Reason' is an atom returned from the call to {@link
%% //kernel/file:read_file/1} or the atom 'invalid_unicode'.
%%
%% See {@link text/4} for options.

%% INHERIT-OPTIONS: text/4

file(File, Context, Env, Opts) ->
    case file:read_file(File) of
	{ok, Bin} ->
            Enc = edoc_lib:read_encoding(File,[{in_comment_only, false}]),
            case catch unicode:characters_to_list(Bin, Enc) of
                String when is_list(String) ->
                    {ok, text(String, Context, Env, Opts, File)};
                _ ->
                    {error, invalid_unicode}
            end;
        {error, _} = Error ->
            Error
    end.


%% @spec (Text::string(), Context, Env::edoc_env(),
%%        Options::proplist()) -> Tags
%%     Context = overview
%%     Tags = [term()]
%%
%% @doc Returns the list of tags in the text. Any lines of text before
%% the first tag are ignored. `Env' is an environment created by {@link
%% edoc_lib:get_doc_env/3}.
%%
%% See {@link source/4} for a description of the `def' option.

%% INHERIT-OPTIONS: add_macro_defs/3
%% DEFER-OPTIONS: source/4

text(Text, Context, Env, Opts) ->
    text(Text, Context, Env, Opts, "").

text(Text, Context, Env, Opts, Where) ->
    Env1 = add_macro_defs(file_macros(Context, Env), Opts, Env),
    Cs = edoc_lib:lines(Text),
    Ts0 = edoc_tags:scan_lines(Cs, 1),
    Tags = sets:from_list(edoc_tags:tag_names()),
    Ts1 = edoc_tags:filter_tags(Ts0, Tags, Where),
    Single = sets:from_list(edoc_tags:tags(single)),
    Allow = sets:from_list(edoc_tags:tags(Context)),
    case edoc_tags:check_tags(Ts1, Allow, Single, Where) of
	true ->
	    exit(error);
	false ->
	    Ts2 = edoc_macros:expand_tags(Ts1, Env1, Where),
	    How = dict:from_list(edoc_tags:tag_parsers()),
	    edoc_tags:parse_tags(Ts2, How, Env1, Where)
    end.


%% @spec (Forms::[syntaxTree()], File::filename()) -> module()
%% @doc Initialises a module-info record with data about the module
%% represented by the list of forms. Exports are guaranteed to exist in
%% the set of defined names.

get_module_info(Forms, File) ->
    L = case catch {ok, erl_syntax_lib:analyze_forms(Forms)} of
	    {ok, L1} ->
		L1;
	    syntax_error ->
		report(File, "syntax error in input.", []),
		exit(error);
	    {'EXIT', R} ->
		exit(R);
	    R ->
		throw(R)
	end,
    {Name, Vars} = case lists:keyfind(module, 1, L) of
		       {module, N} when is_atom(N) ->
			   {N, none};
		       {module, {N, _}=Mod} when is_atom(N) ->
			   Mod;
		       _ ->
			   report(File, "module name missing.", []),
			   exit(error)
		   end,
    Functions = ordsets:from_list(get_list_keyval(functions, L)),
    Exports = ordsets:from_list(get_list_keyval(exports, L)),
    Attributes = ordsets:from_list(get_list_keyval(attributes, L)),
    Records = get_list_keyval(records, L),
    Encoding = edoc_lib:read_encoding(File, []),
    #module{name = Name,
	    parameters = Vars,
	    functions = Functions,
	    exports = ordsets:intersection(Exports, Functions),
	    attributes = Attributes,
	    records = Records,
	    encoding = Encoding}.

get_list_keyval(Key, L) ->
    case lists:keyfind(Key, 1, L) of
	{Key, As} ->
	    ordsets:from_list(As);
	_ ->
	    []
    end.

%% @spec (Forms::[syntaxTree()]) -> [syntaxTree()]
%% @doc Preprocessing: copies any precomments on forms to standalone
%% comments, and removes "invisible" forms from the list.

preprocess_forms(Tree) ->
    preprocess_forms_1(erl_syntax:form_list_elements(
			 erl_syntax:flatten_form_list(Tree))).

preprocess_forms_1([F | Fs]) ->
    case erl_syntax:get_precomments(F) of
	[] ->
	    preprocess_forms_2(F, Fs);
	Cs ->
	    Cs ++ preprocess_forms_2(F, Fs)
    end;
preprocess_forms_1([]) ->
    [].

preprocess_forms_2(F, Fs) ->
    case erl_syntax_lib:analyze_form(F) of
	comment ->
	    [F | preprocess_forms_1(Fs)];
	{function, _} ->
	    [F | preprocess_forms_1(Fs)];
	{attribute, {module, _}} ->
	    [F | preprocess_forms_1(Fs)];
  	text ->
  	    [F | preprocess_forms_1(Fs)];
        {attribute, {record, _}} ->
            [F | preprocess_forms_1(Fs)];
        {attribute, {N, _}} ->
            case edoc_specs:is_tag(N) of
                true ->
                    [F | preprocess_forms_1(Fs)];
                false ->
                    preprocess_forms_1(Fs)
            end;
	_ ->
	    preprocess_forms_1(Fs)
    end.

%% This collects the data for the header and the functions of the
%% module. Note that the list of forms is assumed to have been
%% preprocessed first, so that all "invisible" forms are removed, and
%% the only interesting comments are those that are standalone comments
%% in the list.

collect(Fs, Mod) ->
    collect(Fs, [], [], [], [], [], undefined, Mod).

collect([F | Fs], Cs, Ss, Ts, Rs, As, Header, Mod) ->
    case erl_syntax_lib:analyze_form(F) of
	comment ->
	    collect(Fs, [F | Cs], Ss, Ts, Rs, As, Header, Mod);
	{function, Name} ->
	    L = erl_syntax:get_pos(F),
	    Export = ordsets:is_element(Name, Mod#module.exports),
	    Args = parameters(erl_syntax:function_clauses(F)),
	    collect(Fs, [], [], [], [],
                    [#entry{name = Name, args = Args, line = L,
                            export = Export,
                            data = {comment_text(Cs),Ss,Ts,Rs}} | As],
		    Header, Mod);
	{attribute, {module, _}} when Header =:= undefined ->
	    L = erl_syntax:get_pos(F),
	    collect(Fs, [], [], [], [], As,
                    #entry{name = module, line = L,
                           data = {comment_text(Cs),Ss,Ts,Rs}},
		    Mod);
        {attribute, {record, {_Name, Fields}}} ->
            case is_typed_record(Fields) of
                true ->
                    collect(Fs, Cs, Ss, Ts, [F | Rs], As, Header, Mod);
                false ->
                    collect(Fs, Cs, Ss, Ts, Rs, As, Header, Mod)
            end;
        {attribute, {N, _}} ->
            case edoc_specs:tag(N) of
                spec ->
                    collect(Fs, Cs, [F | Ss], Ts, Rs, As, Header, Mod);
                type ->
                    collect(Fs, Cs, Ss, [F | Ts], Rs, As, Header, Mod);
                unknown ->
                    %% Drop current seen comments.
                    collect(Fs, [], [], [], Rs, As, Header, Mod)
            end;
	_ ->
	    %% Drop current seen comments.
	    collect(Fs, [], [], [], [], As, Header, Mod)
    end;
collect([], Cs, Ss, Ts, Rs, As, Header, _Mod) ->
    Footer = #entry{name = footer, data = {comment_text(Cs),Ss,Ts,Rs}},
    As1 = lists:reverse(As),
    if Header =:= undefined ->
	    {#entry{name = module, data = {[],[],[],[]}}, Footer, As1};
       true ->
	    {Header, Footer, As1}
    end.

is_typed_record([]) ->
    false;
is_typed_record([{_, {_, Type}} | Fs]) ->
    Type =/= none orelse is_typed_record(Fs).

%% Returns a list of simplified comment information (position and text)
%% for a list of abstract comments. The order of elements is reversed.

comment_text(Cs) ->
    comment_text(Cs, []).

comment_text([C | Cs], Ss) ->
    L = erl_syntax:get_pos(C),
    comment_text(Cs, [#comment{line = L,
			       text = [remove_percent_chars(S)
				       || S <- erl_syntax:comment_text(C)]}
		      | Ss]);
comment_text([], Ss) ->
    Ss.

%% @spec (string()) -> string()
%%
%% @doc Replaces leading `%' characters by spaces. For example, `"%%%
%% foo" -> "\s\s\s foo"', but `"% % foo" -> "\s % foo"', since the
%% second `%' is preceded by whitespace.

remove_percent_chars([$% | Cs]) -> [$\s | remove_percent_chars(Cs)];
remove_percent_chars(Cs) -> Cs.

%% Extracting possible parameter names from Erlang clause patterns.  The
%% atom '_' is used when no name can be found. (Better names are made up
%% later, when we also may have typespecs available; see edoc_data.)

parameters(Clauses) ->
    select_names([find_names(Ps) || Ps <- patterns(Clauses)]).

patterns(Cs) ->
    edoc_lib:transpose([erl_syntax:clause_patterns(C) || C <- Cs]).

find_names(Ps) ->
    find_names(Ps, []).

find_names([P | Ps], Ns) ->
    case erl_syntax:type(P) of
	variable ->
	    find_names(Ps, [tidy_name(erl_syntax:variable_name(P)) | Ns]);
	match_expr ->
	    %% Right-hand side gets priority over left-hand side!
	    %% Note that the list is reversed afterwards.
	    P1 = erl_syntax:match_expr_pattern(P),
	    P2 = erl_syntax:match_expr_body(P),
	    find_names([P1, P2 | Ps], Ns);
	list ->
	    P1 = erl_syntax:list_tail(P),
	    find_names([P1 | Ps], Ns);
	record_expr ->
	    A = erl_syntax:record_expr_type(P),
            AtomName = erl_syntax:atom_name(A),
            Atom = list_to_atom(AtomName),
            case AtomName =:= lists:flatten(io_lib:write_atom(Atom)) of
                true ->
                    N = list_to_atom(capitalize(AtomName)),
                    find_names(Ps, [N | Ns]);
                false ->
                    find_names(Ps, Ns)
            end;
	infix_expr ->
	    %% this can only be a '++' operation
	    P1 = erl_syntax:infix_expr_right(P),
	    find_names([P1 | Ps], Ns);
	_ ->
	    find_names(Ps, Ns)
    end;
find_names([], Ns) ->
    lists:reverse(Ns).

select_names(Ls) ->
    select_names(Ls, [], sets:new()).

select_names([Ns | Ls], As, S) ->
    A = select_name(Ns, S),
    select_names(Ls, [A | As], sets:add_element(A, S));
select_names([], As, _) ->
    lists:reverse(As).

select_name([A | Ns], S) ->
    case sets:is_element(A, S) of
	true ->
	    select_name(Ns, S);
	false ->
	    A
    end;
select_name([], _S) ->
    '_'.

%% Strip leading underscore characters from parameter names. If the
%% result does not begin with an uppercase character, we add a single
%% leading underscore. If the result would be empty, the atom '_' is
%% returned.

tidy_name(A) ->
    case atom_to_list(A) of
	[$_ | Cs] ->
	    list_to_atom(tidy_name_1(Cs));
	_ ->
	    A
    end.

tidy_name_1([$_ | Cs]) -> tidy_name_1(Cs);
tidy_name_1([C | _]=Cs) when C >= $A, C =< $Z -> Cs;
tidy_name_1([C | _]=Cs) when C >= $\300, C =< $\336, C =/= $\327-> Cs;
tidy_name_1(Cs) -> [$_ | Cs].

%% Change initial character from lowercase to uppercase.

capitalize([C | Cs]) when C >= $a, C =< $z -> [C - 32 | Cs];
capitalize([C | Cs]) when C >= $\340, C =< $\376, C /= $\367 -> [C - 32 | Cs];
capitalize(Cs) -> Cs.

%% Collects the tags belonging to each entry, checks them, expands
%% macros and parses the content.

%% %This is commented out until it can be made private
%% %@type tags() = #tags{names = set(atom()),
%% %                     single = set(atom()),
%% %                     module = set(atom()),
%% %                     footer = set(atom()),
%% %                     function = set(atom())}
%% %  set(T) = sets:set(T)

-record(tags, {names,single,module,function,footer}).

get_tags(Es, Env, File) ->
    get_tags(Es, Env, File, dict:new()).

get_tags(Es, Env, File, TypeDocs) ->
    %% Cache this stuff for quick lookups.
    Tags = #tags{names = sets:from_list(edoc_tags:tag_names()),
		 single = sets:from_list(edoc_tags:tags(single)),
		 module = sets:from_list(edoc_tags:tags(module)),
		 footer = sets:from_list(edoc_tags:tags(footer)),
		 function = sets:from_list(edoc_tags:tags(function))},
    How = dict:from_list(edoc_tags:tag_parsers()),
    get_tags(Es, Tags, Env, How, File, TypeDocs).

get_tags([#entry{name = Name, data = {Cs,Specs,Types,Records}} = E | Es],
         Tags, Env, How, File, TypeDocs) ->
    Where = {File, Name},
    Ts0 = scan_tags(Cs),
    {Ts1,Specs1} = select_spec(Ts0, Where, Specs),
    Ts2 = check_tags(Ts1, Tags, Where),
    Ts3 = edoc_macros:expand_tags(Ts2, Env, Where),
    Ts4 = edoc_tags:parse_tags(Ts3, How, Env, Where),
    Ts = selected_specs(Specs1, Ts4),
    ETypes = [edoc_specs:type(Type, TypeDocs) || Type <- Types ++ Records],
    [E#entry{data = Ts++ETypes} | get_tags(Es, Tags, Env, How, File, TypeDocs)];
get_tags([], _, _, _, _, _) ->
    [].

%% Scanning a list of separate comments for tags.

scan_tags([#comment{line = L, text = Ss} | Es]) ->
    edoc_tags:scan_lines(Ss, L) ++ scan_tags(Es);
scan_tags([]) ->
    [].

%% Check the set of found tags (depending on context).
%% Completely unknown tags are filtered out with a warning.

check_tags(Ts0, Tags, Where) ->
    Ts = edoc_tags:filter_tags(Ts0, Tags#tags.names, Where),
    case check_tags_1(Ts, Tags, Where) of
	false -> Ts;
	true -> exit(error)
    end.

check_tags_1(Ts, Tags, {_, module} = Where) ->
    Allow = Tags#tags.module,
    Single = Tags#tags.single,
    edoc_tags:check_tags(Ts, Allow, Single, Where);
check_tags_1(Ts, Tags, {_, footer} = Where) ->
    Allow = Tags#tags.footer,
    Single = Tags#tags.single,
    edoc_tags:check_tags(Ts, Allow, Single, Where);
check_tags_1(Ts, Tags, Where) ->
    Allow = Tags#tags.function,
    Single = Tags#tags.single,
    edoc_tags:check_tags(Ts, Allow, Single, Where).

select_spec(Ts, {_, {_F, _A}}, Specs) ->
    case edoc_tags:filter_tags(Ts, sets:from_list([spec])) of
        [] ->
            %% Just a dummy to get us through check_tags()
            {[edoc_specs:dummy_spec(S) || S <- Specs] ++ Ts, Specs};
        _ ->
            {Ts,[]}
    end;
select_spec(Ts, _Where, _Specs) ->
    {Ts,[]}.

selected_specs([], Ts) ->
    Ts;
selected_specs([F], [_ | Ts]) ->
    [edoc_specs:spec(F, _Clause=1) | Ts].

%% Macros for modules

module_macros(Env) ->
    [{module, atom_to_list(Env#env.module)}]
	++ edoc_macros:std_macros(Env).

%% Macros for reading auxiliary edoc-files

file_macros(_Context, Env) ->
    edoc_macros:std_macros(Env).

%% @doc Extracts what will be documentation of Erlang types.
%% Returns a dict of {Name, Doc} where Name is {TypeName, Arity}.
%%
%% The idea is to mimic how the @type tag works.
%% Using @type:
%%```@type t() = t1(). Some docs of t/0;
%%   Further docs of t/0.'''
%% The same thing using -type:
%%```-type t() :: t1(). % Some docs of t/0;
%%   Further docs of t/0.'''
find_type_docs(Forms0, Comments, Env, File) ->
    Tree = erl_recomment:recomment_forms(Forms0, Comments),
    Forms = preprocess_forms(Tree),
    Env1 = add_macro_defs(edoc_macros:std_macros(Env), [], Env),
    F = fun(C, Line) -> find_fun(C, Line, Env1, File) end,
    edoc_specs:docs(Forms, F).

find_fun(C0, Line, Env, File) ->
    C1 = comment_text(C0),
    Text = lists:append([C#comment.text || C <- C1]),
    Comm = #comment{line = Line, text = Text},
    [Tag | _] = scan_tags([Comm]),
    [Tag1] = edoc_macros:expand_tags([Tag], Env, File),
    Tag1.
