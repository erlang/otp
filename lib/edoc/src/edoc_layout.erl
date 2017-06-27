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
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2001-2006 Richard Carlsson
%% @see edoc
%% @end
%% =====================================================================

%% @doc The standard HTML layout module for EDoc. See the {@link edoc}
%% module for details on usage.

%% Note that this is written so that it is *not* depending on edoc.hrl!

-module(edoc_layout).

-export([module/2, overview/2, type/1]).

-import(edoc_report, [report/2]).

-include_lib("xmerl/include/xmerl.hrl").

-define(HTML_EXPORT, xmerl_html).
-define(DEFAULT_XML_EXPORT, ?HTML_EXPORT).
-define(OVERVIEW_SUMMARY, "overview-summary.html").
-define(STYLESHEET, "stylesheet.css").
-define(NL, "\n").
-define(DESCRIPTION_TITLE, "Description").
-define(DESCRIPTION_LABEL, "description").
-define(DATA_TYPES_TITLE, "Data Types").
-define(DATA_TYPES_LABEL, "types").
-define(FUNCTION_INDEX_TITLE, "Function Index").
-define(FUNCTION_INDEX_LABEL, "index").
-define(FUNCTIONS_TITLE, "Function Details").
-define(FUNCTIONS_LABEL, "functions").

%% @doc The layout function.
%%
%% Options to the standard layout:
%% <dl>
%%  <dt>{@type {index_columns, integer()@}}
%%  </dt>
%%  <dd>Specifies the number of column pairs used for the function
%%      index tables. The default value is 1.
%%  </dd>
%%  <dt>{@type {pretty_printer, atom()@}}
%%  </dt>
%%  <dd>Specifies how types and specifications are pretty printed.
%%      If the value `erl_pp' is specified the Erlang pretty printer
%%      (the module `erl_pp') will be used. The default is to do
%%      no pretty printing which implies that lines can be very long.
%%  </dd>
%%  <dt>{@type {stylesheet, string()@}}
%%  </dt>
%%  <dd>Specifies the URI used for referencing the stylesheet. The
%%      default value is `"stylesheet.css"'. If an empty string is
%%      specified, no stylesheet reference will be generated.
%%  </dd>
%%  <dt>{@type {sort_functions, boolean()@}}
%%  </dt>
%%  <dd>If `true', the detailed function descriptions are listed by
%%      name, otherwise they are listed in the order of occurrence in
%%      the source file. The default value is `true'.
%%  </dd>
%%  <dt>{@type {xml_export, Module::atom()@}}
%%  </dt>
%%  <dd>Specifies an {@link //xmerl. `xmerl'} callback module to be
%%      used for exporting the documentation. See {@link
%%      //xmerl/xmerl:export_simple/3} for details.
%%  </dd>
%% </dl>
%%
%% @see edoc:layout/2

%% NEW-OPTIONS: xml_export, index_columns, stylesheet

module(Element, Options) ->
    XML = layout_module(Element, init_opts(Element, Options)),
    Export = proplists:get_value(xml_export, Options,
				 ?DEFAULT_XML_EXPORT),
    xmerl:export_simple(XML, Export, []).

% Put layout options in a data structure for easier access.

%% %Commented out until it can be made private
%% %@type opts() = #opts{root = string(),
%% %                     stylesheet = string(),
%% %                     index_columns = integer()}

-record(opts, {root,
               stylesheet,
               index_columns,
               sort_functions,
               encoding,
               pretty_printer}).

init_opts(Element, Options) ->
    Encoding = case get_attrval(encoding, Element) of
                   "latin1" -> latin1;
                   _ -> utf8
               end,
    R = #opts{root = get_attrval(root, Element),
	      index_columns = proplists:get_value(index_columns,
						  Options, 1),
	      sort_functions = proplists:get_value(sort_functions,
						   Options, true),
              encoding = Encoding,
              pretty_printer = proplists:get_value(pretty_printer,
                                                   Options, '')
	     },
    case proplists:get_value(stylesheet, Options) of
	undefined ->
	    S = edoc_lib:join_uri(R#opts.root, ?STYLESHEET),
	    R#opts{stylesheet = S};
	"" ->
	    R;  % don't use any stylesheet
	S when is_list(S) ->
	    R#opts{stylesheet = S};
	_ ->
	    report("bad value for option `stylesheet'.", []),
	    exit(error)
    end.


%% =====================================================================
%% XML-BASED LAYOUT ENGINE
%% =====================================================================

%% We assume that we have expanded XML data.

%% <!ELEMENT module (behaviour*, description?, author*, copyright?,
%%                   version?, since?, deprecated?, see*, reference*,
%%                   todo?, typedecls?, functions)>
%% <!ATTLIST module
%%   name CDATA #REQUIRED
%%   private NMTOKEN(yes | no) #IMPLIED
%%   root CDATA #IMPLIED>
%% <!ELEMENT behaviour (#PCDATA)>
%% <!ATTLIST behaviour
%%   href CDATA #IMPLIED>
%% <!ELEMENT description (briefDescription, fullDescription?)>
%% <!ELEMENT briefDescription (#PCDATA)>
%% <!ELEMENT fullDescription (#PCDATA)>
%% <!ELEMENT author EMPTY>
%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>
%% <!ELEMENT version (#PCDATA)>
%% <!ELEMENT since (#PCDATA)>
%% <!ELEMENT copyright (#PCDATA)>
%% <!ELEMENT deprecated (description)>
%% <!ELEMENT see (#PCDATA)>
%% <!ATTLIST see
%%   name CDATA #REQUIRED
%%   href CDATA #IMPLIED>
%% <!ELEMENT reference (#PCDATA)>
%% <!ELEMENT todo (#PCDATA)>
%% <!ELEMENT typedecls (typedecl+)>
%% <!ELEMENT functions (function+)>

%% TODO: improve layout of parameterized modules

layout_module(#xmlElement{name = module, content = Es}=E, Opts) ->
    Args = module_params(get_content(args, Es)),
    Name = get_attrval(name, E),
    Title = case get_elem(args, Es) of
		[] -> ["Module ", Name];
		_ -> ["Abstract module ", Name, " [", {Args}, "]"]
	    end,
    Desc = get_content(description, Es),
    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Functions = [{function_name(E, Opts), E} ||
                    E <- get_content(functions, Es)],
    Types = [{type_name(E, Opts), E} || E <- get_content(typedecls, Es)],
    SortedFs = if Opts#opts.sort_functions -> lists:sort(Functions);
                  true -> Functions
               end,
    Body = (navigation("top")
            ++ [?NL, hr, ?NL, ?NL, {h1, Title}, ?NL]
	    ++ doc_index(FullDesc, Functions, Types)
	    ++ ShortDesc
	    ++ [?NL]
	    ++ copyright(Es)
	    ++ deprecated(Es, "module")
	    ++ [?NL]
	    ++ version(Es)
	    ++ since(Es)
	    ++ behaviours(Es, Name, Opts)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ todos(Es)
	    ++ if FullDesc == [] -> [];
		  true -> [?NL,
			   {h2, [{a, [{name, "description"}],
				  ["Description"]}]}
			   | FullDesc]
	       end
	    ++ types(lists:sort(Types), Opts)
	    ++ function_index(SortedFs, Opts#opts.index_columns)
	    ++ functions(SortedFs, Opts)
	    ++ [hr, ?NL]
	    ++ navigation("bottom")
	    ++ footer()),
    Encoding = Opts#opts.encoding,
    xhtml(Title, stylesheet(Opts), Body, Encoding).

module_params(Es) ->
    As = [{get_text(argName, Es1),
	   get_content(fullDescription, get_content(description, Es1))}
	  || #xmlElement{content = Es1} <- Es],
    case As of
	[] -> [];
	[First | Rest] ->
	    [element(1, First) | [ {[", ",A]} || {A, _D} <- Rest]]
    end.

footer() ->
    [?NL, {p, [{i, ["Generated by EDoc"]}]}, ?NL].

stylesheet(Opts) ->
    case Opts#opts.stylesheet of
	undefined ->
	    [];
	CSS ->
	    [{link, [{rel, "stylesheet"},
		     {type, "text/css"},
		     {href, CSS},
		     {title, "EDoc"}], []},
	     ?NL]
    end.

navigation(Where) ->
    [?NL,
     {'div', [{class, "navbar"}],
      [{a, [{name, "#navbar_" ++ Where}], []},
       {table, [{width, "100%"}, {border,0},
		{cellspacing, 0}, {cellpadding, 2},
		{summary, "navigation bar"}],
	[{tr,
	  [{td, [{a, [{href, ?OVERVIEW_SUMMARY}, {target,"overviewFrame"}],
		  ["Overview"]}]},
	   {td, [{a, [{href, "http://www.erlang.org/"}],
		  [{img, [{src, "erlang.png"}, {align, "right"},
			  {border, 0}, {alt, "erlang logo"}],
		    []}]}
		]}
	  ]}
	]}
      ]}
    ].

doc_index(FullDesc, Functions, Types) ->
    case doc_index_rows(FullDesc, Functions, Types) of
	[] -> [];
	Rs ->
	    [{ul, [{class, "index"}],
	      [{li, [{a, [{href, local_label(R)}], [T]}]}
	       || {T, R} <- Rs]}]
    end.

doc_index_rows(FullDesc, Functions, Types) ->
    (if FullDesc == [] -> [];
	true -> [{?DESCRIPTION_TITLE, ?DESCRIPTION_LABEL}]
     end
     ++ if Types == [] -> [];
	   true -> [{?DATA_TYPES_TITLE, ?DATA_TYPES_LABEL}]
	end
     ++ if Functions == [] -> [];
	   true -> [{?FUNCTION_INDEX_TITLE, ?FUNCTION_INDEX_LABEL},
		    {?FUNCTIONS_TITLE, ?FUNCTIONS_LABEL}]
	end).

function_index(Fs, Cols) ->
    case function_index_rows(Fs, Cols, []) of
	[] -> [];
	Rows ->
	    [?NL,
	     {h2, [{a, [{name, ?FUNCTION_INDEX_LABEL}],
		    [?FUNCTION_INDEX_TITLE]}]},
	     ?NL,
	     {table, [{width, "100%"}, {border, 1},
		      {cellspacing,0}, {cellpadding,2},
		      {summary, "function index"}],
	      Rows},
	     ?NL]
    end.

function_index_rows(Fs, Cols, Title) ->
    Rows = (length(Fs) + (Cols - 1)) div Cols,
    (if Title == [] -> [];
	true -> [{tr, [{th, [{colspan, Cols * 2}, {align, left}],
			[Title]}]},
		 ?NL]
     end
     ++ lists:flatmap(fun index_row/1,
		      edoc_lib:transpose(edoc_lib:segment(Fs, Rows)))).

index_row(Fs) ->
    [{tr, lists:flatmap(fun index_col/1, Fs)}, ?NL].

index_col({Name, F=#xmlElement{content = Es}}) ->
    [{td, [{valign, "top"}],
      label_href(function_header(Name, F, "*"), F)},
     {td, index_desc(Es)}].

index_desc(Es) ->
    Desc = get_content(description, Es),
    (case get_content(deprecated, Es) of
 	 [] -> [];
 	 _ -> ["(", {em, ["Deprecated"]}, ".) "]
     end
     ++ case get_content(briefDescription, Desc) of
	    [] ->
		equiv(Es);    % no description at all if no equiv
	    ShortDesc ->
		ShortDesc
	end).

label_href(Content, F) ->
    case get_attrval(label, F) of
	"" -> Content;
	Ref -> [{a, [{href, local_label(Ref)}], Content}]
    end.

%% <!ELEMENT function (args, typespec?, returns?, throws?, equiv?,
%%                     description?, since?, deprecated?, see*, todo?)>
%% <!ATTLIST function
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED
%%   exported NMTOKEN(yes | no) #REQUIRED
%%   label CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT equiv (expr, see?)>
%% <!ELEMENT expr (#PCDATA)>

functions(Fs, Opts) ->
    Es = lists:flatmap(fun ({Name, E}) -> function(Name, E, Opts) end, Fs),
    if Es == [] -> [];
       true ->
	    [?NL,
	     {h2, [{a, [{name, ?FUNCTIONS_LABEL}], [?FUNCTIONS_TITLE]}]},
	     ?NL | Es]
    end.

function(Name, E=#xmlElement{content = Es}, Opts) ->
    ([?NL,
      {h3, [{class, "function"}],
       label_anchor(function_header(Name, E, " *"), E)},
      ?NL]
     ++ [{'div',  [{class, "spec"}],
	  [?NL,
	   {p,
	    case typespec(get_content(typespec, Es), Opts) of
		[] ->
		    signature(get_content(args, Es),
			      atom(get_attrval(name, E), Opts));
		Spec -> Spec
	    end},
	   ?NL]
	  ++ case params(get_content(args, Es)) of
		 [] -> [];
		 Ps -> [{p, Ps}, ?NL]
	     end
	  ++ case returns(get_content(returns, Es)) of
		 [] -> [];
		 Rs -> [{p, Rs}, ?NL]
	     end}]
     ++ throws(Es, Opts)
     ++ equiv_p(Es)
     ++ deprecated(Es, "function")
     ++ fulldesc(Es)
     ++ since(Es)
     ++ sees(Es)
     ++ todos(Es)).

function_name(E, Opts) ->
    atom(get_attrval(name, E), Opts) ++ "/" ++ get_attrval(arity, E).

function_header(Name, E, Private) ->
    case is_exported(E) of
	true -> [Name];
	false -> [Name, Private]
    end.

is_exported(E) ->
    case get_attrval(exported, E) of
 	"yes" -> true;
 	_ -> false
    end.

label_anchor(Content, E) ->
    case get_attrval(label, E) of
	"" -> Content;
	Ref -> [{a, [{name, Ref}], Content}]
    end.

%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>

%% This is currently only done for functions without type spec.

signature(Es, Name) ->
    [{tt, [Name, "("] ++ seq(fun arg/1, Es) ++ [") -> any()"]}].

arg(#xmlElement{content = Es}) ->
    [get_text(argName, Es)].

%% parameter and return value descriptions (if any)

params(Es) ->
    As = [{get_text(argName, Es1),
	   get_content(fullDescription, get_content(description, Es1))}
	  || #xmlElement{content = Es1} <- Es],
    As1 = [A || A <- As, element(2, A) /= []],
    if As1 == [] ->
	    [];
       true ->
	    [ { [{tt, [A]}, ": "] ++  D ++ [br, ?NL] }
	      || {A, D} <- As1]
    end.

returns(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] ->
	    [];
	D ->
	    ["returns: "] ++  D
    end.

%% <!ELEMENT throws (type, localdef*)>

throws(Es, Opts) ->
    case get_content(throws, Es) of
	[] -> [];
	Es1 ->
            %% Doesn't use format_type; keep it short!
	    [{p, (["throws ", {tt, t_utype(get_elem(type, Es1), Opts)}]
		  ++ local_defs(get_elem(localdef, Es1), Opts))},
	     ?NL]
    end.

%% <!ELEMENT typespec (erlangName, type, localdef*)>

typespec([], _Opts) -> [];
typespec(Es, Opts) ->
    Name = t_name(get_elem(erlangName, Es), Opts),
    Defs = get_elem(localdef, Es),
    [Type] = get_elem(type, Es),
    format_spec(Name, Type, Defs, Opts) ++ local_defs(Defs, Opts).

%% <!ELEMENT typedecl (typedef, description?)>
%% <!ELEMENT typedef (erlangName, argtypes, type?, localdef*)>

types([], _Opts) -> [];
types(Ts, Opts) ->
    Es = lists:flatmap(fun ({Name, E}) -> typedecl(Name, E, Opts) end, Ts),
    [?NL,
     {h2, [{a, [{name, ?DATA_TYPES_LABEL}],
	    [?DATA_TYPES_TITLE]}]},
     ?NL | Es].

typedecl(Name, E=#xmlElement{content = Es}, Opts) ->
    ([?NL, {h3, [{class, "typedecl"}], label_anchor([Name, "()"], E)}, ?NL]
     ++ [{p, typedef(get_content(typedef, Es), Opts)}, ?NL]
     ++ fulldesc(Es)).

type_name(#xmlElement{content = Es}, Opts) ->
    t_name(get_elem(erlangName, get_content(typedef, Es)), Opts).

typedef(Es, Opts) ->
    Name = ([t_name(get_elem(erlangName, Es), Opts), "("]
            ++ seq(t_utype_elem_fun(Opts), get_content(argtypes, Es), [")"])),
    (case get_elem(type, Es) of
 	 [] -> [{b, ["abstract datatype"]}, ": ", {tt, Name}];
         Type -> format_type(Name, Name, Type, [], Opts)
     end
     ++ local_defs(get_elem(localdef, Es), Opts)).

local_defs(Es, Opts) ->
    local_defs(Es, [], Opts).

local_defs([], _, _Opts) -> [];
local_defs(Es0, Last, Opts) ->
    [E | Es] = lists:reverse(Es0),
    [?NL,
     {ul, [{class, "definitions"}],
      lists:reverse(lists:append([localdef(E1, [], Opts) || E1 <- Es]),
                    localdef(E, Last, Opts))}].

localdef(E = #xmlElement{content = Es}, Last, Opts) ->
    Name = case get_elem(typevar, Es) of
               [] ->
                   label_anchor(N0 = t_abstype(get_content(abstype, Es),
                                               Opts),
                                E);
               [V] ->
                   N0 = t_var(V)
           end,
    [{li, format_type(Name, N0, get_elem(type, Es), Last, Opts)}].

%% Use the default formatting of EDoc, which creates references, and
%% then insert newlines and indentation according to erl_pp (the
%% (fast) Erlang pretty printer).
format_spec(Name, Type, Defs, #opts{pretty_printer = erl_pp}=Opts) ->
    try
        L = t_clause(Name, Type, Opts),
        O = pp_clause(Name, Type, Opts),
        {R, ".\n"} = etypef(L, O, Opts),
        [{pre, R}]
    catch _:_ ->
        %% Should not happen.
        format_spec(Name, Type, Defs, Opts#opts{pretty_printer=''})
    end;
format_spec(Sep, Type, Defs, Opts) ->
    %% Very limited formatting.
    Br = if Defs =:= [] -> br; true -> [] end,
    [{tt, t_clause(Sep, Type, Opts)}, Br].

t_clause(Name, Type, Opts) ->
    #xmlElement{content = [#xmlElement{name = 'fun', content = C}]} = Type,
    [Name] ++ t_fun(C, Opts).

pp_clause(Pre, Type, Opts) ->
    Types = ot_utype([Type]),
    Atom = lists:duplicate(string:length(Pre), $a),
    Attr = {attribute,0,spec,{{list_to_atom(Atom),0},[Types]}},
    L1 = erl_pp:attribute(erl_parse:new_anno(Attr),
                          [{encoding, Opts#opts.encoding}]),
    "-spec " ++ L2 = lists:flatten(L1),
    L3 = Pre ++ lists:nthtail(length(Atom), L2),
    re:replace(L3, "\n      ", "\n", [{return,list},global,unicode]).

format_type(Prefix, Name, Type, Last, #opts{pretty_printer = erl_pp}=Opts) ->
    try
        L = t_utype(Type, Opts),
        O = pp_type(Name, Type, Opts),
        {R, ".\n"} = etypef(L, O, Opts),
        [{pre, Prefix ++ [" = "] ++ R ++ Last}]
    catch _:_ ->
        %% Example: "t() = record(a)."
        format_type(Prefix, Name, Type, Last, Opts#opts{pretty_printer =''})
    end;
format_type(Prefix, _Name, Type, Last, Opts) ->
    [{tt, Prefix ++ [" = "] ++ t_utype(Type, Opts) ++ Last}].

pp_type(Prefix, Type, Opts) ->
    Atom = list_to_atom(lists:duplicate(string:length(Prefix), $a)),
    Attr = {attribute,0,type,{Atom,ot_utype(Type),[]}},
    L1 = erl_pp:attribute(erl_parse:new_anno(Attr),
                          [{encoding, Opts#opts.encoding}]),
    {L2,N} = case lists:dropwhile(fun(C) -> C =/= $: end, lists:flatten(L1)) of
                 ":: " ++ L3 -> {L3,9}; % compensation for extra "()" and ":"
                 "::\n" ++ L3 -> {"\n"++L3,6}
             end,
    Ss = lists:duplicate(N, $\s),
    re:replace(L2, "\n"++Ss, "\n", [{return,list},global,unicode]).

etypef(L, O0, Opts) ->
    {R, O} = etypef(L, [], O0, [], Opts),
    {lists:reverse(R), O}.

etypef([C | L], St, [C | O], R, Opts) ->
    etypef(L, St, O, [[C] | R], Opts);
etypef(" "++L, St, O, R, Opts) ->
    etypef(L, St, O, R, Opts);
etypef("", [Cs | St], O, R, Opts) ->
    etypef(Cs, St, O, R, Opts);
etypef("", [], O, R, _Opts) ->
    {R, O};
etypef(L, St, " "++O, R, Opts) ->
    etypef(L, St, O, [" " | R], Opts);
etypef(L, St, "\n"++O, R, Opts) ->
    Ss = lists:takewhile(fun(C) -> C =:= $\s end, O),
    etypef(L, St, lists:nthtail(length(Ss), O), ["\n"++Ss | R], Opts);
etypef([{a, HRef, S0} | L], St, O0, R, Opts) ->
    {S, O} = etypef(S0, app_fix(O0, Opts), Opts),
    etypef(L, St, O, [{a, HRef, S} | R], Opts);
etypef("="++L, St, "::"++O, R, Opts) ->
    %% EDoc uses "=" for record field types; Erlang types use "::".
    %% Maybe there should be an option for this, possibly affecting
    %% other similar discrepancies.
    etypef(L, St, O, ["=" | R], Opts);
etypef([Cs | L], St, O, R, Opts) ->
    etypef(Cs, [L | St], O, R, Opts).

app_fix(L, Opts) ->
    try
        {"//" ++ R1,L2} = app_fix1(L, 1),
        [App, Mod] = string:lexemes(R1, "/"),
        "//" ++ atom(App, Opts) ++ "/" ++ atom(Mod, Opts) ++ L2
    catch _:_ -> L
    end.

app_fix1(L, I) -> % a bit slow
    {L1, L2} = lists:split(I, L),
    case erl_scan:tokens([], L1 ++ ". ", 1) of
        {done, {ok,[{atom,_,Atom}|_],_}, _} -> {atom_to_list(Atom), L2};
        _ -> app_fix1(L, I+1)
    end.

fulldesc(Es) ->
    case get_content(fullDescription, get_content(description, Es)) of
	[] -> [?NL];
	Desc -> [{p, Desc}, ?NL]
    end.

sees(Es) ->
    case get_elem(see, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["See also:"]}, " "] ++ seq(fun see/1, Es1, ["."])},
	     ?NL]
    end.

see(E=#xmlElement{content = Es}) ->
    see(E, Es).

see(E, Es) ->
    case href(E) of
	[] -> Es;
	Ref ->
	    [{a, Ref, Es}]
    end.

href(E) ->
    case get_attrval(href, E) of
	"" -> [];
	URI ->
	    T = case get_attrval(target, E) of
		    "" -> [];
		    S -> [{target, S}]
		end,
	    [{href, URI} | T]
    end.

equiv_p(Es) ->
    equiv(Es, true).

equiv(Es) ->
    equiv(Es, false).

equiv(Es, P) ->
    case get_content(equiv, Es) of
	[] -> [];
	Es1 ->
	    case get_content(expr, Es1) of
		[] -> [];
		[Expr] ->
		    Expr1 = [{tt, [Expr]}],
		    Expr2 = case get_elem(see, Es1) of
				[] ->
				    Expr1;
				[E=#xmlElement{}] ->
				    see(E, Expr1)
			    end,
		    Txt = ["Equivalent to "] ++ Expr2 ++ ["."],
		    (case P of
			 true -> [{p, Txt}];
			 false -> Txt
		     end
		     ++ [?NL])
	    end
    end.

copyright(Es) ->
    case get_content(copyright, Es) of
	[] -> [];
	Es1 ->
	    [{p, ["Copyright \251 " | Es1]}, ?NL]
    end.

version(Es) ->
    case get_content(version, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Version:"]}, " " | Es1]}, ?NL]
    end.

since(Es) ->
    case get_content(since, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Introduced in:"]}, " " | Es1]}, ?NL]
    end.

deprecated(Es, S) ->
    Es1 = get_content(description, get_content(deprecated, Es)),
    case get_content(fullDescription, Es1) of
	[] -> [];
	Es2 ->
	    [{p, [{b, ["This " ++ S ++ " is deprecated:"]}, " " | Es2]},
	     ?NL]
    end.

behaviours(Es, Name, Opts) ->
    CBs = get_content(callbacks, Es),
    OCBs = get_content(optional_callbacks, Es),
    (case get_elem(behaviour, Es) of
	 [] -> [];
	 Es1 ->
	     [{p, ([{b, ["Behaviours:"]}, " "]
		   ++ seq(fun behaviour/1, Es1, ["."]))},
	      ?NL]
     end
     ++
     if CBs =:= [], OCBs =:= [] ->
             [];
	 true ->
             CBFun = fun(E) -> callback(E, Opts) end,
             Req = if CBs =:= [] ->
                       [];
                       true ->
                           [br, " Required callback functions: "]
                           ++ seq(CBFun, CBs, ["."])
                   end,
             Opt = if OCBs =:= [] ->
                       [];
                       true ->
                           [br, " Optional callback functions: "]
                           ++ seq(CBFun, OCBs, ["."])
                   end,
	     [{p, ([{b, ["This module defines the ", {tt, [Name]},
			 " behaviour."]}]
                   ++ Req ++ Opt)},
	      ?NL]
     end).

behaviour(E=#xmlElement{content = Es}) ->
    see(E, [{tt, Es}]).

callback(E=#xmlElement{}, Opts) ->
    Name = get_attrval(name, E),
    Arity = get_attrval(arity, E),
    [{tt, [atom(Name, Opts), "/", Arity]}].

authors(Es) ->
    case get_elem(author, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["Authors:"]}, " "] ++ seq(fun author/1, Es1, ["."])},
	     ?NL]
    end.

atom(String, #opts{encoding = latin1}) ->
    io_lib:write_atom_as_latin1(list_to_atom(String));
atom(String, #opts{encoding = utf8}) ->
    io_lib:write_atom(list_to_atom(String)).

%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>

author(E=#xmlElement{}) ->
    Name = get_attrval(name, E),
    Mail = get_attrval(email, E),
    URI = get_attrval(website, E),
    (if Name == Mail ->
	     [{a, [{href, "mailto:" ++ Mail}],[{tt, [Mail]}]}];
	true ->
	     if Mail == "" -> [Name];
		true -> [Name, " (", {a, [{href, "mailto:" ++ Mail}],
				      [{tt, [Mail]}]}, ")"]
	     end
     end
     ++ if URI == "" ->
		[];
	   true ->
		[" [", {em, ["web site:"]}, " ",
		 {tt, [{a, [{href, URI}, {target, "_top"}], [URI]}]},
		 "]"]
	end).

references(Es) ->
    case get_elem(reference, Es) of
	[] -> [];
	Es1 ->
	    [{p, [{b, ["References"]},
		  {ul, [{li, C} || #xmlElement{content = C} <- Es1]}]},
	     ?NL]
    end.

todos(Es) ->
    case get_elem(todo, Es) of
	[] -> [];
	Es1 ->
	    Todos = [{li, [{font, [{color,red}], C}]}
		     || #xmlElement{content = C} <- Es1],
	    [{p, [{b, [{font, [{color,red}], ["To do"]}]},
		  {ul, Todos}]},
	     ?NL]
    end.

t_name([E], Opts) ->
    N = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> atom(N, Opts);
	M ->
	    S = atom(M, Opts) ++ ":" ++ atom(N, Opts),
	    case get_attrval(app, E) of
		"" -> S;
		A -> "//" ++ atom(A, Opts) ++ "/" ++ S
	    end
    end.

t_utype([E], Opts) ->
    t_utype_elem(E, Opts).

t_utype_elem_fun(Opts) ->
    fun(E) -> t_utype_elem(E, Opts) end.

t_utype_elem(E=#xmlElement{content = Es}, Opts) ->
    case get_attrval(name, E) of
	"" -> t_type(Es, Opts);
	Name ->
	    T = t_type(Es, Opts),
	    case T of
		[Name] -> T;    % avoid generating "Foo::Foo"
		T -> [Name] ++ ["::"] ++ T
	    end
    end.

t_type([E=#xmlElement{name = typevar}], _Opts) ->
    t_var(E);
t_type([E=#xmlElement{name = atom}], Opts) ->
    t_atom(E, Opts);
t_type([E=#xmlElement{name = integer}], _Opts) ->
    t_integer(E);
t_type([E=#xmlElement{name = range}], _Opts) ->
    t_range(E);
t_type([E=#xmlElement{name = binary}], _Opts) ->
    t_binary(E);
t_type([E=#xmlElement{name = float}], _Opts) ->
    t_float(E);
t_type([#xmlElement{name = nil}], _Opts) ->
    t_nil();
t_type([#xmlElement{name = paren, content = Es}], Opts) ->
    t_paren(Es, Opts);
t_type([#xmlElement{name = list, content = Es}], Opts) ->
    t_list(Es, Opts);
t_type([#xmlElement{name = nonempty_list, content = Es}], Opts) ->
    t_nonempty_list(Es, Opts);
t_type([#xmlElement{name = map, content = Es}], Opts) ->
    t_map(Es, Opts);
t_type([#xmlElement{name = tuple, content = Es}], Opts) ->
    t_tuple(Es, Opts);
t_type([#xmlElement{name = 'fun', content = Es}], Opts) ->
    ["fun("] ++ t_fun(Es, Opts) ++ [")"];
t_type([E = #xmlElement{name = record, content = Es}], Opts) ->
    t_record(E, Es, Opts);
t_type([E = #xmlElement{name = abstype, content = Es}], Opts) ->
    t_abstype(E, Es, Opts);
t_type([#xmlElement{name = union, content = Es}], Opts) ->
    t_union(Es, Opts).

t_var(E) ->
    [get_attrval(name, E)].

t_atom(E, Opts) ->
    [atom(get_attrval(value, E), Opts)].

t_integer(E) ->
    [get_attrval(value, E)].

t_range(E) ->
    [get_attrval(value, E)].

t_binary(E) ->
    [get_attrval(value, E)].

t_float(E) ->
    [get_attrval(value, E)].

t_nil() ->
    ["[]"].

t_paren(Es, Opts) ->
    ["("] ++ t_utype(get_elem(type, Es), Opts) ++ [")"].

t_list(Es, Opts) ->
    ["["] ++ t_utype(get_elem(type, Es), Opts) ++ ["]"].

t_nonempty_list(Es, Opts) ->
    ["["] ++ t_utype(get_elem(type, Es), Opts) ++ [", ...]"].

t_tuple(Es, Opts) ->
    ["{"] ++ seq(t_utype_elem_fun(Opts), Es, ["}"]).

t_fun(Es, Opts) ->
    ["("] ++ seq(t_utype_elem_fun(Opts), get_content(argtypes, Es),
		 [") -> "] ++ t_utype(get_elem(type, Es), Opts)).

t_map(Es, Opts) ->
    Fs = get_elem(map_field, Es),
    ["#{"] ++ seq(fun(E) -> t_map_field(E, Opts) end, Fs, ["}"]).

t_map_field(#xmlElement{content = [K,V]}=E, Opts) ->
    KElem = t_utype_elem(K, Opts),
    VElem = t_utype_elem(V, Opts),
    AS = case get_attrval(assoc_type, E) of
             "assoc" -> " => ";
             "exact" -> " := "
         end,
    KElem ++ [AS] ++ VElem.

t_record(E, Es, Opts) ->
    Name = ["#"] ++ t_type(get_elem(atom, Es), Opts),
    case get_elem(field, Es) of
        [] ->
            see(E, [Name, "{}"]);
        Fs ->
            see(E, Name) ++ ["{"] ++ seq(fun(F) -> t_field(F, Opts) end,
                                         Fs, ["}"])
    end.

t_field(#xmlElement{content = Es}, Opts) ->
    (t_type(get_elem(atom, Es), Opts) ++ [" = "] ++
     t_utype(get_elem(type, Es), Opts)).

t_abstype(E, Es, Opts) ->
    Name = t_name(get_elem(erlangName, Es), Opts),
    case get_elem(type, Es) of
        [] ->
            see(E, [Name, "()"]);
        Ts ->
            see(E, [Name]) ++ ["("] ++ seq(t_utype_elem_fun(Opts), Ts, [")"])
    end.

t_abstype(Es, Opts) ->
    ([t_name(get_elem(erlangName, Es), Opts), "("]
     ++ seq(t_utype_elem_fun(Opts), get_elem(type, Es), [")"])).

t_union(Es, Opts) ->
    seq(t_utype_elem_fun(Opts), Es, " | ", []).

seq(F, Es) ->
    seq(F, Es, []).

seq(F, Es, Tail) ->
    seq(F, Es, ", ", Tail).

seq(F, [E], _Sep, Tail) ->
    F(E) ++ Tail;
seq(F, [E | Es], Sep, Tail) ->
    F(E) ++ [Sep] ++ seq(F, Es, Sep, Tail);
seq(_F, [], _Sep, Tail) ->
    Tail.

get_elem(Name, [#xmlElement{name = Name} = E | Es]) ->
    [E | get_elem(Name, Es)];
get_elem(Name, [_ | Es]) ->
    get_elem(Name, Es);
get_elem(_, []) ->
    [].

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_content(Name, Es) ->
    case get_elem(Name, Es) of
	[#xmlElement{content = Es1}] ->
	    Es1;
	[] -> []
    end.

get_text(Name, Es) ->
    case get_content(Name, Es) of
	[#xmlText{value = Text}] ->
	    Text;
	[] -> ""
    end.

local_label(R) ->
    "#" ++ R.

xhtml(Title, CSS, Body, Encoding) ->
    EncString = case Encoding of
                    latin1 -> "ISO-8859-1";
                    utf8 -> "UTF-8"
                end,
    [{html, [?NL,
	     {head, [?NL,
		     {meta, [{'http-equiv',"Content-Type"},
			     {content, "text/html; charset="++EncString}],
		      []},
		     ?NL,
		     {title, Title},
		     ?NL] ++ CSS},
	     ?NL,
	     {body, [{bgcolor, "white"}], Body},
	     ?NL]
     },
     ?NL].

%% ---------------------------------------------------------------------

type(E) ->
    Opts = init_opts(E, []),
    type(E, [], Opts).

type(E, Ds, Opts) ->
    xmerl:export_simple_content(t_utype_elem(E, Opts) ++ local_defs(Ds, Opts),
				?HTML_EXPORT).

overview(E=#xmlElement{name = overview, content = Es}, Options) ->
    Opts = init_opts(E, Options),
    Title = [get_text(title, Es)],
    Desc = get_content(description, Es),
%    ShortDesc = get_content(briefDescription, Desc),
    FullDesc = get_content(fullDescription, Desc),
    Body = (navigation("top")
	    ++ [?NL, {h1, [Title]}, ?NL]
%	    ++ ShortDesc
	    ++ copyright(Es)
	    ++ version(Es)
	    ++ since(Es)
	    ++ authors(Es)
	    ++ references(Es)
	    ++ sees(Es)
	    ++ todos(Es)
	    ++ FullDesc
	    ++ [?NL, hr]
	    ++ navigation("bottom")
	    ++ footer()),
    Encoding = Opts#opts.encoding,
    XML = xhtml(Title, stylesheet(Opts), Body, Encoding),
    xmerl:export_simple(XML, ?HTML_EXPORT, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NYTT

ot_utype([E]) ->
    ot_utype_elem(E).

ot_utype_elem(E=#xmlElement{content = Es}) ->
    case get_attrval(name, E) of
	"" -> ot_type(Es);
	N ->
            Name = {var,0,list_to_atom(N)},
	    T = ot_type(Es),
	    case T of
		Name -> T;
                T -> {ann_type,0,[Name, T]}
	    end
    end.

ot_type([E=#xmlElement{name = typevar}]) ->
    ot_var(E);
ot_type([E=#xmlElement{name = atom}]) ->
    ot_atom(E);
ot_type([E=#xmlElement{name = integer}]) ->
    ot_integer(E);
ot_type([E=#xmlElement{name = range}]) ->
    ot_range(E);
ot_type([E=#xmlElement{name = binary}]) ->
    ot_binary(E);
ot_type([E=#xmlElement{name = float}]) ->
    ot_float(E);
ot_type([#xmlElement{name = nil}]) ->
    ot_nil();
ot_type([#xmlElement{name = paren, content = Es}]) ->
    ot_paren(Es);
ot_type([#xmlElement{name = list, content = Es}]) ->
    ot_list(Es);
ot_type([#xmlElement{name = nonempty_list, content = Es}]) ->
    ot_nonempty_list(Es);
ot_type([#xmlElement{name = tuple, content = Es}]) ->
    ot_tuple(Es);
ot_type([#xmlElement{name = map, content = Es}]) ->
    ot_map(Es);
ot_type([#xmlElement{name = 'fun', content = Es}]) ->
    ot_fun(Es);
ot_type([#xmlElement{name = record, content = Es}]) ->
    ot_record(Es);
ot_type([#xmlElement{name = abstype, content = Es}]) ->
     ot_abstype(Es);
ot_type([#xmlElement{name = union, content = Es}]) ->
    ot_union(Es).

ot_var(E) ->
    {var,0,list_to_atom(get_attrval(name, E))}.

ot_atom(E) ->
    Name = list_to_atom(get_attrval(value, E)),
    {atom,erl_anno:new(0),Name}.

ot_integer(E) ->
    {integer,0,list_to_integer(get_attrval(value, E))}.

ot_range(E) ->
    [I1, I2] = string:lexemes(get_attrval(value, E), "."),
    {type,0,range,[{integer,0,list_to_integer(I1)},
                   {integer,0,list_to_integer(I2)}]}.

ot_binary(E) ->
    {Base, Unit} =
        case string:lexemes(get_attrval(value, E), ",:*><") of
            [] ->
                {0, 0};
            ["_",B] ->
                {list_to_integer(B), 0};
            ["_","_",U] ->
                {0, list_to_integer(U)};
            ["_",B,_,"_",U] ->
                {list_to_integer(B), list_to_integer(U)}
        end,
    {type,0,binary,[{integer,0,Base},{integer,0,Unit}]}.

ot_float(E) ->
    {float,0,list_to_float(get_attrval(value, E))}.

ot_nil() ->
    {nil,0}.

ot_paren(Es) ->
    {paren_type,0,[ot_utype(get_elem(type, Es))]}.

ot_list(Es) ->
    {type,0,list,[ot_utype(get_elem(type, Es))]}.

ot_nonempty_list(Es) ->
    {type,0,nonempty_list,[ot_utype(get_elem(type, Es))]}.

ot_tuple(Es) ->
    {type,0,tuple,[ot_utype_elem(E) || E <- Es]}.

ot_map(Es) ->
    {type,0,map,[ot_map_field(E) || E <- get_elem(map_field,Es)]}.

ot_map_field(#xmlElement{content=[K,V]}=E) ->
    A = case get_attrval(assoc_type, E) of
            "assoc" -> map_field_assoc;
            "exact" -> map_field_exact
        end,
    {type,0,A,[ot_utype_elem(K), ot_utype_elem(V)]}.

ot_fun(Es) ->
    Range = ot_utype(get_elem(type, Es)),
    Args = [ot_utype_elem(A) || A <- get_content(argtypes, Es)],
    {type,0,'fun',[{type,0,product,Args},Range]}.

ot_record(Es) ->
    {type,0,record,[ot_type(get_elem(atom, Es)) |
                    [ot_field(F) || F <- get_elem(field, Es)]]}.

ot_field(#xmlElement{content = Es}) ->
    {type,0,field_type,
     [ot_type(get_elem(atom, Es)), ot_utype(get_elem(type, Es))]}.

ot_abstype(Es) ->
    ot_name(get_elem(erlangName, Es),
            [ot_utype_elem(Elem) || Elem <- get_elem(type, Es)]).

ot_union(Es) ->
    {type,0,union,[ot_utype_elem(E) || E <- Es]}.

ot_name(Es, T) ->
    case ot_name(Es) of
        [Mod, ":", Atom] ->
            {remote_type,0,[{atom,0,list_to_atom(Mod)},
                            {atom,0,list_to_atom(Atom)},T]};
        "tuple" when T =:= [] ->
            {type,0,tuple,any};
        Atom ->
            {type,0,list_to_atom(Atom),T}
    end.

ot_name([E]) ->
    Atom = get_attrval(name, E),
    case get_attrval(module, E) of
	"" -> Atom;
	M ->
	    case get_attrval(app, E) of
		"" ->
                    [M, ":", Atom];
                A ->
                    ["//"++A++"/" ++ M, ":", Atom] % EDoc only!
	    end
    end.
