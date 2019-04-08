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
%% @private
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Building the EDoc external data structure. See the file
%% <a href="../priv/edoc.dtd">`edoc.dtd'</a> for details.

-module(edoc_data).

-export([module/4, overview/4, type/2]).

-export([hidden_filter/2, get_all_tags/1]).

-include("edoc.hrl").

%% TODO: check that variables in @equiv are found in the signature
%% TODO: copy types from target (if missing) when using @equiv

%% <!ELEMENT module (args?, description?, author*, copyright?,
%%                   version?, since?, deprecated?, see*, reference*,
%%                   todo?, behaviour*, callbacks?, typedecls?,
%%                   functions)>
%% <!ATTLIST module
%%   name CDATA #REQUIRED
%%   private NMTOKEN(yes | no) #IMPLIED
%%   hidden NMTOKEN(yes | no) #IMPLIED
%%   root CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>
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
%% <!ELEMENT behaviour (#PCDATA)>
%% <!ATTLIST behaviour
%%   href CDATA #IMPLIED>
%% <!ELEMENT callbacks (callback+)>
%% <!ELEMENT typedecls (typedecl+)>
%% <!ELEMENT typedecl (typedef, description?)>
%% <!ELEMENT functions (function+)>

%% NEW-OPTIONS: private, hidden, todo
%% DEFER-OPTIONS: edoc_extract:source/4

module(Module, Entries, Env, Opts) ->
    Name = atom_to_list(Module#module.name),
    HeaderEntry = get_entry(module, Entries),
    HeaderTags = HeaderEntry#entry.data,
    AllTags = get_all_tags(Entries),
    Functions = function_filter(Entries, Opts),
    Out = {module, ([{name, Name},
		     {root, Env#env.root},
                     {encoding, Module#module.encoding}]
		    ++ case is_private(HeaderTags) of
			   true -> [{private, "yes"}];
			   false -> []
		       end
		    ++ case is_hidden(HeaderTags) of
			   true -> [{hidden, "yes"}];
			   false -> []
		       end),
	   (module_args(Module#module.parameters)
	    ++ behaviours(Module#module.attributes, Env)
	    ++ get_doc(HeaderTags)
	    ++ authors(HeaderTags)
	    ++ get_version(HeaderTags)
	    ++ get_since(HeaderTags)
	    ++ get_copyright(HeaderTags)
	    ++ get_deprecated(HeaderTags)
	    ++ sees(HeaderTags, Env)
	    ++ references(HeaderTags)
	    ++ todos(HeaderTags, Opts)
	    ++ [{typedecls, types(AllTags, Env)},
		{functions, functions(Functions, Env, Opts)}
		| callbacks(Functions, Module, Env, Opts)])
	  },
    xmerl_lib:expand_element(Out).

get_all_tags(Es) ->
    lists:flatmap(fun (#entry{data = Ts}) -> Ts end, Es).

is_private(Ts) ->
    get_tags(private, Ts) =/= [].

description([]) ->
    [];
description(Desc) ->
    ShortDesc = edoc_lib:get_first_sentence(Desc),
    [{description,
      [{briefDescription, ShortDesc},
       {fullDescription, Desc}]}].

module_args(none) ->
    [];
module_args(Vs) ->
    [{args, [{arg, [{argName, [atom_to_list(V)]}]} || V <- Vs]}].

types(Tags, Env) ->
    [{typedecl, [{label, edoc_types:to_label(Def)}],
      [edoc_types:to_xml(Def, Env)] ++ description(Doc)}
     || #tag{name = type, data = {Def, Doc}} <- Tags].

functions(Es, Env, Opts) ->
    [function(N, As, Export, Ts, Env, Opts)
     || #entry{name = {_,_}=N, args = As, export = Export, data = Ts}
	    <- Es].

hidden_filter(Es, Opts) ->
    Private = proplists:get_bool(private, Opts),
    Hidden = proplists:get_bool(hidden, Opts),
    [E || E <- Es,
          case E#entry.name of
              {_, _} -> function_filter(E, Private, Hidden);
              _ -> true
          end].

function_filter(Es, Opts) ->
    Private = proplists:get_bool(private, Opts),
    Hidden = proplists:get_bool(hidden, Opts),
    [E || E <- Es, function_filter(E, Private, Hidden)].

%% Note that only entries whose names have the form {_,_} are functions.
function_filter(#entry{name = {_,_}, export = Export, data = Ts},
		Private, Hidden) ->
    ((Export andalso not is_private(Ts)) orelse Private)
	andalso ((not is_hidden(Ts)) orelse Hidden);
function_filter(_, _, _) ->
    false.

is_hidden(Ts) ->
    get_tags(hidden, Ts) =/= [].

callbacks(Es, Module, Env, Opts) ->
    case lists:any(fun (#entry{name = {behaviour_info, 1}}) -> true;
		       (_) -> false
		   end,
		   Es)
	orelse
	lists:keymember(callback, 1, Module#module.attributes)
    of
	true ->
            M = Module#module.name,
            Fs = get_callback_functions(M, callbacks),
            Os1 = get_callback_functions(M, optional_callbacks),
            Fs1 = [FA || FA <- Fs, not lists:member(FA, Os1)],
            Req = if Fs1 =:= [] ->
                          [];
                     true ->
                          [{callbacks,
                            [callback(FA, Env, Opts) || FA <- Fs1]}]
                  end,
            Opt = if Os1 =:= [] ->
                          [];
                     true ->
                          [{optional_callbacks,
                            [callback(FA, Env, Opts) || FA <- Os1]}]
                  end,
            Req ++ Opt;
	false -> []
    end.

get_callback_functions(M, Callbacks) ->
    try
        [FA || {F, A} = FA <- M:behaviour_info(Callbacks),
               is_atom(F), is_integer(A), A >= 0]
    catch
        _:_ -> []
    end.

%% <!ELEMENT callback EMPTY>
%% <!ATTLIST callback
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED>

callback({N, A}, _Env, _Opts) ->
    {callback, [{name, atom_to_list(N)},
		{arity, integer_to_list(A)}],
     []}.

%% <!ELEMENT function (args, typespec?, returns?, throws?, equiv?,
%%                     description?, since?, deprecated?, see*, todo?)>
%% <!ATTLIST function
%%   name CDATA #REQUIRED
%%   arity CDATA #REQUIRED
%%   exported NMTOKEN(yes | no) #REQUIRED
%%   label CDATA #IMPLIED>
%% <!ELEMENT args (arg*)>
%% <!ELEMENT arg (argName, description?)>
%% <!ELEMENT argName (#PCDATA)>
%% <!ELEMENT returns (description)>
%% <!ELEMENT throws (type, localdef*)>
%% <!ELEMENT equiv (expr, see?)>
%% <!ELEMENT expr (#PCDATA)>

function({N, A}, As, Export, Ts, Env, Opts) ->
    {Args, Ret, Spec} = signature(Ts, As, Env),
    {function, [{name, atom_to_list(N)},
		{arity, integer_to_list(A)},
      		{exported, case Export of
			       true -> "yes";
			       false -> "no"
			   end},
		{label, edoc_refs:to_label(edoc_refs:function(N, A))}],
     [{args, [{arg, [{argName, [atom_to_list(A)]}] ++ description(D)}
	      || {A, D} <- Args]}]
     ++ Spec
     ++ case Ret of
	    [] -> [];
	    _ -> [{returns, description(Ret)}]
	end
     ++ get_throws(Ts, Env)
     ++ get_equiv(Ts, Env)
     ++ get_doc(Ts)
     ++ get_since(Ts)
     ++ get_deprecated(Ts, N, A, Env)
     ++ sees(Ts, Env)
     ++ todos(Ts, Opts)
    }.

get_throws(Ts, Env) ->
    case get_tags(throws, Ts) of
	[Throws] ->
	    Type = Throws#tag.data,
	    [edoc_types:to_xml(Type, Env)];
	[] ->
	    []
    end.

get_equiv(Ts, Env) ->
    case get_tags(equiv, Ts) of
	[Equiv] ->
	    Expr = Equiv#tag.data,
	    See = case get_expr_ref(Equiv#tag.data) of
		      none -> [];
		      Ref ->
			  [see(Ref, [edoc_refs:to_string(Ref)], Env)]
		  end,
	    [{equiv, [{expr, [erl_prettypr:format(Expr)]} | See]}];
	[] ->
	    []
    end.

get_doc(Ts) ->
    case get_tags(doc, Ts) of
	[T] ->
	    description(T#tag.data);
	[] ->
	    []
    end.

get_copyright(Ts) ->
    get_pcdata_tag(copyright, Ts).

get_version(Ts) ->
    get_pcdata_tag(version, Ts).

get_since(Ts) ->
    get_pcdata_tag(since, Ts).

get_pcdata_tag(Tag, Ts) ->
    case get_tags(Tag, Ts) of
	[T] ->
	    [{Tag, [T#tag.data]}];
	[] ->
	    []
    end.

%% Deprecation declarations for xref:
%%
%%   -deprecated(Info).
%%       Info = Spec | [Spec]
%%       Spec = module | {F,A} | {F,A,Details}}
%%       Details = next_version | next_major_release | eventually
%%                 (EXTENSION: | string() | {M1,F1,A1}}
%% TODO: use info from '-deprecated(...)' (xref-)declarations.

get_deprecated(Ts) ->
    case get_tags(deprecated, Ts) of
	[T] ->
	    [{deprecated, description(T#tag.data)}];
	[] ->
	    []
    end.

get_deprecated(Ts, F, A, Env) ->
    case get_deprecated(Ts) of
	[] ->
	    M = Env#env.module,
	    case otp_internal:obsolete(M, F, A) of
		{Tag, Text} when Tag =:= deprecated; Tag =:= removed ->
		    deprecated([Text]);
		{Tag, Repl, _Rel} when Tag =:= deprecated; Tag =:= removed ->
		    deprecated(Repl, Env);
		_ ->
		    []
	    end;
	Es ->
	    Es
    end.

deprecated(Repl, Env) ->
    {Text, Ref} = replacement_function(Env#env.module, Repl),
    Desc = ["Use ", {a, href(Ref, Env), [{code, [Text]}]}, " instead."],
    deprecated(Desc).

deprecated(Desc) ->
    [{deprecated, description(Desc)}].

-dialyzer({no_match, replacement_function/2}).

replacement_function(M0, {M,F,A}) when is_list(A) ->
    %% refer to the largest listed arity - the most general version
    replacement_function(M0, {M,F,lists:last(lists:sort(A))});
replacement_function(M, {M,F,A}) ->
    {io_lib:fwrite("~w/~w", [F, A]), edoc_refs:function(F, A)};
replacement_function(_, {M,F,A}) ->
    {io_lib:fwrite("~w:~w/~w", [M, F, A]), edoc_refs:function(M, F, A)}.

get_expr_ref(Expr) ->
    case catch {ok, erl_syntax_lib:analyze_application(Expr)} of
	{ok, {F, A}} when is_atom(F), is_integer(A) ->
	    edoc_refs:function(F, A);
 	{ok, {M, {F, A}}} when is_atom(M), is_atom(F), is_integer(A) ->
 	    edoc_refs:function(M, F, A);
	_ ->
	    none
    end.

authors(Ts) ->
    [author(Info) || #tag{data = Info} <- get_tags(author, Ts)].

%% <!ATTLIST author
%%   name CDATA #REQUIRED
%%   email CDATA #IMPLIED
%%   website CDATA #IMPLIED>

author({Name, Mail, URI}) ->
    %% At least one of Name and Mail must be nonempty in the tag.
    {author, ([{name, if Name =:= "" -> Mail;
			 true -> Name
		      end}]
	      ++ if Mail =:= "" ->
			 case lists:member($@, Name) of
			     true -> [{email, Name}];
			     false -> []
			 end;
		    true -> [{email, Mail}]
		 end
	      ++ if URI =:= "" -> [];
		    true -> [{website, URI}]
		 end), []}.

behaviours(As, Env) ->
    [{behaviour, href(edoc_refs:module(B), Env), [atom_to_list(B)]}
     || {behaviour, B} <- As, is_atom(B)].

sees(Tags, Env) ->
    Ts = get_tags(see, Tags),
    Rs = lists:keysort(1, [Data || #tag{data = Data} <- Ts]),
    [see(Ref, XML, Env) || {Ref, XML} <- Rs].

see(Ref, [], Env) ->
    see(Ref, [edoc_refs:to_string(Ref)], Env);
see(Ref, XML, Env) ->
    {see, [{name, edoc_refs:to_string(Ref)}] ++ href(Ref, Env), XML}.

href(Ref, Env) ->
    [{href, edoc_refs:get_uri(Ref, Env)}]
	++ case edoc_refs:is_top(Ref, Env) of
	       true ->
		   [{target, "_top"}];
	       false ->
		   []
	   end.

references(Tags) ->
    [{reference, XML} || #tag{data = XML} <- get_tags(reference, Tags)].

todos(Tags, Opts) ->
    case proplists:get_bool(todo, Opts) of
	true ->
	    [{todo, XML} || #tag{data = XML} <- get_tags('todo', Tags)];
	false ->
	    []
    end.

signature(Ts, As, Env) ->
    case get_tags(spec, Ts) of
	[T] ->
	    Spec = T#tag.data,
	    R = merge_returns(Spec, Ts),
	    As0 = edoc_types:arg_names(Spec),
	    Ds0 = edoc_types:arg_descs(Spec),
	    %% choose names in spec before names in code
	    P = dict:from_list(params(Ts)),
	    As1 = merge_args(As0, As, Ds0, P),
	    %% check_params(As1, P),
	    Spec1 = edoc_types:set_arg_names(Spec, [A || {A,_} <- As1]),
	    {As1, R, [edoc_types:to_xml(Spec1, Env)]};
	[] ->
	    S = sets:new(),
	    {[{A, ""} || A <- fix_argnames(As, S, 1)], [], []}
    end.

params(Ts) ->
    [T#tag.data || T <- get_tags(param, Ts)].

%% check_params(As, P) ->
%%     case dict:keys(P) -- [N || {N,_} <- As] of
%% 	[] -> ok;
%% 	Ps -> error  %% TODO: report @param declarations with no match
%%     end.

merge_returns(Spec, Ts) ->
    case get_tags(returns, Ts) of
	[] ->
	    case edoc_types:range_desc(Spec) of
		"" -> [];
		Txt -> [Txt]
	    end;
	[T] -> T#tag.data
    end.

%% Names are chosen from the first list (the specification) if possible.
%% Descriptions specified with @param (in P dict) override descriptions
%% from the spec (in Ds).

merge_args(As, As1, Ds, P) ->
    merge_args(As, As1, Ds, [], P, sets:new(), 1).

merge_args(['_' | As], ['_' | As1], [D | Ds], Rs, P, S, N) ->
    merge_args(As, As1, Ds, Rs, P, S, N, make_name(N, S), D);
merge_args(['_' | As], [A | As1], [D | Ds], Rs, P, S, N) ->
    merge_args(As, As1, Ds, Rs, P, S, N, A, D);
merge_args([A | As], [_ | As1], [D | Ds], Rs, P, S, N) ->
    merge_args(As, As1, Ds, Rs, P, S, N, A, D);
merge_args([], [], [], Rs, _P, _S, _N) ->
    lists:reverse(Rs).

merge_args(As, As1, Ds, Rs, P, S, N, A, D0) ->
    D = case dict:find(A, P) of
	    {ok, D1} -> D1;
	    error when D0 =:= [] -> [];  % no description
	    error -> [D0]  % a simple-xml text element
	end,
    merge_args(As, As1, Ds, [{A, D} | Rs], P,
	       sets:add_element(A, S), N + 1).

fix_argnames(['_' | As], S, N) ->
    A = make_name(N, S),
    [A | fix_argnames(As, sets:add_element(A, S), N + 1)];
fix_argnames([A | As], S, N) ->
    [A | fix_argnames(As, sets:add_element(A, S), N + 1)];
fix_argnames([], _S, _N) ->
    [].

make_name(N, S) ->
    make_name(N, S, "X").

make_name(N, S, Base) ->
    A = list_to_atom(Base ++ integer_to_list(N)),
    case sets:is_element(A, S) of
 	true ->
 	    make_name(N, S, Base ++ "x");
 	false ->
 	    A
    end.

get_entry(Name, [#entry{name = Name} = E | _Es]) -> E;
get_entry(Name, [_ | Es]) -> get_entry(Name, Es).

get_tags(Tag, [#tag{name = Tag} = T | Ts]) -> [T | get_tags(Tag, Ts)];
get_tags(Tag, [_ | Ts]) -> get_tags(Tag, Ts);
get_tags(_, []) -> [].

%% ---------------------------------------------------------------------

type(T, Env) ->
    xmerl_lib:expand_element({type, [edoc_types:to_xml(T, Env)]}).

%% <!ELEMENT overview (title, description?, author*, copyright?, version?,
%%                     since?, see*, reference*, todo?, modules)>
%% <!ATTLIST overview
%%   root CDATA #IMPLIED>
%% <!ELEMENT title (#PCDATA)>

overview(Title, Tags, Env, Opts) ->
    Env1 = Env#env{
		   root = ""},
    xmerl_lib:expand_element(overview_1(Title, Tags, Env1, Opts)).

overview_1(Title, Tags, Env, Opts) ->
    {overview, [{root, Env#env.root}],
     ([{title, [get_title(Tags, Title)]}]
      ++ get_doc(Tags)
      ++ authors(Tags)
      ++ get_copyright(Tags)
      ++ get_version(Tags)
      ++ get_since(Tags)
      ++ sees(Tags, Env)
      ++ references(Tags)
      ++ todos(Tags, Opts))
    }.

get_title(Ts, Default) ->
    case get_tags(title, Ts) of
	[T] ->
	    T#tag.data;
	[] ->
	    Default
    end.
