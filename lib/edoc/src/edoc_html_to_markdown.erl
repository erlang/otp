%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
%% %CopyrightEnd%

%% @doc This module can convert application/html+erlang style documentation to markdown.

%% Does translation of Erlang XML docs to EEP-48 doc chunks and
%% EEP-48 doc chunks to ex_doc markdown.
%%----------------------------------------------------------------------
-module(edoc_html_to_markdown).
-feature(maybe_expr, enable).

-include_lib("kernel/include/eep48.hrl").

-compile(nowarn_deprecated_catch).

-export([convert_html/2, convert_xml/2, convert_html/3, convert_xml/3]).

%% @doc
%% Convert `application/html+erlang' ({@type shell_docs:chunk_element()}) to Markdown
%% suitable for usage with ExDoc.
%% 
%% The `Application' and `Module' argument are used to correctly generate links.
-spec convert_html(Application :: atom(),
                   Module :: module(),
                   Html :: shell_docs:chunk_elements()) ->
          unicode:chardata().
convert_html(Application, Module, Html) when is_atom(Module), is_atom(Application) ->
    put(module, Module),
    put(application, atom_to_binary(Application)),
    render_docs(shell_docs:normalize(Html)).
%% @hidden
convert_html(Application, Html) when is_atom(Application) ->
    put(module, ''),
    put(application, atom_to_binary(Application)),
    render_docs(shell_docs:normalize(Html)).

%% @hidden
convert_xml(Application, Binary) when is_atom(Application) ->
    convert_xml(Application, '', Binary).
%% @hidden
convert_xml(Application, Module, Binary) when is_atom(Application), is_atom(Module), is_binary(Binary) ->
    put(application, atom_to_binary(Application)),
    case xmerl_sax_parser:stream(iolist_to_binary(["<section>",Binary,"</section>"]),
                                  [{event_fun, fun event/3},
                                   {event_state, initial_state()}]) of
        {ok, Tree, _} ->
            convert_html(Application, Module, transform(get_dom(Tree), []))
    end.

%%
%% The code below is taken from shell_docs and modified to emit Markdown instead
%%
-record(config, {docs, current = undefined }).

-define(ALL_ELEMENTS, [
    a,
    p,
    'div',
    br,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    hr,
    i,
    b,
    em,
    strong,
    pre,
    code,
    ul,
    ol,
    li,
    dl,
    dt,
    dd,
    table,
    tr,
    td
]).
%% inline elements are:
-define(INLINE, [i, b, em, strong, code, a]).
-define(IS_INLINE(ELEM),
    (((ELEM) =:= a) orelse ((ELEM) =:= code) orelse
        ((ELEM) =:= i) orelse ((ELEM) =:= em) orelse
        ((ELEM) =:= b) orelse ((ELEM) =:= strong))
).
%% non-inline elements are:
-define(BLOCK, [p, 'div', pre, br, ul, ol, li, dl, dt, dd, h1, h2, h3, h4, h5, h6, hr, table, tr, td]).
-define(IS_BLOCK(ELEM), not ?IS_INLINE(ELEM)).
-define(IS_PRE(ELEM), ((ELEM) =:= pre)).
-define(IS_HDR(ELEM),
        (((ELEM) =:= h1) orelse ((ELEM) =:= h2) orelse
         ((ELEM) =:= h3) orelse ((ELEM) =:= h4) orelse
         ((ELEM) =:= h5) orelse ((ELEM) =:= h6))).


%%% General rendering functions
render_docs(DocContents) ->
    render_docs(DocContents, init_config(#docs_v1{ docs = [] }, #{})).
render_docs(DocContents, #config{} = Config) ->
    render_docs(DocContents, 0, Config).
render_docs(DocContents, Ind, D = #config{}) when is_integer(Ind) ->
    try
        {Doc, _} = trimnl(render_docs(preprocess_docs(DocContents, D), [], 0, Ind, D)),
        unicode:characters_to_binary(Doc)
    catch throw:R:ST ->
            io:format("Failed to render: ~tp~n",[R]),
            erlang:raise(throw,R,ST);
          E:R:ST ->
            io:format("Failed to render: ~tp~n",[DocContents]),
            erlang:raise(E,R,ST)
    end.

%% Merge any anchor with its header
%% preprocess_docs([{Hdr,Attr,C},{a,[{id,_}] = Id,[]}|T], D) when ?IS_HDR(Hdr) ->
%%     preprocess_docs([{Hdr,Attr ++ Id, C} | T], D);
%% preprocess_docs([{a,[{id,_}] = Id,[]},{Hdr,Attr,C}|T], D) when ?IS_HDR(Hdr) ->
%%     preprocess_docs([{Hdr,Attr ++ Id, C} | T], D);
preprocess_docs([{a,[{id,_Id}|_] = AAttr,[]},{Tag,PAttr,C}|T], D)
  when Tag =:= pre; Tag =:= em; Tag =:= table; Tag =:= code; Tag =:= img ->
    preprocess_docs([{Tag, AAttr ++ PAttr, C}|T], D);
preprocess_docs([{Tag,_,_} = H,{a,[{id,_Id}|_],[]} = A|T],D) when ?IS_HDR(Tag) ->
    preprocess_docs([A, H | T], D);
preprocess_docs([{a,[{id,Id}|_],[]} = A,{Tag,_,Name} = H|T],D) when ?IS_HDR(Tag) ->
    case string:equal(render_elink_anchor(Id), render_elink_anchor(Name)) of
        true ->
            preprocess_docs([H|T], D);
        false ->
            [A | preprocess_docs([H|T], D)]
    end;
preprocess_docs([{a,[{id,Id}] = Attr,[]}| T],
                #config{ current = {{function,Function,Arity},_,_,_,_} } = D) ->
    maybe
        %% Remove any anchor that is just function-arity
        [FunctionString, ArityString] ?= string:split(Id,"-",all),
        Arity ?= catch binary_to_integer(ArityString),
        true ?= is_integer(Arity),
        Function ?= binary_to_atom(FunctionString),
        preprocess_docs(T, D)
    else
        _ ->
            [{a, Attr, []} | preprocess_docs(T, D)]
    end;
preprocess_docs([{Tag,Attr,C}|T], D) ->
    [{Tag,proplists:delete(ghlink,Attr),preprocess_docs(C, D)}|preprocess_docs(T, D)];
preprocess_docs([Text|T], D) when is_binary(Text) ->
    [Text | preprocess_docs(T,D)];
preprocess_docs([], _) ->
    [].

-spec init_config(#docs_v1{} | undefined, _) -> #config{}.
init_config(D, Config) ->
    #config{docs = D, current = maps:get(current, Config, undefined) }.

render_docs(Elems, State, Pos, Ind, D) when is_list(Elems) ->
    lists:mapfoldl(
        fun(Elem, P) ->
            render_docs(Elem, State, P, Ind, D)
        end,
        Pos,
        Elems
    );
render_docs(Elem, State, Pos, Ind, D) ->
    %    io:format("Elem: ~p (~p) (~p,~p)~n",[Elem,State,Pos,Ind]),
    render_element(Elem, State, Pos, Ind, D).

%%% The function is the main element rendering function
%%%
%%% Elem: The current element to process
%%% Stack: A stack of element names to see where we are in the dom
%%% Pos: The current print position on the current line
%%% Ind: How much the text should be indented after a newline
%%% Config: The renderer's configuration
%%%
%%% Each element is responsible for putting new lines AFTER itself
%%% The indents are done either by render_words when a newline happens
%%% or when a new element is to be rendered and Pos < Ind.
%%%
%%% Any block elements (i.e. p, ul, li etc) are responsible for trimming
%%% extra new lines. eg. <ul><li><p>content</p></li></ul> should only
%%% have two newlines at the end.

%% render_element({IgnoreMe,_,Content}, State, Pos, Ind,D)
%%   when IgnoreMe =:= a ->
%%     render_docs(Content, State, Pos, Ind,D);

%% Catch h* before the padding is done as they reset padding
render_element({Tag = h1, Attr, Content}, State, 0 = Pos, _Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["# ", Docs, ial(Attr)], NewPos});
render_element({Tag = h2, Attr, Content}, State, 0 = Pos, _Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["## ", Docs, ial(Attr)], NewPos});
render_element({Tag = h3, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["### ", Docs, ial(Attr)], NewPos});
render_element({Tag = h4, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["#### ", Docs, ial(Attr)], NewPos});
render_element({Tag = h5, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["##### ", Docs, ial(Attr)], NewPos});
render_element({Tag = h6, Attr, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["###### ", Docs, ial(Attr)], NewPos});
render_element({pre, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    %% We pad `pre` with two newlines if the previous section did not indent the region.
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n\n", Docs], NewPos};
render_element({br, _Attr, _Content}, [td|_State], Pos, _Ind, _D)  ->
    {" ", Pos + 1};
render_element({br, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["  \n", Docs], NewPos};
render_element({p, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n\n", Docs], NewPos};
render_element({Elem, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind, ?IS_BLOCK(Elem) ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n", Docs], NewPos};
render_element({'div', [{class, What}], Content}, State, Pos, Ind, D) ->
    Type = case What of
               <<"warning">> -> What;
               <<"error">> -> What;
               <<"note">> -> <<"info">>;
               <<"change">> -> <<"info">>;
               <<"do">> -> <<"tip">>;
               <<"dont">> -> <<"error">>
           end,
    Title = unicode:characters_to_binary([string:titlecase(What), " ",ial([{class,Type}])]),
    {Header, 0} = render_element({h4, [], [Title]}, State, Pos, Ind, D),
    {Docs, 0} = render_element({'div', [], Content}, ['div' | State], 0, 0, D),
    trimnlnl([pad(Ind - Pos), "> ", string:trim(Header), "\n",
              [[pad(Ind), string:trim(["> ",Line]),"\n"] || Line <- string:split([trim(Docs)],"\n",all)]]);
render_element({Tag, _Attr, Content}, State, Pos, Ind, D) when Tag =:= p; Tag =:= 'div' ->
    trimnlnl(render_docs(Content, [Tag | State], Pos, Ind, D));
%% render_element({a, [{id,_Id}|_], []} = A, State, Pos, Ind, D) when Pos > 0 ->
%%     {Docs, NewPos} = render_element(A, State, 0, Ind, D),
%%     {["\n",Docs], NewPos};
render_element(Elem, State, Pos, Ind, D) when Pos < Ind ->
    {Docs, NewPos} = render_element(Elem, State, Ind, Ind, D),
    {[pad(Ind - Pos), Docs], NewPos};
render_element({a, Ids, []}, _State, Pos, _Ind, _D) ->
    trimnl({[["[]()",ial([{id,render_elink_anchor(Id)}])] || {id,Id} <- Ids], Pos});
render_element({a, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [a|State], Pos, Ind, D),
    Id =
        case proplists:get_all_values(id, Attr) of
            [] -> "";
            [IdStr] -> ial([{id,IdStr}])
        end,
    {[render_link(Attr, Docs),Id],NewPos};
render_element({code, _, Content}, [pre | _] = State, Pos, Ind, D) ->
    %% When code is within a pre we don't emit any underline
    render_docs(Content, [code | State], Pos, Ind, D);
%% Faulty {code,..} generated by diameter containing links.
%% we split them into multiple code segments.
render_element({code,CodeAttr,[Content,{a,AAttr,AContent}|H]}, State, Pos, Ind, D) ->
    AttrWithoutId = proplists:delete(id,CodeAttr),
    render_docs([{code,AttrWithoutId,[Content]},{a,AAttr,[{code,AttrWithoutId,AContent}]},
                 {code,CodeAttr,H}], State, Pos, Ind, D);
render_element({code,_CodeAttr,[]}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({code, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [code | State], Pos, Ind, D),

    IsDocumented = fun(What, #docs_v1{ docs = V1Docs }) ->
                           case lists:keyfind(What, 1, V1Docs) of
                               {What, _, _, #{}, _} ->
                                   true;
                               _ ->
                                   false
                           end
                   end,

    %% Try to convert code segments that refer to types but don't have a link
    %% to have the correct prefix. i.e. <c>byte()</c> should be `t:byte()`.
    TypedDocs =
        maybe
            %% We do not do any transform if we are in an `a` already
            true ?= State =:= [] orelse a =/= hd(State),
            {ok, T, _} ?= erl_scan:string(unicode:characters_to_list([Docs,"."]), {1, 1}),
            {ok, [{call,_,{atom,_,Name},Args}]} ?=
                case erl_parse:parse_exprs(T) of
                    {ok, [{op,A,'/',F,{integer,_,NumArgs}}]} ->
                        %% Translate any byte/0 to byte()
                        {ok,[{call,A,F,lists:duplicate(NumArgs,a)}]};
                    Else ->
                        Else
                end,
            case IsDocumented({function, Name, length(Args)}, D#config.docs) orelse
                erl_internal:bif(Name, length(Args))
            of
                true when length(Args) =:= 0 ->
                    lists:concat([io_lib:write_atom(Name),"/",length(Args)]);
                true ->
                    %% This is a function, so return code as is
                    {lists:concat(["[`",Docs,"`](`",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos};
                false when length(Args) =:= 1,
                           element(1, hd(Args)) =:= integer,
                           element(3, hd(Args)) =:= 3 ->
                    %% Is a foo(3) link
                    try
                        Name:module_info(), %% Check if module exists
                        NameStr = io_lib:write_atom(Name),
                        {["`m:",NameStr,"`"], Pos + string:length(NameStr)}
                    catch error:undef ->
                            Docs
                    end;
                false when length(Args) =:= 1,
                           element(1, hd(Args)) =:= integer,
                           element(3, hd(Args)) =:= 1 ->
                    %% Is a foo(1) link, i.e. a seecom
                    NameStr = io_lib:write_atom(Name),
                    {["[",NameStr,"](",NameStr,"_cmd.md)"], Pos + string:length(NameStr)};
                false ->
                    try
                        %% This is an op type (such as <c>=:=/2</c>)
                        erl_internal:op_type(Name, length(Args)),
                        {lists:concat(["[`",Docs,"`](`erlang:",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos}
                    catch error:function_clause ->
                            case IsDocumented({type,Name,length(Args)}, D#config.docs) orelse
                                erl_internal:is_type(Name,length(Args)) of
                                true when length(Args) =:= 0 ->
                                    lists:concat(["t:",io_lib:write_atom(Name),"/",length(Args)]);
                                true ->
                                    %% This is a type, add type prefix
                                    {lists:concat(["[`",Docs,"`](`t:",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos};
                                false ->
                                    case IsDocumented({callback,Name,length(Args)}, D#config.docs) of
                                        true ->
                                            %% This is a callback
                                            {lists:concat(["[`",Docs,"`](`c:",io_lib:write_atom(Name),"/",length(Args),"`)"]), NewPos};
                                        false ->
                                            %% This is not a type, nor a function, nor a callback
                                            Docs
                                    end
                            end
                    end
            end
        else
            %% Could be a remote type erlang:message_queue_data()
            {ok, [{call,_,{remote,_,{atom,_,RM},{atom,_,RF}},RArgs}]} ->
                case code:get_doc(RM) of
                    {ok, RemoteDocs} ->
                        case IsDocumented({function,RF,length(RArgs)}, RemoteDocs) of
                            true ->
                                %% This is a remote function
                                Docs;
                            false ->
                                case IsDocumented({type,RF,length(RArgs)}, RemoteDocs)  of
                                    true ->
                                        %% This is a valid remote type
                                        {lists:concat(
                                           ["[`",Docs,"`](`t:",io_lib:write_atom(RM),":",
                                            io_lib:write_atom(RF),"/",length(RArgs),"`)"]),
                                         NewPos};
                                    false ->
                                        Docs
                                end
                        end;
                    _ ->
                        %% Could not fetch docs
                        Docs
                end;
            %% Could be a callback Module:init()
            {ok, [{call,_,{remote,_,{var,_,_RM},{atom,_,RF}},RArgs}]} ->
                case IsDocumented({callback,RF,length(RArgs)}, D#config.docs) of
                    true ->
                        %% This is a callback
                        {lists:concat(["[`",Docs,"`](`c:",io_lib:write_atom(RF),"/",length(RArgs),"`)"]), NewPos};
                    false ->
                        Docs
                end;
            false ->
                %% We are in a link already, maybe strip trailing (1/3)
                case re:run(Docs, "^([a-z_]+)\\([13]\\)$",[{capture,all_but_first,list}, unicode]) of
                    {match,[MaybeMod]} ->
                        case code:which(list_to_atom(MaybeMod)) of
                            non_existing ->
                                Docs;
                            _ ->
                                MaybeMod
                        end;
                    _ ->
                        Docs
                end;
            _ ->
                %% Could not parse
                Docs
        end,
    if is_tuple(TypedDocs) ->
            TypedDocs;
       true ->
            case re:run(TypedDocs, "`+", [global,unicode]) of
                nomatch ->
                    {["`", TypedDocs, "`", ial(Attr)], NewPos};
                {match,Matches} ->
                    LargestMatch = lists:max([Size || [{_, Size}] <- Matches]),
                    Ticks = lists:duplicate(LargestMatch+1,$`),
                    {[Ticks," ", TypedDocs, " ",Ticks,ial(Attr)], NewPos}
            end
    end;
render_element({em, Attr, Content}, State, Pos, Ind, D) ->
    render_element({i, Attr, Content}, State, Pos, Ind, D);
render_element({i, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [i | State], Pos, Ind, D),
    case lists:member(pre, State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["*", Docs, "*",ial(Attr)], NewPos}
    end;
render_element({hr, [], []}, _State, Pos, _Ind, _D) ->
    {"---\n", Pos};
render_element({br, [], []}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({strong, Attr, Content}, State, Pos, Ind, D) ->
    render_element({b, Attr, Content}, State, Pos, Ind, D);
render_element({b, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind, D),
    case lists:member(pre, State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["__", Docs, "__",ial(Attr)], NewPos}
    end;
render_element({pre, [], [{code,Attr,Content}]}, State, Pos, Ind, D) ->
    render_element({pre, Attr, Content}, State, Pos, Ind, D);
render_element({pre, Attr, Content}, State, Pos, Ind, D) ->
    %% This is a pre without any links or emphasis, so we use markdown

    %% For pre we make sure to respect the newlines in pre
    {Docs, _} = trimnl(render_docs(strip_tags(Content), [pre | State], Pos, Ind, D)),
    Type =
        case unicode:characters_to_binary(proplists:get_value(type, Attr, "text")) of
            <<"none">> -> "text";
            <<"text">> -> "text";
            <<"erlang">> -> "erlang";
            <<"erl">> -> "erlang";
            <<"erl-repl">> -> "erlang";
            <<"c">> -> "c"
        end,
    IdAttr = proplists:delete(type, Attr),
    trimnlnl(["```",Type,"\n", pad(Ind), Docs, pad(Ind), "```",
              [["\n",pad(Ind),ial(IdAttr)] || IdAttr =/= []]]);
render_element({ul, [{class, <<"types">>}], _Content}, _State, Pos, _Ind, _D) ->
    case _D#config.current of
        {_, _, _, _, #{ specs := _}} ->
            {"", Pos};
        _ ->
            {Docs, _} = render_docs(_Content, [types | _State], 0, _Ind, _D),
            trimnlnl(Docs)
    end;
render_element({li, Attr, Content}, [types | _] = State, Pos, Ind, C) ->
    Doc =
        case {proplists:get_value(name, Attr), proplists:get_value(class, Attr)} of
            {undefined, Class} when Class =:= undefined; Class =:= <<"type">> ->
                %% Inline html for types
                render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind, C);
            {_, <<"description">>} ->
                %% Inline html for type descriptions
                render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind + 2, C);
            {Name, _} ->
                %% Try to render from type metadata
                case render_type_signature(binary_to_atom(Name), C) of
                    undefined when Content =:= [] ->
                        %% Failed and no content, emit place-holder
                        {["```erlang\n-type ", Name, "() :: term().```"], 0};
                    undefined ->
                        %% Failed with metadata, render the content
                        render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind, C);
                    Type ->
                        %% Emit the erl_pp typespec
                        {["```erlang\n", Type, "```"], 0}
                end
        end,
    trimnl(Doc);
render_element({ul, [], Content}, State, Pos, Ind, D) ->
    trimnlnl(render_docs(Content, [ul | State], Pos, Ind, D));
render_element({ol, [], Content}, State, Pos, Ind, D) ->
    trimnlnl(render_docs(Content, [ol | State], Pos, Ind, D));
render_element({li, [], Content}, [ul | _] = State, Pos, Ind, D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2, Ind + 2, D),
    trimnl(["* ", Docs]);
render_element({li, [], Content}, [ol | _] = State, Pos, Ind, D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2, Ind + 2, D),
    trimnl(["1. ", Docs]);
render_element({dl, [], [{dt,DTAttr,DTContent}, {dd,_,_} = DD1, {dd, _, _} = DD2 | Content]}, State, Pos, Ind, D) ->
    {DD, T} = lists:splitwith(fun(E) -> element(1,E) =:= dd end, Content),
    DDs = [{p, [], C} || {_, _, C} <- [DD1, DD2 | DD]],
    render_element({dl, [], [{dt,DTAttr,DTContent}, {dd,[],DDs} | T]}, State, Pos, Ind, D);
render_element({dl, [], [{dt,DTAttr,DTContent}, {dd,[],DDContent} | Content]}, State, Pos, Ind, D) ->
    Since = proplists:get_value(since, DTAttr),
    {DTDocs, _DTNewPos} =
        render_docs(
          [{b, [], DTContent}],
          [li, dl | State],
          Pos + 2,
          Ind + 2,
          D),
    Ids = [{id,Id} || {id,Id} <- DTAttr],
    DTDocsWAnchors = case Ids of
                         [] -> trim(DTDocs);
                         Ids -> [trim(DTDocs),ial(Ids)]
                     end,
    {DDDocs, DDNewPos} = render_docs(DDContent, [li, dd | State], 0, Ind + 2, D),
    {Docs, NewPos} =
        case string:find(DTDocs, "\n") of
            nomatch when Since =:= undefined, is_binary(hd(DDContent)) orelse element(1,hd(DDContent)) =/= pre ->
                trimnlnl({["* ", trim(DTDocsWAnchors), " - ", string:trim(string:trim(DDDocs, both, "\n"), leading, " ")], DDNewPos});
            _ ->
                trimnlnl({["* ", trim(DTDocsWAnchors), [["(Since ",Since,")"] || Since =/= undefined],"  \n",
                           DDDocs], DDNewPos})
        end,
    {DLDocs, DLPos} = render_element({dl, [], Content}, State, NewPos, Ind, D),
    {[Docs,DLDocs], DLPos};
render_element({dl, [], []}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({table, Attr, Rows}, State, Pos, Ind, D) ->
    [{tr,_,Head} | RowsNoCaption] = [Row || {tr,_,_} = Row <- Rows],
    {TableDocs, TablePos} =
        trimnl(render_docs([{th, [], Head} | RowsNoCaption], [table|State], Pos, Ind, D)),
    {CaptionDocs, CaptionPos} =
        render_docs([{em, [], [<<"Table: ">>, C]} || {caption,_,C} <- Rows, not string:equal(C,"")],
                    [table|State], TablePos, Ind, D),
    trimnlnl({[TableDocs, [[pad(Ind),ial(proplists:delete(align,Attr)),"\n\n"] || Attr =/= []], CaptionDocs], CaptionPos});
render_element({th, [], Head}, State, _Pos, _Ind, D) ->
    Header =
        [begin {Docs, _} = render_docs(Td, [th|State], 0, 0, D),
               {["| ", Docs, " "], ["|-", lists:duplicate(string:length(Docs), $-), "-"]}
         end || Td <- Head],
    trimnl({[[ Docs || {Docs,_} <- Header ], "|\n",
             [ Lines || {_, Lines} <- Header ], "|\n"], 0});
render_element({tr, [], Row}, State, _Pos, _Ind, D) ->
    Rows =
        [begin {Docs, _} = render_docs(Td, [tr|State], 0, 0, D),
               ["| ", Docs, " "]
         end || Td <- Row],
    trimnl({[ Rows, "|"], 0});
render_element({td, _, TDContent}, State, Pos, Ind, D) ->
    render_docs(TDContent, [td|State], Pos, Ind, D);
render_element({img,Attr,Content}, _State, Pos, _Ind, _D) ->
    Caption = case lists:keyfind(caption, 1, Content) of
                  false -> "";
                  {caption, _, C} ->
                      C
              end,
    trimnlnl({["![",Caption,"](",filename:join("assets",filename:basename(proplists:get_value(file,Attr))),
               " \"",Caption,"\")",ial(proplists:delete(file, Attr)),"\n"], Pos});
render_element({quote, [], Content}, State, _Pos, Ind, D) ->
    {Docs, 0} = render_element({'div', [], Content}, ['div' | State], 0, 0, D),
    trimnlnl([[pad(Ind), "> ",Line,"\n"] || Line <- string:split(trim(Docs),"\n",all)]);
render_element(B, State, Pos, Ind, _D) when is_binary(B) ->
    %% Indent the string correctly
    Pre = re:replace(B,"\n",nlpad(Ind),[global,unicode]),

    Str =
        case State of
            [pre | _] ->
                Pre;
            [code | _] ->
                Pre;
            [h4 | _] ->
                Pre;
            _ ->
                EscapeChars = [
                               "\\",
                               "`",
                               "{",
                               "}",
                               "!"|
                               [["|"] || lists:member(table,State)]
                              ],
                lists:foldl(
                  fun({Pat, Subst}, S) -> re:replace(S, Pat, Subst, [global,unicode]) end,
                  B,
                  [{["(", lists:join($|, [["\\", C] || C <- EscapeChars]), ")"], "\\\\\\1"},
                   %% [^S\r\n] == All whitespace except \r\n
                   {"(\n\\s*[0-9]+)\\.([^S\r\n])", "\\1\\\\.\\2"},  %% \n1. -> 1\.
                   {"^(\\s*[0-9]+)\\.([^S\r\n])",  "\\1\\\\.\\2"},  %% ^1. -> 1\.
                   {"(\n\\s*)\\*([^S\r\n])",       "\\1\\\\*\\2"},  %% \n* -> \*
                   {"^(\\s*)\\*([^S\r\n])",        "\\1\\\\*\\2"},  %% ^* -> \*
                   {"(\n\\s*)\\-([^S\r\n])",       "\\1\\\\-\\2"},  %% \n- -> \-
                   {"^(\\s*)\\-([^S\r\n])",        "\\1\\\\-\\2"},  %% ^- -> \-
                   {"(\n\\s*)\\+([^S\r\n])",       "\\1\\\\+\\2"},  %% \n+ -> \+
                   {"^(\\s*)\\+([^S\r\n])",        "\\1\\\\+\\2"},  %% ^+ -> \+
                   {"(\n\\s*)\\#([^S\r\n])",       "\\1\\\\#\\2"},  %% \n# -> \#
                   {"^(\\s*)\\#([^S\r\n])",        "\\1\\\\#\\2"},  %% ^# -> \#
                   {"\\[([^]]+\\])",               "\\\\[\\1"},     %% [..] -> \[..]
                   {"<(http[^>]+>)",               "\\\\<\\1"},     %% <..> -> \<..>
                   {"(\s)_([^_]+_\s)",             "\\1\\\\_\\2"}]  %% _.._ -> \_.._
                 )
        end,
    {Str, Pos + lastline(Str)};
render_element({Tag, Attr, Content}, State, Pos, Ind, D) ->
    case lists:member(Tag, ?ALL_ELEMENTS) of
        true ->
            throw({unhandled_element, Tag, Attr, Content});
        false ->
            throw({unknown_element, Tag, Attr, Content}),
            ok
    end,
    render_docs(Content, State, Pos, Ind, D).


render_link(Attr, Docs) ->
    render_link(Docs, proplists:get_value(rel, Attr), proplists:get_value(href, Attr)).

render_link(Docs, undefined, Href) when Href =/= undefined ->
    %% This types of links are usually from edoc, but could also be
    %% <url> from erl_docgen
    case Href of
        <<"overview-summary.html",Rest/binary>> ->
            %% This is an edoc overview anchor
            Anchor = lists:last(string:split(Rest,"#")),
            ["[", Docs, "](chapter.md#", render_link_anchor(Anchor), ")"];
        Href ->
            ["[", Docs, "](", Href, ")"]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seemfa">>, Href) ->
    MFA = case string:split(Href, ":") of
              [_App, HrefMFA] -> HrefMFA;
              [Href] -> Href
          end,
    [Mod, FA] = case string:split(MFA, "#") of
                    [<<>>, MFANoAnchor] -> ["", MFANoAnchor];
                    [Module, FunArgs] ->
                        case string:equal(atom_to_list(get(module)), Module) of
                            true ->
                                ["",FunArgs];
                            false ->
                                [[Module,":"],FunArgs]
                        end
                end,
    {Prefix, Func, Arity} =
        case string:split(FA, "/") of
            [<<"Module:", F/binary>>, A] ->
                {"c:",F, A};
            [<<"Mod:", F/binary>>, A] ->
                {"c:",F, A};
            [F, A] ->
                {"", F, A}
        end,
    Link = [Mod,Func,"/",Arity],
    case string:equal(Docs, Link) orelse string:equal(Docs, ["`",Link,"`"]) of
        true ->
            ["`",Prefix,Link,"`"];
        false ->
             [
              "[", Docs, "](`",Prefix,Link,"`)"
             ]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seetype">>, Href) ->
    MFA = case string:split(Href, ":") of
              [_App, HrefMFA] -> HrefMFA;
              [Href] -> Href
          end,
    [ModDocs, Mod, FA] =
        case string:split(MFA, "#") of
            [<<>>, MFANoAnchor] -> [get(module), "", MFANoAnchor];
            [Module, FunArgs] ->
                case string:equal(atom_to_list(get(module)), Module) of
                    true ->
                        [get(module), "",FunArgs];
                    false ->
                        [binary_to_atom(Module), [Module,":"],FunArgs]
                end
        end,
    {Func, Arity} =
        case string:split(FA, "/") of
            [FA] ->
                {ok, #docs_v1{ docs = Ds}} = code:get_doc(ModDocs),
                App = get(application),
                case lists:search(
                       fun(E) ->
                               case element(1, E) of
                                   {type, Type, _} ->
                                       string:equal(atom_to_list(Type), FA);
                                   _ ->
                                       false
                               end
                       end, lists:sort(Ds)) of
                    {value, {{type,_,TypeArity},_,_,_,_}} ->
                        {FA, integer_to_list(TypeArity)};
                    _Else when App =/= "wx" ->
                        io:format("Could not find find type: ~p~n",
                                  [[Mod, FA]]),
                        exit({Mod, FA});
                    _Else ->
                        {FA,"0"}
                end;
            [F, A] ->
                {F, A}
        end,
    Link = [Mod,Func,"/",Arity],
    ZeroLink = [Mod,Func,"()"],
    case (string:equal(Docs, Link) orelse string:equal(Docs, ["`",Link,"`"])) orelse
        ((string:equal(Docs, ZeroLink) orelse string:equal(Docs, ["`",ZeroLink,"`"])) andalso Arity =:= "0")
    of
        true ->
            ["`t:", Link, "`"];
        false ->
             [
              "[", Docs, "](`t:", Link,"`)"
             ]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>,
            Href = <<"erl_docgen:doc_storage">>) ->
    ["[",Docs,"](`e:",Href,".md`)"];
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>,
            <<"erl_docgen:doc_storage.html">>) ->
    ["[",Docs,"](`e:erl_docgen:doc_storage.md`)"];
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>,
            <<"edoc:edoc_cmd">>) ->
    ["[",Docs,"](edoc_cmd.md)"];
render_link(Docs, <<"https://erlang.org/doc/link/seeerl">>, Href) ->
    ModAnchor =
        case string:split(Href, ":") of
            [MA] ->
                MA;
            [_App, MA] ->
                MA
        end,
    ModFixedAnchor =
        case string:split(ModAnchor, "#") of
            [ModAnchor] ->
                ModAnchor;
            [M, A] ->
                [M, "#", render_elink_anchor(A)]
        end,
    DocsNoMan3 = re:replace(Docs,["(`?",ModFixedAnchor,")\\(3\\)(`?)"],"\\1\\2"),
    case string:equal(DocsNoMan3, ModFixedAnchor) orelse
        string:equal(DocsNoMan3, ["`",ModFixedAnchor,"`"]) of
        true ->
            ["`m:", ModFixedAnchor, "`"];
        false ->
            ["[", Docs, "](`m:", ModFixedAnchor, "`)"]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seeguide">>, Href) ->
    CurrentApplication = unicode:characters_to_binary(get(application)),
    RemoveSystemApp = fun(<<"system/general_info",_/binary>>) ->
                              <<"general_info">>;
                          (<<"system",_/binary>>) ->
                              <<"system">>;
                         (Else) ->
                              Else
                      end,
    case string:lexemes(Href, ":#") of
        [App, <<"index">>] when App =:= CurrentApplication ->
            ["[", Docs, "](index.html)"];
        [App, <<"index">>] ->
            ["[", Docs, "](`e:",RemoveSystemApp(App),":index.html`)"];
        [App, Guide] when App =:= CurrentApplication ->
            ["[", Docs, "](",string:lowercase(Guide),".md)"];
        [App, Guide, Anchor] when App =:= CurrentApplication ->
            ["[", Docs, "](",string:lowercase(Guide),".md#",
              render_elink_anchor(Anchor),")"];
        [App, Guide] ->
            ["[", Docs, "](`e:",RemoveSystemApp(App),":",string:lowercase(Guide),".md`)"];
        [App, Guide, Anchor] ->
            ["[", Docs, "](`e:",RemoveSystemApp(App),":",string:lowercase(Guide),".md#",
              render_elink_anchor(Anchor),"`)"]
    end;
render_link(Docs, Rel, Href)
  when Rel =:= <<"https://erlang.org/doc/link/seecref">>;
       Rel =:= <<"https://erlang.org/doc/link/seecom">>;
       Rel =:= <<"https://erlang.org/doc/link/seeapp">> ->
    CurrentApplication = unicode:characters_to_binary(get(application)),
    Postfix = case Rel of
                  <<"https://erlang.org/doc/link/seecom">> ->
                      "_cmd";
                  <<"https://erlang.org/doc/link/seeapp">> ->
                      "_app";
                  _ ->
                      ""
              end,
    AddPostfix = fun(Guide) ->
                         string:lowercase(
                           case string:prefix(string:reverse(Guide),
                                              string:reverse(Postfix)) of
                               nomatch ->
                                   [Guide,Postfix];
                               _ ->
                                   Guide
                           end)
                 end,
    case string:lexemes(Href, ":#") of
        [App, <<"index">>] when App =:= CurrentApplication ->
            ["[", Docs, "](index.html)"];
        [App, <<"index">>] ->
            ["[", Docs, "](`e:",App,":index.html`)"];
        [App, Guide] when App =:= CurrentApplication ->
            ["[", Docs, "](",AddPostfix(Guide),".md)"];
        [App, Guide, Anchor] when App =:= CurrentApplication ->
            ["[", Docs, "](",AddPostfix(Guide),".md#",
              render_elink_anchor(Anchor),")"];
        [App, Guide] ->
            ["[", Docs, "](`e:",App,":",AddPostfix(Guide),".md`)"];
        [App, Guide, Anchor] ->
            ["[", Docs, "](`e:",App,":",AddPostfix(Guide),".md#",
              render_elink_anchor(Anchor),"`)"]
    end;
render_link(Docs, <<"https://erlang.org/doc/link/seefile">>, Href) ->
    CurrentApplication = unicode:characters_to_binary(get(application)),
    MaybeAddExtension = fun(G) ->
                                string:lowercase(
                                  case string:equal(filename:extension(G),"") of
                                      true -> [G,".md"];
                                      _ -> G
                                  end)
                        end,
    case string:lexemes(Href, ":#") of
        [App, Guide] when App =:= CurrentApplication, App =:= <<"jinterface">> ->
            ["[", Docs, "](assets/",Guide,".html)"];
        [App, Guide, Anchor] when App =:= CurrentApplication, App =:= <<"jinterface">> ->
            ["[", Docs, "](assets/",Guide,".html#",render_link_anchor(Anchor),")"];
        [App, Guide] when App =:= CurrentApplication ->
            ["[", Docs, "](",MaybeAddExtension(Guide),")"];
        [App, Guide, Anchor] when App =:= CurrentApplication ->
            ["[", Docs, "](",MaybeAddExtension(Guide),"#",render_link_anchor(Anchor),")"];
        [App, Guide] ->
            ["[", Docs, "](`e:",App,":",MaybeAddExtension(Guide),"`)"];
        [App, Guide, Anchor] ->
            ["[", Docs, "](`e:",App,":",MaybeAddExtension(Guide),
             "#",render_link_anchor(Anchor),"`)"]
    end;
render_link(Docs, _Rel, _Href) ->
    Docs.

render_elink_anchor(Anchor) ->
      render_link_anchor(
        lists:foldl(
          fun({Re,Sub}, Str) -> re:replace(Str, Re, Sub, [global, unicode]) end,
          Anchor,
          [{" ","-"},{"(--|\\.)","-"}, {"(^-|-$)",""}])).

render_link_anchor(Anchor) ->
    uri_string:quote(Anchor).

-spec render_type_signature(atom(), #config{}) -> 'undefined' | unicode:chardata().
render_type_signature(Name, #config{docs = #docs_v1{metadata = #{types := AllTypes}}}) ->
    case [Type || Type = {TName, _} <- maps:keys(AllTypes), TName =:= Name] of
        [] ->
            undefined;
        Types ->
            [erl_pp:attribute(maps:get(Type, AllTypes)) || Type <- Types]
    end.

ial([]) ->
    "";
ial(Attrs) ->
    ["{: ", [[ial(Tag, Value), " "] || {Tag,Value} <- Attrs], "}"].

ial(class, Value) ->
    [".", maybe_quote_ial(Value)];
ial(id, Value) ->
    ["#", maybe_quote_ial(Value)];
ial(Tag, Value) ->
    [atom_to_list(Tag), "=", maybe_quote_ial(Value)].

maybe_quote_ial(Str) ->
    case string:find(Str, " ") of
        nomatch ->
            Str;
        _ ->
            [$",Str,$"]
    end.

%% Pad N spaces (and possibly pre-prend newline), disabling any ansi formatting while doing so.
-spec pad(non_neg_integer()) -> unicode:chardata().
pad(N) ->
    pad(N, "").
-spec nlpad(non_neg_integer()) -> unicode:chardata().
nlpad(N) ->
    pad(N, "\n").
-spec pad(non_neg_integer(), unicode:chardata()) -> unicode:chardata().
pad(N, Extra) ->
    Pad = lists:duplicate(N, [$\s]),
    [Extra, Pad].

-spec lastline(unicode:chardata()) -> non_neg_integer().
%% Look for the length of the last line of a string
lastline(Str) ->
    LastStr =
        case string:find(Str, "\n", trailing) of
            nomatch ->
                Str;
            Match ->
                tl(string:next_codepoint(Match))
        end,
    string:length(LastStr).

%% These functions make sure that we trim extra newlines added
%% by the renderer. For example if we do <li><p></p></li>
%% that would add 4 \n at after the last </li>. This is trimmed
%% here to only be 2 \n
-spec trimnlnl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) ->
    {unicode:chardata(), 0}.
trimnlnl({Chars, _Pos}) ->
    nl(nl(string:trim(Chars, trailing, "\n")));
trimnlnl(Chars) ->
    nl(nl(string:trim(Chars, trailing, "\n"))).
-spec trimnl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) ->
    {unicode:chardata(), 0}.
trimnl({Chars, _Pos}) ->
    nl(string:trim(Chars, trailing, "\n"));
trimnl(Chars) ->
    nl(string:trim(Chars, trailing, "\n")).
trim(Chars) ->
    string:trim(Chars, trailing, "\n").
-spec nl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) -> {unicode:chardata(), 0}.
nl({Chars, _Pos}) ->
    nl(Chars);
nl(Chars) ->
    {[Chars, "\n"], 0}.


%%======================================================================
%% Records
%%======================================================================

%%----------------------------------------------------------------------
%% State record for the validator
%%----------------------------------------------------------------------
-record(state, {
	  tags=[],         %% Tag stack
	  dom=[]           %% DOM structure
	 }).

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function: initial_state() -> Result
%% Parameters:
%% Result:
%% Description:
%%----------------------------------------------------------------------
initial_state() ->
    #state{}.

%%----------------------------------------------------------------------
%% Function: get_dom(State) -> Result
%% Parameters:
%% Result:
%% Description:
%%----------------------------------------------------------------------
get_dom(#state{dom=Dom}) ->
    Dom.

%%----------------------------------------------------------------------
%% Function: event(Event, LineNo, State) -> Result
%% Parameters:
%% Result:
%% Description:
%%----------------------------------------------------------------------
event(Event, _LineNo, State) ->
    build_dom(Event, State).


%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function  : build_dom(Event, State) -> Result
%% Parameters: Event = term()
%%             State = #xmerl_sax_simple_dom_state{}
%% Result    : #xmerl_sax_simple_dom_state{} |
%% Description:
%%----------------------------------------------------------------------

%% Document
%%----------------------------------------------------------------------
build_dom(startDocument, State) ->
    State#state{dom=[startDocument]};
build_dom(endDocument,
	  #state{dom=[{Tag, Attributes, Content} |D]} = State) ->
    case D of
	[startDocument] ->
	    State#state{dom=[{Tag, Attributes,
                              lists:reverse(Content)}]};
	[Decl, startDocument] ->
	    State#state{dom=[Decl, {Tag, Attributes,
                                    lists:reverse(Content)}]};
	_ ->
            %% endDocument is also sent by the parser when a fault occur to tell
            %% the event receiver that no more input will be sent
	    State
    end;

%% Element
%%----------------------------------------------------------------------
build_dom({startElement, _Uri, LocalName, _QName, Attributes},
	  #state{tags=T, dom=D} = State) ->

    A = parse_attributes(LocalName, Attributes),
    CName = list_to_atom(LocalName),

    State#state{tags=[CName |T],
                dom=[{CName,
                      lists:reverse(A),
                      []
                     } | D]};
build_dom({endElement, _Uri, LocalName, _QName},
	  #state{tags=[_ |T],
                 dom=[{CName, CAttributes, CContent},
                      {PName, PAttributes, PContent} = _Parent | D]} = State) ->
    case list_to_atom(LocalName) of
	CName ->
            SectionDepth = length([E || E <- T, E =:= section]),
            MappedCName =
                case CName of
                    title ->
                        lists:nth(SectionDepth,[h1,h2,h3,h4,h5,h6]);
                    section when SectionDepth > 0 ->
                        'div';
                    CName -> CName
                end,

            State#state{tags=T,
                        dom=[{PName, PAttributes,
                              [{MappedCName, CAttributes,
                                lists:reverse(CContent)}
                               |PContent]
                             } | D]};
        _ ->
            throw({dom_error, "Got end of element: " ++ LocalName ++ " but expected: " ++
                       CName})
    end;

%% Text
%%----------------------------------------------------------------------
build_dom({characters, String},
	  #state{dom=[{Name, Attributes, Content}| D]} = State) ->
    State#state{dom=[{Name, Attributes, [unicode:characters_to_binary(String,utf8)| Content]} | D]};
build_dom(startCDATA, State) ->
    State#state{ tags = [startCDATA | State#state.tags ] };
build_dom(endCDATA, #state{ tags = [ CData | T ] } = State) ->
    CData = startCDATA,
    State#state{ tags = T };

build_dom({ignorableWhitespace, String},
          #state{dom=[{Name,_,_} = _E|_]} = State) ->
    case lists:member(Name,
                      [p,pre,input,code,quote,warning,
                       note,change,dont,do,c,b,i,em,strong,
                       seemfa,seeerl,seetype,seeapp,
                       seecom,seecref,seefile,seeguide,
                       tag,item]) of
        true ->
%            io:format("Keep ign white: ~p ~p~n",[String, _E]),
            build_dom({characters, String}, State);
        false ->
            State
    end;

build_dom({startEntity, _SysId}, State) ->
    %% io:format("startEntity:~p~n",[_SysId]),
    State;

%% Default
%%----------------------------------------------------------------------
build_dom(_E, State) ->
    %% io:format("IgnoredEvent: ~p~n",[_E]),
    State.

%%----------------------------------------------------------------------
%% Function  : parse_attributes(ElName, Attributes) -> Result
%% Parameters:
%% Result    :
%% Description:
%%----------------------------------------------------------------------
parse_attributes(ElName, Attributes) ->
    parse_attributes(ElName, Attributes, 1, []).

parse_attributes(_, [], _, Acc) ->
    Acc;
parse_attributes(ElName, [{_Uri, _Prefix, LocalName, AttrValue} |As], N, Acc) ->
    parse_attributes(ElName, As, N+1, [{list_to_atom(LocalName), AttrValue} |Acc]).

transform([{section,_,Content}|T],Acc) ->
    transform(T,[transform(Content,[])|Acc]);

%% transform <list><item> to <ul><li> or <ol><li> depending on type attribute
transform([{list,Attr,Content}|T],Acc) ->
    transform([transform_list(Attr,Content)|T],Acc);

%% transform <taglist>(tag,item+)+ to <dl>(dt,item+)+
transform([{taglist,Attr,Content}|T],Acc) ->
    transform([transform_taglist(Attr,Content)|T],Acc);

%% remove <anno> as it is only used to validate specs vs xml src
transform([{anno,[],Content}|T],Acc) ->
    transform([Content|T],Acc);

%% transform <c> to <code>
transform([{c,[],Content}|T],Acc) ->
    transform(T, [{code,[],transform(Content,[])}|Acc]);

%% transform <code> to <pre><code>
transform([{code,Attr,Content}|T],Acc) ->
    transform(T, [{pre,[],[{code,a2b(Attr),transform(Content,[])}]}|Acc]);
%% transform <pre> to <pre><code>
transform([{pre,Attr,Content}|T],Acc) ->
    transform(T, [{pre,[],[{code,Attr,transform(Content,[])}]}|Acc]);

%% transform <marker id="name"/>  to <a id="name"/>....
transform([{marker,Attrs,Content}|T],Acc) ->
    transform(T,[{a,a2b(Attrs),transform(Content,[])}|Acc]);
%% transform <url href="external URL"> Content</url> to <a href....
transform([{url,Attrs,Content}|T],Acc) ->
    transform(T,[{a,a2b(Attrs),transform(Content,[])}|Acc]);
%% transform note/change/warning/do/don't to <p class="thing">
transform([{What,[],Content}|T],Acc)
  when What =:= note; What =:= change; What =:= warning; What =:= do; What =:= dont ->
    WhatP = {'div',[{class,atom_to_binary(What)}], transform(Content,[])},
    transform(T,[WhatP|Acc]);

transform([Elem = {See,_Attr,_Content}|T],Acc)
  when See =:= seemfa; See =:= seeerl; See =:= seetype; See =:= seeapp;
       See =:= seecom; See =:= seecref; See =:= seefile; See =:= seeguide ->
    transform([transform_see(Elem)|T],Acc);

transform([{term,Attr,[]}|T],Acc) ->
    transform([list_to_binary(proplists:get_value(id,Attr))|T],Acc);

transform([{p,Attr,Content}|T],Acc) ->
    transform(T,[{p,a2b(Attr),transform(Content,[])}|Acc]);
transform([{'div',Attr,Content}|T],Acc) ->
    transform(T,[{'div',a2b(Attr),transform(Content,[])}|Acc]);

%% Tag and Attr is used as is but Content is transformed
transform([{Tag,Attr,Content}|T],Acc) ->
    transform(T,[{Tag,Attr,transform(Content,[])}|Acc]);
transform([Binary|T],Acc) ->
    transform(T,[Binary|Acc]);
transform([],Acc) ->
    lists:flatten(lists:reverse(Acc)).

transform_list([{type,"ordered"}],Content) ->
    {ol,[],[{li,A2,C2}||{item,A2,C2}<-Content]};
transform_list(_,Content) ->
    {ul,[],[{li,A2,C2}||{item,A2,C2}<-Content]}.

transform_taglist(Attr,Content) ->
    Items =
        lists:map(fun({tag,_A,_C}=Tag) ->
                          transform_tag(Tag);
                     ({item,A,C}) ->
                          {dd,A,C}
                  end, Content),
    %% io:format("Items: ~p~n",[Items]),
    {dl,Attr,Items}.

transform_tag({tag, Attr0, C}) ->
    Attr1 = lists:map(fun({since,Vsn}) ->
                              {since,
                               unicode:characters_to_binary(Vsn)};
                         (A) ->
                              A
                      end,
                      Attr0),
    {dt,Attr1,C}.

strip_tags([{_Tag,_Attr,Content}|T]) ->
    [Content | strip_tags(T)];
strip_tags([H|T]) when not is_tuple(H) ->
    [H | strip_tags(T)];
strip_tags([]) ->
    [].

transform_see({See,[{marker,Marker}],Content}) ->
    AbsMarker =
        case string:split(Marker, "#") of
            [AppFile] -> marker_defaults(AppFile);
            [AppFile, Anchor] -> [marker_defaults(AppFile), "#", Anchor]
        end,

    {a, [{href,iolist_to_binary(AbsMarker)},
         {rel,<<"https://erlang.org/doc/link/",(atom_to_binary(See))/binary>>}], Content}.

marker_defaults("") ->
    [get(application), ":", filename:rootname(unicode:characters_to_list(get(module)))];
marker_defaults(AppFile) ->
    case string:split(AppFile, ":") of
        [File] -> [get(application), ":", File];
        [App, File] -> [App, ":", File]
    end.

a2b(Attrs) ->
    [{Tag,unicode:characters_to_binary(Value)} || {Tag,Value} <- Attrs].
