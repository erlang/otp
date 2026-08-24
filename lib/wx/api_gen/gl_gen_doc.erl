%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2008-2026. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : gl_gen_doc.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Generate inline -doc attributes for gl/glu modules
%%%
%%% Created : 18 Apr 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(gl_gen_doc).

-include("gl_gen.hrl").

-export([gen/2, module_doc/1, func_doc/2]).

-import(lists, [foldl/3,reverse/1,filter/2]).
-import(gen_util, [w/2]).

-define(HTTP_TOP, "https://www.khronos.org/registry/OpenGL-Refpages/").

%% gen/2 scans the Khronos refpages and stores docs in the process dictionary.
%% Called before gl_gen_erl generates code.
gen(GLDefs, GLUDefs) ->
    case os:getenv("GL_MAN_SRC_DIR") of
        false ->
            io:format(" Skipping gl docs: 'GL_MAN_SRC_DIR' not set\n"),
            ok;
        Dir ->
            case filelib:is_dir(Dir) of
                true ->
                    put(gl_src_dir, Dir),
                    scan_docs("gl", GLDefs),
                    Tess = fake_tesselate(),
                    scan_docs("glu", lists:sort([Tess|GLUDefs]));
                false ->
                    io:format(" Skipping gl docs: '~s' not readable\n", [Dir])
            end
    end.

%% Emit -moduledoc for gl or glu
module_doc(_Name) ->
    w("-moduledoc \"\"\"\n", []),
    w("Erlang wrapper functions for OpenGL\n\n", []),
    w("Standard OpenGL API\n\n", []),
    w("This documents the functions as a brief version of the complete\n", []),
    w("[OpenGL reference pages.](~s)\n", [?HTTP_TOP]),
    w("\"\"\".\n", []).

%% Emit -doc attribute for a function.
%% FuncName is the canonical doc name, Equiv is the equiv target (or undefined).
func_doc(_FuncName, Equiv) when Equiv =/= undefined ->
    w("-doc(#{equiv => ~s}).\n", [Equiv]);
func_doc(FuncName, undefined) ->
    case get({gl_doc, FuncName}) of
        undefined ->
            case get({gl_doc_raw, FuncName}) of
                undefined ->
                    ok;
                {Found, Name, Doc} ->
                    Text = format_raw_doc(Found, Name, Doc),
                    emit_doc(Text)
            end;
        Text ->
            emit_doc(Text)
    end.

format_raw_doc(Found, Name, Doc) ->
    {Dir, Ext} = case Found of
                     "gl4" -> {"gl4/html", "xhtml"};
                     "gl2.1" -> {"gl2.1/xhtml", "xml"}
                 end,
    Ref = io_lib:format("~s~s/~s.~s", [?HTTP_TOP, Dir, Name, Ext]),
    DocParts = format_doc(Doc, []),
    ExtLink = io_lib:format("[External documentation.](~s)", [Ref]),
    Text0 = lists:flatten(format_parts(DocParts)),
    Text1 = trim_trailing_newlines(Text0),
    Text2 = fix_tex_quotes(Text1),
    line_wrap(Text2, 80) ++ "\n\n" ++ ExtLink.

trim_trailing_newlines(Str) ->
    lists:reverse(lists:dropwhile(fun(C) -> C =:= $\n orelse C =:= $\s end,
                                  lists:reverse(Str))).

%% Replace TeX-style ``quotes'' with "quotes"
fix_tex_quotes(Str) ->
    fix_tex_quotes(Str, []).

fix_tex_quotes([$`, $`|Rest], Acc) ->
    fix_tex_quotes(Rest, [$"|Acc]);
fix_tex_quotes([$', $'|Rest], Acc) ->
    fix_tex_quotes(Rest, [$"|Acc]);
fix_tex_quotes([C|Rest], Acc) ->
    fix_tex_quotes(Rest, [C|Acc]);
fix_tex_quotes([], Acc) ->
    lists:reverse(Acc).

emit_doc(Doc) ->
    case has_quotes(Doc) orelse not is_single_line(Doc) of
        true ->
            w("-doc \"\"\"\n", []),
            w("~s\n", [Doc]),
            w("\"\"\".\n", []);
        false ->
            w("-doc \"~s\".\n", [Doc])
    end.

is_single_line(Str) ->
    not lists:member($\n, Str).

has_quotes(Str) ->
    lists:member($", Str).

%% --- Internal: scan and store docs ---

scan_docs(_Module, Defs) ->
    Fs = merge_funcs(Defs),
    [store_func_doc(F) || F <- Fs],
    ok.

merge_funcs(All) ->
    Get = fun(Name0) ->
                  Name = case Name0 of
                             [[_|_]=N|_] -> N;
                             Name0 -> Name0
                         end,
                  F = get(Name),
                  DocName = gl_gen_erl:doc_name(Name, F#func.alt),
                  {DocName, F}
          end,
    Rels = [Get(Name) || Name <- All],
    Fam = sofs:relation_to_family(sofs:relation(Rels)),
    sofs:to_external(Fam).

store_func_doc({DocName, _Fs}) ->
    case gen_raw_doc(DocName) of
        ignore -> ok;
        {raw, Dir, Name, Doc} ->
            put({gl_doc_raw, DocName}, {Dir, Name, Doc});
        {text, Text} ->
            put({gl_doc, DocName}, Text)
    end.

gen_raw_doc(Name) ->
    case parse_doc(Name, Dir1="gl4", Dir2="gl2.1") of
        {error, _} when Name =:= "gluTesselate" ->
            {text, tesselate_doc_text()};
        {error, _} ->
            case reverse(Name) of
                "BRA" ++ _ -> ignore;
                "TXE" ++ _ -> ignore;
                "RHK" ++ _ -> ignore;
                _ -> {text, "No documentation available."}
            end;
        {Found, Doc} ->
            {raw, Found, Name, Doc}
    end.

parse_doc(Name, Dir1, Dir2) ->
    GLDir = get(gl_src_dir),
    case gl_scan_doc:file(filename:join([GLDir, Dir1, Name++".xml"]), []) of
        {error, {_, "no such" ++ _}} ->
            case gl_scan_doc:file(filename:join([GLDir, Dir2, Name++".xml"]), []) of
                {error, _} = Err -> Err;
                Doc -> {Dir2, Doc}
            end;
        Doc ->
            {Dir1, Doc}
    end.

%% --- Format doc elements to markdown text ---

format_parts(Parts) ->
    format_parts(Parts, []).

format_parts([], Acc) ->
    lists:reverse(Acc);
format_parts([{p, ""}|Rest], Acc) ->
    format_parts(Rest, Acc);
format_parts([{p, Text}|Rest], Acc) ->
    format_parts(Rest, ["\n\n", Text|Acc]);
format_parts([{fsummary, _}|Rest], Acc) ->
    %% Skip fsummary in inline docs — it's redundant
    format_parts(Rest, Acc);
format_parts([{pre, Code}|Rest], Acc) ->
    format_parts(Rest, ["\n\n```\n", Code, "\n```\n"|Acc]);
format_parts([Str|Rest], Acc) when is_list(Str) ->
    format_parts(Rest, [Str|Acc]);
format_parts([Bin|Rest], Acc) when is_binary(Bin) ->
    format_parts(Rest, [Bin|Acc]).

format_doc([{constant, Const}|Rest], Acc) ->
    format_doc(Rest, ["`?" ++ Const ++ "`"|Acc]);
format_doc([{emphasis, Const}|Rest], Acc) ->
    format_doc(Rest, ["`" ++ Const ++ "`"|Acc]);
format_doc([{function, Func}|Rest], Acc) ->
    format_doc(Rest, [format_func_ref(Func)|Acc]);
format_doc([{reffunc, Func}|Rest], Acc) ->
    format_doc(Rest, [format_func_ref(Func)|Acc]);
format_doc([{parameter, Param}|Rest], Acc) ->
    format_doc(Rest, ["`" ++ gl_gen_erl:erl_arg_name(Param) ++ "`"|Acc]);
format_doc([{equation, Eq}|Rest], Acc) ->
    Doc = lists:reverse(format_doc([Eq], [])),
    format_doc(Rest, [Doc|Acc]);
format_doc([{fenced, Open, Close, Eq}|Rest], Acc) ->
    Doc = lists:flatten([Open|lists:reverse(format_doc(Eq, []))]) ++ Close,
    format_doc(Rest, [Doc|Acc]);
format_doc([{code, Code}|Rest], Acc) ->
    format_doc(Rest, [{pre, Code}|Acc]);
format_doc([para|Rest], Acc) ->
    case lists:splitwith(fun(D) -> D =/= para end, Rest) of
        {[], _} ->
            Para = format_doc(Rest, []),
            [{p, lists:flatten(lists:reverse(Para))}|Acc];
        {P1, P2} ->
            Para = format_doc(P1, []),
            format_doc(P2, [{p, lists:flatten(lists:reverse(Para))}|Acc])
    end;
format_doc([break|Rest], Acc) ->
    format_doc(Rest, ["\n"|Acc]);
format_doc([{purpose, _Purpose}|Doc0], Acc) ->
    %% Skip fsummary/purpose — redundant in inline docs
    case lists:splitwith(fun(D) -> D =/= para end, Doc0) of
        {[], Doc} ->
            format_doc(Doc, Acc);
        {_More, Doc} ->
            format_doc(Doc, Acc)
    end;
format_doc([Str|Rest], Acc) when is_list(Str) ->
    format_doc(Rest, [Str|Acc]);
format_doc([], Acc) ->
    Acc.

format_func_ref(Func) ->
    ErlName = gl_gen_erl:erl_func_name(Func),
    M = case Func of
            "glu" ++ _ -> "glu";
            "gl" ++ _  -> "gl"
        end,
    %% Get the current module being generated
    CurMod = case get(current_module) of
                 undefined -> M;
                 Mod -> Mod
             end,
    case get({export_doc, Func}) of
        undefined ->
            case get({doc_ref, gl_gen_erl:doc_name(Func, undefined)}) of
                undefined ->
                    "`" ++ Func ++ "`";
                Export ->
                    Prefix = case M =/= CurMod of
                                 true -> M ++ ":";
                                 false -> ""
                             end,
                    "[`" ++ M ++ ":" ++ ErlName ++ "/" ++ arity_from_export(Export) ++ "`](`" ++ Prefix ++ Export ++ "`)"
            end;
        {Export, _DocFunc} ->
            Prefix = case M =/= CurMod of
                         true -> M ++ ":";
                         false -> ""
                     end,
            "[`" ++ M ++ ":" ++ ErlName ++ "/" ++ arity_from_export(Export) ++ "`](`" ++ Prefix ++ Export ++ "`)"
    end.

arity_from_export(Export) ->
    case string:split(Export, "/") of
        [_, Arity] -> Arity;
        _ -> "0"
    end.

%% Line wrap text at MaxCol, preserving paragraph breaks
line_wrap(Text, MaxCol) ->
    Paras = string:split(Text, "\n\n", all),
    Wrapped = lists:join("\n\n", [wrap_para(P, MaxCol) || P <- Paras]),
    lists:flatten(Wrapped).

wrap_para(Para, MaxCol) ->
    %% Don't wrap code blocks or URLs
    Words = string:tokens(Para, " "),
    wrap_words(Words, MaxCol, 0, []).

wrap_words([], _MaxCol, _Col, Acc) ->
    lists:flatten(lists:reverse(Acc));
wrap_words([Word|Rest], MaxCol, Col, Acc) ->
    Len = length(Word),
    case Col of
        0 ->
            wrap_words(Rest, MaxCol, Len, [Word|Acc]);
        _ when Col + 1 + Len > MaxCol ->
            wrap_words(Rest, MaxCol, Len, [Word, "\n"|Acc]);
        _ ->
            wrap_words(Rest, MaxCol, Col + 1 + Len, [Word, " "|Acc])
    end.

%% --- Hardcoded docs ---

tesselate_doc_text() ->
    "Triangulates a polygon, the polygon is specified by a `Normal` and `Vs` a list\n"
    "of vertex positions.\n\n"
    "The function returns a list of indices of the vertices and a binary (64bit\n"
    "native float) containing an array of vertex positions, it starts with the\n"
    "vertices in `Vs` and may contain newly created vertices in the end.".

fake_tesselate() ->
    put("gluTesselate", #func{name="gluTesselate", params=[#arg{name="normal"}, #arg{name="vs"}]}),
    "gluTesselate".
