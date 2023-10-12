%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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
%%% File    : gl_gen_erl.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description :
%%%
%%% Created : 18 Apr 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------
-module(gl_gen_doc).

-include("gl_gen.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([gen/2]).

-import(lists, [foldl/3,foldr/3,reverse/1, keysearch/3, map/2, filter/2, max/1]).
-import(gen_util, [lowercase/1, lowercase_all/1, uppercase/1, uppercase_all/1,
		   open_write/1, open_write/2, close/0, erl_copyright/0, w/2,
		   args/3, args/4, strip_name/2]).


-define(HTTP_TOP, "https://www.khronos.org/registry/OpenGL-Refpages/").

gen(GLDefs, GLUDefs) ->
    case os:getenv("GL_MAN_SRC_DIR") of
        false ->
            io:format(" Skipping gl docs: 'GL_MAN_SRC_DIR' not set\n"),
            ok;
        Dir ->
            case filelib:is_dir(Dir) of
                true ->
                    put(gl_src_dir, Dir),
                    gen_gl("gl",GLDefs),
                    Tess = fake_tesselate(),
                    gen_gl("glu", lists:sort([Tess|GLUDefs]));
                false ->
                    io:format(" Skipping gl docs: '~s' not readable\n", [Dir])
            end
    end.

gen_gl(Name, GLDefs) ->
    open_write("../doc/src/" ++ Name ++ ".xml"),
    Intro = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
        "<!DOCTYPE erlref SYSTEM \"erlref.dtd\">\n"
        "\n<!-- THIS FILE IS GENERATED DO NOT EDIT -->\n\n",

    Desc = [{p, ["Standard OpenGL API"]},
            {p, ["This documents the functions as a brief version of the"
                 " complete ", {url, [{href, ?HTTP_TOP}], ["OpenGL reference pages."]}]}],
    Root = [#xmlAttribute{name=prolog, value=Intro}],

    Fs = merge_funcs(GLDefs),
    Funcs = lists:append([gen_func(F) || F <- Fs]),

    Types0 = [
              nl(4), {datatype, [{name, [{name, "i"}], []}]},
              nl(4), {datatype, [{name, [{name, "f"}], []}]},
              nl(4), {datatype, [{name, [{name, "enum"}], []}]},
              nl(4), {datatype, [{name, [{name, "matrix"}], []}]},
              nl(4), {datatype, [{name, [{name, "m12"}], []}]},
              nl(4), {datatype, [{name, [{name, "m16"}], []}]},
              nl(4), {datatype, [{name, [{name, "mem"}], []}]}
             ],
    Types = case Name of
                "glu" -> [nl(4), {datatype, [{name, [{name, "vertex"}], []}]}
                         | Types0];
                "gl"  -> [nl(4), {datatype, [{name, [{name, "clamp"}], []}]},
                          nl(4), {datatype, [{name, [{name, "offset"}], []}]}
                         |Types0]
            end,

    ErlRef = {erlref,
              [nl(0), {header, gen_header(Name)},
               nl(0), {module, [Name]},
               nl(0), {modulesummary, ["Erlang wrapper functions for OpenGL"]},
               nl(0), {description, Desc},
               nl(0), {datatypes, Types},
               nl(0), {funcs, Funcs},
               nl(0)
              ]},
    Export = xmerl:export_simple([nl(0), ErlRef],xmerl_xml, Root),
    w("~s~n",[unicode:characters_to_binary(Export)]),
    close(),
    ok.

gen_header(Name) ->
    Legal ="
      Licensed under the Apache License, Version 2.0 (the \"License\");
      you may not use this file except in compliance with the License.
      You may obtain a copy of the License at

          http://www.apache.org/licenses/LICENSE-2.0

      Unless required by applicable law or agreed to in writing, software
      distributed under the License is distributed on an \"AS IS\" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
      See the License for the specific language governing permissions and
      limitations under the License.
",

    [nl(2), {copyright,
             [nl(4), {year, ["2020"]},
              nl(4), {holder, ["Ericsson AB. All Rights Reserved."]}]},
     nl(2), {legalnotice, [Legal, nl(2)]},
     nl(2), {title, [Name]},
     nl(0)].

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

gen_func({DocName, Fs}) ->
    put(current_func,DocName),
    Names0 = [{name, xml_func_name(F#func.name, args(F), 1), []} || F <- Fs],
    Names = [nl(4)|lists:join(nl(4),Names0)],
    case gen_doc(DocName, Names0) of
        ignore -> [];
        [{fsummary,_}=Fsum|Desc] ->
            check_doc([Fsum]),
            check_doc(Desc),
            erase(current_func),
            All = [Names,
                   [nl(4), Fsum, nl(4)],
                   [{desc, Desc}, nl(2)]],
            [nl(2), {func, lists:append(All)}]
    end.

check_doc(Docs) ->
    try check_doc_1(Docs)
    catch {err, What} ->
            io:format("Error: ~p~n", [What]),
            io:format("~p: ~P~n",[get(current_func),Docs,20]),
            exit(foo)
    end.

check_doc_1(Docs) ->
    try xmerl:export_simple_content(Docs, xmerl_xml) of
        _ -> ok
    catch _:_ ->
            [check_doc_1(E) || {_, E} <- Docs],
            throw({err, Docs})
    end.

args(#func{alt={vector,VecPos,Vec}}) ->
    #func{params=As0} = get(Vec),
    {As1,_As2} = lists:split(VecPos, As0),
    Args = lists:filter(fun(Arg) -> gl_gen_erl:func_arg(Arg) =/= skip end, As1),
    length(Args) +1;
args(#func{params=As0}) ->
    Args = lists:filter(fun(Arg) -> gl_gen_erl:func_arg(Arg) =/= skip end, As0),
    length(Args).

format_doc([{constant, Const}|Rest], Acc) ->
    format_doc(Rest, [{c, ["?" ++ Const]}|Acc]);
format_doc([{emphasis, Const}|Rest], Acc) ->
    format_doc(Rest, [{c, [Const]}|Acc]);
format_doc([{function, Func}|Rest], Acc) ->
    format_doc(Rest, [format_func(Func)|Acc]);
format_doc([{reffunc, Func}|Rest], Acc) ->
    format_doc(Rest, [format_func(Func)|Acc]);
format_doc([{parameter, Param}|Rest], Acc) ->
    format_doc(Rest, [{c, [gl_gen_erl:erl_arg_name(Param)]}|Acc]);
format_doc([{equation, Eq}|Rest], Acc) ->
    Doc = lists:reverse(format_doc([Eq], [])),
    format_doc(Rest, [Doc|Acc]);
format_doc([{fenced, Open, Close, Eq}|Rest], Acc) ->
    Doc = lists:reverse([Close|format_doc(Eq, Open)]),
    format_doc(Rest, [Doc|Acc]);

format_doc([{code, Code}|Rest], Acc) ->
    format_doc(Rest, [{pre, Code}|Acc]);

format_doc([para|Rest], Acc) ->
    case lists:splitwith(fun(D) -> D =/= para end, Rest) of
        {[], _} ->
            Para = format_doc(Rest, []),
            [nl(4),{p, lists:reverse(Para)}|Acc];
        {P1,P2} ->
            Para = format_doc(P1, []),
            format_doc(P2, [nl(4),{p, lists:reverse(Para)}|Acc])
    end;
format_doc([break|Rest], Acc) ->
    format_doc(Rest, ["<br />"|Acc]);
format_doc([{purpose, Purpose} | Doc0], Acc) ->
    case lists:splitwith(fun(D) -> D =/= para end, Doc0) of
        {[], Doc} ->
            format_doc(Doc, [nl(4), {fsummary, [Purpose]}|Acc]);
        {More,Doc} ->
            %% fsummary may not contain links..
            Adds = to_text(More),
            format_doc(Doc, [nl(4), {fsummary, [Purpose|Adds]}|Acc])
    end;
%% format_doc([listentry|Rest], _Count) ->
%%     w("~n%%~n%% ", []),
%%     format_doc(Rest, ?LINE_LEN);
format_doc([Str|Rest], Acc) ->
    format_doc(Rest, [Str|Acc]);
format_doc([], Acc) ->
    Acc.

format_func(Func) ->
    Out = fun(MarkerF, DocF) ->
                  M = case Func of
                          "glu" ++ _ -> "glu";
                          "gl" ++ _ ->  "gl"
                      end,
                  {Marker,DocFunc} = {M ++ "#" ++ MarkerF, M ++ ":" ++ DocF},
                  #xmlElement{name=seemfa, attributes=[{marker,Marker}],
                              content=[{c, [#xmlText{value=DocFunc}]}]}
	  end,
    %% io:format("~s: ~s~n",[get(current_func), Func]),
    case get({export_doc, Func}) of
	undefined ->
	    case get({doc_ref, gl_gen_erl:doc_name(Func, undefined)}) of
		undefined ->
		    %% io:format("Func ~p undefined (~p) ~n",
		    %%  	      [Func, gl_gen_erl:doc_name(Func, undef)]),
		    {c, [Func]};
		Export ->
                    Out(Export, gl_gen_erl:erl_func_name(Func) ++ "()")
	    end;
	{Export, Func} ->
	    Out(Export, Export);
        {Export, DocFunc} ->
            DocExport = get({doc_ref,DocFunc}),
            Out(DocExport, Export)
    end.

gen_doc(Name, _Debug) ->
    case parse_doc(Name, Dir1 ="gl4", Dir2="gl2.1") of
        {error, _} when Name =:= "gluTesselate" ->
            tesselate_doc();
        {error, _} ->
            case reverse(Name) of
                "BRA" ++ _ -> ignore;
                "TXE" ++ _ -> ignore;
                "RHK" ++ _ -> ignore;
                _ ->
                    %% io:format("Missing doc: ~s not found in ~s or ~s~n   ~0.p~n",
                    %%  	      [Name, Dir1, Dir2, _Debug]),
                    [{fsummary, []}, {p, ["No documentation available."]}]
            end;
        {Found, Doc} ->
            {Dir,Ext} = case Found of
                            Dir1 -> {"gl4/html", "xhtml"};
                            Dir2 -> {"gl2.1/xhtml", "xml"}
                        end,
            Ref = io_lib:format("~s~s/~s.~s", [?HTTP_TOP, Dir, Name, Ext]),
            Href = {p, [{url, [{href, Ref}], ["External documentation."]}]},
            lists:reverse([Href, nl(4)|format_doc(Doc, [])])
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

xml_func_name(Name, As, Clause) ->
    ErlName = gl_gen_erl:erl_func_name(Name),
    [{name,ErlName}, {arity, integer_to_list(As)}, {clause_i, integer_to_list(Clause)}, {since,""}].

nl(Indent) ->
    NL = [$\n, lists:duplicate(Indent, $\s)],
    #xmlText{value=NL}.

to_text([{reffunc, Func}|Rest]) ->
    [{c, [Func]}|to_text(Rest)];
to_text([{c,_}=C|Rest]) ->
    [C|to_text(Rest)];
to_text([L|Rest]) when is_list(L) ->
    [to_text(L)|to_text(Rest)];
to_text([C|_]=Rest) when is_integer(C) ->
    {Str, Cont} = lists:splitwith(fun(Char) -> is_integer(Char) end, Rest),
    [Str|to_text(Cont)];
to_text([]) ->
    [].

%% Hardcoded erlang special
fake_tesselate() ->
    put("gluTesselate", #func{name="gluTesselate", params=[#arg{name="normal"}, #arg{name="vs"}]}),
    "gluTesselate".

tesselate_doc() ->
    [{fsummary, ["triangulate a face"]},
     {p, ["Triangulates a polygon, the polygon is specified by a ",
          {c, ["Normal"]}, " and ", {c, ["Vs"]}, " a list of vertex positions. "]},
     {p, ["The function returns a list of indices of the vertices"
          " and a binary (64bit native float) containing an array of"
          " vertex positions, it starts with the vertices in ", {c, ["Vs"]},
          " and may contain newly created vertices in the end."]}].
