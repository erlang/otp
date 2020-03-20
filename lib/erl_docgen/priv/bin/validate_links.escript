#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A 1 +SDio 1 +S 1 -mode minimal
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : chunk.escript
%%
%% Created : 1 Nov 2018 by Kenneth Lundin <uabkeld@elxa31hr002>
%%
%% Trampoline to xml to chunk creation.
%%----------------------------------------------------------------------

-mode(compile).

main([ErlTop,Mod2App|Args]) ->
    Files = [parse_file(Arg) || Arg <- Args],
    M2A = parse_mod2app(Mod2App),
    put(top, ErlTop),
    put(exit_code, 0),
    CachedFiles = maps:from_list([{m2a,M2A}|Files]),
    [validate_links(File,CachedFiles) || File <- Files],
    erlang:halt(get(exit_code)).

parse_file(Filename) ->
%    io:format("Parse: ~p~n",[Filename]),
    case xmerl_sax_parser:file(Filename, [skip_external_dtd,
                                          {event_fun,fun handle_event/3},
                                          {event_state,#{ funcs => [],
                                                          datatypes => []}}]) of
        {ok, Res, _} ->
%            [io:format("~p~n",[Res]) || maps:get(filename,Res) == {"erts","erl_tracer"}],
            {maps:get(filename,Res), maps:remove(filename,Res)}
    end.

handle_event(startDocument, _Line, State) ->
    {State,[]};
handle_event({startElement, _, Tag, _, Attr}, Line, {State, Stack}) ->
    NewState = event({startElement, Tag, [{A,V} || {_,_,A,V} <- Attr]}, Line, State, Stack),
    {NewState,[Tag | Stack]};
handle_event({endElement, _, Tag, _}, Line, {State, [Tag|NewStack]}) ->
    {event({endElement,Tag}, Line, State, NewStack), NewStack};
handle_event({characters, Chars}, Line, {State, Stack}) ->
    {event({characters,Chars}, Line, State, Stack), Stack};
handle_event(endDocument, _Line, {State,[]}) ->
    State;
handle_event(Ignore, _, S) when
      (not is_tuple(Ignore)) orelse (element(1,Ignore) =/= startElement),
      (not is_tuple(Ignore)) orelse (element(1,Ignore) =/= endElement),
      (not is_tuple(Ignore)) orelse (element(1,Ignore) =/= characters),
      Ignore =/= startDocument,
      Ignore =/= endDocument ->
    S;
handle_event(E,L,S) ->
    io:format("handle_event(~p,~p,~p) did not match~n",[E,L,S]),
    halt(1).

%% Get the filename of document
event({startElement, Tag, _Attr}, _Line, #{ type := Type } = State, _) when
      (Tag =:= "module") and (Type =:= "erlref");
      (Tag =:= "lib") and (Type =:= "cref");
      (Tag =:= "com") and (Type =:= "comref");
      (Tag =:= "file") and (Type =:= "fileref");
      (Tag =:= "app") and (Type =:= "appref");
      (Tag =:= "file") and (Type =:= "chapter") ->
    maps:put(filename,undefined,State);
event({characters,ModuleName},{Path,_,_},
      #{ type := "chapter", filename := undefined } = State, _) ->
    maps:put(filename,{path2app(Path),filename:rootname(ModuleName)}, State);
event({characters,ModuleName},{Path,_,_},
      #{ type := "appref", filename := undefined } = State, _) ->
    maps:put(filename,{path2app(Path),ModuleName ++ "_app"}, State);
event({characters,ModuleName},{Path,_,_},#{ filename := undefined } = State, _) ->
    maps:put(filename,{path2app(Path),ModuleName}, State);

%% Get the see* attributes
event({startElement, "see" ++ _ = What, Attr}, Line, State, _) ->
    maps:put(What, [{Line,Attr}|maps:get(What, State, [])], State);

%% Get all the marker attributes
event({startElement, "marker", [{"id",Id}]}, Line, State, _) ->
    maps:put(marker, [{Line,Id}|maps:get(marker, State, [])], State);

%% Get all func->name markers in erlref
event({startElement, "name", Attr}, _Line, #{ type := "erlref" } = State, ["func" | _]) ->
    case {proplists:get_value("name",Attr),proplists:get_value("arity",Attr)} of
        {undefined,undefined} ->
            %% We parse the function name from the content,
            %% so have to capture characters
            error = maps:find(characters,State),
            maps:put(characters,[],State);
        {Func,Arity} when Func =/= undefined, Arity =/= undefined ->
            maps:put(funcs, [{Func,Arity}|maps:get(funcs,State,[])], State)
    end;
event({endElement,"name"}, _Line, #{ characters := Chars} = State, ["func"|_]) ->
    FAs = docgen_xml_to_chunk:func_to_tuple(Chars),
    NewFuncs =
        lists:foldl(
          fun({F,A},Acc) ->
                  [{F,integer_to_list(A)} | Acc]
          end, maps:get(funcs,State,[]), FAs),
    maps:remove(characters, maps:put(funcs, NewFuncs, State));

%% Get all datatype->name markers in erlref
event({startElement, "name", Attr}, _Line, #{ type := "erlref" } = State, ["datatype" | _]) ->
    case {proplists:get_value("name",Attr),proplists:get_value("arity",Attr)} of
        {undefined,undefined} ->
            %% We parse the datatype name from the content,
            %% so have to capture characters
            error = maps:find(characters,State),
            maps:put(characters,[],State);
        {Name,Arity} when Name =/= undefined, Arity =:= undefined ->
            maps:put(datatypes, [Name|maps:get(datatypes,State,[])], State)
    end;
event({endElement,"name"}, _Line, #{ characters := Chars} = State, ["datatype"|_]) ->
    FAs = docgen_xml_to_chunk:func_to_tuple(Chars),
    NewFuncs =
        lists:foldl(
          fun({F,_A},Acc) -> [F | Acc] end, maps:get(datatypes,State,[]), FAs),
    maps:remove(characters, maps:put(datatypes, NewFuncs, State));

%% Capture all characters
event({characters,Chars},_Line, #{ characters := Acc } = State, _) ->
    maps:put(characters,[Acc,Chars],State);

%% Get the type of document
event({startElement, What, _Attr}, _, State, _) when
      What =:= "erlref";
      What =:= "appref";
      What =:= "comref";
      What =:= "cref";
      What =:= "fileref";
      What =:= "chapter" ->
    maps:put(type,What,State);
event({startElement, What, _Attr}, {Path,Filename,_}, State, _) when
      What =:= "book";
      What =:= "application";
      What =:= "internal";
      What =:= "part" ->
    State#{ filename => {path2app(Path),filename:rootname(Filename)}, type => What };
event(_Event, _Line, State, _) ->
    State.

parse_mod2app(Filename) ->
%    io:format("Parse: ~p~n",[Filename]),
    Event =
        fun({startElement,_,"module",_,[{_,_,"name",Name}]}, _, {undefined, Acc}) ->
                {Name,Acc};
           ({characters,Chrs}, _, {Name, Acc}) when Name =/= undefined ->
                {undefined,[{Name,Chrs}|Acc]};
           (_, _, State) ->
                State
        end,
    case xmerl_sax_parser:file(Filename, [skip_external_dtd,
                                          {event_fun,Event},
                                          {event_state,{undefined,[]}}]) of
        {ok, {_,Res}, _} ->
            maps:from_list(Res)
    end.

validate_links({Filename, Links}, CachedFiles) ->
    %% io:format("~s ~p~n",[Links]),
    lists:foreach(
      fun({LinkType, TypeLinks}) ->
              lists:foreach(
                fun({Line,Link}) ->
                        validate_link(Filename, LinkType, Line, Link, CachedFiles)
                end, TypeLinks)
      end, maps:to_list(maps:filter(fun(Key,_) -> not is_atom(Key) end,Links))).
validate_link(Filename, LinkType, Line, [{"marker",Marker}], CachedFiles) ->
    validate_link(Filename, LinkType, Line, Marker, CachedFiles);
validate_link(Filename, "seemfa", Line, Link, CachedFiles) ->
    case string:find(Link,"#") of
        nomatch ->
            fail(Line, "Invalid link in seemfa. "
                 "Must contains a '#'.");
        _ ->
            {App,Mod,Anchor} = ParsedLink = parse_link(Filename, maps:new(), Link),
            case string:lexemes(Anchor,"/") of
                [Func, Arity] ->
                    try list_to_integer(Arity) of
                        _ ->
                            MF = App ++ ":" ++ Mod ++ "#" ++ Func,
                            Funcs = maps:get(funcs,maps:get({App,Mod},CachedFiles)),
                            case lists:member({Func,Arity},Funcs) of
                                true ->
                                    validate_type(Line, "seemfa",
                                                  read_link(Line, ParsedLink, CachedFiles));
                                false ->
                                    fail(Line, "Could not find documentation for ~s when "
                                         "resolving link",[MF  ++ "/" ++ Arity])
                            end
                    catch _:_ ->
                            fail(Line, "Invalid arity for seemfa. "
                                 "Must end in a number")
                    end;
                _ ->
                    fail(Line, "Invalid anchor for seemfa. "
                         "Must contain a '/'.")
            end
    end;
validate_link(Filename, LinkType = "seetype", Line, Link, CachedFiles) ->
    {App,Mod,Type} = ParsedLink = parse_link(Filename, maps:get(m2a,CachedFiles), Link),
    Types = maps:get(datatypes,maps:get({App,Mod},CachedFiles)),
    case lists:member(Type, Types) of
        false ->
            fail(Line, "Could not find documentation for ~s when "
                 "resolving link",[App ++ ":" ++ Mod ++ "#" ++ Type]);
        _ ->
            validate_type(Line,LinkType,read_link(Line, ParsedLink, CachedFiles))
    end;
validate_link(Filename, "seeerl" = LinkType, Line, Link, CachedFiles) ->
    Type = read_link(Line, parse_link(Filename, maps:get(m2a,CachedFiles), Link), CachedFiles),
    validate_type(Line,LinkType,Type);
validate_link(Filename, LinkType, Line, Link, CachedFiles) ->
    Type = read_link(Line, parse_link(Filename, maps:new(), Link), CachedFiles),
    validate_type(Line,LinkType,Type).

parse_link({SelfApp,Filename},Mod2App,Link) ->
    {AppMod, Marker} =
        case string:split(Link, "#") of
            [Link] ->
                {Link, ""};
            [AppMod0,Marker0] ->
                {AppMod0, Marker0}
        end,
    {App,Mod} =
        case string:split(AppMod, ":") of
            [""] ->
                {SelfApp,Filename};
            [AppMod] ->
                {maps:get(AppMod,Mod2App,SelfApp),AppMod};
            [App0,Mod0] ->
                {App0,Mod0}
        end,
    {App,Mod,Marker}.

read_link(_Line, {_App,"index",_}, _Cache) ->
    "index";
read_link(Line, {App,Mod,_}, Cache) ->
    case maps:find({App,Mod}, Cache) of
        {ok, #{ type := Type } } ->
            Type;
        error ->
            %% fail(Line, "Could not find: ~p~p~n~p~n",
            %%   [App,Mod,lists:sort(maps:keys(Cache))]),
            fail(Line, "Could not find: ~p~p~n",[App,Mod]),
            halt(1)
    end.

validate_type(_Line, "seeerl","erlref") ->
    ok;
validate_type(_Line, "seemfa","erlref") ->
    ok;
validate_type(_Line, "seetype","erlref") ->
    ok;
validate_type(_Line, "seeguide","chapter") ->
    ok;
validate_type(_Line, "seeguide","index") ->
    ok;
validate_type(_Line, "seefile","fileref") ->
    ok;
validate_type(_Line, "seeapp","appref") ->
    ok;
validate_type(_Line, "seeapp","index") ->
    ok;
validate_type(_Line, "seecom","comref") ->
    ok;
validate_type(_Line, "seecref","cref") ->
    ok;
validate_type(Line, From, To) ->
    fail(Line, "Invalid <~s> pointing to a <~s> file",[From,To]).

path2app(Path) ->
    case lists:reverse(string:lexemes(Path,"/")) of
        ["xml","doc",App|_] ->
            App;
        [System,"xml","doc","system"|_] ->
            "system/" ++ System
    end.

%% getFileName("erts",Mod) ->
%%     filename:join([get(top),"erts","doc","xml",Mod++".xml"]);
%% getFileName("system/"++System,Mod) ->
%%     filename:join([get(top),"system","doc",System,"..","xml",System,Mod++".xml"]);
%% getFileName(App,Mod) ->
%%     filename:join([get(top),"lib",App,"doc","src","..","xml",Mod++".xml"]).

fail(Line, Slogan) ->
    fail(Line, Slogan,[]).
fail({Path, Filename, Line}, Slogan, SloganArgs) ->
    Format = lists:flatten(
               io_lib:format("~s:~p ~s~n",
                             [filename:join(Path,Filename),Line,Slogan])),
    io:format(Format, SloganArgs),
    put(exit_code, 1).
