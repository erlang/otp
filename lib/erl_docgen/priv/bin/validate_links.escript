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
    lists:foldl(fun validate_links/2,maps:from_list([{m2a,M2A}|Files]),Files),
    erlang:halt(get(exit_code)).

parse_file(Filename) ->
%    io:format("Parse: ~p~n",[Filename]),
    case xmerl_sax_parser:file(Filename, [skip_external_dtd,
                                          {event_fun,fun event/3},
                                          {event_state,#{}}]) of
        {ok, Res, _} ->
            {maps:get(filename,Res), maps:remove(filename,Res)}
    end.

event({startElement, _, Tag, _, _Attr}, _Line, #{ type := Type } = State) when
      (Tag =:= "module") and (Type =:= "erlref");
      (Tag =:= "lib") and (Type =:= "cref");
      (Tag =:= "com") and (Type =:= "comref");
      (Tag =:= "file") and (Type =:= "fileref");
      (Tag =:= "app") and (Type =:= "appref");
      (Tag =:= "file") and (Type =:= "chapter") ->
    maps:put(filename,undefined,State);
event({characters,ModuleName},{Path,_,_},
      #{ type := "chapter", filename := undefined } = State) ->
    maps:put(filename,{path2app(Path),filename:rootname(ModuleName)}, State);
event({characters,ModuleName},{Path,_,_},
      #{ type := "appref", filename := undefined } = State) ->
    maps:put(filename,{path2app(Path),ModuleName ++ "_app"}, State);
event({characters,ModuleName},{Path,_,_},#{ filename := undefined } = State) ->
    maps:put(filename,{path2app(Path),ModuleName}, State);
event({startElement, _, "see" ++ _ = What, _, Attr}, Line, State) ->
    maps:put(What, [{Line,Attr}|maps:get(What, State, [])], State);
event({startElement, _, What, _, _Attr}, _, State) when
      What =:= "erlref";
      What =:= "appref";
      What =:= "comref";
      What =:= "cref";
      What =:= "fileref";
      What =:= "chapter" ->
    maps:put(type,What,State);
event({startElement, _, What, _, _Attr}, {Path,Filename,_}, State) when
      What =:= "book";
      What =:= "application";
      What =:= "internal";
      What =:= "part" ->
    State#{ filename => {path2app(Path),filename:rootname(Filename)}, type => What };
event(_Event, _Line, State) ->
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
    lists:foldl(
      fun({LinkType, TypeLinks}, Cache) ->
              lists:foldl(
                fun({Line,Link}, LinkCache) ->
                        validate_link(Filename, LinkType, Line, Link, LinkCache)
                end, Cache, TypeLinks)
      end, CachedFiles, maps:to_list(maps:remove(type,Links))).
validate_link(Filename, LinkType, Line, [{_,_,"marker",Marker}], CachedFiles) ->
    validate_link(Filename, LinkType, Line, Marker, CachedFiles);
validate_link(Filename, "seemfa", Line, Link, CachedFiles) ->
    case lists:reverse(string:lexemes(Link,"#")) of
        [Link] ->
            fail(Line, "Invalid link in seemfa. "
                 "Must contains a '#'.");
        [Anchor|AppMod] ->
            case string:lexemes(Anchor,"/") of
                [Func, Arity] ->
                    try list_to_integer(Arity) of
                        _ ->
                            validate_link(Filename, "seeerl", Line,
                                          AppMod ++ "#" ++ Func ++ "-" ++ Arity,
                                          CachedFiles)
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
    Type = read_link(Line, parse_link(Filename, maps:get(m2a,CachedFiles), Link), CachedFiles),
    validate_type(Line,LinkType,Type),
    CachedFiles;
validate_link(Filename, LinkType, Line, Link, CachedFiles) ->
    Type = read_link(Line, parse_link(Filename, maps:new(), Link), CachedFiles),
    validate_type(Line,LinkType,Type),
    CachedFiles.

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
