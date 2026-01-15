%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(asn1ct_partial_decode).
-moduledoc false.
-export([prepare/2,format_error/1]).

-include("asn1_records.hrl").

prepare(Items, Mod) ->
    _ = [prepare_item(Item, Mod) || Item <- Items],
    ok.

format_error({bad_decode_instruction,Term}) ->
    io_lib:format(<<"badly formed exclusive decode instruction: ~p">>,
                  [Term]);
format_error({bad_exclusive_decode,Term}) ->
    io_lib:format(<<"badly formed exclusive decode instructions: ~p">>,
                  [Term]);
format_error({bad_module_name,Mod,ShouldBe}) ->
    io_lib:format(<<"the module name is ~p; expected it to be the same as the name"
                    " of the ASN.1 module (~p)">>,
                  [Mod,ShouldBe]);
format_error({bad_selective_decode,Term}) ->
    io_lib:format(<<"badly formed selective decode instructions: ~p">>,
                  [Term]);
format_error({bad_selective_decode_element,Term}) ->
    io_lib:format(<<"badly formed element in selective decode instruction: ~p">>,
                  [Term]);
format_error({bad_selective_decode_type_list,Term}) ->
    io_lib:format(<<"badly formed type list in selective decode instruction: ~p">>,
                  [Term]);
format_error({stepping_into_primitive,Path}) ->
    io_lib:format(<<"the tail end of selective decode instructions attempts to ",
                    "step into a primitive type:\n  ~p">>,
                  [Path]);
format_error({undefined_name,Type}) ->
    io_lib:format(<<"name ~p not found">>, [Type]);
format_error({undefined_type,Type}) ->
    io_lib:format(<<"type ~p does not exist">>, [Type]).

%%%
%%% Common macros.
%%%

-define(ASN1CT_GEN_BER, asn1ct_gen_ber_bin_v2).

%%%
%%% Start of local functions.
%%%

prepare_item({selective_decode,SelectedDecode}, Mod) ->
    CommandList = selective_decode(Mod, SelectedDecode),
    asn1ct:save_config(partial_decode, CommandList),
    asn1ct:save_gen_state(selective_decode, SelectedDecode),
    ok;
prepare_item({exclusive_decode,ExclusiveDecode}, Mod) ->
    ExclusiveCommands = exclusive_decode(Mod, ExclusiveDecode),
    asn1ct:save_config(partial_incomplete_decode, ExclusiveCommands),
    asn1ct:save_gen_state(exclusive_decode, ExclusiveDecode, ExclusiveCommands),
    ok.

%%%
%%% Handle exclusive decode.
%%%

-define(MANDATORY, mandatory).
-define(DEFAULT, default).
-define(DEFAULT_UNDECODED, default_undecoded).
-define(OPTIONAL, opt).
-define(OPTIONAL_UNDECODED, opt_undecoded).
-define(PARTS, parts).
-define(UNDECODED, undecoded).
-define(ALTERNATIVE, alt).
-define(ALTERNATIVE_UNDECODED, alt_undecoded).
-define(ALTERNATIVE_PARTS, alt_parts).

exclusive_decode(Mod, {ModI,Instructions}) when is_list(Instructions) ->
    if
        Mod =:= ModI ->
            exclusive_decode_1(Mod, Instructions);
        true ->
            cfg_error({bad_module_name,ModI,Mod})
    end;
exclusive_decode(_Mod, Term) ->
    cfg_error({bad_exclusive_decode,Term}).

exclusive_decode_1(Mod, [{FunName,[TopType,Directives0]}|Is])
  when is_atom(FunName), is_atom(TopType), is_list(Directives0) ->
    Directives = exclusive_decode_map(Directives0),
    [{FunName,TopType,exclusive_decode_2(Mod, TopType, Directives)} |
     exclusive_decode_1(Mod, Is)];
exclusive_decode_1(_Mod, []) ->
    [];
exclusive_decode_1(_Mod, Term) ->
    cfg_error({bad_exclusive_decode,Term}).

exclusive_decode_2(ModName, TopType, Directives) ->
    case asn1_db:dbget(ModName, TopType) of
	#typedef{typespec=TS} ->
	    Acc = get_tag_command(TS, ?MANDATORY, mandatory),
	    exclusive_decode_command(get_components(TS#type.def),
                                     Directives, Acc);
	undefined ->
            cfg_error({undefined_type,TopType})
    end.

exclusive_decode_map(Commands) ->
    exclusive_decode_map(Commands, []).

exclusive_decode_map([H|T], Acc) ->
    case H of
        {Name,Command0} when is_atom(Name) ->
            Command =
                if
                    Command0 =:= ?UNDECODED; Command0 =:= ?PARTS ->
                        Command0;
                    is_list(Command0) ->
                        exclusive_decode_map(Command0, []);
                    true ->
                        cfg_error({bad_decode_instruction,H})
                end,
            exclusive_decode_map(T, [{Name,Command}|Acc]);
        _ ->
            cfg_error({bad_decode_instruction,H})
    end;
exclusive_decode_map([], Acc) ->
    maps:from_list(Acc).

exclusive_decode_command(_, Commands, Acc) when map_size(Commands) =:= 0 ->
    lists:reverse(Acc);
exclusive_decode_command([#'ComponentType'{name=Name,typespec=TS,
                                           prop=Prop}|Comps],
                         Commands0, Acc) ->
    case maps:take(Name, Commands0) of
        {Command,Commands} when is_atom(Command) ->
            TagCommands = get_tag_command(TS, Command, Prop),
            exclusive_decode_command(Comps, Commands, TagCommands++Acc);
        {InnerCommands0,Commands} when is_map(Commands0) ->
            InnerCommands = exclusive_decode_command(TS#type.def,
                                                     InnerCommands0, []),
            case get_tag_command(TS, ?MANDATORY, Prop) of
                [?MANDATORY] ->
                    exclusive_decode_command(Comps, Commands,
                                             [{?MANDATORY,InnerCommands}|Acc]);
                [{Opt,EncTag}] ->
                    exclusive_decode_command(Comps, Commands,
                                             [{Opt,EncTag,InnerCommands}|Acc])
            end;
        error ->
            case get_tag_command(TS, ?MANDATORY, Prop) of
                [] ->
                    case TS of
                        #type{def=#'Externaltypereference'{}} ->
                            exclusive_decode_command(Comps, Commands0, [mandatory|Acc]);
                        _ ->
                            exclusive_decode_command(Comps, Commands0, Acc)
                    end;
                [_|_]=TagCommands ->
                    exclusive_decode_command(Comps, Commands0, TagCommands ++ Acc)
            end
    end;
exclusive_decode_command({'CHOICE',[_|_]=Cs}, Commands, Acc) ->
    exclusive_decode_choice_cs(Cs, Commands, Acc);
exclusive_decode_command({'CHOICE',{Cs1,Cs2}}, Commands, Acc)
  when is_list(Cs1), is_list(Cs2) ->
    exclusive_decode_choice_cs(Cs1 ++ Cs2, Commands, Acc);
exclusive_decode_command(#'Externaltypereference'{module=M,type=Name},
                         Commands, Acc) ->
    #type{def=Def} = get_referenced_type(M, Name),
    exclusive_decode_command(get_components(Def), Commands, Acc);
exclusive_decode_command([], Commands, _) ->
    [{Name,_}|_] = maps:to_list(maps:iterator(Commands, ordered)),
    cfg_error({undefined_name,Name}).

exclusive_decode_choice_cs(_, Commands, Acc) when map_size(Commands) =:= 0 ->
    lists:reverse(Acc);
exclusive_decode_choice_cs([#'ComponentType'{name=Name,typespec=TS}|Cs],
                           Commands0, Acc) ->
    case maps:take(Name, Commands0) of
        {Inner,Commands} ->
            case Inner of
                ?UNDECODED ->
                    TagCommands = get_tag_command(TS, ?ALTERNATIVE_UNDECODED, mandatory),
                    exclusive_decode_choice_cs(Cs, Commands, TagCommands ++ Acc);
                ?PARTS ->
                    TagCommands = get_tag_command(TS, ?ALTERNATIVE_PARTS, mandatory),
                    exclusive_decode_choice_cs(Cs, Commands, TagCommands ++ Acc);
                _ when is_map(Inner) ->
                    [{Command,Tag}] = get_tag_command(TS, ?ALTERNATIVE, mandatory),
                    CompAcc = exclusive_decode_command(get_components(TS#type.def), Inner, []),
                    exclusive_decode_choice_cs(Cs, Commands, [{Command,Tag,CompAcc}|Acc])
            end;
        error ->
            TagCommands = get_tag_command(TS, ?ALTERNATIVE, mandatory),
            exclusive_decode_choice_cs(Cs, Commands0, TagCommands ++ Acc)
    end.

get_tag_command(#type{tag=[]}, _, _) ->
    [];
get_tag_command(#type{tag=[Tag]}, ?MANDATORY, Prop) ->
    [case Prop of
         mandatory ->
             ?MANDATORY;
         {'DEFAULT',_} ->
             {?DEFAULT,?ASN1CT_GEN_BER:tag_to_integer(Tag)};
         _ ->
             {?OPTIONAL,?ASN1CT_GEN_BER:tag_to_integer(Tag)}
     end];
get_tag_command(#type{tag=[_|_]=Tags}, ?PARTS=Command, Prop) ->
    [{anonymous_dec_command(Command, Prop),
      [?ASN1CT_GEN_BER:tag_to_integer(Tag) || Tag <- Tags]}];
get_tag_command(#type{tag=[_|_]=Tags}, ?UNDECODED=Command, Prop) ->
    [{anonymous_dec_command(Command, Prop),
      [?ASN1CT_GEN_BER:tag_to_integer(Tag) || Tag <- Tags]}];
get_tag_command(#type{tag=[Tag]}, Command, Prop) ->
    [{anonymous_dec_command(Command, Prop),
      ?ASN1CT_GEN_BER:tag_to_integer(Tag)}];
get_tag_command(#type{tag=[_|_]=Tags}=Type, Command, Prop) ->
    lists:reverse([hd(get_tag_command(Type#type{tag=[Tag]}, Command, Prop)) ||
                      Tag <- Tags]).

anonymous_dec_command(?UNDECODED, 'OPTIONAL') ->
    ?OPTIONAL_UNDECODED;
anonymous_dec_command(?UNDECODED, {'DEFAULT',_}) ->
    ?DEFAULT_UNDECODED;
anonymous_dec_command(Command,_) ->
    Command.

%%%
%%% Selective decode.
%%%

-define(CHOSEN, chosen).
-define(SKIP, skip).
-define(SKIP_OPTIONAL, skip_optional).

selective_decode(Mod, {ModI,TypeLists}) ->
    if
        Mod =:= ModI ->
            selective_decode1(Mod, TypeLists);
        true ->
            cfg_error({bad_module_name,ModI,Mod})
    end;
selective_decode(_, Bad) ->
    cfg_error({bad_selective_decode,Bad}).

selective_decode1(Mod, [TL|TypeLists]) ->
    [selective_decode2(Mod, TL) |
     selective_decode1(Mod, TypeLists)];
selective_decode1(_, []) ->
    [];
selective_decode1(_, Bad) ->
    cfg_error({bad_selective_decode,Bad}).

selective_decode2(ModName, {FuncName,TypeList}) ->
    case TypeList of
        [TopType|Types] ->
            case asn1_db:dbget(ModName, TopType) of
                #typedef{typespec=TS} ->
                    TagCommand = get_tag_command(TS, ?CHOSEN),
                    Ret = selective_decode_command(get_components(TS#type.def),
                                                   Types, concat_tags(TagCommand, [])),
                    {FuncName,Ret};
                undefined ->
                    cfg_error({undefined_type,TopType})
            end;
        _ ->
            cfg_error({bad_selective_decode_type_list,TypeList})
    end;
selective_decode2(_, Bad) ->
    cfg_error({bad_selective_decode,Bad}).

selective_decode_command(_, [], Acc) ->
    lists:reverse(Acc);
selective_decode_command([#'ComponentType'{name=Name,typespec=TS}|_],
		    [Name], Acc) ->
    TagCommand = get_tag_command(TS, ?CHOSEN),
    lists:reverse(concat_tags(TagCommand, Acc));
selective_decode_command([#'ComponentType'{name=Name,typespec=TS}|_],
		    [Name|Cs], Acc) ->
    case asn1ct_gen:type(asn1ct_gen:get_inner(TS#type.def)) of
        {primitive,bif} ->
            cfg_error({stepping_into_primitive,[Name|Cs]});
        _ ->
            TagCommand = get_tag_command(TS, ?CHOSEN),
            selective_decode_command(get_components(TS#type.def),
                                     Cs, concat_tags(TagCommand, Acc))
    end;
selective_decode_command([#'ComponentType'{typespec=TS,
                                      prop=Prop}|Comps],
		    [_|_]=Cs, Acc) ->
    TagCommand = case Prop of
                     mandatory ->
                         get_tag_command(TS, ?SKIP);
                     _ ->
                         get_tag_command(TS, ?SKIP_OPTIONAL)
                 end,
    selective_decode_command(Comps, Cs, concat_tags(TagCommand, Acc));
selective_decode_command({'CHOICE',[_|_]=Cs}, [Name|_]=TNL, Acc) ->
    case lists:keyfind(Name, #'ComponentType'.name, Cs) of
        #'ComponentType'{}=C ->
            selective_decode_command([C], TNL, Acc);
        false ->
            cfg_error({undefined_name,Name})
    end;
selective_decode_command({'CHOICE',{Cs1,Cs2}}, TNL, Acc)
  when is_list(Cs1), is_list(Cs2) ->
    selective_decode_command({'CHOICE',Cs1 ++ Cs2}, TNL, Acc);
selective_decode_command(#'Externaltypereference'{module=M,type=C1},
		    TypeNameList, Acc) ->
    #type{def=Def} = get_referenced_type(M, C1),
    selective_decode_command(get_components(Def), TypeNameList, Acc);
selective_decode_command(#type{def=Def}=TS, [C1|Cs], Acc0) ->
    case C1 of
        [N] when is_integer(N), N >= 1 ->
            SkipTags = lists:duplicate(N - 1, ?SKIP),
            Acc = SkipTags ++ Acc0,
            TagCommand = get_tag_command(TS, ?CHOSEN),
            selective_decode_command(Def, Cs, concat_tags(TagCommand, Acc));
        Bad ->
            cfg_error({bad_selective_decode_element,Bad})
    end;
selective_decode_command(_, [Name], _) ->
    cfg_error({undefined_name,Name}).

get_tag_command(#type{tag=[]}, _) ->
    [];
get_tag_command(#type{}, ?SKIP) ->
    [?SKIP];
get_tag_command(#type{tag=[Tag|_]}, ?SKIP_OPTIONAL) ->
    #tag{class=Class,form=Form,number=TagNo} = Tag,
    [{?SKIP_OPTIONAL,
      ?ASN1CT_GEN_BER:encode_tag_val(?ASN1CT_GEN_BER:decode_class(Class),
                                     Form, TagNo)}];
get_tag_command(#type{tag=[Tag]}, Command) ->
    #tag{class=Class,form=Form,number=TagNo} = Tag,
    [{Command,
      ?ASN1CT_GEN_BER:encode_tag_val(?ASN1CT_GEN_BER:decode_class(Class),
                                     Form, TagNo)}];
get_tag_command(T=#type{tag=[Tag|Tags]}, Command) ->
    TC = get_tag_command(T#type{tag=[Tag]}, Command),
    TCs = get_tag_command(T#type{tag=Tags}, Command),
    TC ++ TCs.

concat_tags(Ts, Acc) when is_list(Ts) ->
    lists:reverse(Ts, Acc).

%%%
%%% Common utilities.
%%%

get_components(#'SEQUENCE'{components={Cs1,Cs2}}) when is_list(Cs1), is_list(Cs2) ->
    Cs1 ++ Cs2;
get_components(#'SEQUENCE'{components=Cs}) when is_list(Cs) ->
    Cs;
get_components(#'SET'{components={Cs1,Cs2}}) when is_list(Cs1), is_list(Cs2) ->
    Cs1 ++ Cs2;
get_components(#'SET'{components=Cs}) when is_list(Cs) ->
    Cs;
get_components({'SEQUENCE OF',#type{}=Component})->
    Component;
get_components({'SET OF',#type{}=Component}) ->
    Component;
get_components(Def) ->
    Def.

get_referenced_type(M0, Name0) ->
    #typedef{typespec=TS} = asn1_db:dbget(M0, Name0),
    case TS of
        #type{def=#'Externaltypereference'{module=M,type=Name}} ->
            %% The tags have already been taken care of in the first
            %% reference where they were gathered in a list of tags.
            get_referenced_type(M, Name);
        #type{} ->
            TS
    end.

cfg_error(Error) ->
    throw({structured_error,Error}).
