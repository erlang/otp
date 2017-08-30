%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Megaco message transformation
%% 
%% Usage:   From a base message file, messages for every codec is 
%%          generated. The file should have the following structure:
%% 
%%          {Codec, Messages}.
%% 
%%          Codec = pretty | compact | ber | per | erlang
%%          Messages = [{Name, binary()}]
%%          Name     = atom
%%          
%%          The function messages/0 is used by the meas and mstone
%%          tools, but messages/1 can also be used if another base
%%          message file is to be used.
%% 
%%          The messages can also be exported to the old format,
%%          e.g. a directory for each of the codec's and the 
%%          each message as a file in those directories.
%%
%%             Pretty text:  pretty
%%             Compact text: compact
%%             Binary ber:   ber
%%             Binary per:   per
%%             Erlang:       erlang
%%          
%% 
%%           <message package>/pretty/<message-files>
%%                             compact/<message-files>
%%                             per/<message-files>
%%                             ber/<message-files>
%%                             erlang/<message-files>
%%         
%%----------------------------------------------------------------------

-module(megaco_codec_transform).

-include_lib("kernel/include/file.hrl").

-export([
	 codecs/0, 
	 default_message_package/0, 
	 messages/0, messages/1, 
	 export_messages/0, export_messages/1
	]).

-define(DEFAULT_MESSAGE_PACKAGE, time_test).
-define(ALL_CODECS, [pretty, compact, per, ber, erlang]).
-define(V3, v3).

codecs() ->
    ?ALL_CODECS.

default_message_package() ->
    ?DEFAULT_MESSAGE_PACKAGE.

messages() ->
    messages(?DEFAULT_MESSAGE_PACKAGE).

messages(MessagePackage) when is_atom(MessagePackage) ->
    %% Try the CWD first, and if that does not work try the installation directory
    case load_messages(".", MessagePackage) of
	{error, _Reason} ->
	    AppLibDir = code:lib_dir(megaco),
	    Dir = filename:join([AppLibDir, examples, meas]),
	    load_messages(Dir, MessagePackage);
	Else ->
	    Else
    end.

load_messages(Dir, MessagePackage) ->
    %% io:format("try loading messages from ~s~n", [Dir]),
    Filename = filename:join([Dir, atom_to_list(MessagePackage) ++ ".msgs"]),
    case file:consult(Filename) of
	{ok, [{Codec, Msgs}]} when is_atom(Codec) andalso is_list(Msgs) ->
	    case lists:member(Codec, ?ALL_CODECS) of
		true ->
		    messages(Codec, Msgs);
		false ->
		    {error, {unknown_codec, Codec}}
	    end;

	{ok, [{BadCodec, Msgs}]} when is_list(Msgs) ->
	    {error, {bad_codec, BadCodec}};

	%% No codec specified, try with pretty
	{ok, [Msgs]} when is_list(Msgs) ->
	    messages(pretty, Msgs);
	
	{ok, Crap} ->
	    {error, {bad_messages, Crap}};
	
	Error ->
	    Error
    end.
    
messages(BaseCodec, Msgs) ->
    OutCodecs = lists:delete(BaseCodec, ?ALL_CODECS),
    transform_messages(BaseCodec, Msgs, OutCodecs).
    

export_messages() ->
    export_messages(?DEFAULT_MESSAGE_PACKAGE).

export_messages(MessagePackage) when is_atom(MessagePackage) ->
    case messages(MessagePackage) of
	TMsgs when is_list(TMsgs) ->
	    (catch export_messages(MessagePackage, TMsgs));
	Error ->
	    Error
    end.

export_messages(MessagePackage, TMsgs) ->
    case file:make_dir(MessagePackage) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	Error ->
	    throw(Error)
    end,
    do_export_messages(MessagePackage, TMsgs).

do_export_messages(_MessagePackage, []) ->
    ok;
do_export_messages(MessagePackage, [{Codec, Msgs} | TMsgs]) ->
    ems(MessagePackage, Codec, Msgs),
    do_export_messages(MessagePackage, TMsgs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transform_messages(BaseCodec, BaseMsgs, OutCodecs) ->
    [{BaseCodec, BaseMsgs} | 
     [{Codec, tms(BaseMsgs, BaseCodec, Codec)} || Codec <- OutCodecs]].
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tms(FromMsgs, FromCodec, ToCodec) ->
    [{Name, tm(FromBin, FromCodec, ToCodec)} || {Name, FromBin} <- FromMsgs].

tm(FromBin, FromCodec, ToCodec) ->
    FromMsg = decode_message(FromCodec, FromBin),
    encode_message(ToCodec, FromMsg).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ems(MessagePackage, Codec, Msgs) ->
    Dir = filename:join([MessagePackage, Codec]),
    case file:make_dir(Dir) of
	ok ->
	    ok;
	{error, eexist} ->
	    ok;
	Error ->
	    throw(Error)
    end,
    Extension = extension_of(Codec),
    F = fun({Name, Bin}) -> em(MessagePackage, Codec, Name, Extension, Bin) end,
    lists:foreach(F, Msgs).
   
em(MessagePackage, Codec, Name, Extension, Bin) ->
    Filename = filename:join([MessagePackage, Codec, atom_to_list(Name) ++ Extension]),
    case file:open(Filename, [raw, binary, write]) of
        {ok, Fd} ->
            case file:write(Fd, Bin) of
                ok ->
                    file:close(Fd),
                    ok;
                {error, Reason} ->
                    S = format("failed writing ~w message ~w (~p bytes): ~p",
                               [Codec, Name, size(Bin), Reason]),
                    file:close(Fd),
                    throw({error, S})
            end;

        {error, Reason} ->
            S = format("failed open file ~s: ~p", [Filename, Reason]),
            throw({error, S})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_message(pretty, BinMsg) ->
    Mod  = megaco_pretty_text_encoder,
    Conf = [{version3,?V3}],
    do_decode(Mod, Conf, BinMsg);
decode_message(compact, BinMsg) ->
    Mod  = megaco_compact_text_encoder,
    Conf = [{version3,?V3}],
    do_decode(Mod, Conf, BinMsg);
decode_message(ber, BinMsg) ->
    Mod  = megaco_ber_encoder,
    Conf = [{version3,?V3}],
    do_decode(Mod, Conf, BinMsg);
decode_message(per, BinMsg) ->
    Mod  = megaco_per_encoder,
    Conf = [{version3,?V3}],
    do_decode(Mod, Conf, BinMsg);
decode_message(erlang, BinMsg) ->
    Mod  = megaco_erl_dist_encoder,
    Conf = [{version3,?V3}],
    do_decode(Mod, Conf, BinMsg).


do_decode(Mod, Conf, Bin) ->
    case (catch Mod:decode_message(Conf, Bin)) of
	{ok, Msg} ->
	    Msg;
	 {error, Reason} ->
	    S = format("decode error: ~p", [Reason]),
	    throw({error, S});
	{'EXIT', Reason} ->
	    S = format("decode exit: ~p", [Reason]),
	    throw({error, S});
	Other ->
	    S = format("unknwon decode result: ~p", [Other]),
	    throw({error, S})
    end.


%% encode_message
%% Note: See note above (decode_message)

encode_message(pretty, Msg) ->
    Mod  = megaco_pretty_text_encoder,
    Conf = [{version3,?V3}],
    do_encode(Mod, Conf, Msg);
encode_message(compact, Msg) ->
    Mod  = megaco_compact_text_encoder,
    Conf = [{version3,?V3}],
    do_encode(Mod, Conf, Msg);
encode_message(ber, Msg) ->
    Mod  = megaco_ber_bin_encoder,
    Conf = [{version3,?V3}],
    do_encode(Mod, Conf, Msg);
encode_message(per, Msg) ->
    Mod  = megaco_per_bin_encoder,
    Conf = [{version3,?V3}],
    do_encode(Mod, Conf, Msg);
encode_message(erlang, Msg) ->
    Mod  = megaco_erl_dist_encoder,
    Conf = [{version3,?V3}],
    do_encode(Mod, Conf, Msg).


do_encode(Mod, Conf, Msg) ->
    case (catch Mod:encode_message(Conf, Msg)) of
	{ok, Bin} ->
	    Bin;
	 {error, Reason} ->
	    S = format("encode error: ~p", [Reason]),
	    throw({error, S});
	{'EXIT', Reason} ->
	    S = format("encode exit: ~p", [Reason]),
	    throw({error, S});
	Other ->
	    S = format("unknwon encode result: ~p", [Other]),
	    throw({error, S})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extension_of(pretty) ->
    ".txt";
extension_of(compact) ->
    ".txt";
extension_of(ber) ->
    ".bin";
extension_of(per) ->
    ".bin";
extension_of(erlang) ->
    ".bin".

%% d(F) ->
%%     d(F, []).
%% d(F, A) ->
%%     io:format(F ++ "~n", A).

format(F, A) ->    
    lists:flatten(io_lib:format(F, A)).
