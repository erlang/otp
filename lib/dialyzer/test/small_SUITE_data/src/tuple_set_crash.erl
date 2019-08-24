%% ====================================================================
%% Program which resulted in an erl_types crash due to incomplete
%% handling of tuple_sets in function inf_tuples_in_sets/4.
%% Reported by Alexey Romanov on 10/10/2010 and fixed 16/10/2010.
%% Stavros Aronis provided a better fix of the issue on 8/11/2010.
%% ====================================================================

-module(tuple_set_crash).
-export([test/5]).

%% ====================================================================

-define(PREPEND_IF_BIT_SET(BitMap, Bit,
			   PatternInBinary, PatternInList,
			   OldRestVar, NewRestVar,
			   OldAccVar, NewAccVar),
	case byteset:contains(Bit, BitMap) of
	    true ->
		<<PatternInBinary, NewRestVar/binary>> = OldRestVar,
		NewAccVar = [PatternInList | OldAccVar];
	    false ->
		NewRestVar = OldRestVar,
		NewAccVar = OldAccVar
	end).

%% ====================================================================

%% Types used in parsing binaries
-define(BITMAP1, 8/integer-big-unsigned).
-define(BYTE, 8/integer-little-unsigned).
-define(WORD, 16/integer-little-unsigned).
-define(DWORD, 32/integer-little-unsigned).
-define(DATE, 16/integer-little-signed).
-define(TIME, 32/float-little-unsigned).
-define(TINY_STRING_M(Var, Size), Size:?BYTE, Var:Size/binary).
-define(SMALL_STRING_M(Var, Size), Size:?WORD, Var:Size/binary).

-type config_change() ::
	{device_properties |
	 video_target |
	 audio_target |
	 video_device |
	 audio_device |
	 video_output |
	 audio_output, [{atom(), any()}]}.

-type message_from_server() ::
	 ok |
	 {error, atom()} |
	 config_change() |
	 {media_item_url_reply, integer(), binary()}.

%% ====================================================================

-spec test(integer(), [integer()], binary(), binary(), binary()) -> {binary(), binary()}.
test(_TargetId, [], _Key, IVT, IVF) ->
    {IVT, IVF};
test(TargetId, [Date | DateTail], Key, IVT, IVF) ->
    PlayListRequest = play_list_request(TargetId, Date),
    {ok, Reply, IVT1, IVF1} = culprit(PlayListRequest, Key, IVT, IVF),
    case Reply of
	{play_list, _Playlist} ->
	    test(TargetId, DateTail, Key, IVT1, IVF1);
	{error, 16#11} ->
	    {IVT1, IVF1} %% we can finish early
    end.

-spec culprit(binary(), binary(), binary(), binary()) ->
	{ok, message_from_server(), binary(), binary()}.
culprit(Message, Key, IVecToServer, IVecFromServer) ->
    {Packet, NewIVecToServer} = message_to_packet(Message, Key, IVecToServer),
    Message = crypto:block_decrypt(aes_cbc128, Key, IVecFromServer, Packet),
    NewIVecFromServer = crypto:next_iv(aes_cbc, Packet),
    ParsedMessage = parse_message(Message),
    {ok, ParsedMessage, NewIVecToServer, NewIVecFromServer}.

%% ====================================================================

-spec play_list_request(integer(), integer()) -> binary().
play_list_request(TargetId, Date) ->
    <<16#06:?WORD, TargetId:?DWORD, Date:?DATE>>.

-spec parse_message(binary()) -> message_from_server().
parse_message(<<MessageID:?WORD, Rest/binary>>) ->
    case MessageID of
	16#00 -> parse_error_code(Rest);
	16#22 -> {device_properties, parse_device_properties(Rest)};
	16#24 -> {video_target_info, parse_video_target_info(Rest)};
	16#25 -> {audio_target_info, parse_audio_target_info(Rest)};
	16#26 -> {video_device_info, parse_av_device_info(Rest)};
	16#27 -> {audio_device_info, parse_av_device_info(Rest)};
	16#28 -> {video_output_info, parse_video_output_info(Rest)};
	16#29 -> {audio_output_info, parse_audio_output_info(Rest)}
    end.

-spec parse_error_code(binary()) -> ok | {error, integer()}.
parse_error_code(<<ErrorCode:?BYTE, _Padding/binary>>) ->
    case ErrorCode of
	0 -> ok;
	_ -> {error, ErrorCode}
    end.

-spec parse_device_properties(binary()) -> config_change().
parse_device_properties(<<BitMap:?BITMAP1, Rest/binary>>) ->
    Acc0 = [],
    ?PREPEND_IF_BIT_SET(BitMap, 0,
			FwVersion:3/binary, {fw_version, FwVersion},
			Rest, Rest1, Acc0, Acc1),
    ?PREPEND_IF_BIT_SET(BitMap, 1,
			?TINY_STRING_M(ControllerName, _S1),
			{controller_name, ControllerName},
			Rest1, Rest2, Acc1, Acc2),
    ?PREPEND_IF_BIT_SET(BitMap, 2,
			?SMALL_STRING_M(ControllerDescription, _S2),
			{controller_description, ControllerDescription},
			Rest2, Rest3, Acc2, Acc3),
    ?PREPEND_IF_BIT_SET(BitMap, 3,
			ControllerStatus:?BYTE,
			{controller_status, ControllerStatus},
			Rest3, _Padding, Acc3, Acc4),
    Acc4.

-spec parse_video_target_info(binary()) -> config_change().
parse_video_target_info(<<TargetId:?DWORD, Status:?BYTE, _Padding/binary>>) ->
    [{target_id, TargetId}, {status, Status}].

-spec parse_audio_target_info(binary()) -> [config_change()].
parse_audio_target_info(<<TargetId:?DWORD, BitMap:?BITMAP1, Rest/binary>>) ->
    Acc0 = [{target_id, TargetId}],
    ?PREPEND_IF_BIT_SET(BitMap, 0,
			Status:?BYTE, {status, Status},
			Rest, Rest1, Acc0, Acc1),
    ?PREPEND_IF_BIT_SET(BitMap, 1,
			MasterVolume:?WORD, {master_volume, MasterVolume},
			Rest1, _Padding, Acc1, Acc2),
    Acc2.

-spec parse_av_device_info(binary()) -> [config_change()].
parse_av_device_info(<<DeviceId:?DWORD, BitMap:?BITMAP1, Rest/binary>>) ->
    Acc0 = [{device_id, DeviceId}],
    ?PREPEND_IF_BIT_SET(BitMap, 0,
			TargetId:?DWORD, {target_id, TargetId},
			Rest, Rest1, Acc0, Acc1),
    ?PREPEND_IF_BIT_SET(BitMap, 1,
			?TINY_STRING_M(Model, _S1), {model, Model},
			Rest1, Rest2, Acc1, Acc2),
    ?PREPEND_IF_BIT_SET(BitMap, 2,
			Address:?BYTE, {address, Address},
			Rest2, Rest3, Acc2, Acc3),
    ?PREPEND_IF_BIT_SET(BitMap, 3,
			Status:?BYTE, {status, Status},
			Rest3, _Padding, Acc3, Acc4),
    Acc4.

-spec parse_video_output_info(binary()) -> [config_change()].
parse_video_output_info(<<Output:?DWORD, BitMap:?BITMAP1, Rest/binary>>) ->
    Acc0 = [{output_id, Output}],
    ?PREPEND_IF_BIT_SET(BitMap, 0,
			DeviceId:?DWORD, {device_id, DeviceId},
			Rest, Rest1, Acc0, Acc1),
    ?PREPEND_IF_BIT_SET(BitMap, 1,
			?TINY_STRING_M(DisplayType, _S1),
			{display_type, DisplayType},
			Rest1, Rest2, Acc1, Acc2),
    ?PREPEND_IF_BIT_SET(BitMap, 2,
			AudioVolume:?WORD,
			{audio_volume, AudioVolume},
			Rest2, _Padding, Acc2, Acc3),
    Acc3.

-spec parse_audio_output_info(binary()) -> [config_change()].
parse_audio_output_info(<<Output:?DWORD, BitMap:?BITMAP1, Rest/binary>>) ->
    Acc0 = [{output_id, Output}],
    ?PREPEND_IF_BIT_SET(BitMap, 0,
			DeviceId:?DWORD, {device_id, DeviceId},
			Rest, Rest1, Acc0, Acc1),
    ?PREPEND_IF_BIT_SET(BitMap, 1,
			AudioVolume:?WORD, {audio_volume, AudioVolume},
			Rest1, Rest2, Acc1, Acc2),
    ?PREPEND_IF_BIT_SET(BitMap, 2,
			Delay:?WORD, {delay, Delay},
			Rest2, _Padding, Acc2, Acc3),
    Acc3.

-spec message_to_packet(binary(), binary(), binary()) -> {binary(), binary()}.
message_to_packet(Message, Key, IVec) ->
    PaddedMessage = pad_pkcs5(Message),
    Packet = crypto:block_encrypt(aes_cbc128, Key, IVec, PaddedMessage),
    TotalSize = byte_size(Packet),
    NewIVec = crypto:next_iv(aes_cbc, Packet),
    {<<TotalSize:?WORD, Packet/binary>>, NewIVec}.

-spec pad_pkcs5(binary()) -> binary().
pad_pkcs5(Message) ->
    Size = byte_size(Message),
    PaddingSize = case Size rem 16 of
		      0 -> 0;
		      Rem -> 16 - Rem
		  end,
    pad_pkcs5(Message, PaddingSize, PaddingSize).

-spec pad_pkcs5(binary(), integer(), integer()) -> binary().
pad_pkcs5(Message, _PaddingSize, 0) ->
    Message;
pad_pkcs5(Message, PaddingSize, PaddingSizeRemaining) ->
    pad_pkcs5(<<Message/binary, PaddingSize:?BYTE>>,
	      PaddingSize, PaddingSizeRemaining - 1).
