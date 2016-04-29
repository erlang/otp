%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(zlib_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(error(Format,Args),
	put(test_server_loc,{?MODULE,?LINE}),
	error(Format,Args,?MODULE,?LINE)).

%% Learn erts team how to really write tests ;-)
-define(m(ExpectedRes,Expr),
	fun() ->
		ACtual1 = (catch (Expr)),
		try case ACtual1 of
			ExpectedRes  -> ACtual1
		    end
		catch 
		    error:{case_clause,ACtuAl} ->			
			?error("Not Matching Actual result was:~n ~p ~n",
			       [ACtuAl]),
			ACtuAl
		end
	end()).

-define(BARG, {'EXIT',{badarg,[{zlib,_,_,_}|_]}}).
-define(DATA_ERROR, {'EXIT',{data_error,[{zlib,_,_,_}|_]}}).

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

error(Format, Args, File, Line) ->
    io:format("~p:~p: ERROR: " ++ Format, [File,Line|Args]),
    group_leader() ! {failed, File, Line}.

%% Hopefully I don't need this to get it to work with the testserver..
%%     Fail = #'REASON'{file = filename:basename(File),
%% 		     line = Line,
%% 		     desc = Args},
%%     case global:whereis_name(mnesia_test_case_sup) of
%% 	undefined -> 
%% 	    ignore;
%% 	Pid -> 
%% 	    Pid ! Fail
%% 	    %% 	    global:send(mnesia_test_case_sup, Fail),
%%     end,
%%     log("<>ERROR<>~n" ++ Format, Args, File, Line).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [{group, api}, {group, examples}, {group, func}, smp,
     otp_9981,
     otp_7359].

groups() -> 
    [{api, [],
      [api_open_close, api_deflateInit,
       api_deflateSetDictionary, api_deflateReset,
       api_deflateParams, api_deflate, api_deflateEnd,
       api_inflateInit, api_inflateSetDictionary,
       api_inflateSync, api_inflateReset, api_inflate, api_inflateChunk,
       api_inflateEnd, api_setBufsz, api_getBufsz, api_crc32,
       api_adler32, api_getQSize, api_un_compress, api_un_zip,
       api_g_un_zip]},
     {examples, [], [intro]},
     {func, [],
      [zip_usage, gz_usage, gz_usage2, compress_usage,
       dictionary_usage, large_deflate, crc, adler]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%% Test open/0 and close/1.
api_open_close(Config) when is_list(Config) ->
    Fd1 = zlib:open(),
    Fd2 = zlib:open(),
    ?m(false,Fd1 == Fd2),
    ?m(ok,zlib:close(Fd1)),
    ?m(?BARG, zlib:close(Fd1)),
    ?m(ok,zlib:close(Fd2)),

    %% Make sure that we don't get any EXIT messages if trap_exit is enabled.
    process_flag(trap_exit, true),
    Fd3 = zlib:open(),
    ?m(ok,zlib:close(Fd3)),
    receive
	Any -> ct:fail({unexpected_message,Any})
    after 10 -> ok
    end.

%% Test deflateInit/2 and /6.
api_deflateInit(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(?BARG, zlib:deflateInit(gurka, none)),
    ?m(?BARG, zlib:deflateInit(gurka, gurka)),
    ?m(?BARG, zlib:deflateInit(Z1, gurka)),
    Levels = [none, default, best_speed, best_compression] ++ lists:seq(0,9),
    lists:foreach(fun(Level) ->
			  Z = zlib:open(),
			  ?m(ok, zlib:deflateInit(Z, Level)),
			  ?m(ok,zlib:close(Z))
		  end, Levels),
    %% /6
    ?m(?BARG, zlib:deflateInit(Z1,gurka,deflated,-15,8,default)),

    ?m(?BARG, zlib:deflateInit(Z1,default,undefined,-15,8,default)),

    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,48,8,default)),
    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,-20,8,default)),
    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,-7,8,default)),
    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,7,8,default)),

    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,-15,0,default)),
    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,-15,10,default)),    

    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,-15,8,0)),
    ?m(?BARG, zlib:deflateInit(Z1,default,deflated,-15,8,undefined)),

    lists:foreach(fun(Level) ->
			  Z = zlib:open(),
			  ?m(ok, zlib:deflateInit(Z, Level, deflated, -15, 8, default)),
			  ?m(ok,zlib:close(Z))
		  end, Levels),

    lists:foreach(fun(Wbits) ->
			  Z11 = zlib:open(),
			  ?m(ok, zlib:deflateInit(Z11,best_compression,deflated,
						  Wbits,8,default)),
			  Z12 = zlib:open(),
			  ?m(ok, zlib:deflateInit(Z12,default,deflated,-Wbits,8,default)),
			  ?m(ok,zlib:close(Z11)),
			  ?m(ok,zlib:close(Z12))
		  end, lists:seq(8, 15)),

    lists:foreach(fun(MemLevel) ->
			  Z = zlib:open(),
			  ?m(ok, zlib:deflateInit(Z,default,deflated,-15, 
						  MemLevel,default)),
			  ?m(ok,zlib:close(Z))
		  end, lists:seq(1,8)),

    Strategies = [filtered,huffman_only,rle,default],
    lists:foreach(fun(Strategy) ->
			  Z = zlib:open(),
			  ?m(ok, zlib:deflateInit(Z,best_speed,deflated,-15,8,Strategy)),
			  ?m(ok,zlib:close(Z))
		  end, Strategies),
    ?m(ok, zlib:deflateInit(Z1,default,deflated,-15,8,default)),
    ?m({'EXIT',_}, zlib:deflateInit(Z1,none,deflated,-15,8,default)), %% ?? 
    ?m(ok, zlib:close(Z1)).

%% Test deflateSetDictionary.
api_deflateSetDictionary(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m(Id when is_integer(Id), zlib:deflateSetDictionary(Z1, <<1,1,2,3,4,5,1>>)),
    ?m(Id when is_integer(Id), zlib:deflateSetDictionary(Z1, [1,1,2,3,4,5,1])),
    ?m(?BARG, zlib:deflateSetDictionary(Z1, gurka)),
    ?m(?BARG, zlib:deflateSetDictionary(Z1, 128)),
    ?m(_, zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, none)),
    ?m({'EXIT',{stream_error,_}},zlib:deflateSetDictionary(Z1,<<1,1,2,3,4,5,1>>)),
    ?m(ok, zlib:close(Z1)).

%% Test deflateReset.
api_deflateReset(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m(_, zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, none)),
    ?m(ok, zlib:deflateReset(Z1)),
    ?m(ok, zlib:deflateReset(Z1)),
    %% FIXME how do I make this go wrong??
    ?m(ok, zlib:close(Z1)).

%% Test deflateParams.
api_deflateParams(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m(_, zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, none)),
    ?m(ok, zlib:deflateParams(Z1, best_compression, huffman_only)),
    ?m(_, zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, sync)),
    ?m(ok, zlib:close(Z1)).

%% Test deflate.
api_deflate(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m([B] when is_binary(B), zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, finish)),
    ?m(ok, zlib:deflateReset(Z1)),
    ?m([B] when is_binary(B), zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, finish)),
    ?m(ok, zlib:deflateReset(Z1)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, none)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, sync)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, full)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<>>, finish)),

    ?m(?BARG, zlib:deflate(gurka, <<1,1,1,1,1,1,1,1,1>>, full)),
    ?m(?BARG, zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, asdj)),
    ?m(?BARG, zlib:deflate(Z1, <<1,1,1,1,1,1,1,1,1>>, 198)),
    %% Causes problems ERROR REPORT
    ?m(?BARG, zlib:deflate(Z1, [asdj,asd], none)),

    ?m(ok, zlib:close(Z1)).

%% Test deflateEnd.
api_deflateEnd(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m(ok, zlib:deflateEnd(Z1)),
    ?m({'EXIT', {einval,_}}, zlib:deflateEnd(Z1)), %% ??
    ?m(?BARG, zlib:deflateEnd(gurka)),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<"Kilroy was here">>)),
    ?m({'EXIT', {data_error,_}}, zlib:deflateEnd(Z1)),
    ?m(ok, zlib:deflateInit(Z1, default)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<"Kilroy was here">>)),
    ?m(B when is_list(B), zlib:deflate(Z1, <<"Kilroy was here">>, finish)),
    ?m(ok, zlib:deflateEnd(Z1)),

    ?m(ok, zlib:close(Z1)).

%% Test inflateInit /1 and /2.
api_inflateInit(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(?BARG, zlib:inflateInit(gurka)),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m({'EXIT',{einval,_}}, zlib:inflateInit(Z1, 15)), %% ??
    lists:foreach(fun(Wbits) ->
			  Z11 = zlib:open(),
			  ?m(ok, zlib:inflateInit(Z11,Wbits)),
			  Z12 = zlib:open(),
			  ?m(ok, zlib:inflateInit(Z12,-Wbits)),
			  ?m(ok,zlib:close(Z11)),
			  ?m(ok,zlib:close(Z12))
		  end, lists:seq(8,15)),
    ?m(?BARG, zlib:inflateInit(gurka, -15)),
    ?m(?BARG, zlib:inflateInit(Z1, 7)),
    ?m(?BARG, zlib:inflateInit(Z1, -7)),
    ?m(?BARG, zlib:inflateInit(Z1, 48)),
    ?m(?BARG, zlib:inflateInit(Z1, -16)),
    ?m(ok, zlib:close(Z1)).

%% Test inflateSetDictionary.
api_inflateSetDictionary(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m(?BARG, zlib:inflateSetDictionary(gurka,<<1,1,1,1,1>>)),
    ?m(?BARG, zlib:inflateSetDictionary(Z1,102)),
    ?m(?BARG, zlib:inflateSetDictionary(Z1,gurka)),
    Dict = <<1,1,1,1,1>>,
    ?m({'EXIT',{stream_error,_}}, zlib:inflateSetDictionary(Z1,Dict)),
    ?m(ok, zlib:close(Z1)).

%% Test inflateSync.
api_inflateSync(Config) when is_list(Config) ->
    {skip,"inflateSync/1 sucks"}.
%%     Z1 = zlib:open(),
%%     ?m(ok, zlib:deflateInit(Z1)),
%%     B1list0 = zlib:deflate(Z1, "gurkan gurra ger galna tunnor", full),
%%     B2 = zlib:deflate(Z1, "grodan boll", finish),
%%     io:format("~p\n", [B1list0]),
%%     io:format("~p\n", [B2]),
%%     ?m(ok, zlib:deflateEnd(Z1)),
%%     B1 = clobber(14, list_to_binary(B1list0)),
%%     Compressed = list_to_binary([B1,B2]),
%%     io:format("~p\n", [Compressed]),

%%     ?m(ok, zlib:inflateInit(Z1)),
%%     ?m(?BARG, zlib:inflateSync(gurka)),
%%     ?m({'EXIT',{data_error,_}}, zlib:inflate(Z1, Compressed)),
%%     ?m(ok, zlib:inflateSync(Z1)),
%%     Ubs = zlib:inflate(Z1, []),
%%     <<"grodan boll">> = list_to_binary(Ubs),
%%     ?m(ok, zlib:close(Z1)).

clobber(N, Bin) when is_binary(Bin) ->
    T = list_to_tuple(binary_to_list(Bin)),
    Byte = case element(N, T) of
	       255 -> 254;
	       B -> B+1
	   end,
    list_to_binary(tuple_to_list(setelement(N, T, Byte))).

%% Test inflateReset.
api_inflateReset(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m(?BARG, zlib:inflateReset(gurka)),
    ?m(ok, zlib:inflateReset(Z1)),
    ?m(ok, zlib:close(Z1)).

%% Test inflate.
api_inflate(Config) when is_list(Config) ->
    Data = [<<1,2,2,3,3,3,4,4,4,4>>],
    Compressed = zlib:compress(Data),
    Z1 = zlib:open(),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m([], zlib:inflate(Z1, <<>>)),
    ?m(Data, zlib:inflate(Z1, Compressed)),
    ?m(ok, zlib:inflateEnd(Z1)),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m(Data, zlib:inflate(Z1, Compressed)),
    ?m(?BARG, zlib:inflate(gurka, Compressed)),
    ?m(?BARG, zlib:inflate(Z1, 4384)),
    ?m(?BARG, zlib:inflate(Z1, [atom_list])),    
    ?m(ok, zlib:inflateEnd(Z1)),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m({'EXIT',{data_error,_}}, zlib:inflate(Z1, <<2,1,2,1,2>>)), 
    ?m(ok, zlib:close(Z1)).

%% Test inflateChunk.
api_inflateChunk(Config) when is_list(Config) ->
    ChunkSize = 1024,
    Data = << <<(I rem 150)>> || I <- lists:seq(1, 3 * ChunkSize) >>,
    Part1 = binary:part(Data, 0, ChunkSize),
    Part2 = binary:part(Data, ChunkSize, ChunkSize),
    Part3 = binary:part(Data, ChunkSize * 2, ChunkSize),
    Compressed = zlib:compress(Data),
    Z1 = zlib:open(),
    zlib:setBufSize(Z1, ChunkSize),
    ?m(ok, zlib:inflateInit(Z1)),
    ?m([], zlib:inflateChunk(Z1, <<>>)),
    ?m({more, Part1}, zlib:inflateChunk(Z1, Compressed)),
    ?m({more, Part2}, zlib:inflateChunk(Z1)),
    ?m(Part3, zlib:inflateChunk(Z1)),
    ?m(ok, zlib:inflateEnd(Z1)),

    ?m(ok, zlib:inflateInit(Z1)),
    ?m({more, Part1}, zlib:inflateChunk(Z1, Compressed)),

    ?m(ok, zlib:inflateReset(Z1)),

    zlib:setBufSize(Z1, size(Data)),
    ?m(Data, zlib:inflateChunk(Z1, Compressed)),
    ?m(ok, zlib:inflateEnd(Z1)),

    ?m(ok, zlib:inflateInit(Z1)),
    ?m(?BARG, zlib:inflateChunk(gurka, Compressed)),
    ?m(?BARG, zlib:inflateChunk(Z1, 4384)),
    ?m({'EXIT',{data_error,_}}, zlib:inflateEnd(Z1)),
    ?m(ok, zlib:close(Z1)).

%% Test inflateEnd.
api_inflateEnd(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m({'EXIT',{einval,_}}, zlib:inflateEnd(Z1)), 
    ?m(ok, zlib:inflateInit(Z1)),
    ?m(?BARG, zlib:inflateEnd(gurka)),
    ?m({'EXIT',{data_error,_}}, zlib:inflateEnd(Z1)),
    ?m({'EXIT',{einval,_}}, zlib:inflateEnd(Z1)), 
    ?m(ok, zlib:inflateInit(Z1)),
    ?m(B when is_list(B), zlib:inflate(Z1, zlib:compress("abc"))),
    ?m(ok, zlib:inflateEnd(Z1)),
    ?m(ok, zlib:close(Z1)).

%% Test getBufsz.
api_getBufsz(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(Val when is_integer(Val), zlib:getBufSize(Z1)),
    ?m(?BARG, zlib:getBufSize(gurka)),
    ?m(ok, zlib:close(Z1)).

%% Test setBufsz.
api_setBufsz(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(?BARG, zlib:setBufSize(Z1, gurka)),
    ?m(?BARG, zlib:setBufSize(gurka, 1232330)),
    Sz = ?m( Val when is_integer(Val), zlib:getBufSize(Z1)),
    ?m(ok, zlib:setBufSize(Z1, Sz*2)),
    DSz = Sz*2,
    ?m(DSz, zlib:getBufSize(Z1)),
    ?m(ok, zlib:close(Z1)).

%%% Debug function ??
%% Test getQSize.
api_getQSize(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    Q = ?m(Val when is_integer(Val), zlib:getQSize(Z1)),
    io:format("QSize ~p ~n", [Q]),
    ?m(?BARG, zlib:getQSize(gurka)),
    ?m(ok, zlib:close(Z1)).

%% Test crc32.
api_crc32(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1,best_speed,deflated,-15,8,default)),
    Bin = <<1,1,1,1,1,1,1,1,1>>,
    Compressed1 = ?m(_, zlib:deflate(Z1, Bin, none)),
    Compressed2 = ?m(_, zlib:deflate(Z1, <<>>, finish)),
    Compressed = list_to_binary(Compressed1 ++ Compressed2),
    CRC1 = ?m( CRC1 when is_integer(CRC1), zlib:crc32(Z1)),
    ?m(CRC1 when is_integer(CRC1), zlib:crc32(Z1,Bin)),
    ?m(CRC1 when is_integer(CRC1), zlib:crc32(Z1,binary_to_list(Bin))),
    ?m(CRC2 when is_integer(CRC2), zlib:crc32(Z1,Compressed)),
    CRC2 = ?m(CRC2 when is_integer(CRC2), zlib:crc32(Z1,0,Compressed)),
    ?m(CRC3 when CRC2 /= CRC3, zlib:crc32(Z1,234,Compressed)),
    ?m(?BARG, zlib:crc32(gurka)),
    ?m(?BARG, zlib:crc32(Z1, not_a_binary)),
    ?m(?BARG, zlib:crc32(gurka, <<1,1,2,4,4>>)),
    ?m(?BARG, zlib:crc32(Z1, 2298929, not_a_binary)),
    ?m(?BARG, zlib:crc32(Z1, not_an_int, <<123,123,123,35,231>>)),
    ?m(?BARG, zlib:crc32_combine(Z1, not_an_int, 123123, 123)),
    ?m(?BARG, zlib:crc32_combine(Z1, noint, 123123, 123)),
    ?m(?BARG, zlib:crc32_combine(Z1, 123123, noint, 123)),
    ?m(?BARG, zlib:crc32_combine(Z1, 123123, 123, noint)),
    ?m(ok, zlib:deflateEnd(Z1)),
    ?m(ok, zlib:close(Z1)).    

%% Test adler32.
api_adler32(Config) when is_list(Config) ->
    Z1 = zlib:open(),
    ?m(ok, zlib:deflateInit(Z1,best_speed,deflated,-15,8,default)),
    Bin = <<1,1,1,1,1,1,1,1,1>>,
    Compressed1 = ?m(_, zlib:deflate(Z1, Bin, none)),
    Compressed2 = ?m(_, zlib:deflate(Z1, <<>>, finish)),
    Compressed = list_to_binary(Compressed1 ++ Compressed2),
    ?m(ADLER1 when is_integer(ADLER1), zlib:adler32(Z1,Bin)),
    ?m(ADLER1 when is_integer(ADLER1), zlib:adler32(Z1,binary_to_list(Bin))),
    ADLER2 = ?m(ADLER2 when is_integer(ADLER2), zlib:adler32(Z1,Compressed)),
    ?m(ADLER2 when is_integer(ADLER2), zlib:adler32(Z1,1,Compressed)),
    ?m(ADLER3 when ADLER2 /= ADLER3, zlib:adler32(Z1,234,Compressed)),
    ?m(?BARG, zlib:adler32(Z1, not_a_binary)),
    ?m(?BARG, zlib:adler32(gurka, <<1,1,2,4,4>>)),
    ?m(?BARG, zlib:adler32(Z1, 2298929, not_a_binary)),
    ?m(?BARG, zlib:adler32(Z1, not_an_int, <<123,123,123,35,231>>)),
    ?m(?BARG, zlib:adler32_combine(Z1, noint, 123123, 123)),
    ?m(?BARG, zlib:adler32_combine(Z1, 123123, noint, 123)),
    ?m(?BARG, zlib:adler32_combine(Z1, 123123, 123, noint)),
    ?m(ok, zlib:deflateEnd(Z1)),
    ?m(ok, zlib:close(Z1)).    

%% Test compress.
api_un_compress(Config) when is_list(Config) ->
    ?m(?BARG,zlib:compress(not_a_binary)),
    Bin = <<1,11,1,23,45>>,
    Comp = zlib:compress(Bin),
    ?m(?BARG,zlib:uncompress(not_a_binary)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<171,171,171,171,171>>)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<>>)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<120>>)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<120,156>>)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<120,156,3>>)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<120,156,3,0>>)),
    ?m({'EXIT',{data_error,_}}, zlib:uncompress(<<0,156,3,0,0,0,0,1>>)),
    ?m(Bin, zlib:uncompress(binary_to_list(Comp))),
    ?m(Bin, zlib:uncompress(Comp)).

%% Test zip.
api_un_zip(Config) when is_list(Config) ->
    ?m(?BARG,zlib:zip(not_a_binary)),
    Bin = <<1,11,1,23,45>>,
    Comp = zlib:zip(Bin),
    ?m(Comp, zlib:zip(binary_to_list(Bin))),
    ?m(?BARG,zlib:unzip(not_a_binary)),
    ?m({'EXIT',{data_error,_}}, zlib:unzip(<<171,171,171,171,171>>)),
    ?m({'EXIT',{data_error,_}}, zlib:unzip(<<>>)),
    ?m(Bin, zlib:unzip(Comp)),
    ?m(Bin, zlib:unzip(binary_to_list(Comp))),

    %% OTP-6396
    B = <<131,104,19,100,0,13,99,95,99,105,100,95,99,115,103,115,110,95,50,97,1,107,0,4,208,161,246,29,107,0,3,237,166,224,107,0,6,66,240,153,0,2,10,1,0,8,97,116,116,97,99,104,101,100,104,2,100,0,22,117,112,100,97,116,101,95,112,100,112,95,99,111,110,116,101,120,116,95,114,101,113,107,0,114,69,3,12,1,11,97,31,113,150,64,104,132,61,64,104,12,3,197,31,113,150,64,104,132,61,64,104,12,1,11,97,31,115,150,64,104,116,73,64,104,0,0,0,0,0,0,65,149,16,61,65,149,16,61,1,241,33,4,5,0,33,4,4,10,6,10,181,4,10,6,10,181,38,15,99,111,109,109,97,110,100,1,114,45,97,112,110,45,49,3,99,111,109,5,109,110,99,57,57,6,109,99,99,50,52,48,4,103,112,114,115,8,0,104,2,104,2,100,0,8,97,99,116,105,118,97,116,101,104,23,100,0,11,112,100,112,95,99,111,110,116,1,120,116,100,0,7,112,114,105,109,97,114,121,97,1,100,0,9,117,110,100,101,102,105,110,101,100,97,1,97,4,97,4,97,7,100,0,9,117,110,100,101,102,105,110,101,100,100,0,9,117,110,100,101,102,105,110,10100,100,0,9,117,110,100,101,102,105,110,101,100,100,0,5,102,97,108,115,101,100,0,9,117,110,100,101,102,105,110,101,100,100,0,9,117,110,100,101,102,105,110,101,100,100,0,9,117,110,100,101,102,105,1,101,100,97,0,100,0,9,117,110,100,101,102,105,110,101,100,107,0,4,16,0,1,144,107,0,4,61,139,186,181,107,0,4,10,8,201,49,100,0,9,117,110,100,101,102,105,110,101,100,100,0,9,117,110,100,101,102,105,0,101,100,100,0,9,117,110,100,101,102,105,110,101,100,104,2,104,3,98,0,0,7,214,97,11,97,20,104,3,97,17,97,16,97,21,106,108,0,0,0,3,104,2,97,1,104,2,104,3,98,0,0,7,214,97,11,97,20,104,3,97,17,97,167,20,104,2,97,4,104,2,104,3,98,0,0,7,214,97,11,97,20,104,3,97,17,97,16,97,21,104,2,97,10,104,2,104,3,98,0,0,7,214,97,11,97,20,104,3,97,17,97,16,97,26,106,100,0,5,118,101,114,57,57,100,0,9,117,110,0,101,102,105,110,101,100,107,0,2,0,244,107,0,4,10,6,102,195,107,0,4,10,6,102,195,100,0,9,117,110,100,101,102,105,110,101,100,100,0,9,117,110,100,101,102,105,110,101,100,107,0,125,248,143,0,203,25115,157,116,65,185,65,172,55,87,164,88,225,50,203,251,115,157,116,65,185,65,172,55,87,164,88,225,50,0,0,82,153,50,0,200,98,87,148,237,193,185,65,149,167,69,144,14,16,153,50,3,81,70,94,13,109,193,1,120,5,181,113,198,118,50,3,81,70,94,13,109,193,185,120,5,181,113,198,118,153,3,81,70,94,13,109,193,185,120,5,181,113,198,118,153,50,16,1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,113,92,2,119,128,0,0,108,0,0,1,107,0,114,69,3,12,1,11,97,31,113,150,64,104,132,61,64,104,12,3,11,97,31,113,150,64,104,132,61,64,104,12,1,11,97,31,115,150,64,104,116,73,64,104,0,0,0,0,0,0,65,149,16,61,65,149,16,61,1,241,33,4,0,33,4,4,10,6,10,181,4,10,6,10,181,38,15,99,111,109,109,97,110,100,101,114,45,97,112,110,45,49,3,99,111,109,5,109,110,99,57,57,6,109,99,99,50,52,48,4,103,112,114,115,8,0,106>>,
    Z = zlib:zip(B),
    ?m(B, zlib:unzip(Z)).

%% Test gunzip.
api_g_un_zip(Config) when is_list(Config) ->
    ?m(?BARG,zlib:gzip(not_a_binary)),
    Bin = <<1,11,1,23,45>>,
    Comp = zlib:gzip(Bin),
    ?m(Comp, zlib:gzip(binary_to_list(Bin))),
    ?m(?BARG, zlib:gunzip(not_a_binary)),
    ?m(?DATA_ERROR, zlib:gunzip(<<171,171,171,171,171>>)),
    ?m(?DATA_ERROR, zlib:gunzip(<<>>)),
    ?m(Bin, zlib:gunzip(Comp)),
    ?m(Bin, zlib:gunzip(binary_to_list(Comp))),

    %% Bad CRC; bad length.
    BadCrc = bad_crc_data(),
    ?m({'EXIT',{data_error,_}},(catch zlib:gunzip(BadCrc))),
    BadLen = bad_len_data(),
    ?m({'EXIT',{data_error,_}},(catch zlib:gunzip(BadLen))),
    ok.

bad_crc_data() ->
    %% zlib:zip(<<42>>), one byte changed.
    <<31,139,8,0,0,0,0,0,0,3,211,2,0,91,39,185,9,1,0,0,0>>.

bad_len_data() ->
    %% zlib:zip(<<42>>), one byte changed.
    <<31,139,8,0,0,0,0,0,0,3,211,2,0,91,38,185,9,2,0,0,0>>.


intro(Config) when is_list(Config) ->
    D = <<"This is a binary">>,
    [put({ex, N}, <<"This is a binary">>) || N <- [0,1,2,3,4]],
    put({ex, 5}, end_of_data),
    put(ex,0),
    Read = fun() ->
		   N = get(ex),
		   put(ex,N+1),
		   get({ex,N})
	   end,

    Z = zlib:open(),
    ok = zlib:deflateInit(Z,default),

    Compress = fun(end_of_data, _Cont) -> [];
		  (Data, Cont) ->
		       [zlib:deflate(Z, Data)|Cont(Read(),Cont)]
	       end,
    Compressed = Compress(Read(),Compress),
    Last = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    zlib:close(Z),
    Res = list_to_binary([Compressed|Last]),
    Orig = list_to_binary(lists:duplicate(5, D)),
    ?m(Orig, zlib:uncompress(Res)).


%% Test deflate large file, which had a bug reported on erlang-bugs.
large_deflate(Config) when is_list(Config) ->
    large_deflate_do().
large_deflate_do() ->
    Z = zlib:open(),
    Plain = rand_bytes(zlib:getBufSize(Z)*5),
    ok = zlib:deflateInit(Z),
    _ZlibHeader = zlib:deflate(Z, [], full),
    Deflated = zlib:deflate(Z, Plain, full),
    ?m(ok, zlib:close(Z)),
    ?m(Plain, zlib:unzip(list_to_binary([Deflated, 3, 0]))).

rand_bytes(Sz) ->
    L = <<8,2,3,6,1,2,3,2,3,4,8,7,3,7,2,3,4,7,5,8,9,3>>,
    rand_bytes(erlang:md5(L),Sz).

rand_bytes(Bin, Sz) when byte_size(Bin) >= Sz ->
    <<Res:Sz/binary, _/binary>> = Bin,
    Res;
rand_bytes(Bin, Sz) ->
    rand_bytes(<<(erlang:md5(Bin))/binary, Bin/binary>>, Sz).


%% Test a standard compressed zip file.
zip_usage(Config) when is_list(Config) ->
    zip_usage(zip_usage({get_arg,Config}));
zip_usage({get_arg,Config}) ->
    Out = conf(data_dir,Config),
    {ok,ZIP} = file:read_file(filename:join(Out,"zipdoc.zip")),
    {ok,ORIG} = file:read_file(filename:join(Out,"zipdoc")),
    {run,ZIP,ORIG};
zip_usage({run,ZIP,ORIG}) ->    
    <<_:14/binary, CRC:32/little,
      CompSz:32/little, UnCompSz:32/little,_:31/binary,
      Compressed:CompSz/binary, _/binary>> = ZIP,

    %%io:format("CRC ~p CSz ~p UnCSz ~p ~n", [CRC,CompSz,UnCompSz]),
    Split = split_bin(Compressed,[]),
    Z = zlib:open(),

    ?m(ok, zlib:inflateInit(Z, -15)),
    Bs = [zlib:inflate(Z, Part) || Part <- Split],
    UC0 = list_to_binary(Bs),
    ?m(UnCompSz, byte_size(UC0)),
    ?m(CRC, zlib:crc32(Z)),
    ?m(true, zlib:crc32(Z,UC0) == zlib:crc32(Z,ORIG)),
    ?m(ok, zlib:inflateEnd(Z)),

    UC1 = zlib:unzip(Compressed),
    ?m(UnCompSz, byte_size(UC1)),
    ?m(true, zlib:crc32(Z,UC1) == zlib:crc32(Z,ORIG)),

    ?m(ok, zlib:inflateInit(Z, -15)),
    UC2 = zlib:inflate(Z, Compressed),
    ?m(UnCompSz, byte_size(list_to_binary(UC2))),
    ?m(CRC, zlib:crc32(Z)),
    ?m(true, zlib:crc32(Z,UC2) == zlib:crc32(Z,ORIG)),
    ?m(ok, zlib:inflateEnd(Z)),

    ?m(ok, zlib:inflateInit(Z, -15)),
    UC3 = zlib:inflate(Z, Split), % Test multivec.
    ?m(UnCompSz, byte_size(list_to_binary(UC3))),
    ?m(true, zlib:crc32(Z,UC3) == zlib:crc32(Z,ORIG)),
    ?m(CRC, zlib:crc32(Z)),
    ?m(ok, zlib:inflateEnd(Z)),

    ?m(ok, zlib:inflateInit(Z, -15)),
    ?m(ok, zlib:setBufSize(Z, UnCompSz *2)),
    UC4 = zlib:inflate(Z, Compressed),
    ?m(UnCompSz, byte_size(list_to_binary(UC4))),
    ?m(CRC, zlib:crc32(Z)),
    ?m(CRC, zlib:crc32(Z,UC4)),
    ?m(true, zlib:crc32(Z,UC4) == zlib:crc32(Z,ORIG)),
    ?m(ok, zlib:inflateEnd(Z)),

    C1 = zlib:zip(ORIG),
    UC5 =  zlib:unzip(C1),
    ?m(CRC, zlib:crc32(Z,UC5)),
    ?m(true,zlib:crc32(Z,UC5) == zlib:crc32(Z,ORIG)),

    ?m(ok, zlib:deflateInit(Z, default, deflated, -15, 8, default)),
    C2 = zlib:deflate(Z, ORIG, finish),
    ?m(true, C1 == list_to_binary(C2)),
    ?m(ok, zlib:deflateEnd(Z)),

    ?m(ok, zlib:deflateInit(Z, none, deflated, -15, 8, filtered)),
    ?m(ok, zlib:deflateParams(Z, default, default)),
    C3 = zlib:deflate(Z, ORIG, finish),
    ?m(true, C1 == list_to_binary(C3)),
    ?m(ok, zlib:deflateEnd(Z)),

    ok = zlib:close(Z),
    ok.

%% Test a standard compressed gzipped file.
gz_usage(Config) when is_list(Config) ->
    gz_usage(gz_usage({get_arg,Config}));
gz_usage({get_arg,Config}) ->
    Out = conf(data_dir,Config),
    {ok,GZIP} = file:read_file(filename:join(Out,"zipdoc.1.gz")),
    {ok,ORIG} = file:read_file(filename:join(Out,"zipdoc")),
    {ok,GZIP2} = file:read_file(filename:join(Out,"zipdoc.txt.gz")),
    {run,GZIP,ORIG,GZIP2};    
gz_usage({run,GZIP,ORIG,GZIP2}) ->
    Z = zlib:open(),
    UC1 = zlib:gunzip(GZIP),
    ?m(true,zlib:crc32(Z,UC1) == zlib:crc32(Z,ORIG)),
    UC3 = zlib:gunzip(GZIP2),
    ?m(true,zlib:crc32(Z,UC3) == zlib:crc32(Z,ORIG)),
    Compressed = zlib:gzip(ORIG),
    UC5 = zlib:gunzip(Compressed),
    ?m(true,zlib:crc32(Z,UC5) == zlib:crc32(Z,ORIG)),
    ok = zlib:close(Z).

%% Test more of a standard compressed gzipped file.
gz_usage2(Config) ->
    case os:find_executable("gzip") of
	Name when is_list(Name) ->
	    Z = zlib:open(),
	    Out = conf(data_dir,Config),
	    {ok,ORIG} = file:read_file(filename:join(Out,"zipdoc")),
	    Compressed = zlib:gzip(ORIG),
	    GzOutFile = filename:join(Out,"out.gz"),
	    OutFile = filename:join(Out,"out.txt"),
	    ?m(ok, file:write_file(GzOutFile,Compressed)),
	    os:cmd("gzip -c -d " ++ GzOutFile ++ " > " ++ OutFile),
	    case file:read_file(OutFile) of
		{ok,ExtDecompressed} ->
		    ?m(true, 
		       zlib:crc32(Z,ExtDecompressed) == zlib:crc32(Z,ORIG));
		Error ->
		    io:format("Couldn't test external decompressor ~p\n", 
			      [Error])
	    end,
	    ok = zlib:close(Z),
	    ok;
	false ->
	    {skipped,"No gzip in path"}
    end.



%% Test that (de)compress funcs work with standard tools, for example
%% a chunk from a png file.
compress_usage(Config) when is_list(Config) ->
    compress_usage(compress_usage({get_arg,Config}));
compress_usage({get_arg,Config}) ->
    Out = conf(data_dir,Config),
    {ok,C1} = file:read_file(filename:join(Out,"png-compressed.zlib")),
    {run,C1};
compress_usage({run,C1}) ->
    Z = zlib:open(),
    %% See that we can uncompress a file generated with external prog.
    UC1 = zlib:uncompress(C1),
    %% Check that the crc are correct.
    ?m(4125865008,zlib:crc32(Z,UC1)),
    C2 = zlib:compress(UC1),
    UC2 = zlib:uncompress(C2),
    %% Check that the crc are correct.
    ?m(4125865008,zlib:crc32(Z,UC2)),

    ok = zlib:close(Z),

    D = [<<"We tests some partial">>,
	 <<"data, sent over">>,
	 <<"the stream">>,
	 <<"we check that we can unpack">>,
	 <<"every message we get">>],

    ZC = zlib:open(),
    ZU = zlib:open(),
    Test = fun(finish, {_,Tot}) ->
		   Compressed = zlib:deflate(ZC, <<>>, finish),
		   Data = zlib:inflate(ZU, Compressed),
		   [Tot|Data];
	      (Data, {Op,Tot}) ->
		   Compressed = zlib:deflate(ZC, Data, Op),
		   Res1 = ?m([Data],zlib:inflate(ZU, Compressed)),
		   {Op, [Tot|Res1]}
	   end,
    zlib:deflateInit(ZC),
    zlib:inflateInit(ZU),
    T1 = lists:foldl(Test,{sync,[]},D++[finish]),
    ?m(true, list_to_binary(D) == list_to_binary(T1)),
    zlib:deflateEnd(ZC),
    zlib:inflateEnd(ZU),

    zlib:deflateInit(ZC),
    zlib:inflateInit(ZU),
    T2 = lists:foldl(Test,{full,[]},D++[finish]),
    ?m(true, list_to_binary(D) == list_to_binary(T2)),
    zlib:deflateEnd(ZC),
    zlib:inflateEnd(ZU),

    ok = zlib:close(ZC),
    ok = zlib:close(ZU).


%% Check that crc works as expected.
crc(Config) when is_list(Config) ->
    crc(crc({get_arg,Config}));
crc({get_arg,Config}) ->
    Out = conf(data_dir,Config),
    {ok,C1} = file:read_file(filename:join(Out,"zipdoc")),
    {run,C1};
crc({run,C1}) ->
    Z = zlib:open(),
    Crc = zlib:crc32(Z, C1),
    Bins = split_bin(C1,[]),
    %%io:format("Length ~p ~p ~n", [length(Bins), [size(Bin) || Bin <- Bins]]),
    Last = lists:last(Bins),
    SCrc = lists:foldl(fun(Bin,Crc0) ->  
			       Crc1 = zlib:crc32(Z, Crc0, Bin),
			       ?m(false, Crc == Crc1 andalso Bin /= Last),
			       Crc1
		       end, 0, Bins),
    ?m(Crc,SCrc),
    [First|Rest] = Bins,
    Combine = fun(Bin, CS1) ->
		      CS2 = zlib:crc32(Z, Bin),
		      S2 = byte_size(Bin),
		      zlib:crc32_combine(Z,CS1,CS2,S2)
	      end,
    Comb = lists:foldl(Combine, zlib:crc32(Z, First), Rest),
    ?m(Crc,Comb),
    ok = zlib:close(Z).

%% Check that adler works as expected.
adler(Config) when is_list(Config) ->
    adler(adler({get_arg,Config}));
adler({get_arg,Config}) ->
    Out = conf(data_dir,Config),
    File1 = filename:join(Out,"zipdoc"),
    {ok,C1} = file:read_file(File1),
    {run,C1};
adler({run,C1}) ->
    Z = zlib:open(),
    ?m(1, zlib:adler32(Z,<<>>)),
    Crc = zlib:adler32(Z, C1),
    Bins = split_bin(C1,[]),
    Last = lists:last(Bins),
    SCrc = lists:foldl(fun(Bin,Crc0) ->  
			       Crc1 = zlib:adler32(Z, Crc0, Bin),
			       ?m(false, Crc == Crc1 andalso Bin /= Last),
			       Crc1
		       end, zlib:adler32(Z,<<>>), Bins),
    ?m(Crc,SCrc),
    [First|Rest] = Bins,
    Combine = fun(Bin, CS1) ->
		      CS2 = zlib:adler32(Z, Bin),
		      S2 = byte_size(Bin),
		      zlib:adler32_combine(Z,CS1,CS2,S2)
	      end,
    Comb = lists:foldl(Combine, zlib:adler32(Z, First), Rest),
    ?m(Crc,Comb),
    ok = zlib:close(Z).

%% Test dictionary usage.
dictionary_usage(Config) when is_list(Config) ->
    dictionary_usage(dictionary_usage({get_arg,Config}));
dictionary_usage({get_arg,_Config}) ->
    {run}; % no args
dictionary_usage({run}) ->
    Z1 = zlib:open(),
    Dict = <<"Anka">>,
    Data = <<"Kalle Anka">>,
    ?m(ok, zlib:deflateInit(Z1)),
    DictID = zlib:deflateSetDictionary(Z1, Dict),
    %% io:format("DictID = ~p\n", [DictID]),
    B1 = zlib:deflate(Z1, Data),
    B2 = zlib:deflate(Z1, <<>>, finish),
    ?m(ok, zlib:deflateEnd(Z1)),
    ?m(ok, zlib:close(Z1)),
    Compressed = list_to_binary([B1,B2]),
    %% io:format("~p\n", [Compressed]),

    %% Now uncompress.
    Z2 = zlib:open(),
    ?m(ok, zlib:inflateInit(Z2)),
    {'EXIT',{{need_dictionary,DictID},_}} = (catch zlib:inflate(Z2, Compressed)),
    ?m(ok, zlib:inflateSetDictionary(Z2, Dict)),
    ?m(ok, zlib:inflateSetDictionary(Z2, binary_to_list(Dict))),
    Uncompressed = ?m(B when is_list(B), zlib:inflate(Z2, [])),
    ?m(ok, zlib:inflateEnd(Z2)),
    ?m(ok, zlib:close(Z2)),    
    ?m(Data, list_to_binary(Uncompressed)).

split_bin(<<Part:1997/binary,Rest/binary>>, Acc) ->
    split_bin(Rest, [Part|Acc]);
split_bin(Last,Acc) ->
    lists:reverse([Last|Acc]).


%% Check concurrent access to zlib driver.
smp(Config) ->
    case erlang:system_info(smp_support) of
	true ->
	    NumOfProcs = lists:min([8,erlang:system_info(schedulers)]),
	    io:format("smp starting ~p workers\n",[NumOfProcs]),

	    %% Tests to run in parallel.
	    Funcs = [zip_usage, gz_usage, compress_usage, dictionary_usage,
		     crc, adler],

	    %% We get all function arguments here to avoid repeated parallel
	    %% file read access.
	    FnAList = lists:map(fun(F) -> {F,?MODULE:F({get_arg,Config})}
				end, Funcs),	    

	    Pids = [spawn_link(?MODULE, worker, [rand:uniform(9999),
						 list_to_tuple(FnAList),
						 self()])
		    || _ <- lists:seq(1,NumOfProcs)],
	    wait_pids(Pids);

	false ->
	    {skipped,"No smp support"}
    end.


worker(Seed, FnATpl, Parent) ->
    io:format("smp worker ~p, seed=~p~n",[self(),Seed]),
    rand:seed(exsplus, {Seed,Seed,Seed}),
    worker_loop(100, FnATpl),
    Parent ! self().

worker_loop(0, _FnATpl) ->
    large_deflate_do(), % the time consuming one as finale
    ok;
worker_loop(N, FnATpl) ->
    {F,A} = element(rand:uniform(tuple_size(FnATpl)), FnATpl),
    ?MODULE:F(A),
    worker_loop(N-1, FnATpl).

wait_pids([]) -> 
    ok;
wait_pids(Pids) ->
    receive
	Pid ->
	    true = lists:member(Pid,Pids),
	    Others = lists:delete(Pid,Pids),
	    io:format("wait_pid got ~p, still waiting for ~p\n",[Pid,Others]),
	    wait_pids(Others)
    end.


%% Deflate/inflate data with size close to multiple of internal buffer size.
otp_7359(_Config) ->
    %% Find compressed size
    ZTry = zlib:open(),
    ok = zlib:deflateInit(ZTry),
    ISize = zlib:getBufSize(ZTry),
    IData = list_to_binary([Byte band 255 || Byte <- lists:seq(1,ISize)]),
    ISize = byte_size(IData),

    DSize = iolist_size(zlib:deflate(ZTry, IData, sync)),
    zlib:close(ZTry),

    io:format("Deflated try ~p -> ~p bytes~n", [ISize, DSize]),

    %% Try deflate and inflate with different internal buffer sizes
    ISpan = 1,
    DSpan = 10, % use larger span around deflated size as it may vary depending on buf size

    Cases = [{DS,IS} || DMul<-[1,2],
			DS <- lists:seq((DSize div DMul)-DSpan,
					(DSize div DMul)+DSpan),
			IMul<-[1,2],
			IS <- lists:seq((ISize div IMul)-ISpan,
					(ISize div IMul)+ISpan)],

    lists:foreach(fun(Case) -> otp_7359_def_inf(IData,Case) end,
		  Cases).


otp_7359_def_inf(Data,{DefSize,InfSize}) ->    
    %%io:format("Try: DefSize=~p InfSize=~p~n", [DefSize,InfSize]),
    ZDef = zlib:open(),
    ok = zlib:deflateInit(ZDef),
    ok = zlib:setBufSize(ZDef,DefSize),
    DefData = iolist_to_binary(zlib:deflate(ZDef, Data, sync)),
    %%io:format("Deflated ~p(~p) -> ~p(~p) bytes~n", 
    %%          [byte_size(Data), InfSize, byte_size(DefData), DefSize]),
    ok = zlib:close(ZDef),

    ZInf = zlib:open(),
    ok = zlib:inflateInit(ZInf),
    ok = zlib:setBufSize(ZInf,InfSize),
    Data = iolist_to_binary(zlib:inflate(ZInf, DefData)),
    ok = zlib:close(ZInf),
    ok.

otp_9981(Config) when is_list(Config) ->
    Ports = lists:sort(erlang:ports()),
    Invalid = <<"My invalid data">>,
    catch zlib:compress(invalid),
    Ports = lists:sort(erlang:ports()),
    catch zlib:uncompress(Invalid),
    Ports = lists:sort(erlang:ports()),
    catch zlib:zip(invalid),
    Ports = lists:sort(erlang:ports()),
    catch zlib:unzip(Invalid),
    Ports = lists:sort(erlang:ports()),
    catch zlib:gzip(invalid),
    Ports = lists:sort(erlang:ports()),
    catch zlib:gunzip(Invalid),
    Ports = lists:sort(erlang:ports()),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Helps with testing directly %%%%%%%%%%%%%

conf(What,Config) ->
    try proplists:get_value(What,Config) of
	undefined ->
	    "./zlib_SUITE_data";
	Dir ->
	    Dir
    catch
	_:_ -> "./zlib_SUITE_data"
    end.

t() -> t([all]).

t(What) when not is_list(What) ->
    t([What]);
t(What) ->
    lists:foreach(fun(T) ->
			  try ?MODULE:T([])
			  catch _E:_R ->
				  Line = get(test_server_loc),
				  io:format("Failed ~p:~p ~p ~p ~p~n", 
					    [T,Line,_E,_R, erlang:get_stacktrace()])
			  end
		  end, expand(What)).

expand(All) ->
    lists:reverse(expand(All,[])).
expand([H|T], Acc)  -> 
    case ?MODULE:H(suite) of
	[] -> expand(T,[H|Acc]);
	Cs -> 
	    R = expand(Cs, Acc),
	    expand(T, R)
    end;
expand([], Acc) -> Acc.

