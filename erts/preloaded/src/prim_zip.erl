%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% zip functions that are used by code_server

-module(prim_zip).

%% unzipping piecemeal
-export([
	 open/1,
	 open/3,
         foldl/3,
	 close/1
	]).

%% Internal function. Exported to avoid dialyzer warnings
-export([splitter/3]).

%% includes
-include_lib("kernel/include/file.hrl"). % #file_info
-include_lib("stdlib/include/zip.hrl").  % #zip_file, #zip_comment
-include("zip_internal.hrl").            % #cd_file_header etc

%% max bytes read from files and archives (and fed to zlib)
-define(READ_BLOCK_SIZE, 16*1024).

%% for debugging, to turn off catch
-define(CATCH, catch).

-record(primzip_file,
	{name,
	 get_info,
	 get_bin}).

-record(primzip,
	{files = [] :: [#primzip_file{}],
	 zlib,		% handle to the zlib port from zlib:open
	 input,         % fun/2 for file/memory input
	 in}).		% input (file handle or binary)

filter_fun() ->
    Continue = true,
    Include = true,
    fun({_Name, _GetInfoFun, _GetBinFun}, Acc) ->
	    {Continue, Include, Acc}
    end.

%% Open a zip archive
open(F) ->
    open(filter_fun(), undefined, F).

open(FilterFun, FilterAcc, F) when is_function(FilterFun, 2) ->
    try
	do_open(FilterFun, FilterAcc, F)
    catch
	throw:{filter_fun_throw, Reason} ->
	    throw(Reason);
	throw:InternalReason ->
	    {error, InternalReason};
	Class:Reason:Stk ->
	    erlang:error(erlang:raise(Class, Reason, Stk))
    end;
open(_, _, _) ->
    {error, einval}.

do_open(FilterFun, FilterAcc, F) ->
    Input = get_zip_input(F),
    In0 = Input({open, F, [read, binary, raw]}, []),
    Z = zlib:open(),
    PrimZip = #primzip{files = [], zlib = Z, in = In0, input = Input},
    try
	{PrimZip2, FilterAcc2} = get_central_dir(PrimZip, FilterFun, FilterAcc),
	{ok, PrimZip2, FilterAcc2}
    catch
	Class:Reason:Stk ->
	    _ = close(PrimZip),
	    erlang:error(erlang:raise(Class, Reason, Stk))
    end.

%% iterate over all files in a zip archive
foldl(FilterFun, FilterAcc, #primzip{files = Files} = PrimZip)
  when is_function(FilterFun, 2) ->
    try
	{ok, FilterAcc2, PrimZip2} =
	    do_foldl(FilterFun, FilterAcc, Files, [], PrimZip, PrimZip),
	{ok, PrimZip2, FilterAcc2}
    catch
	throw:{filter_fun_throw, Reason} ->
	    throw(Reason);
	throw:InternalReason ->
	    {error, InternalReason};
	Class:Reason:Stk ->
	    erlang:error(erlang:raise(Class, Reason, Stk))
    end;
foldl(_, _, _) ->
    {error, einval}.

do_foldl(FilterFun, FilterAcc, [PF | Tail], Acc0, PrimZip, PrimZipOrig) ->
    #primzip_file{name = F, get_info = GetInfo, get_bin = GetBin} = PF,
    try FilterFun({F, GetInfo, GetBin}, FilterAcc) of
	{Continue, Include, FilterAcc2} ->
	    Acc1 = include_acc(Include, PF, Acc0),
	    case Continue of
		true ->
		    do_foldl(FilterFun, FilterAcc2, Tail, Acc1, PrimZip, PrimZipOrig);
		false ->
		    {ok, FilterAcc2, PrimZipOrig}
	    end;
	FilterRes ->
	    throw({illegal_filter, FilterRes})
    catch
	throw:Reason ->
	    throw({filter_fun_throw, Reason})
    end;
do_foldl(_FilterFun, FilterAcc, [], Acc, PrimZip, _PrimZipOrig) ->
    {ok, FilterAcc, PrimZip#primzip{files = reverse(Acc)}}.

include_acc(Include, PF, Acc) ->
    case Include of
	false ->
	    Acc;
	true ->
	    [PF | Acc];
	{true, Nick} ->
	    [PF#primzip_file{name = Nick} | Acc];
	{true, Nick, GetInfo, GetBin} ->
	    PF2 = #primzip_file{name = Nick, get_info = GetInfo, get_bin = GetBin},
	    [PF2 | Acc];
	List when is_list(List) ->
	    %% Add new entries
	    Fun = fun(I, A) -> include_acc(I, PF, A) end,
	    lists_foldl(Fun, Acc, List);
	Bad ->
	    throw({illegal_filter, Bad})
    end.

lists_foldl(F, Accu, [Hd|Tail]) ->
    lists_foldl(F, F(Hd, Accu), Tail);
lists_foldl(F, Accu, []) when is_function(F, 2) ->
    Accu.

%% close a zip archive
close(#primzip{in = In0, input = Input, zlib = Z}) ->
    Input(close, In0),
    zlib:close(Z);
close(_) ->
    {error, einval}.

get_zip_input({F, B}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input(F) when is_list(F) ->
    fun prim_file_io/2;
get_zip_input(_) ->
    throw(einval).

%% get a file from the archive
get_z_file(F, Offset, ChunkSize, #primzip{zlib = Z, in = In0, input = Input}) ->
    case Input({pread, Offset, ChunkSize}, In0) of
	{<<?LOCAL_FILE_MAGIC:32/little,
	  BLH:(?LOCAL_FILE_HEADER_SZ-4)/binary, _/binary>> = B, _In1} ->
	    #local_file_header{gp_flag = GPFlag,
			       file_name_length = FNLen,
			       extra_field_length = EFLen,
			       comp_method = CompMethod} =
		local_file_header_from_bin(BLH, F),
	    DataOffs = ?LOCAL_FILE_HEADER_SZ + FNLen + EFLen
		+ offset_over_z_data_descriptor(GPFlag),
	    case B of
		<<_:DataOffs/binary, Data/binary>> ->
		    Out = get_z_all(CompMethod, Data, Z, F),
		    %%{Out, CRC} = get_z_all(CompMethod, Data, Z, F),
		    %%CRC == CRC32 orelse throw({bad_crc, F}),
		    Out;
		_ ->
		    throw({bad_local_file_offset, F})
	    end;
	_ ->
	    throw({bad_local_file_header, F})
    end.

%% flag for zlib
-define(MAX_WBITS, 15).

%% get compressed or stored data
get_z_all(?DEFLATED, Compressed, Z, _F) ->
    ok = zlib:inflateInit(Z, -?MAX_WBITS),
    Uncompressed = zlib:inflate(Z, Compressed),
    %%_CRC = zlib:crc32(Z),
    ?CATCH zlib:inflateEnd(Z),
    erlang:iolist_to_binary(Uncompressed); % {erlang:iolist_to_binary(Uncompressed), CRC}
get_z_all(?STORED, Stored, _Z, _F) ->
    %%CRC0 = zlib:crc32(Z, <<>>),
    %%CRC1 = zlib:crc32(Z, CRC0, Stored),
    Stored; % {Stored, CRC1};
get_z_all(CompMethod, _, _, F) ->
    throw({unsupported_compression, F, CompMethod}).

%% skip data descriptor if any
offset_over_z_data_descriptor(GPFlag) when GPFlag band 8 =:= 8 ->
    12;
offset_over_z_data_descriptor(_GPFlag) ->
    0.

%% get the central directory from the archive
get_central_dir(#primzip{in = In0, input = Input} = PrimZip, FilterFun, FilterAcc) ->
    {B, In1} = get_end_of_central_dir(In0, ?END_OF_CENTRAL_DIR_SZ, Input),
    {EOCD, _BComment} = eocd_and_comment_from_bin(B),
    {BCD, In2} = Input({pread, EOCD#eocd.offset, EOCD#eocd.size}, In1),
    N = EOCD#eocd.entries,
    EndOffset = EOCD#eocd.offset,
    PrimZip2 = PrimZip#primzip{in = In2},
    if
	N =:= 0 ->
	    {PrimZip2, FilterAcc};
	true ->
	    {F, Offset, CFH, BCDRest} = get_file_header(BCD),
	    get_cd_loop(N, BCDRest, [], PrimZip2, F, Offset, CFH, EndOffset, FilterFun, FilterAcc, PrimZip)
    end.

get_cd_loop(N, BCD, Acc0, PrimZip, FileName, Offset, CFH, EndOffset, FilterFun, FilterAcc, PrimZipOrig) ->
    {NextF, NextOffset, NextCFH, BCDRest, Size} =
	if
	    N =:= 1 ->
		{undefined, undefined, undefined, undefined, EndOffset - Offset};
	    true ->
		{NextF0, NextOffset0, NextCFH0, BCDRest0} = get_file_header(BCD),
		{NextF0, NextOffset0, NextCFH0, BCDRest0, NextOffset0 - Offset}
	end,
    %% erlang:display({FileName, N, Offset, Size, NextPF}),
    GetInfo = fun() -> cd_file_header_to_file_info(FileName, CFH, <<>>) end,
    GetBin = fun() -> get_z_file(FileName, Offset, Size, PrimZip) end,
    PF = #primzip_file{name = FileName, get_info = GetInfo, get_bin = GetBin},
    try FilterFun({FileName, GetInfo, GetBin}, FilterAcc) of
	{Continue, Include, FilterAcc2} ->
	    Acc1 =
		case Include of
		    false ->
			Acc0;
		    true ->
			[PF | Acc0];
		    {true, Nick} ->
			[PF#primzip_file{name = Nick} | Acc0];
		    {true, Nick, GI, GB} ->
			PF2 = #primzip_file{name = Nick, get_info = GI, get_bin = GB},
			[PF2 | Acc0];
		    List when is_list(List) ->
			%% Add new entries
			Fun = fun(I, A) -> include_acc(I, PF, A) end,
			lists_foldl(Fun, Acc0, List)
		end,
	    case Continue of
		true when N > 1 ->
		    get_cd_loop(N-1, BCDRest, Acc1, PrimZip, NextF, NextOffset, NextCFH, EndOffset, FilterFun, FilterAcc2, PrimZipOrig);
		true ->
		    PrimZip2 = PrimZip#primzip{files = reverse(Acc1)},
		    {PrimZip2, FilterAcc2};
		false ->
		    {PrimZipOrig, FilterAcc2}
	    end;
	FilterRes ->
	    throw({illegal_filter, FilterRes})
    catch
	throw:Reason ->
	    throw({filter_fun_throw, Reason})
    end.

get_file_header(BCD) ->
    BCFH =
	case BCD of
	    <<?CENTRAL_FILE_MAGIC:32/little,
	     B:(?CENTRAL_FILE_HEADER_SZ-4)/binary,
	     _/binary>> ->
		B;
	    _ ->
		throw(bad_central_directory)
	end,
    CFH = cd_file_header_from_bin(BCFH),
    FileNameLen = CFH#cd_file_header.file_name_length,
    ExtraLen = CFH#cd_file_header.extra_field_length,
    CommentLen = CFH#cd_file_header.file_comment_length,
    ToGet = FileNameLen + ExtraLen + CommentLen,
    {B2, BCDRest} =
	case BCD of
	    <<_:?CENTRAL_FILE_HEADER_SZ/binary,
	     G:ToGet/binary,
	     Rest/binary>> ->
		{G, Rest};
	    _ ->
		throw(bad_central_directory)
	end,
    FileName = get_filename_from_b2(B2, FileNameLen, ExtraLen, CommentLen),
    Offset = CFH#cd_file_header.local_header_offset,
    {FileName, Offset, CFH, BCDRest}.

get_filename_from_b2(B, FileNameLen, ExtraLen, CommentLen) ->
    case B of
	<<BFileName:FileNameLen/binary,
	 _BExtra:ExtraLen/binary,
	 _BComment:CommentLen/binary>> ->
	    binary_to_list(BFileName);
	_ ->
	    throw(bad_central_directory)
    end.

%% get end record, containing the offset to the central directory
%% the end record is always at the end of the file BUT alas it is
%% of variable size (yes that's dumb!)
get_end_of_central_dir(_In, Sz, _Input) when Sz > 16#ffff ->
    throw(bad_eocd);
get_end_of_central_dir(In0, Sz, Input) ->
    In1 = Input({seek, eof, -Sz}, In0),
    {B, In2} = Input({read, Sz}, In1),
    case find_eocd_header(B) of
	none ->
	    get_end_of_central_dir(In2, Sz+Sz, Input);
	Header ->
	    {Header, In2}
    end.

%% find the end record by matching for it
find_eocd_header(<<?END_OF_CENTRAL_DIR_MAGIC:32/little, Rest/binary>>) ->
    Rest;
find_eocd_header(<<_:8, Rest/binary>>)
  when byte_size(Rest) > ?END_OF_CENTRAL_DIR_SZ-4 ->
    find_eocd_header(Rest);
find_eocd_header(_) ->
    none.

%% io objects
prim_file_io({file_info, F}, _) ->
    case prim_file:read_file_info(F) of
	{ok, Info} -> Info;
	{error, E} -> throw(E)
    end;
prim_file_io({open, FN, Opts}, _) ->
    case ?CATCH prim_file:open(FN, Opts++[binary]) of
	{ok, H} ->
	    H;
	{error, E} ->
	    throw(E)
    end;
prim_file_io({read, N}, H) ->
    case prim_file:read(H, N) of
	{ok, B} -> {B, H};
	eof -> {eof, H};
	{error, E} -> throw(E)
    end;
prim_file_io({pread, Pos, N}, H) ->
    case prim_file:pread(H, Pos, N) of
	{ok, B} -> {B, H};
	eof -> {eof, H};
	{error, E} -> throw(E)
    end;
prim_file_io({seek, S, Pos}, H) ->
    case prim_file:position(H, {S, Pos}) of
	{ok, _NewPos} -> H;
	{error, Error} -> throw(Error)
    end;
prim_file_io({write, Data}, H) ->
    case prim_file:write(H, Data) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
prim_file_io({pwrite, Pos, Data}, H) ->
    case prim_file:pwrite(H, Pos, Data) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
prim_file_io({close, FN}, H) ->
    case prim_file:close(H) of
	ok -> FN;
	{error, Error} -> throw(Error)
    end;
prim_file_io(close, H) ->
    prim_file_io({close, ok}, H);
prim_file_io({set_file_info, F, FI}, H) ->
    case prim_file:write_file_info(F, FI) of
	ok -> H;
	{error, Error} -> throw(Error)
    end.

binary_io({pread, NewPos, N}, {OldPos, B}) ->
    case B of
	<<_:NewPos/binary, Read:N/binary, _Rest/binary>> ->
	    {Read, {NewPos+N, B}};
	_ ->
	    {eof, {OldPos, B}}
    end;
binary_io({read, N}, {Pos, B}) when Pos >= byte_size(B) ->
    {eof, {Pos+N, B}};
binary_io({read, N}, {Pos, B}) when Pos + N > byte_size(B) ->
    case B of
	<<_:Pos/binary, Read/binary>> ->
	    {Read, {byte_size(B), B}};
	_ ->
	    {eof, {Pos, B}}
    end;
binary_io({read, N}, {Pos, B}) ->
    case B of
	<<_:Pos/binary, Read:N/binary, _/binary>> ->
	    {Read, {Pos+N, B}};
	_ ->
	    {eof, {Pos, B}}
    end;
binary_io({seek, bof, Pos}, {_OldPos, B}) ->
    {Pos, B};
binary_io({seek, cur, Pos}, {OldPos, B}) ->
    {OldPos + Pos, B};
binary_io({seek, eof, Pos}, {_OldPos, B}) ->
    {byte_size(B) + Pos, B};
binary_io({file_info, {_Filename, B}}, A) ->
    binary_io({file_info, B}, A);
binary_io({file_info, B}, _) ->
    {Type, Size} =
	if
	    is_binary(B) -> {regular, byte_size(B)};
	    B =:= directory -> {directory, 0}
	end,
    Now = erlang:localtime(),
    #file_info{size = Size, type = Type, access = read_write,
	       atime = Now, mtime = Now, ctime = Now,
	       mode = 0, links = 1, major_device = 0,
	       minor_device = 0, inode = 0, uid = 0, gid = 0};
binary_io({pwrite, Pos, Data}, {OldPos, B}) ->
    {OldPos, pwrite_binary(B, Pos, Data)};
binary_io({write, Data}, {Pos, B}) ->
    {Pos + erlang:iolist_size(Data), pwrite_binary(B, Pos, Data)};
binary_io({open, {_Filename, B}, _Opts}, _) ->
    {0, B};
binary_io({open, B, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, Filename, _Opts}, _) when is_list(Filename) ->
    {0, <<>>};
binary_io(close, {_Pos, B}) ->
    B;
binary_io({close, FN}, {_Pos, B}) ->
    {FN, B}.

%% ZIP header manipulations
eocd_and_comment_from_bin(<<DiskNum:16/little,
			   StartDiskNum:16/little,
			   EntriesOnDisk:16/little,
			   Entries:16/little,
			   Size:32/little,
			   Offset:32/little,
			   ZipCommentLength:16/little,
			   Comment:ZipCommentLength/binary>>) ->
    {#eocd{disk_num = DiskNum,
	   start_disk_num = StartDiskNum,
	   entries_on_disk = EntriesOnDisk,
	   entries = Entries,
	   size = Size,
	   offset = Offset,
	   zip_comment_length = ZipCommentLength},
     Comment};
eocd_and_comment_from_bin(_) ->
    throw(bad_eocd).

%% make a file_info from a central directory header
cd_file_header_to_file_info(FileName,
			    #cd_file_header{uncomp_size = UncompSize,
					    last_mod_time = ModTime,
					    last_mod_date = ModDate},
			    ExtraField) when is_binary(ExtraField) ->
    T = dos_date_time_to_datetime(ModDate, ModTime),
    Type =
	case last(FileName) of
	    $/ -> directory;
	    _  -> regular
	end,
    FI = #file_info{size = UncompSize,
		    type = Type,
		    access = read_write,
		    atime = T,
		    mtime = T,
		    ctime = T,
		    mode = 8#066,
		    links = 1,
		    major_device = 0,
		    minor_device = 0,
		    inode = 0,
		    uid = 0,
		    gid = 0},
    add_extra_info(FI, ExtraField).

%% add extra info to file (some day when we implement it)
%% add_extra_info(FI, <<?EXTENDED_TIMESTAMP_TAG:16/little, _Rest/binary>>) ->
%%     FI;     % not yet supported, some other day...
%% add_extra_info(FI, <<?UNIX_EXTRA_FIELD_TAG:16/little, Rest/binary>>) ->
%%     _UnixExtra = unix_extra_field_and_var_from_bin(Rest),
%%     FI;     % not yet supported, and not widely used
add_extra_info(FI, _) ->
    FI.
%%
%% unix_extra_field_and_var_from_bin(<<TSize:16/little,
%% 				   ATime:32/little,
%% 				   MTime:32/little,
%% 				   UID:16/little,
%% 				   GID:16/little,
%% 				   Var:TSize/binary>>) ->
%%     {#unix_extra_field{atime = ATime,
%% 		       mtime = MTime,
%% 		       uid = UID,
%% 		       gid = GID},
%%      Var};
%% unix_extra_field_and_var_from_bin(_) ->
%%     throw(bad_unix_extra_field).

%% convert between erlang datetime and the MSDOS date and time
%% that's stored in the zip archive
%%    	 MSDOS Time  	           MSDOS Date
%% bit   0 - 4 	 5 - 10 11 - 15    16 - 20      21 - 24        25 - 31
%% value second  minute hour 	   day (1 - 31) month (1 - 12) years from 1980
dos_date_time_to_datetime(DosDate, DosTime) ->
    <<Hour:5, Min:6, Sec:5>> = <<DosTime:16>>,
    <<YearFrom1980:7, Month:4, Day:5>> = <<DosDate:16>>,
    {{YearFrom1980+1980, Month, Day},
     {Hour, Min, Sec}}.

cd_file_header_from_bin(<<VersionMadeBy:16/little,
			 VersionNeeded:16/little,
			 GPFlag:16/little,
			 CompMethod:16/little,
			 LastModTime:16/little,
			 LastModDate:16/little,
			 CRC32:32/little,
			 CompSize:32/little,
			 UncompSize:32/little,
			 FileNameLength:16/little,
			 ExtraFieldLength:16/little,
			 FileCommentLength:16/little,
			 DiskNumStart:16/little,
			 InternalAttr:16/little,
			 ExternalAttr:32/little,
			 LocalHeaderOffset:32/little>>) ->
    #cd_file_header{version_made_by = VersionMadeBy,
		    version_needed = VersionNeeded,
		    gp_flag = GPFlag,
		    comp_method = CompMethod,
		    last_mod_time = LastModTime,
		    last_mod_date = LastModDate,
		    crc32 = CRC32,
		    comp_size = CompSize,
		    uncomp_size = UncompSize,
		    file_name_length = FileNameLength,
		    extra_field_length = ExtraFieldLength,
		    file_comment_length = FileCommentLength,
		    disk_num_start = DiskNumStart,
		    internal_attr = InternalAttr,
		    external_attr = ExternalAttr,
		    local_header_offset = LocalHeaderOffset};
cd_file_header_from_bin(_) ->
    throw(bad_cd_file_header).

local_file_header_from_bin(<<VersionNeeded:16/little,
			    GPFlag:16/little,
			    CompMethod:16/little,
			    LastModTime:16/little,
			    LastModDate:16/little,
			    CRC32:32/little,
			    CompSize:32/little,
			    UncompSize:32/little,
			    FileNameLength:16/little,
			    ExtraFieldLength:16/little>>,
			   _F) ->
    #local_file_header{version_needed = VersionNeeded,
		       gp_flag = GPFlag,
		       comp_method = CompMethod,
		       last_mod_time = LastModTime,
		       last_mod_date = LastModDate,
		       crc32 = CRC32,
		       comp_size = CompSize,
		       uncomp_size = UncompSize,
		       file_name_length = FileNameLength,
		       extra_field_length = ExtraFieldLength};
local_file_header_from_bin(_, F) ->
    throw({bad_local_file_header, F}).

%% A pwrite-like function for iolists (used by memory-option)

split_iolist(B, Pos) when is_binary(B) ->
    split_binary(B, Pos);
split_iolist(L, Pos) when is_list(L) ->
    splitter([], L, Pos).

splitter(Left, Right, 0) ->
    {Left, Right};
splitter(<<>>, Right, RelPos) ->
    split_iolist(Right, RelPos);
splitter(Left, [A | Right], RelPos) when is_list(A) or is_binary(A) ->
    Sz = erlang:iolist_size(A),
    case Sz > RelPos of
	true ->
	    {Leftx, Rightx} = split_iolist(A, RelPos),
	    {[Left | Leftx], [Rightx, Right]};
	_ ->
	    splitter([Left | A], Right, RelPos - Sz)
    end;
splitter(Left, [A | Right], RelPos) when is_integer(A) ->
    splitter([Left, A], Right, RelPos - 1);
splitter(Left, Right, RelPos) when is_binary(Right) ->
    splitter(Left, [Right], RelPos).

skip_iolist(B, Pos) when is_binary(B) ->
    case B of
	<<_:Pos/binary, Bin/binary>> -> Bin;
	_ -> <<>>
    end;
skip_iolist(L, Pos) when is_list(L) ->
    skipper(L, Pos).

skipper(Right, 0) ->
    Right;
skipper([A | Right], RelPos) when is_list(A) or is_binary(A) ->
    Sz = erlang:iolist_size(A),
    case Sz > RelPos of
	true ->
	    Rightx = skip_iolist(A, RelPos),
	    [Rightx, Right];
	_ ->
	    skip_iolist(Right, RelPos - Sz)
    end;
skipper([A | Right], RelPos) when is_integer(A) ->
    skip_iolist(Right, RelPos - 1).

pwrite_iolist(Iolist, Pos, Bin) ->
    {Left, Right} = split_iolist(Iolist, Pos),
    Sz = erlang:iolist_size(Bin),
    R = skip_iolist(Right, Sz),
    [Left, Bin | R].

pwrite_binary(B, Pos, Bin) ->
    erlang:iolist_to_binary(pwrite_iolist(B, Pos, Bin)).

reverse(X) ->
    reverse(X, []).

reverse([H|T], Y) ->
    reverse(T, [H|Y]);
reverse([], X) ->
    X.

last([E|Es]) -> last(E, Es).

last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.
