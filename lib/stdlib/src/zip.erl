%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2017. All Rights Reserved.
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
-module(zip).

%% Basic api
-export([unzip/1, unzip/2, extract/1, extract/2,
	 zip/2, zip/3, create/2, create/3, foldl/3,
	 list_dir/1, list_dir/2, table/1, table/2,
	 t/1, tt/1]).

%% unzipping piecemeal
-export([openzip_open/1, openzip_open/2,
	 openzip_get/1, openzip_get/2,
	 openzip_t/1, openzip_tt/1,
	 openzip_list_dir/1, openzip_list_dir/2,
	 openzip_close/1]).
%% 	 openzip_add/2]).

%% zip server
-export([zip_open/1, zip_open/2,
	 zip_get/1, zip_get/2,
	 zip_t/1, zip_tt/1,
	 zip_list_dir/1, zip_list_dir/2,
	 zip_close/1]).

%% just for debugging zip server, not documented, not tested, not to be used
-export([zip_get_state/1]).

%% includes
-include("file.hrl").		 % #file_info
-include("zip.hrl").	         % #zip_file, #zip_comment

%% max bytes fed to zlib
-define(WRITE_BLOCK_SIZE, 8*1024).

%% for debugging, to turn off catch
-define(CATCH, catch).

%% option sets
-record(unzip_opts, {
	  output,      % output object (fun)
	  input,       % input object (fun)
	  file_filter, % file filter (boolean fun)
	  open_opts,   % options passed to file:open
	  feedback,    % feeback (fun)
	  cwd          % directory to relate paths to
	 }).

-record(zip_opts, {
	  output,      % output object (fun)
	  input,       % input object (fun)
	  comment,     % zip-file comment
	  open_opts,   % options passed to file:open
	  feedback,    % feeback (fun)
	  cwd,         % directory to relate paths to
	  compress,    % compress files with these suffixes
	  uncompress   % uncompress files with these suffixes
	 }).

-record(list_dir_opts, {
	  input,       % input object (fun)
	  raw_iterator, % applied to each dir entry
	  open_opts    % options passed to file:open
	 }).

-record(openzip_opts, {
	  output,      % output object (fun)
	  open_opts,   % file:open options
	  cwd	       % directory to relate paths to
	 }).

% openzip record, state for an open zip-file
-record(openzip, {
	  zip_comment, % zip archive comment
	  files,       % filenames, infos, comments and offsets
	  in,          % archive handle
	  input,       % archive io object (fun)
	  output,      % output io object (fun)
	  zlib,	       % handle to open zlib
	  cwd	       % directory to relate paths to
	 }).

% Things that I would like to add to the public record #zip_file,
% but can't as it would make things fail at upgrade.
% Instead we use {#zip_file,#zip_file_extra} internally.
-record(zip_file_extra, {
	  crc32        % checksum
	 }).

%% max bytes read from files and archives (and fed to zlib)
-define(READ_BLOCK_SIZE, 16*1024).

%% -record(primzip_file, {
%% 	  name,
%% 	  offset,
%% 	  chunk_size
%% 	 }).

%% -record(primzip, {
%% 	  zlib,		% handle to the zlib port from zlib:open
%% 	  input,        % fun/2 for file/memory input
%% 	  in,		% input (file handle or binary)
%% 	  files		% [#primzip_file]
%% 	 }).

%% ZIP-file format records and defines

%% compression methods
-define(STORED, 0).
-define(UNCOMPRESSED, 0).
-define(SHRUNK, 1).
-define(REDUCED_1, 2).
-define(REDUCED_2, 3).
-define(REDUCED_3, 4).
-define(REDUCED_4, 5).
-define(IMPLODED, 6).
-define(TOKENIZED, 7).
-define(DEFLATED, 8).
-define(DEFLATED_64, 9).
-define(PKWARE_IMPLODED, 10).
-define(PKWARE_RESERVED, 11).
-define(BZIP2_COMPRESSED, 12).

%% zip-file records
-define(LOCAL_FILE_MAGIC,16#04034b50).
-define(LOCAL_FILE_HEADER_SZ,(4+2+2+2+2+2+4+4+4+2+2)).
-define(LOCAL_FILE_HEADER_CRC32_OFFSET, 4+2+2+2+2+2).
-record(local_file_header, {version_needed,
			    gp_flag,
			    comp_method,
			    last_mod_time,
			    last_mod_date,
			    crc32,
			    comp_size,
			    uncomp_size,
			    file_name_length,
			    extra_field_length}).

-define(CENTRAL_FILE_HEADER_SZ,(4+2+2+2+2+2+2+4+4+4+2+2+2+2+2+4+4)).

-define(CENTRAL_DIR_MAGIC, 16#06054b50).
-define(CENTRAL_DIR_SZ, (4+2+2+2+2+4+4+2)).
-define(CENTRAL_DIR_DIGITAL_SIG_MAGIC, 16#05054b50).
-define(CENTRAL_DIR_DIGITAL_SIG_SZ, (4+2)).

-define(CENTRAL_FILE_MAGIC, 16#02014b50).

-record(cd_file_header, {version_made_by,
			 version_needed,
			 gp_flag,
			 comp_method,
			 last_mod_time,
			 last_mod_date,
			 crc32,
			 comp_size,
			 uncomp_size,
			 file_name_length,
			 extra_field_length,
			 file_comment_length,
			 disk_num_start,
			 internal_attr,
			 external_attr,
			 local_header_offset}).

-define(END_OF_CENTRAL_DIR_MAGIC, 16#06054b50).
-define(END_OF_CENTRAL_DIR_SZ, (4+2+2+2+2+4+4+2)).

-record(eocd, {disk_num,
	       start_disk_num,
	       entries_on_disk,
	       entries,
	       size,
	       offset,
	       zip_comment_length}).


-type create_option() :: memory | cooked | verbose | {comment, string()}
                       | {cwd, file:filename()}
                       | {compress, extension_spec()}
                       | {uncompress, extension_spec()}.
-type extension() :: string().
-type extension_spec() :: all | [extension()] | {add, [extension()]} | {del, [extension()]}.
-type filename() :: file:filename().

-type zip_comment() :: #zip_comment{}.
-type zip_file() :: #zip_file{}.

-opaque handle() :: pid().

-export_type([create_option/0, filename/0, handle/0]).

%% Open a zip archive with options
%%

openzip_open(F) ->
    openzip_open(F, []).

openzip_open(F, Options) ->
    case ?CATCH do_openzip_open(F, Options) of
	{ok, OpenZip} ->
	    {ok, OpenZip};
	Error ->
	    {error, Error}
    end.

do_openzip_open(F, Options) ->
    Opts = get_openzip_options(Options),
    #openzip_opts{output = Output, open_opts = OpO, cwd = CWD} = Opts,
    Input = get_input(F),
    In0 = Input({open, F, OpO -- [write]}, []),
    {[#zip_comment{comment = C} | Files], In1} =
	get_central_dir(In0, fun raw_file_info_etc/5, Input),
    Z = zlib:open(),
    {ok, #openzip{zip_comment = C,
		  files = Files,
		  in = In1,
		  input = Input,
		  output = Output,
		  zlib = Z,
		  cwd = CWD}}.

%% retrieve all files from an open archive
openzip_get(OpenZip) ->
    case ?CATCH do_openzip_get(OpenZip) of
	{ok, Result} -> {ok, Result};
	Error -> {error, Error}
    end.

do_openzip_get(#openzip{files = Files, in = In0, input = Input,
			output = Output, zlib = Z, cwd = CWD}) ->
    ZipOpts = #unzip_opts{output = Output, input = Input,
			  file_filter = fun all/1, open_opts = [],
			  feedback = fun silent/1, cwd = CWD},
    R = get_z_files(Files, Z, In0, ZipOpts, []),
    {ok, R};
do_openzip_get(_) ->
    throw(einval).

%% retrieve a file from an open archive
openzip_get(FileName, OpenZip) ->
    case ?CATCH do_openzip_get(FileName, OpenZip) of
	{ok, Result} -> {ok, Result};
	Error -> {error, Error}
    end.

do_openzip_get(F, #openzip{files = Files, in = In0, input = Input,
			   output = Output, zlib = Z, cwd = CWD}) ->
    %%case lists:keysearch(F, #zip_file.name, Files) of
    case file_name_search(F, Files) of
	{#zip_file{offset = Offset},_}=ZFile ->
	    In1 = Input({seek, bof, Offset}, In0),
	    case get_z_file(In1, Z, Input, Output, [], fun silent/1,
			    CWD, ZFile, fun all/1) of
		{file, R, _In2} -> {ok, R};
		_ -> throw(file_not_found)
	    end;
	_ -> throw(file_not_found)
    end;
do_openzip_get(_, _) ->
    throw(einval).

file_name_search(Name,Files) ->
    case lists:dropwhile(fun({ZipFile,_}) -> ZipFile#zip_file.name =/= Name end,
			 Files) of
	[ZFile|_] -> ZFile;
	[] -> false
    end.

%% %% add a file to an open archive
%% openzip_add(File, OpenZip) ->
%%     case ?CATCH do_openzip_add(File, OpenZip) of
%% 	{ok, Result} -> {ok, Result};
%% 	Error -> {error, Error}
%%     end.

%% do_openzip_add(File, #open_zip{files = Files, in = In0,
%% 			       opts = Opts} = OpenZip0) ->
%%     throw(nyi),
%%     Z = zlib:open(),
%%     R = get_z_files(Files, In0, Z, Opts, []),
%%     zlib:close(Z),
%%     {ok, R};
%% do_openzip_add(_, _) ->
%%     throw(einval).

%% get file list from open archive
openzip_list_dir(#openzip{zip_comment = Comment,
			  files = Files}) ->
    {ZipFiles,_Extras} = lists:unzip(Files),
    {ok, [#zip_comment{comment = Comment} | ZipFiles]};
openzip_list_dir(_) ->
    {error, einval}.

openzip_list_dir(#openzip{files = Files}, [names_only]) ->
    {ZipFiles,_Extras} = lists:unzip(Files),
    Names = [Name || {#zip_file{name=Name},_} <- ZipFiles],
    {ok, Names};
openzip_list_dir(_, _) ->
    {error, einval}.

%% close an open archive
openzip_close(#openzip{in = In0, input = Input, zlib = Z}) ->
    Input(close, In0),
    zlib:close(Z);
openzip_close(_) ->
    {error, einval}.

%% Extract from a zip archive with options
%%
%% Accepted options:
%% verbose, cooked, file_list, keep_old_files, file_filter, memory

-spec(unzip(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}]).

unzip(F) -> unzip(F, []).

-spec(unzip(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      Options :: [Option],
      Option  :: {file_list, FileList} | cooked
               | keep_old_files | verbose | memory |
                 {file_filter, FileFilter} | {cwd, CWD},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}],
      FileFilter :: fun((ZipFile) -> boolean()),
      CWD :: file:filename(),
      ZipFile :: zip_file(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}}).

unzip(F, Options) ->
    case ?CATCH do_unzip(F, Options) of
	{ok, R} -> {ok, R};
	Error -> {error, Error}
    end.

do_unzip(F, Options) ->
    Opts = get_unzip_options(F, Options),
    #unzip_opts{input = Input, open_opts = OpO} = Opts,
    In0 = Input({open, F, OpO -- [write]}, []),
    RawIterator = fun raw_file_info_etc/5,
    {Info, In1} = get_central_dir(In0, RawIterator, Input),
    %% get rid of zip-comment
    Z = zlib:open(),
    Files = try
                get_z_files(Info, Z, In1, Opts, [])
            after
                zlib:close(Z),
                Input(close, In1)
            end,
    {ok, Files}.

%% Iterate over all files in a zip archive
-spec(foldl(Fun, Acc0, Archive) -> {ok, Acc1} | {error, Reason} when
      Fun :: fun((FileInArchive, GetInfo, GetBin, AccIn) -> AccOut),
      FileInArchive :: file:name(),
      GetInfo :: fun(() -> file:file_info()),
      GetBin :: fun(() -> binary()),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Archive :: file:name() | {file:name(), binary()},
      Reason :: term()).

foldl(Fun, Acc0, Archive) when is_function(Fun, 4) ->
    ZipFun =
	fun({Name, GetInfo, GetBin}, A) ->
		A2 = Fun(Name, GetInfo, GetBin, A),
		{true, false, A2}
	end,
    case prim_zip:open(ZipFun, Acc0, Archive) of
	{ok, PrimZip, Acc1} ->
	    ok = prim_zip:close(PrimZip),
	    {ok, Acc1};
	{error, bad_eocd} ->
	    {error, "Not an archive file"};
	{error, Reason} ->
	    {error, Reason}
    end;
foldl(_,_, _) ->
    {error, einval}.

%% Create zip archive name F from Files or binaries
%%
%% Accepted options:
%% verbose, cooked, memory, comment

-spec(zip(Name, FileList) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      RetValue :: {ok, FileName :: file:name()}
                | {ok, {FileName :: file:name(), binary()}}
                | {error, Reason :: term()}).

zip(F, Files) -> zip(F, Files, []).

-spec(zip(Name, FileList, Options) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      Options  :: [Option],
      Option   :: memory | cooked | verbose | {comment, Comment}
                | {cwd, CWD} | {compress, What} | {uncompress, What},
      What     :: all | [Extension] | {add, [Extension]} | {del, [Extension]},
      Extension :: string(),
      Comment  :: string(),
      CWD      :: file:filename(),
      RetValue :: {ok, FileName :: file:name()}
                | {ok, {FileName :: file:name(), binary()}}
                | {error, Reason :: term()}).

zip(F, Files, Options) ->
    case ?CATCH do_zip(F, Files, Options) of
	{ok, R} -> {ok, R};
	Error -> {error, Error}
    end.

do_zip(F, Files, Options) ->
    Opts = get_zip_options(Files, Options),
    #zip_opts{output = Output, open_opts = OpO} = Opts,
    Out0 = Output({open, F, OpO}, []),
    Z = zlib:open(),
    try
        {Out1, LHS, Pos} = put_z_files(Files, Z, Out0, 0, Opts, []),
        zlib:close(Z),
        Out2 = put_central_dir(LHS, Pos, Out1, Opts),
        Out3 = Output({close, F}, Out2),
        {ok, Out3}
    catch
        C:R ->
            Stk = erlang:get_stacktrace(),
            zlib:close(Z),
            Output({close, F}, Out0),
            erlang:raise(C, R, Stk)
    end.


%% List zip directory contents
%%
%% Accepted options:
%% cooked, file_filter, file_output (latter 2 undocumented)

-spec(list_dir(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()]).

list_dir(F) -> list_dir(F, []).

-spec(list_dir(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()],
      Options :: [Option],
      Option :: cooked).

list_dir(F, Options) ->
    case ?CATCH do_list_dir(F, Options) of
	{ok, R} -> {ok, R};
	Error -> {error, Error}
    end.

do_list_dir(F, Options) ->
    Opts = get_list_dir_options(F, Options),
    #list_dir_opts{input = Input, open_opts = OpO,
		   raw_iterator = RawIterator} = Opts,
    In0 = Input({open, F, OpO}, []),
    {Info, In1} = get_central_dir(In0, RawIterator, Input),
    Input(close, In1),
    {ok, Info}.

%% Print zip directory in short form

-spec(t(Archive) -> ok when
      Archive :: file:name() | binary() | ZipHandle,
      ZipHandle :: handle()).

t(F) when is_pid(F) -> zip_t(F);
t(F) when is_record(F, openzip) -> openzip_t(F);
t(F) -> t(F, fun raw_short_print_info_etc/5).

t(F, RawPrint) ->
    case ?CATCH do_t(F, RawPrint) of
	ok -> ok;
	Error -> {error, Error}
    end.

do_t(F, RawPrint) ->
    Input = get_input(F),
    OpO = [raw],
    In0 = Input({open, F, OpO}, []),
    {_Info, In1} = get_central_dir(In0, RawPrint, Input),
    Input(close, In1),
    ok.

%% Print zip directory in long form (like ls -l)

-spec(tt(Archive) -> ok when
      Archive :: file:name() | binary() | ZipHandle,
      ZipHandle :: handle()).

tt(F) when is_pid(F) -> zip_tt(F);
tt(F) when is_record(F, openzip) -> openzip_tt(F);
tt(F) -> t(F, fun raw_long_print_info_etc/5).


%% option utils
get_unzip_opt([], Opts) ->
    Opts;
get_unzip_opt([verbose | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{feedback = fun verbose_unzip/1});
get_unzip_opt([cooked | Rest], #unzip_opts{open_opts = OpO} = Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{open_opts = OpO -- [raw]});
get_unzip_opt([memory | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{output = fun binary_io/2});
get_unzip_opt([{cwd, CWD} | Rest], Opts) ->
    get_unzip_opt(Rest, Opts#unzip_opts{cwd = CWD});
get_unzip_opt([{file_filter, F} | Rest], Opts) ->
    Filter1 = fun({ZipFile,_Extra}) -> F(ZipFile) end,
    Filter2 = fun_and_1(Filter1, Opts#unzip_opts.file_filter),
    get_unzip_opt(Rest, Opts#unzip_opts{file_filter = Filter2});
get_unzip_opt([{file_list, L} | Rest], Opts) ->
    FileInList = fun(F) -> file_in_list(F, L) end,
    Filter = fun_and_1(FileInList, Opts#unzip_opts.file_filter),
    get_unzip_opt(Rest, Opts#unzip_opts{file_filter = Filter});
get_unzip_opt([keep_old_files | Rest], Opts) ->
    Keep = fun keep_old_file/1,
    Filter = fun_and_1(Keep, Opts#unzip_opts.file_filter),
    get_unzip_opt(Rest, Opts#unzip_opts{file_filter = Filter});
get_unzip_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

get_list_dir_opt([], Opts) ->
    Opts;
get_list_dir_opt([cooked | Rest], #list_dir_opts{open_opts = OpO} = Opts) ->
    get_list_dir_opt(Rest, Opts#list_dir_opts{open_opts = OpO -- [raw]});
get_list_dir_opt([names_only | Rest], Opts) ->
    get_list_dir_opt(Rest, Opts#list_dir_opts{
			     raw_iterator = fun(A, B, C, D, E) -> raw_name_only(A, B, C, D, E) end});
%% get_list_dir_opt([{file_output, F} | Rest], Opts) ->
%%     get_list_dir_opt(Rest, Opts#list_dir_opts{file_output = F});
%% get_list_dir_opt([{file_filter, F} | Rest], Opts) ->
%%     get_list_dir_opt(Rest, Opts#list_dir_opts{file_filter = F});
get_list_dir_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

get_zip_opt([], Opts) ->
    Opts;
get_zip_opt([verbose | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{feedback = fun verbose_zip/1});
get_zip_opt([cooked | Rest], #zip_opts{open_opts = OpO} = Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{open_opts = OpO -- [raw]});
get_zip_opt([memory | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{output = fun binary_io/2});
get_zip_opt([{cwd, CWD} | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{cwd = CWD});
get_zip_opt([{comment, C} | Rest], Opts) ->
    get_zip_opt(Rest, Opts#zip_opts{comment = C});
get_zip_opt([{compress, Which} = O| Rest], Opts) ->
    Which2 =
	case Which of
	    all ->
		all;
	    Suffixes when is_list(Suffixes) ->
		lists:usort(Suffixes);
	    {add, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.compress ++ Suffixes);
	    {del, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.compress -- Suffixes);
	    _ ->
		throw({bad_option, O})
	end,
    get_zip_opt(Rest, Opts#zip_opts{compress = Which2});
get_zip_opt([{uncompress, Which} = O| Rest], Opts) ->
    Which2 =
	case Which of
	    all ->
		all;
	    Suffixes when is_list(Suffixes) ->
		lists:usort(Suffixes);
	    {add, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.uncompress ++ Suffixes);
	    {del, Suffixes} when is_list(Suffixes) ->
		lists:usort(Opts#zip_opts.uncompress -- Suffixes);
	    _ ->
		throw({bad_option, O})
	end,
    get_zip_opt(Rest, Opts#zip_opts{uncompress = Which2});
get_zip_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).


%% feedback funs
silent(_) -> ok.

verbose_unzip(FN) -> io:format("extracting: ~tp\n", [FN]).

verbose_zip(FN) -> io:format("adding: ~tp\n", [FN]).

%% file filter funs
all(_) -> true.

file_in_list({#zip_file{name = FileName},_}, List) ->
    lists:member(FileName, List);
file_in_list(_, _) ->
    false.

keep_old_file({#zip_file{name = FileName},_}) ->
    not (filelib:is_file(FileName) orelse filelib:is_dir(FileName));
keep_old_file(_) ->
    false.

%% fun combiner
fun_and_1(Fun1, Fun2) ->
    fun(A) -> Fun1(A) andalso Fun2(A) end.

%% getting options
get_zip_options(Files, Options) ->
    Suffixes = [".Z", ".zip", ".zoo", ".arc", ".lzh", ".arj"],
    Opts = #zip_opts{output = fun file_io/2,
		     input = get_zip_input({files, Files}),
		     open_opts = [raw, write],
		     comment = "",
		     feedback = fun silent/1,
		     cwd = "",
		     compress = all,
		     uncompress = Suffixes
		    },
    get_zip_opt(Options, Opts).

get_unzip_options(F, Options) ->
    Opts = #unzip_opts{file_filter = fun all/1,
		       output = fun file_io/2,
		       input = get_input(F),
		       open_opts = [raw],
		       feedback = fun silent/1,
		       cwd = ""
		      },
    get_unzip_opt(Options, Opts).

get_openzip_options(Options) ->
    Opts = #openzip_opts{open_opts = [raw, read],
			 output = fun file_io/2,
			 cwd = ""},
    get_openzip_opt(Options, Opts).

get_input(F) when is_binary(F) ->
    fun binary_io/2;
get_input(F) when is_list(F) ->
    fun file_io/2;
get_input(_) ->
    throw(einval).

get_zip_input({F, B}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input({F, B, #file_info{}}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input({F, #file_info{}, B}) when is_binary(B), is_list(F) ->
    fun binary_io/2;
get_zip_input(F) when is_list(F) ->
    fun file_io/2;
get_zip_input({files, []}) ->
    fun binary_io/2;
get_zip_input({files, [File | _]}) ->
    get_zip_input(File);
get_zip_input(_) ->
    throw(einval).

get_list_dir_options(F, Options) ->
    Opts = #list_dir_opts{raw_iterator = fun raw_file_info_public/5,
			  input = get_input(F),
			  open_opts = [raw]},
    get_list_dir_opt(Options, Opts).

%% aliases for erl_tar compatibility
-spec(table(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()]).

table(F) -> list_dir(F).

-spec(table(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, CommentAndFiles} | {error, Reason :: term()},
      CommentAndFiles :: [zip_comment() | zip_file()],

      Options :: [Option],
      Option :: cooked).

table(F, O) -> list_dir(F, O).

-spec(create(Name, FileList) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      RetValue :: {ok, FileName :: filename()}
                | {ok, {FileName :: filename(), binary()}}
                | {error, Reason :: term()}).

create(F, Fs) -> zip(F, Fs).

-spec(create(Name, FileList, Options) -> RetValue when
      Name     :: file:name(),
      FileList :: [FileSpec],
      FileSpec :: file:name() | {file:name(), binary()}
                | {file:name(), binary(), file:file_info()},
      Options  :: [Option],
      Option   :: create_option(),
      RetValue :: {ok, FileName :: filename()}
                | {ok, {FileName :: filename(), binary()}}
                | {error, Reason :: term()}).
create(F, Fs, O) -> zip(F, Fs, O).

-spec(extract(Archive) -> RetValue when
      Archive :: file:name() | binary(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}]).

extract(F) -> unzip(F).

-spec(extract(Archive, Options) -> RetValue when
      Archive :: file:name() | binary(),
      Options :: [Option],
      Option  :: {file_list, FileList}
               | keep_old_files | verbose | memory |
                 {file_filter, FileFilter} | {cwd, CWD},
      FileList :: [file:name()],
      FileBinList :: [{file:name(),binary()}],
      FileFilter :: fun((ZipFile) -> boolean()),
      CWD :: file:filename(),
      ZipFile :: zip_file(),
      RetValue :: {ok, FileList}
                | {ok, FileBinList}
                | {error, Reason :: term()}
                | {error, {Name :: file:name(), Reason :: term()}}).

extract(F, O) -> unzip(F, O).


%% put the central directory, at the end of the zip archive
put_central_dir(LHS, Pos, Out0,
		#zip_opts{output = Output, comment = Comment}) ->
    {Out1, Sz} = put_cd_files_loop(LHS, Output, Out0, 0),
    put_eocd(length(LHS), Pos, Sz, Comment, Output, Out1).

put_cd_files_loop([], _Output, Out, Sz) ->
    {Out, Sz};
put_cd_files_loop([{LH, Name, Pos} | LHRest], Output, Out0, Sz0) ->
    CDFH = cd_file_header_from_lh_and_pos(LH, Pos),
    BCDFH = cd_file_header_to_bin(CDFH),
    B = [<<?CENTRAL_FILE_MAGIC:32/little>>, BCDFH, Name],
    Out1 = Output({write, B}, Out0),
    Sz1 = Sz0 + ?CENTRAL_FILE_HEADER_SZ +
	LH#local_file_header.file_name_length,
    put_cd_files_loop(LHRest, Output, Out1, Sz1).

%% put end marker of central directory, the last record in the archive
put_eocd(N, Pos, Sz, Comment, Output, Out0) ->
    %% BComment = list_to_binary(Comment),
    CommentSz = length(Comment), % size(BComment),
    EOCD = #eocd{disk_num = 0,
		 start_disk_num = 0,
		 entries_on_disk = N,
		 entries = N,
		 size = Sz,
		 offset = Pos,
		 zip_comment_length = CommentSz},
    BEOCD = eocd_to_bin(EOCD),
    B = [<<?END_OF_CENTRAL_DIR_MAGIC:32/little>>, BEOCD, Comment], % BComment],
    Output({write, B}, Out0).

get_filename({Name, _}, Type) ->
    get_filename(Name, Type);
get_filename({Name, _, _}, Type) ->
    get_filename(Name, Type);
get_filename(Name, regular) ->
    Name;
get_filename(Name, directory) ->
    %% Ensure trailing slash
    case lists:reverse(Name) of
	[$/ | _Rev] -> Name;
	Rev         -> lists:reverse([$/ | Rev])
    end.

add_cwd(_CWD, {_Name, _} = F) -> F;
add_cwd("", F) -> F;
add_cwd(CWD, F) -> filename:join(CWD, F).

%% already compressed data should be stored as is in archive,
%% a simple name-match is used to check for this
%% files smaller than 10 bytes are also stored, not compressed
get_comp_method(_, N, _, _) when is_integer(N), N < 10 ->
    ?STORED;
get_comp_method(_, _, _, directory) ->
    ?STORED;
get_comp_method(F, _, #zip_opts{compress = Compress, uncompress = Uncompress}, _) ->
    Ext = filename:extension(F),
    Test = fun(Which) -> (Which =:= all) orelse lists:member(Ext, Which) end,
    case Test(Compress) andalso not Test(Uncompress) of
	true  -> ?DEFLATED;
	false -> ?STORED
    end.

put_z_files([], _Z, Out, Pos, _Opts, Acc) ->
    {Out, lists:reverse(Acc, []), Pos};
put_z_files([F | Rest], Z, Out0, Pos0,
	    #zip_opts{input = Input, output = Output, open_opts = OpO,
		      feedback = FB, cwd = CWD} = Opts, Acc) ->
    In0 = [],
    F1 = add_cwd(CWD, F),
    FileInfo = Input({file_info, F1}, In0),
    Type = FileInfo#file_info.type,
    UncompSize =
	case Type of
	    regular -> FileInfo#file_info.size;
	    directory -> 0
	end,
    FileName = get_filename(F, Type),
    CompMethod = get_comp_method(FileName, UncompSize, Opts, Type),
    LH = local_file_header_from_info_method_name(FileInfo, UncompSize, CompMethod, FileName),
    BLH = local_file_header_to_bin(LH),
    B = [<<?LOCAL_FILE_MAGIC:32/little>>, BLH],
    Out1 = Output({write, B}, Out0),
    Out2 = Output({write, FileName}, Out1),
    {Out3, CompSize, CRC} = put_z_file(CompMethod, UncompSize, Out2, F1,
				       0, Input, Output, OpO, Z, Type),
    FB(FileName),
    Patch = <<CRC:32/little, CompSize:32/little>>,
    Out4 = Output({pwrite, Pos0 + ?LOCAL_FILE_HEADER_CRC32_OFFSET, Patch}, Out3),
    Out5 = Output({seek, eof, 0}, Out4),
    Pos1 = Pos0 + ?LOCAL_FILE_HEADER_SZ	+ LH#local_file_header.file_name_length,
    Pos2 = Pos1 + CompSize,
    LH2 = LH#local_file_header{comp_size = CompSize, crc32 = CRC},
    ThisAcc = [{LH2, FileName, Pos0}],
    {Out6, SubAcc, Pos3} =
	case Type of
	    regular ->
		{Out5, ThisAcc, Pos2};
	    directory ->
		Files = Input({list_dir, F1}, []),
		RevFiles = reverse_join_files(F, Files, []),
		put_z_files(RevFiles, Z, Out5, Pos2, Opts, ThisAcc)
	end,
    Acc2 = lists:reverse(SubAcc) ++ Acc,
    put_z_files(Rest, Z, Out6, Pos3, Opts, Acc2).

reverse_join_files(Dir, [File | Files], Acc) ->
    reverse_join_files(Dir, Files, [filename:join([Dir, File]) | Acc]);
reverse_join_files(_Dir, [], Acc) ->
    Acc.

%% flag for zlib
-define(MAX_WBITS, 15).

%% compress a file
put_z_file(_Method, Sz, Out, _F, Pos, _Input, _Output, _OpO, _Z, directory) ->
    {Out, Pos + Sz, 0};
put_z_file(_Method, 0, Out, _F, Pos, _Input, _Output, _OpO, _Z, regular) ->
    {Out, Pos, 0};
put_z_file(?STORED, UncompSize, Out0, F, Pos0, Input, Output, OpO, Z, regular) ->
    In0 = [],
    In1 = Input({open, F, OpO -- [write]}, In0),
    CRC0 = zlib:crc32(Z, <<>>),
    {Data, In2} = Input({read, UncompSize}, In1),
    Out1 = Output({write, Data}, Out0),
    CRC = zlib:crc32(Z, CRC0, Data),
    Input(close, In2),
    {Out1, Pos0+erlang:iolist_size(Data), CRC};
put_z_file(?DEFLATED, UncompSize, Out0, F, Pos0, Input, Output, OpO, Z, regular) ->
    In0 = [],
    In1 = Input({open, F, OpO -- [write]}, In0),
    ok = zlib:deflateInit(Z, default, deflated, -?MAX_WBITS, 8, default),
    {Out1, Pos1} =
	put_z_data_loop(UncompSize, In1, Out0, Pos0, Input, Output, Z),
    CRC = zlib:crc32(Z),
    ok = zlib:deflateEnd(Z),
    Input(close, In1),
    {Out1, Pos1, CRC}.

%%  zlib is finished with the last chunk compressed
get_sync(N, N) -> finish;
get_sync(_, _) -> full.

%% compress data
put_z_data_loop(0, _In, Out, Pos, _Input, _Output, _Z) ->
    {Out, Pos};
put_z_data_loop(UncompSize, In0, Out0, Pos0, Input, Output, Z) ->
    N = erlang:min(?WRITE_BLOCK_SIZE, UncompSize),
    case Input({read, N}, In0) of
	{eof, _In1} ->
	    {Out0, Pos0};
	{Uncompressed, In1} ->
	    Compressed = zlib:deflate(Z, Uncompressed, get_sync(N, UncompSize)),
	    Sz = erlang:iolist_size(Compressed),
	    Out1 = Output({write, Compressed}, Out0),
	    put_z_data_loop(UncompSize - N, In1, Out1, Pos0 + Sz,
			      Input, Output, Z)
    end.

%% raw iterators over central dir

%% name only
raw_name_only(CD, FileName, _FileComment, _BExtraField, Acc)
  when is_record(CD, cd_file_header) ->
    [FileName | Acc];
raw_name_only(EOCD, _, _Comment, _, Acc) when is_record(EOCD, eocd) ->
    Acc.

%% for printing directory (t/1)
raw_short_print_info_etc(CD, FileName, _FileComment, _BExtraField, Acc)
  when is_record(CD, cd_file_header) ->
    print_file_name(FileName),
    Acc;
raw_short_print_info_etc(EOCD, X, Comment, Y, Acc) when is_record(EOCD, eocd) ->
    raw_long_print_info_etc(EOCD, X, Comment, Y, Acc).

print_file_name(FileName) ->
    io:format("~ts\n", [FileName]).


%% for printing directory (tt/1)
raw_long_print_info_etc(#cd_file_header{comp_size = CompSize,
					uncomp_size = UncompSize,
					last_mod_date = LMDate,
					last_mod_time = LMTime},
			FileName, FileComment, _BExtraField, Acc) ->
    MTime = dos_date_time_to_datetime(LMDate, LMTime),
    print_header(CompSize, MTime, UncompSize, FileName, FileComment),
    Acc;
raw_long_print_info_etc(EOCD, _, Comment, _, Acc) when is_record(EOCD, eocd) ->
    print_comment(Comment),
    Acc.

print_header(CompSize, MTime, UncompSize, FileName, FileComment) ->
    io:format("~8w ~s ~8w ~2w% ~ts ~ts\n",
	      [CompSize, time_to_string(MTime), UncompSize,
	       get_percent(CompSize, UncompSize), FileName, FileComment]).

print_comment("") ->
    ok;
print_comment(Comment) ->
    io:format("Archive comment: ~ts\n", [Comment]).

get_percent(_, 0) -> 100;
get_percent(CompSize, Size) -> round(CompSize * 100 / Size).

%% time formatting ("borrowed" from erl_tar.erl)
time_to_string({{Y, Mon, Day}, {H, Min, _}}) ->
    io_lib:format("~s ~2w ~s:~s ~w",
		  [month(Mon), Day, two_d(H), two_d(Min), Y]).

two_d(N) ->
    tl(integer_to_list(N + 100)).

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".

%% zip header functions
cd_file_header_from_lh_and_pos(LH, Pos) ->
    #local_file_header{version_needed = VersionNeeded,
		       gp_flag = GPFlag,
		       comp_method = CompMethod,
		       last_mod_time = LastModTime,
		       last_mod_date = LastModDate,
		       crc32 = CRC32,
		       comp_size = CompSize,
		       uncomp_size = UncompSize,
		       file_name_length = FileNameLength,
		       extra_field_length = ExtraFieldLength} = LH,
    #cd_file_header{version_made_by = 20,
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
		    file_comment_length = 0, % FileCommentLength,
		    disk_num_start = 0, % DiskNumStart,
		    internal_attr = 0, % InternalAttr,
		    external_attr = 0, % ExternalAttr,
		    local_header_offset = Pos}.

cd_file_header_to_bin(
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
		  local_header_offset = LocalHeaderOffset}) ->
    <<VersionMadeBy:16/little,
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
     LocalHeaderOffset:32/little>>.

local_file_header_to_bin(
  #local_file_header{version_needed = VersionNeeded,
		     gp_flag = GPFlag,
		     comp_method = CompMethod,
		     last_mod_time = LastModTime,
		     last_mod_date = LastModDate,
		     crc32 = CRC32,
		     comp_size = CompSize,
		     uncomp_size = UncompSize,
		     file_name_length = FileNameLength,
		     extra_field_length = ExtraFieldLength}) ->
    <<VersionNeeded:16/little,
     GPFlag:16/little,
     CompMethod:16/little,
     LastModTime:16/little,
     LastModDate:16/little,
     CRC32:32/little,
     CompSize:32/little,
     UncompSize:32/little,
     FileNameLength:16/little,
     ExtraFieldLength:16/little>>.

eocd_to_bin(#eocd{disk_num = DiskNum,
	   start_disk_num = StartDiskNum,
	   entries_on_disk = EntriesOnDisk,
	   entries = Entries,
	   size = Size,
	   offset = Offset,
	   zip_comment_length = ZipCommentLength}) ->
    <<DiskNum:16/little,
     StartDiskNum:16/little,
     EntriesOnDisk:16/little,
     Entries:16/little,
     Size:32/little,
     Offset:32/little,
     ZipCommentLength:16/little>>.

%% put together a local file header
local_file_header_from_info_method_name(#file_info{mtime = MTime},
					UncompSize,
					CompMethod, Name) ->
    {ModDate, ModTime} = dos_date_time_from_datetime(MTime),
    #local_file_header{version_needed = 20,
		       gp_flag = 0,
		       comp_method = CompMethod,
		       last_mod_time = ModTime,
		       last_mod_date = ModDate,
		       crc32 = -1,
		       comp_size = -1,
		       uncomp_size = UncompSize,
		       file_name_length = length(Name),
		       extra_field_length = 0}.

server_init(Parent) ->
    %% we want to know if our parent dies
    process_flag(trap_exit, true),
    server_loop(Parent, not_open).

%% small, simple, stupid zip-archive server
server_loop(Parent, OpenZip) ->
    receive
	{From, {open, Archive, Options}} ->
	    case openzip_open(Archive, Options) of
		{ok, NewOpenZip} ->
		    From ! {self(), {ok, self()}},
		    server_loop(Parent, NewOpenZip);
		Error ->
		    From ! {self(), Error}
	    end;
	{From, close} ->
	    From ! {self(), openzip_close(OpenZip)};
	{From, get} ->
	    From ! {self(), openzip_get(OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, {get, FileName}} ->
	    From ! {self(), openzip_get(FileName, OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, list_dir} ->
	    From ! {self(), openzip_list_dir(OpenZip)},
	    server_loop(Parent, OpenZip);
	{From, {list_dir, Opts}} ->
	    From ! {self(), openzip_list_dir(OpenZip, Opts)},
	    server_loop(Parent, OpenZip);
	{From, get_state} ->
	    From ! {self(), OpenZip},
	    server_loop(Parent, OpenZip);
        {'EXIT', Parent, Reason} ->
            _ = openzip_close(OpenZip),
            exit({parent_died, Reason});
	_ ->
	    {error, bad_msg}
    end.

-spec(zip_open(Archive) -> {ok, ZipHandle} | {error, Reason} when
      Archive :: file:name() | binary(),
      ZipHandle :: handle(),
      Reason :: term()).

zip_open(Archive) -> zip_open(Archive, []).

-spec(zip_open(Archive, Options) -> {ok, ZipHandle} | {error, Reason} when
      Archive :: file:name() | binary(),
      ZipHandle :: handle(),
      Options :: [Option],
      Option :: cooked | memory | {cwd, CWD :: file:filename()},
      Reason :: term()).

zip_open(Archive, Options) ->
    Self = self(),
    Pid = spawn_link(fun() -> server_init(Self) end),
    request(Self, Pid, {open, Archive, Options}).

-spec(zip_get(ZipHandle) -> {ok, [Result]} | {error, Reason} when
      ZipHandle :: handle(),
      Result :: file:name() | {file:name(), binary()},
      Reason :: term()).

zip_get(Pid) when is_pid(Pid) ->
    request(self(), Pid, get).

-spec(zip_close(ZipHandle) -> ok | {error, einval} when
      ZipHandle :: handle()).

zip_close(Pid) when is_pid(Pid) ->
    request(self(), Pid, close).

-spec(zip_get(FileName, ZipHandle) -> {ok, Result} | {error, Reason} when
      FileName :: file:name(),
      ZipHandle :: handle(),
      Result :: file:name() | {file:name(), binary()},
      Reason :: term()).

zip_get(FileName, Pid) when is_pid(Pid) ->
    request(self(), Pid, {get, FileName}).

-spec(zip_list_dir(ZipHandle) -> {ok, Result} | {error, Reason} when
      Result :: [zip_comment() | zip_file()],
      ZipHandle :: handle(),
      Reason :: term()).

zip_list_dir(Pid) when is_pid(Pid) ->
    request(self(), Pid, list_dir).

zip_list_dir(Pid, Opts) when is_pid(Pid) ->
    request(self(), Pid, {list_dir, Opts}).

zip_get_state(Pid) when is_pid(Pid) ->
    request(self(), Pid, get_state).

request(Self, Pid, Req) ->
    Pid ! {Self, Req},
    receive
	{Pid, R} -> R
    end.

zip_t(Pid) when is_pid(Pid) ->
    Openzip = request(self(), Pid, get_state),
    openzip_t(Openzip).

zip_tt(Pid) when is_pid(Pid) ->
    Openzip = request(self(), Pid, get_state),
    openzip_tt(Openzip).

openzip_tt(#openzip{zip_comment = ZipComment, files = Files}) ->
    print_comment(ZipComment),
    lists_foreach(fun({#zip_file{comp_size = CompSize,
				name = FileName,
				comment = FileComment,
				info = FI},_}) ->
			  #file_info{size = UncompSize, mtime = MTime} = FI,
			  print_header(CompSize, MTime, UncompSize,
				       FileName, FileComment)
		  end, Files),
    ok.

openzip_t(#openzip{zip_comment = ZipComment, files = Files}) ->
    print_comment(ZipComment),
    lists_foreach(fun({#zip_file{name = FileName},_}) ->
			  print_file_name(FileName)
		  end, Files),
    ok.

lists_foreach(_, []) ->
    ok;
lists_foreach(F, [Hd|Tl]) ->
    F(Hd),
    lists_foreach(F, Tl).

%% option utils
get_openzip_opt([], Opts) ->
    Opts;
get_openzip_opt([cooked | Rest], #openzip_opts{open_opts = OO} = Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{open_opts = OO -- [raw]});
get_openzip_opt([memory | Rest], Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{output = fun binary_io/2});
get_openzip_opt([{cwd, CWD} | Rest], Opts) ->
    get_openzip_opt(Rest, Opts#openzip_opts{cwd = CWD});
get_openzip_opt([Unknown | _Rest], _Opts) ->
    throw({bad_option, Unknown}).

%% get the central directory from the archive
get_central_dir(In0, RawIterator, Input) ->
    {B, In1} = get_end_of_central_dir(In0, ?END_OF_CENTRAL_DIR_SZ, Input),
    {EOCD, BComment} = eocd_and_comment_from_bin(B),
    In2 = Input({seek, bof, EOCD#eocd.offset}, In1),
    N = EOCD#eocd.entries,
    Acc0 = [],
    Out0 = RawIterator(EOCD, "", binary_to_list(BComment), <<>>, Acc0),
    get_cd_loop(N, In2, RawIterator, Input, Out0).

get_cd_loop(0, In, _RawIterator, _Input, Acc) ->
    {lists:reverse(Acc), In};
get_cd_loop(N, In0, RawIterator, Input, Acc0) ->
    {B, In1} = Input({read, ?CENTRAL_FILE_HEADER_SZ}, In0),
    BCD = case B of
	      <<?CENTRAL_FILE_MAGIC:32/little, XBCD/binary>> -> XBCD;
	      _ -> throw(bad_central_directory)
	  end,
    CD = cd_file_header_from_bin(BCD),
    FileNameLen = CD#cd_file_header.file_name_length,
    ExtraLen = CD#cd_file_header.extra_field_length,
    CommentLen = CD#cd_file_header.file_comment_length,
    ToRead = FileNameLen + ExtraLen + CommentLen,
    {B2, In2} = Input({read, ToRead}, In1),
    {FileName, Comment, BExtra} =
	get_name_extra_comment(B2, FileNameLen, ExtraLen, CommentLen),
    Acc1 = RawIterator(CD, FileName, Comment, BExtra, Acc0),
    get_cd_loop(N-1, In2, RawIterator, Input, Acc1).

get_name_extra_comment(B, FileNameLen, ExtraLen, CommentLen) ->
    case B of
	<<BFileName:FileNameLen/binary,
	 BExtra:ExtraLen/binary,
	 BComment:CommentLen/binary>> ->
	    {binary_to_list(BFileName), binary_to_list(BComment), BExtra};
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

%% from a central directory record, filter and accumulate what we need

%% with zip_file_extra
raw_file_info_etc(CD, FileName, FileComment, BExtraField, Acc)
  when is_record(CD, cd_file_header) ->
    #cd_file_header{comp_size = CompSize,
		    local_header_offset = Offset,
		    crc32 = CRC} = CD,
    FileInfo = cd_file_header_to_file_info(FileName, CD, BExtraField),
    [{#zip_file{name = FileName, info = FileInfo, comment = FileComment,
		offset = Offset, comp_size = CompSize}, #zip_file_extra{crc32 = CRC}} | Acc];
raw_file_info_etc(EOCD, _, Comment, _, Acc) when is_record(EOCD, eocd) ->
    [#zip_comment{comment = Comment} | Acc].

%% without zip_file_extra
raw_file_info_public(CD, FileName, FileComment, BExtraField, Acc0) ->
    [H1|T] = raw_file_info_etc(CD,FileName,FileComment,BExtraField,Acc0),
    H2 = case H1 of
	     {ZF,Extra} when is_record(Extra,zip_file_extra) -> ZF;
	     Other -> Other
	 end,
    [H2|T].


%% make a file_info from a central directory header
cd_file_header_to_file_info(FileName,
			    #cd_file_header{uncomp_size = UncompSize,
					    last_mod_time = ModTime,
					    last_mod_date = ModDate},
			    ExtraField) ->
    T = dos_date_time_to_datetime(ModDate, ModTime),
    Type =
	case lists:last(FileName) of
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

%% Currently, we ignore all the extra fields.
add_extra_info(FI, _) ->
    FI.



%% get all files using file list
%% (the offset list is already filtered on which file to get... isn't it?)
get_z_files([], _Z, _In, _Opts, Acc) ->
    lists:reverse(Acc);
get_z_files([#zip_comment{comment = _} | Rest], Z, In, Opts, Acc) ->
    get_z_files(Rest, Z, In, Opts, Acc);
get_z_files([{#zip_file{offset = Offset},_} = ZFile | Rest], Z, In0,
	    #unzip_opts{input = Input, output = Output, open_opts = OpO,
			file_filter = Filter, feedback = FB,
			cwd = CWD} = Opts, Acc0) ->
    case Filter(ZFile) of
	true ->
	    In1 = Input({seek, bof, Offset}, In0),
	    {In2, Acc1} =
		case get_z_file(In1, Z, Input, Output, OpO, FB,
				CWD, ZFile, Filter) of
		    {file, GZD, Inx} -> {Inx, [GZD | Acc0]};
		    {_, Inx}       -> {Inx, Acc0}
		end,
	    get_z_files(Rest, Z, In2, Opts, Acc1);
	_ ->
	    get_z_files(Rest, Z, In0, Opts, Acc0)
    end.

%% get a file from the archive, reading chunks
get_z_file(In0, Z, Input, Output, OpO, FB,
	   CWD, {ZipFile,Extra}, Filter) ->
    case Input({read, ?LOCAL_FILE_HEADER_SZ}, In0) of
	{eof, In1} ->
	    {eof, In1};
	%% Local File Header
	{<<?LOCAL_FILE_MAGIC:32/little, B/binary>>, In1} ->
	    LH = local_file_header_from_bin(B),
	    #local_file_header{gp_flag = GPFlag,
			       comp_method = CompMethod,
			       file_name_length = FileNameLen,
			       extra_field_length = ExtraLen} = LH,

	    {CompSize,CRC32} = case GPFlag band 8 =:= 8 of
				   true -> {ZipFile#zip_file.comp_size,
					    Extra#zip_file_extra.crc32};
				   false -> {LH#local_file_header.comp_size,
					     LH#local_file_header.crc32}
			       end,
	    {BFileN, In3} = Input({read, FileNameLen + ExtraLen}, In1),
	    {FileName, _} = get_file_name_extra(FileNameLen, ExtraLen, BFileN),
	    ReadAndWrite =
		case check_valid_location(CWD, FileName) of
		    {true,FileName1} ->
			true;
		    {false,FileName1} ->
			Filter({ZipFile#zip_file{name = FileName1},Extra})
		end,
	    case ReadAndWrite of
		true ->
		    case lists:last(FileName) of
			$/ ->
			    %% perhaps this should always be done?
			    Output({ensure_dir,FileName1},[]),
			    {dir, In3};
			_ ->
			    %% FileInfo = local_file_header_to_file_info(LH)
			    %%{Out, In4, CRC, UncompSize} =
			    {Out, In4, CRC, _UncompSize} =
				get_z_data(CompMethod, In3, FileName1,
					   CompSize, Input, Output, OpO, Z),
			    In5 = skip_z_data_descriptor(GPFlag, Input, In4),
			    %% TODO This should be fixed some day:
			    %% In5 = Input({set_file_info, FileName, 
			    %% FileInfo#file_info{size=UncompSize}}, In4),
			    FB(FileName),
			    CRC =:= CRC32 orelse throw({bad_crc, FileName}),
			    {file, Out, In5}
		    end;
		false ->
		    {ignore, In3}
	    end;
	_ ->
	    throw(bad_local_file_header)
    end.

%% make sure FileName doesn't have relative path that points over CWD
check_valid_location(CWD, FileName) ->
    %% check for directory traversal exploit
    case check_dir_level(filename:split(FileName), 0) of
	{FileOrDir,Level} when Level < 0 ->
	    CWD1 = if CWD == "" -> "./";
		      true      -> CWD
		   end,
	    error_logger:format("Illegal path: ~ts, extracting in ~ts~n",
				[add_cwd(CWD,FileName),CWD1]),
	    {false,add_cwd(CWD, FileOrDir)};
        _ ->
	    {true,add_cwd(CWD, FileName)}
    end.

check_dir_level([FileOrDir], Level) ->
    {FileOrDir,Level};
check_dir_level(["." | Parts], Level) ->
    check_dir_level(Parts, Level);
check_dir_level([".." | Parts], Level) ->
    check_dir_level(Parts, Level-1);
check_dir_level([_Dir | Parts], Level) ->
    check_dir_level(Parts, Level+1).

get_file_name_extra(FileNameLen, ExtraLen, B) ->
    case B of
	<<BFileName:FileNameLen/binary, BExtra:ExtraLen/binary>> ->
	    {binary_to_list(BFileName), BExtra};
	_ ->
	    throw(bad_file_header)
    end.

%% get compressed or stored data
get_z_data(?DEFLATED, In0, FileName, CompSize, Input, Output, OpO, Z) ->
    ok = zlib:inflateInit(Z, -?MAX_WBITS),
    Out0 = Output({open, FileName, [write | OpO]}, []),
    {In1, Out1, UncompSize} = get_z_data_loop(CompSize, 0, In0, Out0, Input, Output, Z),
    CRC = zlib:crc32(Z),
    ?CATCH zlib:inflateEnd(Z),
    Out2 = Output({close, FileName}, Out1),
    {Out2, In1, CRC, UncompSize};
get_z_data(?STORED, In0, FileName, CompSize, Input, Output, OpO, Z) ->
    Out0 = Output({open, FileName, [write | OpO]}, []),
    CRC0 = zlib:crc32(Z, <<>>),
    {In1, Out1, CRC} = copy_data_loop(CompSize, In0, Out0, Input, Output,
				      CRC0, Z),
    Out2 = Output({close, FileName}, Out1),
    {Out2, In1, CRC, CompSize};
get_z_data(_, _, _, _, _, _, _, _) ->
    throw(bad_file_header).

copy_data_loop(0, In, Out, _Input, _Output, CRC, _Z) ->
    {In, Out, CRC};
copy_data_loop(CompSize, In0, Out0, Input, Output, CRC0, Z) ->
    N = erlang:min(?READ_BLOCK_SIZE, CompSize),
    case Input({read, N}, In0) of
	{eof, In1} -> {Out0, In1};
	{Uncompressed, In1} ->
	    CRC1 = zlib:crc32(Z, CRC0, Uncompressed),
	    Out1 = Output({write, Uncompressed}, Out0),
	    copy_data_loop(CompSize-N, In1, Out1, Input, Output, CRC1, Z)
    end.

get_z_data_loop(0, UncompSize, In, Out, _Input, _Output, _Z) ->
    {In, Out, UncompSize};
get_z_data_loop(CompSize, UncompSize, In0, Out0, Input, Output, Z) ->
    N = erlang:min(?READ_BLOCK_SIZE, CompSize),
    case Input({read, N}, In0) of
	{eof, In1} ->
	    {Out0, In1};
	{Compressed, In1} ->
	    Uncompressed = zlib:inflate(Z, Compressed),
	    Out1 = Output({write, Uncompressed}, Out0),
	    get_z_data_loop(CompSize-N, UncompSize + iolist_size(Uncompressed),
			    In1, Out1, Input, Output, Z)
    end.


%% skip data descriptor if any
skip_z_data_descriptor(GPFlag, Input, In0) when GPFlag band 8 =:= 8 ->
    Input({seek, cur, 12}, In0);
skip_z_data_descriptor(_GPFlag, _Input, In0) ->
    In0.

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

dos_date_time_from_datetime({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    YearFrom1980 = Year-1980,
    <<DosTime:16>> = <<Hour:5, Min:6, Sec:5>>,
    <<DosDate:16>> = <<YearFrom1980:7, Month:4, Day:5>>,
    {DosDate, DosTime}.

%% A pwrite-like function for iolists (used by memory-option)

pwrite_binary(B, Pos, Bin) when byte_size(B) =:= Pos ->
    append_bins(Bin, B);
pwrite_binary(B, Pos, Bin) ->
    erlang:iolist_to_binary(pwrite_iolist(B, Pos, Bin)).

append_bins([Bin|Bins], B) when is_binary(Bin) ->
    append_bins(Bins, <<B/binary, Bin/binary>>);
append_bins([List|Bins], B) when is_list(List) ->
    append_bins(Bins, append_bins(List, B));
append_bins(Bin, B) when is_binary(Bin) ->
    <<B/binary, Bin/binary>>;
append_bins([_|_]=List, B) ->
    <<B/binary, (iolist_to_binary(List))/binary>>;
append_bins([], B) ->
    B.

-dialyzer({no_improper_lists, pwrite_iolist/3}).

pwrite_iolist(B, Pos, Bin) ->
    {Left, Right} = split_binary(B, Pos),
    Sz = erlang:iolist_size(Bin),
    R = skip_bin(Right, Sz),
    [Left, Bin | R].

skip_bin(B, Pos) when is_binary(B) ->
    case B of
	<<_:Pos/binary, Bin/binary>> -> Bin;
	_ -> <<>>
    end.


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
			    ExtraFieldLength:16/little>>) ->
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
local_file_header_from_bin(_) ->
    throw(bad_local_file_header).

%% make a file_info from a local directory header
%% local_file_header_to_file_info(
%%   #local_file_header{last_mod_time = ModTime,
%% 		     last_mod_date = ModDate,
%% 		     uncomp_size = UncompSize}) ->
%%     T = dos_date_time_to_datetime(ModDate, ModTime),
%%     FI = #file_info{size = UncompSize,
%% 		    type = regular,
%% 		    access = read_write,
%% 		    atime = T,
%% 		    mtime = T,
%% 		    ctime = T,
%% 		    mode = 8#066,
%% 		    links = 1,
%% 		    major_device = 0,
%% 		    minor_device = 0,
%% 		    inode = 0,
%% 		    uid = 0,
%% 		    gid = 0},
%%     FI.

%% io functions
binary_io({file_info, {_Filename, _B, #file_info{} = FI}}, _A) ->
    FI;
binary_io({file_info, {_Filename, #file_info{} = FI, _B}}, _A) ->
    FI;
binary_io({file_info, {_Filename, B}}, A) ->
    binary_io({file_info, B}, A);
binary_io({file_info, B}, _) ->
    {Type, Size} =
	if
	    is_binary(B) -> {regular, byte_size(B)};
	    B =:= directory -> {directory, 0}
	end,
    Now = calendar:local_time(),
    #file_info{size = Size, type = Type,
	       access = read_write, atime = Now,
	       mtime = Now, ctime = Now, mode = 0,
	       links = 1, major_device = 0,
	       minor_device = 0, inode = 0,
	       uid = 0, gid = 0};
binary_io({open, {_Filename, B, _FI}, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, {_Filename, _FI, B}, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, {_Filename, B}, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, B, _Opts}, _) when is_binary(B) ->
    {0, B};
binary_io({open, Filename, _Opts}, _) when is_list(Filename) ->
    {0, <<>>};
binary_io({read, N}, {Pos, B}) when Pos >= byte_size(B) ->
    {eof, {Pos+N, B}};
binary_io({read, N}, {Pos, B}) when Pos + N > byte_size(B) ->
    <<_:Pos/binary, Read/binary>> = B,
    {Read, {byte_size(B), B}};
binary_io({pread, Pos, N}, {OldPos, B}) ->
    case B of
	<<_:Pos/binary, Read:N/binary, _Rest/binary>> ->
	    {Read, {Pos+N, B}};
	_ ->
	    {eof, {OldPos, B}}
    end;
binary_io({read, N}, {Pos, B}) ->
    <<_:Pos/binary, Read:N/binary, _/binary>> = B,
    {Read, {Pos+N, B}};
binary_io({seek, bof, Pos}, {_OldPos, B}) ->
    {Pos, B};
binary_io({seek, cur, Pos}, {OldPos, B}) ->
    {OldPos + Pos, B};
binary_io({seek, eof, Pos}, {_OldPos, B}) ->
    {byte_size(B) + Pos, B};
binary_io({pwrite, Pos, Data}, {OldPos, B}) ->
    {OldPos, pwrite_binary(B, Pos, Data)};
binary_io({write, Data}, {Pos, B}) ->
    {Pos + erlang:iolist_size(Data), pwrite_binary(B, Pos, Data)};
binary_io(close, {_Pos, B}) ->
    B;
binary_io({close, FN}, {_Pos, B}) ->
    {FN, B};
binary_io({list_dir, _F}, _B) ->
    [];
binary_io({set_file_info, _F, _FI}, B) ->
    B;
binary_io({ensure_dir, _Dir}, B) ->
    B.

file_io({file_info, F}, _) ->
    case file:read_file_info(F) of
	{ok, Info} -> Info;
	{error, E} -> throw(E)
    end;
file_io({open, FN, Opts}, _) ->
    case lists:member(write, Opts) of
	true -> ok = filelib:ensure_dir(FN);
	_ -> ok
    end,
    case file:open(FN, Opts++[binary]) of
	{ok, H} -> H;
	{error, E} -> throw(E)
    end;
file_io({read, N}, H) ->
    case file:read(H, N) of
	{ok, B} -> {B, H};
	eof -> {eof, H};
	{error, E} -> throw(E)
    end;
file_io({pread, Pos, N}, H) ->
    case file:pread(H, Pos, N) of
	{ok, B} -> {B, H};
	eof -> {eof, H};
	{error, E} -> throw(E)
    end;
file_io({seek, S, Pos}, H) ->
    case file:position(H, {S, Pos}) of
	{ok, _NewPos} -> H;
	{error, Error} -> throw(Error)
    end;
file_io({write, Data}, H) ->
    case file:write(H, Data) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({pwrite, Pos, Data}, H) ->
    case file:pwrite(H, Pos, Data) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({close, FN}, H) ->
    case file:close(H) of
	ok -> FN;
	{error, Error} -> throw(Error)
    end;
file_io(close, H) ->
    file_io({close, ok}, H);
file_io({list_dir, F}, _H) ->
    case file:list_dir(F) of
	{ok, Files} -> Files;
	{error, Error} -> throw(Error)
    end;
file_io({set_file_info, F, FI}, H) ->
    case file:write_file_info(F, FI) of
	ok -> H;
	{error, Error} -> throw(Error)
    end;
file_io({ensure_dir, Dir}, H) ->
    ok = filelib:ensure_dir(Dir),
    H.
