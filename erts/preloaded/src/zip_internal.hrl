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

%% Unix extra fields (not yet supported)
-define(UNIX_EXTRA_FIELD_TAG, 16#000d).
-record(unix_extra_field, {atime,
			   mtime,
			   uid,
			   gid}).

%% extended timestamps (not yet supported)
-define(EXTENDED_TIMESTAMP_TAG, 16#5455).
-record(extended_timestamp, {mtime,
			     atime,
			     ctime}).

-define(END_OF_CENTRAL_DIR_MAGIC, 16#06054b50).
-define(END_OF_CENTRAL_DIR_SZ, (4+2+2+2+2+4+4+2)).

-record(eocd, {disk_num,
	       start_disk_num,
	       entries_on_disk,
	       entries,
	       size,
	       offset,
	       zip_comment_length}).


