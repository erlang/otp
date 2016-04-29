%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-define(DEFAULT_MIN_NO_SLOTS, 256).
-define(DEFAULT_MAX_NO_SLOTS, 32*1024*1024).
-define(DEFAULT_AUTOSAVE, 3). % minutes
-define(DEFAULT_CACHE, {3000, 14000}). % {delay,size} in {milliseconds,bytes}

%% Type.
-define(SET, 1).
-define(BAG, 2).
-define(DUPLICATE_BAG, 3).

-define(MAGIC, 16#0abcdef).   % dets cookie, won't ever change.
%% Status values.
-define(FREE, 16#3abcdef).
-define(ACTIVE, 16#12345678).

-define(FILE_FORMAT_VERSION_POS, 16).

-define(CHUNK_SIZE, 8192).

-define(SERVER_NAME, dets).

-define(POW(X), (1 bsl (X))).

%% REM2(A,B) = A rem B, if B is a power of 2.
-define(REM2(A, B), ((A) band ((B)-1))).

-define(DETS_CALL(Pid, Req), {'$dets_call', Pid, Req}).

%% Record holding the file header and more.
-record(head,  {
	  m,               % size
	  m2,              % m * 2
	  next,            % next position for growth (segm mgmt only)
	  fptr,            % the file descriptor
	  no_objects,      % number of objects in table,
	  no_keys,         % number of keys (version 9 only)
	  maxobjsize,      % 2-log of the size of the biggest object
                           % collection (version 9 only)
	  n,               % split indicator
	  type,            % set | bag | duplicate_bag
	  keypos,          % default is 1 as for ets
	  freelists,       % tuple of free lists of buddies
	                   % if fixed =/= false, then a pair of freelists
	  freelists_p,     % cached FreelistsPointer
	  no_collections,  % [{LogSize,NoCollections}] | undefined; number of
	                   % object collections per size (version 9(b))
	  auto_save,       % Integer | infinity 
	  update_mode,     % saved | dirty | new_dirty | {error, Reason}
	  fixed = false,   % false | {now_time(), [{pid(),Counter}]}
                           % time of first fix, and number of fixes per process
	  hash_bif,        % hash bif used for this file (phash2, phash, hash)
          has_md5,         % whether the header has an MD5 sum (version 9(c))
	  min_no_slots,    % minimum number of slots (default or integer)
	  max_no_slots,    % maximum number of slots (default or integer)
	  cache,           % cache(). Write cache.

	  filename,             % name of the file being used
	  access = read_write,  % read | read_write
	  ram_file = false,     % true | false
	  name,                 % the name of the table

	  parent,               % The supervisor of Dets processes.
	  server,               % The creator of Dets processes.

          %% Depending on the file format:
          version,
          mod,
          bump,
          base

	 }).

%% Info extracted from the file header.
-record(fileheader, {
	  freelist,
          fl_base,
	  cookie,
	  closed_properly,
	  type,
	  version,
	  m,
	  next,
	  keypos,
	  no_objects,
	  no_keys,
	  min_no_slots,
	  max_no_slots,
	  no_colls,
	  hash_method,
          read_md5,
          has_md5,
          md5,
	  trailer,
	  eof,
	  n,
	  mod
	}).

%% Write Cache.
-record(cache, {
	 cache,   % [{Key,{Seq,Item}}], write cache, last item first
         csize,   % current size of the cached items
         inserts, % upper limit on number of inserted keys
	 wrtime,  % last write or update time
	 tsize,   % threshold size of cache, in bytes
	 delay    % max time items are kept in RAM only, in milliseconds
	 }).

