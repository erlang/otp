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
-define(DEFAULT_CACHE, {3000, 14000}). % cache_parms()

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

-type access()      :: 'read' | 'read_write'.
-type auto_save()   :: 'infinity' | non_neg_integer().
-type hash_bif()    :: 'phash' | 'phash2'.
-type keypos()      :: pos_integer().
-type no_colls()    :: [{LogSize :: non_neg_integer(),
                         NoCollections :: non_neg_integer()}].
-type no_slots()    :: 'default' | non_neg_integer().
-type tab_name()    :: term().
-type type()        :: 'bag' | 'duplicate_bag' | 'set'.
-type update_mode() :: 'dirty'
                     | 'new_dirty'
                     | 'saved'
                     | {'error', Reason :: term()}.

%% Record holding the file header and more.
-record(head,  {
	  m :: non_neg_integer(),    % size
	  m2 :: non_neg_integer(),   % m * 2
	  next :: non_neg_integer(), % next position for growth
                                     % (segm mgmt only)
	  fptr :: file:fd(),         % the file descriptor
	  no_objects :: non_neg_integer() , % number of objects in table,
	  no_keys :: non_neg_integer(),     % number of keys
	  maxobjsize :: 'undefined' | non_neg_integer(), % 2-log of
                           % the size of the biggest object collection
	  n,               % split indicator
	  type :: type(),
	  keypos :: keypos(), % default is 1 as for ets
	  freelists :: 'undefined'
                     | tuple(), % tuple of free lists of buddies
	                        % if fixed =/= false, then a pair of freelists
	  freelists_p :: 'undefined'
                       | non_neg_integer(),  % cached FreelistsPointer
	  no_collections :: 'undefined'
                          | no_colls(), % number of object collections
                                        % per size (version 9(b))
	  auto_save :: auto_save(),
	  update_mode :: update_mode(),
	  fixed = false :: 'false'
                         | {{integer(), integer()}, % time of first fix,
                            [{pid(),   % and number of fixes per process
                              non_neg_integer()}]},
	  hash_bif :: hash_bif(),  % hash bif used for this file
          has_md5 :: boolean(),    % whether the header has
                                   % an MD5 sum (version 9(c))
	  min_no_slots :: no_slots(),  % minimum number of slots
	  max_no_slots :: no_slots(),  % maximum number of slots
	  cache :: 'undefined' | cache(), % Write cache.

	  filename :: file:name(), % name of the file being used
	  access = read_write :: access(),
	  ram_file = false :: boolean(),
	  name :: tab_name(),      % the name of the table

	  parent :: 'undefined' | pid(), % The supervisor of Dets processes.
	  server :: 'undefined' | pid(), % The creator of Dets processes.

          bump :: non_neg_integer(),
          base :: non_neg_integer()

	 }).

%% Info extracted from the file header.
-record(fileheader, {
	  freelist :: non_neg_integer(),
          fl_base :: non_neg_integer(),
	  cookie :: non_neg_integer(),
	  closed_properly :: non_neg_integer(),
	  type :: 'badtype' | type(),
	  version :: non_neg_integer(),
	  m :: non_neg_integer(),
	  next :: non_neg_integer(),
	  keypos :: keypos(),
	  no_objects :: non_neg_integer(),
	  no_keys :: non_neg_integer(),
	  min_no_slots :: non_neg_integer(),
	  max_no_slots :: non_neg_integer(),
	  no_colls :: 'undefined' | no_colls(),
	  hash_method :: non_neg_integer(),
          read_md5 :: binary(),
          has_md5 :: boolean(),
          md5 :: binary(),
	  trailer :: non_neg_integer(),
	  eof :: non_neg_integer(),
	  n
	}).

-type delay() :: non_neg_integer().
-type threshold() :: non_neg_integer().
-type cache_parms() ::
        {Delay :: delay(), % max time items are kept in RAM only,
                           % in milliseconds
         Size :: threshold()}. % threshold size of cache, in bytes

%% Write Cache.
-record(cache, {
	 cache :: % write cache, last item first
                 [{Key :: term(),
                   {Seq :: non_neg_integer(), Item :: term()}}],
         csize :: non_neg_integer(), % current size of the cached items
         inserts :: % upper limit on number of inserted keys
                    non_neg_integer(),
	 wrtime :: 'undefined' | integer(),  % last write or update time
	 tsize :: threshold(), % threshold size of cache
	 delay :: delay()      % max time items are kept in RAM only
	 }).

-type cache() :: #cache{}.
