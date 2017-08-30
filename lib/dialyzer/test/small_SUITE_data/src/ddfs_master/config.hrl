
-define(SECOND, 1000).
-define(MINUTE, (60 * ?SECOND)).
-define(HOUR, (60 * ?MINUTE)).
-define(DAY, (24 * ?HOUR)).
-define(MB, (1024 * 1024)).

% Maximum length of tag/blob prefix
-define(NAME_MAX, 511).

% How long ddfs node startup can take.  The most time-consuming part
% is the scanning of the tag objects in the node's DDFS volumes.
-define(NODE_STARTUP, (1 * ?MINUTE)).

% How long to wait on the master for replies from nodes.
-define(NODE_TIMEOUT, (10 * ?SECOND)).

% How long to wait for a reply from an operation coordinated by the
% master that accesses nodes.  This value should be larger than
% NODE_TIMEOUT.
-define(NODEOP_TIMEOUT, (1 * ?MINUTE)).

% The minimum amount of free space a node must have, to be considered
% a primary candidate host for a new blob.
-define(MIN_FREE_SPACE, (1024 * ?MB)).

% The maximum number of active HTTP connections on a system (this
% applies separately for GET and PUT operations).
-define(HTTP_MAX_ACTIVE, 3).

% The maximum number of waiting HTTP connections to queue up on a busy system.
-define(HTTP_QUEUE_LENGTH, 100).

% The maximum number of simultaneous HTTP connections. Note that
% HTTP_MAX_CONNS * 2 * 2 + 32 < Maximum number of file descriptors, where
% 2 = Get and put, 2 = two FDs required for each connection (connection
% itself + a file it accesses), 32 = a guess how many extra fds is needed.
-define(HTTP_MAX_CONNS, 128).

% How long to keep a PUT request in queue if the system is busy.
-define(PUT_WAIT_TIMEOUT, (1 * ?MINUTE)).

% How long to keep a GET request in queue if the system is busy.
-define(GET_WAIT_TIMEOUT, (1 * ?MINUTE)).

% An unused loaded tag expires in TAG_EXPIRES milliseconds.  Note that
% if TAG_EXPIRES is not smaller than GC_INTERVAL, tags will never
% expire from the memory cache and will always take up memory.
-define(TAG_EXPIRES, (10 * ?HOUR)).

% How often the master's cache of all known tag names is refreshed.
% This refresh is only needed to purge deleted tags eventually from
% the tag cache. It doesn't harm to have a long interval.
-define(TAG_CACHE_INTERVAL, (10 * ?MINUTE)).

% How soon a tag object initialized in memory expires if it's content
% cannot be fetched from the cluster.
-define(TAG_EXPIRES_ONERROR, (1 * ?SECOND)).

% How often a DDFS node should refresh its tag cache from disk.
-define(FIND_TAGS_INTERVAL, ?DAY).

% How often buffered (delayed) updates to a tag need to be
% flushed. Tradeoff: The longer the interval, the more updates are
% bundled in a single commit.  On the other hand, in the worst case
% the requester has to wait for the full interval before getting a
% reply. A long interval also increases the likelihood that the server
% crashes before the commit has finished successfully, making requests
% more unreliable.
-define(DELAYED_FLUSH_INTERVAL, (1 * ?SECOND)).

% How long to wait between garbage collection runs.
-define(GC_INTERVAL, ?DAY).

% Max duration for a GC run.  This should be smaller than
% min(ORPHANED_{BLOB,TAG}_EXPIRES).
-define(GC_MAX_DURATION, (3 * ?DAY)).

% How long to wait after startup for cluster to stabilize before
% starting the first GC run.
-define(GC_DEFAULT_INITIAL_WAIT, (5 * ?MINUTE)).

% The longest potential interval between messages in the GC protocol;
% used to ensure GC makes forward progress.  This can be set to the
% estimated time to traverse all the volumes on a DDFS node.
-define(GC_PROGRESS_INTERVAL, (30 * ?MINUTE)).

% Number of extra replicas (i.e. lost replicas recovered during GC) to
% allow before deleting extra replicas.
-define(NUM_EXTRA_REPLICAS, 1).

% Permissions for files backing blobs and tags.
-define(FILE_MODE, 8#00400).

% How often to check available disk space in ddfs_node.
-define(DISKSPACE_INTERVAL, (10 * ?SECOND)).

% The maximum size of payloads of HTTP requests to the /ddfs/tag/
% prefix.
-define(MAX_TAG_BODY_SIZE, (512 * ?MB)).

% Tag attribute names and values have a limited size, and there
% can be only a limited number of them.
-define(MAX_TAG_ATTRIB_NAME_SIZE, 1024).
-define(MAX_TAG_ATTRIB_VALUE_SIZE, 1024).
-define(MAX_NUM_TAG_ATTRIBS, 1000).

% How long HTTP requests that perform tag updates should wait to
% finish (a long time).
-define(TAG_UPDATE_TIMEOUT, ?DAY).

% Timeout for re-replicating a single blob over HTTP PUT.  This
% depends on the largest blobs hosted by DDFS, and the speed of the
% cluster network.
-define(GC_PUT_TIMEOUT, (180 * ?MINUTE)).

% Delete !partial files after this many milliseconds.
-define(PARTIAL_EXPIRES, ?DAY).

% When orphaned blob can be deleted.  This should be large enough that
% you can upload all the new blobs of a tag and perform the tag update
% within this time.
-define(ORPHANED_BLOB_EXPIRES, (5 * ?DAY)).

% When orphaned tag can be deleted.
-define(ORPHANED_TAG_EXPIRES, (5 * ?DAY)).

% How long a tag has to stay on the deleted list before
% we can permanently forget it, after all known instances
% of the tag object have been removed. This quarantine period
% ensures that a node that was temporarily unavailable
% and reactivates can't resurrect deleted tags. You
% must ensure that all temporarily inactive nodes
% are reactivated (or cleaned) within the ?DELETED_TAG_EXPIRES
% time frame.
%
% This value _must_ be larger than the other time-related DDFS
% parameters listed in this file.  In particular, it must be larger
% than ORPHANED_TAG_EXPIRES.
-define(DELETED_TAG_EXPIRES, (30 * ?DAY)).

% How many times a tag operation should be retried before aborting.
-define(MAX_TAG_OP_RETRIES, 3).

% How long to wait before timing out a tag retrieval.  This should be
% large enough to read a large tag object off the disk and send it
% over the network.
-define(GET_TAG_TIMEOUT, (5 * ?MINUTE)).
