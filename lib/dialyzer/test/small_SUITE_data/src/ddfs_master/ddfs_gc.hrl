-type local_object() :: {object_name(), node()}.
-type phase() :: 'start' | 'build_map' | 'map_wait' | 'gc'
               | 'rr_blobs' | 'rr_blobs_wait' | 'rr_tags'.
-type protocol_msg() :: {'check_blob', object_name()} | 'start_gc' | 'end_rr'.

-type blob_update() :: {object_name(), 'filter' | [url()]}.

-type check_blob_result() :: 'false' | {'true', volume_name()}.

% GC statistics

% {Files, Bytes}
-type gc_stat() :: {non_neg_integer(), non_neg_integer()}.
% {Kept, Deleted}
-type obj_stats() :: {gc_stat(), gc_stat()}.
% {Tags, Blobs}.
-type gc_run_stats() :: {obj_stats(), obj_stats()}.
