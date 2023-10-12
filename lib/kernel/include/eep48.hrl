-define(NATIVE_FORMAT,<<"application/erlang+html">>).
-define(CURR_DOC_VERSION, {1,0,0}).
-record(docs_v1, {anno,
                  beam_language = erlang,
                  format = ?NATIVE_FORMAT,
                  module_doc,
                  metadata = #{ otp_doc_vsn => ?CURR_DOC_VERSION },
                  docs}).

-record(docs_v1_entry, {kind_name_arity,
                        anno,
                        signature,
                        doc,
                        metadata}).
