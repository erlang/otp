%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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
