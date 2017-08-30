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
%%

-ifndef(ftp_internal_hrl).
-define(ftp_internal_hrl, true).

-include_lib("inets/src/inets_app/inets_internal.hrl").

-define(SERVICE, ftpc).
-define(fcri(Label, Content), ?report_important(Label, ?SERVICE, Content)).
-define(fcrv(Label, Content), ?report_verbose(Label,   ?SERVICE, Content)).
-define(fcrd(Label, Content), ?report_debug(Label,     ?SERVICE, Content)).
-define(fcrt(Label, Content), ?report_trace(Label,     ?SERVICE, Content)).

-endif. % -ifdef(ftp_internal_hrl).
