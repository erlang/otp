%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: httpd_verbosity.hrl,v 1.1 2008/12/17 09:53:34 mikpe Exp $
%%

-ifndef(dont_use_verbosity).

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-define(vvalidate(V), httpd_verbosity:validate(V)).

-ifdef(VMODULE).

-define(vinfo(F,A), httpd_verbosity:print(get(verbosity),info, ?VMODULE,F,A)).
-define(vlog(F,A),  httpd_verbosity:print(get(verbosity),log,  ?VMODULE,F,A)).
-define(vdebug(F,A),httpd_verbosity:print(get(verbosity),debug,?VMODULE,F,A)).
-define(vtrace(F,A),httpd_verbosity:print(get(verbosity),trace,?VMODULE,F,A)).

-else.

-define(vinfo(F,A), httpd_verbosity:print(get(verbosity),info, F,A)).
-define(vlog(F,A),  httpd_verbosity:print(get(verbosity),log,  F,A)).
-define(vdebug(F,A),httpd_verbosity:print(get(verbosity),debug,F,A)).
-define(vtrace(F,A),httpd_verbosity:print(get(verbosity),trace,F,A)).

-endif.

-define(vinfoc(F,A), httpd_verbosity:printc(get(verbosity),info, F,A)).
-define(vlogc(F,A),  httpd_verbosity:printc(get(verbosity),log,  F,A)).
-define(vdebugc(F,A),httpd_verbosity:printc(get(verbosity),debug,F,A)).
-define(vtracec(F,A),httpd_verbosity:printc(get(verbosity),trace,F,A)).

-else.

-define(vvalidate(V),ok).

-define(vinfo(F,A),ok).
-define(vlog(F,A),ok).
-define(vdebug(F,A),ok).
-define(vtrace(F,A),ok).

-define(vinfoc(F,A),ok).
-define(vlogc(F,A),ok).
-define(vdebugc(F,A),ok).
-define(vtracec(F,A),ok).

-endif.
