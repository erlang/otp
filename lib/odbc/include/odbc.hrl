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
%%     $Id$
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This whole file is DEPRECATED!!!! It is part of the old interface to
%% the erlang odbc-application. Some constants that would never come to use
%% has been removed. This file has also been made static. Before it was
%% generated for some peculiar reason.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Result codes
%% ____________

-define(SQL_SUCCESS, 0).
-define(SQL_SUCCESS_WITH_INFO, 1).
-define(SQL_INVALID_HANDLE, -2).
-define(SQL_ERROR, -1).
-define(SQL_NO_DATA, 100).
-define(SQL_NEED_DATA, 99).

%% Values of "Nullable"
%% ____________________

-define(SQL_NO_NULLS, 0).
-define(SQL_NULLABLE, 1).
-define(SQL_NULLABLE_UNKNOWN, 2).

%% Commit/rollback macros
%% ______________________

-define(SQL_COMMIT, 0).
-define(SQL_ROLLBACK, 1).

%% Connection attributes and values
%% ________________________________

-define(SQL_ATTR_AUTOCOMMIT, 102).
-define(SQL_AUTOCOMMIT_OFF, 0).
-define(SQL_AUTOCOMMIT_ON, 1).
-define(SQL_ATTR_TRACE, 104).
-define(SQL_OPT_TRACE_OFF, 0).
-define(SQL_OPT_TRACE_ON, 1).
-define(SQL_ATTR_TRACEFILE, 105).
