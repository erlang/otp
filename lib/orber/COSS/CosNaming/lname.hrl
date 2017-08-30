%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: lname.hrl
%%-----------------------------------------------------------------

%% LName interface exceptions
-record('LName_NoComponent', {'OE_ID'="PIDL:LName/NoComponent:1.0"}).
-record('LName_InvalidName', {'OE_ID'="PIDL:LName/InvalidName:1.0"}).
% This exception is not used in our implementation.
-record('LName_Overflow', {'OE_ID'="PIDL:LName/Overflow:1.0"}). 

%% LNameComponent interface exceptions
-record('LNameComponent_NotSet',
	{'OE_ID'="PIDL:LNameComponent/NotSet:1.0"}).
