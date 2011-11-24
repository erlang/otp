%  Copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
%%%-------------------------------------------------------------------
%%% File    : comm_layer.hrl
%%% Author  : Thorsten Schuett <schuett@zib.de>
%%% Description :
%%%
%%% Created : 31 Jul 2008 by Thorsten Schuett <schuett@csr-pc11.zib.de>
%%%-------------------------------------------------------------------
%% @author Thorsten Schuett <schuett@zib.de>
%% @copyright 2008 Konrad-Zuse-Zentrum für Informationstechnik Berlin
%% @version $Id: comm_layer.hrl,v 1.1 2009/11/06 12:41:36 maria Exp $
-author('schuett@zib.de').
-vsn('$Id: comm_layer.hrl,v 1.1 2009/11/06 12:41:36 maria Exp $ ').

% enable logging of message statistics
%-define(LOG_MESSAGE(TAG, SIZE), comm_layer.comm_logger:log(TAG, SIZE)).
-define(LOG_MESSAGE(TAG, SIZE), ok).
