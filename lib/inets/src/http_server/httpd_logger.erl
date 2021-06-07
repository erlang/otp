%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
-module(httpd_logger).

-include_lib("kernel/include/logger.hrl").
-include_lib("inets/include/httpd.hrl").

-export([error_report/4, log/3, format/1]). 

error_report(Protocol, Reason, #mod{init_data = #init_data{peername = PeerName, 
                                                         sockname = SockName},
                                     socket_type = Type,
                                     request_uri = URI,
                                    config_db = Db},
             Location) ->
    ServerName = httpd_util:lookup(Db, server_name),
    Report0 = #{protocol => Protocol,
               reason => Reason,
               peer => PeerName,
               host => SockName,
               server_name => ServerName,
               metadata => Location},
    Report1 = case URI of
                  undefined ->
                      Report0;
                  _ ->
                      Report0#{uri => URI}
              end,
    case Protocol of
        'HTTP' ->
            Report1#{transport => transport_type(Type)}; 
        _ ->
            Report1
    end. 
    
log(Level, #{metadata := MetaData} = Report, Domain) ->
    logger:log(Level, maps:without([metadata], Report), 
               MetaData#{domain => [otp,inets, httpd, Domain, Level],
                         report_cb => fun ?MODULE:format/1}).

format(#{protocol := Protocol} = Report) when Protocol == 'TLS';
                                              Protocol == 'TCP' -> 
    #{reason := Desc,
      peer := {PeerPort, Peer},
      host := {HostPort, Host},
      server_name := ServerName
     } = Report,
    {
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s:~p~n"
     "~10s ~s:~p~n"
     "~10s ~p~n"
     "~n",
     ["Server:", ServerName,
      "Protocol:", atom_to_list(Protocol), 
      "Host:", Host, HostPort, 
      "Peer:", Peer, PeerPort, 
      "Reason:", Desc]     
    };
format(#{protocol := 'HTTP' = Protocol,  uri := URI} = Report) -> 
    #{reason := Desc,
      transport := Transport,
      peer := {PeerPort, Peer},
      host := {HostPort, Host},
      server_name := ServerName} = Report,
    {
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s:~p~n"
     "~10s ~s:~p~n"
     "~10s ~p~n"
     "~n",
     ["Server:", ServerName,
      "Protocol:", atom_to_list(Protocol),
      "Transport:", Transport,
      "URI:", URI,
      "Host:", Host, HostPort,
      "Peer:", Peer, PeerPort,
      "Reason:", Desc]
    };
format(#{protocol := 'HTTP' = Protocol} = Report) -> 
    #{reason := Desc,
      transport := Transport,
      peer := {PeerPort, Peer},
      host := {HostPort, Host},
      server_name := ServerName} = Report,
    {
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s:~p~n"
     "~10s ~s:~p~n"
     "~10s ~p~n"
     "~n",
     ["Server:", ServerName,
      "Protocol:", atom_to_list(Protocol),
      "Transport:",  Transport,
      "Host:", Host, HostPort,
      "Peer:", Peer, PeerPort,
      "Reason:", Desc]
    };
format(#{protocol := internal = Protocol} = Report) -> 
    #{reason := Desc,
      host := {HostPort, Host},
      server_name := ServerName
     } = Report,
    {
     "~10s ~s~n"
     "~10s ~s~n"
     "~10s ~s:~p~n"
     "~10s ~p~n"
     "~n",
     ["Server:", ServerName,
      "Protocol:", atom_to_list(Protocol),
      "Host:", Host, HostPort,
      "Reason:", Desc]
    }.

transport_type(ip_comm) ->
    "TCP";
transport_type(_) ->
    "TLS".
