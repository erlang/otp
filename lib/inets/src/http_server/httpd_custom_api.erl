%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2015-2024. All Rights Reserved.
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
-module(httpd_custom_api).
-moduledoc """
Behaviour with optional callbacks to customize the inets HTTP server.

The module implementing this behaviour shall be supplied to to the servers
configuration with the option [customize](`m:httpd#prop_customize`)
""".
-moduledoc(#{since => "OTP 17.5.6"}).

-doc """
Provide default headers for the HTTP servers responses. Note that this option
may override built-in defaults.
""".
-doc(#{since => <<"OTP 18.1.1">>}).
-callback response_default_headers() -> 
    [{Key::string(), Value::string()}].
-doc """
Filter and possible alter HTTP response headers before they are sent to the
client.
""".
-doc(#{since => <<"OTP 17.5.6">>}).
-callback response_header({Key::string(), Value::string()}) -> 
    {true, {Key::string(), Value::string()}} | false |
    {true, string()}. %% Used internally to avoid traversing headers twice 
-doc """
Filter and possible alter HTTP request headers before they are processed by the
server.
""".
-doc(#{since => <<"OTP 17.5.6">>}).
-callback request_header({Key::string(), Value::string()}) -> 
    {true, {Key::string(), Value::string()}} | false.

-optional_callbacks([response_default_headers/0, response_header/1,
		     request_header/1]).
