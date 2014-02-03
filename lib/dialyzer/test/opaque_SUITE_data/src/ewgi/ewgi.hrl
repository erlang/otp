-ifndef(_EWGI_HRL).
-define(_EWGI_HRL, 1).

% ``The contents of this file are subject to the Mozilla Public License
% Version 1.1 (the "License"); you may not use this file except in
% compliance with the License. You may obtain a copy of the License at
% http://www.mozilla.org/MPL/
%
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
% License for the specific language governing rights and limitations
% under the License.
%
% The Original Code is the EWGI reference implementation.
%
% The Initial Developer of the Original Code is S.G. Consulting
% srl. Portions created by S.G. Consulting s.r.l. are Copyright (C)
% 2007 S.G. Consulting srl. All Rights Reserved.
%
% Contributor(s): Filippo Pacini <filippo.pacini@gmail.com>
%                 Hunter Morris <huntermorris@gmail.com>

-define(DEFAULT_CHUNKSIZE, 4096).

-type ewgi_propval() :: atom() | integer() | string() | binary().
-type ewgi_prop() :: {ewgi_propval(), ewgi_propval()}.
-type ewgi_proplist() :: [ewgi_prop()].

%% @type bag() = gb_tree()
-ifdef(HAS_GB_TREE_SPEC).
-type bag() :: gb_trees:tree().
-else.
-type bag() :: {non_neg_integer(), {any(), any(), any(), any()} | 'nil'}.
-endif.

%%% Note: Dialyzer currently doesn't support recursive types. When it does, this should change:
%%%-type ewgi_ri_callback() :: fun(('eof' | {data, binary()}) -> iolist() | ewgi_ri_callback()).
%% @type ewgi_ri_callback() = function()
-type ewgi_ri_callback() :: fun(('eof' | {data, binary()}) -> iolist() | function()) | iolist().

%% @type ewgi_read_input() = function()
-type ewgi_read_input() :: fun((ewgi_ri_callback(), integer()) -> ewgi_ri_callback()).

%% @type ewgi_write_error() = function()
-type ewgi_write_error() :: fun((any()) -> 'ok').

%% @type ewgi_version() = {integer(), integer()}
-type ewgi_version() :: {integer(), integer()}.

%% @type ewgi_spec() = {'ewgi_spec', function(), function(), string(),
%%                      ewgi_version(), bag()}

-type ewgi_spec() :: {'ewgi_spec', ewgi_read_input(),
                      ewgi_write_error(), string(), ewgi_version(),
                      bag()}.

-define(IS_EWGI_SPEC(R), ((element(1, R) =:= 'ewgi_spec')
                          and (size(R) =:= 6))).
-define(GET_EWGI_READ_INPUT(R), element(2, R)).
-define(SET_EWGI_READ_INPUT(A, R), setelement(2, R, A)).
-define(GET_EWGI_WRITE_ERROR(R), element(3, R)).
-define(SET_EWGI_WRITE_ERROR(A, R), setelement(3, R, A)).
-define(GET_EWGI_URL_SCHEME(R), element(4, R)).
-define(SET_EWGI_URL_SCHEME(A, R), setelement(4, R, A)).
-define(GET_EWGI_VERSION(R), element(5, R)).
-define(SET_EWGI_VERSION(A, R), setelement(5, R, A)).
-define(GET_EWGI_DATA(R), element(6, R)).
-define(SET_EWGI_DATA(A, R), setelement(6, R, A)).

%% @type ewgi_header_val() = string() | 'undefined'
-type ewgi_header_val() :: string() | 'undefined'.

%% @type ewgi_header_key() = string()
-type ewgi_header_key() :: string().

%% @type ewgi_http_headers() = {'ewgi_http_headers',
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              ewgi_header_val(),
%%                              bag()}

-type ewgi_http_headers() :: {'ewgi_http_headers', ewgi_header_val(),
                              ewgi_header_val(), ewgi_header_val(),
                              ewgi_header_val(), ewgi_header_val(),
                              ewgi_header_val(), bag()}.

-define(IS_HTTP_HEADERS(R), ((element(1, R) =:= 'ewgi_http_headers')
                             and (size(R) =:= 8))).
-define(GET_HTTP_ACCEPT(R), element(2, R)).
-define(SET_HTTP_ACCEPT(A, R), setelement(2, R, A)).
-define(GET_HTTP_COOKIE(R), element(3, R)).
-define(SET_HTTP_COOKIE(A, R), setelement(3, R, A)).
-define(GET_HTTP_HOST(R), element(4, R)).
-define(SET_HTTP_HOST(A, R), setelement(4, R, A)).
-define(GET_HTTP_IF_MODIFIED_SINCE(R), element(5, R)).
-define(SET_HTTP_IF_MODIFIED_SINCE(A, R), setelement(5, R, A)).
-define(GET_HTTP_USER_AGENT(R), element(6, R)).
-define(SET_HTTP_USER_AGENT(A, R), setelement(6, R, A)).
-define(GET_HTTP_X_HTTP_METHOD_OVERRIDE(R), element(7, R)).
-define(SET_HTTP_X_HTTP_METHOD_OVERRIDE(A, R), setelement(7, R, A)).
-define(GET_HTTP_OTHER(R), element(8, R)).
-define(SET_HTTP_OTHER(A, R), setelement(8, R, A)).

%% @type ewgi_request_method() = 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
%%       'DELETE' | 'TRACE' | 'CONNECT' | string()
-type ewgi_request_method() :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' |
                               'DELETE' | 'TRACE' | 'CONNECT' | string().

%% @type ewgi_val() = string() | 'undefined'
-type ewgi_val() :: string() | 'undefined'.

%% @type ewgi_request() :: {'ewgi_request', ewgi_val(), integer(), ewgi_val(),
%%                          ewgi_spec(), ewgi_val(), ewgi_http_headers(),
%%                          ewgi_val(), ewgi_val(), ewgi_val(), ewgi_val(),
%%                          ewgi_val(), ewgi_val(), ewgi_val(), ewgi_val(),
%%                          ewgi_request_method(), ewgi_val(), ewgi_val(),
%%                          ewgi_val(), ewgi_val(), ewgi_val()}

-type ewgi_request() :: {'ewgi_request', ewgi_val(),
                         non_neg_integer(), ewgi_val(), ewgi_spec(),
                         ewgi_val(), ewgi_http_headers(), ewgi_val(),
                         ewgi_val(), ewgi_val(), ewgi_val(),
                         ewgi_val(), ewgi_val(), ewgi_val(),
                         ewgi_val(), ewgi_request_method(),
                         ewgi_val(), ewgi_val(), ewgi_val(),
                         ewgi_val(), ewgi_val()}.

-define(IS_EWGI_REQUEST(R), ((element(1, R) =:= 'ewgi_request')
                             and (size(R) =:= 21))).
-define(GET_AUTH_TYPE(R), element(2, R)).
-define(SET_AUTH_TYPE(A, R), setelement(2, R, A)).
-define(GET_CONTENT_LENGTH(R), element(3, R)).
-define(SET_CONTENT_LENGTH(A, R), setelement(3, R, A)).
-define(GET_CONTENT_TYPE(R), element(4, R)).
-define(SET_CONTENT_TYPE(A, R), setelement(4, R, A)).
-define(GET_EWGI(R), element(5, R)).
-define(SET_EWGI(A, R), setelement(5, R, A)).
-define(GET_GATEWAY_INTERFACE(R), element(6, R)).
-define(SET_GATEWAY_INTERFACE(A, R), setelement(6, R, A)).
-define(GET_HTTP_HEADERS(R), element(7, R)).
-define(SET_HTTP_HEADERS(A, R), setelement(7, R, A)).
-define(GET_PATH_INFO(R), element(8, R)).
-define(SET_PATH_INFO(A, R), setelement(8, R, A)).
-define(GET_PATH_TRANSLATED(R), element(9, R)).
-define(SET_PATH_TRANSLATED(A, R), setelement(9, R, A)).
-define(GET_QUERY_STRING(R), element(10, R)).
-define(SET_QUERY_STRING(A, R), setelement(10, R, A)).
-define(GET_REMOTE_ADDR(R), element(11, R)).
-define(SET_REMOTE_ADDR(A, R), setelement(11, R, A)).
-define(GET_REMOTE_HOST(R), element(12, R)).
-define(SET_REMOTE_HOST(A, R), setelement(12, R, A)).
-define(GET_REMOTE_IDENT(R), element(13, R)).
-define(SET_REMOTE_IDENT(A, R), setelement(13, R, A)).
-define(GET_REMOTE_USER(R), element(14, R)).
-define(SET_REMOTE_USER(A, R), setelement(14, R, A)).
-define(GET_REMOTE_USER_DATA(R), element(15, R)).
-define(SET_REMOTE_USER_DATA(A, R), setelement(15, R, A)).
-define(GET_REQUEST_METHOD(R), element(16, R)).
-define(SET_REQUEST_METHOD(A, R), setelement(16, R, A)).
-define(GET_SCRIPT_NAME(R), element(17, R)).
-define(SET_SCRIPT_NAME(A, R), setelement(17, R, A)).
-define(GET_SERVER_NAME(R), element(18, R)).
-define(SET_SERVER_NAME(A, R), setelement(18, R, A)).
-define(GET_SERVER_PORT(R), element(19, R)).
-define(SET_SERVER_PORT(A, R), setelement(19, R, A)).
-define(GET_SERVER_PROTOCOL(R), element(20, R)).
-define(SET_SERVER_PROTOCOL(A, R), setelement(20, R, A)).
-define(GET_SERVER_SOFTWARE(R), element(21, R)).
-define(SET_SERVER_SOFTWARE(A, R), setelement(21, R, A)).

%%% Note: Dialyzer currently doesn't support recursive types. When it does, this should change:
%%%-type stream() :: fun(() -> {} | {any(), stream()}).
%% @type stream() = function()
-type stream() :: fun(() -> {} | {any(), function()}).

%% @type ewgi_status() = {integer(), string()}
-type ewgi_status() :: {integer(), string()}.

%% @type ewgi_message_body() = binary() | iolist() | stream()
-type ewgi_message_body() :: binary() | iolist() | stream().

%% @type ewgi_header_list() = [{ewgi_header_key(), ewgi_header_val()}]
-type ewgi_header_list() :: [{ewgi_header_key(), ewgi_header_val()}].

%% @type ewgi_response() = {'ewgi_response', ewgi_status(),
%%                          [{ewgi_header_key(), ewgi_header_val()}],
%%                           ewgi_message_body(), any()}

-type ewgi_response() :: {'ewgi_response', ewgi_status(), ewgi_header_list(), ewgi_message_body(), any()}.

-define(IS_EWGI_RESPONSE(R), ((element(1, R) =:= 'ewgi_response')
                              and (size(R) =:= 5))).
-define(GET_RESPONSE_STATUS(R), element(2, R)).
-define(SET_RESPONSE_STATUS(A, R), setelement(2, R, A)).
-define(GET_RESPONSE_HEADERS(R), element(3, R)).
-define(SET_RESPONSE_HEADERS(A, R), setelement(3, R, A)).
-define(GET_RESPONSE_MESSAGE_BODY(R), element(4, R)).
-define(SET_RESPONSE_MESSAGE_BODY(A, R), setelement(4, R, A)).
-define(GET_RESPONSE_ERROR(R), element(5, R)).
-define(SET_RESPONSE_ERROR(A, R), setelement(5, R, A)).

%% @type ewgi_context() = {'ewgi_context', ewgi_request(), ewgi_response()}

-type ewgi_context() :: {'ewgi_context', ewgi_request(), ewgi_response()}.

-define(IS_EWGI_CONTEXT(R), ((element(1, R) =:= 'ewgi_context')
                             and ?IS_EWGI_REQUEST(element(2, R))
                             and ?IS_EWGI_RESPONSE(element(3, R))
                             and (size(R) =:= 3))).
-define(GET_EWGI_REQUEST(R), element(2, R)).
-define(SET_EWGI_REQUEST(A, R), setelement(2, R, A)).
-define(GET_EWGI_RESPONSE(R), element(3, R)).
-define(SET_EWGI_RESPONSE(A, R), setelement(3, R, A)).

%% @type ewgi_app() = function()
-type ewgi_app() :: fun((ewgi_context()) -> ewgi_context()).

-ifndef(debug).
-define(INSPECT_EWGI_RESPONSE(Ctx), Ctx).
-else.
-define(INSPECT_EWGI_RESPONSE(Ctx),
	begin
	    error_logger:info_msg("Inpecting the final ewgi_response()...~n"
				  "Requested Url: ~p~n"
				  "Status: ~p~n"
				  "Headers: ~p~n"
				  "Body: ~p~n",
				  [ewgi_api:path_info(Ctx),
				   ewgi_api:response_status(Ctx),
				   ewgi_api:response_headers(Ctx),
				   ewgi_api:response_message_body(Ctx)]),
	    Ctx
	end
       ).
-endif.

-endif.
