### Erlang Distribution Without Large Node Container Support

Communication over the Erlang distribution without support for large
[node container data types (version 4)](`e:erts:erl_dist_protocol.md#DFLAG_V4_NC`)
was as of [OTP 24 deprecated](deprecations.md#otp-24) and support for it was
scheduled for removal in OTP 26. That is, as of OTP 26, support for large node
container data types will become mandatory. This also includes external term
format produced by `term_to_binary()`/`term_to_iovec()`.

### Old Link Protocol

The old link protocol used when communicating over the Erlang distribution was
as of [OTP 24 deprecated](deprecations.md#otp-24) and support for it was
scheduled for removal in OTP 26. As of OTP 26 the
[new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`) became
mandatory. That is, Erlang nodes will refuse to connect to nodes not
implementing the new link protocol.

### Functions Removed in OTP 26

-   `code:is_module_native/1` (HiPE has been removed)
-   `code:rehash/0` (the code path cache feature has been removed)
-   `disk_log:accessible_logs/0` (use disk_log:all/0 instead)
-   `disk_log:lclose/1` (use disk_log:close/1 instead)
-   `disk_log:lclose/2` (use disk_log:close/1 instead)
-   `erts_alloc_config:_/_` (this module has as of OTP 26.0 been removed)
-   `ftp:start_service/1` (use ftp:open/2 instead)
-   `ftp:stop_service/1` (use ftp:close/1 instead)
-   `httpd_util:decode_hex/1` (use uri_string:unquote function instead)
-   `httpd_util:encode_hex/1` (use uri_string:quote function instead)
-   `httpd_util:flatlength/1` (use erlang:iolist_size/1 instead)
-   `httpd_util:hexlist_to_integer/1` (use erlang:list_to_integer/2 with base 16
    instead)
-   `httpd_util:integer_to_hexlist/1` (use erlang:integer_to_list/2 with base 16
    instead)
-   `httpd_util:strip/1` (use string:trim/1 instead)
-   `httpd_util:suffix/1` (use filename:extension/1 and string:trim/2 instead)
