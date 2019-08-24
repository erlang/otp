-type host() :: nonempty_string().
-type path() :: nonempty_string().
-type url() :: binary().

% The host portion of a url, if available.
-type url_host() :: host() | none.
