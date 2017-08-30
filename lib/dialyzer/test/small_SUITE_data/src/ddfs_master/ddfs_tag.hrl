
-type tokentype() :: 'read' | 'write'.
-type user_attr() :: [{binary(), binary()}].
% An 'internal' token is also used by internal consumers, but never stored.
-type token() :: 'null' | binary().

-type tagname() :: binary().
-type tagid() :: binary().

-type attrib() :: 'urls' | 'read_token' | 'write_token' | {'user', binary()}.

-record(tagcontent, {id :: tagid(),
                     last_modified :: binary(),
                     read_token = null :: token(),
                     write_token = null :: token(),
                     urls = [] :: [[binary()]],
                     user = [] :: user_attr()}).

-type tagcontent() :: #tagcontent{}.
