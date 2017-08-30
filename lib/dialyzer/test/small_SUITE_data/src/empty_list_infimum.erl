%%
%% This is stripped down code from RabbitMQ. It is used to report an
%% invalid type specification for function list_vhost_permissions/1.
%% The Initial Developer of the Original Code is VMware, Inc.
%%

-module(empty_list_infimum).

-record(permission, {configure, write, read}).
-record(user_vhost, {username, virtual_host}).
-record(user_permission, {user_vhost, permission}).

%%----------------------------------------------------------------------------

-export([i_delete/1]).

-type(vhost() :: binary()).

-type(info_key() :: atom()).
-type(info_keys() :: [info_key()]).

-type(info() :: {info_key(), any()}).
-type(infos() :: [info()]).

%%----------------------------------------------------------------------------

-spec i_delete(vhost()) -> 'ok'.

i_delete(VHostPath) ->
    [ok || _ <- list_vhost_permissions(VHostPath)],
    ok.

%%----------------------------------------------------------------------------

vhost_perms_info_keys() ->
    [user, configure, write, read].

-spec list_vhost_permissions(vhost()) -> infos().

list_vhost_permissions(VHostPath) ->
    list_permissions(vhost_perms_info_keys(), some_mod:some_function()).

filter_props(Keys, Props) ->
    [T || T = {K, _} <- Props, lists:member(K, Keys)].

list_permissions(Keys, SomeList) ->
    [filter_props(Keys, [{user,      Username},
                         {vhost,     VHostPath},
                         {configure, ConfigurePerm},
                         {write,     WritePerm},
                         {read,      ReadPerm}]) ||
        #user_permission{user_vhost = #user_vhost{username     = Username,
                                                  virtual_host = VHostPath},
                         permission = #permission{configure = ConfigurePerm,
						  write     = WritePerm,
						  read      = ReadPerm}} <-
           SomeList].
