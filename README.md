# Erlang/OTP + EEP49

Our entry for Spawnfest is the implementation of [EEP-49](https://github.com/erlang/eep/blob/master/eeps/eep-0049.md) onto regular Erlang/OTP.

## What is this?

EEP-49 extends the `begin ... end` expression to make it a construct usable for control flow and value-based error handling based on pattern matching:

```erlang
begin
    Exprs | MatchOrReturnExprs
cond
    Pattern -> Exprs;
    ...
    Pattern -> Exprs
end
```

Where `Exprs` are any regular Erlang code you know, and `MatchOrReturnExprs` are patterns of the form `Pattern <- Expression`.

For example, in a call such as:

```erlang
begin
    #{<<"user">> := Id} = parse(JSON),
    {ok, User=#{name := Name}} <- get_user(Id),
    io:format("Hello ~ts!~n", [Name]),
    {user, Name, User}
end
```

If the call to `get_user(Id)` returns `{ok, Map}` with the proper fields, the code works as if the `<-` was a `=`. However, if the function instead returns `{error, not_found}`, then this is what the whole `begin ... end` expression returns. If we match, the flow continues, and if it fails, we return the erroneous value right away.

This lets us rework examples such as:

```erlang
-spec commit_write(_) -> {ok, file:filename()} | {error, term()}.
commit_write(OpaqueData) ->
    B = OpaqueData,
    case disk_log:sync(B#backup.file_desc) of
        ok ->
            case disk_log:close(B#backup.file_desc) of
                ok ->
                    case file:rename(B#backup.tmp_file, B#backup.file) of
                       ok ->
                            {ok, B#backup.file};
                       {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

Into being:

```erlang
-spec commit_write(_) -> {ok, file:filename()} | {error, term()}.
commit_write(OpaqueData) ->
    begin
        ok <- disk_log:sync(OpaqueData#backup.file_desc),
        ok <- disk_log:close(OpaqueData#backup.file_desc),
        ok <- file:rename(OpaqueData#backup.tmp_file, OpaqueData#backup.file),
        {ok, OpaqueData#backup.file}
    end.
```

Which is much, much simpler.

More complex forms exist for the cases where we want to protect ourselves against bad return values. Let's take a look at a variation of our first example:

```erlang
as_sudo(UserId, Action) ->
    begin
        {ok, User=#{role := admin}} <- get_user(UserId),
        do_some_admin_thing(User, Action)
    end.
```

If we were to give this function a type signature, we might hope for something like `as_sudo(integer(), string()) -> ok | {error, term()}.` However, this wouldn't always be the case here: if the user isn't an administrator, then we might end up with a return value of `ok | {error, term()} | map()`, which isn't what we want. We can instead use a `cond` clause to capture and handle some of the errors:

```erlang
-spec as_sudo(integer(), string()) -> ok | {error, term()}.
as_sudo(UserId, Action) ->
    begin
        {ok, User=#{role := admin}} <- get_user(UserId),
        do_some_admin_thing(User, Action)
    cond
        {ok, _BadUser} -> {error, admin_only}
    end.
```

And now, when a non-matching value is found, it is handled in the `cond` clause. Here's a longer sample from Rebar3:

```erlang
maybe_upgrade(AppInfo, _AppDir, Upgrade, State) ->
    case Upgrade orelse rebar_app_info:is_lock(AppInfo) of
        true ->
            case rebar_fetch:needs_update(AppInfo, State) of
                true ->
                    ?INFO("Upgrading ~ts", [rebar_resource_v2:format_source(AppInfo)]),
                    rebar_fetch:download_source(AppInfo, State);
                false ->
                    case Upgrade of
                        true ->
                            ?INFO("No upgrade needed for ~ts", [rebar_app_info:name(AppInfo)]),
                            AppInfo;
                        false ->
                            AppInfo
                    end
            end;
        false ->
            AppInfo
    end.
```

Which could be rewritten as:

```erlang
maybe_upgrade(AppInfo, _AppDir, Upgrade, State) ->
    begin
        true <- Upgrade orelse rebar_app_info:is_lock(AppInfo),
        true <- rebar_fetch:needs_update(AppInfo, State),
        ?INFO("Upgrading ~ts", [rebar_resource_v2:format_source(AppInfo)]),
        rebar_fetch:download_source(AppInfo, State)
    cond
        false when Upgrade ->
            ?INFO("No Upgrade needed for ~ts", [rebar_app_info:name(AppInfo)]),
            AppInfo;
        false ->
            AppInfo
    end.
```

## Team Members

- @peerst
- @ferd
- The whole batch of contributors in OTP before Spawnfest.

## Implementation Details

This branch is forked off `master` branch as recommended by the contributing guides. We have covered the following level of features:

- Syntax support
- Implementation handled through the Abstract Syntax Tree (AST)
- Transformations in the compiler's first core conversion pass to nested `case ... of ... end` expressions in the AST, which gets translated to standard core Erlang
- Similar transformations in the shell, so usage of the new construct in the shell is fully supported
- Pretty-printing support (although indentation is a bit odd maybe still)
- Introducing new exceptions of the form `cond_clause`, similar to `if_clause` and `case_clause` exceptions
- Tests
- Adapt standard Emacs mode to indent and highlight `cond` correctly in a `begin...end`

What we haven't really had the time to check:

- Dialyzer compatibility
- fancier pretty printing and more advanced syntax tool cases

### Differences from EEP-49

While implementing we figured out that the choice of `else` in EEP-49 was more trouble than expected. Not only would it collide with usage of atom `else` but also with the `-else` attribute of epp.

So we decided to go with `begin ... cond ... end` instead.  Also trying out the alternative `begin ... catch ... end` in not merged PR [Switch from 'cond' to 'catch' in maybe clauses](https://github.com/spawnfest/eep49ers/pull/10). Keyword `cond` was always reserved but not used so far and `catch` is used for exceptions but that usage wouldn't collide.  We also discussed `cond ... catch ... end` which might or might not collide with the initially indended `cond` expression.

## Building

Nothing should change from regular builds of Erlang/OTP. See the [HOWTO/](https://github.com/spawnfest/eep49ers/tree/eep-49/HOWTO) section.

You can quickly test your build by just opening the Erlang shell:

```erlang
1> begin {ok, Fd} <- file:open("myfile", [read]), ok <- file:write(Fd, "hello, world"), ok <- file:close(Fd) end.
{error,enoent}
2> h().
1: begin
       {ok, Fd} <- file:open("myfile", [read]),
       ok <- file:write(Fd, "hello, world"),
       ok <- file:close(Fd)
   end
-> {error,enoent}
ok
```

## Running Tests

You can follow the standard instructions [in the official wiki](https://github.com/erlang/otp/wiki/Running-tests).

We specifically require compiler change for the tests, so until we add a new suite, just building the project itself might be considered a successful test.

To run only the tests needed, assuming bash and `xterm` being installed (for the compiler suite):

```bash
export ERL_TOP=$PWD
export PATH=$ERL_TOP/bin:$PATH

## The Erlang makefile system does not always catch changes made
## in files, so if the below steps do not work, commit your work
## and try again after running `git clean -xfdq`, which clears all
## unchecked files.
./otp_build setup -a --prefix=$PWD/tests_install
make install
./otp_build tests
export PATH=$PWD/tests_install/bin:$PATH
unset ERL_LIBS

cd release/tests/test_server
$ERL_TOP/tests_install/bin/erl

ts:install().
ts:run(stdlib, [erl_scan_SUITE, erl_lint_SUITE, begin_maybe_SUITE, erl_eval_SUITE]).
ts:run(syntax_tools, [syntax_tools_SUITE]).
% ts:run(stdlib, [batch]). % this takes many minutes to run and tests a lot of unrelated stuff
% ts:run(compiler, [batch]). % requires a bunch of external deps and remote displays?
^C
```

Re-runing tests after requires re-building things and starting afresh. You may have to delete the .beam files you modified to get the rebuild step to work.

```bash
cd $ERL_TOP
rm -rf tests_install/lib/erlang/lib/stdlib-3.15.2/ebin/* tests_install/lib/erlang/lib/syntax_tools-2.6/ebin/*
```

