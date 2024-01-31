[
  annotations_for_docs: fn
    %{module: :erlang} = md ->
      cond do
        md.kind == :function && :erl_internal.bif(md.name, md.arity) ->
          ["auto-imported"]

        md.kind == :type && :erl_internal.is_type(md.name, md.arity) ->
          ["predefined"]

        true ->
          []
      end

    _ ->
      []
  end,
  groups_for_docs: [
    {"Predefined datatypes",
     fn a ->
       a.kind == :type && a.module == :erlang && :erl_internal.is_type(a.name, a.arity)
     end},
    {"Types",
     fn a ->
       a.kind == :type
     end},
    {"Auto-imported BIFs",
     fn a ->
       a.kind == :function && a.module == :erlang && :erl_internal.bif(a.name, a.arity)
     end}
  ],
  ## The order of these items determine
  ## how they are listed in the docs
  extras:
    [
      "guides/introduction.md",
      "guides/communication.md",
      "guides/time_correction.md",
      "guides/match_spec.md",
      "guides/crash_dump.md",
      "guides/alt_dist.md",
      "guides/alt_disco.md",
      "guides/absform.md",
      "guides/tty.md",
      "guides/driver.md",
      "guides/inet_cfg.md",
      "guides/erl_ext_dist.md",
      "guides/erl_dist_protocol.md",
      "references/driver_entry.md",
      "references/epmd_cmd.md",
      "references/erl_cmd.md",
      "references/erlc_cmd.md",
      "references/erl_driver.md",
      "references/erl_nif.md",
      "references/erlsrv_cmd.md",
      "references/erts_alloc.md",
      "references/escript_cmd.md",
      "references/run_erl_cmd.md",
      "references/start_cmd.md",
      "references/start_erl_cmd.md",
      "references/werl_cmd.md"
    ] ++ Path.wildcard("../emulator/internal_doc/*.md"),
  skip_code_autolink_to: [
    "dist_util:net_ticker_spawn_options/0",
    "dist_util:handshake_we_started/1",
    "dist_util:handshake_other_started/1",
    "dist_util:start_timer/1",
    "dist_util:strict_order_flags/0",
    "erl_types:t_is_equal/2",
    "erl_types:t_has_var/1"
  ]
]
