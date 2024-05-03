[
  groups_for_modules: [
    "Code & System": [:application, :code, :erl_ddll, :error_handler, :heart, :os],
    Distribution: [
      :net_adm,
      :net_kernel,
      :auth,
      :erl_boot_server,
      :erl_epmd,
      :erpc,
      :global,
      :global_group,
      :pg,
      :rpc
    ],
    "Files & Networking": [
      :file,
      :gen_tcp,
      :gen_udp,
      :gen_sctp,
      :socket,
      :inet,
      :inet_res,
      :net
    ],
    Logging: [
      :logger,
      :disk_log,
      :error_logger,
      :logger_disk_log_h,
      :logger_filters,
      :logger_formatter,
      :logger_handler,
      :logger_std_h,
      :wrap_log_reader
    ],
    Tracing: [
      :seq_trace,
      :trace
    ]
  ],
  skip_code_autolink_to: ["t:file_descriptor/0"],
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/introduction_chapter.md",
    "guides/socket_usage.md",
    "guides/logger_chapter.md",
    "guides/logger_cookbook.md",
    "guides/eep48_chapter.md",
    "references/app.md",
    "references/config.md",
    "kernel_app.md"
  ]
]
