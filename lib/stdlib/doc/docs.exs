[
  groups_for_modules:
    [
      SHELL: [:shell, :shell_default, :c, :shell_docs, :edlin, :edlin_expand],
      PROCESSES: [
        :gen_event,
        :gen_fsm,
        :gen_server,
        :gen_statem,
        :pool,
        :proc_lib,
        :supervisor,
        :supervisor_bridge,
        :sys,
        :log_mf_h
      ],
      STRINGS: [
        :uri_string,
        :unicode,
        :string,
        :re,
        :io,
        :io_lib,
        :filelib,
        :filename,
        :file_sorter,
        :base64,
        :erl_error
      ],
      DATATYPES: [:binary, :maps, :lists, :math],
      CODE: [
        :erl_scan,
        :erl_pp,
        :erl_parse,
        :erl_lint,
        :erl_internal,
        :erl_id_trans,
        :ms_transform,
        :erl_features,
        :erl_expand_records,
        :erl_eval,
        :erl_anno,
        :epp,
        :beam_lib
      ],
      "DATA STRUCTURES": [
        :array,
        :dict,
        :digraph,
        :digraph_utils,
        :gb_sets,
        :gb_trees,
        :json,
        :orddict,
        :ordsets,
        :proplists,
        :queue,
        :sets,
        :sofs,
        :ets,
        :dets,
        :qlc
      ],
      ALGORITHMS: [:rand, :random, :zip, :erl_tar],
      "DATE & TIME": [:calendar, :timer],
      NODES: [:slave, :peer, :argparse, :escript, :win32reg]
    ]
    |> Enum.sort(),
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/introduction.md",
    "guides/io_protocol.md",
    "guides/unicode_usage.md",
    "guides/uri_string_usage.md",
    "references/assert_hrl.md",
    "stdlib_app.md"
  ]
]
