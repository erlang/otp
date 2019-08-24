defmodule Smoke.MixProject do
  use Mix.Project

  def project do
    [
      app: :smoke,
      version: "0.1.0",
      elixir: "~> 1.8",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    case :os.getenv('SMOKE_DEPS_SET') do
      'main' ->
	[
	  {:bear, "~> 0.8.7"},
	  {:cloudi_core, "~> 1.7"},
	  {:cloudi_service_monitoring, "~> 1.7"},
	  {:cloudi_service_tcp, "~> 1.7"},
	  {:cloudi_service_queue, "~> 1.7"},
	  {:cloudi_service_udp, "~> 1.7"},
	  {:cloudi_service_map_reduce, "~> 1.7"},
	  {:cloudi_service_api_requests, "~> 1.7"},
	  {:cloudi_service_router, "~> 1.7"},
	  {:cloudi_service_request_rate, "~> 1.7"},
	  {:concuerror, "~> 0.20.0"},
	  {:cowboy, "~> 2.6.1"},
	  {:ecto, "~> 3.0.6"},
	  {:ex_doc, "~> 0.19.3"},
	  {:distillery, "~> 2.0.12"},
	  {:erlydtl, "~> 0.12.1"},
	  {:gen_smtp, "~> 0.13.0"},
	  {:getopt, "~> 1.0.1"},
	  {:gettext, "~> 0.16.1"},
	  {:gpb, "~> 4.6"},
	  {:gproc, "~> 0.8.0"},
	  {:graphql, "~> 0.15.0", hex: :graphql_erl},
	  {:hackney, "~> 1.15.0"},
	  {:ibrowse, "~> 4.4.1"},
	  {:jose, "~> 1.9.0"},
	  {:lager, "~> 3.6"},
	  {:locus, "~> 1.6"},
	  {:nimble_parsec, "~> 0.5.0"},
	  {:phoenix, "~> 1.4.0"},
	  {:riak_pb, "~> 2.3"},
	  {:scalaris, git: "https://github.com/scalaris-team/scalaris",
	   compile: build_scalaris()},
	  {:tdiff, "~> 0.1.2"},
	  {:webmachine, "~> 1.11"},
	  {:wings, git: "https://github.com/dgud/wings.git",
	   compile: build_wings()},
	  {:zotonic_stdlib, "~> 1.0"},
	]
      'rabbitmq' ->
	[{:rabbit_common, "~> 3.7"}]
      _ ->
	[]
    end
  end

  defp build_scalaris do
    # Only compile the Erlang code.

    """
    echo '-include("rt_simple.hrl").' >include/rt.hrl
    (cd src && erlc -W0 -I ../include -I ../contrib/log4erl/include -I ../contrib/yaws/include *.erl)
    (cd src/comm_layer && erlc -W0 -I ../../include -I *.erl)
    (cd src/cp && erlc -W0 -I ../../include -I *.erl)
    (cd src/crdt && erlc -W0 -I ../../include -I *.erl)
    (cd src/json && erlc -W0 -I ../../include -I *.erl)
    (cd src/paxos && erlc -W0 -I ../../include -I *.erl)
    (cd src/rbr && erlc -W0 -I ../../include -I *.erl)
    (cd src/rrepair && erlc -W0 -I ../../include -I *.erl)
    (cd src/time && erlc -W0 -I ../../include -I *.erl)
    (cd src/transactions && erlc -W0 -I ../../include -I *.erl)
    (cd src/tx && erlc -W0 -I ../../include -I *.erl)
    """
  end

  defp build_wings do
    # If the Erlang system is not installed, the build will
    # crash in plugins_src/accel when attempting to build
    # the accel driver. Since there is very little Erlang code in
    # the directory, skip the entire directory.

    """
    echo "all:\n\t" >plugins_src/accel/Makefile
    git commit -a -m'Disable for smoke testing'
    git tag -a -m'Smoke test' vsmoke_test
    make
    """
  end
end
