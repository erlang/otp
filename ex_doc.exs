[
	proglang: :erlang,
        source_url: "https://github.com/erlang/otp",
        source_ref: "master",
        extras: Path.wildcard("#{System.get_env("APP")}/doc/src/*.md"),
        assets: "#{System.get_env("APP")}/doc/assets",
        logo: "lib/erl_docgen/priv/images/erlang-logo.png",
        before_closing_head_tag: fn(_) -> "<style>.dark img { background-color: white; }</style>" end,
	deps: (Path.wildcard("lib/*/ebin") |> Enum.map(
              fn path -> app = Path.split(path) |> Enum.at(1);
              {String.to_atom(app), Path.join(["..",app])} end)) ++ [erts: "../erts", system: "../system/"]
]
