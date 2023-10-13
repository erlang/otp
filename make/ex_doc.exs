cwd = File.cwd!()

## Read local configuration
local_config_file = Path.join(cwd, "docs.exs")

local_config =
  case File.exists?(local_config_file) do
    true ->
      {local_config, _} = Code.eval_file(local_config_file)
      local_config

    false ->
      []
  end

## Assert that cwd is a doc directory
true = String.equivalent?("doc", Path.split(cwd) |> Enum.reverse() |> Enum.at(0))

app = Path.split(cwd) |> Enum.reverse() |> Enum.at(1) |> String.to_atom()

{libdir, app_pos} =
  if app in [:erts, :system] do
    {"../../lib/", 3}
  else
    {"../../", 2}
  end

deps =
  Path.wildcard(Path.join(libdir, "*/ebin"))
  |> Enum.map(fn path ->
    app = Path.split(path) |> Enum.at(app_pos)
    {String.to_atom(app), Path.join("..", app)}
  end)

## Read the titles from all modules in order to create the groups_for_docs key
modules =
  cond do
    app == :erts ->
      :erlang.pre_loaded()

    app == :system ->
      []

    true ->
      Application.load(app)
      {:ok, modules} = :application.get_key(app, :modules)
      modules
  end

titles =
  modules
  |> Enum.reduce(
    [],
    fn module, acc ->
      case Code.fetch_docs(module) do
        {:docs_v1, _, :erlang, _, _, %{:titles => ts}, _} ->
          acc ++ ts

        _ ->
          acc
      end
    end
  )
  |> Enum.group_by(fn e -> elem(e, 0) end)

groups_for_docs =
  Enum.map(
    Access.get(titles, :type, []),
    fn {:type, title} ->
      {"Types: #{title}",
       fn a ->
         a[:__doc__] == :type && String.equivalent?(Access.get(a, :title, ""), title)
       end}
    end
  ) ++
    [Types: &(&1[:__doc__] == :type)] ++
    Enum.map(
      Access.get(titles, :function, []),
      fn {:function, title} ->
        {"#{title}",
         fn a ->
           a[:__doc__] == nil && String.equivalent?(Access.get(a, :title, ""), title)
         end}
      end
    )

## Merge the local config with the default
extras =
  (Access.get(local_config, :extras, []) ++
     Path.wildcard("*.md") ++ Path.wildcard("{guides,references,internal_docs}/*.md"))
  |> Enum.uniq()

config = [
  proglang: :erlang,
  source_url: "https://github.com/erlang/otp",
  source_ref: System.get_env("SOURCE_REF", "master"),
  assets: Path.join(cwd, "/assets"),
  logo: Path.join(libdir, "erl_docgen/priv/images/erlang-logo.png"),
  before_closing_head_tag: fn _ -> "<style>.dark img { background-color: white; }</style>" end,
  groups_for_extras:
    (Access.get(local_config, :groups_for_extras, []) ++
       [
         "User's Guides": ~r/guides/,
         References: ~r/references/,
         "Internal Docs": ~r/internal_docs/
       ])
    |> Enum.uniq(),
  extras: extras,
  groups_for_docs: groups_for_docs,
  deps: deps ++ [erts: "../erts", system: "../system/"]
]

Keyword.merge(config, local_config |> Keyword.drop([:extras, :groups_for_extras])) |> dbg
