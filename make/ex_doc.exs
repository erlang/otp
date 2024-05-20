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

app =
  if local_config[:application] do
    local_config[:application]
  else
    Path.split(cwd) |> Enum.reverse() |> Enum.at(1) |> String.to_atom()
  end

## Assert that cwd is a doc directory
true = String.equivalent?("doc", Path.split(cwd) |> Enum.reverse() |> Enum.at(0)) or app == :index

## Calculate relative path to $ERL_TOP
rootdir =
  cond do
    app in [:erts, :system] ->
      "../../"

    app == :index ->
      "../../../"

    true ->
      "../../../"
  end

libdir = Path.join(rootdir, "lib")
app_pos = length(Path.split(libdir))

link_libdir =
  cond do
    app == :index ->
      Path.join(["..", "lib"])

    true ->
      Path.join("..", libdir)
  end

deps =
  Path.wildcard(Path.join(libdir, "*/ebin"))
  |> Enum.map(fn path ->
    app = Path.split(path) |> Enum.at(app_pos)
    {String.to_atom(app), Path.join([link_libdir, app, "doc", "html"])}
  end)

## Read the titles from all modules in order to create the groups_for_docs key
modules =
  cond do
    app == :erts ->
      :erlang.pre_loaded()

    app in [:system, :index] ->
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
         a[:kind] == :type && String.equivalent?(Access.get(a, :title, ""), title)
       end}
    end
  ) ++
    [Types: &(&1[:kind] == :type)] ++
    Enum.map(
      Access.get(titles, :function, []),
      fn {:function, title} ->
        {"#{title}",
         fn a ->
           a[:kind] == :function && String.equivalent?(Access.get(a, :title, ""), title)
         end}
      end
    )

## Create the correct source url to github
base_url = "https://github.com/" <> System.get_env("BASE_URL", "erlang/otp/blob/master/")

source_url_pattern = fn prefix, root ->
  fn path, line ->
    path = path |> Path.absname() |> Path.relative_to(root) |> String.replace_leading("./", "")
    "#{base_url}#{prefix}#{path}#L#{line}"
  end
end

source_url_pattern =
  cond do
    app == :erts ->
      source_url_pattern.(
        "erts/",
        Path.join(:code.root_dir() |> String.Chars.to_string(), "erts")
      )

    app == :system ->
      source_url_pattern.("", :code.root_dir() |> String.Chars.to_string())

    app == :index ->
      source_url_pattern.(
        "system/doc/top/",
        Path.join([:code.root_dir() |> String.Chars.to_string(), "system", "doc", "top"])
      )

    true ->
      source_url_pattern.("lib/#{app}/", :code.lib_dir(app) |> String.Chars.to_string())
  end

## Merge the local config with the default
extras =
  (Access.get(local_config, :extras, []) ++
     Path.wildcard("*.md") ++ Path.wildcard("{guides,references,internal_docs}/*.md"))
  |> Enum.uniq()

annotations = Access.get(local_config, :annotations_for_docs, fn _ -> [] end)

config = [
  proglang: :erlang,
  source_url_pattern: source_url_pattern,
  assets: Path.join(cwd, "/assets"),
  logo: Path.join(:code.root_dir(), "system/doc/assets/erlang-logo.png"),
  before_closing_head_tag: fn _ -> "<style>.dark img { background-color: white; }</style>" end,
  before_closing_footer_tag: fn _ ->
    ~S'<p>Copyright © 1996-2023 <a href="https://www.ericsson.com">Ericsson AB</a></p>'
  end,
  annotations_for_docs: fn md ->
    if Map.has_key?(md, :exported) && not md.exported do
      ["not exported"] ++ annotations.(md)
    else
      annotations.(md)
    end
  end,
  groups_for_extras:
    (Access.get(local_config, :groups_for_extras, []) ++
       [
         "User's Guides": ~r/guides/,
         "Command Line Tools": ~r|references/.*_cmd.md$|,
         References: ~r|references|,
         "Internal Docs": ~r/internal_doc/
       ])
    |> Enum.uniq(),
  extras: extras,
  skip_undefined_reference_warnings_on: ["notes.md"],
  groups_for_docs:
    (Access.get(local_config, :groups_for_extras, []) ++ groups_for_docs) |> Enum.uniq(),
  deps:
    deps ++
      [
        general_info: Path.join([link_libdir, "..", "doc"]),
        erts: Path.join([link_libdir, "..", "erts", "doc", "html"]),
        system: Path.join([link_libdir, "..", "system", "doc", "html"])
      ],
  before_closing_body_tag: fn
    :html ->
      """
      <script src="https://cdn.jsdelivr.net/npm/mermaid@10.9.0/dist/mermaid.min.js"></script>
      <script>
        document.addEventListener("DOMContentLoaded", function () {
          mermaid.initialize({
            startOnLoad: false,
            theme: document.body.className.includes("dark") ? "dark" : "default"
          });
          let id = 0;
          for (const codeEl of document.querySelectorAll("pre code.mermaid")) {
            const preEl = codeEl.parentElement;
            const graphDefinition = codeEl.textContent;
            const graphEl = document.createElement("div");
            const graphId = "mermaid-graph-" + id++;
            mermaid.render(graphId, graphDefinition).then(({svg, bindFunctions}) => {
              graphEl.innerHTML = svg;
              bindFunctions?.(graphEl);
              preEl.insertAdjacentElement("afterend", graphEl);
              preEl.remove();
            });
          }
        });
      </script>
      """

    _ ->
      ""
  end,
  before_closing_head_tag: fn
    :epub ->
      """
      <style type="text/css">
        .content-inner pre code.mermaid {
          display: none;
        }
      </style>
      """

    _ ->
      ""
  end
]

Keyword.merge(config, local_config |> Keyword.drop([:extras, :groups_for_extras]))
