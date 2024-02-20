#!/usr/bin/env elixir

Mix.install([
  :floki
])

defmodule Anchors do
  def get_anchors(file, document) do
    anchors =
      for {_, attr, _} <- Floki.find(document, "[id]") do
        if Enum.count(attr, fn {tag, _} -> String.equivalent?(tag, "id") end) > 1 do
          warn(file, "duplicate id attribute found: ")
        end

        :proplists.get_value("id", attr)
      end

    for anchor <-
          Enum.sort(anchors)
          |> Enum.chunk_by(fn a -> a end)
          |> Enum.filter(&(Enum.count(&1) > 1)) do
      warn(file, "Found duplicate anchor #{hd(anchor)}")
    end

    anchors
  end

  def warn(file, string) do
    Process.put(:exit_status, Process.get(:exit_status) + 1)
    IO.puts("#{Path.relative_to_cwd(file)}: #{string}")
  end

  def maybe_warn(file, target, what, suggestion \\ nil)

  def maybe_warn(_file, _target, true, _suggestion) do
    nil
  end

  def maybe_warn(file, target, false, suggestion) do
    if suggestion do
      warn(
        file,
        "could not find #{Path.relative_to_cwd(target)}, should it be ##{suggestion}?"
      )
    else
      warn(file, "could not find #{Path.relative_to_cwd(target)}")
    end

    nil
  end

  def validate_href(_file, "http://" <> _href, _anchors) do
    nil
  end

  def validate_href(_file, "https://" <> _href, _anchors) do
    nil
  end

  def validate_href(file, "`" <> _ = href, _anchors) do
    warn(file, "found #{href}")
  end

  def validate_href(file, <<"#">> <> _anchor = href, anchors) do
    validate_href(file, Path.basename(file) <> href, anchors)
  end

  def validate_href(file, href, anchors) do
    target = Path.dirname(file) |> Path.join(href) |> Path.expand()

    case String.split(target, "#", parts: 2) do
      [target | anchor] when anchor == [] or anchor == [""] ->
        case Path.extname(target) do
          ".html" -> maybe_warn(file, target, Map.has_key?(anchors, target))
          _ -> maybe_warn(file, target, File.exists?(target))
        end

      [t, anchor] ->
        case Map.fetch(anchors, t) do
          {:ok, targetAnchors} when targetAnchors != [] ->
            cond do
              :lists.member(anchor, targetAnchors) ->
                {t, anchor}

              :lists.member(URI.decode(anchor), targetAnchors) ->
                {t, URI.decode(anchor)}

              :lists.member(URI.decode(anchor), targetAnchors) ->
                {t, URI.decode(anchor)}

              :lists.member(URI.encode_www_form(anchor), targetAnchors) ->
                {t, URI.encode_www_form(anchor)}

              true ->
                {_, closest} =
                  targetAnchors
                  |> Enum.map(fn a ->
                    {String.jaro_distance(String.downcase(anchor), String.downcase(a)), a}
                  end)
                  |> Enum.sort()
                  |> Enum.reverse()
                  |> Enum.fetch!(0)

                maybe_warn(file, target, false, closest)
            end

          _ ->
            maybe_warn(file, target, false)
        end
    end
  end

  def validate_hrefs(file, document, anchors, seen) do
    if String.contains?(file, "doc/html/assets/") do
      seen
    else
      Floki.find(document, "a[href]")
      |> Enum.reduce(seen, fn {_, attr, _}, seen ->
        href = :proplists.get_value("href", attr)

        case validate_href(file, href, anchors) do
          {target, anchor} ->
            {_, seen} =
              Map.get_and_update(seen, target, fn
                nil -> {nil, [anchor]}
                anchors -> {anchors, [anchor | anchors]}
              end)

            seen

          nil ->
            seen
        end
      end)
    end
  end

  def validate_imgs(file, document) do
    Floki.find(document, "img[src]")
    |> Enum.map(fn {_, attr, _} ->
      target = Path.join(Path.dirname(file), :proplists.get_value("src", attr))

      if not File.exists?(target) do
        warn(file, "could not find #{Path.relative_to_cwd(target)}")
      end
    end)
  end
end

Process.put(:exit_status, 0)

patterns =
  case System.argv() do
    [] -> ["doc/", "doc/system/", "lib/*/doc/html/", "erts*/doc/html/", "system/doc/html/"]
    patterns -> patterns
  end

files =
  for pattern <- patterns, file <- Path.wildcard(pattern <> "**/*.html") do
    document = Floki.parse_document!(File.read!(file))
    {Path.expand(file), document}
  end

anchors =
  Map.new(
    for {file, document} <- files do
      anchors = Anchors.get_anchors(file, document)

      {file, anchors}
    end
  )

seen =
  Enum.reduce(files, %{}, fn {file, document}, seen ->
    Anchors.validate_hrefs(file, document, anchors, seen)
  end)

Enum.map(files, fn {file, document} ->
  Anchors.validate_imgs(file, document)
end)

generated_anchors = [
  "sidebar",
  "sidebar-listnav",
  "extras-list-tab-button",
  "modules-list-tab-button",
  "extras-tab-panel",
  "extras-full-list",
  "modules-tab-panel",
  "modules-full-list",
  "toast",
  "content",
  "moduledoc"
]

if false do
  anchors
  |> Enum.sort()
  |> Enum.map(fn {file, anchors} ->
    if not String.contains?(file, "doc/html/assets/") do
      seen_anchors = Map.get(seen, file, [])

      for a <- anchors do
        if a not in (seen_anchors ++ generated_anchors) do
          Anchors.warn(file, "Unused anchor #{a}")
        end
      end
    end
  end)
end

System.halt(Process.get(:exit_status))
