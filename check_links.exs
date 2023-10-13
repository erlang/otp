#!/usr/bin/env elixir

Mix.install([
  :floki
])

defmodule Anchors do

  def get_anchors(document) do
    for {_, attr, _} <- Floki.find(document, "[id]") do
      :proplists.get_value("id",attr) |> String.downcase
    end
  end

  def maybe_warn(_file, _target, :true) do
    :ok
  end
  def maybe_warn(file, target, :false) do
    IO.puts("#{ file }: could not find #{ target }")
  end


  def validate_href(_file, <<"http://">> <> _href, _anchors) do
    :ok
  end
  def validate_href(_file, <<"https://">> <> _href, _anchors) do
    :ok
  end
  def validate_href(file, <<"#">> <> _anchor = href, anchors) do
    validate_href(file, Path.basename(file) <> href, anchors)
  end
  def validate_href(file, href, anchors) do
    target = Path.dirname(file) |> Path.join(href) |> Path.expand
    case String.split(target, "#") do
      [target | anchor] when anchor == [] or anchor == [""] ->
        case Path.extname target do
          ".html" -> maybe_warn(file, target, Map.has_key?(anchors, target))
          _ -> maybe_warn(file, target, File.exists?(target))
        end
      [t, anchor] ->
        case Map.fetch(anchors, t) do
          {:ok, targetAnchors} ->
            maybe_warn(file, target, :lists.member(String.downcase(anchor), targetAnchors))
          _ ->
            maybe_warn(file, target, :false)
        end
    end
  end

  def validate_hrefs(file, document, anchors) do
    if String.contains?(file,"jinterface/assets/java") do
      :ok
    else
      for {_, attr, _} <- Floki.find(document, "a[href]") do
        href = :proplists.get_value("href",attr)
        validate_href(file, href, anchors)
      end
    end
  end

end

patterns =
  case System.argv do
    [] -> ["docs/**/*.html"]
    patterns -> patterns
  end

files = for pattern <- patterns, file <- Path.wildcard(pattern)  do
  document = Floki.parse_document!(File.read!(file))
  { Path.expand(file), document}
end

anchors = :maps.from_list(
  for {file, document} <- files do
    { file, Anchors.get_anchors(document) }
  end);

for {file, document} <- files do
    Anchors.validate_hrefs(file, document, anchors)
end

# IO.inspect files
