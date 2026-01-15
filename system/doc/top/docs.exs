# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2024-2025. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
system_guides =
  File.read!("../guides")
  |> String.trim()
  |> String.split("\n")
  |> Enum.map(fn line ->
    line |> String.split(":") |> Enum.at(0)
  end)

# Root /configure creates artifact /lib/SKIP-APPLICATIONS containing all apps
# not built for various reasons. We can't build documentation for them either.
skipped_apps_from_file =
  with erl_top when is_binary(erl_top) <- System.get_env("ERL_TOP"),
       skip_path <- Path.join([erl_top, "lib", "SKIP-APPLICATIONS"]),
       true <- File.exists?(skip_path),
       {:ok, content} <- File.read(skip_path) do
    content
    |> String.split(~r/\s+/, trim: true)
    |> Enum.map(&String.downcase/1)
    |> MapSet.new()
  else
    _ -> MapSet.new()
  end

apps =
  Path.wildcard("{core,database,oam,interfaces,tools,testing,documentation}/*.md")
  |> Enum.map(&Path.basename/1)
  |> Enum.map(&Path.rootname/1)
  |> Enum.reject(fn app -> skipped_apps_from_file |> MapSet.member?(app) end)

redirects =
  Enum.map(apps, fn
    "erts" -> {"erts", "../erts/doc/html/index"}
    app -> {app, "../lib/#{app}/doc/html/index"}
  end) ++
    Enum.map(system_guides, fn guide -> {guide, "../system/doc/html/#{guide}"} end)

[
  application: :index,
  extras:
    [
      "README.md",
      "man_index.md",
      "../general_info/deprecations.md",
      "../general_info/removed.md",
      "../general_info/scheduled_for_removal.md",
      "../general_info/upcoming_incompatibilities.md"
    ] ++
      (system_guides |> Enum.map(fn guide -> "system/#{guide}.md" end)) ++
      Path.wildcard("{core,database,oam,interfaces,tools,testing,documentation}/*.md"),
  main: "readme",
  api_reference: false,
  redirects: Map.new(redirects),
  groups_for_extras: [
    "System Documentation": ~r{system},
    Core: ~r/core/,
    Database: ~r/database/,
    "Operations & Maintenance": ~r/oam/,
    "Interfaces & Communication": ~r/interfaces/,
    Tools: ~r/tools/,
    Test: ~r/testing/,
    Documentation: ~r/documentation/
  ],
  skip_code_autolink_to: [
    "dbg:stop_clear/0",
    "net:broadcast/3",
    "net:call/4",
    "net:cast/4",
    "net:ping/1",
    "net:sleep/1",
    "net:broadcast/3",
    "net:call/4",
    "net:cast/4",
    "net:ping/1",
    "net:sleep/1",
    "zlib:adler32/2",
    "zlib:adler32/3",
    "zlib:adler32_combine/4",
    "zlib:crc32/1",
    "zlib:crc32/2",
    "zlib:crc32/3",
    "zlib:crc32_combine/4",
    "zlib:getBufSize/1",
    "zlib:inflateChunk/1",
    "zlib:inflateChunk/2",
    "zlib:setBufSize/2"
  ],
  skip_undefined_reference_warnings_on: ["../general_info/removed.md"]
]
