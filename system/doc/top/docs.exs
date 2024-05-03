system_guides =
  File.read!("../guides")
  |> String.trim()
  |> String.split("\n")
  |> Enum.map(fn line ->
    guide = line |> String.split(":") |> Enum.at(0)
    "system/#{guide}.md"
  end)

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
      system_guides ++
      Path.wildcard("{core,database,oam,interfaces,tools,testing,documentation}/*.md"),
  main: "readme",
  api_reference: false,
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
