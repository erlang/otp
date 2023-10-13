{global,_} = Code.eval_file Path.join(System.get_env("ERL_TOP"),"ex_doc.exs");
Keyword.merge(global,
  [
    extras: (["top/README.md",
             "general_info/deprecations.md",
             "general_info/removed.md",
             "general_info/scheduled_for_removal.md",
             "general_info/upcoming_incompatibilities.md",
             "top/system/installation_guide.md",
             "top/system/getting_started.md",
             "top/system/system_principles.md",
             "top/system/programming_examples.md",
             "top/system/reference_manual.md",
             "top/system/design_principles.md",
             "top/system/efficiency_guide.md",
             "top/system/embedded.md",
             "top/system/oam.md"] |> Enum.map(& Path.join("system/doc/", &1) ))
    ++ Path.wildcard("system/doc/top/{basic,database,oam,interfaces,tools,test,docs}/*.md"),
    main: "readme",
    api_reference: false,
    groups_for_extras: [ "System Documentation": ~r{top/system},
                         "Basic": ~r/basic/,
                         "Database": ~r/database/,
                         "Operations & Mainteinance": ~r/oam/,
                         "Interfaces & Communication": ~r/interfaces/,
                         "Tools": ~r/tools/,
                         "Test": ~r/test/,
                         "Documentation": ~r/docs/
                       ],
    skip_undefined_reference_warnings_on:
    ["dbg:stop_clear/0", "code:is_module_native/1"],

    deps: (Path.wildcard("lib/*/ebin") |> Enum.map(
    fn path -> app = Path.split(path) |> Enum.at(1);
      {String.to_atom(app), Path.join([app])} end)) ++ [erts: "erts", system: "system"]
  ]) |> IO.inspect
