[
  annotations_for_docs: fn
    md ->
      if md[:rfc] do
        [md[:rfc]]
      else
        []
      end
  end,
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/introduction.md",
    "guides/using_ssh.md",
    "guides/terminology.md",
    "guides/configurations.md",
    "guides/configure_algos.md",
    "guides/hardening.md",
    "ssh_app.md"
  ]
]
