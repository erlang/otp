[
  ## The order of these items determine
  ## how they are listed in the docs
  extras: [
    "guides/introduction.md",
    "guides/inets_services.md",
    "guides/http_client.md",
    "guides/http_server.md"
  ],
  groups_for_modules: [
    "Service API": [:inets],
    "HTTP client modules": [:httpc],
    "HTTP server modules": [~r/httpd/, ~r/mod_/],
    "Deprecated functionality": [:http_uri]
  ]
]
