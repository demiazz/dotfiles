(µ/default-settings 'elixir
                    `((mix-command     . ,(executable-find "mix"))
                      (repl-command    . ,(executable-find "iex"))
                      (execute-command . ,(executable-find "elixir"))
                      (compile-command . ,(executable-find "elixirc"))))


