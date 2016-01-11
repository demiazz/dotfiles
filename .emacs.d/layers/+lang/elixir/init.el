(defun µ/elixir/use-executable (name path variable)
  (unless path
    (error "Path to `%s` must be defined." name))
  (unless (file-executable-p path)
    (error "`%s` must be executable file."))
  (set-default variable path))

(use-package alchemist
  :ensure t
  :pin    "melpa-stable"
  :init
  (µ/elixir/use-executable
   "mix" µ/elixir/mix-command 'alchemist-mix-command)
  (µ/elixir/use-executable
   "iex" µ/elixir/repl-command 'alchemist-iex-program-name)
  (µ/elixir/use-executable
   "elixir" µ/elixir/execute-command 'alchemist-execute-command)
  (µ/elixir/use-executable
   "elixirc" µ/elixir/compile-command 'alchemist-compile-command))
