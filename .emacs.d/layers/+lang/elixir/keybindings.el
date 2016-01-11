(add-hook 'elixir-mode-hook
          '(lambda ()
             (bind-keys* :prefix-map elixir-prefix-map
                         :prefix     "C-l d"
                         ("h" . elixir-mode-open-elixir-home)
                         ("m" . elixir-mode-open-docs-master)
                         ("s" . elixir-mode-open-docs-stable))))
