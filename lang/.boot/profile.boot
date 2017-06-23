(require 'boot.repl)

(swap! boot.repl/*default-dependencies* conj
       '[refactor-nrepl "1.2.0-SNAPSHOT"]
	          '[cider/cider-nrepl "0.10.0-SNAPSHOT"])

(swap! boot.repl/*default-middleware* conj
       'refactor-nrepl.middleware/wrap-refactor)')
