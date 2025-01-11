(ns lib.util
  (:require [clojure.string :as string]
            [clojure.java.shell :as shell]))

(defn sh
  ([cmd] (->> (shell/sh "bash" "-c" cmd)
              (:out)
              (string/split-lines)))
  ([& cmds]
   (->> (apply shell/sh cmds)
        (:out)
        (string/split-lines))))

(defn shh [& args]
  (first (apply sh args)))

(def HOME (System/getenv "HOME"))



;; lol
(defmacro elisp [sexp]
  `(shh "elisp" "-r" ~(str sexp)))

(defn txtw [s]
  (int (if (System/getenv "ESHELL")
         ;; string-width lies, so we do it ourselves
         (parse-long (string/trim (:out (shell/sh "elisp" (format "(ns/string-width \"%s\")" s)))))
         ;; (parse-long (elisp (/ (string-pixel-width s) (string-pixel-width "═"))))
         (+ (* 1 (count (filter #(< 255 (int %)) s)))
            (* 1 (count (filter #(> 255 (int %)) s)))))))

(defn longest-txtw [lines]
  (last (sort (map txtw lines))))

(comment

  (let [s "arst"]
    (parse-long (elisp (/ (string-pixel-width s) (string-pixel-width "═")))))

  (txtw "arst")

  (shh "echo a")
  (shh "echo" "a")

  (read-string (elisp (ns/emacs-to-theme)))

  )
